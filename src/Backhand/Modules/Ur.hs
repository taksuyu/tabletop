{-# LANGUAGE DataKinds #-}

module Backhand.Modules.Ur where

import GHC.Generics (Generic)

import qualified Control.Concurrent.Chan.Unagi.Bounded as UC
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.String as Str
import qualified STMContainers.Map as SM

import Control.Monad.Reader
import Data.Generics.Product
import Data.UUID as U
import Katip
import Lens.Micro.Platform
import UnliftIO
import UnliftIO.Concurrent

import Backhand
import Game.Ur

import Tabletop.Common
import Tabletop.Message
import Tabletop.Message.Ur

-- | I'll probably move this to it's own module in the future or make a generic
-- version to be used in `Backhand`.
type TabletopService r a = ReaderT r (KatipContextT IO) a

runGenericGame :: KatipContextT IO a -> Tabletop IO a
runGenericGame fn = do
  env <- ask
  let logEnv'       = env ^. logEnv
      logContext'   = env ^. logContext
      logNamespace' = env ^. logNamespace
  liftIO (runKatipContextT logEnv' logContext' logNamespace' fn)

data UrGameEnv = UrGameEnv
  { timerChannel :: Chan ()
  , incomingMessages :: UC.OutChan (TMessager UrMessage)
  , backhandClients :: Clients UrTabletopResponse
  , game :: TVar Game
  }

data Game = Game
  { players :: Map.Map Player (Unique, BS.ByteString)
  , board :: Board
  } deriving (Eq, Generic)

-- NOTE: Sends message over connection if the channel exists within the backhand
-- client map.
returnServerMessage :: Applicative f => (c -> m -> f ()) -> Maybe c -> m -> f ()
returnServerMessage = maybe (void . pure)

joinPlayerMessage :: MonadIO m => Unique -> BS.ByteString -> TVar Game -> Player -> m UrResponse
joinPlayerMessage u sessionId game player = atomically $ do
  gameState <- readTVar game

  let checkSession new@(_, s1) old@(_, s2)
        | s1 == s2  = new
        | otherwise = old

      gameState' = gameState & field @"players" %~ Map.insertWith checkSession player (u, sessionId)
      changed = gameState' /= gameState

  when changed $
    writeTVar game gameState'

  if changed
    then pure $ JoinSuccess player
    else pure $ JoinFailure $ (Str.fromString . show) player `mappend` "was already taken"

-- TODO: Property to test returns are only MoveSuccess or MoveFailure
movePlayerMessage :: Unique -> TVar Game -> Int -> Int -> TabletopService UrGameEnv  UrResponse
movePlayerMessage u game n newDice = atomically $ do
  gameState@Game{ board } <- readTVar game

  -- Check the player's turn and apply the move; passing back the message with
  -- the result if any.
  --
  -- NOTE: We could probably make this a little easier to understand with lens
  fmap fst . traverse (traverse (writeTVar game)) $
    if currentPlayer u gameState
    then
      case move n board of
        Just a ->
          let newBoardState = nextBoard newDice a
          in ( MoveSuccess newBoardState
             , Just $ gameState { board = newBoardState }
             )
        Nothing ->
          ( MoveFailure "Not a valid move", Nothing )
    else
      ( MoveFailure "Not player's turn", Nothing )

-- TODO: Property to test returns are only PassSuccess or PassFailure
passPlayerMessage :: Unique -> TVar Game -> Int -> TabletopService UrGameEnv  UrResponse
passPlayerMessage u game newDice = atomically $ do
  gameState@Game{ board = board@Board { turn } } <- readTVar game
  if currentPlayer u gameState
    then do
      writeTVar game (gameState{ board = board{ turn = nextTurn turn, dice = newDice } })
      pure $ PassSuccess (NextTurn (nextTurn turn) newDice)
    else
      pure $ PassFailure "Not the player's turn"

-- TODO? We may be able to simplfy and remove error messages if we adopt a
-- Either interface, however this may require that our encoding be changed.
urGameService :: TabletopService UrGameEnv ()
urGameService = do
  UrGameEnv { timerChannel, incomingMessages, backhandClients, game } <- ask
  TMessager u sessionId message <- liftIO $ UC.readChan incomingMessages
  newDice <- liftIO $ newDiceRoll
  inChan <- atomically . SM.lookup u $ unClients backhandClients
  case message of
    Join player ->
      joinPlayerMessage u sessionId game player >>= returnServerMessage
        (\ chan reply -> case reply of
            JoinSuccess p ->
              liftIO $ do
                UC.writeChan chan $ ServiceResponse reply
                broadcastOthers u backhandClients $ ServiceResponse (JoinSuccessOther p)
            JoinFailure _ ->
              liftIO . UC.writeChan chan $ ServiceResponse reply
            response ->
              $(logTM) ErrorS $ "Unexpected response from joinPlayerMessage: " `mappend` ls (show response)
        )
        inChan
    Move n ->
      movePlayerMessage u game n newDice >>= returnServerMessage
        (\ chan reply -> case reply of
            MoveSuccess _ ->
              liftIO . broadcast backhandClients $ ServiceResponse reply
            MoveFailure _ ->
              liftIO . UC.writeChan chan $ ServiceResponse reply
            response ->
              $(logTM) ErrorS $ "Unexpected response from movePlayerMessage: " `mappend` ls (show response)
        )
        inChan
    PassTurn ->
      passPlayerMessage u game newDice >>= returnServerMessage
        (\ chan reply -> case reply of
            PassSuccess _ ->
              liftIO . broadcast backhandClients $ ServiceResponse reply
            PassFailure _ ->
              liftIO . UC.writeChan chan $ ServiceResponse reply
            response ->
              $(logTM) ErrorS $ "Unexpected response from passPlayerMessage: " `mappend` ls (show response)
        )
        inChan
    GetCurrentGame -> do
      Game{ board } <- atomically $ readTVar game
      liftIO . returnServerMessage UC.writeChan inChan $ ServiceResponse (CurrentGame board)

  -- Check if the game is finished
  atomically (readTVar game) >>= \case
    Game{ board = Board{ black = Side{ scored = 7 } } } ->
      liftIO . returnServerMessage UC.writeChan inChan $ ServiceResponse (PlayerHasWon PlayerBlack)
    Game{ board = Board{ white = Side{ scored = 7 } } } ->
      liftIO . returnServerMessage UC.writeChan inChan $ ServiceResponse (PlayerHasWon PlayerWhite)
    _ -> do
      writeChan timerChannel ()  -- Tell our timer to restart
      urGameService

-- TODO: Maybe this function should be renamed?
currentPlayer :: Unique -> Game -> Bool
currentPlayer u Game{ players, board = Board{ turn } } =
     Just u == fmap fst (Map.lookup PlayerBlack players) && turn == Black
  || Just u == fmap fst (Map.lookup PlayerWhite players) && turn == White

urGameChannel :: Tabletop IO ChannelInfo
urGameChannel = do
  Env { _bhand = TabletopBackhand { _urChannels } } <- ask
  chanInfo@ChannelInfo { channelUUID } <- do
    (uuid, channel@Channel{ services, clients }) <- liftIO $ newChannel
    (moduleText, unagi) <- liftIO $ newService "UrService"
    atomically $ do
      SM.insert (inChan unagi) moduleText (unServices services)
      addChannel uuid channel _urChannels

    timerChan <- newChan
    game <- liftIO $ (newTVarIO . Game Map.empty) =<< newBoard
    void . runGenericGame $ forkFinally
      (race (timer timerChan)
        (runReaderT urGameService (UrGameEnv timerChan (outChan unagi) clients game)))
      (\_ -> atomically $ SM.delete uuid (unChannels _urChannels))
    pure $ ChannelInfo uuid [moduleText]
  $(logTM) InfoS $ "Started game session: " `mappend` ls (U.toText channelUUID)
  pure chanInfo
  where
    -- Delay 30 minutes while waiting for a message from the service to restart
    -- the timer.
    timer chan = race (threadDelay 1800000000) (readChan chan) >>= \case
      Right _ -> timer chan
      -- TODO? Should we notify clients that the service is shutting down
      Left _ -> pure ()
