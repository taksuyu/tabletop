{-# LANGUAGE TemplateHaskell #-}

module Backhand.Modules.Ur where

import Backhand
import Control.Concurrent as CC
import Control.Concurrent.Async
import Control.Concurrent.Chan.Unagi.Bounded as UC
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Reader
-- import Data.Text
import Data.UUID as U
import Katip
import Game.Ur
import qualified STMContainers.Map as M

import Tabletop.Common
import Tabletop.Message
import Tabletop.Message.Ur

data Game = Game
  { playerBlack :: Maybe Unique
  , playerWhite :: Maybe Unique
  , board :: Board
  }

-- NOTE: Sends message over connection if the channel exists within the backhand client map.
returnServerMessage :: Applicative f => (c -> m -> f ()) -> Maybe c -> m -> f ()
returnServerMessage = maybe (void . pure)

joinPlayerMessage :: Unique -> TVar Game -> Player -> STM UrResponse
joinPlayerMessage u game player = do
  gameState <- readTVar game

  -- If our players already exist we don't want to override them.
  let (s, newGame) =
        case player of
          PlayerBlack ->
            case playerBlack gameState of
              Nothing ->
                ( JoinSuccess player
                , Just $ gameState { playerBlack = Just u } )
              _ ->
                ( JoinFailure "Player Black is already taken"
                , Nothing )
          PlayerWhite ->
            case playerWhite gameState of
              Nothing ->
                ( JoinSuccess player
                , Just $ gameState { playerWhite = Just u } )
              _ ->
                ( JoinFailure "Player White is already taken"
                , Nothing )

  -- Return s after we optionally set the new game state over Maybe
  pure (const s) <*> pure (fmap (writeTVar game) newGame)

movePlayerMessage :: Unique -> TVar Game -> Int -> Int -> STM UrResponse
movePlayerMessage u game n newDice = do
  gameState@Game{ board } <- readTVar game

  -- Check the player's turn and apply the move; passing back the message with
  -- the result if any.
  let (s, newGame) =
        if currentPlayer u gameState
        then
          case move n board of
            Just a ->
              let newBoardState = nextBoard newDice a
              in
              ( MoveSuccess newBoardState
              , Just $ gameState { board = newBoardState }
              )
            Nothing ->
              ( MoveFailure "Not a valid move", Nothing )
        else
          (MoveFailure "Not player's turn", Nothing )

  maybe (pure ()) (writeTVar game) newGame
  pure s

passPlayerMessage :: Unique -> TVar Game -> Int -> STM UrResponse
passPlayerMessage u game newDice = do
  gameState@Game{ board = board@Board { turn } } <- readTVar game
  if currentPlayer u gameState
    then do
      writeTVar game (gameState{ board = board{ turn = nextTurn turn, dice = newDice } })
      pure $ PassSuccess (NextTurn (nextTurn turn) newDice)
    else
      pure $ PassFailure "Not the player's turn"

urGameService :: Chan () -> OutChan (Message UrMessage) -> Clients UrTabletopResponse -> IO ()
urGameService timerChan outChan rmap = do
  board <- newBoard
  game <- newTVarIO $ Game Nothing Nothing board
  urGameLoop timerChan outChan rmap game

urGameLoop :: Chan () -> OutChan (Message UrMessage) -> Clients UrTabletopResponse -> TVar Game -> IO ()
urGameLoop timerChan outChan clients game = do
  Message u message <- UC.readChan outChan
  newDice <- newDiceRoll
  inChan <- atomically . M.lookup u $ unClients clients
  putStrLn "Got Message Ur"
  case message of
    Join player ->
      atomically (joinPlayerMessage u game player)
      >>= returnServerMessage
        (\ chan reply -> case reply of
            JoinSuccess p -> do
              -- FIXME: Client gets both messages
              UC.writeChan chan $ ServiceResponse reply
              broadcastOthers u clients $ ServiceResponse (JoinSuccessOther p)
            _ ->
              UC.writeChan chan $ ServiceResponse reply
        )
        inChan
    Move n ->
      atomically (movePlayerMessage u game n newDice)
      >>= returnServerMessage
        (\ chan reply -> case reply of
            MoveSuccess _ ->
              broadcast clients $ ServiceResponse reply
            _ ->
              UC.writeChan chan $ ServiceResponse reply
        )
        inChan
    PassTurn ->
      atomically (passPlayerMessage u game newDice)
      >>= returnServerMessage
        (\ chan reply -> case reply of
            PassSuccess _ ->
              broadcast clients $ ServiceResponse reply
            _ ->
              UC.writeChan chan $ ServiceResponse reply
        )
        inChan
    GetCurrentGame -> do
      Game{ board } <- atomically $ readTVar game
      returnServerMessage UC.writeChan inChan $ ServiceResponse (CurrentGame board)

  -- Check if the game is finished
  currentGame <- atomically $ readTVar game
  case currentGame of
    Game{ board = Board{ black = Side{ scored = 7 } } } ->
      returnServerMessage UC.writeChan inChan $ ServiceResponse (PlayerHasWon PlayerBlack)
    Game{ board = Board{ white = Side{ scored = 7 } } } ->
      returnServerMessage UC.writeChan inChan $ ServiceResponse (PlayerHasWon PlayerWhite)
    _ -> do
      CC.writeChan timerChan ()  -- Tell our timer to restart
      urGameLoop timerChan outChan clients game

-- TODO: Maybe this function should be renamed?
currentPlayer :: Unique -> Game -> Bool
currentPlayer u Game{ playerBlack, playerWhite, board = Board{ turn } } =
     Just u == playerBlack && turn == Black
  || Just u == playerWhite && turn == White

urGameChannel :: Tabletop IO ChannelInfo
urGameChannel = do
  Env { bhand = TabletopBackhand { urChannelMap } } <- ask
  chanInfo@ChannelInfo { channelUUID } <- liftIO $ do
    (uuid, channel@Channel{ services, clients }) <- newChannel
    (moduleText, unagi) <- newService "UrService"
    atomically $ do
      M.insert (inChan unagi) moduleText (unServices services)
      addChannel uuid channel urChannelMap

    timerChan <- CC.newChan
    void . forkFinally (race (timer timerChan) (urGameService timerChan (outChan unagi) clients))
      $ \ _ -> atomically $ M.delete uuid (unChannels urChannelMap)
    pure $ ChannelInfo uuid [moduleText]
  $(logTM) InfoS $ "Started game session: " `mappend` ls (U.toText channelUUID)
  pure chanInfo
  where
    -- Delay 30 minutes while waiting for a message from the service to restart
    -- the timer.
    timer chan = race (threadDelay 1800000000) (CC.readChan chan)
      >>= \case
        Right _ -> timer chan
        Left _ ->
          -- TODO? Should we notify clients that the service is shutting down
          pure ()
