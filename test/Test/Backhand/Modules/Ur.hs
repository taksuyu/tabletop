module Test.Backhand.Modules.Ur where

import qualified Data.Map as Map
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Control.Concurrent.Unique
import Control.Monad
import Hedgehog
import UnliftIO (atomically, liftIO, newTVarIO, readTVar)

import Game.Ur
import Tabletop.Message.Ur

import Backhand.Modules.Ur

prop_joinPlayerMessage_respects_existing_players :: Property
prop_joinPlayerMessage_respects_existing_players = property $ do
  (u1, u2, board) <- pure (,,)
    <*> liftIO newUnique
    <*> liftIO newUnique
    <*> liftIO newBoard

  let game = Game
        (Map.fromList [(PlayerBlack, (u1, "")), (PlayerWhite, (u1, ""))])
        board

  gameTVar <- newTVarIO game
  player <- forAll $ Gen.enum PlayerBlack PlayerWhite
  session <- forAll $ Gen.bytes (Range.linear 0 10)

  -- We want to make sure we aren't generating an empty string that
  -- would end up replacing a player
  when (session /= "") $
    void (joinPlayerMessage u2 session gameTVar player)

  game' <- atomically $ readTVar gameTVar
  assert $ game == game'

prop_joinPlayerMessage_replaces_player_by_session :: Property
prop_joinPlayerMessage_replaces_player_by_session = property $ do
  (u1, u2, board) <- pure (,,)
    <*> liftIO newUnique
    <*> liftIO newUnique
    <*> liftIO newBoard

  session <- forAll $ Gen.bytes (Range.linear 0 10)

  let game = Game
        (Map.fromList [(PlayerBlack, (u1, session)), (PlayerWhite, (u1, session))])
        board

  gameTVar <- newTVarIO game
  player <- forAll $ Gen.enum PlayerBlack PlayerWhite

  _ <- joinPlayerMessage u2 session gameTVar player

  game' <- atomically $ readTVar gameTVar
  assert $ game /= game'

prop_joinPlayerMessage_returns_proper_messages :: Property
prop_joinPlayerMessage_returns_proper_messages = property $ do
  (u1, u2, board) <- pure (,,)
    <*> liftIO newUnique
    <*> liftIO newUnique
    <*> liftIO newBoard

  s1 <- forAll $ Gen.bytes (Range.linear 0 10)
  s2 <- forAll $ Gen.bytes (Range.linear 0 10)

  let game = Game
        (Map.fromList [(PlayerBlack, (u1, s1))])
        board

  gameTVar <- newTVarIO game
  player <- forAll $ Gen.enum PlayerBlack PlayerWhite

  response <- joinPlayerMessage u2 s2 gameTVar player

  assert $ case response of
    JoinSuccess _ -> True
    JoinFailure _ -> True
    _             -> False

tests :: IO Bool
tests = checkParallel $$(discover)
