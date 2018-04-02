module Test.Backhand.Modules.Ur where

import Control.Concurrent.Unique
import qualified Data.Map as Map
import Hedgehog
import qualified Hedgehog.Gen as Gen
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
        (Map.fromList [(PlayerBlack, Just u1), (PlayerWhite, Just u1)])
        board
  gameTVar <- newTVarIO game
  player <- forAll $ Gen.enum PlayerBlack PlayerWhite

  _ <- joinPlayerMessage u2 gameTVar player

  game' <- atomically $ readTVar gameTVar
  assert $ game == game'

tests :: IO Bool
tests = checkParallel $$(discover)
