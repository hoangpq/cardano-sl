module Test.Spec.TxMetaScenarios (
      txMetaScenarioA
    , txMetaScenarioB
    , txMetaScenarioC
    ) where

import           Universum

import qualified Data.Set as Set

import qualified Cardano.Wallet.Kernel as Kernel
import           Cardano.Wallet.Kernel.DB.TxMeta.Types
import           Cardano.Wallet.Kernel.Internal

import           Pos.Core.Chrono

import           Test.Hspec
import           Test.Infrastructure.Genesis
import           UTxO.Context
import           UTxO.DSL
import           Wallet.Inductive

-- | A Payment from P0 to P1 with change returned to P0
paymentWithChangeFromP0ToP1 :: forall h. Hash h Addr
                            => GenesisValues h Addr -> Transaction h Addr
paymentWithChangeFromP0ToP1 GenesisValues{..} = Transaction {
         trFresh = 0
       , trIns   = Set.fromList [ fst initUtxoP0 ]
       , trOuts  = [ Output p1 1000
                   , Output p0 (initBalP0 - 1 * (1000 + fee)) -- change
                   ]
       , trFee   = fee
       , trHash  = 1
       , trExtra = []
       }
  where
    fee = overestimate txFee 1 2

-- | Scenario A
-- Empty case
txMetaScenarioA :: GenesisValues h Addr
                   -> (Inductive h Addr, (PassiveWallet -> IO ()))
txMetaScenarioA GenesisValues{..} = (ind, lengthCheck 0)
  where
    ind = Inductive {
          inductiveBoot   = boot
        , inductiveOurs   = Set.singleton p0 -- define the owner of the wallet: Poor actor 0
        , inductiveEvents = OldestFirst [
            ]
        }

-- | Scenario B
-- A single pending payment from P0 to P1, with 'change' returned to P0
--
-- This scenario shows that a for an address to be considered 'change', it must be
-- part of a confirmed transaction
txMetaScenarioB :: forall h. Hash h Addr
                   => GenesisValues h Addr
                   -> (Inductive h Addr, PassiveWallet -> IO ())
txMetaScenarioB genVals@GenesisValues{..} = (ind, lengthCheck 1)
  where
    t0 = paymentWithChangeFromP0ToP1 genVals
    ind = Inductive {
          inductiveBoot   = boot
        , inductiveOurs   = Set.singleton p0 -- define the owner of the wallet: Poor actor 0
        , inductiveEvents = OldestFirst [
                NewPending t0
            ]
        }

-- | Scenario C
-- A single pending payment from P0 to P1, with 'change' returned to P0
--
-- This scenario shows that a for an address to be considered 'change', it must be
-- part of a confirmed transaction
txMetaScenarioC :: forall h. Hash h Addr
                   => GenesisValues h Addr
                   -> (Inductive h Addr, PassiveWallet -> IO ())
txMetaScenarioC genVals@GenesisValues{..} = (ind, lengthCheck 1)
  where
    t0 = paymentWithChangeFromP0ToP1 genVals
    ind = Inductive {
          inductiveBoot   = boot
        , inductiveOurs   = Set.singleton p0 -- define the owner of the wallet: Poor actor 0
        , inductiveEvents = OldestFirst [
              NewPending t0
            , ApplyBlock $ OldestFirst [t0] -- confirms t0 and updates block metadata
            ]
        }

lengthCheck :: Int -> PassiveWallet -> IO ()
lengthCheck n pw = do
    let db = pw ^. Kernel.walletMeta
    meta <- getAllTxMetas db
    length meta `shouldBe` n
