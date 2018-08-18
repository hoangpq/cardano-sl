module Test.Pos.Core.Json where

import           Hedgehog (Property)
import qualified Hedgehog as H

import           Pos.Core.JsonLog.LogEvents (InvReqDataFlowLog (..))

import           Test.Pos.Core.ExampleHelpers (exampleAddress, exampleAddress1,
                     exampleAddress2, exampleAddress3, exampleAddress4,
                     exampleGenesisConfiguration_GCSpec0,
                     exampleGenesisConfiguration_GCSpec1,
                     exampleGenesisConfiguration_GCSpec2,
                     exampleGenesisConfiguration_GCSrc, feedPM)
import           Test.Pos.Core.Gen (genAddress, genBlockVersionData, genByte,
                     genCoin, genCoinPortion, genEpochIndex, genFlatSlotId,
                     genGenesisAvvmBalances, genGenesisConfiguration,
                     genGenesisDelegation, genGenesisInitializer,
                     genGenesisProtocolConstants, genInvReqDataFlowLog,
                     genSharedSeed, genSoftforkRule, genTxFeePolicy)
import           Test.Pos.Crypto.Gen (genRedeemPublicKey)
import           Test.Pos.Util.Gen (genMillisecond)
import           Test.Pos.Util.Golden (discoverGolden, eachOf, goldenTestJSON,
                     goldenTestJSONDec)
import           Test.Pos.Util.Tripping (discoverRoundTrip,
                     roundTripsAesonBuildable, roundTripsAesonShow)
import           Universum

--------------------------------------------------------------------------------
-- Address
--------------------------------------------------------------------------------

golden_Address0 :: Property
golden_Address0 =
    goldenTestJSON
        exampleAddress
        "test/golden/json/Address0"

golden_Address1 :: Property
golden_Address1 =
    goldenTestJSON
        exampleAddress1
        "test/golden/json/Address1"

golden_Address2 :: Property
golden_Address2 =
    goldenTestJSON
        exampleAddress2
        "test/golden/json/Address2"

golden_Address3 :: Property
golden_Address3 =
    goldenTestJSON
        exampleAddress3
        "test/golden/json/Address3"

golden_Address4 :: Property
golden_Address4 =
    goldenTestJSON
        exampleAddress4
        "test/golden/json/Address4"

roundTripAddressShow :: Property
roundTripAddressShow =
    eachOf 100 genAddress roundTripsAesonShow

roundTripAddressBuildable :: Property
roundTripAddressBuildable =
    eachOf 100 genAddress roundTripsAesonBuildable

--------------------------------------------------------------------------------
-- GenesisConfiguration
--------------------------------------------------------------------------------

golden_GenesisConfiguration_GCSpec0 :: Property
golden_GenesisConfiguration_GCSpec0 =
    goldenTestJSON
        exampleGenesisConfiguration_GCSpec0
            "test/golden/json/GenesisConfiguration_GCSpec0"

-- Test only decoding (for ensuring backwards compatibility with
-- old GenesisConfiguration format).
golden_GenesisConfiguration_GCSpec0Dec :: Property
golden_GenesisConfiguration_GCSpec0Dec =
    goldenTestJSONDec exampleGenesisConfiguration_GCSpec0
        "test/golden/json/GenesisConfiguration_GCSpec0_NoNetworkMagic"

golden_GenesisConfiguration_GCSpec1Dec :: Property
golden_GenesisConfiguration_GCSpec1Dec =
    goldenTestJSONDec exampleGenesisConfiguration_GCSpec1
        "test/golden/json/GenesisConfiguration_GCSpec1_NoNetworkMagic"

golden_GenesisConfiguration_GCSpec2Dec :: Property
golden_GenesisConfiguration_GCSpec2Dec =
    goldenTestJSONDec exampleGenesisConfiguration_GCSpec2
        "test/golden/json/GenesisConfiguration_GCSpec2_NoNetworkMagic"

golden_GenesisConfiguration_GCSrc :: Property
golden_GenesisConfiguration_GCSrc =
    goldenTestJSON
        exampleGenesisConfiguration_GCSrc
            "test/golden/GenesisConfiguration_GCSrc"

roundTripGenesisConfiguration :: Property
roundTripGenesisConfiguration =
    eachOf 100 (feedPM genGenesisConfiguration) roundTripsAesonShow

--------------------------------------------------------------------------------
-- GenesisData
--------------------------------------------------------------------------------

-- TODO @intricate: goldenTestCanonicalJSON
-- golden_GenesisData :: Property
-- golden_GenesisData =
--     goldenTestCanonicalJSON
--         exampleGenesisData
--             "test/golden/GenesisData"

-- TODO @intricate: genGenesisData
-- roundTripGenesisData :: Property
-- roundTripGenesisData = undefined
--     eachOf 100 (feedPM genGenesisData) roundTripsAesonShow

--------------------------------------------------------------------------------
-- GenesisAvvmBalances
--------------------------------------------------------------------------------

roundTripGenesisAvvmBalances :: Property
roundTripGenesisAvvmBalances =
     eachOf 100 genGenesisAvvmBalances roundTripsAesonShow

--------------------------------------------------------------------------------
-- RedeemPublicKey
--------------------------------------------------------------------------------

roundTripRedeemPublicKey :: Property
roundTripRedeemPublicKey = eachOf 1000 genRedeemPublicKey roundTripsAesonShow

--------------------------------------------------------------------------------
-- Coin
--------------------------------------------------------------------------------

roundTripCoin :: Property
roundTripCoin = eachOf 1000 genCoin roundTripsAesonBuildable

--------------------------------------------------------------------------------
-- SharedSeed
--------------------------------------------------------------------------------

roundTripSharedSeed :: Property
roundTripSharedSeed = eachOf 1000 genSharedSeed roundTripsAesonBuildable

--------------------------------------------------------------------------------
-- GenesisDelegation
--------------------------------------------------------------------------------

roundTripGenesisDelegation :: Property
roundTripGenesisDelegation =
    eachOf 100 (feedPM genGenesisDelegation) roundTripsAesonShow

--------------------------------------------------------------------------------
-- BlockVersionData
--------------------------------------------------------------------------------

roundTripBlockVersionData :: Property
roundTripBlockVersionData =
    eachOf 1000 genBlockVersionData roundTripsAesonBuildable

--------------------------------------------------------------------------------
-- Millisecond
--------------------------------------------------------------------------------

roundTripMillisecond :: Property
roundTripMillisecond = eachOf 1000 genMillisecond roundTripsAesonBuildable

--------------------------------------------------------------------------------
-- Byte
--------------------------------------------------------------------------------

roundTripByte :: Property
roundTripByte = eachOf 1000 genByte roundTripsAesonShow

--------------------------------------------------------------------------------
-- CoinPortion
--------------------------------------------------------------------------------

roundTripCoinPortion :: Property
roundTripCoinPortion = eachOf 1000 genCoinPortion roundTripsAesonBuildable

--------------------------------------------------------------------------------
-- FlatSlotId
--------------------------------------------------------------------------------

roundTripFlatSlotId :: Property
roundTripFlatSlotId = eachOf 1000 genFlatSlotId roundTripsAesonShow

--------------------------------------------------------------------------------
-- SoftforkRule
--------------------------------------------------------------------------------

roundTripSoftforkRule :: Property
roundTripSoftforkRule = eachOf 1000 genSoftforkRule roundTripsAesonBuildable

--------------------------------------------------------------------------------
-- TxFeePolicy
--------------------------------------------------------------------------------

roundTripTxFeePolicy :: Property
roundTripTxFeePolicy = eachOf 1000 genTxFeePolicy roundTripsAesonBuildable

--------------------------------------------------------------------------------
-- EpochIndex
--------------------------------------------------------------------------------

roundTripEpochIndex :: Property
roundTripEpochIndex = eachOf 1000 genEpochIndex roundTripsAesonBuildable


--------------------------------------------------------------------------------
-- ProtocolConstants
--------------------------------------------------------------------------------

roundTripProtocolConstants :: Property
roundTripProtocolConstants =
    eachOf 1000 genGenesisProtocolConstants roundTripsAesonShow

--------------------------------------------------------------------------------
-- GenesisInitializer
--------------------------------------------------------------------------------

roundTripGenesisInitializer :: Property
roundTripGenesisInitializer =
    eachOf 1000 genGenesisInitializer roundTripsAesonShow

--------------------------------------------------------------------------------
-- InvReqDataFlowLog
--------------------------------------------------------------------------------

golden_InvReqDataFlowLog_InvReqAccepted :: Property
golden_InvReqDataFlowLog_InvReqAccepted =
    goldenTestJSON
        (InvReqAccepted 1 2 3 4)
            "test/golden/InvReqDataFlowLog_InvReqAccepted"

golden_InvReqDataFlowLog_InvReqRejected :: Property
golden_InvReqDataFlowLog_InvReqRejected =
    goldenTestJSON
        (InvReqRejected 1 2)
            "test/golden/InvReqDataFlowLog_InvReqRejected"

golden_InvReqDataFlowLog_InvReqException :: Property
golden_InvReqDataFlowLog_InvReqException =
    goldenTestJSON
        (InvReqException "test")
            "test/golden/InvReqDataFlowLog_InvReqException"

roundTripInvReqDataFlowLog :: Property
roundTripInvReqDataFlowLog =
    eachOf 1000 genInvReqDataFlowLog roundTripsAesonShow

tests :: IO Bool
tests = (&&) <$> H.checkSequential $$discoverGolden
             <*> H.checkParallel $$discoverRoundTrip
