module Main where

import Test.Hspec
import Test.HUnit

import Network.AWS.Route53.Types (changeResourceRecordSets, resourceRecordSet, rrsResourceRecords, rrsTTL, resourceRecord)
import Control.Lens ((&), (?~))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Network.AWS.Data.Text (ToText (toText))

import SetDynIp (getCurrentIpAddr, changeIpAddr)

main :: IO ()
main = hspec $ do
  describe "getCurrentIpAddr" $ do
    it "fetches the current IP address successfully" $ do
      -- Mock the AWS Route53 service to return a specific IP address
      let expectedIp = "192.0.2.0"
      actualIp <- getCurrentIpAddr "example.com"
      actualIp `shouldBe` expectedIp

    it "fails to fetch the current IP address due to an error" $ do
      -- Mock the AWS Route53 service to throw an error
      getCurrentIpAddr "example.com" `shouldThrow` anyException

  describe "changeIpAddr" $ do
    it "does not change the IP address when the current IP address is the same as the desired one" $ do
      -- Mock the AWS Route53 service to return the same IP address as the desired one
      let currentIp = "192.0.2.0"
      let desiredIp = "192.0.2.0"
      changeIpAddr "example.com" currentIp desiredIp `shouldReturn` ()

    it "changes the IP address when the current IP address is different from the desired one" $ do
      -- Mock the AWS Route53 service to return a different IP address from the desired one
      let currentIp = "192.0.2.0"
      let desiredIp = "203.0.113.0"
      changeIpAddr "example.com" currentIp desiredIp `shouldReturn` ()
