module Test.MySolutions where

import Data.AddressBook
import Data.Maybe
import Prelude

import Data.Function (on)
import Data.List (List(..), filter, head, nubByEq, null)

-- Note to reader: Add your solutions to this file
findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street = head <<< filter filterEntry
    where
        filterEntry :: Entry -> Boolean
        filterEntry x = x.address.street == street

findEntryByStreet' :: String -> AddressBook -> Maybe Entry
findEntryByStreet' street = head <<< filter (_.address.street >>> (==) street) -- pointfree

isInBook :: String -> String -> AddressBook -> Boolean
isInBook firstName lastName = not null <<< filter  (\x -> x.firstName == firstName && x.lastName == lastName)

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubByEq (eq `on` _.firstName && eq `on` _.lastName)