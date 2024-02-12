module Test.MySolutions where

import Prelude
import Data.AddressBook
import Data.Maybe
import Data.List (List(..), filter, head)
-- Note to reader: Add your solutions to this file
findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street = head <<< filter filterEntry
    where
        filterEntry :: Entry -> Boolean
        filterEntry x = x.address.street == street

findEntryByStreet' :: String -> AddressBook -> Maybe Entry
findEntryByStreet' street = head <<< filter (_.address.street >>> (==) street) -- pointfree
