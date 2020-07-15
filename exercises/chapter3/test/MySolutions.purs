module Test.MySolutions where

import Prelude

import Data.AddressBook (Entry, AddressBook)
import Data.List (filter, head, null, nubBy)
import Data.Maybe (Maybe)

matchByName :: String -> String -> Entry -> Boolean
matchByName firstName lastName entry =
  entry.firstName == firstName && entry.lastName == lastName

filterOne :: (Entry -> Boolean) -> AddressBook -> Maybe Entry
filterOne criterion = head <<< (filter criterion)

matchByStreet :: String -> Entry -> Boolean
matchByStreet street entry = entry.address.street == street

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet = filterOne <<< matchByStreet

isInBook :: String -> String -> AddressBook -> Boolean
isInBook firstName lastName book =
  not null remaining
  where
    remaining = filter (matchByName firstName lastName) book

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubBy sameName
  where sameName a b = a.firstName == b.firstName && a.lastName == b.lastName
