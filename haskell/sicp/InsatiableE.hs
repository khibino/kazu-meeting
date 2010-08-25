module InsatiableE where

{-

問題 2.74

-}

import Data.List
import Data.Maybe

type Name   = String
type Salary = Int
type Address = String

class Record record where
  getName    :: record -> Name
  getSalary  :: record -> Salary
  getAddress :: record -> Address

class OfficeFile office where
  getRecords :: Record r => office -> [r]
  
getRecord :: (Record r, OfficeFile o) => o -> Name -> Maybe r
getRecord office name = find ((== name) . getName) (getRecords office)

findEmployeeRecode :: (Record r, OfficeFile o) => [o] -> Name -> [r]
findEmployeeRecode offices name =
  mapMaybe (`getRecord` name) offices
