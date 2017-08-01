module Horn.Types(
    someFunc
) where

import Record
import Record.Lens

type Person = [r| {
    name        :: String,
    birthday    :: { year :: Int, month :: Int, day :: Int }
} |]


getPersonBirthdayYear :: Person -> Int
getPersonBirthdayYear =
  view ([l|birthday.year|])

setPersonBirthdayYear :: Int -> Person -> Person
setPersonBirthdayYear =
  set [l|birthday.year|]

someFunc :: IO ()
someFunc = putStrLn "someFunc"
