import           Common
import           Data.Maybe
--(Year Code + Month Code + Century Code + Date Number – Leap Year Code) mod 7
data Day = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

type Date = (Int,Month,Int,Int)

isLeapYear :: Date -> Bool
isLeapYear (m,d,h,y) = leapYear y

centuryCodes :: [(Int,Int)]
centuryCodes = [(19,0),(20,6)]

monthsCodes :: [(Month, Int)]
monthsCodes = [(Jan,0),(Feb,3),(Mar,3),(Apr,6),(May,1),(Jun,4),(Jul,6),(Aug,2),(Sep,5),(Oct,0),(Nov,3),(Dec,5)]


yearCode :: Int -> Int
yearCode n = (n + (div n 4)) `mod` 7

leapYear :: Int -> Bool
leapYear n
  | mod n 400 == 0 = True
  | mod n 100 == 0 = False
  | mod n 4 == 0 = True
  | otherwise = False

getDay :: Date -> Day
getDay (d,m,h,y) = toEnum ((yearCode' + monthCode + centuryCode + d - lyCode) `mod` 7) -- :: Day
  where yearCode' = yearCode y
        monthCode = getCode m monthsCodes
        centuryCode = getCode h centuryCodes
        lyCode = 1

getCode key dict = fromMaybe' (findKey key dict)

--toEnum 2 :: Day
--(Year Code + Month Code + Century Code + Date Number – Leap Year Code) mod 7
