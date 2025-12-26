-- Main.hs
-- Build: ghc -O2 Main.hs -o sky
--
-- Usage:
--   ./sky                 (uses current local date+time)
--   ./sky --time 21:30    (uses today's date, local civil time)
--   ./sky --lat 65.01 --time 18.5 --width 100 --height 35
--
-- Notes:
--   * --lat is observer latitude in degrees. Default: Oulu ~65.01
--   * Oulu longitude is fixed (25.47E) for LST computation.
--   * Renders a north-up hemispherical sky chart:
--       center = zenith, outer ellipse = horizon, Polaris near top.

module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Data.Char (chr, ord, toLower)
import Data.List (isPrefixOf, isInfixOf)
import Text.Read (readMaybe)
import Data.Array.IO
import Control.Monad (forM_, when)
import Data.Time
  ( getZonedTime, zonedTimeToLocalTime, zonedTimeZone
  , LocalTime(..), TimeOfDay(..), ZonedTime(..)
  , localDay, localTimeOfDay, localTimeToUTC
  , UTCTime(..), utctDay, utctDayTime
  )
import Data.Time.Calendar (toModifiedJulianDay)

--------------------------------------------------------------------------------
-- Data model

data Constellation
  = UrsaMajor
  | UrsaMinor
  | Cassiopeia
  | Gemini
  | Orion
  | Taurus
  | Leo
  | Cygnus
  | Lyra
  | Aquila
  | Andromeda
  | Perseus
  | Auriga
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Star = Star
  { stName          :: String
  , stConstellation :: Constellation
  , stRaHours       :: Double
  , stDecDeg        :: Double
  , stMagV          :: Double
  , stDistanceLy    :: Maybe Double
  , stKind          :: String
  , stColorHint     :: String
  , stSizeHint      :: String
  , stLuminosity    :: Maybe Double
  } deriving (Show)

-- Major northern-hemisphere constellations with enough stars to draw stick figures.
-- (Not a full catalog; it includes all named endpoints used by the edge lists.)
stars :: [Star]
stars =
  [ -- Ursa Minor (Little Dipper) - 7 stars
    Star "Polaris"  UrsaMinor  2.5303  89.2641  1.98 (Just 433) "Cepheid supergiant system" "yellow-white" "large" Nothing
  , Star "Yildun"   UrsaMinor 17.5369  86.5864  4.35 Nothing "A star" "white" "main-seq" Nothing
  , Star "EpsUMi"   UrsaMinor 16.7662  82.0372  4.21 Nothing "G star" "yellow" "main-seq" Nothing
  , Star "ZetaUMi"  UrsaMinor 15.7346  77.7944  4.32 Nothing "A star" "white" "main-seq" Nothing
  , Star "Kochab"   UrsaMinor 14.8451  74.1556  2.07 Nothing "K giant" "orange" "giant" Nothing
  , Star "Pherkad"  UrsaMinor 15.3455  71.8340  3.00 Nothing "A giant" "white" "giant" Nothing
  , Star "EtaUMi"   UrsaMinor 16.2918  75.7547  4.95 (Just 98) "F star" "white-yellow" "main-seq" Nothing  -- HIP 79822

    -- Ursa Major (Big Dipper asterism)
  , Star "Dubhe"   UrsaMajor 11.0621  61.7509  1.79 Nothing "K giant" "orange" "giant" Nothing
  , Star "Merak"   UrsaMajor 11.0307  56.3824  2.37 Nothing "A star"  "white"  "main-seq" Nothing
  , Star "Phecda"  UrsaMajor 11.8972  53.6948  2.44 Nothing "A star"  "white"  "main-seq" Nothing
  , Star "Megrez"  UrsaMajor 12.2571  57.0326  3.31 Nothing "A star"  "white"  "main-seq" Nothing
  , Star "Alioth"  UrsaMajor 12.9005  55.9598  1.77 Nothing "A star"  "white"  "main-seq" Nothing
  , Star "Mizar"   UrsaMajor 13.3988  54.9254  2.04 Nothing "multiple system" "white" "main-seq" Nothing
  , Star "Alkaid"  UrsaMajor 13.7922  49.3131  1.86 Nothing "B star"  "blue-white" "main-seq" Nothing

    -- Cassiopeia “W”
  , Star "Caph"      Cassiopeia  0.1529  59.1498  2.28 Nothing "F star" "white-yellow" "subgiant" Nothing
  , Star "Schedar"   Cassiopeia  0.6751  56.5373  2.24 Nothing "K giant" "orange" "giant" Nothing
  , Star "GammaCas"  Cassiopeia  0.9451  60.7167  2.47 Nothing "Be star" "blue" "main-seq" Nothing
  , Star "Ruchbah"   Cassiopeia  1.4303  60.2353  2.68 Nothing "A star" "white" "main-seq" Nothing
  , Star "Segin"     Cassiopeia  1.9066  63.6701  3.37 Nothing "B star" "blue-white" "main-seq" Nothing

    -- Gemini
  , Star "Castor" Gemini  7.576666666666666   31.888638888888888  1.58 Nothing "multiple system" "blue-white" "main-seq" Nothing
  , Star "Pollux" Gemini  7.7553777777777775  28.026305555555556  1.16 (Just 34) "K giant" "orange" "giant" Nothing
  , Star "Wasat"  Gemini  7.3354              21.9822             3.53 Nothing "F star" "white-yellow" "main-seq" Nothing
  , Star "Alhena" Gemini  6.6285              16.3992             1.93 Nothing "A star" "white" "main-seq" Nothing

    -- Orion
  , Star "Betelgeuse" Orion  5.919444444444445   7.406666666666667   0.50 Nothing "M supergiant (variable)" "red" "supergiant" Nothing
  , Star "Bellatrix"  Orion  5.41885             6.3497              1.64 Nothing "B giant" "blue-white" "giant" Nothing
  , Star "Meissa"     Orion  5.5856              9.9342              3.39 Nothing "O/B system" "blue-white" "main-seq" Nothing
  , Star "Mintaka"    Orion  5.5334             (-0.2989)            2.23 Nothing "O/B multiple" "blue-white" "main-seq" Nothing
  , Star "Alnilam"    Orion  5.6036             (-1.2019)            1.69 Nothing "B supergiant" "blue-white" "supergiant" Nothing
  , Star "Alnitak"    Orion  5.6793             (-1.9428)            1.74 Nothing "O/B multiple" "blue-white" "main-seq" Nothing
  , Star "Saiph"      Orion  5.7959             (-9.6697)            2.06 Nothing "B supergiant" "blue-white" "supergiant" Nothing
  , Star "Rigel"      Orion  5.242222222222222  (-8.201666666666666) 0.12 Nothing "B supergiant" "blue-white" "supergiant" Nothing

    -- Taurus
  , Star "Aldebaran" Taurus  4.598675            16.509302777777776  0.85 Nothing "K giant" "orange" "giant" Nothing
  , Star "Elnath"    Taurus  5.438194444444445   28.607861111111113  1.65 Nothing "B giant" "blue" "giant" Nothing
  , Star "ZetaTau"   Taurus  5.6274              21.1425             2.97 Nothing "B star" "blue-white" "main-seq" Nothing

    -- Leo
  , Star "Regulus"  Leo 10.139444444444443  11.966944444444444  1.36 Nothing "multiple system" "blue-white" "main-seq" Nothing
  , Star "Algieba"  Leo 10.3329             19.8417             2.01 Nothing "K giant binary" "yellow-orange" "giant" Nothing
  , Star "Zosma"    Leo 11.2351             20.5236             2.56 Nothing "A star" "white" "main-seq" Nothing
  , Star "Denebola" Leo 11.817499999999999  14.571944444444444  2.14 Nothing "A star" "white" "main-seq" Nothing
  , Star "Chertan"  Leo 11.2373             15.4294             3.34 Nothing "A star" "white" "main-seq" Nothing

    -- Cygnus (Northern Cross)
  , Star "Deneb"   Cygnus 20.690530555555554  45.28033333333333  1.25 Nothing "A supergiant" "blue-white" "supergiant" Nothing
  , Star "Sadr"    Cygnus 20.3705             40.2567            2.23 Nothing "F giant" "white-yellow" "giant" Nothing
  , Star "Albireo" Cygnus 19.512025           27.959694444444445 3.05 Nothing "double star (optical)" "orange" "giant" Nothing
  , Star "Gienah"  Cygnus 20.7702             33.9703            2.48 Nothing "K giant" "orange" "giant" Nothing
  , Star "Rukh"    Cygnus 19.7496             45.1308            2.87 Nothing "B star" "blue-white" "main-seq" Nothing

    -- Lyra
  , Star "Vega"     Lyra 18.615555555555556  38.78361111111111  0.03 Nothing "A star" "white-blue" "main-seq" Nothing
  , Star "Sheliak"  Lyra 18.8347             33.3628            3.52 Nothing "binary system" "blue-white" "main-seq" Nothing
  , Star "Sulafat"  Lyra 18.9824             32.6894            3.25 Nothing "B giant" "blue" "giant" Nothing
  , Star "DeltaLyr" Lyra 18.9120             36.8986            4.22 Nothing "giant" "white" "giant" Nothing
  , Star "ZetaLyr"  Lyra 18.7461             37.6050            4.36 Nothing "A star" "white" "main-seq" Nothing

    -- Aquila
  , Star "Altair"  Aquila 19.84611111111111  8.868055555555555  0.76 Nothing "A star" "white" "main-seq" Nothing
  , Star "Tarazed" Aquila 19.7710            10.6131            2.72 Nothing "K giant" "orange" "giant" Nothing
  , Star "Alshain" Aquila 19.9219             6.4067            3.71 Nothing "G star" "yellow" "main-seq" Nothing

    -- Andromeda
  , Star "Alpheratz" Andromeda 0.1398  29.0904  2.06 Nothing "B star" "blue-white" "main-seq" Nothing
  , Star "Mirach"    Andromeda 1.1621666666666666  35.62083333333334  2.07 Nothing "M giant" "red" "giant" Nothing
  , Star "Almach"    Andromeda 2.0649777777777776  42.329861111111114 2.10 Nothing "multiple system" "orange" "giant" Nothing

    -- Perseus
  , Star "Mirfak" Perseus 3.405375           49.86125  1.79 Nothing "F supergiant" "white-yellow" "supergiant" Nothing
  , Star "Algol"  Perseus 3.136147222222222  40.95563888888889  2.09 Nothing "eclipsing binary" "blue-white" "main-seq" Nothing
  , Star "Atik"   Perseus 3.9019             31.8836   2.85 Nothing "B star" "blue-white" "main-seq" Nothing
  , Star "EpsPer" Perseus 3.9642             40.0103   2.89 Nothing "B star" "blue-white" "main-seq" Nothing

    -- Auriga
  , Star "Capella"     Auriga 5.278138888888889  45.999027777777776  0.08 (Just 43) "binary giant system" "yellow" "giant" Nothing
  , Star "Menkalinan"  Auriga 5.9921             44.9472            1.90 Nothing "A star binary" "white" "main-seq" Nothing
  , Star "Mahasim"     Auriga 5.9954             37.2125            2.65 Nothing "A star" "white" "main-seq" Nothing
  , Star "Almaaz"      Auriga 5.0328             43.8233            2.99 Nothing "eclipsing binary" "yellow" "giant" Nothing
  ]

--------------------------------------------------------------------------------
-- Constellation "stick figure" edges
-- Each constellation has its own edge list; all are concatenated into asterismEdges.

constellationAsterisms :: [(Constellation, [(String, String)])]
constellationAsterisms =
  [ (UrsaMajor,
      [ ("Merak","Dubhe"), ("Dubhe","Megrez"), ("Megrez","Phecda"), ("Phecda","Merak")
      , ("Megrez","Alioth"), ("Alioth","Mizar"), ("Mizar","Alkaid")
      ])
  , (UrsaMinor,
      [ ("Polaris","Yildun")
      , ("Yildun","EpsUMi")
      , ("EpsUMi","ZetaUMi")
      -- bowl
      , ("ZetaUMi","Kochab")
      , ("Kochab","Pherkad")
      , ("Pherkad","EtaUMi")
      , ("EtaUMi","ZetaUMi")
      ])
  , (Cassiopeia,
      [ ("Caph","Schedar"), ("Schedar","GammaCas"), ("GammaCas","Ruchbah"), ("Ruchbah","Segin")
      ])
  , (Gemini,
      [ ("Castor","Pollux"), ("Castor","Wasat"), ("Pollux","Wasat"), ("Wasat","Alhena"), ("Pollux","Alhena")
      ])
  , (Orion,
      [ ("Betelgeuse","Bellatrix")
      , ("Bellatrix","Mintaka"), ("Mintaka","Alnilam"), ("Alnilam","Alnitak")
      , ("Alnitak","Saiph"), ("Saiph","Rigel"), ("Rigel","Bellatrix")
      , ("Betelgeuse","Alnitak")
      , ("Meissa","Bellatrix"), ("Meissa","Betelgeuse")
      ])
  , (Taurus,
      [ ("Aldebaran","Elnath"), ("Aldebaran","ZetaTau"), ("ZetaTau","Elnath")
      ])
  , (Leo,
      [ ("Regulus","Algieba"), ("Algieba","Zosma"), ("Zosma","Denebola")
      , ("Regulus","Chertan"), ("Chertan","Denebola")
      ])
  , (Cygnus,
      [ ("Deneb","Sadr"), ("Sadr","Albireo"), ("Sadr","Gienah"), ("Sadr","Rukh")
      ])
  , (Lyra,
      [ ("Sheliak","Sulafat"), ("Sulafat","DeltaLyr"), ("DeltaLyr","ZetaLyr"), ("ZetaLyr","Sheliak")
      , ("Vega","ZetaLyr"), ("Vega","Sheliak")
      ])
  , (Aquila,
      [ ("Altair","Tarazed"), ("Altair","Alshain")
      ])
  , (Andromeda,
      [ ("Alpheratz","Mirach"), ("Mirach","Almach")
      ])
  , (Perseus,
      [ ("Algol","Mirfak"), ("Mirfak","EpsPer"), ("Mirfak","Atik"), ("Atik","EpsPer")
      ])
  , (Auriga,
      [ ("Capella","Menkalinan"), ("Menkalinan","Mahasim"), ("Mahasim","Capella")
      , ("Capella","Almaaz"), ("Almaaz","Menkalinan")
      ])
  ]

asterismEdges :: [(String, String)]
asterismEdges = concatMap snd constellationAsterisms

--------------------------------------------------------------------------------
-- Math / astronomy helpers

deg2rad :: Double -> Double
deg2rad d = d * pi / 180.0

rad2deg :: Double -> Double
rad2deg r = r * 180.0 / pi

wrap2pi :: Double -> Double
wrap2pi x =
  let twoPi = 2*pi
      y = x - fromIntegral (floor (x / twoPi)) * twoPi
  in if y < 0 then y + twoPi else y

wrap24 :: Double -> Double
wrap24 h =
  let y = h - 24.0 * fromIntegral (floor (h / 24.0))
  in if y < 0 then y + 24.0 else y

clamp :: Double -> Double -> Double -> Double
clamp lo hi v = max lo (min hi v)

-- Compute Julian Date from UTCTime using MJD (0h) + fractional day
utcToJulianDate :: UTCTime -> Double
utcToJulianDate utc =
  let day = utctDay utc
      mjd0 = fromIntegral (toModifiedJulianDay day) :: Double
      sec  = realToFrac (utctDayTime utc) :: Double
      frac = sec / 86400.0
  in mjd0 + 2400000.5 + frac

-- GMST (hours) from Julian Date using a standard approximation
gmstHoursFromJD :: Double -> Double
gmstHoursFromJD jd =
  let d = jd - 2451545.0
      t = d / 36525.0
      gmstDeg =
        280.46061837
        + 360.98564736629 * d
        + 0.000387933 * t * t
        - (t * t * t) / 38710000.0
      gmstDegNorm = gmstDeg - 360.0 * fromIntegral (floor (gmstDeg / 360.0))
      h = gmstDegNorm / 15.0
  in wrap24 h

-- Fixed Oulu longitude (east positive)
ouluLonDeg :: Double
ouluLonDeg = 25.47

-- LST (hours) from UTCTime at Oulu longitude
lstHoursFromUTC :: UTCTime -> Double
lstHoursFromUTC utc =
  let jd = utcToJulianDate utc
      gmstH = gmstHoursFromJD jd
      lstH = gmstH + ouluLonDeg / 15.0
  in wrap24 lstH

-- RA/Dec -> Alt/Az for latitude and Local Sidereal Time.
-- Azimuth: 0°=North, 90°=East, increasing eastward.
radecToAltAz :: Double -> Double -> Double -> Double -> (Double, Double)
radecToAltAz latDeg lstHours raHours decDeg =
  let lat = deg2rad latDeg
      dec = deg2rad decDeg
      ha  = deg2rad ((lstHours - raHours) * 15.0)

      sinAlt = sin dec * sin lat + cos dec * cos lat * cos ha
      alt    = asin (clamp (-1) 1 sinAlt)
      cosAlt = cos alt

      sinAz = if abs cosAlt < 1e-12 then 0 else (-cos dec * sin ha) / cosAlt
      cosAz = if abs cosAlt < 1e-12 then 1 else (sin dec - sin alt * sin lat) / (cosAlt * cos lat)

      az = wrap2pi (atan2 sinAz cosAz)
  in (rad2deg alt, rad2deg az)

--------------------------------------------------------------------------------
-- Rendering (braille grid + per-cell override), no Data.Bits

dotBitIndex :: Int -> Int -> Int
dotBitIndex col row =
  case (col,row) of
    (0,0) -> 0
    (0,1) -> 1
    (0,2) -> 2
    (1,0) -> 3
    (1,1) -> 4
    (1,2) -> 5
    (0,3) -> 6
    (1,3) -> 7
    _     -> 0

brailleChar :: Int -> Char
brailleChar mask = chr (0x2800 + mask)

setBitArith :: Int -> Int -> Int
setBitArith mask bitIndex =
  let bitVal = (2 :: Int) ^ bitIndex
      alreadySet = (mask `div` bitVal) `mod` 2 == 1
  in if alreadySet then mask else mask + bitVal

setDot :: IOUArray (Int,Int) Int -> Int -> Int -> Int -> Int -> IO ()
setDot masks cellX cellY col row = do
  let bitIdx = dotBitIndex col row
  cur <- readArray masks (cellX, cellY)
  writeArray masks (cellX, cellY) (setBitArith cur bitIdx)

rMaxX :: Int -> Double
rMaxX subW = fromIntegral subW / 2.0 - 2.0

rMaxY :: Int -> Double
rMaxY subH = fromIntegral subH / 2.0 - 2.0

ellipseNormDist :: Int -> Int -> Double -> Double -> Double
ellipseNormDist subW subH x y =
  let cx = fromIntegral subW / 2.0
      cy = fromIntegral subH / 2.0
      rx = rMaxX subW
      ry = rMaxY subH
      dx = x - cx
      dy = y - cy
  in sqrt ((dx*dx)/(rx*rx) + (dy*dy)/(ry*ry))

insideDome :: Int -> Int -> Int -> Int -> Bool
insideDome subW subH x y =
  ellipseNormDist subW subH (fromIntegral x) (fromIntegral y) <= 1.0

nearBoundaryCell :: Int -> Int -> Int -> Int -> Bool
nearBoundaryCell subW subH cellX cellY =
  let x = fromIntegral (cellX*2) + 0.5
      y = fromIntegral (cellY*4) + 1.5
      d = ellipseNormDist subW subH x y
  in abs (d - 1.0) <= 0.03

projectAltAzToDot :: Int -> Int -> Double -> Double -> Maybe (Int, Int)
projectAltAzToDot subW subH altDeg azDeg =
  if altDeg <= 0 then Nothing
  else
    let rx = rMaxX subW
        ry = rMaxY subH
        zenithAngle = 90.0 - altDeg
        rFrac = zenithAngle / 90.0
        az = deg2rad azDeg
        cx = fromIntegral subW / 2.0
        cy = fromIntegral subH / 2.0
        x = cx + (rFrac * rx) * sin az
        y = cy + (rFrac * ry) * cos az
    in Just (round x, round y)

--------------------------------------------------------------------------------
-- ANSI color/intensity rendering

data Intensity = IDim | INormal | IBright deriving (Eq, Show)

intensityForMag :: Double -> Intensity
intensityForMag m
  | m <= 1.5   = IBright
  | m <= 2.8   = INormal
  | otherwise  = IDim

-- Return ANSI base color code (30..37).
ansiColorFromHint :: String -> Int
ansiColorFromHint hint =
  let h = map toLower hint
  in if "blue" `isInfixOf` h
        then 34   -- blue
     else if "orange" `isInfixOf` h || "red" `isInfixOf` h
        then 31   -- red (used for orange/red)
     else if "yellow" `isInfixOf` h
        then 33   -- yellow
     else 37       -- white (default)

-- styleCode = color*10 + intensityTag; 0 means "no style"
encodeStyle :: Int -> Intensity -> Int
encodeStyle color inten =
  color * 10 + case inten of
    IDim    -> 0
    INormal -> 1
    IBright -> 2

decodeStyle :: Int -> (Int, Intensity)
decodeStyle st =
  let color = st `div` 10
      tag   = st `mod` 10
      inten = case tag of
        0 -> IDim
        1 -> INormal
        _ -> IBright
  in (color, inten)

ansiReset :: String
ansiReset = "\x1b[0m"

ansiStart :: Int -> String
ansiStart st =
  let (color, inten) = decodeStyle st
      attr = case inten of
        IDim    -> "2"   -- faint
        INormal -> "22"  -- normal intensity (clears bold/faint)
        IBright -> "1"   -- bold/bright
  in "\x1b[" ++ attr ++ ";" ++ show color ++ "m"

lineStyle :: Int
lineStyle = encodeStyle 37 IDim    -- dim white/gray for sticks

boundaryStyle :: Int
boundaryStyle = encodeStyle 37 IDim

--------------------------------------------------------------------------------
-- Bresenham for asterism “sticks”

bresenham :: (Int,Int) -> (Int,Int) -> [(Int,Int)]
bresenham (x0,y0) (x1,y1) =
  let dx = abs (x1 - x0)
      sx = if x0 < x1 then 1 else -1
      dy = - abs (y1 - y0)
      sy = if y0 < y1 then 1 else -1
      go x y err acc =
        let acc' = (x,y):acc
        in if x == x1 && y == y1
           then reverse acc'
           else
             let e2 = 2*err
                 (x', err1) = if e2 >= dy then (x + sx, err + dy) else (x, err)
                 (y', err2) = if e2 <= dx then (y + sy, err1 + dx) else (y, err1)
             in go x' y' err2 acc'
  in go x0 y0 (dx + dy) []

--------------------------------------------------------------------------------
-- CLI parsing

data Opts = Opts
  { optLatDeg    :: Double
  , optTimeArg   :: Maybe TimeOfDay  -- civil local time-of-day
  , optWidth     :: Int
  , optHeight    :: Int
  } deriving (Show)

defaultOpts :: Opts
defaultOpts = Opts
  { optLatDeg   = 65.01
  , optTimeArg  = Nothing
  , optWidth    = 80
  , optHeight   = 40
  }

usage :: String
usage = unlines
  [ "sky - terminal star chart (north-up dome projection)"
  , ""
  , "Usage:"
  , "  sky [--time HH:MM|H.dec] [--lat DEG] [--width N] [--height N]"
  , "  sky LAT HH:MM"
  , ""
  , "Behavior:"
  , "  * Uses system local date (today)."
  , "  * --time selects civil local time-of-day for that date."
  , "  * Computes LST internally using fixed Oulu longitude (25.47E)."
  ]

parseTimeOfDay :: String -> Maybe TimeOfDay
parseTimeOfDay s =
  case break (==':') s of
    (hh, ':' : mm) ->
      case (readMaybe hh :: Maybe Int, readMaybe mm :: Maybe Int) of
        (Just h, Just m) | h >= 0 && h < 24 && m >= 0 && m < 60 ->
          Just (TimeOfDay h m 0)
        _ -> Nothing
    _ ->
      case (readMaybe s :: Maybe Double) of
        Just x | x >= 0 && x < 24 ->
          let h = floor x
              frac = x - fromIntegral h
              m = floor (frac * 60.0 + 0.5)
              (h', m') = if m >= 60 then (h+1, m-60) else (h, m)
              h'' = if h' >= 24 then h' - 24 else h'
          in Just (TimeOfDay h'' m' 0)
        _ -> Nothing

parseArgs :: [String] -> Either String Opts
parseArgs args0 =
  let go opts args =
        case args of
          [] -> Right opts
          ("--help":_) -> Left usage
          (a:rest)
            | "--lat=" `isPrefixOf` a ->
                case readMaybe (drop (length "--lat=") a) of
                  Just v  -> go opts{optLatDeg=v} rest
                  Nothing -> Left ("Bad --lat value: " ++ a)
            | "--time=" `isPrefixOf` a ->
                case parseTimeOfDay (drop (length "--time=") a) of
                  Just t  -> go opts{optTimeArg=Just t} rest
                  Nothing -> Left ("Bad --time value: " ++ a)
            | "--time" == a ->
                case rest of
                  (t:rest') ->
                    case parseTimeOfDay t of
                      Just tod -> go opts{optTimeArg=Just tod} rest'
                      Nothing  -> Left ("Bad --time value: " ++ t)
                  _ -> Left "Missing value after --time"
            | "--lat" == a ->
                case rest of
                  (v:rest') ->
                    case readMaybe v of
                      Just lv -> go opts{optLatDeg=lv} rest'
                      Nothing -> Left ("Bad latitude value: " ++ v)
                  _ -> Left "Missing value after --lat"
            | "--width" == a ->
                case rest of
                  (v:rest') ->
                    case readMaybe v of
                      Just w | w > 10 -> go opts{optWidth=w} rest'
                      _ -> Left ("Bad --width: " ++ v)
                  _ -> Left "Missing value after --width"
            | "--height" == a ->
                case rest of
                  (v:rest') ->
                    case readMaybe v of
                      Just h | h > 10 -> go opts{optHeight=h} rest'
                      _ -> Left ("Bad --height: " ++ v)
                  _ -> Left "Missing value after --height"
            | otherwise ->
                -- positional: sky LAT TIME
                case (readMaybe a :: Maybe Double, rest) of
                  (Just lat, t:rest') ->
                    case parseTimeOfDay t of
                      Just tod -> go opts{optLatDeg=lat, optTimeArg=Just tod} rest'
                      Nothing  -> Left ("Bad time value: " ++ t)
                  _ -> Left ("Unrecognized argument: " ++ a ++ "\n\n" ++ usage)
  in go defaultOpts args0

--------------------------------------------------------------------------------
-- Plot orchestration

-- Render *every* star as a symbol (dim stars get a dim symbol).
symbolForMag :: Double -> Char
symbolForMag m
  | m <= 1.0   = '✶'
  | m <= 2.0   = '✦'
  | m <= 3.0   = '✧'
  | otherwise  = '•'   -- always show named stars, even if faint

joinMaybe :: Maybe (Maybe a) -> Maybe a
joinMaybe mma = case mma of
  Nothing -> Nothing
  Just ma -> ma

computeStarDot :: Double -> Double -> Int -> Int -> Star -> (Star, Maybe (Int,Int))
computeStarDot latDeg lstH subW subH s =
  let (alt, az) = radecToAltAz latDeg lstH (stRaHours s) (stDecDeg s)
  in (s, projectAltAzToDot subW subH alt az)

drawFaintLine :: Int -> Int -> IOUArray (Int,Int) Int -> (Int,Int) -> (Int,Int) -> IO ()
drawFaintLine subW subH masks p0 p1 =
  forM_ (bresenham p0 p1) $ \(x,y) ->
    when (x >= 0 && x < subW && y >= 0 && y < subH && insideDome subW subH x y) $ do
      let cellX = x `div` 2
          cellY = y `div` 4
          col   = x `mod` 2
          row   = y `mod` 4
      setDot masks cellX cellY col row

buildCell :: Int -> Int
          -> IOUArray (Int,Int) Int   -- masks (lines)
          -> IOUArray (Int,Int) Int   -- overrides (stars)
          -> IOUArray (Int,Int) Int   -- overridesStyle
          -> Int -> Int
          -> IO (Char, Int)
buildCell subW subH masks overrides overridesStyle y x = do
  let inside =
        let cx = x*2 + 1
            cy = y*4 + 2
        in insideDome subW subH cx cy

  if not inside
    then pure (' ', 0)
    else do
      ov <- readArray overrides (x,y)
      if ov /= 0
        then do
          sty <- readArray overridesStyle (x,y)
          pure (chr ov, sty)
        else if nearBoundaryCell subW subH x y
          then pure ('·', boundaryStyle)
          else do
            m <- readArray masks (x,y)
            pure $ if m == 0 then (' ', 0) else (brailleChar m, lineStyle)

renderRowAnsi :: [(Char, Int)] -> String
renderRowAnsi cells = go 0 cells
  where
    go cur [] =
      if cur /= 0 then ansiReset else ""
    go cur ((ch, st):rest)
      | st == cur =
          ch : go cur rest
      | st == 0 =
          (if cur /= 0 then ansiReset else "") ++ [ch] ++ go 0 rest
      | cur == 0 =
          ansiStart st ++ [ch] ++ go st rest
      | otherwise =
          ansiReset ++ ansiStart st ++ [ch] ++ go st rest

buildRow :: Int -> Int -> Int
         -> IOUArray (Int,Int) Int
         -> IOUArray (Int,Int) Int
         -> IOUArray (Int,Int) Int
         -> Int
         -> IO String
buildRow subW subH wChars masks overrides overridesStyle y = do
  cells <- mapM (buildCell subW subH masks overrides overridesStyle y) [0..wChars-1]
  pure (renderRowAnsi cells)

--------------------------------------------------------------------------------
-- Main

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left msg -> putStrLn msg >> exitFailure
    Right opts -> do
      let lat = optLatDeg opts
      when (lat < 0) $ do
        putStrLn "Error: Southern hemisphere (lat < 0) is not supported yet."
        exitFailure

      -- Get today's local date from system clock, and timezone offset (assumed correct for Oulu system).
      zt <- getZonedTime
      let tz  = zonedTimeZone zt
          ltNow = zonedTimeToLocalTime zt
          dayToday = localDay ltNow
          todNow = localTimeOfDay ltNow
          tod = case optTimeArg opts of
                  Just t  -> t
                  Nothing -> todNow

          local = LocalTime dayToday tod
          utc   = localTimeToUTC tz local
          lstH  = lstHoursFromUTC utc

      let wChars = optWidth opts
          hChars = optHeight opts
          subW   = wChars * 2
          subH   = hChars * 4

      masks <- newArray ((0,0), (wChars-1, hChars-1)) 0
        :: IO (IOUArray (Int,Int) Int)

      overrides <- newArray ((0,0), (wChars-1, hChars-1)) (0 :: Int)
        :: IO (IOUArray (Int,Int) Int)

      overridesStyle <- newArray ((0,0), (wChars-1, hChars-1)) (0 :: Int)
        :: IO (IOUArray (Int,Int) Int)

      -- Tracks the best (brightest) star already placed in a cell, so multiple stars
      -- landing on the same cell choose the brightest one.
      bestStarMag <- newArray ((0,0), (wChars-1, hChars-1)) (1/0 :: Double) -- +Infinity
        :: IO (IOUArray (Int,Int) Double)

      let starDots = map (computeStarDot lat lstH subW subH) stars
          lookupDot nm = lookup nm [(stName s, md) | (s, md) <- starDots]

      -- Draw constellation sticks (braille). These will only show where there are no star overrides.
      forM_ asterismEdges $ \(a,b) ->
        case (joinMaybe (lookupDot a), joinMaybe (lookupDot b)) of
          (Just p0, Just p1) -> drawFaintLine subW subH masks p0 p1
          _ -> pure ()

      -- Place stars as per-cell override glyphs (always visible; dim stars get a dim glyph).
      forM_ starDots $ \(s, mdot) ->
        case mdot of
          Nothing -> pure ()
          Just (dx,dy) -> do
            let cellX = dx `div` 2
                cellY = dy `div` 4
            when (cellX >= 0 && cellX < wChars && cellY >= 0 && cellY < hChars) $ do
              cur <- readArray bestStarMag (cellX, cellY)
              let mag = stMagV s
              when (mag < cur) $ do
                writeArray bestStarMag (cellX, cellY) mag
                let sym   = symbolForMag mag
                    color = ansiColorFromHint (stColorHint s)
                    sty   = encodeStyle color (intensityForMag mag)
                writeArray overrides      (cellX, cellY) (ord sym)
                writeArray overridesStyle (cellX, cellY) sty

      forM_ [0..hChars-1] $ \y -> do
        row <- buildRow subW subH wChars masks overrides overridesStyle y
        putStrLn row

