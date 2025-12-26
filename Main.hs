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
import Data.Char (chr, ord)
import Data.List (isPrefixOf)
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

data Star = Star
  { stName       :: String
  , stRaHours    :: Double
  , stDecDeg     :: Double
  , stMagV       :: Double
  , stDistanceLy :: Maybe Double
  , stKind       :: String
  , stColorHint  :: String
  , stSizeHint   :: String
  , stLuminosity :: Maybe Double
  } deriving (Show)

-- Minimal verification set: Polaris + Big Dipper + Cassiopeia.
stars :: [Star]
stars =
  [ Star "Polaris"  2.5303  89.2641  1.98 (Just 433) "Cepheid supergiant system" "yellow-white" "large" Nothing

  -- Big Dipper (Ursa Major asterism)
  , Star "Dubhe"   11.0621  61.7509  1.79 Nothing "K giant" "orange" "giant" Nothing
  , Star "Merak"   11.0307  56.3824  2.37 Nothing "A star"  "white"  "main-seq" Nothing
  , Star "Phecda"  11.8972  53.6948  2.44 Nothing "A star"  "white"  "main-seq" Nothing
  , Star "Megrez"  12.2571  57.0326  3.31 Nothing "A star"  "white"  "main-seq" Nothing
  , Star "Alioth"  12.9005  55.9598  1.77 Nothing "A star"  "white"  "main-seq" Nothing
  , Star "Mizar"   13.3988  54.9254  2.04 Nothing "multiple system" "white" "main-seq" Nothing
  , Star "Alkaid"  13.7922  49.3131  1.86 Nothing "B star"  "blue-white" "main-seq" Nothing

  -- Cassiopeia “W”
  , Star "Caph"      0.1529  59.1498  2.28 Nothing "F star" "white-yellow" "subgiant" Nothing
  , Star "Schedar"   0.6751  56.5373  2.24 Nothing "K giant" "orange" "giant" Nothing
  , Star "GammaCas"  0.9451  60.7167  2.47 Nothing "Be star" "blue" "main-seq" Nothing
  , Star "Ruchbah"   1.4303  60.2353  2.68 Nothing "A star" "white" "main-seq" Nothing
  , Star "Segin"     1.9066  63.6701  3.37 Nothing "B star" "blue-white" "main-seq" Nothing
  ]

asterismEdges :: [(String, String)]
asterismEdges =
  [ ("Merak","Dubhe"), ("Dubhe","Megrez"), ("Megrez","Phecda"), ("Phecda","Merak")
  , ("Megrez","Alioth"), ("Alioth","Mizar"), ("Mizar","Alkaid")
  , ("Caph","Schedar"), ("Schedar","GammaCas"), ("GammaCas","Ruchbah"), ("Ruchbah","Segin")
  ]

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
-- Rendering (braille grid + bright overrides), no Data.Bits

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

symbolForMag :: Double -> Maybe Char
symbolForMag m
  | m <= 1.0   = Just '✶'
  | m <= 2.0   = Just '✦'
  | m <= 3.0   = Just '✧'
  | otherwise  = Nothing

dotOffsetsForMag :: Double -> [(Int,Int)]
dotOffsetsForMag m
  | m <= 3.2   = [(0,0),(1,0),(0,1)]
  | m <= 4.0   = [(0,0),(1,0)]
  | otherwise  = [(0,0)]

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
          -> IOUArray (Int,Int) Int
          -> IOUArray (Int,Int) Int
          -> Int -> Int
          -> IO Char
buildCell subW subH masks overrides y x = do
  let inside =
        let cx = x*2 + 1
            cy = y*4 + 2
        in insideDome subW subH cx cy
  if not inside
    then pure ' '
    else if nearBoundaryCell subW subH x y
      then pure '·'
      else do
        ov <- readArray overrides (x,y)
        if ov /= 0
          then pure (chr ov)
          else do
            m <- readArray masks (x,y)
            pure $ if m == 0 then ' ' else brailleChar m

buildRow :: Int -> Int -> Int
         -> IOUArray (Int,Int) Int
         -> IOUArray (Int,Int) Int
         -> Int
         -> IO String
buildRow subW subH wChars masks overrides y =
  mapM (buildCell subW subH masks overrides y) [0..wChars-1]

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

      let starDots = map (computeStarDot lat lstH subW subH) stars
          lookupDot nm = lookup nm [(stName s, md) | (s, md) <- starDots]

      forM_ asterismEdges $ \(a,b) ->
        case (joinMaybe (lookupDot a), joinMaybe (lookupDot b)) of
          (Just p0, Just p1) -> drawFaintLine subW subH masks p0 p1
          _ -> pure ()

      forM_ starDots $ \(s, mdot) ->
        case mdot of
          Nothing -> pure ()
          Just (dx,dy) -> do
            let cellX = dx `div` 2
                cellY = dy `div` 4
            if cellX < 0 || cellX >= wChars || cellY < 0 || cellY >= hChars
              then pure ()
              else case symbolForMag (stMagV s) of
                Just sym -> writeArray overrides (cellX,cellY) (ord sym)
                Nothing  -> do
                  forM_ (dotOffsetsForMag (stMagV s)) $ \(ox,oy) -> do
                    let x' = dx + ox
                        y' = dy + oy
                    when (x' >= 0 && x' < subW && y' >= 0 && y' < subH && insideDome subW subH x' y') $ do
                      let cx' = x' `div` 2
                          cy' = y' `div` 4
                          col = x' `mod` 2
                          row = y' `mod` 4
                      setDot masks cx' cy' col row

      forM_ [0..hChars-1] $ \y -> do
        row <- buildRow subW subH wChars masks overrides y
        putStrLn row

