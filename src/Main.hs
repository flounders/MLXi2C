module Main where

import Control.Concurrent
import Control.Monad
import qualified Data.ByteString as B
import Data.Maybe (isJust, fromJust)
import Data.Word
import System.RaspberryPi.GPIO 

main :: IO ()
main = 
  do temps <- withGPIO . withI2C $ replicateM sampleRate readTemp
     let (aTemps, oTemps) = unzip temps
     putStr $ "Ambient temp: " ++ show (avgTemp . map fromJust . filter isJust $ aTemps) ++ " C; "
     putStrLn $ "Object temp: " ++ show (avgTemp . map fromJust . filter isJust $ oTemps) ++ " C"
  where sampleRate = 10

-- readTemp :: Fractional a => IO (a, a)
readTemp =
  do setI2cBaudRate 25000
     getTemp
  where getTemp = do aTemp <- ambientTemp
                     oTemp <- objectTemp
                     return (aTemp, oTemp)

objectTemp :: Fractional a => IO (Maybe a)
objectTemp = genericTemp 7 

ambientTemp :: Fractional a => IO (Maybe a)
ambientTemp = genericTemp 6 

genericTemp :: Fractional a => Word8 -> IO (Maybe a)
genericTemp sen =
  do bytes <- readMLX sen
     return $ kelvinToCelsius <$> bytesToKelvin bytes

avgTemp :: Fractional a => [a] -> a
avgTemp = g . foldr f (0,0) 
  where f x (total, length) = (x + total, length + 1)
        g (total, length) = total / length

readMLX :: Word8 -> IO B.ByteString
readMLX s = return . B.take 2 =<< writeReadRSI2C 0x5a (B.singleton s) 3

bytesToInteger :: [Word8] -> Integer
bytesToInteger = foldl (\acc x -> fromIntegral x + acc * 16^2) 0

bytesToKelvin :: Fractional a => B.ByteString -> Maybe a
bytesToKelvin bs = (/50) . fromIntegral . bytesToInteger <$> (validBytes . reverse . B.unpack $ bs)

kelvinToCelsius :: Fractional a => a -> a
kelvinToCelsius temp = temp - 273.15

validBytes :: [Word8] -> Maybe [Word8]
validBytes (x:xs)
  | x <= 128 = Just (x:xs)
  | otherwise = Nothing
validBytes _ = Nothing
