module Main where

import Control.Concurrent
import Control.Monad
import qualified Data.ByteString as B
import Data.Word
import System.RaspberryPi.GPIO 

main :: IO ()
main = 
  do temps <- withGPIO . withI2C $ replicateM sampleRate readTemp
     let (aTemps, oTemps) = unzip temps
     putStr $ "Ambient temp: " ++ show (avgTemp sampleRate aTemps) ++ " C; "
     putStrLn $ "Object temp: " ++ show (avgTemp sampleRate oTemps) ++ " C"
  where sampleRate = 10

-- readTemp :: Fractional a => IO (a, a)
readTemp =
  do setI2cBaudRate 25000
     getTemp
  where getTemp = do aTemp <- ambientTemp
                     oTemp <- objectTemp
                     return (aTemp, oTemp)

objectTemp :: Fractional a => IO a
objectTemp = genericTemp 7 

ambientTemp :: Fractional a => IO a
ambientTemp = genericTemp 6 

genericTemp :: Fractional a => Word8 -> IO a
genericTemp sen =
  do bytes <- readMLX sen
     return . kelvinToCelsius . bytesToKelvin $ bytes

avgTemp :: Fractional a => Int -> [a] -> a
avgTemp n temps = sum temps / fromIntegral n

readMLX :: Word8 -> IO B.ByteString
readMLX s = return . B.take 2 =<< writeReadRSI2C 0x5a (B.singleton s) 3

bytesToInteger :: [Word8] -> Integer
bytesToInteger = foldl (\acc x -> fromIntegral x + acc * 16^2) 0

bytesToKelvin :: Fractional a => B.ByteString -> a
bytesToKelvin = (/50) . fromIntegral . bytesToInteger . reverse . B.unpack

kelvinToCelsius :: Fractional a => a -> a
kelvinToCelsius temp = temp - 273.15
