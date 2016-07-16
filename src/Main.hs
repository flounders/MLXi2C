module Main where

import qualified Data.ByteString as B
import System.RaspberryPi.GPIO 
import Data.Word

main :: IO ()
main = withGPIO . withI2C $ readTemp

readTemp :: IO ()
readTemp =
  do setI2cBaudRate 25000
     aTemp <- ambientTemp
     iTemp <- infraTemp
     putStrLn $ "Ambient temperature: " ++ show aTemp ++ "C"
     putStrLn $ "Infrared temperature: " ++ show iTemp ++ "C"

infraTemp :: Fractional a => IO a
infraTemp = 
  do bytes <- writeReadRSI2C 0x5a (B.singleton 7) 2
     return (bytesToKelvin bytes - 273.15)

ambientTemp = 
  do bytes <- writeReadRSI2C 0x5a (B.singleton 6) 2
     return (bytesToKelvin bytes - 273.15)

bytesToInteger :: [Word8] -> Integer
bytesToInteger = foldl (\acc x -> fromIntegral x + acc * 16^2) 0

bytesToKelvin :: Fractional a => B.ByteString -> a
bytesToKelvin = (/50) . fromIntegral . bytesToInteger . reverse . B.unpack
