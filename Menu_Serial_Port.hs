------------------------------------------------------------------------------
-- Modulo      :  Menu_FIFO_Haskell
-- Programador :  Sebastián Uriel Flores
-- Estabilidad :  experimental
-- Portabilidad:  experimental
--
-- Programacion Avanzada en Haskell - Ing. Informática. 2018
-- 
-----------------------------------------------------------------------------    

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

--import System.Exit
import Data.Char
import Data.Word
import Control.Applicative
import Control.Monad (when)
import Control.Exception
import System.Hardware.Serialport
import System.IO
import System.Environment
import System.Directory
import Data.Aeson
import Data.Aeson.Types 
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T


main:: IO()
main = do  
      let settingsFileName = "./Settings.json"
      settings <- retrieveSettings settingsFileName
      
      putStrLn "---------------------------------------------------------"
      putStrLn "Se ha obtenido la siguiente información del archivo: "
      putStrLn "---------------------------------------------------------"
      case settings of 
            Just (serialPort, serialPortSettings) -> do
                  putStrLn $ show serialPort
                  putStrLn ""                  
                  putStrLn $ show serialPortSettings
                  putStrLn ""                  
                  putStrLn "---------------------------------------------------------"                  
                  putStrLn "Presione ENTER para continuar"
                  putStrLn "---------------------------------------------------------"                  
                  getLine

                  pathExist <- doesPathExist serialPort
                  fileExist <- doesFileExist serialPort
                  if and [pathExist,fileExist] then do                        
                        permissions <- getPermissions serialPort                        

                        case (readable permissions, writable permissions) of
                              (True, True) -> do
                                    let loopSerial = withSerial serialPort serialPortSettings (\handle -> do
                                          loopMenu handle                                                
                                          ) 
            
                                    loopSerial `catch` ioErrorHandler
                              (False,True) -> do
                                    --Intentar cambiar los permisos desde este software. En caso de no poder, mostrar error
                                    putStrLn "No se poseen permisos de Lectura en la ruta especificada al Puerto Serie"
                              (True, False) -> do
                                    --Intentar cambiar los permisos desde este software. En caso de no poder, mostrar error
                                    putStrLn "No se poseen permisos de Escritura en la ruta especificada al Puerto Serie"
                              (False, False) -> do
                                    --Intentar cambiar los permisos desde este software. En caso de no poder, mostrar error
                                    putStrLn "No se poseen permisos de Lectura y Escritura en la ruta especificada al Puerto Serie"
                  else do
                        putStrLn "La ruta especificada al Puerto Serie no existe, o bien, no corresponde a un Puerto Serie, o bien, no se poseen los permisos de acceso a esa ruta."
                                    
            Nothing -> do 
                  putStrLn "No se pudieron cargar los archivos"
                  putStrLn "---------------------------------------------------------"
            --}

ioErrorHandler :: IOError -> IO ()
ioErrorHandler ioError = putStrLn $ "Error: " ++ show ioError

loopMenu :: SerialPort -> IO()
loopMenu serialPort = do            
      putStrLn " "
      putStrLn "Que desea hacer?"
      putStrLn " - 1. Ingresar la velocidad de los motores"
      putStrLn " - 2. Leer del buffer"
      putStrLn " - 3. Liberar puerto y salir"
      putStr "| --- > "
      userInput <- getLine

      case userInput of
            "1" -> do
                  motor1 <- getLineUntil "Ingrese la velocidad del motor izquierdo --> " :: (IO Int)
                  motor2 <- getLineUntil "Ingrese la velocidad del motor derecho --> " :: (IO Int)
                  send serialPort $ B8.pack ("vel:" ++ (show motor1) ++ "," ++ (show motor2) ++ "\r")
                  loopMenu serialPort
            "2" -> do
                  s <- recv serialPort 10                  
                  B8.putStrLn s
                  loopMenu serialPort
            "3" -> return ()
            otherwise -> do
                  putStrLn ""
                  putStrLn "*************************************"
                  putStrLn "¡ No hemos comprendido su solicitud !"
                  putStrLn "*************************************"
                  loopMenu serialPort

{--
getOption :: String -> ColaProveedores -> ColaItems -> FilePath -> FilePath -> IO()
getOption opcion proveedores items proveedoresFileName itemsFileName = case opcion of
      "1" -> do
            showProveedores proveedores
            loopMenu proveedores items proveedoresFileName itemsFileName
      "2" -> do
            showItems items
            loopMenu proveedores items proveedoresFileName itemsFileName
      "3" -> do 
            proveedoresNuevo <- addProveedor proveedores proveedoresFileName
            loopMenu proveedoresNuevo items proveedoresFileName itemsFileName
      "4" -> do 
            itemsNuevo <- addItem items itemsFileName
            loopMenu proveedores itemsNuevo proveedoresFileName itemsFileName
      "5" -> do 
            proveedoresNuevo <- supressProveedor proveedores proveedoresFileName
            loopMenu proveedoresNuevo items proveedoresFileName itemsFileName
      "6" -> do 
            itemsNuevo <- supressItem items itemsFileName
            loopMenu proveedores itemsNuevo proveedoresFileName itemsFileName
      otherwise -> do 
            putStrLn "---------------------------------------------------------"
            putStrLn "Ha ingresado una opción incorrecta. Intente nuevamente"
            putStrLn "---------------------------------------------------------"            
            loopMenu proveedores items proveedoresFileName itemsFileName
--}
-- Repite una instrucción *getLine* hasta que el Usuario ingrese
-- un dato del tipo *a*.
getLineUntil :: Read a => String -> IO a
getLineUntil guess = do
  putStr guess
  line <- getLine
  case (readMaybe line :: (Read a) => Maybe a) of
    Just x -> return x
    Nothing -> do
      putStrLn ""
      putStrLn "Debe ingresar un elemento del tipo correcto"
      putStrLn ""
      result <- getLineUntil guess
      return result

-- Devuelve *Just a* si lee un elemento de tipo *a*
-- caso contrario, devuelve *Nothing*
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
      [(val, "")] -> Just val
      _           -> Nothing
      
-- Devuelve *Just* con los contenidos de un archivo, o Nothing en caso de error
loadFile :: FilePath -> IO (Maybe B.ByteString)
loadFile path = do 
      fileExists <- doesFileExist path
      case fileExists of
            True -> do
                  fileContents <- B.readFile path
                  return $ Just fileContents
            False -> return Nothing      

-- Devuelve *True* si logra guardar un archivo, o *False* en caso contrario
saveFile :: FilePath -> B.ByteString -> IO Bool
saveFile path contents = do 
      fileExists <- doesFileExist path
      case fileExists of
            True -> do
                  B.writeFile path contents
                  return $ True
            False -> return False

-- Devuelve una Cola con los Items almacenados en un archivo en disco
retrieveSettings :: FilePath -> IO (Maybe (String, SerialPortSettings))
retrieveSettings settingsFileName = do
      settingsRawContent <- loadFile settingsFileName
      case settingsRawContent of
            Just settings -> do                  
                  return $ parseMaybe parseSerialPortJSON =<< decode settings                  

parseSerialPortJSON :: Value -> Parser (String, SerialPortSettings)
parseSerialPortJSON = withObject "Serial Port Name and Settings" $ \o -> do
      --name <- i .:? "name" .!= "Default name"
      port <- o .: "port"
      commSpeed  <- o .:? "baud_rate" .!= CS9600      
      bitsPerWord <- o .:? "bits_per_word" .!= 8
      stopb <- o .:? "stop_bits" .!= One
      parity <- o .:? "parity" .!= NoParity            
      flowControl <- o .:? "flow_control" .!= NoFlowControl      
      timeout <- o .:? "timeout" .!= 1 
      let settings = SerialPortSettings{..}
      return (port,settings)

instance FromJSON CommSpeed where
      parseJSON = withText "CommSpeed" $ \i -> do
            case i of
                  "110" -> return CS110
                  "300" -> return CS300
                  "600" -> return CS600
                  "1200" -> return CS1200
                  "2400" -> return CS2400
                  "4800" -> return CS4800
                  "9600" -> return CS9600
                  "19200" -> return CS19200
                  "38400" -> return CS38400
                  "57600" -> return CS57600
                  "115200" -> return CS115200
                  otherwise -> return CS9600 

instance FromJSON StopBits where
      parseJSON = withText "Stop Bits" $ \i -> do                  
            case i of
                  "1" -> return One
                  "2" -> return Two                  
                  otherwise -> return One
instance FromJSON Parity where
      parseJSON = withText "Parity" $ \i -> do                  
            case i of
                  "Even" -> return Even
                  "Odd" -> return Odd
                  "None" -> return NoParity
                  otherwise -> return NoParity

instance FromJSON FlowControl where
      parseJSON = withText "Flow Control" $ \i -> do                  
            case i of
                  "Software" -> return Software
                  "None" -> return NoFlowControl
                  otherwise -> return NoFlowControl

instance Show SerialPortSettings where
      show sps = unlines [
            "Comm Speed: " ++ show (commSpeed sps),
            "Bits Per Word: " ++ show (bitsPerWord sps),
            "Stop Bits: " ++ show (stopb sps),
            "Parity: " ++ show (parity sps),
            "Flow Control: " ++ show (flowControl sps),
            "Timeout: " ++ show (timeout sps)
            ]