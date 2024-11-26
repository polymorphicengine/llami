{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (void)
import qualified Control.Monad.Exception.Synchronous as Sync
import Control.Monad.State (StateT, evalStateT, get, gets, lift, liftIO, modify)
import qualified Control.Monad.Trans.Class as Trans
import Data.Array.Storable (writeArray)
import qualified Data.ByteString as B
import Data.IORef
import Data.Ollama.Generate
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Sound.JACK as Jack
import Sound.JACK.Audio
import qualified Sound.JACK.Exception as JackExc
import System.Console.Haskeline
import System.Environment (getProgName)
import System.IO
import System.Random

type Port = Jack.Port Sample

data State = State
  { mode :: B.ByteString -> B.ByteString -> B.ByteString, -- what happens with old responses when new ones come
    encoding :: T.Text -> B.ByteString, -- how to encode the generated text into a bytestring
    name :: T.Text -- name of the model
  }

type Ollamaphon = InputT (StateT State IO)

initialState :: State
initialState = State replaceOld Main.utf8 "llama3.2"

runOllamaphon :: Ollamaphon a -> State -> IO a
runOllamaphon x = evalStateT (runInputT defaultSettings x)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering
  ref <- newIORef ""
  mainAudio ref

mainAudio :: IORef B.ByteString -> IO ()
mainAudio be = do
  pName <- getProgName
  ref <- newIORef (0 :: Int)
  Jack.handleExceptions $
    Jack.withClientDefault pName $ \client -> do
      Jack.withPort client "output" $ \output ->
        Jack.withProcess client (processBeat output ref be) $
          Jack.withActivation client $
            Trans.lift (runOllamaphon (initial >> loop be) initialState)

-- actual audio process
processBeat :: (JackExc.ThrowsErrno e) => Main.Port Jack.Output -> IORef Int -> IORef B.ByteString -> Jack.NFrames -> Sync.ExceptionalT e IO ()
processBeat output ref be nframes = Trans.lift $ do
  outArr <- getBufferArray output nframes
  mapM_
    ( \i -> do
        n <- getNow ref
        b <- readIORef be
        let x = if B.length b > 0 then B.index b (mod (fromIntegral n) (B.length b)) else 0
        writeArray outArr (Jack.nframesIndices nframes !! i) (fromIntegral x / 127 - 1)
    )
    [0 .. Prelude.length (Jack.nframesIndices nframes) - 1]

getNow :: IORef Int -> IO Int
getNow ref = modifyIORef' ref (+ 1) >> readIORef ref

-- chat loop
loop :: IORef B.ByteString -> Ollamaphon ()
loop ref = do
  maymsg <- getInputLine ">> "
  outputStr "\n"
  case maymsg of
    Just "\\quit" -> return ()
    Just "\\help" -> displayHelp >> loop ref
    Just "\\reset" -> reset ref >> loop ref
    Just "\\banner" -> displayBanner >> outputStr "\n" >> loop ref
    Just "\\accum" -> changeMode accumulate >> loop ref
    Just "\\replace" -> changeMode replaceOld >> loop ref
    Just "\\utf8" -> changeEncoding Main.utf8 >> loop ref
    Just "\\utf16LE" -> changeEncoding utf16LE >> loop ref
    Just "\\utf16BE" -> changeEncoding utf16BE >> loop ref
    Just "\\llama" -> changeModel "llama3.2" >> loop ref
    Just "\\mistral" -> changeModel "mistral" >> loop ref
    Just "\\qwen" -> changeModel "qwen2.5-coder:0.5b" >> loop ref
    Just msg -> gen ref msg >> outputStr "\n\n" >> loop ref
    Nothing -> return ()

-- generates response via ollama
gen :: IORef B.ByteString -> String -> Ollamaphon ()
gen ref msg = do
  n <- lift $ gets name
  st <- lift get
  liftIO $
    void $
      generate
        defaultGenerateOps
          { modelName = n,
            prompt = T.pack msg,
            stream = Just (\g -> runOllamaphon (streamAction ref g) st, return ()),
            raw = Just False
          }

streamAction :: IORef B.ByteString -> GenerateResponse -> Ollamaphon ()
streamAction ref g = do
  x <- liftIO $ randomReplace $ T.unpack $ response_ g
  liftIO $ putStr x
  switch ref (response_ g)

switch :: IORef B.ByteString -> T.Text -> Ollamaphon ()
switch ref t = do
  refr <- lift $ gets mode
  enc <- lift $ gets encoding
  liftIO $ modifyIORef' ref (refr $ enc t)

-----------------------------------------
--------------- settings ----------------
-----------------------------------------

reset :: IORef B.ByteString -> Ollamaphon ()
reset ref = lift $ liftIO $ modifyIORef' ref (const "")

changeEncoding :: (T.Text -> B.ByteString) -> Ollamaphon ()
changeEncoding x = lift $ modify (\st -> st {encoding = x})

changeModel :: T.Text -> Ollamaphon ()
changeModel x = lift $ modify (\st -> st {name = x})

changeMode :: (B.ByteString -> B.ByteString -> B.ByteString) -> Ollamaphon ()
changeMode m = lift $ modify (\st -> st {mode = m})

replaceOld :: B.ByteString -> B.ByteString -> B.ByteString
replaceOld = const

accumulate :: B.ByteString -> B.ByteString -> B.ByteString
accumulate x y = B.concat [x, y]

utf8 :: T.Text -> B.ByteString
utf8 = T.encodeUtf8

utf16LE :: T.Text -> B.ByteString
utf16LE = T.encodeUtf16LE

utf16BE :: T.Text -> B.ByteString
utf16BE = T.encodeUtf16BE

randomReplace :: String -> IO String
randomReplace s = do
  let rep = "$%#@*&^()!" :: String
  mapM (\x -> if dontReplace x then return x else randomRIO (0 :: Int, length rep - 1) >>= \i -> return $ rep !! i) s

dontReplace :: Char -> Bool
dontReplace ' ' = True
dontReplace '\n' = True
dontReplace '\r' = True
dontReplace _ = False

displayHelp :: Ollamaphon ()
displayHelp = outputStrLn "\nEnter text to generate sound!\n\ncommands:\n\tbasic: \\help, \\reset, \\quit\n\tchange mode: \\accum, \\replace\n\tchange encoding: \\utf8, \\utf16LE, \\utf16BE \n\tchange model: \\llama, \\qwen, \\mistral\n\tshow banner: \\banner\n"

displayBanner :: Ollamaphon ()
displayBanner = outputStrLn banner

initial :: Ollamaphon ()
initial = displayBanner >> displayHelp

banner :: String
banner = "   _      _      _      _      _      _      _      _      _      _      _   \n _( )_  _( )_  _( )_  _( )_  _( )_  _( )_  _( )_  _( )_  _( )_  _( )_  _( )_ \n(_ o _)(_ o _)(_ o _)(_ o _)(_ o _)(_ o _)(_ o _)(_ o _)(_ o _)(_ o _)(_ o _)\n (_,_)  (_,_)  (_,_)  (_,_)  (_,_)  (_,_)  (_,_)  (_,_)  (_,_)  (_,_)  (_,_) \n   _                                                                     _   \n _( )_      ___  _ _                             _                     _( )_ \n(_ o _)    / _ \\| | | __ _ _ __ ___   __ _ _ __ | |__   ___  _ __     (_ o _)\n (_,_)    | | | | | |/ _` | '_ ` _ \\ / _` | '_ \\| '_ \\ / _ \\| '_ \\     (_,_) \n   _      | |_| | | | (_| | | | | | | (_| | |_) | | | | (_) | | | |      _   \n _( )_     \\___/|_|_|\\__,_|_| |_| |_|\\__,_| .__/|_| |_|\\___/|_| |_|    _( )_ \n(_ o _)                                   |_|                         (_ o _)\n (_,_)                                                                 (_,_) \n   _      _      _      _      _      _      _      _      _      _      _   \n _( )_  _( )_  _( )_  _( )_  _( )_  _( )_  _( )_  _( )_  _( )_  _( )_  _( )_ \n(_ o _)(_ o _)(_ o _)(_ o _)(_ o _)(_ o _)(_ o _)(_ o _)(_ o _)(_ o _)(_ o _)\n (_,_)  (_,_)  (_,_)  (_,_)  (_,_)  (_,_)  (_,_)  (_,_)  (_,_)  (_,_)  (_,_) "
