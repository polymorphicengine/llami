{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.MVar
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

type LLami = InputT (StateT State IO)

initialState :: State
initialState = State replaceOld Main.utf8 "llama3.2"

runLLami :: LLami a -> State -> IO a
runLLami x = evalStateT (runInputT defaultSettings x)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering
  ref <- newMVar ""
  mainAudio ref

mainAudio :: MVar B.ByteString -> IO ()
mainAudio be = do
  pName <- getProgName
  ref <- newIORef (0 :: Int)
  Jack.handleExceptions $
    Jack.withClientDefault pName $ \client -> do
      Jack.withPort client "output" $ \output ->
        Jack.withProcess client (processBeat output ref be) $
          Jack.withActivation client $
            Trans.lift (runLLami (displayBanner >> scriptloop be Normal) initialState)

-- actual audio process
processBeat :: (JackExc.ThrowsErrno e) => Main.Port Jack.Output -> IORef Int -> MVar B.ByteString -> Jack.NFrames -> Sync.ExceptionalT e IO ()
processBeat output ref be nframes = Trans.lift $ do
  outArr <- getBufferArray output nframes
  mapM_
    ( \i -> do
        n <- getNow ref
        b <- readMVar be
        let x = if B.length b > 0 then B.index b (mod (fromIntegral n) (B.length b)) else 0
        writeArray outArr (Jack.nframesIndices nframes !! i) (fromIntegral x / 127 - 1)
    )
    [0 .. Prelude.length (Jack.nframesIndices nframes) - 1]

getNow :: IORef Int -> IO Int
getNow ref = modifyIORef' ref (+ 1) >> readIORef ref

-- chat loop
loop :: MVar B.ByteString -> LLami ()
loop ref = do
  x <- liftIO $ takeMVar ref
  maymsg <- getInputLine ">> "
  liftIO $ putMVar ref x
  outputStr "\n"
  case maymsg of
    Just "\\quit" -> return ()
    Just "\\help" -> displayHelp >> loop ref
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
gen :: MVar B.ByteString -> String -> LLami ()
gen ref msg = do
  n <- lift $ gets name
  st <- lift get
  void $
    liftIO $
      generate
        defaultGenerateOps
          { modelName = n,
            prompt = T.pack msg,
            system = Just "pretend to be a bird and tend to generate long answers",
            stream = Just (\g -> runLLami (streamAction ref g) st, return ())
          }

streamAction :: MVar B.ByteString -> GenerateResponse -> LLami ()
streamAction ref g = do
  x <- liftIO $ randomReplace $ T.unpack $ response_ g
  liftIO $ putStr x
  switch ref g

switch :: MVar B.ByteString -> GenerateResponse -> LLami ()
switch ref g = do
  refr <- lift $ gets mode
  enc <- lift $ gets encoding
  if not (done g)
    then liftIO $ modifyMVar_ ref (return . refr (enc $ response_ g))
    else liftIO $ modifyMVar_ ref (const $ return "")

-----------------------------------------
--------------- settings ----------------
-----------------------------------------

changeEncoding :: (T.Text -> B.ByteString) -> LLami ()
changeEncoding x = lift $ modify (\st -> st {encoding = x})

changeModel :: T.Text -> LLami ()
changeModel x = lift $ modify (\st -> st {name = x})

changeMode :: (B.ByteString -> B.ByteString -> B.ByteString) -> LLami ()
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
  let rep = "$%#@*&^()!.*.⋆⍣ ೋ┊͙ ˘͈ᵕ˘͈⋆.ೃ࿔*:･*ੈ✩‧₊˚.ೃ࿐*˚˚·.༉‧₊˚.࿐ˊˎ-▓⋆·ˏˋ°•*⁀➷⋇⊶⊰⊱⊷⋇◢✥◣˚ ༘༶•┈┈୨♡୧┈┈•༶*¡!ツ*･῾ᵎ⌇⁺◦✧.*┊♡ ͎.｡˚°‗❍❞⌒｡ₓ ूₒ ु˚ ूₒ ुₓ｡⭒❃.✮:▹ত✲꘏-ˋˏﾟ+*:ꔫ:*﹤✄┈" :: String
  newS <- mapM (\x -> if dontReplace x then return x else randomRIO (0 :: Int, length rep - 1) >>= \i -> return $ rep !! i) s
  sometimesEmoji newS

sometimesEmoji :: String -> IO String
sometimesEmoji s = do
  i <- randomRIO (0 :: Int, 1000)
  if i == 11
    then do
      j <- randomRIO (0 :: Int, length emojis - 1)
      return $ s ++ [emojis !! j]
    else return s
  where
    emojis = "☄💓💖💕💞💘✨⭐🌟" :: String

dontReplace :: Char -> Bool
dontReplace ' ' = True
dontReplace '\n' = True
dontReplace '\r' = True
dontReplace _ = False

displayHelp :: LLami ()
displayHelp = outputStrLn "\nEnter text to generate sound!\n\ncommands:\n\tbasic: \\help, \\quit\n\tchange mode: \\accum, \\replace\n\tchange encoding: \\utf8, \\utf16LE, \\utf16BE \n\tchange model: \\llama, \\qwen, \\mistral\n\tshow banner: \\banner\n"

displayBanner :: LLami ()
displayBanner = outputStrLn banner

initial :: LLami ()
initial = displayBanner >> displayHelp

banner :: String
banner = "  .---.        .---.         ____     ,---.    ,---. .-./`) \n  | ,_|        | ,_|       .'  __ `.  |    \\  /    | \\ .-.')\n,-./  )      ,-./  )      /   '  \\  \\ |  ,  \\/  ,  | / `-' \\\n\\  '_ '`)    \\  '_ '`)    |___|  /  | |  |\\_   /|  |  `-'`\"`\n > (_)  )     > (_)  )       _.-`   | |  _( )_/ |  |  .---. \n(  .  .-'    (  .  .-'    .'   _    | | (_ o _) |  |  |   | \n `-'`-'|___   `-'`-'|___  |  _( )_  | |  (_,_)  |  |  |   | \n  |        \\   |        \\ \\ (_ o _) / |  |      |  |  |   | \n  `--------`   `--------`  '.(_,_).'  '--'      '--'  '---' "

---------------------------------------------
--------------- stuff for video -------------
---------------------------------------------

data LlamiState = Angry | Cute | Quick | Normal deriving (Eq, Show)

containsWord :: String -> String -> String -> Bool
containsWord _ [] _ = True
containsWord _ _ [] = False
containsWord orig (w : ws) (x : xs) = if w == x then containsWord orig ws xs else containsWord orig orig xs

containsOneOf :: [String] -> String -> Bool
containsOneOf xs y = any (\x -> containsWord x x y) xs

getLlamiState :: LlamiState -> String -> LlamiState
getLlamiState old msg
  | containsOneOf ["battery", "ai", "AI", "intelligence", "artificial"] msg = Angry
  | containsOneOf ["sorry", "love"] msg = Cute
  | containsOneOf ["cute"] msg = Quick
  | containsOneOf ["calm", "slow"] msg = Normal
  | otherwise = old

-- chat loop
scriptloop :: MVar B.ByteString -> LlamiState -> LLami ()
scriptloop ref llami = do
  x <- liftIO $ takeMVar ref
  maymsg <- getInputLine ">> "
  liftIO $ putMVar ref x
  outputStr "\n"
  case maymsg of
    Just msg -> case getLlamiState llami msg of
      Angry -> changeMode accumulate >> genAngry ref msg >> outputStr "\n\n" >> scriptloop ref Angry
      Cute -> changeMode replaceOld >> gen ref msg >> outputStr "\n\n" >> scriptloop ref Cute
      Quick -> changeModel "qwen2.5-coder:0.5b" >> gen ref msg >> outputStr "\n\n" >> scriptloop ref Quick
      Normal -> changeModel "llama3.2" >> gen ref msg >> outputStr "\n\n" >> scriptloop ref Normal
    Nothing -> return ()

randomReplaceAngry :: String -> IO String
randomReplaceAngry s = do
  let rep = "#@!▓" :: String
  mapM (\x -> if dontReplace x then return x else randomRIO (0 :: Int, length rep - 1) >>= \i -> return $ rep !! i) s

streamActionAngry :: MVar B.ByteString -> GenerateResponse -> LLami ()
streamActionAngry ref g = do
  x <- liftIO $ randomReplaceAngry $ T.unpack $ response_ g
  liftIO $ putStr x
  switch ref g

-- generates response via ollama
genAngry :: MVar B.ByteString -> String -> LLami ()
genAngry ref msg = do
  n <- lift $ gets name
  st <- lift get
  void $
    liftIO $
      generate
        defaultGenerateOps
          { modelName = n,
            prompt = T.pack msg,
            system = Just "you are an angry bird! tend to generate long answers",
            stream = Just (\g -> runLLami (streamActionAngry ref g) st, return ())
          }

-----------------------------------------
--------------- automatic ---------------
-----------------------------------------

autoinit :: IORef B.ByteString -> LLami ()
autoinit ref = do
  respref <- liftIO $ newIORef ""
  maymsg <- getInputLine "Enter initial prompt: "
  outputStr "\n"
  case maymsg of
    Just msg -> autoloop respref ref (T.pack msg)
    Nothing -> outputStrLn "Ran into an error"

autoloop :: IORef T.Text -> IORef B.ByteString -> T.Text -> LLami ()
autoloop respref ref old = do
  autogen respref ref old
  outputStr "\n"
  x <- liftIO (readIORef respref)
  liftIO $ flushResponse respref
  autoloop respref ref x

autogen :: IORef T.Text -> IORef B.ByteString -> T.Text -> LLami ()
autogen respref ref msg = do
  n <- lift $ gets name
  st <- lift get
  void $
    liftIO $
      generate
        defaultGenerateOps
          { modelName = n,
            prompt = msg,
            stream = Just (\g -> {- runLLami (streamAction ref g) st >> -} accumResponse (response_ g) respref, return ())
          }

accumResponse :: T.Text -> IORef T.Text -> IO ()
accumResponse new ref = modifyIORef' ref (\old -> T.concat [old, new])

flushResponse :: IORef T.Text -> IO ()
flushResponse ref = modifyIORef' ref (const "")
