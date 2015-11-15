{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Common where

import           GHCJS.Types
import           GHCJS.Foreign.Callback

import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad

import           Data.IORef
import           Data.JSString

import           Diagrams.Backend.GHCJS
import           Diagrams.Prelude

import qualified JavaScript.Web.Canvas as C

createCanvas :: Int -> Int -> IO C.Canvas
createCanvas w h = C.create w h

createDiagramOutput :: Int -> Int -> Diagram Canvas -> IO (JSVal, C.Context)
createDiagramOutput w h d = do
  c   <- C.create w h
  ctx <- C.getContext c
  return (jsval c, ctx)

renderDia' :: Int -> Int -> C.Context -> Diagram Canvas -> IO ()
renderDia' w h c d =
  renderDia Canvas (CanvasOptions (dims2D (fromIntegral w) (fromIntegral h)) c) d

staticDiagram :: String -> Int -> Int -> Diagram Canvas -> IO ()
staticDiagram ref w h d = do
  e        <- getElementById (pack ref)
  (c, ctx) <- createDiagramOutput w h d
  renderDia' w h ctx d
  appendChild c e

dynamicDiagram :: String -> Int -> Int -> String -> (String -> Diagram Canvas) -> IO ()
dynamicDiagram ref w h initial f = do
  e <- getElementById (pack ref)
  (pt, tf) <- createTextField (pack initial)
  (c, ctx) <- createDiagramOutput w h mempty
  let w' = fromIntegral w
      h' = fromIntegral h
      updateResult = do
        v <- getValue tf
        output <- evaluate (f . unpack $ v) `catch`
          \(_::SomeException) -> return mempty
        C.clearRect 0 0 w' h' ctx
        renderDia' w h ctx output `catch` \(_::SomeException) -> return ()
  appendChild pt e
  appendChild c  e
  triggerUpdate <- createInteraction updateResult
  cb <- syncCallback ThrowWouldBlock triggerUpdate
  addEventListener "input" (jsval cb) tf

mouseDiagram :: String -> Int -> Int -> (Double -> Double -> Diagram Canvas) -> IO ()
mouseDiagram ref w h f = do
  e <- getElementById (pack ref)
  (c, ctx) <- createDiagramOutput w h mempty
  mx <- newIORef 0.5
  my <- newIORef 0.5
  let w' = fromIntegral w
      h' = fromIntegral h
      updateResult = do
        mx' <- readIORef mx
        my' <- readIORef my
        output <- evaluate (f mx' my') `catch`
          \(_::SomeException) -> return mempty
        C.clearRect 0 0 w' h' ctx
        renderDia' w h ctx output `catch` \(_::SomeException) -> return ()
  appendChild c  e
  triggerUpdate <- createInteraction updateResult
  let mouseHandler e = do
        cx <- getClientX e
        cy <- getClientY e
        -- looks like there's still something wrong with the coordinates, 
        -- at least on a small screen
        bcr <- getBoundingClientRect c
        bTop <- getTop bcr
        bBottom <- getBottom bcr
        bLeft <- getLeft bcr
        bRight <- getRight bcr
        writeIORef mx ((cx-bLeft)/(bRight-bLeft))
        writeIORef my ((cy-bTop)/(bBottom-bTop))
        triggerUpdate
  cb <- syncCallback1 ThrowWouldBlock mouseHandler
  addEventListener "mousemove" (jsval cb) c

createOutput :: JSString -> IO (JSVal, JSVal)
createOutput xs = do
  p <- createElement "pre"
  setAttribute "class" "sourceCode" p
  c <- createElement "code"
  setAttribute "class" "sourceCode" c
  t <- createTextNode xs
  appendChild t c
  appendChild c p
  return (p, c)

updateOutput :: JSString -> JSVal -> IO ()
updateOutput xs c = do
  fc <- firstChild c
  t  <- createTextNode xs
  replaceChild t fc c

createInteraction :: IO () -> IO (IO ())
createInteraction action = mask_ $ do
  mv <- newMVar ()
  let doUpdate = takeMVar mv >> action
  tid <- forkIOWithUnmask $ \unmask ->
    forever (unmask doUpdate `catch` \(e::AsyncException) -> return ())
  let forceUpdate = killThread tid >> tryPutMVar mv () >> return ()
  return forceUpdate

createTextField :: JSString -> IO (JSVal, JSVal)
createTextField xs = do
  p  <- createElement "pre"
  setAttribute "class" "interact" p
  d  <- createElement "div"
  tf <- createElement "input"
  setAttribute "type" "text" tf
  setAttribute "value" xs tf
  appendChild tf d
  appendChild d p
  return (p, tf)

staticText :: String -> String -> IO ()
staticText elemId xs = do
  e <- getElementById (pack elemId)
  (p, _c) <- createOutput (pack xs)
  appendChild p e

dynamicText :: String -> String -> (String -> String) -> IO ()
dynamicText elemId initial f = do
  e  <- getElementById (pack elemId)
  (pt, tf) <- createTextField (pack initial)
  (p, c) <- createOutput ""
  let updateResult = do
        v <- getValue tf
        output <- evaluate (pack . f . unpack $ v) `catch`
          \(_::SomeException) -> return "invalid input"
        updateOutput output c
  appendChild pt e
  appendChild p  e
  triggerUpdate <- createInteraction updateResult
  cb <- syncCallback ThrowWouldBlock triggerUpdate
  addEventListener "input" (jsval cb) tf

foreign import javascript
  "document.getElementById($1)"
  getElementById :: JSString -> IO JSVal

foreign import javascript
  "document.createTextNode($1)"
  createTextNode :: JSString -> IO JSVal

foreign import javascript
  "$2.appendChild($1);"
  appendChild :: JSVal -> JSVal -> IO ()

foreign import javascript
  "document.createElement($1)"
  createElement :: JSString -> IO JSVal

foreign import javascript
  "$3.setAttribute($1,$2);"
  setAttribute :: JSString -> JSString -> JSVal -> IO ()

foreign import javascript
  "$2.getAttribute($1)"
  getAttribute :: JSString -> JSVal -> IO JSString

foreign import javascript
  "$3.addEventListener($1,$2);"
  addEventListener :: JSString -> JSVal -> JSVal -> IO ()
                      
foreign import javascript
  "$3.replaceChild($1,$2);"
  replaceChild :: JSVal -> JSVal -> JSVal -> IO ()

foreign import javascript
  "$1.firstChild"
  firstChild :: JSVal -> IO JSVal

foreign import javascript
  "$1.value"
  getValue :: JSVal -> IO JSString

foreign import javascript
  "$1.clientX"
  getClientX :: JSVal -> IO Double

foreign import javascript
  "$1.clientY"
  getClientY :: JSVal -> IO Double
                
foreign import javascript
  "$1.getBoundingClientRect()"
  getBoundingClientRect :: JSVal -> IO JSVal
                           
foreign import javascript
  "$1.top"
  getTop :: JSVal -> IO Double

foreign import javascript
  "$1.left"
  getLeft :: JSVal -> IO Double

foreign import javascript
  "$1.right"
  getRight :: JSVal -> IO Double

foreign import javascript
  "$1.bottom"
  getBottom :: JSVal -> IO Double
