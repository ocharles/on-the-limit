{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Lens.Operators
import Control.Monad
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans.State (evalState, state)
import Data.Foldable
import Data.Function (fix)
import DebugHook
import Foreign
import GLObjects
import Graphics.GL.Core33
import Linear hiding (basis)
import Reactive.Banana
import Reactive.Banana.Frameworks
import Render
import System.Clock
import System.Random (randomR, getStdGen)
import qualified SDL

import Paths_on_the_limit

data FrameData =
  FrameData {depthFBO :: Framebuffer
            ,ssaoFBO :: Framebuffer
            ,deferDepth :: Program
            ,ssao :: Program
            ,ssaoBlurred :: Texture
            ,ship :: Program
            ,blur :: Program
            ,depthTexture :: Texture
            ,shipVao :: VertexArrayObject
            ,rotationTexture :: Texture
            ,ssaoBlurFBO1 :: Framebuffer
            ,ssaoResult :: Texture
            ,ssaoBlurFBO2 :: Framebuffer
            ,ssaoBlurredIntermediate :: Texture
            ,feisarDiffuse :: Texture
            ,asphalt :: Texture
            ,road :: VertexArrayObject}

screenWidth, screenHeight :: Int
(screenWidth,screenHeight) = (1024,1024)

loadFrameData :: IO FrameData
loadFrameData =
  do feisarDiffuse <-
       loadTexture =<< getDataFileName "resources/textures/feisar.bmp"
     asphalt <-
       loadTexture =<< getDataFileName "resources/textures/UVCheckerMap01-1024.png"
     depthRenderbuffer <- newRenderbuffer GL_DEPTH_COMPONENT32F 1024 1024
     depthTexture <- newTexture2D 1 GL_DEPTH_COMPONENT32F 1024 1024
     ssaoResult <- newTexture2D 1 GL_R32F 1024 1024
     ssaoBlurredIntermediate <- newTexture2D 1 GL_R32F 1024 1024
     ssaoBlurred <- newTexture2D 1 GL_R32F 1024 1024
     depthFBO <-
       newFramebuffer
         (\case
            DepthAttachment -> Just (AttachToTexture depthTexture 0)
            _ -> Nothing)
     ssaoFBO <-
       newFramebuffer
         (\case
            ColorAttachment 0 -> Just (AttachToTexture ssaoResult 0)
            DepthAttachment -> Just (AttachToRenderbuffer depthRenderbuffer)
            _ -> Nothing)
     ssaoBlurFBO1 <-
       newFramebuffer
         (\case
            ColorAttachment 0 ->
              Just (AttachToTexture ssaoBlurredIntermediate 0)
            DepthAttachment -> Nothing
            _ -> Nothing)
     ssaoBlurFBO2 <-
       newFramebuffer
         (\case
            ColorAttachment 0 -> Just (AttachToTexture ssaoBlurred 0)
            DepthAttachment -> Nothing
            _ -> Nothing)
     deferDepth <-
       join (loadVertexFragmentProgram <$>
             getDataFileName "resources/shaders/depth_vs.glsl" <*>
             getDataFileName "resources/shaders/depth_fs.glsl")
     ssao <-
       join (loadVertexFragmentProgram <$>
             getDataFileName "resources/shaders/ssao_vs.glsl" <*>
             getDataFileName "resources/shaders/ssao_fs.glsl")
     blur <-
       join (loadVertexFragmentProgram <$>
             getDataFileName "resources/shaders/blur_vs.glsl" <*>
             getDataFileName "resources/shaders/blur_fs.glsl")
     ship <-
       join (loadVertexFragmentProgram <$>
             getDataFileName "resources/shaders/ship_vs.glsl" <*>
             getDataFileName "resources/shaders/ship_fs.glsl")
     do kernel <- newSamplingKernel
        setUniform v4Array ssao "kernel" kernel
     setUniform textureUnit ssao "rotations" 1
     setUniform textureUnit ship "diffuseMap" 1
     rotationTexture <- newRotations >>= uploadTexture2D
     shipVao <- loadObj =<< getDataFileName "resources/objects/feisar.obj"
     road <- generateRoad 10000
     return FrameData {..}

generateRoad :: Float -> IO VertexArrayObject
generateRoad extent =
  let a =
        Vertex (V3 (negate extent) 0 extent)
               (V3 0 1 0)
               (V2 0 (2 * extent))
      b =
        Vertex (V3 (negate extent)
                   0
                   (negate extent))
               (V3 0 1 0)
               (V2 0 0)
      c =
        Vertex (V3 extent 0 (negate extent))
               (V3 0 1 0)
               (V2 (2 * extent) 0)
      d =
        Vertex (V3 extent 0 extent)
               (V3 0 1 0)
               (pure (2 * extent))
  in uploadTriangles objVertexAttribs
                     [[a,c,b],[a,d,c]]

frame :: FrameData -> Scene -> IO ()
frame FrameData{..} Scene{..} =
  do let drawScene =
           [DrawCommand {dcVertexArrayObject = shipVao
                        ,dcProgram = ship
                        ,dcTextures = []
                        ,dcUniforms = []
                        ,dcNElements = 5048
                        ,dcViewTransform = viewTransform
                        ,dcProjectionTransform = projTransform
                        ,dcModelTransform = modelTransform}
           ,DrawCommand {dcVertexArrayObject = road
                        ,dcProgram = ship
                        ,dcTextures = []
                        ,dcUniforms = []
                        ,dcNElements = 6
                        ,dcViewTransform = viewTransform
                        ,dcProjectionTransform = projTransform
                        ,dcModelTransform = identity}]
     pass depthPass
          (map (\dc ->
                  dc {dcProgram = deferDepth})
               drawScene)
     pass ssaoPass
          (map (\dc ->
                  dc {dcProgram = ssao
                     ,dcTextures =
                        [depthTexture,rotationTexture]})
               drawScene)
     for_ [(ssaoBlurPass1,V2 1 0,ssaoResult)
          ,(ssaoBlurPass2,V2 0 1,ssaoBlurredIntermediate)]
          (\(p,basis,source) ->
             pass p
                  [DrawCommand {dcVertexArrayObject = fullScreenTriangle
                               ,dcProgram = blur
                               ,dcTextures =
                                  [source]
                               ,dcModelTransform = identity
                               ,dcViewTransform = identity
                               ,dcProjectionTransform = identity
                               ,dcNElements = 3
                               ,dcUniforms =
                                  [Uniform v2f "basis" basis]}])
     pass forwardPass
          (zipWith (\dc t ->
                      dc {dcTextures =
                            [ssaoBlurred,t]})
                   drawScene
                   [feisarDiffuse,asphalt])
  where rtSize = (0,0,1024,1024)
        depthPass = Pass depthFBO rtSize
        ssaoPass = Pass ssaoFBO rtSize
        ssaoBlurPass1 = Pass ssaoBlurFBO1 rtSize
        ssaoBlurPass2 = Pass ssaoBlurFBO2 rtSize
        forwardPass =
          Pass (Framebuffer 0)
               (0,0,viewport ^. _x,viewport ^. _y)
        fullScreenTriangle = shipVao

initialScreenSize = fromIntegral <$> V2 screenWidth screenHeight

main :: IO ()
main =
  do SDL.initializeAll
     win <-
       SDL.createWindow
         "SSAO Example"
         SDL.defaultWindow {SDL.windowInitialSize = initialScreenSize
                           ,SDL.windowOpenGL =
                              Just (SDL.defaultOpenGL {SDL.glProfile =
                                                         SDL.Core SDL.Debug 3 3
                                                      ,SDL.glColorPrecision = 8})}
     SDL.glCreateContext win >>= SDL.glMakeCurrent win
     glEnable GL_DEPTH_TEST
     glEnable GL_CULL_FACE
     glEnable GL_FRAMEBUFFER_SRGB
     installDebugHook
     frameData <- loadFrameData
     realTimeLoop win
                  game
                  (frame frameData)

realTimeLoop :: SDL.Window
             -> (Event SDL.EventPayload -> Event Double -> MomentIO (Behavior a))
             -> (a -> IO ())
             -> IO ()
realTimeLoop win network interpret =
  do (physicsStepped,progressPhysics) <- newAddHandler
     (rendered,render) <- newAddHandler
     (sdlEvent,dispatchSdlEvent) <- newAddHandler
     let dt = 1 / 120 :: Double
         step accumulator lastTime =
           do currentTime <- getTime Monotonic
              events <- SDL.pollEvents
              mapM_ (dispatchSdlEvent . SDL.eventPayload) events
              let frameTime =
                    fromIntegral (timeSpecAsNanoSecs (diffTimeSpec currentTime lastTime)) *
                    1.0e-9
              accumulator' <-
                fix (\loop accumulator' ->
                       do if accumulator' >= dt
                             then do progressPhysics dt
                                     loop (accumulator - dt)
                             else return accumulator')
                    (accumulator + frameTime)
              render ()
              SDL.glSwapWindow win
              step accumulator' currentTime
     compile (do out <-
                   join (liftA2 network
                                (fromAddHandler sdlEvent)
                                (fromAddHandler physicsStepped))
                 rendered' <- fromAddHandler rendered
                 reactimate (fmap interpret out <@ rendered')) >>=
       actuate
     getTime Monotonic >>= step 0

data Scene =
  Scene {modelTransform :: M44 Float
        ,projTransform :: M44 Float
        ,viewTransform :: M44 Float
        ,viewport :: V2 GLint}

game :: Event SDL.EventPayload
     -> Event Double
     -> MomentIO (Behavior Scene)
game sdlEvent tick =
  mdo viewportSize <-
        stepper initialScreenSize
                (filterJust
                   (fmap (\case
                            SDL.WindowResizedEvent d ->
                              Just (fmap fromIntegral (SDL.windowResizedEventSize d))
                            _ -> Nothing)
                         sdlEvent))
      time <- accumB 0 (fmap (+) tick)
      let currentPower =
            fmap (\t ->
                    if t > 10
                       then 0
                       else 1000)
                 time
          mass = 68.2
          efficiency = 0.97
          forces =
            fmap (resistingForces 0 mass) speed
          powerNeeded =
            fmap (efficiency *) (liftA2 (*) forces speed)
      let ifB p a b = (\x y z -> if x then y else z) <$> p <*> a <*> b
          accelRatio =
            ifB (fmap (> 0) currentPower)
                (fmap (** 10) (liftA2 (/) (liftA2 (-) currentPower powerNeeded) currentPower))
                (pure 0)
      speed <-
        accumB 0
               ((\netPower dt v ->
                   sqrt (v * v + 2 * netPower * dt * efficiency / mass)) <$>
                netPower <@> tick)
      let netPower =
            liftA2 (-) currentPower powerNeeded
      distance <- integrate speed
      let shipPosition =
            fmap (\s ->
                    V3 0 1 (id (realToFrac s)))
                 distance
          modelTransform =
            (\s r ->
               m33_to_m44
                 (fromQuaternion
                    (axisAngle (V3 1 0 0)
                               (negate (pi / 8) * r))) !*!
               scaled (V4 1.0e-2 1.0e-2 1.0e-2 1) &
               translation .~
               s) <$>
            shipPosition <*>
            fmap realToFrac accelRatio
      let projTransform =
            fmap (\(V2 w h) ->
                    perspective 1.047
                                (w / h)
                                1
                                100)
                 (fmap (fmap fromIntegral) viewportSize)
      viewTransform <-
        do time <- integrate (pure 1)
           pure ((\t p ->
                    let r = 7
                    in lookAt (p +
                               V3 (-2)
                                  3
                                  (-8))
                              p
                              (V3 0 1 0)) <$>
                 fmap realToFrac time <*>
                 shipPosition)
      return (Scene <$> modelTransform <*> projTransform <*> viewTransform <*>
              fmap (fmap fromIntegral) viewportSize)
  where integrate b =
          accumB 0 (fmap (+) ((*) <$> b <@> tick))

newSamplingKernel :: IO [V4 Float]
newSamplingKernel =
  fmap (evalState (mapM (\i ->
                           do v <-
                                fmap normalize
                                     (V4 <$> state (randomR (-1,1)) <*>
                                      state (randomR (-1,1)) <*>
                                      state (randomR (0,1)) <*>
                                      pure 0)
                              let scale =
                                    fromIntegral (i :: Int) / (4 * 4) :: V1 Float
                              pure (v ^*
                                    case lerp 0.1 1.0 (scale * scale) of
                                      V1 x -> x))
                        [0 .. 4 * 4 + 1]))
       getStdGen

newRotations :: IO [[V4 Float]]
newRotations =
  fmap (evalState (replicateM
                     4
                     (replicateM
                        4
                        (fmap ((+ 0.5) . (^* 0.5) . normalize)
                              (V4 <$> state (randomR (-1,1)) <*>
                               state (randomR (-1,1)) <*>
                               pure 0 <*>
                               pure 0)))))
       getStdGen

calculatePowerOutput :: (Floating a, Ord a)
                     => a -- ^ Gradient of surface, between -1 (down a wall) and 1 (up a wall).
                     -> a -- ^ Combined weight of the rider, bike & clothing.
                     -> a -- ^ Velocity of the rider in m/s
                     -> a
calculatePowerOutput grade weight velocity =
  resistingForces grade weight velocity * velocity

resistingForces :: (Floating a, Ord a)
                => a -> a -> a -> a
resistingForces grade weight velocity =
  recip (1 - drivetrainLoss) * (fGravity + fDrag)
  where fGravity = g * sin (atan grade) * weight
        fDrag = 0.5 * dragCoeff * frontalArea * rho * velocity * velocity
          where dragCoeff = 0.63
                frontalArea = 0.509
                rho = 1.226
        g = 9.8067
        drivetrainLoss = 3.0e-2
