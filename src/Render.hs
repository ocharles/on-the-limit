{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
module Render where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict (evalStateT)
import Control.Monad.Trans.State.Strict (gets, modify, StateT)
import Data.Distributive (distribute)
import Data.Foldable (traverse_)
import Data.List (sort)
import Foreign hiding (new)
import Foreign.C
import GLObjects
import Graphics.GL.ARB.SeparateShaderObjects
import Graphics.GL.Core33
import Linear

data DrawCommand =
  DrawCommand {dcVertexArrayObject :: VertexArrayObject
              ,dcProgram :: Program
              ,dcTextures :: [Texture]
              ,dcModelTransform :: M44 Float
              ,dcViewTransform :: M44 Float
              ,dcProjectionTransform :: M44 Float
              ,dcUniforms :: [Uniform]
              ,dcNElements :: GLint}
  deriving (Eq,Ord)

data Pass =
  Pass {passFramebuffer :: Framebuffer
       ,passViewport :: (GLint,GLint,GLint,GLint)}
  deriving (Eq, Ord)

data CurrentState =
  CurrentState {currentProgram :: Program
               ,currentVertexArrayObject :: VertexArrayObject}

data Uniform where
  Uniform :: UniformSetter a -> String -> a -> Uniform

-- Super suspicious
instance Eq Uniform where
  Uniform _ k _ == Uniform _ k' _ = k == k'

instance Ord Uniform where
  compare (Uniform _ k _) (Uniform _ k' _) = compare k k'

initialState :: CurrentState
initialState = CurrentState (Program 0) (VertexArrayObject 0)

pass :: Pass -> [DrawCommand] -> IO ()
pass _ [] = return ()
pass (Pass (Framebuffer fboName) (x,y,w,h)) drawCommands =
  do glBindFramebuffer GL_FRAMEBUFFER fboName
     glViewport x y w h
     glClearColor 1 1 1 1
     glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
     evalStateT (traverse_ dispatch (sort drawCommands))
                initialState
  where dispatch DrawCommand{..} =
          do bind' bindVertexArray currentVertexArrayObject dcVertexArrayObject
             bind' bindProgram currentProgram dcProgram
             program <- gets currentProgram
             liftIO $
               do sequence_ (zipWith (\n (Texture texture) ->
                                        do glActiveTexture (GL_TEXTURE0 + n)
                                           glBindTexture GL_TEXTURE_2D texture)
                                     [0 ..]
                                     dcTextures)
                  mapM_ (\(Uniform setter name value) ->
                           setUniform setter program name value)
                        dcUniforms
                  setUniform m44 program "u_model" dcModelTransform
                  setUniform m44 program "u_view" dcViewTransform
                  setUniform m44 program "u_proj" dcProjectionTransform
                  setUniform v4i
                             program
                             "u_viewport"
                             (V4 x y w h)
                  setUniform
                    m44
                    program
                    "u_projViewModel"
                    (dcProjectionTransform !*! dcViewTransform !*!
                     dcModelTransform)
                  setUniform m44
                             program
                             "u_viewModel"
                             (dcViewTransform !*! dcModelTransform)
                  setUniform
                    m44
                    program
                    "u_viewModelIT"
                    (distribute (inv44 (dcViewTransform !*! dcModelTransform)))
                  setUniform m44
                             program
                             "u_projViewInv"
                             (inv44 (dcProjectionTransform !*! dcViewTransform))
                  setUniform m44
                             program
                             "u_projView"
                             (dcProjectionTransform !*! dcViewTransform)
                  glDrawElements GL_TRIANGLES dcNElements GL_UNSIGNED_INT nullPtr
        bind' :: Eq a
              => (a -> StateT CurrentState IO ())
              -> (CurrentState -> a)
              -> a
              -> StateT CurrentState IO ()
        bind' f current new =
          do a <- gets current
             when (a /= new)
                  (f new)

bindVertexArray :: VertexArrayObject -> StateT CurrentState IO ()
bindVertexArray vao =
  do modify (\s -> s {currentVertexArrayObject = vao})
     liftIO (glBindVertexArray (vertexArrayObjectName vao))

bindProgram :: Program -> StateT CurrentState IO ()
bindProgram program =
  do modify (\s -> s {currentProgram = program})
     liftIO (glUseProgram (programName program))
