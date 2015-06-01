{-# LANGUAGE RecordWildCards #-}
import Prelude hiding (init) 
import Control.Monad
import Graphics.UI.GLFW.Pal
import Graphics.GL.Pal
import Graphics.GL
import Data.Time
import Quad
import Foreign.Ptr
import Foreign
import Graphics.Oculus
import Linear
import Control.Lens
import Data.Maybe

shaderName :: String
-- shaderName = "RaymarchingPrimitives"
-- shaderName = "shadepuppy"
-- shaderName = "MengerSponge"
shaderName = "Sierpinski"

-- Initialization to set up window
main :: IO ()
main = do
  -- Initialize GLFW
  hmd <- createHMD
  (resX, resY) <- getHMDResolution hmd
  
  (window, events) <- createWindow "ShadePuppy" resX resY
  -- Compensate for retina framebuffers on Mac
  (frameW, frameH) <- getFramebufferSize window
  when (frameW > resX && frameH > resY) $
      setWindowSize window (resX `div` 2) (resY `div` 2)

  renderHMD <- configureHMDRendering hmd "ShadePuppy"
  dismissHSWDisplay hmd
  recenterPose hmd

  glClearColor 0.0 0.1 0.1 0
  glEnable GL_DEPTH_TEST
  shaderProg      <- createShaderProgram "shadepuppy.vert" (shaderName ++ ".frag")
  iGlobalTime     <- getShaderUniform shaderProg "iGlobalTime"
  iResolution     <- getShaderUniform shaderProg "iResolution"
  unCorners       <- getShaderUniform shaderProg "unCorners"
  unViewport      <- getShaderUniform shaderProg "unViewport"
  
  quad            <- makeQuad shaderProg
  glBindVertexArray (unVertexArrayObject (meshVAO quad))
  glUseProgram (unGLProgram shaderProg)
  -- Begin rendering
  whileWindow window $ do
    processEvents events (closeOnEscape window)    
    renderHMDFrame renderHMD $ \eyePoses -> do

      glClear ( GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT )

      -- Send along the current framenumber as a uniform
      glUniform1f (unUniformLocation iGlobalTime) =<< realToFrac . utctDayTime <$> getCurrentTime
      glUniform2f (unUniformLocation iResolution) (fromIntegral resX/2) (fromIntegral resY)

      forM_ (renEyes renderHMD) $ \eye -> do
        let (x,y,w,h) = eyeViewport eye
        glViewport x y w h


        let FOVPort{..} = eyeFOV eye;
            corA0 = point $ V3 (-fovpLeftTan)  (-fovpDownTan) (-1)
            corB0 = point $ V3 ( fovpRightTan) (-fovpDownTan) (-1)
            corC0 = point $ V3 ( fovpRightTan) ( fovpUpTan)   (-1)
            corD0 = point $ V3 (-fovpLeftTan)  ( fovpUpTan)   (-1)
            apex0 = point $ V3 0 0 0

        (eyeOrientation, eyePosition) <- getPoses_OrientationAndPositionForEye eyePoses (eyeIndex eye)
        let eyeCameraMat = mkTransformation eyeOrientation eyePosition
            eyeInvMat    = fromMaybe eyeCameraMat (inv44 eyeCameraMat)
            corA         = eyeInvMat !* corA0
            corB         = eyeInvMat !* corB0
            corC         = eyeInvMat !* corC0
            corD         = eyeInvMat !* corD0
            apex         = eyeInvMat !* apex0

        let corners = [ corA ^. _x, corA ^. _y, corA ^. _z, 
                        corB ^. _x, corB ^. _y, corB ^. _z, 
                        corC ^. _x, corC ^. _y, corC ^. _z, 
                        corD ^. _x, corD ^. _y, corD ^. _z,
                        apex ^. _x, apex ^. _y, apex ^. _z ]
        withArrayLen corners (\len -> glUniform3fv (unUniformLocation unCorners) (fromIntegral len))

        let viewport = map fromIntegral [x,y,w,h]
        withArrayLen viewport (\len -> glUniform4fv (unUniformLocation unViewport) (fromIntegral len))

        glDrawElements GL_TRIANGLES (meshIndexCount quad) GL_UNSIGNED_INT nullPtr
        
