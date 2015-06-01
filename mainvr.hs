import Prelude hiding (init) 
import Control.Monad
import Graphics.UI.GLFW.Pal
import Graphics.GL.Pal
import Graphics.GL
import Data.Time
import Quad
import Foreign.Ptr
import Graphics.Oculus
import Linear
shaderName :: String
-- shaderName = "RaymarchingPrimitives"
-- shaderName = "shadepuppy"
shaderName = "MengerSponge"

-- Initialization to set up window
main :: IO ()
main = do
  -- Initialize GLFW
  hmd <- createHMD
  (resX, resY) <- getHMDResolution hmd
  
  -- (window, events) <- createWindow "ShadePuppy" (resX`div`2) (resY`div`2)
  (window, events) <- createWindow "ShadePuppy" resX resY
  -- Compensate for retina framebuffers on Mac
  -- (frameW, frameH) <- GLFW.getFramebufferSize window
  -- when (frameW > resX && frameH > resY) $
  --     GLFW.setWindowSize window (resX `div` 2) (resY `div` 2)

  renderHMD <- configureHMDRendering hmd "ShadePuppy"
  dismissHSWDisplay hmd
  recenterPose hmd

  glClearColor 0.0 0.1 0.1 0
  glEnable GL_DEPTH_TEST
  shaderProg      <- createShaderProgram "shadepuppy.vert" (shaderName ++ ".frag")
  iGlobalTime     <- getShaderUniform shaderProg "iGlobalTime"
  iResolution     <- getShaderUniform shaderProg "iResolution"
  iEyeOrientation <- getShaderUniform shaderProg "iEyeOrientation"
  iEyePosition    <- getShaderUniform shaderProg "iEyePosition"
  
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

        let (x,y,w,h)     = eyeViewport eye
        glViewport x y w h

        (eyeOrientation, eyePosition) <- getPoses_OrientationAndPositionForEye eyePoses (eyeIndex eye)
        let V3 px py pz = eyePosition

        let orientMat = fromQuaternion eyeOrientation
        -- uniformM33 iEyeOrientation (fromMaybe orientMat (inv33 orientMat))
        uniformM33 iEyeOrientation orientMat
        glUniform3f (unUniformLocation iEyePosition)    px py pz

        glDrawElements GL_TRIANGLES (meshIndexCount quad) GL_UNSIGNED_INT nullPtr
        
 

