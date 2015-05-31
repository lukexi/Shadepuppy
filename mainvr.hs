import Prelude hiding (init) 
import Control.Applicative
import Control.Monad
import Graphics.UI.GLFW.Pal
import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL.Pal
import Graphics.GL
import Foreign.Storable (sizeOf)
import Data.Time
import Halive.Utils
import Quad
import Foreign.Ptr
import Graphics.Oculus
import Linear
import Data.Maybe
shaderName :: String
-- shaderName = "RaymarchingPrimitives"
-- shaderName = "shadepuppy"
shaderName = "MengerSponge"

-- Initialization to set up window
main :: IO ()
main = do
  -- Initialize GLFW
  (window, events, renderHMD, resX, resY) <- reacquire 0 $ do
    hmd <- createHMD
    (resX, resY) <- getHMDResolution hmd
    
    (window, events) <- createWindow "ShadePuppy" (resX`div`2) (resY`div`2)
    -- Compensate for retina framebuffers on Mac
    (frameW, frameH) <- GLFW.getFramebufferSize window
    when (frameW > resX && frameH > resY) $
        GLFW.setWindowSize window (resX `div` 2) (resY `div` 2)

    renderHMD <- configureHMDRendering hmd "Scaffold"
    dismissHSWDisplay hmd
    recenterPose hmd

    
    
    return (window, events, renderHMD, resX, resY)

  shaderProg      <- createShaderProgram "shadepuppy.vert" (shaderName ++ ".frag")
  iGlobalTime     <- getShaderUniform shaderProg "iGlobalTime"
  iResolution     <- getShaderUniform shaderProg "iResolution"
  iEyeOrientation <- getShaderUniform shaderProg "iEyeOrientation"
  iEyePosition    <- getShaderUniform shaderProg "iEyePosition"
  
  quad            <- makeQuad shaderProg
  glBindVertexArray (unVertexArrayObject (meshVAO quad))
  glUseProgram (unGLProgram shaderProg)
  -- Begin rendering
  forever $ do
    processEvents events $ \_ -> return ()
    -- Clear the frame
    glClearColor 0.1 0.2 0.1 0 
    glClear GL_COLOR_BUFFER_BIT

    -- Send along the current framenumber as a uniform
    globalTime <- realToFrac . utctDayTime <$> getCurrentTime
    glUniform1f (unUniformLocation iGlobalTime) globalTime
    glUniform2f (unUniformLocation iResolution) (fromIntegral resX/2) (fromIntegral resY)
    
    renderHMDFrame renderHMD $ \eyePoses -> do
      glClear GL_COLOR_BUFFER_BIT
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
        
 

