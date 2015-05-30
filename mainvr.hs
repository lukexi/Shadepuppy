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

shaderName :: String
-- shaderName = "RaymarchingPrimitives"
shaderName = "shadepuppy"

-- Initialization to set up window
main :: IO ()
main = do
  -- Initialize GLFW
  (window, events, renderHMD, resX, resY) <- reacquire 0 $ do
    hmd <- createHMD
    (resX, resY) <- getHMDResolution hmd
    (window, events) <- createWindow "ShadePuppy" resX resY
    (frameW, frameH) <- GLFW.getFramebufferSize window
    when (frameW > resX && frameH > resY) $
        GLFW.setWindowSize window (resX `div` 2) (resY `div` 2)

    renderHMD <- configureHMDRendering hmd "Scaffold"
    dismissHSWDisplay hmd
    recenterPose hmd

    
    
    return (window, events, renderHMD, resX, resY)

  shaderProg       <- createShaderProgram "shadepuppy.vert" (shaderName ++ ".frag")
  iGlobalTimeU     <- getShaderUniform shaderProg "iGlobalTime"
  iResolutionU     <- getShaderUniform shaderProg "iResolution"
  
  leftQuad  <- makeQuad shaderProg (-1, -1) (0, 1)
  rightQuad <- makeQuad shaderProg ( 0, -1) (1, 1)
  let quads = zip [0..] [leftQuad, rightQuad]
  -- let quads = zip [0..] [leftQuad]
  
  glUseProgram (unGLProgram shaderProg)
  -- Begin rendering
  forever $ do
    processEvents events $ \_ -> return ()
    -- Clear the frame
    glClearColor 0.1 0.2 0.1 0 
    glClear GL_COLOR_BUFFER_BIT

    -- Send along the current framenumber as a uniform
    globalTime <- realToFrac . utctDayTime <$> getCurrentTime
    glUniform1f (unUniformLocation iGlobalTimeU) globalTime
    glUniform2f (unUniformLocation iResolutionU) (fromIntegral resX/2) (fromIntegral resY)
    
    -- renderHMDFrame renderHMD $ \eyePoses -> do
    glClear GL_COLOR_BUFFER_BIT
    forM_ quads $ \(eyeIndex, quad) -> do
      -- (eyeOrientation, eyePosition) <- getPoses_OrientationAndPositionForEye eyePoses eyeIndex

      -- Draw the fullscreens quad
      glBindVertexArray (unVertexArrayObject (meshVAO quad))
      glDrawElements GL_TRIANGLES (meshIndexCount quad) GL_UNSIGNED_INT nullPtr
    swapBuffers window
        
 

