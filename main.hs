import Prelude hiding (init) 
import Control.Applicative
import Control.Monad
import Graphics.UI.GLFW.Pal
import Graphics.GL.Pal
import Graphics.GL
import Foreign.Storable (sizeOf)
import Data.Time
import Halive.Utils
import Quad
import Foreign.Ptr

shaderName :: String
-- shaderName = "shadepuppy" -- default shader
-- shaderName = "CreationBySilexars"
-- shaderName = "ImpactByCabbibo"
shaderName = "RaymarchingPrimitives"

-- Initialization to set up window
main :: IO ()
main = do
  -- Initialize GLFW
  (window, events) <- reacquire 0 $ createWindow "ShadePuppy" 640 480

  shaderProg       <- createShaderProgram "shadepuppy.vert" (shaderName ++ ".frag")
  iGlobalTimeU     <- getShaderUniform shaderProg "iGlobalTime"
  iResolutionU     <- getShaderUniform shaderProg "iResolution"

  quad <- makeQuad shaderProg

  glBindVertexArray (unVertexArrayObject (meshVAO quad))
  glUseProgram (unGLProgram shaderProg)
  -- Begin rendering
  forever $ do
    -- Clear the frame
    glClearColor 0.1 0.2 0.1 0 
    glClear GL_COLOR_BUFFER_BIT
    
    -- Make sure our viewport matches the latest window size
    (width, height) <- getFramebufferSize window
    glViewport 0 0 (fromIntegral width) (fromIntegral height)
    
    -- Send along the current framenumber as a uniform
    globalTime <- realToFrac . utctDayTime <$> getCurrentTime
    glUniform1f (unUniformLocation iGlobalTimeU) globalTime
    glUniform2f (unUniformLocation iResolutionU) (fromIntegral width) (fromIntegral height)
    
    -- Draw the fullscreens quad
    glDrawElements GL_TRIANGLES (meshIndexCount quad) GL_UNSIGNED_INT nullPtr

    -- Swap buffers, poll events, and start rendering the next frame
    swapBuffers window
    processEvents events $ \_ -> return ()
 

