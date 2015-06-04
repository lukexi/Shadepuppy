import Prelude hiding (init) 
import Control.Monad
import Graphics.UI.GLFW.Pal
import Graphics.GL.Pal
import Graphics.GL
import Data.Time
import Halive.Utils
import Quad
import Foreign.Ptr
import qualified Data.Text.IO as Text
import Data.Monoid

shaderName :: String
-- shaderName = "shadepuppy" -- default shader
-- shaderName = "CreationBySilexars"
-- shaderName = "ImpactByCabbibo"
-- shaderName = "RaymarchingPrimitives"
shaderName = "texture-test"

assembleShader :: IO GLProgram
assembleShader = do
    let fragFile = (shaderName ++ ".frag")
    fragSource <- Text.readFile fragFile

    -- Add the needed uniforms and main function from the header and footer
    fragHeader <- Text.readFile "vrHeader.frag"
    fragFooter <- Text.readFile "normalFooter.frag"
    let fullFragSource = fragHeader <> fragSource <> fragFooter

    -- We use a standard vert shader to position the full screen quad
    vertSource <- Text.readFile "shadepuppy.vert"
    createShaderProgramFromSources "shadepuppy.vert" vertSource fragFile fullFragSource

-- Initialization to set up window
main :: IO ()
main = do
  -- Initialize GLFW
  (window, events) <- reacquire 0 $ createWindow "ShadePuppy" 640 480

  shaderProg       <- assembleShader
  iGlobalTimeU     <- getShaderUniform shaderProg "iGlobalTime"
  iResolutionU     <- getShaderUniform shaderProg "iResolution"
  iChannel0        <- getShaderUniform shaderProg "iChannel0"

  texture0         <- loadTexture "tex16.png" SRGB
  
  quad             <- makeQuad shaderProg
  
  glBindVertexArray (unVertexArrayObject (meshVAO quad))
  glUseProgram (unGLProgram shaderProg)

  glUniform1i (unUniformLocation iChannel0) 0
  glActiveTexture GL_TEXTURE0
  glBindTexture GL_TEXTURE_2D (unTextureObject texture0)
  -- glBindSampler 0 linearFiltering

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
 

