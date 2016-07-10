{-# LANGUAGE RecordWildCards #-}
import Prelude hiding (init)
import Graphics.UI.GLFW.Pal
import Graphics.GL.Pal
import Data.Time
import Halive.Utils
import Quad
import Control.Monad
import System.FilePath
import Shader

shaderName :: String
--shaderName = "shadepuppy" -- default shader
--shaderName = "CreationBySilexars"
--shaderName = "ImpactByCabbibo"
shaderName = "RaymarchingPrimitives"
--shaderName = "texture-test"
--shaderName = "meta"

-- Initialization to set up window
main :: IO ()
main = do
  -- Initialize GLFW
  (window, _, events)  <- reacquire 0 $ createWindow "ShadePuppy" 640 480

  ShadepuppyShader{..} <- assembleShaderWithFooter "normalFooter.frag" ("shaders" </> shaderName)

  texture0             <- loadTexture ("textures" </> "tex16.png") SRGB

  quad                 <- makeQuad shadepuppyProgram

  glBindVertexArray (unVertexArrayObject (meshVAO quad))
  glUseProgram (unProgram shadepuppyProgram)

  glUniform1i (unUniformLocation iChannel0) 0
  glActiveTexture GL_TEXTURE0
  glBindTexture GL_TEXTURE_2D (unTextureID texture0)

  -- Begin rendering
  whileWindow window $ do
    es <- gatherEvents events
    forM_ es (closeOnEscape window)

    -- Clear the frame
    glClearColor 0.1 0.2 0.1 0
    glClear GL_COLOR_BUFFER_BIT

    -- Make sure our viewport matches the latest window size
    (width, height) <- getFramebufferSize window
    glViewport 0 0 (fromIntegral width) (fromIntegral height)

    -- Send along the current framenumber as a uniform
    globalTime <- realToFrac . utctDayTime <$> getCurrentTime
    glUniform1f (unUniformLocation iGlobalTime) globalTime
    glUniform2f (unUniformLocation iResolution) (fromIntegral width) (fromIntegral height)
    (cursorX, cursorY) <- getCursorPos window
    glUniform4f (unUniformLocation iMouse) cursorX cursorY 0 0

    -- Draw the fullscreens quad
    glDrawElements GL_TRIANGLES (meshIndexCount quad) GL_UNSIGNED_INT nullPtr

    -- Swap buffers, poll events, and start rendering the next frame
    swapBuffers window


