{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NegativeLiterals #-}
import Prelude hiding (init)
import Control.Monad
import Graphics.UI.GLFW.Pal
import Graphics.GL.Pal
import Graphics.GL
import Graphics.VR.Pal
import Data.Time
import Quad
import Foreign.Ptr
import Foreign
import Linear
import Control.Lens
import System.FilePath
import Halive.Utils
import Shader
import Data.Foldable

-- import Data.Maybe

shaderName :: String
shaderName = "greenvoid"
--shaderName = "minimal"
--shaderName = "RaymarchingPrimitives"
--shaderName = "MengerSponge"
--shaderName = "shadepuppy"
--shaderName = "Sierpinski"
--shaderName = "meta"
--shaderName = "rods"
--shaderName = "deathCube"
--shaderName = "face"
--shaderName = "iriTest1"

-- Initialization to set up window
main :: IO ()
main = do
    vrPal@VRPal{..} <- reacquire 0 $ initVRPalWithConfig "ShadePuppy"
        defaultVRPalConfig { vpcMSAASamples = MSAASamples1 }

    glClearColor 0.0 0.1 0.1 0
    glEnable GL_DEPTH_TEST
    ShadepuppyShader{..} <- assembleShaderWithFooter "vrFooter.frag" ("shaders" </> shaderName)

    quad            <- makeQuad shadepuppyProgram
    glBindVertexArray (unVertexArrayObject (meshVAO quad))
    glUseProgram (unProgram shadepuppyProgram)

    -- Begin rendering
    whileWindow gpWindow $ do
        (headM44, events) <- tickVR vrPal identity

        forM_ events $ \case
            GLFWEvent e -> closeOnEscape gpWindow e
            _ -> return ()

        renderWith vrPal headM44 $ \projMat viewM44 rawProjection viewport -> do
            glClear ( GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT )

            -- Send along the current framenumber as a uniform
            glUniform1f (unUniformLocation iGlobalTime) =<< realToFrac . utctDayTime <$> getCurrentTime
            let (resX, resY) = (viewport ^. _z, viewport ^. _w)
            glUniform2f (unUniformLocation iResolution) resX resY

            (cursorX, cursorY) <- getCursorPos gpWindow
            glUniform4f (unUniformLocation iMouse) cursorX cursorY 0 0

            let fovpLeftTan  = rawProjection ^. _x
                fovpRightTan = rawProjection ^. _y
                fovpDownTan  = rawProjection ^. _z
                fovpUpTan    = rawProjection ^. _w

            let eyeInvMat    = inv44 viewM44

                corA0 = point $ V3 fovpLeftTan  fovpDownTan -1
                corB0 = point $ V3 fovpRightTan fovpDownTan -1
                corC0 = point $ V3 fovpRightTan fovpUpTan   -1
                corD0 = point $ V3 fovpLeftTan  fovpUpTan   -1
                apex0 = point $ V3 0 0 0


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

            withArray corners (glUniform3fv (unUniformLocation unCorners) 5)

            let viewportL = toList viewport
            withArray viewportL (glUniform4fv (unUniformLocation unViewport) 1)

            glDrawElements GL_TRIANGLES (meshIndexCount quad) GL_UNSIGNED_INT nullPtr



