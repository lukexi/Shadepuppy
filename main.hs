import Prelude hiding (init) 
import Control.Applicative

import Graphics.UI.GLFW
import Graphics.Rendering.OpenGL
import Graphics.GLUtil
import Foreign.Storable (sizeOf)

-- Initialization to set up window --
main :: IO ()
main = do
  _ <- init
  applyWindowHints
  
  maybeWindow <- createWindow 640 480 "SpacePuppy" Nothing Nothing
  
  case maybeWindow of
    Nothing     -> print "Couldn't create a window :*(" 
    Just window -> do
      -- Enable VSync
      swapInterval 1
      makeContextCurrent ( Just window )

      shader <- initResources

      mainLoop window shader 0

-- Actually Starting once window is up --
mainLoop :: Window -> SPShader -> GLfloat -> IO a
mainLoop window shader frameNumber = do
  -- Clear the frame
  clearColor $= Color4 0.1 0.2 0.1 0 
  clear [ ColorBuffer ]
  
  -- Make sure our viewport matches the latest window size
  (width, height) <- getFramebufferSize window
  viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
  
  -- Send along the current framenumber as a uniform
  uniform (spsFrameNumberU shader) $= Index1 frameNumber
  
  -- Draw the fullscreens quad
  drawElements TriangleStrip (fromIntegral (length elementBufferData)) UnsignedInt offset0

  -- Swap buffers, poll events, and start rendering the next frame
  swapBuffers window
  pollEvents
  mainLoop window shader ( frameNumber + 1 )
 

-- Bookkeeping

data SPShader = SPShader { spsProgram      :: Program
                         , spsPositionA    :: AttribLocation
                         , spsFrameNumberU :: UniformLocation
                         }

-- Vertices for a fullscreen quad
vertexBufferData :: [ GLfloat ]
vertexBufferData =  [ -1 , -1 
                    , 1  , -1 
                    , -1 , 1 
                    , 1  , 1 ]

-- Indices to render a fullscreen quad as a TriangleStrip
elementBufferData :: [ GLuint ]
elementBufferData =  [ 0..3   ]

initShader :: IO SPShader
initShader = do 
  vs <- loadShader VertexShader   "spacepuppy.vert"
  fs <- loadShader FragmentShader "spacepuppy.frag"
  p  <- linkShaderProgram [vs, fs]
  SPShader p
    <$> get (attribLocation p "position")
    <*> get (uniformLocation p "frameNumber")

initResources :: IO SPShader
initResources = do
  shader   <- initShader
  vertices <- makeBuffer ArrayBuffer vertexBufferData
  indices  <- makeBuffer ElementArrayBuffer elementBufferData

  -- Create a VAO, as required by the OpenGL Core Context
  vao <- makeVAO $ do
    let numComponents = 2 -- We only use X & Y since this is a fullscreen quad
        stride    = fromIntegral $ sizeOf (undefined::GLfloat) * fromIntegral numComponents
        vad       = VertexArrayDescriptor numComponents Float stride offset0
        positionA = spsPositionA shader
    -- Bind our vertices
    bindBuffer ArrayBuffer        $= Just vertices
    vertexAttribArray positionA   $= Enabled
    vertexAttribPointer positionA $= (ToFloat, vad)
    -- Bind the indices to render the quad as a triangle strip
    bindBuffer ElementArrayBuffer $= Just indices

  -- Bind our VAO
  bindVertexArrayObject $= Just vao
  
  -- Bind our shader
  currentProgram $= Just (spsProgram shader)

  return shader

applyWindowHints :: IO ()
applyWindowHints = do
  windowHint $ WindowHint'ClientAPI ClientAPI'OpenGL
  windowHint $ WindowHint'OpenGLForwardCompat True
  windowHint $ WindowHint'OpenGLProfile OpenGLProfile'Core
  windowHint $ WindowHint'ContextVersionMajor 3
  windowHint $ WindowHint'ContextVersionMinor 2