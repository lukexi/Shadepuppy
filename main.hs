import Prelude hiding (init) 
import Control.Applicative

import Graphics.UI.GLFW
import Graphics.Rendering.OpenGL
import Graphics.GLUtil
import Foreign.Storable (sizeOf)

-- Initialization to set up window --
main :: IO ()
main = do
  -- Initialize GLFW
  _ <- init
  applyWindowHints
  
  maybeWindow <- createWindow 640 480 "SpacePuppy" Nothing Nothing
  
  case maybeWindow of
    Nothing     -> print "Couldn't create a window :*(" 
    Just window -> do
      -- Enable VSync
      swapInterval 1

      makeContextCurrent ( Just window )

      -- Create and bind our shader + a VAO holding the fullscreen quad geometry
      shader <- setupResources

      -- Begin rendering
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
  drawElements TriangleStrip elementLength UnsignedInt offset0

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

-- Describe the vertex layout
vertexDescriptor :: VertexArrayDescriptor a
vertexDescriptor = VertexArrayDescriptor vertexComponents Float vertexStride offset0
  where
    -- We only use X & Y for a fullscreen quad
    vertexComponents :: Num a => a
    vertexComponents = 2

    vertexStride :: Num a => a
    vertexStride = vertexComponents * fromIntegral (sizeOf (undefined::GLfloat))

-- Indices to render a fullscreen quad as a TriangleStrip
elementBufferData :: [ GLuint ]
elementBufferData =  [ 0..3   ]

elementLength :: NumArrayIndices
elementLength = fromIntegral (length elementBufferData)

-- | Load our shaders and find their attribute/uniform locations
initShader :: IO SPShader
initShader = do 
  vs <- loadShader VertexShader   "spacepuppy.vert"
  fs <- loadShader FragmentShader "spacepuppy.frag"
  p  <- linkShaderProgram [vs, fs]
  SPShader p
    <$> get (attribLocation p "position")
    <*> get (uniformLocation p "frameNumber")

setupResources :: IO SPShader
setupResources = do
  shader   <- initShader
  vertices <- makeBuffer ArrayBuffer vertexBufferData
  indices  <- makeBuffer ElementArrayBuffer elementBufferData

  -- Create a VAO for our geometry, as required by the OpenGL Core Context
  vao <- makeVAO $ do
    let positionA = spsPositionA shader
    -- Bind the vertex buffer
    bindBuffer ArrayBuffer        $= Just vertices
    -- Indicate where and how to provide the vertexes
    vertexAttribArray   positionA $= Enabled
    vertexAttribPointer positionA $= (ToFloat, vertexDescriptor)
    -- Bind the indices to render the quad as a triangle strip
    bindBuffer ElementArrayBuffer $= Just indices

  -- Bind our VAO
  bindVertexArrayObject $= Just vao
  
  -- Bind our shader
  currentProgram $= Just (spsProgram shader)

  return shader

-- Window hints required for using OpenGL on Mac
applyWindowHints :: IO ()
applyWindowHints = do
  windowHint $ WindowHint'ClientAPI ClientAPI'OpenGL
  windowHint $ WindowHint'OpenGLForwardCompat True
  windowHint $ WindowHint'OpenGLProfile OpenGLProfile'Core
  windowHint $ WindowHint'ContextVersionMajor 3
  windowHint $ WindowHint'ContextVersionMinor 2