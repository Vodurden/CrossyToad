module CrossyToad.Effect.Renderer.SDL.Renderer
  ( runRenderCommand
  ) where

import           Control.Lens
import           Control.Monad.Reader (MonadReader)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Degrees (Degrees)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import           Foreign.C.Types (CInt)
import           Linear.V2
import qualified SDL
import qualified SDL.Font as Font

import           CrossyToad.Effect.Renderer.FontAsset
import           CrossyToad.Effect.Renderer.ImageAsset
import           CrossyToad.Effect.Renderer.Clip (Clip)
import qualified CrossyToad.Effect.Renderer.Clip as Clip
import           CrossyToad.Effect.Renderer.RGBAColour
import           CrossyToad.Effect.Renderer.RenderCommand
import           CrossyToad.Effect.Renderer.SDL.Env
import           CrossyToad.Effect.Renderer.SDL.Fonts (HasFonts(..))
import qualified CrossyToad.Effect.Renderer.SDL.Fonts as Fonts
import           CrossyToad.Effect.Renderer.SDL.Texture (Texture, HasTexture(..))
import qualified CrossyToad.Effect.Renderer.SDL.Texture as Texture
import           CrossyToad.Effect.Renderer.SDL.Textures (HasTextures(..))
import qualified CrossyToad.Effect.Renderer.SDL.Textures as Textures
import           CrossyToad.Geometry.Position
import           CrossyToad.Geometry.Size

runRenderCommand :: (MonadReader r m, HasEnv r, MonadIO m)
                 => RenderCommand
                 -> m ()
runRenderCommand ClearScreen = clearScreen
runRenderCommand DrawScreen = drawScreen
runRenderCommand (Draw asset degrees tClip sClip) =
  draw asset degrees tClip sClip
runRenderCommand (DrawAt asset pos) = drawAt asset pos
runRenderCommand (DrawText asset degrees tClip sClip colour text) =
  drawText asset degrees tClip sClip colour text

clearScreen :: (MonadReader r m, HasEnv r, MonadIO m) => m ()
clearScreen = view renderer >>= SDL.clear

drawScreen :: (MonadReader r m, HasEnv r, MonadIO m) => m ()
drawScreen = view renderer >>= SDL.present

draw ::
  ( MonadReader r m
  , HasEnv r
  , MonadIO m)
  => ImageAsset
  -> (Maybe Degrees)
  -> (Maybe Clip)
  -> (Maybe Clip)
  -> m ()
draw asset' degrees textureClip screenClip = do
    textures' <- view (env.textures)
    let texture' = Textures.fromImageAsset asset' textures'
    drawTexture texture' degrees textureClip screenClip

drawAt ::
  ( MonadReader r m
  , HasEnv r
  , MonadIO m)
  => ImageAsset
  -> Position
  -> m ()
drawAt asset' pos = do
  textures' <- view (env.textures)
  let texture' = Textures.fromImageAsset asset' textures'
  let wh = V2 (texture' ^. Texture.width) (texture' ^. Texture.height)
  let screenClip = Clip.mkAt pos (fromIntegral <$> wh)
  drawTexture texture' Nothing Nothing (Just screenClip)

drawText ::
  ( MonadReader r m
  , HasEnv r
  , MonadIO m)
  => FontAsset
  -> (Maybe Degrees)
  -> (Maybe Clip)
  -> (Maybe Clip)
  -> RGBAColour
  -> Text
  -> m ()
drawText asset' degrees textureClip screenClip colour message = do
  fonts' <- view (env.fonts)
  let font' = Fonts.fromFontAsset asset' fonts'
  surface <- Font.blended font' colour message

  renderer' <- view (env.renderer)
  sdlTexture' <- SDL.createTextureFromSurface renderer' surface
  texture' <- Texture.fromSDL sdlTexture'
  drawTexture texture' degrees textureClip screenClip

drawTexture ::
  ( MonadReader r m
  , HasEnv r
  , MonadIO m)
  => Texture
  -> (Maybe Double)
  -> (Maybe Clip)
  -> (Maybe Clip)
  -> m ()
drawTexture texture' degrees textureClip targetClip = do
    renderer' <- view (env.renderer)
    SDL.copyEx
      renderer'
      (texture' ^. sdlTexture)
      (fromClip <$> textureClip)
      (fromClip <$> targetClip)
      (realToFrac $ fromMaybe 0 degrees)
      Nothing
      (V2 False False)
  where
    fromClip :: Clip -> SDL.Rectangle CInt
    fromClip clip =
      let xy = truncate <$> clip ^. position
          wh = truncate <$> clip ^. size
      in SDL.Rectangle (SDL.P xy) wh
