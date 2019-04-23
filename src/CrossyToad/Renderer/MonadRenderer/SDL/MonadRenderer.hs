module CrossyToad.Renderer.MonadRenderer.SDL.MonadRenderer
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

import           CrossyToad.Renderer.Asset.FontAsset
import           CrossyToad.Renderer.Asset.ImageAsset
import           CrossyToad.Renderer.Clip (Clip)
import qualified CrossyToad.Renderer.Clip as Clip
import           CrossyToad.Renderer.RGBAColour
import           CrossyToad.Renderer.RenderCommand
import           CrossyToad.Renderer.MonadRenderer.SDL.Env
import           CrossyToad.Renderer.MonadRenderer.SDL.Fonts (HasFonts(..))
import qualified CrossyToad.Renderer.MonadRenderer.SDL.Fonts as Fonts
import           CrossyToad.Renderer.MonadRenderer.SDL.Texture (Texture, HasTexture(..))
import qualified CrossyToad.Renderer.MonadRenderer.SDL.Texture as Texture
import           CrossyToad.Renderer.MonadRenderer.SDL.Textures (HasTextures(..))
import qualified CrossyToad.Renderer.MonadRenderer.SDL.Textures as Textures
import           CrossyToad.Geometry.Position
import           CrossyToad.Geometry.Size

runRenderCommand :: (MonadReader r m, HasEnv r, MonadIO m)
                 => RenderCommand
                 -> m ()
runRenderCommand ClearScreen = clearScreen
runRenderCommand DrawScreen = drawScreen
runRenderCommand (Draw asset tClip sClip degrees flip') =
  draw asset tClip sClip degrees flip'
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
  -> Maybe Clip
  -> Maybe Clip
  -> Maybe Degrees
  -> Maybe (V2 Bool)
  -> m ()
draw asset' textureClip screenClip degrees flip' = do
    textures' <- view (env.textures)
    let texture' = Textures.fromImageAsset asset' textures'
    drawTexture texture' textureClip screenClip degrees flip'

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
  drawTexture texture' Nothing (Just screenClip) Nothing Nothing

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
  drawTexture texture' textureClip screenClip degrees Nothing

drawTexture ::
  ( MonadReader r m
  , HasEnv r
  , MonadIO m)
  => Texture
  -> Maybe Clip
  -> Maybe Clip
  -> Maybe Double
  -> Maybe (V2 Bool)
  -> m ()
drawTexture texture' textureClip targetClip degrees flip' = do
    renderer' <- view (env.renderer)
    SDL.copyEx
      renderer'
      (texture' ^. sdlTexture)
      (fromClip <$> textureClip)
      (fromClip <$> targetClip)
      (realToFrac $ fromMaybe 0 degrees)
      Nothing
      (fromMaybe (V2 False False) flip')
  where
    fromClip :: Clip -> SDL.Rectangle CInt
    fromClip clip =
      let xy = round <$> clip ^. position
          wh = round <$> clip ^. size
      in SDL.Rectangle (SDL.P xy) wh
