module CrossyToad.Asset.Sprite.Toad where

filename :: String
filename = "toad.png"

data ToadAnimation
  = Idle
  | Jumping

-- animationClips :: ToadAnimation -> [TextureClip]
