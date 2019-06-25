module WholePixels.Target where

import Relude
import Relude.Extra.Enum
import Data.List (isInfixOf)
import WholePixels.Geometry

data Target
    = Instagram
    | Wallpaper_540p
    | Wallpaper_1080p
    | Wallpaper_4K
    | Wallpaper_5K
    | Wallpaper_8K
    | Wallpaper_iPhone_Xs
    | Wallpaper_iPhone_Xs_Max
    | Wallpaper_iPhone_Xr
    | Wallpaper_iPhone_6_7
    | Wallpaper_iPhone_6_7_Plus
    | Wallpaper_iPhone_5
    | Wallpaper_Galaxy_S10
    | Wallpaper_Galaxy_S10e
    | Apparel
    | WallPortrait
    | WallLandscape
    | WallSquare
    | Duvet
    | Blanket
    | Pillow
    | RugPortrait
    | RugLandscape
    | TapestryPortrait
    | TapestryLandscape
    | BathMat
    | ShowerCurtain
    | BeachTowelPortrait
    | BeachTowelLandscape
    | DrawstringBag
    | ToteBag
    | WeekenderBag
    | LaundryBag
    | ZipPouch
    | Journal5'7
    | Spiral6'8
    | OfficeMug
    | TravelMug
    | WaterBottle
    | PhoneCase
    | LaptopCover
    | Banner
    | NarrowBanner
    | Sticker
    | PolkaDotDress
    deriving (Show, Enum, Bounded)

dimensions :: Target -> (Int, Int)
dimensions Instagram = (1000, 1000)
dimensions Sticker = (2560, 2560)
dimensions Wallpaper_540p = (div 1920 2, div 1080 2)
dimensions Wallpaper_1080p = (1920, 1080)
dimensions Wallpaper_4K = (2 * 1920, 2 * 1080)
dimensions Wallpaper_5K = (2 * 2560, 2 * 1440)
dimensions Wallpaper_8K = (4 * 1920, 4 * 1080)
dimensions Wallpaper_iPhone_Xs = (1125, 2436)
dimensions Wallpaper_iPhone_Xs_Max = (1242, 2688)
dimensions Wallpaper_iPhone_Xr = (828, 1792)
dimensions Wallpaper_iPhone_6_7 = (750, 1334)
dimensions Wallpaper_iPhone_6_7_Plus = (1242, 2208)
dimensions Wallpaper_iPhone_5 = (640, 1136)
dimensions Wallpaper_Galaxy_S10 = (1440, 3040)
dimensions Wallpaper_Galaxy_S10e = (1080, 2280)
dimensions Apparel = (4200, 4800)
dimensions WallPortrait = (8400, 12000)
dimensions WallLandscape = (12000, 8400)
dimensions WallSquare = (10200, 10200)
dimensions Duvet = (10175, 8640)
dimensions Blanket = (9375, 12500)
dimensions Pillow = (4125, 4125)
dimensions RugPortrait = (7875, 12750)
dimensions RugLandscape = (12750, 7875)
dimensions TapestryPortrait = (7875, 12750)
dimensions TapestryLandscape = (12750, 7875)
dimensions BathMat = (6480, 4320)
dimensions ShowerCurtain = (7104, 7392)
dimensions BeachTowelPortrait = (5625, 11025)
dimensions BeachTowelLandscape = (11025, 5625)
dimensions DrawstringBag = (2476, 2775)
dimensions ToteBag = (2925, 2925)
dimensions WeekenderBag = (7650, 4950)
dimensions LaundryBag = (4425, 5738)
dimensions ZipPouch = (4200, 2850)
dimensions Journal5'7 = (3742, 2625)
dimensions Spiral6'8 = (2000, 2826)
dimensions TravelMug = (2376, 2024)
dimensions OfficeMug = (4000, 2000)
dimensions WaterBottle = (3200, 2000)
dimensions PhoneCase = (3600, 7500)
dimensions LaptopCover = (8000, 4500)
dimensions Banner = (2400, 1350)
dimensions NarrowBanner = (2400, 400)
dimensions PolkaDotDress = (8400, 9600)

gridSpec :: Target -> GridSpec
gridSpec Instagram = rectangularGridSpec 4 4
gridSpec Sticker = rectangularGridSpec 1 1
gridSpec OfficeMug = rectangularGridSpec 5 2
gridSpec Wallpaper_4K = rectangularGridSpec 16 9
gridSpec Wallpaper_5K = rectangularGridSpec 16 9
gridSpec Wallpaper_8K = rectangularGridSpec 16 9
gridSpec Wallpaper_1080p = rectangularGridSpec 16 9
gridSpec Wallpaper_540p = rectangularGridSpec 16 9
gridSpec Wallpaper_iPhone_Xs = rectangularGridSpec 9 16
gridSpec Wallpaper_iPhone_Xs_Max = rectangularGridSpec 9 16
gridSpec Wallpaper_iPhone_Xr = rectangularGridSpec 9 16
gridSpec Wallpaper_iPhone_6_7 = rectangularGridSpec 9 16
gridSpec Wallpaper_iPhone_6_7_Plus = rectangularGridSpec 9 16
gridSpec Wallpaper_iPhone_5 = rectangularGridSpec 9 16
gridSpec LaptopCover = rectangularGridSpec 9 16
gridSpec PhoneCase = GridSpec
    [ [N, Y, Y, Y]
    , [N, Y, Y, Y]
    , [Y, Y, Y, Y]
    , [Y, Y, Y, Y]
    , [Y, Y, Y, Y]
    , [Y, Y, Y, Y]
    , [Y, Y, Y, Y]
    , [Y, Y, Y, Y]
    ]
gridSpec PolkaDotDress = rectangularGridSpec 84 96
gridSpec NarrowBanner = rectangularGridSpec 12 2
gridSpec _ = rectangularGridSpec 16 16 

allTargets :: [Target]
allTargets =
    [ Instagram
    , Wallpaper_4K
    , Wallpaper_iPhone_6_7_Plus
    , WallSquare
    , PhoneCase
    , LaptopCover
    , Sticker
    , OfficeMug
    , BathMat
    , NarrowBanner
    ]

isWallpaper :: Target -> Bool
isWallpaper = ("Wallpaper" `isInfixOf`) . show

wallpaperTargets :: [Target]
wallpaperTargets = filter isWallpaper universe

