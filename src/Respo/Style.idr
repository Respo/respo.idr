module Respo.Style

import Data.List
import Data.String

%default total

px : Double -> String
px n = show n ++ "px"

public export
record StyleBuilder where
  constructor MkStyleBuilder
  entries : List (String, String)

public export
style : StyleBuilder
style = MkStyleBuilder []

public export
finish : StyleBuilder -> List (String, String)
finish (MkStyleBuilder xs) = reverse xs

public export
custom : String -> String -> StyleBuilder -> StyleBuilder
custom key value (MkStyleBuilder xs) = MkStyleBuilder ((key, value) :: xs)

public export
padding : Double -> StyleBuilder -> StyleBuilder
padding n (MkStyleBuilder xs) = MkStyleBuilder (("padding", px n) :: xs)

public export
padding4 : (Double, Double, Double, Double) -> StyleBuilder -> StyleBuilder
padding4 (t, r, b, l) (MkStyleBuilder xs) =
  MkStyleBuilder (("padding", show t ++ "px " ++ show r ++ "px " ++ show b ++ "px " ++ show l ++ "px") :: xs)

public export
margin : Double -> StyleBuilder -> StyleBuilder
margin n (MkStyleBuilder xs) = MkStyleBuilder (("margin", px n) :: xs)

public export
margin4 : (Double, Double, Double, Double) -> StyleBuilder -> StyleBuilder
margin4 (t, r, b, l) (MkStyleBuilder xs) =
  MkStyleBuilder (("margin", show t ++ "px " ++ show r ++ "px " ++ show b ++ "px " ++ show l ++ "px") :: xs)

public export
width : Double -> StyleBuilder -> StyleBuilder
width n (MkStyleBuilder xs) = MkStyleBuilder (("width", px n) :: xs)

public export
height : Double -> StyleBuilder -> StyleBuilder
height n (MkStyleBuilder xs) = MkStyleBuilder (("height", px n) :: xs)

public export
gap : Double -> StyleBuilder -> StyleBuilder
gap n (MkStyleBuilder xs) = MkStyleBuilder (("gap", px n) :: xs)

public export
background : String -> StyleBuilder -> StyleBuilder
background color (MkStyleBuilder xs) = MkStyleBuilder (("background", color) :: xs)

public export
color : String -> StyleBuilder -> StyleBuilder
color value (MkStyleBuilder xs) = MkStyleBuilder (("color", value) :: xs)

public export
fontFamily : String -> StyleBuilder -> StyleBuilder
fontFamily value (MkStyleBuilder xs) = MkStyleBuilder (("font-family", value) :: xs)

public export
fontSize : Double -> StyleBuilder -> StyleBuilder
fontSize n (MkStyleBuilder xs) = MkStyleBuilder (("font-size", px n) :: xs)

public export
fontWeight : String -> StyleBuilder -> StyleBuilder
fontWeight value (MkStyleBuilder xs) = MkStyleBuilder (("font-weight", value) :: xs)

public export
borderRadius : Double -> StyleBuilder -> StyleBuilder
borderRadius n (MkStyleBuilder xs) = MkStyleBuilder (("border-radius", px n) :: xs)

public export
border : String -> StyleBuilder -> StyleBuilder
border value (MkStyleBuilder xs) = MkStyleBuilder (("border", value) :: xs)

public export
cursor : String -> StyleBuilder -> StyleBuilder
cursor value (MkStyleBuilder xs) = MkStyleBuilder (("cursor", value) :: xs)

public export
display : String -> StyleBuilder -> StyleBuilder
display value (MkStyleBuilder xs) = MkStyleBuilder (("display", value) :: xs)

public export
flexDirection : String -> StyleBuilder -> StyleBuilder
flexDirection value (MkStyleBuilder xs) = MkStyleBuilder (("flex-direction", value) :: xs)

public export
alignItems : String -> StyleBuilder -> StyleBuilder
alignItems value (MkStyleBuilder xs) = MkStyleBuilder (("align-items", value) :: xs)

public export
justifyContent : String -> StyleBuilder -> StyleBuilder
justifyContent value (MkStyleBuilder xs) = MkStyleBuilder (("justify-content", value) :: xs)

public export
boxShadow : String -> StyleBuilder -> StyleBuilder
boxShadow value (MkStyleBuilder xs) = MkStyleBuilder (("box-shadow", value) :: xs)

public export
transition : String -> StyleBuilder -> StyleBuilder
transition value (MkStyleBuilder xs) = MkStyleBuilder (("transition", value) :: xs)

public export
finishAll : List StyleBuilder -> List (String, String)
finishAll builders =
  foldl (\acc, builder => acc ++ finish builder) [] builders

public export
uiGlobal : List (String, String)
uiGlobal =
  style
    |> padding 32
    |> background "#f7f9fc"
    |> color "#1f2933"
    |> fontFamily "Inter, -apple-system, BlinkMacSystemFont, sans-serif"
    |> cursor "default"
    |> finish

public export
uiButton : List (String, String)
uiButton =
  style
    |> fontWeight "600"
    |> fontSize 15
    |> color "#ffffff"
    |> background "#2563eb"
    |> padding4 (10, 18, 10, 18)
    |> borderRadius 8
    |> border "none"
    |> cursor "pointer"
    |> finish

public export
uiButtonSecondary : List (String, String)
uiButtonSecondary =
  style
    |> fontSize 15
    |> color "#1f2933"
    |> background "#edf2f7"
    |> padding4 (9, 16, 9, 16)
    |> borderRadius 8
    |> border "1px solid #d1d5db"
    |> cursor "pointer"
    |> finish

public export
uiPanel : List (String, String)
uiPanel =
  style
    |> color "#1f2933"
    |> border "1px solid #e5e7eb"
    |> background "#ffffff"
    |> padding 24
    |> borderRadius 16
    |> finish