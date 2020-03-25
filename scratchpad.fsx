#r @".\packages\SharpVG\lib\netstandard2.1\SharpVG.dll"
open SharpVG

let black = Color.ofName Black
let opaque = 1.0
let style = Style.create black black Length.one opaque opaque

let lineAt p1 p2 = Line.create (Point.ofInts p1) (Point.ofInts p2) |> Element.create |> Element.withStyle style

let shapesSvg = 
    [lineAt (10,10) (10,20)
     lineAt (10,10) (20,10)
     lineAt (10,20) (20,20)
     lineAt (20,10) (20,20)
    ]
    |> Svg.ofSeq

System.IO.File.WriteAllText( __SOURCE_DIRECTORY__ + "\out\maze.html", (@"<meta http-equiv=""refresh"" content=""0.5"" >"+Svg.toString shapesSvg))