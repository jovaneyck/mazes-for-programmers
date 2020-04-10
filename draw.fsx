//Stuff to render mazes using an svg lib & some raw html dumps :)
#r @".\packages\SharpVG\lib\netstandard2.1\SharpVG.dll"
#load "maze.fsx"
open SharpVG

type Location = int*int
type Line = Location*Location

let private black = Color.ofName Black
let private opaque = 1.0
let private style = Style.create black black Length.one opaque opaque

let private line p1 p2 = Line.create (Point.ofInts p1) (Point.ofInts p2) |> Element.create |> Element.withStyle style

let private lines spacing ((x,y) : int*int, cell : Maze.Cell) : Line list = 
    cell
    |> Map.toList
    |> List.filter (fun (_,w) -> w = Maze.Wall)
    |> List.map (fun (dir,_) -> 
        match dir with
        | Maze.South-> (1,2), (2,2)
        | Maze.East -> (2,1), (2,2))
    |> List.map (fun ((a,b),(c,d)) -> ((x+a)*spacing, (y+b)*spacing), ((x+c) * spacing, (y+d) * spacing))
    
let private edges ((x,y) as loc, cell) = 
    let spacing = 10
    let lines = lines spacing (loc, cell)
    let left = if x = 0 then [(spacing,(y+1)*spacing),(spacing, (y+2)*spacing)] else []
    let top = if y = 0 then [((x+1)*spacing,spacing),((x+2)*spacing, spacing)] else []
    Seq.concat [lines ; left; top]
    
let toLines (maze : Maze.Maze) = 
    maze 
    |> Seq.collect (fun kvp -> edges (kvp.Key, kvp.Value)) |> List.ofSeq |> Seq.map (fun (x,y) -> line x y) |> SharpVG.Svg.ofSeq
    

let draw shapes =
    shapes
    |> Svg.toString
    |> fun s -> System.IO.File.WriteAllText( __SOURCE_DIRECTORY__ + "\out\maze.html", (@"<html><head><title>My magical maze!</title><style>svg {width: -webkit-fill-available;height: -webkit-fill-available;}</style></head><body>"+s+"</body></html>"))

let drawRefresh shapes =
    shapes
    |> Svg.toString
    |> fun s -> System.IO.File.WriteAllText( __SOURCE_DIRECTORY__ + "\out\maze.html", (@"<html><head><title>My magical maze!</title><meta http-equiv=""refresh"" content=""1"" ><style>svg {width: -webkit-fill-available;height: -webkit-fill-available;}</style></head><body>"+s+"</body></html>"))
