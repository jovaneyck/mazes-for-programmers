#r @".\packages\SharpVG\lib\netstandard2.1\SharpVG.dll"

module Maze = 
    type Direction = East | South
    type Edge = Wall | NoWall
    type Cell = Map<Direction, Edge>
    type Maze = Map<int*int,Cell>
    
    let private initCell = [(East, Wall); (South, Wall)] |> Map.ofList
    let initMaze nbRows nbCols : Maze =
        [ for r in [0..(nbRows - 1)] do
          for c in [0..(nbCols - 1)] -> (r,c)]
        |> List.map (fun loc -> (loc, initCell))
        |> Map.ofList
    
    let update loc direction edge maze = 
        let cell = maze |> Map.find loc
        let newCell = cell |> Map.add direction edge
        maze |> Map.add loc newCell
    
module Draw =
    open SharpVG
    open Maze
    
    type Location = int*int
    type Line = Location*Location

    let private black = Color.ofName Black
    let private opaque = 1.0
    let private style = Style.create black black Length.one opaque opaque

    let private line p1 p2 = Line.create (Point.ofInts p1) (Point.ofInts p2) |> Element.create |> Element.withStyle style

    let private lines spacing ((x,y) : int*int, cell : Cell) : Line list = 
        cell
        |> Map.toList
        |> List.filter (fun (_,w) -> w = Wall)
        |> List.map (fun (dir,_) -> 
            match dir with
            | South-> (1,2), (2,2)
            | East -> (2,1), (2,2))
        |> List.map (fun ((a,b),(c,d)) -> ((x+a)*spacing, (y+b)*spacing), ((x+c) * spacing, (y+d) * spacing))
    
    let private edges ((x,y) as loc, cell) = 
        let spacing = 10
        let lines = lines spacing (loc, cell)
        let left = if x = 0 then [(spacing,(y+1)*spacing),(spacing, (y+2)*spacing)] else []
        let top = if y = 0 then [((x+1)*spacing,spacing),((x+2)*spacing, spacing)] else []
        Seq.concat [lines ; left; top]
    
    let toLines (maze : Maze) = 
        maze 
        |> Seq.collect (fun kvp -> edges (kvp.Key, kvp.Value)) |> List.ofSeq |> Seq.map (fun (x,y) -> line x y) |> SharpVG.Svg.ofSeq
    

    let draw shapes =
        shapes
        |> Svg.toString
        |> fun s -> System.IO.File.WriteAllText( __SOURCE_DIRECTORY__ + "\out\maze.html", (@"<meta http-equiv=""refresh"" content=""1"" >"+s))
   
open Maze 

let m = 
    Maze.initMaze 10 10
    |> Maze.update (4,4) East NoWall
    |> Maze.update (4,4) South NoWall

m
|> Draw.toLines
|> Draw.draw