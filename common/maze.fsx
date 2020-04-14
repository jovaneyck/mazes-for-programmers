//Basic ADT for maze building
type Direction = East | South
type Edge = Wall | NoWall
type Cell = Map<Direction, Edge>
type Maze = Map<int*int,Cell>
    
let mkCell info = info |> Map.ofSeq
let private initCell = mkCell [(East, Wall); (South, Wall)]
let initMaze nbRows nbCols : Maze =
    [ for r in [0..(nbRows - 1)] do
        for c in [0..(nbCols - 1)] -> (r,c)]
    |> List.map (fun loc -> (loc, initCell))
    |> Map.ofList
let mkMaze info = info |> Map.ofSeq
    
let update loc direction edge maze = 
    let cell = maze |> Map.find loc
    let newCell = cell |> Map.add direction edge
    maze |> Map.add loc newCell

let locationsIn (m : Maze) = 
    m |> Map.toSeq |> Seq.map fst

let boundaries (m : Maze) = 
    let locs = locationsIn m
    (locs |> Seq.map fst |> Seq.max,locs |> Seq.map snd |> Seq.max)