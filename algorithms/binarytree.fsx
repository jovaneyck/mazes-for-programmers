#load @"..\common\draw.fsx"
#load @"..\common\random.fsx"
open Maze
open Random

let binaryTreeCell boundary location : Rand<Cell> =   
    let (bx,by) = boundary
    let (x,y) = location
        
    random {
        if x = bx && y = by then 
            return mkCell [ (East, Wall); (South, Wall)] 
        elif x = bx then 
            return mkCell [ (East, Wall); (South, NoWall)] 
        elif y = by then
            return mkCell [ (East, NoWall); (South, Wall)] 
        else
            let! openEast = Rand.Bool
            let east = if openEast then Wall else NoWall
            let south = if openEast then NoWall else Wall
            let cell : Cell = mkCell [(East, east); (South, south)]
            return cell
    }

let binaryTree m : Rand<Maze> = 
    let locations = Maze.locationsIn m
    let boundaries = Maze.boundaries m

    random {    
        let! cells = 
            locations 
            |> Seq.map (fun loc -> binaryTreeCell boundaries loc |> State.map (fun c -> loc,c)) 
            |> (Seq.toList >> State.sequenceList)
        return Maze.mkMaze cells
    }

Maze.initMaze 100 100
|> binaryTree 
|> Rand.run
|> Draw.toLines
|> Draw.draw "binarytree"
//|> Draw.drawRefresh