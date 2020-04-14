/// The state monad - a transition from a state, to a value and a new state.
type State<'s, 'a> = State of ('s -> 'a * 's)

/// The state monad.
module State =   

    let run s (state:State<'s, 'a>) = let (State(run)) = state in run s

    let eval (state:State<'s, 'a>) s = run s state |> fst

    let unit a : State<'s, 'a> = State <| fun s -> (a,s)

    let map f (state:State<'s, 'a>) : State<'s, 'b> = 
        State <| fun s -> 
            let (a,s1) = run s state
            (f a, s1)
    
    let bind f (state:State<'s, 'a>) : State<'s, 'b> =
        State <| fun s ->
            let (a,s1) = run s state
            run s1 (f a)
    
    let map2 (s1:State<'s, 'a>) (s2:State<'s, 'b>) (f:'a -> 'b -> 'c) : State<'s, 'c> =
        bind (fun a -> map (fun b -> f a b) s2) s1

    let sequenceList (ss:State<'s, 'a> list) : State<'s, 'a list> =
        List.foldBack (fun s acc -> map2 s acc (fun x xs -> x::xs)) ss (unit (List.empty))

    type StateBuilder() =
        member x.Bind(s, f) = bind f s
        member x.Return(value) = unit value
        member x.Yield(value) = unit value
        member x.ReturnFrom(value) = value
        member x.Zero() = unit()
        member x.Combine(s1:State<'S,unit>, s2:State<'S,'a>) = map2 s1 s2 (fun _ s -> s)
        member x.For(xs:seq<'a>, f:'a -> State<'S, 'a>) = xs |> Seq.map f

[<AutoOpen>]
module StateBuilder =

    /// State monad workflow builder.
    let state = new State.StateBuilder()

/// A random value represented as a transition from an RNG to a random value and the next state of the RNG.
type Rand<'A> = State<System.Random, 'A>

/// RNG combinators.
module Rand =
    
    open System

    let runSeed (seed:int) (rand:Rand<'a>) : 'a = State.eval rand (new System.Random(seed))

    let run (rand:Rand<'a>) : 'a = State.eval rand (new System.Random())

    let unit a : Rand<'a> = State.unit a

    let Int : Rand<int> = State(fun (rng:Random) -> (rng.Next(),rng))
        
    let IntRange min max : Rand<int> = State(fun (rng:Random) -> (rng.Next(min,max + 1),rng))

    let Bool : Rand<bool> = IntRange 0 1 |> State.map (function 0 -> false | _ -> true)

    let listOf count (rand:Rand<'a>) : Rand<'a list> = 
        List.init count (fun _ -> rand) |> State.sequenceList

    let listOfRand (count:Rand<int>) (rand:Rand<'a>) : Rand<'a list> = 
        count |> State.bind (fun count -> List.init count (fun _ -> rand) |> State.sequenceList)

    let choice (r1:Rand<'a>) (r2:Rand<'a>) : Rand<'a> = 
        Bool |> State.bind (fun b -> if b then r1 else r2)

[<AutoOpen>]
module RandBuilder =

    /// RNG workflow builder.
    let random = new State.StateBuilder()