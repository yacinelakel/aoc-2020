module Stuff

module BinarySearchTree =
    type Tree =
        | Node of Tree * int * Tree
        | Leaf

    let compareInt x y =
        if x > y then 1
        else if x < y then 1
        else 0

    let rec insert (tree: Tree) (value: int): Tree =
        match tree with
        | Node (left, v, right) ->
            match (compareInt value v) with
            | 1 -> insert right value
            | -1 -> insert left value
            | _ -> tree
        | Leaf -> Node(Leaf, value, Leaf)

    let rec search (tree: Tree) (value: int): Tree option =
        match tree with
        | Node (left, v, right) ->
            match (compareInt value v) with
            | 0 -> Some(Node(left, v, right))
            | 1 -> search left value
            | -1 -> search right value
            | _ -> None
        | Leaf -> None


module Graph =
    //// source: https://github.com/CSBiology/FSharp.FGL/blob/developer/src/FSharp.FGL/Graph.fs

    ///Labeled vertex
    type LVertex<'Vertex,'Label> =
        'Vertex * 'Label

    ///Unlabeled edge
    type Edge<'Vertex> =
        'Vertex * 'Vertex

    ///Labeled edge
    type LEdge<'Vertex,'Edge> =
        'Vertex * 'Vertex * 'Edge

    ///Tuple list of adjacent vertices and the linking edges
    type Adj<'Vertex,'Edge> when 'Vertex: comparison =
        List<'Vertex*'Edge>

    ///Context of a vertice as defined by Martin Erwig. Adjacency of type 'Adj'
    type Context<'Vertex,'Label,'Edge> when 'Vertex: comparison=
        Adj<'Vertex,'Edge>*'Vertex*'Label*Adj<'Vertex,'Edge>

    ///Map of adjacent vertices as key and the linking edges as values
    type MAdj<'Vertex,'Edge> when 'Vertex: comparison =
        Map<'Vertex,'Edge>

    ///Context of a vertices as defined by Martin Erwig. Adjacency of type 'MAdj'
    type MContext<'Vertex,'Label,'Edge> when 'Vertex: comparison =
        MAdj<'Vertex,'Edge> * 'Label * MAdj<'Vertex,'Edge>

    ///Map of Vertices as keys and MContexts as values
    type Graph<'Vertex,'Label,'Edge> when 'Vertex: comparison =
        Map<'Vertex, MContext<'Vertex,'Label,'Edge>>




    let empty = Map.empty
                 

module Segment =
    type Point = { x: int; y: int }
    type Segment = { p1: Point; p2: Point }

    type Orientation =
        | Colinear
        | Clockwise
        | CounterClockwise

    // Check if point r lies on
    let onsegment s r =
        let p, q = s.p1, s.p2
        r.x
        <= (max p.x q.x)
        && r.x >= (min p.x q.x)
        && r.y <= (max p.y q.y)
        && r.y >= (min p.y q.y)

    let orientation s r =
        let p, q = s.p1, s.p2

        let v =
            ((q.y - p.y) * (r.x - q.x))
            - ((q.x - p.x) * (r.y - q.y))

        match v with
        | 0 -> Colinear
        | v' -> if v' > 0 then Clockwise else CounterClockwise

    let intersects s1 s2 =
        let o1 = orientation s1 s2.p1
        let o2 = orientation s1 s2.p2
        let o3 = orientation s2 s1.p1
        let o4 = orientation s2 s1.p2

        let gc = o1 <> o2 && o3 <> o4
        match gc with
        | true -> true
        | false ->
            if o1 = Colinear && (onsegment s1 s2.p1) then true
            else if o2 = Colinear && (onsegment s1 s2.p2) then true
            else if o3 = Colinear && (onsegment s2 s1.p1) then true
            else o4 = Colinear && (onsegment s2 s1.p2)

    let slope s =
        let x1, x2, y1, y2 =
            (float) s.p1.x, (float) s.p2.x, (float) s.p1.y, (float) s.p2.y

        (y2 - y1) / (x2 - x1)

    let distance p1 p2 =
        let absX = p1.x - p2.x |> abs |> float
        let absY = (p1.y - p2.y) |> abs |> float
        sqrt (((absX ** 2.0) + (absY ** 2.0)))

    let length s = distance s.p1 s.p2

    type Point with
        member this.DistanceTo p =
            distance this p
    type Segment with
        member this.OnSegment p = onsegment this p
        member this.Orientation p = orientation this p
        member this.Intersects seg = intersects this seg
        member this.Slope() = slope this
        member this.Parallel (seg: Segment) = this.Slope() = seg.Slope()
        member this.Length() = length this
