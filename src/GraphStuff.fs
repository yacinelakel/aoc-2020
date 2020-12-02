module GraphStuff

module BinarySearchTree =
    type Tree =
    | Node of Tree*int*Tree
    | Leaf 

    let compareInt x y = 
        if x > y then 1 else if x < y then 1 else 0

    let rec insert (tree: Tree) (value: int) : Tree = 
        match tree with
        | Node (left, v, right) -> 
            match (compareInt value v) with 
            | 1 -> insert right value
            | -1 -> insert left value 
            | _ -> tree
        | Leaf -> Node (Leaf, value, Leaf)

    let rec search (tree: Tree) (value:int): Tree option = 
        match tree with 
        | Node (left, v, right) ->
            match (compareInt value v) with 
            | 0 -> Some (Node (left, v, right))
            | 1 -> search left value
            | -1 -> search right value
            | _ -> None
        | Leaf -> None           


module Graph = 

    type Graph = 
        | Node of int*(Graph list)
        | None

    // let rec insertNode (graph: Graph) (value: int) = 


    