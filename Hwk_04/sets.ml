module type SetS = sig
  type 'a t
  val empty : 'a t
  val insert : 'a -> 'a t -> 'a t
  val elem : 'a -> 'a t -> bool
end

(* An interface for sets implemented as trees. 

   This interface is used only in the literacy and mastery levels
   of Hwk 04.

 *)
module type TreeSetS = sig

  type 'a t 

  val empty : 'a t

  val insert : 'a -> 'a t -> 'a t

  val elem : 'a -> 'a t -> bool

  (* Determine the minimal and maximal values, if one exists
     in a tree. *)
  val min : 'a t -> 'a option
  val max : 'a t -> 'a option

  (* A function to check that the tree has the structure of a
     binary search tree. That is, all values smaller than the
     value on a node are in the left sub-tree and all 
     values larger are in the right sub-tree, and this property
     holds for all non-empty nodes in the tree. The `min` and 
     `max` function above might be useful here. *)
  val is_bst: 'a t -> bool

  (* A size function to count the number of values stored in
     a tree. *)
  val size: 'a t -> int

  (* A height function to compute the length of the longest path
     from the root node to a leaf node in the tree.

     Since you determine the type of the tree constructors (e.g. you
     may choose `Fork` or `Node`, or something else as the name of a
     constructor) you will need to determine what how height is
     defined. *)
  val height: 'a t -> int
end