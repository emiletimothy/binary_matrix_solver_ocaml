(* Student name: Emile Timothy Anand *)
(* Email: eanand@caltech.edu *)

open Kanoodle_types

(*
 * Utility functions.
 *)

(*
 * Pieces.
 *)

let make_piece (lbl, ls) =
  { label = lbl; locs = LocSet.of_list ls }

let make_piece_from_string (label, s) =
  let st     = String.trim s in
  let lines  = String.split_on_char '\n' st in
  let coords = ref LocSet.empty in
    begin
      List.iteri
        (fun i s ->
           String.iteri 
             (fun j c -> 
                if c = 'X' then 
                   coords := LocSet.add (i, j) !coords)
             s)
        lines;
      { label; locs = !coords }
    end

let show_piece { label; locs } =
  Printf.printf "LABEL: '%c'; " label;
  Printf.printf "LOCS: ";
  LocSet.iter (fun (i, j) -> Printf.printf "(%d, %d) " i j) locs;
  print_newline ()

let translate_piece { label; locs } drow dcol =
  let func = fun (x, y) -> (x + drow, y + dcol) in
  {label = label; locs = LocSet.map func locs}

let smol lst = 
  let min x y = if x < y then x else y in
  let rec get_smol smol_x smol_y lst = match lst with
    | [] -> (smol_x, smol_y)
    | (a, b)::t -> get_smol (min smol_x a) (min smol_y b) t in
  get_smol 999999999999999999 999999999999999999 lst

let normalize_piece { label; locs } = 
  translate_piece { label; locs } 
  (0 - (fst (smol (LocSet.elements locs))))
  (0 - (snd (smol (LocSet.elements locs))))

let reflect_piece { label; locs } =
  let func = fun (x, y) -> (x, 0 - y) in
  normalize_piece {label = label; locs = LocSet.map func locs}

let rotate_piece { label; locs } =
  let func = fun (x, y) -> (y, 0 - x) in
  normalize_piece {label = label; locs = LocSet.map func locs}

let piece_in_all_orientations p = 
  let p1  = normalize_piece p in
  let p2  = rotate_piece p1 in
  let p3  = rotate_piece p2 in
  let p4  = rotate_piece p3 in
  let p1_ = reflect_piece p1 in
  let p2_ = reflect_piece p2 in
  let p3_ = reflect_piece p3 in
  let p4_ = reflect_piece p4 in
  let piece_list = [p1; p2; p3; p4; p1_; p2_; p3_; p4_] in
    PieceSet.of_list piece_list

let all_normalized_pieces piece_string_list = 
  let all_pieces   = List.map make_piece_from_string piece_string_list in
  let orientations = List.map piece_in_all_orientations all_pieces in
    List.fold_left PieceSet.union PieceSet.empty orientations

(*
 * Boards.
 *)

let on_board p b = 
  let test (row, col) =
    row >= 0 && 
    col >= 0 && 
    row < b.nrows && 
    col < b.ncols
  in
    LocSet.for_all test p.locs
    
let addToSet p x y b sets = if (on_board (translate_piece p x y) b) then 
    PieceSet.add (translate_piece p x y) sets 
  else sets

  let translate_piece_on_board b p = let rec vertical_shift b p up sets =
    if up <> b.nrows then
    let rec horizontal_shift b p right up sets =
    if right <> b.ncols then
    let sets = addToSet p up right b sets in
    let sets = addToSet p up (0 - right) b sets in
    let sets = addToSet p (0 - up) (0 - right) b sets in
    let sets = addToSet p (0 - up) right b sets in
    horizontal_shift b p (right + 1) up sets
    else sets in let sets = horizontal_shift b p 0 up sets in
    vertical_shift b p (up + 1) sets
    else sets in vertical_shift b p 0 PieceSet.empty;;
    
let all_pieces_on_board piece_string_list b =
  let pieces     = all_normalized_pieces piece_string_list in
  let elems      = PieceSet.elements pieces in
  let translated = List.map (translate_piece_on_board b) elems in
    List.fold_left PieceSet.union PieceSet.empty translated

let make_piece_array piece_string_list b = 
  let d = all_pieces_on_board piece_string_list b in
  Array.of_list (PieceSet.elements d) 

let all_locs_on_board b =
  let rec iter row col lst =
    match () with
      | _ when row = b.nrows -> lst
      | _ when col = b.ncols -> iter (row + 1) 0 lst
      | _ -> iter row (col + 1) ((row, col) :: lst)
  in iter 0 0 []

(*
 * Constraints. 
 *)

(* Make the constraints array.
   These correspond to the columns in the binary matrix. *)
let make_constraints_array piece_string_list b =
  let func = fun x ->  Loc x in
  let a = List.map func (all_locs_on_board b) in
  let func2 = fun (x, _) ->  Label x in
  let b = List.map func2 (piece_string_list) in
  Array.of_list (a @ b)
  

(* Create a map between constraints and binary matrix column numbers. *)
let make_constraints_map piece_string_list b =
  let kmap = KconstraintMap.empty in
  let kconstraint_lst = Array.to_list (make_constraints_array piece_string_list b) in
  let rec converter kconstraint kmap count =
    match kconstraint with
      | [] -> kmap
      | h::t -> converter t (KconstraintMap.add h count kmap) (count + 1)
    in converter kconstraint_lst kmap 0

(*
 * Inputs to algorithm X.
 *)

let helper_make_matrix row constraints map = 
  let rec iter constraints lst = 
  match constraints with
    | [] -> lst
    | h::t -> 
      let col = KconstraintMap.find h map in 
        iter t (LocSet.add (row, col) lst) in
  iter constraints LocSet.empty

let kconstraints p_loc p_label = 
  let func = fun (x) -> Loc x in
  let a = List.map func (LocSet.elements p_loc) in
  let b = Label p_label in b::a

let make_binary_matrix_base_locs piece_string_list b =
  let piece_array = make_piece_array piece_string_list b in
  let constraint_map = make_constraints_map piece_string_list b in
  let rec for_each_piece piece_array index map binary_lst = 
    if index = Array.length piece_array then binary_lst 
    else let constraints = kconstraints piece_array.(index).locs piece_array.(index).label in
      let new_lst = helper_make_matrix index constraints map in
      for_each_piece piece_array (index + 1) map (LocSet.union binary_lst new_lst) in
  for_each_piece piece_array 0 constraint_map LocSet.empty


let make_binary_matrix_extra_locs piece_string_list b = 
  let cos_map = make_constraints_map piece_string_list b in 
  let func = fun (x, _) -> Label x in 
    let keys = List.map func piece_string_list in
    let rec make_pos keys new_pos count = 
      match keys with
        | [] -> new_pos
        | h::t -> 
          make_pos t (LocSet.add (count, KconstraintMap.find h cos_map) new_pos) (count + 1) in 
    make_pos keys LocSet.empty (Array.length (make_piece_array piece_string_list b))

let make_binary_matrix_locs piece_string_list b =
  LocSet.union
    (make_binary_matrix_base_locs piece_string_list b)
    (make_binary_matrix_extra_locs piece_string_list b)





