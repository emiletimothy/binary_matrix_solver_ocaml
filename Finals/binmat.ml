
(* Set of integers. *)
module IntSet = Set.Make(struct
  type t = int
  let compare = compare
end)

(* Map using ints as keys. *)
module IntMap = Map.Make(struct
  type t = int
  let compare = compare
end)

module type BinaryMatrix =
  sig
    type t

    val make : int -> int -> (int * int) list -> t  

    val nrows : t -> int
    val ncols : t -> int
    val rows  : t -> int list
    val cols  : t -> int list

    val get : t -> int -> int -> bool  

    val dump : t -> unit

    val rows_for_col : t -> int -> int list
    val cols_for_row : t -> int -> int list

    val delete_row : t -> int -> t
    val delete_col : t -> int -> t

    val min_col_sum_index : t -> int  
  end

(*
 * We represent binary matrices as a pair of maps between integer
 * indices and sets of integers.  One map maps row indices to a set of
 * column indices and the other maps column indices to a set of row indices.
 *)

module ImplBinaryMatrix : BinaryMatrix =
  struct
    type t = 
      { 
        _nrows : int; 
        _ncols : int; 

        (* Map between row indices and sets of column indices. *)
        _rows : IntSet.t IntMap.t;

        (* Map between columns indices and sets of row indices. *)
        _cols : IntSet.t IntMap.t;
      }

      (* optimize this section if there is time *)
      let update_matrix x y mat = if IntMap.mem x mat._rows && IntMap.mem y mat._cols
        then let
          mat = {mat with _rows = IntMap.add x (IntSet.add y (IntMap.find x mat._rows)) mat._rows}
            in
          {mat with _cols = IntMap.add y (IntSet.add x (IntMap.find y mat._cols)) mat._cols}
        else if IntMap.mem x mat._rows = false && IntMap.mem y mat._cols = false then
          let mat = {mat with _rows = IntMap.add x (IntSet.singleton y) mat._rows} in
          {mat with _cols = IntMap.add y (IntSet.singleton x) mat._cols}
        else if IntMap.mem x mat._rows = true && IntMap.mem y mat._cols = false then
          let mat = 
            {mat with _rows = IntMap.add x (IntSet.add y (IntMap.find x mat._rows)) mat._rows} in
          {mat with _cols = IntMap.add y (IntSet.singleton x) mat._cols}
        else
          let mat = {mat with _rows = IntMap.add x (IntSet.singleton y) mat._rows} in
          {mat with _cols = IntMap.add y (IntSet.add x (IntMap.find y mat._cols)) mat._cols}

      let make nrows ncols lst = 
        if nrows <= 0 then invalid_arg "make: nrows must be at least 1."
        else if ncols <= 0 then invalid_arg "make: ncols must be atleast 1."
        else let rec iter matr lst = match lst with 
          | [] -> matr
          | (x, y)::t ->
            if (x<0 || y<0 || x>=nrows+0 || y>=ncols+0) then 
              invalid_arg (Printf.sprintf "make: invalid row/column coordinates (%d, %d)" x y)
            else
              iter (update_matrix (x+0) (y+0) matr) t
        in iter {_nrows=nrows; _ncols=ncols; _rows=IntMap.empty; _cols=IntMap.empty} lst;;

    let nrows m = m._nrows
    let ncols m = m._ncols

    let rec get_keys bindings lst = match bindings with
        | [] -> List.rev lst
        | (k, _)::t -> get_keys t ((k+0)::lst)
    ;;

    let rows m = get_keys (IntMap.bindings m._rows) []

    let cols m = get_keys (IntMap.bindings m._cols) []

    let get m r c =
      try
        IntSet.mem c (IntMap.find r m._rows)
      with 
        Not_found -> false

    let dump m =
      let dump_map label1 label2 map =
        IntMap.iter (fun k v -> 
            begin
              Printf.printf "%s: %4d; %s: " label1 k label2;
              IntSet.iter (fun e -> Printf.printf "%d " e) v;
              Printf.printf "\n"
            end)
          map
      in
        begin
          Printf.printf "\n-----\n";
          Printf.printf "IMPL_BINARY_MATRIX: nrows = %d; ncols = %d\n"
            m._nrows m._ncols;
          Printf.printf "\nROW -> COLUMN SET MAP:\n";
          dump_map "row" "columns" m._rows;
          Printf.printf "\nCOLUMN -> ROW SET MAP:\n";
          dump_map "column" "rows" m._cols;
          Printf.printf "-----\n\n";
        end

    let rows_for_col m c = if IntMap.mem c m._cols = false 
        then failwith (Printf.sprintf "rows_for_col: missing column index: %d" c)
      else IntSet.elements (IntMap.find c m._cols)

    let cols_for_row m r = if IntMap.mem r m._rows = false
        then failwith (Printf.sprintf "cols_for_row: missing row index: %d" r)
      else IntSet.elements (IntMap.find r m._rows)

      let delete_row m r = let rec del m lst = match lst with
          | [] -> m
          | h::t -> let col = IntSet.remove r (IntMap.find h m._cols) in
            del {m with _cols = IntMap.add h col m._cols} t
        in del {m with _rows= IntMap.remove r m._rows} (cols_for_row m r)
      ;;

    let delete_col m c = let rec del m lst = match lst with
          | [] -> m
          | h::t -> let row = IntSet.remove c (IntMap.find h m._rows) in
          del {m with _rows = IntMap.add h row m._rows} t
        in del {m with _cols = IntMap.remove c m._cols} (rows_for_col m c)
    ;;

    let rec sum_of_list lst sum = match lst with
    
        | [] -> sum
        | h::t -> sum_of_list t (sum + 1)


    let min_col_sum_index m = 
      let rec iter min_index col_sum list_cols = 
        match list_cols with
      | [] -> min_index + 0
      | h::t -> let sum = sum_of_list (rows_for_col m h) 0 in
          if sum < col_sum then
            iter h sum t
          else
            iter min_index col_sum t
      in iter (-1) 999999999999999999 (cols m);

  end
