module Frame = Mips_frame

type exp = unit

type level = {
  frame : Frame.frame;
  depth : int;
}

and access = level * Frame.access

let outermost =
  { frame = Frame.new_frame (Temp.label_of_string "main") []
  ; depth = 0
  }

let new_level (parent : level) (label : Temp.label) (formals : bool list) : level = 
  { frame = Frame.new_frame label (true :: formals)
  ; depth = parent.depth + 1
  }

let alloc_local (lvl : level) (esc : bool) : access = 
  lvl, Frame.alloc_local lvl.frame esc

let formals (lvl : level) : access list = 
  List.map (fun x -> lvl, x) (List.tl lvl.frame.formals)
