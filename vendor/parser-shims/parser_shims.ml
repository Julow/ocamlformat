module List = struct
  include List

  let rec find_map f = function
    | [] -> None
    | x :: l ->
        begin match f x with
        | Some _ as result -> result
        | None -> find_map f l
        end
end

module Int = struct
  include Int

  let min x y = if x <= y then x else y
  let max x y = if x >= y then x else y
end

module Misc = struct
  include Misc

  module Style = struct
    type setting = unit

    let default_setting = ()
    let setup _ = ()

    let as_inline_code printer ppf x =
      Format.pp_open_stag ppf (Format.String_tag "inline_code");
      printer ppf x;
      Format.pp_close_stag ppf ()

    let inline_code ppf s = as_inline_code Format.pp_print_string ppf s
  end

  module Error_style = struct
    include Error_style

    let default_setting = Contextual
  end
end

module Clflags = struct
  let include_dirs = ref ([] : string list)(* -I *)
  let debug = ref false                   (* -g *)
  let unsafe = ref false                  (* -unsafe *)
  let absname = ref false                 (* -absname *)
  let use_threads = ref false             (* -thread *)
  let open_modules = ref []               (* -open *)
  let principal = ref false               (* -principal *)
  let recursive_types = ref false         (* -rectypes *)
  let applicative_functors = ref true     (* -no-app-funct *)
  let for_package = ref (None: string option) (* -for-pack *)
  let transparent_modules = ref false     (* -trans-mod *)
  let locations = ref true                (* -d(no-)locations *)
  let color = ref None                    (* -color *)
  let error_style = ref None              (* -error-style *)
  let unboxed_types = ref false
  let no_std_include = ref false
end

module Load_path = struct
  type dir
  type auto_include_callback =
    (dir -> string -> string option) -> string -> string
  let init ~auto_include:_ _ = ()
  let get_paths () = []
  let auto_include_otherlibs _ _ s = s
end
