(** Test cases taken from Odoc's testsuite *)

(**
  {table }
  {table {tr } }
  {table {tr {td}}}
  {table {tr {th}}}
  {table {tr {th}} {tr {th}} {tr {td}}}

  {table
    {tr
      {th xxx}
      {th yyy}
    }
    {tr
      {td aaaa bbb ccc {i ddd}
      }
      {td
         {table {tr {td}}}
      }
    }
    {tr
      {td
         - aaa
         - bbb
         - ccc
      }
      {td
        {t
           x | y | z
           --|---|--
           1 | 2 | 3
        }
      }
    }
  }

  {t }

  {t
    | a |
  }

  {t
    |a|   *b*|
    |*c| d* |
  }

  {t
     | `a |`
  }

  {t
   |---|---|
   | x | y |
  }

  {t
   | x | y |
   | x | y |
  }

  {t
    |--|--|
  }

  {t
   | x | y |
   |---|---|
  }

  {t
   | a | b | c | d |
   |---|:--|--:|:-:|
  }

  {t
    a | b | c | d
   ---|:--|--:|:-:
    a | b | c | d
  }

  {t

   | a | b | c | d |

   |---|---|---|---|

   | a | b | c | d |

  }

  {t
   | {i a} {:google.com} \t | | {m b} {e c} {% xyz %} | {b d} [foo] |
   |---|---|---|---|
  }

  {t
    | a | b |c| d |
    |---|--:|:--|:-:|
  }

  {t
   ||a|b|
   |:-|---:|
   |c|d|
   |cc|dd|
   |-:|:-:|
   |e|f|
   |g|h||
  }

  {t
   | x | y |
   |---|---|
   | x | y | z |
  }

  {t
   | x | y |
   |---|---|
   x 
  }

  {t
  | Header and other word |
  |-----------------------|
  | cell and other words  |
  }

  {t
  | Header other word |
  |-------------------|
  | Header other word |
  }

  {t
   | foo | bar |
   | {i foooooooooooooooooooooooooooo} foooooooooooooooooooooooo fooooooooooooooooooooooo | bar |
  }
*)

(**
  {[foo]}
  {[ foo]}

  {[foo bar]}
  {[foo
  bar]}

  {[foo

  bar]}

  {[ foo

     bar]}

  {[
     foo
     bar
  ]}

  {[{[]}
  {[foo]}]}
  {[]]}
  {[foo]]bar]}

  {[
    (** foo *)
    let bar = ()
  ]}

  {@ocaml env=f1 version>=4.06 [code goes here]}
  {delim@ocaml[foo]delim[output {b foo}]}

  {delim@ocaml[
    foo
  ]delim[
  foo
  {[ bar ]}
  baz
  ]}

  {[foo][output {b foo}]}
  {@ocaml[foo][output {b foo}]}
  {@ocaml[foo]unexpected[output {b foo}]}
  {delim@ocaml[foo]wrong[output {b foo}]delim}

  {@ocaml
  [ code ]}

  {@ocaml kind=toplevel
  [ code ]}

  {@ocaml kind=toplevel
  env=e1[ code ]}

  {@ocaml
  kind=toplevel[ code ]}

  {@ocaml kind=toplevel [ code ]}
  {delim@ocaml[ foo ]delim[ ]}
*)
