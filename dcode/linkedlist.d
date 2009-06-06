/LINKEDLIST 100 {
  | -- | --
  /_make {
    {
      /ifunc ~[
        null ~end bind ~stopped bind currentdict ~begin bind 
        {stop} bind ~if bind
      ] def
      /free null max {
        [exch null default]
        dup _next dup null eq ~pop {1 index exch _setprev} ifelse
      } repeat def
    } /box inlayer
  } bind def

  |=========== make ======================
  | max ~default | dict
  |
  | constructor
  | max is the number of links in the list to preallocate
  | ~default is a procedure to call that returns a new data
  |   element of a link -- a dict, list, array, ...
  |
  /make {
    save [
      /default 4 -1 roll
      /max 6 -1 roll
      {/ifunc /alloc /free /box /ip} ~null forall
      {
        /remake /new /del /nth /iter 
        /_next /_prev /_setprev /_setnext 
        /data /setdata
      } {dup find} forall
    ] 1 index capsave makestruct ~_make 1 index indict
    exch restore
  } bind def

|================ funcs called in link list ===============

  |=================== remake ==================
  | max | --
  | dump old links and create a new set of links.
  /remake {/max name _make} bind def

  | [next prev data] | [next prev data]/null
  /_next {0 get} bind def

  | [next prev data] | [next prev data]/null
  /_prev {1 get} bind def  

  | [next prev data]/null [next prev data] | --
  /_setnext {0 put} bind def

  | [next prev data]/null [next prev data] | --
  /_setprev {1 put} bind def

  |==================== data ================
  | [next prev data] | data
  | get the data element of a link
  /data {2 get} bind def

  |=================== setdata ==============
  | data [next prev data] | --
  | replace the data element of a link
  /setdata {2 put} bind def
    
  |================== new ====================
  | -- | [next prev data]
  | allocate a link from the store and make it
  | the head of the list
  /new {
    free 

    dup _next
    dup null ne {null 1 index _setprev} if
    /free name

    alloc dup null ne {2 copy _setprev} if
    1 index _setnext
    dup /alloc name
  } bind def
    
  |================== nth ======================
  | n | dict
  | get the nth allocate link, counting from 0.
  /nth {
    alloc 0 1 4 -1 roll 1 sub {pop _next} for
  } bind def

  |================= del ======================
  | [next prev data] | --
  | remove an allocated link, and put it on
  |  the free store.
  /del {
    dup _next null ne {
      dup _prev 1 index _next _setprev
    } if
    dup _prev null ne {
      dup _next 1 index _prev _setnext
    } {
      dup _next /alloc name
    } ifelse

    free dup null ne {2 copy _setprev} if
    1 index _setnext
    null 1 index _setprev
    /free name
  } bind def

  |============== after ========================
  |
  | [next prev data]/null [next prev data] | --
  |
  | move the second after the first.
  | Both are already allocated links -- we are just 
  |  reordering, without reordering the rest of the links.
  |
  /after {
    del                         | move to the head of free
    dup null eq {pop new pop} { | if it wants to be head, so be it
      free                      | remove from the free store
      dup _next
      dup null ne {null 1 index _setprev} if
      /free name

      | first second
      2 copy _setprev                | first  <-- second
      1 index _next 1 index _setnext | second --> first--third 
      dup dup _next dup null eq 
      {pop pop} ~_setprev ifelse     | second <-- third
      exch _setnext                  | first  --> second
    } ifelse
  } bind def

  |================ iter ======================
  |
  | ~active | bool-if-exited-early
  | ~active: [next prev data] | bool
  |   dict is the next list element, 
  |   bool is true iff we are to exit loop
  |
  | iterate over a list, calling ~active on
  |  each element until ~active returns true.
  | return true if any ~active returned true.
  |
  /iter {
    destruct_exec /ifunc find 0 put
    alloc /ip name {
      ip dup null eq {pop false exit} if
      dup _next /ip name ifunc {true exit} if
    } loop
  } bind def
} moduledef
