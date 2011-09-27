| -*- mode: d; -*-
| Copyright 2011 Alexander Peyser & Wolfgang Nonner
|
| This file is part of Deuterostome.
|
| This program is free software: you can redistribute it and/or modify
| it under the terms of the GNU General Public License as published by
| the Free Software Foundation, either version 2 of the License, or
| (at your option) any later version.
|
| This program is distributed in the hope that it will be useful,
| but WITHOUT ANY WARRANTY; without even the implied warranty of
| MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
| GNU General Public License for more details.
|
| You should have received a copy of the GNU General Public License
| along with this program.  If not, see <http://www.gnu.org/licenses/>.

/LINKEDLIST 100 {

|==================== allocators ========================

  |================ static ===============================
  | size ~default | store
  |
  | preallocate 'size' links, each link data created
  |  by 'default'
  |
  /static {
    [
      /default 3 -1 roll
      /size 5 -1 roll
      /free null
      /_getfree {free dup _next /free name}
      /_setfree {free 1 index _setnext /free name}
    |]
    makestruct_close {
      /free null size {
        [exch null default]
        dup _next dup null eq ~pop {1 index exch _setprev} ifelse
      } repeat def
    } 1 index indict
  } def
  
  |=============== dynamic =================================
  | ~default | store
  |
  | create links on demand (keeping removed elements for reuse),
  |  fill in data element for new elements by 'default'
  |
  /dynamic {
    [
      /default 3 -1 roll
      /free null
      /_getfree {
        free null eq {[null null default]} {free dup _next /free name} ifelse
      }
      /_setfree {free 1 index _setnext /free name} |]
    makestruct_close
  } def

  |==================== linked ==========================
  | linkedlist | store
  |
  | Share store with 'linkedlist' (originally initialized
  |  with another 'linked' or a dynamic or static).
  |
  /linked {/store get} def

|=================== constructor ============================

  |=========== new ======================
  | store | dict
  |
  | store is a storage allocator: linked, static or dynamic
  |   used to add and destroy links.
  |
  /new {
    [
      /store 3 -1 roll
      {/head /tail} ~null forall
      /len 0 |]
    makestruct_close
  } def

|================ funcs called in link list ===============

  |==================== data ================
  | [next prev data] | data
  |
  | get the data element of a link
  |
  /data {2 get} def

  |=================== setdata ==============
  | data [next prev ?] | --
  |
  | replace the data element of a link
  |
  /setdata {2 put} def

  |================= remove ======================
  | [next prev data] | --
  |
  | remove an allocated link, and put it on
  |  the free store.
  |
  /remove {dup _unlink _setfree} def

  |================== prepend ====================
  | -- | [next prev data]
  |
  | allocate a link from the store and make it
  | the head of the list
  |
  /prepend {_getfree null 1 index _insert} def

  |================== append ====================
  | -- | [next prev data]
  |
  | allocate a link from the store and make it
  | the tail of the list
  |
  /append {_getfree tail 1 index _insert} def

  |============== insert ========================
  |
  | [next prev data]/null [next prev data] | --
  |
  | move the second after the first in the same list.
  | Both are already allocated links -- we are just 
  |  reordering, without reordering the rest of the links.
  | If the first link is null, prepend the second to the list.
  |
  /insert {dup _unlink _insert} def

  |==================== move ========================
  | dest-linked-list [next prev data]/null [next prev data] | --
  |
  | move the second link from the current dictionary to the
  |  destination linked list, and insert after the first link 
  |  (which must belong to that list).
  | Both dictionaries must a common store allocator.
  | If the first link is null, prepend the second to the list.
  |
  /move {dup _unlink ~_insert 4 -1 roll indict} def

  |================== nth ======================
  | n | dict
  |
  | get the nth allocate link, counting from 0,
  | or from the tail, counting from -1.
  |
  /nth {
    dup 0 ge {
      head  1  1 4 -1 roll {pop _next} for
    } {
      tail -2 -1 4 -1 roll {pop _prev} for
    } ifelse
  } def

  |=============== getlength ====================
  | -- | length
  |
  | return length of the list
  |
  /getlength ~len def

  |================ iter ======================
  | ~active | bool-if-exited-early
  |
  | ~active: [next prev data] | bool
  |
  |   dict is the next list element, 
  |   bool is true iff we are to exit loop
  |
  | iterate over a list, calling ~active on
  |  each element until ~active returns true.
  | return true if any ~active returned true.
  |
  | expects linked list at top of dict stack, with
  |  LINKEDLIST directly underneath
  |
  /iter {
    false head 3 -1 roll mkpass {              | bool next /func
      3 -1 roll        {pop pop true  exit} if | next /func
      exch dup null eq {pop pop false exit} if | /func next

      [
        3 1 roll               | [ /func next             |]
        1 index mkact 4 1 roll | ~func [ /func next       |]
        dup           5 1 roll | next ~func [ /func next  |]
        _next                  | next ~func [ /func next+ |]]
      {~enddict enddict} push  | bool next+ /func
    } loop                     | bool
  } def

|==================== internal ==========================  

  | [next prev data] | --
  /_unlink {
    dup _next null ne {
      dup _prev 1 index _next _setprev
    } {
      dup _prev /tail name
    } ifelse

    dup _prev null ne {
      dup _next exch _prev _setnext
    } {
      _next /head name
    } ifelse
    /len len 1 sub def
  } def

  | [next prev data]/null [next prev data] | --
  /_insert {
    2 copy _setprev                      | a1 \<-- a2

    1 index null eq {
      head 1 index _setnext              | a2 --\> head
      dup /head name                     | head == a2
    } {
      1 index _next 1 index _setnext     | a2 --\> a3
      2 copy exch _setnext               | a1 --\> a2
    } ifelse
    exch pop

    dup _next null eq {
      /tail name                         | tail == a2
    } {
      dup _next _setprev                 | a2 \<-- a3
    } ifelse
    
    /len len 1 add def
  } def
  
  | [next prev data] | next
  /_next {0 get} def

  | [next prev data] | prev
  /_prev {1 get} def

  | next [? prev data] | --
  /_setnext {0 put} def

  | prev [next ? data] | --
  /_setprev {1 put} def

  |============ allocation store ==================

  | -- | [? ? ?]
  /_getfree {~_getfree store indict} def

  | [? ? ?] | --
  /_setfree {~_setfree store indict} def

} bind moduledef
