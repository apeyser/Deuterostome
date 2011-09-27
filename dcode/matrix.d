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
/MATRIX module 200 dict dup begin

|======================= map
|
| Make a map array in place:
| <l dim_s ... dim_f 1> | <l dim_f*..*dim_s ... dim_f 1>
|

/map {
  1 1 index last -1 0 {/i name | <l> mul
    1 index i get mul                  | <l> mul*<l>_i
    dup 2 index i put                  | <l> mul*<l>_i
  } for
  pop dup dup last get 1 ne {(Illegal cut\n) toconsole halt} if
} bind userdef

| dim_s ... dim_f n | <l dim_f*..*dim_s .. dim_f 1>
/mapn {
  1 exch dup 1 add /l array 1 | s .. f 1 <l> 1
  3 -1 roll -1 0 {/i name     | s .. f   <l> mul  
    3 -1 roll mul             | s .. f-1 <l> mul*f
    dup 2 index i put         | s .. f-1 <l> mul*f
  } for
  pop dup dup last get 1 ne {(Illegal cut\n) toconsole halt} if
} bind userdef

|==================== mat_dict
| dim_s .. dim_f n |  dict
|
| Create a matrix of s. .. .f size
| where /map <d dim_f*...*dim_s> 
|       /matrix <l dim_f*...*dim_s ... dim_f 1>
|
/mat_dict {
  mapn dup 0 get /d array
  openlist /map /matrix makestruct_stack
} bind userdef

|================= mat_dict_get
| matdict | matrix map
|
/mat_dict_get {
  dup /matrix get exch /map get
} bind userdef

|===================== ss
|
| Get a submatrix (row, plane...):
| matrix map i | submatrix submap(map)

/ss {
  /i name                      | matrix map
  1 1 index last getinterval   | matrix submap
  dup 0 get /sz name           | matrix submap
  exch sz i mul sz getinterval | submap submatrix
  exch                         | submatrix submap
} bind userdef

| matdict i | submatrix
/ss_dict {
  exch dup /matrix get exch /map get 3 -1 roll
  ss pop
} bind userdef

|===================== ress =================
| matrix map i len | submatrix map
|
| cuts out len 'rows' starting from 'row' i
| out of matrix, and updates map to describe the new
| matrix
|
/ress {
  /len name /i name exch               | map matrix
  1 index 1 get dup i mul exch len mul | map matrix start length
  getinterval                          | map submatrix
  exch dup 1 get len mul 1 index 0 put | submatrix submap
} bind userdef

| matdict i len | matdict
/ress_dict {
  2 index dup /matrix get exch /map get 4 2 roll
  ress 2 index /map put 1 index /matrix put
} bind userdef

| matrix map submatrix col_s cols | submatrix submap(map)
/columnate {/cols name /col_s name
  exch dup 1 get /sz name dup 2 get /csz name 3 1 roll | map matrix submatrix
  0 2 index length sz div 1 sub | map matrix submatrix 0 rows-1
  0 1 3 -1 roll {/i name        | map matrix submatrix 0
    2 index i sz mul col_s csz mul add cols csz mul getinterval fax
  } for
  0 exch getinterval            | map matrix submatrix
  exch pop                      | map submatrix
  exch cols csz mul dup 2 index 1 put | submatrix submap row-size
  i 1 add mul 1 index 0 put           | submatrix submap
} bind userdef

| matrix map submatrix col | submatrix submap(map)
/columnate1 {
  1 columnate 0 1 index last getinterval
} bind userdef

| matrix map submatrix row_s rows col_s cols | submatrix submap(map)
/compact {/cols name /col_s name /rows name /row_s name
  exch dup 2 get /sz name
  3 -1 roll | map matrix submatrix
  0 row_s 1 row_s rows add {/row name | map matrix submatrix n
    2 index 4 index row cut pop col_s sz mul cols sz mul getinterval fax
  } for
  0 exch getinterval | map matrix submatrix
  exch pop exch      | submatrix map
  cols sz mul dup 2 index 1 put
  rows mul 1 index 0 put
} bind userdef

/ENABLE_ATLAS {

  |========================== matmul_blas
  |
  | Generic form:
  | C <map> beta A <map> trans B <map> trans alpha | C <map>
  | alpha*A^t?*B^t? + beta*C -> C
  | C may not be A or B, but A may be B.
  |
  | You should know the dimensionality of C:
  | C ~ m x n, where m is rows of A^t? and n is columns of B^t?
  |
  | The following are some simplified forms


  |--- matmul_simple
  |
  | C <map> A <map> B <map> | C <map>
  | A*B -> C

  /matmul_simple {
    0 5 1 roll
    false 3 1 roll
    false 1
    matmul_blas
  } bind userdef

  |---- matmul_trans
  |
  | C <map> A <map> transA B <map> transB | C <map>
  | A^t?*B^t? -> C

  /matmul_trans {
    0 7 1 roll 1 matmul_blas
  } bind userdef

  |----- matmul_sum
  |
  | C <map> beta A <map> B <map> alpha | C <map>
  | alpha*A*B + beta*C -> C

  /matmul_sum {
    false 4 1 roll false exch matmul_blas
  } bind userdef

  | y A A_map x | y=Ax
  /vecmatmul_simple {
    0 4 -1 roll false exch 1 vecmatmul_blas
  } bind userdef
} if_compile

end _module
