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

|-------------------------------------------- libs_
| Print out shared libraries in order
| that they were loaded
/libs_ { | -->
  debug_dict begin
  null {dup nextlib not {exit} if} loop   | accumulate all libs
  {
    dup null eq {pop exit} if             | bottom out with null

    begin                                 | enter op lib
    debug_dict begin                      | put libdict over op
    
    line 0                                | get temp buf, index from 0
      2 libnum * number                   | stream output
      * ( : ) text
      hi
      * (\n) text
    0 exch getinterval toconsole          | output used part
    
    end end                               | pop op lib, debug_dict
  } loop
  end                                     | pop debug_dict
} def

save 1024 /b array 
 dup 0 (Plugins: ) fax getplugindir fax (\n) fax 0 exch getinterval toconsole
pop restore

