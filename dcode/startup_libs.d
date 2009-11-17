| -*- mode: d; -*-

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
