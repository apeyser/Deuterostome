| -*-d-*-
| Site configuration for dnode

/ENABLE_THREADS {
  {
    256 /b array {
      (Starting up threads: ) fax
      * /NUMTHREADS get_compile * number
      (\n) fax
    } tostring toconsole
  } insave
  /NUMTHREADS get_compile makethreads
} if_compile



