/LOCK 100 {
  /t 10 def
  /p 10 def
  /f (f.lock) def
  /d null def
  
  /_msg 1024 /b array def
  /msg {
    _msg {
      * getpid unpid * number (: ) fax
      * /lockf find text ( ) fax
      3 -1 roll fax
      (\n) fax
    } tostring toconsole
  } def

  | ~lockfd | --
  /_run {/lockf name
    /fd d f ~appopen inprocess def
    {
      (pending) msg
      {
        (locked) msg
        t 0 sleep
        (done) msg
      } fd lockf
      (unlocked) msg
      p 0 sleep
    } ~loop stopped fd closefd ~stop if
  } def

  /run_fd  {~lockfd _run} def
  /run_ex {~lockfd_ex _run} def
  /run_sh {~lockfd_sh _run} def

  /dir (/mnt/mother/ic) def
  /ncache 10 def

  /_cachetest {
    /n 0 def
    n _ pop out dir (tmp.txt) writeboxcache
    out dir (tmp2.txt) writeboxcache
    out dir (tmp3.txt) writeboxcache
    1 1 ncache 1 sub {
      _ pop {
        dir (tmp.txt) readboxcache pop
      } insave
      {dir (tmp2.txt) readboxcache pop} insave
      {dir (tmp3.txt) readboxcache pop} insave
    } for
    ncache _ pop dir (tmp.txt) rmboxcache
    dir (tmp2.txt) rmboxcache
    dir (tmp3.txt) rmboxcache
  } def

  /tm 8 /x array def
  /sz 1e8 def
  /cachetest {0 sz /b array openlist /n /out} {
    [true false] {
      _ cache_on ~_cachetest tm profiletime
      1000 /b array {
        (user: )   fax * tm 1 get /d ctype 1e6 div tm 0 get add * number (\n) fax
        (system: ) fax * tm 3 get /d ctype 1e6 div tm 2 get add * number (\n) fax
        (uchild: ) fax * tm 5 get /d ctype 1e6 div tm 4 get add * number (\n) fax
        (schild: ) fax * tm 7 get /d ctype 1e6 div tm 6 get add * number (\n) fax
      } tostring toconsole
    } forall
  } localfunc def
} bind moduledef
