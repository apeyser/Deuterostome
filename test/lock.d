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

} bind moduledef
