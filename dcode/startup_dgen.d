| -*- mode: d; -*-

|============================ D Startup =================================

|============================= userdict =================================

/dm_type /dgen def

save /startup_common_save name 
/startup_common_buf vmstatus sub 10 div /b array def
startup_common_save capsave {
  getstartupdir (startup_common.d) startup_common_buf readfile mkact exec
  getstartupdir (startup_libs.d) startup_common_buf readfile mkact exec
} stopped startup_common_save restore {
  1024 /b array 0 
  (Unable to load: ) fax
  getstartupdir fax (startup_common.d\n) fax
  0 exch getinterval toconsole
  stop
} if

| (source) /ERROR_NAME
/makeerror ~[~ERRORS ~exch bind ~get bind ~error] def

| (source) code
/error_ops_length 2 def

|============================= userdict =================================

/lock   ~exec bind def    | just to make dnodes & dvts symmetric
/unlock ~exec bind def


(dgen) userdict /myid put

(End of startup\n) toconsole