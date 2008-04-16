1000 dict dup begin

/plugin_version 1 def
/plugin_name /rthreads def

/plugin_types 1 dict dup begin |{
end def

/plugin_errs 100 dict dup begin |{
  /parent_rank_missing (No parent rank passed in) def
  /rank_printf_err (Unable to sprintf parent rank) def
  /mpi_type (Illegal operand type) def |}
end def

/plugin_ops 10 dict dup begin |{
  /makerthreads null def
  /killrthreads null def
  /init_ null def
  /fini_ null def
end def

/all {
  getstartupdir (new-plugin.d) fromfiles
  NEW_PLUGINS begin all end
} bind def

end userdict /rthreads put
