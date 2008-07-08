100 dict dup begin

/plugin_ops 100 dict dup begin | [
  /legendre null def
  /combinations null def |]
end def
  
/all {
  1024 /b array getwdir s_ pop 
  getstartupdir s_ (new-plugin.d) fromfiles
  NEW_PLUGINS begin all end
} bind def

end userdict /stats put
