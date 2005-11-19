/SSH module 100 dict dup

| dict | --
| dict: 
|  ssh_sess
|  ssh_name (string)
|  instruction (string)
|  prompts [(string)...]
/ssh_getprompt {/sshdict name
    SSH begin sshdict begin {
        save /sshsave name
        (SSH interactive: ) toconsole ssh_name toconsole (\n) toconsole
        instruction toconsole (\n) toconsole
        [prompts {
            (\(Prompt: ) toconsole toconsole (\) pop)
            (\(response\)\n) toconsole
        } forall
        (ssh_interactive) toconsole |]
        halt
    } stopped sshsave restore end {sshdict end sshdisconnect stop} if
} userdict 3 -1 roll put

/ssh_interactive { | [
    ] continue
    sshsave capsave
    sshinteractive
} bind def 

/display 1024 /b array def
/last_display [null] def
| (host) (password) [(params)] | ssh-dict
/ssh_display {/params name /pass name /host name
    save userdict /SSHSAVE put {
        pass [host (-Y) params] true 
        SSHSAVE capsave
        sshconnect dup /lastssh name
        (/bin/echo $DISPLAY) lastssh sshsession_write
        display 0 true lastssh sshsession_read 0 exch getinterval _
        SSH begin last_display 0 put end
        console last_display ({} forall dnode_display) send
    } stopped SSHSAVE restore ~stop if
} bind userdict 3 -1 roll put

| (host) (password) [(params)] dnode-port | ssh-dict
/dnode_start 1024 /b array def
/last_port [null] def
/ssh_dnode {/port name
    ssh_display
    dnode_start 0 (nohup dnode-nice -20 ) fax 
      * port * number ( &) fax 0 exch getinterval
    lastssh sshsession_write
    dnode_start 0 false lastssh sshsession_read (\n) fax
    0 exch getinterval toconsole
    SSH begin port last_port 0 put end
    console last_port ({} forall dnode_port) send
} bind def

end _module
