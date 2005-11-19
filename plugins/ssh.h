#ifndef SSH_H
#define SSH_H

#define PLUGIN_NAME ssh
#include "../src/plugin.h"

#define op_sshinteractive EXPORTNAME(op_sshinteractive)
#define op_sshdisconnect EXPORTNAME(op_sshdisconnect)
#define op_sshconnect EXPORTNAME(op_sshconnect)
#define op_sshforward EXPORTNAME(op_sshforward)
#define op_sshsession_write EXPORTNAME(op_sshsession_write)
#define op_sshsession_read EXPORTNAME(op_sshsession_read)
#define op_sshsession_exec EXPORTNAME(op_sshsession_exec)
#define op_sshchannel_write EXPORTNAME(op_sshchannel_write)
#define op_sshchannel_read EXPORTNAME(op_sshchannel_read)
#define op_sshchannel_close EXPORTNAME(op_sshchannel_close)

L op_sshinteractive(void);
L op_sshdisconnect(void);
L op_sshconnect(void);
L op_sshforward(void);
L op_sshsession_write(void);
L op_sshsession_read(void);
L op_sshsession_exec(void);
L op_sshchannel_write(void);
L op_sshchannel_read(void);
L op_sshchannel_close(void);

#define SSH_HANDLE "ssh"

#define SSH_PASS 0x0100L
#define SSH_KNOWN 0x0200L
#define CHANNEL_ERR 0x0300L
#define SSH_CONNECT 0x0400L

#define SSH_DENIED 0x01L
#define PROMPT_NUM 0x02L
#define SSH_ILL_OPT 0x03L
#define SSH_CHANGED 0x04L
#define SSH_OTHER 0x05L
#define CHANNEL_OVF 0x06L
#define CHANNEL_CLOSED 0x07L

#define ERR_SSH_NO_ERROR 0x08L
#define ERR_SSH_REQUEST_DENIED 0x09L 
#define ERR_SSH_INVALID_REQUEST 0x0AL
#define ERR_SSH_CONNECTION_LOST 0x0BL
#define ERR_SSH_FATAL 0xCL
#define ERR_SSH_INVALID_DATA 0x0DL
#define ERR_SSH_EINTR 0x0EL

#endif //SSH_H
