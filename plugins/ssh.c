#include <string.h>
#include <stdlib.h>
#include <libssh/libssh.h>

#undef PACKAGE_BUGREPORT
#undef PACKAGE_NAME
#undef PACKAGE_STRING
#undef PACKAGE_TARNAME
#undef PACKAGE_VERSION
#include "ssh.h"

#define CHANNELS_MAX 10
static L setup_channel(SSH_SESSION* sess, B* pop_to_frame);

UL ll_type = 0;
L op_hi(void) {return wrap_hi("ssh V1");}
L op_libnum(void) {return wrap_libnum(ll_type);}

static L ssh_errc[100];

L ll_errc[] = {
  SSH_DENIED,
  PROMPT_NUM,
  SSH_ILL_OPT,
  SSH_CHANGED,
  SSH_OTHER,
  CHANNEL_OVF,
  CHANNEL_CLOSED,
  SSH_PASS | ERR_SSH_NO_ERROR,
  SSH_PASS | ERR_SSH_REQUEST_DENIED,
  SSH_PASS | ERR_SSH_INVALID_REQUEST,
  SSH_PASS | ERR_SSH_CONNECTION_LOST,
  SSH_PASS | ERR_SSH_FATAL,
  SSH_PASS | ERR_SSH_INVALID_DATA,
  SSH_PASS | ERR_SSH_EINTR,
  SSH_KNOWN | ERR_SSH_NO_ERROR,
  SSH_KNOWN | ERR_SSH_REQUEST_DENIED,
  SSH_KNOWN | ERR_SSH_INVALID_REQUEST,
  SSH_KNOWN | ERR_SSH_CONNECTION_LOST,
  SSH_KNOWN | ERR_SSH_FATAL,
  SSH_KNOWN | ERR_SSH_INVALID_DATA,
  SSH_KNOWN | ERR_SSH_EINTR,
  CHANNEL_ERR | ERR_SSH_NO_ERROR,
  CHANNEL_ERR | ERR_SSH_REQUEST_DENIED,
  CHANNEL_ERR | ERR_SSH_INVALID_REQUEST,
  CHANNEL_ERR | ERR_SSH_CONNECTION_LOST,
  CHANNEL_ERR | ERR_SSH_FATAL,
  CHANNEL_ERR | ERR_SSH_INVALID_DATA,
  CHANNEL_ERR | ERR_SSH_EINTR,
  SSH_CONNECT | ERR_SSH_NO_ERROR,
  SSH_CONNECT | ERR_SSH_REQUEST_DENIED,
  SSH_CONNECT | ERR_SSH_INVALID_REQUEST,
  SSH_CONNECT | ERR_SSH_CONNECTION_LOST,
  SSH_CONNECT | ERR_SSH_FATAL,
  SSH_CONNECT | ERR_SSH_INVALID_DATA,
  SSH_CONNECT | ERR_SSH_EINTR,
  0L
};

B* ll_errm[] = {
  "*** ssh: connection denied",
  "*** ssh: response number does not match prompts",
  "*** ssh: illegal options",
  "*** ssh: host offering incorrect type of public key",
  "*** ssh: too many channel requested",
  "*** ssh: channel closed",
  "*** ssh: password: no error",
  "*** ssh: password: request denied",
  "*** ssh: password: invalid request",
  "*** ssh: password: connection lost",
  "*** ssh: password: fatal error",
  "*** ssh: password: invalid data",
  "*** ssh: password: system call interrupted",
  "*** ssh: key exchange: no error",
  "*** ssh: key exchange: request denied",
  "*** ssh: key exchange: invalid request",
  "*** ssh: key exchange: connection lost",
  "*** ssh: key exchange: fatal error",
  "*** ssh: key exchange: invalid data",
  "*** ssh: key exchange: system call interrupted",
  "*** ssh: channel: no error",
  "*** ssh: channel: request denied",
  "*** ssh: channel: invalid request",
  "*** ssh: channel: connection lost",
  "*** ssh: channel: fatal error",
  "*** ssh: channel: invalid data",
  "*** ssh: channel: system call interrupted",
  "*** ssh: connect: no error",
  "*** ssh: connect: request denied",
  "*** ssh: connect: invalid request",
  "*** ssh: connect: connection lost",
  "*** ssh: connect: fatal error",
  "*** ssh: connect: invalid data",
  "*** ssh: connect: system call interrupted",
  NULL
};

B* ll_export[] = {
  "hi", (B*) op_hi,
  "libnum", (B*) op_libnum,
  "INIT_", (B*) op_INIT_,
  "FINI_", (B*) op_FINI_,
  "op_sshinteractive", (B*) op_sshinteractive,
  "op_sshdisconnect", (B*) op_sshdisconnect,
  "op_sshconnect", (B*) op_sshconnect,
  "op_sshforward", (B*) op_sshforward,
  "op_sshsession_write", (B*) op_sshsession_write,
  "op_sshsession_read", (B*) op_sshsession_read,
  "op_sshsession_exec", (B*) op_sshsession_exec,
  "op_sshchannel_write", (B*) op_sshchannel_write,
  "op_sshchannel_read", (B*) op_sshchannel_read,
  "op_sshchannel_close", (B*) op_sshchannel_close,
  "", NULL
};

B opaquename[FRAMEBYTES];
static B SSH_NAME[FRAMEBYTES];
static B SSH_INSTRUCTION[FRAMEBYTES];
static B SSH_CHANNEL[FRAMEBYTES];
static B SSH_PROMPTS[FRAMEBYTES];
static B SSH_SESSION_ID[FRAMEBYTES];
static B SSH_CHANNEL_LIST[FRAMEBYTES];
static B SSH_CHANNELS_MAX[FRAMEBYTES];

static L op_readdcode(void) {return wrap_readcode("ssh.d");} 

L op_INIT_(void) {
  if (x2 > CEILexecs) return EXECS_OVF;

  ssh_errc[SSH_NO_ERROR] = ERR_SSH_NO_ERROR;
  ssh_errc[SSH_REQUEST_DENIED] = ERR_SSH_REQUEST_DENIED;
  ssh_errc[SSH_INVALID_REQUEST] = ERR_SSH_INVALID_REQUEST;
  ssh_errc[SSH_CONNECTION_LOST] = ERR_SSH_CONNECTION_LOST;
  ssh_errc[SSH_INVALID_DATA] = ERR_SSH_INVALID_DATA;
  ssh_errc[SSH_EINTR] = ERR_SSH_EINTR;

  makename(SSH_HANDLE, opaquename);
  makename(SSH_NAME, "ssh_name");
  makename(SSH_INSTRUCTION, "instruction");
  makename(SSH_PROMPTS, "prompts");
  makename(SSH_SESSION_ID, "session");
  makename(SSH_CHANNEL, "channel");
  makename(SSH_CHANNEL_LIST, "channel_LIST");
  makename(SSH_CHANNELS_MAX, "channels_max");
  
  TAG(x1) = OP; ATTR(x1) = ACTIVE;
  OP_CODE(x1) = (L) op_readdcode; OP_NAME(x1) = (L) "op_readdcode";
  FREEexecs = x2;
  return OK;
}

L op_FINI_(void) {
  return OK;
}

static L setup_none(SSH_SESSION* sess, B* pop_to_frame) {
  enum ssh_error err;
  switch (ssh_userauth_none(sess, NULL)) {
    case SSH_AUTH_ERROR:
      err = ssh_error_code(sess);
      ssh_disconnect(sess);
      RETURN_ERROR(SSH_PASS | ssh_errc[err]);
    case SSH_AUTH_SUCCESS:
      return setup_channel(sess, pop_to_frame);
    case SSH_AUTH_PARTIAL: case SSH_AUTH_DENIED:
      ssh_disconnect(sess);
      RETURN_ERROR(SSH_DENIED);
    default: //should never happen
      return -1;
  };
}

static L setup_info(SSH_SESSION* sess, B* pop_to_frame) {
  L i; L n; L len;
  B* sshframe;
  B initframe[FRAMEBYTES];
  B* buf; B* list;
  char* name;
  char* instruction;
  char** prompts;
  enum ssh_error err;

  while (! (n = ssh_userauth_kbdint_getnprompts(sess))) {
    switch (ssh_userauth_kbdint(sess, NULL, NULL)) {
      case SSH_AUTH_ERROR:
        err = ssh_error_code(sess);
        ssh_disconnect(sess);
        RETURN_ERROR(SSH_PASS | ssh_errc[err]);
      case SSH_AUTH_SUCCESS:
        return setup_channel(sess, pop_to_frame);
      case SSH_AUTH_DENIED: case SSH_AUTH_PARTIAL:
        return setup_none(sess, pop_to_frame);
      case SSH_AUTH_INFO:
        break;
    }
  }

  if (CEILexecs < x2) return EXECS_OVF;
  
  len = strlen(name = ssh_userauth_kbdint_getname(sess));
  len += strlen(instruction = ssh_userauth_kbdint_getinstruction(sess));
  if (! (prompts = malloc(sizeof(char*)*n))) {
    ssh_disconnect(sess);
    return MEM_OVF;
  }
  for (i = n; i--;) 
    len += strlen(prompts[i] = ssh_userauth_kbdint_getprompt(sess, i, NULL));

  if (! (sshframe = MAKE_OPAQUE_DICT(sizeof(B)*len, 
                                     SSH_SESSION_ID,
                                     SSH_NAME,
                                     SSH_INSTRUCTION,
                                     SSH_PROMPTS))) {
    ssh_disconnect(sess);
    return VM_OVF;
  }

  buf = OPAQUE_MEM(sshframe, buffernameframe);
  TAG(initframe) = (ARRAY | BYTETYPE); ATTR(initframe) = 0;
  VALUE_PTR(initframe) = buf;
  strncpy(buf, name, (ARRAY_SIZE(initframe) = strlen(name)));
  buf += ARRAY_SIZE(initframe);
  OPAQUE_MEM_SET(sshframe, SSH_NAME, initframe);

  VALUE_PTR(initframe) = buf;
  strncpy(buf, name, (ARRAY_SIZE(initframe) = strlen(name)));
  buf += ARRAY_SIZE(initframe);
  OPAQUE_MEM_SET(sshframe, SSH_INSTRUCTION, initframe);

  TAG(initframe) = LIST;
  VALUE_PTR(initframe) = buf;
  LIST_CEIL_PTR(initframe) = (buf += FRAMEBYTES*n);
  OPAQUE_MEM_SET(sshframe, SSH_PROMPTS, initframe);

  i = 0;
  for (list = VALUE_PTR(initframe); 
       list <= LIST_CEIL_PTR(initframe); 
       list += FRAMEBYTES) {
    TAG(list) = (ARRAY | BYTETYPE); ATTR(list) = 0;
    VALUE_PTR(list) = buf;
    strncpy(buf, prompts[i], (ARRAY_SIZE(list) = strlen(prompts[i])));
    ++i;
    buf += ARRAY_SIZE(list);
  }
  free(prompts);

  TAG(initframe) = (NUM | LONGTYPE);
  LONG_VAL(initframe) = (L) sess;
  OPAQUE_MEM_SET(sshframe, SSH_PROMPTS, initframe);
  
  FREEopds = pop_to_frame + FRAMEBYTES;
  moveframe(sshframe, o_1);
  TAG(x1) = OP; ATTR(x1) = ACTIVE | READONLY;
  ARRAY_SIZE(x1) = strlen(VALUE_PTR(x1) = "ssh_getprompt");
  FREEexecs = x2;

  return OK;
}

#ifndef _GNU_SOURCE
static char* strndup(const char* s, size_t n) {
  char* s2;
  if (! (s2 = malloc(sizeof(char)*n+1))) return NULL;
  strncpy(s2, s, n);
  s2[n] = '\0';
  return s2;
}
#endif

// dict [(string)...]
L op_sshinteractive(void) {
  L n;
  B* prompts; B* list;
  SSH_SESSION* sess;
  B listframe[FRAMEBYTES];

  if (FLOORopds > o_2) return OPDS_UNF;
  TEST_OPAQUE(o_2);
  if (! (prompts = OPAQUE_MEM(o_2, SSH_PROMPTS))) return ILL_OPAQUE;
  if (TAG(o_1) != LIST) return OPD_CLA;
  
  if ((LIST_CEIL(o_1) - VALUE_BASE(o_1)) 
      != (LIST_CEIL(prompts) - VALUE_BASE(prompts)))
    RETURN_ERROR(PROMPT_NUM);

  moveframe(o_1, listframe);
  sess = (SSH_SESSION*) LONG_VAL(OPAQUE_MEM(o_2, SSH_SESSION_ID));
  FREEopds = o_2;
  KILL_OPAQUE(o1);
  
  n = 0;
  for (list = VALUE_PTR(listframe); 
       list < LIST_CEIL_PTR(listframe); 
       list += FRAMEBYTES) {
    char* answer;
    if (! (answer = strndup(VALUE_PTR(list), ARRAY_SIZE(list))))
      return MEM_OVF;
    ssh_userauth_kbdint_setanswer(sess, n++, answer);
    free(answer);
  };

  return setup_info(sess, FREEopds);
}

L op_sshdisconnect(void) {
  SSH_SESSION* sess;
  if (FLOORopds > o_1) return OPDS_UNF;
  TEST_OPAQUE(o_1);
  
  sess = (SSH_SESSION*) LONG_VAL(OPAQUE_MEM(o_1, SSH_SESSION_ID));
  ssh_disconnect(sess);
  FREEopds = o_1;
  KILL_OPAQUE(o1);

  return OK;
}

static L setup_channel(SSH_SESSION* sess, B* pop_to_frame) {
  CHANNEL* channel;
  B* sshframe;
  B initframe[FRAMEBYTES];
  enum ssh_error err;

  if (! (channel = channel_open_session(sess))) {
    err = ssh_error_code(sess);
    ssh_disconnect(sess);
    RETURN_ERROR(CHANNEL_ERR | ssh_errc[err]);
  };

  if (! (sshframe = MAKE_OPAQUE_DICT(sizeof(void*)*CHANNELS_MAX, 
                                     SSH_SESSION_ID, 
                                     SSH_CHANNEL, 
                                     SSH_CHANNEL_LIST,
                                     SSH_CHANNELS_MAX))) {
    ssh_disconnect(sess);
    return VM_OVF;
  }

  TAG(initframe) = (NUM | LONGTYPE); ATTR(initframe) = 0;
  LONG_VAL(initframe) = (UL) sess;
  OPAQUE_MEM_SET(sshframe, SSH_SESSION_ID, initframe);
  LONG_VAL(initframe) = (UL) channel;
  OPAQUE_MEM_SET(sshframe, SSH_CHANNEL, initframe);
  LONG_VAL(initframe) = 0;
  OPAQUE_MEM_SET(sshframe, SSH_CHANNELS_MAX, initframe);
  TAG(initframe) = LIST;
  VALUE_PTR(initframe) = OPAQUE_MEM(sshframe, buffernameframe);
  LIST_CEIL_PTR(initframe) = OPAQUE_MEM(sshframe, buffernameframe);
  OPAQUE_MEM_SET(sshframe, SSH_CHANNEL_LIST, initframe);

  FREEopds = pop_to_frame + FRAMEBYTES;
  moveframe(sshframe, o_1);
  return OK;
}

// (pass) ["opt"...]  bool-write-to-known-hosts | ssh-opt-object
L op_sshconnect(void) {
  SSH_OPTIONS* opts;
  SSH_SESSION* sess;
  B* list; UL len; UL tlen;
  char** argv; char** argvp;
  char* argvb; char* argvbp;
  B* pass; char* pass_string;
  B* optlist;
  BOOLEAN write;
  enum ssh_error err;

  if (o_3 < FLOORopds) return OPDS_UNF;
  if (TAG(o_3) != (ARRAY | BYTETYPE)) return OPD_TYP;
  if (TAG(o_2) != LIST) return OPD_CLA;
  if (TAG(o_1) != BOOL) return OPD_CLA;

  write = BOOL_VAL(o_1);
  optlist = o_2;
  pass = o_3;

  len = 0;
  tlen = 0;
  for (list = VALUE_PTR(optlist); 
       list < LIST_CEIL_PTR(optlist); 
       list += FRAMEBYTES) {
    if (TAG(list) != (ARRAY | BYTETYPE)) return OPD_TYP;
    len++;
    tlen += ARRAY_SIZE(list) + 1;
  }

  if (! (argv = malloc(len*sizeof(char*)))) return MEM_OVF;
  if (! (argvb = malloc(tlen*sizeof(char)))) {
    free(argv);
    return MEM_OVF;
  }

  argvp = argv;
  argvbp = argvb;
  for (list = VALUE_PTR(optlist); 
       list < LIST_CEIL_PTR(optlist); 
       list += FRAMEBYTES) {
    *(argvp++) = argvbp;
    strncpy(argvbp, VALUE_PTR(list), ARRAY_SIZE(list));
    argvbp[ARRAY_SIZE(list)] = '\0';
    argvbp += ARRAY_SIZE(list)+1;
  } 

  if (! (opts = ssh_getopt(&len, argv))) {
    free(argv);
    free(argvb);
    RETURN_ERROR(SSH_ILL_OPT);
  }

  if (len != 1) {
    free(argv);
    free(argvb);
    if ((sess = ssh_connect(opts))) ssh_disconnect(sess);
    RETURN_ERROR(SSH_ILL_OPT);
  }

  options_set_host(opts, argv[0]);

  free(argv);
  free(argvb);

  if (! (sess = ssh_connect(opts))) 
    RETURN_ERROR(SSH_CONNECT | ssh_errc[ssh_error_code(NULL)]);

  switch (ssh_is_server_known(sess)) {
    case SSH_SERVER_KNOWN_OK:
      break;
    case SSH_SERVER_KNOWN_CHANGED:
      ssh_disconnect(sess);
      RETURN_ERROR(SSH_CHANGED);
    case SSH_SERVER_NOT_KNOWN:
      if (BOOL_VAL(o_2) && ssh_write_knownhost(sess) < 0) {
        err = ssh_error_code(sess);
        ssh_disconnect(sess);
        RETURN_ERROR(SSH_KNOWN | ssh_errc[err]);
      };
      break;
    case SSH_SERVER_FOUND_OTHER:
      ssh_disconnect(sess);
      RETURN_ERROR(SSH_OTHER);
    case SSH_SERVER_ERROR:
      err = ssh_error_code(sess);
      ssh_disconnect(sess);
      RETURN_ERROR(SSH_PASS | ssh_errc[err]);
  };

  switch (ssh_userauth_autopubkey(sess)) {
    case SSH_AUTH_ERROR:
      err = ssh_error_code(sess);
      ssh_disconnect(sess);
      RETURN_ERROR(SSH_PASS | ssh_errc[err]);
    case SSH_AUTH_SUCCESS:
      return setup_channel(sess, o_3);
    case SSH_AUTH_PARTIAL: case SSH_AUTH_DENIED:
      break;
  };
  
  if (! (pass_string = malloc(sizeof(char)*(ARRAY_SIZE(pass)+1)))) {
    ssh_disconnect(sess);
    return MEM_OVF;
  }
  strncpy(pass_string, VALUE_PTR(pass), ARRAY_SIZE(pass)+1);
  pass_string[ARRAY_SIZE(pass)] = '\0';
  switch (ssh_userauth_password(sess, NULL, pass)) {
    case SSH_AUTH_ERROR:
      err = ssh_error_code(sess);
      free(pass_string);
      ssh_disconnect(sess);
      RETURN_ERROR(SSH_PASS | ssh_errc[err]);
    case SSH_AUTH_SUCCESS:
      free(pass_string);
      return setup_channel(sess, o_3);
    case SSH_AUTH_PARTIAL: case SSH_AUTH_DENIED:
      break;
  };
  free(pass_string);

  switch (ssh_userauth_kbdint(sess, NULL, NULL)) {
    case SSH_AUTH_ERROR:
      err = ssh_error_code(sess);
      ssh_disconnect(sess);
      RETURN_ERROR(SSH_PASS | ssh_errc[err]);
    case SSH_AUTH_SUCCESS:
      return setup_channel(sess, o_3);
    case SSH_AUTH_PARTIAL: case SSH_AUTH_DENIED:
      break;
    case SSH_AUTH_INFO:
      return setup_info(sess, o_3);
  };

  return setup_none(sess, o_3);
}

// dict (remotehost) remoteport | channel-num
L op_sshforward(void) {
  L port;
  SSH_SESSION* sess;
  char* host;
  CHANNEL* channel;
  L channelnum;

  if (FLOORopds > o_3) return OPDS_UNF;
  TEST_OPAQUE(o_3);
  if (! OPAQUE_MEM(o_3, SSH_CHANNEL_LIST)) return ILL_OPAQUE;
  if (TAG(o_2) != (ARRAY | BYTETYPE)) return OPD_TYP;
  if (CLASS(o_1) != NUM) return OPD_CLA;
  if (! VALUE(o_1, &port)) return UNDF_VAL;
  
  sess = (SSH_SESSION*) LONG_VAL(OPAQUE_MEM(o_3, SSH_SESSION_ID));
  host = strndup(VALUE_PTR(o_2), ARRAY_SIZE(o_2));
  if (! (channel = channel_open_forward(sess, host, port, "localhost", 0))) {
    free(host);
    RETURN_ERROR(CHANNEL_ERR | ssh_errc[ssh_error_code(sess)]);
  }
  free(host);
  channelnum = LONG_VAL(OPAQUE_MEM(o_3, SSH_CHANNELS_MAX));
  if (channelnum == CHANNELS_MAX)
    RETURN_ERROR(CHANNEL_OVF);
  ++LONG_VAL(OPAQUE_MEM(o_3, SSH_CHANNELS_MAX));
  
  TAG(o_3) = (NUM | LONGTYPE); ATTR(o_3) = 0;
  LONG_VAL(o_3) = channelnum;
  FREEopds = o_2;
  return OK;
}

// (string) dict | --
L op_sshsession_write(void) {
  CHANNEL* channel;

  if (o_2 > FLOORopds) return OPDS_UNF;
  TEST_OPAQUE(o_1);
  if (! OPAQUE_MEM(o_1, SSH_CHANNEL)) return ILL_OPAQUE;
  if (TAG(o_2) != (ARRAY | BYTETYPE)) return OPD_TYP;
  
  channel = (CHANNEL*) LONG_VAL(OPAQUE_MEM(o_1, SSH_CHANNEL));
  if (! channel_is_open(channel)) RETURN_ERROR(CHANNEL_CLOSED);
  channel_write(channel, VALUE_PTR(o_2), ARRAY_SIZE(o_2));
  
  FREEopds = o_2;
  return OK;
}

// (string) index stdout dict | (string) index
L op_sshsession_read(void) {
  CHANNEL* channel;
  L index, i;
  BUFFER* buf;
  enum ssh_error err;
  
  if (o_4 > FLOORopds) return OPDS_UNF;
  TEST_OPAQUE(o_1);
  if (! OPAQUE_MEM(o_1, SSH_CHANNEL)) return ILL_OPAQUE;
  if (CLASS(o_2) != BOOL) return OPD_CLA;
  if (CLASS(o_3) != NUM) return OPD_CLA;
  if (! VALUE(o_3, &index)) return UNDF_VAL;
  if (TYPE(o_4) != (ARRAY | BYTETYPE)) return OPD_TYP;
  if (index >= ARRAY_SIZE(o_4)) return RNG_CHK;

  channel = (CHANNEL*) LONG_VAL(OPAQUE_MEM(o_1, SSH_CHANNEL));
  if (! channel_is_open(channel)) RETURN_ERROR(CHANNEL_CLOSED);
  buf = buffer_new();
  if ((i = channel_read(channel, buf, 
                        ARRAY_SIZE(o_4)-index, !BOOL_VAL(o_2))) < 0) {
    err = ssh_error_code((SSH_SESSION*) LONG_VAL(OPAQUE_MEM(o_1, 
                                                        SSH_SESSION_ID)));
    buffer_free(buf);
    RETURN_ERROR(CHANNEL_ERR | ssh_errc[err]);
  };
  memcpy(VALUE_PTR(o_4), buffer_get(buf), i);
  index += i;
  buffer_free(buf);
  
  TAG(o_3) = NUM | LONGTYPE; ATTR(o_3) = 0;
  LONG_VAL(o_3) = index;
  FREEopds = o_2;
  return OK;
}

// (string) dict |
L op_sshsession_exec(void) {
  CHANNEL* channel;
  char* exec;
  enum ssh_error err;
  
  if (o_2 > FLOORopds) return OPDS_UNF;
  TEST_OPAQUE(o_1);
  if (! OPAQUE_MEM(o_1, SSH_CHANNEL)) return ILL_OPAQUE;
  if (TAG(o_1) != (ARRAY | BYTETYPE)) return OPD_TYP;
  
  if (! (exec = strndup(VALUE_PTR(o_2), ARRAY_SIZE(o_2)))) return MEM_OVF;
  channel = (CHANNEL*) LONG_VAL(OPAQUE_MEM(o_1, SSH_CHANNEL));
  if (channel_request_exec(channel, exec)) {
    err = ssh_error_code((SSH_SESSION*) LONG_VAL(OPAQUE_MEM(o_1, 
                                                        SSH_SESSION_ID)));
    free(exec);
    RETURN_ERROR(CHANNEL_ERR | ssh_errc[err]);
  }

  FREEopds = o_2;
  return OK;
}
    
// (string) channel dict | --
L op_sshchannel_write(void) {
  CHANNEL* channel;
  B* channel_list;
  L channel_max;
  L channel_num;

  if (o_3 > FLOORopds) return OPDS_UNF;
  TEST_OPAQUE(o_1);
  if (! OPAQUE_MEM(o_1, SSH_CHANNEL_LIST)) return ILL_OPAQUE;
  if (CLASS(o_2) != NUM) return OPD_CLA;
  if (! VALUE(o_2, &channel_num)) return UNDF_VAL;
  if (TAG(o_3) != (ARRAY | BYTETYPE)) return OPD_TYP;  
  channel_max = LONG_VAL(OPAQUE_MEM(o_1, SSH_CHANNELS_MAX));
  if (channel_num >= channel_max || channel_num < 0) return RNG_CHK;

  channel_list = OPAQUE_MEM(o_1, SSH_CHANNEL_LIST);
  channel = (CHANNEL*) LONG_VAL(VALUE_PTR(channel_list)
                                +FRAMEBYTES*channel_num);
  if (! channel_is_open(channel)) RETURN_ERROR(CHANNEL_CLOSED);
  channel_write(channel, VALUE_PTR(o_3), ARRAY_SIZE(o_3));
  
  FREEopds = o_3;
  return OK;
}

// (string) index channel dict | (string) index
L op_sshchannel_read(void) {
  CHANNEL* channel;
  L index, i;
  BUFFER* buf;
  L channel_num, channel_max;
  B* channel_list;
  enum ssh_error err;
  
  if (o_4 > FLOORopds) return OPDS_UNF;
  TEST_OPAQUE(o_1);
  if (! OPAQUE_MEM(o_1, SSH_CHANNEL_LIST)) return ILL_OPAQUE;
  if (CLASS(o_2) != NUM) return OPD_CLA;
  if (! VALUE(o_2, &channel_num)) return UNDF_VAL;
  if (CLASS(o_3) != NUM) return OPD_CLA;
  if (! VALUE(o_3, &index)) return UNDF_VAL;
  if (TYPE(o_4) != (ARRAY | BYTETYPE)) return OPD_TYP;
  if (index >= ARRAY_SIZE(o_4)) return RNG_CHK;

  channel_max = LONG_VAL(OPAQUE_MEM(o_1, SSH_CHANNELS_MAX));
  if (channel_num < 0 || channel_num >= channel_max) return RNG_CHK;
  channel_list = VALUE_PTR(OPAQUE_MEM(o_1, SSH_CHANNEL_LIST));
  channel = (CHANNEL*) LONG_VAL(VALUE_PTR(channel_list)
                                +FRAMEBYTES*channel_num);

  if (! channel_is_open(channel)) RETURN_ERROR(CHANNEL_CLOSED);
  buf = buffer_new();
  if ((i = channel_read(channel, buf, ARRAY_SIZE(o_4)-index, 0)) < 0) {
    err  = ssh_error_code((SSH_SESSION*) LONG_VAL(OPAQUE_MEM(o_1, 
                                                         SSH_SESSION_ID)));
    buffer_free(buf);
    RETURN_ERROR(CHANNEL_ERR | ssh_errc[err]);
  };
  memcpy(VALUE_PTR(o_4), buffer_get(buf), i);
  index += i;
  buffer_free(buf);
  
  TAG(o_3) = NUM | LONGTYPE; ATTR(o_3) = 0;
  LONG_VAL(o_3) = index;
  FREEopds = o_2;
  return OK;
}

// channel dict | --
L op_sshchannel_close(void) {
  CHANNEL* channel;
  L channel_num, channel_max;
  B* channel_list;
  
  if (o_2 > FLOORopds) return OPDS_UNF;
  TEST_OPAQUE(o_1);
  if (! OPAQUE_MEM(o_1, SSH_CHANNEL_LIST)) return ILL_OPAQUE;
  if (CLASS(o_2) != NUM) return OPD_CLA;
  if (! VALUE(o_2, &channel_num)) return UNDF_VAL;
 
  channel_max = LONG_VAL(OPAQUE_MEM(o_1, SSH_CHANNELS_MAX));
  if (channel_num < 0 || channel_num >= channel_max) return RNG_CHK;
  channel_list = VALUE_PTR(OPAQUE_MEM(o_1, SSH_CHANNEL_LIST));
  channel = (CHANNEL*) LONG_VAL(VALUE_PTR(channel_list)
                                +FRAMEBYTES*channel_num);

  if (! channel_is_open(channel)) RETURN_ERROR(CHANNEL_CLOSED);
  channel_close(channel);
  
  FREEopds = o_2;
  return OK;
}
