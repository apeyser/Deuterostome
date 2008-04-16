/*====================== D machine 3.0 (Linux): dm3.c =======================

   network operators and more:

    - connect
    - disconnect
    - send
    - getsocket
    - getmyname

*/

/* #ifdef _XOPEN_SOURCE */
/* #if _XOPEN_SOURCE < 500 */
/* #undef _XOPEN_SOURCE */
/* #endif */
/* #ifndef _XOPEN_SOURCE */
/* #define _XOPEN_SOURCE 500 */
/* #endif */

#include "dm.h"
#include "paths.h"
#include "dm3.h"
#include "dm-nextevent.h"

#include <stdio.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <errno.h>
#include <netdb.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>

#ifndef h_errno
extern int h_errno;
#endif

#define SOCK_TIMEOUT (60)

/*---------------------------- support -------------------------------------*/

/*--------- makeDmemory */ 
 
void makeDmemory(B *em, L64 specs[5])
{
  FREEopds = FLOORopds = (B*)(((((size_t) em) >> 3) + 1) << 3);
  CEILopds = FLOORopds + specs[0] * FRAMEBYTES;

  FLOORexecs = FREEexecs = CEILopds;
  CEILexecs = FLOORexecs + specs[1] * FRAMEBYTES;

  FLOORdicts = FREEdicts = CEILexecs;
  CEILdicts = FLOORdicts + specs[2] * FRAMEBYTES;

  FLOORvm = FREEvm = CEILdicts;
  TOPvm = CEILvm  = FLOORvm + specs[3] * 1000000;
}

//-------------------------------- set close-on-exec value for sockets

P closeonexec(P socket) {
  int oldflags = fcntl(socket, F_GETFD, 0);
  if (oldflags < 0) return -errno;
  if (fcntl(socket, F_SETFD, oldflags | FD_CLOEXEC) < 0) return -errno;

  return OK;
}

P nocloseonexec(P socket) {
  int oldflags = fcntl(socket, F_GETFD, 0);
  if (oldflags < 0) return -errno;
  if (fcntl(socket, F_SETFD, oldflags & ~FD_CLOEXEC) < 0) return -errno;

  return OK;
}

// -------- delsocket ---------------------------
// After a socket has been closed, call delsocket to cleanup maxsocket
// and recsocket, and clear flag from sock_fds
void delsocket(P socketfd) {
  FD_CLR(socketfd, &sock_fds);
  if (socketfd == maxsocket-1) {
    P i, j = -1;
    for (i = 0; i < socketfd; i++)
      if (FD_ISSET(i, &sock_fds)) j = i;
    maxsocket = j+1;
  }

  if (recsocket >= maxsocket) recsocket = maxsocket-1;
}



// -------- addsocket -----------------------------
// After opening a socket, call addsocket to increase maxsocket,
// care for recsocket, and add socket to sock_fds.
void addsocket(P socketfd) {
  FD_SET(socketfd, &sock_fds);
  if (socketfd >= maxsocket) maxsocket = socketfd+1;
  if (recsocket < 0) recsocket = socketfd;
}

//------------------- read/write a block from a file descriptor
// assumes that fd is blocking
// returns within secs seconds
// uses SIGALRM internally

// broken into readfd, writefd, and skipreadframefd:
//   readfd -> read from fd n bytes into where
//   writefd -> write to fd n bytes from where
// The under score versions are for use in this modules,
//   the non under score version are externally linkable.

DM_INLINE_STATIC P readfd_(P fd, B* where, P n, P secs) {
  P r, off = 0;

  alarm(secs);
  timeout = 0;
  do {
    if (timeout) return TIMER;
    switch ((r = read(fd, where+off, n))) {
      case 0: 
	if (n) return LOST_CONN;
	break;

      case -1:
	if (errno == EINTR) continue;
	return -errno;

      default:
	n -= r;
	off += r;
    }
  } while (n > 0);

  return OK;
}

P readfd(P fd, B* where, P n, P secs) {
  return readfd_(fd, where, n, secs);
}

DM_INLINE_STATIC P writefd_(P fd, B* where, P n, P secs) {
  ssize_t r, off = 0;
  alarm(secs);
  timeout = 0;
  do {
    if (timeout) return TIMER;
    if ((r = write(fd, where+off, n)) < 0) switch (errno) {
      case EINTR: continue;
      case EPIPE: return LOST_CONN;
      default: return -errno;
    }

    n -= r;
    off += r;
  } while (n > 0);

  return OK;
}

P writefd(P fd, B* where, P n, P secs) {
  return writefd(fd, where, n, secs);
}

/*--------------------------- initialize a socket address */

P init_sockaddr(struct sockaddr_in *name, 
                const char *hostname,
                UW port)
{
  struct hostent *hostinfo;
  memset(name, 0, sizeof(struct sockaddr_in));
  name->sin_family = AF_INET;
  name->sin_port = htons(port);
  hostinfo = gethostbyname(hostname);
  if (hostinfo == 0) return(-h_errno);
  name->sin_addr = *(struct in_addr *) hostinfo->h_addr;

  return OK;
}

#if ENABLE_UNIX_SOCKETS
P init_unix_sockaddr(struct sockaddr_un *name, UW port) {
  char* sock_path = getenv("DMSOCKDIR");
  memset(name, 0, sizeof(struct sockaddr_un));
  if (! sock_path || ! *sock_path) sock_path = DMSOCKDIR;
  if (sock_path[strlen(sock_path)-1] == '/')
    sock_path[strlen(sock_path)-1] = '\0';

  name->sun_family = AF_UNIX;
  snprintf(name->sun_path, sizeof(name->sun_path)-1, "%s/dnode-%i",
           sock_path, port - DM_IPPORT_USERRESERVED);

  return OK;
}
#endif //ENABLE_UNIX_SOCKETS

/*--------------------------- make a server socket */

P make_socket(UW port)
{
  P sock;
  struct sockaddr_in name;
  memset(&name, 0, sizeof(struct sockaddr_in));

  sock = socket(PF_INET, SOCK_STREAM, 0);
  if (sock < 0) return(-1);
  name.sin_family = AF_INET;
  name.sin_port = htons(port);
  name.sin_addr.s_addr = htonl(INADDR_ANY);
  if (bind(sock, (struct sockaddr *) &name, sizeof(name)) < 0)
    return(-1);
  return(sock);
}

#if ENABLE_UNIX_SOCKETS
typedef struct port_list {
  UW port;
  struct port_list* next;
} port_list;
static port_list* ports_first = NULL;
static port_list* ports_last = NULL;

DM_INLINE_STATIC void unlink_socketfile(void) {
  struct sockaddr_un name;
  port_list* i;
  for (i = ports_first; i; i = i->next)
    if (init_unix_sockaddr(&name, i->port) >= 0)
      unlink(name.sun_path);
}

void set_atexit_socks(P port) {
  if (! ports_first) {
    if (atexit(unlink_socketfile))
      error(EXIT_FAILURE, 0, "Can't set exit function");
    ports_last = ports_first = malloc(sizeof(port_list));
  }
  else {
    if (! (ports_last->next = malloc(sizeof(port_list))))
      error(EXIT_FAILURE, errno, "Mem alloc error");
    ports_last = ports_last->next;
  };
  ports_last->port = port;
  ports_last->next = NULL;
}

P make_unix_socket(UW port) {
  char* sock_dir; char* i;
  P sock;
  struct sockaddr_un name;
  struct stat buf;
  mode_t mask;
  
  if (init_unix_sockaddr(&name, port) != OK) return -1;
  if ((sock = socket(PF_UNIX, SOCK_STREAM, 0)) < 0) return -1;

  mask = umask(0);
  if (! (i = sock_dir = strdup(name.sun_path))) return -1;
  while ((i = strchr(++i, '/'))) {
    *i = '\0';
    if (stat(sock_dir, &buf)) {
      if ((errno != ENOTDIR && errno != ENOENT)
          || mkdir(sock_dir, ~(mode_t) 0)) {
	fprintf(stderr, "Unable to mkdir: %s\n", sock_dir);
        free(sock_dir);
        umask(mask);
        return -1;
      }
    }
    else if (! S_ISDIR(buf.st_mode)) {
      errno = ENOTDIR;
      free(sock_dir);
      umask(mask);
      return -1;
    }
    *i = '/';
  }
  free(sock_dir);

  if (! stat(name.sun_path, &buf) && unlink(name.sun_path)) {
      fprintf(stderr, "Unable to unlink: %s\n", name.sun_path);
      umask(mask);
      return -1;
  }
    
  if (bind(sock, (struct sockaddr *) &name, 
           sizeof(name.sun_family)+strlen(name.sun_path)+1)
      < 0) {
      fprintf(stderr, "Unable to bind: %s\n", name.sun_path);
      umask(mask);
      return -1;
  }
  
  set_atexit_socks(port);

  umask(mask);
  return sock;
}
#endif

/*--------------------------- read a message from a socket
 
 The message format is:
 
   frame   of the message string
   frame   of null or box object
   string  body of message string     (padded to ALIGN)
 [ box of message     ]               (padded to ALIGN)

 The contents of a box object are appended to the VM and the
 root object is pushed on the operand stack, whereas a null object is
 discarded. The string contents are written into the string buffer object,
 and an object representing the substring in the buffer is pushed on the
 operand stack. The return code reflects several overflow conditions,
 closure of the connection  from the other side, and misformatted or
 short messages; normal returns are:

  OK   - a message was read
  DONE - an 'end of file' message was received indicating disconnection
  LOST_CONN - lost connection while receiving message
   
*/

P fromsocket(P socket, B *bufferf)
{
  B isnonnative;
  P retc;
  static B xboxf[FRAMEBYTES*2];
  static B* const xrootf = xboxf+FRAMEBYTES;
  B* oldfreevm = FREEvm;

  /*----- get the root frame and evaluate */
  /*----- we give ourselves SOCK_TIMEOUT secs */
  if ((retc = readfd_(socket, xboxf, sizeof(xboxf), SOCK_TIMEOUT)))
    return retc;
  
  if (! GETNATIVEFORMAT(xboxf) || ! GETNATIVEUNDEF(xboxf))
    return BAD_FMT;

  isnonnative = GETNONNATIVE(xboxf);
  if ((retc = deendian_frame(xboxf, isnonnative)) != OK) return retc;
  if ((retc = deendian_frame(xrootf, isnonnative)) != OK) return retc;

  switch (CLASS(xrootf)) {
    case ARRAY:
      if (TYPE(xrootf) == BYTETYPE) {
	if (VALUE_BASE(xrootf) != 0) return BAD_MSG;
	if (ARRAY_SIZE(xrootf) <= 0) return BAD_MSG;
	if (ARRAY_SIZE(xrootf) > ARRAY_SIZE(bufferf)) return RNG_CHK;

	// reserve this space in the passed in buffer object
	VALUE_PTR(xrootf) = VALUE_PTR(bufferf);
	VALUE_PTR(bufferf) += ARRAY_SIZE(xrootf);
	ARRAY_SIZE(bufferf) -= ARRAY_SIZE(xrootf);
	
	if ((retc = readfd_(socket, VALUE_PTR(xrootf), 
			    ARRAY_SIZE(xrootf), SOCK_TIMEOUT)))
	  return retc;

	if (o1 >= CEILopds) return OPDS_OVF;
	moveframe(xrootf, o1);
	FREEopds = o2;
	return OK;
      };
      // else fall through
    case LIST: case DICT: {
      B* irootf;
      B* iboxf;
      if (FREEvm + FRAMEBYTES + SBOXBYTES + BOX_NB(xboxf) >= CEILvm)
	return VM_OVF;

      iboxf = FREEvm;
      TAG(iboxf) = BOX;
      ATTR(iboxf) = PARENT;
      VALUE_PTR(iboxf) = FREEvm + FRAMEBYTES;
      BOX_NB(iboxf) = SBOXBYTES;
      FREEvm += FRAMEBYTES;
      SBOX_CAP(FREEvm) = NULL;
      FREEvm += SBOXBYTES;
      
      irootf = FREEvm;
      moveframe(xrootf, irootf);
      ATTR(irootf) = PARENT;
      FREEvm += FRAMEBYTES;


      if ((retc = readfd_(socket, FREEvm, BOX_NB(xboxf)-FRAMEBYTES, 
			  SOCK_TIMEOUT))) {
	FREEvm = oldfreevm;
	return retc;
      }
      FREEvm += BOX_NB(xboxf)-FRAMEBYTES;

      if (o2 >= CEILopds) {
	FREEvm = oldfreevm;
	return OPDS_OVF;
      }
 
/*----- relocate root object*/
      if ((retc = unfoldobj(irootf, (P) irootf, isnonnative))) {
	FREEvm = oldfreevm;
	return retc;
      }

      moveframe(iboxf, o1);
      moveframe(irootf, o2);
      FREEopds = o3;
      return OK;
    };
      
    default: 
      return BAD_MSG;
  };
}

/*------------------------------- write a message to a socket
   receives a string frame or a composite frame;
   assembles a message in free VM (see 'fromsocket' above). 
   Sends the message to the socket and returns after the complete message has
   been sent. Besides error conditions, return codes are:

    OK        - the message has been sent
    LOST_CONN - the message could not be sent due to a broken connection

    The parameters should not be pointers into the operand stack,
    but safely stored away (i.e., side effects on rootf frame should
    be discarded).
*/

P tosocket(P socket, B* rootf)
{
  P retc = OK;
  P nb;
  B* oldFREEvm = FREEvm;

  if (FREEvm + FRAMEBYTES >= CEILvm) return VM_OVF;
  TAG(FREEvm) = BOX;
  VALUE_PTR(FREEvm) = 0;
  FREEvm += FRAMEBYTES;

  switch (CLASS(rootf)) {
    case ARRAY:
      if (TYPE(rootf) == BYTETYPE) {
	if (FREEvm + FRAMEBYTES + ARRAY_SIZE(rootf) >= CEILvm)
	  return VM_OVF;
      
	moveframe(rootf, FREEvm);
	moveB(VALUE_PTR(rootf), FREEvm+FRAMEBYTES, ARRAY_SIZE(rootf));
	VALUE_PTR(FREEvm) = NULL;
	FREEvm += FRAMEBYTES + ARRAY_SIZE(rootf);
	break;
      };
      // otherwise fall throught

    case LIST: case DICT: {
      W d = 0;
      retc = foldobj(rootf, (P) FREEvm, &d);
    };
    break;

    default:
      return BAD_FMT;
  };

/*----- we give ourselves SOCK_TIMEOUT secs to get this out */
  nb = FREEvm - oldFREEvm;
  FREEvm = oldFREEvm;
  BOX_NB(FREEvm) = nb-FRAMEBYTES;
  SETNATIVE(FREEvm);
  return retc ? retc : writefd_(socket, FREEvm, nb, SOCK_TIMEOUT);
}

/*----------------------------------------------- connect
    servername port | socket

    NOTE: the socket is returned in the form of a NULL object of type
    SOCKETTYPE; such an object can be made or evaluated only by the
    network operators.
*/

P op_connect(void)
{
  UW port;
  LBIG port_;
  P sock, retc, size = PACKET_SIZE;
  struct sockaddr_in serveraddr;

  if (o_2 < FLOORopds) return(OPDS_UNF);
  if (TAG(o_2) != (ARRAY | BYTETYPE)) return(OPD_ERR);
  if (CLASS(o_1) != NUM) return(OPD_CLA);
  if (!VALUE(o_1,&port_)) return(UNDF_VAL);
  port_ += DM_IPPORT_USERRESERVED;
  if (DM_IPPORT_USERRESERVED != DM_IPPORT_USERRESERVED_STANDARD)
    fprintf(stderr, 
	    "Unusual value for IPPORT_USERRESERVED: %i instead of %i\n",
	    DM_IPPORT_USERRESERVED, DM_IPPORT_USERRESERVED_STANDARD);

  if (port_ >= WMAX) return RNG_CHK;
  port = (UW) port_;
  
  if ((FREEvm + ARRAY_SIZE(o_2) + 1) > CEILvm) return(VM_OVF);
  moveB((B *)VALUE_BASE(o_2),FREEvm,ARRAY_SIZE(o_2));
  FREEvm[ARRAY_SIZE(o_2)] = '\000';
  
#if ENABLE_UNIX_SOCKETS
  {
    struct sockaddr_un unixserveraddr;
    if (! strcmp("localhost", (char*)FREEvm)
        && init_unix_sockaddr(&unixserveraddr, port) == OK
        && (sock = socket(PF_UNIX, SOCK_STREAM, 0)) != -1) {
      if (connect(sock, (struct sockaddr *) &unixserveraddr, 
                  sizeof(unixserveraddr.sun_family)
                  + strlen(unixserveraddr.sun_path)))
        close(sock);
      else goto goodsocket;
    };
  };
#endif //ENABLE_UNIX_SOCKETS

  if ((retc = init_sockaddr(&serveraddr, (char*)FREEvm, port)) != OK) 
    return retc;
  if ((sock = socket(PF_INET, SOCK_STREAM, 0)) == -1) return -errno;
  if (setsockopt(sock, SOL_SOCKET, SO_SNDBUF, &size, sizeof(P)) == -1
      || setsockopt(sock, SOL_SOCKET, SO_RCVBUF, &size, sizeof(P)) == -1
      || connect(sock, (struct sockaddr *)&serveraddr,
                 sizeof(serveraddr)) == -1) {
    int errno_ = errno;
    close(sock);
    return -errno_;
  };
  if ((retc = closeonexec(sock))) {
    close(sock);
    return retc;
  }
  
 goodsocket:
  //  if (fcntl(sock, F_SETFL, O_NONBLOCK) == -1)   /* make non-blocking  */
  //  error(EXIT_FAILURE, errno, "fcntl");
  addsocket(sock);
  TAG(o_2) = NULLOBJ | SOCKETTYPE; ATTR(o_2) = 0;
  LONGBIG_VAL(o_2) = sock;
  FREEopds = o_1;
  return OK;
}


/*----------------------------------------------- disconnect
    socket | --
*/

P op_disconnect(void)
{
  if (o_1 < FLOORopds) return(OPDS_UNF);
  if (TAG(o_1) != (NULLOBJ | SOCKETTYPE)) return(OPD_ERR);
  close((P) LONGBIG_VAL(o_1));
  delsocket((P) LONGBIG_VAL(o_1));
  FREEopds = o_1;
  return(OK);
}

/*----------------------------------------------- send
    socket (string) | --
    socket [ num/\* bool? rootobj (string) ] | --

    op_send defined in dm-nextevent.c
    since op_send must handle calling makesocketdead.
    In case of error, returns the active fd as *socketfd.
*/

P int_send(P* socketfd)
{
  P retc; 
  static B rootf[FRAMEBYTES];

  if (o_2 < FLOORopds) return(OPDS_UNF);
  if (TAG(o_2) != (NULLOBJ | SOCKETTYPE)) return(OPD_ERR);
  *socketfd = (P) LONGBIG_VAL(o_2);

  switch (TAG(o_1)) {
    case ARRAY: case DICT: case LIST:
      moveframe(o_1, rootf);
      break;

    default:
      return OPD_CLA;
  }
	  
  if ((retc = tosocket(*socketfd, rootf))) return retc;
 
  FREEopds = o_2; 
  return OK;
}

/*------------------------------------------- getsocket
    -- | socket

The socket is opaquely encoded in a null object of type socket.
*/

P op_getsocket(void)
{
  if (o1 >= CEILopds) return(OPDS_OVF);
  TAG(o1) = NULLOBJ | SOCKETTYPE; 
  ATTR(o1) = 0;
  LONGBIG_VAL(o1) = recsocket;
  FREEopds = o2;
  return(OK);
}

/*------------------------------------------- getmyname
  -- | string

  returns the host's name
*/

P op_getmyname(void)
{
  if (o1 >= CEILopds) return OPDS_OVF;
  if (! myname_frame) return CORR_OBJ;
  moveframe(myname_frame, o1);
  FREEopds = o2;
  return OK;
}

/*------------------------------------------- getmyfqdn
    -- | string

    returns the host's name
*/

P op_getmyfqdn(void)
{
  if (o1 >= CEILopds) return OPDS_OVF;
  if (! myfqdn_frame) return CORR_OBJ;
  moveframe(myfqdn_frame, o1);
  FREEopds = o2;
  return OK;
}
