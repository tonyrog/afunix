/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1997-2012. All Rights Reserved.
 *
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 *
 * %CopyrightEnd%
 */

#ifdef __linux__
#define __USE_GNU
#define _GNU_SOURCE
#define NO_SA_LEN
#endif

#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdarg.h>
#include <unistd.h>
#include <stdint.h>
#include <memory.h>
#include <ctype.h>
#include <math.h>
#include <sys/types.h>
#include <errno.h>
#include <sys/uio.h>

#define IDENTITY(c) c
#define STRINGIFY_1(b) IDENTITY(#b)
#define STRINGIFY(a) STRINGIFY_1(a)

/* All platforms fail on malloc errors. */
#define FATAL_MALLOC

#include "erl_driver.h"

/* The IS_SOCKET_ERROR macro below is used for portability reasons. While
   POSIX specifies that errors from socket-related system calls should be
   indicated with a -1 return value, some users have experienced non-Windows
   OS kernels that return negative values other than -1. While one can argue
   that such kernels are technically broken, comparing against values less
   than 0 covers their out-of-spec return values without imposing incorrect
   semantics on systems that manage to correctly return -1 for errors, thus
   increasing Erlang's portability.
*/

#define IS_SOCKET_ERROR(val) ((val) < 0)

#define LLU "%llu"
typedef unsigned long long llu_t;

#include <sys/time.h>

#ifdef NETDB_H_NEEDS_IN_H
#include <netinet/in.h>
#endif

#include <netdb.h>
#include <sys/socket.h>
#include <netinet/in.h>

#ifdef DEF_INADDR_LOOPBACK_IN_RPC_TYPES_H
#include <rpc/types.h>
#endif

#include <netinet/tcp.h>
#include <arpa/inet.h>

#include <sys/param.h>
#ifdef HAVE_ARPA_NAMESER_H
#include <arpa/nameser.h>
#endif

#ifdef HAVE_SYS_SOCKIO_H
#include <sys/sockio.h>
#endif

#include <sys/ioctl.h>
#include <sys/un.h>

#if defined (__APPLE__) || defined (__FreeBSD__)
#define HAVE_SUN_LEN_FIELD
#include <sys/ucred.h>
#endif

#if !defined(SOL_LOCAL)
#define SOL_LOCAL 0
#endif

#if defined (__SVR4) && defined (__sun)
#define NO_SA_LEN
#define HAVE_GETPEERUCRED
#include <limits.h>
#include <string.h>
#include <ucred.h>
#endif

// testing #undef LOCAL_PEERCRED
// testing #undef LOCAL_PEERPID
// missing LOCAL_PEERCRED in sys/un.h probably a 10.7.x and earlier
#if defined (__APPLE__) && !defined(LOCAL_PEERCRED)
#define HAVE_GETPEEREID
#endif

// FIXME use dlib!!!
#define DLOG_DEBUG     7
#define DLOG_INFO      6
#define DLOG_NOTICE    5
#define DLOG_WARNING   4
#define DLOG_ERROR     3
#define DLOG_CRITICAL  2
#define DLOG_ALERT     1
#define DLOG_EMERGENCY 0
#define DLOG_NONE     -1

#ifndef DLOG_DEFAULT
#define DLOG_DEFAULT DLOG_NONE
#endif

#define DLOG(level,file,line,args...) do {			\
	if (((level) == DLOG_EMERGENCY) ||			\
	    ((debug_level >= 0) && ((level) <= debug_level))) { \
	    emit_log((level),(file),(line),args);		\
	}							\
    } while(0)

#define DEBUGF(args...) DLOG(DLOG_DEBUG,__FILE__,__LINE__,args)
#define INFOF(args...)  DLOG(DLOG_INFO,__FILE__,__LINE__,args)
#define NOTICEF(args...)  DLOG(DLOG_NOTICE,__FILE__,__LINE__,args)
#define WARNINGF(args...)  DLOG(DLOG_WARNING,__FILE__,__LINE__,args)
#define ERRORF(args...)  DLOG(DLOG_ERROR,__FILE__,__LINE__,args)
#define CRITICALF(args...)  DLOG(DLOG_CRITICAL,__FILE__,__LINE__,args)
#define ALERTF(args...)  DLOG(DLOG_ALERT,__FILE__,__LINE__,args)
#define EMERGENCYF(args...)  DLOG(DLOG_EMERGENCY,__FILE__,__LINE__,args)

#define INVALID_SOCKET -1
#define INVALID_EVENT  -1
#define SOCKET_ERROR   -1

#define SOCKET int
#define HANDLE long int
#define FD_READ    ERL_DRV_READ
#define FD_WRITE   ERL_DRV_WRITE
#define FD_CLOSE   0
#define FD_CONNECT ERL_DRV_WRITE
#define FD_ACCEPT  ERL_DRV_READ

#define sock_connect(s, addr, len)  connect((s), (addr), (len))
#define sock_listen(s, b)           listen((s), (b))
#define sock_bind(s, addr, len)     bind((s), (addr), (len))
#define sock_getopt(s,t,n,v,l)      getsockopt((s),(t),(n),(v),(l))
#define sock_setopt(s,t,n,v,l)      setsockopt((s),(t),(n),(v),(l))
#define sock_name(s, addr, len)     getsockname((s), (addr), (len))
#define sock_peer(s, addr, len)     getpeername((s), (addr), (len))
#define sock_ntohs(x)               ntohs((x))
#define sock_ntohl(x)               ntohl((x))
#define sock_htons(x)               htons((x))
#define sock_htonl(x)               htonl((x))

#define sock_accept(s, addr, len)   accept((s), (addr), (len))
#define sock_send(s,buf,len,flag)   send((s),(buf),(len),(flag))
#define sock_sendto(s,buf,blen,flag,addr,alen) \
                sendto((s),(buf),(blen),(flag),(addr),(alen))
#define sock_sendv(s, vec, size, np, flag) \
		(*(np) = writev((s), (struct iovec*)(vec), (size)))
#define sock_sendmsg(s,msghdr,flag) sendmsg((s),(msghdr),(flag))

#define sock_open(af, type, proto)  socket((af), (type), (proto))
#define sock_close(s)               close((s))
#define sock_shutdown(s, how)       shutdown((s), (how))

#define sock_hostname(buf, len)     gethostname((buf), (len))
#define sock_getservbyname(name,proto) getservbyname((name), (proto))
#define sock_getservbyport(port,proto) getservbyport((port), (proto))

#define sock_recv(s,buf,len,flag)   recv((s),(buf),(len),(flag))
#define sock_recvfrom(s,buf,blen,flag,addr,alen) \
                recvfrom((s),(buf),(blen),(flag),(addr),(alen))
#define sock_recvmsg(s,msghdr,flag) recvmsg((s),(msghdr),(flag))

#define sock_errno()                errno
#define sock_create_event(d)        ((d)->s) /* return file descriptor */
#define sock_close_event(e)                  /* do nothing */

#define inet_driver_select(port, e, mode, on) \
                                    driver_select(port, e, mode | (on?ERL_DRV_USE:0), on)

#define sock_select(d, flags, onoff) do { \
        (d)->event_mask = (onoff) ? \
                 ((d)->event_mask | (flags)) : \
                 ((d)->event_mask & ~(flags)); \
        DEBUGF("sock_select(%ld): flags=%02X, onoff=%d, event_mask=%02lX", \
		(long) (d)->port, (flags), (onoff), (unsigned long) (d)->event_mask); \
        inet_driver_select((d)->port, (ErlDrvEvent)(long)(d)->event, (flags), (onoff)); \
   } while(0)


#include <fcntl.h>
#ifdef NB_O_NDELAY              /* Nothing needs this? */
#   define NB_FLAG O_NDELAY
#   ifndef ERRNO_BLOCK          /* allow override (e.g. EAGAIN) via Makefile */
#      define ERRNO_BLOCK EWOULDBLOCK
#   endif
#else  /* !NB_O_NDELAY */       /* The True Way - POSIX!:-) */
#   define NB_FLAG O_NONBLOCK
#   define ERRNO_BLOCK EAGAIN
#endif /* !NB_O_NDELAY */
#define SET_BLOCKING(fd) \
    fcntl((fd), F_SETFL, fcntl((fd), F_GETFL, 0) & ~NB_FLAG)
#define SET_NONBLOCKING(fd) \
    fcntl((fd), F_SETFL,  fcntl((fd), F_GETFL, 0) | NB_FLAG)

#define HAVE_SOCKLEN_T

#ifdef HAVE_SOCKLEN_T
#  define SOCKLEN_T socklen_t
#else
#  define SOCKLEN_T int
#endif

#include "packet_parser.h"


#define get_int64(s) (((Uint64)(((unsigned char*) (s))[0]) << 56) | \
                      (((Uint64)((unsigned char*) (s))[1]) << 48) | \
                      (((Uint64)((unsigned char*) (s))[2]) << 40) | \
                      (((Uint64)((unsigned char*) (s))[3]) << 32) | \
                      (((Uint64)((unsigned char*) (s))[4]) << 24) | \
                      (((Uint64)((unsigned char*) (s))[5]) << 16) | \
                      (((Uint64)((unsigned char*) (s))[6]) << 8)  | \
                      (((Uint64)((unsigned char*) (s))[7])))

#define put_int64(i, s) do {((char*)(s))[0] = (char)((int64_t)(i)>>56) & 0xff;\
                            ((char*)(s))[1] = (char)((int64_t)(i)>>48) & 0xff;\
                            ((char*)(s))[2] = (char)((int64_t)(i)>>40) & 0xff;\
                            ((char*)(s))[3] = (char)((int64_t)(i)>>32) & 0xff;\
                            ((char*)(s))[4] = (char)((int64_t)(i)>>24) & 0xff;\
                            ((char*)(s))[5] = (char)((int64_t)(i)>>16) & 0xff;\
                            ((char*)(s))[6] = (char)((int64_t)(i)>>8)  & 0xff;\
                            ((char*)(s))[7] = (char)((int64_t)(i))     & 0xff;\
                           } while (0) 

#define get_int32(s) ((((unsigned char*) (s))[0] << 24) | \
                      (((unsigned char*) (s))[1] << 16) | \
                      (((unsigned char*) (s))[2] << 8)  | \
                      (((unsigned char*) (s))[3]))

#define put_int32(i, s) do {((char*)(s))[0] = (char)((i) >> 24) & 0xff;   \
                            ((char*)(s))[1] = (char)((i) >> 16) & 0xff;   \
                            ((char*)(s))[2] = (char)((i) >> 8)  & 0xff;   \
                            ((char*)(s))[3] = (char)(i)         & 0xff;} \
                        while (0)

#define get_int24(s) ((((unsigned char*) (s))[0] << 16) | \
                      (((unsigned char*) (s))[1] << 8)  | \
                      (((unsigned char*) (s))[2]))

#define put_int24(i, s) do {((char*)(s))[0] = (char)((i) >> 16) & 0xff;  \
                            ((char*)(s))[1] = (char)((i) >> 8)  & 0xff;  \
                            ((char*)(s))[2] = (char)(i)         & 0xff;} \
                        while (0)

#define get_int16(s) ((((unsigned char*)  (s))[0] << 8) | \
                      (((unsigned char*)  (s))[1]))


#define put_int16(i, s) do {((char*)(s))[0] = (char)((i) >> 8) & 0xff;  \
                            ((char*)(s))[1] = (char)(i)        & 0xff;} \
                        while (0)

#define get_int8(s) ((((unsigned char*)  (s))[0] ))


#define put_int8(i, s) do {((unsigned char*)(s))[0] = (i) & 0xff;} while (0)


#define memzero(ptr, sz) memset((void*)(ptr),'\0',(sz))

#define VALGRIND_MAKE_MEM_DEFINED(ptr,size)

/*
  Magic errno value used locally for return of {error, system_limit}
  - the emulator definition of SYSTEM_LIMIT is not available here.
*/
#define INET_ERRNO_SYSTEM_LIMIT  (15 << 8)

// Hack to handle R15 driver used with pre R15 driver
#if ERL_DRV_EXTENDED_MAJOR_VERSION == 1
typedef int  ErlDrvSizeT;
typedef int  ErlDrvSSizeT;
#endif

#if (ERL_DRV_EXTENDED_MAJOR_VERSION > 2) || ((ERL_DRV_EXTENDED_MAJOR_VERSION == 2) && (ERL_DRV_EXTENDED_MINOR_VERSION >= 1))
#define OUTPUT_TERM(p, message, len) erl_drv_output_term((p)->dport,(message),(len))
#define SEND_TERM(p, to, message, len) erl_drv_send_term((p)->dport,(to),(message),(len))

#else
#define OUTPUT_TERM(p, message, len) driver_output_term((p)->port,(message),(len))
#define SEND_TERM(p, to, message, len) driver_send_term((p)->port,(to),(message),(len))
#endif

/*----------------------------------------------------------------------------
** Interface constants.
** 
** This section must be "identical" to the corresponding inet_int.hrl
*/

/* general address encode/decode tag */
#define INET_AF_INET        1
#define INET_AF_INET6       2
#define INET_AF_ANY         3 /* INADDR_ANY or IN6ADDR_ANY_INIT */
#define INET_AF_LOOPBACK    4 /* INADDR_LOOPBACK or IN6ADDR_LOOPBACK_INIT */

#define INET_AF_UNIX        5 /* maybe compatible ?  */

/* open and INET_REQ_GETTYPE enumeration */
#define INET_TYPE_STREAM    1
#define INET_TYPE_DGRAM     2
#define INET_TYPE_SEQPACKET 3

/* INET_LOPT_MODE options */
#define INET_MODE_LIST      0
#define INET_MODE_BINARY    1

/* INET_LOPT_DELIVER options */
#define INET_DELIVER_PORT   0
#define INET_DELIVER_TERM   1

/* INET_LOPT_ACTIVE options */
#define INET_PASSIVE        0  /* false */
#define INET_ACTIVE         1  /* true */
#define INET_ONCE           2  /* true; active once then passive */
#define INET_MULTI          3  /* true; active N then passive */

/* INET_REQ_GETSTATUS enumeration */
#define INET_F_OPEN         0x0001
#define INET_F_BOUND        0x0002
#define INET_F_ACTIVE       0x0004
#define INET_F_LISTEN       0x0008
#define INET_F_CON          0x0010
#define INET_F_ACC          0x0020
#define INET_F_LST          0x0040
#define INET_F_BUSY         0x0080 
#define INET_F_MULTI_CLIENT 0x0100 /* Multiple clients for one descriptor, i.e. multi-accept */

/* One numberspace for *_REC_* so if an e.g UDP request is issued
** for a TCP socket, the driver can protest.
*/
#define INET_REQ_OPEN          1
#define INET_REQ_CLOSE         2
#define INET_REQ_CONNECT       3
#define INET_REQ_PEER          4
#define INET_REQ_NAME          5
#define INET_REQ_BIND          6
#define INET_REQ_SETOPTS       7
#define INET_REQ_GETOPTS       8
/* #define INET_REQ_GETIX         9  NOT USED ANY MORE */
/* #define INET_REQ_GETIF         10 REPLACE BY NEW STUFF */
#define INET_REQ_GETSTAT       11
#define INET_REQ_GETHOSTNAME   12
#define INET_REQ_FDOPEN        13
#define INET_REQ_GETFD         14
#define INET_REQ_GETTYPE       15
#define INET_REQ_GETSTATUS     16
#define INET_REQ_GETSERVBYNAME 17
#define INET_REQ_GETSERVBYPORT 18
#define INET_REQ_SETNAME       19
#define INET_REQ_SETPEER       20
#define INET_REQ_GETIFLIST     21
#define INET_REQ_IFGET         22
#define INET_REQ_IFSET         23
#define INET_REQ_SUBSCRIBE     24
#define INET_REQ_GETIFADDRS    25
#define INET_REQ_ACCEPT        26
#define INET_REQ_LISTEN        27
#define INET_REQ_IGNOREFD      28

/* TCP requests */
/* #define TCP_REQ_ACCEPT         40 MOVED */
/* #define TCP_REQ_LISTEN         41 MERGED */
#define TCP_REQ_RECV           42
#define TCP_REQ_UNRECV         43
#define TCP_REQ_SHUTDOWN       44
/* UDP and SCTP requests */

/* INET_REQ_SUBSCRIBE sub-requests */
#define INET_SUBS_EMPTY_OUT_Q  1

/* TCP additional flags */
#define TCP_ADDF_DELAY_SEND    1
#define TCP_ADDF_CLOSE_SENT    2 /* Close sent (active mode only) */
#define TCP_ADDF_DELAYED_CLOSE_RECV 4 /* If receive fails, report {error,closed} (passive mode) */
#define TCP_ADDF_DELAYED_CLOSE_SEND 8 /* If send fails, report {error,closed} (passive mode) */

/* *_REQ_* replies */
#define INET_REP_ERROR       0
#define INET_REP_OK          1
// #define INET_REP             2  - only used by SCTP in inet_drv

/* INET_REQ_SETOPTS and INET_REQ_GETOPTS options */
#define INET_OPT_REUSEADDR  0   /* enable/disable local address reuse */
#define INET_OPT_KEEPALIVE  1   /* enable/disable keep connections alive */
#define INET_OPT_DONTROUTE  2   /* enable/disable routing for messages */
#define INET_OPT_LINGER     3   /* linger on close if data is present */
#define INET_OPT_BROADCAST  4   /* enable/disable transmission of broadcast */
#define INET_OPT_OOBINLINE  5   /* enable/disable out-of-band data in band */
#define INET_OPT_SNDBUF     6   /* set send buffer size */
#define INET_OPT_RCVBUF     7   /* set receive buffer size */
#define INET_OPT_PRIORITY   8   /* set priority */
#define INET_OPT_TOS        9   /* Set type of service */
#define TCP_OPT_NODELAY     10  /* don't delay send to coalesce packets */
#define UDP_OPT_MULTICAST_IF 11  /* set/get IP multicast interface */
#define UDP_OPT_MULTICAST_TTL 12 /* set/get IP multicast timetolive */
#define UDP_OPT_MULTICAST_LOOP 13 /* set/get IP multicast loopback */
#define UDP_OPT_ADD_MEMBERSHIP 14 /* add an IP group membership */
#define UDP_OPT_DROP_MEMBERSHIP 15 /* drop an IP group membership */
#define INET_OPT_IPV6_V6ONLY 16 /* IPv6 only socket, no mapped v4 addrs */

/* LOPT is local options */
#define INET_LOPT_BUFFER      20  /* min buffer size hint */
#define INET_LOPT_HEADER      21  /* list header size */
#define INET_LOPT_ACTIVE      22  /* enable/disable active receive */
#define INET_LOPT_PACKET      23  /* packet header type (TCP) */
#define INET_LOPT_MODE        24  /* list or binary mode */
#define INET_LOPT_DELIVER     25  /* port or term delivery */
#define INET_LOPT_EXITONCLOSE 26  /* exit port on active close or not ! */
#define INET_LOPT_TCP_HIWTRMRK     27  /* set local high watermark */
#define INET_LOPT_TCP_LOWTRMRK     28  /* set local low watermark */
                                /* 29  unused */
#define INET_LOPT_TCP_SEND_TIMEOUT 30  /* set send timeout */
#define INET_LOPT_TCP_DELAY_SEND   31  /* Delay sends until next poll */
#define INET_LOPT_PACKET_SIZE      32  /* Max packet size */
#define INET_LOPT_UDP_READ_PACKETS 33  /* Number of packets to read */
#define INET_OPT_RAW               34  /* Raw socket options */
#define INET_LOPT_TCP_SEND_TIMEOUT_CLOSE 35  /* auto-close on send timeout or not */
#define INET_LOPT_TCP_MSGQ_HIWTRMRK     36  /* set local high watermark */
#define INET_LOPT_TCP_MSGQ_LOWTRMRK     37  /* set local low watermark */
#define INET_LOPT_NETNS             38  /* Network namespace pathname */
#define INET_LOPT_TCP_SHOW_ECONNRESET 39  /* tell user about incoming RST */
#define INET_LOPT_LINE_DELIM        40  /* Line delimiting char */

#define UNIX_OPT_PEERCRED  201
#define UNIX_OPT_PEERPID   202

/* INET_REQ_GETSTAT enumeration */
#define INET_STAT_RECV_CNT   1
#define INET_STAT_RECV_MAX   2
#define INET_STAT_RECV_AVG   3
#define INET_STAT_RECV_DVI   4
#define INET_STAT_SEND_CNT   5
#define INET_STAT_SEND_MAX   6
#define INET_STAT_SEND_AVG   7
#define INET_STAT_SEND_PND   8
#define INET_STAT_RECV_OCT   9      /* received octets */ 
#define INET_STAT_SEND_OCT   10     /* sent octets */


/*
** End of interface constants.
**--------------------------------------------------------------------------*/

#define INET_STATE_CLOSED          (0)
#define INET_STATE_OPEN            (INET_F_OPEN)
#define INET_STATE_BOUND           (INET_STATE_OPEN | INET_F_BOUND)
#define INET_STATE_CONNECTED       (INET_STATE_BOUND | INET_F_ACTIVE)
#define INET_STATE_LISTENING       (INET_STATE_BOUND | INET_F_LISTEN)
#define INET_STATE_CONNECTING      (INET_STATE_BOUND | INET_F_CON)
#define INET_STATE_ACCEPTING       (INET_STATE_LISTENING | INET_F_ACC)
#define INET_STATE_MULTI_ACCEPTING (INET_STATE_ACCEPTING | INET_F_MULTI_CLIENT)

#define IS_OPEN(d) \
 (((d)->state & INET_F_OPEN) == INET_F_OPEN)

#define IS_BOUND(d) \
 (((d)->state & INET_F_BOUND) == INET_F_BOUND)

#define IS_CONNECTED(d) \
  (((d)->state & INET_STATE_CONNECTED) == INET_STATE_CONNECTED)

#define IS_CONNECTING(d) \
  (((d)->state & INET_F_CON) == INET_F_CON)

#define IS_BUSY(d) \
  (((d)->state & INET_F_BUSY) == INET_F_BUSY)

#define INET_MAX_OPT_BUFFER (64*1024)

#define INET_DEF_BUFFER     1460        /* default buffer size */
#define INET_MIN_BUFFER     1           /* internal min buffer */

#define INET_HIGH_WATERMARK (1024*8) /* 8k pending high => busy  */
#define INET_LOW_WATERMARK  (1024*4) /* 4k pending => allow more */
#define INET_HIGH_MSGQ_WATERMARK (1024*8) /* 8k pending high => busy  */
#define INET_LOW_MSGQ_WATERMARK  (1024*4) /* 4k pending => allow more */

#define INET_INFINITY  0xffffffff  /* infinity value */

#define INET_MAX_ASYNC 1           /* max number of async queue ops */

/* INET_LOPT_UDP_PACKETS */
#define INET_PACKET_POLL     5   /* maximum number of packets to poll */

/* INET Ignore states */
#define INET_IGNORE_NONE 0
#define INET_IGNORE_READ 1
#define INET_IGNORE_WRITE 1 << 1

/* Max length of Erlang Term Buffer (for outputting structured terms):  */
#define PACKET_ERL_DRV_TERM_DATA_LEN  32

#define BIN_REALLOC_MARGIN(x)  ((x)/4)  /* 25% */

/* The general purpose sockaddr */
typedef union {
    struct sockaddr sa;
    struct sockaddr_in sai;
    struct sockaddr_un sau;
#ifdef HAVE_IN6
    struct sockaddr_in6 sai6;
#endif
} inet_address;


/* for AF_INET & AF_INET6 */
#define inet_address_port(x) ((x)->sai.sin_port)

#if defined(HAVE_IN6) && defined(AF_INET6)
#define addrlen(family) \
   ((family == AF_INET) ? sizeof(struct in_addr) : \
    ((family == AF_INET6) ? sizeof(struct in6_addr) : 0))
#else
#define addrlen(family) \
   ((family == AF_INET) ? sizeof(struct in_addr) : 0)
#endif

typedef struct _multi_timer_data {
    ErlDrvNowData when;
    ErlDrvTermData caller;
    void (*timeout_function)(ErlDrvData drv_data, ErlDrvTermData caller);
    struct _multi_timer_data *next;
    struct _multi_timer_data *prev;
} MultiTimerData;

static MultiTimerData *add_multi_timer(MultiTimerData **first, ErlDrvPort port, 
			    ErlDrvTermData caller, unsigned timeout,
			    void (*timeout_fun)(ErlDrvData drv_data,
						ErlDrvTermData caller));
static void fire_multi_timers(MultiTimerData **first, ErlDrvPort port,
			      ErlDrvData data);
static void remove_multi_timer(MultiTimerData **first, ErlDrvPort port, MultiTimerData *p);

static void afunix_multi_timeout(ErlDrvData e, ErlDrvTermData caller);
static void clean_multi_timers(MultiTimerData **first, ErlDrvPort port);

typedef struct {
    int            id;      /* id used to identify reply */
    ErlDrvTermData caller;  /* recipient of async reply */
    int            req;     /* Request id (CONNECT/ACCEPT/RECV) */
    union {
	unsigned       value; /* Request timeout (since op issued,not started) */
	MultiTimerData *mtd;
    } tmo;
    ErlDrvMonitor monitor;
} inet_async_op;

typedef struct inet_async_multi_op_ {
    inet_async_op op;
    struct inet_async_multi_op_ *next;
} inet_async_multi_op;


typedef struct subs_list_ {
  ErlDrvTermData subscriber;
  struct subs_list_ *next;
} subs_list;

#define NO_PROCESS 0
#define NO_SUBSCRIBERS(SLP) ((SLP)->subscriber == NO_PROCESS)


typedef struct {
    SOCKET s;                   /* the socket or INVALID_SOCKET if not open */
    HANDLE event;               /* Event handle (same as s in unix) */
    long  event_mask;           /* current FD events */

    ErlDrvPort  port;           /* the port identifier */
    ErlDrvTermData dport;       /* the port identifier as DriverTermData */
    int   state;                /* status */
    int   prebound;             /* only set when opened with inet_fdopen */
    int   mode;                 /* BINARY | LIST
				   (affect how to interpret hsz) */
    int   exitf;                /* exit port on close or not */
    int   deliver;              /* Delivery mode, TERM or PORT */

    ErlDrvTermData caller;      /* recipient of sync reply */
    ErlDrvTermData busy_caller; /* recipient of sync reply when caller busy.
				 * Only valid while INET_F_BUSY. */

    inet_async_op* oph;          /* queue head or NULL */
    inet_async_op* opt;          /* queue tail or NULL */
    inet_async_op  op_queue[INET_MAX_ASYNC];  /* call queue */

    int   active;               /* 0 = passive, 1 = active, 2 = active once */
    int16_t active_count;        /* counter for {active,N} */
    int   stype;                /* socket type:
				    SOCK_STREAM/SOCK_DGRAM/SOCK_SEQPACKET   */
    int   sprotocol;            /* socket protocol:
				   IPPROTO_TCP|IPPROTO_UDP|IPPROTO_SCTP     */
    int   sfamily;              /* address family */
    enum PacketParseType htype; /* header type (TCP only?) */
    unsigned int psize;         /* max packet size (TCP only?) */
    inet_address remote;        /* remote address for connected sockets */
    inet_address peer_addr;     /* fake peer address */
    inet_address name_addr;     /* fake local address */

    inet_address* peer_ptr;    /* fake peername or NULL */
    inet_address* name_ptr;    /* fake sockname or NULL */

    int   bufsz;                /* minimum buffer constraint */
    unsigned int hsz;           /* the list header size, -1 is large !!! */
    /* statistics */
    uint64_t      recv_oct;     /* number of received octets, 64 bits */
    unsigned long recv_cnt;     /* number of packets received */
    unsigned long recv_max;     /* maximum packet size received */
    double recv_avg;            /* average packet size received */
    double recv_dvi;            /* avarage deviation from avg_size */
    uint64_t      send_oct;     /* number of octets sent, 64 bits */
    char          delimiter;    /* Line delimiting character (def: '\n')  */
    unsigned long send_cnt;     /* number of packets sent */
    unsigned long send_max;     /* maximum packet send */
    double send_avg;            /* average packet size sent */

    subs_list empty_out_q_subs; /* Empty out queue subscribers */
    int is_ignored;             /* if a fd is ignored by the inet_drv.
				   This flag should be set to true when
				   the fd is used outside of inet_drv. */
} inet_descriptor;

static void send_to_subscribers(inet_descriptor* desc, subs_list *, int,
				ErlDrvTermData [], int);
static void free_subscribers(subs_list*);
static int save_subscriber(subs_list *, ErlDrvTermData);

#define TCP_MAX_PACKET_SIZE 0x4000000  /* 64 M */

#define MAX_VSIZE 16		/* Max number of entries allowed in an I/O
				 * vector sock_sendv().
				 */

static int afunix_init(void);
static void afunix_finish(void);
static void afunix_stop(ErlDrvData);
static void afunix_command(ErlDrvData, char*, ErlDrvSizeT);
static void afunix_commandv(ErlDrvData, ErlIOVec*);
static void afunix_flush(ErlDrvData drv_data);
static void afunix_drv_input(ErlDrvData, ErlDrvEvent);
static void afunix_drv_output(ErlDrvData data, ErlDrvEvent event);
static ErlDrvData afunix_start(ErlDrvPort, char* command);
static ErlDrvSSizeT afunix_ctl(ErlDrvData, unsigned int,
				 char*, ErlDrvSizeT, char**, ErlDrvSizeT);
static void afunix_timeout(ErlDrvData);
static void afunix_process_exit(ErlDrvData, ErlDrvMonitor *); 
static void inet_stop_select(ErlDrvEvent, void*); 

static ErlDrvEntry afunix_driver_entry;


typedef struct {
    inet_descriptor inet;       /* common data structure (DON'T MOVE) */
    int   high;                 /* high watermark */
    int   low;                  /* low watermark */
    int   send_timeout;         /* timeout to use in send */
    int   send_timeout_close;   /* auto-close socket on send_timeout */
    int   busy_on_send;         /* busy on send with timeout! */
    int   i_bufsz;              /* current input buffer size (<= bufsz) */
    ErlDrvBinary* i_buf;        /* current binary buffer */
    char*         i_ptr;        /* current pos in buf */
    char*         i_ptr_start;  /* packet start pos in buf */
    int           i_remain;     /* remaining chars to read */
    int           tcp_add_flags;/* Additional TCP descriptor flags */
    int           http_state;   /* 0 = response|request  1=headers fields */
    inet_async_multi_op *multi_first;/* NULL == no multi-accept-queue, op is in ordinary queue */
    inet_async_multi_op *multi_last;
    MultiTimerData *mtd;        /* Timer structures for multiple accept */
} tcp_descriptor;

/* send function */
static int tcp_send(tcp_descriptor* desc, char* ptr, ErlDrvSizeT len);
static int tcp_sendv(tcp_descriptor* desc, ErlIOVec* ev);
static int tcp_recv(tcp_descriptor* desc, int request_len);
static int tcp_deliver(tcp_descriptor* desc, int len);

static int afunix_output(tcp_descriptor* desc, HANDLE event);
static int afunix_input(tcp_descriptor* desc, HANDLE event);

/* convert descriptor poiner to inet_descriptor pointer */
#define INETP(d) (&(d)->inet)

static int async_ref = 0;          /* async reference id generator */
#define NEW_ASYNC_ID() ((async_ref++) & 0xffff)

/* check for transition from active to passive */
#define INET_CHECK_ACTIVE_TO_PASSIVE(inet)                              \
    do {                                                                \
        if ((inet)->active == INET_ONCE)                                \
            (inet)->active = INET_PASSIVE;                              \
        else if ((inet)->active == INET_MULTI && --((inet)->active_count) == 0) { \
            (inet)->active = INET_PASSIVE;                              \
            packet_passive_message(inet);                               \
        }                                                               \
    } while (0)


static ErlDrvTermData am_ok;
static ErlDrvTermData am_tcp;
static ErlDrvTermData am_udp;
static ErlDrvTermData am_error;
static ErlDrvTermData am_inet_async;
static ErlDrvTermData am_inet_reply;
static ErlDrvTermData am_timeout;
static ErlDrvTermData am_closed;
static ErlDrvTermData am_tcp_passive;
static ErlDrvTermData am_tcp_closed;
static ErlDrvTermData am_tcp_error;
static ErlDrvTermData am_udp_error;
static ErlDrvTermData am_empty_out_q;
static ErlDrvTermData am_ssl_tls;


/* speical errors for bad ports and sequences */
#define EXBADPORT "exbadport"
#define EXBADSEQ  "exbadseq"


static int inet_init(void);
static ErlDrvSSizeT ctl_reply(int, char*, ErlDrvSizeT, char**, ErlDrvSizeT);


#if HAVE_IN6
#  if ! defined(HAVE_IN6ADDR_ANY) || ! HAVE_IN6ADDR_ANY
#    if HAVE_DECL_IN6ADDR_ANY_INIT
static const struct in6_addr in6addr_any = { { IN6ADDR_ANY_INIT } };
#    else
static const struct in6_addr in6addr_any =
    { { { 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 } } };
#    endif /* HAVE_IN6ADDR_ANY_INIT */
#  endif /* ! HAVE_DECL_IN6ADDR_ANY */

#  if ! defined(HAVE_IN6ADDR_LOOPBACK) || ! HAVE_IN6ADDR_LOOPBACK
#    if HAVE_DECL_IN6ADDR_LOOPBACK_INIT
static const struct in6_addr in6addr_loopback =
    { { IN6ADDR_LOOPBACK_INIT } };
#    else
static const struct in6_addr in6addr_loopback =
    { { { 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1 } } };
#    endif /* HAVE_IN6ADDR_LOOPBACk_INIT */
#  endif /* ! HAVE_DECL_IN6ADDR_LOOPBACK */
#endif /* HAVE_IN6 */

/* XXX: is this a driver interface function ??? */
void erl_exit(int n, char*, ...);

/*
 * Malloc wrapper,
 * we would like to change the behaviour for different 
 * systems here.
 */

#ifdef FATAL_MALLOC

static void *alloc_wrapper(ErlDrvSizeT size){
    void *ret = driver_alloc(size);
    if(ret == NULL) 
	erl_exit(1,"Out of virtual memory in malloc (%s)", __FILE__);
    return ret;
}
#define ALLOC(X) alloc_wrapper(X)

static void *realloc_wrapper(void *current, ErlDrvSizeT size){
    void *ret = driver_realloc(current,size);
    if(ret == NULL) 
	erl_exit(1,"Out of virtual memory in realloc (%s)", __FILE__);
    return ret;
}
#define REALLOC(X,Y) realloc_wrapper(X,Y)
#define FREE(P) driver_free((P))
#else /* FATAL_MALLOC */

#define ALLOC(X) driver_alloc((X))
#define REALLOC(X,Y) driver_realloc((X), (Y))
#define FREE(P) driver_free((P))

#endif /* FATAL_MALLOC */

#define INIT_ATOM(NAME) am_ ## NAME = driver_mk_atom(#NAME)

#define LOAD_ATOM_CNT 2
#define LOAD_ATOM(vec, i, atom) \
  (((vec)[(i)] = ERL_DRV_ATOM), \
  ((vec)[(i)+1] = (atom)), \
  ((i)+LOAD_ATOM_CNT))

#define LOAD_INT_CNT 2
#define LOAD_INT(vec, i, val) \
  (((vec)[(i)] = ERL_DRV_INT), \
  ((vec)[(i)+1] = (ErlDrvTermData)(val)), \
  ((i)+LOAD_INT_CNT))

#define LOAD_UINT_CNT 2
#define LOAD_UINT(vec, i, val) \
  (((vec)[(i)] = ERL_DRV_UINT), \
  ((vec)[(i)+1] = (ErlDrvTermData)(val)), \
  ((i)+LOAD_UINT_CNT))

#define LOAD_PORT_CNT 2
#define LOAD_PORT(vec, i, port) \
  (((vec)[(i)] = ERL_DRV_PORT), \
  ((vec)[(i)+1] = (port)), \
  ((i)+LOAD_PORT_CNT))

#define LOAD_PID_CNT 2
#define LOAD_PID(vec, i, pid) \
  (((vec)[(i)] = ERL_DRV_PID), \
  ((vec)[(i)+1] = (pid)), \
  ((i)+LOAD_PID_CNT))

#define LOAD_BINARY_CNT 4
#define LOAD_BINARY(vec, i, bin, offs, len) \
  (((vec)[(i)] = ERL_DRV_BINARY), \
  ((vec)[(i)+1] = (ErlDrvTermData)(bin)), \
  ((vec)[(i)+2] = (len)), \
  ((vec)[(i)+3] = (offs)), \
  ((i)+LOAD_BINARY_CNT))

#define LOAD_BUF2BINARY_CNT 3
#define LOAD_BUF2BINARY(vec, i, buf, len) \
  (((vec)[(i)] = ERL_DRV_BUF2BINARY), \
  ((vec)[(i)+1] = (ErlDrvTermData)(buf)), \
  ((vec)[(i)+2] = (len)), \
  ((i)+LOAD_BUF2BINARY_CNT))

#define LOAD_STRING_CNT 3
#define LOAD_STRING(vec, i, str, len) \
  (((vec)[(i)] = ERL_DRV_STRING), \
  ((vec)[(i)+1] = (ErlDrvTermData)(str)), \
  ((vec)[(i)+2] = (len)), \
  ((i)+LOAD_STRING_CNT))

#define LOAD_STRING_CONS_CNT 3
#define LOAD_STRING_CONS(vec, i, str, len) \
  (((vec)[(i)] = ERL_DRV_STRING_CONS), \
  ((vec)[(i)+1] = (ErlDrvTermData)(str)), \
  ((vec)[(i)+2] = (len)), \
  ((i)+LOAD_STRING_CONS_CNT))

#define LOAD_TUPLE_CNT 2
#define LOAD_TUPLE(vec, i, size) \
  (((vec)[(i)] = ERL_DRV_TUPLE), \
  ((vec)[(i)+1] = (size)), \
  ((i)+LOAD_TUPLE_CNT))

#define LOAD_NIL_CNT 1
#define LOAD_NIL(vec, i) \
  (((vec)[(i)] = ERL_DRV_NIL), \
  ((i)+LOAD_NIL_CNT))

#define LOAD_LIST_CNT 2
#define LOAD_LIST(vec, i, size) \
  (((vec)[(i)] = ERL_DRV_LIST), \
  ((vec)[(i)+1] = (size)), \
  ((i)+LOAD_LIST_CNT))

/* Assume a cache line size of 64 bytes */
#define INET_DRV_CACHE_LINE_SIZE ((ErlDrvUInt) 64)
#define INET_DRV_CACHE_LINE_MASK (INET_DRV_CACHE_LINE_SIZE - 1)

static int debug_level = DLOG_DEFAULT;

static void emit_log(int level, char* file, int line, ...)
{
    va_list ap;
    char* fmt;

    if ((level == DLOG_EMERGENCY) ||
	((debug_level >= 0) && (level <= debug_level))) {
	va_start(ap, line);
	fmt = va_arg(ap, char*);
	fprintf(stderr, "%s:%d: ", file, line); 
	vfprintf(stderr, fmt, ap);
	fprintf(stderr, "\r\n");
	va_end(ap);
    }
}

/*
** Binary Buffer Managment
** We keep a stack of usable buffers 
*/
#define BUFFER_STACK_SIZE 14
#define BUFFER_STACK_MAX_MEM_SIZE (1024*1024)

ErlDrvTSDKey buffer_stack_key;

typedef struct {
    int mem_size;
    int pos;
    ErlDrvBinary* stk[BUFFER_STACK_SIZE];
} InetDrvBufStkBase;

typedef struct {
    InetDrvBufStkBase buf;
    char align[(((sizeof(InetDrvBufStkBase) - 1) / INET_DRV_CACHE_LINE_SIZE) + 1)
	       * INET_DRV_CACHE_LINE_SIZE];
} InetDrvBufStk;


static InetDrvBufStk *get_bufstk(void)
{
    InetDrvBufStk *bs = erl_drv_tsd_get(buffer_stack_key);
    if (bs)
	return bs;
    bs = driver_alloc(sizeof(InetDrvBufStk)
		      + INET_DRV_CACHE_LINE_SIZE - 1);
    if (!bs)
	return NULL;
    if ((((ErlDrvUInt) bs) & INET_DRV_CACHE_LINE_MASK) != 0)
	bs = ((InetDrvBufStk *)
	      ((((ErlDrvUInt) bs) & ~INET_DRV_CACHE_LINE_MASK)
	       + INET_DRV_CACHE_LINE_SIZE));
    erl_drv_tsd_set(buffer_stack_key, bs);
    bs->buf.pos = 0;
    bs->buf.mem_size = 0;
    return bs;
}

static ErlDrvBinary* alloc_buffer(ErlDrvSizeT minsz)
{
    InetDrvBufStk *bs = get_bufstk();

    DEBUGF("alloc_buffer: "LLU, (llu_t)minsz);

    if (bs && bs->buf.pos > 0) {
	long size;
	ErlDrvBinary* buf = bs->buf.stk[--bs->buf.pos];
	size = buf->orig_size;
	bs->buf.mem_size -= size;
	if (size >= minsz)
	    return buf;

	driver_free_binary(buf);
    }
    return driver_alloc_binary(minsz);
}

static void release_buffer(ErlDrvBinary* buf)
{
    InetDrvBufStk *bs;
    long size;

    DEBUGF("release_buffer: %ld", (buf==NULL) ? 0 : buf->orig_size);

    if (!buf)
	return;

    size = buf->orig_size;

    if (size > BUFFER_STACK_MAX_MEM_SIZE)
	goto free_binary;

    bs = get_bufstk();
    if (!bs
	|| (bs->buf.mem_size + size > BUFFER_STACK_MAX_MEM_SIZE)
	|| (bs->buf.pos >= BUFFER_STACK_SIZE)) {
    free_binary:
	driver_free_binary(buf);
    }
    else {
	bs->buf.mem_size += size;
	bs->buf.stk[bs->buf.pos++] = buf;
    }
}

/* use a TRICK, access the refc field to see if any one else has
 * a ref to this buffer then call driver_free_binary else 
 * release_buffer instead
 */
static void free_buffer(ErlDrvBinary* buf)
{
    DEBUGF("free_buffer: %ld", (buf==NULL) ? 0 : buf->orig_size);

    if (buf != NULL) {
	if (driver_binary_get_refc(buf) == 1)
	    release_buffer(buf);
	else
	    driver_free_binary(buf);
    }
}



/* return lowercase string form of errno value */
static char *errno_str(int err)
{
    switch (err) {
    case INET_ERRNO_SYSTEM_LIMIT:
	return "system_limit";
    default:
	return erl_errno_id(err);
    }
}

/* general control reply function */
static ErlDrvSSizeT ctl_reply(int rep, char* buf, ErlDrvSizeT len,
			      char** rbuf, ErlDrvSizeT rsize)
{
    char* ptr;

    if ((len+1) > rsize) {
	ptr = ALLOC(len+1);
	*rbuf = ptr;
    }
    else
	ptr = *rbuf;
    *ptr++ = rep;
    memcpy(ptr, buf, len);
    return len+1;
}

/* general control error reply function */
static ErlDrvSSizeT ctl_error(int err, char** rbuf, ErlDrvSizeT rsize)
{
    char* s = errno_str(err);

    return ctl_reply(INET_REP_ERROR, s, strlen(s), rbuf, rsize);
}

static ErlDrvSSizeT ctl_xerror(char* xerr, char** rbuf, ErlDrvSizeT rsize)
{
    int n = strlen(xerr);
    return ctl_reply(INET_REP_ERROR, xerr, n, rbuf, rsize);
}


static ErlDrvTermData error_atom(int err)
{
    return driver_mk_atom(errno_str(err));
}


static void enq_old_multi_op(tcp_descriptor *desc, int id, int req, 
			     ErlDrvTermData caller, MultiTimerData *timeout,
			     ErlDrvMonitor *monitorp)
{
    inet_async_multi_op *opp;

    opp = ALLOC(sizeof(inet_async_multi_op));

    opp->op.id = id;
    opp->op.caller = caller;
    opp->op.req = req;
    opp->op.tmo.mtd = timeout;
    memcpy(&(opp->op.monitor), monitorp, sizeof(ErlDrvMonitor));
    opp->next = NULL;

    if (desc->multi_first == NULL) {
	desc->multi_first = opp;
    } else {
	desc->multi_last->next = opp;
    }
    desc->multi_last = opp;
}   

static void enq_multi_op(tcp_descriptor *desc, char *buf, int req, 
			 ErlDrvTermData caller, MultiTimerData *timeout,
			 ErlDrvMonitor *monitorp)
{
    int id = NEW_ASYNC_ID();
    enq_old_multi_op(desc,id,req,caller,timeout,monitorp);
    if (buf != NULL)
	put_int16(id, buf);
}

static int deq_multi_op(tcp_descriptor *desc, int *id_p, int *req_p, 
			ErlDrvTermData *caller_p, MultiTimerData **timeout_p,
			ErlDrvMonitor *monitorp)
{
    inet_async_multi_op *opp;
    opp = desc->multi_first;
    if (!opp) {
	return -1;
    }
    desc->multi_first = opp->next;
    if (desc->multi_first == NULL) {
	desc->multi_last = NULL;
    }
    *id_p = opp->op.id;
    *req_p = opp->op.req;
    *caller_p = opp->op.caller;
    if (timeout_p != NULL) {
	*timeout_p = opp->op.tmo.mtd;
    }
    if (monitorp != NULL) {
	memcpy(monitorp,&(opp->op.monitor),sizeof(ErlDrvMonitor));
    }
    FREE(opp);
    return 0;
}

static int remove_multi_op(tcp_descriptor *desc, int *id_p, int *req_p, 
			   ErlDrvTermData caller, MultiTimerData **timeout_p,
			   ErlDrvMonitor *monitorp)
{
    inet_async_multi_op *opp, *slap;
    for (opp = desc->multi_first, slap = NULL; 
	 opp != NULL && opp->op.caller != caller; 
	 slap = opp, opp = opp->next)
	;
    if (!opp) {
	return -1;
    }
    if (slap == NULL) {
	desc->multi_first = opp->next;
    } else {
	slap->next = opp->next;
    }
    if (desc->multi_last == opp) {
	desc->multi_last = slap;
    }
    *id_p = opp->op.id;
    *req_p = opp->op.req;
    if (timeout_p != NULL) {
	*timeout_p = opp->op.tmo.mtd;
    }
    if (monitorp != NULL) {
	memcpy(monitorp,&(opp->op.monitor),sizeof(ErlDrvMonitor));
    }
    FREE(opp);
    return 0;
}

/* setup a new async id + caller (format async_id into buf) */

static int enq_async_w_tmo(inet_descriptor* desc, char* buf, int req, unsigned timeout,
			   ErlDrvMonitor *monitorp)
{
    int id = NEW_ASYNC_ID();
    inet_async_op* opp;

    if ((opp = desc->oph) == NULL)            /* queue empty */
	opp = desc->oph = desc->opt = desc->op_queue;
    else if (desc->oph == desc->opt) { /* queue full */ 
	DEBUGF("enq(%ld): queue full", (long)desc->port);
	return -1;
    }

    opp->id = id;
    opp->caller = driver_caller(desc->port);
    opp->req = req;
    opp->tmo.value = timeout;
    if (monitorp != NULL) {
	memcpy(&(opp->monitor),monitorp,sizeof(ErlDrvMonitor));
    }

    DEBUGF("enq(%ld): %d %ld %d", 
	    (long) desc->port, opp->id, opp->caller, opp->req);

    opp++;
    if (opp >= desc->op_queue + INET_MAX_ASYNC)
	desc->oph = desc->op_queue;
    else
	desc->oph = opp;

    if (buf != NULL)
	put_int16(id, buf);
    return 0;
}

static int enq_async(inet_descriptor* desc, char* buf, int req) 
{
    return enq_async_w_tmo(desc,buf,req,INET_INFINITY, NULL);
}

static int deq_async_w_tmo(inet_descriptor* desc, int* ap, ErlDrvTermData* cp, 
			   int* rp, unsigned *tp, ErlDrvMonitor *monitorp)
{
    inet_async_op* opp;

    if ((opp = desc->opt) == NULL) {  /* queue empty */
	DEBUGF("deq(%ld): queue empty", (long)desc->port);
	return -1;
    }
    *ap = opp->id;
    *cp = opp->caller;
    *rp = opp->req;
    if (tp != NULL) {
	*tp = opp->tmo.value;
    }
    if (monitorp != NULL) {
	memcpy(monitorp,&(opp->monitor),sizeof(ErlDrvMonitor));
    }
    
    DEBUGF("deq(%ld): %d %ld %d", 
	    (long)desc->port, opp->id, opp->caller, opp->req);
    
    opp++;
    if (opp >= desc->op_queue + INET_MAX_ASYNC)
	desc->opt = desc->op_queue;
    else
	desc->opt = opp;

    if (desc->opt == desc->oph)
	desc->opt = desc->oph = NULL;
    return 0;
}

static int deq_async(inet_descriptor* desc, int* ap, ErlDrvTermData* cp, int* rp)
{
    return deq_async_w_tmo(desc,ap,cp,rp,NULL,NULL);
}
/* send message:
**     {inet_async, Port, Ref, ok} 
*/
static int 
send_async_ok(inet_descriptor* desc, int Ref,ErlDrvTermData recipient)
{
    ErlDrvTermData spec[2*LOAD_ATOM_CNT + LOAD_PORT_CNT + 
			LOAD_INT_CNT + LOAD_TUPLE_CNT];
    int i = 0;
    
    i = LOAD_ATOM(spec, i, am_inet_async);
    i = LOAD_PORT(spec, i, desc->dport);
    i = LOAD_INT(spec, i, Ref);
    i = LOAD_ATOM(spec, i, am_ok);
    i = LOAD_TUPLE(spec, i, 4);
    return SEND_TERM(desc, recipient, spec, i);
}

/* send message:
**     {inet_async, Port, Ref, {ok,Port2}} 
*/
static int 
send_async_ok_port(inet_descriptor* desc, int Ref, 
		   ErlDrvTermData recipient, ErlDrvTermData Port2)
{
    ErlDrvTermData spec[2*LOAD_ATOM_CNT + 2*LOAD_PORT_CNT + 
			LOAD_INT_CNT + 2*LOAD_TUPLE_CNT];
    int i = 0;
    
    i = LOAD_ATOM(spec, i, am_inet_async);
    i = LOAD_PORT(spec, i, desc->dport);
    i = LOAD_INT(spec, i, Ref);
    {
	i = LOAD_ATOM(spec, i, am_ok);
	i = LOAD_PORT(spec, i, Port2);
	i = LOAD_TUPLE(spec, i, 2);
    }
    i = LOAD_TUPLE(spec, i, 4);
    return SEND_TERM(desc, recipient, spec, i);
}

/* send message:
**      {inet_async, Port, Ref, {error,Reason}}
*/
static int
send_async_error(inet_descriptor* desc, int Ref,
		 ErlDrvTermData recipient, ErlDrvTermData Reason)
{
    ErlDrvTermData spec[3*LOAD_ATOM_CNT + LOAD_PORT_CNT + 
			LOAD_INT_CNT + 2*LOAD_TUPLE_CNT];
    int i = 0;
    
    i = LOAD_ATOM(spec, i, am_inet_async);
    i = LOAD_PORT(spec, i, desc->dport);
    i = LOAD_INT(spec, i, Ref);
    {
	i = LOAD_ATOM(spec, i, am_error);
	i = LOAD_ATOM(spec, i, Reason);
	i = LOAD_TUPLE(spec, i, 2);
    }
    i = LOAD_TUPLE(spec, i, 4);
    DEBUGF("send_async_error %ld %ld", recipient, Reason);
    return SEND_TERM(desc, recipient, spec, i);
}


static int async_ok(inet_descriptor* desc)
{
    int req;
    int aid;
    ErlDrvTermData caller;

    if (deq_async(desc, &aid, &caller, &req) < 0)
	return -1;
    return send_async_ok(desc, aid, caller);
}

static int async_ok_port(inet_descriptor* desc, ErlDrvTermData Port2)
{
    int req;
    int aid;
    ErlDrvTermData caller;

    if (deq_async(desc, &aid, &caller, &req) < 0)
	return -1;
    return send_async_ok_port(desc, aid, caller, Port2);
}

static int async_error_am(inet_descriptor* desc, ErlDrvTermData reason)
{
    int req;
    int aid;
    ErlDrvTermData caller;

    if (deq_async(desc, &aid, &caller, &req) < 0)
	return -1;
    return send_async_error(desc, aid, caller, reason);
}

/* dequeue all operations */
static int async_error_am_all(inet_descriptor* desc, ErlDrvTermData reason)
{
    int req;
    int aid;
    ErlDrvTermData caller;

    while (deq_async(desc, &aid, &caller, &req) == 0) {
	send_async_error(desc, aid, caller, reason);
    }
    return 0;
}


static int async_error(inet_descriptor* desc, int err)
{
    return async_error_am(desc, error_atom(err));
}

/* send:
**   {inet_reply, S, ok} 
*/

static int inet_reply_ok(inet_descriptor* desc)
{
    ErlDrvTermData spec[2*LOAD_ATOM_CNT + LOAD_PORT_CNT + LOAD_TUPLE_CNT];
    ErlDrvTermData caller = desc->caller;
    int i = 0;
    
    i = LOAD_ATOM(spec, i, am_inet_reply);
    i = LOAD_PORT(spec, i, desc->dport);
    i = LOAD_ATOM(spec, i, am_ok);
    i = LOAD_TUPLE(spec, i, 3);
    desc->caller = 0;
    return SEND_TERM(desc, caller, spec, i);    
}

/* send:
**   {inet_reply, S, {error, Reason}} 
*/
static int inet_reply_error_am(inet_descriptor* desc, ErlDrvTermData reason)
{
    ErlDrvTermData spec[3*LOAD_ATOM_CNT + LOAD_PORT_CNT + 2*LOAD_TUPLE_CNT];
    ErlDrvTermData caller = desc->caller;
    int i = 0;
    
    i = LOAD_ATOM(spec, i, am_inet_reply);
    i = LOAD_PORT(spec, i, desc->dport);
    i = LOAD_ATOM(spec, i, am_error);
    i = LOAD_ATOM(spec, i, reason);
    i = LOAD_TUPLE(spec, i, 2);
    i = LOAD_TUPLE(spec, i, 3);
    desc->caller = 0;
    
    DEBUGF("inet_reply_error_am %ld %ld", caller, reason);
    return SEND_TERM(desc, caller, spec, i);
}

/* send:
**   {inet_reply, S, {error, Reason}} 
*/
static int inet_reply_error(inet_descriptor* desc, int err)
{
    return inet_reply_error_am(desc, error_atom(err));
}

/* 
** Deliver port data from buffer 
*/
static int inet_port_data(inet_descriptor* desc, const char* buf, int len)
{
    unsigned int hsz = desc->hsz;

    DEBUGF("inet_port_data(%ld): len = %d", (long)desc->port, len);

    if ((desc->mode == INET_MODE_LIST) || (hsz > len))
	return driver_output2(desc->port, (char*)buf, len, NULL, 0);
    else if (hsz > 0)
	return driver_output2(desc->port, (char*)buf, hsz, (char*)buf+hsz, len-hsz);
    else
	return driver_output(desc->port, (char*)buf, len);
}

/* 
** Deliver port data from binary (for an active mode socket)
*/
static int
inet_port_binary_data(inet_descriptor* desc, ErlDrvBinary* bin, int offs, int len)
{
    unsigned int hsz = desc->hsz;

    DEBUGF("inet_port_binary_data(%ld): offs=%d, len = %d", 
	    (long)desc->port, offs, len);

    if ((desc->mode == INET_MODE_LIST) || (hsz > len)) 
	return driver_output2(desc->port, bin->orig_bytes+offs, len, NULL, 0);
    else 
	return driver_output_binary(desc->port, bin->orig_bytes+offs, hsz,
				    bin, offs+hsz, len-hsz);
}

static ErlDrvTermData am_http_eoh;
static ErlDrvTermData am_http_header;
static ErlDrvTermData am_http_request;
static ErlDrvTermData am_http_response;
static ErlDrvTermData am_http_error;
static ErlDrvTermData am_abs_path;
static ErlDrvTermData am_absoluteURI;
static ErlDrvTermData am_star;
static ErlDrvTermData am_undefined;
static ErlDrvTermData am_http;
static ErlDrvTermData am_https;
static ErlDrvTermData am_scheme;

static int http_load_string(tcp_descriptor* desc, ErlDrvTermData* spec, int i,
			    const char* str, int len)
{
    if (desc->inet.htype >= TCP_PB_HTTP_BIN) {
	i = LOAD_BUF2BINARY(spec, i, str, len);
    } else {
	i = LOAD_STRING(spec, i, str, len);
    }
    return i;
}

static int http_response_inetdrv(void *arg, int major, int minor,
				 int status, const char* phrase, int phrase_len)
{
    tcp_descriptor* desc = (tcp_descriptor*) arg;
    int i = 0;
    ErlDrvTermData spec[27];
    ErlDrvTermData caller = ERL_DRV_NIL;
    
    if (desc->inet.active == INET_PASSIVE) {
        /* {inet_async,S,Ref,{ok,{http_response,Version,Status,Phrase}}} */
        int req;
        int aid;
        
        if (deq_async(INETP(desc), &aid, &caller, &req) < 0)
            return -1;
        i = LOAD_ATOM(spec, i,  am_inet_async);
        i = LOAD_PORT(spec, i,  desc->inet.dport);
        i = LOAD_INT(spec, i,   aid);
        i = LOAD_ATOM(spec, i,  am_ok);
    }
    else {
        /* {http, S, {http_response,Version,Status,Phrase}} */
        i = LOAD_ATOM(spec, i, am_http);
        i = LOAD_PORT(spec, i, desc->inet.dport);
    }
    i = LOAD_ATOM(spec, i,  am_http_response);
    i = LOAD_INT(spec, i, major);
    i = LOAD_INT(spec, i, minor);
    i = LOAD_TUPLE(spec, i, 2);
    i = LOAD_INT(spec, i, status);
    i = http_load_string(desc, spec, i, phrase, phrase_len);
    i = LOAD_TUPLE(spec, i, 4);
    
    if (desc->inet.active == INET_PASSIVE) {
        i = LOAD_TUPLE(spec, i, 2);
        i = LOAD_TUPLE(spec, i, 4);
        return SEND_TERM(&desc->inet, caller, spec, i);
    }
    else {
        i = LOAD_TUPLE(spec, i, 3);
        return OUTPUT_TERM(&desc->inet, spec, i);
    }
}

static int http_load_uri(tcp_descriptor* desc, ErlDrvTermData* spec, int i,
			 const PacketHttpURI* uri)
{
    ErlDrvTermData scheme;

    switch (uri->type) {
    case URI_STAR:
        i = LOAD_ATOM(spec, i, am_star);
        break;
    case URI_ABS_PATH:
        i = LOAD_ATOM(spec, i, am_abs_path);
        i = http_load_string(desc, spec, i, uri->s1_ptr, uri->s1_len);
        i = LOAD_TUPLE(spec, i, 2);
        break;
    case URI_HTTP:
        scheme = am_http;
        goto http_common;
    case URI_HTTPS:
        scheme = am_https;
    http_common:
        i = LOAD_ATOM(spec, i, am_absoluteURI);
        i = LOAD_ATOM(spec, i, scheme);
        i = http_load_string(desc, spec, i, uri->s1_ptr, uri->s1_len);
        if (uri->port == 0) {
            i = LOAD_ATOM(spec, i, am_undefined);
        } else {
            i = LOAD_INT(spec, i, uri->port);
        }
        i = http_load_string(desc, spec, i, uri->s2_ptr, uri->s2_len);
        i = LOAD_TUPLE(spec, i, 5);
        break;

    case URI_STRING:
        i = http_load_string(desc, spec, i, uri->s1_ptr, uri->s1_len);
        break;
    case URI_SCHEME:
        i = LOAD_ATOM(spec, i, am_scheme);
        i = http_load_string(desc, spec, i, uri->s1_ptr, uri->s1_len);
        i = http_load_string(desc, spec, i, uri->s2_ptr, uri->s2_len);
        i = LOAD_TUPLE(spec, i, 3);
    }
    return i;
}


static int
http_request_inetdrv(void* arg, const http_atom_t* meth, const char* meth_ptr,
		     int meth_len, const PacketHttpURI* uri,
		     int major, int minor)
{
    tcp_descriptor* desc = (tcp_descriptor*) arg;
    int i = 0;
    ErlDrvTermData spec[43];
    ErlDrvTermData caller = ERL_DRV_NIL;
    
    if (desc->inet.active == INET_PASSIVE) {
        /* {inet_async, S, Ref, {ok,{http_request,Meth,Uri,Version}}} */
        int req;
        int aid;
        
        if (deq_async(INETP(desc), &aid, &caller, &req) < 0)
            return -1;
        i = LOAD_ATOM(spec, i,  am_inet_async);
        i = LOAD_PORT(spec, i,  desc->inet.dport);
        i = LOAD_INT(spec, i,   aid);
        i = LOAD_ATOM(spec, i,  am_ok);
    }
    else {
        /* {http, S, {http_request,Meth,Uri,Version}}} */
        i = LOAD_ATOM(spec, i, am_http);
        i = LOAD_PORT(spec, i, desc->inet.dport);
    }

    i = LOAD_ATOM(spec, i,  am_http_request);
    if (meth != NULL)
      i = LOAD_ATOM(spec, i, meth->atom);
    else
      i = http_load_string(desc, spec, i, meth_ptr, meth_len);
    i = http_load_uri(desc, spec, i, uri);
    i = LOAD_INT(spec, i, major);
    i = LOAD_INT(spec, i, minor);
    i = LOAD_TUPLE(spec, i, 2);
    i = LOAD_TUPLE(spec, i, 4);

    if (desc->inet.active == INET_PASSIVE) {
        i = LOAD_TUPLE(spec, i, 2);
        i = LOAD_TUPLE(spec, i, 4);
        return SEND_TERM(&desc->inet, caller, spec, i);
    }
    else {
        i = LOAD_TUPLE(spec, i, 3);
        return OUTPUT_TERM(&desc->inet, spec, i);
    }
}

static int
http_header_inetdrv(void* arg, const http_atom_t* name, const char* name_ptr,
		    int name_len, const char* value_ptr, int value_len)
{
    tcp_descriptor* desc = (tcp_descriptor*) arg;
    int i = 0;
    ErlDrvTermData spec[26];
    ErlDrvTermData caller = ERL_DRV_NIL;
    
    if (desc->inet.active == INET_PASSIVE) {
        /* {inet_async,S,Ref,{ok,{http_header,Bit,Name,IValue,Value}} */
        int req;
        int aid;
        
        
        if (deq_async(INETP(desc), &aid, &caller, &req) < 0)
            return -1;
        i = LOAD_ATOM(spec, i,  am_inet_async);
        i = LOAD_PORT(spec, i,  desc->inet.dport);
        i = LOAD_INT(spec, i,   aid);
        i = LOAD_ATOM(spec, i,  am_ok);
    }
    else {
        /* {http, S, {http_header,Bit,Name,IValue,Value}} */
        i = LOAD_ATOM(spec, i, am_http);
        i = LOAD_PORT(spec, i, desc->inet.dport);
    }

    i = LOAD_ATOM(spec, i,  am_http_header);
    if (name != NULL) {
      i = LOAD_INT(spec, i,  name->index+1);
      i = LOAD_ATOM(spec, i, name->atom);
    }
    else {
      i = LOAD_INT(spec, i,  0);
      i = http_load_string(desc, spec, i, name_ptr, name_len);
    }
    i = LOAD_ATOM(spec, i, am_undefined);
    i = http_load_string(desc, spec, i, value_ptr, value_len);
    i = LOAD_TUPLE(spec, i, 5);

    if (desc->inet.active == INET_PASSIVE) {
        i = LOAD_TUPLE(spec, i, 2);
        i = LOAD_TUPLE(spec, i, 4);
        return SEND_TERM(&desc->inet, caller, spec, i);
    }
    else {
        i = LOAD_TUPLE(spec, i, 3);
        return OUTPUT_TERM(&desc->inet, spec, i);
    }
}

static int http_eoh_inetdrv(void* arg)
{
  tcp_descriptor* desc = (tcp_descriptor*) arg;
  int i = 0;
  ErlDrvTermData spec[14];

  if (desc->inet.active == INET_PASSIVE) {
    /* {inet_async,S,Ref,{ok,http_eoh}} */
    int req;
    int aid;
    ErlDrvTermData caller;

    if (deq_async(INETP(desc), &aid, &caller, &req) < 0)
      return -1;
    i = LOAD_ATOM(spec, i,  am_inet_async);
    i = LOAD_PORT(spec, i,  desc->inet.dport);
    i = LOAD_INT(spec, i,   aid);
    i = LOAD_ATOM(spec, i,  am_ok);
    i = LOAD_ATOM(spec, i,  am_http_eoh);
    i = LOAD_TUPLE(spec, i, 2);
    i = LOAD_TUPLE(spec, i, 4);
    return SEND_TERM(&desc->inet, caller, spec, i);
  }
  else {
      /* {http, S, http_eoh} */
      i = LOAD_ATOM(spec, i,  am_http);
      i = LOAD_PORT(spec, i,  desc->inet.dport);
      i = LOAD_ATOM(spec, i,  am_http_eoh);
      i = LOAD_TUPLE(spec, i, 3);
      return OUTPUT_TERM(&desc->inet, spec, i);
  }
}

static int http_error_inetdrv(void* arg, const char* buf, int len)
{
  tcp_descriptor* desc = (tcp_descriptor*) arg;
  int i = 0;
  ErlDrvTermData spec[19];

  if (desc->inet.active == INET_PASSIVE) {
    /* {inet_async,S,Ref,{ok,{http_error,Line}}} */
    int req;
    int aid;
    ErlDrvTermData caller;

    if (deq_async(INETP(desc), &aid, &caller, &req) < 0)
      return -1;
    i = LOAD_ATOM(spec, i,  am_inet_async);
    i = LOAD_PORT(spec, i,  desc->inet.dport);
    i = LOAD_INT(spec, i,   aid);
    i = LOAD_ATOM(spec, i,  am_ok);
    i = LOAD_ATOM(spec, i,  am_http_error);
    i = http_load_string(desc, spec, i, buf, len);
    i = LOAD_TUPLE(spec, i, 2);
    i = LOAD_TUPLE(spec, i, 2);
    i = LOAD_TUPLE(spec, i, 4);
    return SEND_TERM(&desc->inet, caller, spec, i);
  }
  else {
      /* {http, S, {http_error,Line} */
      i = LOAD_ATOM(spec, i,  am_http);
      i = LOAD_PORT(spec, i,  desc->inet.dport);
      i = LOAD_ATOM(spec, i,  am_http_error);
      i = http_load_string(desc, spec, i, buf, len);
      i = LOAD_TUPLE(spec, i, 2);
      i = LOAD_TUPLE(spec, i, 3);
      return OUTPUT_TERM(&desc->inet, spec, i);
  }
}


static
int ssl_tls_inetdrv(void* arg, unsigned type, unsigned major, unsigned minor,
                    const char* buf, int len, const char* prefix, int plen)
{
    tcp_descriptor* desc = (tcp_descriptor*) arg;
    int i = 0;
    ErlDrvTermData spec[28];
    ErlDrvTermData caller = ERL_DRV_NIL;
    ErlDrvBinary* bin;
    int ret;

    if ((bin = driver_alloc_binary(plen+len)) == NULL)
        return async_error(&desc->inet, ENOMEM);
    memcpy(bin->orig_bytes+plen, buf, len);
    if (plen) {
        memcpy(bin->orig_bytes, prefix, plen);
        len += plen;
    }

    if (desc->inet.active == INET_PASSIVE) {
        /* {inet_async,S,Ref,{ok,{ssl_tls,...}}} */
        int req;
        int aid;

        if (deq_async(INETP(desc), &aid, &caller, &req) < 0) {
            ret = -1;
            goto done;
        }
        i = LOAD_ATOM(spec, i,  am_inet_async);
        i = LOAD_PORT(spec, i,  desc->inet.dport);
        i = LOAD_INT(spec, i,   aid);
        i = LOAD_ATOM(spec, i,  am_ok);
    }

    /* {ssl_tls,S,ContentType,{Major,Minor},Bin} */
    i = LOAD_ATOM(spec, i,  am_ssl_tls);
    i = LOAD_PORT(spec, i,  desc->inet.dport);
    i = LOAD_INT(spec, i,   type);
    i = LOAD_INT(spec, i,   major);
    i = LOAD_INT(spec, i,   minor);
    i = LOAD_TUPLE(spec, i, 2);
    i = LOAD_BINARY(spec, i, bin, 0, len);
    i = LOAD_TUPLE(spec, i, 5);

    if (desc->inet.active == INET_PASSIVE) {
        i = LOAD_TUPLE(spec, i, 2);
        i = LOAD_TUPLE(spec, i, 4);
        ret = SEND_TERM(&desc->inet, caller, spec, i);
    }
    else {
        ret = OUTPUT_TERM(&desc->inet, spec, i);
    }
done:
    driver_free_binary(bin);
    return ret;
}


static PacketCallbacks packet_callbacks =
{
    http_response_inetdrv,
    http_request_inetdrv,
    http_eoh_inetdrv,
    http_header_inetdrv,
    http_error_inetdrv,
    ssl_tls_inetdrv
};


/* 
** passive mode reply:
**        {inet_async, S, Ref, {ok,[H1,...Hsz | Data]}}
** NB: this is for TCP only;
** UDP and SCTP use inet_async_binary_data .
*/
static int inet_async_data(inet_descriptor* desc, const char* buf, int len)
{
    unsigned int hsz = desc->hsz;
    ErlDrvTermData spec[20];
    ErlDrvTermData caller;
    int req;
    int aid;
    int i = 0;

    DEBUGF("inet_async_data(%ld): len = %d", (long)desc->port, len);

    if (deq_async(desc, &aid, &caller, &req) < 0)
	return -1;

    i = LOAD_ATOM(spec, i, am_inet_async);
    i = LOAD_PORT(spec, i, desc->dport);
    i = LOAD_INT(spec, i, aid);

    i = LOAD_ATOM(spec, i, am_ok);
    if ((desc->mode == INET_MODE_LIST) || (hsz > len)) {
	i = LOAD_STRING(spec, i, buf, len); /* => [H1,H2,...Hn] */ 
	i = LOAD_TUPLE(spec, i, 2);
	i = LOAD_TUPLE(spec, i, 4);
	desc->caller = 0;
	return SEND_TERM(desc, caller, spec, i);
    }
    else {
	/* INET_MODE_BINARY => [H1,H2,...HSz | Binary] */
	int sz = len - hsz;
	int code;

	i = LOAD_BUF2BINARY(spec, i, buf+hsz, sz);
	if (hsz > 0)
	    i = LOAD_STRING_CONS(spec, i, buf, hsz);
	i = LOAD_TUPLE(spec, i, 2);
	i = LOAD_TUPLE(spec, i, 4);
	desc->caller = 0;
	code = SEND_TERM(desc, caller, spec, i);
	return code;
    }
}

/* 
** passive mode reply:
** for UDP:
**        {inet_async, S, Ref, {ok, Data=[H1,...,Hsz | BinData]}}
** or (in the list mode)
**	  {inet_async, S, Ref, {ok, Data=[H1,...,Hsz]}}
**
** for SCTP:
**	  {inet_async, S, Ref, {ok, {[H1,...,HSz], [AncilData], Data_OR_Event}}}
** where  each AncilDatum:Tuple();
**	  Data:List() or Binary(), but if List(), then without the Addr part,
**				   which is moved in front;
**	  Event:Tuple();
** or
** 	  {inet_async, S, Ref, {error, {[H1,...,HSz], [AncilData], ErrorTerm}}}
**
** Cf: the output of send_async_error() is
**	  {inet_async, S, Ref, {error, Cause:Atom()}}
*/
static int
inet_async_binary_data
	(inet_descriptor* desc, unsigned  int phsz,
	 ErlDrvBinary   * bin,  int offs, int len, void * extra)
{
    unsigned int hsz = desc->hsz + phsz;
    ErlDrvTermData spec [PACKET_ERL_DRV_TERM_DATA_LEN];
    ErlDrvTermData caller = desc->caller;
    int aid;
    int req;
    int i = 0;

    DEBUGF("inet_async_binary_data(%ld): offs=%d, len=%d", 
	    (long)desc->port, offs, len);

    if (deq_async(desc, &aid, &caller, &req) < 0)
	return -1;

    i = LOAD_ATOM(spec, i, am_inet_async);	/* 'inet_async' */
    i = LOAD_PORT(spec, i, desc->dport);	/* S		*/
    i = LOAD_INT (spec, i, aid);		/* Ref		*/

    i = LOAD_ATOM(spec, i, am_ok);

    /* Generic case. Both Addr and Data (or a single list of them together) are
       returned: */

    if ((desc->mode == INET_MODE_LIST) || (hsz > len)) {
	/* INET_MODE_LIST => [H1,H2,...Hn] */
	i = LOAD_STRING(spec, i, bin->orig_bytes+offs, len);
    }
    else {
	/* INET_MODE_BINARY => [H1,H2,...HSz | Binary] or [Binary]: */
	int sz = len - hsz;
	i = LOAD_BINARY(spec, i, bin, offs+hsz, sz);
	if (hsz > 0)
	    i = LOAD_STRING_CONS(spec, i, bin->orig_bytes+offs, hsz);
    }
    /* Close up the {ok, ...} or {error, ...} tuple: */
    i = LOAD_TUPLE(spec, i, 2);

    /* Close up the outer {inet_async, S, Ref, {ok|error, ...}} tuple: */
    i = LOAD_TUPLE(spec, i, 4);
    desc->caller = 0;
    return SEND_TERM(desc, caller, spec, i);
}

/* 
** active mode message:
**        {tcp, S, [H1,...Hsz | Data]}
*/
static int tcp_message(inet_descriptor* desc, const char* buf, int len)
{
    unsigned int hsz = desc->hsz;
    ErlDrvTermData spec[20];
    int i = 0;

    DEBUGF("tcp_message(%ld): len = %d", (long)desc->port, len);    
    /* XXX fprintf(stderr,"tcp_message send."); */

    i = LOAD_ATOM(spec, i, am_tcp);
    i = LOAD_PORT(spec, i, desc->dport);

    if ((desc->mode == INET_MODE_LIST) || (hsz > len)) {
	i = LOAD_STRING(spec, i, buf, len); /* => [H1,H2,...Hn] */ 
	i = LOAD_TUPLE(spec, i, 3);
	return OUTPUT_TERM(desc, spec, i);
    }
    else {
	/* INET_MODE_BINARY => [H1,H2,...HSz | Binary] */
	int sz = len - hsz;
	int code;

	i = LOAD_BUF2BINARY(spec, i, buf+hsz, sz);
	if (hsz > 0)
	    i = LOAD_STRING_CONS(spec, i, buf, hsz);
	i = LOAD_TUPLE(spec, i, 3);
	code = OUTPUT_TERM(desc, spec, i);
	return code;
    }
}

/* 
** active mode message:
**        {tcp, S, [H1,...Hsz | Data]}
*/
static int
tcp_binary_message(inet_descriptor* desc, ErlDrvBinary* bin, int offs, int len)
{
    unsigned int hsz = desc->hsz;
    ErlDrvTermData spec[20];
    int i = 0;

    DEBUGF("tcp_binary_message(%ld): len = %d", (long)desc->port, len); 

    i = LOAD_ATOM(spec, i, am_tcp);
    i = LOAD_PORT(spec, i, desc->dport);

    if ((desc->mode == INET_MODE_LIST) || (hsz > len)) {
	/* INET_MODE_LIST => [H1,H2,...Hn] */
	i = LOAD_STRING(spec, i, bin->orig_bytes+offs, len);
    }
    else {
	/* INET_MODE_BINARY => [H1,H2,...HSz | Binary] */
	int sz = len - hsz;

	i = LOAD_BINARY(spec, i, bin, offs+hsz, sz);
	if (hsz > 0)
	    i = LOAD_STRING_CONS(spec, i, bin->orig_bytes+offs, hsz);
    }
    i = LOAD_TUPLE(spec, i, 3);
    return OUTPUT_TERM(desc, spec, i);
}

/*
** send:  active mode  {tcp_closed, S}
*/
static int tcp_closed_message(tcp_descriptor* desc)
{
    ErlDrvTermData spec[6];
    int i = 0;

    DEBUGF("tcp_closed_message(%ld):", (long)desc->inet.port); 
    if (!(desc->tcp_add_flags & TCP_ADDF_CLOSE_SENT)) {
	desc->tcp_add_flags |= TCP_ADDF_CLOSE_SENT;

	i = LOAD_ATOM(spec, i, am_tcp_closed);
	i = LOAD_PORT(spec, i, desc->inet.dport);
	i = LOAD_TUPLE(spec, i, 2);
	return OUTPUT_TERM(&desc->inet, spec, i);
    } 
    return 0;
}

/*
** send active message {tcp_error, S, Error}
*/
static int tcp_error_message(tcp_descriptor* desc, int err)
{
    ErlDrvTermData spec[8];
    ErlDrvTermData am_err = error_atom(err);
    int i = 0;

    DEBUGF("tcp_error_message(%ld): %d", (long)desc->inet.port, err); 

    i = LOAD_ATOM(spec, i, am_tcp_error);
    i = LOAD_PORT(spec, i, desc->inet.dport);
    i = LOAD_ATOM(spec, i, am_err);
    i = LOAD_TUPLE(spec, i, 3);
    return OUTPUT_TERM(&desc->inet, spec, i);
}

/*
** active mode message: send active-to-passive transition message
**        {tcp_passive, S} or
*/
 static int packet_passive_message(inet_descriptor* desc)
 {
     ErlDrvTermData spec[6];
     int i = 0;

     DEBUGF("packet_passive_message(%ld):\r\n", (long)desc->port);

     i = LOAD_ATOM(spec, i, am_tcp_passive);
     i = LOAD_PORT(spec, i, desc->dport);
     i = LOAD_TUPLE(spec, i, 2);
     return erl_drv_output_term(desc->dport, spec, i);
 }

/* 
** active=TRUE:
**  (NOTE! distribution MUST use active=TRUE, deliver=PORT)
**       deliver=PORT  {S, {data, [H1,..Hsz | Data]}}
**       deliver=TERM  {tcp, S, [H1..Hsz | Data]}
**
** active=FALSE:
**       {async, S, Ref, {ok,[H1,...Hsz | Data]}}
*/
static int tcp_reply_data(tcp_descriptor* desc, char* buf, int len)
{
    int code;
    const char* body = buf;
    int bodylen = len;
    
    afunix_packet_get_body(desc->inet.htype, &body, &bodylen);

    if (desc->inet.deliver == INET_DELIVER_PORT) {
        code = inet_port_data(INETP(desc), body, bodylen);
    }
    else if ((code=afunix_packet_parse(desc->inet.htype, buf, len,
                                &desc->http_state, &packet_callbacks,
                                desc)) == 0) {
        /* No body parsing, return raw binary */
        if (desc->inet.active == INET_PASSIVE)
            return inet_async_data(INETP(desc), body, bodylen);
        else
            code = tcp_message(INETP(desc), body, bodylen);
    }

    if (code < 0)
	return code;
    INET_CHECK_ACTIVE_TO_PASSIVE(INETP(desc));
    return code;
}

static int
tcp_reply_binary_data(tcp_descriptor* desc, ErlDrvBinary* bin, int offs, int len)
{
    int code;
    const char* buf = bin->orig_bytes + offs;
    const char* body = buf;
    int bodylen = len;

    afunix_packet_get_body(desc->inet.htype, &body, &bodylen);
    offs = body - bin->orig_bytes; /* body offset now */

    if (desc->inet.deliver == INET_DELIVER_PORT)
        code = inet_port_binary_data(INETP(desc), bin, offs, bodylen);
    else if ((code=afunix_packet_parse(desc->inet.htype, buf, len, &desc->http_state,
                                     &packet_callbacks,desc)) == 0) {
        /* No body parsing, return raw data */
        if (desc->inet.active == INET_PASSIVE)
            return inet_async_binary_data(INETP(desc), 0, bin, offs, bodylen, NULL);
        else
            code = tcp_binary_message(INETP(desc), bin, offs, bodylen);
    }
    if (code < 0)
	return code;
    if (desc->inet.active == INET_ONCE)
	desc->inet.active = INET_PASSIVE;
    return code;
}

/* ----------------------------------------------------------------------------

   INET

---------------------------------------------------------------------------- */

static int
sock_init(void) /* May be called multiple times. */
{
    afunix_packet_parser_init();
    return 1;
}

static int inet_init()
{
    if (!sock_init())
	goto error;

    if (0 != erl_drv_tsd_key_create("afunix_buffer_stack_key",
				    &buffer_stack_key))
	goto error;

    INIT_ATOM(ok);
    INIT_ATOM(tcp);
    INIT_ATOM(udp);
    INIT_ATOM(error);
    INIT_ATOM(inet_async);
    INIT_ATOM(inet_reply);
    INIT_ATOM(timeout);
    INIT_ATOM(closed);
    INIT_ATOM(tcp_passive);
    INIT_ATOM(tcp_closed);
    INIT_ATOM(tcp_error);
    INIT_ATOM(udp_error);
    INIT_ATOM(empty_out_q);
    INIT_ATOM(ssl_tls);

    INIT_ATOM(http_eoh);
    INIT_ATOM(http_header);
    INIT_ATOM(http_request);
    INIT_ATOM(http_response);
    INIT_ATOM(http_error);
    INIT_ATOM(abs_path);
    INIT_ATOM(absoluteURI);
    am_star = driver_mk_atom("*");
    INIT_ATOM(undefined);
    INIT_ATOM(http);
    INIT_ATOM(https);
    INIT_ATOM(scheme);

    return 0;

 error:
    return -1;
}


/*
** Set a inaddr structure:
**  src = [P1,P0,X1,X2,.....]
**  dst points to a structure large enugh to keep any kind
**  of inaddr.
** *len is set to length of src on call
** and is set to actual length of dst on return
** return NULL on error and ptr after port address on success
*/
static char* inet_set_address(int family, inet_address* dst,
			      char* src, ErlDrvSizeT* len)
{
    short port;

    if ((family == AF_UNIX) && 
	((*len >= 1) && (*len < sizeof(dst->sau.sun_path)))) {
	size_t n;
	memzero((char*)dst, sizeof(struct sockaddr_un));
	n = *len;
	memcpy(dst->sau.sun_path, src, n);
	dst->sau.sun_path[n] = '\0';
#ifdef HAVE_SUN_LEN_FIELD
	dst->sau.sun_len = n+1;  // length including '\0'
#endif
	dst->sau.sun_family = family;
	*len = offsetof(struct sockaddr_un,sun_path) + n;
	// *len = SUN_LEN(&dst->sau);
	return src + n;
    }
    else if ((family == AF_INET) && (*len >= 2+4)) {
	memzero((char*)dst, sizeof(struct sockaddr_in));
	port = get_int16(src);
#ifndef NO_SA_LEN
	dst->sai.sin_len    = sizeof(struct sockaddr_in);
#endif
	dst->sai.sin_family = family;
	dst->sai.sin_port   = sock_htons(port);
	memcpy(&dst->sai.sin_addr, src+2, 4);
	*len = sizeof(struct sockaddr_in);
	return src + 2+4;
    }
#if defined(HAVE_IN6) && defined(AF_INET6)
    else if ((family == AF_INET6) && (*len >= 2+16)) {
	memzero((char*)dst, sizeof(struct sockaddr_in6));
	port = get_int16(src);
#ifndef NO_SA_LEN
	dst->sai6.sin6_len    = sizeof(struct sockaddr_in6);
#endif
	dst->sai6.sin6_family = family;
	dst->sai6.sin6_port   = sock_htons(port);
	dst->sai6.sin6_flowinfo = 0;   /* XXX this may be set as well ?? */
	memcpy(&dst->sai6.sin6_addr, src+2, 16);
	*len = sizeof(struct sockaddr_in6); 
	return src + 2+16;
    }
#endif
    return NULL;
}

/*
** Set an inaddr structure, address family comes from source data,
** or from argument if source data specifies constant address.
** 
** src = [TAG,P1,P0]           when TAG = INET_AF_ANY  | INET_AF_LOOPBACK
** src = [TAG,P1,P0,X1,X2,...] when TAG = INET_AF_INET | INET_AF_INET6
** src = [TAG,name]            when TAG = INET_AF_UNIX
*/
static char *inet_set_faddress(int family, inet_address* dst,
			       char *src, ErlDrvSizeT* len) {
    int tag;
    
    if (*len < 1) return NULL;
    (*len) --;
    tag = *(src ++);
    switch (tag) {
    case INET_AF_UNIX:
	family = AF_UNIX;
	break;
    case INET_AF_INET:
	family = AF_INET;
	break;
#   if defined(HAVE_IN6) && defined(AF_INET6)
    case INET_AF_INET6:
	family = AF_INET6;
	break;
#   endif
    case INET_AF_ANY:
    case INET_AF_LOOPBACK: {
	int port;
	
	if (*len < 2) return NULL;
	port = get_int16(src);
	switch (family) {
	case AF_INET: {
	    struct in_addr addr;
	    switch (tag) {
	    case INET_AF_ANY: 
		addr.s_addr = sock_htonl(INADDR_ANY);
		break;
	    case INET_AF_LOOPBACK:
		addr.s_addr = sock_htonl(INADDR_LOOPBACK);
		break;
	    default:
		return NULL;
	    }
	    memzero((char*)dst, sizeof(struct sockaddr_in));
#ifndef NO_SA_LEN
	    dst->sai.sin_len         = sizeof(struct sockaddr_in6);
#endif
	    dst->sai.sin_family      = family;
	    dst->sai.sin_port        = sock_htons(port);
	    dst->sai.sin_addr.s_addr = addr.s_addr;
	    *len = sizeof(struct sockaddr_in);
	}   break;
#       if defined(HAVE_IN6) && defined(AF_INET6)
	case AF_INET6: {
	    const struct in6_addr* paddr;
	    switch (tag) {
	    case INET_AF_ANY: 
		paddr = &in6addr_any;
		break;
	    case INET_AF_LOOPBACK:
		paddr = &in6addr_loopback;
		break;
	    default:
		return NULL;
	    }
	    memzero((char*)dst, sizeof(struct sockaddr_in6));
#ifndef NO_SA_LEN
	    dst->sai6.sin6_len    = sizeof(struct sockaddr_in6);
#endif
	    dst->sai6.sin6_family = family;
	    dst->sai6.sin6_port   = sock_htons(port);
	    dst->sai6.sin6_flowinfo = 0;   /* XXX this may be set as well ?? */
	    dst->sai6.sin6_addr = *paddr;
	    *len = sizeof(struct sockaddr_in6);
	}   break;
#       endif
	default:
	    return NULL;
	}
	return src + 2;
    }   break;
    default:
	return NULL;
    }
    return inet_set_address(family, dst, src, len);
}


/* Get a inaddr structure
** src = inaddr structure
** *len is the lenght of structure
** dst is filled with [F,P1,P0,X1,....] 
** where F is the family code (coded)
** and *len is the length of dst on return 
** (suitable to deliver to erlang)
*/
static int inet_get_address(int family, char* dst, inet_address* src, unsigned int* len)
{
    short port;

    if ((family == AF_INET) && (*len >= sizeof(struct sockaddr_in))) {
	dst[0] = INET_AF_INET;
	port = sock_ntohs(src->sai.sin_port);
	put_int16(port, dst+1);
	memcpy(dst+3, (char*)&src->sai.sin_addr, sizeof(struct in_addr));
	*len = 3 + sizeof(struct in_addr);
	return 0;
    }
    else if ((family == AF_UNIX) && (*len <= sizeof(struct sockaddr_un))) {
	int n;
	dst[0] = INET_AF_UNIX;
#ifdef HAVE_SUN_LEN_FIELD
	n = *len - offsetof(struct sockaddr_un, sun_path); // -1
	// apple/bsd do not support abstract sockets
	if ((n > 0) && (src->sau.sun_path[0] == '\0'))
	    n = 0;
#else
	n = *len - offsetof(struct sockaddr_un, sun_path) - 1;
	if ((n > 0) && (src->sau.sun_path[0] == '\0')) // abstract
	    n++; // hmmm
	DEBUGF("inet_get_address *len = %u, n=%d", *len, n);
	if (n < 0)
	    n = 0;
#endif
	memcpy(dst+1, (char*)&src->sau.sun_path, n);
	*len = 1 + n;
	return 0;
    }
#if defined(HAVE_IN6) && defined(AF_INET6)
    else if ((family == AF_INET6) && (*len >= sizeof(struct sockaddr_in6))) {
	dst[0] = INET_AF_INET6;
	port = sock_ntohs(src->sai6.sin6_port);
	put_int16(port, dst+1);
	memcpy(dst+3, (char*)&src->sai6.sin6_addr,sizeof(struct in6_addr));
	*len = 3 + sizeof(struct in6_addr);
	return 0;
    }
#endif
    return -1;
}

static void desc_close(inet_descriptor* desc)
{
    if (desc->s != INVALID_SOCKET) {
#ifdef __WIN32__
	winsock_event_select(desc, FD_READ|FD_WRITE|FD_CLOSE, 0);
	sock_close(desc->s);
	desc->forced_events = 0;
	desc->send_would_block = 0;
#endif
	// We should close the fd here, but the other driver might still
	// be selecting on it.
	if (!desc->is_ignored)
	    driver_select(desc->port,(ErlDrvEvent)(long)desc->event, 
			  ERL_DRV_USE, 0);
	else
	  inet_stop_select((ErlDrvEvent)(long)desc->event,NULL);
	desc->event = INVALID_EVENT; /* closed by stop_select callback */
	desc->s = INVALID_SOCKET;
	desc->event_mask = 0;
    }
}

static void desc_close_read(inet_descriptor* desc)
{
    if (desc->s != INVALID_SOCKET) {
#ifdef __WIN32__
	/* This call can not be right???
	 * We want to turn off read events but keep any write events.
	 * But on windows driver_select(...,READ,1) is only used as a
	 * way to hook into the pollset. sock_select is used to control
	 * which events to wait for.
	 * It seems we used to disabled all events for the socket here.
	 *
	driver_select(desc->port, desc->event, DO_READ, 0); REMOVED */
#endif
	sock_select(desc, FD_READ | FD_CLOSE, 0);
    }
}


static int erl_inet_close(inet_descriptor* desc)
{
    free_subscribers(&desc->empty_out_q_subs);
    if ((desc->prebound == 0) && (desc->state & INET_F_OPEN)) {
	desc_close(desc);
	desc->state = INET_STATE_CLOSED;
    } else if (desc->prebound && (desc->s != INVALID_SOCKET)) {
	sock_select(desc, FD_READ | FD_WRITE | FD_CLOSE, 0);
	desc->event_mask = 0;
#ifdef __WIN32__
	desc->forced_events = 0;
	desc->send_would_block = 0;
#endif
    }
    return 0;
}


static ErlDrvSSizeT inet_ctl_open(inet_descriptor* desc, int domain, int type,
				  char** rbuf, ErlDrvSizeT rsize)
{
    if (desc->state != INET_STATE_CLOSED)
	return ctl_xerror(EXBADSEQ, rbuf, rsize);
    if ((desc->s = sock_open(domain, type, desc->sprotocol)) == INVALID_SOCKET)
	return ctl_error(sock_errno(), rbuf, rsize);
    if ((desc->event = sock_create_event(desc)) == INVALID_EVENT)
	return ctl_error(sock_errno(), rbuf, rsize);
    SET_NONBLOCKING(desc->s);
#ifdef __WIN32__
    driver_select(desc->port, desc->event, ERL_DRV_READ, 1);
#endif
    desc->state = INET_STATE_OPEN;
    desc->stype = type;
    desc->sfamily = domain;
    return ctl_reply(INET_REP_OK, NULL, 0, rbuf, rsize);
}


/* as inet_open but pass in an open socket (MUST BE OF RIGHT TYPE) */
static ErlDrvSSizeT inet_ctl_fdopen(inet_descriptor* desc, int domain, int type,
				    SOCKET s, char** rbuf, ErlDrvSizeT rsize)
{
    inet_address name;
    unsigned int sz = sizeof(name);

    /* check that it is a socket and that the socket is bound */
    if (IS_SOCKET_ERROR(sock_name(s, (struct sockaddr*) &name, &sz)))
	return ctl_error(sock_errno(), rbuf, rsize);
    if (name.sa.sa_family != domain)
	return ctl_error(EINVAL, rbuf, rsize);
    desc->s = s;
    if ((desc->event = sock_create_event(desc)) == INVALID_EVENT)
	return ctl_error(sock_errno(), rbuf, rsize);
    SET_NONBLOCKING(desc->s);
#ifdef __WIN32__
    driver_select(desc->port, desc->event, ERL_DRV_READ, 1);
#endif
    desc->state = INET_STATE_BOUND; /* assume bound */
    if (type == SOCK_STREAM) { /* check if connected */
	sz = sizeof(name);
	if (!IS_SOCKET_ERROR(sock_peer(s, (struct sockaddr*) &name, &sz)))
	    desc->state = INET_STATE_CONNECTED;
    }

    desc->prebound = 1; /* used to prevent a real close since
			 * the fd probably comes from an 
			 * external wrapper program, so it is
			 * not certain that we can open it again */
    desc->stype = type;
    desc->sfamily = domain;
    return ctl_reply(INET_REP_OK, NULL, 0, rbuf, rsize);
}

/* set socket options:
** return -1 on error
**         0 if ok
**         1 if ok force deliver of queued data
*/

static int inet_set_opts(inet_descriptor* desc, char* ptr, int len)
{
    int type;
    int proto;
    int opt;
    struct linger li_val;
    int ival;
    char* arg_ptr;
    int arg_sz;
    enum PacketParseType old_htype = desc->htype;
    int old_active = desc->active;
    int propagate = 0; /* Set to 1 if failure to set this option
			  should be propagated to erlang (not all 
			  errors can be propagated for BC reasons) */
    int res;

    while(len >= 5) {
	opt = *ptr++;
	ival = get_int32(ptr);
	ptr += 4;
	len -= 5;
	arg_ptr = (char*) &ival;
	arg_sz = sizeof(ival);
	proto = SOL_SOCKET;

	switch(opt) {
	case INET_LOPT_HEADER:
	    DEBUGF("inet_set_opts(%ld): s=%d, HEADER=%d",
		    (long)desc->port, desc->s,ival);
	    desc->hsz = ival;
	    continue;

	case INET_LOPT_MODE:
	    /* List or Binary: */
	    DEBUGF("inet_set_opts(%ld): s=%d, MODE=%d",
		    (long)desc->port, desc->s, ival);
	    desc->mode = ival;
	    continue;

	case INET_LOPT_DELIVER:
	    DEBUGF("inet_set_opts(%ld): s=%d, DELIVER=%d",
		    (long)desc->port, desc->s, ival);
	    desc->deliver = ival;
	    continue;
	    
	case INET_LOPT_BUFFER:
	    DEBUGF("inet_set_opts(%ld): s=%d, BUFFER=%d",
		    (long)desc->port, desc->s, ival);
	    if (ival < INET_MIN_BUFFER) ival = INET_MIN_BUFFER;
	    desc->bufsz = ival;
	    continue;

	case INET_LOPT_ACTIVE:
	    DEBUGF("inet_set_opts(%ld): s=%d, ACTIVE=%d",
		    (long)desc->port, desc->s,ival);
	    desc->active = ival;
            if (desc->active == INET_MULTI) {
                long ac = desc->active_count;
                int16_t nval = get_int16(ptr);
                ptr += 2;
                len -= 2;
                ac += nval;
                if (ac > INT16_MAX || ac < INT16_MIN)
                    return -1;
                desc->active_count += nval;
                if (desc->active_count < 0)
                    desc->active_count = 0;
                if (desc->active_count == 0) {
                    desc->active = INET_PASSIVE;
                    packet_passive_message(desc);
                }
            } else
                desc->active_count = 0;
	    if ((desc->stype == SOCK_STREAM) && (desc->active != INET_PASSIVE) && 
		(desc->state == INET_STATE_CLOSED)) {
		tcp_closed_message((tcp_descriptor *) desc);
		if (desc->exitf) {
		    driver_exit(desc->port, 0);
		    return 0; /* Give up on this socket, descriptor lost */
		} else {
		    desc_close_read(desc);
		}
	    }
	    continue;

	case INET_LOPT_PACKET:
	    DEBUGF("inet_set_opts(%ld): s=%d, PACKET=%d",
		    (long)desc->port, desc->s, ival);
	    desc->htype = ival;
	    continue;

	case INET_LOPT_PACKET_SIZE:
	    DEBUGF("inet_set_opts(%ld): s=%d, PACKET_SIZE=%d",
		    (long)desc->port, desc->s, ival);
	    desc->psize = (unsigned int)ival;
	    continue;

	case INET_LOPT_EXITONCLOSE:
	    DEBUGF("inet_set_opts(%ld): s=%d, EXITONCLOSE=%d",
		    (long)desc->port, desc->s, ival);
	    desc->exitf = ival;
	    continue;

	case INET_LOPT_TCP_HIWTRMRK:
	    if (desc->stype == SOCK_STREAM) {
		tcp_descriptor* tdesc = (tcp_descriptor*) desc;
		if (ival < 0) ival = 0;
		if (tdesc->low > ival)
		    tdesc->low = ival;
		tdesc->high = ival;
	    }
	    continue;

	case INET_LOPT_TCP_LOWTRMRK:
	    if (desc->stype == SOCK_STREAM) {
		tcp_descriptor* tdesc = (tcp_descriptor*) desc;
		if (ival < 0) ival = 0;
		if (tdesc->high < ival)
		    tdesc->high = ival;
		tdesc->low = ival;
	    }
	    continue;

	case INET_LOPT_TCP_MSGQ_HIWTRMRK:
	    if (desc->stype == SOCK_STREAM) {
#ifdef ERL_DRV_BUSY_MSGQ_LIM_MIN
	        ErlDrvSizeT high;
		if (ival < ERL_DRV_BUSY_MSGQ_LIM_MIN
		    || ERL_DRV_BUSY_MSGQ_LIM_MAX < ival)
		    return -1;
		high = (ErlDrvSizeT) ival;
		erl_drv_busy_msgq_limits(desc->port, NULL, &high);
#endif
	    }
	    continue;

	case INET_LOPT_TCP_MSGQ_LOWTRMRK:
	    if (desc->stype == SOCK_STREAM) {
#ifdef ERL_DRV_BUSY_MSGQ_LIM_MIN
		ErlDrvSizeT low;
		if (ival < ERL_DRV_BUSY_MSGQ_LIM_MIN
		    || ERL_DRV_BUSY_MSGQ_LIM_MAX < ival)
		    return -1;
		low = (ErlDrvSizeT) ival;
		erl_drv_busy_msgq_limits(desc->port, &low, NULL);
#endif
	    }
	    continue;

	case INET_LOPT_TCP_SEND_TIMEOUT:
	    if (desc->stype == SOCK_STREAM) {
		tcp_descriptor* tdesc = (tcp_descriptor*) desc;
		tdesc->send_timeout = ival;
	    }
	    continue;

	case INET_LOPT_TCP_SEND_TIMEOUT_CLOSE:
	    if (desc->stype == SOCK_STREAM) {
		tcp_descriptor* tdesc = (tcp_descriptor*) desc;
		tdesc->send_timeout_close = ival;
	    }
	    continue;
	    

	case INET_LOPT_TCP_DELAY_SEND:
	    if (desc->stype == SOCK_STREAM) {
		tcp_descriptor* tdesc = (tcp_descriptor*) desc;
		if (ival)
		    tdesc->tcp_add_flags |= TCP_ADDF_DELAY_SEND;
		else
		    tdesc->tcp_add_flags &= ~TCP_ADDF_DELAY_SEND;
	    }
	    continue;

	case INET_LOPT_LINE_DELIM:
	    DEBUGF("inet_set_opts(%ld): s=%d, LINE_DELIM=%d\r\n",
		   (long)desc->port, desc->s, ival);
	    desc->delimiter = (char)ival;
	    continue;

	case INET_OPT_REUSEADDR: 
	    type = SO_REUSEADDR;
	    DEBUGF("inet_set_opts(%ld): s=%d, SO_REUSEADDR=%d",
		    (long)desc->port, desc->s,ival);
	    break;
	case INET_OPT_KEEPALIVE: type = SO_KEEPALIVE;
	    DEBUGF("inet_set_opts(%ld): s=%d, SO_KEEPALIVE=%d",
		    (long)desc->port, desc->s, ival);
	    break;
	case INET_OPT_DONTROUTE: type = SO_DONTROUTE;
	    DEBUGF("inet_set_opts(%ld): s=%d, SO_DONTROUTE=%d",
		    (long)desc->port, desc->s, ival);
	    break;
	case INET_OPT_BROADCAST: type = SO_BROADCAST;
	    DEBUGF("inet_set_opts(%ld): s=%d, SO_BROADCAST=%d",
		    (long)desc->port, desc->s,ival);
	    break;
	case INET_OPT_OOBINLINE: type = SO_OOBINLINE; 
	    DEBUGF("inet_set_opts(%ld): s=%d, SO_OOBINLINE=%d",
		    (long)desc->port, desc->s, ival);
	    break;
	case INET_OPT_SNDBUF:    type = SO_SNDBUF; 
	    DEBUGF("inet_set_opts(%ld): s=%d, SO_SNDBUF=%d",
		    (long)desc->port, desc->s, ival);
	    break;
	case INET_OPT_RCVBUF:    type = SO_RCVBUF; 
	    DEBUGF("inet_set_opts(%ld): s=%d, SO_RCVBUF=%d",
		    (long)desc->port, desc->s, ival);
	    break;
	case INET_OPT_LINGER:    type = SO_LINGER; 
	    if (len < 4)
		return -1;
	    li_val.l_onoff = ival;
	    li_val.l_linger = get_int32(ptr);
	    ptr += 4;
	    len -= 4;
	    arg_ptr = (char*) &li_val;
	    arg_sz = sizeof(li_val);
	    DEBUGF("inet_set_opts(%ld): s=%d, SO_LINGER=%d,%d",
		    (long)desc->port, desc->s, li_val.l_onoff,li_val.l_linger);
	    break;

	case INET_OPT_PRIORITY: 
#ifdef SO_PRIORITY
	    type = SO_PRIORITY;
	    propagate = 1; /* We do want to know if this fails */
	    DEBUGF("inet_set_opts(%ld): s=%d, SO_PRIORITY=%d",
		    (long)desc->port, desc->s, ival);
	    break;
#else
	    continue;
#endif
	case INET_OPT_TOS:
#if defined(IP_TOS) && defined(SOL_IP)
	    proto = SOL_IP;
	    type = IP_TOS;
	    propagate = 1;
	    DEBUGF("inet_set_opts(%ld): s=%d, IP_TOS=%d",
		    (long)desc->port, desc->s, ival);
	    break;
#else
	    continue;
#endif

	case TCP_OPT_NODELAY:
	    proto = IPPROTO_TCP; 
	    type = TCP_NODELAY; 
	    DEBUGF("inet_set_opts(%ld): s=%d, TCP_NODELAY=%d",
		    (long)desc->port, desc->s, ival);
	    break;


	case INET_OPT_IPV6_V6ONLY:
#if HAVE_DECL_IPV6_V6ONLY
	    proto = IPPROTO_IPV6;
	    type = IPV6_V6ONLY;
	    propagate = 1;
	    DEBUGF("inet_set_opts(%ld): s=%d, IPV6_V6ONLY=%d",
		    (long)desc->port, desc->s, ival);
	    break;
#elif defined(__WIN32__) && defined(HAVE_IN6) && defined(AF_INET6)
	    /* Fake a'la OpenBSD; set to 'true' is fine but 'false' invalid. */
	    if (ival != 0) continue;
	    else return -1;
	    break;
#else
	    continue;
#endif

	case INET_OPT_RAW:
	    if (len < 8) {
		return -1;
	    }
	    proto = ival;
	    type = get_int32(ptr);
	    ptr += 4;
	    arg_sz = get_int32(ptr);
	    ptr += 4;
	    len -= 8;
	    if (len < arg_sz) {
		return -1;
	    }
	    arg_ptr = ptr;
	    ptr += arg_sz;
	    len -= arg_sz;
	    break;

	default:
	    return -1;
	}
	res = sock_setopt	    (desc->s, proto, type, arg_ptr, arg_sz);

	if (propagate && res != 0) {
	    return -1;
	}
	DEBUGF("inet_set_opts(%ld): s=%d returned %d",
		(long)desc->port, desc->s, res);
	if (type == SO_RCVBUF) {
	    /* make sure we have desc->bufsz >= SO_RCVBUF */
	    if (ival > desc->bufsz)
		desc->bufsz = ival;
	}
    }

    if ( ((desc->stype == SOCK_STREAM) && IS_CONNECTED(desc)) ||
	((desc->stype == SOCK_DGRAM) && IS_OPEN(desc))) {

	if (desc->active != old_active)
	    sock_select(desc, (FD_READ|FD_CLOSE), (desc->active>0));

	/* XXX: UDP sockets could also trigger immediate read here NIY */
	if ((desc->stype==SOCK_STREAM) && desc->active) {
	    if (!old_active || (desc->htype != old_htype)) {
		/* passive => active change OR header type change in active mode */
		/* Return > 1 if only active changed to INET_ONCE -> direct read if
		   header type is unchanged. */
		/* XXX fprintf(stderr,"desc->htype == %d, old_htype == %d, 
		   desc->active == %d, old_active == %d",(int)desc->htype, 
		   (int) old_htype, (int) desc->active, (int) old_active );*/
		return 1+(desc->htype == old_htype && desc->active == INET_ONCE);
	    }
	    return 0;
	}
    }
    return 0;
}


/* load all option values into the buf and reply 
** return total length of reply filled into ptr
** ptr should point to a buffer with 9*len +1 to be safe!!
*/

static ErlDrvSSizeT inet_fill_opts(inet_descriptor* desc,
				   char* buf, ErlDrvSizeT len,
				   char** dest, ErlDrvSizeT destlen)
{
    int type;
    int proto;
    int opt;
    struct linger li_val;
    int ival;
    char* arg_ptr;
    unsigned int arg_sz;
    char *ptr = NULL;
    ErlDrvSizeT dest_used = 0;
    ErlDrvSizeT dest_allocated = destlen;
    char *orig_dest = *dest;

    /* Ptr is a name parameter */ 
#define RETURN_ERROR()				\
    do {					\
	if (dest_allocated > destlen) {		\
	    FREE(*dest);			\
	    *dest = orig_dest;			\
	}					\
	return -1;				\
    } while(0)

#define PLACE_FOR(Size,Ptr)						   \
    do {								   \
	ErlDrvSizeT need = dest_used + (Size);					   \
	if (need > INET_MAX_OPT_BUFFER) {				   \
	    RETURN_ERROR();						   \
	}								   \
	if (need > dest_allocated) {					   \
	    char *new_buffer;						   \
	    if (dest_allocated == destlen) {				   \
		new_buffer = ALLOC((dest_allocated = need + 10));	   \
		memcpy(new_buffer,*dest,dest_used);			   \
	    } else {							   \
		new_buffer = REALLOC(*dest, (dest_allocated = need + 10)); \
	    }								   \
	    *dest = new_buffer;						   \
	}								   \
	(Ptr) = (*dest) + dest_used;					   \
	dest_used = need;						   \
    } while (0)

    /* Ptr is a name parameter */ 
#define TRUNCATE_TO(Size,Ptr)				\
    do {						\
	ErlDrvSizeT new_need = ((Ptr) - (*dest)) + (Size);	\
	if (new_need > dest_used) {			\
	    erl_exit(1,"Internal error in inet_drv, "	\
		     "miscalculated buffer size");	\
	}						\
	dest_used = new_need;				\
    } while(0)

    
    PLACE_FOR(1,ptr);
    *ptr = INET_REP_OK;

    while(len--) {
	opt = *buf++;
	proto = SOL_SOCKET;
	ival = 0; /* Windows Vista needs this (only writes part of it) */
	arg_sz = sizeof(ival);
	arg_ptr = (char*) &ival;

	PLACE_FOR(5,ptr);

	switch(opt) {
	case INET_LOPT_BUFFER:
	    *ptr++ = opt;
	    put_int32(desc->bufsz, ptr);
	    continue;
	case INET_LOPT_HEADER:
	    *ptr++ = opt;
	    put_int32(desc->hsz, ptr);
	    continue;
	case INET_LOPT_MODE:
	    *ptr++ = opt;
	    put_int32(desc->mode, ptr);
	    continue;
	case INET_LOPT_DELIVER:
	    *ptr++ = opt;
	    put_int32(desc->deliver, ptr);
	    continue;
	case INET_LOPT_ACTIVE:
	    *ptr++ = opt;
	    put_int32(desc->active, ptr);
	    continue;
	case INET_LOPT_PACKET:
	    *ptr++ = opt;
	    put_int32(desc->htype, ptr);
	    continue;
	case INET_LOPT_PACKET_SIZE:
	    *ptr++ = opt;
	    put_int32(desc->psize, ptr);
	    continue;
	case INET_LOPT_EXITONCLOSE:
	    *ptr++ = opt;
	    put_int32(desc->exitf, ptr);
	    continue;

	case INET_LOPT_TCP_HIWTRMRK:
	    if (desc->stype == SOCK_STREAM) {
		*ptr++ = opt;
		ival = ((tcp_descriptor*)desc)->high;
		put_int32(ival, ptr);
	    } else {
		TRUNCATE_TO(0,ptr);
	    }
	    continue;

	case INET_LOPT_TCP_LOWTRMRK:
	    if (desc->stype == SOCK_STREAM) {
		*ptr++ = opt;
		ival = ((tcp_descriptor*)desc)->low;
		put_int32(ival, ptr);
	    } else {
		TRUNCATE_TO(0,ptr);
	    }
	    continue;

	case INET_LOPT_TCP_MSGQ_HIWTRMRK:
	    if (desc->stype == SOCK_STREAM) {
#ifdef ERL_DRV_BUSY_MSGQ_READ_ONLY
	        ErlDrvSizeT high = ERL_DRV_BUSY_MSGQ_READ_ONLY;
		*ptr++ = opt;
		erl_drv_busy_msgq_limits(desc->port, NULL, &high);
		high = 1;
		ival = high > INT_MAX ? INT_MAX : (int) high;
		put_int32(ival, ptr);
#else
		TRUNCATE_TO(0,ptr);
#endif
	    }
	    else {
		TRUNCATE_TO(0,ptr);
	    }
	    continue;

	case INET_LOPT_TCP_MSGQ_LOWTRMRK:
	    if (desc->stype == SOCK_STREAM) {
#ifdef ERL_DRV_BUSY_MSGQ_READ_ONLY
		ErlDrvSizeT low = ERL_DRV_BUSY_MSGQ_READ_ONLY;
		*ptr++ = opt;
		erl_drv_busy_msgq_limits(desc->port, &low, NULL);
		low = 1;
		ival = low > INT_MAX ? INT_MAX : (int) low;
		put_int32(ival, ptr);
#else
		TRUNCATE_TO(0,ptr);
#endif
	    }
	    else {
		TRUNCATE_TO(0,ptr);
	    }
	    continue;

	case INET_LOPT_TCP_SEND_TIMEOUT:
	    if (desc->stype == SOCK_STREAM) {
		*ptr++ = opt;
		ival = ((tcp_descriptor*)desc)->send_timeout;
		put_int32(ival, ptr);
	    } else {
		TRUNCATE_TO(0,ptr);
	    }
	    continue;

	case INET_LOPT_TCP_SEND_TIMEOUT_CLOSE:
	    if (desc->stype == SOCK_STREAM) {
		*ptr++ = opt;
		ival = ((tcp_descriptor*)desc)->send_timeout_close;
		put_int32(ival, ptr);
	    } else {
		TRUNCATE_TO(0,ptr);
	    }
	    continue;

	case INET_LOPT_TCP_DELAY_SEND:
	    if (desc->stype == SOCK_STREAM) {
		*ptr++ = opt;
		ival = !!(((tcp_descriptor*)desc)->tcp_add_flags & TCP_ADDF_DELAY_SEND);
		put_int32(ival, ptr);
	    } else {
		TRUNCATE_TO(0,ptr);
	    }
	    continue;

	case INET_OPT_PRIORITY:
#ifdef SO_PRIORITY
	    type = SO_PRIORITY;
	    break;
#else
	    *ptr++ = opt;
	    put_int32(0, ptr);
	    continue;
#endif
	case INET_OPT_TOS:
#if defined(IP_TOS) && defined(SOL_IP)
	    proto = SOL_IP;
	    type = IP_TOS;
	    break;
#else
	    *ptr++ = opt;
	    put_int32(0, ptr);
	    continue;
#endif
	case INET_OPT_REUSEADDR: 
	    type = SO_REUSEADDR; 
	    break;
	case INET_OPT_KEEPALIVE: 
	    type = SO_KEEPALIVE; 
	    break;
	case INET_OPT_DONTROUTE: 
	    type = SO_DONTROUTE; 
	    break;
	case INET_OPT_BROADCAST: 
	    type = SO_BROADCAST;
	    break;
	case INET_OPT_OOBINLINE: 
	    type = SO_OOBINLINE; 
	    break;
	case INET_OPT_SNDBUF:    
	    type = SO_SNDBUF; 
	    break;
	case INET_OPT_RCVBUF:    
	    type = SO_RCVBUF; 
	    break;
	case TCP_OPT_NODELAY:
	    proto = IPPROTO_TCP;
	    type = TCP_NODELAY;
	    break;

	case INET_OPT_IPV6_V6ONLY:
#if HAVE_DECL_IPV6_V6ONLY
	    proto = IPPROTO_IPV6;
	    type = IPV6_V6ONLY;
	    break;
#elif defined(__WIN32__) && defined(HAVE_IN6) && defined(AF_INET6)
	    /* Fake reading 'true' */
	    *ptr++ = opt;
	    put_int32(1, ptr);
	    ptr += 4;
	    continue;
#else
	    TRUNCATE_TO(0,ptr);
	    continue; /* skip - no result */
#endif

	case INET_OPT_RAW:
	    {
		int data_provided;
		/* Raw options are icky, handle directly... */
		if (len < 13) {
		    RETURN_ERROR();
		}
		len -= 13;
		proto = get_int32(buf);
		buf += 4;
		type = get_int32(buf);
		buf += 4;
		data_provided = (int) *buf++;
		arg_sz = get_int32(buf);
		if (arg_sz > INET_MAX_OPT_BUFFER) {
		    RETURN_ERROR();
		}
		buf += 4;
		TRUNCATE_TO(0,ptr);
		PLACE_FOR(13 + arg_sz,ptr);
		arg_ptr = ptr + 13;
		if (data_provided) {
		    if (len < arg_sz) {
			RETURN_ERROR();
		    }
		    memcpy(arg_ptr,buf,arg_sz);
		    buf += arg_sz;
		    len -= arg_sz;
		}
		if (IS_SOCKET_ERROR(sock_getopt(desc->s,proto,type,
						arg_ptr,&arg_sz))) {
		    TRUNCATE_TO(0,ptr); 
		    continue;
		}
		TRUNCATE_TO(arg_sz + 13,ptr);
		*ptr++ = opt;
		put_int32(proto,ptr);
		ptr += 4;
		put_int32(type,ptr);
		ptr += 4;
		put_int32(arg_sz,ptr);
		continue;
	    }

	case UNIX_OPT_PEERCRED: {
#ifdef LOCAL_PEERCRED
	    struct xucred x;
	    socklen_t xucredlen = sizeof(x);
	    if (IS_SOCKET_ERROR(sock_getopt(desc->s,SOL_LOCAL,LOCAL_PEERCRED,
					    &x,&xucredlen)) ||
		(x.cr_version != XUCRED_VERSION)) {
		TRUNCATE_TO(0,ptr);
		continue;
	    }
	    *ptr++ = opt;
	    put_int32(x.cr_uid, ptr);
#elif defined (HAVE_GETPEERUCRED)
	    ucred_t *uc = NULL;
	    if (IS_SOCKET_ERROR(getpeerucred(desc->s,&uc))) {
		TRUNCATE_TO(0,ptr);
		continue;
	    }
	    *ptr++ = opt;
	    put_int32(ucred_geteuid(uc), ptr);
	    (void) ucred_free(uc);
#elif defined (SO_PEERCRED)
	    struct ucred u;
	    socklen_t ucredlen = sizeof(u);
	    if (IS_SOCKET_ERROR(sock_getopt(desc->s,SOL_SOCKET,SO_PEERCRED,
					    &u,&ucredlen))) {
	        TRUNCATE_TO(0,ptr);
		continue;
	    }
	    *ptr++ = opt;
	    put_int32(u.uid, ptr);
#elif defined (HAVE_GETPEEREID)
#warning "using getpeereid"
	    uid_t euid;
	    gid_t egid;
	    if (IS_SOCKET_ERROR(getpeereid(desc->s, &euid, &egid))) {
	        TRUNCATE_TO(0,ptr);
		continue;
	    }
	    *ptr++ = opt;
	    put_int32(euid, ptr);
#else
#warning "no method of accessing socket peercred"
	    TRUNCATE_TO(0,ptr);
#endif
	    continue;
	}

	case UNIX_OPT_PEERPID: {
#ifdef LOCAL_PEERPID
	    pid_t p;
	    socklen_t plen = sizeof(p);
	    if (IS_SOCKET_ERROR(sock_getopt(desc->s,SOL_LOCAL,LOCAL_PEERPID,
					    &p,&plen))) {
	        TRUNCATE_TO(0,ptr);
		continue;
	    }
	    *ptr++ = opt;
	    put_int32(p, ptr);
#elif defined (HAVE_GETPEERUCRED)
	    ucred_t *uc = NULL;
	    if (IS_SOCKET_ERROR(getpeerucred(desc->s,&uc))) {
		TRUNCATE_TO(0,ptr);
		continue;
	    }
	    *ptr++ = opt;
	    put_int32(ucred_getpid(uc), ptr);
	    (void) ucred_free(uc);
#elif defined (SO_PEERCRED)
	    struct ucred u;
	    socklen_t ucredlen = sizeof(u);
	    if (IS_SOCKET_ERROR(sock_getopt(desc->s,SOL_SOCKET,SO_PEERCRED,
					    &u,&ucredlen))) {
	        TRUNCATE_TO(0,ptr);
		continue;
	    }
	    *ptr++ = opt;
	    put_int32(u.pid, ptr);
#else
#warning "no method of accessing socket peerpid found"
	    TRUNCATE_TO(0,ptr);
#endif
	    continue;
	}

	default:
	    RETURN_ERROR();
	}
	/* We have 5 bytes allocated to ptr */
	if (IS_SOCKET_ERROR(sock_getopt(desc->s,proto,type,arg_ptr,&arg_sz))) {
	    TRUNCATE_TO(0,ptr);
	    continue;
	}
	*ptr++ = opt;
	if (arg_ptr == (char*)&ival) {
	    put_int32(ival, ptr);
	}
	else {
	    put_int32(((uint32_t) li_val.l_onoff), ptr);
	    PLACE_FOR(4,ptr);
	    put_int32(((uint32_t) li_val.l_linger), ptr);
	}
    }
    return (dest_used);
#undef PLACE_FOR
#undef TRUNCATE_TO
#undef RETURN_ERROR
}


/* fill statistics reply, op codes from src and result in dest
** dst area must be a least 5*len + 1 bytes
*/
static ErlDrvSSizeT inet_fill_stat(inet_descriptor* desc,
				   char* src, ErlDrvSizeT len, char* dst)
{
    unsigned long val;
    int op;
    char* dst_start = dst;

    *dst++ = INET_REP_OK;     /* put reply code */
    while (len--) {
	op = *src++;
	*dst++ = op;  /* copy op code */
	switch(op) {
	case INET_STAT_RECV_CNT:  
	    val = desc->recv_cnt;    
	    break;
	case INET_STAT_RECV_MAX:  
	    val = (unsigned long) desc->recv_max;    
	    break;
	case INET_STAT_RECV_AVG:  
	    val = (unsigned long) desc->recv_avg;    
	    break;
	case INET_STAT_RECV_DVI:  
	    val = (unsigned long) fabs(desc->recv_dvi); 
	    break;
	case INET_STAT_SEND_CNT:  
	    val = desc->send_cnt; 
	    break;
	case INET_STAT_SEND_MAX:  
	    val = desc->send_max; 
	    break;
	case INET_STAT_SEND_AVG: 
	    val = (unsigned long) desc->send_avg;
	    break;
	case INET_STAT_SEND_PND:  
	    val = (unsigned long) driver_sizeq(desc->port);
	    break;
	case INET_STAT_RECV_OCT:
	    put_int64(desc->recv_oct, dst); /* write it all */
	    dst += 8;
	    continue;
	case INET_STAT_SEND_OCT:
	    put_int64(desc->send_oct, dst); /* write it all */
	    dst += 8;
	    continue;
	default: return -1; /* invalid argument */
	}
	put_int32(val, dst);  /* write 32bit value */
	dst += 4;
    }
    return dst - dst_start;  /* actual length */
}

static void
send_empty_out_q_msgs(inet_descriptor* desc)
{
  ErlDrvTermData msg[6];
  int msg_len = 0;

  if(NO_SUBSCRIBERS(&desc->empty_out_q_subs))
    return;

  msg_len = LOAD_ATOM(msg, msg_len, am_empty_out_q);
  msg_len = LOAD_PORT(msg, msg_len, desc->dport);
  msg_len = LOAD_TUPLE(msg, msg_len, 2);

  send_to_subscribers(desc,
		      &desc->empty_out_q_subs,
		      1,
		      msg,
		      msg_len);
}

/* subscribe and fill subscription reply, op codes from src and
** result in dest dst area must be a least 5*len + 1 bytes
*/
static ErlDrvSSizeT inet_subscribe(inet_descriptor* desc,
				   char* src, ErlDrvSizeT len, char* dst)
{
    unsigned long val;
    int op;
    char* dst_start = dst;

    *dst++ = INET_REP_OK;     /* put reply code */
    while (len--) {
	op = *src++;
	*dst++ = op;  /* copy op code */
	switch(op) {
	case INET_SUBS_EMPTY_OUT_Q:  
	  val = driver_sizeq(desc->port);
	  if(val > 0)
	    if(!save_subscriber(&desc->empty_out_q_subs,
				driver_caller(desc->port)))
	      return 0;
	  break;
	default: return -1; /* invalid argument */
	}
	put_int32(val, dst);  /* write 32bit value */
	dst += 4;
    }
    return dst - dst_start;  /* actual length */
}

/* Terminate socket */
static void inet_stop(inet_descriptor* desc)
{
    erl_inet_close(desc);
    FREE(desc);
}


/* Allocate descriptor */
static ErlDrvData inet_start(ErlDrvPort port, int size, int protocol)
{
    inet_descriptor* desc;

    if ((desc = (inet_descriptor*) ALLOC(size)) == NULL)
	return NULL;

    desc->s = INVALID_SOCKET;
    desc->event = INVALID_EVENT;
    desc->event_mask = 0;
#ifdef __WIN32__
    desc->forced_events = 0;
    desc->send_would_block = 0;
#endif
    desc->port = port;
    desc->dport = driver_mk_port(port);
    desc->state = INET_STATE_CLOSED;
    desc->prebound = 0;
    desc->bufsz = INET_DEF_BUFFER; 
    desc->hsz = 0;                     /* list header size */
    desc->htype = TCP_PB_RAW;          /* default packet type */
    desc->psize = 0;                   /* no size check */
    desc->stype = -1;                  /* bad stype */
    desc->sfamily = -1;
    desc->sprotocol = protocol;
    desc->mode    = INET_MODE_LIST;    /* list mode */
    desc->exitf   = 1;                 /* exit port when close on active 
					  socket */
    desc->deliver = INET_DELIVER_TERM; /* standard term format */
    desc->active  = INET_PASSIVE;      /* start passive */
    desc->active_count = 0;
    desc->delimiter    = '\n';         /* line delimiting char */
    desc->oph = NULL;
    desc->opt = NULL;

    desc->peer_ptr = NULL;
    desc->name_ptr = NULL;

    desc->recv_oct  = 0;
    desc->recv_cnt = 0;
    desc->recv_max = 0;    
    desc->recv_avg = 0.0;
    desc->recv_dvi = 0.0;
    desc->send_oct = 0;
    desc->send_cnt = 0;
    desc->send_max = 0;
    desc->send_avg = 0.0;
    desc->empty_out_q_subs.subscriber = NO_PROCESS;
    desc->empty_out_q_subs.next = NULL;

    memzero((char *)&desc->remote,sizeof(desc->remote));

    desc->is_ignored = 0;

    return (ErlDrvData)desc;
}


#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 256
#endif

/*
** common TCP control command
*/
static ErlDrvSSizeT inet_ctl(inet_descriptor* desc, int cmd, char* buf,
			     ErlDrvSizeT len, char** rbuf, ErlDrvSizeT rsize)
{
    switch (cmd) {

    case INET_REQ_GETSTAT: {
	  char* dst;
	  ErlDrvSizeT i;
	  int dstlen = 1;  /* Reply code */

	  for (i = 0; i < len; i++) {
	      switch(buf[i]) {
	      case INET_STAT_SEND_OCT: dstlen += 9; break;
	      case INET_STAT_RECV_OCT: dstlen += 9; break;
	      default: dstlen += 5; break;
	      }
	  }
	  DEBUGF("inet_ctl(%ld): GETSTAT", (long) desc->port); 
	  if (dstlen > INET_MAX_OPT_BUFFER) /* sanity check */
	      return 0;
	  if (dstlen > rsize) {
	      if ((dst = (char*) ALLOC(dstlen)) == NULL)
		  return 0;
	      *rbuf = dst;  /* call will free this buffer */
	  }
	  else
	      dst = *rbuf;  /* ok we fit in buffer given */
	  return inet_fill_stat(desc, buf, len, dst);
      }

    case INET_REQ_SUBSCRIBE: {
	  char* dst;
	  int dstlen = 1 /* Reply code */ + len*5;
	  DEBUGF("inet_ctl(%ld): INET_REQ_SUBSCRIBE", (long) desc->port); 
	  if (dstlen > INET_MAX_OPT_BUFFER) /* sanity check */
	      return 0;
	  if (dstlen > rsize) {
	      if ((dst = (char*) ALLOC(dstlen)) == NULL)
		  return 0;
	      *rbuf = dst;  /* call will free this buffer */
	  }
	  else
	      dst = *rbuf;  /* ok we fit in buffer given */
	  return inet_subscribe(desc, buf, len, dst);
      }

    case INET_REQ_GETOPTS: {    /* get options */
	ErlDrvSSizeT replen;
	DEBUGF("inet_ctl(%ld): GETOPTS", (long)desc->port); 

	if ((replen = inet_fill_opts(desc, buf, len, rbuf, rsize)) < 0) {
	    return ctl_error(EINVAL, rbuf, rsize);
	}
	return replen;
    }

    case INET_REQ_SETOPTS:  {   /* set options */
	DEBUGF("inet_ctl(%ld): SETOPTS", (long)desc->port); 
	/* XXX fprintf(stderr,"inet_ctl(%ld): SETOPTS (len = %d)", (long)desc->port,(int) len); */
	switch(inet_set_opts(desc, buf, len)) {
	case -1: 
	    return ctl_error(EINVAL, rbuf, rsize);
	case 0: 
	    return ctl_reply(INET_REP_OK, NULL, 0, rbuf, rsize);
	case 1:
	    /*
	     * Let's hope that the descriptor really is a tcp_descriptor here.
	     */
	    /* fprintf(stderr,"Triggered tcp_deliver by setopt."); */
	    tcp_deliver((tcp_descriptor *) desc, 0);
	    return ctl_reply(INET_REP_OK, NULL, 0, rbuf, rsize);
	default:  
	    /* fprintf(stderr,"Triggered tcp_recv by setopt."); */
	    /*
	     * Same as above, but active changed to once w/o header type
	     * change, so try a read instead of just deliver. 
	     */
	    tcp_recv((tcp_descriptor *) desc, 0);
	    return ctl_reply(INET_REP_OK, NULL, 0, rbuf, rsize);
	}
    }

    case INET_REQ_GETSTATUS: {
	char tbuf[4];

	DEBUGF("inet_ctl(%ld): GETSTATUS", (long)desc->port); 
	put_int32(desc->state, tbuf);
	return ctl_reply(INET_REP_OK, tbuf, 4, rbuf, rsize);
    }

    case INET_REQ_GETTYPE: {
	char tbuf[8];

	DEBUGF("inet_ctl(%ld): GETTYPE", (long)desc->port); 
	if (desc->sfamily == AF_UNIX) {
	    put_int32(INET_AF_UNIX, &tbuf[0]);   // yes, it is a funny name :-)
	}
	else if (desc->sfamily == AF_INET) {
	    put_int32(INET_AF_INET, &tbuf[0]);
	}
#if defined(HAVE_IN6) && defined(AF_INET6)
        else if (desc->sfamily == AF_INET6) {
	    put_int32(INET_AF_INET6, &tbuf[0]);
	}
#endif
	else
	    return ctl_error(EINVAL, rbuf, rsize);

	if (desc->stype == SOCK_STREAM) {
	    put_int32(INET_TYPE_STREAM, &tbuf[4]);
	}
	else if (desc->stype == SOCK_DGRAM) {
	    put_int32(INET_TYPE_DGRAM, &tbuf[4]);
	}

	else
	    return ctl_error(EINVAL, rbuf, rsize);
	return ctl_reply(INET_REP_OK, tbuf, 8, rbuf, rsize);
    }


    case INET_REQ_GETFD: {
	char tbuf[4];

	DEBUGF("inet_ctl(%ld): GETFD", (long)desc->port); 
	if (!IS_OPEN(desc))
	    return ctl_error(EINVAL, rbuf, rsize);
	put_int32((long)desc->s, tbuf);
	return ctl_reply(INET_REP_OK, tbuf, 4, rbuf, rsize);
    }
	
    case INET_REQ_GETHOSTNAME: { /* get host name */
	char tbuf[MAXHOSTNAMELEN];

	DEBUGF("inet_ctl(%ld): GETHOSTNAME", (long)desc->port); 
	if (len != 0)
	    return ctl_error(EINVAL, rbuf, rsize);

	if (IS_SOCKET_ERROR(sock_hostname(tbuf, MAXHOSTNAMELEN)))
	    return ctl_error(sock_errno(), rbuf, rsize);
	return ctl_reply(INET_REP_OK, tbuf, strlen(tbuf), rbuf, rsize);
    }

    case INET_REQ_PEER:  {      /* get peername */
	char tbuf[sizeof(inet_address)];
	inet_address peer;
	inet_address* ptr;
	unsigned int sz = sizeof(peer);

	DEBUGF("inet_ctl(%ld): PEER", (long)desc->port); 

	if (!(desc->state & INET_F_ACTIVE))
	    return ctl_error(ENOTCONN, rbuf, rsize);
	if ((ptr = desc->peer_ptr) == NULL) {
	    ptr = &peer;
	    if (IS_SOCKET_ERROR(sock_peer(desc->s, (struct sockaddr*)ptr,&sz)))
		return ctl_error(sock_errno(), rbuf, rsize);
	}
	if (inet_get_address(ptr->sa.sa_family, tbuf, ptr, &sz) < 0)
	    return ctl_error(EINVAL, rbuf, rsize);
	return ctl_reply(INET_REP_OK, tbuf, sz, rbuf, rsize);
    }

    case INET_REQ_SETPEER: { /* set fake peername Port Address */
	if (len == 0) {
	    desc->peer_ptr = NULL;
	    return ctl_reply(INET_REP_OK, NULL, 0, rbuf, rsize);
	}
	else if (len < 2)
	    return ctl_error(EINVAL, rbuf, rsize);	    
	else if (inet_set_faddress(desc->sfamily, &desc->peer_addr,
				   buf, &len) == NULL)
	    return ctl_error(EINVAL, rbuf, rsize);
	else {
	    desc->peer_ptr = &desc->peer_addr;
	    return ctl_reply(INET_REP_OK, NULL, 0, rbuf, rsize);	    
	}
    }

    case INET_REQ_NAME:  {      /* get sockname */
	char tbuf[sizeof(inet_address)];
	inet_address name;
	inet_address* ptr;
	unsigned int sz = sizeof(name);

	DEBUGF("inet_ctl(%ld): NAME", (long)desc->port); 

	if (!IS_BOUND(desc))
	    return ctl_error(EINVAL, rbuf, rsize); /* address is not valid */

	if ((ptr = desc->name_ptr) == NULL) {
	    ptr = &name;
	    if (IS_SOCKET_ERROR(sock_name(desc->s, (struct sockaddr*)ptr, &sz)))
		return ctl_error(sock_errno(), rbuf, rsize);
	}
	if (inet_get_address(ptr->sa.sa_family, tbuf, ptr, &sz) < 0)
	    return ctl_error(EINVAL, rbuf, rsize);
	return ctl_reply(INET_REP_OK, tbuf, sz, rbuf, rsize);
    }

    case INET_REQ_SETNAME: { /* set fake peername Port Address */
	if (len == 0) {
	    desc->name_ptr = NULL;
	    return ctl_reply(INET_REP_OK, NULL, 0, rbuf, rsize);
	}
	else if (len < 2)
	    return ctl_error(EINVAL, rbuf, rsize);	    
	else if (inet_set_faddress(desc->sfamily, &desc->name_addr,
				  buf, &len) == NULL)
	    return ctl_error(EINVAL, rbuf, rsize);
	else {
	    desc->name_ptr = &desc->name_addr;
	    return ctl_reply(INET_REP_OK, NULL, 0, rbuf, rsize);	    
	}
    }

    case INET_REQ_BIND:  {      /* bind socket */
	inet_address local;

	DEBUGF("inet_ctl(%ld): BIND", (long)desc->port); 

	if (len < 2)
	    return ctl_error(EINVAL, rbuf, rsize);
	if (desc->state != INET_STATE_OPEN)
	    return ctl_xerror(EXBADPORT, rbuf, rsize);

	if (inet_set_faddress(desc->sfamily, &local, buf, &len) == NULL)
	    return ctl_error(EINVAL, rbuf, rsize);

	if (IS_SOCKET_ERROR(sock_bind(desc->s,(struct sockaddr*) &local, len)))
	    return ctl_error(sock_errno(), rbuf, rsize);

	desc->state = INET_STATE_BOUND;
	return ctl_reply(INET_REP_OK, NULL, 0, rbuf, rsize);
    }

    case INET_REQ_IGNOREFD: {
      DEBUGF("inet_ctl(%ld): IGNOREFD, IGNORED = %d",
	      (long)desc->port,(int)*buf);

      /*
       * FD can only be ignored for connected TCP connections for now,
       * possible to add UDP and SCTP support if needed.
       */
      if (!IS_CONNECTED(desc))
	  return ctl_error(ENOTCONN, rbuf, rsize);

      if (!desc->stype == SOCK_STREAM)
	  return ctl_error(EINVAL, rbuf, rsize);

      if (*buf == 1 && !desc->is_ignored) {
	  sock_select(desc, (FD_READ|FD_WRITE|FD_CLOSE|ERL_DRV_USE_NO_CALLBACK), 0);
	  desc->is_ignored = INET_IGNORE_READ;
      } else if (*buf == 0 && desc->is_ignored) {
	  int flags = (FD_READ|FD_CLOSE|((desc->is_ignored & INET_IGNORE_WRITE)?FD_WRITE:0));
	  desc->is_ignored = INET_IGNORE_NONE;
	  sock_select(desc, flags, 1);
      } else
	  return ctl_error(EINVAL, rbuf, rsize);

      return ctl_reply(INET_REP_OK, NULL, 0, rbuf, rsize);
    }

    case INET_REQ_GETSERVBYNAME: { /* L1 Name-String L2 Proto-String */
	char namebuf[256];
	char protobuf[256];
	char tbuf[2];
	struct servent* srv;
	short port;
	int n;

	if (len < 2)
	    return ctl_error(EINVAL, rbuf, rsize);
	n = get_int8(buf); buf++; len--;
	if (n >= len) /* the = sign makes the test inklude next length byte */
	    return ctl_error(EINVAL, rbuf, rsize);
	memcpy(namebuf, buf, n);
	namebuf[n] = '\0';
	len -= n; buf += n;
	n = get_int8(buf); buf++; len--;
	if (n > len)
	    return ctl_error(EINVAL, rbuf, rsize);
	memcpy(protobuf, buf, n);
	protobuf[n] = '\0';
	if ((srv = sock_getservbyname(namebuf, protobuf)) == NULL)
	    return ctl_error(EINVAL, rbuf, rsize);
	port = sock_ntohs(srv->s_port);
	put_int16(port, tbuf);
	return ctl_reply(INET_REP_OK, tbuf, 2, rbuf, rsize);
    }

    case INET_REQ_GETSERVBYPORT: { /* P1 P0 L1 Proto-String */
	char protobuf[256];
	unsigned short port;
	int n;
	struct servent* srv;

	if (len < 3)
	    return ctl_error(EINVAL, rbuf, rsize);
	port = get_int16(buf);
	port = sock_htons(port);
	buf += 2;
	n = get_int8(buf); buf++; len -= 3;
	if (n > len)
	    return ctl_error(EINVAL, rbuf, rsize);
	memcpy(protobuf, buf, n);
	protobuf[n] = '\0';
	if ((srv = sock_getservbyport(port, protobuf)) == NULL)
	    return ctl_error(EINVAL, rbuf, rsize);
	len = strlen(srv->s_name);
	return ctl_reply(INET_REP_OK, srv->s_name, len, rbuf, rsize);
    }
	
    default:
	return ctl_xerror(EXBADPORT, rbuf, rsize);
    }
}

/* update statistics on output packets */
static void inet_output_count(inet_descriptor* desc, ErlDrvSizeT len)
{
    unsigned long n = desc->send_cnt + 1;
    double avg = desc->send_avg;

    desc->send_oct += len;
    if (n == 0) /* WRAP, use old avg as input to a new sequence */
	n = 1;
    desc->send_avg += (len - avg) / n;
    if (len > desc->send_max)
	desc->send_max = len;
    desc->send_cnt = n;
}

/* update statistics on input packets */
static void inet_input_count(inet_descriptor* desc, ErlDrvSizeT len)
{
    unsigned long n = desc->recv_cnt + 1;
    double avg = desc->recv_avg;
    double dvi;

    desc->recv_oct += len;
    if (n == 0) /* WRAP */
	n = 1;

    /* average packet length */
    avg = avg + (len - avg) / n;
    desc->recv_avg = avg;

    if (len > desc->recv_max)
	desc->recv_max = len;

    /* average deviation from average packet length */
    dvi = desc->recv_dvi;
    desc->recv_dvi = dvi + ((len - avg) - dvi) / n;
    desc->recv_cnt = n;
}

/*----------------------------------------------------------------------------

   TCP

-----------------------------------------------------------------------------*/

/*
** Set new size on buffer, used when packet size is determined
** and the buffer is to small.
** buffer must have a size of at least len bytes (counting from ptr_start!)
*/
static int tcp_expand_buffer(tcp_descriptor* desc, int len)
{
    ErlDrvBinary* bin;
    int offs1;
    int offs2;
    int used = desc->i_ptr_start - desc->i_buf->orig_bytes;
    int ulen = used + len;

    if (desc->i_bufsz >= ulen) /* packet will fit */
	return 0;
    else if (desc->i_buf->orig_size >= ulen) { /* buffer is large enough */
	desc->i_bufsz = ulen;  /* set "virtual" size */
	return 0;
    }

    DEBUGF("tcp_expand_buffer(%ld): s=%d, from %ld to %d",
	    (long)desc->inet.port, desc->inet.s, desc->i_buf->orig_size, ulen);

    offs1 = desc->i_ptr_start - desc->i_buf->orig_bytes;
    offs2 = desc->i_ptr - desc->i_ptr_start;

    if ((bin = driver_realloc_binary(desc->i_buf, ulen)) == NULL)
	return -1;

    desc->i_buf = bin;
    desc->i_ptr_start = bin->orig_bytes + offs1;
    desc->i_ptr       = desc->i_ptr_start + offs2;
    desc->i_bufsz     = ulen;
    return 0;
}

/* push data into i_buf  */
static int tcp_push_buffer(tcp_descriptor* desc, char* buf, int len)
{
    ErlDrvBinary* bin;

    if (desc->i_buf == NULL) {
	bin = alloc_buffer(len);
	memcpy(bin->orig_bytes, buf, len);
	desc->i_buf = bin;
	desc->i_bufsz = len;
	desc->i_ptr_start = desc->i_buf->orig_bytes;
	desc->i_ptr = desc->i_ptr_start + len;
    }
    else {
	char* start =  desc->i_buf->orig_bytes;
	int sz_before = desc->i_ptr_start - start;
	int sz_filled = desc->i_ptr - desc->i_ptr_start;
	
	if (len <= sz_before) {
	    memcpy(desc->i_ptr_start - len, buf, len);
	    desc->i_ptr_start -= len;
	}
	else {
	    bin = alloc_buffer(desc->i_bufsz+len);
	    memcpy(bin->orig_bytes, buf, len);
	    memcpy(bin->orig_bytes+len, desc->i_ptr_start, sz_filled);
	    free_buffer(desc->i_buf);
	    desc->i_bufsz += len;
	    desc->i_buf = bin;
	    desc->i_ptr_start = bin->orig_bytes;
	    desc->i_ptr = desc->i_ptr_start + sz_filled + len;
	}
    }
    desc->i_remain = 0;	
    return 0;
}

/* clear CURRENT input buffer */
static void tcp_clear_input(tcp_descriptor* desc)
{
    if (desc->i_buf != NULL)
	free_buffer(desc->i_buf);
    desc->i_buf = NULL;
    desc->i_remain    = 0;
    desc->i_ptr       = NULL;
    desc->i_ptr_start = NULL;
    desc->i_bufsz     = 0;
}

/* clear QUEUED output */
static void tcp_clear_output(tcp_descriptor* desc)
{
    ErlDrvPort ix  = desc->inet.port;
    ErlDrvSizeT qsz = driver_sizeq(ix);

    driver_deq(ix, qsz);
    send_empty_out_q_msgs(INETP(desc));
}


/* Move data so that ptr_start point at buf->orig_bytes */
static void tcp_restart_input(tcp_descriptor* desc)
{
    if (desc->i_ptr_start != desc->i_buf->orig_bytes) {
	int n = desc->i_ptr - desc->i_ptr_start;

	DEBUGF("tcp_restart_input: move %d bytes", n);
	memmove(desc->i_buf->orig_bytes, desc->i_ptr_start, n);
	desc->i_ptr_start = desc->i_buf->orig_bytes;
	desc->i_ptr = desc->i_ptr_start + n;
    }
}


static int afunix_init(void)
{
    DEBUGF("afunix_init() {}");
    inet_init();
    return 0;
}

static void afunix_finish(void)
{
    // nothing right now
}

/* initialize the TCP descriptor */

static ErlDrvData afunix_start(ErlDrvPort port, char* args)
{
#ifdef ERL_DRV_BUSY_MSGQ_LIM_MIN
    ErlDrvSizeT q_low, q_high;
#endif
    tcp_descriptor* desc;
    DEBUGF("afunix_start(%ld) {", (long)port);

    desc = (tcp_descriptor*)
	inet_start(port, sizeof(tcp_descriptor), 0);
    if (desc == NULL)
	return ERL_DRV_ERROR_ERRNO;
    desc->high = INET_HIGH_WATERMARK;
    desc->low  = INET_LOW_WATERMARK;
#ifdef ERL_DRV_BUSY_MSGQ_LIM_MIN
    q_high = INET_HIGH_MSGQ_WATERMARK;
    q_low = INET_LOW_MSGQ_WATERMARK;
    if (q_low < ERL_DRV_BUSY_MSGQ_LIM_MIN)
	q_low = ERL_DRV_BUSY_MSGQ_LIM_MIN;
    else if (q_low > ERL_DRV_BUSY_MSGQ_LIM_MAX)
	q_low = ERL_DRV_BUSY_MSGQ_LIM_MAX;
    if (q_high < ERL_DRV_BUSY_MSGQ_LIM_MIN)
	q_high = ERL_DRV_BUSY_MSGQ_LIM_MIN;
    else if (q_high > ERL_DRV_BUSY_MSGQ_LIM_MAX)
	q_high = ERL_DRV_BUSY_MSGQ_LIM_MAX;
    erl_drv_busy_msgq_limits(port, &q_low, &q_high);
#endif
    desc->send_timeout = INET_INFINITY;
    desc->send_timeout_close = 0;
    desc->busy_on_send = 0;
    desc->i_buf = NULL;
    desc->i_ptr = NULL;
    desc->i_ptr_start = NULL;
    desc->i_remain = 0;
    desc->i_bufsz = 0;
    desc->tcp_add_flags = 0;
    desc->http_state = 0;
    desc->mtd = NULL;
    desc->multi_first = desc->multi_last = NULL;
    DEBUGF("afunix_start(%ld) }", (long)port);
    return (ErlDrvData) desc;
}

#if (ERL_DRV_EXTENDED_MAJOR_VERSION > 2) || ((ERL_DRV_EXTENDED_MAJOR_VERSION == 2) && (ERL_DRV_EXTENDED_MINOR_VERSION >= 1))
#define CREATE_CAST(ptr) (ptr)
#elif ((ERL_DRV_EXTENDED_MAJOR_VERSION == 2) && (ERL_DRV_EXTENDED_MINOR_VERSION == 0))
#define CREATE_CAST(ptr) (((long)(ptr)))
#else
#define CREATE_CAST(ptr) ((ErlDrvPort)((long)(ptr)))
#endif


/* Copy a descriptor, by creating a new port with same settings
 * as the descriptor desc.
 * return NULL on error (SYSTEM_LIMIT no ports avail)
 */
static tcp_descriptor* afunix_copy(tcp_descriptor* desc,SOCKET s,
				     ErlDrvTermData owner, int* err)
{
#ifdef ERL_DRV_BUSY_MSGQ_LIM_MIN
    ErlDrvSizeT q_low, q_high;
#endif
    ErlDrvPort port = desc->inet.port;
    tcp_descriptor* copy_desc;

    copy_desc = (tcp_descriptor*) afunix_start(port, NULL);

    /* Setup event if needed */
    if ((copy_desc->inet.s = s) != INVALID_SOCKET) {
	if ((copy_desc->inet.event = sock_create_event(INETP(copy_desc))) ==
	    INVALID_EVENT) {
	    *err = sock_errno();
	    FREE(copy_desc);
	    return NULL;
	}
    }

    /* Some flags must be inherited at this point */
    copy_desc->inet.mode     = desc->inet.mode;
    copy_desc->inet.exitf    = desc->inet.exitf;
    copy_desc->inet.deliver  = desc->inet.deliver;
    copy_desc->inet.htype    = desc->inet.htype; 
    copy_desc->inet.psize    = desc->inet.psize; 
    copy_desc->inet.stype    = desc->inet.stype;
    copy_desc->inet.sfamily  = desc->inet.sfamily;
    copy_desc->inet.hsz      = desc->inet.hsz;
    copy_desc->inet.bufsz    = desc->inet.bufsz;
    copy_desc->high          = desc->high;
    copy_desc->low           = desc->low;
    copy_desc->send_timeout  = desc->send_timeout;
    copy_desc->send_timeout_close = desc->send_timeout_close;
    
    /* The new port will be linked and connected to the original caller */
    port =  (ErlDrvPort) driver_create_port(CREATE_CAST(port), owner, "afunix_drv", (ErlDrvData) ((long)copy_desc));
    if ((long)port == -1) {
	*err = INET_ERRNO_SYSTEM_LIMIT;
	FREE(copy_desc);
	return NULL;
    }

    /* Read busy msgq limits of parent */
#ifdef ERL_DRV_BUSY_MSGQ_LIM_MIN
    q_low = q_high = ERL_DRV_BUSY_MSGQ_READ_ONLY;
    erl_drv_busy_msgq_limits(desc->inet.port, &q_low, &q_high);
    /* Write same busy msgq limits to child */
    erl_drv_busy_msgq_limits(port, &q_low, &q_high);
#endif

    copy_desc->inet.port = port;
    copy_desc->inet.dport = driver_mk_port(port);
    *err = 0;
    return copy_desc;
}

/*
** Check Special cases:
** 1. we are a listener doing nb accept -> report error on accept !
** 2. we are doing accept -> restore listener state
*/
static void tcp_close_check(tcp_descriptor* desc)
{
    /* XXX:PaN - multiple clients to handle! */
    if (desc->inet.state == INET_STATE_ACCEPTING) {
	inet_async_op *this_op = desc->inet.opt;
	sock_select(INETP(desc), FD_ACCEPT, 0);
	desc->inet.state = INET_STATE_LISTENING;
	if (this_op != NULL) {
	    driver_demonitor_process(desc->inet.port, &(this_op->monitor));
	}
	async_error_am(INETP(desc), am_closed);
    } 
    else if (desc->inet.state == INET_STATE_MULTI_ACCEPTING) {
	int id,req;
	ErlDrvTermData caller;
	ErlDrvMonitor monitor;

	sock_select(INETP(desc), FD_ACCEPT, 0);
	desc->inet.state = INET_STATE_LISTENING;
	while (deq_multi_op(desc,&id,&req,&caller,NULL,&monitor) == 0) {
	    driver_demonitor_process(desc->inet.port, &monitor);
	    send_async_error(&desc->inet, id, caller, am_closed);
	}
	clean_multi_timers(&(desc->mtd), desc->inet.port);
    }

    else if (desc->inet.state == INET_STATE_CONNECTING) {
	async_error_am(INETP(desc), am_closed);
    }
    else if (desc->inet.state == INET_STATE_CONNECTED) {
	async_error_am_all(INETP(desc), am_closed);
    }
}

/*
** Cleanup & Free
*/
static void afunix_stop(ErlDrvData e)
{
    tcp_descriptor* desc = (tcp_descriptor*)e;
    DEBUGF("afunix_stop(%ld) {s=%d", 
	    (long)desc->inet.port, desc->inet.s);
    tcp_close_check(desc);
    /* free input buffer & output buffer */
    if (desc->i_buf != NULL)
	release_buffer(desc->i_buf);
    desc->i_buf = NULL; /* net_mess2 may call this function recursively when 
			   faulty messages arrive on dist ports*/
    DEBUGF("afunix_stop(%ld) }", (long)desc->inet.port);
    inet_stop(INETP(desc));
}


    

/* TCP requests from Erlang */
static ErlDrvSSizeT afunix_ctl(ErlDrvData e, unsigned int cmd,
				 char* buf, ErlDrvSizeT len,
				 char** rbuf, ErlDrvSizeT rsize)
{
    tcp_descriptor* desc = (tcp_descriptor*)e;

    switch(cmd) {
    case INET_REQ_OPEN: { /* open socket and return internal index */
	int domain;
	DEBUGF("afunix_ctl(%ld): OPEN", (long)desc->inet.port);
	if (len != 2) return ctl_error(EINVAL, rbuf, rsize);
	switch(buf[0]) {
	case INET_AF_UNIX:
	    domain = AF_UNIX;
	    break;
	case INET_AF_INET:
	    domain = AF_INET;
	    break;
#if defined(HAVE_IN6) && defined(AF_INET6)
	case INET_AF_INET6:
	    domain = AF_INET6;
	    break;
#else
	case INET_AF_INET6:
	    return ctl_xerror("eafnosupport", rbuf, rsize);
	    break;
#endif
	default:
	    return ctl_error(EINVAL, rbuf, rsize);
	}
	if (buf[1] != INET_TYPE_STREAM) 
	    return ctl_error(EINVAL, rbuf, rsize);
	return inet_ctl_open(INETP(desc), domain, SOCK_STREAM, rbuf, rsize);
	break;
    }

    case INET_REQ_FDOPEN: {  /* pass in an open socket */
	int domain;
	DEBUGF("afunix_ctl(%ld): FDOPEN", (long)desc->inet.port);
	if (len != 6) return ctl_error(EINVAL, rbuf, rsize);
	switch(buf[0]) {
	case INET_AF_UNIX:
	    domain = AF_UNIX;
	    break;	    
	case INET_AF_INET:
	    domain = AF_INET;
	    break;
#if defined(HAVE_IN6) && defined(AF_INET6)
	case INET_AF_INET6:
	    domain = AF_INET6;
	    break;
#else
	case INET_AF_INET6:
	    return ctl_xerror("eafnosupport", rbuf, rsize);
	    break;
#endif
	default:
	    return ctl_error(EINVAL, rbuf, rsize);
	}
	if (buf[1] != INET_TYPE_STREAM) return ctl_error(EINVAL, rbuf, rsize);
	return inet_ctl_fdopen(INETP(desc), domain, SOCK_STREAM,
			       (SOCKET) get_int32(buf+2), rbuf, rsize);
	break;
    }

    case INET_REQ_LISTEN: { /* argument backlog */

	int backlog;
	DEBUGF("afunix_ctl(%ld): LISTEN", (long)desc->inet.port); 
	if (desc->inet.state == INET_STATE_CLOSED)
	    return ctl_xerror(EXBADPORT, rbuf, rsize);
	if (!IS_OPEN(INETP(desc)))
	    return ctl_xerror(EXBADPORT, rbuf, rsize);
	if (!IS_BOUND(INETP(desc)))
	    return ctl_xerror(EXBADSEQ, rbuf, rsize);
	if (len != 2)
	    return ctl_error(EINVAL, rbuf, rsize);
	backlog = get_int16(buf);
	if (IS_SOCKET_ERROR(sock_listen(desc->inet.s, backlog)))
	    return ctl_error(sock_errno(), rbuf, rsize);
	desc->inet.state = INET_STATE_LISTENING;
	return ctl_reply(INET_REP_OK, NULL, 0, rbuf, rsize);
    }


    case INET_REQ_CONNECT: {   /* do async connect */
	int code;
	char tbuf[2];
	unsigned timeout;

	DEBUGF("afunix_ctl(%ld): CONNECT", (long)desc->inet.port); 
	/* INPUT: Timeout(4), Port(2), Address(N) */

	if (!IS_OPEN(INETP(desc)))
	    return ctl_xerror(EXBADPORT, rbuf, rsize);
	if (IS_CONNECTED(INETP(desc)))
	    return ctl_error(EISCONN, rbuf, rsize);
//	if (!IS_BOUND(INETP(desc)))
//	    return ctl_xerror(EXBADSEQ, rbuf, rsize);
	if (IS_CONNECTING(INETP(desc)))
	    return ctl_error(EINVAL, rbuf, rsize);
	if (len < 6)
	    return ctl_error(EINVAL, rbuf, rsize);
	timeout = get_int32(buf);
	buf += 4;
	len -= 4;
	if (inet_set_address(desc->inet.sfamily, &desc->inet.remote,
			     buf, &len) == NULL)
	    return ctl_error(EINVAL, rbuf, rsize);
	
	code = sock_connect(desc->inet.s, 
			    (struct sockaddr*) &desc->inet.remote, len);
	if (IS_SOCKET_ERROR(code) &&
		((sock_errno() == ERRNO_BLOCK) ||  /* Winsock2 */
		 (sock_errno() == EINPROGRESS))) {	/* Unix & OSE!! */
          sock_select(INETP(desc), FD_CONNECT, 1);
	    desc->inet.state = INET_STATE_CONNECTING;
	    if (timeout != INET_INFINITY)
		driver_set_timer(desc->inet.port, timeout);
	    enq_async(INETP(desc), tbuf, INET_REQ_CONNECT);
	}
	else if (code == 0) { /* ok we are connected */
	    desc->inet.state = INET_STATE_CONNECTED;
	    if (desc->inet.active)
		sock_select(INETP(desc), (FD_READ|FD_CLOSE), 1);
	    enq_async(INETP(desc), tbuf, INET_REQ_CONNECT);
	    async_ok(INETP(desc));
	}
	else {
	    return ctl_error(sock_errno(), rbuf, rsize);
	}
	return ctl_reply(INET_REP_OK, tbuf, 2, rbuf, rsize);
    }

    case INET_REQ_ACCEPT: {  /* do async accept */
	char tbuf[2];
	unsigned timeout;
	inet_address remote;
	unsigned int n;
	SOCKET s;

	DEBUGF("afunix_ctl(%ld): ACCEPT", (long)desc->inet.port); 
	/* INPUT: Timeout(4) */

	if ((desc->inet.state != INET_STATE_LISTENING && desc->inet.state != INET_STATE_ACCEPTING &&
	     desc->inet.state != INET_STATE_MULTI_ACCEPTING) || len != 4) {
	    return ctl_error(EINVAL, rbuf, rsize);
	}

	timeout = get_int32(buf);

	if (desc->inet.state == INET_STATE_ACCEPTING) {
	    unsigned long time_left = 0;
	    int oid = 0;
	    ErlDrvTermData ocaller = ERL_DRV_NIL;
	    int oreq = 0;
	    unsigned otimeout = 0;
	    ErlDrvTermData caller = driver_caller(desc->inet.port);
	    MultiTimerData *mtd = NULL,*omtd = NULL;
	    ErlDrvMonitor monitor, omonitor;


	    if (driver_monitor_process(desc->inet.port, caller ,&monitor) != 0) { 
		return ctl_xerror("noproc", rbuf, rsize);
	    }
	    deq_async_w_tmo(INETP(desc),&oid,&ocaller,&oreq,&otimeout,&omonitor);
	    if (otimeout != INET_INFINITY) {
		driver_read_timer(desc->inet.port, &time_left);
		driver_cancel_timer(desc->inet.port);
		if (time_left <= 0) {
		    time_left = 1;
		}
		omtd = add_multi_timer(&(desc->mtd), desc->inet.port, ocaller, 
				       time_left, &afunix_multi_timeout);
	    }
	    enq_old_multi_op(desc, oid, oreq, ocaller, omtd, &omonitor);
	    if (timeout != INET_INFINITY) {
		mtd = add_multi_timer(&(desc->mtd), desc->inet.port, caller, 
				      timeout, &afunix_multi_timeout);
	    }
	    enq_multi_op(desc, tbuf, INET_REQ_ACCEPT, caller, mtd, &monitor);
	    desc->inet.state = INET_STATE_MULTI_ACCEPTING;
	    return ctl_reply(INET_REP_OK, tbuf, 2, rbuf, rsize);
	} else if (desc->inet.state == INET_STATE_MULTI_ACCEPTING) {
	    ErlDrvTermData caller = driver_caller(desc->inet.port);
	    MultiTimerData *mtd = NULL;
	    ErlDrvMonitor monitor;

	    if (driver_monitor_process(desc->inet.port, caller ,&monitor) != 0) { 
		return ctl_xerror("noproc", rbuf, rsize);
	    }
	    if (timeout != INET_INFINITY) {
		mtd = add_multi_timer(&(desc->mtd), desc->inet.port, caller, 
				      timeout, &afunix_multi_timeout);
	    }
	    enq_multi_op(desc, tbuf, INET_REQ_ACCEPT, caller, mtd, &monitor);
	    return ctl_reply(INET_REP_OK, tbuf, 2, rbuf, rsize);
 	} else {
	    n = sizeof(desc->inet.remote);
	    s = sock_accept(desc->inet.s, (struct sockaddr*) &remote, &n);
	    if (s == INVALID_SOCKET) {
		if (sock_errno() == ERRNO_BLOCK) {
		    ErlDrvMonitor monitor;
		    if (driver_monitor_process(desc->inet.port, driver_caller(desc->inet.port),
					       &monitor) != 0) { 
			return ctl_xerror("noproc", rbuf, rsize);
		    }
		    enq_async_w_tmo(INETP(desc), tbuf, INET_REQ_ACCEPT, timeout, &monitor);
		    desc->inet.state = INET_STATE_ACCEPTING;
		    sock_select(INETP(desc),FD_ACCEPT,1);
		    if (timeout != INET_INFINITY) {
			driver_set_timer(desc->inet.port, timeout);
		    }
		} else {
		    return ctl_error(sock_errno(), rbuf, rsize);
		}
	    } else {
		ErlDrvTermData caller = driver_caller(desc->inet.port);
		tcp_descriptor* accept_desc;
		int err;
		
		if ((accept_desc = afunix_copy(desc,s,caller,&err)) == NULL) {
		    sock_close(s);
		    return ctl_error(err, rbuf, rsize);
		}
		/* FIXME: may MUST lock access_port 
		 * 1 - Port is accessible via the erlang:ports()
		 * 2 - Port is accessible via callers process_info(links)
		 */
		accept_desc->inet.remote = remote;
		SET_NONBLOCKING(accept_desc->inet.s);
#ifdef __WIN32__
		driver_select(accept_desc->inet.port, accept_desc->inet.event, 
			      ERL_DRV_READ, 1);
#endif
		accept_desc->inet.state = INET_STATE_CONNECTED;
		enq_async(INETP(desc), tbuf, INET_REQ_ACCEPT);
		async_ok_port(INETP(desc), accept_desc->inet.dport);
	    }
	    return ctl_reply(INET_REP_OK, tbuf, 2, rbuf, rsize);
	}
    }
    case INET_REQ_CLOSE:
	DEBUGF("afunix_ctl(%ld): CLOSE", (long)desc->inet.port); 
	tcp_close_check(desc);
	erl_inet_close(INETP(desc));
	return ctl_reply(INET_REP_OK, NULL, 0, rbuf, rsize);


    case TCP_REQ_RECV: {
	unsigned timeout;
	char tbuf[2];
	int n;

	DEBUGF("afunix_ctl(%ld): RECV", (long)desc->inet.port); 
	/* INPUT: Timeout(4),  Length(4) */
	if (!IS_CONNECTED(INETP(desc))) {
	    if (desc->tcp_add_flags & TCP_ADDF_DELAYED_CLOSE_RECV) {
		desc->tcp_add_flags &= ~(TCP_ADDF_DELAYED_CLOSE_RECV|
					 TCP_ADDF_DELAYED_CLOSE_SEND);
		return ctl_reply(INET_REP_ERROR, "closed", 6, rbuf, rsize);
	    }
	    return ctl_error(ENOTCONN, rbuf, rsize);
	}
	if (desc->inet.active || (len != 8))
	    return ctl_error(EINVAL, rbuf, rsize);
	timeout = get_int32(buf);
	buf += 4;
	n = get_int32(buf);
	DEBUGF("afunix_ctl(%ld) timeout = %d, n = %d",
		(long)desc->inet.port,timeout,n);
	if ((desc->inet.htype != TCP_PB_RAW) && (n != 0))
	    return ctl_error(EINVAL, rbuf, rsize);
	if (n > TCP_MAX_PACKET_SIZE)
	    return ctl_error(ENOMEM, rbuf, rsize);
	if (enq_async(INETP(desc), tbuf, TCP_REQ_RECV) < 0)
	    return ctl_error(EALREADY, rbuf, rsize);

	if (INETP(desc)->is_ignored || tcp_recv(desc, n) == 0) {
	    if (timeout == 0)
		async_error_am(INETP(desc), am_timeout);
	    else {
		if (timeout != INET_INFINITY)
		    driver_set_timer(desc->inet.port, timeout);
		if (!INETP(desc)->is_ignored)
		    sock_select(INETP(desc),(FD_READ|FD_CLOSE),1);
	    }
	}
	return ctl_reply(INET_REP_OK, tbuf, 2, rbuf, rsize);
    }

    case TCP_REQ_UNRECV: {
	DEBUGF("afunix_ctl(%ld): UNRECV", (long)desc->inet.port); 
	if (!IS_CONNECTED(INETP(desc)))
   	    return ctl_error(ENOTCONN, rbuf, rsize);
	tcp_push_buffer(desc, buf, len);
	if (desc->inet.active)
	    tcp_deliver(desc, 0);
	return ctl_reply(INET_REP_OK, NULL, 0, rbuf, rsize);
    }
    case TCP_REQ_SHUTDOWN: {
	int how;
	DEBUGF("afunix_ctl(%ld): SHUTDOWN", (long)desc->inet.port); 
	if (!IS_CONNECTED(INETP(desc))) {
	    return ctl_error(ENOTCONN, rbuf, rsize);
	}
	if (len != 1) {
	    return ctl_error(EINVAL, rbuf, rsize);
	}
	how = buf[0];
	if (sock_shutdown(INETP(desc)->s, how) == 0) {
	    return ctl_reply(INET_REP_OK, NULL, 0, rbuf, rsize);
	} else {
	    return ctl_error(sock_errno(), rbuf, rsize);
	}
    }
    default:
	DEBUGF("afunix_ctl(%ld): %u", (long)desc->inet.port, cmd); 
	return inet_ctl(INETP(desc), cmd, buf, len, rbuf, rsize);
    }

}

/*
** afunix_timeout:
** called when timer expire:
** TCP socket may be:
**
** a)  receiving   -- deselect
** b)  connecting  -- close socket
** c)  accepting   -- reset listener
**
*/

static void afunix_timeout(ErlDrvData e)
{
    tcp_descriptor* desc = (tcp_descriptor*)e;
    int state = desc->inet.state;

    DEBUGF("afunix_timeout(%ld) {s=%d", 
	    (long)desc->inet.port, desc->inet.s); 
    if ((state & INET_F_MULTI_CLIENT)) { /* Multi-client always means multi-timers */
	fire_multi_timers(&(desc->mtd), desc->inet.port, e);
    } else if ((state & INET_STATE_CONNECTED) == INET_STATE_CONNECTED) {
	if (desc->busy_on_send) {
	    desc->inet.caller = desc->inet.busy_caller;
	    desc->inet.state &= ~INET_F_BUSY;
	    desc->busy_on_send = 0;
	    set_busy_port(desc->inet.port, 0);
	    inet_reply_error_am(INETP(desc), am_timeout);
	    if (desc->send_timeout_close) {
		erl_inet_close(INETP(desc));
	    }
	}
	else {
	    /* assume recv timeout */
	    sock_select(INETP(desc),(FD_READ|FD_CLOSE),0);
	    desc->i_remain = 0;
	    async_error_am(INETP(desc), am_timeout);
	}
    }
    else if ((state & INET_STATE_CONNECTING) == INET_STATE_CONNECTING) {
	/* assume connect timeout */
	/* close the socket since it's not usable (see man pages) */
	erl_inet_close(INETP(desc));
	async_error_am(INETP(desc), am_timeout);
    }
    else if ((state & INET_STATE_ACCEPTING) == INET_STATE_ACCEPTING) {
	inet_async_op *this_op = desc->inet.opt;
	/* timer is set on accept */
	sock_select(INETP(desc), FD_ACCEPT, 0);
	if (this_op != NULL) {
	    driver_demonitor_process(desc->inet.port, &(this_op->monitor));
	}
	desc->inet.state = INET_STATE_LISTENING;
	async_error_am(INETP(desc), am_timeout);
    }
    DEBUGF("afunix_timeout(%ld) }", (long)desc->inet.port); 
}

static void afunix_multi_timeout(ErlDrvData e, ErlDrvTermData caller)
{
    tcp_descriptor* desc = (tcp_descriptor*)e;
    int id,req;
    ErlDrvMonitor monitor;

    if (remove_multi_op(desc, &id, &req, caller, NULL, &monitor) != 0) {
	return;
    }
    driver_demonitor_process(desc->inet.port, &monitor);
    if (desc->multi_first == NULL) {
	sock_select(INETP(desc),FD_ACCEPT,0);
	desc->inet.state = INET_STATE_LISTENING; /* restore state */
    }
    send_async_error(&desc->inet, id, caller, am_timeout);
}
	
/*
** command:
**   output on a socket only !
**   a reply code will be sent to connected (caller later)
**   {inet_reply, S, Status}
** NOTE! normal sockets use the the afunix_commandv
** but distribution still uses the afunix_command!!
*/

static void afunix_command(ErlDrvData e, char *buf, ErlDrvSizeT len)
{
    tcp_descriptor* desc = (tcp_descriptor*)e;
    desc->inet.caller = driver_caller(desc->inet.port);

    DEBUGF("afunix_command(%ld) {s=%d", 
	    (long)desc->inet.port, desc->inet.s); 
    if (!IS_CONNECTED(INETP(desc)))
	inet_reply_error(INETP(desc), ENOTCONN);
    else if (tcp_send(desc, buf, len) == 0)
	inet_reply_ok(INETP(desc));
    DEBUGF("afunix_command(%ld) }", (long)desc->inet.port); 
}


static void afunix_commandv(ErlDrvData e, ErlIOVec* ev)
{
    tcp_descriptor* desc = (tcp_descriptor*)e;
    desc->inet.caller = driver_caller(desc->inet.port);

    DEBUGF("afunix_commanv(%ld) {s=%d", 
	    (long)desc->inet.port, desc->inet.s); 
    if (!IS_CONNECTED(INETP(desc))) {
	if (desc->tcp_add_flags & TCP_ADDF_DELAYED_CLOSE_SEND) {
	    desc->tcp_add_flags &= ~TCP_ADDF_DELAYED_CLOSE_SEND;
	    inet_reply_error_am(INETP(desc), am_closed);
	}
	else
	    inet_reply_error(INETP(desc), ENOTCONN);
    }
    else if (tcp_sendv(desc, ev) == 0)
	inet_reply_ok(INETP(desc));
    DEBUGF("afunix_commandv(%ld) }", (long)desc->inet.port); 
}
    
static void afunix_flush(ErlDrvData e)
{
    tcp_descriptor* desc = (tcp_descriptor*)e;
    if (!(desc->inet.event_mask & FD_WRITE)) {
	/* Discard send queue to avoid hanging port (OTP-7615) */
	tcp_clear_output(desc);
    }
}

static void afunix_process_exit(ErlDrvData e, ErlDrvMonitor *monitorp) 
{
    tcp_descriptor* desc = (tcp_descriptor*)e;
    ErlDrvTermData who = driver_get_monitored_process(desc->inet.port,monitorp);
    int state = desc->inet.state;

    if ((state & INET_STATE_MULTI_ACCEPTING) == INET_STATE_MULTI_ACCEPTING) {
	int id,req;
	MultiTimerData *timeout;
	if (remove_multi_op(desc, &id, &req, who, &timeout, NULL) != 0) {
	    return;
	}
	if (timeout != NULL) {
	    remove_multi_timer(&(desc->mtd), desc->inet.port, timeout);
	}
	if (desc->multi_first == NULL) {
	    sock_select(INETP(desc),FD_ACCEPT,0);
	    desc->inet.state = INET_STATE_LISTENING; /* restore state */
	}
    } else if ((state & INET_STATE_ACCEPTING) == INET_STATE_ACCEPTING) {
	int did,drid; 
	ErlDrvTermData dcaller;
	deq_async(INETP(desc), &did, &dcaller, &drid);
	driver_cancel_timer(desc->inet.port);
	sock_select(INETP(desc),FD_ACCEPT,0);
	desc->inet.state = INET_STATE_LISTENING; /* restore state */
    }
} 

static void inet_stop_select(ErlDrvEvent event, void* _)
{
    sock_close((SOCKET)(long)event);
}

/* The peer socket has closed, cleanup and send event */
static int tcp_recv_closed(tcp_descriptor* desc)
{
    long port = (long) desc->inet.port; /* Used after driver_exit() */

    DEBUGF("tcp_recv_closed(%ld): s=%d, in %s, line %d",
	   port, desc->inet.s, __FILE__, __LINE__);
    if (IS_BUSY(INETP(desc))) {
	/* A send is blocked */
	desc->inet.caller = desc->inet.busy_caller;
	tcp_clear_output(desc);
	if (desc->busy_on_send) {
	    driver_cancel_timer(desc->inet.port);
	    desc->busy_on_send = 0;
	    DEBUGF("tcp_recv_closed(%ld): busy on send", port);
	}
	desc->inet.state &= ~INET_F_BUSY;
	set_busy_port(desc->inet.port, 0);
	inet_reply_error_am(INETP(desc), am_closed);
	DEBUGF("tcp_recv_closed(%ld): busy reply 'closed'", port);
    }
    if (!desc->inet.active) {
	/* We must cancel any timer here ! */
	driver_cancel_timer(desc->inet.port);
	/* passive mode do not terminate port ! */
	tcp_clear_input(desc);
	if (desc->inet.exitf) {
	    tcp_clear_output(desc);
	    desc_close(INETP(desc));
	} else {
	    desc_close_read(INETP(desc));
	}
	async_error_am_all(INETP(desc), am_closed);
	/* next time EXBADSEQ will be delivered  */
	DEBUGF("tcp_recv_closed(%ld): passive reply all 'closed'", port);
    } else {
	tcp_clear_input(desc);
	tcp_closed_message(desc);
	if (desc->inet.exitf) {
	    driver_exit(desc->inet.port, 0);
	} else {
	    desc_close_read(INETP(desc));
	}
	DEBUGF("tcp_recv_closed(%ld): active close", port);
    }
    DEBUGF("tcp_recv_closed(%ld): done", port);
    return -1;
}


/* We have a read error determine the action */
static int tcp_recv_error(tcp_descriptor* desc, int err)
{
    if (err != ERRNO_BLOCK) {
	if (IS_BUSY(INETP(desc))) {
	    /* A send is blocked */
	    desc->inet.caller = desc->inet.busy_caller;
	    tcp_clear_output(desc);
	    if (desc->busy_on_send) {
		driver_cancel_timer(desc->inet.port);
		desc->busy_on_send = 0;
	    }
	    desc->inet.state &= ~INET_F_BUSY;
	    set_busy_port(desc->inet.port, 0);
	    inet_reply_error_am(INETP(desc), am_closed);
	}
	if (!desc->inet.active) {
	    /* We must cancel any timer here ! */
	    driver_cancel_timer(desc->inet.port);
	    tcp_clear_input(desc);
	    if (desc->inet.exitf) {
		desc_close(INETP(desc));
	    } else {
		desc_close_read(INETP(desc));
	    }
	    async_error_am_all(INETP(desc), error_atom(err));
	} else {
	    tcp_clear_input(desc);
	    tcp_error_message(desc, err); /* first error */
	    tcp_closed_message(desc);     /* then closed */
	    if (desc->inet.exitf)
		driver_exit(desc->inet.port, err);
	    else
		desc_close(INETP(desc));
	}
	return -1;
    }
    return 0;
}



/*
** Calculate number of bytes that remain to read before deliver
** Assume buf, ptr_start, ptr has been setup
**
** return  > 0 if more to read
**         = 0 if holding complete packet
**         < 0 on error
**
** if return value == 0 then *len will hold the length of the first packet
**    return value > 0 then if *len == 0 then value means upperbound
**                             *len > 0  then value means exact
**
*/
static int tcp_remain(tcp_descriptor* desc, int* len)
{
    char* ptr = desc->i_ptr_start;
    int nfill = (desc->i_ptr - desc->i_buf->orig_bytes); /* filled */
    int nsz   = desc->i_bufsz - nfill;                   /* remain */
    int n = desc->i_ptr - ptr;  /* number of bytes read */
    int tlen;

    DEBUGF("tcp_remain(%ld): s=%d, n=%d, nfill=%d nsz=%d", 
	    (long)desc->inet.port, desc->inet.s, n, nfill, nsz);

    DEBUGF("htype=%d, ptr=%p, n=%d, psize=%u, tlen=%u, delimiter=%d *state=%d",
	   desc->inet.htype, ptr, n, 
	   desc->inet.psize, desc->i_bufsz,
	   desc->inet.delimiter, desc->http_state);


    tlen = afunix_packet_get_length(desc->inet.htype, ptr, n, 
                             desc->inet.psize, desc->i_bufsz,
                             desc->inet.delimiter, &desc->http_state);
    if (tlen > 0) {
        if (tlen <= n) { /* got a packet */
            *len = tlen;
            DEBUGF(" => nothing remain packet=%d", tlen);
            return 0;
        }
        else { /* need known more */
            if (tcp_expand_buffer(desc, tlen) < 0)
                return -1;
            *len = tlen - n;
            DEBUGF(" => remain=%d", *len);
            return *len;
        }
    }
    else if (tlen == 0) { /* need unknown more */
        *len = 0;
        if (nsz == 0) {
            if (nfill == n) {
                if (desc->inet.psize != 0 && desc->inet.psize > nfill) {
                    if (tcp_expand_buffer(desc, desc->inet.psize) < 0)
                        return -1;
                    return desc->inet.psize;
                }
                else
                    goto error;
            }
            DEBUGF(" => restart more=%d", nfill - n);
            return nfill - n;
        }
        else {
            DEBUGF(" => more=%d ", nsz);
            return nsz;
        }	    
    }

error:
    DEBUGF(" => packet error");
    return -1;
}

/*
** Deliver all packets ready 
** if len == 0 then check start with a check for ready packet
*/
static int tcp_deliver(tcp_descriptor* desc, int len)
{
    int count = 0;
    int n;

    /* Poll for ready packet */
    if (len == 0) {
	/* empty buffer or waiting for more input */
	if ((desc->i_buf == NULL) || (desc->i_remain > 0))
	    return count;
	if ((n = tcp_remain(desc, &len)) != 0) {
	    if (n < 0) /* packet error */
		return n;
	    if (len > 0)  /* more data pending */
		desc->i_remain = len;
	    return count;
	}
    }

    while (len > 0) {
	int code;

	inet_input_count(INETP(desc), len);

	/* deliver binary? */
	if (len*4 >= desc->i_buf->orig_size*3) { /* >=75% */
	    code = tcp_reply_binary_data(desc, desc->i_buf,
					 (desc->i_ptr_start -
					  desc->i_buf->orig_bytes),
					 len);
	    if (code < 0)
		return code;

	    /* something after? */
	    if (desc->i_ptr_start + len == desc->i_ptr) { /* no */
		tcp_clear_input(desc);
	    }
	    else { /* move trail to beginning of a new buffer */
		ErlDrvBinary* bin = alloc_buffer(desc->i_bufsz);
		char* ptr_end = desc->i_ptr_start + len;
		int sz = desc->i_ptr - ptr_end;

		memcpy(bin->orig_bytes, ptr_end, sz);
		free_buffer(desc->i_buf);
		desc->i_buf = bin;
		desc->i_ptr_start = desc->i_buf->orig_bytes;
		desc->i_ptr = desc->i_ptr_start + sz;
		desc->i_remain = 0;
	    }
	}
	else {
	    code = tcp_reply_data(desc, desc->i_ptr_start, len);
	    /* XXX The buffer gets thrown away on error  (code < 0)    */
	    if (code < 0)
		return code;
	    desc->i_ptr_start += len;
	    if (desc->i_ptr_start == desc->i_ptr)
		tcp_clear_input(desc);
	    else
		desc->i_remain = 0;
	}

	count++;
	len = 0;

	if (!desc->inet.active) {
	    if (!desc->busy_on_send) {
		driver_cancel_timer(desc->inet.port);
	    }
	    sock_select(INETP(desc),(FD_READ|FD_CLOSE),0);
	    if (desc->i_buf != NULL)
		tcp_restart_input(desc);
	}
	else if (desc->i_buf != NULL) {
	    if ((n = tcp_remain(desc, &len)) != 0) {
		if (n < 0) /* packet error */
		    return n;
		tcp_restart_input(desc);
		if (len > 0)
		    desc->i_remain = len;
		len = 0;
	    }
	}
    }
    return count;
}


static int tcp_recv(tcp_descriptor* desc, int request_len)
{
    int n;
    int len;
    int nread;

    if (desc->i_buf == NULL) {  /* allocte a read buffer */
	int sz = (request_len > 0) ? request_len : desc->inet.bufsz;

	if ((desc->i_buf = alloc_buffer(sz)) == NULL)
	    return -1;
	/* XXX: changing bufsz during recv SHOULD/MAY? affect 
	 * ongoing operation but is not now 
	 */
	desc->i_bufsz = sz; /* use i_bufsz not i_buf->orig_size ! */
	desc->i_ptr_start = desc->i_buf->orig_bytes;
	desc->i_ptr = desc->i_ptr_start;
	nread = sz;
	if (request_len > 0)
	    desc->i_remain = request_len;
	else
	    desc->i_remain = 0;
    }
    else if (request_len > 0) { /* we have a data in buffer and a request */
	n = desc->i_ptr - desc->i_ptr_start;
	if (n >= request_len)
	    return tcp_deliver(desc, request_len);
	else if (tcp_expand_buffer(desc, request_len) < 0)
	    return tcp_recv_error(desc, ENOMEM);
	else
	    desc->i_remain = nread = request_len - n;
    }
    else if (desc->i_remain == 0) {  /* poll remain from buffer data */
	if ((nread = tcp_remain(desc, &len)) < 0)
	    return tcp_recv_error(desc, EMSGSIZE);
	else if (nread == 0)
	    return tcp_deliver(desc, len);
	else if (len > 0)
	    desc->i_remain = len;  /* set remain */
    }
    else  /* remain already set use it */
	nread = desc->i_remain;
    
    DEBUGF("tcp_recv(%ld): s=%d about to read %d bytes...",  
	    (long)desc->inet.port, desc->inet.s, nread);

    n = sock_recv(desc->inet.s, desc->i_ptr, nread, 0);

    if (IS_SOCKET_ERROR(n)) {
	int err = sock_errno();
	if (err == ECONNRESET) {
	    DEBUGF(" => detected close (connreset)");
	    return tcp_recv_closed(desc);
	}
	if (err == ERRNO_BLOCK) {
	    DEBUGF(" => would block");
	    return 0;
	}
	else {
	    DEBUGF(" => error: %d", err);
	    return tcp_recv_error(desc, err);
	}
    }
    else if (n == 0) {
	DEBUGF("  => detected close");
	return tcp_recv_closed(desc);
    }

    DEBUGF(" => got %d bytes", n);
    desc->i_ptr += n;
    if (desc->i_remain > 0) {
	desc->i_remain -= n;
	if (desc->i_remain == 0)
	    return tcp_deliver(desc, desc->i_ptr - desc->i_ptr_start);
    }
    else {
	if ((nread = tcp_remain(desc, &len)) < 0)
	    return tcp_recv_error(desc, EMSGSIZE);
	else if (nread == 0)
	    return tcp_deliver(desc, len);
	else if (len > 0)
	    desc->i_remain = len;  /* set remain */
    }
    return 0;
}

/* socket has input:
** 1. INET_STATE_ACCEPTING  => non block accept ?
** 2. INET_STATE_CONNECTED => read input
*/
static int afunix_input(tcp_descriptor* desc, HANDLE event)
{
    int ret = 0;
    long port = (long) desc->inet.port;  /* Used after driver_exit() */

    DEBUGF("afunix_input(%ld) {s=%d", port, desc->inet.s);
    /* XXX fprintf(stderr,"afunix_input(%ld) {s=%d}",(long) desc->inet.port, desc->inet.s); */
    if (desc->inet.state == INET_STATE_ACCEPTING) {
	SOCKET s;
	unsigned int len;
	inet_address remote;
	inet_async_op *this_op = desc->inet.opt;
	
	len = sizeof(desc->inet.remote);
	s = sock_accept(desc->inet.s, (struct sockaddr*) &remote, &len);
	if (s == INVALID_SOCKET && sock_errno() == ERRNO_BLOCK) {
	    /* Just try again, no real error, just a ghost trigger from poll, 
	       keep the default return code and everything else as is */
	    goto done;
	}

	sock_select(INETP(desc),FD_ACCEPT,0);
	desc->inet.state = INET_STATE_LISTENING; /* restore state */

	if (this_op != NULL) {
	    driver_demonitor_process(desc->inet.port, &(this_op->monitor));
	}


	driver_cancel_timer(desc->inet.port); /* posssibly cancel a timer */

	if (s == INVALID_SOCKET) {
	    ret = async_error(INETP(desc), sock_errno());
	    goto done;
	}
	else {
	    ErlDrvTermData caller;
	    tcp_descriptor* accept_desc;
	    int err;

	    if (desc->inet.opt == NULL) {
		/* No caller setup */
		sock_close(s);
		ret = async_error(INETP(desc), EINVAL);
		goto done;
	    }
	    caller = desc->inet.opt->caller;
	    if ((accept_desc = afunix_copy(desc,s,caller,&err)) == NULL) {
		sock_close(s);
		ret = async_error(INETP(desc), err);
		goto done;
	    }
	    /* FIXME: may MUST lock port 
	     * 1 - Port is accessible via the erlang:ports()
	     * 2 - Port is accessible via callers process_info(links)
	     */
	    accept_desc->inet.remote = remote;
	    SET_NONBLOCKING(accept_desc->inet.s);

	    accept_desc->inet.state = INET_STATE_CONNECTED;
	    ret =  async_ok_port(INETP(desc), accept_desc->inet.dport);
	    goto done;
	}
    } else if (desc->inet.state == INET_STATE_MULTI_ACCEPTING) {
	SOCKET s;
	unsigned int len;
	inet_address remote;
	int id,req;
	ErlDrvTermData caller;
	MultiTimerData *timeout;
	ErlDrvMonitor monitor;
#ifdef HARDDEBUG
	int times = 0;
#endif

	while (desc->inet.state == INET_STATE_MULTI_ACCEPTING) {
	    len = sizeof(desc->inet.remote);
	    s = sock_accept(desc->inet.s, (struct sockaddr*) &remote, &len);
	    
	    if (s == INVALID_SOCKET && sock_errno() == ERRNO_BLOCK) {
		/* Just try again, no real error, keep the last return code */
		goto done;
	    }
#ifdef HARDDEBUG
	    if (++times > 1) {
		erts_fprintf(stderr,"Accepts in one suite: %d :-)",times);
	    }
#endif
	    if (deq_multi_op(desc,&id,&req,&caller,&timeout,&monitor) != 0) {
		ret = -1;
		goto done;
	    }
	    
	    if (desc->multi_first == NULL) {
		sock_select(INETP(desc),FD_ACCEPT,0);
		desc->inet.state = INET_STATE_LISTENING; /* restore state */
	    }
	    
	    if (timeout != NULL) {
		remove_multi_timer(&(desc->mtd), desc->inet.port, timeout);
	    }
	    
	    driver_demonitor_process(desc->inet.port, &monitor);
	    
	    
	    if (s == INVALID_SOCKET) { /* Not ERRNO_BLOCK, that's handled right away */
		ret = send_async_error(&desc->inet, 
				       id, caller, error_atom(sock_errno()));
		goto done;
	    }
	    else {
		tcp_descriptor* accept_desc;
		int err;
		
		if ((accept_desc = afunix_copy(desc,s,caller,&err)) == NULL) {
		    sock_close(s);
		    ret = send_async_error(&desc->inet, 
					   id, caller, error_atom(err));
		    goto done;
		}
		accept_desc->inet.remote = remote;
		SET_NONBLOCKING(accept_desc->inet.s);
		accept_desc->inet.state = INET_STATE_CONNECTED;
		ret =  send_async_ok_port(&desc->inet, 
					  id, caller, accept_desc->inet.dport);
	    }
	}
    }
    else if (IS_CONNECTED(INETP(desc))) {
	ret = tcp_recv(desc, 0);
	goto done;
    }
    else {
	/* maybe a close op from connection attempt?? */
	sock_select(INETP(desc),FD_ACCEPT,0);
	DEBUGF("afunix_input(%ld): s=%d bad state: %04x", 
		port, desc->inet.s, desc->inet.state);
    }
 done:
    DEBUGF("afunix_input(%ld) }", port);
    return ret;
}

static int tcp_send_error(tcp_descriptor* desc, int err)
{
    /*
     * If the port is busy, we must do some clean-up before proceeding.
     */
    if (IS_BUSY(INETP(desc))) {
	desc->inet.caller = desc->inet.busy_caller;
	if (desc->busy_on_send) {
	    driver_cancel_timer(desc->inet.port);
	    desc->busy_on_send = 0;	
	}
	desc->inet.state &= ~INET_F_BUSY;
	set_busy_port(desc->inet.port, 0);
    }

    /*
     * We used to handle "expected errors" differently from unexpected ones.
     * Now we handle all errors in the same way. We just have to distinguish
     * between passive and active sockets.
     */
    DEBUGF("driver_failure_eof(%ld) in %s, line %d",
	    (long)desc->inet.port, __FILE__, __LINE__);
    if (desc->inet.active) {
	tcp_closed_message(desc);
	inet_reply_error_am(INETP(desc), am_closed);
	if (desc->inet.exitf)
	    driver_exit(desc->inet.port, 0);
	else
	    desc_close(INETP(desc));
    } else {
	tcp_clear_output(desc);
	tcp_clear_input(desc);
	tcp_close_check(desc);
	erl_inet_close(INETP(desc));

	if (desc->inet.caller) {
	    inet_reply_error_am(INETP(desc), am_closed);
	}
	else {
	    /* No blocking send op to reply to right now.
	     * If next op is a send, make sure it returns {error,closed}
	     * rather than {error,enotconn}.
	     */
	    desc->tcp_add_flags |= TCP_ADDF_DELAYED_CLOSE_SEND;
	}

	/*
	 * Make sure that the next receive operation gets an {error,closed}
	 * result rather than {error,enotconn}. That means that the caller
	 * can safely ignore errors in the send operations and handle them
	 * in the receive operation.
	 */
	desc->tcp_add_flags |= TCP_ADDF_DELAYED_CLOSE_RECV;
    }
    return -1;
}

/*
** Send non-blocking vector data
*/
static int tcp_sendv(tcp_descriptor* desc, ErlIOVec* ev)
{
    ErlDrvSizeT sz;
    char buf[4];
    ErlDrvSizeT h_len;
    ssize_t n;
    ErlDrvPort ix = desc->inet.port;
    ErlDrvSizeT len = ev->size;

     switch(desc->inet.htype) {
     case TCP_PB_1:
         put_int8(len, buf);
         h_len = 1;
         break;
     case TCP_PB_2:
         put_int16(len, buf);
         h_len = 2;
         break;
     case TCP_PB_4:
         put_int32(len, buf);
         h_len = 4;
         break;
     default:
         if (len == 0)
             return 0;
         h_len = 0;
         break;
     }

    inet_output_count(INETP(desc), len+h_len);

    if (h_len > 0) {
	ev->iov[0].iov_base = buf;
	ev->iov[0].iov_len = h_len;
	ev->size += h_len;
    }

    if ((sz = driver_sizeq(ix)) > 0) {
	driver_enqv(ix, ev, 0);
	if (sz+ev->size >= desc->high) {
	    DEBUGF("tcp_sendv(%ld): s=%d, sender forced busy",
		    (long)desc->inet.port, desc->inet.s);
	    desc->inet.state |= INET_F_BUSY;  /* mark for low-watermark */
	    desc->inet.busy_caller = desc->inet.caller;
	    set_busy_port(desc->inet.port, 1);
	    if (desc->send_timeout != INET_INFINITY) {
		desc->busy_on_send = 1;
		driver_set_timer(desc->inet.port, desc->send_timeout);
	    }
	    return 1;
	}
    }
    else {
	int vsize = (ev->vsize > MAX_VSIZE) ? MAX_VSIZE : ev->vsize;
	
	DEBUGF("tcp_sendv(%ld): s=%d, about to send "LLU","LLU" bytes",
		(long)desc->inet.port, desc->inet.s, (llu_t)h_len, (llu_t)len);

	if (INETP(desc)->is_ignored) {
	    INETP(desc)->is_ignored |= INET_IGNORE_WRITE;
	    n = 0;
	} else if (desc->tcp_add_flags & TCP_ADDF_DELAY_SEND) {
	    n = 0;
	} else if (IS_SOCKET_ERROR(sock_sendv(desc->inet.s, ev->iov,
					      vsize, &n, 0))) {
	    if ((sock_errno() != ERRNO_BLOCK) && (sock_errno() != EINTR)) {
		int err = sock_errno();
		DEBUGF("tcp_sendv(%ld): s=%d, "
			"sock_sendv(size=2) errno = %d",
			(long)desc->inet.port, desc->inet.s, err);
		return tcp_send_error(desc, err);
	    }
	    n = 0;
	}
	else if (n == ev->size) {
	    return 0;
	}
	else {
	    DEBUGF("tcp_sendv(%ld): s=%d, only sent "
		    LLU"/%d of "LLU"/%d bytes/items",
		    (long)desc->inet.port, desc->inet.s,
		    (llu_t)n, vsize, (llu_t)ev->size, ev->vsize);
	}

	DEBUGF("tcp_sendv(%ld): s=%d, Send failed, queuing", 
		(long)desc->inet.port, desc->inet.s);
	driver_enqv(ix, ev, n); 
	if (!INETP(desc)->is_ignored)
	    sock_select(INETP(desc),(FD_WRITE|FD_CLOSE), 1);
    }
    return 0;
}

/*
** Send non blocking data
*/
static int tcp_send(tcp_descriptor* desc, char* ptr, ErlDrvSizeT len)
{
    int sz;
    char buf[4];
    int h_len;
    int n;
    ErlDrvPort ix = desc->inet.port;
    SysIOVec iov[2];

    switch(desc->inet.htype) {
    case TCP_PB_1: 
	put_int8(len, buf);
	h_len = 1;
	break;
    case TCP_PB_2: 
	put_int16(len, buf);
	h_len = 2; 
	break;
    case TCP_PB_4: 
	put_int32(len, buf);
	h_len = 4; 
	break;
    default:
	if (len == 0)
	    return 0;
	h_len = 0;
	break;
    }

    inet_output_count(INETP(desc), len+h_len);


    if ((sz = driver_sizeq(ix)) > 0) {
	if (h_len > 0)
	    driver_enq(ix, buf, h_len);
	driver_enq(ix, ptr, len);
	if (sz+h_len+len >= desc->high) {
	    DEBUGF("tcp_send(%ld): s=%d, sender forced busy",
		    (long)desc->inet.port, desc->inet.s);
	    desc->inet.state |= INET_F_BUSY;  /* mark for low-watermark */
	    desc->inet.busy_caller = desc->inet.caller;
	    set_busy_port(desc->inet.port, 1);
	    if (desc->send_timeout != INET_INFINITY) {
		desc->busy_on_send = 1;
		driver_set_timer(desc->inet.port, desc->send_timeout);
	    }
	    return 1;
	}
    }
    else {
	iov[0].iov_base = buf;
	iov[0].iov_len = h_len;
	iov[1].iov_base = ptr;
	iov[1].iov_len = len;

	DEBUGF("tcp_send(%ld): s=%d, about to send "LLU","LLU" bytes",
		(long)desc->inet.port, desc->inet.s, (llu_t)h_len, (llu_t)len);
	if (INETP(desc)->is_ignored) {
	    INETP(desc)->is_ignored |= INET_IGNORE_WRITE;
	    n = 0;
	} else if (desc->tcp_add_flags & TCP_ADDF_DELAY_SEND) {
	    sock_send(desc->inet.s, buf, 0, 0);
	    n = 0;
	} else 	if (IS_SOCKET_ERROR(sock_sendv(desc->inet.s,iov,2,&n,0))) {
	    if ((sock_errno() != ERRNO_BLOCK) && (sock_errno() != EINTR)) {
		int err = sock_errno();
		DEBUGF("tcp_send(%ld): s=%d,sock_sendv(size=2) errno = %d",
			(long)desc->inet.port, desc->inet.s, err);
		return tcp_send_error(desc, err);
	    }
	    n = 0;
	}
	else if (n == len+h_len) {
	    return 0;
	}

	DEBUGF("tcp_send(%ld): s=%d, Send failed, queuing", 
		(long)desc->inet.port, desc->inet.s);

	if (n < h_len) {
	    driver_enq(ix, buf+n, h_len-n);
	    driver_enq(ix, ptr, len);
	}
	else {
	    n -= h_len;
	    driver_enq(ix, ptr+n, len-n);
	}
	if (!INETP(desc)->is_ignored)
	    sock_select(INETP(desc),(FD_WRITE|FD_CLOSE), 1);
    }
    return 0;
}

static void afunix_drv_output(ErlDrvData data, ErlDrvEvent event)
{
    (void)afunix_output((tcp_descriptor*)data, (HANDLE)event);
}

static void afunix_drv_input(ErlDrvData data, ErlDrvEvent event)
{
    (void)afunix_input((tcp_descriptor*)data, (HANDLE)event);
}

/* socket ready for ouput:
** 1. INET_STATE_CONNECTING => non block connect ?
** 2. INET_STATE_CONNECTED  => write output
*/
static int afunix_output(tcp_descriptor* desc, HANDLE event)
{
    int ret = 0;
    ErlDrvPort ix = desc->inet.port;

    DEBUGF("afunix_output(%ld) {s=%d", 
	    (long)desc->inet.port, desc->inet.s);
    if (desc->inet.state == INET_STATE_CONNECTING) {
	sock_select(INETP(desc),FD_CONNECT,0);

	driver_cancel_timer(ix);  /* posssibly cancel a timer */
#ifndef __WIN32__
	/*
	 * XXX This is strange.  This *should* work on Windows NT too,
	 * but doesn't.  An bug in Winsock 2.0 for Windows NT?
	 *
	 * See "Unix Netwok Programming", W.R.Stevens, p 412 for a
	 * discussion about Unix portability and non blocking connect.
	 */

#ifndef SO_ERROR
	{
	    int sz = sizeof(desc->inet.remote);
	    int code = sock_peer(desc->inet.s,
				 (struct sockaddr*) &desc->inet.remote, &sz);

	    if (IS_SOCKET_ERROR(code)) {
		desc->inet.state = INET_STATE_BOUND;  /* restore state */
		ret =  async_error(INETP(desc), sock_errno());
		goto done;
	    }
	}
#else
	{
	    int error = 0;	/* Has to be initiated, we check it */
	    unsigned int sz = sizeof(error); /* even if we get -1 */
	    int code = sock_getopt(desc->inet.s, SOL_SOCKET, SO_ERROR, 
				   (void *)&error, &sz);

	    if ((code < 0) || error) {
		desc->inet.state = INET_STATE_BOUND;  /* restore state */
		ret = async_error(INETP(desc), error);
		goto done;
	    }
	}
#endif /* SO_ERROR */
#endif /* !__WIN32__ */

	desc->inet.state = INET_STATE_CONNECTED;
	if (desc->inet.active)
	    sock_select(INETP(desc),(FD_READ|FD_CLOSE),1);
	async_ok(INETP(desc));
    }
    else if (IS_CONNECTED(INETP(desc))) {
	for (;;) {
	    int vsize;
	    ssize_t n;
	    SysIOVec* iov;

	    if ((iov = driver_peekq(ix, &vsize)) == NULL) {
		sock_select(INETP(desc), FD_WRITE, 0);
		send_empty_out_q_msgs(INETP(desc));
		goto done;
	    }
	    vsize = vsize > MAX_VSIZE ? MAX_VSIZE : vsize;
	    DEBUGF("afunix_output(%ld): s=%d, About to send %d items", 
		    (long)desc->inet.port, desc->inet.s, vsize);
	    if (IS_SOCKET_ERROR(sock_sendv(desc->inet.s, iov, vsize, &n, 0))) {
	    write_error:
		if ((sock_errno() != ERRNO_BLOCK) && (sock_errno() != EINTR)) {
		    DEBUGF("afunix_output(%ld): sock_sendv(%d) errno = %d",
			    (long)desc->inet.port, vsize, sock_errno());
		    ret =  tcp_send_error(desc, sock_errno());
		    goto done;
		}
		goto done;
	    } else if (n == 0) { /* Workaround for redhat/CentOS 6.3 returning 
				    0 when sending packets with 
				    sizes > (max 32 bit signed int) */
	      size_t howmuch = 0x7FFFFFFF; /* max signed 32 bit */
	      int x;
	      for(x = 0; x < vsize && iov[x].iov_len == 0; ++x)
		;
	      if (x < vsize) {
		if (howmuch > iov[x].iov_len) {
		  howmuch = iov[x].iov_len;
		}
		n = sock_send(desc->inet.s, iov[x].iov_base,howmuch,0);
		if (IS_SOCKET_ERROR(n)) {
		  goto write_error;
		}
	      }
	    }
	    if (driver_deq(ix, n) <= desc->low) {
		if (IS_BUSY(INETP(desc))) {
		    desc->inet.caller = desc->inet.busy_caller;
		    desc->inet.state &= ~INET_F_BUSY;
		    set_busy_port(desc->inet.port, 0);
		    /* if we have a timer then cancel and send ok to client */
		    if (desc->busy_on_send) {
			driver_cancel_timer(desc->inet.port);
			desc->busy_on_send = 0;
		    }
		    inet_reply_ok(INETP(desc));
		}
	    }
	}
    }
    else {
	sock_select(INETP(desc),FD_CONNECT,0);
	DEBUGF("afunix_output(%ld): bad state: %04x", 
		(long)desc->inet.port, desc->inet.state);
    }
 done:
    DEBUGF("afunix_output(%ld) }", (long)desc->inet.port);
    return ret;
}

/*-----------------------------------------------------------------------------

   UDP & SCTP (the latter in a 1<->M Mode)

-----------------------------------------------------------------------------*/


static void absolute_timeout(unsigned millis, ErlDrvNowData *out)
{
    unsigned rest;
    unsigned long millipart;
    unsigned long secpart;
    unsigned long megasecpart;
    unsigned tmo_secs = (millis / 1000U);
    unsigned tmo_millis = (millis % 1000);
    driver_get_now(out);
    rest = (out->microsecs) % 1000;
    millipart = ((out->microsecs) / 1000UL);
    if (rest >= 500) {
	++millipart;
    }
    secpart = out->secs;
    megasecpart = out->megasecs;
    millipart += tmo_millis;
    secpart += (millipart / 1000000UL);
    millipart %= 1000000UL;
    secpart += tmo_secs;
    megasecpart += (secpart / 1000000UL);
    secpart %= 1000000UL;
    out->megasecs = megasecpart;
    out->secs = secpart;
    out->microsecs = (millipart * 1000UL);
}

static unsigned relative_timeout(ErlDrvNowData *in) 
{
    ErlDrvNowData now;
    unsigned rest;
    unsigned long millipart, in_millis, in_secs, in_megasecs;

    driver_get_now(&now);

    in_secs = in->secs;
    in_megasecs = in->megasecs;

    rest = (now.microsecs) % 1000;
    millipart = ((now.microsecs) / 1000UL);
    if (rest >= 500) {
	++millipart;
    }
    in_millis = ((in->microsecs) / 1000UL);
    if ( in_millis < millipart ) {
	if (in_secs > 0) {
	    --in_secs;
	} else {
	    in_secs = (1000000UL - 1UL);
	    if (in_megasecs <= now.megasecs) {
		return 0;
	    } else {
		--in_megasecs;
	    }
	}
	in_millis += 1000UL;
    }
    in_millis -= millipart;
    
    if (in_secs < now.secs) {
	if (in_megasecs <= now.megasecs) {
	    return 0;
	} else {
	    --in_megasecs;
	}
	in_secs += 1000000;
    }
    in_secs -= now.secs;
    if (in_megasecs < now.megasecs) {
	return 0;
    } else {
	in_megasecs -= now.megasecs;
    }
    return (unsigned) ((in_megasecs * 1000000000UL) + 
		       (in_secs * 1000UL) + 
		       in_millis);
}


static void fire_multi_timers(MultiTimerData **first, ErlDrvPort port,
			      ErlDrvData data)
{
    unsigned next_timeout;
    if (!*first) {
	return;
    }
#ifdef DEBUG
    {
	ErlDrvNowData chk;
	driver_get_now(&chk);
	chk.microsecs /= 10000UL;
	chk.microsecs *= 10000UL;
	chk.microsecs += 10000;
    }
#endif
    do {
	MultiTimerData *save = *first;
	*first = save->next;
	(*(save->timeout_function))(data,save->caller);
	FREE(save);
	if (*first == NULL) {
	    return;
	}
	(*first)->prev = NULL;
	next_timeout = relative_timeout(&((*first)->when));
    } while (next_timeout == 0);
    driver_set_timer(port,next_timeout);
}

static void clean_multi_timers(MultiTimerData **first, ErlDrvPort port)
{
    MultiTimerData *p;
    if (*first) {
	driver_cancel_timer(port);
    }
    while (*first) {
	p = *first;
	*first = p->next;
	FREE(p);
    }
}
static void remove_multi_timer(MultiTimerData **first, ErlDrvPort port, MultiTimerData *p)
{
    if (p->prev != NULL) {
	p->prev->next = p->next;
    } else {
	driver_cancel_timer(port);
	*first = p->next;
	if (*first) {
	    unsigned ntmo = relative_timeout(&((*first)->when));
	    driver_set_timer(port,ntmo);
	}
    }
    if (p->next != NULL) {
	p->next->prev = p->prev;
    }
    FREE(p);
}

static MultiTimerData *add_multi_timer(MultiTimerData **first, ErlDrvPort port, 
				       ErlDrvTermData caller, unsigned timeout,
				       void (*timeout_fun)(ErlDrvData drv_data, 
							   ErlDrvTermData caller))
{
    MultiTimerData *mtd, *p, *s;
    mtd = ALLOC(sizeof(MultiTimerData));
    absolute_timeout(timeout, &(mtd->when));
    mtd->timeout_function = timeout_fun;
    mtd->caller = caller;
    mtd->next = mtd->prev = NULL;
    for(p = *first,s = NULL; p != NULL; s = p, p = p->next) {
	if (p->when.megasecs >= mtd->when.megasecs) {
	    break;
	}
    }
    if (!p || p->when.megasecs > mtd->when.megasecs) {
	goto found;
    }
    for (; p!= NULL; s = p, p = p->next) {
	if (p->when.secs >= mtd->when.secs) {
	    break;
	}
    }
    if (!p || p->when.secs > mtd->when.secs) {
	goto found;
    }
    for (; p!= NULL; s = p, p = p->next) {
	if (p->when.microsecs >= mtd->when.microsecs) {
	    break;
	}
    }
 found:
    if (!p) {
	if (!s) {
	    *first = mtd;
	} else {
	    s->next = mtd;
	    mtd->prev = s;
	}
    } else {
	if (!s) {
	    *first = mtd;
	} else {
	    s->next = mtd;
	    mtd->prev = s;
	}
	mtd->next = p;
	p->prev = mtd;
    }
    if (!s) {
	if (mtd->next) {
	    driver_cancel_timer(port);
	}
	driver_set_timer(port,timeout);
    }
    return mtd;
}
	




/*-----------------------------------------------------------------------------

   Subscription

-----------------------------------------------------------------------------*/

static int
save_subscriber(subs, subs_pid)
subs_list *subs; ErlDrvTermData subs_pid;
{
  subs_list *tmp;

  if(NO_SUBSCRIBERS(subs)) {
    subs->subscriber = subs_pid;
    subs->next = NULL;
  }
  else {
    tmp = subs->next;
    subs->next = ALLOC(sizeof(subs_list));
    if(subs->next == NULL) {
      subs->next = tmp;
      return 0;
    }
    subs->next->subscriber = subs_pid;
    subs->next->next = tmp;
  }
  return 1;
}

static void
free_subscribers(subs)
subs_list *subs;
{
  subs_list *this;
  subs_list *next;

  this = subs->next;
  while(this) {
    next = this->next;
    FREE((void *) this);
    this = next;
  }

  subs->subscriber = NO_PROCESS;
  subs->next = NULL;
}

static void send_to_subscribers
(
    inet_descriptor* desc,
    subs_list	   *subs,
    int		   free_subs,
    ErlDrvTermData msg[],
    int msg_len
)
{
  subs_list *this;
  subs_list *next;
  int first = 1;

  if(NO_SUBSCRIBERS(subs))
    return;

  this = subs;
  while(this) {
    
    (void) SEND_TERM(desc, this->subscriber, msg, msg_len);

    if(free_subs && !first) {
      next = this->next;
      FREE((void *) this);
      this = next;
    }
    else
      this = this->next;
    first = 0;
  }

  if(free_subs) {
    subs->subscriber = NO_PROCESS;
    subs->next = NULL;
  }

}


DRIVER_INIT(afunix_drv)
{
    ErlDrvEntry* ptr = &afunix_driver_entry;

    DEBUGF("afunix_drv DRIVER_INIT");

    ptr->init  = afunix_init;
    ptr->start = afunix_start;
    ptr->stop  = afunix_stop;
    ptr->output = afunix_command;
    ptr->outputv = afunix_commandv;
    ptr->ready_input  = afunix_drv_input;
    ptr->ready_output = afunix_drv_output;
    ptr->finish = afunix_finish;
    ptr->driver_name = "afunix_drv";
    ptr->control = afunix_ctl;
    ptr->timeout = afunix_timeout;
    ptr->flush   = afunix_flush;
    ptr->extended_marker = ERL_DRV_EXTENDED_MARKER;
    ptr->major_version = ERL_DRV_EXTENDED_MAJOR_VERSION;
    ptr->minor_version = ERL_DRV_EXTENDED_MINOR_VERSION;
    ptr->driver_flags = ERL_DRV_FLAG_USE_PORT_LOCKING;
    ptr->process_exit = afunix_process_exit;
    ptr->stop_select = inet_stop_select;
    return ptr;
}
