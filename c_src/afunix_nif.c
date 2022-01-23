//
// Special NIF to get peercred/peerpid/uid/euid
//
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

#include <netdb.h>
#include <sys/socket.h>
#include <netinet/in.h>


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

#if defined (__APPLE__) && !defined(LOCAL_PEERCRED)
#define HAVE_GETPEEREID
#endif



#include "erl_nif.h"

static ERL_NIF_TERM get_peercred(ErlNifEnv* env, int argc, const ERL_NIF_TERM* argv)
{
    int fd;

    if (!enif_get_int(env, argv[0], &fd))
	return enif_make_badarg(env);
#ifdef LOCAL_PEERCRED
    {
	struct xucred x;
	socklen_t xucredlen = sizeof(x);
	if ((getsockopt(fd,SOL_LOCAL,LOCAL_PEERCRED,&x,&xucredlen) < 0) ||
	    (x.cr_version != XUCRED_VERSION)) {
	    return enif_make_int(env, -1);
	}
	return enif_make_int(env, x.cr_uid);
    }
#elif defined (HAVE_GETPEERUCRED)
    {
	ucred_t *uc = NULL;
	int euid;
	if (getpeerucred(fd,&uc) < 0) {
	    return enif_make_int(env, -1);
	}
	euid = ucred_geteuid(uc);
	(void) ucred_free(uc);
	return enif_make_int(env, euid);
    }
#elif defined (SO_PEERCRED)
    {
	struct ucred u;
	socklen_t ucredlen = sizeof(u);
	if (getsockopt(fd,SOL_SOCKET,SO_PEERCRED,&u,&ucredlen) < 0) {
	    return enif_make_int(env, -1);
	}
	return enif_make_int(env, u.uid);
    }
#elif defined (HAVE_GETPEEREID)
#warning "using getpeereid"
    {
	uid_t euid;
	gid_t egid;
	if (getpeereid(fd, &euid, &egid) < 0) {
	    return enif_make_int(env, -1);	    
	}
	return enif_make_int(env, euid);
    }
#else
#warning "no method of accessing socket peercred"
    return enif_make_int(env, -1);
#endif
}
    

static ERL_NIF_TERM get_peerpid(ErlNifEnv* env, int argc, const ERL_NIF_TERM* argv)
{
    int fd;

    if (!enif_get_int(env, argv[0], &fd))
	return enif_make_badarg(env);
#ifdef LOCAL_PEERPID
    {
	pid_t p;
	socklen_t plen = sizeof(p);
	if (getsockopt(fd,SOL_LOCAL,LOCAL_PEERPID,&p,&plen) < 0) {
	    return enif_make_int(env, -1);
	}
	return enif_make_int(env, (int)p);
    }
#elif defined (HAVE_GETPEERUCRED)
    {
	ucred_t *uc = NULL;
	int ucred;
	if (getpeerucred(desc->s,&uc) < 0) {
	    return enif_make_int(env, -1);
	}
	ucred = (int) ucred_getpid(uc);
	(void) ucred_free(uc);
	return enif_make_int(env, ucred);
    }
#elif defined (SO_PEERCRED)
    {	
	struct ucred u;
	socklen_t ucredlen = sizeof(u);
	if (getsockopt(fd,SOL_SOCKET,SO_PEERCRED,&u,&ucredlen) < 0) {
	    return enif_make_int(env, -1);
	}
	return enif_make_int(env, (int) u.pid);
    }
#else
#warning "no method of accessing socket peerpid found"
    return enif_make_int(env, -1);
#endif
}

static ERL_NIF_TERM get_uid(ErlNifEnv* env, int argc, const ERL_NIF_TERM* argv)
{
    uid_t uid = getuid();
    return enif_make_int(env, uid);    
}

static ERL_NIF_TERM get_euid(ErlNifEnv* env, int argc, const ERL_NIF_TERM* argv)
{
    uid_t euid = geteuid();
    return enif_make_int(env, euid);
}


static ErlNifFunc nif_funcs[] =
{
    {"get_peercred_", 1, get_peercred},
    {"get_peerpid_", 1, get_peerpid},
    {"get_uid_", 0, get_uid},
    {"get_euid_", 0, get_euid}
};

ERL_NIF_INIT(afunix_nif,nif_funcs,NULL,NULL,NULL,NULL)
