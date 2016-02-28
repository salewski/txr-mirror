/* Copyright 2010-2016
 * Kaz Kylheku <kaz@kylheku.com>
 * Vancouver, Canada
 * All rights reserved.
 *
 * Redistribution of this software in source and binary forms, with or without
 * modification, is permitted provided that the following two conditions are met.
 *
 * Use of this software in any manner constitutes agreement with the disclaimer
 * which follows the two conditions.
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DAMAGES, HOWEVER CAUSED,
 * AND UNDER ANY THEORY OF LIABILITY, ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <wchar.h>
#include <signal.h>
#include <dirent.h>
#include <errno.h>
#include "config.h"
#include <sys/un.h>
#include <netdb.h>
#include ALLOCA_H
#include "lib.h"
#include "stream.h"
#include "signal.h"
#include "utf8.h"
#include "unwind.h"
#include "gc.h"
#include "eval.h"
#include "args.h"
#include "struct.h"
#include "arith.h"
#include "socket.h"

val sockaddr_in_s, sockaddr_in6_s, sockaddr_un_s, addrinfo_s;
val flags_s, family_s, socktype_s, protocol_s, addr_s, canonname_s;
val port_s, flow_info_s, scope_id_s, path_s;

static val ipv4_addr_to_num(struct in_addr *src)
{
  return num_from_buffer(coerce(mem_t *, &src->s_addr), 4);
}

static void ipv4_addr_from_num(struct in_addr *dst, val addr)
{
  if (!num_to_buffer(addr, coerce(mem_t *, &dst->s_addr), 4))
    uw_throwf(socket_error_s, lit("~s out of range for IPv4 address"),
              addr, nao);
}

static val ipv6_addr_to_num(struct in6_addr *src)
{
  return num_from_buffer(src->s6_addr, 16);
}

static void ipv6_addr_from_num(struct in6_addr *dst, val addr)
{
  if (!num_to_buffer(addr, dst->s6_addr, 16))
    uw_throwf(socket_error_s, lit("~s out of range for IPv6 address"),
              addr, nao);
}

static void ipv6_flow_info_from_num(struct sockaddr_in6 *dst, val flow)
{
  if (!num_to_buffer(flow, coerce(mem_t *, &dst->sin6_flowinfo), 4))
    uw_throwf(socket_error_s, lit("~s out of range for IPv6 flow info"),
              flow, nao);
}

static void ipv6_scope_id_from_num(struct sockaddr_in6 *dst, val scope)
{
  if (!num_to_buffer(scope, coerce(mem_t *, &dst->sin6_scope_id), 4))
    uw_throwf(socket_error_s, lit("~s out of range for IPv6 scope ID"),
              scope, nao);
}


static val sockaddr_in_out(struct sockaddr_in *src)
{
  args_decl(args, ARGS_MIN);
  val out = make_struct(sockaddr_in_s, nil, args);
  slotset(out, addr_s, ipv4_addr_to_num(&src->sin_addr));
  slotset(out, port_s, num_fast(ntohs(src->sin_port)));
  return out;
}

static val sockaddr_in6_out(struct sockaddr_in6 *src)
{
  args_decl(args, ARGS_MIN);
  val out = make_struct(sockaddr_in6_s, nil, args);
  slotset(out, addr_s, ipv6_addr_to_num(&src->sin6_addr));
  slotset(out, port_s, num_fast(ntohs(src->sin6_port)));
  return out;
}

static val unix_sockaddr_out(struct sockaddr_un *src)
{
  args_decl(args, ARGS_MIN);
  val out = make_struct(sockaddr_un_s, nil, args);
  slotset(out, path_s, string_utf8(src->sun_path));
  return out;
}

#ifdef HAVE_GETADDRINFO

static void addrinfo_in(struct addrinfo *dest, val src)
{
  dest->ai_flags = c_num(default_arg(slot(src, flags_s), zero));
  dest->ai_family = c_num(default_arg(slot(src, family_s), zero));
  dest->ai_socktype = c_num(default_arg(slot(src, socktype_s), zero));
  dest->ai_protocol = c_num(default_arg(slot(src, protocol_s), zero));
}

static val getaddrinfo_wrap(val node_in, val service_in, val hints_in)
{
  val node = default_arg(node_in, nil);
  val service = default_arg(service_in, nil);
  val hints = default_arg(hints_in, nil);
  struct addrinfo hints_ai, *phints = hints ? &hints_ai : 0, *alist, *aiter;
  char *node_u8 = stringp(node) ? utf8_dup_to(c_str(node)) : 0;
  char *service_u8 = stringp(service) ? utf8_dup_to(c_str(service)) : 0;
  val node_num_p = integerp(node);
  val svc_num_p = integerp(service);
  int res;
  list_collect_decl (out, ptail);

  if (hints) {
    memset(&hints_ai, 0, sizeof hints_ai);
    addrinfo_in(&hints_ai, hints);
  }

  res = getaddrinfo(node_u8, service_u8, phints, &alist);

  free(node_u8);
  free(service_u8);

  if (res == 0) {
    for (aiter = alist; aiter; aiter = aiter->ai_next) {
      switch (aiter->ai_family) {
      case AF_INET:
        {
          struct sockaddr_in *sa = coerce(struct sockaddr_in *, aiter->ai_addr);
          if (node_num_p)
            ipv4_addr_from_num(&sa->sin_addr, node);
          if (svc_num_p)
            sa->sin_port = htons(c_num(service));
          ptail = list_collect(ptail, sockaddr_in_out(sa));
        }
        break;
      case AF_INET6:
        {
          struct sockaddr_in6 *sa = coerce(struct sockaddr_in6 *, aiter->ai_addr);
          if (node_num_p)
            ipv6_addr_from_num(&sa->sin6_addr, node);
          if (svc_num_p)
            sa->sin6_port = ntohs(c_num(service));
          ptail = list_collect(ptail, sockaddr_in6_out(sa));
        }
        break;
      }
    }
  }

  freeaddrinfo(alist);

  return out;
}

#endif

static void addr_mismatch(val addr, val family)
{
  uw_throwf(socket_error_s, lit("address ~s doesn't match address family ~s"),
            addr, family, nao);
}

static void sockaddr_in(val sockaddr, val family,
                        struct sockaddr_storage *buf, socklen_t *len)
{
  val addr_type = typeof(sockaddr);

  if (addr_type == sockaddr_in_s) {
    val addr = slot(sockaddr, addr_s);
    val port = slot(sockaddr, port_s);
    struct sockaddr_in *sa = coerce(struct sockaddr_in *, buf);
    if (family != num_fast(AF_INET))
      addr_mismatch(sockaddr, family);
    sa->sin_family = AF_INET;
    ipv4_addr_from_num(&sa->sin_addr, addr);
    sa->sin_port = ntohs(c_num(port));
    *len = sizeof *sa;
  } else if (addr_type == sockaddr_in6_s) {
    val addr = slot(sockaddr, addr_s);
    val port = slot(sockaddr, port_s);
    val flow = slot(sockaddr, flow_info_s);
    val scope = slot(sockaddr, scope_id_s);
    struct sockaddr_in6 *sa = coerce(struct sockaddr_in6 *, buf);
    if (family != num_fast(AF_INET6))
      addr_mismatch(sockaddr, family);
    sa->sin6_family = AF_INET6;
    ipv6_addr_from_num(&sa->sin6_addr, addr);
    ipv6_flow_info_from_num(sa, flow);
    ipv6_scope_id_from_num(sa, scope);
    sa->sin6_port = ntohs(c_num(port));
    *len = sizeof *sa;
  } else if (addr_type == sockaddr_un_s) {
    val path = slot(sockaddr, path_s);
    char *path_u8 = utf8_dup_to(c_str(path));
    struct sockaddr_un *sa = coerce(struct sockaddr_un *, buf);
    memset(sa, 0, sizeof *sa);
    sa->sun_family = AF_UNIX;
    strncpy(sa->sun_path, path_u8, sizeof sa->sun_path - 1);
    free(path_u8);
    *len = sizeof *sa;
  } else {
    uw_throwf(socket_error_s, lit("object ~s isn't a socket address"),
              sockaddr, nao);
  }
}

static val sock_bind(val sock, val sockaddr)
{
  int sfd = c_num(stream_fd(sock));
  val family = sock_family(sock);
  struct sockaddr_storage sa;
  socklen_t salen;
  int reuse = 1;

  (void) setsockopt(sfd, SOL_SOCKET, SO_REUSEADDR, &reuse, sizeof(reuse));

  sockaddr_in(sockaddr, family, &sa, &salen);

  if (bind(sfd, coerce(struct sockaddr *, &sa), salen) != 0)
    uw_throwf(socket_error_s, lit("bind failed: ~d/~s"),
              num(errno), string_utf8(strerror(errno)), nao);

  return t;
}

static val sock_connect(val sock, val sockaddr)
{
  val sfd = stream_fd(sock);
  val family = sock_family(sock);
  struct sockaddr_storage sa;
  socklen_t salen;

  sockaddr_in(sockaddr, family, &sa, &salen);

  if (connect(c_num(sfd), coerce(struct sockaddr *, &sa), salen) != 0)
    uw_throwf(socket_error_s, lit("connect failed: ~d/~s"),
              num(errno), string_utf8(strerror(errno)), nao);

  return t;
}

static val sock_listen(val sock, val backlog)
{
  val sfd = stream_fd(sock);

  if (listen(c_num(sfd), c_num(default_arg(backlog, num_fast(16)))))
    uw_throwf(socket_error_s, lit("listen failed: ~d/~s"),
              num(errno), string_utf8(strerror(errno)), nao);

  return t;
}

static val sock_accept(val sock, val mode_str)
{
  val sfd = stream_fd(sock);
  val family = sock_family(sock);
  struct sockaddr_storage sa;
  socklen_t salen;
  int afd;
  val peer;

  sig_save_enable;

  afd = accept(c_num(sfd), coerce(struct sockaddr *, &sa), &salen);

  sig_restore_enable;

  if (afd < 0)
    uw_throwf(socket_error_s, lit("accept failed: ~d/~s"),
              num(errno), string_utf8(strerror(errno)), nao);

  if (family == num_fast(AF_INET))
    peer = sockaddr_in_out(coerce(struct sockaddr_in *, &sa));
  else if (family == num_fast(AF_INET6))
    peer = sockaddr_in6_out(coerce(struct sockaddr_in6 *, &sa));
  else if (family == num_fast(AF_UNIX))
    peer = unix_sockaddr_out(coerce(struct sockaddr_un *, &sa));
  else
    uw_throwf(socket_error_s, lit("accept: ~s isn't a supported socket family"),
              family, nao);

  {
    val stream = open_sockfd(num(afd), family, num_fast(SOCK_STREAM), mode_str);
    sock_set_peer(stream, peer);
    return stream;
  }
}

static val sock_shutdown(val sock, val how)
{
  val sfd = stream_fd(sock);

  flush_stream(sock);

  if (shutdown(c_num(sfd), c_num(default_arg(how, num_fast(SHUT_WR)))))
    uw_throwf(socket_error_s, lit("shutdown failed: ~d/~s"),
              num(errno), string_utf8(strerror(errno)), nao);

  return t;
}

void sock_load_init(void)
{
  sockaddr_in_s = intern(lit("sockaddr-in"), user_package);
  sockaddr_in6_s = intern(lit("sockaddr-in6"), user_package);
  sockaddr_un_s = intern(lit("sockaddr-un"), user_package);
  addrinfo_s = intern(lit("addrinfo"), user_package);
  flags_s = intern(lit("flags"), user_package);
  family_s = intern(lit("family"), user_package);
  socktype_s = intern(lit("socktype"), user_package);
  protocol_s = intern(lit("protocol"), user_package);
  addr_s = intern(lit("addr"), user_package);
  canonname_s = intern(lit("canonname"), user_package);
  port_s = intern(lit("port"), user_package);
  flow_info_s = intern(lit("flow-info"), user_package);
  scope_id_s = intern(lit("scope-id"), user_package);
  path_s = intern(lit("path"), user_package);

#ifdef HAVE_GETADDRINFO
  reg_fun(intern(lit("getaddrinfo"), user_package), func_n3o(getaddrinfo_wrap, 1));
#endif

  reg_varl(intern(lit("af-unspec"), user_package), num_fast(AF_UNSPEC));
  reg_varl(intern(lit("af-unix"), user_package), num_fast(AF_UNIX));
  reg_varl(intern(lit("af-inet"), user_package), num_fast(AF_INET));
  reg_varl(intern(lit("af-inet6"), user_package), num_fast(AF_INET6));
  reg_varl(intern(lit("sock-stream"), user_package), num_fast(SOCK_STREAM));
  reg_varl(intern(lit("sock-dgram"), user_package), num_fast(SOCK_DGRAM));
  reg_varl(intern(lit("inaddr-any"), user_package), zero);
  reg_varl(intern(lit("inaddr-loopback"), user_package), num(0x7F000001));
  reg_varl(intern(lit("in6addr-any"), user_package), zero);
  reg_varl(intern(lit("in6addr-loopback"), user_package), one);
#ifdef SOCK_NONBLOCK
  reg_varl(intern(lit("sock-nonblock"), user_package), num_fast(SOCK_NONBLOCK));
#endif
#ifdef SOCK_CLOEXEC
  reg_varl(intern(lit("sock-cloexec"), user_package), num_fast(SOCK_CLOEXEC));
#endif
#ifdef HAVE_GETADDRINFO
  reg_varl(intern(lit("ai-passive"), user_package), num_fast(AI_PASSIVE));
  reg_varl(intern(lit("ai-canonname"), user_package), num_fast(AI_CANONNAME));
  reg_varl(intern(lit("ai-numerichost"), user_package), num_fast(AI_NUMERICHOST));
  reg_varl(intern(lit("ai-v4mapped"), user_package), num_fast(AI_V4MAPPED));
  reg_varl(intern(lit("ai-all"), user_package), num_fast(AI_ALL));
  reg_varl(intern(lit("ai-addrconfig"), user_package), num_fast(AI_ADDRCONFIG));
  reg_varl(intern(lit("ai-numericserv"), user_package), num_fast(AI_NUMERICSERV));
#endif

  reg_fun(intern(lit("sock-bind"), user_package), func_n2(sock_bind));
  reg_fun(intern(lit("sock-connect"), user_package), func_n2(sock_connect));
  reg_fun(intern(lit("sock-listen"), user_package), func_n2o(sock_listen, 1));
  reg_fun(intern(lit("sock-accept"), user_package), func_n2o(sock_accept, 1));
  reg_fun(intern(lit("sock-shutdown"), user_package), func_n2o(sock_shutdown, 1));
}
