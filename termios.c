/* Copyright 2016
 * Kaz Kylheku <kaz@kylheku.com>
 * Vancouver, Canada
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>
#include <signal.h>
#include <dirent.h>
#include <errno.h>
#include "config.h"
#include <termios.h>
#include ALLOCA_H
#include "lib.h"
#include "gc.h"
#include "args.h"
#include "eval.h"
#include "signal.h"
#include "unwind.h"
#include "stream.h"
#include "struct.h"
#include "termios.h"

val termios_s, iflag_s, oflag_s, cflag_s, lflag_s, cc_s, ispeed_s, ospeed_s;

static cnum termios_speed_to_baud(speed_t code)
{
  switch (code) {
  case B0: return 0;
  case B50: return 50;
  case B75: return 75;
  case B110: return 110;
  case B134: return 134;
  case B150: return 150;
  case B200: return 200;
  case B300: return 300;
  case B600: return 600;
  case B1200: return 1200;
  case B1800: return 1800;
  case B2400: return 2400;
  case B4800: return 4800;
  case B9600: return 9600;
  case B19200: return 19200;
  case B38400: return 38400;
#ifdef B57600
  case B57600: return 57600;
#endif
#ifdef B115200
  case B115200: return 115200;
#endif
#ifdef B230400
  case B230400: return 230400;
#endif
#ifdef B460800
  case B460800: return 460800;
#endif
#ifdef B500000
  case B500000: return 500000;
#endif
#ifdef B576000
  case B576000: return 576000;
#endif
#ifdef B921600
  case B921600: return 921600;
#endif
#ifdef B1000000
  case B1000000: return 1000000;
#endif
#ifdef B1152000
  case B1152000: return 1152000;
#endif
#ifdef B1500000
  case B1500000: return 1500000;
#endif
#ifdef B2000000
  case B2000000: return 2000000;
#endif
#ifdef B2500000
  case B2500000: return 2500000;
#endif
#ifdef B3000000
  case B3000000: return 3000000;
#endif
#ifdef B3500000
  case B3500000: return 3500000;
#endif
#ifdef B4000000
  case B4000000: return 4000000;
#endif
  default: return 9600;
  }
}

static speed_t termios_baud_to_speed(cnum baud)
{
  switch (baud) {
  case 0: return B0;
  case 50: return B50;
  case 75: return B75;
  case 110: return B110;
  case 134: return B134;
  case 150: return B150;
  case 200: return B200;
  case 300: return B300;
  case 600: return B600;
  case 1200: return B1200;
  case 1800: return B1800;
  case 2400: return B2400;
  case 4800: return B4800;
  case 9600: return B9600;
  case 19200: return B19200;
  case 38400: return B38400;
#ifdef B57600
  case 57600: return B57600;
#endif
#ifdef B115200
  case 115200: return B115200;
#endif
#ifdef B230400
  case 230400: return B230400;
#endif
#ifdef B460800
  case 460800: return B460800;
#endif
#ifdef B500000
  case 500000: return B500000;
#endif
#ifdef B576000
  case 576000: return B576000;
#endif
#ifdef B921600
  case 921600: return B921600;
#endif
#ifdef B1000000
  case 1000000: return B1000000;
#endif
#ifdef B1152000
  case 1152000: return B1152000;
#endif
#ifdef B1500000
  case 1500000: return B1500000;
#endif
#ifdef B2000000
  case 2000000: return B2000000;
#endif
#ifdef B2500000
  case 2500000: return B2500000;
#endif
#ifdef B3000000
  case 3000000: return B3000000;
#endif
#ifdef B3500000
  case 3500000: return B3500000;
#endif
#ifdef B4000000
  case 4000000: return B4000000;
#endif
  default:
    uw_throwf(error_s, lit("unsupported tty speed: ~a"), num(baud), nao);
  }
}

static val termios_unpack(struct termios *in)
{
  args_decl(args, ARGS_MIN);
  val out = make_struct(termios_s, nil, args);
  int i, cc_sz = convert(int, sizeof in->c_cc / sizeof in->c_cc[0]);
  val cc = vector(num_fast(cc_sz), nil);

  slotset(out, iflag_s, num(in->c_iflag));
  slotset(out, oflag_s, num(in->c_oflag));
  slotset(out, cflag_s, num(in->c_cflag));
  slotset(out, lflag_s, num(in->c_lflag));
  slotset(out, cc_s, cc);
  slotset(out, ispeed_s, num(termios_speed_to_baud(cfgetispeed(in))));
  slotset(out, ospeed_s, num(termios_speed_to_baud(cfgetospeed(in))));

  for (i = 0; i < cc_sz; i++)
    set(vecref_l(cc, num_fast(i)), num(in->c_cc[i]));

  return out;
}

static void termios_pack(struct termios *out, val in)
{
  int i, cc_sz = convert(int, sizeof out->c_cc / sizeof out->c_cc[0]);
  val cc = slot(in, cc_s);

  out->c_iflag = c_num(slot(in, iflag_s));
  out->c_oflag = c_num(slot(in, oflag_s));
  out->c_cflag = c_num(slot(in, cflag_s));
  out->c_lflag = c_num(slot(in, lflag_s));

  cfsetispeed(out, termios_baud_to_speed(c_num(slot(in, ispeed_s))));
  cfsetospeed(out, termios_baud_to_speed(c_num(slot(in, ospeed_s))));

  for (i = 0; i < cc_sz; i++) {
    val ch = vecref(cc, num_fast(i));
    cnum c = c_num(ch);

    out->c_cc[i] = c;

    if (out->c_cc[i] != c)
      uw_throwf(system_error_s, lit("value ~s is out of range for c_cc[~a] "
                                    "of struct termios"),
                ch, num_fast(i), nao);
  }
}

static val get_fd(val stream)
{
  val fd;

  if (missingp(stream))
    fd = stream_fd(std_input);
  else if (integerp(stream))
    fd = stream;
  else
    fd = stream_fd(stream);

  return fd;
}

static val tcgetattr_wrap(val stream)
{
  struct termios tio;
  int res;
  val fd = get_fd(stream);

  res = tcgetattr(c_num(fd), &tio);

  if (res < 0)
    uw_throwf(system_error_s, lit("tcgetattr failed: ~d/~s"),
              num(errno), string_utf8(strerror(errno)), nao);

  return termios_unpack(&tio);
}

static val tcsetattr_wrap(val termios, val actions, val stream)
{
  struct termios tio;
  int res;
  val fd = get_fd(stream);

  actions = default_arg(actions, num(TCSADRAIN));

  res = tcgetattr(c_num(fd), &tio);

  if (res < 0)
    uw_throwf(system_error_s, lit("tcgetattr failed: ~d/~s"),
              num(errno), string_utf8(strerror(errno)), nao);

  termios_pack(&tio, termios);

  res = tcsetattr(c_num(fd), c_num(actions), &tio);

  if (res < 0)
    uw_throwf(system_error_s, lit("tcsetattr failed: ~d/~s"),
              num(errno), string_utf8(strerror(errno)), nao);

  return termios;
}

static val tcsendbreak_wrap(val duration, val stream)
{
  val fd = get_fd(stream);
  int res = tcsendbreak(c_num(fd), if3(missingp(duration),
                                       500, c_num(duration)));

  if (res < 0)
    uw_throwf(system_error_s, lit("tcsendbreak failed: ~d/~s"),
              num(errno), string_utf8(strerror(errno)), nao);

  return t;
}

static val tcdrain_wrap(val stream)
{
  val fd = get_fd(stream);
  int res = tcdrain(c_num(fd));

  if (res < 0)
    uw_throwf(system_error_s, lit("tcdrain failed: ~d/~s"),
              num(errno), string_utf8(strerror(errno)), nao);

  return t;
}

static val tcflush_wrap(val queue, val stream)
{
  val fd = get_fd(stream);
  int res = tcflush(c_num(fd), c_num(queue));

  if (res < 0)
    uw_throwf(system_error_s, lit("tcflush failed: ~d/~s"),
              num(errno), string_utf8(strerror(errno)), nao);

  return t;
}

static val tcflow_wrap(val action, val stream)
{
  val fd = get_fd(stream);
  int res = tcflow(c_num(fd), c_num(action));

  if (res < 0)
    uw_throwf(system_error_s, lit("tcflow failed: ~d/~s"),
              num(errno), string_utf8(strerror(errno)), nao);

  return t;
}

static val encode_speeds(val termios)
{
  struct termios tio = { 0 };

  tio.c_iflag = c_num(slot(termios, iflag_s));
  tio.c_cflag = c_num(slot(termios, cflag_s));
  cfsetispeed(&tio, termios_baud_to_speed(c_num(slot(termios, ispeed_s))));
  cfsetospeed(&tio, termios_baud_to_speed(c_num(slot(termios, ospeed_s))));
  slotset(termios, iflag_s, num(tio.c_iflag));
  slotset(termios, cflag_s, num(tio.c_cflag));

  return termios;
}

static val decode_speeds(val termios)
{
  struct termios tio = { 0 };

  tio.c_cflag = c_num(slot(termios, cflag_s));
  tio.c_iflag = c_num(slot(termios, iflag_s));
  slotset(termios, ispeed_s, num(termios_speed_to_baud(cfgetispeed(&tio))));
  slotset(termios, ospeed_s, num(termios_speed_to_baud(cfgetospeed(&tio))));

  return termios;
}

void termios_init(void)
{
  val termios_t;

  termios_s = intern(lit("termios"), user_package);
  iflag_s = intern(lit("iflag"), user_package);
  oflag_s = intern(lit("oflag"), user_package);
  cflag_s = intern(lit("cflag"), user_package);
  lflag_s = intern(lit("lflag"), user_package);
  cc_s = intern(lit("cc"), user_package);
  ispeed_s = intern(lit("ispeed"), user_package);
  ospeed_s = intern(lit("ospeed"), user_package);

  termios_t = make_struct_type(termios_s, nil, nil,
                               list(iflag_s, oflag_s, cflag_s, lflag_s,
                                    cc_s, ispeed_s, ospeed_s, nao),
                               nil, nil, nil, nil);

  reg_fun(intern(lit("tcgetattr"), user_package), func_n1o(tcgetattr_wrap, 0));
  reg_fun(intern(lit("tcsetattr"), user_package), func_n3o(tcsetattr_wrap, 1));
  reg_fun(intern(lit("tcsendbreak"), user_package), func_n2o(tcsendbreak_wrap, 0));
  reg_fun(intern(lit("tcdrain"), user_package), func_n1(tcdrain_wrap));
  reg_fun(intern(lit("tcflush"), user_package), func_n2(tcflush_wrap));
  reg_fun(intern(lit("tcflow"), user_package), func_n2(tcflow_wrap));
  static_slot_ensure(termios_t, intern(lit("encode-speeds"), system_package),
                     func_n1(encode_speeds), nil);
  static_slot_ensure(termios_t, intern(lit("decode-speeds"), system_package),
                     func_n1(decode_speeds), nil);

  /* cc array indexes */
  reg_varl(intern(lit("vintr"), user_package), num_fast(VINTR));
  reg_varl(intern(lit("vquit"), user_package), num_fast(VQUIT));
  reg_varl(intern(lit("verase"), user_package), num_fast(VERASE));
  reg_varl(intern(lit("vkill"), user_package), num_fast(VKILL));
  reg_varl(intern(lit("veof"), user_package), num_fast(VEOF));
  reg_varl(intern(lit("vtime"), user_package), num_fast(VTIME));
  reg_varl(intern(lit("vmin"), user_package), num_fast(VMIN));
#ifdef VSWTC
  reg_varl(intern(lit("vswtc"), user_package), num_fast(VSWTC));
#endif
  reg_varl(intern(lit("vstart"), user_package), num_fast(VSTART));
  reg_varl(intern(lit("vstop"), user_package), num_fast(VSTOP));
  reg_varl(intern(lit("vsusp"), user_package), num_fast(VSUSP));
  reg_varl(intern(lit("veol"), user_package), num_fast(VEOL));
#ifdef VREPRINT
  reg_varl(intern(lit("vreprint"), user_package), num_fast(VREPRINT));
#endif
#ifdef VDISCARD
  reg_varl(intern(lit("vdiscard"), user_package), num_fast(VDISCARD));
#endif
#ifdef VWERASE
  reg_varl(intern(lit("vwerase"), user_package), num_fast(VWERASE));
#endif
#ifdef VLNEXT
  reg_varl(intern(lit("vlnext"), user_package), num_fast(VLNEXT));
#endif
#ifdef VEOL2
  reg_varl(intern(lit("veol2"), user_package), num_fast(VEOL2));
#endif
  /* iflag bits */
  reg_varl(intern(lit("ignbrk"), user_package), num_fast(IGNBRK));
  reg_varl(intern(lit("brkint"), user_package), num_fast(BRKINT));
  reg_varl(intern(lit("ignpar"), user_package), num_fast(IGNPAR));
  reg_varl(intern(lit("parmrk"), user_package), num_fast(PARMRK));
  reg_varl(intern(lit("inpck"), user_package), num_fast(INPCK));
  reg_varl(intern(lit("istrip"), user_package), num_fast(ISTRIP));
  reg_varl(intern(lit("inlcr"), user_package), num_fast(INLCR));
  reg_varl(intern(lit("igncr"), user_package), num_fast(IGNCR));
  reg_varl(intern(lit("icrnl"), user_package), num_fast(ICRNL));
#ifdef IUCLC
  reg_varl(intern(lit("iuclc"), user_package), num_fast(IUCLC));
#endif
  reg_varl(intern(lit("ixon"), user_package), num_fast(IXON));
  reg_varl(intern(lit("ixany"), user_package), num_fast(IXANY));
  reg_varl(intern(lit("ixoff"), user_package), num_fast(IXOFF));
#ifdef IMAXBEL
  reg_varl(intern(lit("imaxbel"), user_package), num_fast(IMAXBEL));
#endif
#ifdef IUTF8
  reg_varl(intern(lit("iutf8"), user_package), num_fast(IUTF8));
#endif
  /* oflag bits */
  reg_varl(intern(lit("opost"), user_package), num_fast(OPOST));
#ifdef OLCUC
  reg_varl(intern(lit("olcuc"), user_package), num_fast(OLCUC));
#endif
  reg_varl(intern(lit("onlcr"), user_package), num_fast(ONLCR));
  reg_varl(intern(lit("ocrnl"), user_package), num_fast(OCRNL));
  reg_varl(intern(lit("onocr"), user_package), num_fast(ONOCR));
  reg_varl(intern(lit("onlret"), user_package), num_fast(ONLRET));
  reg_varl(intern(lit("ofill"), user_package), num_fast(OFILL));
#ifdef OFDEL
  reg_varl(intern(lit("ofdel"), user_package), num_fast(OFDEL));
#endif
  reg_varl(intern(lit("vtdly"), user_package), num_fast(VTDLY));
  reg_varl(intern(lit("vt0"), user_package), num_fast(VT0));
  reg_varl(intern(lit("vt1"), user_package), num_fast(VT1));
#ifdef NLDLY
  reg_varl(intern(lit("nldly"), user_package), num_fast(NLDLY));
  reg_varl(intern(lit("nl0"), user_package), num_fast(NL0));
  reg_varl(intern(lit("nl1"), user_package), num_fast(NL1));
#endif
#ifdef CRDLY
  reg_varl(intern(lit("crdly"), user_package), num_fast(CRDLY));
  reg_varl(intern(lit("cr0"), user_package), num_fast(CR0));
  reg_varl(intern(lit("cr1"), user_package), num_fast(CR1));
  reg_varl(intern(lit("cr2"), user_package), num_fast(CR2));
  reg_varl(intern(lit("cr3"), user_package), num_fast(CR3));
#endif
#ifdef TABDLY
  reg_varl(intern(lit("tabdly"), user_package), num_fast(TABDLY));
  reg_varl(intern(lit("tab0"), user_package), num_fast(TAB0));
  reg_varl(intern(lit("tab1"), user_package), num_fast(TAB1));
  reg_varl(intern(lit("tab2"), user_package), num_fast(TAB2));
  reg_varl(intern(lit("tab3"), user_package), num_fast(TAB3));
#endif
#ifdef BSDLY
  reg_varl(intern(lit("bsdly"), user_package), num_fast(BSDLY));
  reg_varl(intern(lit("bs0"), user_package), num_fast(BS0));
  reg_varl(intern(lit("bs1"), user_package), num_fast(BS1));
#endif
#ifdef FFDLY
  reg_varl(intern(lit("ffdly"), user_package), num_fast(FFDLY));
  reg_varl(intern(lit("ff0"), user_package), num_fast(FF0));
  reg_varl(intern(lit("ff1"), user_package), num_fast(FF1));
#endif
  /* cflag bits */
  reg_varl(intern(lit("csize"), user_package), num_fast(CSIZE));
  reg_varl(intern(lit("cs5"), user_package), num_fast(CS5));
  reg_varl(intern(lit("cs6"), user_package), num_fast(CS6));
  reg_varl(intern(lit("cs7"), user_package), num_fast(CS7));
  reg_varl(intern(lit("cs8"), user_package), num_fast(CS8));
  reg_varl(intern(lit("cstopb"), user_package), num_fast(CSTOPB));
  reg_varl(intern(lit("cread"), user_package), num_fast(CREAD));
  reg_varl(intern(lit("parenb"), user_package), num_fast(PARENB));
  reg_varl(intern(lit("parodd"), user_package), num_fast(PARODD));
  reg_varl(intern(lit("hupcl"), user_package), num_fast(HUPCL));
  reg_varl(intern(lit("clocal"), user_package), num_fast(CLOCAL));
#ifdef CBAUD
  reg_varl(intern(lit("cbaud"), user_package), num_fast(CBAUD));
#endif
#ifdef CBAUDEX
  reg_varl(intern(lit("cbaudex"), user_package), num_fast(CBAUDEX));
#endif
#ifdef CMSPAR
  reg_varl(intern(lit("cmspar"), user_package), num_fast(CMSPAR));
#endif
#ifdef CRTSCTS
  reg_varl(intern(lit("crtscts"), user_package), num_fast(CRTSCTS));
#endif
  /* lflag bits */
  reg_varl(intern(lit("isig"), user_package), num_fast(ISIG));
  reg_varl(intern(lit("icanon"), user_package), num_fast(ICANON));
  reg_varl(intern(lit("echo"), user_package), num_fast(ECHO));
  reg_varl(intern(lit("echoe"), user_package), num_fast(ECHOE));
  reg_varl(intern(lit("echok"), user_package), num_fast(ECHOK));
  reg_varl(intern(lit("echonl"), user_package), num_fast(ECHONL));
  reg_varl(intern(lit("noflsh"), user_package), num_fast(NOFLSH));
  reg_varl(intern(lit("tostop"), user_package), num_fast(TOSTOP));
#ifdef IEXTEN
  reg_varl(intern(lit("iexten"), user_package), num_fast(IEXTEN));
#endif
#ifdef XCASE
  reg_varl(intern(lit("xcase"), user_package), num_fast(XCASE));
#endif
#ifdef ECHOCTL
  reg_varl(intern(lit("echoctl"), user_package), num_fast(ECHOCTL));
#endif
#ifdef ECHOPRT
  reg_varl(intern(lit("echoprt"), user_package), num_fast(ECHOPRT));
#endif
#ifdef ECHOKE
  reg_varl(intern(lit("echoke"), user_package), num_fast(ECHOKE));
#endif
#ifdef FLUSHO
  reg_varl(intern(lit("flusho"), user_package), num_fast(FLUSHO));
#endif
#ifdef PENDIN
  reg_varl(intern(lit("pendin"), user_package), num_fast(PENDIN));
#endif
#ifdef EXTPROC
  reg_varl(intern(lit("extproc"), user_package), num_fast(EXTPROC));
#endif
  /* tcflow */
  reg_varl(intern(lit("tcooff"), user_package), num_fast(TCOOFF));
  reg_varl(intern(lit("tcoon"), user_package), num_fast(TCOON));
  reg_varl(intern(lit("tcioff"), user_package), num_fast(TCIOFF));
  reg_varl(intern(lit("tcion"), user_package), num_fast(TCION));
  /* tcflush */
  reg_varl(intern(lit("tciflush"), user_package), num_fast(TCIFLUSH));
  reg_varl(intern(lit("tcoflush"), user_package), num_fast(TCOFLUSH));
  reg_varl(intern(lit("tcioflush"), user_package), num_fast(TCIOFLUSH));
  /* tcsetattr */
  reg_varl(intern(lit("tcsanow"), user_package), num_fast(TCSANOW));
  reg_varl(intern(lit("tcsadrain"), user_package), num_fast(TCSADRAIN));
  reg_varl(intern(lit("tcsaflush"), user_package), num_fast(TCSAFLUSH));
}
