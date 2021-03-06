/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2007 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id: signals_byt.c,v 1.1 2007/02/23 09:29:45 xleroy Exp $ */

/* Signal handling, code specific to the bytecode interpreter */

#include <signal.h>
#include "config.h"
#include "memory.h"
#include "osdeps.h"
#include "signals.h"
#include "signals_machdep.h"

#ifndef NSIG
#define NSIG 64
#endif

#ifdef _WIN32
typedef void (*sighandler)(int sig);
extern sighandler caml_win32_signal(int sig, sighandler action);
#define signal(sig,act) caml_win32_signal(sig,act)
#endif

CAMLexport int volatile caml_something_to_do = 0;
CAMLexport void (* volatile caml_async_action_hook)(void) = NULL;

void caml_process_event(void)
{
  void (*async_action)(void);

  if (caml_force_major_slice) caml_minor_collection ();
                             /* FIXME should be [caml_check_urgent_gc] */
  caml_process_pending_signals();
  async_action = caml_async_action_hook;
  if (async_action != NULL) {
    caml_async_action_hook = NULL;
    (*async_action)();
  }
}

static void handle_signal(int signal_number)
{
#if !defined(POSIX_SIGNALS) && !defined(BSD_SIGNALS)
  signal(signal_number, handle_signal);
#endif
  if (signal_number < 0 || signal_number >= NSIG) return;
  if (caml_try_leave_blocking_section_hook()) {
    caml_execute_signal(signal_number, 1);
    caml_enter_blocking_section_hook();
  }else{
    caml_record_signal(signal_number);
 }
}

int caml_set_signal_action(int signo, int action)
{
  void (*act)(int signo), (*oldact)(int signo);
#ifdef POSIX_SIGNALS
  struct sigaction sigact, oldsigact;
#endif

  switch (action) {
  case 0:  act = SIG_DFL; break;
  case 1:  act = SIG_IGN; break;
  default: act = handle_signal; break;
  }

#ifdef POSIX_SIGNALS
  sigact.sa_handler = act;
  sigemptyset(&sigact.sa_mask);
  sigact.sa_flags = 0;
  if (sigaction(signo, &sigact, &oldsigact) == -1) return -1;
  oldact = oldsigact.sa_handler;
#else
  oldact = signal(signo, act);
  if (oldact == SIG_ERR) return -1;
#endif
  if (oldact == handle_signal)
    return 2;
  else if (oldact == SIG_IGN)
    return 1;
  else
    return 0;
}
