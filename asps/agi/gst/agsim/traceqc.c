/*
 * $Id: traceqc.c,v 1.9 1998/12/11 21:16:38 perev Exp $
 *
 * $Log: traceqc.c,v $
 * Revision 1.9  1998/12/11 21:16:38  perev
 * Back to oldDsl
 *
 * Revision 1.8  1998/09/28 03:14:08  nevski
 * protection against break in break
 *
 * Revision 1.7  1998/09/27 19:29:48  nevski
 * break treatment refined
 *
 * Revision 1.6  1998/09/24 08:04:41  nevski
 * verbose interrupt handler
 *
 * Revision 1.5  1998/09/24 05:25:30  nevski
 * verbose interrupt handler
 *
 * Revision 1.4  1998/08/03 17:23:57  didenko
 * correction for NT version by Faine
 *
 * Revision 1.3  1998/07/10 01:09:37  fisyak
 * remove comment
 *
 * Revision 1.2  1998/07/09 22:59:03  perev
 * replace fgsim.f -> fgsim.F
 *
 * Revision 1.1  1998/04/16 17:03:34  fisyak
 * 2nd pass with gstar
 *
 */
/*CMZ :          20/03/98  12.55.44  by  Pavel Nevski*/
/*CMZ :  1.30/00 22/04/97  14.49.55  by  Pavel Nevski*/
/*-- Author :    FR & JZ*/
#include <stdio.h>
#include "PAM.h"
/* include "kuip/kmenu4.h" */
int finite(double word);

#define traceqc_  F77_NAME(traceqc,TRACEQC)
void type_of_call traceqc_()
{  
#ifdef CERNLIB_HPUX 
void    U_STACK_TRACE();
        U_STACK_TRACE();
#else
printf (" interrupt trace routine (traceqc) not awailable yet \n"); 
#endif
}

/*  emulate kuip kuexec member function to get command handle */
#define   SUBROUTINE int
#define   IntFunc    int
#define   pCharFunc  int
#include  "kuip/kmenu.h"
 
#if !defined(HPUX) && !defined(Linux)
void reset_arg_list( KmCommand *cmd )
{
  extern KmCommand *cmd_current_;
  cmd_current_ = cmd;
  /* clear current values */
  if( cmd->argc > 0 )
  { int i;
    for(i=0; i<cmd->argc; i++ )  free( cmd->argv[i] );
    cmd->argc = 0;               free( (char*)cmd->argv );
    cmd->argv = 0;               free( cmd->argline );
    cmd->argline = 0;            free( (char*)cmd->argoffs );
    cmd->argoffs = 0;
} }
#endif  
void dump_arg_list_ ( )
{
  extern KmCommand *cmd_current_;
  static KmCommand *cmd;
  cmd = cmd_current_;
  if (cmd == 0 ) { printf (" *** Kuip tracing not available ***\n"); return; }

  if( cmd->argc > 0 )
  { int i;
    printf (" *** Last command: %s \n",cmd->name);
    for( i = 0; i < cmd->argc; i++ )
    printf (" *** argument %d: %s \n",i,cmd->argv[i] );
/*  printf (" *** argument line as entered: \n %s \n", cmd->argline ); */
    printf (" ****************************************************** \n");
} }

  
  int finite_ (float *r)  { double d; d=*r; return finite(*r); }

