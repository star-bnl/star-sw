/*
 * $Id: traceqc.c,v 1.6 1998/09/24 08:04:41 nevski Exp $
 *
 * $Log: traceqc.c,v $
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
  
typedef struct _KmCommand {     /*                                         */ \
  struct _KmCommand *next;      /* link to next command                    */ \
  char         *path;           /* command path                            */ \
  char         *name;           /* command name                            */ \
  int           hidden;         /* flag if command is invisible            */ \
  int           level;          /* depth of submenus                       */ \
  int           total;          /* total number of parameters              */ \
  int           mandatory;      /* number of mandatory parameters          */ \
  int         **par;            /* list of total parameter descriptions    */ \
  int           list_par;       /* index+1 of parameter taking a list      */ \
  int           xcount;         /* count number of action calls            */ \
  int          *action_F;       /* action routine                          */ \
  int          *action_C;       /* action routine                          */ \
  int          *user_help_F;    /* user help routine                       */ \
  int          *user_help_C;    /* user help routine                       */ \
  int          nguidance;       /* number of lines in guidance text        */ \
  char        **guidance;       /* help text                               */ \
  int          nkeyword;        /* number of lines for keywords            */ \
  char        **keyword;        /* list of keywords                        */ \
  int          nhlink;          /* number of lines for links               */ \
  char        **hlink;          /* list of links                           */ \
  int           argc;           /* number of arguments entered             */ \
  char        **argv;           /* argc argument values                    */ \
  char         *argline;        /* argument line as entered                */ \
  int          *argoffs;        /* argc offsets into argline for KUGETE    */ \
} KmCommand;                    /*                                         */ \
 
void reset_arg_list( KmCommand *cmd )
{
  extern KmCommand *cmd_current_;
  cmd_current_ = cmd;
 
  /* clear current values */
  if( cmd->argc > 0 )
  {
    int i;
    for( i = 0; i < cmd->argc; i++ )
      free( cmd->argv[i] );
    cmd->argc = 0;
    free( (char*)cmd->argv );
    cmd->argv = 0;
    free( cmd->argline );
    cmd->argline = 0;
    free( (char*)cmd->argoffs );
    cmd->argoffs = 0;
  }
}
 
void dump_arg_list_ ( )
{
  extern KmCommand *cmd_current_;
  static KmCommand *cmd;
 
  cmd = cmd_current_;
  if( cmd->argc > 0 )
  {
    int i;
    for( i = 0; i < cmd->argc; i++ )
    printf (" argument %d: %s \n",i,cmd->argv[i] );
    printf(" argument line as entered: \n %s \n", cmd->argline );
  }
}
 
