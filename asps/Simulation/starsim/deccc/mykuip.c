/* 
* $Id: mykuip.c,v 1.1.1.1 2004/01/12 23:49:39 potekhin Exp $
* $Name:  $
* $Log: mykuip.c,v $
* Revision 1.1.1.1  2004/01/12 23:49:39  potekhin
*
* Revision 1.4  2002/04/29 00:58:47  nevski
* production support utilities
*
* Revision 1.3  2001/03/21 00:26:49  nevski
* temporarely remove underscore
*
* Revision 1.2  2001/03/05 11:55:22  nevski
* headers clean-up
*
* Revision 1.1  2001/02/27 10:15:18  nevski
*  first working release
*/
/*CMZ :  2.00/00 01/09/99  22.08.18  by  Pavel Nevski*/
/*-- Author :    Pavel Nevski   24/09/98*/

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

extern KmCommand *cmdcurrent_;
#if defined(CAN_BE_CHANGED)
void reset_arg_list( KmCommand *cmd )
{
  cmdcurrent_ = cmd;

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
#endif
void dump_arg_list_ ( )
{
  static KmCommand *cmd;
  cmd = cmdcurrent_;
  if (cmd == 0 ) { printf (" *** Kuip tracing not available ***\n"); return; }

  if( cmd->argc > 0 )
  { int i;
    printf (" *** Last command: %s \n",cmd->name);
    for( i = 0; i < cmd->argc; i++ )
    printf (" *** argument %d: %s \n",i,cmd->argv[i] );
/*  printf (" *** argument line as entered: \n %s \n", cmd->argline ); */
    printf (" ****************************************************** \n");
  }
}

