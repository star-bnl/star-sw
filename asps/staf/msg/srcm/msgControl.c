
/*  File: msgControl.c */

static const char sccsid[] = "@(#)"__FILE__"\t\t1.55\tCreated 10/1/96 14:34:38, \tcompiled "__DATE__;

#define TRUE -1
#define FALSE 0
#define JLUN 7
#define SLUN 8

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <assert.h>
#include <sys/signal.h>

#include <time.h>

#include <sys/ipc.h>
#include <sys/shm.h>

#ifndef HPUX
#ifndef AIX
/*#error  (This is not an error!)   Compiling dlfcn (sgi, sun) version of msgControl  */
#define DOdlfcn TRUE
#endif
#endif

#ifdef DOdlfcn
#include <dlfcn.h>
#endif


#include <msg.h>




void usage( int argc, char*argv[] )
{
  fprintf(stderr,"Usage:\n\tmsgControl [-<switchName1>[<switchValue1>]] ... [-<switchNameN>[<switchValueN>]] [command line]\n");

  fprintf(stderr,"Switches:  (Must preceed the command)\n");
  fprintf(stderr,"\t-e<pathName>\t- specify name of msg-linked executable to manipulate.\n");
  fprintf(stderr,"\t-f<fileName>\t- specify name of msg state file to manipulate.  default: default.msg\n");
  fprintf(stderr,"\t-h\t- get this list of switches and commands.\n");
  fprintf(stderr,"\t-l<1 or 0>whether to list inactive prefixes (1) or not (0 -- default).\n");

  fprintf(stderr,"Commands:  (see MsgSetByCommand -- these commands are the same.)\n" );

  exit(0);
}



void	main( int argc, char*argv[] )
{

/*  Description:  Manipulates an msg state file.  */

#define MAXBYTES 1000

	char msgFileName[1000];
	char msgExecutable[1000];
	char msgCommand[1000];
	int listInactives=0;
	int JournalEnabled=0;

	int ret;
	int i, j, k;
	char*arg;

/*	Shared object stuff:  */
	int UseExecutable = FALSE;
#ifdef DOdlfcn
	void *handle;
	int  (*MsgShare)(char*);
	void (*MsgSummaryFile)(FILE*);
	int  (*MsgStateLoad)(char*);
	void (*MsgSetSummaryModeInactive)(int);
	int  (*MsgJournalEnabled)(void);
	void (*MsgJournalOff)(void);
	void (*MsgJournalOn)(void);
	int  (*MsgSetByCommand)(char*);
	int  (*MsgStateStore)(char*);
#endif
  
	char c1000[1000];
	int shmid;

	static int idd1 = 0;
	static int idd2 = 0;
	static int ide4 = 0;

	strcpy( msgExecutable, "libmsg.so" );

	for (i=1; i<argc; i++) {
	  if (argv[i][0]=='-') {
	    arg = &argv[i][2];
	    switch (argv[i][1]) {
	      case 'e': strcpy( msgExecutable, arg );UseExecutable = TRUE; break;
	      case 'f': strcpy( msgFileName, arg );UseExecutable = FALSE; break;
	      case 'h': usage(argc,argv); break;
	      case 'l': listInactives=strtoul( arg, NULL, 10 ); break;
	      default: {
	        printf( "MsgControl-E1 Unknown option: %s\n",&argv[i][0]);
	        usage(argc,argv);
	        exit(0);
	      }
	    } /* switch (argv[i][1] */
	  } else {                 /* if (argv[i][0]=='-')  */

/*	      Regenerate the complete command line, starting with the MsgSetByCommand command:  */
	      k=0;
	      msgCommand[0] = NULL;
	      for (j=i; j<argc; j++) {
	        arg = argv[j];
	        sprintf( &msgCommand[k], "%s ", arg );
	        k = strlen( msgCommand );
	      }

#ifdef DOdlfcn
	      handle = dlopen( msgExecutable, RTLD_LAZY);

	      if ( !handle ) {
	        printf( "msgControl-F1 Could not open executable [%s]\n   So long, now!\n", msgExecutable );
	        exit(1);
	      }

	      MsgShare                  = dlsym( handle, "MsgShare" );
	      MsgSetSummaryModeInactive = dlsym( handle, "MsgSetSummaryModeInactive" );
	      MsgSetByCommand           = dlsym( handle, "MsgSetByCommand" );
	      MsgStateLoad              = dlsym( handle, "MsgStateLoad" );
	      MsgStateStore             = dlsym( handle, "MsgStateStore" );
	      MsgSummaryFile            = dlsym( handle, "MsgSummaryFile" );
	      MsgJournalOff             = dlsym( handle, "MsgJournalOff" );
	      MsgJournalOn              = dlsym( handle, "MsgJournalOn" );
	      MsgJournalEnabled         = dlsym( handle, "MsgJournalEnabled" );

	      if ( UseExecutable ) {

	        shmid = (*MsgShare)( msgExecutable );
	        JournalEnabled = (*MsgJournalEnabled)();
	        if ( listInactives ) (*MsgSetSummaryModeInactive)( TRUE  ); /*  List inactive messages */
	        if ( JournalEnabled ) (*MsgJournalOff)();
	        ret = (*MsgSetByCommand)( msgCommand );
	        if ( JournalEnabled ) (*MsgJournalOn)();
	        if ( listInactives ) (*MsgSetSummaryModeInactive)( FALSE ); /*  Revert msg state before storing.  */

	      } else {

	        ret = (*MsgStateLoad)( msgFileName );
	        if ( ret ) {
	          if ( listInactives ) (*MsgSetSummaryModeInactive)( TRUE  ); /*  List inactive messages */
	          ret = (*MsgSetByCommand)( msgCommand );
	          if ( listInactives ) (*MsgSetSummaryModeInactive)( FALSE ); /*  Revert msg state before storing.  */
	          if ( ret ) {
	            ret = (*MsgStateStore)( msgFileName );
	          }
	        }
	      }
#else
	      if ( UseExecutable ) {

	        shmid = MsgShare( msgExecutable );
	        JournalEnabled = MsgJournalEnabled();
	        if ( listInactives ) MsgSetSummaryModeInactive( TRUE  ); /*  List inactive messages */
	        if ( JournalEnabled ) MsgJournalOff();
	        ret = MsgSetByCommand( msgCommand );
	        if ( JournalEnabled ) MsgJournalOn();
	        if ( listInactives ) MsgSetSummaryModeInactive( FALSE ); /*  Revert msg state before storing.  */

	      } else {

	        ret = MsgStateLoad( msgFileName );
	        if ( ret ) {
	          if ( listInactives ) MsgSetSummaryModeInactive( TRUE  ); /*  List inactive messages */
	          ret = MsgSetByCommand( msgCommand );
	          if ( listInactives ) MsgSetSummaryModeInactive( FALSE ); /*  Revert msg state before storing.  */
	          if ( ret ) {
	            ret = MsgStateStore( msgFileName );
	          }
	        }
	      }
#endif
	      if ( !ret ) exit(1);
	      exit(0);
	  }                        /* if (argv[i][0]=='-')  */
	}

	exit(0);

}

