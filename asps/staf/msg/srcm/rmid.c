#define TRUE -1
#define FALSE 0

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>

extern int errno;

void main( int argc, char*argv[] )

{
	int i;
	int shmflg;
	int shmid;
	int ProcessID;
	key_t key;

	for (i=1; i<argc; i++) {
	  ProcessID = strtoul( argv[i], NULL, 10 );
	  key = (key_t)( ProcessID );
	  shmflg = 0660;  /*  Read/Write Owner/Group  */
	  shmid = shmget( key, 0, shmflg );  /*  unique key, size, read/write user/group.  */
	  if ( shmid < 0 ) {
	    perror( "rmid-e1 system error:\n" );
	    exit( -1 );
	  }
	  fprintf(stderr, "Removing shmid %d (pid %d)\n", shmid, ProcessID );
	  shmctl( shmid, IPC_RMID, NULL );
	}
	exit(0);
}
