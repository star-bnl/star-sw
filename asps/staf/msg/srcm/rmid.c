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
	int shmid;
	for (i=1; i<argc; i++) {
	  shmid = strtoul( argv[i], NULL, 10 );
	  printf( "Removing shmid %d\n", shmid );
	  shmctl( shmid, IPC_RMID, NULL );
	}
	exit(0);
}
