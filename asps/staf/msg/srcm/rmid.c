#define TRUE -1
#define FALSE 0

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <msg.h>

extern int errno;

void main( int argc, char*argv[] )

{
	int i;
	int ProcessID;

	for (i=1; i<argc; i++) {
	  ProcessID = strtoul( argv[i], NULL, 10 );
	  fprintf(stderr, "rmid-I1  Removing Shared Memory Segment (pid %d)\n", ProcessID );
	  MsgRemoveSharedMemory( ProcessID );
	}
	exit(0);
}
