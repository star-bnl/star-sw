/* Copyright 1993, Lawrence Berkeley Laboratory */

/* tcptest.c - unix test for ds via tcp */

/*
modification history
--------------------
01a,29jul93,whg  written.
*/

/*
DESCRIPTION
TBS...
*/
#include <sys/types.h>
#include "tcplib.h"

int dsTestTcp(char *host);
/******************************************************************************
*
*
*/
void main(argc, argv)
int argc;
char **argv;
{
	if (argc != 2) {
		printf("usage: tcptest host\n");
		exit(0);
	}

	dsTestTcp(argv[1]);

}
