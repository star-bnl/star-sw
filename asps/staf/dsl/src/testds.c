/* Copyright 1993, Lawrence Berkeley Laboratory */
 
/* testds.c - test routines to support development of dslib */

/*
modification history
--------------------
01a,01aug93,whg  written.
*/

/*
DESCRIPTION
TBS ...
*/
#include <stdlib.h>
#include <stdio.h>
#define DS_PRIVATE
#include "dstype.h"
int dsTestAdt(void);
int dsTestErr(void);
int dsTestMisc(void);
int dsTestDset(void);
int dsTestTree(void);
int projectTest(void);
void testStats(void);
void usage(char *name);
int xdrMemTest(void);
int xdrReadTest(int fast);
int xdrWriteTest(void);

/******************************************************************************
*
* main program for development tests
*
*/
void main(argc, argv)
int argc;
char **argv;
{
	char buf[10], *ptr;
	int errflag = 1, status = 0;
    
    if (argc != 2) {
		usage(argv[0]);
    	printf("enter arg: ");
    	gets(buf);
    	ptr = buf;
    }
    else {
		ptr = argv[1];
	}
	if (ptr[1] == '\0') {
		errflag = 0;
		switch(*ptr) {
			case 'a':
				status = dsTestAdt();
				break;

			case 'b':
				status = TRUE;
				printf("NO DEBUG\n");
				break;

			case 'd':
				status = dsTestMisc();
				break;

			case 'e':
				status = dsTestErr();
				break;

			case 'f':
				status = xdrReadTest(1);
				break;

			case 'm':
				status = xdrMemTest();
				break;
				
            case 'p':
            	status = projectTest();
            	break;
            
			case 'r':
				status = xdrReadTest(0);
				break;

			case 's':
				status = dsTestDset();
				break;
			case 't':
				status = dsTestTree();
				break;

			case 'w':
				status = xdrWriteTest();
				break;

			default:
				errflag = 1;
				break;
		}
		
	}
	if (!errflag) {
		testStats();
		printf("\nstatus = %s\n", status ? "TRUE" : "FALSE");
		exit(0);
	}
	else {
		usage(argv[0]);
	}
}
/*****************************************************************************
*
* usage - print program options
*
*/
void usage(char *name)
{
	printf("usage: %s [a | b | d | f | m | p | r | s| t | w]\n", name);
	printf("\ta - testAdt\n");
	printf("\tb - dsBug\n\td - dumpBasic\n\te - dsTestErr\n");
	printf("\tf - fast xdrReadTest\n");
	printf("\tm - dsMemTest\n\tp - projectTest\tr - xdrReadTest\n");
	printf("\ts - dsTestDset\n\tt - testTree\n\tw - xdrWriteTest\n");
}
