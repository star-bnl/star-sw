/* Copyright 1993, Lawrence Berkeley Laboratory */

/* testds.c - test routines to support development of dslib */

/*
modification history
--------------------
01aug93,whg  written.
*/

/*
DESCRIPTION
main program to run test code for dsl
*/
#include <stdlib.h>
#define DS_PRIVATE
#include "dstype.h"
#include "sample.h"

int dsTestType(void);
int dsTestCorba(void);
int dsTestErr(void);
int dsTestMisc(void);
int dsTestDset(void);
int dsTestTree(void);
int dsTestJoin(void);
int projectTest(void);
int testSort();
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
void main(int argc, char **argv)
{
	char buf[10], *ptr;
	int errflag = 1, status = 0;


	if (argc != 2) {
		dsToDo();
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
				printf("no a/n");
				break;

			case 'b':
				status = dsDumpTypes();
				break;
				
			case 'c':
				status = dsTestCorba();
	 		   	break;

			case 'd':
				status = dsTestDset();
				status = dsTestTree();
				break;

			case 'e':
				status = dsTestErr();
				break;

			case 'f':
				status = xdrReadTest(1);
				break;
				
			case 'j':
				status = dsTestJoin();
				break;

			case 'm':
				status = xdrMemTest();
				break;
				
			case 'p':
				status = projectTest();
				break;
	
			case 'q':
				status = testSort();
				break;
	
			case 'r':
				status = xdrReadTest(0);
				break;

			case 's':
				status = sample();
				break;
				
			case 't':
				status = dsTestType();
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
	printf("usage: %s [a | b | c | d | f | j | m | p | q | r | s| t | w]\n", name);
	printf("\ta - reserved\n");
	printf("\tb - dumpBasic\n\tc - testCorba\n\td - dsTestDset\n");
	printf("\te - dsTestErr\n\tf - fast xdrReadTest\n\tj - joinTest\n");
	printf("\tm - dsMemTest\n\tp - projectTest\n\tq - testQuickSort\n");
	printf("\tr - xdrReadTest\n");
	printf("\ts - sample\n\tt - testType\n\tw - xdrWriteTest\n");
}
