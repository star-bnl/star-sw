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
#include <sys/types.h>
#include <time.h>
#define DS_PRIVATE
#include "dstype.h"
#include "sample.h"



static void usage(char *name);

/******************************************************************************
*
* main program for development tests
*
*/
void main(int argc, char **argv)
{
	char *arg, buf[10], *ptr;
	double t, t0;
	int errflag = 1, status = 0;


	if (argc != 2) {
		dsToDo();
		usage(argv[0]);
		printf("enter arg: ");
		gets(buf);
		arg = buf;
	}
	else {
		arg = argv[1];
	}
	if (arg[1] == '\0') {
		errflag = 0;
		t0 = msecTime(&ptr);
		printf("\n%s Start %s test\n", ptr, arg);

		switch(arg[0]) {
			case 'a':
				status = dsTestApi();
				break;

			case 'b':
				status = dsDumpTypes();
				break;
				
			case 'c':
				status = dsTestCorba();
	 		   	break;

			case 'd':
				if (TRUE == (status = dsTestDset())) {
					status = dsTestTree();
				}
				break;

			case 'e':
				status = dsTestErr();
				break;

			case 'f':
				status = xdrReadTest(1);
				break;

			case 'g':
				status = dsTestGraph();
				break;

			case 'j':
				status = dsTestJoin();
				break;

			case 'l':
				status = xdrWriteTest(FALSE);
				break;

			case 'm':
				status = xdrMemTest(TRUE);
				break;

			case 'n':
				status = xdrMemTest(FALSE);
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
				status = xdrWriteTest(TRUE);
				break;

			case 'I':
				status = xdrRandIOTest();
				break;

			case 'R':
				status = xdrRandReadTest();
				break;

			case 'S':
				status = xdrRandSkipTest();
				break;

			case 'W':
				status = xdrRandWriteTest();
				break;

			default:
				errflag = 1;
				break;
		}
	}
	if (!errflag) {
		t = msecTime(&ptr);
		printf("%.12s elapsed time %.3f sec\n", ptr + 11, t - t0);
		testStats();
		printf("\nstatus = %s for %s\n", status ? "TRUE" : "FALSE", arg);
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
static void usage(char *name)
{
	printf("usage: %s optLetter\n", name);
	printf("\ta - dsTestApi\n");
	printf("\tb - dumpBasic\n");
	printf("\tc - testCorba\n");
	printf("\td - dsTestDset\n");
	printf("\te - dsTestErr\n");
	printf("\tf - fast xdrReadTest\n");
	printf("\tg - dsTestGraph\n");
	printf("\tj - joinTest\n");
	printf("\tl - xdrWriteTest little endian\n");
	printf("\tm - dsMemTest big endian\n");
	printf("\tn - dsMemTest little endian\n");
	printf("\tp - projectTest\n");
	printf("\tq - testQuickSort\n");
	printf("\tr - xdrReadTest\n");
	printf("\ts - sample\n");
	printf("\tt - testType\n");
	printf("\tw - xdrWriteTest big endian\n");
	printf("\tI - xdrRandIOTest read of meta data\n");
	printf("\tR - xdrRandReadTest sequential read of rand file\n");
	printf("\tS - xdrRandSkipTest test xdr_dataset_skip\n");
	printf("\tW - xdrRandWriteTest write file for random tests\n");
}
