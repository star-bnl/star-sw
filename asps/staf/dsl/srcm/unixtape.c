/* Copyright 1995, Lawrence Berkeley Laboratory */

/* unixtest.c unix test program for xdrtape */

#ifndef WIN32
/*
modification history
--------------------
01a,30jan95,whg  written
01b,23apr96,bem  fix read,write declarations for AIX
*/

#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "dsxdr.h"
#include "xdrtape.h"

#define DATASET_COUNT 200
#define BLOCK_SIZE 0

/* #define DISK_DEBUG */

#ifndef _MSC_VER	/* if NOT WINNT visual C++ */
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#ifndef _AIX           /* if NOT ibm aix */ 
int read(int fd, char *buf, unsigned count);
int write(int fd, char *buf, unsigned count);
#endif /* aix */
#define RD_FLAG O_RDONLY
#define WR_DISK_FLAG  O_RDWR  | O_CREAT | O_TRUNC
#define WR_DISK_MODE S_IRWXU

#else  /* _MSC_VER */
#include <io.h>
#include <sys/stat.h>
#define RD_FLAG O_RDONLY | O_BINARY
#define WR_DISK_FLAG O_RDWR  | O_CREAT | O_TRUNC | O_BINARY
#define WR_DISK_MODE S_IWRITE
#endif /* _MSC_VER */

static void usage(void);
/*****************************************************************************
*
* main - test program for xdrtape
*
*/
void main(int argc, char **argv)
{
	int fd;
	XDR xdr;

	if (argc != 3) {
		usage();
		exit(0);
	}
	if (strcmp(argv[1], "-w") == 0) {
		if (strncmp(argv[2], "/dev/", 5) == 0) {
			fd = open(argv[2], O_RDWR, 0);
		}
		else {
			fd = open(argv[2], WR_DISK_FLAG, WR_DISK_MODE);
		}
		if (fd < 0) {
			perror("open failed for write ");
			exit(0);
		}
		if (!xdrtape_create(&xdr, XDR_ENCODE, fd, BLOCK_SIZE, write)) {
			printf("xdrTape_create failed\n");
			exit(0);
		}
		if (!dsWriteTest(&xdr, DATASET_COUNT)) {
			xdrtape_perror(&xdr, "tape status");
			dsPerror("dsWriteTest failed");
		}
		if (!xdrtape_flush(&xdr)) {
			xdrtape_perror(&xdr, "writeTest: flush failed");
		}
		printf("write successful, %d bytes\n", xdr_getpos(&xdr));
		xdr_destroy(&xdr);
		close(fd);
		exit(0);
	}
	if ((fd = open(argv[2], RD_FLAG, 0)) < 0) {
		perror("open failed for read");
		exit(0);
	}
	if (!xdrtape_create(&xdr, XDR_DECODE, fd, BLOCK_SIZE, read)) {
		printf("xdrTape_create failed\n");
		exit(0);
	}
	if (strcmp(argv[1], "-r") == 0) {
		if (!dsReadTest(&xdr, DATASET_COUNT)) {
			xdrtape_perror(&xdr, "tape status");
			dsPerror("dsReadTest failed");
		}
		printf("read check successful, %d bytes\n", xdr_getpos(&xdr));
		xdr_destroy(&xdr);
		close(fd);
		exit(0);
	}
	else if (strcmp(argv[1], "-a") == 0) {
		dsReadAll(&xdr);
		xdrtape_perror(&xdr, "tape status");
		xdr_destroy(&xdr);
		close(fd);
		exit(0);
	}
	usage();
}
#ifdef MTIO_OK		/* if build mtio routines */
#include <mtio.h>
/*****************************************************************************
*
* mtRewind - rewind tape
*
*/
int mtRewind(int fd)
{
	struct mtop mtop;

	mtop.mt_op = MTREW;
	mtop.mt_count = 1;
	return  (ioctl(fd, MTIOCTOP, &mtop) < 0) ? FALSE : TRUE;
}
/*****************************************************************************
*
* mtWriteEOF - write tape mark
*
*/
int mtWriteEOF(int fd)
{
	struct mtop mtop;

	mtop.mt_op = MTWEOF;
	mtop.mt_count = 1;
	return  (ioctl(fd, MTIOCTOP, &mtop) < 0) ? FALSE : TRUE;
}
#endif /* MTIO_OK */
/*****************************************************************************
*
* usage - print usage message and exit
*
*/
static void usage(void)
{
	printf("usage:\tunixtape -a | -r | -w device\n");
	printf("\ta - read tape and print dataset types\n");
	printf("\tr - read tape and check datasets\n");
	printf("\tw - write known datasets on tape\n");
	exit(0);
}

#endif /* WIN32 */
