/* Copyright 1993, Lawrence Berkeley Laboratory */
 
/* dserror.c - error handler routines */

/*
modification history
--------------------
01a,10aug93,whg  written.
*/

/*
DESCRIPTION
TBS ...
*/
#include <stdio.h>
#include <stddef.h>
#include "dscodes.h"
#define DS_PRIVATE
#include "dstype.h"

/******************************************************************************
*
* dsClearError - set error code to DS_E_OK
*
*/
int dsClearError()
{
	DS_LOG_ERROR(DS_E_OK);
	return TRUE;
}
/******************************************************************************
*
* dsErrorCode - return code for last error
*
*/
int dsErrorCode()
{
	return dsErrorLogger(0, NULL, NULL, 0);
}
/******************************************************************************
*
* dsErrorLogger - log, print or return error code
*
*/
int dsErrorLogger(int code, char *msg, char *file, int line)
{
	static struct {
		int pid;
		int code;
		char *file;
		int line;
		char *msg;
	}errInfo[2], *err = NULL;

	/* need to find thread for VxWorks */
	if (err == NULL) {
		err = errInfo;
		err->code = 0;
		err->file = "<noFile>";
		err->line = 0;
		err->msg = "DS_E_OK";
	}

	if (line != 0) {
		err->code = code;
		err->msg = msg;
		err->file = file;
		err->line = line;
	}
	else if (msg != NULL) {
#ifdef VXWORKS
		printf(
#else
		fprintf(stderr,
#endif
			"%s%s%s - %s %d\n", msg, (*msg == '\0' ? "" : ": "),
			err->msg, err->file, err->line);
	}
	return err->code;
}
/******************************************************************************
*
* dsPerror - print info about last error
*
*/
int dsPerror(char *msg)
{
	if (msg == NULL) {
		msg = "";
	}
	return dsErrorLogger(0, msg, NULL, 0);
}
