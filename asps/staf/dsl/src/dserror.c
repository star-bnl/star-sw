/* Copyright 1993, Lawrence Berkeley Laboratory */
 
/* dserror.c - error handler routines */

/*
modification history
--------------------
10aug93,whg  written.
*/

/*
DESCRIPTION
error code routines...
*/
#include <stdarg.h>
#include <stddef.h>
#include "dscodes.h"
#define DS_PRIVATE
#include "dstype.h"

/******************************************************************************
*
* dsClearError - set error code to DS_E_OK
*
* RETURNS: TRUE
*/
int dsClearError()
{
	DS_LOG_ERROR(DS_E_OK);
	return TRUE;
}
/******************************************************************************
*
* dsErrorCode
*
* RETURNS: last error code
*/
int dsErrorCode()
{
	return dsErrorLogger(0, NULL, NULL, 0);
}
/******************************************************************************
*
* dsErrorLogger - log, print or return error code
*
* RETURNS: last error code
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
		dsErrorPrint("%s%s%s - %s %d\n", msg, (*msg == '\0' ? "" : ": "),
			err->msg, err->file, err->line);
	}
	return err->code;
}
/******************************************************************************
*
*/
 int dsErrorPrint(char *fmt, ...)
 {
 	va_list args;
	va_start(args, fmt);

#ifdef VXWORKS
	return vprintf(fmt, args);
#else
	return vfprintf(stderr, fmt, args);
#endif
}
/******************************************************************************
*
* dsPerror - print info about last error
*
* RETURNS: last error code
*/
int dsPerror(char *msg)
{
	if (msg == NULL) {
		msg = "";
	}
	return dsErrorLogger(0, msg, NULL, 0);
}
