/***********************************************************************
** emlLib.c
*/
#ifdef sun4os5pc
#define __BUILTIN_VA_ARG_INCR
#endif /* sun4os5pc */

#include <stdio.h>
#include <stdarg.h>
#include "emlLib.h"

int emlMessage(char *fmt, ...)
{
	int status;

	va_list args;
	va_start(args, fmt);

	status = vfprintf(stdout, fmt, args);

	va_end(args);

	return status;
}

char * emlContext(char *fmt, ...)
{
	int status;

	va_list args;
	va_start(args, fmt);

	vsprintf(eml_context, fmt, args);

	va_end(args);

	return eml_context;
}

