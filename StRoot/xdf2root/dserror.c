/* Copyright 1993, Lawrence Berkeley Laboratory */
 
/* dserror.c - error handler routines */

/*
modification history
--------------------
10aug93,whg  written
24apr95,whg  simple multi-thread version
22jul97,cet  add dsError
*/
/*
DESCRIPTION
error code routines...
*/
#include <stdarg.h>
#include <stddef.h>
#include <string.h>
#define DS_PRIVATE
#include "asuAlloc.h"
#include "dstype.h"
/* error information struct */
typedef struct ds_error_info_t{
	unsigned age;
	DS_ERROR_CODE_T code;
	char *file;
	int line;
	char *msg;
	int pid;
}DS_ERROR_INFO_T;

static DS_ERROR_INFO_T *dsErrorInfo(void);
/******************************************************************************
*
* dsErrorCode
*
* RETURNS: last error code
*/
int dsErrorCode()
{
	return dsErrorInfo()->code;
}
/******************************************************************************
*
* dsErrorInfo - return error area for this thread
*
* RETURNS: pointer to error struct
*/
static DS_ERROR_INFO_T *dsErrorInfo(void)
{
	int i, pid;
	DS_ERROR_INFO_T *pInfo;
	static DS_ERROR_INFO_T errInfo[DS_MAX_ERR], *pLast = NULL;

#ifdef VXWORKS
	/* thread ID for VxWorks */
	pid = taskIdSelf();
#else
	/* constant for UNIX */
	pid = 1;
#endif
	pInfo = pLast;
	if (pInfo == NULL || pInfo->pid != pid) {
		/********* start critical section *********/
		dsErrSemTake();
		if (pLast == NULL) {
			memset(errInfo, 0, sizeof(errInfo));
		}
		/* find error structure for this thread */
		for (i = 0, pInfo = NULL; i < DS_MAX_ERR; i++) {
			errInfo[i].age++;
			if (pid == errInfo[i].pid) {
				if(pInfo != NULL) {
					dsErrorPrint("dsErrorInfo: corrupt structure");
					dsErrorPrint(" - %s(%d)\n", __FILE__, __LINE__);
				}
				pInfo = &errInfo[i];
			}
		}
		if (pInfo == NULL) {
			/* recycle oldest error structure */
			for (i = 0, pInfo = errInfo; i < DS_MAX_ERR; i++) {
				if (pInfo->age < errInfo[i].age) {
					pInfo = &errInfo[i];
				}
			}
			pInfo->code = DS_E_OK;
			pInfo->file = __FILE__;
			pInfo->line = __LINE__;
			pInfo->msg = "DS_E_OK";
			pInfo->pid = pid;
		}
		pInfo->age = 0;
		pLast = pInfo;
		dsErrSemGive();
		/********** end critical section **********/
	}
	return pInfo;
}
/******************************************************************************
*
* dsErrorPrint - print error message
*
* RETURN: number of characters written or a negative value if an error occurs
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
* dsLogError - log error info
*
* RETURNS: none
*/
void dsLogError(DS_ERROR_CODE_T code, char *msg, char *file, size_t line)
{
	DS_ERROR_INFO_T *pInfo;

	pInfo = dsErrorInfo();
	pInfo->code = code;
	pInfo->msg = (msg == NULL) ? "<nullMsg>" : msg;
	pInfo->file = (file == NULL) ? "<noFile>" : file;
	pInfo->line = line;
	return;
}
/**********************************************************************
*
* dsPerror - print info about last error
*
* RETURNS: none
*/
void dsPerror(const char *str)
{
	DS_ERROR_INFO_T *pInfo;

	pInfo = dsErrorInfo();
	if (str != NULL && *str != '\0') {
		dsErrorPrint("%s: ", str); 
	}
	dsErrorPrint("%s - %s(%d)\n", pInfo->msg, pInfo->file, pInfo->line);
}
/**********************************************************************
*
* dsError - return info string about last error
*
* RETURNS: none
*/
const char * dsError(const char *str)
{
	static char b[1024];
	char * buff=b;
/*xxx	char * s;					*/
	DS_ERROR_INFO_T *pInfo;

	pInfo = dsErrorInfo();
	if (str != NULL && *str != '\0') {
		dsErrorPrint("%s: ", str); 
	}
	sprintf(buff,"%s - %s(%d)\n", pInfo->msg, pInfo->file
			, pInfo->line);
	return buff;
/*xxx	s = (char *)MALLOC(strlen(buff)+1);		*/
/*xxx	strcpy(s,buff);					*/
/*xxx	return s;					*/
}
