/* Copyright 1993, Lawrence Berkeley Laboratory */

/* dslock.c - lock and unlock routines for static structures  */

/*
modification history
--------------------
27jul93,whg  written
25apr95,whg  added error structures
*/

/*
DESCRIPTION
routines for sharing structures in VxWorks or other multi-threaded OS
*/
#include <stdlib.h>
#define DS_PRIVATE
#include "dstype.h"
#ifdef VXWORKS
#include "semLib.h"
SEM_ID errSemID = NULL;
SEM_ID tidSemID = NULL;
#endif
static int errSemVal = 0, nErrTake = 0;
/******************************************************************************
*
* dsErrSemGive - unlock error structures
*
* RETURNS: TRUE if success else FALSE
*/
int dsErrSemGive(void)
{
#ifdef VXWORKS
	if (semGive(tidSemID)) {
#else
	if (--errSemVal){
#endif
		dsErrorPrint("dsErrSemGive FAILED");
		dsErrorPrint(" - %s(%d)\n", __FILE__, __LINE__);
		return FALSE;
	}
	return TRUE;
}
/******************************************************************************
*
* dsErrSemTake - lock error structures
*
* RETURNS: TRUE if success else FALSE
*/
int dsErrSemTake(void)
{
	nErrTake++;
#ifdef VXWORKS
	if (semTake(errSemID, WAIT_FOREVER)) {
#else
	if (errSemVal++){
#endif
		dsErrorPrint("dsErrSemTake FAILED");
		dsErrorPrint(" - %s(%d)\n", __FILE__, __LINE__);
		return FALSE;
	}
	return TRUE;
}
/******************************************************************************
*
* dsSemInit - intialize semaphore for vxWorks or dummy
*
* RETURNS: TRUE if success else FALSE
*/
int dsSemInit(void)
{
#ifdef VXWORKS
	if (errSemID != NULL || tidSemID != NULL ||
		(errSemID = semBCreate(SEM_Q_PRIORITY, SEM_FULL)) == NULL ||
		(tidSemID = semBCreate(SEM_Q_PRIORITY, SEM_FULL)) == NULL) {
		dsErrorPrint("dsSemInit FAILED");
		dsErrorPrint(" - %s(%d)\n", __FILE__, __LINE__);
		return FALSE;
	}
#endif
	return TRUE;
}
