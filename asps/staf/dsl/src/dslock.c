/* Copyright 1993, Lawrence Berkeley Laboratory */

/* dslock.c - lock and unlock routines for static type structures  */

/*
modification history
--------------------
27jul93,whg  written.
*/

/*
DESCRIPTION
routines for sharing structures in VxWorks or other multi-threaded OS
*/
#include <stdlib.h>
#define DS_PRIVATE
#include "dscodes.h"
#include "dstype.h"
#ifdef VXWORKS
#include "semLib.h"

SEM_ID tidSemID = NULL;
#endif
static int semVal = 0, nLock = 0;
/******************************************************************************
*
* dsTypeLock - lock type structures
*
* RETURNS: TRUE if success else FALSE
*/
int dsTypeLock(void)
{
	nLock++;
#ifdef VXWORKS
	if (semTake(tidSemID, WAIT_FOREVER)) {
#else
	if (semVal++){
#endif
		DS_ERROR(DS_E_SEM_TAKE_ERROR);
	}
	return TRUE;
}
/******************************************************************************
*
* dsTypeLockInit - intialize semaphore for vxWorks or dummy
*
* RETURNS: TRUE if success else FALSE
*/
int dsTypeLockInit(void)
{
#ifdef VXWORKS
	if (tidSemID != NULL ||
		(tidSemID = semBCreate(SEM_Q_PRIORITY, SEM_FULL)) == NULL) {
		DS_ERROR(DS_E_SEM_CREATE_FAILED);
	}
#endif
	return TRUE;
}
/******************************************************************************
*
* dsTypeLockStats - print lock statsistics
*
* RETURNS: TRUE
*/
int dsTypeLockStats(void)
{
	printf("lockStats: semVal %d, nLock %d\n", semVal, nLock);
	return TRUE;
}
/******************************************************************************
*
* dsTypeUnock - unlock structures
*
* RETURNS: TRUE if success else FALSE
*/
int dsTypeUnlock(void)
{
#ifdef VXWORKS
	if (semGive(tidSemID)) {
#else
	if (--semVal){
#endif
		DS_ERROR(DS_E_SEM_GIVE_ERROR);
	}
	return TRUE;
}
