/*
 *  quest.h  --
 *	Map the /QUEST/ common block.
 *
 *  Original: 21-Oct-1994 17:29
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: c_quest.h,v 1.1 1999/02/16 15:45:03 fisyak Exp $
 *
 *  $Log: c_quest.h,v $
 *  Revision 1.1  1999/02/16 15:45:03  fisyak
 *  add kuip stuff
 *
 *  Revision 1.2  1996/04/23 18:37:53  maartenb
 *  - Add RCS keywords
 *
 *
 */

#ifndef CERN_QUEST
#define CERN_QUEST

#include	"cfortran.h"


typedef struct {
	int	iquest[100];
} quest_def;

#define QUEST COMMON_BLOCK(QUEST,quest)

COMMON_BLOCK_DEF(quest_def,QUEST);


#endif	/*	CERN_QUEST	*/
