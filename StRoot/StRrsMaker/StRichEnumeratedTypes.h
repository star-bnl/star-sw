/****************************************************************
 * $Id: StRichEnumeratedTypes.h,v 1.2 2000/03/17 14:54:26 lasiuk Exp $
 *
 * Description:
 *   Enumerated type definitions
 *
 ****************************************************************
 *
 * $Log: StRichEnumeratedTypes.h,v $
 * Revision 1.2  2000/03/17 14:54:26  lasiuk
 * Large scale revisions after ROOT dependent memory leak
 *
 * Revision 1.2  2000/03/17 14:54:26  lasiuk
 * Large scale revisions after ROOT dependent memory leak
 *
 * Revision 1.1  2000/02/29 18:14:09  lasiuk
 * Initial Revision
 *
 ****************************************************************/
#ifndef ST_RICH_ENUMERATED_TYPES_H
#define ST_RICH_ENUMERATED_TYPES_H

//
// Bit operations for the cluster finder
//
enum StRichSinglePixelFlag {eUsed=1, eBorder=2, eGood=4, eIsolated=8, eLocalMaximum=16};

//
// MC flags for repsonsible particle
//
enum StRichSignalType {eCharged,ePhoton,eFeedback};
#endif
