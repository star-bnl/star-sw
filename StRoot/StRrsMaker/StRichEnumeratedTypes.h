/****************************************************************
 * $Id: StRichEnumeratedTypes.h,v 1.4 2000/05/17 22:19:22 lasiuk Exp $
 *
 * Description:
 *   Enumerated type definitions
 *
 ****************************************************************
 *
 * $Log: StRichEnumeratedTypes.h,v $
 * Revision 1.4  2000/05/17 22:19:22  lasiuk
 * noise type
 *
 * Revision 1.3  2000/04/05 15:58:35  lasiuk
 * set bits instead of incremental numbers
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
enum StRichSignalType {eUnknown=0, eCharged=1, ePhoton=2, eFeedback=4, eNoise=8};
#endif
