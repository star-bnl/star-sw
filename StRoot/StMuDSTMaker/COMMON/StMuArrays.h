/***************************************************************************
 *
 * $Id: StMuArrays.h,v 1.1 2002/03/08 17:04:17 laue Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 ***************************************************************************/
#ifndef StMuArrays_hh
#define StMuArrays_hh

enum strangeTypes {smuEv=0, smuEvMc, smuV0, smuV0mc, smuV0Assoc, smuXi, smuXiMc, smuXiAssoc, smuKink, smuKinkMc, smuKinkAssoc};
enum muDstTypes {muEvent=0, muPrimary, muGlobal, muOther, muL3, muRich, muState, muAccept, muReject}; 

/* #define __MAX_Events__ 1 */
/* #define __MAX_Tracks__ 10000 */

/* #define __MAX_StRichSpectra__ 100 */
/* #define __MAX_StDetectorState__ 100 */
/* #define __MAX_StL3AlgorithmInfo__ 100 */

/* #define __MAX_StStrangeEvMuDst__ 1 */
/* #define __MAX_StV0MuDst__ 10000 */
/* #define __MAX_StV0Mc__ 10000 */
/* #define __MAX_StXiMuDst__ 10000 */
/* #define __MAX_StXiMc__ 10000 */
/* #define __MAX_StKinkMuDst__ 100 */
/* #define __MAX_StKinkMc__ 100 */
/* #define __MAX_StStrangeAssoc__ 100 */

#define __NARRAYS__ 9
#define __NSTRANGEARRAYS__ 11

class StMuArrays {
 public:
  static char* arrayNames[__NARRAYS__];
  static char* arrayTypes[__NARRAYS__];
  static int arraySizes[__NARRAYS__];
  static int arrayCounters[__NARRAYS__];
  
  static char* strangeArrayNames[__NSTRANGEARRAYS__];
  static char* strangeArrayTypes[__NSTRANGEARRAYS__];
  static int strangeArrayCounters[__NSTRANGEARRAYS__];
  static int strangeArraySizes[__NSTRANGEARRAYS__];
};

#endif

/***************************************************************************
 *
 * $Log: StMuArrays.h,v $
 * Revision 1.1  2002/03/08 17:04:17  laue
 * initial revision
 *
 *
 **************************************************************************/
