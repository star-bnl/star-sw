/***************************************************************************
 *
 * $Id: StMuArrays.cxx,v 1.10 2004/04/26 00:13:28 perev Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 **************************************************************************/
#include "StMuArrays.h"
#include "stdio.h"
#include "assert.h"

//		ARRAY NAMES
//============================================================================================
char* StMuArrays::arrayNames       [__NALLARRAYS__    ] = {"MuEvent",
					                   "PrimaryTracks","GlobalTracks","OtherTracks","L3Tracks",
					                   "RichSpectra","DetectorStates","L3AlgoAccept","L3AlgoReject",
/*strangeArrayNames[__NSTRANGEARRAYS__]*/                  "Event","McEvent",
							   "V0","McV0","V0Assoc",
							   "Xi","McXi","XiAssoc",
							   "Kink","McKink","KinkAssoc",
							   "StrangeCuts",
/*emcArrayNames    [__NEMCARRAYS__    ]*/                  "EmcCollection",
/*pmdArrayNames    [__NPMDARRAYS__    ]*/                  "PmdCollection",
/*tofArrayNames    [__NTOFARRAYS__    ]*/                  "TofHit","TofData"};

char** StMuArrays::strangeArrayNames = StMuArrays::arrayNames    +__NARRAYS__;
char** StMuArrays::emcArrayNames = StMuArrays::strangeArrayNames +__NSTRANGEARRAYS__;
char** StMuArrays::pmdArrayNames = StMuArrays::emcArrayNames     +__NEMCARRAYS__;
char** StMuArrays::tofArrayNames = StMuArrays::pmdArrayNames     +__NPMDARRAYS__;


//		ARRAY TYPES
//============================================================================================
char* StMuArrays::arrayTypes       [__NALLARRAYS__    ] = {"StMuEvent",
					                   "StMuTrack","StMuTrack","StMuTrack","StMuTrack",
					                   "StRichSpectra","StDetectorState","StL3AlgorithmInfo","StL3AlgorithmInfo",
/*strangeArrayTypes[__NSTRANGEARRAYS__]*/                  "StStrangeEvMuDst","StStrangeEvMuDst",
							   "StV0MuDst","StV0Mc","StStrangeAssoc",
							   "StXiMuDst","StXiMc","StStrangeAssoc",
							   "StKinkMuDst","StKinkMc","StStrangeAssoc",
							   "TCut",
/*emcArrayTypes   [__NEMCARRAYS__     ]*/                  "StMuEmcCollection",
/*pmdArrayTypes   [__NPMDARRAYS__     ]*/                  "StMuPmdCollection",
/*tofArrayTypes   [__NTOFARRAYS__     ]*/                  "StMuTofHit","StTofData"};
char** StMuArrays::strangeArrayTypes = StMuArrays::arrayTypes    +__NARRAYS__;
char** StMuArrays::emcArrayTypes = StMuArrays::strangeArrayTypes +__NSTRANGEARRAYS__;
char** StMuArrays::pmdArrayTypes = StMuArrays::emcArrayTypes     +__NEMCARRAYS__;
char** StMuArrays::tofArrayTypes = StMuArrays::pmdArrayTypes     +__NPMDARRAYS__;


//		ARRAY SIZES
//============================================================================================
int   StMuArrays::arraySizes       [__NALLARRAYS__    ] = {1,50000,50000,50000,50000,100,100,100,100,
/*strangeArraySizes[__NSTRANGEARRAYS__]*/                  1,1,50000,100,100,50000,100,100,50000,100,100,200,
/*emcArraySizes    [__NEMCARRAYS__    ]*/                  1,
/*pmdArraySizes    [__NPMDARRAYS__    ]*/                  1,
/*tofArraySizes    [__NTOFARRAYS__    ]*/                  100, 200};
int* StMuArrays::strangeArraySizes = StMuArrays::arraySizes    +__NARRAYS__;
int* StMuArrays::emcArraySizes = StMuArrays::strangeArraySizes +__NSTRANGEARRAYS__;
int* StMuArrays::pmdArraySizes = StMuArrays::emcArraySizes     +__NEMCARRAYS__;
int* StMuArrays::tofArraySizes = StMuArrays::pmdArraySizes     +__NPMDARRAYS__;


//		ARRAY COUNTERS
//============================================================================================
int   StMuArrays::arrayCounters       [__NALLARRAYS__ ] = {0,0,0,0,0,0,0,0,0,
/*strangeArrayCounters[__NSTRANGEARRAYS__]*/               0,0,0,0,0,0,0,0,0,0,0,0,
/*emcArrayCounters    [__NEMCARRAYS__    ]*/               0,
/*pmdArrayCounters    [__NPMDARRAYS__    ]*/               0,
/*tofArrayCounters    [__NTOFARRAYS__    ]*/               0, 0};

StMuArrays test;

int* StMuArrays::strangeArrayCounters = StMuArrays::arrayCounters    +__NARRAYS__;
int* StMuArrays::emcArrayCounters = StMuArrays::strangeArrayCounters +__NSTRANGEARRAYS__;
int* StMuArrays::pmdArrayCounters = StMuArrays::emcArrayCounters     +__NEMCARRAYS__;
int* StMuArrays::tofArrayCounters = StMuArrays::pmdArrayCounters     +__NPMDARRAYS__;

StMuArrays::StMuArrays()
{
  int i = strangeArrayNames-arrayNames-__NARRAYS__;
  printf("strangeArrayNames-arrayNames-__NARRAYS__ = %d\n",i);
  assert(i==0);
}











/***************************************************************************
 *
 * $Log: StMuArrays.cxx,v $
 * Revision 1.10  2004/04/26 00:13:28  perev
 * Cleanup+simplification
 *
 * Revision 1.9  2004/04/09 22:06:39  subhasis
 * after tof createevent fix by Xin
 *
 * Revision 1.8  2004/04/09 03:36:14  jeromel
 * Removed TOF support entirely for now as we need a working version ... Will
 * revisit later.
 *
 * Revision 1.7  2004/04/02 03:24:53  jeromel
 * Changes implements PMD and TOF.  TOF is clearly incomplete.
 *
 * Revision 1.6  2003/01/09 18:59:45  laue
 * initial check in of new EMC classes and the changes required
 *
 * Revision 1.5  2002/05/20 17:23:31  laue
 * StStrangeCuts added
 *
 * Revision 1.4  2002/04/15 22:29:28  laue
 * updates
 *
 * Revision 1.2  2002/04/11 14:19:30  laue
 * - update for RH 7.2
 * - decrease default arrays sizes
 * - add data base readerfor number of events in a file
 *
 * Revision 1.1  2002/03/08 17:04:17  laue
 * initial revision
 *
 *
 **************************************************************************/


















