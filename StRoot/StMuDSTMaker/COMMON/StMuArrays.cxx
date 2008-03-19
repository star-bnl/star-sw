/***************************************************************************
 *
 * $Id: StMuArrays.cxx,v 1.17 2008/03/19 14:51:03 fisyak Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 **************************************************************************/
#include "StMuArrays.h"
#include "stdio.h"
#include "assert.h"

//		ARRAY NAMES
//============================================================================================
const char* StMuArrays::arrayNames [__NALLARRAYS__    ] = {"MuEvent","PrimaryVertices",
					                   "PrimaryTracks","GlobalTracks","OtherTracks","L3Tracks",
					                   "RichSpectra","DetectorStates","L3AlgoAccept","L3AlgoReject",
							   "CovGlobTrack","CovPrimTrack",
/*strangeArrayNames[__NSTRANGEARRAYS__]*/                  "Event","McEvent",
							   "V0","McV0","V0Assoc",
							   "Xi","McXi","XiAssoc",
							   "Kink","McKink","KinkAssoc",
							   "StrangeCuts",
/*emcArrayNames    [__NEMCARRAYS__    ]*/                  "EmcTow",
                                                           "EmcPrs","EmcSmde",
                                                           "EmcSmdp",
                                                           "EEmcPrs","EEmcSmdu","EEmcSmdv",
/*pmdArrayNames    [__NPMDARRAYS__    ]*/                  "PmdHit","CpvHit",
							   "PmdCluster",
							   "CpvCluster",
/*tofArrayNames    [__NTOFARRAYS__    ]*/                  "TofHit","TofData",
							   "TofRawData",
/*eztArrayNames    [__NEZTARRAYS__    ]*/                  "EztHead","EztTrig",
							   "EztETow","EztESmd","EztFpd"};

const char** StMuArrays::strangeArrayNames = StMuArrays::arrayNames    +__NARRAYS__;
const char** StMuArrays::emcArrayNames = StMuArrays::strangeArrayNames +__NSTRANGEARRAYS__;
const char** StMuArrays::pmdArrayNames = StMuArrays::emcArrayNames     +__NEMCARRAYS__;
const char** StMuArrays::tofArrayNames = StMuArrays::pmdArrayNames     +__NPMDARRAYS__;
const char** StMuArrays::eztArrayNames = StMuArrays::tofArrayNames     +__NTOFARRAYS__;

//		ARRAY TYPES
//============================================================================================
const char* StMuArrays::arrayTypes [__NALLARRAYS__    ] = {"StMuEvent","StMuPrimaryVertex",
					                   "StMuTrack","StMuTrack","StMuTrack","StMuTrack",
					                   "StRichSpectra","StDetectorState","StL3AlgorithmInfo","StL3AlgorithmInfo",
							   "StDcaGeometry","StMuPrimaryTrackCovariance",
/*strangeArrayTypes[__NSTRANGEARRAYS__]*/                  "StStrangeEvMuDst","StStrangeEvMuDst",
							   "StV0MuDst","StV0Mc","StStrangeAssoc",
							   "StXiMuDst","StXiMc","StStrangeAssoc",
							   "StKinkMuDst","StKinkMc","StStrangeAssoc",
							   "TCut",
/*emcArrayTypes   [__NEMCARRAYS__     ]*/                  "StMuEmcTowerData","StMuEmcHit",
							   "StMuEmcHit","StMuEmcHit","StMuEmcHit","StMuEmcHit","StMuEmcHit",
/*pmdArrayTypes   [__NPMDARRAYS__     ]*/                  "StMuPmdHit","StMuPmdHit","StMuPmdCluster","StMuPmdCluster",
/*tofArrayTypes   [__NTOFARRAYS__     ]*/                  "StMuTofHit","StTofData","StTofRawData",
/*eztArrayTypes   [__NEZTARRAYS__     ]*/                  "EztEventHeader","EztTrigBlob","EztEmcRawData","EztEmcRawData","EztFpdBlob"};
const char** StMuArrays::strangeArrayTypes = StMuArrays::arrayTypes    +__NARRAYS__;
const char** StMuArrays::emcArrayTypes = StMuArrays::strangeArrayTypes +__NSTRANGEARRAYS__;
const char** StMuArrays::pmdArrayTypes = StMuArrays::emcArrayTypes     +__NEMCARRAYS__;
const char** StMuArrays::tofArrayTypes = StMuArrays::pmdArrayTypes     +__NPMDARRAYS__;
const char** StMuArrays::eztArrayTypes = StMuArrays::tofArrayTypes     +__NTOFARRAYS__;

//		ARRAY SIZES
//============================================================================================
// These are intial sizes. Automatically resized if too small.
// Choosing too large initial values gives a performance penalty when reading 
// only selected MuDst branches 
int   StMuArrays::arraySizes       [__NALLARRAYS__    ] = {1,10,1000,1000,1000,1000,100,100,100,100, 1000,1000,
/*strangeArraySizes[__NSTRANGEARRAYS__]*/                  1,1,1000,100,100,1000,100,100,1000,100,100,200,
/*emcArraySizes    [__NEMCARRAYS__    ]*/                  1,1000,1000,1000,1000,1000,1000,
/*pmdArraySizes    [__NPMDARRAYS__    ]*/                  1000,1000,1000,1000,
/*tofArraySizes    [__NTOFARRAYS__    ]*/                  100, 200, 1000,
/*eztArraySizes    [__NEZTARRAYS__    ]*/                  1, 1, 1, 1, 1};
int* StMuArrays::strangeArraySizes = StMuArrays::arraySizes    +__NARRAYS__;
int* StMuArrays::emcArraySizes = StMuArrays::strangeArraySizes +__NSTRANGEARRAYS__;
int* StMuArrays::pmdArraySizes = StMuArrays::emcArraySizes     +__NEMCARRAYS__;
int* StMuArrays::tofArraySizes = StMuArrays::pmdArraySizes     +__NPMDARRAYS__;
int* StMuArrays::eztArraySizes = StMuArrays::tofArraySizes     +__NTOFARRAYS__;


//		ARRAY COUNTERS
//============================================================================================
int   StMuArrays::arrayCounters       [__NALLARRAYS__ ] = {0,0,0,0,0,0,0,0,0,0, 0,0,
/*strangeArrayCounters[__NSTRANGEARRAYS__]*/               0,0,0,0,0,0,0,0,0,0,0,0,
/*emcArrayCounters    [__NEMCARRAYS__    ]*/               0,0,0,0,0,0,0,
/*pmdArrayCounters    [__NPMDARRAYS__    ]*/               0,0,0,0,
/*tofArrayCounters    [__NTOFARRAYS__    ]*/               0, 0, 0,
/*eztArrayCounters    [__NEZTARRAYS__    ]*/               0, 0, 0, 0, 0};

StMuArrays test;

int* StMuArrays::strangeArrayCounters = StMuArrays::arrayCounters    +__NARRAYS__;
int* StMuArrays::emcArrayCounters = StMuArrays::strangeArrayCounters +__NSTRANGEARRAYS__;
int* StMuArrays::pmdArrayCounters = StMuArrays::emcArrayCounters     +__NEMCARRAYS__;
int* StMuArrays::tofArrayCounters = StMuArrays::pmdArrayCounters     +__NPMDARRAYS__;
int* StMuArrays::eztArrayCounters = StMuArrays::tofArrayCounters     +__NTOFARRAYS__;

StMuArrays::StMuArrays()
{
  int i = strangeArrayNames-arrayNames-__NARRAYS__;
  printf("strangeArrayNames-arrayNames-__NARRAYS__ = %d\n",i);
  assert(i==0);
}











/***************************************************************************
 *
 * $Log: StMuArrays.cxx,v $
 * Revision 1.17  2008/03/19 14:51:03  fisyak
 * Add two clone arrays for global and primary track covariance matrices, remove mSigmaDcaD and mSigmaDcaZ
 *
 * Revision 1.16  2005/07/15 21:45:08  mvl
 * Added support for multiple primary vertices (StMuPrimaryVertex). Track Dcas are now calculated with repect to the first vertex in the list (highest rank), but another vertex number can be specified. Tarcks also store the index of the vertex they belong to (StMuTrack::vertexIndex())
 *
 * Revision 1.15  2005/04/12 21:56:29  mvl
 * Changes by Xin Dong for year-5 TOF data format: extra TClonesArray and routines to fill it from StEvent (StTofRawData).
 *
 * Revision 1.14  2004/11/29 15:53:21  mvl
 * Additions by Jan for Fpd ezTree
 *
 * Revision 1.13  2004/10/28 00:11:32  mvl
 * Added stuff to support ezTree mode of MuDstMaker.
 * This is a special mode for fast-online processing of fast-detector data.
 *
 * Revision 1.12  2004/10/19 01:43:05  mvl
 * Changes for splitting Emc and Pmd collections
 *
 * Revision 1.11  2004/05/04 00:10:28  perev
 * Cleanup
 *
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


















