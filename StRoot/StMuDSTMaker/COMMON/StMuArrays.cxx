/***************************************************************************
 *
 * $Id: StMuArrays.cxx,v 1.35 2019/02/21 13:32:54 jdb Exp $
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
							   "CovGlobTrack","CovPrimTrack","pp2pp","mtd",
#ifndef __NO_STRANGE_MUDST__
/*strangeArrayNames[__NSTRANGEARRAYS__]*/                  "Event","McEvent",
							   "V0","McV0","V0Assoc",
							   "Xi","McXi","XiAssoc",
							   "Kink","McKink","KinkAssoc",
							   "StrangeCuts",
#endif
/*mcArrayNames[__NMCARRAYS__]*/                            "StMuMcVertex","StMuMcTrack",
/*emcArrayNames    [__NEMCARRAYS__    ]*/                  "EmcTow",
                                                           "EmcPrs","EmcSmde",
                                                           "EmcSmdp",
                                                           "EEmcPrs","EEmcSmdu","EEmcSmdv",
/*pmdArrayNames    [__NPMDARRAYS__    ]*/                  "PmdHit","CpvHit", "PmdCluster", "CpvCluster",
/*fmsArrayNames    [__NFMSARRAYS__    ]*/                  "FmsHit","FmsCluster","FmsPoint","FmsInfo",
/*tofArrayNames    [__NTOFARRAYS__    ]*/                  "TofHit","TofData", "TofRawData",
/*btofArrayNames   [__NBTOFARRAYS__   ]*/                  "BTofHit","BTofRawHit","BTofHeader", // dongx
/*etofArrayNames   [__NETOFARRAYS__   ]*/                  "ETofDigi","ETofHit","ETofHeader",   // jdb
/*epdArrayNames [__NEPDARRAYS ]       */                   "EpdHit",  // MALisa
/*mtdArrayNames    [__NMTDARRAYS__    ]*/                  "MTDHit","MTDRawHit","MTDHeader",
/*fgtArrayNames    [__NFGTARRAYS__    ]*/                  "FgtStrip","FgtCluster","FgtStripAssociation","FgtAdc",
    /*eztArrayNames    [__NEZTARRAYS__    ]*/              "EztHead","EztTrig","EztETow","EztESmd","EztFpd"};
                                                            
#ifndef __NO_STRANGE_MUDST__
const char** StMuArrays::strangeArrayNames = StMuArrays::arrayNames   +__NARRAYS__;
const char** StMuArrays::mcArrayNames = StMuArrays::strangeArrayNames +__NSTRANGEARRAYS__;
#else
const char** StMuArrays::mcArrayNames = StMuArrays::arrayNames        +__NARRAYS__;
#endif
const char** StMuArrays::emcArrayNames = StMuArrays::mcArrayNames     +__NMCARRAYS__;
const char** StMuArrays::pmdArrayNames = StMuArrays::emcArrayNames    +__NEMCARRAYS__;
const char** StMuArrays::fmsArrayNames = StMuArrays::pmdArrayNames    +__NPMDARRAYS__;
const char** StMuArrays::tofArrayNames = StMuArrays::fmsArrayNames    +__NFMSARRAYS__;
const char** StMuArrays::btofArrayNames = StMuArrays::tofArrayNames   +__NTOFARRAYS__;  // dongx
const char** StMuArrays::etofArrayNames = StMuArrays::btofArrayNames  +__NBTOFARRAYS__; // jdb
const char** StMuArrays::epdArrayNames  = StMuArrays::etofArrayNames  +__NETOFARRAYS__; // MALisa
const char** StMuArrays::mtdArrayNames = StMuArrays::epdArrayNames    +__NEPDARRAYS__;
const char** StMuArrays::fgtArrayNames = StMuArrays::mtdArrayNames    +__NMTDARRAYS__;
const char** StMuArrays::eztArrayNames = StMuArrays::fgtArrayNames    +__NFGTARRAYS__; // dongx


//		ARRAY TYPES
//============================================================================================
const char* StMuArrays::arrayTypes [__NALLARRAYS__    ] = {"StMuEvent","StMuPrimaryVertex",
					                   "StMuTrack","StMuTrack","StMuTrack","StMuTrack",
					                   "StRichSpectra","StDetectorState","StL3AlgorithmInfo","StL3AlgorithmInfo",
							   "StDcaGeometry","StMuPrimaryTrackCovariance","StMuRpsCollection","StMuMtdCollection",
#ifndef __NO_STRANGE_MUDST__
/*strangeArrayTypes[__NSTRANGEARRAYS__]*/                  "StStrangeEvMuDst","StStrangeEvMuDst",
							   "StV0MuDst","StV0Mc","StStrangeAssoc",
							   "StXiMuDst","StXiMc","StStrangeAssoc",
							   "StKinkMuDst","StKinkMc","StStrangeAssoc",
							   "TCut",
#endif
/*mcArrayTypes[__NMCARRAYS__]*/                            "StMuMcVertex","StMuMcTrack",
/*emcArrayTypes   [__NEMCARRAYS__     ]*/                  "StMuEmcTowerData","StMuEmcHit",
							   "StMuEmcHit","StMuEmcHit","StMuEmcHit","StMuEmcHit","StMuEmcHit",
/*pmdArrayTypes   [__NPMDARRAYS__     ]*/                  "StMuPmdHit","StMuPmdHit","StMuPmdCluster","StMuPmdCluster",
/*fmsArrayTypes   [__NFMSARRAYS__     ]*/                  "StMuFmsHit","StMuFmsCluster","StMuFmsPoint","StMuFmsInfo",
/*tofArrayTypes   [__NTOFARRAYS__     ]*/                  "StMuTofHit","StTofData","StTofRawData",
/*btofArrayTypes  [__NBTOFARRAYS__    ]*/                  "StMuBTofHit","StBTofRawHit","StBTofHeader",  // dongx
/*etofArrayTypes  [__NETOFARRAYS__    ]*/                  "StMuETofDigi","StMuETofHit","StMuETofHeader",  // jdb+fseck
/*epdArrayTypes   [__NEPDARRAYS__     ]*/                  "StMuEpdHit",  // MALisa
/*mtdArrayNames   [__NMTDARRAYS__     ]*/                  "StMuMtdHit","StMuMtdRawHit","StMuMtdHeader",
/*fgtArrayTypes   [__NFGTARRAYS__     ]*/                  "StMuFgtStrip","StMuFgtCluster","StMuFgtStripAssociation","StMuFgtAdc",
/*eztArrayTypes   [__NEZTARRAYS__     ]*/                  "EztEventHeader","EztTrigBlob","EztEmcRawData","EztEmcRawData","EztFpdBlob"};
#ifndef __NO_STRANGE_MUDST__
const char** StMuArrays::strangeArrayTypes = StMuArrays::arrayTypes    +__NARRAYS__;
const char** StMuArrays::mcArrayTypes = StMuArrays::strangeArrayTypes  +__NSTRANGEARRAYS__;
#else
const char** StMuArrays::mcArrayTypes = StMuArrays::arrayTypes         +__NARRAYS__;
#endif
const char** StMuArrays::emcArrayTypes = StMuArrays::mcArrayTypes      +__NMCARRAYS__;
const char** StMuArrays::pmdArrayTypes =  StMuArrays::emcArrayTypes    +__NEMCARRAYS__;
const char** StMuArrays::fmsArrayTypes =  StMuArrays::pmdArrayTypes    +__NPMDARRAYS__;
const char** StMuArrays::tofArrayTypes = StMuArrays::fmsArrayTypes     +__NFMSARRAYS__;
const char** StMuArrays::btofArrayTypes = StMuArrays::tofArrayTypes    +__NTOFARRAYS__;  // dongx
const char** StMuArrays::etofArrayTypes = StMuArrays::btofArrayTypes    +__NBTOFARRAYS__;  // jdb
const char** StMuArrays::epdArrayTypes = StMuArrays::etofArrayTypes    +__NETOFARRAYS__; // MALisa
const char** StMuArrays::mtdArrayTypes = StMuArrays::epdArrayTypes    +__NEPDARRAYS__;  // dongx
const char** StMuArrays::fgtArrayTypes = StMuArrays::mtdArrayTypes     +__NMTDARRAYS__;
const char** StMuArrays::eztArrayTypes = StMuArrays::fgtArrayTypes     +__NFGTARRAYS__;

//		ARRAY SIZES
//============================================================================================
// These are intial sizes. Automatically resized if too small.
// Choosing too large initial values gives a performance penalty when reading 
// only selected MuDst branches 
int   StMuArrays::arraySizes       [__NALLARRAYS__    ] = {1,10,1000,1000,1000,1000,100,100,100,100, 1000,1000,1,1,
#ifndef __NO_STRANGE_MUDST__
/*strangeArraySizes[__NSTRANGEARRAYS__]*/                  1,1,1000,100,100,1000,100,100,1000,100,100,200,
#endif
/*mcArraySizes[__NMCARRAYS__]*/                            10,1000,
/*emcArraySizes    [__NEMCARRAYS__    ]*/                  1,1000,1000,1000,1000,1000,1000,
/*pmdArraySizes    [__NPMDARRAYS__    ]*/                  1000,1000,1000,1000,
/*fmsArraySizes    [__NFMSARRAYS__    ]*/                  1,1,1,1,
/*tofArraySizes    [__NTOFARRAYS__    ]*/                  100, 200, 1000,
/*btofArraySizes   [__NBTOFARRAYS__   ]*/                  1000,1000,1,   // dongx
/*etofArraySizes   [__NETOFARRAYS__   ]*/                  1000,1000,1,   // jdb
/*epdArraySizes    [__NEPDARRAYS__ ] */                    744,  // MALisa
/*mtdArraySizes    [__NMTDARRAYS__    ]*/                  1000,1000,1,
/*fgtArraySizes    [__NFGTARRAYS__    ]*/                  500, 50, 500, 2000,
    /*eztArraySizes    [__NEZTARRAYS__    ]*/                  1, 1, 1, 1, 1};
#ifndef __NO_STRANGE_MUDST__
int* StMuArrays::strangeArraySizes = StMuArrays::arraySizes    +__NARRAYS__;
int* StMuArrays::mcArraySizes = StMuArrays::strangeArraySizes  +__NSTRANGEARRAYS__;
#else
int* StMuArrays::mcArraySizes = StMuArrays::arraySizes         +__NARRAYS__;
#endif
int* StMuArrays::emcArraySizes = StMuArrays::mcArraySizes      +__NMCARRAYS__;
int* StMuArrays::pmdArraySizes = StMuArrays::emcArraySizes     +__NEMCARRAYS__;
int* StMuArrays::fmsArraySizes = StMuArrays::pmdArraySizes     +__NPMDARRAYS__;
int* StMuArrays::tofArraySizes = StMuArrays::fmsArraySizes     +__NFMSARRAYS__;
int* StMuArrays::btofArraySizes = StMuArrays::tofArraySizes    +__NTOFARRAYS__;  // dongx
int* StMuArrays::etofArraySizes = StMuArrays::btofArraySizes   +__NBTOFARRAYS__;  // jdb
int* StMuArrays::epdArraySizes  = StMuArrays::etofArraySizes   +__NETOFARRAYS__;  // MALisa
int* StMuArrays::mtdArraySizes = StMuArrays::epdArraySizes     +__NEPDARRAYS__;  // dongx
int* StMuArrays::fgtArraySizes = StMuArrays::mtdArraySizes     +__NMTDARRAYS__;
int* StMuArrays::eztArraySizes = StMuArrays::fgtArraySizes     +__NFGTARRAYS__;

//		ARRAY COUNTERS
//============================================================================================
int   StMuArrays::arrayCounters       [__NALLARRAYS__ ] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,
#ifndef __NO_STRANGE_MUDST__
/*strangeArrayCounters[__NSTRANGEARRAYS__]*/               0,0,0,0,0,0,0,0,0,0,0,0,
#endif
/*mcArrayCounterss    [__NMCARRAYS__]*/                    0,0,
/*emcArrayCounters    [__NEMCARRAYS__    ]*/               0,0,0,0,0,0,0,
/*pmdArrayCounters    [__NPMDARRAYS__    ]*/               0,0,0,0,
/*fmsArrayCounters    [__NFMSARRAYS__    ]*/               0,0,0,0,
/*tofArrayCounters    [__NTOFARRAYS__    ]*/               0, 0, 0,
/*btofArrayCounters   [__NBTOFARRAYS__   ]*/               0, 0, 0,      // dongx
/*etofArrayCounters   [__NETOFARRAYS__   ]*/               0, 0, 0,      // jdb
/*epdArrayCounters   [__NEPDARRAYS__     ] */              0,   // MALisa
/*mtdArrayCounters    [__NMTDARRAYS__    ]*/               0, 0, 0,
/*fgtArrayCounters    [__NFGTARRAYS__    ]*/               0, 0, 0, 0,
   /*eztArrayCounters    [__NEZTARRAYS__    ]*/            0, 0, 0, 0, 0};

StMuArrays test;
#ifndef __NO_STRANGE_MUDST__
int* StMuArrays::strangeArrayCounters = StMuArrays::arrayCounters        +__NARRAYS__;
int* StMuArrays::mcArrayCounters      = StMuArrays::strangeArrayCounters + __NSTRANGEARRAYS__;
#else
int* StMuArrays::mcArrayCounters  = StMuArrays::arrayCounters            +__NARRAYS__;
#endif
int* StMuArrays::emcArrayCounters = StMuArrays::mcArrayCounters      +__NMCARRAYS__;
int* StMuArrays::pmdArrayCounters = StMuArrays::emcArrayCounters     +__NEMCARRAYS__;
int* StMuArrays::fmsArrayCounters = StMuArrays::pmdArrayCounters     +__NPMDARRAYS__;
int* StMuArrays::tofArrayCounters = StMuArrays::fmsArrayCounters     +__NFMSARRAYS__;
int* StMuArrays::btofArrayCounters = StMuArrays::tofArrayCounters    +__NTOFARRAYS__;  // dongx
int* StMuArrays::etofArrayCounters = StMuArrays::btofArrayCounters   +__NBTOFARRAYS__;  // jdb
int* StMuArrays::epdArrayCounters = StMuArrays::etofArrayCounters    +__NETOFARRAYS__;  // MALisa
int* StMuArrays::mtdArrayCounters = StMuArrays::epdArrayCounters     +__NEPDARRAYS__;
int* StMuArrays::fgtArrayCounters = StMuArrays::mtdArrayCounters     +__NMTDARRAYS__;
int* StMuArrays::eztArrayCounters = StMuArrays::fgtArrayCounters     +__NFGTARRAYS__;  

ClassImp(StMuArrays);
StMuArrays::StMuArrays()
{
#ifndef __NO_STRANGE_MUDST__
  int i = strangeArrayNames-arrayNames-__NARRAYS__;
  printf("strangeArrayNames-arrayNames-__NARRAYS__ = %d\n",i);
#else
  int i = mcArrayNames-arrayNames-__NARRAYS__;
  printf("mcArrayNames-arrayNames-__NARRAYS__ = %d\n",i);
#endif
  assert(i==0);
}











/***************************************************************************
 *
 * $Log: StMuArrays.cxx,v $
 * Revision 1.35  2019/02/21 13:32:54  jdb
 * Inclusion of ETOF MuDst code. This code adds support for the full set of ETOF data which includes EtofDigi, EtofHit, EtofHeader. The code essentially copies similar structures from StEvent and additionally rebuilds the maps between Digis and Hits. Accessor methods are added based on the pattern from BTOF to provide access to data at various levels. The code for accessing the PID traits provided by ETOF is also provided
 *
 * Revision 1.34  2018/02/27 04:11:17  jdb
 * Added EPD types
 *
 * Revision 1.33  2015/11/06 17:47:16  jdb
 * Added StMuFmsInfo.{h,cxx} as a new branch for storing event-by-event FMS paramters
 *
 * Revision 1.32  2015/08/28 18:36:03  jdb
 * Added Akios FMS codes
 *
 * Revision 1.31  2013/07/23 11:02:59  jeromel
 * Undo changes (KF and other)
 *
 * Revision 1.29  2013/04/10 19:28:35  jeromel
 * Step back to 04/04 version (van aware) - previous changes may be recoverred
 *
 * Revision 1.27  2013/01/08 22:57:33  sangalin
 * Merged in FGT changes allowing for a variable number of timebins to be read out for each strip.
 *
 * Revision 1.26  2012/11/15 22:26:13  sangalin
 * Added the FGT. Fixed bugs in array offsets for the MTD.
 *
 * Revision 1.25  2012/09/28 22:38:05  tone421
 * Changed array stucture of MTD upon request of the TOF group. MTD arrays now on top level, rather than within __NARRAYS__
 *
 * Revision 1.24  2011/10/17 00:19:13  fisyak
 * Active handing of IdTruth
 *
 * Revision 1.23  2011/05/04 19:51:31  tone421
 * Added MTD infomation
 *
 * Revision 1.22  2011/04/08 01:25:50  fisyak
 * Add branches for MC track and vertex information, add IdTruth to  tracks and vertices, reserve a possiblity to remove Strange MuDst
 *
 * Revision 1.21  2010/05/26 04:25:50  tone421
 * Added StTriggerData arrays in muevent and fixed an issue with PMD arrays being read....
 *
 * Revision 1.20  2010/01/25 03:57:39  tone421
 * Added FMS and Roman pot arrays
 *
 * Revision 1.19  2009/02/20 16:37:43  tone421
 * *** empty log message ***
 *
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


















