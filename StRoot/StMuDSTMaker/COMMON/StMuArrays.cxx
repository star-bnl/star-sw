/***************************************************************************
 *
 * $Id: StMuArrays.cxx,v 1.7 2004/04/02 03:24:53 jeromel Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 **************************************************************************/
#include "StMuArrays.h"

// Pmd
char* StMuArrays::pmdArrayNames[__NPMDARRAYS__]  = {"PmdCollection"};
char* StMuArrays::pmdArrayTypes[__NPMDARRAYS__]  = {"StMuPmdCollection"};
int StMuArrays::pmdArraySizes[__NPMDARRAYS__]    = {1};
int StMuArrays::pmdArrayCounters[__NPMDARRAYS__] = {0};

// Tofr
char* StMuArrays::tofArrayNames[__NTOFARRAYS__]  = {"TofHit","TofData"};
char* StMuArrays::tofArrayTypes[__NTOFARRAYS__]  = {"StMuTofHit","StTofData"};
int StMuArrays::tofArraySizes[__NTOFARRAYS__]    = {100, 200};
int StMuArrays::tofArrayCounters[__NTOFARRAYS__] = {0, 0};

//**************************************************************************
char* StMuArrays::emcArrayNames[__NEMCARRAYS__]  = {"EmcCollection"};
char* StMuArrays::emcArrayTypes[__NEMCARRAYS__]  = {"StMuEmcCollection"};
int StMuArrays::emcArraySizes[__NEMCARRAYS__]    = {1};
int StMuArrays::emcArrayCounters[__NEMCARRAYS__] = {0};
//**************************************************************************
char* StMuArrays::strangeArrayNames[__NSTRANGEARRAYS__] = {"Event","McEvent",
							   "V0","McV0","V0Assoc",
							   "Xi","McXi","XiAssoc",
							   "Kink","McKink","KinkAssoc",
							   "StrangeCuts"};
char* StMuArrays::strangeArrayTypes[__NSTRANGEARRAYS__] = {"StStrangeEvMuDst","StStrangeEvMuDst",
							   "StV0MuDst","StV0Mc","StStrangeAssoc",
							   "StXiMuDst","StXiMc","StStrangeAssoc",
							   "StKinkMuDst","StKinkMc","StStrangeAssoc",
							   "TCut"};
int StMuArrays::strangeArraySizes[__NSTRANGEARRAYS__]       = {1,1,50000,100,100,50000,100,100,50000,100,100,200};
int StMuArrays::strangeArrayCounters[__NSTRANGEARRAYS__]    = {0,0,0,0,0,0,0,0,0,0,0,0};
//**************************************************************************
char* StMuArrays::arrayNames[__NARRAYS__] = {"MuEvent",
					     "PrimaryTracks","GlobalTracks","OtherTracks","L3Tracks",
					     "RichSpectra","DetectorStates","L3AlgoAccept","L3AlgoReject"};
char* StMuArrays::arrayTypes[__NARRAYS__] = {"StMuEvent",
					     "StMuTrack","StMuTrack","StMuTrack","StMuTrack",
					     "StRichSpectra","StDetectorState","StL3AlgorithmInfo","StL3AlgorithmInfo"};
int StMuArrays::arraySizes[__NARRAYS__]       = {1,50000,50000,50000,50000,100,100,100,100};
int StMuArrays::arrayCounters[__NARRAYS__]    = {0,0,0,0,0,0,0,0,0};

/***************************************************************************
 *
 * $Log: StMuArrays.cxx,v $
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


















