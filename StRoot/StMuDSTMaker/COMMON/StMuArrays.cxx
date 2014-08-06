/***************************************************************************
 *
 * $Id: StMuArrays.cxx,v 1.5 2002/05/20 17:23:31 laue Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 **************************************************************************/
#include "StMuArrays.h"


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


















