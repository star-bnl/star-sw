/***************************************************************************
 *
 * $Id: StMuArrays.cxx,v 1.3 2002/04/11 15:45:44 laue Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 **************************************************************************/
#include "StMuArrays.h"


char* StMuArrays::strangeArrayNames[__NSTRANGEARRAYS__] = {"Event","McEvent",
							       "V0","McV0","V0Assoc",
							       "Xi","McXi","XiAssoc",
							       "Kink","McKink","KinkAssoc"};
char* StMuArrays::strangeArrayTypes[__NSTRANGEARRAYS__] = {"StStrangeEvMuDst","StStrangeEvMuDst",
							       "StV0MuDst","StV0Mc","StStrangeAssoc",
							       "StXiMuDst","StXiMc","StStrangeAssoc",
							       "StKinkMuDst","StKinkMc","StStrangeAssoc"};
int StMuArrays::strangeArraySizes[__NSTRANGEARRAYS__]       = {1,1,10000,100,100,10000,100,100,10000,100,100};
int StMuArrays::strangeArrayCounters[__NSTRANGEARRAYS__]    = {0,0,0,0,0,0,0,0,0,0,0};


char* StMuArrays::arrayNames[__NARRAYS__] = {"MuEvent",
						 "PrimaryTracks","GlobalTracks","OtherTracks","L3Tracks",
                                                 "RichSpectra","DetectorStates","L3AlgoAccept","L3AlgoReject"};
char* StMuArrays::arrayTypes[__NARRAYS__] = {"StMuEvent",
						 "StMuTrack","StMuTrack","StMuTrack","StMuTrack",
						 "StRichSpectra","StDetectorState","StL3AlgorithmInfo","StL3AlgorithmInfo"};
int StMuArrays::arraySizes[__NARRAYS__]       = {1,10000,10000,10000,10000,100,100,100,100};
int StMuArrays::arrayCounters[__NARRAYS__]    = {0,0,0,0,0,0,0,0,0};

/***************************************************************************
 *
 * $Log: StMuArrays.cxx,v $
 * Revision 1.3  2002/04/11 15:45:44  laue
 * array size changed back, was crashing program
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


















