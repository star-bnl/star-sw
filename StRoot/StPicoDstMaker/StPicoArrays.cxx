#include "StPicoArrays.h"

//              ARRAY NAMES
//============================================================================================
const char* StPicoArrays::picoArrayNames [__NALLPICOARRAYS__] = {"Event","Tracks","EmcTrigger","MtdTrigger",
								 "BTOWHit","BTofHit","MtdHit",
								 "EmcPidTraits","BTofPidTraits","MtdPidTraits",
/*picoV0ArrayNames[__NPICOV0ARRAYS__]*/                          "V0Ks","V0L","V0Lbar"};

const char** StPicoArrays::picoV0ArrayNames = StPicoArrays::picoArrayNames + __NPICOARRAYS__;

//              ARRAY TYPES
//============================================================================================
const char* StPicoArrays::picoArrayTypes [__NALLPICOARRAYS__] = {"StPicoEvent","StPicoTrack","StPicoEmcTrigger","StPicoMtdTrigger",
								 "StPicoBTOWHit","StPicoBTofHit","StPicoMtdHit",
								 "StPicoEmcPidTraits","StPicoBTofPidTraits","StPicoMtdPidTraits",
/*picoV0ArrayTypes[__NPICOV0ARRAYS__]*/                          "StPicoV0","StPicoV0","StPicoV0"};

const char** StPicoArrays::picoV0ArrayTypes = StPicoArrays::picoArrayTypes + __NPICOARRAYS__;

//              ARRAY SIZES
//============================================================================================
// These are intial sizes. Automatically resized if too small.
// Choosing too large initial values gives a performance penalty when reading
// only selected picoDst branches
int StPicoArrays::picoArraySizes [__NALLPICOARRAYS__    ] = {1,1000,100,100,
							     100,100,100,
							     100,100,100,
/*picoV0ArraySizes[__NPICOV0ARRAYS__]*/                      1000,1000,1000};

int* StPicoArrays::picoV0ArraySizes = StPicoArrays::picoArraySizes + __NPICOARRAYS__;

//              ARRAY COUNTERS
//============================================================================================
int   StPicoArrays::picoArrayCounters [__NALLPICOARRAYS__ ] = {0,0,0,0,
							       0,0,0,
							       0,0,0,
/*picoV0ArrayCounters[__NPICOV0ARRAYS__]*/                     0,0,0};

int* StPicoArrays::picoV0ArrayCounters = StPicoArrays::picoArrayCounters + __NPICOARRAYS__;

StPicoArrays::StPicoArrays()
{}
