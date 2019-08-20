//
// The StPicoArrays holds names, types and initial sizes of the pico arrays
//

// PicoDst headers
#include "StPicoArrays.h"

//    ARRAY NAMES
//_________________
const char* StPicoArrays::picoArrayNames [NAllPicoArrays] = { "Event",
							      "Track",
							      "EmcTrigger",
							      "MtdTrigger",
							      "BTowHit",
							      "BTofHit",
							      "MtdHit",
							      "BbcHit",
							      "EpdHit",
							      "FmsHit",
							      "EmcPidTraits",
							      "BTofPidTraits",
							      "MtdPidTraits",
							      "TrackCovMatrix",
							      "BEmcSmdEHit",
							      "BEmcSmdPHit",
							      "ETofHit",
							      "ETofPidTraits"
};

//   ARRAY TYPES
//_________________
const char* StPicoArrays::picoArrayTypes [NAllPicoArrays] = { "StPicoEvent",
							      "StPicoTrack",
							      "StPicoEmcTrigger",
							      "StPicoMtdTrigger",
							      "StPicoBTowHit",
							      "StPicoBTofHit",
							      "StPicoMtdHit",
							      "StPicoBbcHit",
							      "StPicoEpdHit",
							      "StPicoFmsHit",
							      "StPicoBEmcPidTraits",
							      "StPicoBTofPidTraits",
							      "StPicoMtdPidTraits",
							      "StPicoTrackCovMatrix",
							      "StPicoBEmcSmdEHit",
							      "StPicoBEmcSmdPHit",
							      "StPicoETofHit",
							      "StPicoETofPidTraits"
};

//              ARRAY SIZES
// These are intial sizes. Automatically resized if too small.
// Choosing too large initial values gives a performance penalty when reading
// only selected picoDst branches
//_________________
int StPicoArrays::picoArraySizes [NAllPicoArrays] = { 1,    // StPicoEvent
						      1000, // StPicoTrack
						      100,  // StPicoEmcTrigger
						      100,  // StPicoMtdTrigger
						      4800, // StPicoBTowHit
						      100,  // StPicoBTofHit
						      100,  // StPicoMtdHit
						      32,   // StPicoBbcHit
						      100,  // StPicoEpdHit
						      1300, // StPicoFmsHit
						      100,  // StPicoBEmcPidTraits
						      100,  // StPicoBTofPidTraits
						      100,  // StPicoMtdPidTraits
						      1000, // StPicoTrackCovMatrix
						      100,  // StPicoBEmcSmdEHit
						      100,  // StPicoBEmcSmdPHit
						      100,  // StPicoETofHit
						      100   // StPicoETofPidTraits
};

//_________________
StPicoArrays::StPicoArrays() { // Destructor
  /* empty */
}
