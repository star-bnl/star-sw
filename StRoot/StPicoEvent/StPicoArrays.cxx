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
																					                    "BEmcSmdPHit"
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
																					                    "StPicoBEmcSmdPHit"
};

//              ARRAY SIZES
// These are intial sizes. Automatically resized if too small.
// Choosing too large initial values gives a performance penalty when reading
// only selected picoDst branches
//_________________
int StPicoArrays::picoArraySizes [NAllPicoArrays] = { 1,
																								      1000,
																								      100,
																								      100,
																								      4800,
																								      100,
																								      100,
																								      32,
																								      100,
																								      1300,
																								      100,
																								      100,
																								      100,
																								      1000,
																											100,
																											100
};

//_________________
StPicoArrays::StPicoArrays() { /// Destructor
  /* empty */
}
