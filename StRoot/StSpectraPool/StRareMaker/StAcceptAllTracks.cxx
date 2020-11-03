#include "StAcceptAllTracks.h"
#include "StEventTypes.h"
ClassImp(StAcceptAllTracks)

int StAcceptAllTracks::Accept(StPrimaryTrack* event){return 1;}
void StAcceptAllTracks::Report(){cout << "StAcceptAllTracks: No track cuts for uDST" << endl;}
