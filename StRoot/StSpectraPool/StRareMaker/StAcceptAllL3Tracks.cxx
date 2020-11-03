#include "StAcceptAllL3Tracks.h"
#include "StEventTypes.h"

ClassImp(StAcceptAllL3Tracks)

int StAcceptAllL3Tracks::Accept(StGlobalTrack* event){return 1;}
void StAcceptAllL3Tracks::Report(){cout << "StAcceptAllL3Tracks: No track cuts for uDST" << endl;}
