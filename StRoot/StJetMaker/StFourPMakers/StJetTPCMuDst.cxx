// $Id: StJetTPCMuDst.cxx,v 1.1 2008/07/08 04:16:05 tai Exp $
#include "StJetTPCMuDst.h"

#include "../StMuTrackEmuFactory.h"

#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"

namespace StSpinJet {

StJetTPCMuDst::StJetTPCMuDst(StMuDstMaker* uDstMaker)
  : _uDstMaker(uDstMaker)
{

}

StJetTPCMuDst::TrackList StJetTPCMuDst::getTrackList()
{
  TrackList ret;

  StMuDst* uDst = _uDstMaker->muDst();

  long nTracks = uDst->numberOfPrimaryTracks();

  double magneticField = _uDstMaker->muDst()->event()->magneticField()/10.0; // Tesla
  for(int i = 0; i < nTracks; ++i) {
    const StMuTrack* track = uDst->primaryTracks(i);

    if(track->flag() < 0) continue;

    if(track->topologyMap().trackFtpcEast() || track->topologyMap().trackFtpcWest()) continue;

    StMuTrackEmu* trackEmu = StMuTrackEmuFactory::createStMuTrackEmu(track, i, magneticField);

    ret.push_back(trackEmu);
  }

  return ret;
}


}
