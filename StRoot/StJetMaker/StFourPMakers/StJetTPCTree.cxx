// $Id: StJetTPCTree.cxx,v 1.1 2008/07/13 22:30:03 tai Exp $
#include "StJetTPCTree.h"

#include <TTree.h>

namespace StSpinJet {

StJetTPCTree::StJetTPCTree(TTree *tree)
 : _tree(tree)
{
  _tree->SetBranchAddress("eventId"    , &_eventId     );
  _tree->SetBranchAddress("nTracks"    , &_nTracks     );
  _tree->SetBranchAddress("pt"         ,  _pt          );
  _tree->SetBranchAddress("eta"        ,  _eta         );
  _tree->SetBranchAddress("phi"        ,  _phi         );
  _tree->SetBranchAddress("etaext"     ,  _etaext      );
  _tree->SetBranchAddress("phiext"     ,  _phiext      );
  _tree->SetBranchAddress("trackId"    ,  _trackId     );
  _tree->SetBranchAddress("flag"       ,  _flag        );
  _tree->SetBranchAddress("nHits"      ,  _nHits       );
  _tree->SetBranchAddress("charge"     ,  _charge      );
  _tree->SetBranchAddress("nHitsPoss"  ,  _nHitsPoss   );
  _tree->SetBranchAddress("nHitsDedx"  ,  _nHitsDedx   );
  _tree->SetBranchAddress("nHitsFit"   ,  _nHitsFit    );
  _tree->SetBranchAddress("nSigmaPion" ,  _nSigmaPion  );
  _tree->SetBranchAddress("Tdca"       ,  _Tdca        );
  _tree->SetBranchAddress("dcaZ"       ,  _dcaZ        );
  _tree->SetBranchAddress("dcaD"       ,  _dcaD        );
  _tree->SetBranchAddress("BField"     ,  _BField      );
  _tree->SetBranchAddress("bemcRadius" ,  _bemcRadius  );
  _tree->SetBranchAddress("dEdx"       ,  _dEdx        );
  _tree->SetBranchAddress("trackIndex" ,  _trackIndex  );
  _tree->SetBranchAddress("runNumber"  , &_runNumber   );
}

TrackList StJetTPCTree::getTrackList()
{
  TrackList ret;

  for(int i = 0; i < _nTracks; ++i) {

    Track track;

    track.runNumber  = _runNumber;
    track.eventId    = _eventId;
    track.pt         = _pt[i];
    track.eta        = _eta[i];
    track.phi        = _phi[i];
    track.flag       = _flag[i];
    track.nHits      = _nHits[i];
    track.charge     = _charge[i];
    track.nHitsPoss  = _nHitsPoss[i];
    track.nHitsDedx  = _nHitsDedx[i];
    track.nHitsFit   = _nHitsFit[i];
    track.nSigmaPion = _nSigmaPion[i];
    track.Tdca       = _Tdca[i];
    track.dcaZ       = _dcaZ[i];
    track.dcaD       = _dcaD[i];
    track.BField     = _BField[i];
    track.bemcRadius = _bemcRadius[i];
    track.etaext     = _etaext[i];
    track.phiext     = _phiext[i];
    track.dEdx       = _dEdx[i];
    track.trackIndex = _trackIndex[i];
    track.id         =  _trackId[i];

    ret.push_back(track);
  }

  return ret;
}

}
