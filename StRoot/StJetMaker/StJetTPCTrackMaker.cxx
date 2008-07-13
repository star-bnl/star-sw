// $Id: StJetTPCTrackMaker.cxx,v 1.2 2008/07/13 00:05:25 tai Exp $
#include "StJetTPCTrackMaker.h"

#include "StJetTPCMuDst.h"

#include <TFile.h>
#include <TTree.h>

#include <iostream>

using namespace std;
using namespace StSpinJet;

ClassImp(StJetTPCTrackMaker)
  

StJetTPCTrackMaker::StJetTPCTrackMaker(const Char_t *name, TDirectory* file, StMuDstMaker* uDstMaker)
  : StMaker(name)
  , _file(file)
  , _uDstMaker(uDstMaker)
{ }

Int_t StJetTPCTrackMaker::Init()
{
  _tpc  = new StJetTPCMuDst(_uDstMaker);

  _file->cd();
  _tree = new TTree("tpcTracks", "tpcTracks");

  _tree->Branch("runNumber"  , &_runNumber    , "runNumber/I"    );
  _tree->Branch("eventId"    , &_eventId      , "eventId/I"      );
  _tree->Branch("nTracks"    , &_nTracks      , "nTracks/I"      );
  _tree->Branch("trackId"    ,  _trackId      , "trackId[nTracks]/S"      );
  _tree->Branch("px"         ,  _px           , "px[nTracks]/D"           );
  _tree->Branch("py"         ,  _py           , "py[nTracks]/D"           );
  _tree->Branch("pz"         ,  _pz           , "pz[nTracks]/D"           );
  _tree->Branch("flag"       ,  _flag         , "flag[nTracks]/S"         );
  _tree->Branch("nHits"      ,  _nHits        , "nHits[nTracks]/s"        );
  _tree->Branch("charge"     ,  _charge       , "charge[nTracks]/S"       );
  _tree->Branch("nHitsPoss"  ,  _nHitsPoss    , "nHitsPoss[nTracks]/s"    );
  _tree->Branch("nHitsDedx"  ,  _nHitsDedx    , "nHitsDedx[nTracks]/s"    );
  _tree->Branch("nHitsFit"   ,  _nHitsFit     , "nHitsFit[nTracks]/s"     );
  _tree->Branch("nSigmaPion" ,  _nSigmaPion   , "nSigmaPion[nTracks]/D"   );
  _tree->Branch("Tdca"       ,  _Tdca         , "Tdca[nTracks]/D"         );
  _tree->Branch("dcaZ"       ,  _dcaZ         , "dcaZ[nTracks]/D"         );
  _tree->Branch("dcaD"       ,  _dcaD         , "dcaD[nTracks]/D"         );
  _tree->Branch("BField"     ,  _BField       , "BField[nTracks]/D"       );
  _tree->Branch("bemcRadius" ,  _bemcRadius   , "bemcRadius[nTracks]/D"   );
  _tree->Branch("etaext"     ,  _etaext       , "etaext[nTracks]/D"       );
  _tree->Branch("phiext"     ,  _phiext       , "phiext[nTracks]/D"       );
  _tree->Branch("dEdx"       ,  _dEdx         , "dEdx[nTracks]/D"         );
  _tree->Branch("trackIndex" ,  _trackIndex   , "trackIndex[nTracks]/I"   );

  return kStOk;
}

Int_t StJetTPCTrackMaker::Make()
{
  TrackList trackList = _tpc->getTrackList();

  if(trackList.empty()) return kStOk;

  _runNumber = trackList[0].runNumber;
  _eventId = trackList[0].eventId;

  _nTracks = trackList.size();
  for(int i = 0; i < _nTracks; ++i) {
    const Track& track = trackList[i];
    _px[i]         = track.px;
    _py[i]         = track.py;
    _pz[i]         = track.pz;
    _flag[i]       = track.flag;
    _nHits[i]      = track.nHits;
    _charge[i]     = track.charge;
    _nHitsPoss[i]  = track.nHitsPoss;
    _nHitsDedx[i]  = track.nHitsDedx;
    _nHitsFit[i]   = track.nHitsFit;
    _nSigmaPion[i] = track.nSigmaPion;
    _Tdca[i]       = track.Tdca;
    _dcaZ[i]       = track.dcaZ;
    _dcaD[i]       = track.dcaD;
    _BField[i]     = track.BField;
    _bemcRadius[i] = track.bemcRadius;
    _etaext[i]     = track.etaext;
    _phiext[i]     = track.phiext;
    _dEdx[i]       = track.dEdx;
    _trackIndex[i] = track.trackIndex;
    _trackId[i]    = track.id;
  }

  _tree->Fill();

  return kStOk;

}

Int_t StJetTPCTrackMaker::Finish()
{
  _tree->BuildIndex("runNumber", "eventId");

  return kStOk;
}
