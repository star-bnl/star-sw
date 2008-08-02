// $Id: StjTrackListReader.cxx,v 1.1 2008/08/02 04:22:17 tai Exp $
#include "StjTrackListReader.h"

#include <TTree.h>

using namespace StSpinJet;

StJetTrackListReader::StJetTrackListReader(TTree *tree)
 : _tree(tree)
{
  _tree->SetBranchAddress("eventId"        , &_eventId         );
  _tree->SetBranchAddress("nTracks"        , &_nTracks         );
  _tree->SetBranchAddress("pt"             ,  _pt              );
  _tree->SetBranchAddress("eta"            ,  _eta             );
  _tree->SetBranchAddress("phi"            ,  _phi             );
  _tree->SetBranchAddress("exitEta"        ,  _exitEta         );
  _tree->SetBranchAddress("exitPhi"        ,  _exitPhi         );
  _tree->SetBranchAddress("trackId"        ,  _trackId         );
  _tree->SetBranchAddress("flag"           ,  _flag            );
  _tree->SetBranchAddress("nHits"          ,  _nHits           );
  _tree->SetBranchAddress("charge"         ,  _charge          );
  _tree->SetBranchAddress("nHitsPoss"      ,  _nHitsPoss       );
  _tree->SetBranchAddress("nHitsDedx"      ,  _nHitsDedx       );
  _tree->SetBranchAddress("nHitsFit"       ,  _nHitsFit        );
  _tree->SetBranchAddress("nSigmaPion"     ,  _nSigmaPion      );
  _tree->SetBranchAddress("Tdca"           ,  _Tdca            );
  _tree->SetBranchAddress("dcaZ"           ,  _dcaZ            );
  _tree->SetBranchAddress("dcaD"           ,  _dcaD            );
  _tree->SetBranchAddress("BField"         , &_BField          );
  _tree->SetBranchAddress("bemcRadius"     , &_bemcRadius      );
  _tree->SetBranchAddress("dEdx"           ,  _dEdx            );
  _tree->SetBranchAddress("trackIndex"     ,  _trackIndex      );
  _tree->SetBranchAddress("exitDetectorId" ,  _exitDetectorId  );
  _tree->SetBranchAddress("exitTowerId"    ,  _exitTowerId     );
  _tree->SetBranchAddress("vertexZ"        , &_vertexZ      );
  _tree->SetBranchAddress("detectorId"     , &_detectorId      );
  _tree->SetBranchAddress("runNumber"      , &_runNumber       );
}

TrackList StJetTrackListReader::GetEntry(Long64_t entry)
{
  TrackList ret;

  if(entry < 0) return ret;

  if(_tree->GetEntry(entry) <= 0) return ret;

  for(int i = 0; i < _nTracks; ++i) {

    Track track;

    track.runNumber      = _runNumber;
    track.eventId        = _eventId;
    track.detectorId     = _detectorId;
    track.pt             = _pt[i];
    track.eta            = _eta[i];
    track.phi            = _phi[i];
    track.flag           = _flag[i];
    track.nHits          = _nHits[i];
    track.charge         = _charge[i];
    track.nHitsPoss      = _nHitsPoss[i];
    track.nHitsDedx      = _nHitsDedx[i];
    track.nHitsFit       = _nHitsFit[i];
    track.nSigmaPion     = _nSigmaPion[i];
    track.Tdca           = _Tdca[i];
    track.dcaZ           = _dcaZ[i];
    track.dcaD           = _dcaD[i];
    track.BField         = _BField;
    track.bemcRadius     = _bemcRadius;
    track.exitDetectorId = _exitDetectorId[i];
    track.exitTowerId    = _exitTowerId[i];
    track.exitEta        = _exitEta[i];
    track.exitPhi        = _exitPhi[i];
    track.dEdx           = _dEdx[i];
    track.trackIndex     = _trackIndex[i];
    track.id             = _trackId[i];
    track.vertexZ        = _vertexZ;

    ret.push_back(track);
  }

  return ret;
}
