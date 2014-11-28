// $Id: StjTrackListReader.cxx,v 1.5 2008/08/11 03:51:00 tai Exp $
#include "StjTrackListReader.h"

#include <TTree.h>

ClassImp(StjTrackListReader)

void StjTrackListReader::SetBranchAddress(TTree *tree)
{
  tree->SetBranchAddress("eventId"        , &_eventId         );
  tree->SetBranchAddress("nTracks"        , &_nTracks         );
  tree->SetBranchAddress("pt"             ,  _pt              );
  tree->SetBranchAddress("eta"            ,  _eta             );
  tree->SetBranchAddress("phi"            ,  _phi             );
  tree->SetBranchAddress("exitEta"        ,  _exitEta         );
  tree->SetBranchAddress("exitPhi"        ,  _exitPhi         );
  tree->SetBranchAddress("trackId"        ,  _trackId         );
  tree->SetBranchAddress("flag"           ,  _flag            );
  tree->SetBranchAddress("nHits"          ,  _nHits           );
  tree->SetBranchAddress("charge"         ,  _charge          );
  tree->SetBranchAddress("nHitsPoss"      ,  _nHitsPoss       );
  tree->SetBranchAddress("nHitsDedx"      ,  _nHitsDedx       );
  tree->SetBranchAddress("nHitsFit"       ,  _nHitsFit        );
  tree->SetBranchAddress("nSigmaPion"     ,  _nSigmaPion      );
  tree->SetBranchAddress("Tdca"           ,  _Tdca            );
  tree->SetBranchAddress("dcaZ"           ,  _dcaZ            );
  tree->SetBranchAddress("dcaD"           ,  _dcaD            );
  tree->SetBranchAddress("BField"         , &_BField          );
  tree->SetBranchAddress("bemcRadius"     , &_bemcRadius      );
  tree->SetBranchAddress("dEdx"           ,  _dEdx            );
  tree->SetBranchAddress("trackIndex"     ,  _trackIndex      );
  tree->SetBranchAddress("exitDetectorId" ,  _exitDetectorId  );
  tree->SetBranchAddress("exitTowerId"    ,  _exitTowerId     );
  tree->SetBranchAddress("vertexZ"        , &_vertexZ      );
  tree->SetBranchAddress("detectorId"     , &_detectorId      );
  tree->SetBranchAddress("runNumber"      , &_runNumber       );
}

void StjTrackListReader::clearEntry()
{
  _list.clear();
}

void StjTrackListReader::readEntry()
{
  clearEntry();

  for(int i = 0; i < _nTracks; ++i) {

    StjTrack track;

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

    _list.push_back(track);
  }
}
