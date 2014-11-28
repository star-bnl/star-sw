// $Id: StjTrackListWriter.cxx,v 1.4 2008/08/04 06:10:47 tai Exp $
#include "StjTrackListWriter.h"

#include <TDirectory.h>
#include <TTree.h>

#include <iostream>

ClassImp(StjTrackListWriter)

using namespace std;

StjTrackListWriter::StjTrackListWriter(const char* treeName, TDirectory* file)
  : _file(file)
{
  _file->cd();
  _tree = new TTree(treeName, treeName);
  _tree->SetAutoSave(kMaxLong64);
  _tree->SetMaxTreeSize(kMaxLong64);

  _tree->Branch("eventId"       , &_eventId         , "eventId/I"                );
  _tree->Branch("nTracks"       , &_nTracks         , "nTracks/I"                );
  _tree->Branch("pt"            ,  _pt              , "pt[nTracks]/D"            );
  _tree->Branch("eta"           ,  _eta             , "eta[nTracks]/D"           );
  _tree->Branch("phi"           ,  _phi             , "phi[nTracks]/D"           );
  _tree->Branch("exitEta"       ,  _exitEta         , "exitEta[nTracks]/D"       );
  _tree->Branch("exitPhi"       ,  _exitPhi         , "exitPhi[nTracks]/D"       );
  _tree->Branch("trackId"       ,  _trackId         , "trackId[nTracks]/S"       );
  _tree->Branch("flag"          ,  _flag            , "flag[nTracks]/S"          );
  _tree->Branch("nHits"         ,  _nHits           , "nHits[nTracks]/s"         );
  _tree->Branch("charge"        ,  _charge          , "charge[nTracks]/S"        );
  _tree->Branch("nHitsPoss"     ,  _nHitsPoss       , "nHitsPoss[nTracks]/s"     );
  _tree->Branch("nHitsDedx"     ,  _nHitsDedx       , "nHitsDedx[nTracks]/s"     );
  _tree->Branch("nHitsFit"      ,  _nHitsFit        , "nHitsFit[nTracks]/s"      );
  _tree->Branch("nSigmaPion"    ,  _nSigmaPion      , "nSigmaPion[nTracks]/D"    );
  _tree->Branch("Tdca"          ,  _Tdca            , "Tdca[nTracks]/D"          );
  _tree->Branch("dcaZ"          ,  _dcaZ            , "dcaZ[nTracks]/D"          );
  _tree->Branch("dcaD"          ,  _dcaD            , "dcaD[nTracks]/D"          );
  _tree->Branch("BField"        , &_BField          , "BField/D"                 );
  _tree->Branch("bemcRadius"    , &_bemcRadius      , "bemcRadius/D"             );
  _tree->Branch("dEdx"          ,  _dEdx            , "dEdx[nTracks]/D"          );
  _tree->Branch("trackIndex"    ,  _trackIndex      , "trackIndex[nTracks]/I"    );
  _tree->Branch("exitDetectorId",  _exitDetectorId  , "exitDetectorId[nTracks]/I");    
  _tree->Branch("exitTowerId"   ,  _exitTowerId     , "exitTowerId[nTracks]/I"   );    
  _tree->Branch("vertexZ"       , &_vertexZ         , "vertexZ/D"                );    
  _tree->Branch("detectorId"    , &_detectorId      , "detectorId/I"             ); 
  _tree->Branch("runNumber"     , &_runNumber       , "runNumber/I"              );
}

void StjTrackListWriter::Fill(const StjTrackList& trackList)
{
  if(trackList.empty()) return;

  _runNumber  = trackList[0].runNumber;
  _eventId    = trackList[0].eventId;
  _detectorId = trackList[0].detectorId; 
  _vertexZ    = trackList[0].vertexZ;

  _BField     = trackList[0].BField;
  _bemcRadius = trackList[0].bemcRadius;

  _nTracks = trackList.size();
  for(int i = 0; i < _nTracks; ++i) {
    const StjTrack& track = trackList[i];
    _pt[i]             = track.pt;
    _eta[i]            = track.eta;
    _phi[i]            = track.phi;
    _flag[i]           = track.flag;
    _nHits[i]          = track.nHits;
    _charge[i]         = track.charge;
    _nHitsPoss[i]      = track.nHitsPoss;
    _nHitsDedx[i]      = track.nHitsDedx;
    _nHitsFit[i]       = track.nHitsFit;
    _nSigmaPion[i]     = track.nSigmaPion;
    _Tdca[i]           = track.Tdca;
    _dcaZ[i]           = track.dcaZ;
    _dcaD[i]           = track.dcaD;
    _exitDetectorId[i] = track.exitDetectorId;
    _exitTowerId[i]    = track.exitTowerId;
    _exitEta[i]        = track.exitEta;
    _exitPhi[i]        = track.exitPhi;
    _dEdx[i]           = track.dEdx;
    _trackIndex[i]     = track.trackIndex;
    _trackId[i]        = track.id;
  }

  _tree->Fill();
}

void StjTrackListWriter::Finish()
{
  _tree->BuildIndex("runNumber", "eventId");
}
