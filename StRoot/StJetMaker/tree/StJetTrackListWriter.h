// -*- mode: c++;-*-
// $Id: StJetTrackListWriter.h,v 1.1 2008/07/24 20:57:16 tai Exp $
#ifndef STJETTRACKLISTWRITER_HH
#define STJETTRACKLISTWRITER_HH

#include "TrackList.h"
#include <Rtypes.h>

class TDirectory;
class TTree;

class StJetTrackListWriter {

public:

  StJetTrackListWriter(const char* treeName, TDirectory* file);
  virtual ~StJetTrackListWriter() { }

  void Fill(const StSpinJet::TrackList& theList);
  void Finish();
    
private:

  TDirectory* _file;
  TTree* _tree;

  Int_t    _runNumber;
  Int_t    _eventId;
  Int_t    _detectorId; // 1: TPC
  Int_t    _nTracks;
  Double_t _pt[4096];
  Double_t _eta[4096];
  Double_t _phi[4096];
  Short_t  _flag[4096];
  UShort_t _nHits[4096];
  Short_t  _charge[4096];
  UShort_t _nHitsPoss[4096];
  UShort_t _nHitsDedx[4096];
  UShort_t _nHitsFit[4096];
  Double_t _nSigmaPion[4096];
  Double_t _Tdca[4096];
  Double_t _dcaZ[4096];
  Double_t _dcaD[4096];
  Double_t _BField[4096];
  Double_t _vertexZ[4096];
  Double_t _bemcRadius[4096];
  Int_t    _exitDetectorId[4096]; // 9: BEMC, 13: EEMC
  Int_t    _exitTowerId[4096];
  Double_t _exitEta[4096];
  Double_t _exitPhi[4096];
  Double_t _dEdx[4096];
  Int_t    _trackIndex[4096];
  Short_t  _trackId[4096];

};

#endif // STJETTRACKLISTWRITER_HH
