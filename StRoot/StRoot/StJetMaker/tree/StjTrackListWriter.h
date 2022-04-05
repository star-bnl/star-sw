// -*- mode: c++;-*-
// $Id: StjTrackListWriter.h,v 1.5 2008/08/04 06:10:47 tai Exp $
#ifndef STJTRACKLISTWRITER_H
#define STJTRACKLISTWRITER_H

#include <TObject.h>

#include "StjTrackList.h"
#include <Rtypes.h>

class TDirectory;
class TTree;

class StjTrackListWriter : public TObject {

public:

  StjTrackListWriter(const char* treeName, TDirectory* file);
  virtual ~StjTrackListWriter() { }

  void Fill(const StjTrackList& theList);
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
  Double_t _BField;
  Double_t _vertexZ;
  Double_t _bemcRadius;
  Int_t    _exitDetectorId[4096]; // 9: BEMC, 13: EEMC
  Int_t    _exitTowerId[4096];
  Double_t _exitEta[4096];
  Double_t _exitPhi[4096];
  Double_t _dEdx[4096];
  Int_t    _trackIndex[4096];
  Short_t  _trackId[4096];

  ClassDef(StjTrackListWriter, 1)

};

#endif // STJTRACKLISTWRITER_H
