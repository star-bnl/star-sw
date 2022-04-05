// -*- mode: c++;-*-
// $Id: StjTrackListReader.h,v 1.7 2008/08/11 06:07:53 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTRACKLISTREADER_H
#define STJTRACKLISTREADER_H

#include "StjTreeReader.h"

#include "StjTrackList.h"

#include <Rtypes.h>

class StjTrackListReader : public StjTreeReader {

public:
  StjTrackListReader(TTree *tree) : StjTreeReader(tree) { }
  virtual ~StjTrackListReader() { }

  StjTrackList getTrackList() { return _list; }

private:

  void SetBranchAddress(TTree *tree);

  void clearEntry();
  void readEntry();

  StjTrackList _list;

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

  ClassDef(StjTrackListReader, 1)

};

#endif // STJTRACKLISTREADER_H
