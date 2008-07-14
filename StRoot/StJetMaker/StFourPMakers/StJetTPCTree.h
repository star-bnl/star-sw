// -*- mode: c++;-*-
// $Id: StJetTPCTree.h,v 1.2 2008/07/14 03:35:53 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETTPCTREE_H
#define STJETTPCTREE_H

#include "StJetTPC.h"
#include <Rtypes.h>

class TTree;

namespace StSpinJet {

class StJetTPCTree : public StJetTPC {

public:
  StJetTPCTree(TTree *tree,
	       const Int_t& indexMajor, const Int_t& indexMinor,
	       const char* indexMajorName = "runNumber",
	       const char* indexMinorName = "eventId"
	       );
  virtual ~StJetTPCTree() { }

  TrackList getTrackList();

private:

  TTree* _tree;

  const Int_t& _indexMajor;
  const Int_t& _indexMinor;

  Int_t _runNumber;
  Int_t _eventId;
  Int_t _nTracks;
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
  Double_t _bemcRadius[4096];
  Double_t _etaext[4096];
  Double_t _phiext[4096];
  Double_t _dEdx[4096];
  Int_t    _trackIndex[4096];
  Short_t  _trackId[4096];
};

}

#endif // STJETTPCTREE_H
