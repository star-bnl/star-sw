// -*- mode: c++;-*-
// $Id: StMuTrackEmu.h,v 1.12 2008/07/12 01:40:10 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STMUTRACKEMU_H
#define STMUTRACKEMU_H

#include <Rtypes.h>

namespace StSpinJet {

class StMuTrackEmu {

public:

  StMuTrackEmu()
    : _px(0)
    , _py(0)
    , _pz(0)
    , _flag(0)
    , _nHits(0)
    , _charge(0)
    , _nHitsPoss(0)
    , _nHitsDedx(0)
    , _nHitsFit(0)
    , _nSigmaPion(0)
    , _Tdca(0)
    , _dcaZ(0)
    , _dcaD(0)
    , _BField(0)
    , _bemcRadius(0)
    , _etaext(0)
    , _phiext(0)
    , _dEdx(0)
    , _trackIndex(0)
    , _id(0)
 { }

  virtual ~StMuTrackEmu() { }

  double         px()          const { return _px; }
  double         py()          const { return _py; }
  double         pz()          const { return _pz; }
  short          flag()        const { return _flag; }
  unsigned short nHits()       const { return _nHits; }
  Short_t        charge()      const { return _charge; }
  unsigned short nHitsPoss()   const { return _nHitsPoss; }
  unsigned short nHitsDedx()   const { return _nHitsDedx; }
  unsigned short nHitsFit()    const { return _nHitsFit; }
  double         nSigmaPion()  const { return _nSigmaPion; }
  double         Tdca()        const { return _Tdca; }
  Float_t        dcaZ()        const { return _dcaZ; }
  Float_t        dcaD()        const { return _dcaD; }
  double         BField()      const { return _BField; }
  double         bemcRadius()  const { return _bemcRadius; }
  double         etaext()      const { return _etaext; }
  double         phiext()      const { return _phiext; }
  double         dEdx()        const { return _dEdx; }
			       
  int            trackIndex()  const { return _trackIndex; }
			       
  short          id()          const { return _id; }

private:

  friend class StMuTrackEmuFactory;
  friend class TrackListToFourList;

  double         _px;
  double         _py;
  double         _pz;
  short          _flag;
  unsigned short _nHits;
  Short_t        _charge;
  unsigned short _nHitsPoss;
  unsigned short _nHitsDedx;
  unsigned short _nHitsFit;
  double         _nSigmaPion;
  double         _Tdca;
  Float_t        _dcaZ;
  Float_t        _dcaD;
  double         _BField;
  double         _bemcRadius;
  double         _etaext;
  double         _phiext;
  double         _dEdx;

  int            _trackIndex;

  short          _id;
};

}

#endif // STMUTRACKEMU_H
