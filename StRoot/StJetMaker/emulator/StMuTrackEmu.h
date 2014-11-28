// -*- mode: c++;-*-
// $Id: StMuTrackEmu.h,v 1.13 2012/03/23 05:44:20 pibero Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STMUTRACKEMU_H
#define STMUTRACKEMU_H

#include "TVector3.h"

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
    , _nSigmaKaon(0)
    , _nSigmaProton(0)
    , _nSigmaElectron(0)
    , _Tdca(0)
    , _dcaX(0)
    , _dcaY(0)
    , _dcaZ(0)
    , _dcaD(0)
    , _chi2(0)
    , _chi2prob(0)
    , _BField(0)
    , _bemcRadius(0)
    , _etaext(0)
    , _phiext(0)
    , _exitTowerId(0)
    , _exitDetectorId(0)
    , _dEdx(0)
    , _beta(0)
    , _trackIndex(0)
    , _id(0)
    , _detectorId(0)
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
  double         nSigmaKaon() const { return _nSigmaKaon; }
  double         nSigmaProton() const { return _nSigmaProton; }
  double         nSigmaElectron() const { return _nSigmaElectron; }
  double         Tdca()        const { return _Tdca; }
  double         dcaX()        const { return _dcaX; }
  double         dcaY()        const { return _dcaY; }
  double         dcaZ()        const { return _dcaZ; }
  double         dcaD()        const { return _dcaD; }
  double         chi2()        const { return _chi2; }
  double         chi2prob()    const { return _chi2prob; }
  double         BField()      const { return _BField; }
  double         bemcRadius()  const { return _bemcRadius; }
  double         etaext()      const { return _etaext; }
  double         phiext()      const { return _phiext; }
  short          exitTowerId() const { return _exitTowerId; }
  short          exitDetectorId() const { return _exitDetectorId; }
  double         dEdx()        const { return _dEdx; }
  double         beta()        const { return _beta; }
  const TVector3& firstPoint() const { return _firstPoint; }
  const TVector3&  lastPoint() const { return _lastPoint;  }
  int            trackIndex()  const { return _trackIndex; }
  short          id()          const { return _id; }
  short          detectorId()  const { return _detectorId; }

private:

  friend class StjeTrackListToStMuTrackFourVecList;

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
  double         _nSigmaKaon;
  double         _nSigmaProton;
  double         _nSigmaElectron;
  double         _Tdca;
  double         _dcaX;
  double         _dcaY;
  double         _dcaZ;
  double         _dcaD;
  double         _chi2;
  double         _chi2prob;
  double         _BField;
  double         _bemcRadius;
  double         _etaext;
  double         _phiext;
  short          _exitTowerId;
  short          _exitDetectorId;
  double         _dEdx;
  double         _beta;
  TVector3       _firstPoint;
  TVector3       _lastPoint;

  int            _trackIndex;

  short          _id;
  short          _detectorId;
};

#endif // STMUTRACKEMU_H
