//-*- Mode: C++ -*-
// ************************************************************************
// This file is property of and copyright by the ALICE HLT Project        *
// ALICE Experiment at CERN, All rights reserved.                         *
// See cxx source for full Copyright notice                               *
//                                                                        *
//*************************************************************************

#ifndef ALIHLTTPCCAMCPOINT_H
#define ALIHLTTPCCAMCPOINT_H

#include "AliHLTTPCCADef.h"
#include <vector>

/**
 * @class AliHLTTPCCAMCPoint
 * store MC point information for AliHLTTPCCAPerformance
 */
class AliHLTTPCCAMCPoint
{
  public:

    AliHLTTPCCAMCPoint();

    float  X()           const { return fX; }
    float  Y()           const { return fY; }
    float  Z()           const { return fZ; }
    float  Sx()          const { return fSx; }
    float  Sy()          const { return fSy; }
    float  Sz()          const { return fSz; }
    float  Time()        const { return fTime; }
    int    ISlice()      const { return fISlice; }
    int    TrackID()     const { return fTrackID; }

    void SetX( float v )           { fX = v; }
    void SetY( float v )           { fY = v; }
    void SetZ( float v )           { fZ = v; }
    void SetSx( float v )          { fSx = v; }
    void SetSy( float v )          { fSy = v; }
    void SetSz( float v )          { fSz = v; }
    void SetTime( float v )        { fTime = v; }
    void SetISlice( int v )      { fISlice = v; }
    void SetTrackID( int v )     { fTrackID = v; }

    static bool Compare( const AliHLTTPCCAMCPoint &p1, const AliHLTTPCCAMCPoint &p2 ) {
      return ( p1.fTrackID < p2.fTrackID );
    }

  protected:

    float fX;         //* global X position
    float fY;         //* global Y position
    float fZ;         //* global Z position
    float fSx;        //* slice X position
    float fSy;        //* slice Y position
    float fSz;        //* slice Z position
    float fTime;      //* time
    int   fISlice;    //* slice number
    int   fTrackID;   //* mc track number
};


/**
 * @class AliHLTTPCCALocalMCPoint
 * store MC point information for AliHLTTPCCAPerformance
 */
class AliHLTTPCCALocalMCPoint
{
  public:

    AliHLTTPCCALocalMCPoint(){};
    ~AliHLTTPCCALocalMCPoint(){};

    float  X()           const { return fX; }
    float  Y()           const { return fY; }
    float  Z()           const { return fZ; }
    float  Px()          const { return fPx; }
    float  Py()          const { return fPy; }
    float  Pz()          const { return fPz; }
    float  QP()          const { return fQP; }
    int    ISlice()      const { return fISlice; }
    int    IRow()      const { return fIRow; }
    int    TrackI()     const { return fTrackI; }
    int    TrackID()     const { return fTrackID; }
    
    static bool Compare( const AliHLTTPCCALocalMCPoint &p1, const AliHLTTPCCALocalMCPoint &p2 ) {
      return ( p1.fTrackID < p2.fTrackID );
    }

  protected:

    float fX;         //* local X position
    float fY;         //* local Y position
    float fZ;         //* local Z position
    float fPx;        //* local momentum projection - Px
    float fPy;
    float fPz;
    float fQP;        //* Q/P

    int   fISlice;   //* slice number
    int   fIRow;
    int   fTrackI;   //* mc track index
    int   fTrackID;  //* mc track simulation ID
  public:
    std::vector<int> fHits;
};

#endif
