//-*- Mode: C++ -*-
// @(#) $Id: AliHLTTPCCAOutTrack.h,v 1.1 2016/02/05 23:27:28 fisyak Exp $
// ************************************************************************
// This file is property of and copyright by the ALICE HLT Project        *
// ALICE Experiment at CERN, All rights reserved.                         *
// See cxx source for full Copyright notice                               *
//                                                                        *
//*************************************************************************

#ifndef ALIHLTTPCCAOUTTRACK_H
#define ALIHLTTPCCAOUTTRACK_H

#include "AliHLTTPCCATrackParam.h"
#if 0
namespace std
{
  template<typename T> struct char_traits;
  template<typename _CharT, typename _Traits> class basic_istream;
  typedef basic_istream<char, char_traits<char> > istream;
  template<typename _CharT, typename _Traits> class basic_ostream;
  typedef basic_ostream<char, char_traits<char> > ostream;
} // namespace std
#endif
/**
 * @class AliHLTTPCCAOutTrack
 * AliHLTTPCCAOutTrack class is used to store the final
 * reconstructed tracks which will be then readed
 * by the AliHLTTPCCATrackerComponent
 *
 * The class contains no temporary variables, etc.
 *
 */
class AliHLTTPCCAOutTrack
{
    friend std::istream &operator>>( std::istream &, AliHLTTPCCAOutTrack & );
    friend std::ostream &operator<<( std::ostream &, const AliHLTTPCCAOutTrack & );
  public:

    AliHLTTPCCAOutTrack(): fFirstHitRef( 0 ), fNHits( 0 ), fStartPoint(), fEndPoint(), fOrigTrackID( 0 ) {}
    virtual ~AliHLTTPCCAOutTrack() {}

    int NHits()               const { return fNHits; }
    int FirstHitRef()         const { return fFirstHitRef; }

    const AliHLTTPCCATrackParam &StartPoint() const { return fStartPoint; }
    const AliHLTTPCCATrackParam &EndPoint()   const { return fEndPoint; }
    int OrigTrackID()                const { return fOrigTrackID; }

    void SetNHits( int v )               { fNHits = v; }
    void SetFirstHitRef( int v )         { fFirstHitRef = v; }

    void SetStartPoint( const AliHLTTPCCATrackParam &v ) { fStartPoint = v; }
    void SetEndPoint( const AliHLTTPCCATrackParam &v )   { fEndPoint = v; }
    void SetOrigTrackID( int v )                { fOrigTrackID = v; }

  protected:
    int fFirstHitRef;   //* index of the first hit reference in track->hit reference array
    int fNHits;         //* number of track hits
    AliHLTTPCCATrackParam fStartPoint; //* fitted track parameters at the start point
    AliHLTTPCCATrackParam fEndPoint;   //* fitted track parameters at the start point
    int fOrigTrackID;                //* index of the original slice track
};

std::istream &operator>>( std::istream &in, AliHLTTPCCAOutTrack &ot );
std::ostream &operator<<( std::ostream &out, const AliHLTTPCCAOutTrack &ot );

#endif
