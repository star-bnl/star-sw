//-*- Mode: C++ -*-
// $Id: AliHLTTPCCAGBTrack.h,v 1.2 2010/08/02 16:45:28 ikulakov Exp $
// ************************************************************************
// This file is property of and copyright by the ALICE HLT Project        *
// ALICE Experiment at CERN, All rights reserved.                         *
// See cxx source for full Copyright notice                               *
//                                                                        *
//*************************************************************************

#ifndef ALIHLTTPCCAGBTRACK_H
#define ALIHLTTPCCAGBTRACK_H


#include "AliHLTTPCCADef.h"
#include "AliHLTTPCCATrackParam.h"

namespace std
{
  template<typename T> struct char_traits;
  template<typename _CharT, typename _Traits> class basic_istream;
  typedef basic_istream<char, char_traits<char> > istream;
  template<typename _CharT, typename _Traits> class basic_ostream;
  typedef basic_ostream<char, char_traits<char> > ostream;
} // namespace std

/**
 * @class AliHLTTPCCAGBTrack
 *
 *
 */
class AliHLTTPCCAGBTrack
{
    friend std::istream &operator>>( std::istream &, AliHLTTPCCAGBTrack & );
    friend std::ostream &operator<<( std::ostream &, const AliHLTTPCCAGBTrack & );
  public:

    AliHLTTPCCAGBTrack(): fFirstHitRef( 0 ), fNHits( 0 ), fInnerParam(), fOuterParam(), fAlpha( 0 ), fDeDx( 0 ) { ; }

    int NHits()               const { return fNHits; }
    int FirstHitRef()         const { return fFirstHitRef; }
    const AliHLTTPCCATrackParam &Param() const { return InnerParam(); }
    const AliHLTTPCCATrackParam &InnerParam() const { return fInnerParam; }
    const AliHLTTPCCATrackParam &OuterParam() const { return fOuterParam; }
    float Alpha()            const { return fAlpha; }
    float DeDx()             const { return fDeDx; }


    void SetNHits( int v )                 {  fNHits = v; }
    void SetFirstHitRef( int v )           {  fFirstHitRef = v; }
    void SetInnerParam( const AliHLTTPCCATrackParam &v ) {  fInnerParam = v; }
    void SetOuterParam( const AliHLTTPCCATrackParam &v ) {  fOuterParam = v; }
    void SetAlpha( float v )               {  fAlpha = v; }
    void SetDeDx( float v )                {  fDeDx = v; }


    static bool ComparePNClusters( const AliHLTTPCCAGBTrack *a, const AliHLTTPCCAGBTrack *b ) {
      return ( a->fNHits > b->fNHits );
    }

  protected:

    int fFirstHitRef;        // index of the first hit reference in track->hit reference array
    int fNHits;              // number of track hits
    AliHLTTPCCATrackParam fInnerParam; // fitted track parameters
    AliHLTTPCCATrackParam fOuterParam;
    float fAlpha;             //* Alpha angle of the parametrerisation
    float fDeDx;              //* DE/DX
};

std::istream &operator>>( std::istream &, AliHLTTPCCAGBTrack & );
std::ostream &operator<<( std::ostream &, const AliHLTTPCCAGBTrack & );

#endif
