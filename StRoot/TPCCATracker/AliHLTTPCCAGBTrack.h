//-*- Mode: C++ -*-
// $Id: AliHLTTPCCAGBTrack.h,v 1.1 2013/01/28 21:19:03 yiguo Exp $
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

    AliHLTTPCCAGBTrack(): fFirstHitRef( 0 ), fNHits( 0 ), fInnerParam(), fOuterParam(), fAlpha( 0 ), fNDeDx(0), fDeDx( 0 ), tIsLooper(0), tIsMerged(false), tReverse(false) { ; }

    int NHits()               const { return fNHits; }
    int FirstHitRef()         const { return fFirstHitRef; }
    const AliHLTTPCCATrackParam &Param() const { return InnerParam(); }
    const AliHLTTPCCATrackParam &InnerParam() const { return fInnerParam; }
    const AliHLTTPCCATrackParam &OuterParam() const { return fOuterParam; }
    float Alpha()            const { return fAlpha; }
    float OuterAlpha()       const { return fOuterAlpha; }
    int   NDeDx()            const { return fNDeDx; }
    float DeDx()             const { return fDeDx; }


    void SetNHits( int v )                 {  fNHits = v; }
    void SetFirstHitRef( int v )           {  fFirstHitRef = v; }
    void SetInnerParam( const AliHLTTPCCATrackParam &v ) {  fInnerParam = v; }
    void SetOuterParam( const AliHLTTPCCATrackParam &v ) {  fOuterParam = v; }
    void SetAlpha( float v )               {  fAlpha = v; }
    void SetOuterAlpha( float v )          {  fOuterAlpha = v; }
    void SetNDeDx( int n)                  {  fNDeDx = n; }
    void SetDeDx( float v )                {  fDeDx = v; }


    static bool ComparePNClusters( const AliHLTTPCCAGBTrack *a, const AliHLTTPCCAGBTrack *b ) {
      return ( a->fNHits > b->fNHits );
    }

    // ---
    void SetMerged() { tIsMerged = true; }
    bool IsMerged() const { return tIsMerged; }
    void SetQPt0( float qpt ) { tQPt0 = qpt; }
    float QPt0() const { return tQPt0; }
    void SetDzDs0( float dzds ) { tDzDs0 = dzds; }
    float DzDs0() const { return tDzDs0; }
    void SetQPt1( float qpt ) { tQPt1 = qpt; }
    float QPt1() const { return tQPt1; }
    void SetDzDs1( float dzds ) { tDzDs1 = dzds; }
    float DzDs1() const { return tDzDs1; }
    void SetClone( bool c = true ) { tIsClone = c; }
    bool IsClone() const { return tIsClone; }
    void SetFirstMC( int mc ) { tFirstMC = mc; }
    int GetFirstMC() const { return tFirstMC; }

    void SetReco( bool r ) { tReco = r; }
    bool IsReco() const { return tReco; }

    void SetLooper() { tIsLooper++; }
    void SetLooperClone() { tIsLooper = -1; }
    int IsLooper() { return tIsLooper; }
    int IsLooper() const { return tIsLooper; }

    void SetReverse( bool r = true ) { tReverse = r; }
    bool IsReverse() const { return tReverse; }

    void ReverseInnerPar() { fInnerParam.ReversePar(); }
    void ReverseOuterPar() { fOuterParam.ReversePar(); }
    // ---

  protected:

    int fFirstHitRef;        // index of the first hit reference in track->hit reference array
    int fNHits;              // number of track hits
    AliHLTTPCCATrackParam fInnerParam; // fitted track parameters
    AliHLTTPCCATrackParam fOuterParam;
    float fAlpha;             //* Alpha angle of the parametrerisation
    float fOuterAlpha;
    int   fNDeDx;
    float fDeDx;              //* DE/DX

    // ---
    float tQPt0;
    float tDzDs0;
    float tQPt1;
    float tDzDs1;
    int tFirstMC;

    int tIsLooper;

    bool tReco;
    bool tIsClone;
    bool tIsMerged;

    bool tReverse;
    // ---
};

std::istream &operator>>( std::istream &, AliHLTTPCCAGBTrack & );
std::ostream &operator<<( std::ostream &, const AliHLTTPCCAGBTrack & );

#endif
