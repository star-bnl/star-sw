/*
 * This file is part of TPCCATracker package
 * Copyright (C) 2007-2020 FIAS Frankfurt Institute for Advanced Studies
 *               2007-2020 Goethe University of Frankfurt
 *               2007-2020 Ivan Kisel <I.Kisel@compeng.uni-frankfurt.de>
 *               2007-2019 Sergey Gorbunov
 *               2007-2019 Maksym Zyzak
 *               2007-2014 Igor Kulakov
 *               2014-2020 Grigory Kozlov
 *
 * TPCCATracker is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * TPCCATracker is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

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

    AliHLTTPCCAGBTrack()
      : fInnerParam()
      , fOuterParam()
      , fAlpha( 0 )
      , fDeDx( 0 )
      , fOuterAlpha( 0 )
      , fNDeDx(0)
      , fFirstHitRef( 0 )
      , fNHits( 0 )
      , fIsLooper(0)
      , fReco(false)
      , fIsClone(false)
      , fIsMerged(false)
      , fReverse(false)
    {}

    int NHits()               const { return fNHits; }
    int FirstHitRef()         const { return fFirstHitRef; }
    const AliHLTTPCCATrackParam &Param() const { return InnerParam(); }
    const AliHLTTPCCATrackParam &InnerParam() const { return fInnerParam; }
    const AliHLTTPCCATrackParam &OuterParam() const { return fOuterParam; }
    float Alpha()            const { return fAlpha; }
    int   NDeDx()            const { return fNDeDx; }
    float DeDx()             const { return fDeDx; }


    void SetNHits( int v )                 {  fNHits = v; }
    void SetFirstHitRef( int v )           {  fFirstHitRef = v; }
    void SetInnerParam( const AliHLTTPCCATrackParam &v ) {  fInnerParam = v; }
    void SetOuterParam( const AliHLTTPCCATrackParam &v ) {  fOuterParam = v; }
    void SetAlpha( float v )               {  fAlpha = v; }
    void SetNDeDx( int n)                  {  fNDeDx = n; }
    void SetDeDx( float v )                {  fDeDx = v; }


    static bool ComparePNClusters( const AliHLTTPCCAGBTrack *a, const AliHLTTPCCAGBTrack *b ) {
      return ( a->fNHits > b->fNHits );
    }

    void SetMerged() { fIsMerged = true; }
    bool IsMerged() const { return fIsMerged; }
    void SetClone( bool c = true ) { fIsClone = c; }
    bool IsClone() const { return fIsClone; }
#ifdef LOOPER_TEST
    void SetFirstMC( int mc ) { fFirstMC = mc; }
    int GetFirstMC() const { return fFirstMC; }
#endif
    float OuterAlpha()            const { return fOuterAlpha; }
    void SetOuterAlpha( float v )               {  fOuterAlpha = v; }
    void SetReco( bool r ) { fReco = r; }
    bool IsReco() const { return fReco; }
    void SetLooper() { fIsLooper++; }
    void SetLooperClone() { fIsLooper = -1; }
    int IsLooper() { return fIsLooper; }
    int IsLooper() const { return fIsLooper; }
    void SetReverse( bool r = true ) { fReverse = r; }
    bool IsReverse() const { return fReverse; }

    void ReverseInnerPar() { fInnerParam.ReversePar(); }
    void ReverseOuterPar() { fOuterParam.ReversePar(); }

  protected:

    AliHLTTPCCATrackParam fInnerParam; // fitted track parameters
    AliHLTTPCCATrackParam fOuterParam;
    float fAlpha;             //* Alpha angle of the parametrerisation
    float fDeDx;              //* DE/DX
    float fOuterAlpha;
    int   fNDeDx;
    int fFirstHitRef;        // index of the first hit reference in track->hit reference array
    int fNHits;              // number of track hits
    int fIsLooper;
#ifdef LOOPER_TEST
    int fFirstMC;
#endif

    bool fReco;
    bool fIsClone;
    bool fIsMerged;
    bool fReverse;
};

std::istream &operator>>( std::istream &, AliHLTTPCCAGBTrack & );
std::ostream &operator<<( std::ostream &, const AliHLTTPCCAGBTrack & );

#endif
