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

#ifndef ALIHLTTPCCAGBHIT_H
#define ALIHLTTPCCAGBHIT_H

#include "AliHLTTPCCADef.h"

#include <iostream>
using std::ostream;
using std::istream;


/**
 * @class AliHLTTPCCAGBHit
 *
 * The AliHLTTPCCAGBHit class is the internal representation
 * of the TPC clusters for the AliHLTTPCCAGBTracker algorithm.
 *
 */
class AliHLTTPCCAGBHit
{
  public:
    AliHLTTPCCAGBHit()
        : fX( 0 ), fY( 0 ), fZ( 0 ), fErrX( 0 ), fErrY( 0 ), fErrZ( 0 ), fAmp( 0 ),
        fISlice( 0 ), fIRow( 0 ), fID( 0 ), fIsUsed( 0 ) {}

    float X() const { return fX; }
    float Y() const { return fY; }
    float Z() const { return fZ; }

    float ErrX() const { return fErrX; }
    float ErrY() const { return fErrY; }
    float ErrZ() const { return fErrZ; }
    float Amp()  const { return fAmp; }

    int ISlice() const { return fISlice; }
    int IRow() const { return fIRow; }
    int ID() const { return fID; }
    bool IsUsed() const { return fIsUsed; };

    void SetX( float v ) {  fX = v; }
    void SetY( float v ) {  fY = v; }
    void SetZ( float v ) {  fZ = v; }
    void SetErrX( float v ) {  fErrX = v; }
    void SetErrY( float v ) {  fErrY = v; }
    void SetErrZ( float v ) {  fErrZ = v; }
    void SetAmp( float v ) {  fAmp = v; }
    void SetISlice( int v ) {  fISlice = v; }
    void SetIRow( int v ) {  fIRow = v; }
    void SetID( int v ) {  fID = v; }
    void SetIsUsed( bool v ) {  fIsUsed = v; };

    static bool Compare( const AliHLTTPCCAGBHit &a, const AliHLTTPCCAGBHit &b );

/// \brief Hits reordering  in accordance with the geometry and the track-finder needs:
/// Hits are sorted by sector number at first, than by row number and at last by z-coordinate

    static bool CompareRowDown( const AliHLTTPCCAGBHit &a, const AliHLTTPCCAGBHit &b ) {
      return ( a.fIRow > b.fIRow );
      
/// Hits are sorted by  row number 
    }
    static bool ComparePRowDown( const AliHLTTPCCAGBHit *a, const AliHLTTPCCAGBHit *b ) {
      return ( a->fIRow > b->fIRow );
/// Hits are sorted by  row number 
    }

  friend ostream& operator<<(ostream& out, const AliHLTTPCCAGBHit &a);
  friend istream& operator>>(istream& in, AliHLTTPCCAGBHit &a);

  protected:

    float fX; //* X position
    float fY; //* Y position
    float fZ; //* Z position

    float fErrX; //* X position error
    float fErrY; //* Y position error
    float fErrZ; //* Z position error

    float fAmp;   //* Maximal amplitude
    int fISlice; //* slice number
    int fIRow;   //* row number
    int fID;     //* external ID (id of AliTPCcluster)
    bool fIsUsed; //* is used by GBTracks

    //ClassDef(AliHLTTPCCAGBHit,1);

};

inline bool AliHLTTPCCAGBHit::Compare( const AliHLTTPCCAGBHit &a, const AliHLTTPCCAGBHit &b )

/// \brief Hits reordering  in accordance with the geometry and the track-finder needs:
/// Hits are sorted by sector number at first, than by row number and at last by z-coordinate.



{
  //* Comparison function for sorting hits
  if ( a.fISlice < b.fISlice ) return 1;
  if ( a.fISlice > b.fISlice ) return 0;
  if ( a.fIRow < b.fIRow ) return 1;
  if ( a.fIRow > b.fIRow ) return 0;
  return ( a.fZ < b.fZ );
}

#endif
