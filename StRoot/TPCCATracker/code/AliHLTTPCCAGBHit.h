//-*- Mode: C++ -*-
// $Id: AliHLTTPCCAGBHit.h,v 1.1.1.1 2010/07/26 20:55:38 ikulakov Exp $
// ************************************************************************
// This file is property of and copyright by the ALICE HLT Project        *
// ALICE Experiment at CERN, All rights reserved.                         *
// See cxx source for full Copyright notice                               *
//                                                                        *
//*************************************************************************

#ifndef ALIHLTTPCCAGBHIT_H
#define ALIHLTTPCCAGBHIT_H

#include "AliHLTTPCCADef.h"

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

    static bool CompareRowDown( const AliHLTTPCCAGBHit &a, const AliHLTTPCCAGBHit &b ) {
      return ( a.fIRow > b.fIRow );
    }
    static bool ComparePRowDown( const AliHLTTPCCAGBHit *a, const AliHLTTPCCAGBHit *b ) {
      return ( a->fIRow > b->fIRow );
    }

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
{
  //* Comparison function for sorting hits
  if ( a.fISlice < b.fISlice ) return 1;
  if ( a.fISlice > b.fISlice ) return 0;
  if ( a.fIRow < b.fIRow ) return 1;
  if ( a.fIRow > b.fIRow ) return 0;
  return ( a.fZ < b.fZ );
}

#endif
