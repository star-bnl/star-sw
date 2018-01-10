//***************************************************************************
// This file is property of and copyright by the ALICE HLT Project          *
// ALICE Experiment at CERN, All rights reserved.                           *
//                                                                          *
// Primary Authors: Sergey Gorbunov <sergey.gorbunov@kip.uni-heidelberg.de> *
//                  Ivan Kisel <kisel@kip.uni-heidelberg.de>                *
//                  for The ALICE HLT Project.                              *
//                                                                          *
// Developed by:   Igor Kulakov <I.Kulakov@gsi.de>                          *
//                 Maksym Zyzak <M.Zyzak@gsi.de>                            *
//                                                                          *
// Permission to use, copy, modify and distribute this software and its     *
// documentation strictly for non-commercial purposes is hereby granted     *
// without fee, provided that the above copyright notice appears in all     *
// copies and that both the copyright notice and this permission notice     *
// appear in the supporting documentation. The authors make no claims       *
// about the suitability of this software for any purpose. It is            *
// provided "as is" without express or implied warranty.                    *
//                                                                          *
//***************************************************************************

#include "AliHLTTPCCAHitArea.h"
#include "AliHLTTPCCASliceDataVector.h"
#include "AliHLTTPCCAGrid.h"
#include "AliHLTTPCCARow.h"
#include "debug.h"

AliHLTTPCCAHitArea::AliHLTTPCCAHitArea( const AliHLTTPCCARow &row, const AliHLTTPCCASliceData &slice,
    const float_v &y, const float_v &z, float dy, float dz, int_m mask )
  : fRow( row ), fSlice( slice ),
  fHitYlst( Vc::Zero ),
  fIh( Vc::Zero ),
  fNy( fRow.Grid().Ny() )
{
  const AliHLTTPCCAGrid &grid = fRow.Grid();

  const float_v minZ = z - dz;
  const float_v maxZ = z + dz;
  const float_v minY = y - dy;
  const float_v maxY = y + dy;

  uint_v bYmin, bZmin, bYmax; // boundary bin indexes
  grid.GetBinBounded( minY, minZ, &bYmin, &bZmin );
  grid.GetBinBounded( maxY, maxZ, &bYmax, &fBZmax );

  fBDY = ( bYmax - bYmin + 1 ); // bin index span in y direction

  fIndYmin = ( bZmin * fNy + bYmin ); // same as grid.GetBin(fMinY, fMinZ), i.e. the smallest bin index of interest
  // fIndYmin + fBDY then is the largest bin index of interest with the same Z

  fIz = bZmin;

  const int_m invalidMask = !mask;
  if ( !invalidMask.isEmpty() ) {
    debugS() << "not all parts of the HitArea are valid: " << mask << std::endl;

    fBZmax.setZero( invalidMask );
    fBDY.setZero( invalidMask );
    fIndYmin.setZero( invalidMask );
    fIz.setZero( invalidMask );

      // for given fIz (which is min atm.) get
    fIh.gather( fSlice.FirstUnusedHitInBin( fRow ), fIndYmin, mask ); // first and
    fHitYlst.gather( fSlice.FirstUnusedHitInBin( fRow ), fIndYmin + fBDY, mask ); // last hit index in the bin
  } else {
    fIh = fSlice.FirstUnusedHitInBin( fRow, fIndYmin );
    fHitYlst = fSlice.FirstUnusedHitInBin( fRow, fIndYmin + fBDY );
  }

  debugS() << "HitArea created:\n"
    << "bYmin:    " << bYmin << "\n"
    << "bZmin:    " << bZmin << "\n"
    << "bYmax:    " << bYmax << "\n"
    << "fBZmax:   " << fBZmax << "\n"
    << "fBDY:     " << fBDY << "\n"
    << "fIndYmin: " << fIndYmin << "\n"
    << "fIz:      " << fIz << "\n"
    << "fHitYlst: " << fHitYlst << "\n"
    << "fIh:      " << fIh << "\n"
    << "fNy:      " << fNy << std::endl;
  
#ifdef __ASSERT_YF__
  ASSERT( fHitYlst <= fRow.NUnusedHits() || invalidMask, fHitYlst << " <= " << fRow.NUnusedHits() );
#endif
}

uint_m AliHLTTPCCAHitArea::GetNext( NeighbourData *data )
{
  // get next hit index

  uint_m yIndexOutOfRange = fIh >= fHitYlst;     // current y is not in the area
  uint_m nextZIndexOutOfRange = fIz >= fBZmax;   // there isn't any new z-line

  if ( yIndexOutOfRange.isFull() && nextZIndexOutOfRange.isFull() ) { // all iterators are over the end
    if (data)
      data->fValid = uint_m(false);
    return uint_m(false);
  }

    // at least one entry in the vector has (fIh >= fHitYlst && fIz < fBZmax)
  uint_m needNextZ = yIndexOutOfRange && !nextZIndexOutOfRange;

    // skip as long as fIh is outside of the interesting bin y-index
  while ( !needNextZ.isEmpty() ) {
    
    ++fIz( needNextZ );
    nextZIndexOutOfRange = fIz >= fBZmax;
    
      // get next hit
    fIndYmin( needNextZ ) += fNy;
    fIh.gather( fSlice.FirstUnusedHitInBin( fRow ), fIndYmin, needNextZ ); // get first hit in cell, if z-line is new
    fHitYlst.gather( fSlice.FirstUnusedHitInBin( fRow ), fIndYmin + fBDY, needNextZ );
#ifdef __ASSERT_YF__
    assert( fHitYlst <= fRow.NUnusedHits() || !needNextZ );
#endif    
    yIndexOutOfRange = fIh >= fHitYlst;

    needNextZ = yIndexOutOfRange && !nextZIndexOutOfRange;
  }

  if (data) {
    data->fValid = !yIndexOutOfRange;
    const float_m valid( data->fValid );

    data->fY = fSlice.UnusedHitPDataY( fRow, fIh, valid );
    data->fZ = fSlice.UnusedHitPDataZ( fRow, fIh, valid );
    data->fLinks = fIh;
  }
  
  ++fIh;

  return !yIndexOutOfRange;
}

uint_v AliHLTTPCCAHitArea::NHits()
{ // suppose we didn't call GetNextHit yet!
  uint_v nHits(Vc::Zero);
  uint_v iz = fIz;
  uint_v indYmin = fIndYmin;
  uint_v ih = fIh;
  uint_v hitYlst = fHitYlst;
  uint_m needNextZ = iz < fBZmax;
  nHits += hitYlst - ih;
  while ( !needNextZ.isEmpty() ) {
    ++iz( needNextZ );   // get new z-line
    indYmin( needNextZ ) += fNy;
    ih.gather( fSlice.FirstUnusedHitInBin( fRow ), indYmin, needNextZ ); // get first hit in cell, if z-line is new
    hitYlst.gather( fSlice.FirstUnusedHitInBin( fRow ), indYmin + fBDY, needNextZ );
    nHits( needNextZ ) += hitYlst - ih;
    
    needNextZ = iz < fBZmax;
  }

  return nHits;
}
