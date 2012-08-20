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

#include <valgrind/memcheck.h>

AliHLTTPCCAHitArea::AliHLTTPCCAHitArea( const AliHLTTPCCARow &row, const AliHLTTPCCASliceData &slice,
    const sfloat_v &y, const sfloat_v &z, float dy, float dz, short_m mask )
  : fRow( row ), fSlice( slice ),
  fHitYlst( Vc::Zero ),
  fIh( Vc::Zero ),
  fNy( fRow.Grid().Ny() )
{
  const AliHLTTPCCAGrid &grid = fRow.Grid();

  const sfloat_v minZ = z - dz;
  const sfloat_v maxZ = z + dz;
  const sfloat_v minY = y - dy;
  const sfloat_v maxY = y + dy;

  ushort_v bYmin, bZmin, bYmax; // boundary bin indexes
  grid.GetBinBounded( minY, minZ, &bYmin, &bZmin );
  grid.GetBinBounded( maxY, maxZ, &bYmax, &fBZmax );

  fBDY = ( bYmax - bYmin + 1 ); // bin index span in y direction

  fIndYmin = ( bZmin * fNy + bYmin ); // same as grid.GetBin(fMinY, fMinZ), i.e. the smallest bin index of interest
  // fIndYmin + fBDY then is the largest bin index of interest with the same Z

  fIz = bZmin;

  const short_m invalidMask = !mask;
  if ( !invalidMask.isEmpty() ) {
    debugS() << "not all parts of the HitArea are valid: " << mask << std::endl;

    fBZmax.setZero( invalidMask );
    fBDY.setZero( invalidMask );
    fIndYmin.setZero( invalidMask );
    fIz.setZero( invalidMask );

    // for given fIz (which is min atm.) get
    fIh.gather( fSlice.FirstHitInBin( fRow ), fIndYmin, mask ); // first and
    fHitYlst.gather( fSlice.FirstHitInBin( fRow ), fIndYmin + fBDY, mask ); // last hit index in the bin
  } else {
    fIh = fSlice.FirstHitInBin( fRow, fIndYmin );
    fHitYlst = fSlice.FirstHitInBin( fRow, fIndYmin + fBDY );
  }
  assert( fHitYlst <= fRow.NHits() || invalidMask );

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
}

bool AliHLTTPCCAHitArea::GetNext( NeighbourData *data )
{
  // get next hit index
  
  VALGRIND_CHECK_VALUE_IS_DEFINED( fIh );
  VALGRIND_CHECK_VALUE_IS_DEFINED( fHitYlst );
  VALGRIND_CHECK_VALUE_IS_DEFINED( fIz );
  VALGRIND_CHECK_VALUE_IS_DEFINED( fBZmax );
  ushort_m yIndexOutOfRange = fIh >= fHitYlst;
  ushort_m zIndexOutOfRange = fIz >= fBZmax;
  ushort_m isUsed = static_cast<ushort_m>( short_v(fSlice.HitDataIsUsed( fRow ), fIh) != short_v( Vc::Zero ) ); // TODO change rot.fDataIsUsed type on ushort
  VALGRIND_CHECK_VALUE_IS_DEFINED( yIndexOutOfRange );
  VALGRIND_CHECK_VALUE_IS_DEFINED( zIndexOutOfRange );
  if ( yIndexOutOfRange && zIndexOutOfRange ) { // all iterators are over the end
    return false;
  }

  // at least one entry in the vector has (fIh >= fHitYlst && fIz < fBZmax)
  ushort_m needNextZ = yIndexOutOfRange && !zIndexOutOfRange;
  ushort_m needNextHit = isUsed && !yIndexOutOfRange && !zIndexOutOfRange;

  debugS() << "fIh >= fHitYlst: " <<  yIndexOutOfRange << " fIz >= fBZmax: " << zIndexOutOfRange << " -> " << needNextZ << std::endl;

  // skip as long as fIh is outside of the interesting bin y-index
  while ( !(needNextZ || needNextHit).isEmpty() ) {
    // go to next z and start y from the min again
    ++fIz( needNextZ );
    fIndYmin( needNextZ ) += fNy;
    ++fIh( needNextHit );
    
    fIh.gather( fSlice.FirstHitInBin( fRow ), fIndYmin, needNextZ );
    fHitYlst.gather( fSlice.FirstHitInBin( fRow ), fIndYmin + fBDY, needNextZ );
    assert( fHitYlst <= fRow.NHits() || !needNextZ );

    yIndexOutOfRange = fIh >= fHitYlst;
    zIndexOutOfRange = fIz >= fBZmax;
    isUsed = static_cast<ushort_m>( short_v(fSlice.HitDataIsUsed( fRow ), fIh) != short_v( Vc::Zero ) );

    needNextZ = yIndexOutOfRange && !zIndexOutOfRange;
    needNextHit = isUsed && !yIndexOutOfRange && !zIndexOutOfRange;
    debugS() << "fIh >= fHitYlst: " <<  yIndexOutOfRange << " fIz >= fBZmax: " << zIndexOutOfRange << " -> " << needNextZ << std::endl;
  }

  data->fValid = !yIndexOutOfRange && !isUsed;

  const sfloat_m valid( data->fValid );

  data->fY.setZero();
  data->fY.gather( fSlice.HitDataY( fRow ), fIh, valid );
  data->fZ.setZero();
  data->fZ.gather( fSlice.HitDataZ( fRow ), fIh, valid );

  data->fLinks = fIh.staticCast<short_v>();

  ++fIh;

  debugS() << "HitArea found next. New state:\n"
    << "fIndYmin: " << fIndYmin << "\n"
    << "fIz:      " << fIz << "\n"
    << "fHitYlst: " << fHitYlst << "\n"
    << "fIh:      " << fIh << std::endl;

  return true;
}
