//-*- Mode: C++ -*-
// $Id: AliHLTTPCCAGrid.h,v 1.1 2016/02/05 23:27:27 fisyak Exp $
// ************************************************************************
// This file is property of and copyright by the ALICE HLT Project        *
// ALICE Experiment at CERN, All rights reserved.                         *
// See cxx source for full Copyright notice                               *
//                                                                        *
//*************************************************************************

#ifndef ALIHLTTPCCAGRID_H
#define ALIHLTTPCCAGRID_H

#include "AliHLTTPCCADef.h"
#include "AliHLTTPCCAMath.h"

/**
 * @class AliHLTTPCCAGrid
 *
 * 2-dimensional grid of pointers.
 * pointers to (y,z)-like objects are assigned to the corresponding grid bin
 * used by AliHLTTPCCATracker to speed-up the hit operations
 * grid axis are named Z,Y to be similar to TPC row coordinates.
 */
class AliHLTTPCCAGrid {
  public:
    void CreateEmpty();
    void Create1( float y, float z, float sy, float sz );
    void Create( float yMin, float yMax, float zMin, float zMax, float sy, float sz  );

    int GetBin( float Y, float Z ) const;
    /**
     * returns -1 if the row is empty == no hits
     */
    ushort_v GetBinBounded( const sfloat_v &Y, const sfloat_v &Z ) const;
    void GetBinBounded( const sfloat_v &Y, const sfloat_v &Z, ushort_v *bY, ushort_v *bZ ) const;

    static ushort_v GetBinBounded( const AliHLTTPCCAGrid *array, const ushort_v &indexes, const sfloat_v &Y, const sfloat_v &Z );
    static void GetBinBounded( const AliHLTTPCCAGrid *array, const ushort_v &indexes, const sfloat_v &Y, const sfloat_v &Z, ushort_v *bY, ushort_v *bZ );
    static ushort_v Ny( const AliHLTTPCCAGrid *array, const ushort_v &indexes ) { return ushort_v( array, &AliHLTTPCCAGrid::fNy, indexes ); }

    void GetBinBounds( int iBin, float &Ymin, float &Ymax, float &Zmin, float &Zmax) const;
    
    unsigned int   N()        const { return fN;  }
    unsigned short Ny()       const { return fNy; }
    unsigned short Nz()       const { return fNz; }

  private:

    unsigned int   fN;       //* total N bins
    unsigned short fNy;      //* N bins in Y
    unsigned short fNz;      //* N bins in Z
    float fYMinOverStep;     //* minimal Y value * fStepYInv
    float fZMinOverStep;     //* minimal Z value * fStepZInv
    float fStepYInv; //* inverse bin size in Y
    float fStepZInv; //* inverse bin size in Z
};

inline ushort_v AliHLTTPCCAGrid::GetBinBounded( const AliHLTTPCCAGrid *array, const ushort_v &indexes, const sfloat_v &Y, const sfloat_v &Z )
{
//   const sfloat_v fZMinOverStep( array, &AliHLTTPCCAGrid::fZMinOverStep, indexes );
//   const sfloat_v fStepZInv( array, &AliHLTTPCCAGrid::fStepZInv, indexes );
//   const ushort_v fNz( array, &AliHLTTPCCAGrid::fNz, indexes );
//   ushort_v zBin = ( Z * fStepZInv - fZMinOverStep ).staticCast<unsigned short>();
//   zBin = CAMath::Max( ushort_v( Vc::Zero ), CAMath::Min( ushort_v( fNz - 1 ), zBin ) );
// 
//   const sfloat_v fYMinOverStep( array, &AliHLTTPCCAGrid::fYMinOverStep, indexes );
//   const sfloat_v fStepYInv( array, &AliHLTTPCCAGrid::fStepYInv, indexes );
//   const ushort_v fNy( array, &AliHLTTPCCAGrid::fNy, indexes );
//   ushort_v yBin = ( Y * fStepYInv - fYMinOverStep ).staticCast<unsigned short>();
//   yBin = CAMath::Max( ushort_v( Vc::Zero ), CAMath::Min( ushort_v( fNy - 1 ), yBin ) );
  
  const sfloat_v fZMinOverStep( array, &AliHLTTPCCAGrid::fZMinOverStep, indexes );
  const sfloat_v fStepZInv( array, &AliHLTTPCCAGrid::fStepZInv, indexes );
  const ushort_v fNz( array, &AliHLTTPCCAGrid::fNz, indexes );
  short_v zBin = ( Z * fStepZInv - fZMinOverStep ).staticCast<short_v>();
  ushort_v zBin2 = CAMath::Max( short_v( Vc::Zero ), CAMath::Min( short_v( fNz - 1 ), zBin ) ).staticCast<ushort_v>();

  const sfloat_v fYMinOverStep( array, &AliHLTTPCCAGrid::fYMinOverStep, indexes );
  const sfloat_v fStepYInv( array, &AliHLTTPCCAGrid::fStepYInv, indexes );
  const ushort_v fNy( array, &AliHLTTPCCAGrid::fNy, indexes );
  short_v yBin = ( Y * fStepYInv - fYMinOverStep ).staticCast<short_v>();
  ushort_v yBin2 = CAMath::Max( short_v( Vc::Zero ), CAMath::Min( short_v( fNy - 1 ), yBin ) ).staticCast<ushort_v>();
  return zBin2 * fNy + yBin2;
}

inline void AliHLTTPCCAGrid::GetBinBounded( const AliHLTTPCCAGrid *array, const ushort_v &indexes, const sfloat_v &Y, const sfloat_v &Z, ushort_v *bY, ushort_v *bZ )
{
//   const sfloat_v fYMinOverStep( array, &AliHLTTPCCAGrid::fYMinOverStep, indexes );
//   const sfloat_v fStepYInv( array, &AliHLTTPCCAGrid::fStepYInv, indexes );
//   const ushort_v fNy( array, &AliHLTTPCCAGrid::fNy, indexes );
//   const ushort_v &yBin = ( Y * fStepYInv - fYMinOverStep ).staticCast<unsigned short>();
//   *bY = CAMath::Max( ushort_v( Vc::Zero ), CAMath::Min( fNy - 1, yBin ) );
// 
//   const sfloat_v fZMinOverStep( array, &AliHLTTPCCAGrid::fZMinOverStep, indexes );
//   const sfloat_v fStepZInv( array, &AliHLTTPCCAGrid::fStepZInv, indexes );
//   const ushort_v fNz( array, &AliHLTTPCCAGrid::fNz, indexes );
//   const ushort_v &zBin = ( Z * fStepZInv - fZMinOverStep ).staticCast<unsigned short>();
//   *bZ = CAMath::Max( ushort_v( Vc::Zero ), CAMath::Min( fNz - 1, zBin ) );
  
  const sfloat_v fYMinOverStep( array, &AliHLTTPCCAGrid::fYMinOverStep, indexes );
  const sfloat_v fStepYInv( array, &AliHLTTPCCAGrid::fStepYInv, indexes );
  const ushort_v fNy( array, &AliHLTTPCCAGrid::fNy, indexes );
  const short_v fNy2 = fNy.staticCast<short_v>();
  const short_v &yBin = ( Y * fStepYInv - fYMinOverStep ).staticCast<short_v>();
  *bY = CAMath::Max( short_v( Vc::Zero ), CAMath::Min( fNy2 - 1, yBin ) ).staticCast<ushort_v>();

  const sfloat_v fZMinOverStep( array, &AliHLTTPCCAGrid::fZMinOverStep, indexes );
  const sfloat_v fStepZInv( array, &AliHLTTPCCAGrid::fStepZInv, indexes );
  const ushort_v fNz( array, &AliHLTTPCCAGrid::fNz, indexes );
  const short_v fNz2 = fNz.staticCast<short_v>();
  const short_v &zBin = ( Z * fStepZInv - fZMinOverStep ).staticCast<short_v>();
  *bZ = CAMath::Max( short_v( Vc::Zero ), CAMath::Min( fNz2 - 1, zBin ) ).staticCast<ushort_v>();
}

inline ushort_v AliHLTTPCCAGrid::GetBinBounded( const sfloat_v &Y, const sfloat_v &Z ) const
{
  //* get the bin pointer

  ushort_v yBin, zBin;
  GetBinBounded( Y, Z, &yBin, &zBin );
  return zBin * fNy + yBin;

  // XXX: the code below can wrap incorrectly because saturation is only done after
  // calculation of bin:
//X   const ushort_v &yBin = ( Y * fStepYInv - fYMinOverStep ).staticCast<unsigned short>();
//X   const ushort_v &zBin = ( Z * fStepZInv - fZMinOverStep ).staticCast<unsigned short>();
//X   const ushort_v &bin = zBin * fNy + yBin;
//X 
//X   return CAMath::Max( ushort_v( Vc::Zero ), CAMath::Min( ushort_v( fN - 1 ), bin ) );
}

inline void AliHLTTPCCAGrid::GetBinBounded( const sfloat_v &Y, const sfloat_v &Z, ushort_v *bY, ushort_v *bZ ) const
{
  //* get the bin pointer

//   const ushort_v &yBin = ( Y * fStepYInv - fYMinOverStep ).staticCast<unsigned short>();
//   const ushort_v &zBin = ( Z * fStepZInv - fZMinOverStep ).staticCast<unsigned short>();
// 
//   *bY = CAMath::Max( ushort_v( Vc::Zero ), CAMath::Min( ushort_v( fNy - 1 ), yBin ) );
//   *bZ = CAMath::Max( ushort_v( Vc::Zero ), CAMath::Min( ushort_v( fNz - 1 ), zBin ) );
// IKu bag was here   :  -1 = 65000 > 0  !

  const short_v &yBin = ( Y * fStepYInv - fYMinOverStep ).staticCast<short_v>();
  const short_v &zBin = ( Z * fStepZInv - fZMinOverStep ).staticCast<short_v>();

  *bY = CAMath::Max( short_v( Vc::Zero ), CAMath::Min( short_v( fNy - 1 ), yBin ) ).staticCast<ushort_v>();
  *bZ = CAMath::Max( short_v( Vc::Zero ), CAMath::Min( short_v( fNz - 1 ), zBin ) ).staticCast<ushort_v>();

}

#endif
