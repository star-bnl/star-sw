//-*- Mode: C++ -*-
// $Id: AliHLTTPCCAGrid.h,v 1.1.1.1 2010/07/26 20:55:38 ikulakov Exp $
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
    uint_v GetBinBounded( const float_v &Y, const float_v &Z ) const;
    void GetBinBounded( const float_v &Y, const float_v &Z, uint_v *bY, uint_v *bZ ) const;

//     static uint_v GetBinBounded( const AliHLTTPCCAGrid *array, const uint_v &indexes, const float_v &Y, const float_v &Z );
//     static void GetBinBounded( const AliHLTTPCCAGrid *array, const uint_v &indexes, const float_v &Y, const float_v &Z, uint_v *bY, uint_v *bZ );
//     static uint_v Ny( const AliHLTTPCCAGrid *array, const uint_v &indexes ) { return uint_v( array, &AliHLTTPCCAGrid::fNy, indexes ); }

    void GetBinBounds( int iBin, float &Ymin, float &Ymax, float &Zmin, float &Zmax) const;
    
    unsigned int   N()        const { return fN;  }
    unsigned int Ny()       const { return fNy; }
    unsigned int Nz()       const { return fNz; }

  private:

    unsigned int   fN;       //* total N bins
    unsigned int fNy;      //* N bins in Y
    unsigned int fNz;      //* N bins in Z
    float fYMinOverStep;     //* minimal Y value * fStepYInv
    float fZMinOverStep;     //* minimal Z value * fStepZInv
    float fStepYInv; //* inverse bin size in Y
    float fStepZInv; //* inverse bin size in Z
};

// inline uint_v AliHLTTPCCAGrid::GetBinBounded( const AliHLTTPCCAGrid *array, const uint_v &indexes, const float_v &Y, const float_v &Z )
// {
// //   const float_v fZMinOverStep( array, &AliHLTTPCCAGrid::fZMinOverStep, indexes );
// //   const float_v fStepZInv( array, &AliHLTTPCCAGrid::fStepZInv, indexes );
// //   const uint_v fNz( array, &AliHLTTPCCAGrid::fNz, indexes );
// //   uint_v zBin = ( Z * fStepZInv - fZMinOverStep ).staticCast<unsigned int>();
// //   zBin = CAMath::Max( uint_v( Vc::Zero ), CAMath::Min( uint_v( fNz - 1 ), zBin ) );
// // 
// //   const float_v fYMinOverStep( array, &AliHLTTPCCAGrid::fYMinOverStep, indexes );
// //   const float_v fStepYInv( array, &AliHLTTPCCAGrid::fStepYInv, indexes );
// //   const uint_v fNy( array, &AliHLTTPCCAGrid::fNy, indexes );
// //   uint_v yBin = ( Y * fStepYInv - fYMinOverStep ).staticCast<unsigned int>();
// //   yBin = CAMath::Max( uint_v( Vc::Zero ), CAMath::Min( uint_v( fNy - 1 ), yBin ) );
//   
//   const float_v fZMinOverStep( array, &AliHLTTPCCAGrid::fZMinOverStep, indexes );
//   const float_v fStepZInv( array, &AliHLTTPCCAGrid::fStepZInv, indexes );
//   const uint_v fNz( array, &AliHLTTPCCAGrid::fNz, indexes );
//   int_v zBin = ( Z * fStepZInv - fZMinOverStep ).staticCast<int_v>();
//   uint_v zBin2 = CAMath::Max( int_v( Vc::Zero ), CAMath::Min( int_v( fNz - 1 ), zBin ) ).staticCast<uint_v>();
// 
//   const float_v fYMinOverStep( array, &AliHLTTPCCAGrid::fYMinOverStep, indexes );
//   const float_v fStepYInv( array, &AliHLTTPCCAGrid::fStepYInv, indexes );
//   const uint_v fNy( array, &AliHLTTPCCAGrid::fNy, indexes );
//   int_v yBin = ( Y * fStepYInv - fYMinOverStep ).staticCast<int_v>();
//   uint_v yBin2 = CAMath::Max( int_v( Vc::Zero ), CAMath::Min( int_v( fNy - 1 ), yBin ) ).staticCast<uint_v>();
//   return zBin2 * fNy + yBin2;
// }
// 
// inline void AliHLTTPCCAGrid::GetBinBounded( const Vc::vector<AliHLTTPCCAGrid> &array, const uint_v &indexes, const float_v &Y, const float_v &Z, uint_v *bY, uint_v *bZ )
// {
// //   const float_v fYMinOverStep( array, &AliHLTTPCCAGrid::fYMinOverStep, indexes );
// //   const float_v fStepYInv( array, &AliHLTTPCCAGrid::fStepYInv, indexes );
// //   const uint_v fNy( array, &AliHLTTPCCAGrid::fNy, indexes );
// //   const uint_v &yBin = ( Y * fStepYInv - fYMinOverStep ).staticCast<unsigned int>();
// //   *bY = CAMath::Max( uint_v( Vc::Zero ), CAMath::Min( fNy - 1, yBin ) );
// // 
// //   const float_v fZMinOverStep( array, &AliHLTTPCCAGrid::fZMinOverStep, indexes );
// //   const float_v fStepZInv( array, &AliHLTTPCCAGrid::fStepZInv, indexes );
// //   const uint_v fNz( array, &AliHLTTPCCAGrid::fNz, indexes );
// //   const uint_v &zBin = ( Z * fStepZInv - fZMinOverStep ).staticCast<unsigned int>();
// //   *bZ = CAMath::Max( uint_v( Vc::Zero ), CAMath::Min( fNz - 1, zBin ) );
//   
//   const float_v fYMinOverStep = array[indexes][&AliHLTTPCCAGrid::fYMinOverStep]; //Vc::Common::subscript_operator(array, indexes)[&AliHLTTPCCAGrid::fYMinOverStep];
//   const float_v fYMinOverStep( array, &AliHLTTPCCAGrid::fYMinOverStep, indexes );
//   const float_v fStepYInv( array, &AliHLTTPCCAGrid::fStepYInv, indexes );
//   const uint_v fNy( array, &AliHLTTPCCAGrid::fNy, indexes );
//   const int_v fNy2 = fNy.staticCast<int_v>();
//   const int_v &yBin = ( Y * fStepYInv - fYMinOverStep ).staticCast<int_v>();
//   *bY = CAMath::Max( int_v( Vc::Zero ), CAMath::Min( fNy2 - 1, yBin ) ).staticCast<uint_v>();
// 
//   const float_v fZMinOverStep( array, &AliHLTTPCCAGrid::fZMinOverStep, indexes );
//   const float_v fStepZInv( array, &AliHLTTPCCAGrid::fStepZInv, indexes );
//   const uint_v fNz( array, &AliHLTTPCCAGrid::fNz, indexes );
//   const int_v fNz2 = fNz.staticCast<int_v>();
//   const int_v &zBin = ( Z * fStepZInv - fZMinOverStep ).staticCast<int_v>();
//   *bZ = CAMath::Max( int_v( Vc::Zero ), CAMath::Min( fNz2 - 1, zBin ) ).staticCast<uint_v>();
// }

inline uint_v AliHLTTPCCAGrid::GetBinBounded( const float_v &Y, const float_v &Z ) const
{
  //* get the bin pointer

  uint_v yBin, zBin;
  GetBinBounded( Y, Z, &yBin, &zBin );
  return zBin * fNy + yBin;

  // XXX: the code below can wrap incorrectly because saturation is only done after
  // calculation of bin:
//X   const uint_v &yBin = ( Y * fStepYInv - fYMinOverStep ).staticCast<unsigned int>();
//X   const uint_v &zBin = ( Z * fStepZInv - fZMinOverStep ).staticCast<unsigned int>();
//X   const uint_v &bin = zBin * fNy + yBin;
//X 
//X   return CAMath::Max( uint_v( Vc::Zero ), CAMath::Min( uint_v( fN - 1 ), bin ) );
}

inline void AliHLTTPCCAGrid::GetBinBounded( const float_v &Y, const float_v &Z, uint_v *bY, uint_v *bZ ) const
{
  //* get the bin pointer

//   const uint_v &yBin = ( Y * fStepYInv - fYMinOverStep ).staticCast<unsigned int>();
//   const uint_v &zBin = ( Z * fStepZInv - fZMinOverStep ).staticCast<unsigned int>();
// 
//   *bY = CAMath::Max( uint_v( Vc::Zero ), CAMath::Min( uint_v( fNy - 1 ), yBin ) );
//   *bZ = CAMath::Max( uint_v( Vc::Zero ), CAMath::Min( uint_v( fNz - 1 ), zBin ) );
// IKu bag was here   :  -1 = 65000 > 0  !

  const int_v &yBin = ( Y * fStepYInv - fYMinOverStep ).staticCast<int_v>();
  const int_v &zBin = ( Z * fStepZInv - fZMinOverStep ).staticCast<int_v>();

  *bY = CAMath::Max( int_v( Vc::Zero ), CAMath::Min( int_v( fNy - 1 ), yBin ) ).staticCast<uint_v>();
  *bZ = CAMath::Max( int_v( Vc::Zero ), CAMath::Min( int_v( fNz - 1 ), zBin ) ).staticCast<uint_v>();

}

#endif
