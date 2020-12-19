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

    unsigned int GetBinBounded( const float &Y, const float &Z ) const;
    void GetBinBounded( const float &Y, const float &Z, unsigned int *bY, unsigned int *bZ ) const;

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
    // --- test
    unsigned int *fFirstUnusedHitInBin;
};

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
  for( unsigned int i = 0; i < float_v::Size; i++ ) {
    (*bY)[i] = (int)(*bY)[i];
    (*bZ)[i] = (int)(*bZ)[i];
  }
}

inline unsigned int AliHLTTPCCAGrid::GetBinBounded( const float &Y, const float &Z ) const
{
  //* get the bin pointer
  unsigned int yBin, zBin;
  GetBinBounded( Y, Z, &yBin, &zBin );
  return zBin * fNy + yBin;
}

inline void AliHLTTPCCAGrid::GetBinBounded( const float &Y, const float &Z, unsigned int *bY, unsigned int *bZ ) const
{
  //* get the bin pointer
  const int &yBin = ( Y * fStepYInv - fYMinOverStep );
  const int &zBin = ( Z * fStepZInv - fZMinOverStep );

  *bY = CAMath::Max( 0, CAMath::Min( (int)fNy - 1, yBin ) );
  *bZ = CAMath::Max( 0, CAMath::Min( (int)fNz - 1, zBin ) );
}

#endif
