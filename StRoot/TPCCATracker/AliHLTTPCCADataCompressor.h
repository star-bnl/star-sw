//-*- Mode: C++ -*-
// ************************************************************************
// This file is property of and copyright by the ALICE HLT Project        *
// ALICE Experiment at CERN, All rights reserved.                         *
// See cxx source for full Copyright notice                               *
//                                                                        *
//*************************************************************************

#ifndef ALIHLTTPCCADATACOMPRESSOR_H
#define ALIHLTTPCCADATACOMPRESSOR_H

#include "AliHLTTPCCADef.h"

/**
 * @class AliHLTTPCCADataCompressor
 *
 * The AliHLTTPCCADataCompressor class is used to
 * pack and unpack diffferent data, such as TPC cluster IDs, posistion, amplitude etc
 *
 */
namespace AliHLTTPCCADataCompressor
{
  class RowCluster
  {
    public:
      RowCluster(): fRow(0), fCluster(0) {}
      RowCluster( unsigned int iRow, unsigned int iCluster )
        : fRow( iRow ), fCluster( iCluster ) {}
      unsigned int Row() const { return fRow; }
      unsigned int Cluster() const { return fCluster; }
    private:
      unsigned int fRow     :  8;
      unsigned int fCluster : 24;
  };

  class SliceRowCluster
  {
    public:
      SliceRowCluster(): fSlice(0), fRow(0), fCluster(0) {}
      SliceRowCluster( unsigned int s, unsigned int r, unsigned int c )
        : fSlice( s ), fRow( r ), fCluster( c ) {}
      unsigned int Slice() const { return fSlice; }
      unsigned int Row() const { return fRow; }
      unsigned int Cluster() const { return fCluster; }
    private:
      unsigned int fSlice   :  6;
      unsigned int fRow     :  8;
      unsigned int fCluster : 18;
  };

  class YZ
  {
    public:
      YZ() {}
      YZ( float Y, float Z );
      float Y() const;
      float Z() const;
    private:
      unsigned char fY;
      unsigned char fZ;
  };

  // Inline methods

  inline YZ::YZ( float _Y, float _Z )
  {
    // compress Y and Z coordinates in range [-3., 3.] to 8 bits each

    const float kMult = 255. / 6.;
    _Y = ( _Y + 3. ) * kMult;
    _Z = ( _Z + 3. ) * kMult;
    if ( _Y < 0.f ) {
      fY = 0;
    } else if ( _Y > 255.f ) {
      fY = 255;
    } else {
      fY = static_cast<unsigned char>( CAMath::Round( _Y ) );
    }
    if ( _Z < 0.f ) {
      fZ = 0;
    } else if ( _Z > 255.f ) {
      fZ = 255;
    } else {
      fZ = static_cast<unsigned char>( CAMath::Round( _Z ) );
    }
  }

  inline float YZ::Y() const
  {
    // extract Y coordinate from the compressed 16bits format to [-3.,3.]

    const float kMult = 6. / 255.;
    return fY * kMult - 3.f;
  }

  inline float YZ::Z() const
  {
    // extract Z coordinate from the compressed 16bits format to [-3.,3.]

    const float kMult = 6. / 255.;
    return fZ * kMult - 3.f;
  }

} // namespace AliHLTTPCCADataCompressor

namespace DataCompressor
{
  using namespace AliHLTTPCCADataCompressor;
} // namespace DataCompressor

#endif
