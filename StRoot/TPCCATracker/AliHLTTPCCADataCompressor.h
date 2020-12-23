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
