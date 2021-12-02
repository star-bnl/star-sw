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



#include "AliHLTTPCCAGrid.h"
#include "AliHLTTPCCAMath.h"
#include <assert.h>
#include <cstdio>

void AliHLTTPCCAGrid::CreateEmpty()
{
  fYMinOverStep = 0.f;
  fZMinOverStep = 0.f;

  fNy = 0;
  fNz = 0;
  fN = 0;

  fStepYInv = 1.f;
  fStepZInv = 1.f;
}

void AliHLTTPCCAGrid::Create1( float y, float z, float sy, float sz )
{
  fN = 1;
  fNy = 1;
  fNz = 1;

  fStepYInv = 1.f / sy;
  fStepZInv = 1.f / sz;

  fYMinOverStep = y * fStepYInv - 0.5f;
  fZMinOverStep = z * fStepZInv - 0.5f;
}

void AliHLTTPCCAGrid::Create( float yMin, float yMax, float zMin, float zMax, float sy, float sz )
{
  //* Create the grid
  fStepYInv = 1.f / sy;
  fStepZInv = 1.f / sz;

  fYMinOverStep = yMin * fStepYInv;
  fZMinOverStep = zMin * fStepZInv;

  fNy = static_cast<unsigned int>( yMax * fStepYInv - fYMinOverStep + 1.f );
  fNz = static_cast<unsigned int>( zMax * fStepZInv - fZMinOverStep + 1.f );

  fN = fNy * fNz;
}


int AliHLTTPCCAGrid::GetBin( float Y, float Z ) const
{
  //* get the bin pointer
  const int yBin = static_cast<int>( Y * fStepYInv - fYMinOverStep );
  const int zBin = static_cast<int>( Z * fStepZInv - fZMinOverStep );
  assert( yBin >= 0 );
  assert( zBin >= 0 );
  assert( yBin < static_cast<int>( fNy ) );
  assert( zBin < static_cast<int>( fNz ) );
  const int bin = zBin * fNy + yBin;
  return bin;
}

void AliHLTTPCCAGrid::GetBinBounds( int iBin, float &Ymin, float &Ymax, float &Zmin, float &Zmax) const
{
  int zBin = iBin / fNy;
  int yBin = iBin % fNy;
  Ymin = (fYMinOverStep + yBin)/fStepYInv;
  Zmin = (fZMinOverStep + zBin)/fStepZInv;
  Ymax = Ymin + 1. / fStepYInv;
  Zmax = Zmin + 1. / fStepZInv;
}
