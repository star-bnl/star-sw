// $Id: AliHLTTPCCAGrid.cxx,v 1.1 2016/02/05 23:27:27 fisyak Exp $
// **************************************************************************
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

//   std::cout << "fYMinOverStep " << yMax * fStepYInv - fYMinOverStep  << "fZMinOverStep " << zMax * fStepZInv - fZMinOverStep   << std::endl;
  fNy = static_cast<unsigned short>( yMax * fStepYInv - fYMinOverStep + 1.f );
  fNz = static_cast<unsigned short>( zMax * fStepZInv - fZMinOverStep + 1.f );

//   std::cout << "fNy " << fNy  << "fNz " << fNz   << std::endl;
  fN = fNy * fNz;

  //printf( "Grid::Create( %f, %f, %f, %f, %f, %f ): %d (%d x %d) with %f, %f\n", yMin, yMax, zMin, zMax, sy, sz, fN, fNy, fNz, fYMinOverStep, fZMinOverStep );
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
