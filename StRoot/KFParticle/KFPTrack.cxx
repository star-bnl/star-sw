/*
 * This file is part of KFParticle package
 * Copyright (C) 2007-2019 FIAS Frankfurt Institute for Advanced Studies
 *               2007-2019 Goethe University of Frankfurt
 *               2007-2019 Ivan Kisel <I.Kisel@compeng.uni-frankfurt.de>
 *               2007-2019 Maksym Zyzak
 *
 * KFParticle is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * KFParticle is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

#include "KFPTrack.h"

#ifdef __ROOT__
ClassImp(KFPTrack);
#endif

void KFPTrack::RotateXY( float alpha )
{
  /** Rotates the parameters of the track on an angle alpha in the XY plane.
   ** Can be used in case of the transforamtion of the coordinate system.
   ** The rotation matrix is:
   ** \verbatim
   {  cos(A), -sin(A),  0,        0,        0,   0 }
   {  sin(A),  cos(A),  0,        0,        0,   0 }
   {       0,       0,  1,        0,        0,   0 }
   {       0,       0,  0,   cos(A),  -sin(A),   0 }
   {       0,       0,  0,   sin(A),   cos(A),   0 }
   {       0,       0,  0,        0,        0,   1 } 
   \endverbatim
   ** \param[in] alpha - rotation angle
   **/
  const float cA = cos( alpha );
  const float sA = sin( alpha );

  //float J[6][6] = { {  cA, -sA,  0,    0,    0,   0 }, // X
  //                  {  sA,  cA,  0,    0,    0,   0 }, // Y
  //                  {  0,   0,   1,    0,    0,   0 }, // Z
  //                  {  0,   0,   0,   cA,  -sA,   0 }, // Px
  //                  {  0,   0,   0,   sA,   cA,   0 }, // Py
  //                  {  0,   0,   0,    0,    0,   1 } }; // Pz

  const float x  = GetX(),  y = GetY();

  SetX( -(x*sA +  y*cA) );
  SetY( x*cA -  y*sA );

  const float px  = GetPx(),  py = GetPy();

  SetPx( -(px*sA + py*cA)  );
  SetPy( px*cA - py*sA );

  float cov[21];
  for(int iC=0; iC<21; iC++)
    cov[iC] = fC[iC];

  fC[0] = cA*cA*  cov[2] + 2* cA* cov[1]* sA + cov[0]*sA* sA;
  
  fC[1] = -(cA*cA * cov[1]) + cA* (-cov[0] + cov[2])* sA + cov[1]*sA* sA;
  fC[2] = cA*cA*  cov[0] - 2* cA* cov[1]* sA + cov[2]*sA* sA; 
  
  fC[3] = -(cA* cov[4]) - cov[3]* sA;
  fC[4] = cA* cov[3] - cov[4]* sA;
  fC[5] = cov[5]; 
  
  fC[6] = cA*cA*  cov[11] + cA *(cov[10] + cov[7])* sA + cov[6]*sA* sA;
  fC[7] = -(cA*cA * cov[10]) + cA* (cov[11] - cov[6])* sA + cov[7] *sA*sA;
  fC[8] = -(cA *cov[12]) - cov[8] *sA;
  fC[9] = cA*cA*  cov[14] + 2 *cA* cov[13]* sA + cov[9]* sA*sA;

  fC[10] = -(cA*cA*  cov[7]) + cA* (cov[11] - cov[6])* sA + cov[10]*sA* sA; 
  fC[11] = cA*cA*  cov[6] - cA* (cov[10] + cov[7]) *sA + cov[11]*sA* sA;
  fC[12] = cA* cov[8] - cov[12]* sA; 
  fC[13] = -(cA*cA*  cov[13]) + cA* (cov[14] - cov[9])* sA + cov[13]* sA*sA;
  fC[14] = cA*cA*  cov[9] - 2* cA* cov[13]* sA + cov[14]* sA*sA;
  
  fC[15] = -(cA* cov[16]) - cov[15]* sA;
  fC[16] = cA* cov[15] - cov[16]* sA;
  fC[17] = cov[17]; 
  fC[18] = -(cA* cov[19]) - cov[18]* sA;
  fC[19] = cA* cov[18] - cov[19]* sA;
  fC[20] = cov[20];

}
