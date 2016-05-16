//----------------------------------------------------------------------------
// Implementation of the KFParticle class
// .
// @author  I.Kisel, I.Kulakov, M.Zyzak
// @version 1.0
// @since   20.08.13
// 
// 
//  -= Copyright &copy ALICE HLT and CBM L1 Groups =-
//____________________________________________________________________________

#include "KFPTrack.h"

#include <cmath>
#ifdef __ROOT__
ClassImp(KFPTrack);
#endif
  // rotate on alpha in XY plane
void KFPTrack::RotateXY( float alpha )
{
  const float cA = cos( alpha );
  const float sA = sin( alpha );

#if 0
    //float J[6][6] = { {  cA, sA, 0,  0,  0,  0 }, // X
    //                  { -sA, cA, 0,  0,  0,  0 }, // Y
    //                  {  0,  0,  1,  0,  0,  0 }, // Z
    //                  {  0,  0,  0,  cA, sA, 0 }, // Px
    //                  {  0,  0,  0, -sA, cA, 0 }, // Py
    //                  {  0,  0,  0,  0,  0,  1 } }; // Pz
  
  float J[2][2] = { {  cA, sA },
                    { -sA, cA } };

  { // convert x, y TODO optimize
    const float x  = GetX(),  y = GetY();
    SetX( x*cA  +  y*sA );
    SetY( -x*sA +  y*cA );

    float Cov1[2][2]; // triangular -> symmetric matrix
    Cov1[0][0] = fC[0];
    Cov1[0][1] = fC[1];
    Cov1[1][0] = fC[1];
    Cov1[1][1] = fC[2];
    
    float Cov2[2][2];      // Cov2 = Cov1 * J^t
    for (int i = 0; i < 2; i++)
      for (int j = 0; j < 2; j++) {
        Cov2[i][j] = 0;
        for (int k = 0; k < 2; k++) {
          Cov2[i][j] += Cov1[i][k] * J[j][k];
        }
      }
    
      // Cov1 = J * Cov2
    for (int i = 0; i < 2; i++)
      for (int j = 0; j < 2; j++) {
        Cov1[i][j] = 0;
        for (int k = 0; k < 2; k++) {
          Cov1[i][j] += J[i][k] * Cov2[k][j];
        }
      }

      // symmetric matrix -> triangular
    fC[0] = Cov1[0][0];
    fC[1] = Cov1[0][1];
    fC[2] = Cov1[1][1];
  }

  { // convert Px, Py TODO optimize
    const float x  = GetPx(),  y = GetPy();
    SetPx( x*cA  +  y*sA );
    SetPy( -x*sA +  y*cA );

    float Cov1[2][2]; // triangular -> symmetric matrix
    Cov1[0][0] = fC[9];
    Cov1[0][1] = fC[13];
    Cov1[1][0] = fC[13];
    Cov1[1][1] = fC[14];

    
    float Cov2[2][2];      // Cov2 = Cov1 * J^t
    for (int i = 0; i < 2; i++)
      for (int j = 0; j < 2; j++) {
        Cov2[i][j] = 0;
        for (int k = 0; k < 2; k++) {
          Cov2[i][j] += Cov1[i][k] * J[j][k];
        }
      }
    
      // Cov1 = J * Cov2
    for (int i = 0; i < 2; i++)
      for (int j = 0; j < 2; j++) {
        Cov1[i][j] = 0;
        for (int k = 0; k < 2; k++) {
          Cov1[i][j] += J[i][k] * Cov2[k][j];
        }
      }

      // symmetric matrix -> triangular

    fC[9] = Cov1[0][0];
    fC[13] = Cov1[0][1];
    fC[14] = Cov1[1][1];
  }
  

#else
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
#endif

} // RotateXY
