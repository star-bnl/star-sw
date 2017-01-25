#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "TMath.h"
#include "TMatrixD.h"
#include "TVectorD.h"
#include "TCernLib.h"
#include "StvFitter.h"
#include "StvUtil/StvNodePars.h"
#include "StvHit.h"
#include "StvUtil/StvDebug.h"
#include "StvUtil/StvHitErrCalculator.h"
#include "StarVMC/GeoTestMaker/StTGeoProxy.h"

StvFitter *StvFitter::mgFitter=0;
#define VDOT(a,b)   ( a[0]*b[0]+a[1]*b[1]+a[2]*b[2])
#define DIST(a,b)   ((a[0]-b[0])*(a[0]-b[0])+(a[1]-b[1])*(a[1]-b[1])+(a[2]-b[2])*(a[2]-b[2]))
#define DDOT(a,b,c) ((a[0]-b[0])*c[0]+(a[1]-b[1])*c[1]+(a[2]-b[2])*c[2])
#define VADD(a,b)   { a[0]+=b[0];a[1]+=b[1];a[2]+=b[2];}

enum {kDeltaFactor = 21,kTooBigErrFactor = 100*100};

static const double kXtraBigXi2 = 9e9;

static inline double MyXi2(const double G[3],double dA,double dB)  
{
  double Gdet = G[0]*G[2]-G[1]*G[1];
  if (Gdet < 1e-11) return kXtraBigXi2;
  double Xi2 =  (G[2]*dA*dA-2*G[1]*dA*dB+G[0]*dB*dB)/Gdet;
  if (Xi2 > kXtraBigXi2) Xi2 = kXtraBigXi2;
  return Xi2;
}

class TCLx 
{
public:
static int trsinv2x2(const double *pM,double *pMi);
static int trsinv3x3(const double *pM,double *pMi);
static int trsinv4x4(const double *pM,double *pMi);
static int trsinv5x5(const double *pM,double *pMi);
static void Test();
};

#include <assert.h>
#include <math.h>

//______________________________________________________________________________
int TCLx::trsinv2x2(const double *pM,double *pMi)
{
   double F[3]={pM[0],0,pM[2]};
   if (F[0]<1e-20) F[0]=1e-20;if (F[2]>1e+20) F[2]=1e+20;
   F[0] =1./F[0]; F[2] =1./F[2];
   F[1] = sqrt(F[0]*F[2]);
   for (int i=0;i<3;i++) {pMi[i] = pM[i]*F[i];}

   const double det = pMi[0] * pMi[2] - pMi[1] * pMi[1];

   if (det<=3e-33) return 1;
   const double s = 1./det;

   const double tmp = s*pMi[2];
   pMi[1] = -pMi[1]*s;
   pMi[2] = s*pMi[0];
   pMi[0] = tmp;
   for (int i=0;i<3;i++) {pMi[i] = pMi[i]*F[i];}
   return 0;
}
//______________________________________________________________________________
int TCLx::trsinv3x3(const double *pM,double *pMi)
{
//  0
//  1  2 
//  3  4  5
// 
//  0  1  2  
//  3  4  5
//  6  7  8
// 
// 2=>3
// 3=>1
// 4=>2
// 5=>4
// 6=>3
// 7=>4
// 8=>5

   const double c00 = pM[2] * pM[5] - pM[4] * pM[4];
   const double c10 = pM[4] * pM[3] - pM[1] * pM[5];
   const double c20 = pM[1] * pM[4] - pM[2] * pM[3];
   const double c11 = pM[5] * pM[0] - pM[3] * pM[3];
   const double c21 = pM[3] * pM[1] - pM[4] * pM[0];
   const double c22 = pM[0] * pM[2] - pM[1] * pM[1];

   const double t0 = fabs(pM[0]);
   const double t1 = fabs(pM[1]);
   const double t2 = fabs(pM[3]);
   double det;
   double tmp;
   if (t0 >= t1) {
      if (t2 >= t0) {
      tmp = pM[3];
      det = c21*c10-c11*c20;
      } else {
         tmp = pM[0];
         det = c11*c22-c21*c21;
      }
   } else if (t2 >= t1) {
      tmp = pM[3];
      det = c21*c10-c11*c20;
   } else {
      tmp = pM[1];
      det = c20*c21-c10*c22;
   }

   if (det<=0) 		return -1;
   if (det<=3e-33) 	return  1;

   const double s = tmp/det;

   pMi[0] = s*c00;
   pMi[1] = s*c10;
   pMi[2] = s*c11;
   pMi[3] = s*c20;
   pMi[4] = s*c21;
   pMi[5] = s*c22;
   return 0;  
}
//______________________________________________________________________________
// GFij are indices for a 4x4 matrix.

//  0
//  1  2 
//  3  4  5
//  6  7  8  9
#define GF00 0

#define GF10 1
#define GF11 2

#define GF20 3
#define GF21 4
#define GF22 5

#define GF30 6
#define GF31 7
#define GF32 8
#define GF33 9

//______________________________________________________________________________
int TCLx::trsinv4x4(const double *pM,double *pMi)
{
  // Find all NECESSARY 2x2 dets:  (18 of them)

   const Double_t det2_12_01 = pM[GF10]*pM[GF21] - pM[GF11]*pM[GF20];
   const Double_t det2_12_02 = pM[GF10]*pM[GF22] - pM[GF21]*pM[GF20];
   const Double_t det2_12_03 = pM[GF10]*pM[GF32] - pM[GF31]*pM[GF20];
   const Double_t det2_12_13 = pM[GF11]*pM[GF32] - pM[GF31]*pM[GF21];
   const Double_t det2_12_23 = pM[GF21]*pM[GF32] - pM[GF31]*pM[GF22];
   const Double_t det2_12_12 = pM[GF11]*pM[GF22] - pM[GF21]*pM[GF21];
   const Double_t det2_13_01 = pM[GF10]*pM[GF31] - pM[GF11]*pM[GF30];
   const Double_t det2_13_02 = pM[GF10]*pM[GF32] - pM[GF21]*pM[GF30];
   const Double_t det2_13_03 = pM[GF10]*pM[GF33] - pM[GF31]*pM[GF30];
   const Double_t det2_13_12 = pM[GF11]*pM[GF32] - pM[GF21]*pM[GF31];
   const Double_t det2_13_13 = pM[GF11]*pM[GF33] - pM[GF31]*pM[GF31];
   const Double_t det2_13_23 = pM[GF21]*pM[GF33] - pM[GF31]*pM[GF32];
   const Double_t det2_23_01 = pM[GF20]*pM[GF31] - pM[GF21]*pM[GF30];
   const Double_t det2_23_02 = pM[GF20]*pM[GF32] - pM[GF22]*pM[GF30];
   const Double_t det2_23_03 = pM[GF20]*pM[GF33] - pM[GF32]*pM[GF30];
   const Double_t det2_23_12 = pM[GF21]*pM[GF32] - pM[GF22]*pM[GF31];
   const Double_t det2_23_13 = pM[GF21]*pM[GF33] - pM[GF32]*pM[GF31];
   const Double_t det2_23_23 = pM[GF22]*pM[GF33] - pM[GF32]*pM[GF32];

  // Find all NECESSARY 3x3 dets:   (16 of them)

   const Double_t det3_012_012 = pM[GF00]*det2_12_12 - pM[GF10]*det2_12_02 
                                 + pM[GF20]*det2_12_01;
   const Double_t det3_012_013 = pM[GF00]*det2_12_13 - pM[GF10]*det2_12_03 
                                 + pM[GF30]*det2_12_01;
   const Double_t det3_012_023 = pM[GF00]*det2_12_23 - pM[GF20]*det2_12_03 
                                 + pM[GF30]*det2_12_02;
   const Double_t det3_012_123 = pM[GF10]*det2_12_23 - pM[GF20]*det2_12_13 
                                 + pM[GF30]*det2_12_12;
   const Double_t det3_013_013 = pM[GF00]*det2_13_13 - pM[GF10]*det2_13_03
                                 + pM[GF30]*det2_13_01;
   const Double_t det3_013_023 = pM[GF00]*det2_13_23 - pM[GF20]*det2_13_03
                                 + pM[GF30]*det2_13_02;
   const Double_t det3_013_123 = pM[GF10]*det2_13_23 - pM[GF20]*det2_13_13
                                 + pM[GF30]*det2_13_12;
   const Double_t det3_023_023 = pM[GF00]*det2_23_23 - pM[GF20]*det2_23_03
                                 + pM[GF30]*det2_23_02;
   const Double_t det3_023_123 = pM[GF10]*det2_23_23 - pM[GF20]*det2_23_13
                                 + pM[GF30]*det2_23_12;
   const Double_t det3_123_012 = pM[GF10]*det2_23_12 - pM[GF11]*det2_23_02 
                                 + pM[GF21]*det2_23_01;
   const Double_t det3_123_013 = pM[GF10]*det2_23_13 - pM[GF11]*det2_23_03 
                                  + pM[GF31]*det2_23_01;
   const Double_t det3_123_023 = pM[GF10]*det2_23_23 - pM[GF21]*det2_23_03 
                                 + pM[GF31]*det2_23_02;
   const Double_t det3_123_123 = pM[GF11]*det2_23_23 - pM[GF21]*det2_23_13 
                                 + pM[GF31]*det2_23_12;

  // Find the 4x4 det:

   const Double_t det = pM[GF00]*det3_123_123 - pM[GF10]*det3_123_023 
                        + pM[GF20]*det3_123_013 - pM[GF30]*det3_123_012;

   if (det<=3e-33) return 1;

   const Double_t oneOverDet = 1.0/det;
   const Double_t mn1OverDet = - oneOverDet;

   pMi[GF00] =  det3_123_123 * oneOverDet;
   pMi[GF10] =  det3_023_123 * mn1OverDet;
   pMi[GF20] =  det3_013_123 * oneOverDet;
   pMi[GF30] =  det3_012_123 * mn1OverDet;

   pMi[GF11] =  det3_023_023 * oneOverDet;
   pMi[GF21] =  det3_013_023 * mn1OverDet;
   pMi[GF31] =  det3_012_023 * oneOverDet;

   pMi[GF22] =  det3_013_013 * oneOverDet;
   pMi[GF32] =  det3_012_013 * mn1OverDet;

   pMi[GF33] =  det3_012_012 * oneOverDet;
   return 0;

}  
// GMij are indices for a 5x5 matrix.

//  0
//  1  2 
//  3  4  5
//  6  7  8  9
// 10 11 12 13 14
#define GM00 0

#define GM10 1
#define GM11 2

#define GM20 3
#define GM21 4
#define GM22 5

#define GM30 6
#define GM31 7
#define GM32 8
#define GM33 9

#define GM40 10
#define GM41 11
#define GM42 12
#define GM43 13
#define GM44 14

//______________________________________________________________________________
int TCLx::trsinv5x5(const double  *pMp,double  *pMi)
{
   double F[5],pM[15];
   for (int i=0,li=0;i< 5;li+=++i) {
     F[i] = sqrt(pMp[li+i]);
     if (F[i]<1e-10) F[i]=1e-10;
     if (F[i]>1e+10) F[i]=1e+10;
  }

   for (int i=0,li=0;i< 5;li+=++i) {
     for (int j=0;j<=i;j++) {
       pM[li+j] = pMp[li+j]/F[i]/F[j];
  } }
  // Find all NECESSARY 2x2 dets:  (30 of them)

   const Double_t det2_23_01 = pM[GM20]*pM[GM31] - pM[GM21]*pM[GM30];
   const Double_t det2_23_02 = pM[GM20]*pM[GM32] - pM[GM22]*pM[GM30];
   const Double_t det2_23_03 = pM[GM20]*pM[GM33] - pM[GM32]*pM[GM30];
   const Double_t det2_23_04 = pM[GM20]*pM[GM43] - pM[GM42]*pM[GM30];
   const Double_t det2_23_12 = pM[GM21]*pM[GM32] - pM[GM22]*pM[GM31];
   const Double_t det2_23_13 = pM[GM21]*pM[GM33] - pM[GM32]*pM[GM31];
   const Double_t det2_23_14 = pM[GM21]*pM[GM43] - pM[GM42]*pM[GM31];
   const Double_t det2_23_23 = pM[GM22]*pM[GM33] - pM[GM32]*pM[GM32];
   const Double_t det2_23_24 = pM[GM22]*pM[GM43] - pM[GM42]*pM[GM32];
   const Double_t det2_23_34 = pM[GM32]*pM[GM43] - pM[GM42]*pM[GM33];
   const Double_t det2_24_01 = pM[GM20]*pM[GM41] - pM[GM21]*pM[GM40];
   const Double_t det2_24_02 = pM[GM20]*pM[GM42] - pM[GM22]*pM[GM40];
   const Double_t det2_24_03 = pM[GM20]*pM[GM43] - pM[GM32]*pM[GM40];
   const Double_t det2_24_04 = pM[GM20]*pM[GM44] - pM[GM42]*pM[GM40];
   const Double_t det2_24_12 = pM[GM21]*pM[GM42] - pM[GM22]*pM[GM41];
   const Double_t det2_24_13 = pM[GM21]*pM[GM43] - pM[GM32]*pM[GM41];
   const Double_t det2_24_14 = pM[GM21]*pM[GM44] - pM[GM42]*pM[GM41];
   const Double_t det2_24_23 = det2_23_24;
   const Double_t det2_24_24 = pM[GM22]*pM[GM44] - pM[GM42]*pM[GM42];
   const Double_t det2_24_34 = pM[GM32]*pM[GM44] - pM[GM42]*pM[GM43];
   const Double_t det2_34_01 = pM[GM30]*pM[GM41] - pM[GM31]*pM[GM40];
   const Double_t det2_34_02 = pM[GM30]*pM[GM42] - pM[GM32]*pM[GM40];
   const Double_t det2_34_03 = pM[GM30]*pM[GM43] - pM[GM33]*pM[GM40];
   const Double_t det2_34_04 = pM[GM30]*pM[GM44] - pM[GM43]*pM[GM40];
   const Double_t det2_34_12 = pM[GM31]*pM[GM42] - pM[GM32]*pM[GM41];
   const Double_t det2_34_13 = pM[GM31]*pM[GM43] - pM[GM33]*pM[GM41];
   const Double_t det2_34_14 = pM[GM31]*pM[GM44] - pM[GM43]*pM[GM41];
   const Double_t det2_34_23 = det2_23_34;

   const Double_t det2_34_24 = pM[GM32]*pM[GM44] - pM[GM43]*pM[GM42];
   const Double_t det2_34_34 = pM[GM33]*pM[GM44] - pM[GM43]*pM[GM43];

  // Find all NECESSARY 3x3 dets:   (40 of them)

   const Double_t det3_123_012 = pM[GM10]*det2_23_12 - pM[GM11]*det2_23_02 + pM[GM21]*det2_23_01;
   const Double_t det3_123_013 = pM[GM10]*det2_23_13 - pM[GM11]*det2_23_03 + pM[GM31]*det2_23_01;
   const Double_t det3_123_014 = pM[GM10]*det2_23_14 - pM[GM11]*det2_23_04 + pM[GM41]*det2_23_01;
   const Double_t det3_123_023 = pM[GM10]*det2_23_23 - pM[GM21]*det2_23_03 + pM[GM31]*det2_23_02;
   const Double_t det3_123_024 = pM[GM10]*det2_23_24 - pM[GM21]*det2_23_04 + pM[GM41]*det2_23_02;
   const Double_t det3_123_034 = pM[GM10]*det2_23_34 - pM[GM31]*det2_23_04 + pM[GM41]*det2_23_03;
   const Double_t det3_123_123 = pM[GM11]*det2_23_23 - pM[GM21]*det2_23_13 + pM[GM31]*det2_23_12;
   const Double_t det3_123_124 = pM[GM11]*det2_23_24 - pM[GM21]*det2_23_14 + pM[GM41]*det2_23_12;
   const Double_t det3_123_134 = pM[GM11]*det2_23_34 - pM[GM31]*det2_23_14 + pM[GM41]*det2_23_13;
   const Double_t det3_123_234 = pM[GM21]*det2_23_34 - pM[GM31]*det2_23_24 + pM[GM41]*det2_23_23;
   const Double_t det3_124_012 = pM[GM10]*det2_24_12 - pM[GM11]*det2_24_02 + pM[GM21]*det2_24_01;
   const Double_t det3_124_013 = pM[GM10]*det2_24_13 - pM[GM11]*det2_24_03 + pM[GM31]*det2_24_01;
   const Double_t det3_124_014 = pM[GM10]*det2_24_14 - pM[GM11]*det2_24_04 + pM[GM41]*det2_24_01;
   const Double_t det3_124_023 = pM[GM10]*det2_24_23 - pM[GM21]*det2_24_03 + pM[GM31]*det2_24_02;
   const Double_t det3_124_024 = pM[GM10]*det2_24_24 - pM[GM21]*det2_24_04 + pM[GM41]*det2_24_02;
   const Double_t det3_124_034 = pM[GM10]*det2_24_34 - pM[GM31]*det2_24_04 + pM[GM41]*det2_24_03;
   const Double_t det3_124_123 = pM[GM11]*det2_24_23 - pM[GM21]*det2_24_13 + pM[GM31]*det2_24_12;
   const Double_t det3_124_124 = pM[GM11]*det2_24_24 - pM[GM21]*det2_24_14 + pM[GM41]*det2_24_12;
   const Double_t det3_124_134 = pM[GM11]*det2_24_34 - pM[GM31]*det2_24_14 + pM[GM41]*det2_24_13;
   const Double_t det3_124_234 = pM[GM21]*det2_24_34 - pM[GM31]*det2_24_24 + pM[GM41]*det2_24_23;
   const Double_t det3_134_013 = pM[GM10]*det2_34_13 - pM[GM11]*det2_34_03 + pM[GM31]*det2_34_01;
   const Double_t det3_134_014 = pM[GM10]*det2_34_14 - pM[GM11]*det2_34_04 + pM[GM41]*det2_34_01;
   const Double_t det3_134_023 = pM[GM10]*det2_34_23 - pM[GM21]*det2_34_03 + pM[GM31]*det2_34_02;
   const Double_t det3_134_024 = pM[GM10]*det2_34_24 - pM[GM21]*det2_34_04 + pM[GM41]*det2_34_02;
   const Double_t det3_134_034 = pM[GM10]*det2_34_34 - pM[GM31]*det2_34_04 + pM[GM41]*det2_34_03;
   const Double_t det3_134_123 = pM[GM11]*det2_34_23 - pM[GM21]*det2_34_13 + pM[GM31]*det2_34_12;
   const Double_t det3_134_124 = pM[GM11]*det2_34_24 - pM[GM21]*det2_34_14 + pM[GM41]*det2_34_12;
   const Double_t det3_134_134 = pM[GM11]*det2_34_34 - pM[GM31]*det2_34_14 + pM[GM41]*det2_34_13;
   const Double_t det3_134_234 = pM[GM21]*det2_34_34 - pM[GM31]*det2_34_24 + pM[GM41]*det2_34_23;
   const Double_t det3_234_012 = pM[GM20]*det2_34_12 - pM[GM21]*det2_34_02 + pM[GM22]*det2_34_01;
   const Double_t det3_234_013 = pM[GM20]*det2_34_13 - pM[GM21]*det2_34_03 + pM[GM32]*det2_34_01;
   const Double_t det3_234_014 = pM[GM20]*det2_34_14 - pM[GM21]*det2_34_04 + pM[GM42]*det2_34_01;
   const Double_t det3_234_023 = pM[GM20]*det2_34_23 - pM[GM22]*det2_34_03 + pM[GM32]*det2_34_02;
   const Double_t det3_234_024 = pM[GM20]*det2_34_24 - pM[GM22]*det2_34_04 + pM[GM42]*det2_34_02;
   const Double_t det3_234_034 = pM[GM20]*det2_34_34 - pM[GM32]*det2_34_04 + pM[GM42]*det2_34_03;
   const Double_t det3_234_123 = pM[GM21]*det2_34_23 - pM[GM22]*det2_34_13 + pM[GM32]*det2_34_12;
   const Double_t det3_234_124 = pM[GM21]*det2_34_24 - pM[GM22]*det2_34_14 + pM[GM42]*det2_34_12;
   const Double_t det3_234_134 = pM[GM21]*det2_34_34 - pM[GM32]*det2_34_14 + pM[GM42]*det2_34_13;
   const Double_t det3_234_234 = pM[GM22]*det2_34_34 - pM[GM32]*det2_34_24 + pM[GM42]*det2_34_23;

  // Find all NECESSARY 4x4 dets:   (25 of them)

   const Double_t det4_0123_0123 = pM[GM00]*det3_123_123 - pM[GM10]*det3_123_023 
                                   + pM[GM20]*det3_123_013 - pM[GM30]*det3_123_012;
   const Double_t det4_0123_0124 = pM[GM00]*det3_123_124 - pM[GM10]*det3_123_024 
                                   + pM[GM20]*det3_123_014 - pM[GM40]*det3_123_012;
   const Double_t det4_0123_0134 = pM[GM00]*det3_123_134 - pM[GM10]*det3_123_034 
                                   + pM[GM30]*det3_123_014 - pM[GM40]*det3_123_013;
   const Double_t det4_0123_0234 = pM[GM00]*det3_123_234 - pM[GM20]*det3_123_034 
                                   + pM[GM30]*det3_123_024 - pM[GM40]*det3_123_023;
   const Double_t det4_0123_1234 = pM[GM10]*det3_123_234 - pM[GM20]*det3_123_134 
                                   + pM[GM30]*det3_123_124 - pM[GM40]*det3_123_123;
   const Double_t det4_0124_0124 = pM[GM00]*det3_124_124 - pM[GM10]*det3_124_024 
                                   + pM[GM20]*det3_124_014 - pM[GM40]*det3_124_012;
   const Double_t det4_0124_0134 = pM[GM00]*det3_124_134 - pM[GM10]*det3_124_034 
                                   + pM[GM30]*det3_124_014 - pM[GM40]*det3_124_013;
   const Double_t det4_0124_0234 = pM[GM00]*det3_124_234 - pM[GM20]*det3_124_034 
                                   + pM[GM30]*det3_124_024 - pM[GM40]*det3_124_023;
   const Double_t det4_0124_1234 = pM[GM10]*det3_124_234 - pM[GM20]*det3_124_134 
                                   + pM[GM30]*det3_124_124 - pM[GM40]*det3_124_123;
   const Double_t det4_0134_0134 = pM[GM00]*det3_134_134 - pM[GM10]*det3_134_034 
                                   + pM[GM30]*det3_134_014 - pM[GM40]*det3_134_013;
   const Double_t det4_0134_0234 = pM[GM00]*det3_134_234 - pM[GM20]*det3_134_034 
                                   + pM[GM30]*det3_134_024 - pM[GM40]*det3_134_023;
   const Double_t det4_0134_1234 = pM[GM10]*det3_134_234 - pM[GM20]*det3_134_134 
                                   + pM[GM30]*det3_134_124 - pM[GM40]*det3_134_123;
   const Double_t det4_0234_0234 = pM[GM00]*det3_234_234 - pM[GM20]*det3_234_034 
                                   + pM[GM30]*det3_234_024 - pM[GM40]*det3_234_023;
   const Double_t det4_0234_1234 = pM[GM10]*det3_234_234 - pM[GM20]*det3_234_134 
                                   + pM[GM30]*det3_234_124 - pM[GM40]*det3_234_123;
   const Double_t det4_1234_0123 = pM[GM10]*det3_234_123 - pM[GM11]*det3_234_023 
                                   + pM[GM21]*det3_234_013 - pM[GM31]*det3_234_012;
   const Double_t det4_1234_0124 = pM[GM10]*det3_234_124 - pM[GM11]*det3_234_024 
                                    + pM[GM21]*det3_234_014 - pM[GM41]*det3_234_012;
   const Double_t det4_1234_0134 = pM[GM10]*det3_234_134 - pM[GM11]*det3_234_034 
                                   + pM[GM31]*det3_234_014 - pM[GM41]*det3_234_013;
   const Double_t det4_1234_0234 = pM[GM10]*det3_234_234 - pM[GM21]*det3_234_034 
                                   + pM[GM31]*det3_234_024 - pM[GM41]*det3_234_023;
   const Double_t det4_1234_1234 = pM[GM11]*det3_234_234 - pM[GM21]*det3_234_134 
                                   + pM[GM31]*det3_234_124 - pM[GM41]*det3_234_123;

  // Find the 5x5 det:

   const Double_t det = pM[GM00]*det4_1234_1234 - pM[GM10]*det4_1234_0234 + pM[GM20]*det4_1234_0134 
                      - pM[GM30]*det4_1234_0124 + pM[GM40]*det4_1234_0123;

   if (det<=    0) return -1;
   if (det<=3e-33) return  1;

   const Double_t oneOverDet = 1.0/det;
   const Double_t mn1OverDet = - oneOverDet;

   pMi[GM00] =  det4_1234_1234 * oneOverDet;
   pMi[GM10] =  det4_0234_1234 * mn1OverDet;
   pMi[GM20] =  det4_0134_1234 * oneOverDet;
   pMi[GM30] =  det4_0124_1234 * mn1OverDet;
   pMi[GM40] =  det4_0123_1234 * oneOverDet;

   pMi[GM11] =  det4_0234_0234 * oneOverDet;
   pMi[GM21] =  det4_0134_0234 * mn1OverDet;
   pMi[GM31] =  det4_0124_0234 * oneOverDet;
   pMi[GM41] =  det4_0123_0234 * mn1OverDet;

   pMi[GM22] =  det4_0134_0134 * oneOverDet;
   pMi[GM32] =  det4_0124_0134 * mn1OverDet;
   pMi[GM42] =  det4_0123_0134 * oneOverDet;

   pMi[GM33] =  det4_0124_0124 * oneOverDet;
   pMi[GM43] =  det4_0123_0124 * mn1OverDet;

   pMi[GM44] =  det4_0123_0123 * oneOverDet;

   for (int i=0,li=0;i< 5;li+=++i) {
     for (int j=0;j<=i;j++) {
       pMi[li+j] = pMi[li+j]/F[i]/F[j];
  } }
   return 0;
}
#include "TCernLib.h"
#include "TRandom.h"

//______________________________________________________________________________
void TCLx::Test()
{
  int nErr = 0;
  double e[15],e1inv[15],e2inv[15];
  for (int i=0,li=0;i< 5;li+=++i) {
    e[li+i] = gRandom->Rndm()*10+10;
    for (int j=0;j<i;j++) {
      e[li+j] = gRandom->Rndm();
  } }

   for (int N=2;N<=5;N++) {
     TCL::trsinv(e,e1inv,N);
     switch (N) {
       case 2:TCLx::trsinv2x2(e,e2inv); break;
       case 3:TCLx::trsinv3x3(e,e2inv); break;
       case 4:TCLx::trsinv4x4(e,e2inv); break;
       case 5:TCLx::trsinv5x5(e,e2inv); break;
       default: assert(0);
    }
    int l = (N*N+N)/2;
    for (int i=0;i<l;i++) {
      double delta = fabs(e1inv[i]-e2inv[i]);
      printf("%d.%d = %g %g \t%g\n",N,i,e1inv[i],e2inv[i],delta);
      if (delta<1e-8) continue;
      delta /= fabs(e1inv[i])+fabs(e2inv[i]);
      if (delta<1e-8) continue;
      printf("%d.%d = %g %g \t%g\n",N,i,e1inv[i],e2inv[i],delta);
      nErr++;
    }
  }
  printf("TCLx::Test()  %d errors\n",nErr);

}




double JoinTwoT(int nP1,const double *P1,const double *E1
               ,int nP2,const double *P2,const double *E2
	               ,      double *PJ,      double *EJ);
//______________________________________________________________________________
//______________________________________________________________________________
double JoinTwo(int nP1,const double *P1,const double *E1
              ,int nP2,const double *P2,const double *E2
	               ,     double *PJ,      double *EJ)
{
static int nCall = 0;  nCall++;
  assert(nP1<=nP2);
  int nE1 = nP1*(nP1+1)/2;
  int nE2 = nP2*(nP2+1)/2;
//  TArrayD ard(nE2*5+nP2*3);
  std::vector<double> ard(nE2+nP2*2+1);
  double *a = &ard[0];  
  double *P1sP2  = a; a+=nP2;	//=P1-P2
  double *Ptmp   = a; a+=nP2;	//=tmp vector
  double *E1aE2i = a; a+=nE2;	//=(E1+E2)**(-1)
  *a = 1946;
  
  if (P2) { TCL::vsub (P1,P2,P1sP2,nP1);}
  else    { TCL::ucopy(P1,   P1sP2,nP1);}
  TCL::vadd (E1,E2,E1aE2i,nE1);
  if (nP1<nP2) {TCL::vzero(P1sP2+nP1,nP2-nP1);TCL::vzero(E1aE2i+nE1,nE2-nE1);}
    
//  TCL::trsinv(E1aE2i,E1aE2i,nP1);		//E1aE2i = (E1+E2)**(-1)
  int kase = nP1;
SWITCHa:  switch(kase) {
    case 2: if ((kase=TCLx::trsinv2x2(E1aE2i,E1aE2i))) {goto SWITCHa;}; break;
    case 5: if ((kase=TCLx::trsinv5x5(E1aE2i,E1aE2i))) {goto SWITCHa;}; break;
    case -1: return 1e11;
    default: TCL::trsinv   (E1aE2i,E1aE2i,nP1);
  }
  
  double Xi2=0;
  TCL::trasat(P1sP2,E1aE2i,&Xi2,1,nP1); 
  if (!PJ) return Xi2;

//		Evaluate output parameters
  TCL::trsa (E1aE2i,P1sP2,Ptmp,nP2,1);
  TCL::trsa (E2,Ptmp,PJ,nP2,1);
  if (!EJ) return Xi2;

//		Evaluate output error matrix
  TCL::trqsq(E2,E1aE2i,EJ,nP2);		// E2*(E1+E2)**(-1)*E2
  TCL::vsub (E2    ,EJ,EJ,nE2);         // E2 - E2*(E1+E2)**(-1)*E2
assert(!(*a-1946));
  return Xi2;
}
//______________________________________________________________________________
double JoinTwoT(int nP1,const double *P1,const double *E1
               ,int nP2,const double *P2,const double *E2
	               ,      double *PJ,      double *EJ)
{
///  Fit track(P2) & errors E2 with track or hit (P1) & errors E1
///  nP1 size of array P1. E1 errors packed as low triangle
///  nP2 size of array P2. E2 errors packed as low triangle
///  PJ output parameters size nP2. EJ according packed errors

  assert(nP1<=nP2);
//  int nE1 = nP1*(nP1+1)/2;
//  int nE2 = nP2*(nP2+1)/2;
  TMatrixD  E1i(nP2,nP2 ),E2i(nP2,nP2);
  for (int i=0,li=0;i< nP1;li+=++i) {
    for (int j=0;j<=i; j++) {E1i[i][j]=E1[li+j]; E1i[j][i]=E1[li+j];}}

  for (int i=0,li=0;i< nP2;li+=++i) {
    for (int j=0;j<=i; j++) {E2i[i][j]=E2[li+j]; E2i[j][i]=E2[li+j];}}

  for (int i=nP1;i<nP2;i++) {E1i[i][i]=1;};
  E1i.InvertFast();
  for (int i=nP1;i<nP2;i++) {E1i[i][i]=0;};
  E2i.InvertFast();
  TMatrixD  EJi = E1i+E2i;
  TMatrixD  EJm = EJi; EJm.InvertFast();
  TVectorD P1v(nP2); TCL::ucopy(P1,P1v.GetMatrixArray(),nP1);
  TVectorD P2v(nP2,P2);
  
  TVectorD PJv = EJm*((E1i*P1v) + (E2i*P2v));
  double chi2 = (PJv-P1v)*(E1i*(PJv-P1v))+(PJv-P2v)*(E2i*(PJv-P2v));
  if (!PJ) return chi2;
  TCL::ucopy(PJv.GetMatrixArray(),PJ,nP2);
  TCL::trpck(EJm.GetMatrixArray(),EJ,nP2);
  return chi2;


}
//______________________________________________________________________________
double JoinVtx(int nP1,const double *P1,const double *E1
              ,int nP2,const double *P2,const double *E2
	               ,     double *PJ,      double *EJ)
{
///  Fit track(P2) & errors E2 with vertex (P1) & errors E1
///  Track must exactly pass thru the vertex. 
///  Vertex errors added afterwards
///  nP1 size of array P1. E1 errors packed as low triangle
///  nP2 size of array P2. E2 errors packed as low triangle
///  PJ output parameters size nP2. EJ according packed errors

  assert(nP1<nP2);
  int nPBig = nP1+nP2;

  TVectorD P1t(nP1,P1),P2t(nP2,P2);
  TVectorD D = (P1t-P2t.GetSub(0,nP1-1));
  TMatrixDSym smaMx(nP1);
  TCL::trupck(E2,smaMx.GetMatrixArray(),nP1);
  smaMx.InvertFast();
  double Xi2 = D*(smaMx*D);
  if (!PJ) return Xi2;

  TMatrixDSym E2t(nP2);
  TCL::trupck(E2,E2t.GetMatrixArray(),nP2);
  E2t.InvertFast();

  TVectorD bigB(nPBig),bigP(nPBig);
  bigB.SetSub(0,E2t*P2t);
  bigB.SetSub(nP2,P1t);

  TMatrixDSym bigMx(nPBig);
  bigMx.SetSub(0,E2t);
  for (int i=nP2,j=0;i< nPBig;++i,j++) {bigMx[i][j]=1;bigMx[j][i]=1;}
  bigMx.InvertFast();
  bigP = bigMx*bigB;
  assert(fabs(bigP[0]-P1[0])<1e-5);
  assert(fabs(bigP[1]-P1[1])<1e-5);

//		set vertex errors (not add because in bigMx they are zeros
  TMatrixDSym E1t(nP1);
  TCL::trupck(E1,E1t.GetMatrixArray(),nP1);
  bigMx.SetSub(0,E1t);

//		To output  
  TCL::ucopy(bigP.GetMatrixArray(),PJ,nP2);
  TCL::trpck(bigMx.GetSub(0,nP2-1,0,nP2-1).GetMatrixArray(),EJ,nP2);
  return Xi2;


}



//______________________________________________________________________________
StvFitter::StvFitter(const char *name):TNamed(name,"")
{
  memset(mBeg,0,mEnd-mBeg+1);
  assert(!mgFitter);
  mgFitter = this;
}
//______________________________________________________________________________
void StvFitter::Set(const StvNodePars *inPars, const StvFitErrs *inErrs
                   ,      StvNodePars *otPars,       StvFitErrs *otErrs)
{
  memset(mBeg,'Z',mEnd-mBeg+1);
  mKase = 0;		// track + hit case
  mInPars = inPars; mInErrs = inErrs;
  mOtPars = otPars; mOtErrs = otErrs;
  mJnPars =      0; mJnErrs =      0;
}
//______________________________________________________________________________
void StvFitter::Set(const StvNodePars *inPars, const StvFitErrs *inErrs
                   ,const StvNodePars *jnPars, const StvFitErrs *jnErrs
                   ,      StvNodePars *otPars,       StvFitErrs *otErrs)
{
  mKase = 1;		// join left & rite part of track
  mInPars = inPars; mInErrs = inErrs;
  mOtPars = otPars; mOtErrs = otErrs;
  mJnPars = jnPars; mJnErrs = jnErrs;
  mDelta  = mInPars->delta();  mDelta *= kDeltaFactor;

}
//______________________________________________________________________________
void StvFitter::Prep()
{
  mDelta  = mInPars->delta();  mDelta *= kDeltaFactor;
  mHit   = 0; mHitPlane = 0;
  double myTan = mInPars->_tanl;
  mCos2L = 1./(1+myTan*myTan);
  mCosL = sqrt(mCos2L);
  mSinL = myTan*mCosL;
  mCosP = mInPars->_cosCA;
  mSinP = mInPars->_sinCA;

  mTkPars = *mInPars;
//		Track Frame
  mDcaFrame[0][0] =  mCosL*mCosP;
  mDcaFrame[0][1] =  mCosL*mSinP;
  mDcaFrame[0][2] =  mSinL;

  mDcaFrame[1][0] = -mSinP;
  mDcaFrame[1][1] =  mCosP;
  mDcaFrame[1][2] =  0;

  mDcaFrame[2][0] = -mSinL*mCosP;
  mDcaFrame[2][1] = -mSinL*mSinP;
  mDcaFrame[2][2] =  mCosL;

}
//______________________________________________________________________________
double StvFitter::Xi2(const StvHit *hit)
{
  if (mHit == hit) return mXi2;
  mFailed = 0;
  mHit = hit;
  const float *errMtx=mHit->errMtx();
  if (!mHit->IsHit()) mKase=2; 		//Hit is a vertex

  mHitPlane = mHit->detector();

//	restore old parameters for nhits>1  
  mTkPars._x = mInPars->_x; mTkPars._y = mInPars->_y; mTkPars._z = mInPars->_z;

//		Hit position
  const float *hP = mHit->x();

//		Track direction
  double *tD = mDcaFrame[0];
//		Start track position
  double *tP = &mTkPars._x;


//		Distance to DCA along track in xy
//mDeltaL = DDOT(hP,tP,tD);  
//		DCA track position
  switch (mKase) {
    case 0: {
      mHitErrCalc = (StvHitErrCalculator*)mHitPlane->GetHitErrCalc();
      assert(mHitErrCalc);
      mHitErrCalc->SetTrack(tD);
//       const StHitPlane *hp = hit->detector(); 
//       const Mtx33F_t &hD = hp->GetDir(hit->x());
//       int ans = mHitErrCalc->CalcDcaErrs(hit->x(),hD,mHitErrs);
      int ans = mHitErrCalc->CalcDcaErrs(hit,mHitErrs);
      if (ans) {mXi2 = 1e11; return mXi2;}
      assert(mHitErrs[0]>=1e-8);
      assert(mHitErrs[1]*mHitErrs[1]<=mHitErrs[0]*mHitErrs[2]);
      assert(mHitErrs[2]>=1e-8);
    }; break;

    case 1: assert(0 && "Wrong case 1");

    case 2: {
      double d[6]={errMtx[0],errMtx[1],errMtx[2]
                  ,errMtx[3],errMtx[4],errMtx[5]};
      TCL::trasat(mDcaFrame[1],d,mHitErrs,2,3); }
  }
  assert(mHitErrs[0]>0);
  assert(mHitErrs[2]>0);
  assert(mHitErrs[2]*mHitErrs[0]>mHitErrs[1]*mHitErrs[1]);

//		Hit position wrt track 
  double dca[3] = {hP[0]-tP[0],hP[1]-tP[1],hP[2]-tP[2]};

  mDcaT=VDOT(mDcaFrame[0],dca);
  mDcaP=VDOT(mDcaFrame[1],dca);
  mDcaL=VDOT(mDcaFrame[2],dca);
//		small account non zero distance to hit along track
  double dS = mDcaT*mCosL;
  mDcaP-= 0.5*mTkPars._curv*dS*dS;

  double G[3] = {mInErrs->mHH,mInErrs->mHZ,mInErrs->mZZ};
  if (mKase==0) {// Include Hit Errs
//VP??    if (G[0]+G[2]>(mHitErrs[0]+mHitErrs[2])*kTooBigErrFactor) mFailed = kBigErrs;
    for (int j=0;j<3;j++) {G[j]+=mHitErrs[j];}
  }// end Include Hit Errs

//  (BB*dX*dX-2*BA*dX*dY+AAdY*dY)/det 
  mXi2 = MyXi2(G,mDcaP,mDcaL);
  return mXi2 ; 
}  
//______________________________________________________________________________
double StvFitter::Xi2()
{
  mFailed = 0;
  double inErr = mInErrs->mHH+mInErrs->mZZ;
  double jnErr = mJnErrs->mHH+mJnErrs->mZZ;
  if (jnErr>inErr) {//Not good order
    const StvNodePars *swp = mInPars; mInPars=mJnPars; mJnPars=swp;
    const StvFitErrs  *swe = mInErrs; mInErrs=mJnErrs; mJnErrs=swe;
  }

  StvFitPars F   = (*mInPars-*mJnPars);
  double     Zero[5]= {0};
  mXi2 = JoinTwo(5,F.Arr()    ,mInErrs->Arr()
                ,5,Zero       ,mJnErrs->Arr()
		,mQQPars.Arr(),mQQErrs.Arr());
  mFailed = (mXi2>kXtraBigXi2); 
  return mXi2;
}  
//______________________________________________________________________________
int StvFitter::Update()
{
static int nCall=0; nCall++;
StvDebug::Break(nCall);
  if(mFailed>0) return mFailed;
  mFailed = 0;
  switch (mKase) {
    case 0: mFailed = Hpdate(); break;		//Hit+Track
    case 1: mFailed = Jpdate();	break; 		//Track join
    case 2: mFailed = Vpdate();	break;		//Vertex+track
  }

  double fak = 1.;
  for (int i=0;i<5;i++) {
    double f = fabs(mQQPars[i])/mDelta[i];
    if (fak<f) fak=f;
  }
  if (fak>1.) { mFailed = kBigVari; TCL::vscale(mQQPars,1./fak,mQQPars,5);}

  *mOtPars+= mQQPars;
  mOtErrs->SetHz(mOtPars->_hz);
  return mFailed;
}
//______________________________________________________________________________
int StvFitter::Hpdate()
{
///		this is Update for track+hit fit
		
  mTkErrs = *mInErrs;


//		New Z ortogonal to X (track direction)
  StvFitPars myHitPars(mDcaP, mDcaL );
  StvFitErrs myHitErrs(mHitErrs[0],mHitErrs[1],mHitErrs[2]);
  StvFitPars myTrkPars;

  double myXi2 = JoinTwo(2,myHitPars.Arr(),myHitErrs.Arr()
                        ,5,myTrkPars.Arr(),mTkErrs.Arr()
		        ,  mQQPars.Arr(),mOtErrs->Arr());
  mFailed = (myXi2>kXtraBigXi2); 
  *mOtPars = mTkPars;

  return mFailed;
}  
//______________________________________________________________________________
int StvFitter::Vpdate()
{
///		this is Update for track+vertex fit
static int nCall=0; nCall++;
  mTkErrs = *mInErrs;

//		New Z ortogonal to X (track direction)
  StvFitPars myHitPars(mDcaP, mDcaL );
  StvFitPars myTrkPars;

  double myXi2 = JoinVtx(2,myHitPars.Arr(),mHitErrs
                        ,5,myTrkPars.Arr(),mTkErrs.Arr()
		        ,  mQQPars.Arr(),mOtErrs->Arr());
  if (myXi2){}
  *mOtPars = mTkPars;
  for (int i=0;i<3;i++) {mOtErrs->Arr()[i]+=mHitErrs[i];}
  return 0;
}  
//______________________________________________________________________________
int StvFitter::Jpdate()
{
///		this is Update for sub track+sub track fit (join)
  *mOtPars = *mJnPars; 
  *mOtErrs =  mQQErrs;   
  return mFailed;
}
//______________________________________________________________________________
double StvFitter::TooBig(StvFitPars &fp, int *mask) const
{
  double fakt = 0;
  int msk=0;
  for (int i=0;i<5;i++) { 
    double f = fabs(fp[i])/mDelta[i];
    if (fakt>f) continue;
    fakt=f; msk|= 1<<i;
  }
  if (mask) *mask = msk;
  return fakt;
}
//______________________________________________________________________________
void StvFitter::Test()
{
  double A[5]={1,2,3,4,5};
  double AA[15]=
  {1
  ,0,2
  ,0,0,3
  ,0,0,0,4
  ,0,0,0,0,5};
  double DA[5]={1,2,3,4,5};
  
  double B[5]={6,7,8,9,10};
  double BB[15]=
  {6
  ,0,7
  ,0,0,8
  ,0,0,0,9
  ,0,0,0,0,10};
  double DB[5]={6,7,8,9,10};
  
  double C[5],DC[5],CC[15],myC[5];
  
  for (int i=0;i<5;i++) {
    C[i] = (A[i]/DA[i]+ B[i]/DB[i])/(1/DA[i]+ 1/DB[i]);
    DC[i] = 1/(1/DA[i]+ 1/DB[i]);
    printf("%d C=%g (%g)\n",i,C[i],DC[i]);
  }
  
  
  JoinTwo(5,A,AA,5,B,BB,myC,CC);
  printf("Result =");
  for (int i=0;i<5;i++) {printf(" %g",myC[i]);}
  printf("\nError matrix =\n");
  for (int i=0,li=0;i< 5;li+=++i) {
    for (int j=0;j<=i; j++) { printf("%g\t",CC[li+j]);}
    printf("\n");
  }


 BB[5]=1e3;BB[9]=1e3;BB[14]=1e3;

  printf("  JoinTwo(5,A,AA,5,B,BB,myC,CC)\n");
  JoinTwo(5,A,AA,5,B,BB,myC,CC);
  printf("Result =");
  for (int i=0;i<5;i++) {printf(" %g",myC[i]);}
  printf("\nError matrix =\n");
  for (int i=0,li=0;i< 5;li+=++i) {
    for (int j=0;j<=i; j++) { printf("%g\t",CC[li+j]);}
    printf("\n");
  }
  printf("  JoinTwo(2,B,BB,5,A,AA,myC,CC)\n");
  JoinTwo(2,B,BB,5,A,AA,myC,CC);
  printf("Result =");
  for (int i=0;i<5;i++) {printf(" %g",myC[i]);}
  printf("\nError matrix =\n");
  for (int i=0,li=0;i< 5;li+=++i) {
    for (int j=0;j<=i; j++) { printf("%g\t",CC[li+j]);}
    printf("\n");
  }

}
