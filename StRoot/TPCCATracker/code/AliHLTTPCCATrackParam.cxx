// $Id: AliHLTTPCCATrackParam.cxx,v 1.7 2012/08/13 19:35:06 fisyak Exp $
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


#include "AliHLTTPCCATrackParam.h"
#include "AliHLTTPCCAMath.h"
#include "AliHLTTPCCATrackLinearisation.h"
#include <iostream>

//
// Circle in XY:
//
// kCLight = 0.000299792458;
// Kappa = Bz*kCLight*QPt;
// R  = 1/CAMath::Abs(Kappa);
// Xc = X - sin(Phi)/Kappa;
// Yc = Y + cos(Phi)/Kappa;
//

float AliHLTTPCCATrackParam::GetDist2( const AliHLTTPCCATrackParam &t ) const
{
  // get squared distance between tracks

  float dx = GetX() - t.GetX();
  float dy = GetY() - t.GetY();
  float dz = GetZ() - t.GetZ();
  return dx*dx + dy*dy + dz*dz;
}

float AliHLTTPCCATrackParam::GetDistXZ2( const AliHLTTPCCATrackParam &t ) const
{
  // get squared distance between tracks in X&Z

  float dx = GetX() - t.GetX();
  float dz = GetZ() - t.GetZ();
  return dx*dx + dz*dz;
}


float  AliHLTTPCCATrackParam::GetS( float x, float y, float Bz ) const
{
  //* Get XY path length to the given point

  float k  = GetKappa( Bz );
  float ex = GetCosPhi();
  float ey = GetSinPhi();
  x -= GetX();
  y -= GetY();
  float dS = x * ex + y * ey;
  if ( CAMath::Abs( k ) > 1.e-4 ) dS = CAMath::ATan2( k * dS, 1 + k * ( x * ey - y * ex ) ) / k;
  return dS;
}

void  AliHLTTPCCATrackParam::GetDCAPoint( float x, float y, float z,
    float &xp, float &yp, float &zp,
    float Bz ) const
{
  //* Get the track point closest to the (x,y,z)

  float x0 = GetX();
  float y0 = GetY();
  float k  = GetKappa( Bz );
  float ex = GetCosPhi();
  float ey = GetSinPhi();
  float dx = x - x0;
  float dy = y - y0;
  float ax = dx * k + ey;
  float ay = dy * k - ex;
  float a = sqrt( ax * ax + ay * ay );
  xp = x0 + ( dx - ey * ( ( dx * dx + dy * dy ) * k - 2 * ( -dx * ey + dy * ex ) ) / ( a + 1 ) ) / a;
  yp = y0 + ( dy + ex * ( ( dx * dx + dy * dy ) * k - 2 * ( -dx * ey + dy * ex ) ) / ( a + 1 ) ) / a;
  float s = GetS( x, y, Bz );
  zp = GetZ() + GetDzDs() * s;
  if ( CAMath::Abs( k ) > 1.e-2 ) {
    float dZ = CAMath::Abs( GetDzDs() * CAMath::TwoPi() / k );
    if ( dZ > .1 ) {
      zp += CAMath::Nint( ( z - zp ) / dZ ) * dZ;
    }
  }
}


//*
//* Transport routines
//*


bool  AliHLTTPCCATrackParam::TransportToX( float x, AliHLTTPCCATrackLinearisation &t0, float Bz,  float maxSinPhi, float *DL )
{
  //* Transport the track parameters to X=x, using linearization at t0, and the field value Bz
  //* maxSinPhi is the max. allowed value for |t0.SinPhi()|
  //* linearisation of trajectory t0 is also transported to X=x,
  //* returns 1 if OK
  //*

  float ex = t0.CosPhi();
  float ey = t0.SinPhi();
  float k   = t0.QPt() * Bz;
  float dx = x - X();

  float ey1 = k * dx + ey;
  float ex1;

  // check for intersection with X=x

  if ( CAMath::Abs( ey1 ) > maxSinPhi ) return 0;

  ex1 = CAMath::Sqrt( 1 - ey1 * ey1 );
  if ( ex < 0 ) ex1 = -ex1;

  float dx2 = dx * dx;
  float ss = ey + ey1;
  float cc = ex + ex1;

  if ( CAMath::Abs( cc ) < 1.e-4 || CAMath::Abs( ex ) < 1.e-4 || CAMath::Abs( ex1 ) < 1.e-4 ) return 0;

  float tg = ss / cc; // tan((phi1+phi)/2)

  float dy = dx * tg;
  float dl = dx * CAMath::Sqrt( 1 + tg * tg );


/*  float dS;
  float dl = dx * ex + dy * ey;
  float dSin = dl * k;
  float dCos = ex*ex1 + ey*ey1;
  if(fabs(dSin)<0.02f) 
  {
    dS = dl*(1.f+dSin*dSin/6.f);
  }
  else    
  {
    dS = CAMath::ATan2(dSin,dCos)/k;
  }
  float dz = dS * t0.DzDs();

  float cci = 1. / cc;
  float exi = 1. / ex;
  float ex1i = 1. / ex1;

  float s_p = dx*Bz;
  float y_f = dx * ( 1 + dCos ) * exi * ex1i * cci;;
  float y_p = (1+dCos)*dx*dx*Bz * cci * cci * ex1i;
  
  float dl0_p = ey * y_p;
  float dSin_p = k * dl0_p + dl*Bz;
  float dSdsin;
  if(fabs(dSin)<0.02f) 
  {
    dSdsin = dl0_p*(1+dSin*dSin/6.f) + dl/3.f*dSin*dSin_p;
  }
  else    
  {
    float dCos_p = dx*Bz*(ey - ey1 * ex * ex1i);
    float tgd = dSin/dCos;
    dSdsin = 1.f/(1.f + tgd*tgd) * (dSin_p/dCos - tgd/dCos * dCos_p) / k;
    dSdsin -= dS/t0.QPt() ;
  }  
  
  float z_f = fP[3]*dy*ex1i;
  float z_p = fP[3]*dSdsin;
  float z_t = dS;

  t0.SetCosPhi( ex1 );
  t0.SetSinPhi( ey1 );

  fX    = X() + dx;
  fP[0] = Y() + dy;
  fP[1] = Z() + dz;
  fP[2] = t0.SinPhi();

  float c00 = fC[0];
  float c10 = fC[1];
  float c11 = fC[2];
  float c20 = fC[3];
  float c21 = fC[4];
  float c22 = fC[5];
  float c30 = fC[6];
  float c31 = fC[7];
  float c32 = fC[8];
  float c33 = fC[9];
  float c40 = fC[10];
  float c41 = fC[11];
  float c42 = fC[12];
  float c43 = fC[13];
  float c44 = fC[14];

  fC[0] = c00 + y_f*c20 + y_p*c40 + (c20+y_f*c22+y_p*c42)*y_f + (c40+y_f*c42+y_p*c44)*y_p;
  fC[1] = c10 + z_f*c20 + z_t*c30 + z_p*c40 + (c21+z_f*c22+z_t*c32+z_p*c42)*y_f + (c41+z_f*c42+z_t*c43+z_p*c44)*y_p;
  fC[2] = c11 + z_f*c21 + z_t*c31 + z_p*c41 + (c21+z_f*c22+z_t*c32+z_p*c42)*z_f + (c31+z_f*c32+z_t*c33+z_p*c43)*z_t + (c41+z_f*c42+z_t*c43+z_p*c44)*z_p;
  fC[3] = c20 + y_f*c22 + y_p*c42 + (c40+y_f*c42+y_p*c44)*s_p;
  fC[4] = c21 + z_f*c22 + z_t*c32 + z_p*c42 + (c41+z_f*c42+z_t*c43+z_p*c44)*s_p;
  fC[5] = c22 + s_p*c42 + (c42 + s_p*c44)*s_p;
  fC[6] = c30 + y_f*c32 + y_p*c43;
  fC[7] = c31 + z_f*c32 + z_t*c33 + z_p*c43;
  fC[8] = c32 + s_p*c43;
  fC[9] = c33;
  fC[10]= c40 + y_f*c42 + y_p*c44;
  fC[11]= c41 + z_f*c42 + z_t*c43 + z_p*c44;
  fC[12]= c42 + s_p*c44;
  fC[13]= c43;
  fC[14]= c44;*/

  if ( cc < 0 ) dl = -dl;
  float dSin = dl * k / 2;
  if ( dSin > 1 ) dSin = 1;
  if ( dSin < -1 ) dSin = -1;
  float dS = ( CAMath::Abs( k ) > 1.e-4 )  ? ( 2 * CAMath::ASin( dSin ) / k ) : dl;
  float dz = dS * t0.DzDs();

  if ( DL ) *DL = -dS * CAMath::Sqrt( 1 + t0.DzDs() * t0.DzDs() );

  float cci = 1. / cc;
  float exi = 1. / ex;
  float ex1i = 1. / ex1;

  float d[5] = { 0,
                   0,
                   fP[2] - t0.SinPhi(),
                   fP[3] - t0.DzDs(),
                   fP[4] - t0.QPt()
                 };

  //float H0[5] = { 1,0, h2,  0, h4 };
  //float H1[5] = { 0, 1, 0, dS,  0 };
  //float H2[5] = { 0, 0, 1,  0, dxBz };
  //float H3[5] = { 0, 0, 0,  1,  0 };
  //float H4[5] = { 0, 0, 0,  0,  1 };

  float h2 = dx * ( 1 + ey * ey1 + ex * ex1 ) * exi * ex1i * cci;
  float h4 = dx2 * ( cc + ss * ey1 * ex1i ) * cci * cci * Bz;
  float dxBz = dx * Bz;

  t0.SetCosPhi( ex1 );
  t0.SetSinPhi( ey1 );

  float DSin = dx*Bz*fP[4];
  float sin = fP[2] + DSin;
  if(sin>0.999) sin = 0.999;
  if(sin<-0.999) sin = -0.999;
//  float cos = sqrt(1-sin*sin);
//  float cos0 = sqrt(1-fP[2]*fP[2]);
//  float Ds = dx/cos0;    //(asin(sin) - asin(fP[2]))/(Bz*fP[4]);
//  float yy = Y() + 1.f/(Bz*fP[4])*(cos0 - cos);
//  float zz = Z() + fP[3] * Ds;

//  float y_f = 1.f/(Bz*fP[4])*(sin/cos - fP[2]/sqrt(1-fP[2]*fP[2]));
//  float y_p = 1.f/(Bz*fP[4])*(1/fP[4]*(sqrt(1-sin*sin) - sqrt(1-fP[2]*fP[2])) + dx*Bz*sin/cos);
//  float z_f = fP[3]*dx*fP[2]/(cos0*cos0*cos0); //fP[3]*dx*sin/(cos*cos*cos);
//  float z_t = Ds;
//  float z_p = 0.f;  //fP[3]*dx*dx*Bz*sin/(cos*cos*cos);
//  float s_p = dx*Bz;
  float rrr3 = t0.SinPhi() + d[2] + dxBz * d[4];
  if (rrr3 >  1) rrr3 =  1;
  if (rrr3 < -1) rrr3 = -1;
  float rrr = (asin(rrr3));
  rrr = CAMath::Cos(rrr);
//  std::cout << cos <<"  "<< rrr << "     "<< fP[2] <<"  "<<dx<<"  "<<Bz <<"  "<<1-sin*sin<<"  "<< std::endl;

  fX    = X() + dx;
  fP[0] = Y() + dy     + h2 * d[2]           +   h4 * d[4];
  fP[1] = Z() + dz               + dS * d[3];
  fP[2] = t0.SinPhi() +     d[2]           + dxBz * d[4];
  if(CAMath::Abs(fP[2]) > maxSinPhi) fP[2] = t0.SinPhi();
//  fP[0] = yy;
//  fP[1] = zz;
//  fP[2] = sin;

  float c00 = fC[0];
  float c10 = fC[1];
  float c11 = fC[2];
  float c20 = fC[3];
  float c21 = fC[4];
  float c22 = fC[5];
  float c30 = fC[6];
  float c31 = fC[7];
  float c32 = fC[8];
  float c33 = fC[9];
  float c40 = fC[10];
  float c41 = fC[11];
  float c42 = fC[12];
  float c43 = fC[13];
  float c44 = fC[14];

//  float fC1[15];
/*  fC[0] = c00 + y_f*c20 + y_p*c40 + (c20+y_f*c22+y_p*c42)*y_f + (c40+y_f*c42+y_p*c44)*y_p;
  fC[1] = c10 + z_f*c20 + z_t*c30 + z_p*c40 + (c21+z_f*c22+z_t*c32+z_p*c42)*y_f + (c41+z_f*c42+z_t*c43+z_p*c44)*y_p;
  fC[2] = c11 + z_f*c21 + z_t*c31 + z_p*c41 + (c21+z_f*c22+z_t*c32+z_p*c42)*z_f + (c31+z_f*c32+z_t*c33+z_p*c43)*z_t + (c41+z_f*c42+z_t*c43+z_p*c44)*z_p;
  fC[3] = c20 + y_f*c22 + y_p*c42 + (c40+y_f*c42+y_p*c44)*s_p;
  fC[4] = c21 + z_f*c22 + z_t*c32 + z_p*c42 + (c41+z_f*c42+z_t*c43+z_p*c44)*s_p;
  fC[5] = c22 + s_p*c42 + (c42 + s_p*c44)*s_p;
  fC[6] = c30 + y_f*c32 + y_p*c43;
  fC[7] = c31 + z_f*c32 + z_t*c33 + z_p*c43;
  fC[8] = c32 + s_p*c43;
  fC[9] = c33;
  fC[10]= c40 + y_f*c42 + y_p*c44;
  fC[11]= c41 + z_f*c42 + z_t*c43 + z_p*c44;
  fC[12]= c42 + s_p*c44;
  fC[13]= c43;
  fC[14]= c44;*/


  fC[0] = ( c00  + h2 * h2 * c22 + h4 * h4 * c44
            + 2 * ( h2 * c20 + h4 * c40 + h2 * h4 * c42 )  );

  fC[1] = c10 + h2 * c21 + h4 * c41 + dS * ( c30 + h2 * c32 + h4 * c43 );
  fC[2] = c11 + 2 * dS * c31 + dS * dS * c33;

  fC[3] = c20 + h2 * c22 + h4 * c42 + dxBz * ( c40 + h2 * c42 + h4 * c44 );
  fC[4] = c21 + dS * c32 + dxBz * ( c41 + dS * c43 );
  fC[5] = c22 + 2 * dxBz * c42 + dxBz * dxBz * c44;

  fC[6] = c30 + h2 * c32 + h4 * c43;
  fC[7] = c31 + dS * c33;
  fC[8] = c32 + dxBz * c43;
  fC[9] = c33;

  fC[10] = c40 + h2 * c42 + h4 * c44;
  fC[11] = c41 + dS * c43;
  fC[12] = c42 + dxBz * c44;
  fC[13] = c43;
  fC[14] = c44;

//std::cout << fC[0] << "  "<<fC1[0]<<"       "<<fC[2] << "  "<<fC1[2]<<"       "<<fC[5] << "  "<<fC1[5]<<"       "<<fC[9] << "  "<<fC1[9]<<"       "<<fC[14] << "  "<<fC1[14]<<std::endl;
  return 1;
}


bool  AliHLTTPCCATrackParam::TransportToX( float x, float sinPhi0, float cosPhi0,  float Bz, float maxSinPhi )
{
  //* Transport the track parameters to X=x, using linearization at phi0 with 0 curvature,
  //* and the field value Bz
  //* maxSinPhi is the max. allowed value for |t0.SinPhi()|
  //* linearisation of trajectory t0 is also transported to X=x,
  //* returns 1 if OK
  //*

  float ex = cosPhi0;
  float ey = sinPhi0;
  float dx = x - X();

  if ( CAMath::Abs( ex ) < 1.e-4 ) return 0;
  float exi = 1. / ex;

  float dxBz = dx * Bz;
  float dS = dx * exi;
  float h2 = dS * exi * exi;
  float h4 = .5 * h2 * dxBz;

  //float H0[5] = { 1,0, h2,  0, h4 };
  //float H1[5] = { 0, 1, 0, dS,  0 };
  //float H2[5] = { 0, 0, 1,  0, dxBz };
  //float H3[5] = { 0, 0, 0,  1,  0 };
  //float H4[5] = { 0, 0, 0,  0,  1 };

  float sinPhi = SinPhi() + dxBz * QPt();
  if ( maxSinPhi > 0 && CAMath::Abs( sinPhi ) > maxSinPhi ) return 0;

  fX    = X() + dx;
  fP[0] += dS * ey + h2 * ( SinPhi() - ey )  +   h4 * QPt();
  fP[1] += dS * DzDs();
  fP[2] = sinPhi;

  float c00 = fC[0];
  float c10 = fC[1];
  float c11 = fC[2];
  float c20 = fC[3];
  float c21 = fC[4];
  float c22 = fC[5];
  float c30 = fC[6];
  float c31 = fC[7];
  float c32 = fC[8];
  float c33 = fC[9];
  float c40 = fC[10];
  float c41 = fC[11];
  float c42 = fC[12];
  float c43 = fC[13];
  float c44 = fC[14];


  fC[0] = ( c00  + h2 * h2 * c22 + h4 * h4 * c44
            + 2 * ( h2 * c20 + h4 * c40 + h2 * h4 * c42 )  );

  fC[1] = c10 + h2 * c21 + h4 * c41 + dS * ( c30 + h2 * c32 + h4 * c43 );
  fC[2] = c11 + 2 * dS * c31 + dS * dS * c33;

  fC[3] = c20 + h2 * c22 + h4 * c42 + dxBz * ( c40 + h2 * c42 + h4 * c44 );
  fC[4] = c21 + dS * c32 + dxBz * ( c41 + dS * c43 );
  fC[5] = c22 + 2 * dxBz * c42 + dxBz * dxBz * c44;

  fC[6] = c30 + h2 * c32 + h4 * c43;
  fC[7] = c31 + dS * c33;
  fC[8] = c32 + dxBz * c43;
  fC[9] = c33;

  fC[10] = c40 + h2 * c42 + h4 * c44;
  fC[11] = c41 + dS * c43;
  fC[12] = c42 + dxBz * c44;
  fC[13] = c43;
  fC[14] = c44;

  return 1;
}






bool  AliHLTTPCCATrackParam::TransportToX( float x, float Bz, float maxSinPhi )
{
  //* Transport the track parameters to X=x

  AliHLTTPCCATrackLinearisation t0( *this );

  return TransportToX( x, t0, Bz, maxSinPhi );
}



bool  AliHLTTPCCATrackParam::TransportToXWithMaterial( float x,  AliHLTTPCCATrackLinearisation &t0, AliHLTTPCCATrackFitParam &par, float Bz, float maxSinPhi )
{
  //* Transport the track parameters to X=x  taking into account material budget

  const float kRho = 1.54e-3;//1.025e-3 ;//0.9e-3;
//  const float kRadLen = 29.532;//28.94;
  //const float kRhoOverRadLen = kRho / kRadLen;
  const float kRhoOverRadLen = 7.68e-5;
  float dl;

  if ( !TransportToX( x, t0, Bz,  maxSinPhi, &dl ) ) return 0;

  CorrectForMeanMaterial( dl*kRhoOverRadLen, dl*kRho, par );
  return 1;
}


bool  AliHLTTPCCATrackParam::TransportToXWithMaterial( float x,  AliHLTTPCCATrackFitParam &par, float Bz, float maxSinPhi )
{
  //* Transport the track parameters to X=x  taking into account material budget

  AliHLTTPCCATrackLinearisation t0( *this );
  return TransportToXWithMaterial( x, t0, par, Bz, maxSinPhi );
}

bool AliHLTTPCCATrackParam::TransportToXWithMaterial( float x, float Bz, float maxSinPhi )
{
  //* Transport the track parameters to X=x taking into account material budget

  AliHLTTPCCATrackFitParam par;
  CalculateFitParameters( par );
  return TransportToXWithMaterial( x, par, Bz, maxSinPhi );
}


//*
//*  Multiple scattering and energy losses
//*


float AliHLTTPCCATrackParam::BetheBlochGeant( float bg2,
    float kp0,
    float kp1,
    float kp2,
    float kp3,
    float kp4 )
{
  //
  // This is the parameterization of the Bethe-Bloch formula inspired by Geant.
  //
  // bg2  - (beta*gamma)^2
  // kp0 - density [g/cm^3]
  // kp1 - density effect first junction point
  // kp2 - density effect second junction point
  // kp3 - mean excitation energy [GeV]
  // kp4 - mean Z/A
  //
  // The default values for the kp* parameters are for silicon.
  // The returned value is in [GeV/(g/cm^2)].
  //

  const float mK  = 0.307075e-3; // [GeV*cm^2/g]
  const float me  = 0.511e-3;    // [GeV/c^2]
  const float rho = kp0;
  const float x0  = kp1 * 2.303;
  const float x1  = kp2 * 2.303;
  const float mI  = kp3;
  const float mZA = kp4;
  const float maxT = 2 * me * bg2;    // neglecting the electron mass

  //*** Density effect
  float d2 = 0.;
  const float x = 0.5 * CAMath::Log( bg2 );
  const float lhwI = CAMath::Log( 28.816 * 1e-9 * CAMath::Sqrt( rho * mZA ) / mI );
  if ( x > x1 ) {
    d2 = lhwI + x - 0.5;
  } else if ( x > x0 ) {
    const float r = ( x1 - x ) / ( x1 - x0 );
    d2 = lhwI + x - 0.5 + ( 0.5 - lhwI - x0 ) * r * r * r;
  }

  return mK*mZA*( 1 + bg2 ) / bg2*( 0.5*CAMath::Log( 2*me*bg2*maxT / ( mI*mI ) ) - bg2 / ( 1 + bg2 ) - d2 );
}

float AliHLTTPCCATrackParam::BetheBlochSolid( float bg )
{
  //------------------------------------------------------------------
  // This is an approximation of the Bethe-Bloch formula,
  // reasonable for solid materials.
  // All the parameters are, in fact, for Si.
  // The returned value is in [GeV]
  //------------------------------------------------------------------

  return BetheBlochGeant( bg );
}

float AliHLTTPCCATrackParam::BetheBlochGas( float bg )
{
  //------------------------------------------------------------------
  // This is an approximation of the Bethe-Bloch formula,
  // reasonable for gas materials.
  // All the parameters are, in fact, for Ne.
  // The returned value is in [GeV]
  //------------------------------------------------------------------

  const float rho = 0.9e-3;
  const float x0  = 2.;
  const float x1  = 4.;
  const float mI  = 140.e-9;
  const float mZA = 0.49555;

  return BetheBlochGeant( bg, rho, x0, x1, mI, mZA );
}




float AliHLTTPCCATrackParam::ApproximateBetheBloch( float beta2 )
{
  //------------------------------------------------------------------
  // This is an approximation of the Bethe-Bloch formula with
  // the density effect taken into account at beta*gamma > 3.5
  // (the approximation is reasonable only for solid materials)
  //------------------------------------------------------------------
  if ( beta2 >= 1 ) return 0;

  if ( beta2 / ( 1 - beta2 ) > 3.5*3.5 )
    return 0.153e-3 / beta2*( log( 3.5*5940 ) + 0.5*log( beta2 / ( 1 - beta2 ) ) - beta2 );
  return 0.153e-3 / beta2*( log( 5940*beta2 / ( 1 - beta2 ) ) - beta2 );
}


void AliHLTTPCCATrackParam::CalculateFitParameters( AliHLTTPCCATrackFitParam &par, float mass )
{
  //*!

  float p2 = ( 1. + fP[3] * fP[3] );
  float k2 = fP[4] * fP[4];
  float mass2 = mass * mass;
  float beta2 = p2 / ( p2 + mass2 * k2 );

  float pp2 = ( k2 > 1.e-8 ) ? p2 / k2 : 10000; // impuls 2

  //par.fBethe = BetheBlochGas( pp2/mass2);
  par.fBethe = ApproximateBetheBloch( pp2 / mass2 );
  par.fE = CAMath::Sqrt( pp2 + mass2 );
  par.fTheta2 = 14.1 * 14.1 / ( beta2 * pp2 * 1e6 );
  par.fEP2 = par.fE / pp2;

  // Approximate energy loss fluctuation (M.Ivanov)

  const float knst = 0.07; // To be tuned.
  par.fSigmadE2 = knst * par.fEP2 * fP[4];
  par.fSigmadE2 = par.fSigmadE2 * par.fSigmadE2;

  par.fK22 = ( 1. + fP[3] * fP[3] );
  par.fK33 = par.fK22 * par.fK22;
  par.fK43 = fP[3] * fP[4] * par.fK22;
  par.fK44 = fP[3] * fP[3] * fP[4] * fP[4];

}


bool AliHLTTPCCATrackParam::CorrectForMeanMaterial( float xOverX0,  float xTimesRho, const AliHLTTPCCATrackFitParam &par )
{
  //------------------------------------------------------------------
  // This function corrects the track parameters for the crossed material.
  // "xOverX0"   - X/X0, the thickness in units of the radiation length.
  // "xTimesRho" - is the product length*density (g/cm^2).
  //------------------------------------------------------------------

  float &fC22 = fC[5];
  float &fC33 = fC[9];
  float &fC40 = fC[10];
  float &fC41 = fC[11];
  float &fC42 = fC[12];
  float &fC43 = fC[13];
  float &fC44 = fC[14];

  //Energy losses************************

  float dE = par.fBethe * xTimesRho;
  if ( CAMath::Abs( dE ) > 0.3*par.fE ) return 0; //30% energy loss is too much!
  float corr = ( 1. - par.fEP2 * dE );
  if ( corr < 0.3 || corr > 1.3 ) return 0;

  fP[4] *= corr;
  fC40 *= corr;
  fC41 *= corr;
  fC42 *= corr;
  fC43 *= corr;
  fC44 *= corr * corr;
  fC44 += par.fSigmadE2 * CAMath::Abs( dE );
//  std::cout << "dE "<<dE<<"    corr "<<corr<<"  fBethe  " <<par.fBethe<<"  XxRo  "<<xTimesRho<<std::endl;

  //Multiple scattering******************

  float theta2 = par.fTheta2 * CAMath::Abs( xOverX0 );
  fC22 += theta2 * par.fK22 * ( 1. - fP[2] * fP[2] );
  fC33 += theta2 * par.fK33;
  fC43 += theta2 * par.fK43;
  fC44 += theta2 * par.fK44;

  return 1;
}


//*
//* Rotation
//*


bool AliHLTTPCCATrackParam::Rotate( float alpha, float maxSinPhi )
{
  //* Rotate the coordinate system in XY on the angle alpha

  float cA = CAMath::Cos( alpha );
  float sA = CAMath::Sin( alpha );
  float x = X(), y = Y(), sP = SinPhi(), cP = GetCosPhi();
  float cosPhi = cP * cA + sP * sA;
  float sinPhi = -cP * sA + sP * cA;

  if ( CAMath::Abs( sinPhi ) > maxSinPhi || CAMath::Abs( cosPhi ) < 1.e-2 || CAMath::Abs( cP ) < 1.e-2  ) return 0;

  float j0 = cP / cosPhi;
  float j2 = cosPhi / cP;

  SetX( x*cA +  y*sA );
  SetY( -x*sA +  y*cA );
  SetSignCosPhi( cosPhi );
  SetSinPhi( sinPhi );


  //float J[5][5] = { { j0, 0, 0,  0,  0 }, // Y
  //                      {  0, 1, 0,  0,  0 }, // Z
  //                      {  0, 0, j2, 0,  0 }, // SinPhi
  //                    {  0, 0, 0,  1,  0 }, // DzDs
  //                    {  0, 0, 0,  0,  1 } }; // Kappa
  //cout<<"alpha="<<alpha<<" "<<x<<" "<<y<<" "<<sP<<" "<<cP<<" "<<j0<<" "<<j2<<endl;
  //cout<<"      "<<fC[0]<<" "<<fC[1]<<" "<<fC[6]<<" "<<fC[10]<<" "<<fC[4]<<" "<<fC[5]<<" "<<fC[8]<<" "<<fC[12]<<endl;
  fC[0] *= j0 * j0;
  fC[1] *= j0;
  fC[3] *= j0;
  fC[6] *= j0;
  fC[10] *= j0;

  fC[3] *= j2;
  fC[4] *= j2;
  fC[5] *= j2 * j2;
  fC[8] *= j2;
  fC[12] *= j2;
  //cout<<"      "<<fC[0]<<" "<<fC[1]<<" "<<fC[6]<<" "<<fC[10]<<" "<<fC[4]<<" "<<fC[5]<<" "<<fC[8]<<" "<<fC[12]<<endl;
  return 1;
}

void AliHLTTPCCATrackParam::RotateXY( float alpha, float &x, float &y, float &sin, float /* maxSinPhi */) const
{
  //* Rotate the coordinate system in XY on the angle alpha

  float cA = CAMath::Cos( alpha );
  float sA = CAMath::Sin( alpha );

  x = ( X()*cA +  Y()*sA );
  y = ( -X()*sA +  Y()*cA );
  sin = -GetCosPhi() * sA + SinPhi() * cA;

}

bool AliHLTTPCCATrackParam::Rotate( float alpha, AliHLTTPCCATrackLinearisation &t0, float maxSinPhi )
{
  //* Rotate the coordinate system in XY on the angle alpha

  float cA = CAMath::Cos( alpha );
  float sA = CAMath::Sin( alpha );
  float x0 = X(), y0 = Y(), sP = t0.SinPhi(), cP = t0.CosPhi();
  float cosPhi = cP * cA + sP * sA;
  float sinPhi = -cP * sA + sP * cA;

  if ( CAMath::Abs( sinPhi ) > maxSinPhi || CAMath::Abs( cosPhi ) < 1.e-2 || CAMath::Abs( cP ) < 1.e-2  ) return 0;

  //float J[5][5] = { { j0, 0, 0,  0,  0 }, // Y
  //                    {  0, 1, 0,  0,  0 }, // Z
  //                    {  0, 0, j2, 0,  0 }, // SinPhi
  //                  {  0, 0, 0,  1,  0 }, // DzDs
  //                  {  0, 0, 0,  0,  1 } }; // Kappa

  float j0 = cP / cosPhi;
  float j2 = cosPhi / cP;
  float d[2] = {Y() - y0, SinPhi() - sP};

  SetX( x0*cA +  y0*sA );
  SetY( -x0*sA +  y0*cA + j0*d[0] );
  t0.SetCosPhi( cosPhi );
  t0.SetSinPhi( sinPhi );

  SetSinPhi( sinPhi + j2*d[1] );

  fC[0] *= j0 * j0;
  fC[1] *= j0;
  fC[3] *= j0;
  fC[6] *= j0;
  fC[10] *= j0;

  fC[3] *= j2;
  fC[4] *= j2;
  fC[5] *= j2 * j2;
  fC[8] *= j2;
  fC[12] *= j2;

  return 1;
}

bool AliHLTTPCCATrackParam::Filter( float y, float z, float err2Y, float err2Z, float maxSinPhi )
{
  assert( maxSinPhi > 0.f );
  //* Add the y,z measurement with the Kalman filter

  float c00 = fC[0];
  float c10 = fC[1];
  float c11 = fC[2];
  float c20 = fC[3];
  float c21 = fC[4];
//  float c22 = fC[5];
  float c30 = fC[6];
  float c31 = fC[7];
//  float c32 = fC[8];
//  float c33 = fC[9];
  float c40 = fC[10];
  float c41 = fC[11];
//  float c42 = fC[12];
//  float c43 = fC[13];
//  float c44 = fC[14];
  
  float d = err2Y*err2Z + err2Y*c11 + err2Z*c00 + c00*c11 - c10*c10;
  d = 1.f/d;
  err2Y += c00;
  err2Z += c11;

  float
  z0 = y - fP[0],
  z1 = z - fP[1];

  if ( ISUNLIKELY( err2Y < 1.e-8f ) || ISUNLIKELY( err2Z < 1.e-8f ) ) return 0;

  float mS0 = err2Z*d;
  float mS1 = -c10*d;
  float mS2 = err2Y*d;

  // K = CHtS

  float 
  k00 = c00 * mS0 + c10*mS1,   k01 = c00 * mS1 + c10*mS2,
  k10 = c10 * mS0 + c11*mS1,   k11 = c10 * mS1 + c11*mS2,
  k20 = c20 * mS0 + c21*mS1,   k21 = c20 * mS1 + c21*mS2,
  k30 = c30 * mS0 + c31*mS1,   k31 = c30 * mS1 + c31*mS2,
  k40 = c40 * mS0 + c41*mS1,   k41 = c40 * mS1 + c41*mS2; 

  float sinPhi = fP[2] + k20 * z0 + k21 * z1;

  if ( ISUNLIKELY( CAMath::Abs( sinPhi ) >= maxSinPhi ) ) return 0;

  fNDF  += 2;
  fChi2 += mS0 * z0 * z0 + mS2 * z1 * z1 + 2 * z0 * z1 * mS1;

  fP[ 0] += k00 * z0 + k01 * z1;
  fP[ 1] += k10 * z0 + k11 * z1;
  fP[ 2] = sinPhi ;
  fP[ 3] += k30 * z0 + k31 * z1;
  fP[ 4] += k40 * z0 + k41 * z1;

  fC[ 0] -= (k00 * c00 + k01 * c10); //c00
  
  fC[ 1] -= (k10 * c00 + k11 * c10); //c10
  fC[ 2] -= (k10 * c10 + k11 * c11); //c11
  
  fC[ 3] -= (k20 * c00 + k21 * c10); //c20
  fC[ 4] -= (k20 * c10 + k21 * c11); //c21
  fC[ 5] -= (k20 * c20 + k21 * c21); //c22
  
  fC[ 6] -= (k30 * c00 + k31 * c10); //c30
  fC[ 7] -= (k30 * c10 + k31 * c11); //c31
  fC[ 8] -= (k30 * c20 + k31 * c21); //c32
  fC[ 9] -= (k30 * c30 + k31 * c31); //c33

  fC[10] -= (k40 * c00 + k41 * c10); //c40
  fC[11] -= (k40 * c10 + k41 * c11); //c41
  fC[12] -= (k40 * c20 + k41 * c21); //c42
  fC[13] -= (k40 * c30 + k41 * c31); //c43
  fC[14] -= (k40 * c40 + k41 * c41); //c44
  
  return 1;
}

/*bool AliHLTTPCCATrackParam::Filter( float y, float z, float err2Y, float err2Z, float maxSinPhi )
{
  assert( maxSinPhi > 0.f );
  /// Add the y,z measurement with the Kalman filter

  float
  c00 = fC[ 0],
  c11 = fC[ 2],
  c20 = fC[ 3],
  c31 = fC[ 7],
  c40 = fC[10];

  err2Y += c00;
  err2Z += c11;

  float
  z0 = y - fP[0],
  z1 = z - fP[1];

  if ( ISUNLIKELY( err2Y < 1.e-8f ) || ISUNLIKELY( err2Z < 1.e-8f ) ) return 0;

  float mS0 = 1.f / err2Y;
  float mS2 = 1.f / err2Z;

  // K = CHtS

  float k00, k11, k20, k31, k40;

  k00 = c00 * mS0;
  k20 = c20 * mS0;
  k40 = c40 * mS0;

  k11 = c11 * mS2;
  k31 = c31 * mS2;

  float sinPhi = fP[2] + k20 * z0  ;

  if ( ISUNLIKELY( CAMath::Abs( sinPhi ) >= maxSinPhi ) ) return 0;

  fNDF  += 2;
  fChi2 += mS0 * z0 * z0 + mS2 * z1 * z1 ;

  fP[ 0] += k00 * z0 ;
  fP[ 1] += k11 * z1 ;
  fP[ 2] = sinPhi ;
  fP[ 3] += k31 * z1 ;
  fP[ 4] += k40 * z0 ;

  fC[ 0] -= k00 * c00 ;
  fC[ 3] -= k20 * c00 ;
  fC[ 5] -= k20 * c20 ;
  fC[10] -= k40 * c00 ;
  fC[12] -= k40 * c20 ;
  fC[14] -= k40 * c40 ;

  fC[ 2] -= k11 * c11 ;
  fC[ 7] -= k31 * c11 ;
  fC[ 9] -= k31 * c31 ;

  return 1;
}*/




#if !defined(HLTCA_GPUCODE)
#include <iostream>
#endif

void AliHLTTPCCATrackParam::Print() const
{
  //* print parameters

#if !defined(HLTCA_GPUCODE)
  std::cout << "track: x=" << GetX() << " c=" << GetSignCosPhi() << ", P= " << GetY() << " " << GetZ() << " " << GetSinPhi() << " " << GetDzDs() << " " << GetQPt() << std::endl;
  std::cout << "errs2: " << GetErr2Y() << " " << GetErr2Z() << " " << GetErr2SinPhi() << " " << GetErr2DzDs() << " " << GetErr2QPt() << std::endl;
#endif
}

std::istream &operator>>( std::istream &in, AliHLTTPCCATrackParam &t )
{
  in >> t.fX;
  in >> t.fSignCosPhi;
  in >> t.fChi2;
  in >> t.fNDF;
  for ( int i = 0; i < 5; i++ ) in >> t.fP[i];
  for ( int i = 0; i < 15; i++ ) in >> t.fC[i];
  return in;
}

std::ostream &operator<<( std::ostream &out, const AliHLTTPCCATrackParam &t )
{
  out << t.X() << " "
  << t.SignCosPhi() << " "
  << t.Chi2() << " "
  << t.NDF()
  << '\n';
  for ( int i = 0; i < 5; i++ ) out << t.Par()[i] << " ";
  out << '\n';
  for ( int i = 0; i < 15; i++ ) out << t.Cov()[i] << " ";
  out << '\n';
  return out;
}
