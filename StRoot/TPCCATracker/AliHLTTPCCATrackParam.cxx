// $Id: AliHLTTPCCATrackParam.cxx,v 1.1 2016/02/05 23:27:29 fisyak Exp $
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

bool  AliHLTTPCCATrackParam::TransportToX( float x, float sinPhi0, float cosPhi0,  float Bz, float maxSinPhi )
{
  //* Transport the track parameters to X=x, using linearization at phi0 with 0 curvature,
  //* and the field value Bz
  //* maxSinPhi is the max. allowed value for |t0.SinPhi()|
  //* linearisation of trajectory t0 is also transported to X=x,
  //* returns 1 if OK
  //*

  const float ex = cosPhi0;
  const float ey = sinPhi0;
  const float dx = x - X();

  if ( CAMath::Abs( ex ) < 1.e-4 ) return 0;
  const float exi = 1. / ex;
  
  const float dxBz = dx * Bz;
  const float dS = dx * exi;
  const float h2 = dS * exi * exi;
  const float h4 = .5 * h2 * dxBz;

  //const float H0[5] = { 1,0, h2,  0, h4 };
  //const float H1[5] = { 0, 1, 0, dS,  0 };
  //const float H2[5] = { 0, 0, 1,  0, dxBz };
  //const float H3[5] = { 0, 0, 0,  1,  0 };
  //const float H4[5] = { 0, 0, 0,  0,  1 };

  const float sinPhi = SinPhi() + dxBz * QPt();
  if ( maxSinPhi > 0 && CAMath::Abs( sinPhi ) > maxSinPhi ) return 0;

  fX    = X() + dx;
  fP[0] += dS * ey + h2 * ( SinPhi() - ey )  +   h4 * QPt();
  fP[1] += dS * DzDs();
  fP[2] = sinPhi;

  const float c00 = fC[0];
  const float c10 = fC[1];
  const float c11 = fC[2];
  const float c20 = fC[3];
  const float c21 = fC[4];
  const float c22 = fC[5];
  const float c30 = fC[6];
  const float c31 = fC[7];
  const float c32 = fC[8];
  const float c33 = fC[9];
  const float c40 = fC[10];
  const float c41 = fC[11];
  const float c42 = fC[12];
  const float c43 = fC[13];
  const float c44 = fC[14];


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






//*
//* Rotation
//*


bool AliHLTTPCCATrackParam::Rotate( float alpha, float maxSinPhi )
{
  //* Rotate the coordinate system in XY on the angle alpha

  const float cA = CAMath::Cos( alpha );
  const float sA = CAMath::Sin( alpha );
  const float x = X(), y = Y(), sP = SinPhi(), cP = GetCosPhi();
  const float cosPhi = cP * cA + sP * sA;
  const float sinPhi = -cP * sA + sP * cA;

  if ( CAMath::Abs( sinPhi ) > maxSinPhi || CAMath::Abs( cosPhi ) < 1.e-2 || CAMath::Abs( cP ) < 1.e-2  ) return 0;

  const float j0 = cP / cosPhi;
  const float j2 = cosPhi / cP;

  SetX( x*cA +  y*sA );
  SetY( -x*sA +  y*cA );
  SetSignCosPhi( CAMath::Abs(cosPhi)/cosPhi );
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

bool AliHLTTPCCATrackParam::Rotate( float alpha, AliHLTTPCCATrackLinearisation &t0, float maxSinPhi )
{
  //* Rotate the coordinate system in XY on the angle alpha

  const float cA = CAMath::Cos( alpha );
  const float sA = CAMath::Sin( alpha );
  const float x0 = X(), y0 = Y(), sP = t0.SinPhi(), cP = t0.CosPhi();
  const float cosPhi = cP * cA + sP * sA;
  const float sinPhi = -cP * sA + sP * cA;

  if ( CAMath::Abs( sinPhi ) > maxSinPhi || CAMath::Abs( cosPhi ) < 1.e-2 || CAMath::Abs( cP ) < 1.e-2  ) return 0;

  //float J[5][5] = { { j0, 0, 0,  0,  0 }, // Y
  //                    {  0, 1, 0,  0,  0 }, // Z
  //                    {  0, 0, j2, 0,  0 }, // SinPhi
  //                  {  0, 0, 0,  1,  0 }, // DzDs
  //                  {  0, 0, 0,  0,  1 } }; // Kappa

  const float j0 = cP / cosPhi;
  const float j2 = cosPhi / cP;
  const float d[2] = {Y() - y0, SinPhi() - sP};

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

AliHLTTPCCATrackParam AliHLTTPCCATrackParam::GetGlobalParam(float alpha) const
{
  AliHLTTPCCATrackParam r = *this;
  r.Rotate( -alpha );
  return r;
}
