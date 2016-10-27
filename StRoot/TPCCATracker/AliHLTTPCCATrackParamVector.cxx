// $Id: AliHLTTPCCATrackParamVector.cxx,v 1.3 2012/08/14 16:30:42 fisyak Exp $
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


#include "AliHLTTPCCATrackParamVector.h"
#include "AliHLTTPCCATrackParam.h"
#include "AliHLTTPCCAMath.h"
#include "AliHLTTPCCATrackLinearisationVector.h"
#include <iostream>
#include <iomanip>

#include <assert.h>
#include "debug.h"

//
// Circle in XY:
//
// kCLight = 0.000299792458;
// Kappa = Bz*kCLight*QPt;
// R  = 1/CAMath::Abs(Kappa);
// Xc = X - sin(Phi)/Kappa;
// Yc = Y + cos(Phi)/Kappa;
//

AliHLTTPCCATrackParamVector::AliHLTTPCCATrackParamVector(const AliHLTTPCCATrackParam &param):
  fX( param.X() ),
  fSignCosPhi( param.SignCosPhi() ),
  fChi2( param.Chi2() ),
  fNDF( param.NDF() )
{
  for(int i=0; i<5; i++) fP[i] = param.Par()[i];
  for(int i=0; i<15; i++) fC[i] = param.Cov()[i];
}

void AliHLTTPCCATrackParamVector::NormilizeSignCosPhi( const AliHLTTPCCATrackLinearisationVector& l, const float_m &m ) {
  SetSignCosPhi( float_v(Vc::One),  m && (l.CosPhi() >= float_v(Vc::Zero)) );
  SetSignCosPhi( float_v(-1.f), m && (l.CosPhi() < float_v(Vc::Zero)) );
}

float_v AliHLTTPCCATrackParamVector::GetDist2( const AliHLTTPCCATrackParamVector &t ) const
{
  // get squared distance between tracks

  const float_v &dx = GetX() - t.GetX();
  const float_v &dy = GetY() - t.GetY();
  const float_v &dz = GetZ() - t.GetZ();
  return dx*dx + dy*dy + dz*dz;
}

float_v AliHLTTPCCATrackParamVector::GetDistXZ2( const AliHLTTPCCATrackParamVector &t ) const
{
  // get squared distance between tracks in X&Z

  const float_v &dx = GetX() - t.GetX();
  const float_v &dz = GetZ() - t.GetZ();
  return dx*dx + dz*dz;
}


float_v  AliHLTTPCCATrackParamVector::GetS( const float_v &_x, const float_v &_y, const float_v &Bz ) const
{
  //* Get XY path length to the given point

  const float_v &k  = GetKappa( Bz );
  const float_v &ex = GetCosPhi();
  const float_v &ey = GetSinPhi();
  const float_v &x = _x - GetX();
  const float_v &y = _y - GetY();
  float_v dS = x * ex + y * ey;
  const float_m &mask = CAMath::Abs( k ) > 1.e-4f;
  if ( !mask.isEmpty() ) {
    dS( mask ) = CAMath::ATan2( k * dS, 1 + k * ( x * ey - y * ex ) ) / k;
  }
  return dS;
}

void  AliHLTTPCCATrackParamVector::GetDCAPoint( const float_v &x, const float_v &y, const float_v &z,
    float_v *xp, float_v *yp, float_v *zp, const float_v &Bz ) const
{
  //* Get the track point closest to the (x,y,z)

  const float_v &x0 = GetX();
  const float_v &y0 = GetY();
  const float_v &k  = GetKappa( Bz );
  const float_v &ex = GetCosPhi();
  const float_v &ey = GetSinPhi();
  const float_v &dx = x - x0;
  const float_v &dy = y - y0;
  const float_v &ax = dx * k + ey;
  const float_v &ay = dy * k - ex;
  const float_v &a = sqrt( ax * ax + ay * ay );
  *xp = x0 + ( dx - ey * ( ( dx * dx + dy * dy ) * k - 2.f * ( -dx * ey + dy * ex ) ) / ( a + 1.f ) ) / a;
  *yp = y0 + ( dy + ex * ( ( dx * dx + dy * dy ) * k - 2.f * ( -dx * ey + dy * ex ) ) / ( a + 1.f ) ) / a;
  const float_v s = GetS( x, y, Bz );
  *zp = GetZ() + GetDzDs() * s;
  const float_v dZ = CAMath::Abs( GetDzDs() * CAMath::TwoPi() / k );
  const float_m mask = CAMath::Abs( k ) > 1.e-2f && dZ > .1f;
  ( *zp )( mask ) += CAMath::Round( ( z - *zp ) / dZ ) * dZ;
}


//*
//* Transport routines
//*


float_m AliHLTTPCCATrackParamVector::TransportToX( const float_v &x, AliHLTTPCCATrackLinearisationVector &t0, const float_v &Bz,  const float maxSinPhi, float_v *DL, const float_m &_mask )
{
  //* Transport the track parameters to X=x, using linearization at t0, and the field value Bz
  //* maxSinPhi is the max. allowed value for |t0.SinPhi()|
  //* linearisation of trajectory t0 is also transported to X=x,
  //* returns 1 if OK
  //*

  const float_v ex = t0.CosPhi();
  const float_v ey = t0.SinPhi();
  const float_v k  = t0.QPt() * Bz;
  const float_v dx = x - X();

  float_v ey1 = k * dx + ey;

  // check for intersection with X=x

  float_v ex1 = CAMath::Sqrt( 1.f - ey1 * ey1 );
  ex1( ex < float_v(Vc::Zero) ) = -ex1;

  const float_v dx2 = dx * dx;
  const float_v ss = ey + ey1;
  const float_v cc = ex + ex1;

  const float_v cci = 1.f / cc;
  const float_v exi = 1.f / ex;
  const float_v ex1i = 1.f / ex1;

  float_m mask = _mask && CAMath::Abs( ey1 ) <= maxSinPhi && CAMath::Abs( cc ) >= 1.e-4f && CAMath::Abs( ex ) >= 1.e-4f && CAMath::Abs( ex1 ) >= 1.e-4f;

  const float_v tg = ss * cci; // tan((phi1+phi)/2)

  const float_v dy = dx * tg;
  float_v dl = dx * CAMath::Sqrt( 1.f + tg * tg );

  dl( cc < float_v(Vc::Zero) ) = -dl;
  const float_v dSin = CAMath::Max( float_v( -1.f ), CAMath::Min( float_v( Vc::One ), dl * k * 0.5f ) );
  float_v dS = dl;
  dS( CAMath::Abs( k ) > float_v(1.e-4f) )  = ( float_v(2.f) * CAMath::ASin( dSin ) / k );
  const float_v dz = dS * t0.DzDs();

  if ( DL ) {
    ( *DL )( mask ) = -dS * CAMath::Sqrt( 1.f + t0.DzDs() * t0.DzDs() );
  }

  const float_v d[3] = { fP[2] - t0.SinPhi(), fP[3] - t0.DzDs(), fP[4] - t0.QPt() };

  //float H0[5] = { 1,0, h2,  0, h4 };
  //float H1[5] = { 0, 1, 0, dS,  0 };
  //float H2[5] = { 0, 0, 1,  0, dxBz };
  //float H3[5] = { 0, 0, 0,  1,  0 };
  //float H4[5] = { 0, 0, 0,  0,  1 };

  const float_v h2 = dx * ( 1.f + ey * ey1 + ex * ex1 ) * exi * ex1i * cci;
  const float_v h4 = dx2 * ( cc + ss * ey1 * ex1i ) * cci * cci * Bz;
  const float_v dxBz = dx * Bz;

  ex1( !mask ) = ex;
  ey1( !mask ) = ey;
  t0.SetCosPhi( ex1 );
  t0.SetSinPhi( ey1 );

  fX( mask )    = X() + dx;
  fP[0]( mask ) = Y() + dy     + h2 * d[0]           +   h4 * d[2];
  fP[1]( mask ) = Z() + dz               + dS * d[1];
  fP[2]( mask ) = t0.SinPhi() +     d[0]           + dxBz * d[2];
  fP[2]( CAMath::Abs(fP[2]) > maxSinPhi ) = t0.SinPhi();

  const float_v c00 = fC[0];
  const float_v c10 = fC[1];
  const float_v c11 = fC[2];
  const float_v c20 = fC[3];
  const float_v c21 = fC[4];
  const float_v c22 = fC[5];
  const float_v c30 = fC[6];
  const float_v c31 = fC[7];
  const float_v c32 = fC[8];
  const float_v c33 = fC[9];
  const float_v c40 = fC[10];
  const float_v c41 = fC[11];
  const float_v c42 = fC[12];
  const float_v c43 = fC[13];
  const float_v c44 = fC[14];

  fC[0]( mask ) = ( c00  + h2 * h2 * c22 + h4 * h4 * c44
            + 2.f * ( h2 * c20 + h4 * c40 + h2 * h4 * c42 )  );

  fC[1]( mask ) = c10 + h2 * c21 + h4 * c41 + dS * ( c30 + h2 * c32 + h4 * c43 );
  fC[2]( mask ) = c11 + 2.f * dS * c31 + dS * dS * c33;

  fC[3]( mask ) = c20 + h2 * c22 + h4 * c42 + dxBz * ( c40 + h2 * c42 + h4 * c44 );
  fC[4]( mask ) = c21 + dS * c32 + dxBz * ( c41 + dS * c43 );
  fC[5]( mask ) = c22 + 2.f * dxBz * c42 + dxBz * dxBz * c44;

  fC[6]( mask ) = c30 + h2 * c32 + h4 * c43;
  fC[7]( mask ) = c31 + dS * c33;
  fC[8]( mask ) = c32 + dxBz * c43;
  fC[9]( mask ) = c33;

  fC[10]( mask ) = c40 + h2 * c42 + h4 * c44;
  fC[11]( mask ) = c41 + dS * c43;
  fC[12]( mask ) = c42 + dxBz * c44;
  fC[13]( mask ) = c43;
  fC[14]( mask ) = c44;

  debugKF() << mask << "\n" << *this << std::endl;
  return mask;
}


float_m AliHLTTPCCATrackParamVector::TransportToX( const float_v &x, const float_v &Bz,
    const float maxSinPhi, const float_m &mask )
{
  //* Transport the track parameters to X=x

//  assert( ( x == 0 && mask ).isEmpty() );

  AliHLTTPCCATrackLinearisationVector t0( *this );

  return TransportToX( x, t0, Bz, maxSinPhi, 0, mask );
}



float_m AliHLTTPCCATrackParamVector::TransportToXWithMaterial( const float_v &x,  AliHLTTPCCATrackLinearisationVector &t0, AliHLTTPCCATrackFitParam &par, const float_v &Bz, const float maxSinPhi, const float_m &mask_ )
{
  //* Transport the track parameters to X=x  taking into account material budget

  const float kRho = 1.025e-3f;//0.9e-3;
//  const float kRadLen = 29.532f;//28.94;
//  const float kRhoOverRadLen = kRho / kRadLen;
  const float kRhoOverRadLen = 7.68e-5;
  float_v dl;

  const float_m &mask = mask_ && TransportToX( x, t0, Bz,  maxSinPhi, &dl, mask_ );
  if ( !mask.isEmpty() ) {
    CorrectForMeanMaterial( dl * kRhoOverRadLen, dl * kRho, par, mask );
  }

  return mask;
}


float_m AliHLTTPCCATrackParamVector::TransportToXWithMaterial( const float_v &x,  AliHLTTPCCATrackFitParam &par, const float_v &Bz, const float maxSinPhi )
{
  //* Transport the track parameters to X=x  taking into account material budget

  AliHLTTPCCATrackLinearisationVector t0( *this );
  return TransportToXWithMaterial( x, t0, par, Bz, maxSinPhi );
}

float_m AliHLTTPCCATrackParamVector::TransportToXWithMaterial( const float_v &x, const float_v &Bz, const float maxSinPhi )
{
  //* Transport the track parameters to X=x taking into account material budget

  AliHLTTPCCATrackFitParam par;
  CalculateFitParameters( par );
  return TransportToXWithMaterial( x, par, Bz, maxSinPhi );
}


//*
//*  Multiple scattering and energy losses
//*


float_v AliHLTTPCCATrackParamVector::BetheBlochGeant( const float_v &bg2,
    const float_v &kp0,
    const float_v &kp1,
    const float_v &kp2,
    const float_v &kp3,
    const float_v &kp4 )
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

  const float mK  = 0.307075e-3f; // [GeV*cm^2/g]
  const float _2me  = 1.022e-3f;    // [GeV/c^2]
  const float_v &rho = kp0;
  const float_v &x0  = kp1 * 2.303f;
  const float_v &x1  = kp2 * 2.303f;
  const float_v &mI  = kp3;
  const float_v &mZA = kp4;
  const float_v &maxT = _2me * bg2;    // neglecting the electron mass

  //*** Density effect
  float_v d2( Vc::Zero );
  const float_v x = 0.5f * CAMath::Log( bg2 );
  const float_v lhwI = CAMath::Log( 28.816f * 1e-9f * CAMath::Sqrt( rho * mZA ) / mI );
  d2( x > x1 ) = lhwI + x - 0.5f;
  const float_v &r = ( x1 - x ) / ( x1 - x0 );
  d2( x > x0 && x <= x1 ) = lhwI + x - 0.5f + ( 0.5f - lhwI - x0 ) * r * r * r;

  return mK*mZA*( float_v( Vc::One ) + bg2 ) / bg2*( 0.5f*CAMath::Log( maxT * maxT / ( mI*mI ) ) - bg2 / ( float_v( Vc::One ) + bg2 ) - d2 );
}

float_v AliHLTTPCCATrackParamVector::BetheBlochSolid( const float_v &bg )
{
  //------------------------------------------------------------------
  // This is an approximation of the Bethe-Bloch formula,
  // reasonable for solid materials.
  // All the parameters are, in fact, for Si.
  // The returned value is in [GeV]
  //------------------------------------------------------------------

  return BetheBlochGeant( bg );
}

float_v AliHLTTPCCATrackParamVector::BetheBlochGas( const float_v &bg )
{
  //------------------------------------------------------------------
  // This is an approximation of the Bethe-Bloch formula,
  // reasonable for gas materials.
  // All the parameters are, in fact, for Ne.
  // The returned value is in [GeV]
  //------------------------------------------------------------------

  const float_v rho = 0.9e-3f;
  const float_v x0  = 2.f;
  const float_v x1  = 4.f;
  const float_v mI  = 140.e-9f;
  const float_v mZA = 0.49555f;

  return BetheBlochGeant( bg, rho, x0, x1, mI, mZA );
}




float_v AliHLTTPCCATrackParamVector::ApproximateBetheBloch( const float_v &beta2 )
{
  //------------------------------------------------------------------
  // This is an approximation of the Bethe-Bloch formula with
  // the density effect taken into account at beta*gamma > 3.5
  // (the approximation is reasonable only for solid materials)
  //------------------------------------------------------------------

  const float_v &beta2_1subBeta2 = beta2 / ( float_v( Vc::One ) - beta2 ); // beta2 * CAMath::Reciprocal( float_v( Vc::One ) - beta2 );
  const float_v &_0p000153_beta2 = 0.153e-3f / beta2;
  const float log_3p5mul5940 = 9.942227380852058f; // log( 3.5 * 5940 )
  const float log_5940 = 8.68946441235669f; // log( 5940 )
  const float_v &log_beta2_1subBeta2 = CAMath::Log( beta2_1subBeta2 );

  float_v ret = _0p000153_beta2 * ( log_5940 + log_beta2_1subBeta2 - beta2 );
  ret( beta2_1subBeta2 > 3.5f*3.5f ) =
    _0p000153_beta2 * ( log_3p5mul5940 + 0.5f * log_beta2_1subBeta2 - beta2 );
  ret.setZero( beta2 >= float_v( Vc::One ) );
  return ret;
}


void AliHLTTPCCATrackParamVector::CalculateFitParameters( AliHLTTPCCATrackFitParam &par, const float_v &mass )
{
  //*!

  const float_v p2 = ( float_v( Vc::One ) + fP[3] * fP[3] );
  float_v k2 = fP[4] * fP[4];
  const float_v mass2 = mass * mass;
  const float_v beta2 = p2 / ( p2 + mass2 * k2 );

  const float_m badK2 = k2 <= 1.e-8f;
  k2(badK2) = 1.f;
  float_v pp2 = 10000.f; pp2( !badK2 ) = p2 / k2; // impuls 2

  //par.fBethe = BetheBlochGas( pp2/mass2);
  par.fBethe = ApproximateBetheBloch( pp2 / mass2 );
  par.fE = CAMath::Sqrt( pp2 + mass2 );
  par.fTheta2 = 14.1f * 14.1f / ( beta2 * pp2 * 1.e6f );
  par.fEP2 = par.fE / pp2;

  // Approximate energy loss fluctuation (M.Ivanov)

  const float knst = 0.07f; // To be tuned.
  par.fSigmadE2 = knst * par.fEP2 * fP[4];
  par.fSigmadE2 = par.fSigmadE2 * par.fSigmadE2;

  par.fK22 = ( float_v( Vc::One ) + fP[3] * fP[3] );
  par.fK33 = par.fK22 * par.fK22;
  par.fK43 = fP[3] * fP[4] * par.fK22;
  par.fK44 = fP[3] * fP[3] * fP[4] * fP[4];
}


float_m AliHLTTPCCATrackParamVector::CorrectForMeanMaterial( const float_v &xOverX0,  const float_v &xTimesRho, const AliHLTTPCCATrackFitParam &par, const float_m &_mask )
{
  //------------------------------------------------------------------
  // This function corrects the track parameters for the crossed material.
  // "xOverX0"   - X/X0, the thickness in units of the radiation length.
  // "xTimesRho" - is the product length*density (g/cm^2).
  //------------------------------------------------------------------

  float_v &fC22 = fC[5];
  float_v &fC33 = fC[9];
  float_v &fC40 = fC[10];
  float_v &fC41 = fC[11];
  float_v &fC42 = fC[12];
  float_v &fC43 = fC[13];
  float_v &fC44 = fC[14];

  //Energy losses************************

  const float_v &dE = par.fBethe * xTimesRho;
  float_m mask = _mask && CAMath::Abs( dE ) <= 0.3f * par.fE; //30% energy loss is too much!
  const float_v &corr = ( float_v( Vc::One ) - par.fEP2 * dE );
  mask &= corr >= 0.3f && corr <= 1.3f;

  fP[4]( mask ) *= corr;
  fC40 ( mask ) *= corr;
  fC41 ( mask ) *= corr;
  fC42 ( mask ) *= corr;
  fC43 ( mask ) *= corr;
  fC44 ( mask ) *= corr * corr;
  fC44 ( mask ) += par.fSigmadE2 * CAMath::Abs( dE );

  //Multiple scattering******************

  const float_v &theta2 = par.fTheta2 * CAMath::Abs( xOverX0 );
  fC22( mask ) += theta2 * par.fK22 * ( float_v( Vc::One ) - fP[2] * fP[2] );
  fC33( mask ) += theta2 * par.fK33;
  fC43( mask ) += theta2 * par.fK43;
  fC44( mask ) += theta2 * par.fK44;

  return mask;
}

float_m AliHLTTPCCATrackParamVector::Rotate( const float_v &alpha, AliHLTTPCCATrackLinearisationVector &t0,
                                              const float maxSinPhi, const float_m &mask )
{
  //* Rotate the coordinate system in XY on the angle alpha

  const float_v cA = CAMath::Cos( alpha );
  const float_v sA = CAMath::Sin( alpha );
  const float_v x0 = X(),y0 = Y(), sP = t0.SinPhi(), cP = t0.CosPhi();
  const float_v cosPhi = cP * cA + sP * sA;
  const float_v sinPhi = -cP * sA + sP * cA;

  float_m ReturnMask(mask);
  ReturnMask &= (!( CAMath::Abs( sinPhi ) > maxSinPhi || CAMath::Abs( cosPhi ) < 1.e-2f || CAMath::Abs( cP ) < 1.e-2f  ));

  //float J[5][5] = { { j0, 0, 0,  0,  0 }, // Y
  //                    {  0, 1, 0,  0,  0 }, // Z
  //                    {  0, 0, j2, 0,  0 }, // SinPhi
  //                  {  0, 0, 0,  1,  0 }, // DzDs
  //                  {  0, 0, 0,  0,  1 } }; // Kappa

  const float_v j0 = cP / cosPhi;
  const float_v j2 = cosPhi / cP;
  const float_v d = SinPhi() - sP;

  SetX( x0*cA +  y0*sA, ReturnMask );
  SetY(-x0*sA +  y0*cA, ReturnMask );
  t0.SetCosPhi( cosPhi );
  t0.SetSinPhi( sinPhi );

  SetSinPhi( sinPhi + j2*d, ReturnMask );

  fC[0](ReturnMask) *= j0 * j0;
  fC[1](ReturnMask) *= j0;
  fC[3](ReturnMask) *= j0;
  fC[6](ReturnMask) *= j0;
  fC[10](ReturnMask) *= j0;

  fC[3](ReturnMask) *= j2;
  fC[4](ReturnMask) *= j2;
  fC[5](ReturnMask) *= j2 * j2;
  fC[8](ReturnMask) *= j2;
  fC[12](ReturnMask) *= j2;

  return ReturnMask;
}

float_m AliHLTTPCCATrackParamVector::FilterWithMaterial( const float_v &y, const float_v &z, float_v err2Y, float_v err2Z, 
                                              float maxSinPhi, const float_m &mask  )
{
  assert( maxSinPhi > 0.f );
  //* Add the y,z measurement with the Kalman filter

  const float_v c00 = fC[0];
  const float_v c10 = fC[1];
  const float_v c11 = fC[2];
  const float_v c20 = fC[3];
  const float_v c21 = fC[4];
//  float c22 = fC[5];
  const float_v c30 = fC[6];
  const float_v c31 = fC[7];
//  float c32 = fC[8];
//  float c33 = fC[9];
  const float_v c40 = fC[10];
  const float_v c41 = fC[11];
//  float c42 = fC[12];
//  float c43 = fC[13];
//  float c44 = fC[14];
  
  float_v d = float_v( Vc::One ) / ( err2Y*err2Z + err2Y*c11 + err2Z*c00 + c00*c11 - c10*c10 );
  err2Y += c00;
  err2Z += c11;

  const float_v
  z0 = y - fP[0],
  z1 = z - fP[1];

  float_m success = mask;//  if ( ISUNLIKELY( err2Y < 1.e-8f ) || ISUNLIKELY( err2Z < 1.e-8f ) ) return 0;
  success &= (err2Y > 1.e-8f ) && ( err2Z > 1.e-8f );

  const float_v mS0 = err2Z*d;
  const float_v mS1 = -c10*d;
  const float_v mS2 = err2Y*d;

  // K = CHtS

  const float_v
  k00 = c00 * mS0 + c10*mS1,   k01 = c00 * mS1 + c10*mS2,
  k10 = c10 * mS0 + c11*mS1,   k11 = c10 * mS1 + c11*mS2,
  k20 = c20 * mS0 + c21*mS1,   k21 = c20 * mS1 + c21*mS2,
  k30 = c30 * mS0 + c31*mS1,   k31 = c30 * mS1 + c31*mS2,
  k40 = c40 * mS0 + c41*mS1,   k41 = c40 * mS1 + c41*mS2; 

  const float_v sinPhi = fP[2] + k20 * z0 + k21 * z1;

  success &= CAMath::Abs( sinPhi ) < maxSinPhi;

  fNDF( static_cast<int_m>(success) )  += 2;
  fChi2(success) += mS0 * z0 * z0 + mS2 * z1 * z1 + 2 * z0 * z1 * mS1;

  fP[ 0](success) += k00 * z0 + k01 * z1;
  fP[ 1](success) += k10 * z0 + k11 * z1;
  fP[ 2](success) = sinPhi ;
  fP[ 3](success) += k30 * z0 + k31 * z1;
  fP[ 4](success) += k40 * z0 + k41 * z1;

  fC[ 0](success) -= (k00 * c00 + k01 * c10); //c00
  
  fC[ 1](success) -= (k10 * c00 + k11 * c10); //c10
  fC[ 2](success) -= (k10 * c10 + k11 * c11); //c11
  
  fC[ 3](success) -= (k20 * c00 + k21 * c10); //c20
  fC[ 4](success) -= (k20 * c10 + k21 * c11); //c21
  fC[ 5](success) -= (k20 * c20 + k21 * c21); //c22
  
  fC[ 6](success) -= (k30 * c00 + k31 * c10); //c30
  fC[ 7](success) -= (k30 * c10 + k31 * c11); //c31
  fC[ 8](success) -= (k30 * c20 + k31 * c21); //c32
  fC[ 9](success) -= (k30 * c30 + k31 * c31); //c33

  fC[10](success) -= (k40 * c00 + k41 * c10); //c40
  fC[11](success) -= (k40 * c10 + k41 * c11); //c41
  fC[12](success) -= (k40 * c20 + k41 * c21); //c42
  fC[13](success) -= (k40 * c30 + k41 * c31); //c43
  fC[14](success) -= (k40 * c40 + k41 * c41); //c44
  
  return success;
}

#include <iostream>

std::istream &operator>>( std::istream &in, AliHLTTPCCATrackParamVector &t )
{
  Vc::array<float, float_v::Size> x, s, p[5], c[15], chi2;
  Vc::array<int, int_v::Size> ndf;
  for ( int j = 0; j < uint_v::Size; ++j ) {
    in >> x[j];
    in >> s[j];
    for ( int i = 0; i < 5; i++ ) in >> p[i][j];
    for ( int i = 0; i < 15; i++ ) in >> c[i][j];
    in >> chi2[j];
    in >> ndf[j];
  }
  t.fX.load( &x[0] );
  t.fSignCosPhi.load( &s[0] );
  for ( int i = 0; i < 5; i++ ) t.fP[i].load( &p[i][0] );
  for ( int i = 0; i < 5; i++ ) t.fC[i].load( &c[i][0] );
  t.fChi2.load( &chi2[0] );
  t.fNDF.load( &ndf[0] );
  return in;
}

std::ostream &operator<<( std::ostream &out, const AliHLTTPCCATrackParamVector &t )
{
//   if ( out == std::cerr ) 
//   {
//     out << "------------------------------ Track Param ------------------------------"
//         << "\n             X: " << t.X()
//         << "\n    SignCosPhi: " << t.SignCosPhi()
//         << "\n          Chi2: " << t.Chi2()
//         << "\n           NDF: " << t.NDF()
//         << "\n             Y: " << t.Par()[0]
//         << "\n             Z: " << t.Par()[1]
//         << "\n        SinPhi: " << t.Par()[2]
//         << "\n          DzDs: " << t.Par()[3]
//         << "\n          q/Pt: " << t.Par()[4]
//         << "\nCovariance Matrix\n";
//     int i = 0;
//     out << std::setprecision( 2 );
//     for ( int step = 1; step <= 5; ++step ) {
//       int end = i + step;
//       for ( ; i < end; ++i ) {
//         out << t.Cov()[i] << '\t';
//       }
//       out << "\n";
//     }
//     out << std::setprecision( 6 );
//     return out << std::endl;
//   }
  for ( int j = 0; j < uint_v::Size; ++j ) {
    out << t.X()[j] << " "
        << t.SignCosPhi()[j] << " "
        << t.Chi2()[j] << " "
        << t.NDF()[j]
        << std::endl;
    for ( int i = 0; i < 5; i++ ) out << t.Par()[i][j] << " ";
    out << std::endl;
    for ( int i = 0; i < 15; i++ ) out << t.Cov()[i][j] << " ";
    out << std::endl;
  }
  return out;
}
