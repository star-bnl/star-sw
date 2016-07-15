//-*- Mode: C++ -*-
// $Id: AliHLTTPCCATrackParamVector.h,v 1.2 2016/07/15 14:43:33 fisyak Exp $
// ************************************************************************
// This file is property of and copyright by the ALICE HLT Project        *
// ALICE Experiment at CERN, All rights reserved.                         *
// See cxx source for full Copyright notice                               *
//                                                                        *
//*************************************************************************


#ifndef ALIHLTTPCCATRACKPARAMVECTOR_H
#define ALIHLTTPCCATRACKPARAMVECTOR_H

#include "AliHLTTPCCADef.h"
#include "AliHLTTPCCAMath.h"
#include <cstring>

class AliHLTTPCCATrackLinearisationVector;
#if 0
namespace std
{
  template<typename T> struct char_traits;
  template<typename _CharT, typename _Traits> class basic_istream;
  typedef basic_istream<char, char_traits<char> > istream;
  template<typename _CharT, typename _Traits> class basic_ostream;
  typedef basic_ostream<char, char_traits<char> > ostream;
} // namespace std
#endif
/**
 * @class AliHLTTPCCATrackParamVector
 *
 * AliHLTTPCCATrackParamVector class describes the track parametrisation
 * which is used by the AliHLTTPCCATracker slice tracker.
 *
 */
class AliHLTTPCCATrackParamVector
{
    friend std::istream &operator>>( std::istream &, AliHLTTPCCATrackParamVector & );
    friend std::ostream &operator<<( std::ostream &, const AliHLTTPCCATrackParamVector & );
  public:
    AliHLTTPCCATrackParamVector()
      : fX( Vc::Zero ),
      fSignCosPhi( Vc::Zero ),
      fChi2( Vc::Zero ),
      fNDF( Vc::Zero )
    {
      for ( int i = 0; i <  5; ++i ) fP[i].setZero();
      for ( int i = 0; i < 15; ++i ) fC[i].setZero();
    }

    struct AliHLTTPCCATrackFitParam {
      sfloat_v fBethe;
      sfloat_v fE;
      sfloat_v fTheta2;
      sfloat_v fEP2;
      sfloat_v fSigmadE2;
      sfloat_v fK22;
      sfloat_v fK33;
      sfloat_v fK43;
      sfloat_v fK44;
    };

    sfloat_v X()      const { return fX;    }
    sfloat_v Y()      const { return fP[0]; }
    sfloat_v Z()      const { return fP[1]; }
    sfloat_v SinPhi() const { return fP[2]; }
    sfloat_v DzDs()   const { return fP[3]; }
    sfloat_v QPt()    const { return fP[4]; }

    /**
     * The sign of cos phi is always positive in the slice tracker. Only after coordinate
     * transformation can the sign change to negative.
     */
    sfloat_v SignCosPhi() const { return fSignCosPhi; }
    sfloat_v Chi2()  const { return fChi2; }
    short_v   NDF()   const { return fNDF; }

    sfloat_v Err2Y()      const { return fC[0]; }
    sfloat_v Err2Z()      const { return fC[2]; }
    sfloat_v Err2SinPhi() const { return fC[5]; }
    sfloat_v Err2DzDs()   const { return fC[9]; }
    sfloat_v Err2QPt()    const { return fC[14]; }

    sfloat_v GetX()      const { return fX; }
    sfloat_v GetY()      const { return fP[0]; }
    sfloat_v GetZ()      const { return fP[1]; }
    sfloat_v GetSinPhi() const { return fP[2]; }
    sfloat_v GetDzDs()   const { return fP[3]; }
    sfloat_v GetQPt()    const { return fP[4]; }
    sfloat_v GetSignCosPhi() const { return fSignCosPhi; }
    sfloat_v GetChi2()   const { return fChi2; }
    short_v   GetNDF()    const { return fNDF; }

    sfloat_v GetKappa( const sfloat_v &Bz ) const { return fP[4]*Bz; }
    sfloat_v GetCosPhiPositive() const { return CAMath::Sqrt( sfloat_v( Vc::One ) - SinPhi()*SinPhi() ); }
    sfloat_v GetCosPhi() const { return fSignCosPhi*CAMath::Sqrt( sfloat_v( Vc::One ) - SinPhi()*SinPhi() ); }

    sfloat_v GetErr2Y()      const { return fC[0]; }
    sfloat_v GetErr2Z()      const { return fC[2]; }
    sfloat_v GetErr2SinPhi() const { return fC[5]; }
    sfloat_v GetErr2DzDs()   const { return fC[9]; }
    sfloat_v GetErr2QPt()    const { return fC[14]; }

    const sfloat_v *Par() const { return fP; }
    const sfloat_v *Cov() const { return fC; }

    const sfloat_v *GetPar() const { return fP; }
    const sfloat_v *GetCov() const { return fC; }

    void SetTrackParam(const AliHLTTPCCATrackParamVector &param, const sfloat_m &m = sfloat_m( true )  )
         {
           for(int i=0; i<5; i++) fP[i](m) = param.Par()[i];
           for(int i=0; i<15; i++) fC[i](m) = param.Cov()[i];
           fX(m) = param.X();
           fSignCosPhi(m) = param.SignCosPhi();
           fChi2(m) = param.GetChi2();
           fNDF(static_cast<short_m>(m)) = param.GetNDF();
         }

    void SetPar( int i, const sfloat_v &v ) { fP[i] = v; }
    void SetPar( int i, const sfloat_v &v, const sfloat_m &m ) { fP[i]( m ) = v; }
    void SetCov( int i, const sfloat_v &v ) { fC[i] = v; }
    void SetCov( int i, const sfloat_v &v, const sfloat_m &m ) { fC[i]( m ) = v; }

    void SetX( const sfloat_v &v )     {  fX = v;    }
    void SetY( const sfloat_v &v )     {  fP[0] = v; }
    void SetZ( const sfloat_v &v )     {  fP[1] = v; }
    void SetX( const sfloat_v &v, const sfloat_m &m )     {  fX( m ) = v;    }
    void SetY( const sfloat_v &v, const sfloat_m &m )     {  fP[0]( m ) = v; }
    void SetZ( const sfloat_v &v, const sfloat_m &m )     {  fP[1]( m ) = v; }
    void SetSinPhi( const sfloat_v &v ) {  fP[2] = v; }
    void SetSinPhi( const sfloat_v &v, const sfloat_m &m ) {  fP[2]( m ) = v; }
    void SetDzDs( const sfloat_v &v )  {  fP[3] = v; }
    void SetDzDs( const sfloat_v &v, const sfloat_m &m )  {  fP[3]( m ) = v; }
    void SetQPt( const sfloat_v &v )   {  fP[4] = v; }
    void SetQPt( const sfloat_v &v, const sfloat_m &m ) {  fP[4]( m ) = v; }
    void SetSignCosPhi( const sfloat_v &v ) {  fSignCosPhi = v; }
    void SetSignCosPhi( const sfloat_v &v, const sfloat_m &m ) {  fSignCosPhi(m) = v; }
    void SetChi2( const sfloat_v &v )  {  fChi2 = v; }
    void SetChi2( const sfloat_v &v, const sfloat_m &m  )  {  fChi2(m) = v; }
    void SetNDF( int v )   { fNDF = v; }
    void SetNDF( const short_v &v )   { fNDF = v; }
    void SetNDF( const short_v &v, const short_m &m )   { fNDF(m) = v; }

    sfloat_v GetDist2( const AliHLTTPCCATrackParamVector &t ) const;
    sfloat_v GetDistXZ2( const AliHLTTPCCATrackParamVector &t ) const;


    sfloat_v GetS( const sfloat_v &x, const sfloat_v &y, const sfloat_v &Bz  ) const;

    void GetDCAPoint( const sfloat_v &x, const sfloat_v &y, const sfloat_v &z,
                             sfloat_v *px, sfloat_v *py, sfloat_v *pz, const sfloat_v &Bz  ) const;


    sfloat_m TransportToXWithMaterial( const sfloat_v &x, const sfloat_v &Bz, const float maxSinPhi = .999f );

    sfloat_m TransportToX( const sfloat_v &x, const sfloat_v &Bz, const float maxSinPhi = .999f, const sfloat_m &mask = sfloat_m( true ) );

    sfloat_m TransportToX( const sfloat_v &x, AliHLTTPCCATrackLinearisationVector &t0,
        const sfloat_v &Bz,  const float maxSinPhi = .999f, sfloat_v *DL = 0, const sfloat_m &mask = sfloat_m( true ) );

    sfloat_m TransportToX( const sfloat_v &x, const sfloat_v &sinPhi0,
        const sfloat_v &Bz, const sfloat_v maxSinPhi = .999f, const sfloat_m &mask = sfloat_m( true ) );

    sfloat_m  TransportToXWithMaterial( const sfloat_v &x,  AliHLTTPCCATrackLinearisationVector &t0,
        AliHLTTPCCATrackFitParam &par, const sfloat_v &Bz, const float maxSinPhi = .999f, const sfloat_m &mask = sfloat_m( true ) );

    sfloat_m  TransportToXWithMaterial( const sfloat_v &x,
        AliHLTTPCCATrackFitParam &par, const sfloat_v &Bz, const float maxSinPhi = .999f );

    sfloat_m Rotate( const sfloat_v &alpha, AliHLTTPCCATrackLinearisationVector &t0, 
                     const float maxSinPhi = .999f, const sfloat_m &mask = sfloat_m( true ) );
    sfloat_m Rotate( const sfloat_v &alpha, const float maxSinPhi = .999f, const sfloat_m &mask = sfloat_m( true ) );
    void RotateXY( sfloat_v alpha, sfloat_v &x, sfloat_v &y, sfloat_v &sin, const sfloat_m &mask = sfloat_m( true ) ) const ;

    sfloat_m FilterWithMaterial( const sfloat_v &y, const sfloat_v &z, sfloat_v err2Y, sfloat_v err2Z, 
                                 float maxSinPhi=0.999f, const sfloat_m &mask = sfloat_m( true ) );

    static sfloat_v ApproximateBetheBloch( const sfloat_v &beta2 );
    static sfloat_v BetheBlochGeant( const sfloat_v &bg,
                                           const sfloat_v &kp0 = 2.33f,
                                           const sfloat_v &kp1 = 0.20f,
                                           const sfloat_v &kp2 = 3.00f,
                                           const sfloat_v &kp3 = 173e-9f,
                                           const sfloat_v &kp4 = 0.49848f
                                         );
    static sfloat_v BetheBlochSolid( const sfloat_v &bg );
    static sfloat_v BetheBlochGas( const sfloat_v &bg );


    void CalculateFitParameters( AliHLTTPCCATrackFitParam &par, const sfloat_v &mass = 0.13957f );
    sfloat_m CorrectForMeanMaterial( const sfloat_v &xOverX0,  const sfloat_v &xTimesRho,
        const AliHLTTPCCATrackFitParam &par, const sfloat_m &_mask );

    sfloat_m FilterDelta( const sfloat_m &mask, const sfloat_v &dy, const sfloat_v &dz,
        sfloat_v err2Y, sfloat_v err2Z, const float maxSinPhi = .999f );
    sfloat_m Filter( const sfloat_m &mask, const sfloat_v &y, const sfloat_v &z,
        sfloat_v err2Y, sfloat_v err2Z, const float maxSinPhi = .999f );


  private:

    sfloat_v fX;      // x position
    sfloat_v fSignCosPhi; // sign of cosPhi
    sfloat_v fP[5];   // 'active' track parameters: Y, Z, SinPhi, DzDs, q/Pt
    sfloat_v fC[15];  // the covariance matrix for Y,Z,SinPhi,..
    sfloat_v fChi2;   // the chi^2 value
    short_v   fNDF;    // the Number of Degrees of Freedom
};

#include "debug.h"

inline sfloat_m AliHLTTPCCATrackParamVector::TransportToX( const sfloat_v &x, const sfloat_v &sinPhi0,
    const sfloat_v &Bz, const sfloat_v maxSinPhi, const sfloat_m &_mask )
{
  //* Transport the track parameters to X=x, using linearization at phi0 with 0 curvature,
  //* and the field value Bz
  //* maxSinPhi is the max. allowed value for |t0.SinPhi()|
  //* linearisation of trajectory t0 is also transported to X=x,
  //* returns 1 if OK
  //*

  debugKF() << "Start TransportToX(" << x << ", " << _mask << ")\n" << *this << std::endl;

  const sfloat_v &ey = sinPhi0;
  const sfloat_v &dx = x - X();
  const sfloat_v &exi = sfloat_v( Vc::One ) * CAMath::RSqrt( sfloat_v( Vc::One ) - ey * ey ); // RSqrt

  const sfloat_v &dxBz = dx * Bz;
  const sfloat_v &dS = dx * exi;
  const sfloat_v &h2 = dS * exi * exi;
  const sfloat_v &h4 = .5f * h2 * dxBz;
//#define LOSE_DEBUG
#ifdef LOSE_DEBUG
  std::cout << " TrTo-sinPhi0 = " << sinPhi0 << std::endl;
#endif
///mvz start 23.01.2010
//  const sfloat_v &sinPhi = SinPhi() * (sfloat_v( Vc::One ) - 0.5f * dxBz * QPt() *dxBz * QPt()/ ( sfloat_v( Vc::One ) - SinPhi()*SinPhi() )) + dxBz * QPt();
  const sfloat_v &sinPhi = SinPhi() + dxBz * QPt();
///mvz end 23.01.2010
#ifdef LOSE_DEBUG
  std::cout << " TrTo-sinPhi = " << sinPhi << std::endl;
#endif
  sfloat_m mask = _mask && CAMath::Abs( exi ) <= 1.e4f;
  mask &= ( (CAMath::Abs( sinPhi ) <= maxSinPhi) || (maxSinPhi <= 0.f) );


  fX   ( mask ) += dx;
  fP[0]( mask ) += dS * ey + h2 * ( SinPhi() - ey )  +   h4 * QPt();
  fP[1]( mask ) += dS * DzDs();
  fP[2]( mask ) = sinPhi;


  //const sfloat_v c00 = fC[0];
  //const sfloat_v c11 = fC[2];
  const sfloat_v c20 = fC[3];
  //const sfloat_v c21 = fC[4];
  const sfloat_v c22 = fC[5];
  //const sfloat_v c30 = fC[6];
  const sfloat_v c31 = fC[7];
  //const sfloat_v c32 = fC[8];
  const sfloat_v c33 = fC[9];
  const sfloat_v c40 = fC[10];
  //const sfloat_v c41 = fC[11];
  const sfloat_v c42 = fC[12];
  //const sfloat_v c43 = fC[13];
  const sfloat_v c44 = fC[14];

  const sfloat_v two( 2.f );

  fC[0] ( mask ) += h2 * h2 * c22 + h4 * h4 * c44
                     + two * ( h2 * c20 + h4 * ( c40 + h2 * c42 ) );

  //fC[1] ( mask ) += h2 * c21 + h4 * c41 + dS * ( c30 + h2 * c32 + h4 * c43 );
  fC[2] ( mask ) += dS * ( two * c31 + dS * c33 );

  fC[3] ( mask ) += h2 * c22 + h4 * c42 + dxBz * ( c40 + h2 * c42 + h4 * c44 );
  //fC[4] ( mask ) += dS * c32 + dxBz * ( c41 + dS * c43 );
  const sfloat_v &dxBz_c44 = dxBz * c44;
  fC[12]( mask ) += dxBz_c44;
  fC[5] ( mask ) += dxBz * ( two * c42 + dxBz_c44 );

  //fC[6] ( mask ) += h2 * c32 + h4 * c43;
  fC[7] ( mask ) += dS * c33;
  //fC[8] ( mask ) += dxBz * c43;
  //fC[9] ( mask ) = c33;

  fC[10]( mask ) += h2 * c42 + h4 * c44;
  //fC[11]( mask ) += dS * c43;
  //fC[13]( mask ) = c43;
  //fC[14]( mask ) = c44;

  debugKF() << mask << "\n" << *this << std::endl;
  return mask;
}

#include <assert.h>

inline sfloat_m AliHLTTPCCATrackParamVector::FilterDelta( const sfloat_m &mask, const sfloat_v &dy, const sfloat_v &dz,
    sfloat_v err2Y, sfloat_v err2Z, const float maxSinPhi )
{
  debugKF() << "Kalman filter( " << mask
    << "\n  " << dy
    << "\n  " << dz
    << "\n  " << err2Y
    << "\n  " << err2Z
    << "\n):" << std::endl;

  assert( err2Y > 0.f || !mask );
  assert( err2Z > 0.f || !mask );

  //* Add the y,z measurement with the Kalman filter

  const sfloat_v c00 = fC[ 0];
  const sfloat_v c11 = fC[ 2];
  const sfloat_v c20 = fC[ 3];
  const sfloat_v c31 = fC[ 7];
  const sfloat_v c40 = fC[10];

  err2Y += c00;
  err2Z += c11;
#ifndef NODEBUG
  if ( !( err2Y > 0.f || !mask ).isFull() ) {
    std::cerr << err2Y << mask << ( err2Y > 0.f || !mask ) << c00 << std::endl;
  }
#endif
  assert( err2Y > 0.f || !mask );
  assert( err2Z > 0.f || !mask );

  const sfloat_v &z0 = dy;
  const sfloat_v &z1 = dz;

  const sfloat_v &mS0 = sfloat_v( Vc::One ) / err2Y;
  const sfloat_v &mS2 = sfloat_v( Vc::One ) / err2Z;
  //const sfloat_v &mS0 = CAMath::Reciprocal( err2Y );
  //const sfloat_v &mS2 = CAMath::Reciprocal( err2Z );
  debugKF() << "delta(mS0): " << CAMath::Abs( sfloat_v( Vc::One ) / err2Y - mS0 ) << std::endl;
  debugKF() << "delta(mS2): " << CAMath::Abs( sfloat_v( Vc::One ) / err2Z - mS2 ) << std::endl;
  assert( mS0 > 0.f || !mask );
  assert( mS2 > 0.f || !mask );

  // K = CHtS

  const sfloat_v &k00 = c00 * mS0;
  const sfloat_v &k20 = c20 * mS0;
  const sfloat_v &k40 = c40 * mS0;

  const sfloat_v &k11 = c11 * mS2;
  const sfloat_v &k31 = c31 * mS2;

  debugKF() << "delta(k00): " << ( c00 / err2Y - k00 ) << std::endl;
  debugKF() << "delta(k20): " << ( c20 / err2Y - k20 ) << std::endl;
  debugKF() << "delta(k40): " << ( c40 / err2Y - k40 ) << std::endl;

  debugKF() << "delta(k11): " << ( c11 / err2Z - k11 ) << std::endl;
  debugKF() << "delta(k31): " << ( c31 / err2Z - k31 ) << std::endl;

  const sfloat_v &sinPhi = fP[2] + k20 * z0  ;
  debugKF() << "delta(sinPhi): " << ( z0 * c20 / err2Y + fP[2] - sinPhi ) << std::endl;

  assert( maxSinPhi > 0.f );
  const sfloat_m &success = mask && err2Y >= 1.e-8f && err2Z >= 1.e-8f && CAMath::Abs( sinPhi ) < maxSinPhi;

  fNDF  ( static_cast<short_m>( success ) ) += 2;
  fChi2 ( success ) += mS0 * z0 * z0 + mS2 * z1 * z1 ;

  fP[ 0]( success ) += k00 * z0 ;
  fP[ 1]( success ) += k11 * z1 ;
  fP[ 2]( success ) = sinPhi ;
  fP[ 3]( success ) += k31 * z1 ;
  fP[ 4]( success ) += k40 * z0 ;

  fC[ 0]( success ) -= k00 * c00 ;
  fC[ 3]( success ) -= k20 * c00 ;
  fC[ 5]( success ) -= k20 * c20 ;
  fC[10]( success ) -= k40 * c00 ;
  fC[12]( success ) -= k40 * c20 ;
  fC[14]( success ) -= k40 * c40 ;

  fC[ 2]( success ) -= k11 * c11 ;
  fC[ 7]( success ) -= k31 * c11 ;
  fC[ 9]( success ) -= k31 * c31 ;
#if 1
  const sfloat_m check = ( fC[ 0] >= 0.f ) && ( fC[ 2] >= 0.f ) && ( fC[ 5] >= 0.f ) && ( fC[ 9] >= 0.f ) && ( fC[14] >= 0.f );
#else
  assert( fC[ 0] >= 0.f );
  assert( fC[ 2] >= 0.f );
  assert( fC[ 5] >= 0.f );
  assert( fC[ 9] >= 0.f );
  assert( fC[14] >= 0.f );
#endif
  return success && check;
}

inline sfloat_m AliHLTTPCCATrackParamVector::Filter( const sfloat_m &mask, const sfloat_v &y, const sfloat_v &z,
    sfloat_v err2Y, sfloat_v err2Z, const float maxSinPhi )
{
  debugKF() << "Kalman filter( " << mask
    << "\n  " << y
    << "\n  " << z
    << "\n  " << err2Y
    << "\n  " << err2Z
    << "\n):" << std::endl;

  assert( err2Y > 0.f || !mask );
  assert( err2Z > 0.f || !mask );

  //* Add the y,z measurement with the Kalman filter

  const sfloat_v c00 = fC[ 0];
  const sfloat_v c11 = fC[ 2];
  const sfloat_v c20 = fC[ 3];
  const sfloat_v c31 = fC[ 7];
  const sfloat_v c40 = fC[10];

  err2Y += c00;
  err2Z += c11;
#ifndef NODEBUG
  if ( !( err2Y > 0.f || !mask ).isFull() ) {
    std::cerr << err2Y << mask << ( err2Y > 0.f || !mask ) << c00 << std::endl;
  }
#endif
  assert( err2Y > 0.f || !mask );
  assert( err2Z > 0.f || !mask );

  const sfloat_v &z0 = y - fP[0];
  const sfloat_v &z1 = z - fP[1];

  const sfloat_v &mS0 = sfloat_v( Vc::One ) / err2Y;
  const sfloat_v &mS2 = sfloat_v( Vc::One ) / err2Z;
  //const sfloat_v &mS0 = CAMath::Reciprocal( err2Y );
  //const sfloat_v &mS2 = CAMath::Reciprocal( err2Z );
  debugKF() << "delta(mS0): " << CAMath::Abs( sfloat_v( Vc::One ) / err2Y - mS0 ) << std::endl;
  debugKF() << "delta(mS2): " << CAMath::Abs( sfloat_v( Vc::One ) / err2Z - mS2 ) << std::endl;
  assert( mS0 > 0.f || !mask );
  assert( mS2 > 0.f || !mask );

  // K = CHtS

  const sfloat_v &k00 = c00 * mS0;
  const sfloat_v &k20 = c20 * mS0;
  const sfloat_v &k40 = c40 * mS0;

  const sfloat_v &k11 = c11 * mS2;
  const sfloat_v &k31 = c31 * mS2;

  debugKF() << "delta(k00): " << ( c00 / err2Y - k00 ) << std::endl;
  debugKF() << "delta(k20): " << ( c20 / err2Y - k20 ) << std::endl;
  debugKF() << "delta(k40): " << ( c40 / err2Y - k40 ) << std::endl;

  debugKF() << "delta(k11): " << ( c11 / err2Z - k11 ) << std::endl;
  debugKF() << "delta(k31): " << ( c31 / err2Z - k31 ) << std::endl;

  const sfloat_v &sinPhi = fP[2] + k20 * z0  ;
  debugKF() << "delta(sinPhi): " << ( z0 * c20 / err2Y + fP[2] - sinPhi ) << std::endl;

  assert( maxSinPhi > 0.f );
  const sfloat_m &success = mask && err2Y >= 1.e-8f && err2Z >= 1.e-8f && CAMath::Abs( sinPhi ) < maxSinPhi;

  fNDF  ( static_cast<short_m>( success ) ) += 2;
  fChi2 ( success ) += mS0 * z0 * z0 + mS2 * z1 * z1 ;

  fP[ 0]( success ) += k00 * z0 ;
  fP[ 1]( success ) += k11 * z1 ;
  fP[ 2]( success ) = sinPhi ;
  fP[ 3]( success ) += k31 * z1 ;
  fP[ 4]( success ) += k40 * z0 ;

  fC[ 0]( success ) -= k00 * c00 ;
  fC[ 3]( success ) -= k20 * c00 ;
  fC[ 5]( success ) -= k20 * c20 ;
  fC[10]( success ) -= k40 * c00 ;
  fC[12]( success ) -= k40 * c20 ;
  fC[14]( success ) -= k40 * c40 ;

  fC[ 2]( success ) -= k11 * c11 ;
  fC[ 7]( success ) -= k31 * c11 ;
  fC[ 9]( success ) -= k31 * c31 ;
#if 1
  const sfloat_m check = ( fC[ 0] >= 0.f ) && ( fC[ 2] >= 0.f ) && ( fC[ 5] >= 0.f ) && ( fC[ 9] >= 0.f ) && ( fC[14] >= 0.f );
#else
  assert( fC[ 0] >= 0.f );
  assert( fC[ 2] >= 0.f );
  assert( fC[ 5] >= 0.f );
  assert( fC[ 9] >= 0.f );
  assert( fC[14] >= 0.f );
#endif
  return success && check;
}

inline sfloat_m AliHLTTPCCATrackParamVector::Rotate( const sfloat_v &alpha, const float maxSinPhi, const sfloat_m &mask )
{
  //* Rotate the coordinate system in XY on the angle alpha

  const sfloat_v cA = CAMath::Cos( alpha );
  const sfloat_v sA = CAMath::Sin( alpha );
  const sfloat_v x = X(), y = Y(), sP = SinPhi(), cP = GetCosPhi();
  const sfloat_v cosPhi = cP * cA + sP * sA;
  const sfloat_v sinPhi = -cP * sA + sP * cA;

  sfloat_m mReturn = mask && (CAMath::Abs( sinPhi ) < maxSinPhi) && (CAMath::Abs( cosPhi ) > 1.e-2f) && (CAMath::Abs( cP ) > 1.e-2f);

  const sfloat_v j0 = cP / cosPhi;
  const sfloat_v j2 = cosPhi / cP;

  SetX( x*cA +  y*sA, mReturn);
  SetY( -x*sA +  y*cA, mReturn );
  SetSignCosPhi( CAMath::Abs(cosPhi)/cosPhi, mReturn );
  SetSinPhi( sinPhi, mReturn );


  //float J[5][5] = { { j0, 0, 0,  0,  0 }, // Y
  //                      {  0, 1, 0,  0,  0 }, // Z
  //                      {  0, 0, j2, 0,  0 }, // SinPhi
  //                    {  0, 0, 0,  1,  0 }, // DzDs
  //                    {  0, 0, 0,  0,  1 } }; // Kappa
  //cout<<"alpha="<<alpha<<" "<<x<<" "<<y<<" "<<sP<<" "<<cP<<" "<<j0<<" "<<j2<<endl;
  //cout<<"      "<<fC[0]<<" "<<fC[1]<<" "<<fC[6]<<" "<<fC[10]<<" "<<fC[4]<<" "<<fC[5]<<" "<<fC[8]<<" "<<fC[12]<<endl;
  fC[0](mReturn) *= j0 * j0;
  fC[1](mReturn) *= j0;
  fC[3](mReturn) *= j0;
  fC[6](mReturn) *= j0;
  fC[10](mReturn) *= j0;

  fC[3](mReturn) *= j2;
  fC[4](mReturn) *= j2;
  fC[5](mReturn) *= j2 * j2;
  fC[8](mReturn) *= j2;
  fC[12](mReturn) *= j2;
  //cout<<"      "<<fC[0]<<" "<<fC[1]<<" "<<fC[6]<<" "<<fC[10]<<" "<<fC[4]<<" "<<fC[5]<<" "<<fC[8]<<" "<<fC[12]<<endl;
  return mReturn;
}

inline void AliHLTTPCCATrackParamVector::RotateXY( sfloat_v alpha, sfloat_v &x, sfloat_v &y, sfloat_v &sin, const sfloat_m &mask ) const
{
  //* Rotate the coordinate system in XY on the angle alpha
  const sfloat_v cA = CAMath::Cos( alpha );
  const sfloat_v sA = CAMath::Sin( alpha );

  x(mask) = ( X()*cA +  Y()*sA );
  y(mask) = ( -X()*sA +  Y()*cA );
  sin(mask) = -GetCosPhi() * sA + SinPhi() * cA;
}

typedef AliHLTTPCCATrackParamVector TrackParamVector;

std::istream &operator>>( std::istream &in, AliHLTTPCCATrackParamVector &ot );
std::ostream &operator<<( std::ostream &out, const AliHLTTPCCATrackParamVector &ot );

#endif
