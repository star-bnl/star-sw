//-*- Mode: C++ -*-
// $Id: AliHLTTPCCATrackParamVector.h,v 1.5 2013/08/06 18:59:38 fisyak Exp $
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
class AliHLTTPCCATrackParam;

namespace std
{
  template<typename T> struct char_traits;
  template<typename _CharT, typename _Traits> class basic_istream;
  typedef basic_istream<char, char_traits<char> > istream;
  template<typename _CharT, typename _Traits> class basic_ostream;
  typedef basic_ostream<char, char_traits<char> > ostream;
} // namespace std

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
  
    AliHLTTPCCATrackParamVector(const AliHLTTPCCATrackParam &param);

  void InitCovMatrixAndChi2AndNDF( const float_m &mask = float_m( true ) ) {
    SetCov( 0, 10.f, mask );
    SetCov( 1,  0.f, mask );
    SetCov( 2, 10.f, mask );
    SetCov( 3,  0.f, mask );
    SetCov( 4,  0.f, mask );
    SetCov( 5,  1.f, mask );
    SetCov( 6,  0.f, mask );
    SetCov( 7,  0.f, mask );
    SetCov( 8,  0.f, mask );
    SetCov( 9,  1.f, mask );
    SetCov( 10,  0.f, mask );
    SetCov( 11,  0.f, mask );
    SetCov( 12,  0.f, mask );
    SetCov( 13,  0.f, mask );
    SetCov( 14,  10.f, mask );
    SetChi2( 0.f, mask);
    SetNDF( int_v(-5), static_cast<int_m>(mask) );
  }

    void SetTrackParam(const AliHLTTPCCATrackParamVector &param, const float_m &m = float_m( true )  ) {
      for(int i=0; i<5; i++) fP[i](m) = param.Par()[i];
      for(int i=0; i<15; i++) fC[i](m) = param.Cov()[i];
      fX(m) = param.X();
      fSignCosPhi(m) = param.SignCosPhi();
      fChi2(m) = param.GetChi2();
      fNDF(static_cast<int_m>(m)) = param.GetNDF();
    }
  
    struct AliHLTTPCCATrackFitParam {
      float_v fBethe;
      float_v fE;
      float_v fTheta2;
      float_v fEP2;
      float_v fSigmadE2;
      float_v fK22;
      float_v fK33;
      float_v fK43;
      float_v fK44;
    };

    float_v X()      const { return fX;    }
    float_v Y()      const { return fP[0]; }
    float_v Z()      const { return fP[1]; }
    float_v SinPhi() const { return fP[2]; }
    float_v DzDs()   const { return fP[3]; }
    float_v QPt()    const { return fP[4]; }

    /**
     * The sign of cos phi is always positive in the slice tracker. Only after coordinate
     * transformation can the sign change to negative.
     */
    float_v SignCosPhi() const { return fSignCosPhi; }
    float_v Chi2()  const { return fChi2; }
    int_v   NDF()   const { return fNDF; }

    float_v Err2Y()      const { return fC[0]; }
    float_v Err2Z()      const { return fC[2]; }
    float_v Err2SinPhi() const { return fC[5]; }
    float_v Err2DzDs()   const { return fC[9]; }
    float_v Err2QPt()    const { return fC[14]; }

    float_v GetX()      const { return fX; }
    float_v GetY()      const { return fP[0]; }
    float_v GetZ()      const { return fP[1]; }
    float_v GetSinPhi() const { return fP[2]; }
    float_v GetDzDs()   const { return fP[3]; }
    float_v GetQPt()    const { return fP[4]; }
    float_v GetSignCosPhi() const { return fSignCosPhi; }
    float_v GetChi2()   const { return fChi2; }
    int_v   GetNDF()    const { return fNDF; }

    float_v GetKappa( const float_v &Bz ) const { return fP[4]*Bz; }
    float_v GetCosPhiPositive() const { return CAMath::Sqrt( float_v( Vc::One ) - SinPhi()*SinPhi() ); }
    float_v GetCosPhi() const { return fSignCosPhi*CAMath::Sqrt( float_v( Vc::One ) - SinPhi()*SinPhi() ); }

    float_v GetErr2Y()      const { return fC[0]; }
    float_v GetErr2Z()      const { return fC[2]; }
    float_v GetErr2SinPhi() const { return fC[5]; }
    float_v GetErr2DzDs()   const { return fC[9]; }
    float_v GetErr2QPt()    const { return fC[14]; }

    const float_v *Par() const { return fP; }
    const float_v *Cov() const { return fC; }

    const float_v *GetPar() const { return fP; }
    const float_v *GetCov() const { return fC; }


    void SetPar( int i, const float_v &v ) { fP[i] = v; }
    void SetPar( int i, const float_v &v, const float_m &m ) { fP[i]( m ) = v; }
    void SetCov( int i, const float_v &v ) { fC[i] = v; }
    void SetCov( int i, const float_v &v, const float_m &m ) { fC[i]( m ) = v; }

    void SetX( const float_v &v )     {  fX = v;    }
    void SetY( const float_v &v )     {  fP[0] = v; }
    void SetZ( const float_v &v )     {  fP[1] = v; }
    void SetX( const float_v &v, const float_m &m )     {  fX( m ) = v;    }
    void SetY( const float_v &v, const float_m &m )     {  fP[0]( m ) = v; }
    void SetZ( const float_v &v, const float_m &m )     {  fP[1]( m ) = v; }
    void SetSinPhi( const float_v &v ) {  fP[2] = v; }
    void SetSinPhi( const float_v &v, const float_m &m ) {  fP[2]( m ) = v; }
    void SetDzDs( const float_v &v )  {  fP[3] = v; }
    void SetDzDs( const float_v &v, const float_m &m )  {  fP[3]( m ) = v; }
    void SetQPt( const float_v &v )   {  fP[4] = v; }
    void SetQPt( const float_v &v, const float_m &m ) {  fP[4]( m ) = v; }
    void SetSignCosPhi( const float_v &v ) {  fSignCosPhi = v; }
    void SetSignCosPhi( const float_v &v, const float_m &m ) {  fSignCosPhi(m) = v; }
    void SetChi2( const float_v &v )  {  fChi2 = v; }
    void SetChi2( const float_v &v, const float_m &m  )  {  fChi2(m) = v; }
    void SetNDF( int v )   { fNDF = v; }
    void SetNDF( const int_v &v )   { fNDF = v; }
    void SetNDF( const int_v &v, const int_m &m )   { fNDF(m) = v; }

    float_v GetDist2( const AliHLTTPCCATrackParamVector &t ) const;
    float_v GetDistXZ2( const AliHLTTPCCATrackParamVector &t ) const;

  void NormilizeSignCosPhi( const AliHLTTPCCATrackLinearisationVector& l, const float_m &m = float_m(true) );

  float_m IsNotDiverged() const {
      // if track has infinite partameters or covariances, or negative diagonal elements of Cov. matrix, mark it as fitted uncorrectly
    float_m ok(true);
    for ( unsigned char i = 0; i < 15; i++ ) ok &= CAMath::Finite( fC[i] );
    for ( unsigned char i = 0; i <  5; i++ ) ok &= CAMath::Finite( fP[i] );
    ok &= (fC[0] > float_v(Vc::Zero)) && (fC[2] > float_v(Vc::Zero)) && (fC[5] > float_v(Vc::Zero)) && (fC[9] > float_v(Vc::Zero)) && (fC[14] > float_v(Vc::Zero));
    ok &= CAMath::Abs( SinPhi() ) < .999f;
    return ok;
  }

    float_v GetS( const float_v &x, const float_v &y, const float_v &Bz  ) const;

    void GetDCAPoint( const float_v &x, const float_v &y, const float_v &z,
                             float_v *px, float_v *py, float_v *pz, const float_v &Bz  ) const;


    float_m TransportToXWithMaterial( const float_v &x, const float_v &Bz, const float maxSinPhi = .999f );

    float_m TransportToX( const float_v &x, const float_v &Bz, const float maxSinPhi = .999f, const float_m &mask = float_m( true ) );

    float_m TransportToX( const float_v &x, AliHLTTPCCATrackLinearisationVector &t0,
        const float_v &Bz,  const float maxSinPhi = .999f, float_v *DL = 0, const float_m &mask = float_m( true ) );

    float_m TransportToX( const float_v &x, const float_v &sinPhi0,
        const float_v &Bz, const float_v maxSinPhi = .999f, const float_m &mask = float_m( true ) );

    float_m  TransportToXWithMaterial( const float_v &x,  AliHLTTPCCATrackLinearisationVector &t0,
        AliHLTTPCCATrackFitParam &par, const float_v &Bz, const float maxSinPhi = .999f, const float_m &mask = float_m( true ) );

    float_m  TransportToXWithMaterial( const float_v &x,
        AliHLTTPCCATrackFitParam &par, const float_v &Bz, const float maxSinPhi = .999f );

    float_m Rotate( const float_v &alpha, AliHLTTPCCATrackLinearisationVector &t0, 
                     const float maxSinPhi = .999f, const float_m &mask = float_m( true ) );
    float_m Rotate( const float_v &alpha, const float maxSinPhi = .999f, const float_m &mask = float_m( true ) );
    void RotateXY( float_v alpha, float_v &x, float_v &y, float_v &sin, const float_m &mask = float_m( true ) ) const ;

    float_m FilterWithMaterial( const float_v &y, const float_v &z, float_v err2Y, float_v err2Z, 
                                 float maxSinPhi=0.999f, const float_m &mask = float_m( true ) );

    static float_v ApproximateBetheBloch( const float_v &beta2 );
    static float_v BetheBlochGeant( const float_v &bg,
                                           const float_v &kp0 = 2.33f,
                                           const float_v &kp1 = 0.20f,
                                           const float_v &kp2 = 3.00f,
                                           const float_v &kp3 = 173e-9f,
                                           const float_v &kp4 = 0.49848f
                                         );
    static float_v BetheBlochSolid( const float_v &bg );
    static float_v BetheBlochGas( const float_v &bg );


    void CalculateFitParameters( AliHLTTPCCATrackFitParam &par, const float_v &mass = 0.13957f );
    float_m CorrectForMeanMaterial( const float_v &xOverX0,  const float_v &xTimesRho,
        const AliHLTTPCCATrackFitParam &par, const float_m &_mask );

    float_m FilterDelta( const float_m &mask, const float_v &dy, const float_v &dz,
        float_v err2Y, float_v err2Z, const float maxSinPhi = .999f );
    float_m Filter( const float_m &mask, const float_v &y, const float_v &z,
        float_v err2Y, float_v err2Z, const float maxSinPhi = .999f );


  private:

    float_v fX;      // x position
    float_v fSignCosPhi; // sign of cosPhi
    float_v fP[5];   // 'active' track parameters: Y, Z, SinPhi, DzDs, q/Pt
    float_v fC[15];  // the covariance matrix for Y,Z,SinPhi,..
    float_v fChi2;   // the chi^2 value
    int_v   fNDF;    // the Number of Degrees of Freedom
};

#include "debug.h"

inline float_m AliHLTTPCCATrackParamVector::TransportToX( const float_v &x, const float_v &sinPhi0,
    const float_v &Bz, const float_v maxSinPhi, const float_m &_mask )
{
  //* Transport the track parameters to X=x, using linearization at phi0 with 0 curvature,
  //* and the field value Bz
  //* maxSinPhi is the max. allowed value for |t0.SinPhi()|
  //* linearisation of trajectory t0 is also transported to X=x,
  //* returns 1 if OK
  //*

  debugKF() << "Start TransportToX(" << x << ", " << _mask << ")\n" << *this << std::endl;

  const float_v &ey = sinPhi0;
  const float_v &dx = x - X();
  const float_v &exi = float_v( Vc::One ) * CAMath::RSqrt( float_v( Vc::One ) - ey * ey ); // RSqrt

  const float_v &dxBz = dx * Bz;
  const float_v &dS = dx * exi;
  const float_v &h2 = dS * exi * exi;
  const float_v &h4 = .5f * h2 * dxBz;
//#define LOSE_DEBUG
#ifdef LOSE_DEBUG
  std::cout << " TrTo-sinPhi0 = " << sinPhi0 << std::endl;
#endif
///mvz start 23.01.2010
//  const float_v &sinPhi = SinPhi() * (float_v( Vc::One ) - 0.5f * dxBz * QPt() *dxBz * QPt()/ ( float_v( Vc::One ) - SinPhi()*SinPhi() )) + dxBz * QPt();
  const float_v &sinPhi = SinPhi() + dxBz * QPt();
///mvz end 23.01.2010
#ifdef LOSE_DEBUG
  std::cout << " TrTo-sinPhi = " << sinPhi << std::endl;
#endif
  float_m mask = _mask && CAMath::Abs( exi ) <= 1.e4f;
  mask &= ( (CAMath::Abs( sinPhi ) <= maxSinPhi) || (maxSinPhi <= 0.f) );


  fX   ( mask ) += dx;
  fP[0]( mask ) += dS * ey + h2 * ( SinPhi() - ey )  +   h4 * QPt();
  fP[1]( mask ) += dS * DzDs();
  fP[2]( mask ) = sinPhi;


  //const float_v c00 = fC[0];
  //const float_v c11 = fC[2];
  const float_v c20 = fC[3];
  //const float_v c21 = fC[4];
  const float_v c22 = fC[5];
  //const float_v c30 = fC[6];
  const float_v c31 = fC[7];
  //const float_v c32 = fC[8];
  const float_v c33 = fC[9];
  const float_v c40 = fC[10];
  //const float_v c41 = fC[11];
  const float_v c42 = fC[12];
  //const float_v c43 = fC[13];
  const float_v c44 = fC[14];

  const float_v two( 2.f );

  fC[0] ( mask ) += h2 * h2 * c22 + h4 * h4 * c44
                     + two * ( h2 * c20 + h4 * ( c40 + h2 * c42 ) );

  //fC[1] ( mask ) += h2 * c21 + h4 * c41 + dS * ( c30 + h2 * c32 + h4 * c43 );
  fC[2] ( mask ) += dS * ( two * c31 + dS * c33 );

  fC[3] ( mask ) += h2 * c22 + h4 * c42 + dxBz * ( c40 + h2 * c42 + h4 * c44 );
  //fC[4] ( mask ) += dS * c32 + dxBz * ( c41 + dS * c43 );
  const float_v &dxBz_c44 = dxBz * c44;
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

inline float_m AliHLTTPCCATrackParamVector::FilterDelta( const float_m &mask, const float_v &dy, const float_v &dz,
    float_v err2Y, float_v err2Z, const float maxSinPhi )
{
  debugKF() << "Kalman filter( " << mask
    << "\n  " << dy
    << "\n  " << dz
    << "\n  " << err2Y
    << "\n  " << err2Z
    << "\n):" << std::endl;
  assert( (err2Y > 0.f || !mask).isFull() );
  assert( (err2Z > 0.f || !mask).isFull() );
  VALGRIND_CHECK_VALUE_IS_DEFINED( mask );
  VALGRIND_CHECK_MASKED_VECTOR_IS_DEFINED( dy, mask );
  VALGRIND_CHECK_MASKED_VECTOR_IS_DEFINED( dz, mask );
  VALGRIND_CHECK_MASKED_VECTOR_IS_DEFINED( err2Y, mask );
  VALGRIND_CHECK_MASKED_VECTOR_IS_DEFINED( err2Z, mask );
#ifndef NVALGRIND
  err2Y.setZero( !mask );
  err2Z.setZero( !mask );
#endif
  VALGRIND_CHECK_VALUE_IS_DEFINED( maxSinPhi );
  //* Add the y,z measurement with the Kalman filter

  const float_v c00 = fC[ 0];
  const float_v c11 = fC[ 2];
  const float_v c20 = fC[ 3];
  const float_v c31 = fC[ 7];
  const float_v c40 = fC[10];

  VALGRIND_CHECK_VALUE_IS_DEFINED( c00 );
  VALGRIND_CHECK_VALUE_IS_DEFINED( c11 );
  VALGRIND_CHECK_VALUE_IS_DEFINED( c20 );
  VALGRIND_CHECK_VALUE_IS_DEFINED( c40 );
  VALGRIND_CHECK_VALUE_IS_DEFINED( c31 );

  err2Y += c00;
  err2Z += c11;
#ifndef NODEBUG
  if ( !( err2Y > 0.f || !mask ).isFull() ) {
    std::cerr << err2Y << mask << ( err2Y > 0.f || !mask ) << c00 << std::endl;
  }
#endif
#ifdef __ASSERT_YF__
  assert( err2Y > 0.f || !mask );
  assert( err2Z > 0.f || !mask );

  VALGRIND_CHECK_VALUE_IS_DEFINED( fP[0] );
  VALGRIND_CHECK_VALUE_IS_DEFINED( fP[1] );
  VALGRIND_CHECK_VALUE_IS_DEFINED( fP[2] );
#endif
  const float_v &z0 = dy;
  const float_v &z1 = dz;

  const float_v &mS0 = float_v( Vc::One ) / err2Y;
  const float_v &mS2 = float_v( Vc::One ) / err2Z;
  //const float_v &mS0 = CAMath::Reciprocal( err2Y );
  //const float_v &mS2 = CAMath::Reciprocal( err2Z );
  debugKF() << "delta(mS0): " << CAMath::Abs( float_v( Vc::One ) / err2Y - mS0 ) << std::endl;
  debugKF() << "delta(mS2): " << CAMath::Abs( float_v( Vc::One ) / err2Z - mS2 ) << std::endl;
#ifdef __ASSERT_YF__
  assert( mS0 > 0.f || !mask );
  assert( mS2 > 0.f || !mask );
#endif
  // K = CHtS

  const float_v &k00 = c00 * mS0;
  const float_v &k20 = c20 * mS0;
  const float_v &k40 = c40 * mS0;

  const float_v &k11 = c11 * mS2;
  const float_v &k31 = c31 * mS2;

  debugKF() << "delta(k00): " << ( c00 / err2Y - k00 ) << std::endl;
  debugKF() << "delta(k20): " << ( c20 / err2Y - k20 ) << std::endl;
  debugKF() << "delta(k40): " << ( c40 / err2Y - k40 ) << std::endl;

  debugKF() << "delta(k11): " << ( c11 / err2Z - k11 ) << std::endl;
  debugKF() << "delta(k31): " << ( c31 / err2Z - k31 ) << std::endl;

  const float_v &sinPhi = fP[2] + k20 * z0  ;
  debugKF() << "delta(sinPhi): " << ( z0 * c20 / err2Y + fP[2] - sinPhi ) << std::endl;

  assert( float_m(maxSinPhi > 0.f).isFull() );
  VALGRIND_CHECK_MASKED_VECTOR_IS_DEFINED( sinPhi, mask );
  const float_m &success = mask && err2Y >= 1.e-8f && err2Z >= 1.e-8f && CAMath::Abs( sinPhi ) < maxSinPhi;
  VALGRIND_CHECK_VALUE_IS_DEFINED( success );

  fNDF  ( static_cast<int_m>( success ) ) += 2;
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
  const float_m check = ( fC[ 0] >= 0.f ) && ( fC[ 2] >= 0.f ) && ( fC[ 5] >= 0.f ) && ( fC[ 9] >= 0.f ) && ( fC[14] >= 0.f );
#else
  assert( fC[ 0] >= 0.f );
  assert( fC[ 2] >= 0.f );
  assert( fC[ 5] >= 0.f );
  assert( fC[ 9] >= 0.f );
  assert( fC[14] >= 0.f );
#endif
  return success && check;
}

inline float_m AliHLTTPCCATrackParamVector::Filter( const float_m &mask, const float_v &y, const float_v &z,
    float_v err2Y, float_v err2Z, const float maxSinPhi )
{
  debugKF() << "Kalman filter( " << mask
    << "\n  " << y
    << "\n  " << z
    << "\n  " << err2Y
    << "\n  " << err2Z
    << "\n):" << std::endl;
  assert( (err2Y > 0.f || !mask).isFull() );
  assert( (err2Z > 0.f || !mask).isFull() );
  VALGRIND_CHECK_VALUE_IS_DEFINED( mask );
  VALGRIND_CHECK_MASKED_VECTOR_IS_DEFINED( y, mask );
  VALGRIND_CHECK_MASKED_VECTOR_IS_DEFINED( z, mask );
  VALGRIND_CHECK_MASKED_VECTOR_IS_DEFINED( err2Y, mask );
  VALGRIND_CHECK_MASKED_VECTOR_IS_DEFINED( err2Z, mask );
#ifndef NVALGRIND
  err2Y.setZero( !mask );
  err2Z.setZero( !mask );
#endif
  VALGRIND_CHECK_VALUE_IS_DEFINED( maxSinPhi );
  //* Add the y,z measurement with the Kalman filter

  const float_v c00 = fC[ 0];
  const float_v c11 = fC[ 2];
  const float_v c20 = fC[ 3];
  const float_v c31 = fC[ 7];
  const float_v c40 = fC[10];

  VALGRIND_CHECK_VALUE_IS_DEFINED( c00 );
  VALGRIND_CHECK_VALUE_IS_DEFINED( c11 );
  VALGRIND_CHECK_VALUE_IS_DEFINED( c20 );
  VALGRIND_CHECK_VALUE_IS_DEFINED( c40 );
  VALGRIND_CHECK_VALUE_IS_DEFINED( c31 );

  err2Y += c00;
  err2Z += c11;
#ifndef NODEBUG
  if ( !( err2Y > 0.f || !mask ).isFull() ) {
    std::cerr << err2Y << mask << ( err2Y > 0.f || !mask ) << c00 << std::endl;
  }
#endif
#ifdef __ASSERT_YF__
  assert( err2Y > 0.f || !mask );
  assert( err2Z > 0.f || !mask );
#endif
  VALGRIND_CHECK_VALUE_IS_DEFINED( fP[0] );
  VALGRIND_CHECK_VALUE_IS_DEFINED( fP[1] );
  VALGRIND_CHECK_VALUE_IS_DEFINED( fP[2] );

  const float_v &z0 = y - fP[0];
  const float_v &z1 = z - fP[1];

  const float_v &mS0 = float_v( Vc::One ) / err2Y;
  const float_v &mS2 = float_v( Vc::One ) / err2Z;
  //const float_v &mS0 = CAMath::Reciprocal( err2Y );
  //const float_v &mS2 = CAMath::Reciprocal( err2Z );
  debugKF() << "delta(mS0): " << CAMath::Abs( float_v( Vc::One ) / err2Y - mS0 ) << std::endl;
  debugKF() << "delta(mS2): " << CAMath::Abs( float_v( Vc::One ) / err2Z - mS2 ) << std::endl;
#ifdef __ASSERT_YF__
  assert( mS0 > 0.f || !mask );
  assert( mS2 > 0.f || !mask );
#endif
  // K = CHtS

  const float_v &k00 = c00 * mS0;
  const float_v &k20 = c20 * mS0;
  const float_v &k40 = c40 * mS0;

  const float_v &k11 = c11 * mS2;
  const float_v &k31 = c31 * mS2;

  debugKF() << "delta(k00): " << ( c00 / err2Y - k00 ) << std::endl;
  debugKF() << "delta(k20): " << ( c20 / err2Y - k20 ) << std::endl;
  debugKF() << "delta(k40): " << ( c40 / err2Y - k40 ) << std::endl;

  debugKF() << "delta(k11): " << ( c11 / err2Z - k11 ) << std::endl;
  debugKF() << "delta(k31): " << ( c31 / err2Z - k31 ) << std::endl;

  const float_v &sinPhi = fP[2] + k20 * z0  ;
  debugKF() << "delta(sinPhi): " << ( z0 * c20 / err2Y + fP[2] - sinPhi ) << std::endl;

  assert( float_m(maxSinPhi > 0.f).isFull() );
  VALGRIND_CHECK_MASKED_VECTOR_IS_DEFINED( sinPhi, mask );
  const float_m &success = mask && err2Y >= 1.e-8f && err2Z >= 1.e-8f && CAMath::Abs( sinPhi ) < maxSinPhi;
  VALGRIND_CHECK_VALUE_IS_DEFINED( success );

  fNDF  ( static_cast<int_m>( success ) ) += 2;
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
  const float_m check = ( fC[ 0] >= 0.f ) && ( fC[ 2] >= 0.f ) && ( fC[ 5] >= 0.f ) && ( fC[ 9] >= 0.f ) && ( fC[14] >= 0.f );
#else
  assert( fC[ 0] >= 0.f );
  assert( fC[ 2] >= 0.f );
  assert( fC[ 5] >= 0.f );
  assert( fC[ 9] >= 0.f );
  assert( fC[14] >= 0.f );
#endif
  return success && check;
}

inline float_m AliHLTTPCCATrackParamVector::Rotate( const float_v &alpha, const float maxSinPhi, const float_m &mask )
{
  //* Rotate the coordinate system in XY on the angle alpha

  const float_v cA = CAMath::Cos( alpha );
  const float_v sA = CAMath::Sin( alpha );
  const float_v x = X(), y = Y(), sP = SinPhi(), cP = GetCosPhi();
  const float_v cosPhi = cP * cA + sP * sA;
  const float_v sinPhi = -cP * sA + sP * cA;

  float_m mReturn = mask && (CAMath::Abs( sinPhi ) < maxSinPhi) && (CAMath::Abs( cosPhi ) > 1.e-2f) && (CAMath::Abs( cP ) > 1.e-2f);

  const float_v j0 = cP / cosPhi;
  const float_v j2 = cosPhi / cP;

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

inline void AliHLTTPCCATrackParamVector::RotateXY( float_v alpha, float_v &x, float_v &y, float_v &sin, const float_m &mask ) const
{
  //* Rotate the coordinate system in XY on the angle alpha
  const float_v cA = CAMath::Cos( alpha );
  const float_v sA = CAMath::Sin( alpha );

  x(mask) = ( X()*cA +  Y()*sA );
  y(mask) = ( -X()*sA +  Y()*cA );
  sin(mask) = -GetCosPhi() * sA + SinPhi() * cA;
}

typedef AliHLTTPCCATrackParamVector TrackParamVector;

std::istream &operator>>( std::istream &in, AliHLTTPCCATrackParamVector &ot );
std::ostream &operator<<( std::ostream &out, const AliHLTTPCCATrackParamVector &ot );

#endif
