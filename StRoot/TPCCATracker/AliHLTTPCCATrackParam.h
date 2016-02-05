//-*- Mode: C++ -*-
// $Id: AliHLTTPCCATrackParam.h,v 1.1 2016/02/05 23:27:29 fisyak Exp $
// ************************************************************************
// This file is property of and copyright by the ALICE HLT Project        *
// ALICE Experiment at CERN, All rights reserved.                         *
// See cxx source for full Copyright notice                               *
//                                                                        *
//*************************************************************************


#ifndef ALIHLTTPCCATRACKPARAM_H
#define ALIHLTTPCCATRACKPARAM_H
#include <string.h>
#include "AliHLTTPCCADef.h"
#include "AliHLTTPCCAMath.h"
#include "AliHLTTPCCATrackParamVector.h"
#include "AliHLTTPCCADef.h"

class AliHLTTPCCATrackLinearisation;
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
 * @class AliHLTTPCCATrackParam
 *
 * AliHLTTPCCATrackParam class describes the track parametrisation
 * which is used by the AliHLTTPCCATracker slice tracker.
 *
 */
class AliHLTTPCCATrackParam
{
    friend std::istream &operator>>( std::istream &, AliHLTTPCCATrackParam & );
    friend std::ostream &operator<<( std::ostream &, const AliHLTTPCCATrackParam & );
  public:

    AliHLTTPCCATrackParam() { Reset();}
    AliHLTTPCCATrackParam( const TrackParamVector &v, int i )
      : fX( v.X()[i] ),
      fSignCosPhi( v.SignCosPhi()[i] ),
      fChi2( v.Chi2()[i] ),
      fNDF( v.NDF()[i] )
    {
      for ( int j = 0; j <  5; ++j ) fP[j] = v.Par()[j][i];
      for ( int j = 0; j < 15; ++j ) fC[j] = v.Cov()[j][i];
    }

    struct AliHLTTPCCATrackFitParam {
      float fBethe;
      float fE;
      float fTheta2;
      float fEP2;
      float fSigmadE2;
      float fK22;
      float fK33;
      float fK43;
      float fK44;
    };

    float X()      const { return fX;    }
    float Y()      const { return fP[0]; }
    float Z()      const { return fP[1]; }
    float SinPhi() const { return fP[2]; }
    float DzDs()   const { return fP[3]; }
    float QPt()    const { return fP[4]; }

    /**
     * The sign of cos phi is always positive in the slice tracker. Only after coordinate
     * transformation can the sign change to negative.
     */
    float SignCosPhi() const { return fSignCosPhi; }
    float Chi2()  const { return fChi2; }
    int   NDF()   const { return fNDF; }

    float Err2Y()      const { return fC[0]; }
    float Err2Z()      const { return fC[2]; }
    float Err2SinPhi() const { return fC[5]; }
    float Err2DzDs()   const { return fC[9]; }
    float Err2QPt()    const { return fC[14]; }

    float GetX()      const { return fX; }
    float GetY()      const { return fP[0]; }
    float GetZ()      const { return fP[1]; }
    float GetSinPhi() const { return fP[2]; }
    float GetDzDs()   const { return fP[3]; }
    float GetQPt()    const { return fP[4]; }
    float GetSignCosPhi() const { return fSignCosPhi; }
    float GetChi2()   const { return fChi2; }
    int   GetNDF()    const { return fNDF; }

    float GetKappa( float Bz ) const { return fP[4]*Bz; }
    float GetCosPhiPositive() const { return CAMath::Sqrt( 1 - SinPhi()*SinPhi() ); }
    float GetCosPhi() const { return fSignCosPhi*CAMath::Sqrt( 1 - SinPhi()*SinPhi() ); }

    float GetErr2Y()      const { return fC[0]; }
    float GetErr2Z()      const { return fC[2]; }
    float GetErr2SinPhi() const { return fC[5]; }
    float GetErr2DzDs()   const { return fC[9]; }
    float GetErr2QPt()    const { return fC[14]; }

    const float *Par() const { return fP; }
    const float *Cov() const { return fC; }

    const float *GetPar() const { return fP; }
    const float *GetCov() const { return fC; }

    void SetPar( int i, float v ) { fP[i] = v; }
    void SetCov( int i, float v ) { fC[i] = v; }

    void SetX( float v )     {  fX = v;    }
    void SetY( float v )     {  fP[0] = v; }
    void SetZ( float v )     {  fP[1] = v; }
    void SetSinPhi( float v ) {  fP[2] = v; }
    void SetDzDs( float v )  {  fP[3] = v; }
    void SetQPt( float v )   {  fP[4] = v; }
    void SetSignCosPhi( float v ) {  fSignCosPhi = v; }
    void SetChi2( float v )  {  fChi2 = v; }
    void SetNDF( int v )   { fNDF = v; }


    float GetDist2( const AliHLTTPCCATrackParam &t ) const;
    float GetDistXZ2( const AliHLTTPCCATrackParam &t ) const;


    float GetS( float x, float y, float Bz  ) const;

    void GetDCAPoint( float x, float y, float z,
                             float &px, float &py, float &pz, float Bz  ) const;


    bool TransportToX( float x, float Bz, float maxSinPhi = .999 );
    bool TransportToXWithMaterial( float x, float Bz, float maxSinPhi = .999 );

    bool  TransportToX( float x, AliHLTTPCCATrackLinearisation &t0,
                                 float Bz,  float maxSinPhi = .999, float *DL = 0 );

    bool  TransportToX( float x, float sinPhi0, float cosPhi0,  float Bz, float maxSinPhi = .999 );


    bool  TransportToXWithMaterial( float x,  AliHLTTPCCATrackLinearisation &t0,
        AliHLTTPCCATrackFitParam &par, float Bz, float maxSinPhi = .999 );

    bool  TransportToXWithMaterial( float x,
        AliHLTTPCCATrackFitParam &par, float Bz, float maxSinPhi = .999 );



    static float ApproximateBetheBloch( float beta2 );
    static float BetheBlochGeant( float bg,
                                           float kp0 = 2.33,
                                           float kp1 = 0.20,
                                           float kp2 = 3.00,
                                           float kp3 = 173e-9,
                                           float kp4 = 0.49848
                                         );
    static float BetheBlochSolid( float bg );
    static float BetheBlochGas( float bg );


    void CalculateFitParameters( AliHLTTPCCATrackFitParam &par, float mass = 0.13957 );
    bool CorrectForMeanMaterial( float xOverX0,  float xTimesRho, const AliHLTTPCCATrackFitParam &par );

    bool Rotate( float alpha, float maxSinPhi = .999 );
    bool Rotate( float alpha, AliHLTTPCCATrackLinearisation &t0, float maxSinPhi = .999 );

    void RotateXY( float alpha, float &x, float &y, float &sin ) const;
    bool Filter( float y, float z, float err2Y, float err2Z, float maxSinPhi = .999 );


    void Print() const;
    
    void ResetCovMatrix()
    {
      fC[0] = 10.f; 
      fC[1] = 0.f;   fC[2] = 10.f;
      fC[3] = 0.f;   fC[4] = 0.f;  fC[5] = 1.f;
      fC[6] = 0.f;   fC[7] = 0.f;  fC[8] = 0.f;  fC[9] = 1.f;
      fC[10] = 0.f;  fC[11] = 0.f; fC[12] = 0.f; fC[13] = 0.f;  fC[14] = 10.f;
    }

    AliHLTTPCCATrackParam GetGlobalParam(float alpha) const; // alpha - angle of the current slice

    void Reset() {
      fX = 0; fSignCosPhi = 0;
      for(int i=0; i<5; i++) fP[i] = 0;
      for(int i=0; i<15; i++) fC[i] = 0;
      fChi2 = 0; fNDF = 0;
    }

  private:
    float fX;      // x position
    float fSignCosPhi; // sign of cosPhi   // phi = arctg (Dy/Dx)
    float fP[5];   // 'active' track parameters: Y, Z, SinPhi, Dz/Ds (ds = sqrt( dx^2 + dy^2 )), q/Pt
    float fC[15];  // the covariance matrix for Y,Z,SinPhi,..
    float fChi2;   // the chi^2 value
    int   fNDF;    // the Number of Degrees of Freedom
};

inline void AliHLTTPCCATrackParam::RotateXY( float alpha, float &x, float &y, float &sin ) const
{
  //* Rotate the coordinate system in XY on the angle alpha

  const float cA = CAMath::Cos( alpha );
  const float sA = CAMath::Sin( alpha );

  x = ( X()*cA +  Y()*sA );
  y = ( -X()*sA +  Y()*cA );
  sin = -GetCosPhi() * sA + SinPhi() * cA;

}

inline bool AliHLTTPCCATrackParam::Filter( float y, float z, float err2Y, float err2Z, float maxSinPhi )
{
  assert( maxSinPhi > 0.f );
  //* Add the y,z measurement with the Kalman filter

  const float c00 = fC[0];
  const float c10 = fC[1];
  const float c11 = fC[2];
  const float c20 = fC[3];
  const float c21 = fC[4];
//  float c22 = fC[5];
  const float c30 = fC[6];
  const float c31 = fC[7];
//  float c32 = fC[8];
//  float c33 = fC[9];
  const float c40 = fC[10];
  const float c41 = fC[11];
//  float c42 = fC[12];
//  float c43 = fC[13];
//  float c44 = fC[14];
  
  float d = 1.f / ( err2Y*err2Z + err2Y*c11 + err2Z*c00 + c00*c11 - c10*c10 );
  err2Y += c00;
  err2Z += c11;

  const float
  z0 = y - fP[0],
  z1 = z - fP[1];

  if ( ISUNLIKELY( err2Y < 1.e-8f ) || ISUNLIKELY( err2Z < 1.e-8f ) ) return 0;

  const float mS0 = err2Z*d;
  const float mS1 = -c10*d;
  const float mS2 = err2Y*d;

  // K = CHtS

  const float 
  k00 = c00 * mS0 + c10*mS1,   k01 = c00 * mS1 + c10*mS2,
  k10 = c10 * mS0 + c11*mS1,   k11 = c10 * mS1 + c11*mS2,
  k20 = c20 * mS0 + c21*mS1,   k21 = c20 * mS1 + c21*mS2,
  k30 = c30 * mS0 + c31*mS1,   k31 = c30 * mS1 + c31*mS2,
  k40 = c40 * mS0 + c41*mS1,   k41 = c40 * mS1 + c41*mS2; 

  const float sinPhi = fP[2] + k20 * z0 + k21 * z1;

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

inline float AliHLTTPCCATrackParam::ApproximateBetheBloch( float beta2 )
{
  //------------------------------------------------------------------
  // This is an approximation of the Bethe-Bloch formula with
  // the density effect taken into account at beta*gamma > 3.5
  // (the approximation is reasonable only for solid materials)
  //------------------------------------------------------------------
  if ( beta2 >= 1 )
    return 0;
  else {
    const float beta2_beta21i = beta2 / ( 1 - beta2 );
    if ( beta2_beta21i > 12.25 ) // 3.5^2 = 12.25
      return 0.153e-3 / beta2 * ( 9.94223 + 0.5 * log( beta2_beta21i ) - beta2 ); // log( 3.5*5940 ) = 9.94223
  else
      return 0.153e-3 / beta2 * ( 8.6895 + log( beta2_beta21i ) - beta2 ); // log( 5940 ) = 8.6895
  }
}

inline void AliHLTTPCCATrackParam::CalculateFitParameters( AliHLTTPCCATrackFitParam &par, float mass )
{
  const float p2 = ( 1. + fP[3] * fP[3] );
  const float k2 = fP[4] * fP[4];
  const float mass2 = mass * mass;
  
  const float beta2 = p2 / ( p2 + mass2 * k2 );
	
  const float pp2 = ( k2 > 1.e-8 ) ? p2 / k2 : 10000; // impuls 2
	
    //par.fBethe = BetheBlochGas( pp2/mass2);
  par.fBethe = ApproximateBetheBloch( pp2 / mass2 );
  par.fE = CAMath::Sqrt( pp2 + mass2 );
  par.fTheta2 = 198.81e-6  / ( beta2 * pp2 ); // 14.1^2 * 1e-6 
  par.fEP2 = par.fE / pp2;                    // have tried reduce number of "/", but it was slower. (may be bacause of additional of constants = memory)

  // Approximate energy loss fluctuation (M.Ivanov)

  const float knst = 0.07; // To be tuned.
  par.fSigmadE2 = knst * par.fEP2 * fP[4];
  par.fSigmadE2 = par.fSigmadE2 * par.fSigmadE2;

  par.fK22 = p2;
  par.fK33 = par.fK22 * par.fK22;
  par.fK43 = fP[3] * fP[4] * par.fK22;
  par.fK44 = (p2 - 1.f) * k2;

}


#include "AliHLTTPCCAMath.h"
#include "AliHLTTPCCATrackLinearisation.h"

inline bool  AliHLTTPCCATrackParam::TransportToX( float x, AliHLTTPCCATrackLinearisation &t0, float Bz,  float maxSinPhi, float *DL )
{
  //* Transport the track parameters to X=x, using linearization at t0, and the field value Bz
  //* maxSinPhi is the max. allowed value for |t0.SinPhi()|
  //* linearisation of trajectory t0 is also transported to X=x,
  //* returns 1 if OK
  //*

  const float ex = t0.CosPhi();
  const float ey = t0.SinPhi();
  const float k  = t0.QPt() * Bz;
  const float dx = x - X();

  const float ey1 = k * dx + ey;

  // check for intersection with X=x

  if ( CAMath::Abs( ey1 ) > maxSinPhi ) return 0;

  float ex1 = CAMath::Sqrt( 1.f - ey1 * ey1 );
  if ( ex < 0 ) ex1 = -ex1;

  const float dx2 = dx * dx;
  const float ss = ey + ey1;
  const float cc = ex + ex1;

  if ( ( CAMath::Abs( cc ) < 1.e-4 || CAMath::Abs( ex ) < 1.e-4 || CAMath::Abs( ex1 ) < 1.e-4 ) ) return 0;
  
  const float cci = 1.f / cc;
  const float exi = 1.f / ex;
  const float ex1i = 1.f / ex1;
  
  const float tg = ss * cci; // tan((phi1+phi)/2)

  const float dy = dx * tg;
  float dl = dx * CAMath::Sqrt( 1.f + tg * tg );

  if ( cc < 0 ) dl = -dl;
  float dSin = dl * k * 0.5;
  if ( dSin > 1.f ) dSin = 1.f;
  if ( dSin < -1.f ) dSin = -1.f;
  const float dS = ( CAMath::Abs( k ) > 1.e-4 )  ? ( 2 * CAMath::ASin( dSin ) / k ) : dl;
  const float dz = dS * t0.DzDs();

  if ( DL ) *DL = -dS * CAMath::Sqrt( 1.f + t0.DzDs() * t0.DzDs() );


  const float d[3] = { fP[2] - t0.SinPhi(), fP[3] - t0.DzDs(), fP[4] - t0.QPt() };

  //float H0[5] = { 1,0, h2,  0, h4 };
  //float H1[5] = { 0, 1, 0, dS,  0 };
  //float H2[5] = { 0, 0, 1,  0, dxBz };
  //float H3[5] = { 0, 0, 0,  1,  0 };
  //float H4[5] = { 0, 0, 0,  0,  1 };

  const float h2 = dx * ( 1.f + ey * ey1 + ex * ex1 ) * exi * ex1i * cci;
  const float h4 = dx2 * ( cc + ss * ey1 * ex1i ) * cci * cci * Bz;
  const float dxBz = dx * Bz;

  t0.SetCosPhi( ex1 );
  t0.SetSinPhi( ey1 );

  fX    = X() + dx;
  fP[0] = Y() + dy     + h2 * d[0]           +   h4 * d[2];
  fP[1] = Z() + dz               + dS * d[1];
  fP[2] = t0.SinPhi() +     d[0]           + dxBz * d[2];
  if(CAMath::Abs(fP[2]) > maxSinPhi) fP[2] = t0.SinPhi();

#if 1
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
            + 2.f * ( h2 * c20 + h4 * c40 + h2 * h4 * c42 )  );

  fC[1] = c10 + h2 * c21 + h4 * c41 + dS * ( c30 + h2 * c32 + h4 * c43 );
  fC[2] = c11 + 2.f * dS * c31 + dS * dS * c33;

  fC[3] = c20 + h2 * c22 + h4 * c42 + dxBz * ( c40 + h2 * c42 + h4 * c44 );
  fC[4] = c21 + dS * c32 + dxBz * ( c41 + dS * c43 );
  fC[5] = c22 + 2.f * dxBz * c42 + dxBz * dxBz * c44;

  fC[6] = c30 + h2 * c32 + h4 * c43;
  fC[7] = c31 + dS * c33;
  fC[8] = c32 + dxBz * c43;
  fC[9] = c33;

  fC[10] = c40 + h2 * c42 + h4 * c44;
  fC[11] = c41 + dS * c43;
  fC[12] = c42 + dxBz * c44;
  fC[13] = c43;
  fC[14] = c44;
#else
  fC[0] = ( fC[0]  + h2 * h2 * fC[5] + h4 * h4 * fC[14]
            + 2 * ( h2 * fC[3] + h4 * fC[10] + h2 * h4 * fC[12] )  );

  fC[1] = fC[1] + h2 * fC[4] + h4 * fC[11] + dS * ( fC[6] + h2 * fC[8] + h4 * fC[13] );
  fC[2] = fC[2] + 2 * dS * fC[7] + dS * dS * fC[9];

  fC[3] = fC[3] + h2 * fC[5] + h4 * fC[12] + dxBz * ( fC[10] + h2 * fC[12] + h4 * fC[14] );
  fC[4] = fC[4] + dS * fC[8] + dxBz * ( fC[11] + dS * fC[13] );
  fC[5] = fC[5] + 2 * dxBz * fC[12] + dxBz * dxBz * fC[14];

  fC[6] = fC[6] + h2 * fC[8] + h4 * fC[13];
  fC[7] = fC[7] + dS * fC[9];
  fC[8] = fC[8] + dxBz * fC[13];
  fC[9] = fC[9];

  fC[10] = fC[10] + h2 * fC[12] + h4 * fC[14];
  fC[11] = fC[11] + dS * fC[13];
  fC[12] = fC[12] + dxBz * fC[14];
  fC[13] = fC[13];
  fC[14] = fC[14];
#endif

//std::cout << fC[0] << "  "<<fC1[0]<<"       "<<fC[2] << "  "<<fC1[2]<<"       "<<fC[5] << "  "<<fC1[5]<<"       "<<fC[9] << "  "<<fC1[9]<<"       "<<fC[14] << "  "<<fC1[14]<<std::endl;
  return 1;
}

inline bool AliHLTTPCCATrackParam::CorrectForMeanMaterial( float xOverX0,  float xTimesRho, const AliHLTTPCCATrackFitParam &par )
{
  //------------------------------------------------------------------
  // This function corrects the track parameters for the crossed material.
  // "xOverX0"   - X/X0, the thickness in units of the radiation length.
  // "xTimesRho" - is the product length*density (g/cm^2).
  //------------------------------------------------------------------
  // float &fC22 = fC[5];
  // float &fC33 = fC[9];
  // float &fC40 = fC[10];
  // float &fC41 = fC[11];
  // float &fC42 = fC[12];
  // float &fC43 = fC[13];
  // float &fC44 = fC[14];

  //Energy losses************************

  const float dE = par.fBethe * xTimesRho;
  if ( CAMath::Abs( dE ) > 0.3 * par.fE ) return 0; //30% energy loss is too much!
  const float corr = ( 1. - par.fEP2 * dE );
  if ( corr < 0.3 || corr > 1.3 ) return 0;

  fP[4] *= corr;
  fC[10] *= corr;
  fC[11] *= corr;
  fC[12] *= corr;
  fC[13] *= corr;
  fC[14] *= corr * corr;
  fC[14] += par.fSigmadE2 * CAMath::Abs( dE );
//  std::cout << "dE "<<dE<<"    corr "<<corr<<"  fBethe  " <<par.fBethe<<"  XxRo  "<<xTimesRho<<std::endl;

  //Multiple scattering******************

  const float theta2 = par.fTheta2 * CAMath::Abs( xOverX0 );
  fC[5] += theta2 * par.fK22 * ( 1. - fP[2] * fP[2] );
  fC[9] += theta2 * par.fK33;
  fC[13] += theta2 * par.fK43;
  fC[14] += theta2 * par.fK44;
  
  return 1;
}

inline bool  AliHLTTPCCATrackParam::TransportToXWithMaterial( float x,  AliHLTTPCCATrackLinearisation &t0, AliHLTTPCCATrackFitParam &par, float Bz, float maxSinPhi )
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

inline bool  AliHLTTPCCATrackParam::TransportToXWithMaterial( float x,  AliHLTTPCCATrackFitParam &par, float Bz, float maxSinPhi )
{
  //* Transport the track parameters to X=x  taking into account material budget

  AliHLTTPCCATrackLinearisation t0( *this );
  return TransportToXWithMaterial( x, t0, par, Bz, maxSinPhi );
}



typedef AliHLTTPCCATrackParam TrackParam;

std::istream &operator>>( std::istream &in, AliHLTTPCCATrackParam &ot );
std::ostream &operator<<( std::ostream &out, const AliHLTTPCCATrackParam &ot );

#endif
