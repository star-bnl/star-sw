//-*- Mode: C++ -*-
// $Id: AliHLTTPCCATrackParam.h,v 1.1.1.1 2010/07/26 20:55:38 ikulakov Exp $
// ************************************************************************
// This file is property of and copyright by the ALICE HLT Project        *
// ALICE Experiment at CERN, All rights reserved.                         *
// See cxx source for full Copyright notice                               *
//                                                                        *
//*************************************************************************


#ifndef ALIHLTTPCCATRACKPARAM_H
#define ALIHLTTPCCATRACKPARAM_H

#include "AliHLTTPCCADef.h"
#include "AliHLTTPCCAMath.h"
#include "AliHLTTPCCATrackParamVector.h"

class AliHLTTPCCATrackLinearisation;

namespace std
{
  template<typename T> struct char_traits;
  template<typename _CharT, typename _Traits> class basic_istream;
  typedef basic_istream<char, char_traits<char> > istream;
  template<typename _CharT, typename _Traits> class basic_ostream;
  typedef basic_ostream<char, char_traits<char> > ostream;
} // namespace std

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

    AliHLTTPCCATrackParam() {}
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
    bool Filter( float y, float z, float err2Y, float err2Z, float maxSinPhi = .999 );


    void Print() const;

  private:

    float fX;      // x position
    float fSignCosPhi; // sign of cosPhi
    float fP[5];   // 'active' track parameters: Y, Z, SinPhi, DzDs, q/Pt
    float fC[15];  // the covariance matrix for Y,Z,SinPhi,..
    float fChi2;   // the chi^2 value
    int   fNDF;    // the Number of Degrees of Freedom
};

typedef AliHLTTPCCATrackParam TrackParam;

std::istream &operator>>( std::istream &in, AliHLTTPCCATrackParam &ot );
std::ostream &operator<<( std::ostream &out, const AliHLTTPCCATrackParam &ot );

#endif
