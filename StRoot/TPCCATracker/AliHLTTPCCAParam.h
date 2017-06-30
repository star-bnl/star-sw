//-*- Mode: C++ -*-
// @(#) $Id: AliHLTTPCCAParam.h,v 1.4 2011/10/01 00:23:44 perev Exp $
// ************************************************************************
// This file is property of and copyright by the ALICE HLT Project        *
// ALICE Experiment at CERN, All rights reserved.                         *
// See cxx source for full Copyright notice                               *
//                                                                        *
//*************************************************************************

#ifndef ALIHLTTPCCAPARAM_H
#define ALIHLTTPCCAPARAM_H

#include "AliHLTTPCCADef.h"
#include "AliHLTTPCCAMath.h"
#include "AliHLTTPCCATrackParam.h"
#include "AliHLTTPCCAParameters.h"
#include <cstdio>
#include <vector>
using std::vector;

namespace std
{
  template<typename T> struct char_traits;
  template<typename _CharT, typename _Traits> class basic_istream;
  typedef basic_istream<char, char_traits<char> > istream;
  template<typename _CharT, typename _Traits> class basic_ostream;
  typedef basic_ostream<char, char_traits<char> > ostream;
} // namespace std

/**
 * @class ALIHLTTPCCAParam
 * parameters of the AliHLTTPCCATracker, including geometry information
 * and some reconstructon constants.
 *
 * The class is under construction.
 *
 */
class AliHLTTPCCAParam
{
    friend std::istream &operator>>( std::istream &, AliHLTTPCCAParam & );
    friend std::ostream &operator<<( std::ostream &, const AliHLTTPCCAParam & );
  public:

    AliHLTTPCCAParam();

    void Initialize( int iSlice, int nRows, float rowX[],
                     float alpha, float dAlpha,
                     float rMin, float rMax, float zMin, float zMax,
                     float padPitch, float zSigma, float bz );
    void Update();

    void Slice2Global( float x, float y,  float z,
                       float *X, float *Y,  float *Z ) const;

    void Global2Slice( float x, float y,  float z,
                       float *X, float *Y,  float *Z ) const;


    int ISlice() const { return fISlice;}
    int NRows() const { return fNRows; }
    int NRows8() const { return fNRows + float_v::Size - (fNRows-1)%float_v::Size - 1;} // NRows8 % float_v::Size == 0 && NRows + float_v::Size > NRows8 >= NRows
//
    int NInnerRows() const { return fNInnerRows; }
  
    const float *RowX() const { return &(fRowX[0]); }
    float RowX( int iRow ) const { return fRowX[iRow]; }

    float Alpha() const { return fAlpha;}
    float Alpha( int iSlice ) const { return 0.0 + DAlpha()*iSlice;} // TODO: From file!
    float DAlpha() const { return fDAlpha;}
    float CosAlpha() const { return fCosAlpha;}
    float SinAlpha() const { return fSinAlpha;}
    float AngleMin() const { return fAngleMin;}
    float AngleMax() const { return fAngleMax;}
    float RMin() const { return fRMin;}
    float RMax() const { return fRMax;}
    float ZMin() const { return fZMin;}
    float ZMax() const { return fZMax;}
//     float ErrZ() const { return fErrZ;}
//     float ErrX() const { return fErrX;}
//     float ErrY() const { return fErrY;}
    float Bz() const { return fBz;}
    float cBz() const { return fBz*0.000299792458;}

    float TrackConnectionFactor() const { return fTrackConnectionFactor; }
    float TrackChiCut()  const { return fTrackChiCut; }
    float TrackChi2Cut() const { return fTrackChi2Cut; }
    int   MaxTrackMatchDRow() const { return fMaxTrackMatchDRow; }
    float HitPickUpFactor() const { return fHitPickUpFactor; }



    void SetISlice( int v ) {  fISlice = v;}
    void SetNRows( int v ) {  fNRows = v; fRowX.resize(fNRows);}
    void SetNInnerRows( int v ) { fNInnerRows = v;}
    void SetRowX( int iRow, float v ) {  fRowX[iRow] = v; }
    void SetAlpha( float v ) {  fAlpha = v;}
    void SetDAlpha( float v ) {  fDAlpha = v;}
    void SetCosAlpha( float v ) {  fCosAlpha = v;}
    void SetSinAlpha( float v ) {  fSinAlpha = v;}
    void SetAngleMin( float v ) {  fAngleMin = v;}
    void SetAngleMax( float v ) {  fAngleMax = v;}
    void SetRMin( float v ) {  fRMin = v;}
    void SetRMax( float v ) {  fRMax = v;}
    void SetZMin( float v ) {  fZMin = v;}
    void SetZMax( float v ) {  fZMax = v;}
    void SetErrZ( float v ) {  fErrZ = v;}
    void SetErrX( float v ) {  fErrX = v;}
    void SetErrY( float v ) {  fErrY = v;}
    void SetBz( float v ) {  fBz = v;}
    void SetTrackConnectionFactor( float v ) { fTrackConnectionFactor = v;}
    void SetTrackChiCut( float v ) {  fTrackChiCut = v; }
    void SetTrackChi2Cut( float v ) {  fTrackChi2Cut = v; }
    void SetMaxTrackMatchDRow( int v ) {  fMaxTrackMatchDRow = v; }
    void SetHitPickUpFactor( float v ) {  fHitPickUpFactor = v; }
    void SetRecoType( int reco)        {  fRecoType = reco; }

///mvz start 20.01.2010
/*    void GetClusterErrors2( int iRow, float z, float sinPhi, float cosPhi, float DzDs, float &Err2Y, float &Err2Z ) const;

    void GetClusterErrors2( int iRow, float_v z, float_v sinPhi, float_v DzDs, float_v &Err2Y, float_v &Err2Z ) const;
    void GetClusterErrors2( uint_v rowIndexes, float_v z, float_v sinPhi, float_v DzDs, float_v &Err2Y, float_v &Err2Z ) const;*/

    void GetClusterErrors2( int iRow, const AliHLTTPCCATrackParam &t, float &Err2Y, float &Err2Z ) const;
    void GetClusterErrors2( uint_v rowIndexes, const float_v &X, const float_v &Y, float_v &Z, float_v &Err2Y, float_v &Err2Z ) const;
///mvz end 20.01.2010

    void GetClusterErrors2( int iRow, const TrackParamVector &t, float_v *Err2Y, float_v *Err2Z ) const;
    void GetClusterErrors2( uint_v rowIndexes, const TrackParamVector &t, float_v *Err2Y, float_v *Err2Z ) const;

    void SetParamS0Par( int i, int j, int k, float val ) {
      fParamS0Par[i][j][k] = val;
    }

    float GetBz() const { return fBz;}
    float GetBz( float x, float y, float z ) const;
    float_v GetBz( float_v x, float_v y, float_v z ) const;
    float GetBz( const AliHLTTPCCATrackParam &t ) const;
    float_v GetBz( const AliHLTTPCCATrackParamVector &t ) const;

    void StoreToFile( FILE *f ) const;
    void RestoreFromFile( FILE *f );

  protected:
    float GetClusterError2( int yz, int type, float z, float angle ) const;
    float_v GetClusterError2( int yz, int type, float_v z, float_v angle ) const;

    int fISlice; // slice number
    int fNRows; // number of rows
    int fNInnerRows; // number of inner rows

    float fAlpha, fDAlpha; // slice angle and angular size
    float fCosAlpha, fSinAlpha;// sign and cosine of the slice angle
    float fAngleMin, fAngleMax; // minimal and maximal angle
    float fRMin, fRMax;// slice R range
    float fZMin, fZMax;// slice Z range
    float fErrX, fErrY, fErrZ;// default cluster errors
    float fPadPitch; // pad pitch
    float fBz;       // magnetic field value (only constant field can be used)

    float fHitPickUpFactor;// multiplier for the chi2 window for hit pick up procedure

    int   fMaxTrackMatchDRow;// maximal jump in TPC row for connecting track segments
    float fTrackConnectionFactor; // allowed distance in Chi^2/3.5 for neighbouring tracks
    float fTrackChiCut; // cut for track Sqrt(Chi2/NDF);
    float fTrackChi2Cut;// cut for track Chi^2/NDF

    vector<float> fRowX;// X-coordinate of rows 
    int   fRecoType;		   // 0=Sti error parametrization; 1=Stv
    float fParamS0Par[2][3][7];    // cluster error parameterization coeficients
    float fPolinomialFieldBz[6];   // field coefficients

  private:
///mvz start 20.01.2010
/*
    inline int errorType( int row ) const {
      int type = 0;
//       const int numberOfRows = AliHLTTPCCAParameters::NumberOfRows;
    if ( ISLIKELY( row >= fInnerRows ) ) {
        type = ( row > 126 ? 1 : 2 );
      }
      return type;
    }
    inline uint_v errorType( int_v row ) const {
      uint_v type( 14 );
    type.makeZero( row < fInnerRows );
      type( row > 126 ) = 7;
      return type;
    }
*/
    inline int errorType( int row ) const {
      int type = 0;
      type = ( row < fNInnerRows ? 0 : 1 );
      return type;
    }
    inline uint_v errorType( int_v row ) const {
      uint_v type( 7 );
      type.setZero( row < fNInnerRows );
      //type( row > 126 ) = 7;
      return type;
    }
///mvz end 20.01.2010
};


std::istream &operator>>( std::istream &, AliHLTTPCCAParam & );
std::ostream &operator<<( std::ostream &, const AliHLTTPCCAParam & );

inline float AliHLTTPCCAParam::GetBz( float x, float y, float z ) const
{
  float r2 = x * x + y * y;
  float r  = CAMath::Sqrt( r2 );
  const float *c = fPolinomialFieldBz;
  return ( c[0] + c[1]*z  + c[2]*r  + c[3]*z*z + c[4]*z*r + c[5]*r2 );
}

inline float_v AliHLTTPCCAParam::GetBz( float_v x, float_v y, float_v z ) const
{
  float_v r2 = x * x + y * y;
  float_v r  = CAMath::Sqrt( r2 );
  const float *c = fPolinomialFieldBz;
  return ( c[0] + c[1]*z  + c[2]*r  + c[3]*z*z + c[4]*z*r + c[5]*r2 );
}

inline float AliHLTTPCCAParam::GetBz( const AliHLTTPCCATrackParam &t ) const
{
  return GetBz( t.X(), t.Y(), t.Z() );
}

inline float_v AliHLTTPCCAParam::GetBz( const AliHLTTPCCATrackParamVector &t ) const
{
  return GetBz( t.X(), t.Y(), t.Z() );
}

///mvz start 20.01.2010
/*
inline float_v AliHLTTPCCAParam::GetClusterError2( int yz, int type, float_v z, float_v angle ) const
{
  // recalculate the cluster error wih respect to the track slope
  const float_v angle2 = angle * angle;
  const float *c = fParamS0Par[yz][type];
  const float_v v = c[0] + z * ( c[1] + c[3] * z ) + angle2 * ( c[2] + angle2 * c[4] + c[5] * z );
  return CAMath::Abs( v );
}

inline void AliHLTTPCCAParam::GetClusterErrors2( int iRow, const TrackParamVector &t, float_v *Err2Y, float_v *Err2Z ) const
{
  //
  // Use calibrated cluster error from OCDB
  //

  const float_v &z = CAMath::Abs( ( 250.f - 0.275f ) - CAMath::Abs( t.Z() ) );
  const int type = errorType( iRow );
  const float_v &sinPhi = t.SinPhi();
  const float_v &cosPhiInv = t.SignCosPhi() / CAMath::Sqrt( float_v( Vc::One ) - sinPhi * sinPhi ); // RSqrt
  const float_v &angleY = sinPhi * cosPhiInv;
  const float_v &angleZ = t.DzDs() * cosPhiInv; // SG was bug???

  debugF() << "GetClusterErrors2 y,z angles: " << angleY << angleZ << std::endl;

  *Err2Y = GetClusterError2( 0, type, z, angleY );
  *Err2Z = GetClusterError2( 1, type, z, angleZ );
}

inline void AliHLTTPCCAParam::GetClusterErrors2( int iRow, float_v z, float_v sinPhi, float_v DzDs, float_v &Err2Y, float_v &Err2Z ) const
{
  //
  // Use calibrated cluster error from OCDB
  //

  z = CAMath::Abs( ( 250.f - 0.275f ) - CAMath::Abs( z ) );
  const int type = errorType( iRow );
  const float_v cosPhiInv = float_v( Vc::One ) / CAMath::Sqrt( float_v( Vc::One ) - sinPhi * sinPhi ); // RSqrt
  float_v angleY = sinPhi * cosPhiInv;
  float_v angleZ = DzDs * cosPhiInv; // SG was bug???

  debugF() << "GetClusterErrors2 y,z angles: " << angleY << angleZ << std::endl;

  Err2Y = GetClusterError2( 0, type, z, angleY );
  Err2Z = GetClusterError2( 1, type, z, angleZ );
}

inline void AliHLTTPCCAParam::GetClusterErrors2( uint_v rowIndexes, const TrackParamVector &t, float_v *Err2Y, float_v *Err2Z ) const
{
  const float_v &z = CAMath::Abs( ( 250.f - 0.275f ) - CAMath::Abs( t.Z() ) );
  const uint_v type = errorType( static_cast<int_v>( rowIndexes ) );
  const float_v &sinPhi = t.SinPhi();
  const float_v &cosPhiInv = t.SignCosPhi() / CAMath::Sqrt( float_v( Vc::One ) - sinPhi * sinPhi ); // RSqrt
  const float_v &angleY = sinPhi * cosPhiInv;
  const float_v &angleZ = t.DzDs() * cosPhiInv; // SG was bug???

  const float_v angleY2 = angleY * angleY;
  const float *c = &fParamS0Par[0][0][0];
  float_v v( c, type );
  v += z * ( float_v( c + 1, type ) + float_v( c + 3, type ) * z );
  v += angleY2 * ( float_v( c + 2, type ) + angleY2 * float_v( c + 4, type ) + float_v( c + 5, type ) * z );
  *Err2Y = CAMath::Abs( v );

  const float_v angleZ2 = angleZ * angleZ;
  c = &fParamS0Par[1][0][0];
  v.gather( c, type );
  v += z * ( float_v( c + 1, type ) + float_v( c + 3, type ) * z );
  v += angleZ2 * ( float_v( c + 2, type ) + angleZ2 * float_v( c + 4, type ) + float_v( c + 5, type ) * z );
  *Err2Z = CAMath::Abs( v );
}

inline void AliHLTTPCCAParam::GetClusterErrors2( uint_v rowIndexes, float_v z, float_v sinPhi, float_v DzDs, float_v &Err2Y, float_v &Err2Z ) const
{
  z = CAMath::Abs( ( 250.f - 0.275f ) - CAMath::Abs( z ) );
  const uint_v type = errorType( static_cast<int_v>( rowIndexes ) );
  const float_v cosPhiInv = float_v( Vc::One ) / CAMath::Sqrt( float_v( Vc::One ) - sinPhi * sinPhi ); // RSqrt
  float_v angleY = sinPhi * cosPhiInv;
  float_v angleZ = DzDs * cosPhiInv; // SG was bug???

  const float_v angleY2 = angleY * angleY;
  const float *c = &fParamS0Par[0][0][0];
  float_v v( c, type );
  v += z * ( float_v( c + 1, type ) + float_v( c + 3, type ) * z );
  v += angleY2 * ( float_v( c + 2, type ) + angleY2 * float_v( c + 4, type ) + float_v( c + 5, type ) * z );
  Err2Y = CAMath::Abs( v );

  const float_v angleZ2 = angleZ * angleZ;
  c = &fParamS0Par[1][0][0];
  v.gather( c, type );
  v += z * ( float_v( c + 1, type ) + float_v( c + 3, type ) * z );
  v += angleZ2 * ( float_v( c + 2, type ) + angleZ2 * float_v( c + 4, type ) + float_v( c + 5, type ) * z );
  Err2Z = CAMath::Abs( v );
}
*/
inline void AliHLTTPCCAParam::GetClusterErrors2( int iRow, const TrackParamVector &t, float_v *Err2Y, float_v *Err2Z ) const
{
  const float_v one = float_v(Vc::One);
  const float_v zero = float_v(Vc::Zero);
  float_v z = t.Z();
  const int type = errorType( iRow );
  z = (200.f - CAMath::Abs(z)) * 0.01f;
  z(z < zero) = zero;

  float_v sin2Phi = t.GetSinPhi()*t.GetSinPhi();
  float_v cos2Phi = (one - sin2Phi);
  cos2Phi(cos2Phi < 0.0001f) = 0.0001f;
  float_v tg2Phi = sin2Phi/cos2Phi;

  float_v tg2Lambda = t.DzDs()*t.DzDs();

  const float *c = fParamS0Par[0][type];
  float_v v = c[0] + c[1]*z/cos2Phi + c[2]*tg2Phi;
  float_v w = c[3] + c[4]*z*(one + tg2Lambda) + c[5]*tg2Lambda;
  v(v>one) = one;
  w(w>one) = one;

  const float_v errmin=1e-6f;
  v(v<errmin) = errmin;
  w(w<errmin) = errmin;

  *Err2Y = CAMath::Abs( v );
  *Err2Z = CAMath::Abs( w );
}

inline void AliHLTTPCCAParam::GetClusterErrors2( uint_v rowIndexes, const TrackParamVector &t, float_v *Err2Y, float_v *Err2Z ) const
{
  const float_v one = float_v(Vc::One);
  const float_v zero = float_v(Vc::Zero);
  float_v z = t.Z();
  z = (200.f - CAMath::Abs(z)) * 0.01f;
  z(z < zero) = zero;

  const uint_v type = errorType( static_cast<int_v>( rowIndexes ) );

  float_v sin2Phi = t.GetSinPhi()*t.GetSinPhi();
  float_v cos2Phi = (one - sin2Phi);
  cos2Phi(cos2Phi < 0.0001f) = 0.0001f;
  float_v tg2Phi = sin2Phi/cos2Phi;

  float_v tg2Lambda = t.DzDs()*t.DzDs();

  const float *c = &fParamS0Par[0][0][0];
  const float_v errmin=1e-6f;
  float_v v( c, type );
  v += z * float_v( c + 1, type )/cos2Phi +  float_v( c + 2, type ) *tg2Phi;
  v(v>one) = one;
  v(v<errmin) = errmin;
  *Err2Y = CAMath::Abs( v );

  v.gather( c+3, type );
  v += z * float_v( c + 4, type )*(one + tg2Lambda) + float_v( c + 5, type )*tg2Lambda;
  v(v>one) = one;
  v(v<errmin) = errmin;
  *Err2Z = CAMath::Abs( v );
}

// inline void AliHLTTPCCAParam::GetClusterErrors2( uint_v rowIndexes, const float_v &X, const float_v &Y, float_v &Z, float_v &Err2Y, float_v &Err2Z ) const
// {
//   const float_v one = float_v(Vc::One);
//   const float_v zero = float_v(Vc::Zero);
//   float_v z = Z;
//   z = (200.f - CAMath::Abs(z)) * 0.01f;
//   z(z < zero) = zero;

//   const uint_v type = errorType( static_cast<int_v>( rowIndexes ) );

//   float_v tg2Phi = Y/X;
//   tg2Phi = tg2Phi*tg2Phi;
//   float_v cos2Phi = one / (one + tg2Phi);
//   if (cos2Phi<0.0001f) cos2Phi=0.0001f;

//   float_v s2 = X*X+Y*Y;
//   float_v tg2Lambda = s2/(Z*Z);
// //  float_v tg2Lambda = one/t.DzDs();

//   const float *c = &fParamS0Par[0][0][0];
//   const float_v errmin=1e-6f;
//   float_v v( c, type );
//   v += z * float_v( c + 1, type )/cos2Phi +  float_v( c + 2, type ) *tg2Phi;
//   v(v>one) = one;
//   v(v<errmin) = errmin;
//   Err2Y = CAMath::Abs( v );

//   v.gather( c+3, type );
//   v += z * float_v( c + 4, type )*(one + tg2Lambda) + float_v( c + 5, type )*tg2Lambda;
//   v(v>one) = one;
//   v(v<errmin) = errmin;
//   Err2Z = CAMath::Abs( v );
// }

///mvz end 20.01.2010

#endif
