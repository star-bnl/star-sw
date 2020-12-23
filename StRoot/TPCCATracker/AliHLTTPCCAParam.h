/*
 * This file is part of TPCCATracker package
 * Copyright (C) 2007-2020 FIAS Frankfurt Institute for Advanced Studies
 *               2007-2020 Goethe University of Frankfurt
 *               2007-2020 Ivan Kisel <I.Kisel@compeng.uni-frankfurt.de>
 *               2007-2019 Sergey Gorbunov
 *               2007-2019 Maksym Zyzak
 *               2007-2014 Igor Kulakov
 *               2014-2020 Grigory Kozlov
 *
 * TPCCATracker is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * TPCCATracker is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

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
    int NTpcRows()   const { return fNTpcRows; }
  
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
    float ErrZ() const { return fErrZ;}
    float ErrX() const { return fErrX;}
    float ErrY() const { return fErrY;}
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
    void SetNTpcRows( int v ) { fNTpcRows = v;}
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

    void GetClusterErrors2( int iRow, const AliHLTTPCCATrackParam &t, float &Err2Y, float &Err2Z ) const;
    void GetClusterErrors2( uint_v rowIndexes, const float_v &X, const float_v &Y, float_v &Z, float_v &Err2Y, float_v &Err2Z ) const;

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
    int fNTpcRows; // total number of Tpc rows  
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
    float fParamS0Par[2][4][7];    // cluster error parameterization coeficients; 0 -> iTPC, 1 -> oTPC, 2 -> BToF, 3 -> EToF
    float fPolinomialFieldBz[6];   // field coefficients

  private:
  inline int errorType( int row) const {
    //    if (CAMath::Abs(z) > 210) return 3; // EToF
    if (row < fNInnerRows )   return 0; // Inner Tpc
    if (row < fNTpcRows )     return 1; // Outer Tpc
    return 2;                           // BToF
  }
    inline uint_v errorType( int_v row) const {
      uint_v type( 7 );
      type.setZero( row < fNInnerRows );
      return type;
    }
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

inline void AliHLTTPCCAParam::GetClusterErrors2( int iRow, const TrackParamVector &t, float_v *Err2Y, float_v *Err2Z ) const
{
  const float_v one = float_v(Vc::One);
  const float_v zero = float_v(Vc::Zero);
  float_v z = t.Z();
  const int type = errorType( iRow);// , z);
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
#if 0
  v(v>one) = one;
  w(w>one) = one;
#endif
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
  float_v v, v1, v2, v4, v5;
  for( unsigned int i = 0; i < float_v::Size; i++ ) {
    const float *c_temp = &c[(unsigned int)type[i]];
    v[i]  = c_temp[0];
    v1[i] = c_temp[1];
    v2[i] = c_temp[2];
//    v3[i] = c_temp[3];
    v4[i] = c_temp[4];
    v5[i] = c_temp[5];
  }
  v += z * v1/cos2Phi +  v2 *tg2Phi;
#if 0
  v(v>one) = one;
#endif
  v(v<errmin) = errmin;
  *Err2Y = CAMath::Abs( v );
#ifdef VC_GATHER_SCATTER
  v.gather( c+3, type );
#else
  for( unsigned int i = 0; i < float_v::Size; i++ ) {
      v[i] = c[(unsigned int)type[i] + 3];
  }
#endif
  v += z * v4*(one + tg2Lambda) + v5*tg2Lambda;
#if 0
  v(v>one) = one;
#endif
  v(v<errmin) = errmin;
  *Err2Z = CAMath::Abs( v );
}

#endif
