//-*- Mode: C++ -*-
// ************************************************************************
// This file is property of and copyright by the ALICE HLT Project        *
// ALICE Experiment at CERN, All rights reserved.                         *
// See cxx source for full Copyright notice                               *
//                                                                        *
//*************************************************************************

#ifndef ALIHLTTPCCAMERGER_H
#define ALIHLTTPCCAMERGER_H

#include "AliHLTTPCCADef.h"
#include "AliHLTTPCCAParam.h"
#include "AliHLTTPCCATrackParam.h"
#include "AliHLTTPCCASliceTrack.h"

#include <vector>

#if !defined(HLTCA_GPUCODE)
#include <iostream>
#endif

class AliHLTTPCCATrackParamVector;
class AliHLTTPCCASliceTrack;
class AliHLTTPCCASliceOutput;
class AliHLTTPCCAMergedTrack;
class AliHLTTPCCAMergerOutput;

/**
 * @class AliHLTTPCCAMerger
 *
 */

class AliHLTTPCCAClusterInfo;
class AliHLTTPCCAMerger
{
    // public:
    //   class AliHLTTPCCAClusterInfo;
 private:
//  struct AliHLTTPCCATrackMemory;
//  struct AliHLTTPCCAHitMemory;

  class AliHLTTPCCASliceTrackInfo;

  class AliHLTTPCCABorderTrack
  {
   public:

    int   TrackID() const { return fTrackID; }
    float b()       const{ return fb;        }
    float bErr2()   const{ return fbErr2;    }
    float p()       const{ return fp;        }
    float pErr2()   const{ return fpErr2;    }
    unsigned short InnerRow() const {return fInnerRow;}
    unsigned short OuterRow() const {return fOuterRow;}

    void SetInnerRow(unsigned short v) {fInnerRow = v;}
    void SetOuterRow(unsigned short v) {fOuterRow = v;}
    void SetTrackID ( int v )   { fTrackID   = v; }
    void Setb       (float v)   { fb         = v; }
    void SetbErr2   (float v)   { fbErr2     = v; }
    void Setp       (float v)   { fp         = v; }
    void SetpErr2   (float v)   { fpErr2     = v; }

   private:
    float fb;
    float fbErr2;
    float fp;
    float fpErr2;
    int   fTrackID;              // track index
    unsigned short fInnerRow;
    unsigned short fOuterRow;
  };

 public:

  AliHLTTPCCAMerger();
  ~AliHLTTPCCAMerger();

  void SetSliceParam( const AliHLTTPCCAParam &v ) { fSliceParam = v; }

  void Clear();
  void SetSliceData( int index, const AliHLTTPCCASliceOutput *SliceData );
  void Reconstruct();
  const AliHLTTPCCAMergerOutput * Output() const { return fOutput; }

//  bool FitTrack( AliHLTTPCCATrackParam &T, float &Alpha,
//  AliHLTTPCCATrackParam t0, float Alpha0, int hits[], int &NHits,  bool dir = 0 );

  void SetSlices ( int i, AliHLTTPCCATracker *sl );

  static void SetDoNotMergeBorders(int i = 0) {fgDoNotMergeBorders = i;}
  
  int NTimers() { return fNTimers; }
  float Timer( int i ) { return fTimers[i]; };
 private:

  void ConvertTrackParamToVector( AliHLTTPCCATrackParam t0[ushort_v::Size], AliHLTTPCCATrackParamVector &t, int &nTracksV);

  sfloat_m FitTrack( AliHLTTPCCATrackParamVector &t, sfloat_v &Alpha0V,
                     int hits[2000][ushort_v::Size], ushort_v &firstHits, ushort_v::Memory &NTrackHits,
                     int &nTracksV, sfloat_m active0 = sfloat_m(true), bool dir = 1 );

  AliHLTTPCCAMerger( const AliHLTTPCCAMerger& );
  const AliHLTTPCCAMerger &operator=( const AliHLTTPCCAMerger& ) const;

  void InvertCholetsky(float a[15]);
  void MultiplySS(float const C[15], float const V[15], float K[5][5]);
  void MultiplyMS(float const C[5][5], float const V[15], float K[15]);
  void MultiplySR(float const C[15], float const r_in[5], float r_out[5]);
  void FilterTracks(float const r[5], float const C[15], float const m[5], float const V[15], float R[5], float W[15], float &chi2);

  void InvertCholetsky(sfloat_v a[15]);
  void MultiplySS(sfloat_v const C[15], sfloat_v const V[15], sfloat_v K[5][5]);
  void MultiplyMS(sfloat_v const C[5][5], sfloat_v const V[15], sfloat_v K[15]);
  void MultiplySR(sfloat_v const C[15], sfloat_v const r_in[5], sfloat_v r_out[5]);
  void FilterTracks(sfloat_v const r[5], sfloat_v const C[15], sfloat_v const m[5], sfloat_v const V[15],
                    sfloat_v R[5], sfloat_v W[15], sfloat_v &chi2, const sfloat_m &mask = sfloat_m( true ));

  static bool CompareInnerRow (const AliHLTTPCCABorderTrack &b1, const AliHLTTPCCABorderTrack &b2) {
    return (b1.InnerRow() > b2.InnerRow()) || ( (b1.InnerRow() == b2.InnerRow()) && (b1.b() > b2.b()) ) ;
  }
  static bool CompareOuterRow (const AliHLTTPCCABorderTrack &b1, const AliHLTTPCCABorderTrack &b2) {
    return (b1.OuterRow() < b2.OuterRow())/* || ( (b1.OuterRow() == b2.OuterRow()) && (b1.b() > b2.b()) )*/ ;
  }

  void UnpackSlices();
  void Merging(int number=0);

  void MakeBorderTracks(AliHLTTPCCABorderTrack B[], unsigned short &nB, unsigned char &iSlice );
  void MergeBorderTracks( AliHLTTPCCABorderTrack B1[], int N1, int iSlice1, AliHLTTPCCABorderTrack B2[], int N2, int iSlice2, int number,
                            unsigned short FirstTrIR[], unsigned short LastTrIR[]);

  static const int fgkNSlices = AliHLTTPCCAParameters::NumberOfSlices;       //* N slices
  static       int fgDoNotMergeBorders;
  AliHLTTPCCAParam fSliceParam;           //* slice parameters (geometry, calibr, etc.)
  const AliHLTTPCCASliceOutput *fkSlices[fgkNSlices]; //* array of input slice tracks
  AliHLTTPCCATracker *slices[fgkNSlices]; //* array of input slice tracks
  int fMaxClusterInfos;                   //* booked size of fClusterInfos array
  AliHLTTPCCAClusterInfo *fClusterInfos;  //* information about track clusters

  int fMaxTrackInfos;  //* booked size of fTrackInfos array
  AliHLTTPCCASliceTrackInfo *fTrackInfos; //* additional information for slice tracks
  int fSliceTrackInfoStart[fgkNSlices];   //* slice starting index in fTrackInfos array;
  int fSliceNTrackInfos[fgkNSlices];      //* N of slice track infos in fTrackInfos array;

  AliHLTTPCCAMergerOutput *fOutput;       //* array of output merged tracks

  static const int fNTimers = 8;
  float fTimers[fNTimers];

//  AliHLTTPCCAHitMemory fHitMemory;
};

#include "AliHLTTPCCATrackParamVector.h"

inline void AliHLTTPCCAMerger::ConvertTrackParamToVector( AliHLTTPCCATrackParam t0[ushort_v::Size], AliHLTTPCCATrackParamVector &t, int &nTracksV)
{
  sfloat_v tmpVec;
  short_v tmpVecShort;
  sfloat_v::Memory tmpFloat;
  short_v::Memory tmpShort;

  for(int iV=0; iV < nTracksV; iV++) tmpFloat[iV] = t0[iV].X();
  tmpVec.load( tmpFloat );
  t.SetX(tmpVec);
  for(int iV=0; iV < nTracksV; iV++) tmpFloat[iV] = t0[iV].SignCosPhi();
  tmpVec.load( tmpFloat );
  t.SetSignCosPhi(tmpVec);

  for(int iP=0; iP<5; iP++)
  {
    for(int iV=0; iV < nTracksV; iV++) tmpFloat[iV] = t0[iV].Par()[iP];
    tmpVec.load( tmpFloat );
    t.SetPar(iP,tmpVec);
  }
  for(int iC=0; iC<15; iC++)
  {
    for(int iV=0; iV < nTracksV; iV++) tmpFloat[iV] = t0[iV].Cov()[iC];
    tmpVec.load( tmpFloat );
    t.SetCov(iC,tmpVec);
  }
  for(int iV=0; iV < nTracksV; iV++) tmpFloat[iV] = t0[iV].Chi2();
  tmpVec.load( tmpFloat );
  t.SetChi2(tmpVec);
  for(int iV=0; iV < nTracksV; iV++) tmpShort[iV] = t0[iV].NDF();
  tmpVecShort.load( tmpShort );
  t.SetNDF(tmpVecShort);
}

inline void AliHLTTPCCAMerger::InvertCholetsky(sfloat_v a[15])
{
  sfloat_v d[5], uud, u[5][5];
  for(int i=0; i<5; i++) 
  {
    d[i]=0.f;
    for(int j=0; j<5; j++) 
      u[i][j]=0.;
  }

  for(int i=0; i<5; i++)
  {
    uud=0.;
    for(int j=0; j<i; j++) 
      uud += u[j][i]*u[j][i]*d[j];
    uud = a[i*(i+3)/2] - uud;
    sfloat_m small_val = CAMath::Abs(uud)<1.e-12f;
    uud(small_val) = 1.e-12f;
    d[i] = uud/CAMath::Abs(uud);
    u[i][i] = sqrt(CAMath::Abs(uud));

    for(int j=i+1; j<5; j++) 
    {
      uud = 0.;
      for(int k=0; k<i; k++)
        uud += u[k][i]*u[k][j]*d[k];
      uud = a[j*(j+1)/2+i] - uud;
      u[i][j] = d[i]/u[i][i]*uud;
    }
  }

  sfloat_v u1[5];

  for(int i=0; i<5; i++)
  {
    u1[i] = u[i][i];
    u[i][i] = 1.f/u[i][i];
  }
  for(int i=0; i<4; i++)
  {
    u[i][i+1] = - u[i][i+1]*u[i][i]*u[i+1][i+1];
  }
  for(int i=0; i<3; i++)
  {
    u[i][i+2] = u[i][i+1]*u1[i+1]*u[i+1][i+2]-u[i][i+2]*u[i][i]*u[i+2][i+2];
  }
  for(int i=0; i<2; i++)
  {
    u[i][i+3] = u[i][i+2]*u1[i+2]*u[i+2][i+3] - u[i][i+3]*u[i][i]*u[i+3][i+3];
    u[i][i+3] -= u[i][i+1]*u1[i+1]*(u[i+1][i+2]*u1[i+2]*u[i+2][i+3] - u[i+1][i+3]);
  }
  u[0][4] = u[0][2]*u1[2]*u[2][4] - u[0][4]*u[0][0]*u[4][4];
  u[0][4] += u[0][1]*u1[1]*(u[1][4] - u[1][3]*u1[3]*u[3][4] - u[1][2]*u1[2]*u[2][4]);
  u[0][4] += u[3][4]*u1[3]*(u[0][3] - u1[2]*u[2][3]*(u[0][2] - u[0][1]*u1[1]*u[1][2]));

  for(int i=0; i<5; i++)
    a[i+10] = u[i][4]*d[4]*u[4][4];
  for(int i=0; i<4; i++)
    a[i+6] = u[i][3]*u[3][3]*d[3] + u[i][4]*u[3][4]*d[4];
  for(int i=0; i<3; i++)
    a[i+3] = u[i][2]*u[2][2]*d[2] + u[i][3]*u[2][3]*d[3] + u[i][4]*u[2][4]*d[4];
  for(int i=0; i<2; i++)
    a[i+1] = u[i][1]*u[1][1]*d[1] + u[i][2]*u[1][2]*d[2] + u[i][3]*u[1][3]*d[3] + u[i][4]*u[1][4]*d[4];
  a[0] = u[0][0]*u[0][0]*d[0] + u[0][1]*u[0][1]*d[1] + u[0][2]*u[0][2]*d[2] + u[0][3]*u[0][3]*d[3] + u[0][4]*u[0][4]*d[4];
}

inline void AliHLTTPCCAMerger::MultiplySS(sfloat_v const C[15], sfloat_v const V[15], sfloat_v K[5][5])
{
  K[0][0] = C[0]*V[ 0] + C[1]*V[ 1] + C[3]*V[ 3] + C[6]*V[ 6] + C[10]*V[10];
  K[0][1] = C[0]*V[ 1] + C[1]*V[ 2] + C[3]*V[ 4] + C[6]*V[ 7] + C[10]*V[11];
  K[0][2] = C[0]*V[ 3] + C[1]*V[ 4] + C[3]*V[ 5] + C[6]*V[ 8] + C[10]*V[12];
  K[0][3] = C[0]*V[ 6] + C[1]*V[ 7] + C[3]*V[ 8] + C[6]*V[ 9] + C[10]*V[13];
  K[0][4] = C[0]*V[10] + C[1]*V[11] + C[3]*V[12] + C[6]*V[13] + C[10]*V[14];

  K[1][0] = C[1]*V[ 0] + C[2]*V[ 1] + C[4]*V[ 3] + C[7]*V[ 6] + C[11]*V[10];
  K[1][1] = C[1]*V[ 1] + C[2]*V[ 2] + C[4]*V[ 4] + C[7]*V[ 7] + C[11]*V[11];
  K[1][2] = C[1]*V[ 3] + C[2]*V[ 4] + C[4]*V[ 5] + C[7]*V[ 8] + C[11]*V[12];
  K[1][3] = C[1]*V[ 6] + C[2]*V[ 7] + C[4]*V[ 8] + C[7]*V[ 9] + C[11]*V[13];
  K[1][4] = C[1]*V[10] + C[2]*V[11] + C[4]*V[12] + C[7]*V[13] + C[11]*V[14];

  K[2][0] = C[3]*V[ 0] + C[4]*V[ 1] + C[5]*V[ 3] + C[8]*V[ 6] + C[12]*V[10];
  K[2][1] = C[3]*V[ 1] + C[4]*V[ 2] + C[5]*V[ 4] + C[8]*V[ 7] + C[12]*V[11];
  K[2][2] = C[3]*V[ 3] + C[4]*V[ 4] + C[5]*V[ 5] + C[8]*V[ 8] + C[12]*V[12];
  K[2][3] = C[3]*V[ 6] + C[4]*V[ 7] + C[5]*V[ 8] + C[8]*V[ 9] + C[12]*V[13];
  K[2][4] = C[3]*V[10] + C[4]*V[11] + C[5]*V[12] + C[8]*V[13] + C[12]*V[14];

  K[3][0] = C[6]*V[ 0] + C[7]*V[ 1] + C[8]*V[ 3] + C[9]*V[ 6] + C[13]*V[10];
  K[3][1] = C[6]*V[ 1] + C[7]*V[ 2] + C[8]*V[ 4] + C[9]*V[ 7] + C[13]*V[11];
  K[3][2] = C[6]*V[ 3] + C[7]*V[ 4] + C[8]*V[ 5] + C[9]*V[ 8] + C[13]*V[12];
  K[3][3] = C[6]*V[ 6] + C[7]*V[ 7] + C[8]*V[ 8] + C[9]*V[ 9] + C[13]*V[13];
  K[3][4] = C[6]*V[10] + C[7]*V[11] + C[8]*V[12] + C[9]*V[13] + C[13]*V[14];

  K[4][0] = C[10]*V[ 0] + C[11]*V[ 1] + C[12]*V[ 3] + C[13]*V[ 6] + C[14]*V[10];
  K[4][1] = C[10]*V[ 1] + C[11]*V[ 2] + C[12]*V[ 4] + C[13]*V[ 7] + C[14]*V[11];
  K[4][2] = C[10]*V[ 3] + C[11]*V[ 4] + C[12]*V[ 5] + C[13]*V[ 8] + C[14]*V[12];
  K[4][3] = C[10]*V[ 6] + C[11]*V[ 7] + C[12]*V[ 8] + C[13]*V[ 9] + C[14]*V[13];
  K[4][4] = C[10]*V[10] + C[11]*V[11] + C[12]*V[12] + C[13]*V[13] + C[14]*V[14];
}

inline void AliHLTTPCCAMerger::MultiplyMS(sfloat_v const C[5][5], sfloat_v const V[15], sfloat_v K[15])
{
  K[0] = C[0][0]*V[0] + C[0][1]*V[1] + C[0][2]*V[3] + C[0][3]*V[6] + C[0][4]*V[10];

  K[1] = C[1][0]*V[0] + C[1][1]*V[1] + C[1][2]*V[3] + C[1][3]*V[6] + C[1][4]*V[10];
  K[2] = C[1][0]*V[1] + C[1][1]*V[2] + C[1][2]*V[4] + C[1][3]*V[7] + C[1][4]*V[11];

  K[3] = C[2][0]*V[0] + C[2][1]*V[1] + C[2][2]*V[3] + C[2][3]*V[6] + C[2][4]*V[10];
  K[4] = C[2][0]*V[1] + C[2][1]*V[2] + C[2][2]*V[4] + C[2][3]*V[7] + C[2][4]*V[11];
  K[5] = C[2][0]*V[3] + C[2][1]*V[4] + C[2][2]*V[5] + C[2][3]*V[8] + C[2][4]*V[12];

  K[6] = C[3][0]*V[0] + C[3][1]*V[1] + C[3][2]*V[3] + C[3][3]*V[6] + C[3][4]*V[10];
  K[7] = C[3][0]*V[1] + C[3][1]*V[2] + C[3][2]*V[4] + C[3][3]*V[7] + C[3][4]*V[11];
  K[8] = C[3][0]*V[3] + C[3][1]*V[4] + C[3][2]*V[5] + C[3][3]*V[8] + C[3][4]*V[12];
  K[9] = C[3][0]*V[6] + C[3][1]*V[7] + C[3][2]*V[8] + C[3][3]*V[9] + C[3][4]*V[13];

  K[10] = C[4][0]*V[ 0] + C[4][1]*V[ 1] + C[4][2]*V[ 3] + C[4][3]*V[ 6] + C[4][4]*V[10];
  K[11] = C[4][0]*V[ 1] + C[4][1]*V[ 2] + C[4][2]*V[ 4] + C[4][3]*V[ 7] + C[4][4]*V[11];
  K[12] = C[4][0]*V[ 3] + C[4][1]*V[ 4] + C[4][2]*V[ 5] + C[4][3]*V[ 8] + C[4][4]*V[12];
  K[13] = C[4][0]*V[ 6] + C[4][1]*V[ 7] + C[4][2]*V[ 8] + C[4][3]*V[ 9] + C[4][4]*V[13];
  K[14] = C[4][0]*V[10] + C[4][1]*V[11] + C[4][2]*V[12] + C[4][3]*V[13] + C[4][4]*V[14];
}

inline void AliHLTTPCCAMerger::MultiplySR(sfloat_v const C[15], sfloat_v const r_in[5], sfloat_v r_out[5])
{
  r_out[0] = r_in[0]*C[ 0] + r_in[1]*C[ 1] + r_in[2]*C[ 3] +r_in[3]*C[ 6] + r_in[4]*C[10];
  r_out[1] = r_in[0]*C[ 1] + r_in[1]*C[ 2] + r_in[2]*C[ 4] +r_in[3]*C[ 7] + r_in[4]*C[11];
  r_out[2] = r_in[0]*C[ 3] + r_in[1]*C[ 4] + r_in[2]*C[ 5] +r_in[3]*C[ 8] + r_in[4]*C[12];
  r_out[3] = r_in[0]*C[ 6] + r_in[1]*C[ 7] + r_in[2]*C[ 8] +r_in[3]*C[ 9] + r_in[4]*C[13];
  r_out[4] = r_in[0]*C[10] + r_in[1]*C[11] + r_in[2]*C[12] +r_in[3]*C[13] + r_in[4]*C[14];
}

inline void AliHLTTPCCAMerger::FilterTracks(sfloat_v const r[5], sfloat_v const C[15],
                                            sfloat_v const m[5], sfloat_v const V[15],
                                            sfloat_v R[5], sfloat_v W[15], sfloat_v &chi2, const sfloat_m &mask)
{
  sfloat_v S[15];
  for(int i=0; i<15; i++)
  {
    W[i] = C[i];
    S[i] = C[i] + V[i];
  }
  for(int i=0; i<5; i++)
    R[i] = r[i];

  InvertCholetsky(S);
  
  sfloat_v K[5][5];
  MultiplySS(C,S,K);
  sfloat_v dzeta[5];
  for(int i=0; i<5; i++) dzeta[i] = m[i] - r[i];
  sfloat_v KC[15];
  MultiplyMS(K,C,KC);
  for(int i=0; i< 15; i++)
    W[i](mask) -= KC[i];

  sfloat_v kd(Vc::Zero);
  for(int i=0; i<5; i++)
  {
    kd = 0.f;
    for(int j=0; j<5; j++)
      kd += K[i][j]*dzeta[j];
    R[i](mask) += kd;
  }
  sfloat_v S_dzeta[5];
  MultiplySR(S, dzeta, S_dzeta);
  chi2(mask) = dzeta[0]*S_dzeta[0] + dzeta[1]*S_dzeta[1] + dzeta[2]*S_dzeta[2] + dzeta[3]*S_dzeta[3] + dzeta[4]*S_dzeta[4];
}

class AliHLTTPCCAClusterInfo
{

 public:

  unsigned char  ISlice()    const { return fISlice;    }
  unsigned char  IRow()      const { return fIRow;      }
  unsigned int  IClu()      const { return fIClu;      }
  float X()         const { return fX;         }
  float Y()         const { return fY;         }
  float Z()         const { return fZ;         }

  void SetISlice    ( unsigned char v  ) { fISlice    = v; }
  void SetIRow      ( unsigned char v  ) { fIRow      = v; }
  void SetIClu      ( unsigned int v  ) { fIClu      = v; }
  void SetX         ( float v ) { fX         = v; }
  void SetY         ( float v ) { fY         = v; }
  void SetZ         ( float v ) { fZ         = v; }

 public:

  unsigned char fISlice;            // slice number
  unsigned char fIRow;              // row number
  unsigned int fIClu;              // cluster number
  float fX;                // x position (slice coord.system)
  float fY;                // y position (slice coord.system)
  float fZ;                // z position (slice coord.system)
};

class TrackHitsCompare {
    //  typedef AliHLTTPCCAMerger::AliHLTTPCCAClusterInfo AliHLTTPCCAClusterInfo;
 public:
  TrackHitsCompare(AliHLTTPCCAClusterInfo *clusterInfos): fClusterInfos(clusterInfos) {};

  bool operator()(int i, int j) { return fClusterInfos[i].X() < fClusterInfos[j].X(); };
 private:
  const AliHLTTPCCAClusterInfo *fClusterInfos;
};

#endif
