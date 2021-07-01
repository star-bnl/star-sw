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

#ifndef ALIHLTTPCCAMERGER_H
#define ALIHLTTPCCAMERGER_H

#include "AliHLTTPCCADef.h"
#include "AliHLTTPCCAParam.h"
#include "AliHLTTPCCATrackParam.h"
#include "AliHLTTPCCASliceTrack.h"
#include "AliHLTTPCCASliceTrackVector.h"

#include "AliHLTTPCCATrackParamVector.h"

#include <vector>
#include <map>

#if !defined(HLTCA_GPUCODE)
#include <iostream>
#endif

//#define TETA

class AliHLTTPCCATrackParamVector;
class AliHLTTPCCASliceTrack;
class AliHLTTPCCASliceOutput;
class AliHLTTPCCAMergedTrack;
class AliHLTTPCCAMergerOutput;
class AliHLTTPCCATracker;

/**
 * @class AliHLTTPCCAMerger
 *
 */

class AliHLTTPCCAClusterInfo;
class SliceTrackInfoPTSimpleSOA;
class AliHLTTPCCAMerger
{
  class AliHLTTPCCABorderTrack
  {
   public:

    int   TrackID() const { return fTrackID; }
    float b()       const{ return fb;        }
    float bErr2()   const{ return fbErr2;    }
    float p()       const{ return fp;        }
    float pErr2()   const{ return fpErr2;    }
    unsigned int InnerRow() const {return fInnerRow;}
    unsigned int OuterRow() const {return fOuterRow;}

    void SetInnerRow(unsigned int v) {fInnerRow = v;}
    void SetOuterRow(unsigned int v) {fOuterRow = v;}
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
    unsigned int fInnerRow;
    unsigned int fOuterRow;
  };

 public:
  class AliHLTTPCCASliceTrackInfo;
  class AliHLTTPCCASliceTrackInfoV;
  // ---
  class SliceTrackInfoPT;
//  class SliceTrackInfoPTSimpleSOA;
  // ---

  AliHLTTPCCAMerger();
  ~AliHLTTPCCAMerger();

  AliHLTTPCCAMerger( const AliHLTTPCCAMerger& );
  const AliHLTTPCCAMerger &operator=( const AliHLTTPCCAMerger& ) const;

  void Clear();

    // accsessors
  void SetSliceParam( const AliHLTTPCCAParam &v ) { fSliceParam = v; }
//  void SetSliceData( int index, const AliHLTTPCCASliceOutput *SliceData );
  void SetSliceData( int index, AliHLTTPCCASliceOutput *SliceData );
  void SetSlices ( int i, AliHLTTPCCATracker *sl );
  static void SetDoNotMergeBorders(int i = 0) {fgDoNotMergeBorders = i;}

  const AliHLTTPCCAMergerOutput * Output() const { return fOutput; }
  AliHLTTPCCAMergerOutput * Output() { return fOutput; }

  int NTimers() { return fNTimers; }
  float Timer( int i ) { return fTimers[i]; };

    // process
  void UnpackSlices();
  void Reconstruct();
#ifdef CALC_DCA_ON
  vector<point_3d>& GetLeftDCA() { return dca_left; }
  vector<point_3d>& GetRightDCA() { return dca_right; }
#endif
 private:
  void FindNeighbourTracks(int number=0);
  void Merging(int number=0);
  float_m AddNeighbour( const uint_v& jIndexes, const int& nVecElements, const float_m& isNeighbour,
  int hits[2000][uint_v::Size], uint_v& firstHit, AliHLTTPCCATrackParamVector& vStartPoint, AliHLTTPCCATrackParamVector& vEndPoint, float_v& vStartAlpha, float_v& vEndAlpha, uint_v& vNHits );

  void MergingFix(int number=0);

#if 0
  // --- PT ---
  void UnpackSlicesPT();
  void ClearMemoryPT();
  void MergeUpPT( int s );
  void MergingPT( int number );
  bool AddNeighbourPTSimple( int_v iTr, int_v jTr, int hits[2000][uint_v::Size], uint_v& vNHits );
  void MergingPTmultimap();
  // ---
#endif

  void MakeBorderTracks( AliHLTTPCCABorderTrack B[], unsigned int &nB, unsigned char &iSlice );

  void MergeBorderTracks( AliHLTTPCCABorderTrack B1[], int N1, unsigned int iSlice1, AliHLTTPCCABorderTrack B2[], int N2, unsigned int iSlice2, int number,
                            const unsigned int FirstTrIR[], const unsigned int LastTrIR[]);
  void FindMinMaxIndex( int N2, const unsigned int FirstTrIR[], const unsigned int LastTrIR[], int minIRow, int maxIRow, int &min, int &max );
  void CheckTracksMatch( int number,
  const AliHLTTPCCATrackParamVector &InParT1, const AliHLTTPCCATrackParamVector &OutParT1, const float_v &OutAlphaT1, const float_v &InAlphaT1,
  const AliHLTTPCCATrackParamVector &InParT2, const AliHLTTPCCATrackParamVector &OutParT2, const float_v &OutAlphaT2, const float_v &InAlphaT2,
  const float_v dxArr[4], const float_v dyArr[4], const float_v &sinS1_E2v, const float_v &sinS2_E1v,
  float_v &minL2v, const float &bestChi2, float_v& min_chi2, float_m& active );

    // helping functions
  void ConvertPTrackParamToVector( const AliHLTTPCCATrackParam *t0[uint_v::Size], AliHLTTPCCATrackParamVector &t, const int &nTracksV);
  void ConvertPTrackParamToVectorSimple( const AliHLTTPCCATrackParam *t0[uint_v::Size], AliHLTTPCCATrackParamVector &t, const int &nTracksV);

  void ConvertPTrackParamToVectorAdd( AliHLTTPCCATrackParam *t0[uint_v::Size], AliHLTTPCCATrackParamVector &t, const int &nTracksV, float_m mask);

  float_m FitTrack( AliHLTTPCCATrackParamVector &t, float_v &Alpha0V,
                     int hits[2000][uint_v::Size], uint_v &firstHits, uint_v &NTrackHits,
                     int &nTracksV, float_m active0 = float_m(true), bool dir = 1 );
  float_m FitTrackMerged( AliHLTTPCCATrackParamVector &t, float_v &Alpha0V,
                         int hits[100][uint_v::Size], uint_v &firstHits, uint_v &NTrackHits,
                         int &nTracksV, float_m active0 = float_m(true), bool dir = 1 );

  unsigned int HitIndex( const uint_v& firstHits, const uint_v& nHits, bool dir, int iV, unsigned int i ) { // vectorized is slower
    return static_cast<unsigned int>(firstHits[iV]) + static_cast<unsigned int>(dir ? ( nHits[iV] - 1 - i ) : i);
  }

  void InvertCholetsky(float_v a[15]);
  void MultiplySS(const float_v C[15], const float_v V[15], float_v K[5][5]);
  void MultiplyMS(const float_v C[5][5], const float_v V[15], float_v K[15]);
  void MultiplySR(const float_v C[15], const float_v r_in[5], float_v r_out[5]);
  void FilterTracks(const float_v r[5], const float_v C[15], const  float_v m[5], const float_v V[15],
                    float_v R[5],       float_v W[15],       float_v &chi2, const float_m &mask = float_m( true ));

  static bool CompareInnerRow (const AliHLTTPCCABorderTrack &b1, const AliHLTTPCCABorderTrack &b2) {
    return (b1.InnerRow() > b2.InnerRow()) || ( (b1.InnerRow() == b2.InnerRow()) && (b1.b() > b2.b()) ) ;
  }
  static bool CompareOuterRow (const AliHLTTPCCABorderTrack &b1, const AliHLTTPCCABorderTrack &b2) {
    return (b1.OuterRow() < b2.OuterRow())/* || ( (b1.OuterRow() == b2.OuterRow()) && (b1.b() > b2.b()) )*/ ;
  }


  static const int fgkNSlices = AliHLTTPCCAParameters::NumberOfSlices;       //* N slices
  static       int fgDoNotMergeBorders;
  AliHLTTPCCAParam fSliceParam;           //* slice parameters (geometry, calibr, etc.)
//  const AliHLTTPCCASliceOutput *fkSlices[fgkNSlices]; //* array of input slice tracks
  AliHLTTPCCASliceOutput *fkSlices[fgkNSlices]; //* array of input slice tracks
  AliHLTTPCCATracker *slices[fgkNSlices]; //* array of input slice tracks
  int fMaxClusterInfos;                   //* booked size of fClusterInfos array
  AliHLTTPCCAClusterInfo *fClusterInfos;  //* information about track clusters

  int fMaxTrackInfos;  //* booked size of fTrackInfos array
  Vc::vector<AliHLTTPCCASliceTrackInfo> fTrackInfos; //* additional information for slice tracks
  int fSliceTrackInfoStart[fgkNSlices];   //* slice starting index in fTrackInfos array;
  int fSliceNTrackInfos[fgkNSlices];      //* N of slice track infos in fTrackInfos array;

  AliHLTTPCCAMergerOutput *fOutput;       //* array of output merged tracks

#if 0
  int GetFirstMappedTrackID( unsigned int islice, unsigned int irow ) {
    for( int iRow = irow; iRow < irow + sMaxGape; iRow++ ) {
      if( iRow > 45 ) return -1;
      if( sTrackMap[islice][iRow] != -1 ) return vTrackID[sTrackMap[islice][iRow]];
    }
    return -1;
  }
  int GetFirstMappedTrackN( unsigned int islice, unsigned int irow ) {
    for( int iRow = irow; iRow < irow + sMaxGape; iRow++ ) {
      if( iRow > 45 ) return -1;
      if( sTrackMap[islice][iRow] != -1 ) return sTrackMap[islice][iRow];
    }
    return -1;
  }

  SliceTrackInfoPTSimpleSOA *fptTrackInfoPT;
  float* vInnerX;
  float* vOuterX;
  float* vInnerY;
  float* vOuterY;
  float* vInnerZ;
  float* vOuterZ;
  float* vTeta;
  int* vTrackID;
  int* vLastRow;
  int fptSliceFirstTrack[fgkNSlices + 1];
  int fptSliceFirstTrackV[fgkNSlices + 1];
  int fptSliceFirstHit[fgkNSlices + 1];
  int sTrackMap[fgkNSlices][46];
  int sMaxGape;
  int tracksOnSlice[fgkNSlices];
  int fTracksTotal;
  struct TrSort {

    int nHits, trId, iSlice;
    bool operator() ( const TrSort& a, const TrSort& b ) { return (a.nHits > b.nHits);}
    static bool trComp( const TrSort& a, const TrSort& b ) { return (a.nHits > b.nHits); }
  };
  TrSort* fTrList;

  Vc::vector<AliHLTTPCCASliceTrackInfoV> fTrackInfosV; //* additional information for slice tracks

  struct HitInfo {
    int id;
    int slice;
    HitInfo( int ID, int SLICE ) { id = ID; slice = SLICE; }
    bool operator< ( const HitInfo& a ) const { return (this->id < a.id); }
  };
  std::multimap<HitInfo, HitInfo> fTrackLinks;
  std::multimap<HitInfo, HitInfo> fTrackLinksV;
  HitInfo* sliceTrackId;
  unsigned short fMaxMerged[2];
  struct TrMerge {
    int nTracks, iTrack[3], iSlice[3];
  };
#endif

#ifdef CALC_DCA_ON
  std::vector<point_3d> dca_left;
  std::vector<point_3d> dca_right;
#endif

  int fNMergedSegments;
  int fNMergedSegmentClusters;

  static const int fNTimers = 8;
  float fTimers[fNTimers];
};

class AliHLTTPCCAMerger::AliHLTTPCCASliceTrackInfoV
{
  public:
    AliHLTTPCCASliceTrackInfoV()
      : fNClusters()
      , fFirstClusterRef()
      , fNSegments(0)
      , fUsed()
      , fActive()
    {}
    AliHLTTPCCASliceTrackInfoV( AliHLTTPCCASliceTrackVector& track )
      : fNSegments(1)
      , fUsed(int_v(0))
      , fActive(track.Active())
    {
      fNClusters[0] = track.NClusters();
      fFirstClusterRef[0] = track.FirstClusterRef();
    }

    void SetNClustersV( int_v ncl, int i ) { fNClusters[i] = ncl; }
    void SetNClusters( int ncl, int iV, int i ) { fNClusters[i][iV] = ncl; }
    int_v NClusters( int i ) { return fNClusters[i]; }

    void SetFirstClusterRefV( int_v fcl, int i ) { fFirstClusterRef[i] = fcl; }
    void SetFirstClusterRef( int fcl, int iV, int i ) { fFirstClusterRef[i][iV] = fcl; }
    int_v FirstClusterRef( int i ) { return fFirstClusterRef[i]; }

    void AddSegment() { fNSegments++; }
    int NSegments() { return fNSegments; }

    void SetUsedV( int_v u ) { fUsed = u; }
    void SetUsed( int u, int i ) { fUsed[i] = u; }
    int_v Used() { return fUsed; }

    void SetActiveV( int_m a ) { fActive = a; }
    void SetActive( bool a, int i ) { fActive[i] = a; }
    int_m Active() { return fActive; }

  private:
   int_v fNClusters[3];  //Clusters number of the track
   int_v fFirstClusterRef[3];  // index of the first track cluster in the global cluster array (AliHLTTPCCAMerger::fClusterInfos)
   int fNSegments;
   int_v fUsed;
   int_m fActive;
};

class AliHLTTPCCAMerger::AliHLTTPCCASliceTrackInfo
{
 public:
  AliHLTTPCCASliceTrackInfo()
    : ChiPrev(1e10f)
    , ChiNext(1e10f)
    , fInnerRow(-1)
    , fOuterRow(-1)
    , fInnerAlpha(0.f)
    , fOuterAlpha(0.f)
    , fNClusters(0)
    , fPrevNeighbour(-1)
    , fNextNeighbour(-1)
    , fSlicePrevNeighbour(-1)
    , fSliceNextNeighbour(-1)
    , fUsed(false)
    , fMerged(false)
    , fInnerParam()
    , fOuterParam()
#if 0
    , fPrevdTeta(-1)
#endif
 {}

    const AliHLTTPCCATrackParam &InnerParam() const { return fInnerParam;      }
    const AliHLTTPCCATrackParam &OuterParam() const { return fOuterParam;      }
    float InnerAlpha() const { return fInnerAlpha;      }
    float OuterAlpha() const { return fOuterAlpha;      }
    unsigned int   NClusters() const { return fNClusters;       }
    int   FirstClusterRef() const { return fFirstClusterRef; }
    int   PrevNeighbour()  const { return fPrevNeighbour;   }
    int   NextNeighbour()  const { return fNextNeighbour;   }
    unsigned int SlicePrevNeighbour() const { return fSlicePrevNeighbour;   }
    unsigned int SliceNextNeighbour() const { return fSliceNextNeighbour;   }
    int Used()                        const { return fUsed;            }

    void SetInnerParam( const AliHLTTPCCATrackParam &v ) { fInnerParam = v;      }
    void SetOuterParam( const AliHLTTPCCATrackParam &v ) { fOuterParam = v;      }
    void SetInnerAlpha( float v )                      { fInnerAlpha = v;      }
    void SetOuterAlpha( float v )                      { fOuterAlpha = v;      }
    void SetNClusters ( unsigned int v )                        { fNClusters = v;       }
    void SetFirstClusterRef( int v )                   { fFirstClusterRef = v; }
    void SetPrevNeighbour( int v )                     { fPrevNeighbour = v;   }
    void SetNextNeighbour( int v )                     { fNextNeighbour = v;   }
    void SetSlicePrevNeighbour( unsigned char v )                     { fSlicePrevNeighbour = v;   }
    void SetSliceNextNeighbour( unsigned char v )                     { fSliceNextNeighbour = v;   }
    void SetUsed( int v )                             { fUsed = v; }

    void SetMerged() { fMerged = true; }
    bool IsMerged() const { return fMerged; }
#if 0
    float PrevdTeta() { return fPrevdTeta; }			//TODO: add next dTeta
    void SetPrevdTeta( float dT ) { fPrevdTeta = dT; }
    float PrevdPhi() { return fPrevdPhi; }			//TODO: add next dTeta
    void SetPrevdPhi( float dT ) { fPrevdPhi = dT; }
#endif
    void SetId( int id ) { fId = id; }
    int Id() { return fId; }
#ifdef DO_TPCCATRACKER_EFF_PERFORMANCE
    int orig_track_id;
    unsigned char fSlice;
    int number;
#endif // DO_TPCCATRACKER_EFF_PERFORMANCE

   public:

    float ChiPrev; //characteristic of the link quality to the inner neighbour (for overlaped tracks it is chi2, for not overlaped - distance between tracks)
    float ChiNext; //characteristic of the link quality to the outer neighbour
    unsigned char fInnerRow; // number of the inner row of the track
    unsigned char fOuterRow; // number of the outer row of the track

    float fInnerAlpha;               // The angle of the sector, where inner parameters are
    float fOuterAlpha;               // The angle of the sector, where outers parameters are
    unsigned int fNClusters;  //Clusters number of the track
    int fFirstClusterRef;  // index of the first track cluster in the global cluster array (AliHLTTPCCAMerger::fClusterInfos)
    int fPrevNeighbour; // The number of the inner (previous) neighbour in the tracks array (AliHLTTPCCAMerger::fTrackInfos)
    int fNextNeighbour; // The number of the outer (previous) neighbour in the tracks array (AliHLTTPCCAMerger::fTrackInfos)
    unsigned int fSlicePrevNeighbour; // The number of the sector, which contains inner neighbour
    unsigned int fSliceNextNeighbour; // The number of the sector, which contains outer neighbour
    int fUsed;            // is the slice track already merged (=1 -> merged, 0 -> not merged)
#if 0
    float fPrevdTeta;
    float fPrevdPhi;
    // ---
    float QPt0;
    float DzDs0;
    float QPt1;
    float DzDs1;
#endif
    int fId;
#ifdef CALC_DCA_ON
    float DCA[3];
#endif

    bool fMerged;
 private:

    AliHLTTPCCATrackParam fInnerParam; // Parameters of the track at the inner point
    AliHLTTPCCATrackParam fOuterParam; // Parameters of the track at the outer point
};


//#include "AliHLTTPCCATrackParamVector.h"

inline void AliHLTTPCCAMerger::ConvertPTrackParamToVector( const AliHLTTPCCATrackParam *t0[uint_v::Size], AliHLTTPCCATrackParamVector &t, const int &nTracksV)
{
  float_v tmpFloat;
  int_v tmpShort;

  for(int iV=0; iV < nTracksV; iV++) if(t0[iV]) tmpFloat[iV] = t0[iV]->X();
  t.SetX(tmpFloat);
  for(int iV=0; iV < nTracksV; iV++) if(t0[iV]) tmpFloat[iV] = t0[iV]->SignCosPhi();
  t.SetSignCosPhi(tmpFloat);

  for(int iP=0; iP<5; iP++)
  {
    for(int iV=0; iV < nTracksV; iV++) if(t0[iV]) tmpFloat[iV] = t0[iV]->Par()[iP];
    t.SetPar(iP,tmpFloat);
  }
  for(int iC=0; iC<15; iC++)
  {
    for(int iV=0; iV < nTracksV; iV++) if(t0[iV]) tmpFloat[iV] = t0[iV]->Cov()[iC];
    t.SetCov(iC,tmpFloat);
  }
  for(int iV=0; iV < nTracksV; iV++) if(t0[iV]) tmpFloat[iV] = t0[iV]->Chi2();
  t.SetChi2(tmpFloat);
  for(int iV=0; iV < nTracksV; iV++) if(t0[iV]) tmpShort[iV] = t0[iV]->NDF();
  t.SetNDF(tmpShort);
}

inline void AliHLTTPCCAMerger::ConvertPTrackParamToVectorAdd( AliHLTTPCCATrackParam *t0[uint_v::Size], AliHLTTPCCATrackParamVector &t, const int &nTracksV, float_m mask)
{
  float_v tmpFloat;
  int_v tmpShort;

  for(int iV=0; iV < nTracksV; iV++) {
    if(mask[iV]) tmpFloat[iV] = t0[iV]->X();
    else tmpFloat[iV] = t.X()[iV];
  }
  t.SetX(tmpFloat);
  for(int iV=0; iV < nTracksV; iV++) {
      if(mask[iV]) tmpFloat[iV] = t0[iV]->SignCosPhi();
      else tmpFloat[iV] = t.SignCosPhi()[iV];
  }
  t.SetSignCosPhi(tmpFloat);

  for(int iP=0; iP<5; iP++)
  {
    for(int iV=0; iV < nTracksV; iV++) {
	if(mask[iV]) tmpFloat[iV] = t0[iV]->Par()[iP];
	else tmpFloat[iV] = t.Par()[iP][iV];
    }
    t.SetPar(iP,tmpFloat);
  }
  for(int iC=0; iC<15; iC++)
  {
    for(int iV=0; iV < nTracksV; iV++) {
	if(mask[iV]) tmpFloat[iV] = t0[iV]->Cov()[iC];
	else tmpFloat[iV] = t.Cov()[iC][iV];
    }
    t.SetCov(iC,tmpFloat);
  }
  for(int iV=0; iV < nTracksV; iV++) {
      if(mask[iV]) tmpFloat[iV] = t0[iV]->Chi2();
      else tmpFloat[iV] = t.Chi2()[iV];
  }
  t.SetChi2(tmpFloat);
  for(int iV=0; iV < nTracksV; iV++) {
      if(mask[iV]) tmpShort[iV] = t0[iV]->NDF();
      else tmpShort[iV] = t.NDF()[iV];
  }
  t.SetNDF(tmpShort);
}

inline void AliHLTTPCCAMerger::ConvertPTrackParamToVectorSimple( const AliHLTTPCCATrackParam *t0[uint_v::Size], AliHLTTPCCATrackParamVector &t, const int &nTracksV)
{
  float_v tmpFloat;
  int_v tmpShort;

  for(int iV=0; iV < nTracksV; iV++) if(t0[iV]) tmpFloat[iV] = t0[iV]->X();
  t.SetX(tmpFloat);
  for(int iV=0; iV < nTracksV; iV++) if(t0[iV]) tmpFloat[iV] = t0[iV]->SignCosPhi();
  t.SetSignCosPhi(tmpFloat);

  for(int iP=0; iP<5; iP++)
  {
    for(int iV=0; iV < nTracksV; iV++) if(t0[iV]) tmpFloat[iV] = t0[iV]->Par()[iP];
    t.SetPar(iP,tmpFloat);
  }
  for(int iV=0; iV < nTracksV; iV++) if(t0[iV]) tmpFloat[iV] = t0[iV]->Chi2();
  t.SetChi2(tmpFloat);
  for(int iV=0; iV < nTracksV; iV++) if(t0[iV]) tmpShort[iV] = t0[iV]->NDF();
  t.SetNDF(tmpShort);
}

inline void AliHLTTPCCAMerger::InvertCholetsky(float_v a[15])
{
  float_v d[5], uud, u[5][5];
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
    float_m small_val = CAMath::Abs(uud)<1.e-12f;
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

  float_v u1[5];

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

inline void AliHLTTPCCAMerger::MultiplySS(const float_v C[15], const float_v V[15], float_v K[5][5])
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

inline void AliHLTTPCCAMerger::MultiplyMS(const float_v C[5][5], const float_v V[15], float_v K[15])
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

inline void AliHLTTPCCAMerger::MultiplySR(const float_v C[15], const float_v r_in[5], float_v r_out[5])
{
  r_out[0] = r_in[0]*C[ 0] + r_in[1]*C[ 1] + r_in[2]*C[ 3] +r_in[3]*C[ 6] + r_in[4]*C[10];
  r_out[1] = r_in[0]*C[ 1] + r_in[1]*C[ 2] + r_in[2]*C[ 4] +r_in[3]*C[ 7] + r_in[4]*C[11];
  r_out[2] = r_in[0]*C[ 3] + r_in[1]*C[ 4] + r_in[2]*C[ 5] +r_in[3]*C[ 8] + r_in[4]*C[12];
  r_out[3] = r_in[0]*C[ 6] + r_in[1]*C[ 7] + r_in[2]*C[ 8] +r_in[3]*C[ 9] + r_in[4]*C[13];
  r_out[4] = r_in[0]*C[10] + r_in[1]*C[11] + r_in[2]*C[12] +r_in[3]*C[13] + r_in[4]*C[14];
}

inline void AliHLTTPCCAMerger::FilterTracks(const float_v r[5], const float_v C[15],
                                            const float_v m[5], const float_v V[15],
                                            float_v R[5], float_v W[15], float_v &chi2, const float_m &mask)
{
  float_v S[15];
  for(int i=0; i<15; i++)
  {
    W[i] = C[i];
    S[i] = C[i] + V[i];
  }
  for(int i=0; i<5; i++)
    R[i] = r[i];

  InvertCholetsky(S);

  float_v K[5][5];
  MultiplySS(C,S,K);
  float_v dzeta[5];
  for(int i=0; i<5; i++) dzeta[i] = m[i] - r[i];
  float_v KC[15];
  MultiplyMS(K,C,KC);
  for(int i=0; i< 15; i++)
    W[i](mask) -= KC[i];

  float_v kd(Vc::Zero);
  for(int i=0; i<5; i++)
  {
    kd = 0.f;
    for(int j=0; j<5; j++)
      kd += K[i][j]*dzeta[j];
    R[i](mask) += kd;
  }
  float_v S_dzeta[5];
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

class AliHLTTPCCAMerger::SliceTrackInfoPT
{
 public:
  //
  SliceTrackInfoPT()
    : fInnerRow(-1)
    , fOuterRow(-1)
    , fInnerAlpha(0.f)
    , fPrevNeighbour(-1)
    , fNextNeighbour(-1)
    , fSlicePrevNeighbour(-1)
    , fSliceNextNeighbour(-1)
    , fUsed(false)
    , fInnerParam(){}

  const AliHLTTPCCATrackParam &InnerParam() 	const { return fInnerParam;      }
  float InnerAlpha() 				const { return fInnerAlpha;      }
  int   PrevNeighbour()  			const { return fPrevNeighbour;   }
  int   NextNeighbour()  			const { return fNextNeighbour;   }
  unsigned short SlicePrevNeighbour() 		const { return fSlicePrevNeighbour;   }
  unsigned short SliceNextNeighbour() 		const { return fSliceNextNeighbour;   }
  int Used()                        		const { return fUsed;            }
  unsigned short InnerRow() 			const { return fInnerRow; }
  unsigned short OuterRow() 			const { return fOuterRow; }
  double X( bool i )				const { return fInOutX[i]; }
  double Y( bool i )				const { return fInOutY[i]; }
  double Z( bool i )				const { return fInOutZ[i]; }
  double Teta()					const { return fTeta; }
  double Phi()					const { return fPhi; }
  unsigned short Slice()			const { return fSlice; }

  void SetInnerParam( const AliHLTTPCCATrackParam &v )	{ fInnerParam = v;      }
  void SetInnerAlpha( float v )                     	{ fInnerAlpha = v;      }
  void SetPrevNeighbour( int v )                     	{ fPrevNeighbour = v;   }
  void SetNextNeighbour( int v )                     	{ fNextNeighbour = v;   }
  void SetSlicePrevNeighbour( unsigned short v )         { fSlicePrevNeighbour = v;   }
  void SetSliceNextNeighbour( unsigned short v )         { fSliceNextNeighbour = v;   }
  void SetUsed( bool v )                        	{ fUsed = v;            }
  void SetInnerRow( unsigned short v )			{ fInnerRow = v; }
  void SetOuterRow( unsigned short v )			{ fOuterRow = v; }
  void SetX( bool i, double v )				{ fInOutX[i] = v; }
  void SetY( bool i, double v )				{ fInOutY[i] = v; }
  void SetZ( bool i, double v )				{ fInOutZ[i] = v; }
  void SetTeta( double v )				{ fTeta = v; }
  void SetPhi( double v )				{ fPhi = v; }
  void SetSlice( unsigned short v )			{ fSlice = v; }

 private:
  //
  unsigned short fInnerRow; // number of the inner row of the track
  unsigned short fOuterRow; // number of the outer row of the track
  double fInOutX[2];
  double fInOutY[2];
  double fInOutZ[2];
  double fTeta;
  double fPhi;
  unsigned short fSlice;
  float fInnerAlpha;               // The angle of the sector, where inner parameters are
  int fPrevNeighbour; // The number of the inner (previous) neighbour in the tracks array (AliHLTTPCCAMerger::fTrackInfos)
  int fNextNeighbour; // The number of the outer (previous) neighbour in the tracks array (AliHLTTPCCAMerger::fTrackInfos)
  unsigned short fSlicePrevNeighbour; // The number of the sector, which contains inner neighbour
  unsigned short fSliceNextNeighbour; // The number of the sector, which contains outer neighbour
  int fUsed;            // is the slice track already merged (=1 -> merged, 0 -> not merged)
  AliHLTTPCCATrackParam fInnerParam; // Parameters of the track at the inner point
};

#if 0
#include "MemoryAssignmentHelpers.h"

class SliceTrackInfoPTSimpleSOA
{
 public:
  SliceTrackInfoPTSimpleSOA()
 {}

  SliceTrackInfoPTSimpleSOA( int nTracks )
  {
    sNTracks = nTracks;
    CreateStorage(nTracks);
  }

  ~SliceTrackInfoPTSimpleSOA()
  {
  }

  void Clear()
  {
    if (fMemory) delete[] fMemory;
  }

  void SetPointers()
  {
    // set all pointers

    char *mem = &fMemory[0];
  }

  void CreateStorage( int nTracks )
  {
    sNTracks = nTracks;
    for( int iSlice = 0; iSlice < AliHLTTPCCAParameters::NumberOfSlices; iSlice++ ) {
      for( int iRow = 1; iRow <= 45; iRow++) {
	sTrackMap[iSlice][iRow] = -1;
      }
    }
    sMaxGape = 5;
  }

  void CalculateTeta()
  {
    for( unsigned int iV = 0; iV < sNTracks; iV+= float_v::Size ) {
      float_v& vX0 = reinterpret_cast<float_v&>(vInnerX[iV]);
      float_v& vX1 = reinterpret_cast<float_v&>(vOuterX[iV]);
      float_v& vZ0 = reinterpret_cast<float_v&>(vInnerZ[iV]);
      float_v& vZ1 = reinterpret_cast<float_v&>(vOuterZ[iV]);
      float_v& teta = reinterpret_cast<float_v&>(vTeta[iV]);

      float_v x = vX1 - vX0;
      float_v z = vZ1 - vZ0;
      teta = atan( x / z );
    }
  }

  void CalculateTetaScalar()
  {
      for( unsigned int iV = 0; iV < sNTracks; iV++ ) {
        float& vX0 = reinterpret_cast<float&>(vInnerX[iV]);
        float& vX1 = reinterpret_cast<float&>(vOuterX[iV]);
        float& vZ0 = reinterpret_cast<float&>(vInnerZ[iV]);
        float& vZ1 = reinterpret_cast<float&>(vOuterZ[iV]);
        float& teta = reinterpret_cast<float&>(vTeta[iV]);

        float x = vX1 - vX0;
        float z = vZ1 - vZ0;
        teta = atan( x / z );
      }
  }

  void SetInnerCoord( unsigned int itr, float x, float2 yz ) {
    vInnerX[itr] = x;
  }

  void SetOuterCoord( unsigned int itr, float x, float2 yz ) {
    vOuterX[itr] = x;
    vOuterY[itr] = yz.x;
    vOuterZ[itr] = yz.y;
  }

  void SetZero( unsigned int itr ) {
    vInnerX[itr] = 0;
    vInnerY[itr] = 0;
    vInnerZ[itr] = 0;
    vOuterX[itr] = 0;
    vOuterY[itr] = 0;
    vOuterZ[itr] = 0;
  }

  void SetTrackID( unsigned int itr, int trid ) { vTrackID[itr] = trid; }
  void SetLastRow( unsigned int itr, int lrow ) { vLastRow[itr] = lrow; }

  void SetMappedTrack( unsigned int islice, unsigned int irow, unsigned int itr ) { sTrackMap[islice][irow] = itr; }
  void SetMaxGape( int m ) { sMaxGape = m; }

  float GetInnerX( unsigned int itr )	const { return vInnerX[itr]; }
  float GetInnerY( unsigned int itr )	const { return vInnerY[itr]; }
  float GetInnerZ( unsigned int itr )	const { return vInnerZ[itr]; }
  float GetOuterX( unsigned int itr )	const { return vOuterX[itr]; }
  float GetOuterY( unsigned int itr )	const { return vOuterY[itr]; }
  float GetOuterZ( unsigned int itr )	const { return vOuterZ[itr]; }
  float GetTeta( unsigned int itr )	const { return vTeta[itr]; }
  int GetTrackID( unsigned int itr )	const { return vTrackID[itr]; }
  int GetLastRow( unsigned int itr )	const { return vLastRow[itr]; }
  int GetFirstMappedTrackID( unsigned int islice, unsigned int irow ) {
    for( int iRow = irow; iRow < irow + sMaxGape; iRow++ ) {
      if( iRow > 45 ) return -1;
      if( sTrackMap[islice][iRow] != -1 ) return vTrackID[sTrackMap[islice][iRow]];
    }
    return -1;
  }
  int GetFirstMappedTrackN( unsigned int islice, unsigned int irow ) {
    for( int iRow = irow; iRow < irow + sMaxGape; iRow++ ) {
      if( iRow > 45 ) return -1;
      if( sTrackMap[islice][iRow] != -1 ) return sTrackMap[islice][iRow];
    }
    return -1;
  }

  //
  float* vInnerX;
  float* vOuterX;
  float* vInnerY;
  float* vOuterY;
  float* vInnerZ;
  float* vOuterZ;
  float* vTeta;
  int* vTrackID;
  int* vLastRow;
 private:
  //
  int sNTracks;
  int sTrackMap[AliHLTTPCCAParameters::NumberOfSlices][46];
  int sMaxGape;
  char fMemory[1]; // the memory where the pointers above point into
};
#endif

#endif
