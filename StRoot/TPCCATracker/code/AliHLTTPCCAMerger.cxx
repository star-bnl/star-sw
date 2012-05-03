// $Id: AliHLTTPCCAMerger.cxx,v 1.12 2012/05/03 14:00:26 fisyak Exp $
// **************************************************************************
// This file is property of and copyright by the ALICE HLT Project          *
// ALICE Experiment at CERN, All rights reserved.                           *
//                                                                          *
// Primary Authors: Sergey Gorbunov <sergey.gorbunov@kip.uni-heidelberg.de> *
//                  Ivan Kisel <kisel@kip.uni-heidelberg.de>                *
//                  for The ALICE HLT Project.                              *
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


#include "AliHLTTPCCASliceTrack.h"
#include "AliHLTTPCCATracker.h"
#include "AliHLTTPCCAGBTrack.h"
#include "AliHLTTPCCATrackParam.h"

#include "AliHLTTPCCAMerger.h"

#include "AliHLTTPCCAMath.h"
#include "TStopwatch.h"

#include "AliHLTTPCCATrackParam.h"
#include "AliHLTTPCCASliceTrack.h"
#include "AliHLTTPCCASliceOutput.h"
#include "AliHLTTPCCAMergedTrack.h"
#include "AliHLTTPCCAMergerOutput.h"
#include "AliHLTTPCCADataCompressor.h"
#include "AliHLTTPCCAParam.h"
#include "AliHLTTPCCATrackLinearisation.h"

#ifdef DRAW3
#include "AliHLTTPCCADisplay.h"
#define MAIN_DRAW
#endif //DRAW

#ifdef DRAW
#include "AliHLTTPCCADisplay.h"
#endif //DRAW

#include <vector>
#include "AliHLTTPCCAMergerPerformance.h"
#include "AliHLTTPCCAPerformance.h"

#ifdef DO_TPCCATRACKER_EFF_PERFORMANCE
#define DO_MERGER_PERF
#endif //DO_TPCCATRACKER_EFF_PERFORMANCE
int AliHLTTPCCAMerger::fgDoNotMergeBorders = 0;
class AliHLTTPCCAMerger::AliHLTTPCCAClusterInfo
{

  public:

    unsigned int  ISlice()    const { return fISlice;    }
    unsigned int  IRow()      const { return fIRow;      }
    unsigned int  IClu()      const { return fIClu;      }
    UChar_t PackedAmp() const { return fPackedAmp; }
    float X()         const { return fX;         }
    float Y()         const { return fY;         }
    float Z()         const { return fZ;         }
    float Err2Y()     const { return fErr2Y;     }
    float Err2Z()     const { return fErr2Z;     }

    void SetISlice    ( unsigned int v  ) { fISlice    = v; }
    void SetIRow      ( unsigned int v  ) { fIRow      = v; }
    void SetIClu      ( unsigned int v  ) { fIClu      = v; }
    void SetPackedAmp ( UChar_t v ) { fPackedAmp = v; }
    void SetX         ( float v ) { fX         = v; }
    void SetY         ( float v ) { fY         = v; }
    void SetZ         ( float v ) { fZ         = v; }
    void SetErr2Y     ( float v ) { fErr2Y     = v; }
    void SetErr2Z     ( float v ) { fErr2Z     = v; }

  private:

    unsigned int fISlice;            // slice number
    unsigned int fIRow;              // row number
    unsigned int fIClu;              // cluster number
    UChar_t fPackedAmp; // packed cluster amplitude
    float fX;                // x position (slice coord.system)
    float fY;                // y position (slice coord.system)
    float fZ;                // z position (slice coord.system)
    float fErr2Y;            // Squared measurement error of y position
    float fErr2Z;            // Squared measurement error of z position
};


class AliHLTTPCCAMerger::AliHLTTPCCASliceTrackInfo
{

  public:

    const AliHLTTPCCATrackParam &InnerParam() const { return fInnerParam;      }
    const AliHLTTPCCATrackParam &OuterParam() const { return fOuterParam;      }
    float InnerAlpha()                      const { return fInnerAlpha;      }
    float OuterAlpha()                      const { return fOuterAlpha;      }
    int   NClusters()                       const { return fNClusters;       }
    int   FirstClusterRef()                 const { return fFirstClusterRef; }
    int   BestNeighbour()                   const { return fBestNeighbour; }
    std::vector<int>   &PrevNeighbour()                   { return fPrevNeighbour;   }
    std::vector<int>   &NextNeighbour()                    { return fNextNeighbour;   }

    int   PrevNeighbourPre()                   { return fPrevNeighbourPre;  }
    int   NextNeighbourPre()                   { return fNextNeighbourPre;  }

    std::vector<int>   &SlicePrevNeighbour()              { return fSlicePrevNeighbour;   }
    std::vector<int>   & SliceNextNeighbour()               { return fSliceNextNeighbour;   }
    bool  Used()                            const { return fUsed;            }

    void SetInnerParam( const AliHLTTPCCATrackParam &v ) { fInnerParam = v;      }
    void SetOuterParam( const AliHLTTPCCATrackParam &v ) { fOuterParam = v;      }
    void SetInnerAlpha( float v )                      { fInnerAlpha = v;      }
    void SetOuterAlpha( float v )                      { fOuterAlpha = v;      }
    void SetNClusters ( int v )                        { fNClusters = v;       }
    void SetFirstClusterRef( int v )                   { fFirstClusterRef = v; }
    void SetPrevNeighbour( int v )                     { fPrevNeighbour.push_back(v);   }
    void SetNextNeighbour( int v )                     { fNextNeighbour.push_back(v);   }
    void SetPrevNeighbourPre( int v )                     { fPrevNeighbourPre = v;   }
    void SetNextNeighbourPre( int v )                     { fNextNeighbourPre = v;   }
    void SetSlicePrevNeighbour( int v )                { fSlicePrevNeighbour.push_back(v);   }
    void SetSliceNextNeighbour( int v )                { fSliceNextNeighbour.push_back(v);   }
    void SetUsed( bool v )                             { fUsed = v;            }
    void SetBestNeighbour( int v )                     { fBestNeighbour = v;   }
///mvz start
    int orig_track_id;
    int number;
    float ChiPrev;
    float ChiNext;
    int fSlice;
    int rowInner;
    int rowOuter;
///mvz end
  private:

    AliHLTTPCCATrackParam fInnerParam; // inner parameters
    AliHLTTPCCATrackParam fOuterParam; // outer parameters
    float fInnerAlpha;               // alpha angle for inner parameters
    float fOuterAlpha;               // alpha angle for outer parameters
    int fNClusters;                  // N clusters
    int fFirstClusterRef;  // index of the first track cluster in the global cluster array
    std::vector<int> fPrevNeighbour;    // neighbour in the previous slise
    std::vector<int> fNextNeighbour;    // neighbour in the next slise

    int fPrevNeighbourPre;    // neighbour in the previous slise
    int fNextNeighbourPre;    // neighbour in the next slise

    std::vector<int> fSlicePrevNeighbour;    // neighbour in the previous slise
    std::vector<int> fSliceNextNeighbour;    // neighbour in the next slise
    bool fUsed;            // is the slice track already merged
    int fBestNeighbour;
};

class AliHLTTPCCAMerger::AliHLTTPCCABorderTrackGlobal
{
  public:

    int   TrackID()                    const { return fTrackID;   }
    int   NClusters()                  const { return fNClusters; }
    int   IRow()                       const { return fIRow;      }
    float X()                          const { return fX;         }
    bool  OK()                         const { return fOK;        }
    int   Slice()                      const { return fSlice;     }

    void SetTrackID   ( int v )                        { fTrackID   = v; }
    void SetNClusters ( int v )                        { fNClusters = v; }
    void SetIRow      ( int v )                        { fIRow      = v; }
    void SetX         ( float v )                      { fX         = v; }
    void SetOK        ( bool v )                       { fOK        = v; }
    void SetSlice     ( int v)                         { fSlice     = v; }

    float b()           const{ return fb;    }
    float bErr2()           const{ return fbErr2;    }

    void Setb(float v)           { fb = v;    }
    void SetbErr2(float v)        { fbErr2 = v; }

  private:
    float fb;
    float fbErr2;

    int   fTrackID;              // track index
    int   fNClusters;            // n clusters
    int   fIRow;                 // row number of the closest cluster
    float fX;                    // X coordinate of the closest cluster
    bool  fOK;                   // is the track rotated and extrapolated correctly
    int   fSlice;                // slice number
    int fNDF;
};



AliHLTTPCCAMerger::AliHLTTPCCAMerger()
    :
    fSliceParam(),
    fOutput( 0 ),
    fTrackInfos( 0 ),
    fMaxTrackInfos( 0 ),
    fClusterInfos( 0 ),
    fMaxClusterInfos( 0 )
{
  //* constructor
  Clear();
}

/*
AliHLTTPCCAMerger::AliHLTTPCCAMerger(const AliHLTTPCCAMerger&)
  :
  fSliceParam(),
  fkSlices(0),
  fOutput(0),
  fTrackInfos(0),
  fMaxTrackInfos(0),
  fClusterInfos(0),
  fMaxClusterInfos(0)
{
}

const AliHLTTPCCAMerger &AliHLTTPCCAMerger::operator=(const AliHLTTPCCAMerger&) const
{
  return *this;
}
*/

AliHLTTPCCAMerger::~AliHLTTPCCAMerger()
{
  //* destructor
  if ( fTrackInfos ) delete[] fTrackInfos;
  if ( fClusterInfos ) delete[] fClusterInfos;
  if ( fOutput ) delete[] ( ( char* )( fOutput ) );
}

void AliHLTTPCCAMerger::Clear()
{
  for ( int i = 0; i < fgkNSlices; ++i ) {
    fkSlices[i] = 0;
  }
}

void AliHLTTPCCAMerger::SetSlices (int i, AliHLTTPCCATracker *sl )
{
  slices[i] = sl;
}

void AliHLTTPCCAMerger::SetSliceData( int index, const AliHLTTPCCASliceOutput *SliceData )
{
  fkSlices[index] = SliceData;
}

void AliHLTTPCCAMerger::Reconstruct()
{
  //* main merging routine

  UnpackSlices();
#ifdef DO_MERGER_PERF  
  AliHLTTPCCAPerformance::Instance().CreateHistos("Merger");
  ((AliHLTTPCCAMergerPerformance*)(AliHLTTPCCAPerformance::Instance().GetSubPerformance("Merger")))->SetNewEvent(
                                   AliHLTTPCCAPerformance::Instance().GetTracker(),
				   AliHLTTPCCAPerformance::Instance().GetHitLabels(),
				   AliHLTTPCCAPerformance::Instance().GetMCTracks(),
				   AliHLTTPCCAPerformance::Instance().GetMCPoints());
  ((AliHLTTPCCAMergerPerformance*)(AliHLTTPCCAPerformance::Instance().GetSubPerformance("Merger")))->FillMC();
#endif //DO_MERGER_PERF  
  Merging(1);
  Merging(0);
}

void AliHLTTPCCAMerger::UnpackSlices()
{
  //* unpack the cluster information from the slice tracks and initialize track info array

  // get N tracks and N clusters in event
#ifdef DO_MERGER_PERF
  int NTracksPrev=0;
#endif //DO_MERGER_PERF
  int nTracksTotal = 0;
  int nTrackClustersTotal = 0;
  for ( int iSlice = 0; iSlice < fgkNSlices; iSlice++ ) {
    if ( !fkSlices[iSlice] ) continue;
    nTracksTotal += fkSlices[iSlice]->NTracks();
    nTrackClustersTotal += fkSlices[iSlice]->NTrackClusters();
///mvz start
    slices[iSlice]->fNOutTracks1 = 0;
///mvz end
  }

  // book/clean memory if necessary
  {
 //   if ( nTracksTotal > fMaxTrackInfos || ( fMaxTrackInfos > 100 && nTracksTotal < 0.5*fMaxTrackInfos ) ) 
    {
      if ( fTrackInfos ) delete[] fTrackInfos;
      fMaxTrackInfos = ( int ) ( nTracksTotal * 1.2 );
      fTrackInfos = new AliHLTTPCCASliceTrackInfo[fMaxTrackInfos];
    }

//    if ( nTrackClustersTotal > fMaxClusterInfos || ( fMaxClusterInfos > 1000 && nTrackClustersTotal < 0.5*fMaxClusterInfos ) ) 
    {
      if ( fClusterInfos ) delete[] fClusterInfos;
      fMaxClusterInfos = ( int ) ( nTrackClustersTotal * 1.2 );
      fClusterInfos = new AliHLTTPCCAClusterInfo [fMaxClusterInfos];
    }

    if ( fOutput ) delete[] ( ( char* )( fOutput ) );
    int size = fOutput->EstimateSize( nTracksTotal, nTrackClustersTotal );
    fOutput = ( AliHLTTPCCAMergerOutput* )( new float2[size/sizeof( float2 )+1] );
  }

  // unpack track and cluster information

  int nTracksCurrent = 0;
  int nClustersCurrent = 0;

  for ( int iSlice = 0; iSlice < fgkNSlices; iSlice++ ) {

    fSliceTrackInfoStart[ iSlice ] = nTracksCurrent;
    fSliceNTrackInfos[ iSlice ] = 0;

    if ( !fkSlices[iSlice] ) continue;

    const AliHLTTPCCASliceOutput &slice = *( fkSlices[iSlice] );
    for ( int itr = 0; itr < slice.NTracks(); itr++ ) {

      const AliHLTTPCCASliceTrack &sTrack = slice.Track( itr );
      AliHLTTPCCATrackParam t0 = sTrack.Param();
      if (! t0.NDF()) continue;
///mvz start 03.02.2010
//      bool ttt = 0;
//      if( 1./CAMath::Abs((CAMath::Abs(t0.QPt()) + CAMath::Sqrt(t0.Err2QPt()))) < 0.6 ) continue;
//      if( 1./CAMath::Abs(t0.QPt()) -  CAMath::Sqrt(t0.Err2QPt())/CAMath::Abs(t0.QPt())< 0.6 ) ttt = 1;
//      if(!ttt) continue;
//      std::cout <<"R  "<< 1./(CAMath::Abs(t0.QPt()*fSliceParam.cBz())) << std::endl;
///mvz end 03.02.2010

      int nCluNew = 0;
      for ( int iTrClu = 0; iTrClu < sTrack.NClusters(); iTrClu++ ) {

        // unpack cluster information

        AliHLTTPCCAClusterInfo &clu = fClusterInfos[nClustersCurrent + nCluNew];
        int ic = sTrack.FirstClusterRef() + iTrClu;

        clu.SetISlice( iSlice );
        clu.SetIRow( slice.ClusterIDrc( ic ).Row() );
        clu.SetIClu( slice.ClusterIDrc( ic ).Cluster() );
        clu.SetPackedAmp( slice.ClusterPackedAmp( ic ) );
        float2 yz = slice.ClusterUnpackedYZ( ic );
        clu.SetX( slice.ClusterUnpackedX( ic ) );
        clu.SetY( yz.x );
        clu.SetZ( yz.y );

//        if ( !t0.TransportToX( fSliceParam.RowX( clu.IRow() ), fSliceParam.GetBz( t0 ), .999 ) ) continue;
        if ( !t0.TransportToX( fSliceParam.RowX( clu.IRow() ), fSliceParam.cBz(), .999 ) )  continue;

        float err2Y, err2Z;
///mvz start 20.01.2010
//        fSliceParam.GetClusterErrors2( clu.IRow(), clu.Z(), t0.SinPhi(), t0.GetCosPhi(), t0.DzDs(), err2Y, err2Z );
        fSliceParam.GetClusterErrors2( clu.IRow(), t0, err2Y, err2Z );
///mvz end 20.01.2010
        clu.SetErr2Y( err2Y );
        clu.SetErr2Z( err2Z );
        nCluNew++ ;
      }

      if ( nCluNew < AliHLTTPCCAParameters::MinTrackPurity*sTrack.NClusters() ) continue;
      // refit the track

      int hits[1000];
      int nHits = nCluNew;
      for ( int i = 0; i < nHits; i++ ) hits[i] = nClustersCurrent + i;

      AliHLTTPCCATrackParam startPoint = sTrack.Param();
      AliHLTTPCCATrackParam endPoint = startPoint;
///mvz start 02.06.2010
      //float startAlpha = fSliceParam.Alpha( iSlice );
      float startAlpha = slices[iSlice]->Param().Alpha();
///mvz end   02.06.2010
      float endAlpha = startAlpha;

//std::cout<<"Inner->Outer"<<std::endl;
//
//when we turn off the extrapolation step in the tracklet constructor, we have parameters in the last point, not in the first!
//that's why the fitting direction should be changed
      if ( !FitTrack( endPoint, endAlpha, startPoint, startAlpha, hits, nHits, 0 ) ) continue;
//      if ( !FitTrack( startPoint, startAlpha, endPoint, endAlpha, hits, nHits, 1 ) ) continue;
      startPoint = endPoint;
      startAlpha = endAlpha;
//      endPoint = startPoint;
//      endAlpha = startAlpha;
//std::cout<<"Outer->Inner"<<std::endl;
      if ( !FitTrack( startPoint, startAlpha, endPoint, endAlpha, hits, nHits, 1 ) ) continue;
//      if ( !FitTrack( endPoint, endAlpha, startPoint, startAlpha, hits, nHits, 0 ) ) continue;

      if ( nHits < AliHLTTPCCAParameters::MinTrackPurity*sTrack.NClusters() ) continue;

      // store the track

      AliHLTTPCCASliceTrackInfo &track = fTrackInfos[nTracksCurrent];

      track.SetInnerParam( startPoint );
      track.SetInnerAlpha( startAlpha );
      track.SetOuterParam( endPoint );
      track.rowInner = (fClusterInfos[hits[0]]).IRow();
      track.rowOuter = (fClusterInfos[hits[nHits-1]]).IRow();
//      track.SetOuterParam( sTrack.Param() );
      track.SetOuterAlpha( endAlpha );
      track.SetFirstClusterRef( nClustersCurrent );
      track.SetNClusters( nHits );
      track.SetPrevNeighbourPre( -1 );
      //track.SetSlicePrevNeighbourPre( -1 );
      track.SetNextNeighbourPre( -1 );
      //track.SetSliceNextNeighbourPre( -1 );
      track.ChiPrev = 10000000;
      track.ChiNext = 10000000;
      track.SetUsed( 0 );
///mvz start
      track.orig_track_id = itr;
      track.fSlice = iSlice;
      track.number = nTracksCurrent - fSliceTrackInfoStart[ iSlice ];
///mvz end
      for ( int i = 0; i < nHits; i++ )
        fClusterInfos[nClustersCurrent + i] = fClusterInfos[hits[i]];
      nTracksCurrent++;
      fSliceNTrackInfos[ iSlice ]++;
      nClustersCurrent += nHits;

/*#ifdef MAIN_DRAW
      AliHLTTPCCADisplay::Instance().ClearView();
      AliHLTTPCCADisplay::Instance().SetTPCView();
      AliHLTTPCCADisplay::Instance().DrawTPC();

      for(int ihit=0; ihit<track.NClusters(); ihit++)
      {
        AliHLTTPCCAClusterInfo &h = fClusterInfos[track.FirstClusterRef() + ihit];
        AliHLTTPCCADisplay::Instance().SetCurrentSlice( slices[h.ISlice()] );
        AliHLTTPCCADisplay::Instance().DrawPoint(h.X(), h.Y(), h.Z(), 0, 0.3 );
      }

      AliHLTTPCCADisplay::Instance().SetCurrentSlice( slices[fClusterInfos[track.FirstClusterRef()].ISlice()] );
      AliHLTTPCCADisplay::Instance().SaveCanvasToFile( "Track.png");
      AliHLTTPCCADisplay::Instance().Ask();
#endif*/
/*#ifdef DRAW
        AliHLTTPCCADisplay::Instance().SetSliceView();
        AliHLTTPCCADisplay::Instance().SetCurrentSlice(slices[iSlice]);
        AliHLTTPCCADisplay::Instance().ClearView();
        AliHLTTPCCADisplay::Instance().DrawSlice( slices[iSlice], 0 );
        AliHLTTPCCADisplay::Instance().DrawSliceHits();
        AliHLTTPCCADisplay::Instance().DrawTrackParam( startPoint, 4 );
        AliHLTTPCCADisplay::Instance().Ask();
#endif*/
    }
#ifdef DO_MERGER_PERF
///mvz start
    if (slices[iSlice]->fOutTracks1) delete[] slices[iSlice]->fOutTracks1;
    slices[iSlice]->fOutTracks1 = new AliHLTTPCCAOutTrack [nTracksCurrent-NTracksPrev];
    for (int i=0; i<nTracksCurrent-NTracksPrev; i++)
    {
      slices[iSlice]->fOutTracks1[i].SetStartPoint(fTrackInfos[i+NTracksPrev].InnerParam());
      slices[iSlice]->fOutTracks1[i].SetEndPoint(fTrackInfos[i+NTracksPrev].OuterParam());
      slices[iSlice]->fOutTracks1[i].SetOrigTrackID(fTrackInfos[i+NTracksPrev].orig_track_id);
      slices[iSlice]->fNOutTracks1++;
    }
    NTracksPrev = nTracksCurrent;
///mvz end
#endif //DO_MERGER_PERF
    //std::cout<<"Unpack slice "<<iSlice<<": ntracks "<<slice.NTracks()<<"/"<<fSliceNTrackInfos[iSlice]<<std::endl;
  }
}

bool AliHLTTPCCAMerger::FitTrack( AliHLTTPCCATrackParam &T, float &Alpha,
                                    AliHLTTPCCATrackParam t0, float Alpha0,
                                    int hits[], int &NTrackHits, bool dir )
{
  // Fit the track
/*#ifdef MAIN_DRAW
      AliHLTTPCCADisplay::Instance().ClearView();
      AliHLTTPCCADisplay::Instance().SetTPCView();
      AliHLTTPCCADisplay::Instance().DrawTPC();
#endif*/

  AliHLTTPCCATrackParam::AliHLTTPCCATrackFitParam fitPar;
  AliHLTTPCCATrackParam t = t0;
  AliHLTTPCCATrackLinearisation l( t0 );

  bool first = 1;

  t.CalculateFitParameters( fitPar );

  int hitsNew[1000];
  int nHitsNew = 0;

  for ( int ihit = 0; ihit < NTrackHits; ihit++ ) {
//    AliHLTTPCCATrackLinearisation l( t );
    int jhit = dir ? ( NTrackHits - 1 - ihit ) : ihit;
    AliHLTTPCCAClusterInfo &h = fClusterInfos[hits[jhit]];

    int iSlice = h.ISlice();

///mvz start 02.06.2010
    //float sliceAlpha =  fSliceParam.Alpha( iSlice );
    float sliceAlpha =  slices[iSlice]->Param().Alpha();
///mvz start 02.06.2010

    if ( CAMath::Abs( sliceAlpha - Alpha0 ) > 1.e-4 ) {
      if ( ! t.Rotate(  sliceAlpha - Alpha0, l, .999 ) ) continue;
      Alpha0 = sliceAlpha;
    }

    //float x = fSliceParam.RowX( h.IRow() );
    float x = h.X();

    if ( !t.TransportToXWithMaterial( x, l, fitPar, fSliceParam.cBz( ) ) ) continue;

/*#ifdef MAIN_DRAW
      AliHLTTPCCADisplay::Instance().SetCurrentSlice( slices[h.ISlice()] );
      AliHLTTPCCADisplay::Instance().DrawPoint(h.X(), h.Y(), h.Z(), 1, 1 );

      AliHLTTPCCADisplay::Instance().DrawPoint(t.X(), t.Y(), t.Z(), 0,0.5 );
#endif*/


    if ( first ) {
      t.SetCov( 0, 10 );
      t.SetCov( 1,  0 );
      t.SetCov( 2, 10 );
      t.SetCov( 3,  0 );
      t.SetCov( 4,  0 );
      t.SetCov( 5,  1 );
      t.SetCov( 6,  0 );
      t.SetCov( 7,  0 );
      t.SetCov( 8,  0 );
      t.SetCov( 9,  1 );
      t.SetCov( 10,  0 );
      t.SetCov( 11,  0 );
      t.SetCov( 12,  0 );
      t.SetCov( 13,  0 );
      t.SetCov( 14,  10 );
      t.SetChi2( 0 );
      t.SetNDF( -5 );
      t.CalculateFitParameters( fitPar );
    }

    ///mvz start
    float err2Y = h.Err2Y();
    float err2Z = h.Err2Z();

    fSliceParam.GetClusterErrors2( h.IRow(), t, err2Y, err2Z );
    
    if ( !t.Filter( h.Y(), h.Z(), err2Y, err2Z ) ) continue;
//    std::cout << t.Chi2() << std::endl;
    ///mvz end*/
    
//    if ( !t.Filter( h.Y(), h.Z(), h.Err2Y(), h.Err2Z() ) ) continue;

    first = 0;

    hitsNew[nHitsNew++] = hits[jhit];
  }

  if ( CAMath::Abs( t.QPt() ) < 1.e-8 ) t.SetQPt( 1.e-8 );

  bool ok = 1;

  const float *c = t.Cov();
  for ( int i = 0; i < 15; i++ ) ok = ok && finite( c[i] );
  for ( int i = 0; i < 5; i++ ) ok = ok && finite( t.Par()[i] );
///  ok = ok && ( t.GetX() > 50 ); //this check is wrong! X could be < 50, when a sector is rotated!

  if ( c[0] <= 0 || c[2] <= 0 || c[5] <= 0 || c[9] <= 0 || c[14] <= 0 ) ok = 0;
//  if ( c[0] > 5. || c[2] > 5. || c[5] > 2. || c[9] > 2 || c[14] > 2. ) ok = 0;

  if ( CAMath::Abs( t.SinPhi() ) > .999 ) ok = 0;
  else if ( l.CosPhi() >= 0 ) t.SetSignCosPhi( 1 );
  else t.SetSignCosPhi( -1 );

  if ( ok ) {
    T = t;
    Alpha = Alpha0;
    NTrackHits = nHitsNew;
    for ( int i = 0; i < NTrackHits; i++ ) {
      hits[dir ?( NTrackHits-1-i ) :i] = hitsNew[i];
    }
  }
/*#ifdef MAIN_DRAW
      AliHLTTPCCADisplay::Instance().Ask();
#endif*/
  return ok;
}

void AliHLTTPCCAMerger::MakeBorderTracksGlobal(AliHLTTPCCABorderTrackGlobal B[], int &nB )
{
  //* prepare slice tracks for merging with next/previous/same sector
  //* each track transported to the border line,
  //* in some cases both inner and outer parameters of the track are transported

  for (int iSlice = 0; iSlice < fgkNSlices; iSlice++)
  {

    for ( int itr = 0; itr < fSliceNTrackInfos[iSlice]; itr++ ) {

      const AliHLTTPCCASliceTrackInfo &track = fTrackInfos[ fSliceTrackInfoStart[iSlice] + itr ];

      double b = track.InnerParam().DzDs();
      double bErr2 = track.InnerParam().Err2DzDs();

      AliHLTTPCCABorderTrackGlobal &bTr = B[nB];


      bTr.SetX( track.InnerParam().GetX() );
      {
        bTr.SetOK( 1 );
        bTr.SetTrackID( itr );
        bTr.SetNClusters( track.NClusters() );
        bTr.SetIRow( fClusterInfos[ track.FirstClusterRef() + 0 ].IRow() );
        bTr.SetSlice(iSlice);

        bTr.Setb(b);
        bTr.SetbErr2(bErr2);

        nB++;
      }
    }
  }
}

void AliHLTTPCCAMerger::InvertCholetsky(float a[15])
{
  float d[5], uud, u[5][5];
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

    if(fabs(uud)<1.e-12) uud = 1.e-12;
    d[i] = uud/fabs(uud);
    u[i][i] = sqrt(fabs(uud));

    for(int j=i+1; j<5; j++) 
    {
      uud = 0.;
      for(int k=0; k<i; k++)
        uud += u[k][i]*u[k][j]*d[k];
      uud = a[j*(j+1)/2+i]/*a[i][j]*/ - uud;
      u[i][j] = d[i]/u[i][i]*uud;
    }
  }

  float u1[5];

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

void AliHLTTPCCAMerger::MultiplySS(float const C[15], float const V[15], float K[5][5])
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

void AliHLTTPCCAMerger::MultiplyMS(float const C[5][5], float const V[15], float K[15])
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

void AliHLTTPCCAMerger::MultiplySR(float const C[15], float const r_in[5], float r_out[5])
{
  r_out[0] = r_in[0]*C[ 0] + r_in[1]*C[ 1] + r_in[2]*C[ 3] +r_in[3]*C[ 6] + r_in[4]*C[10];
  r_out[1] = r_in[0]*C[ 1] + r_in[1]*C[ 2] + r_in[2]*C[ 4] +r_in[3]*C[ 7] + r_in[4]*C[11];
  r_out[2] = r_in[0]*C[ 3] + r_in[1]*C[ 4] + r_in[2]*C[ 5] +r_in[3]*C[ 8] + r_in[4]*C[12];
  r_out[3] = r_in[0]*C[ 6] + r_in[1]*C[ 7] + r_in[2]*C[ 8] +r_in[3]*C[ 9] + r_in[4]*C[13];
  r_out[4] = r_in[0]*C[10] + r_in[1]*C[11] + r_in[2]*C[12] +r_in[3]*C[13] + r_in[4]*C[14];
}

void AliHLTTPCCAMerger::FilterTracks(float const r[5], float const C[15], float const m[5], float const V[15], float R[5], float W[15], float &chi2)
{
  float S[15];
  for(int i=0; i<15; i++)
  {
    W[i] = C[i];
    S[i] = C[i] + V[i];
  }
  for(int i=0; i<5; i++)
    R[i] = r[i];

  InvertCholetsky(S);
  
  float K[5][5];
  MultiplySS(C,S,K);
  float dzeta[5];
  for(int i=0; i<5; i++) dzeta[i] = m[i] - r[i];
  float KC[15];
  MultiplyMS(K,C,KC);
  for(int i=0; i< 15; i++)
    W[i] -= KC[i];

  float kd;
  for(int i=0; i<5; i++)
  {
    kd = 0.f;
    for(int j=0; j<5; j++)
      kd += K[i][j]*dzeta[j];
    R[i] += kd;
  }
  float S_dzeta[5];
  MultiplySR(S, dzeta, S_dzeta);
  chi2 = dzeta[0]*S_dzeta[0] + dzeta[1]*S_dzeta[1] + dzeta[2]*S_dzeta[2] + dzeta[3]*S_dzeta[3] + dzeta[4]*S_dzeta[4];
}


void AliHLTTPCCAMerger::SplitBorderTracksGlobal( AliHLTTPCCABorderTrackGlobal B1[], int N1, AliHLTTPCCABorderTrackGlobal B2[], int N2, int number)
{
  //* split two sets of tracks
  float factor2k = 100.f;

  int iSlice1, iSlice2;
  bool IsNext;

  int mode = 0;  

  int nNeighSlices;

  if(mode == 0)  nNeighSlices = 6;
  if(mode == 1)  nNeighSlices = 10;

  int kkk = nNeighSlices*0.5;
  int nnn = (kkk - 1)*0.5;
  int MidSlice = fgkNSlices*0.5;

//  int NeighSlice[fgkNSlices][nNeighSlices];
  int * NeighSlice = new int[fgkNSlices*nNeighSlices];

//nNeighSlices =2;
//nnn=0;
//kkk=2;

  for(int j=0; j<kkk; j++)
  {
    for(int i=0; i<MidSlice; i++)
    {
      NeighSlice[i*nNeighSlices+j] = i + j - nnn;
      if(NeighSlice[i*nNeighSlices+j] >= MidSlice) NeighSlice[i*nNeighSlices+j] -= MidSlice;
      if(NeighSlice[i*nNeighSlices+j] < 0) NeighSlice[i*nNeighSlices+j] += MidSlice;

      NeighSlice[i*nNeighSlices +j+kkk] = 22 - NeighSlice[i*nNeighSlices+j]; 
      if(NeighSlice[i*nNeighSlices +j+kkk]<MidSlice) NeighSlice[i*nNeighSlices +j+kkk] += MidSlice;
    }
    for(int i=MidSlice; i<fgkNSlices; i++)
    {
      NeighSlice[i*nNeighSlices+j] = i + j - nnn;
      if(NeighSlice[i*nNeighSlices+j] >= fgkNSlices) NeighSlice[i*nNeighSlices+j] -= MidSlice;
      if(NeighSlice[i*nNeighSlices+j] < MidSlice) NeighSlice[i*nNeighSlices+j] += MidSlice;

      NeighSlice[i*nNeighSlices +j+kkk] = 22 - NeighSlice[i*nNeighSlices+j]; 
      if(NeighSlice[i*nNeighSlices +j+kkk]<0) NeighSlice[i*nNeighSlices +j+kkk] += MidSlice;
    }
  }

  for ( int i1 = 0; i1 < N1; i1++ ) {
    AliHLTTPCCABorderTrackGlobal &b1 = B1[i1];
    iSlice1 = b1.Slice();

    int iBest2 = -1;
    int iSliceBest2 = -1;
//    float chi2;

    float dr_min2_local = 10000000.;

    for ( int i2 = i1+1; i2 < N2; i2++ ) {
      IsNext = 1;
      if(i1==i2) continue;
      AliHLTTPCCABorderTrackGlobal &b2 = B2[i2];
      iSlice2 = b2.Slice();

      bool IsNeigh = 0;
      for(int i=0; i<nNeighSlices; i++)
        if( iSlice2 == NeighSlice[iSlice1*nNeighSlices +i]) IsNeigh = 1;
      if( iSlice2 == iSlice1) IsNeigh = 1;
      if(!IsNeigh) continue;

      float db2 = (b1.b()) - (b2.b());
      db2 = db2*db2;
      float ddb2 = b1.bErr2() + b2.bErr2();
      if(db2 > factor2k * ddb2) continue;

      float dx;

      AliHLTTPCCASliceTrackInfo *Tt1 = &fTrackInfos[fSliceTrackInfoStart[iSlice1] + b1.TrackID() ];
      AliHLTTPCCASliceTrackInfo *Tt2 = &fTrackInfos[fSliceTrackInfoStart[iSlice2] + b2.TrackID() ];

      float xE1 = Tt1->OuterParam().X();
//      float xS1 = Tt1->InnerParam().X();
      float xE2 = Tt2->OuterParam().X();
      float xS2 = Tt2->InnerParam().X();

      float yE1 = Tt1->OuterParam().Y();
//      float yS1 = Tt1->InnerParam().Y();
      float yE2 = Tt2->OuterParam().Y();
      float yS2 = Tt2->InnerParam().Y();

      float xE1_E2, xS1_S2, xS1_E2, xS2_E1;
      float yE1_E2, yS1_S2, yS1_E2, yS2_E1;
      float sinE1_E2, sinS1_S2, sinS1_E2, sinS2_E1;

      Tt1->OuterParam().RotateXY(Tt2->OuterAlpha() - Tt1->OuterAlpha(),xE1_E2,yE1_E2,sinE1_E2);
      Tt1->InnerParam().RotateXY(Tt2->InnerAlpha() - Tt1->InnerAlpha(),xS1_S2,yS1_S2,sinS1_S2);
      Tt1->InnerParam().RotateXY(Tt2->OuterAlpha() - Tt1->InnerAlpha(),xS1_E2,yS1_E2,sinS1_E2);
      Tt2->InnerParam().RotateXY(Tt1->OuterAlpha() - Tt2->InnerAlpha(),xS2_E1,yS2_E1,sinS2_E1);

      if(xS1_S2 < xS2) IsNext=1; 
      else             IsNext=0;

      AliHLTTPCCATrackParam Thelp, Thelp1;
#ifdef DO_MERGER_PERF      
      int sh = -1, sh1 = -1, Itr = -1, Itr1 = -1;
#endif // DO_MERGER_PERF
      float dxArr[4] = {(xE1_E2 - xE2), 
                        (xS1_S2 - xS2), 
                        (xS1_E2 - xE2), 
                        (xS2_E1 - xE1)};
      float dyArr[4] = {(yE1_E2 - yE2), 
                        (yS1_S2 - yS2), 
                        (yS1_E2 - yE2), 
                        (yS2_E1 - yE1)};

      float k1   = Tt1->InnerParam().QPt() * fSliceParam.cBz();
      float k2   = Tt2->InnerParam().QPt() * fSliceParam.cBz();
      float dx_1 = xE2 - xS1_E2;
      float dx_2 = xE1 - xS2_E1;
      float t_sin1 = k1 * dx_1 + sinS1_E2;
      float t_sin2 = k2 * dx_2 + sinS2_E1;
      if(CAMath::Abs( t_sin1 ) > 0.999 || CAMath::Abs( t_sin2 ) > 0.999) continue;

      float dxyArr[4] = {dxArr[0]*dxArr[0] + dyArr[0]*dyArr[0],
                         dxArr[1]*dxArr[1] + dyArr[1]*dyArr[1], 
                         dxArr[2]*dxArr[2] + dyArr[2]*dyArr[2], 
                         dxArr[3]*dxArr[3] + dyArr[3]*dyArr[3]};
      int idx = 0, idx_temp1 = 0, idx_temp2 = 2;
      dx = fabs(dxyArr[0]);
      float dx_temp1 = fabs(dxyArr[0]), dx_temp2 = fabs(dxyArr[2]);
      if(fabs(dxyArr[1]) < dx_temp1) { dx_temp1 = dxyArr[1]; idx_temp1 = 1; }
      if(fabs(dxyArr[3]) < dx_temp2) { dx_temp2 = dxyArr[3]; idx_temp2 = 3; }
      if(number == 1) 
        { dx = dx_temp2; idx = idx_temp2; }
      else
      {
        if(dx_temp2 < dx_temp1) { dx = dx_temp2; idx = idx_temp2; }
        else                    { dx = dx_temp1; idx = idx_temp1; }
      }
//int row, row1;
      float a1;
      float a2;

      switch(idx)
      {
        case 0:
          {
//            row = Tt1->rowOuter;
//            row1 = Tt2->rowOuter;
            Thelp  = Tt1->OuterParam();
            Thelp1 = Tt2->OuterParam();
            a1 = Tt1->OuterAlpha();
            a2 = Tt2->OuterAlpha();
            dx = dxArr[0];
          }
          break;
        case 1:
          {
//            row = Tt1->rowInner;
//            row1 = Tt2->rowInner;
            Thelp  = Tt1->InnerParam();
            Thelp1 = Tt2->InnerParam();
            a1 = Tt1->InnerAlpha();
            a2 = Tt2->InnerAlpha();
            dx = dxArr[1];
          }
          break;
        case 2:
          {
//            row = Tt1->rowInner;
//            row1 = Tt2->rowOuter;
            Thelp  = Tt1->InnerParam();
            Thelp1 = Tt2->OuterParam();
            a1 = Tt1->InnerAlpha();
            a2 = Tt2->OuterAlpha();
            dx = dxArr[2];
          }
          break;
        case 3:
          {
//            row = Tt1->rowOuter;
//            row1 = Tt2->rowInner;
            Thelp  = Tt1->OuterParam();
            Thelp1 = Tt2->InnerParam();
            a1 = Tt1->OuterAlpha();
            a2 = Tt2->InnerAlpha();
            dx = dxArr[3];
          }
          break;
      }

      if(number == 1)  if(dx < 0) continue;
      if(number == 0)  if(dx > 0) continue;

#ifdef DO_MERGER_PERF
      sh = Tt1->fSlice;
      sh1 = Tt2->fSlice;
      Itr = Tt1->number;
      Itr1 = Tt2->number;
#endif //DO_MERGER_PERF

      if(!Thelp.Rotate( a2 - a1 , 0.999 )) continue;

      dr_min2_local = (Thelp.Y()-Thelp1.Y())*(Thelp.Y()-Thelp1.Y()) + (Thelp.Z()-Thelp1.Z())*(Thelp.Z()-Thelp1.Z()) + dx*dx;

      float ToX = 0.5 * (Thelp.X() + Thelp1.X());
      if(!Thelp.TransportToXWithMaterial( ToX, fSliceParam.cBz()))  continue;
      if(!Thelp1.TransportToXWithMaterial( ToX, fSliceParam.cBz())) continue;

      if(fabs(Thelp1.Y() - Thelp.Y())>10.f) continue;
      if(fabs(Thelp1.Z() - Thelp.Z())>10.f) continue;
      if(fabs(Thelp1.SinPhi() - Thelp.SinPhi())>0.15f) continue;

      float C[15], r[5], chi2;
#ifdef DO_MERGER_PERF       
      float delta2, Si;
#endif // DO_MERGER_PERF  
      FilterTracks(Thelp.GetPar(), Thelp.GetCov(), Thelp1.GetPar(), Thelp1.GetCov(), r, C, chi2);

      bool Ok = 1;
      bool Ok1 = 1;

      for ( int i = 0; i < 15; i++ ) Ok = Ok && finite( C[i] );
      for ( int i = 0; i < 5; i++ ) Ok = Ok && finite( r[i] );
      if ( C[0] <= 0 || C[2] <= 0 || C[5] <= 0 || C[9] <= 0 || C[14] <= 0 ) Ok = 0;
      if ( C[0] > 5. || C[2] > 5. || C[5] > 2. || C[9] > 2 || C[14] > 2 ) Ok1 = 0;

      if ( C[1]*C[1] > C[0]*C[2]) Ok1 = 0;
      if ( C[3]*C[3] > C[0]*C[5]) Ok1 = 0;
      if ( C[4]*C[4] > C[2]*C[5]) Ok1 = 0;
      if ( C[6]*C[6] > C[0]*C[9]) Ok1 = 0;
      if ( C[7]*C[7] > C[2]*C[9]) Ok1 = 0;
      if ( C[8]*C[8] > C[5]*C[9]) Ok1 = 0;
      if ( C[10]*C[10] > C[0]*C[14]) Ok1 = 0;
      if ( C[11]*C[11] > C[2]*C[14]) Ok1 = 0;
      if ( C[12]*C[12] > C[5]*C[14]) Ok1 = 0;
      if ( C[13]*C[13] > C[9]*C[14]) Ok1 = 0;

      if(number == 1) if(chi2>100.f) continue;
      if(number == 0) if(chi2>300.f) continue;

#ifdef DO_MERGER_PERF
      float dbb2 = (b1.b()) - (b2.b());
      dbb2 = dbb2*dbb2;
      float ddbb2 = b1.bErr2() + b2.bErr2();
      float ady = fabs(Thelp1.Y() - Thelp.Y());
      float adz = fabs(Thelp1.Z() - Thelp.Z());
      float adsin = fabs(Thelp1.SinPhi() - Thelp.SinPhi());

//      delta2 = fabs(row-row1);
//      Si = (Thelp.X() - xc1)/(Thelp.Y() - yc1) - (Thelp1.X() - xc2)/(Thelp1.Y() - yc2);
      if(!(sh == -1 || sh1 == -1 || Itr == -1 || Itr1 == -1))
        ((AliHLTTPCCAMergerPerformance*)(AliHLTTPCCAPerformance::Instance().GetSubPerformance("Merger")))->
	  AddMergerData(number, sh, Itr, sh1, Itr1, dbb2 <= factor2k * ddbb2, Ok, Ok1, chi2, delta2, Si, ady,adz,adsin,Tt1->NClusters(),Tt2->NClusters());
#endif //DO_MERGER_PERF

#ifdef MAIN_DRAW
if(number == 0 ){
      AliHLTTPCCADisplay::Instance().ClearView();
      AliHLTTPCCADisplay::Instance().SetTPCView();
      AliHLTTPCCADisplay::Instance().DrawTPC();

      for(int ihit=0; ihit<Tt1->NClusters(); ihit++)
      {
        AliHLTTPCCAClusterInfo &h = fClusterInfos[Tt1->FirstClusterRef() + ihit];
        AliHLTTPCCADisplay::Instance().SetCurrentSlice( slices[h.ISlice()] );
        AliHLTTPCCADisplay::Instance().DrawPoint(h.X(), h.Y(), h.Z(), 2, 1 );
      }

      for(int ihit=0; ihit<Tt2->NClusters(); ihit++)
      {
        AliHLTTPCCAClusterInfo &h = fClusterInfos[Tt2->FirstClusterRef() + ihit];
        AliHLTTPCCADisplay::Instance().SetCurrentSlice( slices[h.ISlice()] );
        AliHLTTPCCADisplay::Instance().DrawPoint(h.X(), h.Y(), h.Z(), 3, 1 );
      }

      AliHLTTPCCADisplay::Instance().SetCurrentSlice( slices[iSlice1] );
      AliHLTTPCCADisplay::Instance().DrawPoint(Thelp.X(), Thelp.Y(), Thelp.Z(), 4, 1 );
      AliHLTTPCCADisplay::Instance().DrawPoint(Thelp1.X(), Thelp1.Y(), Thelp1.Z(), 5, 0.5 );

      AliHLTTPCCADisplay::Instance().Ask();
}
#endif

#ifdef DO_MERGER_PERF
      if(!(sh == -1 || sh1 == -1 || Itr == -1 || Itr1 == -1))
        ((AliHLTTPCCAMergerPerformance*)(AliHLTTPCCAPerformance::Instance().GetSubPerformance("Merger")))->SetMerged(number);
#endif //DO_MERGER_PERF
      iBest2 = b2.TrackID();
      iSliceBest2 = iSlice2;

      AliHLTTPCCASliceTrackInfo *T1, *T2;
      int NextNew, SliceNextNew, PrevNew, SlicePrevNew;
      if(IsNext)
      {
        T1 = &fTrackInfos[fSliceTrackInfoStart[iSlice1] + b1.TrackID() ];
        T2 = &fTrackInfos[fSliceTrackInfoStart[iSliceBest2] + iBest2 ];

        NextNew = iBest2;
        SliceNextNew = iSliceBest2;
        PrevNew = b1.TrackID();
        SlicePrevNew = iSlice1;
      }
      else
      {
        T2 = &fTrackInfos[fSliceTrackInfoStart[iSlice1] + b1.TrackID() ];
        T1 = &fTrackInfos[fSliceTrackInfoStart[iSliceBest2] + iBest2 ];

        NextNew = b1.TrackID();
        SliceNextNew = iSlice1;
        PrevNew = iBest2;
        SlicePrevNew = iSliceBest2;
      }
      if(1)
      {
        if(T1->ChiNext > dr_min2_local && T2->ChiPrev > dr_min2_local)
        {
          //std::cout << sqrt(T1->ChiNext) << "  " << sqrt(dr_min2_local) << std::endl;
//std::cout << T1->NextNeighbour().size() << " " << T2->PrevNeighbour().size() << std::endl;
          if(T1->NextNeighbour().size() > 0)
          {
            AliHLTTPCCASliceTrackInfo *T3 = &fTrackInfos[fSliceTrackInfoStart[T1->SliceNextNeighbour()[0]] + T1->NextNeighbour()[0] ];
            T3->PrevNeighbour().clear();
            T3->SlicePrevNeighbour().clear();
            T3->ChiPrev = 10000000;
          }
          if(T2->PrevNeighbour().size() > 0)
          {
            AliHLTTPCCASliceTrackInfo *T3 = &fTrackInfos[fSliceTrackInfoStart[T2->SlicePrevNeighbour()[0]] + T2->PrevNeighbour()[0] ];
            T3->NextNeighbour().clear();
            T3->SliceNextNeighbour().clear();
            T3->ChiNext = 10000000;
          }

          T1->NextNeighbour().clear();
          T1->SliceNextNeighbour().clear();
          T1->SetNextNeighbour( NextNew );
          T1->SetSliceNextNeighbour( SliceNextNew );
          T1->ChiNext = dr_min2_local;

          T2->PrevNeighbour().clear();
          T2->SlicePrevNeighbour().clear();
          T2->SetPrevNeighbour( PrevNew );
          T2->SetSlicePrevNeighbour( SlicePrevNew );
          T2->ChiPrev = dr_min2_local;

        }
      }
      else
      {
        T1->SetNextNeighbour( NextNew );
        T1->SetSliceNextNeighbour( SliceNextNew );

        T2->SetPrevNeighbour( PrevNew );
        T2->SetSlicePrevNeighbour( SlicePrevNew );
      }
    }
  }
  
  delete[] NeighSlice;
}

void AliHLTTPCCAMerger::Merging(int number)
{
  //* track merging between slices

  fOutput->SetNTracks( 0 );
  fOutput->SetNTrackClusters( 0 );
  fOutput->SetPointers();


  // for each slice set number of the next neighbouring slice

  int nextSlice[100], prevSlice[100];

  int mid = fgkNSlices / 2 - 1 ;
  int last = fgkNSlices - 1 ;

  for ( int iSlice = 0; iSlice < fgkNSlices/2; iSlice++ ) {
    nextSlice[iSlice] = iSlice + 1;
    prevSlice[iSlice] = iSlice - 1;
  }

  for ( int iSlice = fgkNSlices/2; iSlice < fgkNSlices; iSlice++ ) {
    nextSlice[iSlice] = iSlice - 1;
    prevSlice[iSlice] = iSlice + 1;
  }

  if ( mid < 0 ) mid = 0; // to avoid compiler warning
  if ( last < 0 ) last = 0; //
  nextSlice[ mid ] = 0;
  prevSlice[ 0 ] = mid;
  prevSlice[ last ] = fgkNSlices / 2;
  nextSlice[ fgkNSlices/2 ] = last;

  int maxNSliceTracks = 0;
  for ( int iSlice = 0; iSlice < fgkNSlices; iSlice++ ) {
    if ( maxNSliceTracks < fSliceNTrackInfos[iSlice] ) maxNSliceTracks = fSliceNTrackInfos[iSlice];
  }
  if (! fgDoNotMergeBorders ) {// Don't merge over borders ?
    AliHLTTPCCABorderTrackGlobal *bCurr = new AliHLTTPCCABorderTrackGlobal[maxNSliceTracks*10*24];
    int nCurr = 0;
    MakeBorderTracksGlobal(bCurr,nCurr);
    SplitBorderTracksGlobal( bCurr, nCurr, bCurr, nCurr, number );
    if ( bCurr ) delete[] bCurr;
  }

  int nOutTracks = 0;
  int nOutTrackClusters = 0;

  AliHLTTPCCAMergedTrack *outTracks;
  DataCompressor::SliceRowCluster *outClusterIDsrc;
  UChar_t  *outClusterPackedAmp;

  if(number == 0)
  {
    outTracks = new AliHLTTPCCAMergedTrack[fMaxTrackInfos];
    outClusterIDsrc = new DataCompressor::SliceRowCluster[fMaxClusterInfos*10];
    outClusterPackedAmp = new UChar_t [fMaxClusterInfos];
  }

  AliHLTTPCCAClusterInfo *tmpH = new AliHLTTPCCAClusterInfo[fMaxClusterInfos*10];
  AliHLTTPCCASliceTrackInfo *tmpT = new AliHLTTPCCASliceTrackInfo[fgkNSlices*fMaxTrackInfos];

  int nTrNew[fgkNSlices];

  for(int iSlice=0; iSlice < fgkNSlices; iSlice++) nTrNew[iSlice]=0;

  int nH = 0;

  for ( int iSlice = 0; iSlice < fgkNSlices; iSlice++ ) {
///    std::cout << "slice " << iSlice << std::endl;


//std::cout << iSlice << "   " << nH << std::endl;

    for ( int itr = 0; itr < fSliceNTrackInfos[iSlice]; itr++ ) {

/// ///////////////////////////////////////////////////////////////////////////////////////////////////
      AliHLTTPCCASliceTrackInfo &trackOld = fTrackInfos[fSliceTrackInfoStart[iSlice] + itr];

//      std::cout << "neigh next " << track.NextNeighbour().size() << " " ;
//      std::cout << "neigh prev " << track.PrevNeighbour().size() << std::endl ;

      if ( trackOld.Used() ) continue;
      if ( trackOld.PrevNeighbour().size() != 0 ) continue;

      AliHLTTPCCASliceTrackInfo &track = tmpT[fMaxTrackInfos*iSlice + nTrNew[iSlice]];
      track = trackOld;

      AliHLTTPCCATrackParam startPoint = track.InnerParam(), endPoint = track.OuterParam();
      AliHLTTPCCATrackParam startPoint1, endPoint1;
      float startAlpha = track.InnerAlpha(), endAlpha = track.OuterAlpha();

      if ( endPoint.X() < startPoint.X() ) {
        AliHLTTPCCATrackParam helpPoint = endPoint;
        endPoint = startPoint;
        startPoint = helpPoint;
      }
#ifdef DO_MERGER_PERF
      track.number = trackOld.number;
      track.fSlice = trackOld.fSlice;
#endif //DO_MERGER_PERF
      int hits[2000];
      int firstHit = 1000;
      int nHits = 0;
      int jSlice = iSlice;
      int jtr = itr;

      {
        track.SetUsed( 1 );
        trackOld.SetUsed( 1 );
        for ( int jhit = 0; jhit < track.NClusters(); jhit++ ) {
          int id = track.FirstClusterRef() + jhit;
          hits[firstHit+jhit] = id;
        }
        nHits = track.NClusters();
        if(track.NextNeighbour().size() > 0)
        {
          jtr = track.NextNeighbour()[0];
          jSlice = track.SliceNextNeighbour()[0];
        }
        else
          jtr = -1;
        //jSlice = nextSlice[iSlice];
      }

      while ( jtr >= 0 ) 
      {
        AliHLTTPCCASliceTrackInfo &segment = fTrackInfos[fSliceTrackInfoStart[jSlice] + jtr];
        if ( segment.Used() ) break;

        segment.SetUsed( 1 );
        bool dir = 0;
        int startHit = firstHit + nHits;
        float d00 = startPoint.GetDistXZ2( segment.InnerParam() );
        float d01 = startPoint.GetDistXZ2( segment.OuterParam() );
        float d10 = endPoint.GetDistXZ2( segment.InnerParam() );
        float d11 = endPoint.GetDistXZ2( segment.OuterParam() );
        if ( d00 <= d01 && d00 <= d10 && d00 <= d11 ) {
          startPoint = segment.OuterParam();
          startAlpha = segment.OuterAlpha();
          dir = 1;
          firstHit -= segment.NClusters();
          startHit = firstHit;
        } else if ( d01 <= d10 && d01 <= d11 ) {
          startPoint = segment.InnerParam();
          startAlpha = segment.InnerAlpha();
          dir = 0;
          firstHit -= segment.NClusters();
          startHit = firstHit;
        } else if ( d10 <= d11 ) {
          endPoint = segment.OuterParam();
          endAlpha = segment.OuterAlpha();
          dir = 0;
        } else {
          endPoint = segment.InnerParam();
          endAlpha = segment.InnerAlpha();
          dir = 1;
        }

        if ( endPoint.X() < segment.OuterParam().X() ) {
          endPoint = segment.OuterParam();
        }
        if ( endPoint.X() < segment.InnerParam().X() ) {
          endPoint = segment.InnerParam();
        }
        if ( startPoint.X() > segment.InnerParam().X() ) {
          startPoint = segment.InnerParam();
        }
        if ( startPoint.X() > segment.OuterParam().X() ) {
          startPoint = segment.OuterParam();
        }

        for ( int jhit = 0; jhit < segment.NClusters(); jhit++ ) {
          int id = segment.FirstClusterRef() + jhit;
          hits[startHit+( dir ?( segment.NClusters()-1-jhit ) :jhit )] = id;
        }
        nHits += segment.NClusters();
        if(segment.NextNeighbour().size()>0)
        {
          jtr = segment.NextNeighbour()[0];
          jSlice = segment.SliceNextNeighbour()[0];
        }
        else jtr = -1;
      }

      if ( endPoint.X() < startPoint.X() ) { // swap
        for ( int i = 0; i < nHits; i++ ) hits[i] = hits[firstHit+nHits-1-i];
        firstHit = 0;
      }

/// ///////////////////////////////////////////////////////////////////////////////////////////////////

///mvz      if ( nHits < AliHLTTPCCAParameters::MinimumHitsForGBTrack ) continue;    //SG!!!

      // refit

      // need best t0!!!SG
//TODO: why tracks sometimes are killed after a merging?
      endPoint = startPoint;
      if ( !FitTrack( endPoint, endAlpha, startPoint, startAlpha, hits + firstHit, nHits, 0 ) ) continue;
      if ( !FitTrack( startPoint, startAlpha, endPoint, endAlpha, hits + firstHit, nHits, 1 ) ) continue;

///mvz      if ( nHits < AliHLTTPCCAParameters::MinimumHitsForGBTrack ) continue;    //SG!!!

      AliHLTTPCCATrackParam &p = startPoint;

      if(0){
//        double xTPC = 83.65; //SG!!!
        double xTPC = 53.2; //mvz TODO: read from PARAMETERS!!
        double dAlpha = 0.00609235; // TODO: read from file!!  ?
        AliHLTTPCCATrackParam::AliHLTTPCCATrackFitParam fitPar;
        p.CalculateFitParameters( fitPar );

        if ( p.TransportToXWithMaterial( xTPC, fitPar, fSliceParam.GetBz( p ) ) ) {
          double y = p.GetY();
          double ymax = xTPC * CAMath::Tan( dAlpha / 2. );
          if ( y > ymax ) {
            if ( p.Rotate( dAlpha ) ) { startAlpha += dAlpha;  p.TransportToXWithMaterial( xTPC, fitPar, fSliceParam.GetBz( p ) ); }
          } else if ( y < -ymax ) {
            if ( p.Rotate( -dAlpha ) ) {  startAlpha -= dAlpha; p.TransportToXWithMaterial( xTPC, fitPar, fSliceParam.GetBz( p ) );}
          }
        }
      }

      {
        bool ok = 1;

        const float *c = p.Cov();
        for ( int i = 0; i < 15; i++ ) ok = ok && finite( c[i] );
        for ( int i = 0; i < 5; i++ ) ok = ok && finite( p.Par()[i] );
        ok = ok && ( p.GetX() > 50 ); // TODO: read from file!!

        if ( c[0] <= 0 || c[2] <= 0 || c[5] <= 0 || c[9] <= 0 || c[14] <= 0 ) ok = 0;
        if ( c[0] > 5. || c[2] > 10. || c[5] > 2. || c[9] > 2 || c[14] > 2 ) ok = 0;
///mvz        if ( !ok ) continue;
      }

      // int   hits2[nHits];
      // float Zhits[nHits];
      int *hits2 = new int[nHits];
      float *Zhits = new float[nHits];
      float Zm = 100000;
      float Zcur;
      int   hitCur=0;
      int   nHits2=0;

      // bool  index[nHits];
      bool *index = new bool[nHits];

      for(int ihit=0; ihit<nHits; ihit++)
      {
        bool cont=0;
        for(int jhit=0; jhit<nHits2; jhit++)
          if(hits[firstHit+ihit] == hits2[jhit]) cont=1;

        if(cont) continue;

        hits2[nHits2] = hits[firstHit+ihit];

        AliHLTTPCCAClusterInfo &clu = fClusterInfos[hits2[nHits2]];
        Zhits[nHits2] = clu.X();
        nHits2++;
        index[ihit]=0;
      }

      nHits=0;

/*      nHits = nHits2;
      for(int ihit=0; ihit<nHits; ihit++)
      {
        hits[ihit] = hits2[ihit];
      }
*/
      while(nHits != nHits2)
      {
        for(int ihit=0; ihit<nHits2; ihit++)
        {
          Zcur = Zhits[ihit];
          if(!index[ihit] && Zcur<Zm)
          {
            Zm = Zcur;
            hitCur = ihit;
          }
        }
        hits[nHits] = hits2[hitCur];
        index[hitCur]=1;
        nHits++;
        Zm = 1000000;
      }


      if ( endPoint.X() < startPoint.X() ) {
        AliHLTTPCCATrackParam helpPoint = endPoint;
        endPoint = startPoint;
        startPoint = helpPoint;
      }

      if(number == 0)
      {
        AliHLTTPCCAMergedTrack &mergedTrack = outTracks[nOutTracks];
        mergedTrack.SetNClusters( nHits );
        mergedTrack.SetFirstClusterRef( nOutTrackClusters );
        mergedTrack.SetInnerParam( startPoint );
        mergedTrack.SetInnerAlpha( startAlpha );
        mergedTrack.SetOuterParam( endPoint );
        mergedTrack.SetOuterAlpha( endAlpha );

        for ( int i = 0; i < nHits; i++ ) {
          AliHLTTPCCAClusterInfo &clu = fClusterInfos[hits[i]];

          outClusterIDsrc[nOutTrackClusters+i] =
            DataCompressor::SliceRowCluster( clu.ISlice(), clu.IRow(), clu.IClu() );
          outClusterPackedAmp[nOutTrackClusters+i] = clu.PackedAmp();
        }

        nOutTracks++;
        nOutTrackClusters += nHits;
      }

//std::cout << track.NClusters();

      nTrNew[iSlice]++;
      track.SetFirstClusterRef(nH);
      track.SetNClusters(nHits);
      track.SetUsed(0);
      track.PrevNeighbour().clear();
      track.NextNeighbour().clear();
      track.SlicePrevNeighbour().clear();
      track.SliceNextNeighbour().clear();
      track.ChiPrev = 10000000;
      track.ChiNext = 10000000;
      track.SetPrevNeighbourPre( -1 );
      track.SetNextNeighbourPre( -1 );
      track.SetInnerParam(startPoint);
      track.SetInnerAlpha( startAlpha );
      track.SetOuterParam(endPoint);
      track.SetOuterAlpha( endAlpha );

      for(int iClu=0; iClu < nHits; iClu++) tmpH[nH + iClu] = fClusterInfos[hits[iClu]];
      nH += nHits;

      delete[] hits2;
      delete[] Zhits;
      delete[] index;
//std::cout  << "  " << track.NClusters() << "  " << nH << std::endl;
    }
  }

  for ( int ih = 0; ih < nH; ih++ ) fClusterInfos[ih] = tmpH[ih];
  if(tmpH) delete[] tmpH;
  
  for(int iSlice=0; iSlice < fgkNSlices; iSlice++ )
  {
    fSliceNTrackInfos[iSlice] = nTrNew[iSlice];

    for ( int itr = 0; itr < nTrNew[iSlice]; itr++ ) 
      fTrackInfos[fSliceTrackInfoStart[iSlice]+itr] = tmpT[fMaxTrackInfos*iSlice + itr];
  }
  if(tmpT) delete[] tmpT;


  if(number == 0)
  {
    fOutput->SetNTracks( nOutTracks );
    fOutput->SetNTrackClusters( nOutTrackClusters );
    fOutput->SetPointers();

    for ( int itr = 0; itr < nOutTracks; itr++ ) fOutput->SetTrack( itr, outTracks[itr] );

    for ( int ic = 0; ic < nOutTrackClusters; ic++ ) {
      fOutput->SetClusterIDsrc( ic, outClusterIDsrc[ic] );
      fOutput->SetClusterPackedAmp( ic, outClusterPackedAmp[ic] );
    }

    delete[] outTracks;
    delete[] outClusterIDsrc;
    delete[] outClusterPackedAmp;
  }
}
