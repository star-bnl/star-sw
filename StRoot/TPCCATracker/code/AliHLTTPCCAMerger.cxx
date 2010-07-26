// $Id: AliHLTTPCCAMerger.cxx,v 1.1.1.1 2010/07/26 20:55:38 ikulakov Exp $
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

#include <vector>

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

    float XStart()      const{ return fxStart; }
    float YStart()      const{ return fyStart; }
    float ZStart()      const{ return fzStart; }
    float XStartErr2()   const{ return fxStartErr2; }
    float YStartErr2()   const{ return fyStartErr2; }
    float ZStartErr2()   const{ return fzStartErr2; }

    float XEnd()        const{ return fxEnd; }
    float YEnd()        const{ return fyEnd; }
    float ZEnd()        const{ return fzEnd; }
    float XEndErr2()     const{ return fxEndErr2; }
    float YEndErr2()     const{ return fyEndErr2; }
    float ZEndErr2()     const{ return fzEndErr2; }

    float XCentr()      const{ return fxc; }
    float YCentr()      const{ return fyc; }
    float XCentrErr2()   const{ return fxcErr2; }
    float YCentrErr2()   const{ return fycErr2; }

    float R()           const{ return fR;    }
    float RErr2()        const{ return fRErr2; }

    float b()           const{ return fb;    }
    float a()           const{ return fa;    }
    float bErr2()        const{ return fbErr2; }
    float p0()          const{ return fp0;    }
    float p0Err2()       const{ return fp0Err2; }
    int   c()           const{ return fc; }

    int NDF()           const{ return fNDF; }

    void SetXStart(float v)      { fxStart = v; }
    void SetYStart(float v)      { fyStart = v; }
    void SetZStart(float v)      { fzStart = v; }
    void SetXStartErr2(float v)   { fxStartErr2 = v; }
    void SetYStartErr2(float v)   { fyStartErr2 = v; }
    void SetZStartErr2(float v)   { fzStartErr2 = v; }
    void SetXEnd(float v)        { fxEnd = v; }
    void SetYEnd(float v)        { fyEnd = v; }
    void SetZEnd(float v)        { fzEnd = v; }
    void SetXEndErr2(float v)     { fxEndErr2 = v; }
    void SetYEndErr2(float v)     { fyEndErr2 = v; }
    void SetZEndErr2(float v)     { fzEndErr2 = v; }

    void SetXCentr(float v)      { fxc = v; }
    void SetYCentr(float v)      { fyc = v; }
    void SetXCentrErr2(float v)   { fxcErr2 = v; }
    void SetYCentrErr2(float v)   { fycErr2 = v; }

    void SetR(float v)           { fR = v;    }

    void SetRErr2(float v)        { fRErr2 = v; }
    void Setb(float v)           { fb = v;    }
    void Seta(float v)           { fa = v;    }
    void SetbErr2(float v)        { fbErr2 = v; }

    void Setp0(float v)          { fp0 = v;    }
    void Setp0Err2(float v)       { fp0Err2 = v; }

    void  Setc(int v)            { fc = v; }
    void  SetNDF(int v)          { fNDF = v; }

    void  SetdRdPhi(float v)     { fdRdPhi = v;    }
    float dRdPhi()               { return fdRdPhi;    }

    void  SetChiBest(float v)   { fChiBest = v;    }
    float ChiBest()              { return fChiBest; }

    void  SetQPt(float v)        { fQPt = v;    }
    float QPt()                  { return fQPt; }
    void  SetQPtErr2(float v)        { fQPtErr2 = v;    }
    float QPtErr2()                  { return fQPtErr2; }

    void CalculateCoord(float a, float r, float z, float &x, float &y)
    {
      float p = fp0 + fc*(z-fzStart)/(fb*a*r);
      y = fyc + r*sin(p);
      x = fxc + fc*r*cos(p);
    }
    void CalculateCoord(float z, float &x, float &y)
    {
      float p = fp0 + fc*(z-fzStart)/(fb*fa*fR);
      y = fyc + fR*sin(p);
      x = fxc + fc*fR*cos(p);
    }
    void CalculateCoord(float xc, float yc, float a, float r, float z, float &x, float &y)
    {
      float p = fp0 + fc*(z-fzStart)/(fb*a*r);
      y = yc + r*sin(p);
      x = xc + fc*r*cos(p);
    }
    void CalculateCoord(float a, float r, float p, float &x, float &y, float &z)
    {
      z = fzStart + fc*fb*r*a*(p - fp0) ;
      y = fyc + r*sin(p);
      x = fxc + fc*r*cos(p);
    }

  private:

    float fxStart, fxEnd;
    float fyStart, fyEnd;
    float fzStart, fzEnd;
    float fxc, fyc;
    float fR;
    float fb;
    float fa;
    float fp0;
    int   fc;

    float fxStartErr2, fxEndErr2;
    float fyStartErr2, fyEndErr2;
    float fzStartErr2, fzEndErr2;
    float fxcErr2, fycErr2;
    float fRErr2;
    float fbErr2;
    float fp0Err2;

    int   fTrackID;              // track index
    int   fNClusters;            // n clusters
    int   fIRow;                 // row number of the closest cluster
    float fX;                    // X coordinate of the closest cluster
    bool  fOK;                   // is the track rotated and extrapolated correctly
    int   fSlice;                // slice number
    int fNDF;

    float fChiBest;
    float fQPt;
    float fQPtErr2;
    float fdRdPhi;
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
  Merging(1);
  Merging(0);
}

void AliHLTTPCCAMerger::UnpackSlices()
{
  //* unpack the cluster information from the slice tracks and initialize track info array

  // get N tracks and N clusters in event
///mvz start
  int NTracksPrev=0;
///mvz end
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
        if ( !t0.TransportToX( fSliceParam.RowX( clu.IRow() ), fSliceParam.cBz(), .999 ) ) continue;

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
      if ( !FitTrack( endPoint, endAlpha, startPoint, startAlpha, hits, nHits, 0 ) ) continue;
      startPoint = endPoint;
      startAlpha = endAlpha;
//std::cout<<"Outer->Inner"<<std::endl;
      if ( !FitTrack( startPoint, startAlpha, endPoint, endAlpha, hits, nHits, 1 ) ) continue;

      if ( nHits < AliHLTTPCCAParameters::MinTrackPurity*sTrack.NClusters() ) continue;

      // store the track

      AliHLTTPCCASliceTrackInfo &track = fTrackInfos[nTracksCurrent];

      track.SetInnerParam( startPoint );
      track.SetInnerAlpha( startAlpha );
      track.SetOuterParam( endPoint );
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
      track.number = nTracksCurrent;
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

    }

///mvz start
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
    //std::cout<<"Unpack slice "<<iSlice<<": ntracks "<<slice.NTracks()<<"/"<<fSliceNTrackInfos[iSlice]<<std::endl;
  }
}



bool AliHLTTPCCAMerger::FitTrack( AliHLTTPCCATrackParam &T, float &Alpha,
                                    AliHLTTPCCATrackParam t0, float Alpha0,
                                    int hits[], int &NTrackHits, bool dir )
{
  // Fit the track

  AliHLTTPCCATrackParam::AliHLTTPCCATrackFitParam fitPar;
  AliHLTTPCCATrackParam t = t0;
  AliHLTTPCCATrackLinearisation l( t0 );

  bool first = 1;

  t.CalculateFitParameters( fitPar );

  int hitsNew[1000];
  int nHitsNew = 0;

  for ( int ihit = 0; ihit < NTrackHits; ihit++ ) {

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

//    if ( !t.TransportToXWithMaterial( x, l, fitPar, fSliceParam.GetBz( t ) ) ) continue;
    if ( !t.TransportToXWithMaterial( x, l, fitPar, fSliceParam.cBz( ) ) ) continue;

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
  ok = ok && ( t.GetX() > 50 );

  if ( c[0] <= 0 || c[2] <= 0 || c[5] <= 0 || c[9] <= 0 || c[14] <= 0 ) ok = 0;
//  if ( c[0] > 5. || c[2] > 5. || c[5] > 2. || c[9] > 2 || c[14] > 2. ) ok = 0;

  if ( CAMath::Abs( t.SinPhi() ) > .99 ) ok = 0;
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

  return ok;
}


float AliHLTTPCCAMerger::GetChi2( float x1, float y1, float a00, float a10, float a11, float x2, float y2, float b00, float b10, float b11)
{
  //* Calculate Chi2/ndf deviation

  float d[2] = { x1 - x2, y1 - y2 };

  float mSi[3] = { a00 + b00, a10 + b10, a11 + b11 };

  float s = ( mSi[0] * mSi[2] - mSi[1] * mSi[1] );

  if ( s < 1.E-10 ) return 10000.;

  float mS[3] = { mSi[2], -mSi[1], mSi[0] };

  return CAMath::Abs( ( ( mS[0]*d[0] + mS[1]*d[1] )*d[0]
                       + ( mS[1]*d[0] + mS[2]*d[1] )*d[1] ) / s *0.5 /*/ (NDF1+NDF2) */);
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
      AliHLTTPCCATrackParam t0 = track.InnerParam();
      AliHLTTPCCATrackParam t1 = track.OuterParam();

      double vxStart = t0.X();
      double vxEnd   = t1.X();
  
      double vyStart = t0.Y();
      double vyEnd   = t1.Y();

      float alpha = slices[iSlice]->Param().Alpha();
      float fSliceCos = TMath::Cos( alpha );
      float fSliceSin = TMath::Sin( alpha );


      double xStart = vxStart * fSliceCos - vyStart * fSliceSin;
      double yStart = vyStart * fSliceCos + vxStart * fSliceSin;
      double xEnd = vxEnd * fSliceCos - vyEnd * fSliceSin;
      double yEnd = vyEnd * fSliceCos + vxEnd * fSliceSin;

      double r1 = 1./(t0.QPt() * slices[iSlice]->Param().cBz());
      double r2 = 1./(t1.QPt() * slices[iSlice]->Param().cBz());

//std::cout << "r1  "<<r1<<"  r2  "<<r2 << endl;

      double R2 = r1*r1;
      double R = CAMath::Abs(r1);

      double a = r1/CAMath::Abs(r1);

      double xk = (xStart + xEnd)*0.5;
      double yk = (yStart + yEnd)*0.5;
      double l2 = (xStart - xEnd)*(xStart - xEnd) + (yStart - yEnd)*(yStart - yEnd);
      double l  = CAMath::Sqrt(l2);
      double li = 1./l;
      double d2 = R2 - l2*0.25;
      double d  = CAMath::Sqrt(d2);
      double xc = xk - a*d*li*(yEnd - yStart);
      double yc = yk + a*d*li*(xEnd - xStart);
  
      double fCos2 = fSliceCos*fSliceCos;
      double fSin2 = fSliceSin*fSliceSin;

      double dxStart2 = fSin2 * t0.GetErr2Y();
      double dyStart2 = fCos2 * t0.GetErr2Y();

      double dxEnd2 = fSin2 * t1.GetErr2Y();
      double dyEnd2 = fCos2 * t1.GetErr2Y();

      double ddx2 = dxStart2 + dxEnd2;
      double ddy2 = dyStart2 + dyEnd2;

      double dR2 = R2/(t0.QPt()*t0.QPt()) * t0.GetErr2QPt();

      double dl22 = ((xStart - xEnd)*(xStart - xEnd)*ddx2 + (yStart - yEnd)*(yStart - yEnd)*ddy2);
      double dl2  = li*li*dl22;
      double dd22 = R2*dR2 + 0.25*dl22;
      double dd2  = dd22/d2;


      double dxc2;
      double dyc2;

      double k = d*li*(yEnd - yStart);
      double ss = (yEnd - yStart)*(yEnd - yStart);
      dxc2 = 0.25*ddx2 + k*k*(ddy2/ss + dl2/(l*l) + dd2/d2);
      k = d*li*(xEnd - xStart);
      ss = (xEnd - xStart)*(xEnd - xStart);
      dyc2 = 0.25*ddy2 + k*k*(ddx2/ss + dl2/(l*l) + dd2/d2);

      double b = t0.DzDs();
      double c = (xStart-xc)/CAMath::Abs((xStart-xc));
      double sinPhi0 = (yStart-yc)/CAMath::Abs(r1);
      double p0 = asin(sinPhi0);
      double bErr2 = t0.Err2DzDs();
 //     double bErr2 = R2*t0.Err2DzDs() + t0.DzDs()*t0.DzDs()*dR2;
      double ys_yc2 = yStart - yc;
      ys_yc2 = ys_yc2*ys_yc2;
      double p0Err2 = (dyStart2 + dyc2 + dR2/R2*ys_yc2)/(R2-ys_yc2);

 //   if( 1./(CAMath::Abs(t0.QPt()*fSliceParam.cBz())) < 200. ) continue;

      double r11 = 1./(t1.QPt() * slices[iSlice]->Param().cBz());
      double R1 = CAMath::Abs(r11);

      double sinPhi1 = (yEnd-yc)/CAMath::Abs(r11);
      double p1 = asin(sinPhi1);

/*      if(R<R1)
      {
        std::cout << "R0 " << R << "   R1 "<< R1 << "   R0/R1  "<<R/R1 << std::endl;
        std::cout << "dR/dPhi " << fabs((R1 - R)/(p0 - p1)) << std::endl;
      }
      else
      {
        std::cout << "R0 " << R << "   R1 "<< R1 << "   R0/R1  "<<R1/R << std::endl;
        std::cout << "dR/dPhi " << fabs((R1 - R)/(p0 - p1)) << std::endl;
      }
std::cout << "phi0 "<<p0<<"   phi1 "<<p1<<std::endl;*/

      AliHLTTPCCABorderTrackGlobal &bTr = B[nB];

      double px0_0 = t0.GetCosPhi();
      double py0_0 = t0.GetSinPhi();

      double xc1_0 = t0.X() + py0_0*r1;
      double yc1_0 = t0.Y() - px0_0*r1;

      double xc1 = xc1_0 * fSliceCos - yc1_0 * fSliceSin;
      double yc1 = yc1_0 * fSliceCos + xc1_0 * fSliceSin;

      double px1_0 = t1.GetCosPhi();
      double py1_0 = t1.GetSinPhi();

      double xc2_0 = t1.X() + py1_0*r2;
      double yc2_0 = t1.Y() - px1_0*r2;

      double xc2 = xc2_0 * fSliceCos - yc2_0 * fSliceSin;
      double yc2 = yc2_0 * fSliceCos + xc2_0 * fSliceSin;

///      std::cout << "xc " << xc << " +- " << sqrt(dxc2) << "  " << xc1 << "  " << xc2 << std::endl;
///      std::cout << "yc " << yc << " +- " << sqrt(dyc2) << "  " << yc1 << "  " << yc2 << std::endl;

      float drdp = 1./(R*sqrt(1+b*b)) * (R1 - R)/(p0 - p1);
///      bTr.SetdRdPhi(fabs((R1 - R)/(p0 - p1)));
///      bTr.SetdRdPhi(fabs(drdp));
      bTr.SetdRdPhi(fabs((R1 - R)*b/(sqrt(1.+b*b)*(t0.Z()-t1.Z()))));
      bTr.SetQPt(t0.QPt());
      bTr.SetQPtErr2(t0.GetErr2QPt());

      bTr.SetX( t0.GetX() );
      {
        bTr.SetOK( 1 );
        bTr.SetTrackID( itr );
        bTr.SetNClusters( track.NClusters() );
        bTr.SetIRow( fClusterInfos[ track.FirstClusterRef() + 0 ].IRow() );
        bTr.SetSlice(iSlice);

        bTr.SetXStart(xStart);
        bTr.SetYStart(yStart);
        bTr.SetZStart(t0.Z());
        bTr.SetXStartErr2(dxStart2);
        bTr.SetYStartErr2(dyStart2);
        bTr.SetZStartErr2(t0.GetErr2Z());
  
        bTr.SetXEnd(xEnd);
        bTr.SetYEnd(yEnd);
        bTr.SetZEnd(t1.Z());
        bTr.SetXEndErr2(dxEnd2);
        bTr.SetYEndErr2(dyEnd2);
        bTr.SetZEndErr2(t1.GetErr2Z());
  
        bTr.SetXCentr(xc);
        bTr.SetYCentr(yc);
        bTr.SetXCentrErr2(dxc2);
        bTr.SetYCentrErr2(dyc2);
  
        bTr.SetR(R);
        bTr.SetRErr2(CAMath::Sqrt(dR2));

        bTr.Setb(b);
        bTr.Seta(a);
        bTr.SetbErr2(bErr2);

        bTr.Setp0(p0);
        bTr.Setp0Err2(p0Err2);

        bTr.Setc(c);
        bTr.SetNDF(t0.NDF());
        bTr.SetChiBest(1000000.);
        nB++;
      }
    }
  }
}

void AliHLTTPCCAMerger::CalculateHelix( AliHLTTPCCABorderTrackGlobal *bL, float z, float &x, float &y, float &dx2, float &dy2)
{
  const float &R = bL->R();
  const float &b = bL->b();
  const float &xc = bL->XCentr();
  const float &yc = bL->YCentr();
  const float &phi0 = bL->p0();
  const float &c = bL->c();
  const float &zStart = bL->ZStart();

  float phi = phi0 + c*bL->a()*(z-zStart)/(b*R);
  y = yc + R*sin(phi);
  x = xc + c*R*cos(phi);

  float R2 = R*R;
  float b2 = b*b;
  float b2R2i = R2*b2;
  b2R2i = 1./b2R2i;

  float dphi2 = bL->p0Err2() + b2R2i*(bL->ZStartErr2() + (z-zStart)*(z-zStart)*(bL->bErr2()/b2 + bL->RErr2()/R2));

//  std::cout <<"R    "<< R2*bL->p0Err2()<<"  "<<bL->RErr2() << "  " << bL->XCentrErr2()<<"  "<<dx2/(x*x) <<std::endl;

  dx2 = bL->XCentrErr2() + cos(phi)*cos(phi)*bL->RErr2() + R2*sin(phi)*sin(phi)*dphi2;
  dy2 = bL->YCentrErr2() + sin(phi)*sin(phi)*bL->RErr2() + R2*cos(phi)*cos(phi)*dphi2;
}

void AliHLTTPCCAMerger::CalculateR(const float &x1, const float &y1, const float &x2, const float &y2, const float &x3, const float &y3, float &R)
{
  float S = 0.5*fabs((x2-x1)*(y3-y1)-(x3-x1)*(y2-y1));
  float a2 = (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2);
  float b2 = (x1-x3)*(x1-x3) + (y1-y3)*(y1-y3);
  float c2 = (x3-x2)*(x3-x2) + (y3-y2)*(y3-y2);
  R = c2*a2*b2;
  R = sqrt(R);
  R = 0.25*R/S;

/*  float dS2 = 0.5*0.5*((y3-y1)*(y3-y1)*(dx2+dx1)+(x2-x1)*(x2-x1)*(dy3+dy1) + (x3-x1)*(x3-x1)*(dy2+dy1)+(y2-y1)*(y2-y1)*(dx3+dx1));
  float da2 = ((x1-x2)*(x1-x2)*(dx1 + dx2) + (y1-y2)*(y1-y2)*(dy1 + dy2))/a2;
  float db2 = ((x1-x3)*(x1-x3)*(dx1 + dx3) + (y1-y3)*(y1-y3)*(dy1 + dy3))/b2;
  float dc2 = ((x3-x2)*(x3-x2)*(dx3 + dx2) + (y3-y2)*(y3-y2)*(dy3 + dy2))/c2;

  dR2 = R*R*(da2/a2 + db2/b2 + dc2/c2 + dS2/(S*S));*/
}

void AliHLTTPCCAMerger::CalculatedRToPoint(AliHLTTPCCABorderTrackGlobal *b, const float &x, const float &y, const float &dx2, const float &dy2, float &R, float &dR)
{
  float Rx = x - b->XCentr();
  float Ry = y - b->YCentr();
  float Rx2 = Rx*Rx;
  float Ry2 = Ry*Ry;
  float Rr2 = Rx2 + Ry2;
  R = sqrt(Rr2);

  float dRx2 = dx2 + b->XCentrErr2();
  float dRy2 = dy2 + b->YCentrErr2();
  float dRr2 = (Rx2*dRx2 + Ry2*dRy2)/Rr2;
  dR = sqrt(dRr2);
}

void AliHLTTPCCAMerger::SplitBorderTracksGlobal( AliHLTTPCCABorderTrackGlobal B1[], int N1, AliHLTTPCCABorderTrackGlobal B2[], int N2, int number)
{
  //* split two sets of tracks
  float factor2k = 2.0;//2.2;

//  factor2k  = 3.5 * 3.5 * factor2k * factor2k;
  factor2k = 10. * 10.;

  int iSlice1, iSlice2;
  bool IsNext;

  int mode = 0;  

  int nNeighSlices;

  if(mode == 0)  nNeighSlices = 6;
  if(mode == 1)  nNeighSlices = 10;

  int kkk = nNeighSlices*0.5;
  int nnn = (kkk - 1)*0.5;
  int MidSlice = fgkNSlices*0.5;

  int NeighSlice[fgkNSlices][nNeighSlices];

//nNeighSlices =2;
//nnn=0;
//kkk=2;

  for(int j=0; j<kkk; j++)
  {
    for(int i=0; i<MidSlice; i++)
    {
      NeighSlice[i][j] = i + j - nnn;
      if(NeighSlice[i][j] >= MidSlice) NeighSlice[i][j] -= MidSlice;
      if(NeighSlice[i][j] < 0) NeighSlice[i][j] += MidSlice;

      NeighSlice[i][j+kkk] = 22 - NeighSlice[i][j]; 
      if(NeighSlice[i][j+kkk]<MidSlice) NeighSlice[i][j+kkk] += MidSlice;
    }
    for(int i=MidSlice; i<fgkNSlices; i++)
    {
      NeighSlice[i][j] = i + j - nnn;
      if(NeighSlice[i][j] >= fgkNSlices) NeighSlice[i][j] -= MidSlice;
      if(NeighSlice[i][j] < MidSlice) NeighSlice[i][j] += MidSlice;

      NeighSlice[i][j+kkk] = 22 - NeighSlice[i][j]; 
      if(NeighSlice[i][j+kkk]<0) NeighSlice[i][j+kkk] += MidSlice;
    }
  }

  for ( int i1 = 0; i1 < N1; i1++ ) {
    AliHLTTPCCABorderTrackGlobal &b1 = B1[i1];
    iSlice1 = b1.Slice();
//    if ( !b1.OK() ) continue;
//    if ( b1.NClusters() < minNPartHits ) continue;
    int iBest2 = -1;
    int iSliceBest2 = -1;
    int lBest2 = 0;
    bool IsNext2 = 1;
    float hiBest2 = 100000.;
    float hi = 10000000.;
    float chi2;

    float r_min2 = 10000000.;
    float dr_min2_local = 10000000.;

    AliHLTTPCCASliceTrackInfo *Ttt = &fTrackInfos[fSliceTrackInfoStart[iSlice1] + b1.TrackID() ];
    AliHLTTPCCATrackParam To = Ttt->OuterParam();
    AliHLTTPCCATrackParam Ti = Ttt->InnerParam();
    Ti.TransportToX( To.X(), fSliceParam.cBz());
///    std::cout << fabs(Ti.Y() - To.Y()) <<"  " << To.X() - Ti.X()<<std::endl;


    //int start2 = ( iSlice1 != iSlice2 ) ? 0 : i1 + 1;
    for ( int i2 = i1+1; i2 < N2; i2++ ) {
      IsNext = 1;
      if(i1==i2) continue;
      AliHLTTPCCABorderTrackGlobal &b2 = B2[i2];
      iSlice2 = b2.Slice();

      bool IsNeigh = 0;
      for(int i=0; i<nNeighSlices; i++)
        if( iSlice2 == NeighSlice[iSlice1][i]) IsNeigh = 1;
      if( iSlice2 == iSlice1) IsNeigh = 1;
      if(!IsNeigh) continue;


/*      if(IsNext)
      {
        if(b2.ZStart()<b1.ZEnd()) continue;
      }
      else if(b1.ZStart()<b2.ZEnd()) continue;*/

      float xyS1, xyS2,xyE1, xyE2, dxy, dz12, dy12, dx12;

      AliHLTTPCCASliceTrackInfo *Tt1 = &fTrackInfos[fSliceTrackInfoStart[iSlice1] + b1.TrackID() ];
      AliHLTTPCCASliceTrackInfo *Tt2 = &fTrackInfos[fSliceTrackInfoStart[iSlice2] + b2.TrackID() ];

      AliHLTTPCCASliceTrackInfo *trackDraw1, *trackDraw;

      xyE1 = Tt1->OuterParam().X();
      xyS1 = Tt1->InnerParam().X();
      xyE2 = Tt2->OuterParam().X();
      xyS2 = Tt2->InnerParam().X();

      AliHLTTPCCATrackParam Thelp, Thelp1;

      if(xyS1 < xyS2)
      {
        dxy = xyS2 - xyE1;
        dz12 = b2.ZStart() - b1.ZEnd();
        dy12 = b2.YStart() - b1.YEnd();
        dx12 = b2.XStart() - b1.XEnd();
        IsNext=1;
        //if(b2.ZStart()<b1.ZEnd()) continue;
        if(dxy > 0)
        {
          Thelp = Tt1->OuterParam();
          Thelp1 = Tt2->InnerParam();

          trackDraw = Tt1;
          trackDraw1 = Tt2;

          float a1 = slices[iSlice1]->Param().Alpha();
//        std::cout << iSlice1 <<"  "<<a1<< std::endl;
          float a2 = slices[iSlice2]->Param().Alpha();
//        std::cout << iSlice2 <<"  "<<a2<< std::endl;
          Thelp.Rotate( a2 - a1 , 0.999 );
          Thelp.TransportToX( Tt2->InnerParam().X(), fSliceParam.cBz());
        }
        else
        {
          Thelp = Tt2->InnerParam();
          trackDraw = Tt2;
          float dist1, dist2;
          dist1 = sqrt(Thelp.X() - Tt1->OuterParam().X())*(Thelp.X() - Tt1->OuterParam().X()) + 
                  (Thelp.Y() - Tt1->OuterParam().Y())*(Thelp.Y() - Tt1->OuterParam().Y());
          dist2 = sqrt(Thelp.X() - Tt1->InnerParam().X())*(Thelp.X() - Tt1->InnerParam().X()) + 
                  (Thelp.Y() - Tt1->InnerParam().Y())*(Thelp.Y() - Tt1->InnerParam().Y());

          float a1 = slices[iSlice1]->Param().Alpha();
//        std::cout << iSlice1 <<"  "<<a1<< std::endl;
          float a2 = slices[iSlice2]->Param().Alpha();
//        std::cout << iSlice2 <<"  "<<a2<< std::endl;
          float da;
          if(dist2 > dist1)
          {
            Thelp1 = Tt1->OuterParam();
            trackDraw1 = Tt1;
            da = a1-a2;
          }
          else
          {
            Thelp = Tt1->InnerParam();
            Thelp1 = Tt2->InnerParam();
            trackDraw = Tt1;
            trackDraw1 = Tt2;
            da = a2-a1;
          }
          Thelp.Rotate( da , 0.999 );
          Thelp.TransportToX( Thelp1.X(), fSliceParam.cBz());
        }
      }
      else
      {
        dxy = xyS1 - xyE2;
        dz12 = b1.ZStart() - b2.ZEnd();
        dy12 = b1.YStart() - b2.YEnd();
        dx12 = b1.XStart() - b2.XEnd();
        IsNext=0;
        //if(b1.ZStart()<b2.ZEnd()) continue;
        if(dxy > 0)
        {
          Thelp = Tt2->OuterParam();
          Thelp1 = Tt1->InnerParam();
          trackDraw = Tt2;
          trackDraw1 = Tt1;
          float a1 = slices[iSlice1]->Param().Alpha();
//        std::cout << iSlice1 <<"  "<<a1<< std::endl;
          float a2 = slices[iSlice2]->Param().Alpha();
//        std::cout << iSlice2 <<"  "<<a2<< std::endl;
          Thelp.Rotate( a1 - a2 , 0.999 );
          Thelp.TransportToX( Tt1->InnerParam().X(), fSliceParam.cBz());
        }
        else
        {
          Thelp = Tt1->InnerParam();
          trackDraw = Tt1;
          float dist1, dist2;
          dist1 = sqrt(Thelp.X() - Tt2->OuterParam().X())*(Thelp.X() - Tt2->OuterParam().X()) + 
                  (Thelp.Y() - Tt2->OuterParam().Y())*(Thelp.Y() - Tt2->OuterParam().Y());
          dist2 = sqrt(Thelp.X() - Tt2->InnerParam().X())*(Thelp.X() - Tt2->InnerParam().X()) + 
                  (Thelp.Y() - Tt2->InnerParam().Y())*(Thelp.Y() - Tt2->InnerParam().Y());

          float a1 = slices[iSlice1]->Param().Alpha();
//        std::cout << iSlice1 <<"  "<<a1<< std::endl;
          float a2 = slices[iSlice2]->Param().Alpha();
//        std::cout << iSlice2 <<"  "<<a2<< std::endl;
          float da;
          if(dist2 > dist1)
          {
            Thelp1 = Tt2->OuterParam();
            trackDraw1 = Tt2;
            da = a1-a2;
          }
          else
          {
            Thelp = Tt2->InnerParam();
            Thelp1 = Tt1->InnerParam();
            da = a2 - a1;
          }
          Thelp.Rotate( da , 0.999 );
          Thelp.TransportToX( Thelp1.X(), fSliceParam.cBz());
        }
      }

      dr_min2_local = dy12*dy12 + dx12*dx12 + dz12*dz12;

      if(number == 1)  if(dxy < 0) continue;
///continue;
 //     if(CAMath::Abs(Thelp.X() - Thelp1.X())>10.) continue;
//std::cout << "x1  " << Thelp.X() << "  x2  "<< Thelp1.X() <<"  y1   "<< Thelp.Y() <<"  y2  "<<Thelp1.Y() << std::endl;

      if((Thelp1.Y() - Thelp.Y())*(Thelp1.Y() - Thelp.Y()) > 100*(Thelp1.Err2Y() + Thelp.Err2Y())) continue;
      if((Thelp1.Z() - Thelp.Z())*(Thelp1.Z() - Thelp.Z()) > 100*(Thelp1.Err2Z() + Thelp.Err2Z())) continue;

      if(fabs(Thelp1.Y() - Thelp.Y())>10.) continue;
      if(fabs(Thelp1.Z() - Thelp.Z())>10.) continue;

      if((Thelp1.SinPhi() - Thelp.SinPhi())*(Thelp1.SinPhi() - Thelp.SinPhi()) > 100*(Thelp1.Err2SinPhi() + Thelp.Err2SinPhi())) continue;
//std::cout << fabs(Thelp1.SinPhi() - Thelp.SinPhi()) << "  "<<sqrt(Thelp1.Err2SinPhi() + Thelp.Err2SinPhi())<<std::endl;
      if(fabs(Thelp1.SinPhi() - Thelp.SinPhi())>0.15) continue;

      //if(fabs(YsYe1*dz12 - ZsZe1*dy12) > fabs(YsYe1*dz12)) continue;
      //if(fabs(YsYe2*dz12 - ZsZe2*dy12) > fabs(YsYe2*dz12)) continue;

      AliHLTTPCCABorderTrackGlobal *bLong;
      AliHLTTPCCABorderTrackGlobal *bShort;

      if (b1.NClusters()>b2.NClusters())
//      if (b1.NClusters()>b2.NClusters())
//      if     (b1.NClusters()>1.5*b2.NClusters()) {bLong = &b1; bShort = &b2;}
//      else if(b2.NClusters()>1.5*b1.NClusters()) {bLong = &b2; bShort = &b1;}
//      else if(b1.R()<b2.R())
      { bLong = &b1; bShort = &b2; }
      else
      { bLong = &b2; bShort = &b1; }

      float dqpt2 = fabs(b1.QPt()) - fabs(b2.QPt());
      dqpt2 = dqpt2*dqpt2;
      float ddqpt2 = b1.QPtErr2() + b2.QPtErr2();

      float dr2 = b1.R() - b2.R();
      dr2 = dr2*dr2;
      float ddr2 = b1.RErr2() + b2.RErr2();
//      if(dr2 > 4*factor2k * ddr2) continue;
//      if(fabs(b1.R() - b2.R()) > 0.5*(b1.R() + b2.R())) continue;

//      float db2 = CAMath::Abs(b1.b()) - CAMath::Abs(b2.b());
      float db2 = (b1.b()) - (b2.b());
      db2 = db2*db2;
      float ddb2 = b1.bErr2() + b2.bErr2();
      //if(db2 > factor2k * ddb2) continue;

      float dxc2 = b1.XCentr() - b2.XCentr();
      dxc2 = dxc2*dxc2;
      float ddxc2 = b1.XCentrErr2() + b2.XCentrErr2();

//      if(dxc2 > 4*factor2k * ddxc2 ) continue;
//std::cout << sqrt(dxc2) << "  " << sqrt(ddxc2) << std::endl;

      float dyc2 = b1.YCentr() - b2.YCentr();
      dyc2 = dyc2*dyc2;
      float ddyc2 = b1.YCentrErr2() + b2.YCentrErr2();
//      if(dyc2 > 4*factor2k * ddyc2 ) continue;

      hi=0;

      //if(dqpt2>factor2k *ddqpt2) continue;
      //if(dr2 > factor2k * ddr2) continue;
      if(db2 > factor2k * ddb2) continue;
      //if(bLong->R() > 150 || bShort->R() < 50)      if(dqpt2>factor2k *ddqpt2) continue;
//if(bLong->NClusters() > 15 && bShort->NClusters() > 15)      if(dqpt2>factor2k *ddqpt2) continue;

        //if(dxc2 > factor2k * ddxc2 ) continue;
        //if(dyc2 > factor2k * ddyc2 ) continue
      hi += db2/(factor2k * ddb2) + dxc2/(factor2k * ddxc2) + dyc2/(factor2k * ddyc2);

if(number == 0 &&0){
#ifdef MAIN_DRAW
      AliHLTTPCCADisplay::Instance().ClearView();
      AliHLTTPCCADisplay::Instance().SetTPCView();
      AliHLTTPCCADisplay::Instance().DrawTPC();

      for(int ihit=0; ihit<trackDraw->NClusters(); ihit++)
      {
        AliHLTTPCCAClusterInfo &h = fClusterInfos[trackDraw->FirstClusterRef() + ihit];
        AliHLTTPCCADisplay::Instance().SetCurrentSlice( slices[h.ISlice()] );
        AliHLTTPCCADisplay::Instance().DrawPoint(h.X(), h.Y(), h.Z(), 1, 1 );
      }

      for(int ihit=0; ihit<trackDraw1->NClusters(); ihit++)
      {
        AliHLTTPCCAClusterInfo &h = fClusterInfos[trackDraw1->FirstClusterRef() + ihit];
        AliHLTTPCCADisplay::Instance().SetCurrentSlice( slices[h.ISlice()] );
        AliHLTTPCCADisplay::Instance().DrawPoint(h.X(), h.Y(), h.Z(), 2, 1 );
      }

      AliHLTTPCCADisplay::Instance().SetCurrentSlice( slices[fClusterInfos[trackDraw1->FirstClusterRef()].ISlice()] );
      AliHLTTPCCADisplay::Instance().DrawPoint(Thelp.X(), Thelp.Y(), Thelp.Z(), 0, 1 );

      AliHLTTPCCADisplay::Instance().Ask();
#endif
}

     {
      AliHLTTPCCASliceTrackInfo track = fTrackInfos[fSliceTrackInfoStart[bLong->Slice()] + bLong->TrackID()];

      AliHLTTPCCATrackParam startPoint = track.InnerParam(), endPoint = track.OuterParam();
      float startAlpha = track.InnerAlpha(), endAlpha = track.OuterAlpha();

      int hits[2000];
      int firstHit = 1000;
      int nHits = 0;

      {
        track.SetUsed( 1 );
        for ( int jhit = 0; jhit < track.NClusters(); jhit++ ) {
          int id = track.FirstClusterRef() + jhit;
          hits[firstHit+jhit] = id;
        }
        nHits = track.NClusters();
      }

      AliHLTTPCCASliceTrackInfo segment = fTrackInfos[fSliceTrackInfoStart[bShort->Slice()] + bShort->TrackID()];
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

        for ( int jhit = 0; jhit < segment.NClusters(); jhit++ ) {
          int id = segment.FirstClusterRef() + jhit;
          hits[startHit+( dir ?( segment.NClusters()-1-jhit ) :jhit )] = id;
        }
        nHits += segment.NClusters();

      if ( endPoint.X() < startPoint.X() ) { // swap
        for ( int i = 0; i < nHits; i++ ) hits[i] = hits[firstHit+nHits-1-i];
        firstHit = 0;
      }

      endPoint = startPoint;
  ///mmm    if ( !FitTrack( endPoint, endAlpha, startPoint, startAlpha, hits + firstHit, nHits, 0 ) ) continue;
  ///mmm    if ( !FitTrack( startPoint, startAlpha, endPoint, endAlpha, hits + firstHit, nHits, 1 ) ) continue;

      AliHLTTPCCATrackParam &p = startPoint;
      chi2 = p.Chi2();
        bool ok = 1;

        const float *c = p.Cov();
        for ( int i = 0; i < 15; i++ ) ok = ok && finite( c[i] );
        for ( int i = 0; i < 5; i++ ) ok = ok && finite( p.Par()[i] );
        //ok = ok && ( p.GetX() > 50 ); // TODO: read from file!!

        if ( c[0] <= 0 || c[2] <= 0 || c[5] <= 0 || c[9] <= 0 || c[14] <= 0 ) ok = 0;
        if ( c[0] > 5. || c[2] > 5. || c[5] > 2. || c[9] > 2 || c[14] > 2 ) ok = 0;
 ///mmm       if ( !ok ) continue;
     }

//      if (hiBest2 < dr2/ddr2 + db2/ddb2 + dxc2/ddxc2 + dyc2/ddyc2 ) continue;
//      hiBest2 = dr2/ddr2 + db2/ddb2 + dxc2/ddxc2 + dyc2/ddyc2;
      iBest2 = b2.TrackID();
      iSliceBest2 = iSlice2;
      IsNext2 = IsNext;

      AliHLTTPCCASliceTrackInfo *T1, *T2;
      int NextNew, SliceNextNew, PrevNew, SlicePrevNew;
      if(IsNext)
      {
        T1 = &fTrackInfos[fSliceTrackInfoStart[iSlice1] + b1.TrackID() ];
        T2 = &fTrackInfos[fSliceTrackInfoStart[iSliceBest2] + iBest2 ];
        /*if(hi < b1.ChiBest())
        {
          b1.SetChiBest(hi);
          T1 -> SetBestNeighbour(fSliceTrackInfoStart[iSliceBest2] + iBest2);
        }
        if(hi < b2.ChiBest())
        {
          b2.SetChiBest(hi);
          T2 -> SetBestNeighbour(fSliceTrackInfoStart[iSlice1] + b1.TrackID());
        }*/
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
/*#ifdef MAIN_DRAW
      CalculateHelix(p0, c, b1.ZStart(), z0, xc, yc, R, b, x1,y1);
      AliHLTTPCCADisplay::Instance().DrawPoint(x1, y1, b1.ZStart(), 1, 1 );

      CalculateHelix(p0, c, b2.ZStart(), z0, xc, yc, R, b, x1,y1);
      AliHLTTPCCADisplay::Instance().DrawPoint(x1, y1, b2.ZStart(), 1, 1 );

      CalculateHelix(p0, c, b1.ZEnd(), z0, xc, yc, R, b, x1,y1);
      AliHLTTPCCADisplay::Instance().DrawPoint(x1, y1, b1.ZEnd(), 2, 1 );

      CalculateHelix(p0, c, b2.ZEnd(), z0, xc, yc, R, b, x1,y1);
      AliHLTTPCCADisplay::Instance().DrawPoint(x1, y1, b2.ZEnd(), 2, 1 );

      if(IsNext2)
        AliHLTTPCCADisplay::Instance().DrawHelix(p0, c, zMax2, zMin1, z0, xc, yc, R, b, 2, 0.8);
      else;
        AliHLTTPCCADisplay::Instance().DrawHelix(p0, c, zMax1, zMin2, z0, xc, yc, R, b, 2, 0.8);
#endif*/
    }
  }
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

  AliHLTTPCCABorderTrackGlobal *bCurr = new AliHLTTPCCABorderTrackGlobal[maxNSliceTracks*10*24];
  int nCurr = 0;
  MakeBorderTracksGlobal(bCurr,nCurr);
  SplitBorderTracksGlobal( bCurr, nCurr, bCurr, nCurr, number );

  if ( bCurr ) delete[] bCurr;

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
  AliHLTTPCCASliceTrackInfo tmpT[fgkNSlices][fMaxTrackInfos];

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

      AliHLTTPCCASliceTrackInfo &track = tmpT[iSlice][nTrNew[iSlice]];
      track = trackOld;

      AliHLTTPCCATrackParam startPoint = track.InnerParam(), endPoint = track.OuterParam();
      AliHLTTPCCATrackParam startPoint1, endPoint1;
      float startAlpha = track.InnerAlpha(), endAlpha = track.OuterAlpha();

      if ( endPoint.X() < startPoint.X() ) {
        AliHLTTPCCATrackParam helpPoint = endPoint;
        endPoint = startPoint;
        startPoint = helpPoint;
      }

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

      endPoint = startPoint;
  ///    if ( !FitTrack( endPoint, endAlpha, startPoint, startAlpha, hits + firstHit, nHits, 0 ) ) continue;
  ///    if ( !FitTrack( startPoint, startAlpha, endPoint, endAlpha, hits + firstHit, nHits, 1 ) ) continue;

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
        if ( c[0] > 5. || c[2] > 5. || c[5] > 2. || c[9] > 2 || c[14] > 2 ) ok = 0;
///mvz        if ( !ok ) continue;
      }

      int   hits2[nHits];
      float Zhits[nHits];
      float Zm = 100000;
      float Zcur;
      int   hitCur=0;
      int   nHits2=0;

      bool  index[nHits];

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
      if ( endPoint.X() < startPoint.X() ) {
        AliHLTTPCCATrackParam helpPoint = endPoint;
        endPoint = startPoint;
        startPoint = helpPoint;
      }

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
      track.SetInnerParam(startPoint);
      track.SetOuterParam(endPoint);

      for(int iClu=0; iClu < nHits; iClu++) tmpH[nH + iClu] = fClusterInfos[hits[iClu]];
      nH += nHits;
//std::cout  << "  " << track.NClusters() << "  " << nH << std::endl;
    }
  }

  for ( int ih = 0; ih < nH; ih++ ) fClusterInfos[ih] = tmpH[ih];
  if(tmpH) delete[] tmpH;

  for(int iSlice=0; iSlice < fgkNSlices; iSlice++ )
  {
    fSliceNTrackInfos[iSlice] = nTrNew[iSlice];

    for ( int itr = 0; itr < nTrNew[iSlice]; itr++ ) 
      fTrackInfos[fSliceTrackInfoStart[iSlice]+itr] = tmpT[iSlice][itr];
  }
  //if(tmpT) delete[] tmpT;


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