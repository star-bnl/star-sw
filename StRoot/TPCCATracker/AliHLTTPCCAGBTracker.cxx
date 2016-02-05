// $Id: AliHLTTPCCAGBTracker.cxx,v 1.1 2016/02/05 23:27:27 fisyak Exp $
// **************************************************************************
// This file is property of and copyright by the ALICE HLT Project          *
// ALICE Experiment at CERN, All rights reserved.                           *
//                                                                          *
// Primary Authors: Sergey Gorbunov <sergey.gorbunov@kip.uni-heidelberg.de> *
//                  Ivan Kisel <kisel@kip.uni-heidelberg.de>                *
//                  for The ALICE HLT Project.                              *
//                                                                          *
// Developed by:   Igor Kulakov <I.Kulakov@gsi.de>                          *
//                 Maksym Zyzak <M.Zyzak@gsi.de>                            *
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



#include "AliHLTTPCCAGBTracker.h"
#include "AliHLTTPCCAGBHit.h"
#include "AliHLTTPCCAOutTrack.h"
#include "AliHLTTPCCATracker.h"
#include "AliHLTTPCCAGBTrack.h"
#include "AliHLTTPCCATrackParam.h"
#include "AliHLTTPCCAMerger.h"
#include "AliHLTTPCCAMergerOutput.h"
#include "AliHLTTPCCADataCompressor.h"
#include "AliHLTArray.h"
#include "AliHLTTPCCAMath.h"
#include "AliHLTTPCCATrackLinearisation.h"
#include "AliHLTTPCCAClusterData.h"
#include "Stopwatch.h"
#include <algorithm>
#include <fstream>
#include <iostream>
using namespace std;

#ifdef MAIN_DRAW
#include "AliHLTTPCCADisplay.h"
#include "AliHLTTPCCAPerformance.h"
#include "AliHLTTPCCASliceOutput.h"
#endif

#ifdef USE_TBB
#include <tbb/blocked_range.h>
#include <tbb/parallel_for.h>
#include <tbb/parallel_sort.h>
#include <tbb/partitioner.h>
#include <tbb/spin_mutex.h>
#include <tbb/task_scheduler_init.h>
#endif //USE_TBB
using namespace std;
bool SINGLE_THREADED = false;

AliHLTTPCCAGBTracker::AliHLTTPCCAGBTracker()
    :
    fNSlices( 0 ),
    fHits( 0 ),
    fExt2IntHitID( 0 ),
    fNHits( 0 ),
    fTrackHits( 0 ),
    fTracks( 0 ),
    fNTracks( 0 ),
    fMerger( 0 ),
    fClusterData( 0 ),
    fTime( 0 ),
    fStatNEvents( 0 ),
    fSliceTrackerTime( 0 ),
    fSliceTrackerCpuTime( 0 )
{
  //* constructor
  for ( int i = 0; i < 20; i++ ) fStatTime[i] = 0;
  fMerger = new AliHLTTPCCAMerger;
}

void AliHLTTPCCAGBTracker::Init()
{
  fNSlices = 0;
  fNHits = 0;
  fTrackHits = 0;
  fTracks = 0;
  fNTracks = 0;
  fTime = 0.;
  fStatNEvents = 0;
  fSliceTrackerTime = 0.;
  fSliceTrackerCpuTime = 0.;
  for ( int i = 0; i < 20; ++i ) {
    fStatTime[i] = 0.;
  }
}

AliHLTTPCCAGBTracker::~AliHLTTPCCAGBTracker()
{
  //* destructor
  StartEvent();
  if (fMerger) delete fMerger;
}

void AliHLTTPCCAGBTracker::SetNSlices( int N )
{
  //* set N of slices
  StartEvent();
  fNSlices = N;
  fSlices.Resize( N );
}

void AliHLTTPCCAGBTracker::StartEvent()
{
  //* clean up track and hit arrays

  if (fTrackHits) delete[] fTrackHits;
  fTrackHits = 0;
  if (fTracks) delete[] fTracks;
  fTracks = 0;
  if (fExt2IntHitID) delete[] fExt2IntHitID;
  fExt2IntHitID = 0;
  fNHits = 0;
  fNTracks = 0;
  for ( int i = 0; i < fNSlices; i++ ) fSlices[i].StartEvent();
}


void AliHLTTPCCAGBTracker::SetNHits( int nHits )
{
  //* set the number of hits
  fHits.Resize( nHits );
  fNHits = nHits;
  if (fExt2IntHitID) delete[] fExt2IntHitID;
  fExt2IntHitID = new int[ nHits ];
}

#ifdef USE_TBB
class Initializer
{
    AliHLTArray<int> &sliceNHits;
    AliHLTArray<int, 2> &rowNHits;
  public:
    inline Initializer( AliHLTArray<int> &_sliceNHits, AliHLTArray<int, 2> &_rowNHits )
        : sliceNHits( _sliceNHits ), rowNHits( _rowNHits ) {}

    inline void operator()( const tbb::blocked_range<int> &r ) const {
      for ( int i = r.begin(); i < r.end(); ++i ) {
        sliceNHits[i] = 0;
        for ( int ir = 0; ir < 200; ++ir ) {
          rowNHits( i, ir ) = 0;
        }
      }
    }
};

class ReconstructSliceTracks
{
    AliHLTArray<AliHLTTPCCATracker> &fSlices;
    double *fStatTime;
    tbb::spin_mutex &fMutex;
  public:
    inline ReconstructSliceTracks( AliHLTArray<AliHLTTPCCATracker> &fSlices_, double *fStatTime_, tbb::spin_mutex &fMutex_ )
        : fSlices( fSlices_ ), fStatTime( fStatTime_ ), fMutex( fMutex_ ) {}//  2.1. Data preparation  is done as follows:

    inline void operator()( const tbb::blocked_range<int> &r ) const {
      for ( int iSlice = r.begin(); iSlice < r.end(); ++iSlice ) {
#ifdef USE_TIMERS
        Stopwatch timer;
#endif // USE_TIMERS
        AliHLTTPCCATracker &slice = fSlices[iSlice];
        slice.Reconstruct();
#ifdef USE_TIMERS
        timer.Stop();
#endif // USE_TIMERS
        tbb::spin_mutex::scoped_lock lock( fMutex );
        //fTime+= timer.RealTime();
        //blaTime+= timer.RealTime();
#ifdef USE_TIMERS
        fStatTime[0] += timer.RealTime();
#endif // USE_TIMERS
        fStatTime[1] += slice.Timer( 0 );
        fStatTime[2] += slice.Timer( 1 );
        fStatTime[3] += slice.Timer( 2 );
        fStatTime[4] += slice.Timer( 3 );
        fStatTime[5] += slice.Timer( 4 );
        fStatTime[6] += slice.Timer( 5 );
        fStatTime[7] += slice.Timer( 6 );
        fStatTime[8] += slice.Timer( 7 );
        fStatTime[11] += slice.Timer( 10 );
      }
    }
};
#endif //USE_TBB

void AliHLTTPCCAGBTracker::FindTracks()
{
  //* main tracking routine
  fTime = 0;
  fStatNEvents++;

#ifdef MAIN_DRAW
  AliHLTTPCCAPerformance::Instance().SetTracker( this );
  AliHLTTPCCADisplay::Instance().Init();
  AliHLTTPCCADisplay::Instance().SetGB( this );
  AliHLTTPCCADisplay::Instance().SetTPC( fSlices[0].Param() );
#endif //MAIN_DRAW
  
#ifdef MAIN_DRAW

//   AliHLTTPCCADisplay::Instance().SetTPCView();
// /**  AliHLTTPCCADisplay::Instance().DrawTPC();
//   AliHLTTPCCADisplay::Instance().DrawGBHits( *this, -1, 0.2, 1 );
//   AliHLTTPCCADisplay::Instance().SaveCanvasToFile( "Hits.pdf");
//   AliHLTTPCCADisplay::Instance().Ask();
// */
// //   AliHLTTPCCADisplay::Instance().DrawGBHits( *this, kRed, 0.2, 2 );
// //   AliHLTTPCCADisplay::Instance().SaveCanvasToFile( "Hits_b.pdf");
// //   AliHLTTPCCADisplay::Instance().Ask();
#endif //MAIN_DRAW

 if ( fNHits <= 0 ) return; // TODO rid of it. Can be problems with performance

#ifdef USE_TBB
#ifndef NUM_THREADS
#define NUM_THREADS SINGLE_THREADED ? 1 : tbb::task_scheduler_init::automatic
#endif
  tbb::task_scheduler_init *taskScheduler = new tbb::task_scheduler_init( NUM_THREADS );
#undef NUM_THREADS
#endif //USE_TBB

  Stopwatch timer1;
  Stopwatch timer2;

#ifdef USE_TBB
  tbb::parallel_sort( fHits.Data(), fHits.Data() + fNHits, AliHLTTPCCAGBHit::Compare );
#else //USE_TBB
  std::sort( fHits.Data(), fHits.Data() + fNHits, AliHLTTPCCAGBHit::Compare );
  /// \brief The necessary data is transfered to the track-finder
/// The necessary data is transfered to the track-finder
///Data is structured and saved by track-finders for each sector. 
///To speed up the process  in each row 2D-grid with the bin size
///inversely proportional to the number of hits in the row is introduced.
///Hits are sorted by grid bins and for each grid bin 1st  hit is found and saved. 
///Such data structure allows to quickly   find closest hits to the point with given
///coordinates, which is required while neighbours hits are searched and additional 
///hits are attached to segments.
#endif //USE_TBB

  for ( int i = 0; i < 20; ++i ) {
    fStatTime[i] = 0.;
  }
  
//  GroupHits();
#ifdef USE_TIMERS
  timer1.Start();
#endif /// USE_TIMERS
  
  {
    int offset = 0;
    int nextSlice = 0;
    int numberOfUsedSlices = 0;

    fClusterData.Resize(fNSlices);
    do {
      fFirstSliceHit[nextSlice] = offset;
      AliHLTTPCCAClusterData &data = fClusterData[numberOfUsedSlices++];
      const int iSlice = fHits.Data()[offset].ISlice();
      data.readEvent( fHits.Data(), &offset, fNHits, fSlices[iSlice].Param().NRows8() );

      while ( nextSlice < iSlice ) {
        fSlices[nextSlice++].StartEvent();
        fFirstSliceHit[nextSlice] = fFirstSliceHit[nextSlice - 1];
        
      }
      ++nextSlice;
      /// give the data to the slice tracker
      fSlices[data.Slice()].ReadEvent( &data );
    } while ( offset < fNHits );
    while ( nextSlice < fNSlices ) {
      fFirstSliceHit[nextSlice] = offset;
      fSlices[nextSlice++].StartEvent();
    }
  }
  
#ifdef USE_TIMERS
  timer1.Stop();
  fStatTime[12] = timer1.RealTime();
#endif /// USE_TIMERS
  /// Read hits, row by row

#ifdef USE_TBB
  tbb::spin_mutex mutex;
#endif //USE_TBB

  /// Run the slice trackers in parallel. The mutex is only necessary for storing the timings after
  /// the reconstruction.
  timer2.Start();
#ifdef USE_TBB
  tbb::parallel_for( tbb::blocked_range<int>( 0, fNSlices, 1 ),
      ReconstructSliceTracks( fSlices, fStatTime, mutex ) );
#else //USE_TBB
  for ( int iSlice = 0; iSlice < fSlices.Size(); ++iSlice ) {
    Stopwatch timer;
    AliHLTTPCCATracker &slice = fSlices[iSlice];
    slice.Reconstruct();
    timer.Stop();
    fStatTime[0] += timer.RealTime();
    fStatTime[1] += slice.Timer( 0 );
    fStatTime[2] += slice.Timer( 1 );
    fStatTime[3] += slice.Timer( 2 );
    fStatTime[4] += slice.Timer( 3 );
    fStatTime[5] += slice.Timer( 4 );
    fStatTime[6] += slice.Timer( 5 );
    fStatTime[7] += slice.Timer( 6 );
    fStatTime[8] += slice.Timer( 7 );
    fStatTime[11] += slice.Timer( 10 );
  }
#endif //USE_TBB
  timer2.Stop();
  fSliceTrackerTime = timer2.RealTime();
  fSliceTrackerCpuTime = timer2.CpuTime();

  Stopwatch timerMerge;
  Merge();
  timerMerge.Stop();
  timer1.Stop();
  fStatTime[9] += timerMerge.RealTime();
  fStatTime[10] += timerMerge.CpuTime();
  //fTime+=timerMerge.RealTime();
  //std::cout<<"Merge time = "<<timerMerge.RealTime()*1.e3<<"ms"<<std::endl;
  //std::cout<<"End CA merging"<<std::endl;
  fTime += timer1.RealTime();
#if 0
#ifndef NDEBUG
  {
    int iHit = 0;
    for ( int i = 0; i < fNTracks; i++ ) {
      const AliHLTTPCCAGBTrack &t = fTracks[i];
      
      const AliHLTTPCCAGBHit &hF = fHits[fTrackHits[iHit]];
      const AliHLTTPCCAGBHit &hL = fHits[fTrackHits[iHit + t.NHits() - 1]];
      assert ( t.NHits() >= 3 );

      ASSERT( CAMath::Abs( t.InnerParam().X() - hF.X() ) < .01,  "Track " << i << ": " << t.InnerParam().X() << " == " << hF.X() );
      ASSERT( CAMath::Abs( t.OuterParam().X() - hL.X() ) < .01,  "Track " << i << ": " << t.OuterParam().X() << " == " << hL.X() );
      
      iHit += t.NHits();
    }
  }
#endif
#endif  
#ifdef MAIN_DRAW
  if ( AliHLTTPCCADisplay::Instance().DrawType() == 1 )
    AliHLTTPCCADisplay::Instance().Ask();
#endif // MAIN_DRAW
#ifdef USE_TBB
  if (taskScheduler) delete taskScheduler;
#endif //USE_TBB
}

void AliHLTTPCCAGBTracker::Merge()
{
  // test

// #ifdef MAIN_DRAW
//   AliHLTTPCCADisplay::Instance().SetTPCView();
//   AliHLTTPCCADisplay::Instance().DrawTPC();
//   AliHLTTPCCADisplay::Instance().DrawGBHits( *this, -1, 0.4, 1  );
//   std::cout << "Slice tracks:" << std::endl;
//   for ( int iSlice = 0; iSlice < fNSlices; iSlice++ ) {
//     AliHLTTPCCATracker &slice = fSlices[iSlice];
//     AliHLTTPCCADisplay::Instance().SetCurrentSlice( &slice );
//     for ( int itr = 0; itr < slice.Output()->NTracks(); itr++ ) {
//       AliHLTTPCCADisplay::Instance().DrawSliceOutTrack( itr, 2, 1 );
//     }
//   }
//   AliHLTTPCCADisplay::Instance().SaveCanvasToFile( "SectorTracks.pdf" );
//   AliHLTTPCCADisplay::Instance().Ask();
  
// /*  AliHLTTPCCADisplay::Instance().ClearView();
//   AliHLTTPCCADisplay::Instance().SetTPCView();
//   AliHLTTPCCADisplay::Instance().DrawTPC();
//   AliHLTTPCCADisplay::Instance().DrawGBHits( *this, -1, 0.4, 1  );
//   for ( int icl = 0; icl < NHits(); icl++ )
//   {
//     if(Hits()[icl].ISlice() != 6) continue;
//     int id  =Hits()[icl].ID();
//     int iRow = Hits()[icl].IRow();
//     int islice = Hits()[icl].ISlice();
//     const SliceData &data = fSlices[islice].Data();
//     const AliHLTTPCCARow &row = data.Row( iRow );
    
// //    int color = 
//     AliHLTTPCCADisplay::Instance().DrawGBPoint(*this,Hits()[icl].ISlice(), 
//                                  Hits()[icl].X(),
//                                       Hits()[icl].Y(), 
//                                            Hits()[icl].Z(),1,1);
//     int icl1 = fFirstSliceHit[islice] - data.ClusterDataIndex( row, id );
//     std::cout <<"N clustera  " <<icl <<"  IRow  "<< iRow <<"  Poschitannyj  "<<icl1;
//         std::cout   << std::endl;
//     AliHLTTPCCADisplay::Instance().Ask();
//   }

// */
// #endif //MAIN_DRAW


  AliHLTTPCCAMerger &merger = *fMerger;

  merger.Clear();
  merger.SetSliceParam( fSlices[0].Param() );

  for ( int i = 0; i < fNSlices; i++ ) {
    merger.SetSliceData( i, fSlices[i].Output() );
    merger.SetSlices(i, &fSlices[i]);
  }

///mvz start
/**
#ifdef MAIN_DRAW
  std::cout << "bdzyn! " << std::endl;
  AliHLTTPCCADisplay::Instance().ClearView();
  AliHLTTPCCADisplay::Instance().SetTPCView();
  AliHLTTPCCADisplay::Instance().DrawTPC();
  AliHLTTPCCADisplay::Instance().DrawGBHits( *this );
#endif*/
  merger.Reconstruct();
  assert( fNTimers >= merger.NTimers()+13-1 );
  for (int i = 0; i < merger.NTimers(); i++) {
    fStatTime[13+i] = merger.Timer(i);
  }

/**
#ifdef MAIN_DRAW
  AliHLTTPCCADisplay::Instance().Ask();
#endif // MAIN_DRAW
*/
///mvz end

  const AliHLTTPCCAMergerOutput &out = *( merger.Output() );

  if ( fTrackHits ) delete[] fTrackHits;
  fTrackHits = 0;
  if ( fTracks ) delete[] fTracks;
  fTracks = 0;
  fTrackHits = new int [out.NTrackClusters()];
  fTracks = new AliHLTTPCCAGBTrack[out.NTracks()];
  fNTracks = 0;

  int nTrackHits = 0;

  for ( int itr = 0; itr < out.NTracks(); itr++ ) {
    const AliHLTTPCCAMergedTrack &track = out.Track( itr );

    AliHLTTPCCAGBTrack &trackGB = fTracks[fNTracks];
    trackGB.SetFirstHitRef( nTrackHits );
    trackGB.SetNHits( track.NClusters() );
    trackGB.SetInnerParam( track.InnerParam() );
    trackGB.SetOuterParam( track.OuterParam() );
    trackGB.SetAlpha( track.InnerAlpha() );
    trackGB.SetDeDx( 0 );

    for ( int icl = 0; icl < track.NClusters(); icl++ ) {
      const DataCompressor::SliceRowCluster &iDsrc = out.ClusterIDsrc( track.FirstClusterRef() + icl );
      unsigned int iSlice = iDsrc.Slice();
      unsigned int iRow   = iDsrc.Row();
      unsigned int iClu   = iDsrc.Cluster();
      //const SliceData &data = fSlices[iSlice].Data();
      //const AliHLTTPCCARow &row = data.Row( iRow );
      fTrackHits[nTrackHits + icl] = fFirstSliceHit[iSlice] + fSlices[iSlice].ClusterData().RowOffset( iRow ) + iClu;/*data.ClusterDataIndex( row, iClu );*/
    }
         
//    if(itr==1 || itr == 2) std::cout << std::endl;
    nTrackHits += track.NClusters();
    fNTracks++;
  }
/*
#ifdef MAIN_DRAW
///mvz start
  for ( int iSlice = 0; iSlice < fNSlices; iSlice++ ) {
    AliHLTTPCCATracker &slice = fSlices[iSlice];
  for(int i=0; i<slice.NOutTracks1(); i++)
  {
    int slice_tr_id = slice.OutTrack1(i).OrigTrackID();
    slice.fOutTracks1[i].SetFirstHitRef(-1);
    slice.fOutTracks1[i].SetNHits(-1);
    for(int j=0; j<slice.NOutTracks(); j++)
    {
      if(slice_tr_id == slice.OutTrack(j).OrigTrackID())
      {
        slice.fOutTracks1[i].SetFirstHitRef(slice.OutTrack(j).FirstHitRef());
        slice.fOutTracks1[i].SetNHits(slice.OutTrack(j).NHits());
      }
    }
  }
}
///mvz end
  AliHLTTPCCADisplay::Instance().SetTPCView();
  AliHLTTPCCADisplay::Instance().DrawTPC();
  AliHLTTPCCADisplay::Instance().DrawGBHits( *this, -1, 0.4, 1  );
  int col = 2;
  for ( int iSlice = 0; iSlice < fNSlices; iSlice++ ) {
    AliHLTTPCCATracker &slice = fSlices[iSlice];
    AliHLTTPCCADisplay::Instance().SetCurrentSlice( &slice );
    for ( int itr = 0; itr < slice.NOutTracks1(); itr++ ) {
      AliHLTTPCCADisplay::Instance().DrawSliceOutTrack1( itr, 4, 2 );
    }
  }

  for ( int iSlice = 0; iSlice < fNSlices; iSlice++ ) {
    AliHLTTPCCATracker &slice = fSlices[iSlice];
    AliHLTTPCCADisplay::Instance().SetCurrentSlice( &slice );
    for ( int itr = 0; itr < slice.NOutTracks1(); itr++ ) {
      if(col>9) col = 2;
      if(col==4) col=5;
      std::cout << iSlice << "  ";
      AliHLTTPCCADisplay::Instance().DrawSliceOutTrackParam( itr, col, 3 );
      col++;
    }
  }
  AliHLTTPCCADisplay::Instance().Ask();
#endif
*/
// #ifdef MAIN_DRAW
//   std::cout << "Global tracks: " << std::endl;
// ///  AliHLTTPCCADisplay::Instance().ClearView();
//   AliHLTTPCCADisplay::Instance().SetTPCView();
//   AliHLTTPCCADisplay::Instance().DrawTPC();
//   AliHLTTPCCADisplay::Instance().DrawGBHits( *this );
// 
//   for ( int itr = 0; itr < fNTracks; itr++ ) {
// /*    AliHLTTPCCADisplay::Instance().ClearView();
//     AliHLTTPCCADisplay::Instance().SetTPCView();
//     AliHLTTPCCADisplay::Instance().DrawTPC();
//     AliHLTTPCCADisplay::Instance().DrawGBHits( *this );
//     AliHLTTPCCAGBTrack &trackGB = fTracks[itr];
//     
//     for ( int icl = 0; icl < trackGB.NHits(); icl++ ) {
//       AliHLTTPCCADisplay::Instance().DrawGBPoint(*this,Hits()[fTrackHits[trackGB.FirstHitRef() + icl]].ISlice(), 
//                                                  Hits()[fTrackHits[trackGB.FirstHitRef() + icl]].X(),
//                                                  Hits()[fTrackHits[trackGB.FirstHitRef() + icl]].Y(), 
//                                                  Hits()[fTrackHits[trackGB.FirstHitRef() + icl]].Z());
// if(itr==1 || itr==2)
// std::cout <<"x  "<< Hits()[fTrackHits[trackGB.FirstHitRef() + icl]].X() <<"  y  "<<Hits()[fTrackHits[trackGB.FirstHitRef() + icl]].Y()<<"  ";
// std::cout<<(Hits()[fTrackHits[trackGB.FirstHitRef()]]).ISlice()<<"  "<<
//     (Hits()[fTrackHits[trackGB.FirstHitRef()]]).IRow()<<"  "<<
//     (Hits()[fTrackHits[trackGB.FirstHitRef()]]).ID()<<std::endl;
//     }
// 
//     AliHLTTPCCADisplay::Instance().Ask();*/
//     AliHLTTPCCADisplay::Instance().DrawGBTrack( itr, kBlue, 2. );
//  //   AliHLTTPCCADisplay::Instance().Ask();
// //if(itr==1 || itr==2)std::cout <<std::endl;
//   }
//   AliHLTTPCCADisplay::Instance().SaveCanvasToFile( "GlobalTracks.pdf" );
//   AliHLTTPCCADisplay::Instance().Ask();
// #endif // MAIN_DRAW
}



bool AliHLTTPCCAGBTracker::FitTrack( AliHLTTPCCATrackParam &T, AliHLTTPCCATrackParam t0,
                                       float &Alpha, int hits[], int &NTrackHits,
                                       bool dir )
{
  // Fit the track

  //return fMerger->FitTrack( T, Alpha, t0, Alpha, hits, NTrackHits, dir );

  float alpha0 = Alpha;

  AliHLTTPCCATrackParam::AliHLTTPCCATrackFitParam fitPar;
  AliHLTTPCCATrackParam t = t0;
  AliHLTTPCCATrackLinearisation l( t0 );

  bool first = 1;

  t.CalculateFitParameters( fitPar );

  int hitsNew[1000];
  int nHitsNew = 0;

  for ( int ihit = 0; ihit < NTrackHits; ihit++ ) {

    int jhit = dir ? ( NTrackHits - 1 - ihit ) : ihit;
    AliHLTTPCCAGBHit &h = fHits[hits[jhit]];

    int iSlice = h.ISlice();

    float sliceAlpha =  fSlices[0].Param().Alpha( iSlice );

    if ( CAMath::Abs( sliceAlpha - alpha0 ) > 1.e-4 ) {
      if ( ! t.Rotate(  sliceAlpha - alpha0, l, .999 ) ) continue;
      alpha0 = sliceAlpha;
    }

    //float x = fSliceParam.RowX( h.IRow() );
    float x = h.X();

    if ( !t.TransportToXWithMaterial( x, l, fitPar, fSlices[0].Param().GetBz( t ) ) ) continue;

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


    float err2Y, err2Z;
///mvz start 20.01.2010
    //fSlices[0].Param().GetClusterErrors2( h.IRow(), h.Z(), l.SinPhi(), l.CosPhi(), l.DzDs(), err2Y, err2Z );
    fSlices[0].Param().GetClusterErrors2( h.IRow(), t0, err2Y, err2Z );
///mvz end 20.01.2010
    if ( !t.Filter( h.Y(), h.Z(), err2Y, err2Z ) ) continue;

    first = 0;

    hitsNew[nHitsNew++] = hits[jhit];
  }

  if ( CAMath::Abs( t.QPt() ) < 1.e-8 ) t.SetQPt( 1.e-8 );

  bool ok = 1;

  const float *c = t.Cov();
  for ( int i = 0; i < 15; i++ ) ok = ok && CAMath::Finite( c[i] );
  for ( int i = 0; i < 5; i++ ) ok = ok && CAMath::Finite( t.Par()[i] );
  ok = ok && ( t.GetX() > 50 );

  if ( c[0] <= 0 || c[2] <= 0 || c[5] <= 0 || c[9] <= 0 || c[14] <= 0 ) ok = 0;
  if ( c[0] > 5. || c[2] > 5. || c[5] > 2. || c[9] > 2 || c[14] > 2. ) ok = 0;

  if ( CAMath::Abs( t.SinPhi() ) > .99 ) ok = 0;
  else if ( l.CosPhi() >= 0 ) t.SetSignCosPhi( 1 );
  else t.SetSignCosPhi( -1 );

  if ( ok ) {
    T = t;
    Alpha = alpha0;
    NTrackHits = nHitsNew;
    for ( int i = 0; i < NTrackHits; i++ ) {
      hits[dir ?( NTrackHits-1-i ) :i] = hitsNew[i];
    }
  }
  return ok;
}



void AliHLTTPCCAGBTracker::WriteSettings( std::ostream &out ) const
{
  //* write settings to the file
  out << NSlices() << std::endl;
  for ( int iSlice = 0; iSlice < NSlices(); iSlice++ ) {
    out << fSlices[iSlice].Param();
  }
}

void AliHLTTPCCAGBTracker::ReadSettings( std::istream &in )
{
  //* Read settings from the file
  int nSlices = 0;
  in >> nSlices;
  SetNSlices( nSlices );
  for ( int iSlice = 0; iSlice < NSlices(); iSlice++ ) {
    AliHLTTPCCAParam param;
    in >> param;

      // remove 13 row (iRow = 12) IKu
//     param.SetNRows(param.NRows()-1);
//     for (int iRow = 12; iRow < param.NRows(); iRow++){
//       param.SetRowX(param.RowX(iRow+1)-1,iRow);
//     }
        
    fSlices[iSlice].Initialize( param );
  }
}

void AliHLTTPCCAGBTracker::WriteEvent( FILE *file ) const
{
  // write event to the file

  const int nHits = NHits();
  int written = std::fwrite( &nHits, sizeof( int ), 1, file );
  assert( written == 1 );
  written = std::fwrite( fHits.Data(), sizeof( AliHLTTPCCAGBHit ), nHits, file );
  assert( written == nHits );
  std::fflush( file );
  UNUSED_PARAM1(written);
}

void AliHLTTPCCAGBTracker::ReadEvent( FILE *file )
{
  //* Read event from file

  StartEvent();
  int nHits;
  int read = std::fread( &nHits, sizeof( int ), 1, file );
  assert( read == 1 );
  SetNHits( nHits );
  read = std::fread( fHits.Data(), sizeof( AliHLTTPCCAGBHit ), nHits, file );
  assert( read == nHits );
  UNUSED_PARAM1(read);
    // check
//   for (int iHit = 0; iHit <= fHits.Size(); iHit++){
//     std::cout << "iHit " << iHit << std::endl;
//     std::cout << fHits.Data()[iHit].X() << " "
//               << fHits.Data()[iHit].Y() << " "
//               << fHits.Data()[iHit].Z() << std::endl;
//     std::cout << fHits.Data()[iHit].ErrX() << " "
//               << fHits.Data()[iHit].ErrY() << " "
//               << fHits.Data()[iHit].ErrZ() << std::endl;
//   }
  
    // remove 13 row (iRow = 12) IKu
//   for (int iHit = 0; iHit <= fHits.Size(); iHit++){
//     if (fHits.Data()[iHit].IRow() >= 13)
//       fHits.Data()[iHit].SetIRow(fHits.Data()[iHit].IRow()-1);
//   }
}

void AliHLTTPCCAGBTracker::WriteTracks( const string& prefix ) const
{
  //* Write tracks to file
  ofstream out((prefix+"tracks.data").data());

  // out << fSliceTrackerTime << std::endl;
  // int nTrackHits = 0;
  // for ( int itr = 0; itr < fNTracks; itr++ ) {
  //   nTrackHits += fTracks[itr].NHits();
  // }
  // out << nTrackHits << std::endl;
  // for ( int ih = 0; ih < nTrackHits; ih++ ) {
  //   out << fTrackHits[ih] << " ";
  // }
  // out << std::endl;
  
  out << fNHits << std::endl;
  out << fNTracks << std::endl;
  for ( int itr = 0; itr < fNTracks; itr++ ) {
    AliHLTTPCCAGBTrack &t = fTracks[itr];
    out << t.NHits() << std::endl;
    for ( int ih = t.FirstHitRef(); ih < t.FirstHitRef() + t.NHits(); ih++ ) {
      out << fTrackHits[ih] << " ";
    }
    out << std::endl;
    out << t.Alpha() << " ";
    out << t.NDeDx()  << " ";
    out << t.DeDx() << std::endl;
    out << t.InnerParam() << endl << t.OuterParam();
  }
}

void AliHLTTPCCAGBTracker::ReadTracks( std::istream &in )
{
  //* Read tracks  from file

  in >> fTime;
  fSliceTrackerTime = fTime;
  fStatTime[0] += fTime;
  fStatNEvents++;
  if (fTrackHits) delete[] fTrackHits;
  fTrackHits = 0;
  int nTrackHits = 0;
  in >> nTrackHits;
  fTrackHits = new int [nTrackHits];
  for ( int ih = 0; ih < nTrackHits; ih++ ) {
    in >> TrackHits()[ih];
  }
  if (fTracks) delete[] fTracks;
  fTracks = 0;
  in >> fNTracks;
  fTracks = new AliHLTTPCCAGBTrack[fNTracks];
  for ( int itr = 0; itr < fNTracks; itr++ ) {
    AliHLTTPCCAGBTrack &t = Tracks()[itr];
    in >> t;
  }
}

#include "BinaryStoreHelper.h"

void AliHLTTPCCAGBTracker::StoreToFile( const char *filename ) const
{
  FILE *f = std::fopen( filename, "wb" );
  BinaryStoreWrite( fNSlices, f );
  for ( int i = 0; i < fNSlices; ++i ) {
    fSlices[i].StoreToFile( f );
  }

  BinaryStoreWrite( fNHits, f );
  BinaryStoreWrite( fHits.Data(), fNHits, f );
  BinaryStoreWrite( fTrackHits, fNHits * 10, f );

  BinaryStoreWrite( fNTracks, f );
  BinaryStoreWrite( fTracks, fNTracks, f );

  BinaryStoreWrite( fTime, f );
  BinaryStoreWrite( fStatTime, 20, f );
  BinaryStoreWrite( fStatNEvents, f );
  BinaryStoreWrite( fFirstSliceHit, 100, f );

  BinaryStoreWrite( fSliceTrackerTime, f );

  std::fflush( f );
  std::fclose( f );
}

void AliHLTTPCCAGBTracker::RestoreFromFile( FILE *f )
{
  BinaryStoreRead( fNSlices, f );
  fSlices.Resize( fNSlices );
  for ( int i = 0; i < fNSlices; ++i ) {
    fSlices[i].RestoreFromFile( f );
  }

  BinaryStoreRead( fNHits, f );
  fHits.Resize( fNHits );
  BinaryStoreRead( fHits.Data(), fNHits, f );
  fTrackHits = new int[fNHits * 10];
  BinaryStoreRead( fTrackHits, fNHits * 10, f );

  BinaryStoreRead( fNTracks, f );
  fTracks = new AliHLTTPCCAGBTrack[fNTracks];
  BinaryStoreRead( fTracks, fNTracks, f );

  BinaryStoreRead( fTime, f );
  BinaryStoreRead( fStatTime, 20, f );
  BinaryStoreRead( fStatNEvents, f );
  BinaryStoreRead( fFirstSliceHit, 100, f );

  BinaryStoreRead( fSliceTrackerTime, f );
}

  /// Try to group close hits in row formed by one track. After sort hits.
/*void AliHLTTPCCAGBTracker::GroupHits() // iklm
{
  const float minD = AliHLTTPCCAParameters::MinHitsMergeDist;

  bool perf = 0;
  AliHLTTPCCAPerformance *perfomance = &AliHLTTPCCAPerformance::Instance();
//   std::cout << perfomance->GetHitLabels().Size() << std::endl;
  if ( perfomance->GetHitLabels().Size() ) perf = 1;

    // copy arrays
  int nHitsOld = fNHits;
  AliHLTResizableArray<AliHLTTPCCAGBHit> hitsOld;
  hitsOld.Resize( nHitsOld );
  for (int i = 0; i < nHitsOld; i++){
    hitsOld[i] = fHits[i];
  }

    // create arrays for new data
  AliHLTResizableArray<AliHLTTPCCAGBHit> hitsNew;
  hitsNew.Resize( nHitsOld );
//   std::cout << fHits.Size() << " " << hitLabbels.Size() << std::endl;
    // select hits
  
  int iHitNew = -1; // index in new arrays

  int oldIHit = -1; // old* are info of prev hit - for compare to current
  int oldSlice = -1;
  int oldRow = -1;
  float oldY = -10000;
  float oldZ = -10000;
  for (int iHit = 0; iHit < nHitsOld; iHit++){
    int curSlice = hitsOld[iHit].ISlice();
    int curRow = hitsOld[iHit].IRow();
    float curY = hitsOld[iHit].Y();
    float curZ = hitsOld[iHit].Z();
    if ((curSlice == oldSlice) && (curRow == oldRow)){

      float curDY = fabs(curY-oldY);
      float curDZ = fabs(curZ-oldZ);
      if ((curDY < minD) && (curDZ < minD)){  // TODO: minD should depend of errors
          // merge hits // TODO: write normal merge
        float oldErrY = hitsOld[oldIHit].ErrY();
        float oldErrZ = hitsOld[oldIHit].ErrZ();
        float curErrY = hitsOld[oldIHit].ErrY();
        float curErrZ = hitsOld[oldIHit].ErrZ();
        
        float wOldY = 1/curErrY; // !? weights can be different
        float wOldZ = 1/curErrZ;
        float wCurY = 1/oldErrY;
        float wCurZ = 1/oldErrZ;
        float wYNorm = 1/(wOldY + wCurY);
        float wZNorm = 1/(wOldZ + wCurZ);
        wOldY *= wYNorm;
        wOldZ *= wZNorm;
        wCurY *= wYNorm;
        wCurZ *= wZNorm;
          // delete hit

//         std::cout  << oldY << " 1 " << oldZ << std::endl;
//         std::cout  << curY << " 1 " << curZ << std::endl;
        oldY = curY*wCurY+oldY*wOldY;  // FIXME: if we just delete one hit it get better result.
        oldZ = curZ*wCurZ+oldZ*wOldZ;
//         std::cout  << oldY << " 2 " << oldZ << std::endl;
        hitsNew[iHitNew].SetY( oldY );
        hitsNew[iHitNew].SetZ( oldZ );
//         hitsNew[iHitNew].SetErrY( wYNorm ); // !?
//         hitsNew[iHitNew].SetErrZ( wZNorm );
        hitsNew[iHitNew].SetErrY( (curDY < wYNorm) ? wYNorm : curDY ); // !?
        hitsNew[iHitNew].SetErrZ( (curDZ < wZNorm) ? wZNorm : curDZ );
//         hitsNew[oldIHit].SetErrY( curErrY*wCurY+oldErrY*wOldY ); // !?
//         hitsNew[oldIHit].SetErrZ( curErrZ*wCurZ+oldErrZ*wOldZ );
          // merge labels
        if (perf){
          AliHLTResizableArray<AliHLTTPCCAHitLabel>& hitLabels =  AliHLTTPCCAPerformance::Instance().GetHitLabels();
          int oldILabel = hitsOld[oldIHit].ID();
          int curILabel = hitsOld[iHit].ID();
          int i = 0;
          for (; i < 3; i++) if (hitLabels[oldILabel].fLab[i] < 0) break;
          for (int ii = i; ii < 3; ii++) hitLabels[oldILabel].fLab[ii] = hitLabels[curILabel].fLab[ii-i];
        }
        continue;
      } // if hits are close

    } // if slice and row are same
      // leave this hit
    iHitNew++;
    oldIHit = iHit;
    oldRow = curRow;
    oldSlice = curSlice;
    oldY = curY;
    oldZ = curZ;
    hitsNew[iHitNew] = hitsOld[iHit];
  }; // for iHit

    // save new hits array

  fHits.Resize( iHitNew );
  for (int i = 0; i < iHitNew; i++){
    fHits[i] =  hitsNew[i];
  };

  fNHits = iHitNew;
  std::cout << "Merge clusters hits done: " <<  nHitsOld << " hits -> " << iHitNew << " hits." << std::endl;
} // EO: AliHLTTPCCAGBTracker::GroupHits()
*/

void AliHLTTPCCAGBTracker::SetHits( const std::vector<AliHLTTPCCAGBHit> &hits)
{
  const int NHits2 = hits.size();

  SetNHits(NHits2);

  fHits.Resize(NHits2);
  for (int iH = 0; iH < NHits2; iH++){
    fHits[iH] = hits[iH];
  }
} // need for StRoot

void AliHLTTPCCAGBTracker::SetHits( const AliHLTTPCCAGBHit *hits, int nHits )
{
  const int NHits2 = nHits;

  SetNHits(NHits2);

  fHits.Resize(NHits2);
  for (int iH = 0; iH < NHits2; iH++){
    fHits[iH] = hits[iH];
  }
}

void AliHLTTPCCAGBTracker::SetSettings( const std::vector<AliHLTTPCCAParam>& settings )
{
  SetNSlices( settings.size() );
  for ( int iSlice = 0; iSlice < NSlices(); iSlice++ ) {
    fSlices[iSlice].Initialize( settings[iSlice] );
  }
}

void AliHLTTPCCAGBTracker::SaveHitsInFile(string prefix) const
{
    ofstream ofile((prefix+"hits.data").data(),std::ios::out|std::ios::app);
    const int Size = fHits.Size();
    ofile << Size << std::endl;
    for (int i = 0; i < fHits.Size(); i++){
      const AliHLTTPCCAGBHit &l = fHits[i];
      ofile << l;
    }
    ofile.close();

}

void AliHLTTPCCAGBTracker::SaveSettingsInFile(string prefix) const
{
  ofstream ofile((prefix+"settings.data").data(),std::ios::out|std::ios::app);
  WriteSettings(ofile);
}

bool AliHLTTPCCAGBTracker::ReadHitsFromFile(string prefix)
{
    ifstream ifile((prefix+"hits.data").data());
    if ( !ifile.is_open() ) return 0;
    int Size;
    ifile >> Size;
    fHits.Resize(Size);
    SetNHits(Size);
    for (int i = 0; i < Size; i++){
      AliHLTTPCCAGBHit &l = fHits[i];
      ifile >> l;
    }
    ifile.close();
    return 1;
}

bool AliHLTTPCCAGBTracker::ReadSettingsFromFile(string prefix)
{
  ifstream ifile((prefix+"settings.data").data());
  if ( !ifile.is_open() ) return 0;
  ReadSettings(ifile);
  return 1;
}




