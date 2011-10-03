/*
    Copyright (C) 2009 Matthias Kretz <kretz@kde.org>

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) version 3.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public License
    along with this library; see the file COPYING.LIB.  If not, write to
    the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
    Boston, MA 02110-1301, USA.

*/

#define NDEBUG

#include "Reconstructor.h"

#include "AliHLTTPCCATracker.h"
#include "AliHLTTPCCARow.h"
#include "AliHLTTPCCANeighboursFinder.h"
#include "AliHLTTPCCANeighboursCleaner.h"
#include "AliHLTTPCCAStartHitsFinder.h"
#include "AliHLTTPCCATrackletConstructor.h"
#include "AliHLTTPCCATrackletSelector.h"
#include "AliHLTTPCCASliceOutput.h"

#include "TStopwatch.h"
#include "tsc.h"

#ifdef DRAW3
#include "AliHLTTPCCADisplay.h"
#include "TApplication.h"
#endif //DRAW3

#ifdef DRAW2
#include "AliHLTTPCCADisplay.h"
#include "TApplication.h"
#endif //DRAW2


#ifdef DRAW
  #include "AliHLTTPCCADisplay.h"
  #include "TApplication.h"
#endif //DRAW

#ifdef DUMP_LINKS
#include <cstdio>
extern void dumpLinksFile( std::fstream &file, int sliceNumber );

template<unsigned int X>
static inline UInt_t nextMultipleOf( UInt_t value )
{
  STATIC_ASSERT( ( X & ( X - 1 ) ) == 0, X_needs_to_be_a_multiple_of_2 );
  const Int_t offset = value & ( X - 1 );
  if ( offset > 0 ) {
    return value + X - offset;
  }
  return value;
}
#include <fstream>
#include <iomanip>
#endif

#ifdef DUMP_TC_OUTPUT
#include <list>
#include <fstream>
#include <iomanip>
#include "AliHLTTPCCATracklet.h"
extern void dumpTrackletconstructionFile( std::fstream &file, int sliceNumber );
#endif

// reconstruct tracks in slice
tbb::task *AliHLTTPCCATracker::Reconstructor::execute()
{
//   std::cout << " AliHLTTPCCATracker::Reconstructor::execute() " << std::endl; // dbg 0
  d->fTimers[0] = 0; // NeighboursFinder
  d->fTimers[1] = 0; // TrackletConstructor
  d->fTimers[2] = 0; // TrackletSelector
  d->fTimers[3] = 0; // StartHitsFinder
  d->fTimers[4] = 0; // NeighboursFinder cycles
  d->fTimers[5] = 0; // write output
  d->fTimers[6] = 0; // TrackletConstructor cycles
  d->fTimers[7] = 0; // TrackletSelector cycles

  d->fNTracklets = 0;

  if ( d->fData.NumberOfHits() < 1 ) {
    d->RecalculateTrackMemorySize( 1, 1 );
    d->fTrackMemory = new char[d->fTrackMemorySize + 1600];
    d->SetPointersTracks( 1, 1 ); // set pointers for tracks
    d->fOutput->SetNTracks( 0 );
    d->fOutput->SetNTrackClusters( 0 );
    d->fNOutTracks = 0;
    d->fNOutTrackHits = 0;
    return 0;
  }

  TStopwatch timer;
  TimeStampCounter tsc;

  timer.Start();
  tsc.Start();
  AliHLTTPCCATracker::NeighboursFinder neighboursFinder( d, d->fData );
  neighboursFinder.execute();
  tsc.Stop();
  timer.Stop();
  d->fTimers[0] = timer.RealTime();
  d->fTimers[4] = tsc.Cycles();

  d->fData.ClearHitWeights();
//   std::cout << " AliHLTTPCCATracker::Reconstructor::execute() 2" << std::endl; // dbg 0
#ifndef DISABLE_ALL_DRAW
#ifdef DRAW // iklm
  {
#ifdef IKLMCOUT0
  std::cout << " Reconstructor 1 " << std::endl;
#endif // IKLMCOUT0
  AliHLTTPCCADisplay &disp = AliHLTTPCCADisplay::Instance();
  disp.SetSliceView();
  disp.SetCurrentSlice( d );
  disp.DrawSlice( d, 1/*DrawRows*/ );
  disp.DrawSliceHits(1,0.1);
  disp.DrawSliceLinks(-1,-1,0.03);
  AliHLTTPCCADisplay::Instance().SaveCanvasToFile("NFinder.pdf");
  disp.Ask();
}
#endif
#endif
  
#ifdef DUMP_LINKS
  std::fstream linksFStream;
  dumpLinksFile( linksFStream, d->fParam.ISlice() );
  if ( linksFStream.is_open() ) {
    for ( int rowIndex = 0; rowIndex < d->Param().NRows(); ++rowIndex ) {
      linksFStream.width( 3 );
      linksFStream << rowIndex << ": ";
      const AliHLTTPCCARow &row = d->fData.Row( rowIndex );
      for ( int hitIndex = 0; hitIndex < row.NHits(); ++hitIndex ) {
        const int hitIndexB = hitIndex % short_v::Size;
        const int hitIndexA = hitIndex - hitIndexB;
        linksFStream << std::setw( 8 ) << d->fData.HitLinkUpData( row, hitIndexA )[hitIndexB]
          << std::setw( 4 ) << d->fData.HitLinkDownData( row, hitIndexA )[hitIndexB];
      }
      linksFStream << std::endl;
    }
    linksFStream.flush();
  }
#endif

#ifndef NDEBUG
//X   for ( int i = 0; i < d->fData.NumberOfHits(); ++i ) {
//X     assert( d->fData.fHitWeights[i] == 0 );
//X   }
#endif
  //TStopwatch timer0;

#ifdef DRAW
  {
    AliHLTTPCCADisplay &disp = AliHLTTPCCADisplay::Instance();
    disp.SetSliceView();
    disp.SetCurrentSlice( d );
    disp.DrawSlice( d, 0 );
    disp.DrawSliceHits();
    disp.DrawSliceLinks();
    disp.Ask();
  }
#endif

  AliHLTTPCCANeighboursCleaner::run( d->Param().NRows(), d->fData, d->Param() );
#ifndef DISABLE_ALL_DRAW
#ifdef DRAW // iklm
  {
#ifdef IKLMCOUT0
    std::cout << " Reconstructor 1 " << std::endl;
#endif // IKLMCOUT0
    AliHLTTPCCADisplay &disp = AliHLTTPCCADisplay::Instance();
    disp.SetSliceView();
    disp.SetCurrentSlice( d );
    disp.DrawSlice( d, 0/*DrawRows*/ );
    disp.DrawSliceHits(1,0.1);
    disp.DrawSliceLinks(-1,-1,0.03);
    AliHLTTPCCADisplay::Instance().SaveCanvasToFile("NCleaner.pdf");
    disp.Ask();
  }
#endif // end iklm
#endif
//   std::cout << " AliHLTTPCCATracker::Reconstructor::execute() 3" << std::endl; // dbg 0
#ifdef DUMP_LINKS
  if ( linksFStream.is_open() ) {
    linksFStream << "\nCleaner:\n";
    for ( int rowIndex = 0; rowIndex < d->Param().NRows(); ++rowIndex ) {
      linksFStream.width( 3 );
      linksFStream << rowIndex << ": ";
      const AliHLTTPCCARow &row = d->fData.Row( rowIndex );
      for ( int hitIndex = 0; hitIndex < row.NHits(); ++hitIndex ) {
        const int hitIndexB = hitIndex % short_v::Size;
        const int hitIndexA = hitIndex - hitIndexB;
        linksFStream << std::setw( 8 ) << d->fData.HitLinkUpData( row, hitIndexA )[hitIndexB]
          << std::setw( 4 ) << d->fData.HitLinkDownData( row, hitIndexA )[hitIndexB];
      }
      linksFStream << std::endl;
    }
    linksFStream.flush();
  }
#endif

//X   sseTimer.Stop();
//X   static double sseReal = 0.;
//X   static double sseCpu  = 0.;
//X   sseReal += sseTimer.RealTime();
//X   sseCpu  += sseTimer.CpuTime();
//X   std::cout << "SSE code needed real = " << sseReal << ", CPU = " << sseCpu << std::endl;

  timer.Start();
//   std::cout << " AliHLTTPCCATracker::Reconstructor::execute() 4" << std::endl; // dbg 0
  AliHLTTPCCAStartHitsFinder::run( *d, d->fData );
//   std::cout << " AliHLTTPCCATracker::Reconstructor::execute() 5" << std::endl; // dbg 0
  timer.Stop();
  d->fTimers[3] = timer.RealTime();
#ifdef DUMP_LINKS
  if ( linksFStream.is_open() ) {
    linksFStream << "\nStartHits:\n";
    for ( int trackletIndex = 0; trackletIndex < *d->NTracklets(); ++trackletIndex ) {
      const HitId &id = d->TrackletStartHits()[trackletIndex];
      linksFStream << std::setw( 4 ) << id.RowIndex() << " " << id.HitIndex() << "\n";
    }
    linksFStream.flush();
    linksFStream.close();
  }
#endif

#ifndef NDEBUG
  const AliHLTTPCCARow &firstRow = d->fData.Row( 0 );
  const AliHLTTPCCARow &secondRow = d->fData.Row( 1 );
  const int rowStep = AliHLTTPCCAParameters::RowStep;
  const AliHLTTPCCARow &beforeLastRow  = d->fData.Row( d->fParam.NRows() - rowStep );
  const AliHLTTPCCARow &lastRow  = d->fData.Row( d->fParam.NRows() - 1 );
  for ( int hitIndex = 0; hitIndex < firstRow.NHits(); hitIndex += short_v::Size ) {
    assert( d->fData.HitLinkUpData  ( firstRow, hitIndex ) == -1 );
    assert( d->fData.HitLinkDownData( firstRow, hitIndex ) == -1 );
  }
  for ( int hitIndex = 0; hitIndex < secondRow.NHits(); hitIndex += short_v::Size ) {
    assert( d->fData.HitLinkUpData  ( secondRow, hitIndex ) == -1 );
    assert( d->fData.HitLinkDownData( secondRow, hitIndex ) == -1 );
  }
  for ( int rowIndex = 2; rowIndex < d->Param().NRows() - rowStep; ++rowIndex ) {
    const AliHLTTPCCARow &row = d->fData.Row( rowIndex );
    const short_v nHitsUp = d->fData.Row( rowIndex + rowStep ).NHits();
    const short_v nHitsDown = d->fData.Row( rowIndex - rowStep ).NHits();
    for ( int hitIndex = 0; hitIndex < row.NHits(); hitIndex += short_v::Size ) {
      assert( d->fData.HitLinkUpData  ( row, hitIndex ) < nHitsUp );
      assert( d->fData.HitLinkDownData( row, hitIndex ) < nHitsDown );
    }
  }
  for ( int hitIndex = 0; hitIndex < beforeLastRow.NHits(); hitIndex += short_v::Size ) {
    assert( d->fData.HitLinkUpData  ( beforeLastRow, hitIndex ) == -1 );
    assert( d->fData.HitLinkDownData( beforeLastRow, hitIndex ) == -1 );
  }
  for ( int hitIndex = 0; hitIndex < lastRow.NHits(); hitIndex += short_v::Size ) {
    assert( d->fData.HitLinkUpData  ( lastRow, hitIndex ) == -1 );
    assert( d->fData.HitLinkDownData( lastRow, hitIndex ) == -1 );
  }
#endif
//   std::cout << " AliHLTTPCCATracker::Reconstructor::execute() 5.5" << std::endl; // dbg 0
  d->fTrackletVectors.Resize( ( d->fNTracklets + short_v::Size - 1 ) / short_v::Size);
//   std::cout << " AliHLTTPCCATracker::Reconstructor::execute() 6" << std::endl; // dbg 0
  timer.Start();
  tsc.Start();
  AliHLTTPCCATrackletConstructor( *d, d->fData, d->fTrackletVectors ).run();
  tsc.Stop();
  timer.Stop();
  d->fTimers[1] = timer.RealTime();
  d->fTimers[6] = tsc.Cycles();

#ifdef DUMP_TC_OUTPUT
  {
    std::fstream file;
    dumpTrackletconstructionFile( file, d->fParam.ISlice() );
    if ( file.is_open() ) {
      typedef std::list<AliHLTTPCCATracklet> TrackletList;
      TrackletList sortedTracklets;
      for ( int i = 0; i < d->fTrackletVectors.Size(); ++i ) {
        const TrackletVector &tv = d->fTrackletVectors[i];
        for ( int j = 0; j < ushort_v::Size; ++j ) {
          if ( tv.NHits()[j] > 0 ) {
            sortedTracklets.push_back( AliHLTTPCCATracklet( tv, j ) );
          }
        }
      }
      sortedTracklets.sort();
      const TrackletList::const_iterator end = sortedTracklets.end();
      for ( TrackletList::const_iterator it = sortedTracklets.begin(); it != end; ++it ) {
        file << it->FirstRow() << "\t" << it->NHits() << "\t" << it->LastRow() << '\n';
        file << it->Param();
        for ( int row = 0; row < Parameters::NumberOfRows; ++row ) {
          file << std::setw( 3 ) << row << ": " << it->RowHit( row ) << '\n';
        }
        file << "-----------------------------------------------------------------------\n";
      }
      file.flush();
    }
  }
#endif
//   std::cout << " AliHLTTPCCATracker::Reconstructor::execute() 7" << std::endl; // dbg 0
  timer.Start();
  tsc.Start();
  AliHLTTPCCATrackletSelector( *d, &d->fTracks, &d->fNTrackHits, &d->fNumberOfTracks, d->fData, d->fTrackletVectors ).run();
  tsc.Stop();
  timer.Stop();
  d->fTimers[2] = timer.RealTime();
  d->fTimers[7] = tsc.Cycles();

  {
    d->RecalculateTrackMemorySize( d->fNTracklets, d->fNTrackHits ); // to calculate the size
    d->fTrackMemory = new char[d->fTrackMemorySize + 1600];
    d->SetPointersTracks( d->fNTracklets, d->fNTrackHits ); // set pointers for hits
  }

  //std::cout<<"Slice "<<Param().ISlice()<<": N start hits/tracklets/tracks = "<<nStartHits<<" "<<nStartHits<<" "<<*fNTracks<<std::endl;
  d->WriteOutput();
//   std::cout << " AliHLTTPCCATracker::Reconstructor::execute() 10" << std::endl; // dbg 0
  return 0;
}
