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
#define IKLMCOUT0

#include "Reconstructor.h"

#include "AliHLTTPCCATracker.h"
#include "AliHLTTPCCARow.h"
#include "AliHLTTPCCANeighboursFinder.h"
#include "AliHLTTPCCANeighboursCleaner.h"
#include "AliHLTTPCCAStartHitsFinder.h"
#include "AliHLTTPCCATrackletConstructor.h"
#include "AliHLTTPCCATrackletSelector.h"
#include "AliHLTTPCCASliceOutput.h"

#include "Stopwatch.h"
#include "tsc.h"

// ---
#define __YF__
// ---

#ifdef MAIN_DRAW
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

#ifdef USE_TBB
tbb::task *AliHLTTPCCATracker::Reconstructor::execute()
#else //USE_TBB
int AliHLTTPCCATracker::Reconstructor::execute()
#endif //USE_TBB
{
  d->fTimers[0] = 0.; // NeighboursFinder
  d->fTimers[1] = 0.; // TrackletConstructor
  d->fTimers[2] = 0.; // TrackletSelector
  d->fTimers[3] = 0.; // StartHitsFinder
  d->fTimers[4] = 0.; // NeighboursFinder cycles
  d->fTimers[5] = 0.; // write output
  d->fTimers[6] = 0.; // TrackletConstructor cycles
  d->fTimers[7] = 0.; // TrackletSelector cycles
  d->fTimers[10] = 0.; // NeighboursCleaner
  
  d->fNTracklets = 0;

  if ( d->fData.NumberOfHits() <= 0 ) {
    d->RecalculateTrackMemorySize( 1, 1 );
    d->fTrackMemory = new char[d->fTrackMemorySize + 1600]; // TODO rid of 1600
    d->SetPointersTracks( 1, 1 ); // set pointers for tracks
    d->fOutput->SetNTracks( 0 );
    d->fOutput->SetNTrackClusters( 0 );
    d->fTracks.resize(0);
    return 0;
  }

#ifdef USE_TIMERS
  Stopwatch timer;
  TimeStampCounter tsc;
#endif // USE_TIMERS

  
    // initialize the slice tracker's number of tracklets to 0
#ifdef USE_TBB
  CAMath::AtomicExch( d->NTracklets(), 0 ); 
#else //USE_TBB
  d->SetNTracklets(0);
#endif //USE_TBB

#ifndef NDEBUG
 // check of isUsed
  for ( int rowIndex = 0; rowIndex < d->Param().NRows(); ++rowIndex ) {
    const AliHLTTPCCARow &row = d->fData.Row( rowIndex );
    const unsigned numberOfHits = row.NHits();
    for ( unsigned int i = 0; i < numberOfHits; i += int_v::Size ) {
      const uint_v hitIndexes = uint_v( Vc::IndexesFromZero ) + i;
      const int_m validHitsMask = (hitIndexes < numberOfHits);
      const int_v isUsed = int_v( d->fData.HitDataIsUsed( row ), static_cast<uint_v>(hitIndexes), validHitsMask);
       ASSERT( ((isUsed == int_v( Vc::Zero )) && validHitsMask) == validHitsMask,
              isUsed << validHitsMask);
    }
  }
#endif // NDEBUG

  int nIt = 2;

#if defined(V6_1) || defined(V7)
  nIt = 1;
#endif

#ifdef V6
  std::vector<hit_link> save_up_links;
#endif

  for (int iter = 0; iter < nIt; iter++) {

#ifdef USE_TIMERS
    timer.Start();
    tsc.Start();
#endif // USE_TIMERS
      // clean unused hits
    for ( int rowIndex = 0; rowIndex < d->Param().NRows(); ++rowIndex ) {
      d->fData.CleanUsedHits( rowIndex, iter == 0 );
    }
    // unset all unused links
  const int_v minusOne(-1);
  for ( int rowIndex = 0; rowIndex < d->Param().NRows(); ++rowIndex ) {
    const AliHLTTPCCARow &row = d->fData.Row( rowIndex );
    const unsigned numberOfHits = row.NHits();
    for ( unsigned int i = 0; i < numberOfHits; i += int_v::Size ) {
      const uint_v hitIndexes = uint_v( Vc::IndexesFromZero ) + i;
      int_v usedTemp;
      for( unsigned int j = 0; j < float_v::Size; j++ ) {
	usedTemp[j] = d->fData.HitDataIsUsed( row )[(unsigned int)hitIndexes[j]];
      }
      const int_m validHitsMask = (hitIndexes < numberOfHits) && (usedTemp == int_v( Vc::Zero ));
      d->fData.SetHitLinkUpData  ( row, hitIndexes, minusOne, validHitsMask );
      d->fData.SetHitLinkDownData( row, hitIndexes, minusOne, validHitsMask );
    }
  }

#ifndef NDEBUG
 // check of isUsed
  for ( int rowIndex = 0; rowIndex < d->Param().NRows(); ++rowIndex ) {
    const AliHLTTPCCARow &row = d->fData.Row( rowIndex );
    const unsigned numberOfHits = row.NHits();
    for ( unsigned int i = 0; i < numberOfHits; i += int_v::Size ) {
      const uint_v hitIndexes = uint_v( Vc::IndexesFromZero ) + i;
      const int_m validHitsMask = (hitIndexes < numberOfHits);
      const int_v isUsed = int_v( d->fData.HitDataIsUsed( row ), static_cast<uint_v>(hitIndexes), validHitsMask);
       ASSERT( ((isUsed >= int_v( Vc::Zero )) && (isUsed <= int_v( 1 )) && validHitsMask) == validHitsMask,
              isUsed << validHitsMask);
    }
  }
#endif // NDEBUG

#ifndef V5
    AliHLTTPCCATracker::NeighboursFinder neighboursFinder( d, d->fData, iter );
    neighboursFinder.execute();
#endif
    
#ifdef USE_TIMERS
    tsc.Stop();
    timer.Stop();
    d->fTimers[0] += (double)timer.RealTime();
    d->fTimers[4] += tsc.Cycles();
#endif // USE_TIMERS

    d->fData.ClearHitWeights();
#ifndef DISABLE_ALL_DRAW
#ifdef MAIN_DRAW // iklm
    if ( AliHLTTPCCADisplay::Instance().DrawType() == 1 )
    {
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
          const int hitIndexB = hitIndex % int_v::Size;
          const int hitIndexA = hitIndex - hitIndexB;
          linksFStream << std::setw( 8 ) << d->fData.HitLinkUpData( row, hitIndexA )[hitIndexB]
                       << std::setw( 4 ) << d->fData.HitLinkDownData( row, hitIndexA )[hitIndexB];
        }
        linksFStream << std::endl;
      }
      linksFStream.flush();
    }
#endif

#ifdef MAIN_DRAW
    if ( AliHLTTPCCADisplay::Instance().DrawType() == 1 )
    {
      AliHLTTPCCADisplay &disp = AliHLTTPCCADisplay::Instance();
      disp.SetSliceView();
      disp.SetCurrentSlice( d );
      disp.DrawSlice( d, 0 );
      disp.DrawSliceHits(1, 0.5);
      disp.DrawSliceLinks(-1,-1,1);
      AliHLTTPCCADisplay::Instance().SaveCanvasToFile("NFinder1.pdf");
      disp.Ask();
    }
#endif

#ifndef NDEBUG
#if 1
    {
      const int rowStep = AliHLTTPCCAParameters::RowStep;
      const int NRows = d->Param().NRows();
      for ( int rowIndex = 0; rowIndex < NRows; ++rowIndex ) {
        const AliHLTTPCCARow &row = d->fData.Row( rowIndex );
        const int NRowHits = row.NHits();
        int NRowUpHits = 0;
        if (rowIndex + rowStep < NRows) NRowUpHits = d->fData.Row( rowIndex + rowStep ).NHits();
        int NRowDnHits = 0;
        if (rowIndex - rowStep >= 0)    NRowDnHits = d->fData.Row( rowIndex - rowStep ).NHits();
        for ( int i = 0; i < NRowHits; i += int_v::Size ) {
          const int_v hitIndexes = int_v( Vc::IndexesFromZero ) + i;
          int_m validHitsMask = hitIndexes < NRowHits;
            // validHitsMask &= ( int_v(d->fData.HitDataIsUsed( row ), static_cast<uint_v>(hitIndexes) ) == int_v( Vc::Zero ) );

          int_v up = int_v(d->fData.HitLinkUpData( row ), static_cast<uint_v>(hitIndexes), validHitsMask );
          int_v dn = int_v(d->fData.HitLinkDownData( row ), static_cast<uint_v>(hitIndexes), validHitsMask );
          ASSERT ( (validHitsMask && (up >= -1 ) && (up < NRowUpHits )) == validHitsMask,
          " validHitsMask= " << validHitsMask <<  " up= "  << up
          << " iter= " << iter << " row= " << rowIndex << " NRowUpHits= " << NRowUpHits );
          ASSERT ( (validHitsMask && (dn >= -1 ) && (dn < NRowDnHits )) == validHitsMask,
          " validHitsMask= " << validHitsMask <<  " dn= "  << dn
          << " iter= " << iter << " row= " << rowIndex << " NRowDnHits= " << NRowDnHits );
        }
      }
    }
#endif // 0
#endif // NDEBUG

#ifdef USE_TIMERS
    timer.Start();
#endif // USE_TIMERS

#ifndef V6
    AliHLTTPCCANeighboursCleaner::run( d->Param().NRows(), d->fData, d->Param() );
#else
    AliHLTTPCCANeighboursCleaner::run( d->Param().NRows(), d->fData, d->Param(), iter, save_up_links );
#endif
    
#ifdef USE_TIMERS
    timer.Stop();
    d->fTimers[10] += (double)timer.RealTime();
#endif // USE_TIMERS
    
#ifndef DISABLE_ALL_DRAW
#ifdef MAIN_DRAW // iklm
    if ( AliHLTTPCCADisplay::Instance().DrawType() == 1 )
    {
#ifdef IKLMCOUT0
      std::cout << " Reconstructor. AliHLTTPCCANeighboursCleaner. iter=" << iter << std::endl;
#endif // IKLMCOUT0
      AliHLTTPCCADisplay &disp = AliHLTTPCCADisplay::Instance();
      disp.SetSliceView();
      disp.SetCurrentSlice( d );
      disp.DrawSlice( d, 0/*DrawRows*/ );
      disp.DrawSliceHits(1,0.5);
      disp.DrawSliceLinks(-1,-1,1);
      AliHLTTPCCADisplay::Instance().SaveCanvasToFile("NCleaner.pdf");
      disp.Ask();
    }
#endif // end iklm
#endif

#ifdef DUMP_LINKS
    if ( linksFStream.is_open() ) {
      linksFStream << "\nCleaner:\n";
      for ( int rowIndex = 0; rowIndex < d->Param().NRows(); ++rowIndex ) {
        linksFStream.width( 3 );
        linksFStream << rowIndex << ": ";
        const AliHLTTPCCARow &row = d->fData.Row( rowIndex );
        for ( int hitIndex = 0; hitIndex < row.NHits(); ++hitIndex ) {
          const int hitIndexB = hitIndex % int_v::Size;
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

#ifndef NDEBUG
#if 1
    {
      const int rowStep = AliHLTTPCCAParameters::RowStep;
      const int NRows = d->Param().NRows();
      for ( int rowIndex = 0; rowIndex < NRows; ++rowIndex ) {
        const AliHLTTPCCARow &row = d->fData.Row( rowIndex );
        const int NRowHits = row.NHits();
        int NRowUpHits = 0;
        if (rowIndex + rowStep < NRows) NRowUpHits = d->fData.Row( rowIndex + rowStep ).NHits();
        int NRowDnHits = 0;
        if (rowIndex - rowStep >= 0)    NRowDnHits = d->fData.Row( rowIndex - rowStep ).NHits();
        for ( int i = 0; i < NRowHits; i += int_v::Size ) {
          const int_v hitIndexes = int_v( Vc::IndexesFromZero ) + i;
          int_m validHitsMask = hitIndexes < NRowHits;
            // validHitsMask &= ( int_v(d->fData.HitDataIsUsed( row ), static_cast<uint_v>(hitIndexes) ) == int_v( Vc::Zero ) );

          int_v up = int_v(d->fData.HitLinkUpData( row ), static_cast<uint_v>(hitIndexes), validHitsMask );
          int_v dn = int_v(d->fData.HitLinkDownData( row ), static_cast<uint_v>(hitIndexes), validHitsMask );
          ASSERT ( (validHitsMask && (up >= -1 ) && (up < NRowUpHits )) == validHitsMask,
          " validHitsMask= " << validHitsMask <<  " up= "  << up
          << " iter= " << iter << " row= " << rowIndex << " NRowUpHits= " << NRowUpHits );
          ASSERT ( (validHitsMask && (dn >= -1 ) && (dn < NRowDnHits )) == validHitsMask,
          " validHitsMask= " << validHitsMask <<  " dn= "  << dn
          << " iter= " << iter << " row= " << rowIndex << " NRowDnHits= " << NRowDnHits );
        }
      }
    }
#endif // 0
#endif // NDEBUG

#ifdef USE_TIMERS
    timer.Start();
#endif // USE_TIMERS
    
    AliHLTTPCCAStartHitsFinder::run( *d, d->fData, iter );

#ifdef USE_TIMERS
    timer.Stop();
    d->fTimers[3] += (double)timer.RealTime();
#endif // USE_TIMERS
  } // iterations
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
#if 0 // since we connect edge hits into chain don't need this
  const AliHLTTPCCARow &firstRow = d->fData.Row( 0 );
  const int rowStep = AliHLTTPCCAParameters::RowStep;
  const AliHLTTPCCARow &lastRow  = d->fData.Row( d->fParam.NRows() - 1 );
  for ( int hitIndex = 0; hitIndex < firstRow.NHits(); hitIndex += int_v::Size ) {
    assert( d->fData.HitLinkUpData  ( firstRow, hitIndex ) == -1 );
    assert( d->fData.HitLinkDownData( firstRow, hitIndex ) == -1 );
  }
  for ( int rowIndex = 1; rowIndex < d->Param().NRows() - rowStep; ++rowIndex ) {
    const AliHLTTPCCARow &row = d->fData.Row( rowIndex );
    const int_v nHitsUp = d->fData.Row( rowIndex + rowStep ).NHits();
    const int_v nHitsDown = d->fData.Row( rowIndex - rowStep ).NHits();
    for ( int hitIndex = 0; hitIndex < row.NHits(); hitIndex += int_v::Size ) {
      assert( d->fData.HitLinkUpData  ( row, hitIndex ) < nHitsUp );
      assert( d->fData.HitLinkDownData( row, hitIndex ) < nHitsDown );
    }
  }
  for ( int hitIndex = 0; hitIndex < lastRow.NHits(); hitIndex += int_v::Size ) {
    assert( d->fData.HitLinkUpData  ( lastRow, hitIndex ) == -1 );
    assert( d->fData.HitLinkDownData( lastRow, hitIndex ) == -1 );
  }
#endif // 0
#endif // NDEBUG
  
#ifndef NDEBUG
 // check of isUsed
  for ( int rowIndex = 0; rowIndex < d->Param().NRows(); ++rowIndex ) {
    const AliHLTTPCCARow &row = d->fData.Row( rowIndex );
    const unsigned numberOfHits = row.NHits();
    for ( unsigned int i = 0; i < numberOfHits; i += int_v::Size ) {
      const uint_v hitIndexes = uint_v( Vc::IndexesFromZero ) + i;
      const int_m validHitsMask = (hitIndexes < numberOfHits);
      const int_v isUsed = int_v( d->fData.HitDataIsUsed( row ), static_cast<uint_v>(hitIndexes), validHitsMask);
       ASSERT( ((isUsed >= int_v( Vc::Zero )) && (isUsed <= int_v( 1 )) && validHitsMask) == validHitsMask,
              isUsed << validHitsMask);
    }
  }
#endif // NDEBUG

#ifdef V7
  d->fTrackletVectors.Resize( d->fData.NumberOfHits() / 10 + 5 );
#else
#ifdef V6
  d->fTrackletVectors.Resize( ( d->fNTracklets + save_up_links.size()/2 + int_v::Size - 1 ) / int_v::Size );
#else
  d->fTrackletVectors.Resize( ( d->fNTracklets + int_v::Size - 1 ) / int_v::Size);
#endif
#endif

#ifdef USE_TIMERS
  timer.Start();
  tsc.Start();
#endif // USE_TIMERS

  unsigned int tracksSaved = 0;

#ifdef V7
  AliHLTTPCCATrackletConstructor( *d, d->fData, d->fTrackletVectors ).run(0, tracksSaved, -1);
#else
  AliHLTTPCCATrackletConstructor( *d, d->fData, d->fTrackletVectors ).run(0, tracksSaved);
#endif

#ifndef NDEBUG
    // check of isUsed
  for ( int rowIndex = 0; rowIndex < d->Param().NRows(); ++rowIndex ) {
    const AliHLTTPCCARow &row = d->fData.Row( rowIndex );
    const unsigned numberOfHits = row.NHits();
    for ( unsigned int i = 0; i < numberOfHits; i += int_v::Size ) {
      const uint_v hitIndexes = uint_v( Vc::IndexesFromZero ) + i;
      const int_m validHitsMask = (hitIndexes < numberOfHits);
      const int_v isUsed = int_v( d->fData.HitDataIsUsed( row ), static_cast<uint_v>(hitIndexes), validHitsMask);
      ASSERT( ((isUsed >= int_v( Vc::Zero )) && (isUsed <= int_v( 3 )) && validHitsMask) == validHitsMask,
      isUsed << validHitsMask);
    }
  }
#endif // NDEBUG

#ifdef V6
  Vc::vector<AliHLTTPCCAStartHitId>& startHits = d->TrackletStartHits();
  startHits.clear();
  int nseeds = 0;
  const int hitsStartOffset = *d->NTracklets();
  for( unsigned int i = 0; i < save_up_links.size(); i += 2 ) {
    const AliHLTTPCCARow &row = d->fData.Row( save_up_links[i].row );
    const AliHLTTPCCARow &row1 = d->fData.Row( save_up_links[i].row + 1 );
    const AliHLTTPCCARow &row2 = d->fData.Row( save_up_links[i].row + 2 );
    if(  d->fData.HitDataIsUsed( row )[save_up_links[i].hit] > 0 ) continue;
    if(  d->fData.HitDataIsUsed( row1 )[save_up_links[i].link] > 0 ) continue;
    if(  d->fData.HitDataIsUsed( row2 )[save_up_links[i+1].link] > 0 ) continue;
    d->fData.SetHitLinkUpData( row, save_up_links[i].hit, save_up_links[i].link );
    d->fData.SetHitLinkUpData( row1, save_up_links[i].link, save_up_links[i+1].link );
    Vc::vector<AliHLTTPCCAStartHitId>& startHits1 = d->TrackletStartHits();
    startHits1[hitsStartOffset+nseeds].Set( save_up_links[i].row, save_up_links[i].hit, 3 );
      nseeds++;
    }
  d->SetNTracklets(hitsStartOffset+nseeds);
  AliHLTTPCCATrackletConstructor( *d, d->fData, d->fTrackletVectors ).run(0, tracksSaved);
#endif

#ifdef V7
  Vc::vector<AliHLTTPCCAStartHitId>& startHits2 = d->TrackletStartHits();
  startHits2.clear();
  for( int i = 0; i < d->Param().NRows()-4; i += 2 ) {
    AliHLTTPCCATrackletConstructor( *d, d->fData, d->fTrackletVectors ).run(i, tracksSaved, 1);
  }
#endif

#ifdef USE_TIMERS
  tsc.Stop();
  timer.Stop();
  d->fTimers[1] = timer.RealTime();
  d->fTimers[6] = tsc.Cycles();
#endif // USE_TIMERS
  
#ifdef DUMP_TC_OUTPUT
  {
    std::fstream file;
    dumpTrackletconstructionFile( file, d->fParam.ISlice() );
    if ( file.is_open() ) {
      typedef std::list<AliHLTTPCCATracklet> TrackletList;
      TrackletList sortedTracklets;
      for ( int i = 0; i < d->fTrackletVectors.Size(); ++i ) {
        const TrackletVector &tv = d->fTrackletVectors[i];
        for ( int j = 0; j < uint_v::Size; ++j ) {
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
        for ( int row = 0; row < AliHLTTPCCAParameters::NumberOfRows; ++row ) {
          file << std::setw( 3 ) << row << ": " << it->RowHit( row ) << '\n';
        }
        file << "-----------------------------------------------------------------------\n";
      }
      file.flush();
    }
  }
#endif
  
#ifdef USE_TIMERS
  timer.Start();
  tsc.Start();
#endif // USE_TIMERS
  

  d->fNumberOfTracks = tracksSaved;

  AliHLTTPCCATrackletSelector( *d, &d->fTracks, &d->fNTrackHits, &d->fNumberOfTracks, d->fData, d->fTrackletVectors ).run();
  
#ifdef USE_TIMERS
  tsc.Stop();
  timer.Stop();
  d->fTimers[2] = timer.RealTime();
  d->fTimers[7] = tsc.Cycles();
#endif // USE_TIMERS

  {
    d->RecalculateTrackMemorySize( d->fNTracklets, d->fNTrackHits ); // to calculate the size
    d->fTrackMemory = new char[d->fTrackMemorySize + 1600]; // TODO rid of 1600
    d->SetPointersTracks( d->fNTracklets, d->fNTrackHits ); // set pointers for hits
  }
  d->WriteOutput();
  return 0;
}
