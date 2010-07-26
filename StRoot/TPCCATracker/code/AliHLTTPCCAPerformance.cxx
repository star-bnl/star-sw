// $Id: AliHLTTPCCAPerformance.cxx,v 1.1.1.1 2010/07/26 20:55:38 ikulakov Exp $
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
#include "AliHLTTPCCAPerformance.h"

#include "AliHLTTPCCounters.h"
#include "AliHLTTPCCAPerformanceBase.h"
#include "AliHLTTPCCASlicesPerformance.h"
#include "AliHLTTPCCAGlobalSlicesPerformance.h"
#include "AliHLTTPCCAGlobalPerformance.h"


#include "AliHLTTPCCAGBHit.h"
#include "AliHLTTPCCAMCTrack.h"
#ifndef HLTCA_STANDALONE
#include "AliHLTTPCCAMCPoint.h"
#endif
#include "AliHLTTPCCAOutTrack.h"
#include "AliHLTTPCCAGBTrack.h"
#include "AliHLTTPCCAGBTracker.h"
#include "AliHLTTPCCATracklet.h"

#include "AliHLTTPCCADisplay.h"


#include "TMath.h"
#include "TROOT.h"
#include "Riostream.h"
#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "TProfile.h"
#include "TStyle.h"

AliHLTTPCCAPerformance::AliHLTTPCCAPerformance()
{
  //* constructor
}

AliHLTTPCCAPerformance::~AliHLTTPCCAPerformance()
{
  //* destructor
  fHitLabels.Resize( 0 );
  fMCTracks.Resize( 0 );
  for (unsigned int iPerf = 0; iPerf < subPerformances.size(); iPerf++){ // TODO: why we can't do this in subPerformances.~destructor() ??
    if (subPerformances[iPerf].perf) delete subPerformances[iPerf].perf;
  }
}

AliHLTTPCCAPerformance &AliHLTTPCCAPerformance::Instance()
{
  // reference to static object
  static AliHLTTPCCAPerformance gAliHLTTPCCAPerformance;
  return gAliHLTTPCCAPerformance;
}

bool AliHLTTPCCAPerformance::SetNewEvent(AliHLTTPCCAGBTracker* const Tracker, string mcTracksFile, string mcPointsFile)
{
  fTracker = Tracker;
    // Read MC info from file
  FILE *file = std::fopen( mcTracksFile.data(), "rb" );
  if ( !file ) {
    return false;
  }
  ReadMCEvent( file );
  fclose( file );
  
  file = std::fopen( mcPointsFile.data(), "rb" );
  if ( !file ) {
    return false;
  }
  ReadLocalMCPoints( file );
  fclose( file );

  return true;

} // void AliHLTTPCCAPerformance::SetNewEvent

void AliHLTTPCCAPerformance::InitSubPerformances()
{
    // Init subperformances
  static bool first_call = true;

  if (first_call){
    typedef TSubPerformance TSP;
    
      /// Just define here all sub-performances
      /// TSP(new __ClassName__               , __Name__      ),
    const int NSPerfo = 3;
    const TSP perfos[NSPerfo] = {
      TSP(new AliHLTTPCCASlicesPerformance, "SliceTracker"),
      TSP(new AliHLTTPCCAGlobalSlicesPerformance, "GlobalSliceTracker"),
      TSP(new AliHLTTPCCAGlobalPerformance, "GlobalTracker")
    };
    
    subPerformances.resize(NSPerfo);
    for (int iP = 0; iP < NSPerfo; iP++){
      subPerformances[iP] = perfos[iP];
    }
  }
  
  for (unsigned int iPerf = 0; iPerf < subPerformances.size(); iPerf++){
    subPerformances[iPerf]->SetNewEvent(fTracker, &fHitLabels, &fMCTracks, &fLocalMCPoints);
  }

  if (first_call) CreateHistos();
  
  first_call = false;
} // void AliHLTTPCCAPerformance::InitSubPerformances

void AliHLTTPCCAPerformance::CreateHistos()
{
  for (unsigned int iPerf = 0; iPerf < subPerformances.size(); iPerf++){
    subPerformances[iPerf]->CreateHistos(subPerformances[iPerf].name);
  }
}

void AliHLTTPCCAPerformance::WriteHistos()
{
//   //* write histograms to the file
  TDirectory *curr = gDirectory;
//   // Open output file and write histograms
  TFile* outfile = new TFile( "HLTTPCCATrackerPerformance.root", "RECREATE" );
  outfile->cd();

  for (unsigned int iPerf = 0; iPerf < subPerformances.size(); iPerf++)
    subPerformances[iPerf]->WriteHistos();

  outfile->Close();
  curr->cd();
}

void AliHLTTPCCAPerformance::ExecPerformance()
{
  for (unsigned int iPerf = 0; iPerf < subPerformances.size(); iPerf++){
    subPerformances[iPerf]->Exec(0);
    cout << endl
        << " ---- " << subPerformances[iPerf].name << " Statistic ---- " << endl;
    subPerformances[iPerf]->PrintEfficiencyStatistic();
  };

  WriteHistos();
}

  /// -------------------- Read\write MC information ---------------------
void AliHLTTPCCAPerformance::WriteMCEvent( FILE *file ) const
{
  // write MC information to the file
  int written;
  int n = fMCTracks.Size();
  written = std::fwrite( &n, sizeof( int ), 1, file );
  assert( written == 1 );
  n = fHitLabels.Size();
  written = std::fwrite( &n, sizeof( int ), 1, file );
  assert( written == 1 );
  written = std::fwrite( fMCTracks.Data(), sizeof( AliHLTTPCCAMCTrack ), fMCTracks.Size(), file );
  assert( written == fMCTracks.Size() );
  written = std::fwrite( fHitLabels.Data(), sizeof( AliHLTTPCCAHitLabel ), fHitLabels.Size(), file );
  assert( written == fHitLabels.Size() );
}

void AliHLTTPCCAPerformance::ReadMCEvent( FILE *file )
{
  // read mc info from the file
  int read;
  int n;

  read = std::fread( &n, sizeof( int ), 1, file );
  assert( read == 1 );
  fMCTracks.Resize( n );
  read = std::fread( &n, sizeof( int ), 1, file );
  assert( read == 1 );
  fHitLabels.Resize( n );

  read = std::fread( fMCTracks.Data(), sizeof( AliHLTTPCCAMCTrack ), fMCTracks.Size(), file );
  assert( read == fMCTracks.Size() );

  read = std::fread( fHitLabels.Data(), sizeof( AliHLTTPCCAHitLabel ), fHitLabels.Size(), file );
  assert( read == fHitLabels.Size() );

//   for (int i = 0; i < fMCTracks.Size(); i++){
//     AliHLTTPCCAMCTrack& mc = fMCTracks[i];
//         std::cout << mc.PDG() << " ";
//         std::cout << std::endl;
//         for (int iPar = 0; iPar < 7; iPar++)
//           std::cout << mc.Par(iPar) << " ";
//         std::cout << std::endl;
//         for (int iPar = 0; iPar < 7; iPar++)
//           std::cout << mc.TPCPar(iPar) << " ";
//         std::cout << std::endl;
// //         std::cout << mc.P() << " ";
//   };
//   for (int i = 0; i < fHitLabels.Size(); i++){
//     AliHLTTPCCAHitLabel& l = fHitLabels[i];
//     for (int iPar = 0; iPar < 3; iPar++)
//       std::cout << l.fLab[iPar] << " ";
//     std::cout << std::endl;
//   };
}

void AliHLTTPCCAPerformance::ReadLocalMCPoints( FILE *file )
{
  
  int read;
  int n;

  read = std::fread( &n, sizeof( int ), 1, file );
  assert( read == 1 );
  fLocalMCPoints.Resize( n );

  read = std::fread( fLocalMCPoints.Data(), sizeof( AliHLTTPCCALocalMCPoint ), fLocalMCPoints.Size(), file );
  assert( read == fLocalMCPoints.Size() );

}

