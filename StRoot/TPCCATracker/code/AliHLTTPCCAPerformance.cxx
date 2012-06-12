// $Id: AliHLTTPCCAPerformance.cxx,v 1.14 2012/06/12 18:05:51 fisyak Exp $
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
#include "AliHLTTPCCASlicesLinksPerformance.h"
#include "AliHLTTPCCASlicesPerformance.h"
#include "AliHLTTPCCAStiPerformance.h"
#include "AliHLTTPCCAMergerPerformance.h"
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
using namespace std;
AliHLTTPCCAPerformance::AliHLTTPCCAPerformance()
{
  static bool first_call = true;

  if (first_call){
    typedef TSubPerformance TSP;
    
      /// Just define here all sub-performances
      /// TSP(new __ClassName__               , __Name__      ),
    const int NSPerfo = 6;
    const TSP perfos[NSPerfo] = {
      TSP(new AliHLTTPCCASlicesLinksPerformance, "Chains Performance"),
      TSP(new AliHLTTPCCASlicesPerformance, "Sector Performance"),
      TSP(new AliHLTTPCCAGlobalSlicesPerformance, "Global Sector Performance"),
      TSP(new AliHLTTPCCAGlobalPerformance, "Global Performance"),
      TSP(new AliHLTTPCCAStiPerformance, "Sti Performance"),
      TSP(new AliHLTTPCCAMergerPerformance, "Merger", 0)
    };
    
    subPerformances.resize(NSPerfo);
    for (int iP = 0; iP < NSPerfo; iP++){
      subPerformances[iP] = perfos[iP];
    }
  }
  first_call = false;
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

/*  if (first_call){
    typedef TSubPerformance TSP;
    
      /// Just define here all sub-performances
      /// TSP(new __ClassName__               , __Name__      ),
    const int NSPerfo = 5;
    const TSP perfos[NSPerfo] = {

    };
    
    subPerformances.resize(NSPerfo);
    for (int iP = 0; iP < NSPerfo; iP++){
      subPerformances[iP] = perfos[iP];
    }
  }*/
  
  for (unsigned int iPerf = 0; iPerf < subPerformances.size(); iPerf++){
    subPerformances[iPerf]->SetNewEvent(fTracker, &fHitLabels, &fMCTracks, &fLocalMCPoints);
  }

  if (first_call) CreateHistos();
  
  first_call = false;
} // void AliHLTTPCCAPerformance::InitSubPerformances

void AliHLTTPCCAPerformance::CreateHistos()
{
  for (unsigned int iPerf = 0; iPerf < subPerformances.size(); iPerf++){
    if(!(subPerformances[iPerf]->IsHistoCreated())) 
      subPerformances[iPerf]->CreateHistos(subPerformances[iPerf].name, fOutputFile);
  }
}

bool AliHLTTPCCAPerformance::CreateHistos(string name)
{
  unsigned i = 0;
  for( ; i < (subPerformances.size()) && (subPerformances[i].name != name); i++);
  if(!(subPerformances[i]->IsHistoCreated()) && i != subPerformances.size()) 
    subPerformances[i]->CreateHistos(subPerformances[i].name, fOutputFile);
  if ( i == subPerformances.size() ) return 0;
  return 1;
}

void AliHLTTPCCAPerformance::WriteHistos()
{
  if (!fOutputFile) {
    cout << "W AliHLTTPCCAPerformance:Warning: Output file has NOT been set." << endl;
    return;
  }
//   //* write histograms to the file
  TDirectory *curr = gDirectory;
//   // Open output file and write histograms
  fOutputFile->cd();

  for (unsigned int iPerf = 0; iPerf < subPerformances.size(); iPerf++)
    subPerformances[iPerf]->WriteHistos();

#ifdef STAR_STANDALONE // TODO use it. 
  fOutputFile->Close();
#endif //
  curr->cd();
}

void AliHLTTPCCAPerformance::ExecPerformance()
{
  fStatNEvents++;
  
  for (unsigned int iPerf = 0; iPerf < subPerformances.size(); iPerf++){
    if(subPerformances[iPerf].IsGlobalPerf) subPerformances[iPerf]->Exec(0);
  }
#if 1  // current event efficiencies
  for (unsigned int iPerf = 0; iPerf < subPerformances.size(); iPerf++){    
    cout << endl
        << " ---- " << subPerformances[iPerf].name << " event " << fStatNEvents << " ---- "<< endl;
    subPerformances[iPerf]->PrintEfficiency();
  }
  cout << endl << " ============================== " << endl;
#endif //0
  for (unsigned int iPerf = 0; iPerf < subPerformances.size(); iPerf++){  
    cout << endl
        << " ---- " << subPerformances[iPerf].name << " " << fStatNEvents << " events Statistic ---- " << endl;
    if(subPerformances[iPerf].IsGlobalPerf) subPerformances[iPerf]->PrintEfficiencyStatistic();
  };

#ifdef STAR_STANDALONE // TODO use it!!
  WriteHistos();
#endif //

}

AliHLTTPCCAPerformanceBase* AliHLTTPCCAPerformance::GetSubPerformance(string name)
{
  unsigned i = 0;
  for( ; (i < subPerformances.size()) && (subPerformances[i].name != name); i++);
  assert ( i != subPerformances.size() || ("" == " Incorrect name of subPerformance used.") );
  return subPerformances[i].perf;
} // AliHLTTPCCAPerformanceBase* AliHLTTPCCAPerformance::GetSubPerformance(string name)


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
//      std::cout << l.fLab[iPar] << " ";
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

} // void AliHLTTPCCAPerformance::ReadLocalMCPoints( FILE *file )


void AliHLTTPCCAPerformance::SetMCTracks(vector<AliHLTTPCCAMCTrack>& mcTracks)
{
  const int N = mcTracks.size();
  fMCTracks.Resize(N);
  for(int i = 0; i < N; i++){
    fMCTracks[i] = mcTracks[i];
  }
}

void AliHLTTPCCAPerformance::SetMCPoints(vector<AliHLTTPCCALocalMCPoint>& mcPoints)
{
  const int N = mcPoints.size();
  fLocalMCPoints.Resize(N);
  for(int i = 0; i < N; i++){
    fLocalMCPoints[i] = mcPoints[i];
  }
}

void AliHLTTPCCAPerformance::SetHitLabels(vector<AliHLTTPCCAHitLabel>& hitLabels)
{
  const int N = hitLabels.size();
  fHitLabels.Resize(N);
  for(int i = 0; i < N; i++){
    fHitLabels[i] = hitLabels[i];
  }					   
}

void AliHLTTPCCAPerformance::SaveDataInFiles(string prefix) const
{

  {
    ofstream ofile((prefix+"hitLabels.data").data(),ios::out|ios::app);
    const int Size = fHitLabels.Size();
    ofile << Size << std::endl;
    for (int i = 0; i < fHitLabels.Size(); i++){
      const AliHLTTPCCAHitLabel &l = fHitLabels[i];
      ofile << l;
    }
    ofile.close();
  }

  {
    ofstream ofile((prefix+"MCTracks.data").data(),ios::out|ios::app);
    const int Size = fMCTracks.Size();
    ofile << Size << std::endl;
    for (int i = 0; i < fMCTracks.Size(); i++){
      const AliHLTTPCCAMCTrack &l = fMCTracks[i];
      ofile << l;
    }
    ofile.close();
  }

  {
    ofstream ofile((prefix+"MCPoints.data").data(),ios::out|ios::app);
    const int Size = fLocalMCPoints.Size();
    ofile << Size << std::endl;
    for (int i = 0; i < fLocalMCPoints.Size(); i++){
      const AliHLTTPCCALocalMCPoint &l = fLocalMCPoints[i];
      ofile << l;
    }
    ofile.close();
  }
}

void AliHLTTPCCAPerformance::ReadDataFromFiles(string prefix)
{

  {
    ifstream ifile((prefix+"hitLabels.data").data());
    int Size;
    ifile >> Size;
    fHitLabels.Resize(Size);
    for (int i = 0; i < Size; i++){
      AliHLTTPCCAHitLabel &l = fHitLabels[i];
      ifile >> l;
    }
    ifile.close();
  }

  {
    ifstream ifile((prefix+"MCTracks.data").data());
    int Size;
    ifile >> Size;
    fMCTracks.Resize(Size);
    for (int i = 0; i < Size; i++){
      AliHLTTPCCAMCTrack &l = fMCTracks[i];
      ifile >> l;
    }
    ifile.close();
  }


  {
    ifstream ifile((prefix+"MCPoints.data").data());
    int Size;
    ifile >> Size;
    fLocalMCPoints.Resize(Size);
    for (int i = 0; i < Size; i++){
      AliHLTTPCCALocalMCPoint &l = fLocalMCPoints[i];
      ifile >> l;
    }
    ifile.close();
  }
}

