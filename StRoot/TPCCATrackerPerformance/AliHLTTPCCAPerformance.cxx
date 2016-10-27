// $Id: AliHLTTPCCAPerformance.cxx,v 1.3 2013/11/21 13:07:28 mzyzak Exp $
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
#if 1//def DO_TPCCATRACKER_EFF_PERFORMANCE

#include "AliHLTTPCCAPerformance.h"

#include "AliHLTTPCCounters.h"
#include "AliHLTTPCPerformanceBase.h"
#include "AliHLTTPCCASlicesLinksPerformance.h"
#include "AliHLTTPCCASlicesPerformance.h"
#include "AliHLTTPCCAStiPerformance.h"
#include "AliHLTTPCCAMergerPerformance.h"
#include "AliHLTTPCCAGlobalSlicesPerformance.h"
#include "AliHLTTPCCAGlobalPerformance.h"
#ifdef KFPARTICLE
#include "KFTopoPerformance.h"
#endif // KFPARTICLE

#include "AliHLTTPCCAGBHit.h"
#include "AliHLTTPCCAMCTrack.h"
#include "AliHLTTPCCAMCPoint.h"
#include "AliHLTTPCCAOutTrack.h"
#include "AliHLTTPCCAGBTrack.h"
#include "AliHLTTPCCAGBTracker.h"
#include "AliHLTTPCCATracklet.h"

#ifndef HLTCA_STANDALONE
#include "AliHLTTPCCADisplay.h"

#include "TRandom3.h" //dbg
#include "TMath.h"
#include "TROOT.h"
#include "Riostream.h"
#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "TProfile.h"
#include "TStyle.h"
#endif

using namespace std;
AliHLTTPCCAPerformance::AliHLTTPCCAPerformance()
{
  static bool first_call = true;

  if (first_call){
    typedef TSubPerformance TSP;
    
      /// Just define here all sub-performances
      /// TSP(new __ClassName__               , __Name__      ),
#ifndef STAR_STANDALONE

#ifdef KFPARTICLE
    const int NSPerfo = 6;
#else
    const int NSPerfo = 5;
#endif // KFPARTICLE
    
#else // HLTCA_STANDALONE

#ifdef KFPARTICLE
    const int NSPerfo = 5;
#else
    const int NSPerfo = 4;
#endif // KFPARTICLE
#endif // STAR_STANDALONE
    const TSP perfos[NSPerfo] = {
      TSP(new AliHLTTPCCASlicesLinksPerformance, "Chains Performance"),
      TSP(new AliHLTTPCCASlicesPerformance, "Sector Performance"),
      TSP(new AliHLTTPCCAGlobalSlicesPerformance, "Global Sector Performance"),
      TSP(new AliHLTTPCCAGlobalPerformance, "Global Performance")
#ifndef STAR_STANDALONE
    , TSP(new AliHLTTPCCAStiPerformance, "Sti Performance")
#endif // HLTCA_STANDALONE

//      TSP(new AliHLTTPCCAMergerPerformance, "Merger", 0)
#ifdef KFPARTICLE
    , TSP(new KFTopoPerformance, "Topo Performance")
#endif // KFPARTICLE

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

bool AliHLTTPCCAPerformance::SetNewEvent(AliHLTTPCCAGBTracker* const tracker, string mcTracksFile, string mcPointsFile)
{
  fTracker = tracker;
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
  for (unsigned int iPerf = 0; iPerf < subPerformances.size(); iPerf++){
    subPerformances[iPerf]->SetNewEvent(fTracker, &fHitLabels, &fMCTracks, &fLocalMCPoints);
  }

#if KFPARTICLE
  if ( GetSubPerformance("Topo Performance") )
    dynamic_cast<KFTopoPerformance*>(GetSubPerformance("Topo Performance"))->SetNewEvent2(fTopoReconstructor);
#endif // KFPARTICLE

#ifndef HLTCA_STANDALONE
  static bool first_call = true;
  if (first_call) CreateHistos();
  
  first_call = false;
#endif
} // void AliHLTTPCCAPerformance::InitSubPerformances

#ifndef HLTCA_STANDALONE
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


void AliHLTTPCCAPerformance::WriteDir2Current( TObject *obj )
{
  //* recursive function to write down all created in the file histos
  if ( !obj->IsFolder() ) obj->Write();
  else {
    TDirectory *cur = gDirectory;
    ((TDirectory*)obj)->cd();
    TList *listSub = ( ( TDirectory* )obj )->GetList();
    TIter it( listSub );
    while ( TObject *obj1 = it() ) WriteDir2Current( obj1 );
    cur->cd();
  }
}

void AliHLTTPCCAPerformance::WriteHistos()
{
#if KFPARTICLE
  if ( GetSubPerformance("Topo Performance") )
    dynamic_cast<KFTopoPerformance*>(GetSubPerformance("Topo Performance"))->AddV0Histos();
#endif // KFPARTICLE
  if(fOutputFile) WriteDir2Current(fOutputFile);
}
#endif // HLTCA_STANDALONE

void AliHLTTPCCAPerformance::ExecPerformance()
{
  fStatNEvents++;

#ifdef KFPARTICLE
  const unsigned int NRunPerf = subPerformances.size() - 1; // TopoPerf is run from KFParticle dirrectly
#else
  const unsigned int NRunPerf = subPerformances.size();
#endif
  
  for (unsigned int iPerf = 0; iPerf < NRunPerf; iPerf++){
    if(subPerformances[iPerf].IsGlobalPerf) subPerformances[iPerf]->Exec(0);
  }
#if 0  // current event efficiencies
  for (unsigned int iPerf = 0; iPerf < NRunPerf; iPerf++){    
    cout << endl
        << " ---- " << subPerformances[iPerf].name << " event " << fStatNEvents << " ---- "<< endl;
    subPerformances[iPerf]->PrintEfficiency();
  }
  cout << endl << " ============================== " << endl;
#endif //0
  for (unsigned int iPerf = 0; iPerf < NRunPerf; iPerf++){  
    cout << endl
        << " ---- " << subPerformances[iPerf].name << " " << fStatNEvents << " events Statistic ---- " << endl;
    if(subPerformances[iPerf].IsGlobalPerf) subPerformances[iPerf]->PrintEfficiencyStatistic();
  };

}

AliHLTTPCPerformanceBase* AliHLTTPCCAPerformance::GetSubPerformance(string name)
{
  unsigned i = 0;
  for( ; (i < subPerformances.size()) && (subPerformances[i].name != name); i++);
  //ASSERT ( i != subPerformances.size() , " Incorrect name of subPerformance used.");
  if ( i == subPerformances.size() ) return 0;
  return subPerformances[i].perf;
} // AliHLTTPCPerformanceBase* AliHLTTPCCAPerformance::GetSubPerformance(string name)


  /// -------------------- Read\write MC information ---------------------
void AliHLTTPCCAPerformance::WriteMCEvent( FILE *file ) const
{
  // write MC information to the file
  int n = fMCTracks.Size();
  std::fwrite( &n, sizeof( int ), 1, file );
  n = fHitLabels.Size();
  std::fwrite( &n, sizeof( int ), 1, file );
  std::fwrite( fMCTracks.Data(), sizeof( AliHLTTPCCAMCTrack ), fMCTracks.Size(), file );
  std::fwrite( fHitLabels.Data(), sizeof( AliHLTTPCCAHitLabel ), fHitLabels.Size(), file );
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
  UNUSED_PARAM1(read);
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
  UNUSED_PARAM1(read);
} // void AliHLTTPCCAPerformance::ReadLocalMCPoints( FILE *file )


void AliHLTTPCCAPerformance::SetMCTracks(vector<AliHLTTPCCAMCTrack>& mcTracks)
{
  const int N = mcTracks.size();
  fMCTracks.Resize(N);
  for(int i = 0; i < N; i++){
    fMCTracks[i] = mcTracks[i];
  }
}

void AliHLTTPCCAPerformance::SetRecoData(vector<int> &mcIndexes )
{  
  vector<AliHLTTPCCAPerformanceRecoTrackData> &recoData = GetSubPerformance("Global Performance")->GetRecoData();
  vector<AliHLTTPCCAPerformanceMCTrackData>   &mcData   = GetSubPerformance("Global Performance")->GetMCData();
    
  mcData.clear();
  recoData.clear();
  
  mcData.resize(fMCTracks.Size());  
  recoData.resize(mcIndexes.size());
  
  for(int iTr=0; iTr<mcIndexes.size(); iTr++)
  {
    recoData[iTr].SetMCTrack(mcIndexes[iTr], 1., 0);
    if( mcIndexes[iTr] >=0 )
      mcData[ mcIndexes[iTr] ].AddReconstructed();
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

bool AliHLTTPCCAPerformance::ReadDataFromFiles(string prefix)
{

  {
    ifstream ifile((prefix+"hitLabels.data").data());
    if ( !ifile.is_open() ) return 0;
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
    if ( !ifile.is_open() ) return 0;
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
    if ( !ifile.is_open() ) return 0;
    int Size;
    ifile >> Size;
    fLocalMCPoints.Resize(Size);
    for (int i = 0; i < Size; i++){
      AliHLTTPCCALocalMCPoint &l = fLocalMCPoints[i];
      ifile >> l;
    }
    ifile.close();
  }

	// calculate needed additional info
	{
  for (int i = 0; i < fMCTracks.Size(); ++i ){
    AliHLTTPCCAMCTrack& t = fMCTracks[i];

    vector<int> rows;
	rows.resize( AliHLTTPCCAParameters::MaxNumberOfRows8, 0 );
	for (int j = 0, iP = t.FirstMCPointID(); j < t.NMCPoints(); iP++, j++){
	   rows[fLocalMCPoints[iP].IRow()]++;
	}

	int nRows = 0;
	for (unsigned int j = 0 ; j < rows.size(); j++)
		if( rows[j] > 0 ) nRows++;

    t.SetNMCRows( nRows );
  }
	}


  return 1;
}

#ifndef HLTCA_STANDALONE
  // Use spreaded MCposition instead of hits
void AliHLTTPCCAPerformance::ShiftHitsToMC(){
  AliHLTResizableArray<AliHLTTPCCAGBHit>& hits = const_cast<AliHLTTPCCAGBTracker *>(fTracker)->fHits;

  TRandom3 rand;
  {
    int nHits = fTracker->NHits();
    for ( int ih = 0; ih < nHits; ih++ ) {
      AliHLTTPCCAGBHit &hit = hits[ih];
      const AliHLTTPCCAHitLabel &l = (fHitLabels)[hit.ID()];

      const int iMC = l.fLab[0];
      AliHLTTPCCAMCTrack &mc = (fMCTracks)[iMC];


      int MCindex = -1;
      int nFirstMC = mc.FirstMCPointID();
      int nMCPoints = mc.NMCPoints();

      AliHLTTPCCALocalMCPoint *points = &((fLocalMCPoints).Data()[nFirstMC]);
      for(int iMCPoint=0; iMCPoint<nMCPoints; iMCPoint++)
      {
        if(points[iMCPoint].ISlice() != hit.ISlice()) continue;
        if(points[iMCPoint].IRow() == hit.IRow())
        {
          if(fabs(hit.Y() - points[iMCPoint].Y())<20 && fabs(hit.Z() - points[iMCPoint].Z())<20) // dbg +
            MCindex = iMCPoint;
        }
      }
      if(MCindex == -1)
      {
        continue;
      }

      const AliHLTTPCCALocalMCPoint& point = points[MCindex];

        // get errors
      double mcEx = point.Px();
      double mcEy = point.Py();
      double mcEz = point.Pz();
      double mcEt = TMath::Sqrt( mcEx * mcEx + mcEy * mcEy );
      
      sfloat_v Err2Y = 0.f, Err2Z = 0.f;
      //float Err2Y = 0, Err2Z = 0;
      const AliHLTTPCCAParam par;
      TrackParamVector t;
      t.SetZ( point.Z() );
      float ty = mcEy / mcEt;
      t.SetSinPhi(ty);
      float tz = mcEz / mcEt;
      t.SetDzDs(tz);
      par.GetClusterErrors2(ushort_v(hit.IRow()), t, &Err2Y, &Err2Z );
      // par.GetClusterErrors2((hit.IRow()), t, Err2Y, Err2Z );
      
        // fix hits
      hit.SetZ( point.Z() - sqrt(Err2Z[0])*rand.Gaus() );
      hit.SetY( point.Y() - sqrt(Err2Y[0])*rand.Gaus() );
      hit.SetX( point.X() );
    }
  }
  
}

  // Create new hits spreading MCPositions
void AliHLTTPCCAPerformance::ResimulateHits(){
  AliHLTResizableArray<AliHLTTPCCAGBHit>& hits = const_cast<AliHLTTPCCAGBTracker *>(fTracker)->fHits;

  const int NHits = fLocalMCPoints.Size();
  const_cast<AliHLTTPCCAGBTracker *>(fTracker)->fNHits = NHits;
  // hits.Clear();
  hits.Resize(NHits);
  fHitLabels.Resize(NHits);

  
  TRandom3 rand;
  for ( int ih = 0; ih < NHits; ih++ ) {
    AliHLTTPCCALocalMCPoint &mcP = fLocalMCPoints[ih];

    AliHLTTPCCAGBHit &hit = hits[ih];
    hit.SetID(ih);
    AliHLTTPCCAHitLabel &l = fHitLabels[ih];
    l.fLab[0] = mcP.TrackI();
    l.fLab[1] = -1;
    l.fLab[2] = -1;


      // get errors
    double mcEx = mcP.Px();
    double mcEy = mcP.Py();
    double mcEz = mcP.Pz();
    double mcEt = TMath::Sqrt( mcEx * mcEx + mcEy * mcEy );
      
    sfloat_v Err2Y = 0.f, Err2Z = 0.f;
      //float Err2Y = 0, Err2Z = 0;
    const AliHLTTPCCAParam par;
    TrackParamVector t;
    t.SetZ( mcP.Z() );
    float ty = mcEy / mcEt;
    t.SetSinPhi(ty);
    float tz = mcEz / mcEt;
    t.SetDzDs(tz);
    par.GetClusterErrors2(ushort_v(hit.IRow()), t, &Err2Y, &Err2Z );
      // par.GetClusterErrors2((hit.IRow()), t, Err2Y, Err2Z );
      
      // fix hits
    hit.SetZ( mcP.Z() - sqrt(Err2Z[0])*rand.Gaus() );
    hit.SetY( mcP.Y() - sqrt(Err2Y[0])*rand.Gaus() );
    hit.SetX( mcP.X() );

    hit.SetIRow( mcP.IRow() );
    hit.SetISlice( mcP.ISlice() );
    hit.SetErrY( 0.12 );
    hit.SetErrZ( 0.16 );
  }
}
#endif // HLTCA_STANDALONE

  // Match hits with closest MCPoint
void AliHLTTPCCAPerformance::RematchHits(){
  AliHLTResizableArray<AliHLTTPCCAGBHit>& hits = const_cast<AliHLTTPCCAGBTracker *>(fTracker)->fHits;

  const int NHits = fTracker->NHits();

  for ( int ih = 0; ih < NHits; ih++ ) {
    AliHLTTPCCAGBHit &hit = hits[ih];
    AliHLTTPCCAHitLabel &l = fHitLabels[ih];

    int MCindex = -1;
    float R2 = 10e30;
    for(int iMCPoint=0; iMCPoint < fLocalMCPoints.Size(); iMCPoint++)
    {
      if(fLocalMCPoints[iMCPoint].ISlice() != hit.ISlice()) continue;
      if(fLocalMCPoints[iMCPoint].IRow()   != hit.IRow()) continue;

      const float dY = hit.Y() - fLocalMCPoints[iMCPoint].Y();
      const float dZ = hit.Z() - fLocalMCPoints[iMCPoint].Z();
      const float r2 = dY*dY + dZ*dZ;
      
      if( r2 < R2 ) {
        MCindex = iMCPoint;
        R2 = r2;
      }
    }
    if(MCindex == -1)
    {
      l.fLab[0] = -1;
      l.fLab[1] = -1;
      l.fLab[2] = -1;
      continue;
    }

    
    l.fLab[0] = fLocalMCPoints[MCindex].TrackI();
    l.fLab[1] = -1;
    l.fLab[2] = -1;
  }
}


#endif //DO_TPCCATRACKER_EFF_PERFORMANCE
