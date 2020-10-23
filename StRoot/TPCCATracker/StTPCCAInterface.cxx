#include "StTPCCAInterface.h"
#include "TPCCATracker/AliHLTTPCCAGBHit.h"
#include "TPCCATracker/AliHLTTPCCAGBTrack.h"
#include "TPCCATracker/AliHLTTPCCAParam.h"
  // need for hits data
#include "StTpcHit.h"                
#include "StTpcDb/StTpcDb.h"
#include "StDbUtilities/StTpcCoordinateTransform.hh"
#include "StDbUtilities/StTpcLocalSectorCoordinate.hh"
  // for MCdata
#include "tables/St_g2t_track_Table.h" 
#include "tables/St_g2t_tpc_hit_Table.h"
#include "TDatabasePDG.h"
  //to obtain error coefficients
#include "StDetectorDbMaker/StiTpcInnerHitErrorCalculator.h"
#include "StDetectorDbMaker/StiTpcOuterHitErrorCalculator.h"
#include "StDetectorDbMaker/StiTPCHitErrorCalculator.h"
  //to get Magnetic Field
#include "StarMagField/StarMagField.h"
#include "TStopwatch.h"
#include <vector>
#include <algorithm>
using std::vector;
//________________________________________________________________________________
void StTPCCAInterface::SetNewEvent()
{
  fIdTruth.clear(); // id of the Track, which has created CaHit
  fCaParam.clear();// settings for all sectors to give CATracker
  fCaHits.clear(); // hits to give CATracker
  if (fTracker)    delete fTracker;
  fTracker    = new AliHLTTPCCAGBTracker;
}
//________________________________________________________________________________
void StTPCCAInterface::Run()
{
  assert(fHitsMap != 0);

  TStopwatch timer;
  timer.Start();

  MakeSettings();
  MakeHits();
  // run tracker
  fTracker->SetSettings(fCaParam);
  fTracker->SetHits(fCaHits);
  timer.Stop();
  fPreparationTime_real = timer.RealTime();
  fPreparationTime_cpu = timer.CpuTime();  
  
  std::cout<<" - CA FindTracks() start -\n";
  fTracker->FindTracks();
  std::cout<<" - fTracker->NTracks(): "<<fTracker->NTracks()<<"\n";
  
  // --- DCA test ---
  std::cout<<" ------- FindTracks - done - dca test -------\n";
  auto dca_left = fTracker->GetLeftDCA();	// dca_right, GetRightDCA
  std::cout<<" - sca_left.size: "<<dca_left.size()<<"\n";
  for( int i = 0; i < dca_left.size(); i++ ) {
    std::cout<<" - > i: "<<i<<"; x: "<<dca_left[i].x<<"; y: "<<dca_left[i].y<<"; z: "<<dca_left[i].z<<"\n";
  }
  auto dca_right = fTracker->GetRightDCA();
  std::cout<<" - sca_right.size: "<<dca_right.size()<<"\n";
  for( int i = 0; i < dca_right.size(); i++ ) {
    std::cout<<" - > i: "<<i<<"; x: "<<dca_right[i].x<<"; y: "<<dca_right[i].y<<"; z: "<<dca_right[i].z<<"\n";
  }
  // ---

  // copy hits
  timer.Start();
  // --- Tracking time ---
  const int NTimers = fTracker->NTimers();
  static int statIEvent = 0;
  static double *statTime = new double[NTimers];
  static double statTime_SliceTrackerTime = 0;
  static double statTime_SliceTrackerCpuTime = 0;
  
  if (!statIEvent){
    for (int i = 0; i < NTimers; i++){
      statTime[i] = 0;
    }
  }
  
  statIEvent++;
  for (int i = 0; i < NTimers; i++){
    statTime[i] += fTracker->StatTime( i );
  }
  statTime_SliceTrackerTime += fTracker->SliceTrackerTime();
  statTime_SliceTrackerCpuTime += fTracker->SliceTrackerCpuTime();

  if (statTime_SliceTrackerTime > 0) {
    std::cout << "Reconstruction Time"
	      << " Real = " << std::setw( 10 ) << 1./statIEvent*(statTime_SliceTrackerTime+statTime[ 9 ]) * 1.e3 << " ms,"
	      << " CPU = " << std::setw( 10 ) << 1./statIEvent*(statTime_SliceTrackerCpuTime+statTime[ 10 ]) * 1.e3 << " ms,"
	      << " parallelization speedup (only SectorTracker): " << statTime_SliceTrackerCpuTime / statTime_SliceTrackerTime
	      << std::endl;
    if ( 1 ) {
      std::cout
	<< " |  ------ Sector trackers (w\\o init): " << std::setw( 10 ) << 1./statIEvent*statTime[ 0 ] * 1000. << " ms\n"
	<< " |      Initialization: " << std::setw( 10 )  << 1./statIEvent*statTime[ 12 ] * 1000. << " ms\n"
	<< " |    NeighboursFinder: " << std::setw( 10 ) << 1./statIEvent*statTime[ 1 ] * 1000. << " ms, " << std::setw( 12 ) << 1./statIEvent*statTime[ 5 ] << " cycles\n"
	<< " |   NeighboursCleaner: " << std::setw( 10 ) << 1./statIEvent*statTime[ 11 ] * 1000. << " ms\n"
	<< " |     StartHitsFinder: " << std::setw( 10 ) << 1./statIEvent*statTime[ 4 ] * 1000. << " ms\n"
	<< " | TrackletConstructor: " << std::setw( 10 ) << 1./statIEvent*statTime[ 2 ] * 1000. << " ms, " << std::setw( 12 ) << 1./statIEvent*statTime[ 7 ] << " cycles\n"
	<< " |    TrackletSelector: " << std::setw( 10 ) << 1./statIEvent*statTime[ 3 ] * 1000. << " ms, " << std::setw( 12 ) << 1./statIEvent*statTime[ 8 ] << " cycles\n"
	<< " |         WriteOutput: " << std::setw( 10 ) << 1./statIEvent*statTime[ 6 ] * 1000. << " ms\n"
	<< " |  --------------------------  Merge: " << std::setw( 10 ) << 1./statIEvent*statTime[ 9 ] * 1000. << " ms\n"
	<< " |      Initialization: " << std::setw( 10 )  << 1./statIEvent*statTime[ 13 ] * 1000. << " ms\n"
	<< " | -- NoOverlapTrackMerge: " << std::setw( 10 )  << 1./statIEvent*statTime[ 14 ] * 1000. << " ms\n"
	<< " |               Merge: " << std::setw( 10 )  << 1./statIEvent*statTime[ 16 ] * 1000. << " ms\n"
	<< " |           DataStore: " << std::setw( 10 )  << 1./statIEvent*statTime[ 18 ] * 1000. << " ms\n"
	<< " | ---- OverlapTrackMerge: " << std::setw( 10 )  << 1./statIEvent*statTime[ 15 ] * 1000. << " ms\n"
	<< " |               Merge: " << std::setw( 10 )  << 1./statIEvent*statTime[ 17 ] * 1000. << " ms\n"
	<< " |           DataStore: " << std::setw( 10 )  << 1./statIEvent*statTime[ 19 ] * 1000. << " ms\n"
	;
    }
  }
  //
  MakeSeeds();

  timer.Stop();
  fPreparationTime_real += timer.RealTime();
  fPreparationTime_cpu += timer.CpuTime();
} // void StTPCCAInterface::Run()
//________________________________________________________________________________
void StTPCCAInterface::RunPerformance()
{
  cout << " ---- CA TPC Tracker ---- " << endl;
  
  // ----- Timing ---------
  //  TODO saparete in procedure in TPCCATracker/ ...
#if 1
  const int printTime = 3; // 0 - show nothing; 1 - show avarege; 2 - show cur event; 3 - show both
  const int fullTiming = 1; // show info about each stage of tracking; same as printTime
  if ((printTime == 2) || (printTime == 3)){
        
    std::cout << "Sector reconstruction Time"
              << " Real = " << std::setw( 10 ) << fTracker->SliceTrackerTime() * 1.e3 << " ms,"
              << " CPU = " << std::setw( 10 ) << fTracker->SliceTrackerCpuTime() * 1.e3 << " ms,";
    if (fTracker->SliceTrackerTime() > 0)
      std::cout        << " parallelization speedup: " << fTracker->SliceTrackerCpuTime() / fTracker->SliceTrackerTime();
    std::cout        << std::endl;
    if ((fullTiming == 2) || (fullTiming == 3)) {
      std::cout
        << " |  sum slice trackers: " << std::setw( 10 ) << fTracker->StatTime( 0 ) * 1000. << " ms\n"
        << " |    NeighboursFinder: " << std::setw( 10 ) << fTracker->StatTime( 1 ) * 1000. << " ms, " << std::setw( 12 ) << fTracker->StatTime( 5 ) << " cycles\n"
        << " |     StartHitsFinder: " << std::setw( 10 ) << fTracker->StatTime( 4 ) * 1000. << " ms\n"
        << " | TrackletConstructor: " << std::setw( 10 ) << fTracker->StatTime( 2 ) * 1000. << " ms, " << std::setw( 12 ) << fTracker->StatTime( 7 ) << " cycles\n"
        << " |    TrackletSelector: " << std::setw( 10 ) << fTracker->StatTime( 3 ) * 1000. << " ms, " << std::setw( 12 ) << fTracker->StatTime( 8 ) << " cycles\n"
        << " |         WriteOutput: " << std::setw( 10 ) << fTracker->StatTime( 6 ) * 1000. << " ms\n"
        << " |               merge: " << std::setw( 10 ) << fTracker->StatTime( 9 ) * 1000. << " ms" << std::endl;
    }
    std::cout << "Total (sector+merge) reconstuction time"
              << " Real = " << std::setw( 10 ) << (fTracker->SliceTrackerTime()+fTracker->StatTime( 9 )) * 1.e3 << " ms,"
              << " CPU = " << std::setw( 10 ) << (fTracker->SliceTrackerCpuTime()+fTracker->StatTime( 10 )) * 1.e3 << " ms"
              << std::endl;
    std::cout << "Preparation time"
              << " Real = " << std::setw( 10 ) << fPreparationTime_real * 1.e3 << " ms,"
              << " CPU = " << std::setw( 10 ) << fPreparationTime_cpu * 1.e3 << " ms"
              << std::endl;
  };
  if ((printTime == 1) || (printTime == 3)){
    const int NTimers = 20;
    static int statIEvent = 0;
    static double statTime[NTimers];
    static double statTime_SliceTrackerTime = 0;
    static double statTime_SliceTrackerCpuTime = 0;
    static double statPreparationTime_real = 0;
    static double statPreparationTime_cpu = 0;
    
    if (!statIEvent){
      for (int i = 0; i < NTimers; i++){
        statTime[i] = 0;
      }
    }

    statIEvent++;
    for (int i = 0; i < NTimers; i++){
      statTime[i] += fTracker->StatTime( i );
    }
    statTime_SliceTrackerTime += fTracker->SliceTrackerTime();
    statTime_SliceTrackerCpuTime += fTracker->SliceTrackerCpuTime();
    statPreparationTime_real += fPreparationTime_real;
    statPreparationTime_cpu  += fPreparationTime_cpu;
        
    std::cout << "Average sector reconstruction Time"
              << " Real = " << std::setw( 10 ) << 1./statIEvent*statTime_SliceTrackerTime * 1.e3 << " ms,"
              << " CPU = " << std::setw( 10 ) << 1./statIEvent*statTime_SliceTrackerCpuTime * 1.e3 << " ms,";
    if (statTime_SliceTrackerTime > 0)
      std::cout       << " parallelization speedup: " << statTime_SliceTrackerCpuTime / statTime_SliceTrackerTime;
    std::cout       << std::endl;
    if ((fullTiming == 1) || (fullTiming == 3)) {
      std::cout
        << " |  sum slice trackers: " << std::setw( 10 ) << 1./statIEvent*statTime[ 0 ] * 1000. << " ms\n"
        << " |    NeighboursFinder: " << std::setw( 10 ) << 1./statIEvent*statTime[ 1 ] * 1000. << " ms, " << std::setw( 12 ) << 1./statIEvent*statTime[ 5 ] << " cycles\n"
        << " |     StartHitsFinder: " << std::setw( 10 ) << 1./statIEvent*statTime[ 4 ] * 1000. << " ms\n"
        << " | TrackletConstructor: " << std::setw( 10 ) << 1./statIEvent*statTime[ 2 ] * 1000. << " ms, " << std::setw( 12 ) << 1./statIEvent*statTime[ 7 ] << " cycles\n"
        << " |    TrackletSelector: " << std::setw( 10 ) << 1./statIEvent*statTime[ 3 ] * 1000. << " ms, " << std::setw( 12 ) << 1./statIEvent*statTime[ 8 ] << " cycles\n"
        << " |         WriteOutput: " << std::setw( 10 ) << 1./statIEvent*statTime[ 6 ] * 1000. << " ms\n"
        << " |               merge: " << std::setw( 10 ) << 1./statIEvent*statTime[ 9 ] * 1000. << " ms" << std::endl;
    }
    std::cout << "Total (sector+merge) avarage reconstuction time" 
              << " Real = " << std::setw( 10 ) << (statTime_SliceTrackerTime+statTime[ 9 ])/statIEvent * 1.e3 << " ms,"
              << " CPU = " << std::setw( 10 ) << (statTime_SliceTrackerCpuTime+statTime[ 10 ])/statIEvent * 1.e3 << " ms"
              << std::endl;
    std::cout << "Avarage preparation time"
              << " Real = " << std::setw( 10 ) << statPreparationTime_real/statIEvent * 1.e3 << " ms,"
              << " CPU = " << std::setw( 10 ) << statPreparationTime_cpu/statIEvent * 1.e3 << " ms."
              << std::endl;
  }
#endif // 0 timing
  
} // void StTPCCAInterface::Run()
//________________________________________________________________________________
void StTPCCAInterface::MakeSettings()
{
  
  const int NSlices = 24; //TODO initialize from StRoot
  for ( int iSlice = 0; iSlice < NSlices; iSlice++ ) {
    AliHLTTPCCAParam SlicePar;
    //    memset(&SlicePar, 0, sizeof(AliHLTTPCCAParam));

    Int_t sector = iSlice+1;
      // Int_t sector = iSlice;
    const int NoOfInnerRows = St_tpcPadConfigC::instance()->innerPadRows(sector);
    const int NRows = St_tpcPadConfigC::instance()->padRows(sector);
    SlicePar.SetISlice( iSlice );
    SlicePar.SetNRows ( NRows ); 
    SlicePar.SetNInnerRows ( NoOfInnerRows ); 
    SlicePar.SetNTpcRows ( NRows ); 
    Double_t beta = 0;
    if (sector > 12) beta = (24-sector)*2.*TMath::Pi()/12.;
    else             beta =     sector *2.*TMath::Pi()/12.;
    SlicePar.SetAlpha  ( beta );
    SlicePar.SetDAlpha  ( 30*TMath::DegToRad() );                        //TODO initialize from StRoot
    SlicePar.SetCosAlpha ( TMath::Cos(SlicePar.Alpha()) );
    SlicePar.SetSinAlpha ( TMath::Sin(SlicePar.Alpha()) );
    SlicePar.SetAngleMin ( SlicePar.Alpha() - 0.5*SlicePar.DAlpha() );
    SlicePar.SetAngleMax ( SlicePar.Alpha() + 0.5*SlicePar.DAlpha() );
    SlicePar.SetRMin     (  51. );                                        //TODO initialize from StRoot
    SlicePar.SetRMax     ( 194. );                                        //TODO initialize from StRoot
    SlicePar.SetErrX     (   0. );                                        //TODO initialize from StRoot
    SlicePar.SetErrY     (   0.12 ); // 0.06  for Inner                        //TODO initialize from StRoot
    SlicePar.SetErrZ     (   0.16 ); // 0.12  for Inner                NodePar->fitPars()        //TODO initialize from StRoot
      //   SlicePar.SetPadPitch (   0.675 );// 0.335 -"-
    float x[3]={0,0,0},b[3];
    StarMagField::Instance()->BField(x,b);
    SlicePar.SetBz       ( - b[2] );   // change sign because change z
    if (sector <= 12) {
      SlicePar.SetZMin     (   0. );                                        //TODO initialize from StRoot
      SlicePar.SetZMax     ( 210. );                                        //TODO initialize from StRoot
    } else {
      SlicePar.SetZMin     (-210. );                                        //TODO initialize from StRoot
      SlicePar.SetZMax     (   0. );                                        //TODO initialize from StRoot
    }
    for( int iR = 0; iR < NRows; iR++){
      SlicePar.SetRowX(iR, St_tpcPadConfigC::instance()->radialDistanceAtRow(sector,iR+1));
    }

    Double_t *coeffInner = 0;
    if (St_tpcPadConfigC::instance()->iTPC(sector)) {
      coeffInner = StiTPCHitErrorCalculator::instance()->coeff();
    } else {
      coeffInner = StiTpcInnerHitErrorCalculator::instance()->coeff();
    }
    for(int iCoef=0; iCoef<6; iCoef++)
    {
      SlicePar.SetParamS0Par(0, 0, iCoef, (float)coeffInner[iCoef] );
    }  
  
    SlicePar.SetParamS0Par(0, 0, 6, 0.0f );
    
    Double_t *coeffOuter =StiTpcOuterHitErrorCalculator::instance()->coeff();
    for(int iCoef=0; iCoef<6; iCoef++)
    {
      SlicePar.SetParamS0Par(0, 1, iCoef, (float)coeffOuter[iCoef] );
    }
  
    SlicePar.SetParamS0Par(0, 1, 6, 0.0f );
    SlicePar.SetParamS0Par(0, 2, 0, 0.0f );
    SlicePar.SetParamS0Par(0, 2, 1, 0.0f );
    SlicePar.SetParamS0Par(0, 2, 2, 0.0f );
    SlicePar.SetParamS0Par(0, 2, 3, 0.0f );
    SlicePar.SetParamS0Par(0, 2, 4, 0.0f );
    SlicePar.SetParamS0Par(0, 2, 5, 0.0f );
    SlicePar.SetParamS0Par(0, 2, 6, 0.0f );
    SlicePar.SetParamS0Par(1, 0, 0, 0.0f );
    SlicePar.SetParamS0Par(1, 0, 1, 0.0f );
    SlicePar.SetParamS0Par(1, 0, 2, 0.0f );
    SlicePar.SetParamS0Par(1, 0, 3, 0.0f );
    SlicePar.SetParamS0Par(1, 0, 4, 0.0f );
    SlicePar.SetParamS0Par(1, 0, 5, 0.0f );
    SlicePar.SetParamS0Par(1, 0, 6, 0.0f );
    SlicePar.SetParamS0Par(1, 1, 0, 0.0f );
    SlicePar.SetParamS0Par(1, 1, 1, 0.0f );
    SlicePar.SetParamS0Par(1, 1, 2, 0.0f );
    SlicePar.SetParamS0Par(1, 1, 3, 0.0f );
    SlicePar.SetParamS0Par(1, 1, 4, 0.0f );
    SlicePar.SetParamS0Par(1, 1, 5, 0.0f );
    SlicePar.SetParamS0Par(1, 1, 6, 0.0f );
    SlicePar.SetParamS0Par(1, 2, 0, 0.0f );
    SlicePar.SetParamS0Par(1, 2, 1, 0.0f );
    SlicePar.SetParamS0Par(1, 2, 2, 0.0f );
    SlicePar.SetParamS0Par(1, 2, 3, 0.0f );
    SlicePar.SetParamS0Par(1, 2, 4, 0.0f );
    SlicePar.SetParamS0Par(1, 2, 5, 0.0f );
    SlicePar.SetParamS0Par(1, 2, 6, 0.0f );
    
    fCaParam.push_back(SlicePar);
  } // for iSlice
} // void StTPCCAInterface::MakeSettings()
