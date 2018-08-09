#include "StiCATpcTrackerInterface.h"
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
#include "StBFChain.h"
#include "Sti/StiDetectorBuilder.h"
#include "Sti/StiDetectorGroups.h"
#include "Sti/StiGenericDetectorGroup.h"
#include "Sti/StiToolkit.h"
  //to obtain error coefficients
#include "StDetectorDbMaker/StiTpcInnerHitErrorCalculator.h"
#include "StDetectorDbMaker/StiTpcOuterHitErrorCalculator.h"
  //to get Magnetic Field
#include "StarMagField/StarMagField.h"

#include <vector>
#include <algorithm>
using std::vector;

#include <string>
using std::string;


StiCATpcTrackerInterface &StiCATpcTrackerInterface::Instance()
{
    // reference to static object
  static StiCATpcTrackerInterface g;
  return g;
}

StiCATpcTrackerInterface::StiCATpcTrackerInterface()
{
  //yf   SetNewEvent();
} // StiCATpcTrackerInterface::StiCATpcTrackerInterface()

StiCATpcTrackerInterface::~StiCATpcTrackerInterface(  )
{ // never called for static object
} // StiCATpcTrackerInterface::StiCATpcTrackerInterface()

void StiCATpcTrackerInterface::SetNewEvent()
{
  fHitsMap = 0;
  fSeeds.clear();

  fSeedFinder = 0;
  fStiTracks = 0;

  fIdTruth.clear(); // id of the Track, which has created CaHit
  fCaParam.clear();// settings for all sectors to give CATracker
  fCaHits.clear(); // hits to give CATracker
  fSeedHits.clear();          // hits to make seeds

//VP  if (!fSeedFinder) fSeedFinder = new StiCATpcSeedFinder;
  
  if (fTracker)    delete fTracker;
  fTracker    = new AliHLTTPCCAGBTracker;
  if (fStiTracker) delete fStiTracker;
  fStiTracker = new AliHLTTPCCAGBTracker; 
  
}


  /// Copy data to CATracker. Run CATracker. Copy tracks in fSeeds.
void StiCATpcTrackerInterface::Run()
{
  assert(fHitsMap != 0);

  TStopwatch timer;
  timer.Start();

  MakeSettings();
  MakeHits();


  
    // run tracker
  fTracker->SetSettings(fCaParam);
  fTracker->SetHits(fCaHits);
  
#ifdef STORE_STANDALONE_DATA // write data in files for Standalone
  static int iEvent = -1;
  iEvent++;
  TString name = "./data/";
  if (iEvent == 0) fTracker->SaveSettingsInFile(string(name));
  name += "event";
  name += iEvent;
  name += "_";
  fTracker->SaveHitsInFile(string(name));
// check
  if(1){
  if (fTracker)    delete fTracker;
  fTracker    = new AliHLTTPCCAGBTracker;
  TString name = "./data/";
  fTracker->ReadSettingsFromFile(string(name));
  name += "event";
  name += iEvent;
  name += "_";
  fTracker->ReadHitsFromFile(string(name));
  fTracker->SetSettings(fCaParam);
  fTracker->SetHits(fCaHits);

  }
#endif // STORE_STANDALONE_DATA



  timer.Stop();
  fPreparationTime_real = timer.RealTime();
  fPreparationTime_cpu = timer.CpuTime();  
  
  std::cout<<" - CA FindTracks() start -\n";
  fTracker->FindTracks();
  std::cout<<" - fTracker->NTracks(): "<<fTracker->NTracks()<<"\n";

    // copy hits
  timer.Start();
  // --- Tracking time ---
  const int NTimers = fTracker->NTimers();
  static int statIEvent = 0;
  static double *statTime = new double[NTimers];
  static double statTime_SliceTrackerTime = 0;
  static double statTime_SliceTrackerCpuTime = 0;
  
// #ifndef __Kozlov__
//   MakeSeeds();
// #else /* __Kozlov__ */
  if (!statIEvent){
    for (int i = 0; i < NTimers; i++){
      statTime[i] = 0;
    }
  }
// #endif /* __Kozlov__ */
  
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
  // ---

//  RunPerformance();

    // copy hits
//  timer.Start();
//
  MakeSeeds();

  timer.Stop();
  fPreparationTime_real += timer.RealTime();
  fPreparationTime_cpu += timer.CpuTime();
} // void StiCATpcTrackerInterface::Run()

void StiCATpcTrackerInterface::RunPerformance()
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
  
} // void StiCATpcTrackerInterface::Run()


void StiCATpcTrackerInterface::MakeSettings()
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
    if (! StiKalmanTrackNode::IsLaser()) {
      float x[3]={0,0,0},b[3];
      StarMagField::Instance()->BField(x,b);
      SlicePar.SetBz       ( - b[2] );   // change sign because change z
    } else {
      static const double HZERO=2e-6;
      SlicePar.SetBz (HZERO);
    }
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

    Double_t *coeffInner = StiTpcInnerHitErrorCalculator::instance()->coeff();
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
} // void StiCATpcTrackerInterface::MakeSettings()


void StiCATpcTrackerInterface::MakeHits()
{
  StTpcCoordinateTransform tran(gStTpcDb);
  StTpcLocalSectorCoordinate loc;
  for (HitMapToVectorAndEndType::iterator it= fHitsMap->begin(); it!= fHitsMap->end(); ++it){
    vector<StiHit*>& tempvec = (*it).second.hits();
    vector<StiHit*>::iterator  start = tempvec.begin();
    vector<StiHit*>::iterator  stop  = tempvec.end();
    for (vector<StiHit*>::iterator cit = start; cit != stop; cit++) {

        // get local coordinates. take into account distortion
      StiHit *hit = *cit;
      if (! hit->stHit()) 	continue;
      //yf      if (  hit->timesUsed()) 	continue;//VP
      
      const StTpcHit *tpcHit = dynamic_cast<const StTpcHit*>(hit->stHit());
      if ( ! tpcHit) continue;
      StGlobalCoordinate glob(tpcHit->position());
      tran(glob,loc,tpcHit->sector(),tpcHit->padrow());

        // obtain seed Hit
      SeedHit_t hitc;
#if 0
      hitc.mMinPad  = tpcHit->minPad();
      hitc.mMaxPad  = tpcHit->maxPad();
      hitc.mMinTmbk = tpcHit->minTmbk();
      hitc.mMaxTmbk = tpcHit->maxTmbk();
#endif
      hitc.padrow = tpcHit->padrow()-1;
      hitc.x = loc.position().x();
      hitc.y = loc.position().y();
      hitc.z = loc.position().z();
      hitc.status=0;
      hitc.taken=0;
      hitc.track_key=tpcHit->idTruth();
      hitc.hit  = hit;
      fSeedHits.push_back(hitc);

        // convert to CA Hit
      AliHLTTPCCAGBHit caHit;
      caHit.SetIRow( hitc.padrow );
//      caHit.SetX( hit->x() );
      caHit.SetX( hit->position() ); // take position of the row
      caHit.SetY( - hit->y() );
      caHit.SetZ( - hit->z() );
        // caHit.SetErrX(   );
      caHit.SetErrY( 0.12 );// TODO: read parameters from somewhere 
      caHit.SetErrZ( 0.16 );
      caHit.SetISlice( tpcHit->sector() - 1 );
      caHit.SetIRow( hitc.padrow );
      caHit.SetID( fCaHits.size() );
      fIdTruth.push_back( hitc.track_key );

      fCaHits.push_back(caHit);
    }
  }

} // void StiCATpcTrackerInterface::MakeHits()

void StiCATpcTrackerInterface::ConvertPars(const AliHLTTPCCATrackParam& caPar, double _alpha, StiNodePars& nodePars, StiNodeErrs& nodeErrs)
{
    // set jacobian integral coef
  double JI[5]; 
  JI[0] = -1.;                    // y
  JI[1] = -1.;                    // z
  JI[2] = -1.;         // eta
  JI[3] =  1.;         // ptin
  JI[4] = -1.;         // tanl
    // get parameters
  nodePars.x()    = caPar.GetX();
  nodePars.y()    = JI[0] * caPar.GetY();
  nodePars.z()    = JI[1] * caPar.GetZ();
  nodePars.eta()  = JI[2] * asin(caPar.GetSinPhi()); // (signed curvature)*(local Xc of helix axis - X current point on track)
  nodePars.ptin() = JI[3] * caPar.GetQPt();        // signed invert pt [sign = sign(-qB)]
  nodePars.tanl() = JI[4] * caPar.GetDzDs();         // tangent of the track momentum dip angle
  
    // get h & signB
  static const double EC = 2.99792458e-4, ZEROHZ = 2e-6;
// #define USE_CA_FIELD
#ifndef USE_CA_FIELD // use field in the point. Need this because of check in StiTrackNodeHelper::set()
  const double ca = cos(_alpha), sa = sin(_alpha); // code has been taken from StiKalmanTrackNode::getHz()
  double globalXYZ[3] = {
    ca * nodePars.x() - sa * nodePars.y(),
    sa * nodePars.x() + ca * nodePars.y(),
    nodePars.z()
  };

  double h2=ZEROHZ;
  if (! StiKalmanTrackNode::IsLaser()) {
    double b[3];
    StarMagField::Instance()->BField(globalXYZ,b);
    h2 = b[2];
  } 

#else  // these parameters have been obtained with that MF, so let's use it.
  double h2 = - fTracker->Slice(0).Param().Bz(); // change sign because change z
#endif // 1
  h2 *= EC;
    // get parameters. continue
  nodePars.hz() = h2;  // Z component magnetic field in units Pt(Gev) = Hz * RCurv(cm)
  nodePars.ready(); // set cosCA, sinCA & curv
//std::cout << nodePars._ptin << std::endl;
    // set jacobian integral coef
  double J[5]; 
  J[0] = JI[0];                    // y
  J[1] = JI[1];                    // z
  J[2] = JI[2]/cos(nodePars.eta()); // eta
  J[3] = JI[3];                    // ptin
  J[4] = JI[4];                    // tanl
  
    // get cov matrises
  const float *caCov = caPar.GetCov();
//   double nodeCov[15];
//   for (int i1 = 0, i = 0; i1 < 5; i1++){
//     for (int i2 = 0; i2 <= i1; i2++, i++){
//       nodeCov[i] = J[i1]*J[i2]*caCov[i];
//     }
//   }
  // if ( (caCov[0] <= 0) || (caCov[2] <= 0) || (caCov[5] <= 0) || (caCov[9] <= 0) || (caCov[14] <= 0))
  //   cout << "Warrning: Bad CA Cov Matrix." << endl;
  // if ( (nodeCov[0] <= 0) || (nodeCov[2] <= 0) || (nodeCov[5] <= 0) || (nodeCov[9] <= 0) || (nodeCov[14] <= 0))
  //   cout << "Warrning: Bad Node Cov Matrix." << endl;

  double *A = nodeErrs.G();
/*  for (int i1 = 0, i = 0; i1 < 5; i1++){
    for (int i2 = 0; i2 <= i1; i2++, i++){
      A[i+i1+2] = caCov[i];
    }
  }*/
  
  nodeErrs._cYY = caCov[ 0];
  nodeErrs._cZY = caCov[ 1]*J[0]*J[1];
  nodeErrs._cZZ = caCov[ 2]*J[0]*J[1];
  nodeErrs._cEY = caCov[ 3]*J[0]*J[2];
  nodeErrs._cEZ = caCov[ 4]*J[1]*J[2];
  nodeErrs._cEE = caCov[ 5]*J[2]*J[2];
  nodeErrs._cTY = caCov[ 6]*J[0]*J[4];
  nodeErrs._cTZ = caCov[ 7]*J[1]*J[4];
  nodeErrs._cTE = caCov[ 8]*J[2]*J[4];    
  nodeErrs._cTT = caCov[ 9]*J[4]*J[4];
  nodeErrs._cPY = caCov[10]*J[0]*J[3];
  nodeErrs._cPZ = caCov[11]*J[1]*J[3];
  nodeErrs._cPE = caCov[12]*J[2]*J[3];
  nodeErrs._cTP = caCov[13]*J[4]*J[3];
  nodeErrs._cPP = caCov[14]*J[3]*J[3];
#if 1  
  A[0] = 1; // don't use parameter X
  A[1] = 0;
  A[3] = 0;
  A[6] = 0;
  A[10] = 0;
  A[15] = 0;
#endif
}
//________________________________________________________________________________
void StiCATpcTrackerInterface::MakeSeeds()
{
  const int NRecoTracks = fTracker->NTracks();
  for ( int iTr = 0; iTr < NRecoTracks; iTr++ ) {
      // get seed
    const AliHLTTPCCAGBTrack tr = fTracker->Track( iTr );
    Seed_t seed;

    const int NHits = tr.NHits();
//VP    float last_x = 1e10; // for check
    for ( int iHit = NHits-1; iHit >= 0; iHit-- ){ 
      const int index = fTracker->TrackHit( tr.FirstHitRef() + iHit );
      const int hId   = fTracker->Hit( index ).ID();
//      if ( last_x == fSeedHits[hId].hit->position() ) continue; // track can have 2 hits on 1 row because of track segments merger.
      seed.vhit.push_back(&(fSeedHits[hId]));
//      assert( last_x >= fSeedHits[hId].hit->position() ); // should be back order - from outer to inner.
//VP      last_x = fSeedHits[hId].hit->position();
    }

    seed.total_hits = seed.vhit.size();

    ConvertPars( tr.OuterParam(), tr.Alpha(), seed.firstNodePars, seed.firstNodeErrs );
    ConvertPars( tr.InnerParam(), tr.Alpha(), seed.lastNodePars,  seed.lastNodeErrs );

    fSeeds.push_back(seed);
  }
} // void StiCATpcTrackerInterface::MakeSeeds()

