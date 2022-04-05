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

  // for sti perfo
#ifdef DO_TPCCATRACKER_EFF_PERFORMANCE
#include "TPCCATrackerPerformance/AliHLTTPCCAStiPerformance.h"
#include "TPCCATrackerPerformance/AliHLTTPCCAMergerPerformance.h"
#include "StDetectorDbMaker/St_tpcPadConfigC.h"
#include "Sti/StiKalmanTrack.h"
#include "Sti/StiKalmanTrackNode.h"
#endif /* DO_TPCCATRACKER_EFF_PERFORMANCE */
#include <vector>
#include <algorithm>
using std::vector;

#include <string>
using std::string;

#ifdef DO_TPCCATRACKER_EFF_PERFORMANCE
//#define STORE_STANDALONE_DATA // write data in files for Standalone
#endif

StiCATpcTrackerInterface &StiCATpcTrackerInterface::Instance()
{
    // reference to static object
  static StiCATpcTrackerInterface g;
  return g;
}

StiCATpcTrackerInterface::StiCATpcTrackerInterface()
{
#ifdef DO_TPCCATRACKER_EFF_PERFORMANCE
  fOutFile = StMaker::GetChain()->GetTFile();
  if(!fOutFile) cout << "W StiCATpcTrackerInterface: Warning - There isn't any tag file, so histograms won't be saved!" << endl;

  fPerformance = &(AliHLTTPCCAPerformance::Instance());
  fPerformance->SetOutputFile(fOutFile);
#endif
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
#ifdef DO_TPCCATRACKER_EFF_PERFORMANCE
  assert(fPerformance != 0);
  fMCTracks.clear();
  fMCPoints.clear();
  fHitLabels.clear();
#endif
  fCaParam.clear();// settings for all sectors to give CATracker
  fCaHits.clear(); // hits to give CATracker
  fSeedHits.clear();          // hits to make seeds

//VP  if (!fSeedFinder) fSeedFinder = new StiCATpcSeedFinder;
  
  if (fTracker)    delete fTracker;
  fTracker    = new AliHLTTPCCAGBTracker;
  if (fStiTracker) delete fStiTracker;
  fStiTracker = new AliHLTTPCCAGBTracker; 
  
#ifdef DO_TPCCATRACKER_EFF_PERFORMANCE
  fPerformance->SetTracker(fTracker);
#endif
}


  /// Copy data to CATracker. Run CATracker. Copy tracks in fSeeds.
void StiCATpcTrackerInterface::Run()
{
  assert(fHitsMap != 0);

  TStopwatch timer;
  timer.Start();

  MakeSettings();
  MakeHits();

#ifdef DO_TPCCATRACKER_EFF_PERFORMANCE
    // put data in performance
  FillPerformance(fCaHits,fIdTruth, fMCTracks,fMCPoints,fHitLabels);
  fPerformance->SetMCTracks(fMCTracks);
  fPerformance->SetMCPoints(fMCPoints);
  fPerformance->SetHitLabels(fHitLabels);
#endif //  DO_TPCCATRACKER_EFF_PERFORMANCE

  
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
#ifdef DO_TPCCATRACKER_EFF_PERFORMANCE
  fPerformance->SaveDataInFiles(string(name));
#endif
// check
  if(1){
  if (fTracker)    delete fTracker;
  fTracker    = new AliHLTTPCCAGBTracker;
#ifdef DO_TPCCATRACKER_EFF_PERFORMANCE
  fPerformance->SetTracker(fTracker);
#endif
  TString name = "./data/";
  fTracker->ReadSettingsFromFile(string(name));
  name += "event";
  name += iEvent;
  name += "_";
  fTracker->ReadHitsFromFile(string(name));
#ifdef DO_TPCCATRACKER_EFF_PERFORMANCE
  fPerformance->ReadDataFromFiles(string(name));
#endif
  fTracker->SetSettings(fCaParam);
  fTracker->SetHits(fCaHits);

  }
#endif // STORE_STANDALONE_DATA



  timer.Stop();
  fPreparationTime_real = timer.RealTime();
  fPreparationTime_cpu = timer.CpuTime();  
  
  fTracker->FindTracks();

    // copy hits
  timer.Start();
  
  MakeSeeds();
  
  timer.Stop();
  fPreparationTime_real += timer.RealTime();
  fPreparationTime_cpu += timer.CpuTime();    
} // void StiCATpcTrackerInterface::Run()

void StiCATpcTrackerInterface::RunPerformance()
{
  cout << " ---- CA TPC Tracker ---- " << endl;
#ifdef DO_TPCCATRACKER_EFF_PERFORMANCE
  TStopwatch timer;
  timer.Start();
    
  assert(fStiTracks  != 0);
  assert(fStiTracker != 0);

  // ----- Efficiency ---------
  
    // init all performances
  fPerformance->InitSubPerformances();

    // -- correct init of stiPerformance --

  FillStiPerformance(); // get fStiCaHits fStiIdTruth fStiCaTracks
  
    // prepare tracker
  fStiTracker->SetHits(fStiCaHits);
  fStiTracker->SetSettings(fCaParam);

  AliHLTTPCCAStiPerformance* stiPerf = (AliHLTTPCCAStiPerformance*)fPerformance->GetSubPerformance("Sti Performance");
  stiPerf->SetNewEvent(fStiTracker,&fStiHitLabels,&fStiMCTracks,&fStiMCPoints);
  stiPerf->SetRecoTracks(fStiCaTracks);  
  fPerformance->ExecPerformance();

  timer.Stop();
  fPreparationTime_real += timer.RealTime();
  fPreparationTime_cpu += timer.CpuTime();   
#endif //  DO_TPCCATRACKER_EFF_PERFORMANCE

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
  
#if 0//def DO_TPCCATRACKER_EFF_PERFORMANCE   outdated
  ((AliHLTTPCCAMergerPerformance*)(AliHLTTPCCAPerformance::Instance().GetSubPerformance("Merger")))->FillTree();
#endif //DO_TPCCATRACKER_EFF_PERFORMANCE
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
      if ( !(hit->detector()->isActive()) ) continue;
      if (! hit->stHit()) 	continue;
      if (  hit->timesUsed()) 	continue;//VP
      
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

#ifdef DO_TPCCATRACKER_EFF_PERFORMANCE
bool myfunction (AliHLTTPCCALocalMCPoint i,AliHLTTPCCALocalMCPoint j) { 
  return (i.TrackI() < j.TrackI()) || ( i.TrackI()==j.TrackI() && i.IRow() < j.IRow() ); 
}
  /// fill fPerformance by MCTracks, MCPoints and Hit-MCPointsMatch
void StiCATpcTrackerInterface::FillPerformance(const vector<AliHLTTPCCAGBHit>& caHits, const vector<int>& idsTruth, vector<AliHLTTPCCAMCTrack>& mcTracks, vector<AliHLTTPCCALocalMCPoint>& mcPoints, vector<AliHLTTPCCAHitLabel>& hitLabels)
{
  mcTracks.clear();
  mcPoints.clear();
  hitLabels.clear();
  hitLabels.resize(caHits.size());

  StBFChain *chain = (StBFChain *) StMaker::GetChain();
  St_g2t_track *Track = (St_g2t_track *) chain->FindObject("g2t_track");
  St_g2t_tpc_hit *mcTpcHits = (St_g2t_tpc_hit *) chain->FindObject("g2t_tpc_hit");
  if (Track && mcTpcHits) {
    static  StTpcLocalSectorCoordinate        localSect;
    static  StTpcLocalSectorDirection         dirSect;
    StTpcCoordinateTransform transform(gStTpcDb);
    int nMCTracks = Track->GetNRows();
    const g2t_track_st *track = Track->GetTable();
    
    vector<int> truthToIndex; // need for finding MCPoints to MCTracks correspondence
    truthToIndex.resize(nMCTracks+1,-1);
    
    const g2t_tpc_hit_st *mcTpcHit = mcTpcHits->GetTable();
    const Int_t nMcTpcHits = mcTpcHits->GetNRows();
     
    AliHLTTPCCAMCTrack mcTrack;
    for (Int_t l = 0; l < nMCTracks; l++, track++) {
      if (! track->n_tpc_hit) continue;
      if (track->ptot < 1e-3) continue;
      memset(&mcTrack, 0, sizeof(AliHLTTPCCAMCTrack));
      mcTrack.SetPDG( TDatabasePDG::Instance()->ConvertGeant3ToPdg(track->ge_pid) );
      TParticlePDG *part = TDatabasePDG::Instance()->GetParticle( mcTrack.PDG() );
      Float_t      q     = part->Charge();
      mcTrack.SetP ( track->ptot );
      mcTrack.SetPt( track->pt );
        //      mcTrack.SetNHits( track->n_tpc_hit ); // it is not really what we need. See below
      for (Int_t i = 0; i < 3; i++) { // TODO
        mcTrack.SetPar( i, ((i>=1)?-1:1) * 0 );
      }
      for (Int_t i = 0; i < 3; i++) {
        mcTrack.SetPar( 3+i, ((i>=1)?-1:1) * track->p[i]/mcTrack.P() );
      }
      mcTrack.SetPar(6, q/mcTrack.P()/3 ); // q=3q      
      mcTrack.SetNTurns( 1 ); // TODO: read from somewhere
      const int idTruth = l+1;

      truthToIndex[idTruth] = mcTracks.size();
      mcTracks.push_back(mcTrack);
    } // for iMCTrack
    
      // get hits
    const int nHits = caHits.size();
    for (int iHit = 0; iHit < nHits; iHit++){
      const int iTrack = truthToIndex[idsTruth[iHit]];
      if (iTrack == -1) continue;
      AliHLTTPCCAHitLabel &l = hitLabels[iHit];
      l.fLab[0] = iTrack;
      l.fLab[1] = -1;
      l.fLab[2] = -1;
      mcTracks[iTrack].SetNHits( 1 + mcTracks[iTrack].NHits() );
    } // j
      
      // get MCPoints
    for (Int_t iP = 0; iP < nMcTpcHits; iP++, mcTpcHit++) {
      AliHLTTPCCALocalMCPoint mcPoint;
      Int_t is     =  mcTpcHit->volume_id/100000;
      if (is) continue;
      const int iTrack = truthToIndex[mcTpcHit->track_p];
      if (iTrack == -1) continue;
      AliHLTTPCCAMCTrack& mcTrack = mcTracks[iTrack];
      
      mcPoint.SetIRow( mcTpcHit->volume_id%100 - 1 );
      mcPoint.SetISlice( (mcTpcHit->volume_id/100)%100 - 1 );

      Int_t sector = (mcTpcHit->volume_id%100000)/100;
      Int_t row    =  mcTpcHit->volume_id%100;

        // get detector
      StiDetectorBuilder* detectorBuilder = 0;
      StiDetectorGroups *groups=StiToolkit::instance()->getDetectorGroups();
      vector<StiGenericDetectorGroup *>::iterator it = groups->begin();
      for (; it != groups->end(); ++it) {
        StiGenericDetectorGroup *group = *it;
        detectorBuilder = group->getDetectorBuilder();
        TString builderName = (const char*)(detectorBuilder->getName().c_str());
        if (builderName == "TpcBuilder")
          break;
      }
      int sectorTpm = sector - 1;
      if (sectorTpm >= 12)    sectorTpm = 11 - (sectorTpm - 11)%12;
      StiDetector* detector = detectorBuilder->getDetector(row-1,sectorTpm);

      static StiHit tmpStiHit; // used for convert only

        // Get XYZ
      StTpcHit* mpTmp = new StTpcHit;
      double energyTmp = 0.;
      tmpStiHit.setGlobal(detector, mpTmp, mcTpcHit->x[0], mcTpcHit->x[1], mcTpcHit->x[2], energyTmp);
      mcPoint.SetX( tmpStiHit.x() );
      mcPoint.SetY( - tmpStiHit.y() );
      mcPoint.SetZ( - tmpStiHit.z() );

        // Get P
      StGlobalDirection  gDir(mcTpcHit->p[0],mcTpcHit->p[1],mcTpcHit->p[2]);
      transform(gDir,dirSect,sector,row);
      double px = mcTpcHit->p[0], py = mcTpcHit->p[1], pz_new = mcTpcHit->p[2], px_new, py_new;
      double angle = detector->getPlacement()->getNormalRefAngle();
      px_new =  cos(angle)*px + sin(angle)*py;
      py_new = -sin(angle)*px + cos(angle)*py;

      mcPoint.SetPx( px_new );
      mcPoint.SetPy( - py_new );
      mcPoint.SetPz( - pz_new );
      const int qpSign = fabs(mcTrack.Par(6))/mcTrack.Par(6);
      mcPoint.SetQP( double( qpSign ) / sqrt(px_new*px_new + py_new*py_new + pz_new*pz_new) );
      mcPoint.SetTrackI( iTrack );
      mcPoint.SetTrackID( mcTpcHit->track_p );

      mcPoints.push_back(mcPoint);
    } // for mcPoints
    
    std::sort( mcPoints.begin(), mcPoints.end(), myfunction );
    mcTracks[0].SetFirstMCPointID(0);
    int iPLast = 0;
    int iTr = 0;
    for( UInt_t iP = 0; iP < mcPoints.size(); ++iP ) {
      if ( mcPoints[iP].TrackI() != iTr ) {
        mcTracks[iTr].SetNMCPoints(iP - iPLast);
        iPLast = iP;
        iTr = mcPoints[iP].TrackI();
        mcTracks[iTr].SetFirstMCPointID(iP);
      }
    }
    mcTracks[iTr].SetNMCPoints(mcPoints.size() - iPLast);
  }
} // void StiCATpcTrackerInterface::FillPerformance()

void StiCATpcTrackerInterface::FillStiPerformance()
{
  fStiCaHits.clear();
  fStiIdTruth.clear();
  fStiCaTracks.clear();
  
  AliHLTTPCCAStiPerformance* stiPerf = (AliHLTTPCCAStiPerformance*)fPerformance->GetSubPerformance("Sti Performance");
  int NGBHits = 0;

  
  StTpcCoordinateTransform tran(gStTpcDb);
  StTpcLocalSectorCoordinate loc;

  for(int iTr=0; iTr<fStiTracks->getTrackCount(0); iTr++)
  {
    auto * track = (StiKalmanTrack*) fStiTracks->at(iTr);
    vector<StiHit*> hits_v = track-> getHits();

    AliHLTTPCCAGBTrack GBTrack;
    int nTrackHits = 0;

        // get local coordinates. take into account distortion

    for(unsigned iH=0; iH<hits_v.size(); iH++)
    {
      StiHit *hit = hits_v[iH];
      if (! hit->stHit()) 	continue;
      if (  hit->timesUsed()) 	continue;
      const StTpcHit *tpcHit = dynamic_cast<const StTpcHit*>(hit->stHit());
      if ( ! tpcHit) continue;
      StGlobalCoordinate glob(tpcHit->position());
      tran(glob,loc,tpcHit->sector(),tpcHit->padrow());

        // convert to CA Hit
      AliHLTTPCCAGBHit caHit;
      caHit.SetIRow( tpcHit->padrow()-1 );
      caHit.SetX( hit->position() ); // take position of the row
      caHit.SetY( - hit->y() );
      caHit.SetZ( - hit->z() );
      caHit.SetErrY( 0.12 );// TODO: read parameters from somewhere 
      caHit.SetErrZ( 0.16 );
      caHit.SetISlice( tpcHit->sector() - 1 );
      caHit.SetID( fStiCaHits.size() );
      fStiIdTruth.push_back( tpcHit->idTruth() );

      int iHit = NGBHits+iH;
      stiPerf->AddHit(iHit);
      nTrackHits++;
      
      fStiCaHits.push_back(caHit);
    }
            
    StiKalmanTrackNode *NodePar = track->getInnerMostDetHitNode(kTpcId);
    if (!NodePar) continue;

    double JI[5]; 
    JI[0] = -1.;                    // y
    JI[1] = -1.;                    // z
    JI[2] = -1.;                    // eta
    JI[3] =  1.;                    // ptin
    JI[4] = -1.;                    // tanl

    double J[5]; 
    J[0] = JI[0];                              // y
    J[1] = JI[1];                              // z
    J[2] = JI[2]*cos(NodePar->fitPars().eta()); // eta
    J[3] = JI[3];                              // ptin
    J[4] = JI[4];                              // tanl
    
    double Cos = cos(NodePar->fitPars().eta());
    float sgnCos = fabs(Cos)/Cos;

    AliHLTTPCCATrackParam    GBTrackParam;
    GBTrackParam.SetX     ( NodePar->fitPars().x() );
    GBTrackParam.SetY     ( JI[0] * NodePar->fitPars().y() );
    GBTrackParam.SetZ     ( JI[1] * NodePar->fitPars().z() );
    GBTrackParam.SetSinPhi( JI[2] * sin(NodePar->fitPars().eta()));
    GBTrackParam.SetQPt   ( JI[3] * NodePar->fitPars().ptin() );
    GBTrackParam.SetDzDs  ( JI[4] * NodePar->fitPars().tanl() );

    GBTrackParam.SetCov( 0, NodePar->fitErrs()._cYY);
    GBTrackParam.SetCov( 1, NodePar->fitErrs()._cZY*J[0]*J[1]);
    GBTrackParam.SetCov( 2, NodePar->fitErrs()._cZZ*J[0]*J[1]);
    GBTrackParam.SetCov( 3, NodePar->fitErrs()._cEY*J[0]*J[2]);
    GBTrackParam.SetCov( 4, NodePar->fitErrs()._cEZ*J[1]*J[2]);
    GBTrackParam.SetCov( 5, NodePar->fitErrs()._cEE*J[2]*J[2]);
    GBTrackParam.SetCov( 6, NodePar->fitErrs()._cTY*J[0]*J[4]);
    GBTrackParam.SetCov( 7, NodePar->fitErrs()._cTZ*J[1]*J[4]);
    GBTrackParam.SetCov( 8, NodePar->fitErrs()._cTE*J[2]*J[4]);    
    GBTrackParam.SetCov( 9, NodePar->fitErrs()._cTT*J[4]*J[4]);
    GBTrackParam.SetCov(10, NodePar->fitErrs()._cPY*J[0]*J[3]);
    GBTrackParam.SetCov(11, NodePar->fitErrs()._cPZ*J[1]*J[3]);
    GBTrackParam.SetCov(12, NodePar->fitErrs()._cPE*J[2]*J[3]);
    GBTrackParam.SetCov(13, NodePar->fitErrs()._cTP*J[4]*J[3]);
    GBTrackParam.SetCov(14, NodePar->fitErrs()._cPP*J[3]*J[3]);
    
    GBTrackParam.SetSignCosPhi(sgnCos);
    
    GBTrack.SetInnerParam(GBTrackParam);
    
    GBTrack.SetNHits( nTrackHits );
    GBTrack.SetFirstHitRef( NGBHits );
    
    NGBHits += nTrackHits;
    
    fStiCaTracks.push_back(GBTrack);
 }

    // prepare mc
  vector<AliHLTTPCCAMCTrack> stiMCTracks;
  vector<AliHLTTPCCALocalMCPoint> stiMCPoints;
  vector<AliHLTTPCCAHitLabel> stiHitLabels;
  FillPerformance(fStiCaHits,fStiIdTruth, stiMCTracks,stiMCPoints,stiHitLabels);

  fStiMCTracks.Resize(stiMCTracks.size());
  for (unsigned i = 0; i < stiMCTracks.size(); i++) {
    fStiMCTracks[i] = stiMCTracks[i];
  }
  fStiMCPoints.Resize(stiMCPoints.size());
  for (unsigned i = 0; i < stiMCPoints.size(); i++) {
    fStiMCPoints[i] = stiMCPoints[i];
  }
  fStiHitLabels.Resize(stiHitLabels.size());
  for (unsigned i = 0; i < stiHitLabels.size(); i++) {
    fStiHitLabels[i] = stiHitLabels[i];
  }
  

} // void StiCATpcTrackerInterface::FillStiPerformance()
#endif // DO_TPCCATRACKER_EFF_PERFORMANCE
