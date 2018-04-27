#include "StHLTTPCCATrackerInterface.h"
// need for hits data
#include "StTpcHit.h"                
#include "StTpcDb/StTpcDb.h"
#include "StDbUtilities/StTpcCoordinateTransform.hh"
#include "StDbUtilities/StTpcLocalSectorCoordinate.hh"
#include "StDbUtilities/StMagUtilities.h"

#include "tables/St_MagFactor_Table.h"
#include "StarMagField/StarMagField.h"
#include "StDetectorDbMaker/St_MagFactorC.h"

//#include "online/RTS/src/L4_HLT/online_tracking_TpcHitMap.h"
#include "online_tracking_TpcHitMap.h"

//________________________________________________________________________________
StHLTTPCCATrackerInterface::StHLTTPCCATrackerInterface() : StiCATpcTrackerInterface() {
#if 0
  char mapName[256];
  const char* hltPara = "HLTConf/HLTparameters"; // file name of the HLTParamaters
#endif
  double driftVelocity = 1.0E-6 * StTpcDb::instance()->DriftVelocity();
  LOG_INFO << ">>> driftvelocity = " << driftVelocity << endm;
  
  // St_MagFactor *fMagFactor    = (St_MagFactor *)GetDataBase("RunLog")->Find("MagFactor");
  // double        gFactor       = (*fMagFactor)[0].ScaleFactor;
  double gFactor = StarMagField::Instance()->GetFactor();
  LOG_INFO << "gFactor = " << gFactor << endm;
  if (!(StDetectorDbSpaceChargeR2::instance())) {
    LOG_INFO << "StDetectorDbSpaceChargeR2::instance() == 0" << endm;
    exit(1);
  }
  double spaceCharge = StDetectorDbSpaceChargeR2::instance()->getSpaceChargeCoulombs((double)gFactor);
  LOG_INFO << "spaceCharge = " << spaceCharge << endm;
#if 0
  for (int i = 0; i < nTpcSectors; ++i) {
    sprintf(mapName, "HLTConf/tpcHitMap_sector%i.bin", i+1); // hard code the path here, tmp
    hitMap[i] = new online_tracking_TpcHitMap(hltPara, i);
    hitMap[i]->loadMap(mapName);
    hitMap[i]->setDriftVelocity(driftVelocity);
    hitMap[i]->setSpaceCharge(spaceCharge);
    LOG_INFO << "loading Map: " << mapName
             << "  driftVelocity = " << hitMap[i]->getDriftVelocity()
             << "  mapDriftVelocity = " << hitMap[i]->getMapDriftVelocity() << endm;
  }
#endif
}
  
//________________________________________________________________________________
StHLTTPCCATrackerInterface::~StHLTTPCCATrackerInterface() {
#if 0
  for (int i = 0; i < nTpcSectors; ++i) {
    delete hitMap[i];
  }
#endif
}

//________________________________________________________________________________
StHLTTPCCATrackerInterface &StHLTTPCCATrackerInterface::Instance()
{
  // reference to static object
  static StHLTTPCCATrackerInterface g;
  return g;
}

//________________________________________________________________________________
void StHLTTPCCATrackerInterface::MakeHits()
{
#if 1 /*  only for alignment */
  StTpcCoordinateTransform tran(gStTpcDb);
  StTpcLocalSectorCoordinate loc;
#endif

#ifdef KEHW_DEBUG
  hit_info = fopen("hit_info.txt", "w");
#endif // KEHW_DEBUG

  for (HitMapToVectorAndEndType::iterator it= fHitsMap->begin(); it!= fHitsMap->end(); ++it){
    vector<StiHit*>& tempvec = (*it).second.hits();
    vector<StiHit*>::iterator  start = tempvec.begin();
    vector<StiHit*>::iterator  stop  = tempvec.end();
    for (vector<StiHit*>::iterator cit = start; cit != stop; cit++) {

      // get local coordinates. take into account distortion
      StiHit *hit = *cit;
      if (! hit->stHit()) continue;
      const StTpcHit *tpcHit = dynamic_cast<const StTpcHit*>(hit->stHit());
      if ( ! tpcHit) continue;
#if 1 /*  only for alignment */
      StGlobalCoordinate glob(tpcHit->position());
      tran(glob,loc,tpcHit->sector(),tpcHit->padrow());
#else
      StGlobalCoordinate loc(tpcHit->position());
#endif

      double  globalXyz[3];
      double  HLTLocalXyz[3];
      Int_t   sector     = tpcHit->sector(); // 1 - 24
      Int_t   padrow     = tpcHit->padrow();
#if 0
      Float_t pad        = tpcHit->pad();
      Float_t timeBucket = tpcHit->timeBucket();
#endif
#ifdef KEHW_DEBUG
      fprintf(hit_info, "sector = %i, padrow = %i, pad = %f, timeBucket = %f\n",
              sector, padrow, pad, timeBucket);
#endif // KEHW_DEBUG
#if 0
      hitMap[sector-1]->mapping(globalXyz, padrow, pad, timeBucket);
#endif
#ifdef KEHW_DEBUG
      fprintf(hit_info, "   St Global: %10.4f %10.4f %10.4f\n", loc.position().x(), loc.position().y(), loc.position().z());
      fprintf(hit_info, "  HLT Global: %10.4f %10.4f %10.4f\n", globalXyz[0], globalXyz[1], globalXyz[2]);
      fprintf(hit_info, "   TPC local: %10.4f %10.4f %10.4f\n", hit->x(), hit->y(), hit->z());
#endif // KEHW_DEBUG

      HLTHitG2L(sector, padrow, globalXyz, HLTLocalXyz);

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
      // caHit.SetX( hit->x() );

      caHit.SetX( hit->position() ); // take position of the row
      // caHit.SetY( - hit->y() );
      // caHit.SetZ( - hit->z() );

      // caHit.SetX( HLTLocalXyz[0] );
      caHit.SetY( HLTLocalXyz[1] );
      caHit.SetZ( HLTLocalXyz[2] );

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

#ifdef KEHW_DEBUG
  fclose(hit_info);
#endif  // KEHW_DEBUG
} // void StHLTTPCCATrackerInterface::MakeHits()

//________________________________________________________________________________
void StHLTTPCCATrackerInterface::HLTHitG2L(const int iSector, const int iPadrow,
                                           const double globalXyz[3], double HLTLocalXyz[3])
{
  // convert TPC hit from global coordinations to CA sector local coordinations
  
  const double pi      = 3.14159265358979323846;
  const double piOver6 = pi / 6.0;
  const int    NRows   = 45;
  
  float R[NRows] = { 60.000,  64.800,  69.600,   74.400,  79.200, //  5  //TODO initialize from StRoot
                     84.000,  88.800,  93.600,   98.800, 104.000, // 10
                     109.200, 114.400, 119.600, 127.195, 129.195, // 15
                     131.195, 133.195, 135.195, 137.195, 139.195, // 20
                     141.195, 143.195, 145.195, 147.195, 149.195, // 25
                     151.195, 153.195, 155.195, 157.195, 159.195, // 30
                     161.195, 163.195, 165.195, 167.195, 169.195, // 35
                     171.195, 173.195, 175.195, 177.195, 179.195, // 40
                     181.195, 183.195, 185.195, 187.195, 189.195};// 45

  // STAR global coordinates to CA local
  // setcori ( 1 <= i <= 12), rotateZ i * pi/6
  // (13 <= i <= 24), rotateZ (24 - i) * pi/6
  // x <-> y
  // z -> -Z

  double x = globalXyz[0];
  double y = globalXyz[1];
  double z = globalXyz[2];

  int i = iSector;        // Ftf Hit sector 1 - 24
  double turn_angle = 0;
  if ( i <= 12 ) {
    turn_angle = i * piOver6;
  } else {
    turn_angle = (24 - i) * piOver6;
  }

  double cos_turn_angle = std::cos(turn_angle);
  double sin_turn_angle = std::sin(turn_angle);
    
  // root TVector3::RotateZ(turn_angle)
  double xn = x*cos_turn_angle - y*sin_turn_angle ;
  double yn = x*sin_turn_angle + y*cos_turn_angle ;
  std::swap(xn, yn);
  double zn = -z;

  // printf("HTL CA local: %10.4f %10.4f %10.4f\n\n", HLTLocalXyz[0], HLTLocalXyz[1], HLTLocalXyz[2]);
#ifdef KEHW_DEBUG
    fprintf(hit_info, "HTL CA local: %10.4f %10.4f %10.4f\n\n", xn, yn, zn);
#endif // KEHW_DEBUG

  if (iPadrow <= 0 || iPadrow > 45) {
    LOG_ERROR << "Error: TPC hit pasrow run out of range, padrow = " << iPadrow << endm;
    exit(1);
  }
  xn = R[iPadrow-1];              // use padrow x for CA
  
  HLTLocalXyz[0] = xn;
  HLTLocalXyz[1] = yn;
  HLTLocalXyz[2] = zn;

  return;
}
