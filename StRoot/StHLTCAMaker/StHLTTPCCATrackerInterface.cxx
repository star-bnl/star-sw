#include "StHLTTPCCATrackerInterface.h"
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
//#include "online_tracking_TpcHitMap.h"
using std::vector;
StHLTTPCCATrackerInterface *StHLTTPCCATrackerInterface::fgStHLTTPCCATrackerInterface = 0;
//________________________________________________________________________________
StHLTTPCCATrackerInterface &StHLTTPCCATrackerInterface::Instance() {
  if (! fgStHLTTPCCATrackerInterface) fgStHLTTPCCATrackerInterface = new StHLTTPCCATrackerInterface(); 
  return *fgStHLTTPCCATrackerInterface;
}
//________________________________________________________________________________
StHLTTPCCATrackerInterface::StHLTTPCCATrackerInterface() : StTPCCAInterface() {
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
#ifdef __HLT_COOR__
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
#endif /*  __HLT_COOR__ */
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
#ifdef __HLT_COOR__
      // caHit.SetX( HLTLocalXyz[0] );
      caHit.SetY( HLTLocalXyz[1] );
      caHit.SetZ( HLTLocalXyz[2] );
#else /* ! __HLT_COOR__ */
      caHit.SetY( - hit->y() );
      caHit.SetZ( - hit->z() );
#endif /*  __HLT_COOR__ */
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
void StHLTTPCCATrackerInterface::ConvertPars(const AliHLTTPCCATrackParam& caPar, double _alpha, StiNodePars& nodePars, StiNodeErrs& nodeErrs)
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
void StHLTTPCCATrackerInterface::MakeSeeds()
{
  const int NRecoTracks = fTracker->NTracks();
  for ( int iTr = 0; iTr < NRecoTracks; iTr++ ) {
      // get seed
    const AliHLTTPCCAGBTrack tr = fTracker->Track( iTr );
    Seed_t seed;

    const int NHits = tr.NHits();
    for ( int iHit = NHits-1; iHit >= 0; iHit-- ){ 
      const int index = fTracker->TrackHit( tr.FirstHitRef() + iHit );
      const int hId   = fTracker->Hit( index ).ID();
      seed.vhit.push_back(&(fSeedHits[hId]));
    }
    seed.total_hits = seed.vhit.size();
    ConvertPars( tr.OuterParam(), tr.Alpha(), seed.firstNodePars, seed.firstNodeErrs );
    ConvertPars( tr.InnerParam(), tr.Alpha(), seed.lastNodePars,  seed.lastNodeErrs );

    fSeeds.push_back(seed);
  }
} // void StHLTTPCCATrackerInterface::MakeSeeds()

//________________________________________________________________________________
void StHLTTPCCATrackerInterface::HLTHitG2L(const int iSector, const int iPadrow,
                                           const double globalXyz[3], double HLTLocalXyz[3])
{
  // convert TPC hit from global coordinations to CA sector local coordinations
  
  const double pi      = 3.14159265358979323846;
  const double piOver6 = pi / 6.0;
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
  xn = St_tpcPadConfigC::instance()->radialDistanceAtRow(iSector,iPadrow);//
  
  HLTLocalXyz[0] = xn;
  HLTLocalXyz[1] = yn;
  HLTLocalXyz[2] = zn;

  return;
}
