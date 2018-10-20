#include "StxCAInterface.h"

#include "TPCCATracker/AliHLTTPCCAGBHit.h"
#include "TPCCATracker/AliHLTTPCCAGBTrack.h"
#include "TPCCATracker/AliHLTTPCCAParam.h"
// need for hits data
#include "StMaker.h"
#include "StEvent/StEvent.h"
#include "StEvent/StGlobalTrack.h"
#include "StEvent/StTrackDetectorInfo.h"
#include "StEvent/StTrackNode.h"
#include "StTpcHitCollection.h"
#include "StTpcSectorHitCollection.h"
#include "StTpcPadrowHitCollection.h"
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
StxCAInterface *StxCAInterface::fgStxCAInterface = 0;
//________________________________________________________________________________
StxCAInterface &StxCAInterface::Instance() {
  if (! fgStxCAInterface) fgStxCAInterface = new StxCAInterface(); 
  return *fgStxCAInterface;
}
//________________________________________________________________________________
StxCAInterface::StxCAInterface() : StTPCCAInterface() {
}
//________________________________________________________________________________
void StxCAInterface::MakeHits() {
  StEvent   *pEvent = dynamic_cast<StEvent*>( StMaker::GetTopChain()->GetInputDS("StEvent") );
  if (! pEvent) return;
  StTpcHitCollection* TpcHitCollection = pEvent->tpcHitCollection();
  if (! TpcHitCollection) { cout << "No TPC Hit Collection" << endl; return;}
  UInt_t numberOfSectors = TpcHitCollection->numberOfSectors();
  for (UInt_t i = 0; i< numberOfSectors; i++) {
    StTpcSectorHitCollection* sectorCollection = TpcHitCollection->sector(i);
    if (sectorCollection) {
      Int_t numberOfPadrows = sectorCollection->numberOfPadrows();
#if 1 /* Sti Convention */
      Int_t sector = i + 1;
      Double_t beta = (sector <= 12) ? (60 - 30*(sector - 1)) : (120 + 30 *(sector - 13));
      Double_t cb   = TMath::Cos(TMath::DegToRad()*beta);
      Double_t sb   = TMath::Sin(TMath::DegToRad()*beta);
#else
      AliHLTTPCCAParam &SlicePar = fCaParam[i];
      Double_t cb   = SlicePar.CosAlpha();
      Double_t sb   = SlicePar.SinAlpha();
#endif
      for (int j = 0; j< numberOfPadrows; j++) {
	StTpcPadrowHitCollection *rowCollection = sectorCollection->padrow(j);
	if (rowCollection) {
	  StSPtrVecTpcHit &hits = rowCollection->hits();
	  Long64_t NoHits = hits.size();
	  for (Long64_t k = 0; k < NoHits; k++) {
	    const StTpcHit *tpcHit = static_cast<const StTpcHit *> (hits[k]);
	    if ( ! tpcHit) continue;
	    //	    Int_t Id = fCaHits.size();
	    Int_t Id = fSeedHits.size();
	    StThreeVectorD glob(tpcHit->position());
	    //  StTpcCoordinateTransform tran(gStTpcDb);
	    //  StTpcLocalSectorCoordinate loc;
	    //	tran(glob,loc,tpcHit->sector(),tpcHit->padrow());
	    
	    // obtain seed Hit
	    SeedHit_t hitc;
	    hitc.padrow = tpcHit->padrow();
	    hitc.status=0;
	    hitc.taken=0;
	    hitc.track_key=tpcHit->idTruth();
	    hitc.hit  = tpcHit;
	    hitc.Id = Id;
	    fSeedHits.push_back(hitc);
	    //yf      if (  hit->timesUsed()) 	continue;//VP
	    // convert to CA Hit
	    AliHLTTPCCAGBHit caHit;
// 	    caHit.SetX( loc.position().x() );
// 	    //	    caHit.SetX( loc.position() ); // take position of the row
// 	    caHit.SetY( - loc.position().y() );
// 	    caHit.SetZ( - loc.position().z() );
	    Double_t xL =  cb*glob.x() + sb*glob.y();
	    Double_t yL = -sb*glob.x() + cb*glob.y();
	    Double_t zL =                  glob.z();
            caHit.SetX(   xL);
	    caHit.SetY( - yL);
	    caHit.SetZ( - zL);
	    // caHit.SetErrX(   );
	    caHit.SetErrY( 0.12 );// TODO: read parameters from somewhere 
	    caHit.SetErrZ( 0.16 );
	    caHit.SetISlice( tpcHit->sector() - 1 );
	    caHit.SetIRow( tpcHit->padrow()-1 );
	    caHit.SetID( Id );
	    //	    caHit.SetID( tpcHit->id() );
	    fIdTruth.push_back( tpcHit->idTruth() );
	    fCaHits.push_back(caHit);
	  }
	}
      }
    }
  }
} // void StxCAInterface::MakeHits()
//________________________________________________________________________________
void StxCAInterface::ConvertPars(const AliHLTTPCCATrackParam& caPar, double _alpha, StxNodePars& nodePars, StxNodeErrs& nodeErrs)
{
  // set jacobian integral coef 
  //                      y   z eta ptin tanl  
  static Double_t JI[5] {-1, -1, -1,   1, -1};
  // get parameters
  nodePars.x()    =         caPar.GetX();
  nodePars.y()    = JI[0] * caPar.GetY();
  nodePars.z()    = JI[1] * caPar.GetZ();
  nodePars.eta()  = JI[2] * asin(caPar.GetSinPhi()); // (signed curvature)*(local Xc of helix axis - X current point on track)
  nodePars.ptin() = JI[3] * caPar.GetQPt();          // signed invert pt [sign = sign(-qB)]
  nodePars.tanl() = JI[4] * caPar.GetDzDs();         // tangent of the track momentum dip angle
  // set jacobian integral coef
  Double_t J[5]; 
  J[0] = JI[0];                     // y
  J[1] = JI[1];                     // z
  J[2] = JI[2]/cos(nodePars.eta()); // eta
  J[3] = JI[3];                     // ptin
  J[4] = JI[4];                     // tanl
  
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
