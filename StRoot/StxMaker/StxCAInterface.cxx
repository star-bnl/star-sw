#include "StxCAInterface.h"
#include "TPCCATracker/AliHLTTPCCAGBTracker.h"
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
#include "RTS/src/DAQ_TPX/tpxFCF_flags.h" // for FCF flag definition
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
	    if (tpcHit->flag() & FCF_CHOPPED || tpcHit->flag() & FCF_SANITY)     continue; // ignore hits marked by AfterBurner as chopped or bad sanity
	    if (tpcHit->pad() > 182 || tpcHit->timeBucket() > 511) continue; // some garbadge  for y2001 daq
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
void StxCAInterface::ConvertPars(const AliHLTTPCCATrackParam& caPar, Double_t _alpha, StxCApar& stxPar)
{
  // set jacobian integral coef 
  //                      y   z eta ptin tanl  
  static Double_t JI[5] {-1, -1, -1,   1, -1};
  // get parameters
  stxPar.pars.x()    =         caPar.GetX();
  stxPar.pars.y()    = JI[0] * caPar.GetY();
  stxPar.pars.z()    = JI[1] * caPar.GetZ();
  stxPar.pars.eta()  = JI[2] * asin(caPar.GetSinPhi()); // (signed curvature)*(local Xc of helix axis - X current point on track)
  stxPar.pars.ptin() = JI[3] * caPar.GetQPt();          // signed invert pt [sign = sign(-qB)]
  stxPar.pars.tanl() = JI[4] * caPar.GetDzDs();         // tangent of the track momentum dip angle
  // set jacobian integral coef
  Double_t J[5]; 
  J[0] = JI[0];                        // y
  J[1] = JI[1];                        // z
  J[2] = JI[2]/cos(stxPar.pars.eta()); // eta
  J[3] = JI[3];                        // ptin
  J[4] = JI[4];                        // tanl
  
    // get cov matrises
  const float *caCov = caPar.GetCov();

  Double_t *A = stxPar.errs.G();
  
  stxPar.errs._cYY = caCov[ 0];
  stxPar.errs._cZY = caCov[ 1]*J[0]*J[1];
  stxPar.errs._cZZ = caCov[ 2]*J[0]*J[1];
  stxPar.errs._cEY = caCov[ 3]*J[0]*J[2];
  stxPar.errs._cEZ = caCov[ 4]*J[1]*J[2];
  stxPar.errs._cEE = caCov[ 5]*J[2]*J[2];
  stxPar.errs._cTY = caCov[ 6]*J[0]*J[4];
  stxPar.errs._cTZ = caCov[ 7]*J[1]*J[4];
  stxPar.errs._cTE = caCov[ 8]*J[2]*J[4];    
  stxPar.errs._cTT = caCov[ 9]*J[4]*J[4];
  stxPar.errs._cPY = caCov[10]*J[0]*J[3];
  stxPar.errs._cPZ = caCov[11]*J[1]*J[3];
  stxPar.errs._cPE = caCov[12]*J[2]*J[3];
  stxPar.errs._cTP = caCov[13]*J[4]*J[3];
  stxPar.errs._cPP = caCov[14]*J[3]*J[3];

  A[0] = 1; // don't use parameter X
  A[1] = 0;
  A[3] = 0;
  A[6] = 0;
  A[10] = 0;
  A[15] = 0;
  stxPar.chi2 = caPar.Chi2();
  stxPar.NDF  = caPar.NDF();
}
//________________________________________________________________________________
