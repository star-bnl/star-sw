#include "StiCATpcTrackerInterface.h"
#include "TPCCATracker/AliHLTTPCCAGBHit.h"
#include "TPCCATracker/AliHLTTPCCAGBTrack.h"
#include "TPCCATracker/AliHLTTPCCAParam.h"
// need for hits data
#include "StMaker.h"
#include "StMessMgr.h"
#include "StTpcHit.h"                
#include "StTpcDb/StTpcDb.h"
#include "StDbUtilities/StTpcCoordinateTransform.hh"
#include "StDbUtilities/StTpcLocalSectorCoordinate.hh"
// for MCdata
#include "tables/St_g2t_track_Table.h" 
#include "tables/St_g2t_tpc_hit_Table.h"
#include "TDatabasePDG.h"
#include "TF1.h"
//to obtain error coefficients
#include "StDetectorDbMaker/StiTpcInnerHitErrorCalculator.h"
#include "StDetectorDbMaker/StiTpcOuterHitErrorCalculator.h"
#include "StDetectorDbMaker/StiTPCHitErrorCalculator.h"
#include "Sti/StiKalmanTrackNode.h"
#include "StEvent/StEnumerations.h"
#include "StEvent/StEvent.h"
#include "StEvent/StEventSummary.h"
//to get Magnetic Field
#include "StarMagField/StarMagField.h"
#include "TStopwatch.h"
#include <vector>
#include <algorithm>
using std::vector;
StiCATpcTrackerInterface *StiCATpcTrackerInterface::fgStiCATpcTrackerInterface = 0;
//________________________________________________________________________________
StiCATpcTrackerInterface &StiCATpcTrackerInterface::Instance() {
  if (! fgStiCATpcTrackerInterface) fgStiCATpcTrackerInterface = new StiCATpcTrackerInterface(); 
  return *fgStiCATpcTrackerInterface;
}
//________________________________________________________________________________
void StiCATpcTrackerInterface::SetNewEvent() {
  fSeedFinder = 0; 
  fSeeds.clear(); 
  fSeedHits.clear(); 
  fHitsMap = 0; 
  StTPCCAInterface::SetNewEvent();
  if (! fSpectrum) {
    TFile *f = 0;
    if (StMaker::GetTopChain()) {
      f = StMaker::GetTopChain()->GetTFile();
      if (f) f->cd();
    }
    Int_t NzBins = 2500;
    Int_t npeaks = 250;
    Double_t zmin = -250;
    Double_t zmax = 250;
    const Char_t *side[3] = {"East","West","All"};
    for (Int_t i = 0; i < 3; i++) {
      fVertexZPlots[i] = new TH1F(Form("VertexZPlot%s",side[i]),Form("z-dca distribution for side = %s",side[i]),NzBins,zmin,zmax);
      fVertexZPlots[i]->SetDirectory(f);
      fVertexXYPlots[i] = new TH2F(Form("VertexXYPlot%s",side[i]),Form("xy-dca distribution for side = %s at Z +/- 10 cm of the 1st peak",side[i]),100,-5,5,100,-5,5);
      fVertexXYPlots[i]->SetDirectory(f);
      if (i != 2) {
	fVertexZPlots[i]->SetMarkerColor(i+2); 
	fVertexZPlots[i]->SetLineColor(i+2); 
	fVertexXYPlots[i]->SetMarkerColor(i+2); 
	fVertexXYPlots[i]->SetLineColor(i+2); 
      }
    }
    fSpectrum = new TSpectrum(npeaks);
  } else {
    for (Int_t i = 0; i < 3; i++) {
      fVertexZPlots[i]->Reset();  fVertexZPlots[i]->GetXaxis()->SetRange(0,-1);
      fVertexXYPlots[i]->Reset();
    }
  }
  StiKalmanTrackNode::SetExternalTriggerOffset(0);
}
//________________________________________________________________________________
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
      Int_t Id = fSeedHits.size();
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
      //      caHit.SetID( fCaHits.size() );
      caHit.SetID( Id );
      fIdTruth.push_back( hitc.track_key );

      fCaHits.push_back(caHit);
    }
  }

} // void StiCATpcTrackerInterface::MakeHits()
//________________________________________________________________________________
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
  // Reset trigger offset from CA
  if (fVertexZPlots[east] && fVertexZPlots[west]) {
    StEvent* pEvent = (StEvent*) StMaker::GetChain()->GetInputDS("StEvent");
    StEventSummary *summary = pEvent->summary();
    L4CAVertex *l4Vx[3] = {&summary->L4VxEast, &summary->L4VxWest, &summary->L4Vx};
    for (Int_t i = 0; i < 3; i++) {
      //      memset (&(l4Vx->Const), 0, sizeof(L4CAVertex));
      TF1 *gaus = (TF1 *) fVertexZPlots[i]->GetListOfFunctions()->FindObject("gaus");
      if (! gaus) continue;
      l4Vx[i]->Const = gaus->GetParameter(0); l4Vx[i]->dConst = gaus->GetParError(0); 
      l4Vx[i]->Mu    = gaus->GetParameter(1); l4Vx[i]->dMu = gaus->GetParError(1); 
      l4Vx[i]->Sigma = gaus->GetParameter(2); l4Vx[i]->dSigma = gaus->GetParError(2); 
      l4Vx[i]->Chisq = gaus->GetChisquare();
      l4Vx[i]->Ndf   = gaus->GetNDF();
      l4Vx[i]->Prob  = gaus->GetProb();
      l4Vx[i]->X     = fVertexXYPlots[i]->GetMean(1);
      l4Vx[i]->Y     = fVertexXYPlots[i]->GetMean(2);
    }
#if 0
    Double_t Z[2] = {0};
    Double_t dZ[2] = {0};
    for (Int_t i = 0; i < 2; i++) {
      if (gaus[i]) {
	if (gaus[i]->GetProb() > 1e-5) {
	  if (gaus[i]->GetParError(1) < 1.0) {
	    dZ[i] = gaus[i]->GetParError(1);
	    Z[i] = gaus[i]->GetParameter(1);
	  }
	}
      }
    }
    if (dZ[0] > 0 && dZ[1] > 0) {
      Double_t Zdiff = 0.5*(Z[1] - Z[0]);
      Double_t dZdiff = 0.5*TMath::Sqrt(dZ[0]*dZ[0] + dZ[1]*dZ[1]);
      if (TMath::Abs(Zdiff) > 3*dZdiff) {
	Double_t dT = Zdiff/StTpcDb::instance()->DriftVelocity()*1e6; // 
	if (TMath::Abs(dT) < 1) {
	  LOG_INFO << "dZW_E/2 = " << Zdiff << " +/- " << dZdiff << ". Set trigger offset = " << dT << " musec" << endm;
	  StiKalmanTrackNode::SetExternalTriggerOffset(dT);
	}
      }
    }
#endif
  }
} // void StiCATpcTrackerInterface::MakeSeeds()
//________________________________________________________________________________

