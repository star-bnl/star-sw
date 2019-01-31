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
#include "StBTofHit.h"
#include "StBTofCollection.h"
// for MCdata
#include "tables/St_g2t_track_Table.h" 
#include "tables/St_g2t_tpc_hit_Table.h"
#include "TDatabasePDG.h"
//to obtain error coefficients
#include "StDetectorDbMaker/StiTpcInnerHitErrorCalculator.h"
#include "StDetectorDbMaker/StiTpcOuterHitErrorCalculator.h"
#include "StDetectorDbMaker/StiTPCHitErrorCalculator.h"
#include "StDetectorDbMaker/StiBTofHitErrorCalculator.h"
#include "RTS/src/DAQ_TPX/tpxFCF_flags.h" // for FCF flag definition
//to get Magnetic Field
#include "StarMagField/StarMagField.h"
#include "TGeoManager.h"
#include "TGeoMatrix.h"
#include "TGeoPhysicalNode.h"
#include "TGeoBBox.h"
#include "StarVMC/StarVMCApplication/StarVMCDetector.h"
#include "TStopwatch.h"
#include <vector>
#include <algorithm>
using std::vector;
StxCAInterface *StxCAInterface::fgStxCAInterface = 0;
Int_t StxCAInterface::fDebug = 0;
//________________________________________________________________________________
StxCAInterface &StxCAInterface::Instance() {
  if (! fgStxCAInterface) fgStxCAInterface = new StxCAInterface(); 
  return *fgStxCAInterface;
}
//________________________________________________________________________________
StxCAInterface::StxCAInterface() : StTPCCAInterface() {
}
//________________________________________________________________________________
void StxCAInterface::MakeSettings() {
  
  const int NSlices = 24; //TODO initialize from StRoot
  for ( int iSlice = 0; iSlice < NSlices; iSlice++ ) {
    AliHLTTPCCAParam SlicePar;
    //    memset(&SlicePar, 0, sizeof(AliHLTTPCCAParam));

    Int_t sector = iSlice+1;
      // Int_t sector = iSlice;
    const int NoOfInnerRows = St_tpcPadConfigC::instance()->innerPadRows(sector);
    const int NRows = St_tpcPadConfigC::instance()->padRows(sector);
    SlicePar.SetISlice( iSlice );
    SlicePar.SetNInnerRows ( NoOfInnerRows ); 
    SlicePar.SetNTpcRows ( NRows ); 
    SlicePar.SetNRows ( NRows + 2); // +2 for BToF
    
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
    SlicePar.SetRMax     ( 223. );                                        //TODO initialize from StRoot
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
    for( int iR = 0; iR < SlicePar.NRows(); iR++){
      if (iR < SlicePar.NTpcRows()    )
	SlicePar.SetRowX(iR, St_tpcPadConfigC::instance()->radialDistanceAtRow(sector,iR+1));
      else 
	if (iR == SlicePar.NRows()) SlicePar.SetRowX(iR, 214.5); // ToF 
	else                        SlicePar.SetRowX(iR, 211.5); // ToF
	
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

    Double_t *coeffBToF =StiBTofHitErrorCalculator::instance()->coeff();
    for(int iCoef=0; iCoef<6; iCoef++)
    {
      SlicePar.SetParamS0Par(0, 2, iCoef, (float)coeffBToF[iCoef] );
    }
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
//________________________________________________________________________________
void StxCAInterface::MakeHits() {
  StEvent   *pEvent = dynamic_cast<StEvent*>( StMaker::GetTopChain()->GetInputDS("StEvent") );
  if (! pEvent) return;
  StTpcHitCollection* TpcHitCollection = pEvent->tpcHitCollection();
  if (! TpcHitCollection) { LOG_ERROR << "StxCAInterface::MakeHits: No TPC Hit Collection" << endm; return;}
  // BToF hits
  Int_t nBToFHit = 0;
  StBTofCollection *bToFcol = pEvent->btofCollection();
  if (!bToFcol) {
    LOG_ERROR << "StxCAInterface::MakeHits:\tNo StBTofCollection" << endm;
  }
  UInt_t numberOfSectors = TpcHitCollection->numberOfSectors();
  for (UInt_t i = 0; i< numberOfSectors; i++) {
    StTpcSectorHitCollection* sectorCollection = TpcHitCollection->sector(i);
    Int_t numberOfPadrows = sectorCollection->numberOfPadrows();
    Int_t sector = i + 1;
    Double_t beta = (sector <= 12) ? (60 - 30*(sector - 1)) : (120 + 30 *(sector - 13));
    Double_t cb   = TMath::Cos(TMath::DegToRad()*beta);
    Double_t sb   = TMath::Sin(TMath::DegToRad()*beta);
    if (sectorCollection) {
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
	    Int_t Id = fSeedHits.size();
	    StThreeVectorD glob(tpcHit->position());
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
  LOG_INFO << "StxCAInterface::MakeHits:  Loaded " << fCaHits.size() << " TPC hits." << endm;
#if 1
  // BToF hits
  if (bToFcol) {
    StSPtrVecBTofHit& vec = bToFcol->tofHits();
    for(UInt_t j=0; j<vec.size(); j++)	{
      StBTofHit *aHit = vec[j];
      if(!aHit)   continue;
      if (Debug()) {
	LOG_INFO <<Form("hit tray: %i module: %i cell: %i",aHit->tray(), aHit->module(), aHit->cell()) << endm;
      }
      // Sanity check	
      if (aHit->tray()   <  1 || aHit->tray()   > StBTofHit::kNTray   ||
	  aHit->module() <= 0 || aHit->module() > StBTofHit::kNModule ||
	  aHit->cell()   <= 0 || aHit->cell()   > StBTofHit::kNCell) continue;
      Int_t iSlice = (aHit->tray() - 1)/5;
      Int_t sector = iSlice + 1;
#if 0
      Int_t half = (sector <= 12) ? 1 : 2;
      Int_t indx[3] = {half, (aHit->tray()-1)%60+1, aHit->module()};
      static TString path2ToF("HALL_1/CAVE_1/TpcRefSys_1/BTOF_1/BTOH_%d/BSEC_%d/BTRA_1/BXTR_1/BRTC_1/BGMT_1/BRMD_%d/BRDT_1/BRSG_3");
#endif
      TString path = aHit->GetPath(); // (StarVMCDetector::FormPath(path2ToF,3,indx));
      if (! gGeoManager->CheckPath(path)) {
	cout << "Illegal path " << path.Data() << endl;
	assert(0);
      }
      Double_t beta = (sector <= 12) ? (60 - 30*(sector - 1)) : (120 + 30 *(sector - 13));
      Double_t cb   = TMath::Cos(TMath::DegToRad()*beta);
      Double_t sb   = TMath::Sin(TMath::DegToRad()*beta);
      TObjArray *nodes = gGeoManager->GetListOfPhysicalNodes();
      TGeoPhysicalNode *nodeP = 0;
      if (nodes) nodeP = (TGeoPhysicalNode *) nodes->FindObject(path);
      if (! nodeP) nodeP =gGeoManager->MakePhysicalNode(path);
      if (! nodeP) {
	cout << "TGeoPhysicalNode with path " << path.Data() << " does not exists" << endl;
	assert(0);
      }
      StThreeVectorD local(0, aHit->position().y(), 0);
      static StThreeVectorD glob;
      nodeP->GetMatrix()->LocalToMaster(local.xyz(), glob.xyz());
      // obtain seed Hit
      Int_t Id = fSeedHits.size();
      SeedHit_t hitc;
      Int_t row = fCaParam[iSlice].NTpcRows() + 1 + aHit->tray()%2;
      hitc.padrow =  row;
      hitc.status = 0;
      hitc.taken = 0;
      hitc.track_key = aHit->idTruth();
      hitc.hit  = aHit;
      hitc.Id = Id;
      fSeedHits.push_back(hitc);
      //yf      if (  hit->timesUsed()) 	continue;//VP
      // convert to CA Hit
      AliHLTTPCCAGBHit caHit;
      Double_t xL =  cb*glob.x() + sb*glob.y();
      Double_t yL = -sb*glob.x() + cb*glob.y();
      Double_t zL =                  glob.z();
      caHit.SetX(   xL);
      caHit.SetY( - yL);
      caHit.SetZ( - zL);
      // caHit.SetErrX(   );
      static Float_t BTofPadWidth  =   3.45;        //! Pad Width    
      static Float_t BTofPadLength = 2*4.70;        //! Pad Length
      static Double_t sigma_y = BTofPadWidth/TMath::Sqrt(12.);
      static Double_t sigma_z = BTofPadLength/TMath::Sqrt(12.);
      caHit.SetErrY( sigma_y );
      caHit.SetErrZ( sigma_z );
      caHit.SetISlice(sector - 1 );
      caHit.SetIRow( row - 1 );
      caHit.SetID( Id );
      //	    caHit.SetID( tpcHit->id() );
      fIdTruth.push_back( aHit->idTruth() );
      fCaHits.push_back(caHit);
      
      //done loop over hits
      nBToFHit++;
    }
  }
  LOG_INFO <<"StxCAInterface::MakeHits: Loaded "<< nBToFHit << " BTof hits."<<endm;
#endif /* BTOF */  
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
