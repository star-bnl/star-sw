//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StSpaceChargeDistMaker looks at the distribution of charge           //
// in the TPC                                                           //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "StSpaceChargeDistMaker.h"
#include "StMessMgr.h"
#include "StEvent.h"
#include "StEventTypes.h"
#include "StDetectorDbMaker/St_tpcPadGainT0BC.h"
#include "StDetectorDbMaker/St_tss_tssparC.h"
#include "StDetectorDbMaker/StDetectorDbTpcRDOMasks.h"
#include "StDetectorDbMaker/St_tpcPadConfigC.h"
#include "StDetectorDbMaker/St_TpcSecRowBC.h"
#include "StDetectorDbMaker/St_TpcAvgCurrentC.h"
#include "StDbUtilities/StMagUtilities.h"
#include "StDbUtilities/StTpcCoordinateTransform.hh"
#include "StdEdxY2Maker/StTpcdEdxCorrection.h"


#include "TH2.h"
#include "TH3.h"
#include "TFile.h"
#include "TRandom.h"
#include "TMatrixD.h"
#include "TVectorD.h"

static const Int_t nz = 105;
static const Double_t zmin = -205;
static const Double_t zmax =  205;
static const Int_t nr = 17; //85
static const Double_t rmin = 40.;
static const Double_t rmax = 210;
static const Int_t nph = 12;
static const Double_t phmin = -TMath::Pi();
static const Double_t phmax = TMath::Pi();
static const Double_t PhiMax = TMath::Pi()/12;
static const Int_t nrph = nr*nph;
static const Double_t ZdcMax = 2e6;

static const Double_t MINGAIN = 0.1;

static TMatrixD* RPMats[nz];
static TMatrixD* RMats[nz];
static TVectorD* RPMatHs[nz];
static TVectorD* RMatHs[nz];

ClassImp(StSpaceChargeDistMaker)
  
//_____________________________________________________________________________
StSpaceChargeDistMaker::StSpaceChargeDistMaker(const char *name):StMaker(name),
    event(0), Space3ChargePRZ(0), Space3ChargeU(0),
    thrownR(0), acceptedR(0),
    thrownRP(0), acceptedRP(0),
    thrownP(0), acceptedP(0),
    ZdcC(0) {
  run = 0;
  GGZ = 0; NP = 0; NR = 0; NS = 0;
  throws = 500000; // default = 500k throws
  trigs.Set(0);
  memset(Xpads,0,128*sizeof(Float_t));
  memset(Npads,0,128*sizeof(UShort_t));
  memset(XMIN,0,128*sizeof(Float_t));
  memset(YMIN,0,32768*sizeof(Float_t));
  memset(LiveRow,0,4096*sizeof(Bool_t));
  memset(LivePad,0,1048576*sizeof(Bool_t));
}
//_____________________________________________________________________________
StSpaceChargeDistMaker::~StSpaceChargeDistMaker() {
}
//_____________________________________________________________________________
void StSpaceChargeDistMaker::AcceptTrigger(Int_t trig) {
  Int_t ntrig = trigs.GetSize();
  if (trig<0) {
    LOG_INFO << "StSpaceChargeDistMaker: Accepting all triggers." << endm;
  } else {
    LOG_INFO << "StSpaceChargeDistMaker: Accepting trigger (" << ntrig << ") : " << trig << endm;
  }
  trigs.Set(ntrig+1);
  trigs.AddAt(trig,ntrig);
}
//_____________________________________________________________________________
Int_t StSpaceChargeDistMaker::Finish() {

  TH2D *rGeom = new TH2D(*acceptedR);
  rGeom->SetName("rGeom");
  rGeom->Divide(thrownR);
  TH2D *phGeom = new TH2D(*acceptedP);
  phGeom->SetName("phGeom");
  phGeom->Divide(thrownP);
  TH3D *rpGeom = new TH3D(*acceptedRP);
  rpGeom->SetName("rpGeom");
  rpGeom->Divide(thrownRP);

  TH3D* S3CPRZ = new TH3D(*Space3ChargePRZ);
  S3CPRZ->SetName(Form("%sOrig",Space3ChargePRZ->GetName()));
  TH3D* S3CU = new TH3D(*Space3ChargeU);
  S3CU->SetName(Form("%sOrig",Space3ChargeU->GetName()));

  TFile* ff = new TFile(Form("SCdist_%d.root",run),"RECREATE");
  S3CPRZ->Write();
  S3CU->Write();
  rGeom->Write();
  phGeom->Write();
  rpGeom->Write();
  ZdcC->Write();

  // De-smear the real data if possible
  // Start with matrix of found (rows) and generated (columns),
  //   invert, then apply to found data
  LOG_INFO << "StSpaceChargeDistMaker: attempting to de-smear..." << endm;
  for (Int_t zbin=1; zbin<=nz; zbin++) { // do the matrices for each z bin one at a time 

    Int_t bingen,binfnd,desmearing_mode = -1;
    Double_t denom,Det;

    Det = 1.0;
    TMatrixD& MatM = *(RPMats[zbin-1]);
    TVectorD& MatH = *(RPMatHs[zbin-1]);
    for (bingen=0; bingen<nrph; bingen++) {
      denom = MatH[bingen];
      for (binfnd=0; binfnd<nrph; binfnd++) {
        if (denom > 0) MatM[binfnd][bingen] /= denom;
        else if (binfnd==bingen) MatM[binfnd][bingen] = 1.0; // diagonals should be ~1
      }
    }
    if (MatH.Sum()) {
      MatM.Invert(&Det);
      MatM.Write(Form("RPMat%03d",zbin));
      MatH.Write(Form("RPMatH%03d",zbin));
    }

    if (Det) {
      desmearing_mode = 2; // r-phi
    } else {
      Det = 1.0;
      MatM = *(RMats[zbin-1]);
      MatH = *(RMatHs[zbin-1]);
      for (bingen=0; bingen<nr; bingen++) {
        denom = MatH[bingen];
        for (binfnd=0; binfnd<nr; binfnd++) {
          if (denom > 0) MatM[binfnd][bingen] /= denom;
          else if (binfnd==bingen) MatM[binfnd][bingen] = 1.0;
        }
      }
      if (MatH.Sum()) {
        MatM.Invert(&Det);
        MatM.Write(Form("RMat%03d",zbin));
        MatH.Write(Form("RMatH%03d",zbin));
      }

      if (Det) {
        LOG_INFO << "StSpaceChargeDistMaker: z bin " << zbin
                 << " will use r matrices instead of r-phi" << endm;
        desmearing_mode = 1; // r
      } else {
        LOG_WARN << "StSpaceChargeDistMaker: z bin " << zbin
                 << " could not invert matrices, giving up!" << endm;
        desmearing_mode = 0; // cannot do de-smearing
      }
    }


    if (desmearing_mode > 0) {
      Double_t newcontPRZ, newcontU, coef;
      for (Int_t rbin=1; rbin<=nr; rbin++) {
        for (Int_t phibin=1; phibin<=nph; phibin++) {
          bingen = (rbin-1) + (desmearing_mode == 2 ? nr*(phibin-1) : 0);
          newcontPRZ = 0;
          newcontU   = 0;
          for (Int_t rbin2=1; rbin2<=nr; rbin2++) {
            for (Int_t phibin2=1; phibin2<=nph; phibin2++) {
              if (desmearing_mode == 1 && phibin2 != phibin) continue;
              binfnd = (rbin2-1) + (desmearing_mode == 2 ? nr*(phibin2-1) : 0);
              coef = MatM[bingen][binfnd];
              if (TMath::IsNaN(coef)) {
                LOG_ERROR << "StSpaceChargeDistMaker: de-smearing matrix element ["
                  << bingen << "][" << binfnd << "] (zbin=" << zbin << ") is NaN !"
                  << endm;
              } else {
                newcontPRZ += coef * S3CPRZ->GetBinContent(phibin2,rbin2,zbin);
                newcontU   += coef * S3CU  ->GetBinContent(phibin2,rbin2,zbin);
              }
            }
          }
          Space3ChargePRZ->SetBinContent(phibin,rbin,zbin,newcontPRZ);
          Space3ChargeU  ->SetBinContent(phibin,rbin,zbin,newcontU  );
        }
      }
    }

  } // z bins


  Space3ChargePRZ->Write();
  Space3ChargeU->Write();

  ff->Close();

  delete Space3ChargePRZ;
  delete Space3ChargeU;

  return kStOk;
}
//_____________________________________________________________________________
Int_t StSpaceChargeDistMaker::InitRun(Int_t run) {
  if (thrownR->GetEntries() < 1) GeomInit();
  return kStOk;
}
//_____________________________________________________________________________
Int_t StSpaceChargeDistMaker::Init() {
  Space3ChargePRZ  = new TH3D("Space3ChargePRZ","Space charged versus Phi(rads), Rho and Z",
                              nph,phmin,phmax,nr,rmin,rmax,nz,zmin,zmax);
  Space3ChargeU    = new TH3D("Space3ChargeU"  ,"Space charged versus Phi(rads), Rho and Z (on tracks)",
                              nph,phmin,phmax,nr,rmin,rmax,nz,zmin,zmax);
  Space3ChargePRZ->Sumw2();
  Space3ChargeU->Sumw2();
  thrownR = new TH2D("R","r",nr,rmin,rmax,3,-1.5,1.5);
  acceptedR = new TH2D("P","pads",nr,rmin,rmax,3,-1.5,1.5);
  thrownRP = new TH3D("RP","rp",nr,rmin,rmax,nph,phmin,phmax,3,-1.5,1.5);
  acceptedRP = new TH3D("RPP","pads rp",nr,rmin,rmax,nph,phmin,phmax,3,-1.5,1.5);
  thrownP = new TH2D("PH","phi",nph,phmin,phmax,3,-1.5,1.5);
  acceptedP = new TH2D("PP","pads phi",nph,phmin,phmax,3,-1.5,1.5);
  ZdcC = new TH1D("ZdcC","ZDC coincidence rate",1024,0,ZdcMax);

  for (int i=0;i<nz;i++) {
    RPMats[i] = new TMatrixD(nrph,nrph);
    RPMatHs[i] = new TVectorD(nrph);
    RMats[i] = new TMatrixD(nr,nr);
    RMatHs[i] = new TVectorD(nr);
    RPMats[i]->SetTol(1e-6);
    RMats[i]->SetTol(1e-6);
  }

  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StSpaceChargeDistMaker::Make() {

  if (trigs.GetSize() < 1) {
    LOG_ERROR << "StSpaceChargeDistMaker: NO ACCEPTABLE TRIGGERS DEFINED!" << endm;
    return kStErr;
  }

  // Get StEvent and related info, determine if things are OK
  event = static_cast<StEvent*>(GetInputDS("StEvent"));
  if (!event) {
    LOG_WARN << "StSpaceChargeDistMaker: no StEvent; skipping event." << endm;
    return kStWarn;
  } 

  // Accept all triggers if first trig id is -1, otherwise compare the list
  if (trigs.At(0) >= 0) {
    Bool_t passTrigs = kFALSE;
    if (event->triggerIdCollection() &&
        event->triggerIdCollection()->nominal()) {
      for (Int_t i=0; (!passTrigs) && (i<trigs.GetSize()); i++) {
        passTrigs = event->triggerIdCollection()->nominal()->isTrigger(trigs.At(i));
      }
      if (!passTrigs) { LOG_WARN << "StSpaceChargeDistMaker: Triggers not accepted" << endm; }
    } else { LOG_WARN << "StSpaceChargeDistMaker: Could not find nominal trigger id collection" << endm; }
    if (!passTrigs) return kStOK;
  }

  if (!gStTpcDb) {
    LOG_ERROR << "StSpaceChargeDistMaker: no gStTpcDb - should quit!" << endm;
    return kStFatal;
  }
  static StTpcCoordinateTransform transform(gStTpcDb);
  static StTpcLocalCoordinate  coorLT; // local TPC coordinate
  //static StTpcLocalSectorCoordinate coorLS; // local sector coordinate

  static dEdxY2_t CdEdx;
  static Int_t number_dedx_faults = 0;
  static Int_t warn_dedx_faults = 1;
  static StTpcdEdxCorrection* m_TpcdEdxCorrection = 0;
  if (!m_TpcdEdxCorrection) {
   Int_t Mask = -1; // 22 bits
   // Don't know dX, so exclude this correction
   CLRBIT(Mask,StTpcdEdxCorrection::kdXCorrection);
   m_TpcdEdxCorrection = new StTpcdEdxCorrection(Mask, Debug());
  }
  if (!m_TpcdEdxCorrection) {
    LOG_ERROR << "StSpaceChargeDistMaker: no dE/dx corrections - should quit!" << endm;
    return kStFatal;
  }
  

  StTpcHitCollection* TpcHitCollection = event->tpcHitCollection();
  if (TpcHitCollection) {
    Double_t zdcc = event->runInfo()->zdcCoincidenceRate();
    ZdcC->Fill(zdcc);
    if (zdcc>ZdcMax) {
      LOG_WARN << "StSpaceChargeDistMaker: ZDC rate greater than hist max ( "
               << zdcc << " > " << ZdcMax << " )" << endm;
    }
    if (!run) run = event->runId();

    UInt_t numberOfSectors = TpcHitCollection->numberOfSectors();
    for (UInt_t i = 0; i< numberOfSectors; i++) {
      StTpcSectorHitCollection* sectorCollection = TpcHitCollection->sector(i);
      if (sectorCollection) {
        Int_t numberOfPadrows = sectorCollection->numberOfPadrows();
        for (Int_t j = 0; j< numberOfPadrows; j++) {
          if (! LiveRow[j + NP*i]) continue;
          StTpcPadrowHitCollection *rowCollection = TpcHitCollection->sector(i)->padrow(j);
          if (rowCollection) {
            UInt_t NoHits = rowCollection->hits().size();
            for (UInt_t k = 0; k < NoHits; k++) {
              StTpcHit* tpcHit = TpcHitCollection->sector(i)->padrow(j)->hits().at(k);
              const StThreeVectorF& positionG = tpcHit->position();

              // Discard hits near endcaps
              if (TMath::Abs(positionG.z()) > 200) continue;

              // Discard outermost 4 pads on any row (pad is [1..Npad])
              if (tpcHit->pad() < 5 || tpcHit->pad() > Npads[j]-4) continue;

              // Problematic or false hits?
              Float_t charge = tpcHit->charge();
              if (charge <= 0) continue;

              // Want to fill histograms in TPC local, sector-aligned coordinates
              // Need to convert global coords to TPC local coords
              StGlobalCoordinate coorG(positionG);
              transform(coorG,coorLT,i+1,j+1);
              StThreeVectorD& positionL = coorLT.position();
              //transform(coorLT,coorLS);

              // Discard post-central-membrane and hits at gated grid opening
              if (positionL.z() * (((Float_t) i)-11.5) > 0) continue;
              if (TMath::Abs(positionL.z()) > 180) continue;

              // dE/dx corrections to charge copied from StTpcRSMaker
              memset(&CdEdx, 0, sizeof(dEdxY2_t));
              CdEdx.DeltaZ = 5.2;
              CdEdx.QRatio = -2;
              CdEdx.QRatioA= -2.;
              CdEdx.QSumA  = 0;
              CdEdx.sector = i+1;
              CdEdx.row    = j+1;
              Double_t Qcm = St_TpcAvgCurrentC::instance()->AcChargeRowL(CdEdx.sector,CdEdx.row); // C/cm
              CdEdx.pad    = tpcHit->pad();
              CdEdx.edge   = CdEdx.pad;
              if (CdEdx.edge > 0.5*Npads[j])
                CdEdx.edge += 1 - Npads[j];
              CdEdx.F.dE     = charge;
              CdEdx.adc    = tpcHit->adc();
              CdEdx.F.dx     = XWID[j];
              CdEdx.xyz[0] = positionL.x();
              CdEdx.xyz[1] = positionL.y();
              CdEdx.xyz[2] = positionL.z();
              Double_t probablePad = ((Double_t) Npads[j])/2.;
              Double_t pitch = St_tpcPadConfigC::instance()->PadPitchAtRow(CdEdx.sector,CdEdx.row);
              Double_t PhiMax = TMath::ATan2(probablePad*pitch, Xpads[j]);
              CdEdx.PhiR   = TMath::ATan2(CdEdx.xyz[0],CdEdx.xyz[1])/PhiMax;
              //CdEdx.xyzD[] left as 0 as these hits are not necessarily on tracks
              CdEdx.zG     = CdEdx.xyz[2];
              CdEdx.Qcm    = 1e6*Qcm; // uC/cm
              CdEdx.Crow   = St_TpcAvgCurrentC::instance()->AvCurrRow(CdEdx.sector,CdEdx.row);
              CdEdx.Zdc    = zdcc;
              //CdEdx.ZdriftDistance = coorLS.position().z(); // drift length
              // random drift length (may be wrong by as much as +/-full drift length)
              // is worse than fixed at half full drift length (wrong by as much as
              // +/-half full drift length, RMS of error down by sqrt(2))
              CdEdx.ZdriftDistance = 104.3535;
              St_tpcGas *tpcGas = m_TpcdEdxCorrection->tpcGas();
              if (tpcGas)
                CdEdx.ZdriftDistanceO2 = CdEdx.ZdriftDistance*(*tpcGas)[0].ppmOxygenIn;
              Int_t dedx_status = m_TpcdEdxCorrection->dEdxCorrection(CdEdx);
              if (dedx_status) {
                number_dedx_faults++;
                if (number_dedx_faults == warn_dedx_faults) {
                  LOG_WARN << "StSpaceChargeDistMaker: found at least " <<
                    number_dedx_faults << " dE/dx fault(s), code: " << dedx_status << endm;
                  warn_dedx_faults *= 10;
                }
                if (Debug()) {
                  LOG_WARN << Form("FAULT: %d %d %d %g %d %g %g %g %g %g\n",dedx_status,
                    i+1,j+1,tpcHit->pad(),Npads[j],
                    positionL.x(),positionL.y(),positionL.z(),
                    charge,CdEdx.F.dE) << endm;
                }
                continue;
              }
              charge = CdEdx.F.dE;

              Space3ChargePRZ->Fill(positionL.phi(),
                                    positionL.perp(),
                                    positionL.z(),
                                    charge);
              if (tpcHit->trackReferenceCount() > 0)
              Space3ChargeU  ->Fill(positionL.phi(),
                                    positionL.perp(),
                                    positionL.z(),
                                    charge);
              for (Double_t throwi = throws; throwi>0; throwi--)
                if (throwi>=1 || ((Int_t) (gRandom->Rndm()/throwi))==0)
                  GeomFill(positionL.z());
            }
          }
        }
      }
    }
  }
  return kStOk;
}
//_____________________________________________________________________________
void StSpaceChargeDistMaker::GeomInit() {

  // Calculated in sector 3 coordinates

  GGZ = St_tpcDimensionsC::instance()->gatingGridZ();
  NP = St_tpcPadConfigC::instance()->padRows(20); // # padrows (45)
  NR = 256; // max # pads/padrow (actually 182)
  NS = NP * NR; // > # pads/sector

  /*
  Double_t XWID[2] = {1.2, 2.}; //{1.15, 1.95};
  Double_t pitches[2] = {0.335, 0.67};
  Double_t Xpads[NP] = {
    60.0, 64.8, 69.6, 74.4, 79.2, 84.0, 88.8, 93.6, 98.8, 104.0,109.2,114.4,119.6, // inner Centres
    127.195, 129.195, 131.195, 133.195, 135.195, //Outer 
    137.195, 139.195, 141.195, 143.195, 145.195, 
    147.195, 149.195, 151.195, 153.195, 155.195, 
    157.195, 159.195, 161.195, 163.195, 165.195, 
    167.195, 169.195, 171.195, 173.195, 175.195, 
    177.195, 179.195, 181.195, 183.195, 185.195, 
    187.195, 189.195};
  Int_t Npads[NP] = {
    88,96,104,112,118,126,134,142,150,158,166,174,182, // inner Centres
    98,100,102,104,106,106,108,110,112,112,114,116,118,120,122,122, //Outer 
    124,126,128,128,130,132,134,136,138,138,140,142,144,144,144,144};
  */

  Float_t pitch;
  Int_t i,j,k,l,m,isec,irow,ipad;

  LOG_INFO << "StSpaceChargeDistMaker: Now reading live/dead/gains for acceptance" << endm;
  for (i = 0; i < 24; i++) {
    isec = i + 1;
    Float_t* gainScales = St_TpcSecRowBC::instance()->GainScale(i);
    for (j = 0; j < NP; j++) {
      irow = j + 1; 
      pitch = St_tpcPadConfigC::instance()->PadPitchAtRow(20,irow);
      if (i==0) {
        Npads[j] = St_tpcPadConfigC::instance()->padsPerRow(20,irow);
        Xpads[j] = St_tpcPadConfigC::instance()->radialDistanceAtRow(20,irow);
        XWID[j] = St_tpcPadConfigC::instance()->PadLengthAtRow(20,irow);
        XMIN[j] = Xpads[j] - 0.5*XWID[j];
      }
      m = j + NP*i;
      LiveRow[m] = (StDetectorDbTpcRDOMasks::instance()->isOn(isec,
       StDetectorDbTpcRDOMasks::instance()->rdoForPadrow(isec,irow)) &&
            (St_tss_tssparC::instance()->gain(isec,irow) > 0) &&
            gainScales[j]>0); // gainScales necessary for dE/dx

      for (k=0; k<Npads[j]; k++) {
        ipad = Npads[j] - k;
        l = k + NR*j + NS*i;
        LivePad[l] = (LiveRow[m] &&
             k>3 && k<Npads[j]-4 && // exclude outermost 4 pads on any row
             St_tpcPadGainT0BC::instance()->Gain(isec,irow,ipad) > MINGAIN);
        if (i==0) YMIN[l] = pitch * (k - 0.5*Npads[j]);
      }
    }
  }
  LOG_INFO << "StSpaceChargeDistMaker: will throw approximately 12*" << throws << " hits per TPC hit." << endm;

}
//_____________________________________________________________________________
void StSpaceChargeDistMaker::GeomFill(Float_t z) {

  // Monte Carlo approach to determining geometrical acceptance
  Float_t pitch;
  Int_t j,k,l,i=1;

  // phi0 and phi3 will be in sector 3 local coordinates
  // phi and phi2 are in TPC local coordinates
  // already required z as correct side for sector before GeomFill()
  Float_t phi0,phi,phi2,phi3,phi4,r,r2,r4,x,y,zbin,zpileup;
  Float_t pos1[3];
  Float_t pos2[3];
  Float_t pos4[3];
  Int_t ix,iy;
  Int_t rbin,phibin,tempbin,bingen,binfnd,bingenR,binfndR;

  // must use StEvtTrigDetSumsMaker if reading from event.root
  StMagUtilities* mExB = StMagUtilities::Instance();

  // for r^-1.71 in r:
  static const Double_t rdist0 = -1.71 + 1.0;
  // static const Double_t rdist1 = TMath::Power(rmin,rdist0);
  // save some CPU by starting at r=49 since acceptance is zero below that...
  static const Double_t rdist1 = TMath::Power(49,rdist0);
  static const Double_t rdist2 = TMath::Power(rmax,rdist0) - rdist1;
  static const Double_t rdist3 = 1.0/rdist0;

  while (i < 12*TMath::Max(1,(Int_t) throws)) {
    // flat in r:
    // r = rmin + gRandom->Rndm()*(rmax - rmin);

    // r^-1.71 in r:
    r = TMath::Power(rdist1 + rdist2*gRandom->Rndm(),rdist3);
    
    phi0 = PhiMax*(2*gRandom->Rndm() - 1); //sector 3 coordinates
    j = (Int_t) (12*gRandom->Rndm());
    zpileup = GGZ*gRandom->Rndm();
    if (j>=12) j-=12;
    if (z > 0) {  // west end
      zbin = 1;
      k = j; // k = [0..11]
      phi = phi0 - (k-2)*TMath::Pi()/6.0;
    } else { // east end
      zbin = -1;
      k = j + 12; // k = [12..23]
      phi = -phi0 + (k-20)*TMath::Pi()/6.0;
      zpileup *= -1.0;
    }
    while (phi>=phmax) phi -= TMath::TwoPi();
    while (phi< phmin) phi += TMath::TwoPi();
    thrownR->Fill(r,0);
    thrownRP->Fill(r,phi,0);
    thrownP->Fill(phi,0);
    thrownR->Fill(r,zbin);
    thrownRP->Fill(r,phi,zbin);
    thrownP->Fill(phi,zbin);

    // Apply distortions in TPC local
    pos1[0] = r*TMath::Cos(phi);
    pos1[1] = r*TMath::Sin(phi);
    pos1[2] = zpileup;
    mExB->DoDistortion(pos1,pos2,k+1);
    phi2 = TMath::ATan2(pos2[1],pos2[0]);
    r2 = TMath::Sqrt(pos2[0]*pos2[0]+pos2[1]*pos2[1]);

    // Ignoring misalignment of TPC
    // but transforming back to sector 3
    phi3 = phi0 + ((phi2-phi)*(pos2[2] < 0 ? -1.0 : 1.0));
    x = r2*TMath::Cos(phi3);
    y = r2*TMath::Sin(phi3);

    // Determine if hit falls on a pad
    ix = (Int_t) (TMath::BinarySearch(NP,&XMIN[0],x));
    if (ix < 0 || ix >= NP) continue;
    pitch = St_tpcPadConfigC::instance()->PadPitchAtRow(20,ix+1);
    if (x > XMIN[ix] + XWID[ix]) continue;
    iy = (Int_t) (TMath::BinarySearch(Npads[ix],&YMIN[ix*NR],y));
    if (y > YMIN[iy + ix*NR] + pitch) continue;

    // Determine if hit falls on an active pad
    // Use gains file:
    l = iy + ix*NR + k*NS;
    if (! LivePad[l]) continue;

    // The need to exclude channels like the following should no
    // longer be necessary, but I am leaving it commented here
    // as an example for possible testing purposes. -GVB
    //
    // Sector 20 RDO problem: account for only 23 sectors active
    // for padrows 9-21.
    //if (k==19 && ix >=8 && ix <=20) continue;
    // Sector 5 RDO problem: account for only 23 sectors active
    // for padrow 39.
    //if (k==4 && ix == 38) continue;

    // ACCEPTED!

    if (Debug()) {
      LOG_INFO << "r " << r << "\tphi " << phi << "\tx " << x << "\tix " << ix 
           << "\tXMIN[ix] " << XMIN[ix] << "\tRMAX " << XMIN[ix] + XWID[ix];
      if (ix < 44) { LOG_INFO << "\tXMIN[ix+1] " << XMIN[ix+1]; }
      if (x <= XMIN[ix] + XWID[ix]) { LOG_INFO << "  o.k."; }
      LOG_INFO << endm;
    }

    pos2[2] = z; // generated at zpileup, but found at z
    mExB->UndoDistortion(pos2,pos4,k+1);
    phi4 = TMath::ATan2(pos4[1],pos4[0]);
    r4 = TMath::Sqrt(pos4[0]*pos4[0]+pos4[1]*pos4[1]);
    // generated at (r,phi), but found at (r4,phi4)
    acceptedRP->GetBinXYZ(acceptedRP->FindBin(r ,phi ,0),rbin,phibin,tempbin);
    bingen = (rbin-1) + nr*(phibin-1);
    bingenR = rbin-1;
    acceptedRP->GetBinXYZ(acceptedRP->FindBin(r4,phi4,0),rbin,phibin,tempbin);
    binfnd = (rbin-1) + nr*(phibin-1);
    binfndR = rbin-1;
    int zbin4 = Space3ChargePRZ->GetZaxis()->FindBin(z) - 1;
    (*(RPMats[zbin4]))[binfnd][bingen]++;
    (*(RPMatHs[zbin4]))[bingen]++;
    (*(RMats[zbin4]))[binfndR][bingenR]++;
    (*(RMatHs[zbin4]))[bingenR]++;

    acceptedR->Fill(r,0);
    acceptedP->Fill(phi,0);
    acceptedRP->Fill(r,phi,0);
    acceptedR->Fill(r,zbin);
    acceptedP->Fill(phi,zbin);
    acceptedRP->Fill(r,phi,zbin);
    i++;
  } // i
}


//_____________________________________________________________________________
// $Id: StSpaceChargeDistMaker.cxx,v 1.11 2018/10/17 20:45:27 fisyak Exp $
// $Log: StSpaceChargeDistMaker.cxx,v $
// Revision 1.11  2018/10/17 20:45:27  fisyak
// Restore update for Run XVIII dE/dx calibration removed by Gene on 08/07/2018
//
// Revision 1.10  2018/04/11 02:43:21  smirnovd
// Enable TPC/iTPC switch via St_tpcPadConfig
//
// This is accomplished by substituting St_tpcPadPlanes with St_tpcPadConfig.
// A sector ID is passed to St_tpcPadConfig in order to extract parameters for
// either TPC or iTPC
//
// Revision 1.9  2017/02/14 23:38:38  fisyak
// Adjustment to structure changes in StTpcdEdxCorrection
//
// Revision 1.8  2016/05/11 20:44:52  genevb
// Initialize vars, cast to double for division
//
// Revision 1.7  2015/12/18 17:31:40  genevb
// Updated copy of TpcRS code for dE/dx
//
// Revision 1.6  2015/05/19 19:36:09  genevb
// Code cleanup in preparation for C++11
//
// Revision 1.5  2012/11/28 02:08:52  genevb
// Remove de-smearing bias in z and treat z more differentially
//
// Revision 1.4  2012/11/13 22:05:19  genevb
// Use TPC dE/dx correction code, and introduce de-smearing
//
// Revision 1.3  2012/10/15 17:51:12  genevb
// Include distortion corrections, which must be evenly sampled per event (per hit)
//
// Revision 1.2  2012/09/13 20:58:56  fisyak
// Corrections for iTpx
//
// Revision 1.1  2012/07/06 17:23:00  genevb
// Introduce StSpaceChargeDistMaker
//
// 
