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
#include "StDetectorDbMaker/St_tpcPadGainT0C.h"
#include "StDetectorDbMaker/St_tpcAnodeHVavgC.h"
#include "StDetectorDbMaker/StDetectorDbTpcRDOMasks.h"
#include "StDetectorDbMaker/St_tpcPadPlanesC.h"


#include "TH2.h"
#include "TH3.h"
#include "TFile.h"
#include "TRandom.h"

static const Int_t nr = 17; //85
static const Double_t rmin = 40.;
static const Double_t rmax = 210;
static const Int_t nph = 12;
static const Double_t phmin = -TMath::Pi();
static const Double_t phmax = TMath::Pi();
static const Double_t PhiMax = TMath::Pi()/12;
static const Double_t ZdcMax = 2e6;

static const Double_t MINGAIN = 0.1;

ClassImp(StSpaceChargeDistMaker)
  
//_____________________________________________________________________________
StSpaceChargeDistMaker::StSpaceChargeDistMaker(const char *name):StMaker(name),
    event(0), Space3ChargePRZ(0), Space3ChargeU(0),
    Rhist(0), Phist(0), RPhist(0),
    RPPhist(0), PHhist(0), PPhist(0), ZdcC(0) {
  run = 0;
  throws = 500000; // default = 500k throws
  trigs.Set(0);
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

  TH2D *rGeom = new TH2D(*Phist);
  rGeom->SetName("rGeom");
  rGeom->Divide(Rhist);
  TH2D *phGeom = new TH2D(*PPhist);
  phGeom->SetName("phGeom");
  phGeom->Divide(PHhist);
  TH3D *rpGeom = new TH3D(*RPPhist);
  rpGeom->SetName("rpGeom");
  rpGeom->Divide(RPhist);

  TFile* ff = new TFile(Form("SCdist_%d.root",run),"RECREATE");

  Space3ChargePRZ->Write();
  Space3ChargeU->Write();
  rGeom->Write();
  phGeom->Write();
  rpGeom->Write();
  ZdcC->Write();

  ff->Close();

  delete Space3ChargePRZ;
  delete Space3ChargeU;

  return kStOk;
}
//_____________________________________________________________________________
Int_t StSpaceChargeDistMaker::InitRun(Int_t run) {
  if (Rhist->GetEntries() < 1) Pads();
  return kStOk;
}
//_____________________________________________________________________________
Int_t StSpaceChargeDistMaker::Init() {
  Space3ChargePRZ  = new TH3D("Space3ChargePRZ","Space charged versus Phi(rads), Rho and Z",
                              nph,phmin,phmax,nr,rmin,rmax,105,-210.,210.);
  Space3ChargeU    = new TH3D("Space3ChargeU"  ,"Space charged versus Phi(rads), Rho and Z (on tracks)",
                              nph,phmin,phmax,nr,rmin,rmax,105,-210.,210.);
  Space3ChargePRZ->Sumw2();
  Space3ChargeU->Sumw2();
  Rhist = new TH2D("R","r",nr,rmin,rmax,3,-1.5,1.5);
  Phist = new TH2D("P","pads",nr,rmin,rmax,3,-1.5,1.5);
  RPhist = new TH3D("RP","rp",nr,rmin,rmax,nph,phmin,phmax,3,-1.5,1.5);
  RPPhist = new TH3D("RPP","pads rp",nr,rmin,rmax,nph,phmin,phmax,3,-1.5,1.5);
  PHhist = new TH2D("PH","phi",nph,phmin,phmax,3,-1.5,1.5);
  PPhist = new TH2D("PP","pads phi",nph,phmin,phmax,3,-1.5,1.5);
  ZdcC = new TH1D("ZdcC","ZDC coincidence rate",1024,0,ZdcMax);

  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StSpaceChargeDistMaker::Make() {

  if (trigs.GetSize() < 1) {
    LOG_ERROR << "StSpaceChargeDistMaker: NO ACCEPTABLE TRIGGERS DEFINED!" << endm;
    return kStErr;
  }

  // Get StEvent and related info, determine if things are OK
  event = (StEvent*) GetInputDS("StEvent");
  if (!event) {
    LOG_WARN << "StSpaceChargeDistMaker: no StEvent; skipping event." << endm;
    return kStWarn;
  } 

  // Accept all triggers if first trig id is -1, otherwise compare the list
  if (trigs.At(0) >= 0) {
    Bool_t passTrigs = kFALSE;
    if (event->triggerIdCollection() &&
        event->triggerIdCollection()->nominal()) {
      for (int i=0; (!passTrigs) && (i<trigs.GetSize()); i++) {
        passTrigs = event->triggerIdCollection()->nominal()->isTrigger(trigs.At(i));
      }
      if (!passTrigs) { LOG_WARN << "StSpaceChargeDistMaker: Triggers not accepted" << endm; }
    } else { LOG_WARN << "StSpaceChargeDistMaker: Could not find nominal trigger id collection" << endm; }
    if (!passTrigs) return kStOK;
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
        for (int j = 0; j< numberOfPadrows; j++) {
          StTpcPadrowHitCollection *rowCollection = TpcHitCollection->sector(i)->padrow(j);
          if (rowCollection) {
            UInt_t NoHits = rowCollection->hits().size();
            for (UInt_t k = 0; k < NoHits; k++) {
              StTpcHit* tpcHit = TpcHitCollection->sector(i)->padrow(j)->hits().at(k);
              float charge = tpcHit->charge() * gainCorr[i*128 + j];
              Space3ChargePRZ->Fill(tpcHit->position().phi(),
                                    tpcHit->position().perp(),
                                    tpcHit->position().z(),
                                    charge);
              if (tpcHit->trackReferenceCount() > 0)
              Space3ChargeU->Fill(tpcHit->position().phi(),
                                    tpcHit->position().perp(),
                                    tpcHit->position().z(),
                                    charge);
            }
          }
        }
      }
    }
  }
  return kStOk;
}
//_____________________________________________________________________________
void StSpaceChargeDistMaker::Pads() {

  // Monte Carlo approach to determining acceptance
  LOG_INFO << "StSpaceChargeDistMaker: Start filling acceptance histograms..." << endm;

  Double_t Xpads[128];
  Int_t Npads[128];

  const Int_t NP = St_tpcPadPlanesC::instance()->padRows(); // # padrows (45)
  const Int_t NR = 256; // max # pads/padrow (actually 182)
  const Int_t NS = NP * NR; // > # pads/sector

  /*
  Double_t widths[2] = {1.2, 2.}; //{1.15, 1.95};
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

  Double_t XMIN[128];
  Double_t YMIN[32768]; // 128*256
  Bool_t PLIVE[1048576]; // 128*256*32
  Double_t pitch,width;
  int i,j,k,l;

  LOG_INFO << "StSpaceChargeDistMaker: Now reading live/dead/gains." << endm;
  int isec,irow,ipad;
  for (i = 0; i < 24; i++) {
    isec = i + 1;
    for (j = 0; j < NP; j++) {
      irow = j + 1; 
      pitch = St_tpcPadPlanesC::instance()->PadPitchAtRow(irow);
      if (i==0) {
        Npads[j] = St_tpcPadPlanesC::instance()->padsPerRow(irow);
        Xpads[j] = St_tpcPadPlanesC::instance()->radialDistanceAtRow(irow);
        width = St_tpcPadPlanesC::instance()->PadLengthAtRow(irow);
        XMIN[j] = Xpads[j] - 0.5*width;
      }
      bool liveRow = (StDetectorDbTpcRDOMasks::instance()->isOn(isec,
            StDetectorDbTpcRDOMasks::instance()->rdoForPadrow(irow)) &&
            St_tpcAnodeHVavgC::instance()->livePadrow(isec,irow));

      gainCorr[i*128 + j] = (liveRow ?
        TMath::Exp(irow <= 13 ? 1170*13.05e-3 : 1390*10.26e-3) /
        TMath::Exp(St_tpcAnodeHVavgC::instance()->voltagePadrow(isec,irow) *
                   (irow <= 13 ? 13.05e-3 : 10.26e-3))
        : 0);

      for (k=0; k<Npads[j]; k++) {
        ipad = k + 1;
        l = k + NR*j + NS*i;
        PLIVE[l] = (liveRow &&
             St_tpcPadGainT0C::instance()->Gain(isec,irow,ipad) > MINGAIN);
        if (i==0) YMIN[l] = pitch * (k - 0.5*Npads[j]);
      }
    }
  }

  LOG_INFO << "StSpaceChargeDistMaker: Now throwing " << throws << " hits." << endm;
  Double_t phi0,phi,r,x,y,rr,pp,pp0,z;
  Int_t ix,iy,skip;
  for (i = 1; i < throws; i++) {
   r = rmin + gRandom->Rndm()*(rmax - rmin);
   phi0 = PhiMax*(2*gRandom->Rndm() - 1);
   x = r*TMath::Cos(phi0);
   y = r*TMath::Sin(phi0);
   skip = 0;
   for (j = 0; j < 24; j++) {
    if (j < 12) {  // west end
      z = 1;
      phi = -phi0 - (j-2)*TMath::Pi()/6.0;
    } else { // east end
      z = -1;
      phi = phi0 + (j-20)*TMath::Pi()/6.0;
    }
    while (phi>=phmax) phi -= TMath::TwoPi();
    while (phi< phmin) phi += TMath::TwoPi();
    Rhist->Fill(r,0);
    RPhist->Fill(r,phi,0);
    PHhist->Fill(phi,0);
    Rhist->Fill(r,z);
    RPhist->Fill(r,phi,z);
    PHhist->Fill(phi,z);

    if (skip != 0) continue; // Skip if hit falls on a pad

    // Determine if hit falls on a pad
    if (j==0) {
     skip = 1;
     ix = (Int_t) (TMath::BinarySearch(NP,&XMIN[0],x));
     if (ix < 0 || ix >= NP) continue;
     width = St_tpcPadPlanesC::instance()->PadLengthAtRow(ix+1);
     pitch = St_tpcPadPlanesC::instance()->PadPitchAtRow(ix+1);
     if (x > XMIN[ix] + width) continue;
     iy = (Int_t) (TMath::BinarySearch(Npads[ix],&YMIN[ix*NR],y));
     if (iy < 1 || iy >= Npads[ix]-1) continue; // Don't use boundary pads
     //if (iy < 0 || iy >= Npads[ix]) continue;
     if (y > YMIN[iy + ix*NR] + pitch) continue;
     skip = 0;
     rr = TMath::Sqrt(y*y + Xpads[ix]*Xpads[ix]);
     pp0 = TMath::ATan2(y,Xpads[ix]);
    }

    // Determine if hit falls on an active pad
    // Use gains file:
    l = iy + ix*NR + j*NS;
    if (! PLIVE[l]) continue;

    // Sector 20 RDO problem: account for only 23 sectors active
    // for padrows 9-21.
    //if (j==19 && ix >=8 && ix <=20) continue;
    // Sector 5 RDO problem: account for only 23 sectors active
    // for padrow 39.
    //if (j==4 && ix == 38) continue;


    if (Debug()) {
      LOG_INFO << "r " << r << "\tphi " << phi << "\tx " << x << "\tix " << ix 
           << "\tXMIN[ix] " << XMIN[ix] << "\tRMAX " << XMIN[ix] + width;
      if (ix < 44) { LOG_INFO << "\tXMIN[ix+1] " << XMIN[ix+1]; }
      if (x <= XMIN[ix] + width) { LOG_INFO << "  o.k."; }
      LOG_INFO << endm;
    }

    pp = pp0 + (phi-phi0);
    while (pp>=phmax) pp -= TMath::TwoPi();
    while (pp< phmin) pp += TMath::TwoPi();
    Phist->Fill(rr,0);
    PPhist->Fill(pp,0);
    RPPhist->Fill(rr,pp,0);
    Phist->Fill(rr,z);
    PPhist->Fill(pp,z);
    RPPhist->Fill(rr,pp,z);
   }
  }
  LOG_INFO << "StSpaceChargeDistMaker: Done filling acceptance histograms." << endm;
}


//_____________________________________________________________________________
// $Id: StSpaceChargeDistMaker.cxx,v 1.1 2012/07/06 17:23:00 genevb Exp $
// $Log: StSpaceChargeDistMaker.cxx,v $
// Revision 1.1  2012/07/06 17:23:00  genevb
// Introduce StSpaceChargeDistMaker
//
// 
