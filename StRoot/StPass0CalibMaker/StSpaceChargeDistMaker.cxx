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
#include "StDetectorDbMaker/St_tpcAnodeHVavgC.h"
#include "StDetectorDbMaker/StDetectorDbTpcRDOMasks.h"
#include "StDetectorDbMaker/St_tpcPadPlanesC.h"
#include "StDbUtilities/StMagUtilities.h"
#include "StDbUtilities/StTpcCoordinateTransform.hh"


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
static const Double_t Phi2Max = TMath::Pi()/6;
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
  memset(Xpads,0,128*sizeof(Float_t));
  memset(Npads,0,128*sizeof(UShort_t));
  memset(XMIN,0,128*sizeof(Float_t));
  memset(YMIN,0,32768*sizeof(Float_t));
  memset(gainCorr,0,4096*sizeof(Float_t));
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
  if (Rhist->GetEntries() < 1) GeomInit();
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

  if (!gStTpcDb) {
    LOG_ERROR << "StSpaceChargeDistMaker: no gStTpcDb - should quit!" << endm;
    return kStFatal;
  }
  static StTpcCoordinateTransform transform(gStTpcDb);
  static StTpcLocalCoordinate  coorLT; // local TPC coordinate

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
              const StThreeVectorF& positionG = tpcHit->position();

              // Discard post-central-membrane and hist at gated grid opening
              if (positionG.z() * (((float) i)-11.5) > 0) continue;
              if (TMath::Abs(positionG.z()) > 180) continue;

              // Discard outermost 3 pads on any row (pad is [1..Npad])
              if (tpcHit->pad() < 4 || tpcHit->pad() > Npads[j]-3) continue;

              // Want to fill histograms in TPC local, sector-aligned coordinates
              // Need to convert global coords to TPC local coords
              StGlobalCoordinate coorG(positionG);
              transform(coorG,coorLT,i+1,j+1);
              StThreeVectorD& positionL = coorLT.position();

              float charge = tpcHit->charge() * gainCorr[i*128 + j];
              Space3ChargePRZ->Fill(positionL.phi(),
                                    positionL.perp(),
                                    positionL.z(),
                                    charge);
              if (tpcHit->trackReferenceCount() > 0)
              Space3ChargeU  ->Fill(positionL.phi(),
                                    positionL.perp(),
                                    positionL.z(),
                                    charge);
              if (throws<1 && k%((int) (1.0/throws))==0)
                GeomFill(tpcHit);
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

  NP = St_tpcPadPlanesC::instance()->padRows(); // # padrows (45)
  NR = 256; // max # pads/padrow (actually 182)
  NS = NP * NR; // > # pads/sector

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

  Float_t pitch,width;
  int i,j,k,l;

  LOG_INFO << "StSpaceChargeDistMaker: Now reading live/dead/gains for acceptance" << endm;
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
        ipad = Npads[j] - k;
        l = k + NR*j + NS*i;
        PLIVE[l] = (liveRow &&
             k>2 && k<Npads[j]-3 && // exclude outermost 3 pads on any row
             St_tpcPadGainT0BC::instance()->Gain(isec,irow,ipad) > MINGAIN);
        if (i==0) YMIN[l] = pitch * (k - 0.5*Npads[j]);
      }
    }
  }
  LOG_INFO << "StSpaceChargeDistMaker: will throw approximately 12*" << throws << " hits per TPC hit." << endm;

}
//_____________________________________________________________________________
void StSpaceChargeDistMaker::GeomFill(StTpcHit* hit) {

  // Monte Carlo approach to determining geometrical acceptance
  Float_t pitch,width;
  int j,k,l,i=1;

  // phi0 and phi3 will be in sector 3 local coordinates
  // phi and phi2 are in TPC local coordinates
  Float_t phi0,phi,phi2,phi3,r,r2,x,y,z,zbin;
  Float_t pos1[3];
  Float_t pos2[3];
  Int_t ix,iy,isec;

  z = hit->position().z();
  isec = hit->sector(); // 1..24
  // already required z as correct side for sector before GeomFill()

  // must use StEvtTrigDetSumsMaker if reading from event.root
  StMagUtilities* mExB = StMagUtilities::Instance();

  // for r^-1.71 in r:
  static const Double_t rdist0 = -1.71 + 1.0;
  // static const Double_t rdist1 = TMath::Power(rmin,rdist0);
  // save some CPU by starting at r=49 since acceptance is zero below that...
  static const Double_t rdist1 = TMath::Power(49,rdist0);
  static const Double_t rdist2 = TMath::Power(rmax,rdist0) - rdist1;
  static const Double_t rdist3 = 1.0/rdist0;

  while (i < 12*TMath::Max(1,(int) throws)) {
    // flat in r:
    // r = rmin + gRandom->Rndm()*(rmax - rmin);

    // r^-1.71 in r:
    r = TMath::Power(rdist1 + rdist2*gRandom->Rndm(),rdist3);
    
    phi0 = PhiMax*(2*gRandom->Rndm() - 1); //sector 3 coordinates
    j = (int) (12*gRandom->Rndm());
    if (j>=12) j-=12;
    if (isec < 13) {  // west end
      zbin = 1;
      k = j; // k = [0..11]
      phi = phi0 - (k-2)*TMath::Pi()/6.0;
    } else { // east end
      zbin = -1;
      k = j + 12; // k = [12..23]
      phi = -phi0 + (k-20)*TMath::Pi()/6.0;
    }
    while (phi>=phmax) phi -= TMath::TwoPi();
    while (phi< phmin) phi += TMath::TwoPi();
    Rhist->Fill(r,0);
    RPhist->Fill(r,phi,0);
    PHhist->Fill(phi,0);
    Rhist->Fill(r,zbin);
    RPhist->Fill(r,phi,zbin);
    PHhist->Fill(phi,zbin);

    // Apply distortions in TPC local
    pos1[0] = r*TMath::Cos(phi);
    pos1[1] = r*TMath::Sin(phi);
    pos1[2] = z;
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
    width = St_tpcPadPlanesC::instance()->PadLengthAtRow(ix+1);
    pitch = St_tpcPadPlanesC::instance()->PadPitchAtRow(ix+1);
    if (x > XMIN[ix] + width) continue;
    iy = (Int_t) (TMath::BinarySearch(Npads[ix],&YMIN[ix*NR],y));
    if (y > YMIN[iy + ix*NR] + pitch) continue;

    // Determine if hit falls on an active pad
    // Use gains file:
    l = iy + ix*NR + k*NS;
    if (! PLIVE[l]) continue;

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


    if (Debug()) {
      LOG_INFO << "r " << r << "\tphi " << phi << "\tx " << x << "\tix " << ix 
           << "\tXMIN[ix] " << XMIN[ix] << "\tRMAX " << XMIN[ix] + width;
      if (ix < 44) { LOG_INFO << "\tXMIN[ix+1] " << XMIN[ix+1]; }
      if (x <= XMIN[ix] + width) { LOG_INFO << "  o.k."; }
      LOG_INFO << endm;
    }

    Phist->Fill(r,0);
    PPhist->Fill(phi,0);
    RPPhist->Fill(r,phi,0);
    Phist->Fill(r,zbin);
    PPhist->Fill(phi,zbin);
    RPPhist->Fill(r,phi,zbin);
    i++;
  } // i
}


//_____________________________________________________________________________
// $Id: StSpaceChargeDistMaker.cxx,v 1.3 2012/10/15 17:51:12 genevb Exp $
// $Log: StSpaceChargeDistMaker.cxx,v $
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
