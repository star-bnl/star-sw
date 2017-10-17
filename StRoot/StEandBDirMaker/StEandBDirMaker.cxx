// $Id: StEandBDirMaker.cxx,v 1.10 2014/03/19 21:04:16 fisyak Exp $
// $Log: StEandBDirMaker.cxx,v $
// Revision 1.10  2014/03/19 21:04:16  fisyak
// Switch to 2D mag. field
//
// Revision 1.9  2014/01/07 15:00:56  fisyak
// Freeze
//
// Revision 1.8  2013/09/25 22:33:14  fisyak
// More debugging
//
// Revision 1.7  2013/08/29 23:11:20  fisyak
// Back to distrotion, simplify fit
//
// Revision 1.5  2013/08/29 15:45:47  fisyak
// Add Debug print out
//
// Revision 1.4  2013/08/26 17:01:35  fisyak
// Add
//
// Revision 1.3  2013/08/26 16:51:58  fisyak
// Add rms
//
// Revision 1.2  2013/08/19 20:25:30  fisyak
// Add dirST
//
// Revision 1.1.1.1  2013/08/16 15:27:45  fisyak
// First version
//
#include <assert.h>
#include "StEandBDirMaker.h"
	       // StDb
#include "StDbUtilities/StTpcCoordinateTransform.hh"
#include "StDbUtilities/StCoordinates.hh" 
#include "StDbUtilities/StTpcLocalSectorCoordinate.hh"
#include "RTS/src/DAQ_TPX/tpxFCF_flags.h" // for FCF flag definition
#include "StarMagField.h"
#include "StTpcDb/StTpcDb.h"
	       // StarClassLibrary
#include "SystemOfUnits.h"
	       // StEvent 
#include "StEventTypes.h"
	       // StarRoot
#include "TRVector.h"
#include "TRMatrix.h"
	       // ROOT
#include "Riostream.h"
#include "TSpectrum2.h"
#include "TCanvas.h"
#include "TFile.h"
#if ROOT_VERSION_CODE < 331013
#include "TCL.h"
#else
#include "TCernLib.h"
#endif
#include "StMessMgr.h" 
#include "TH2.h"
#include "TTree.h"
#define __DEBUG__
#ifdef __DEBUG__
#define DEBUG_LEVEL if (Debug()%10 > 1)
#define PrPP(A,B)  DEBUG_LEVEL {LOG_INFO << "StEandBDirMaker::" << (#A) << "\t" << (#B) << " = \t" << (B) << endm;}
#else
#define PrPP(A,B)
#endif
ClassImp(Tracklet);
ClassImp(StEandBDirMaker)
static TTree* TrackletTree = 0;
static TH2D*  secXY = 0, *secXZ = 0, *secYZ = 0;
static TCanvas *c2 = 0;
static TSpectrum2 *spectr = 0;
static const Int_t maxpeaks = 5;
//________________________________________________________________________________
Bool_t Ask() {
  static Bool_t fAsk = kTRUE;
  char symbol;
  if (fAsk){
    std::cout << "ask (enter - next, r - don't ask, q - quit) >";
    do{
      std::cin.get(symbol);
      if (symbol == 'r') {
        fAsk = kFALSE;
      } else if (symbol == 'q') return kTRUE;
    } while (symbol != '\n');
    std::cout << std::endl;
  }
  return kFALSE;
}
//_____________________________________________________________________________
/// Init - is a first method the top level StChain calls to initialize all its makers 
Int_t StEandBDirMaker::Init(){
  TFile *f = GetTFile();
  if (! f) {
    gMessMgr->Warning() << "StEandBDirMaker::Init root file has not been found" << endm;
    assert(f);
  }
  gMessMgr->Warning() << "StEandBDirMaker::Init found " << f->GetName() << " Create TrackletTree" << endm;
  f->cd();
  TrackletTree = new TTree("TrackletTree","the TPC residuals between Inner and Outer sub sectors");
  TrackletTree->SetAutoSave(100000000);
  Int_t bufsize = 64000;
  Int_t split = 99;
  if (split)  bufsize /= 4;
#if  1 /* bug in TStreamerInfo*, fixed 09/05/14, ROOT_VERSION_CODE < ROOT_VERSION(5,34,20) */ 
  Tracklet::Class()->IgnoreTObjectStreamer();
#endif
  fTracklet = new Tracklet;
  TrackletTree->Branch("Tracklet","Tracklet",&fTracklet, bufsize,split);
  return StMaker::Init();
}
//_____________________________________________________________________________
/// Make - this method is called in loop for each event
Int_t StEandBDirMaker::Make(){
  if (! secXY) {
    GetTFile()->cd();
    secXY = new TH2D("secXY","clusters X and Y in the sector",60,-60,60,70,50,190); secXY->SetDirectory(0);
    secXZ = new TH2D("secXZ","clusters X vs Z in the sector",105,0,210,60,-60, 60); secXZ->SetDirectory(0);
    secYZ = new TH2D("secYZ","clusters Y vs Z in the sector",105,0,210,70, 50,190); secYZ->SetDirectory(0);
  }
  StEvent* pEvent = (StEvent*) StMaker::GetChain()->GetInputDS("StEvent");
  if (!pEvent) { cout << "Can't find StEvent" << endl; return kStWarn;}
  StTpcCoordinateTransform tran;
  StTpcLocalSectorCoordinate loc;
  StTpcLocalSectorDirection  locD;
  StTpcLocalCoordinate locT;
  StTpcLocalDirection  locDT;
  
  StTpcHitCollection* TpcHitCollection = pEvent->tpcHitCollection();
  if (! TpcHitCollection) { cout << "No TPC Hit Collection" << endl; return kStWarn;}
  UInt_t numberOfSectors = TpcHitCollection->numberOfSectors();
  for (UInt_t sec = 1; sec <= numberOfSectors; sec++) {
    StTpcSectorHitCollection* sectorCollection = TpcHitCollection->sector(sec-1);
    if (! sectorCollection) continue;
    if (sectorCollection->numberOfHits() < 5) continue;
    secXY->Reset();
    secXY->SetTitle(Form("clusters X and Y in the sector %i",sec));
    Int_t numberOfPadrows = sectorCollection->numberOfPadrows();
    for (int j = 0; j< numberOfPadrows; j++) {
      StTpcPadrowHitCollection *rowCollection = sectorCollection->padrow(j);
      if (rowCollection) {
	StSPtrVecTpcHit &hits = rowCollection->hits();
	Long_t NoHits = hits.size();
	for (Long_t l = 0; l < NoHits; l++) {
	  StTpcHit *tpcHit = static_cast<StTpcHit *> (hits[l]);
	  if (! tpcHit) continue;
	  if (tpcHit->flag() & FCF_CHOPPED || tpcHit->flag() & FCF_SANITY)     continue; // ignore hits marked by AfterBurner as chopped or bad sanity
	  if (tpcHit->pad() > 182 || tpcHit->timeBucket() > 511) continue; // some garbadge  for y2001 daq
#if 1
	  StGlobalCoordinate glob(tpcHit->position());
	  tran(glob,loc,tpcHit->sector(),tpcHit->padrow());
#else
	  StTpcPadCoordinate pad(tpcHit->sector(),tpcHit->padrow(),tpcHit->pad(),tpcHit->timeBucket());
	  tran(pad,loc);
#endif
	  secXY->Fill(loc.position().x(),loc.position().y(),tpcHit->adc());
	}
      }
    }
    if (secXY->GetEntries() < 5) continue;
    if (! spectr) spectr = new TSpectrum2(2*maxpeaks,1);
    Int_t nfound = spectr->Search(secXY,2,"colnomarkov");
    if (Debug()) {
      cout << "Found " << nfound << endl;
      spectr->Print();
      c2 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c2");
      if (! c2) c2 = new TCanvas("c2","c2",800,500);
      else      c2->Clear();
      c2->Divide(1,3);
      c2->cd(1);
      secXY->Draw("colz");
    }
#if ROOT_VERSION_CODE < ROOT_VERSION(6,5,1)
    Float_t *xpeaks = spectr->GetPositionX();
    Float_t *ypeaks = spectr->GetPositionY();
#else
    Double_t *xpeaks = spectr->GetPositionX();
    Double_t *ypeaks = spectr->GetPositionY();
#endif
    static Double_t windowX = 5;
    static Double_t windowY = 5;
    for (Int_t pf = 0; pf < nfound; pf++) {
      // Check significance
#if ROOT_VERSION_CODE < ROOT_VERSION(6,5,1)
      Float_t xp = xpeaks[pf];
      Float_t yp = ypeaks[pf];
#else
      Double_t xp = xpeaks[pf];
      Double_t yp = ypeaks[pf];
#endif
      Int_t binx = secXY->GetXaxis()->FindBin(xp);
      Int_t biny = secXY->GetYaxis()->FindBin(yp);
      Double_t zp = secXY->GetBinContent(binx,biny);
      Double_t ep = secXY->GetBinError(binx,biny);
      if (zp-3*ep < 0) continue;
      fTracklet->Clear();
      fTracklet->run = GetRunNumber();
      fTracklet->sector = sec;
      Double_t       W = 0;
      StThreeVectorD L, D, WG;
      StThreeVectorD L2, LT2, WG2;
      StThreeVectorD LT, DT; // in Tpc coordinate system
      Double_t avRow = 0;
      if (Debug()) {
	secXZ->Reset();
	secXZ->SetTitle(Form("clusters X vs Z the sector %i",sec));
	secYZ->Reset();
	secYZ->SetTitle(Form("clusters Y vs Z in the sector %i",sec));
      }
      for (int j = 0; j< numberOfPadrows; j++) {
	StTpcPadrowHitCollection *rowCollection = sectorCollection->padrow(j);
	if (rowCollection) {
	  StSPtrVecTpcHit &hits = rowCollection->hits();
	  Long_t NoHits = hits.size();
	  for (Long_t l = 0; l < NoHits; l++) {
	    StTpcHit *tpcHit = static_cast<StTpcHit *> (hits[l]);
	    if (! tpcHit) continue;
	    if (tpcHit->flag() & FCF_CHOPPED || tpcHit->flag() & FCF_SANITY)     continue; // ignore hits marked by AfterBurner as chopped or bad sanity
	    if (tpcHit->pad() > 182 || tpcHit->timeBucket() > 511) continue; // some garbadge  for y2001 daq
#if 1
	    StGlobalCoordinate glob(tpcHit->position());
	    tran(glob,loc,tpcHit->sector(),tpcHit->padrow());
	    tran(glob,locT,tpcHit->sector(),tpcHit->padrow());
#else
	    StTpcPadCoordinate pad(tpcHit->sector(),tpcHit->padrow(),tpcHit->pad(),tpcHit->timeBucket());
	    tran(pad,loc);
	    tran(loc,locT);
	    StGlobalCoordinate glob;
	    tran(locT,glob);
#endif
	    if (TMath::Abs(loc.position().x() - xp) > windowX ||
		TMath::Abs(loc.position().y() - yp) > windowY) continue;
	    fTracklet->nhits++;
	    if (Debug() > 1) {
	      cout << fTracklet->nhits << " " << *tpcHit << endl;
	    }
	    if (Debug()) {
	      secXZ->Fill(loc.position().z(),loc.position().x(),tpcHit->adc());
	      secYZ->Fill(loc.position().z(),loc.position().y(),tpcHit->adc());
	    }
	    avRow += (j+1)*tpcHit->adc();
	    W  += tpcHit->adc();
	    WG += tpcHit->adc()*tpcHit->position();
	    WG2 += tpcHit->adc()*StThreeVectorD(tpcHit->position().x()*tpcHit->position().x(),
						tpcHit->position().y()*tpcHit->position().y(),
						tpcHit->position().z()*tpcHit->position().z());
	    L += tpcHit->adc()*loc.position();
	    L2 += tpcHit->adc()*StThreeVectorD(loc.position().x()*loc.position().x(),
					       loc.position().y()*loc.position().y(),
					       loc.position().z()*loc.position().z());
	    D += tpcHit->adc()*loc.position()*loc.position().z();
	    LT += tpcHit->adc()*locT.position();
	    LT2 += tpcHit->adc()*StThreeVectorD(locT.position().x()*locT.position().x(),
						locT.position().y()*locT.position().y(),
						locT.position().z()*locT.position().z());
	    DT += tpcHit->adc()*locT.position()*locT.position().z();
	  }
	}
      }
      if (fTracklet->nhits < 5) continue;
      if (W < 100) continue;
      fTracklet->AdcSum = W;
      avRow /= W;
      fTracklet->row = avRow;
      StThreeVectorD posG = WG/W;
      StThreeVectorD posL = L/W;
      StThreeVectorD posT = LT/W;
      fTracklet->posG = posG;
      fTracklet->posL = posL;
      fTracklet->posT = posT;
      StThreeVectorD posG2 = WG2/W;
      StThreeVectorD posGA = StThreeVectorD(posG.x()*posG.x(),posG.y()*posG.y(),posG.z()*posG.z());
      posG2 -= posGA;
      fTracklet->posRMSG = StThreeVectorD(TMath::Sqrt(posG2.x()), TMath::Sqrt(posG2.y()), TMath::Sqrt(posG2.z()));
      StThreeVectorD posL2 = L2/W;
      StThreeVectorD posLA = StThreeVectorD(posL.x()*posL.x(),posL.y()*posL.y(),posL.z()*posL.z());
      posL2 -= posLA;
      fTracklet->posRMSL = StThreeVectorD(TMath::Sqrt(posL2.x()), TMath::Sqrt(posL2.y()), TMath::Sqrt(posL2.z()));
      StThreeVectorD posT2 = LT2/W;
      StThreeVectorD posTA = StThreeVectorD(posT.x()*posT.x(),posT.y()*posT.y(),posT.z()*posT.z());
      posT2 -= posTA;
      fTracklet->posRMST = StThreeVectorD(TMath::Sqrt(posT2.x()), TMath::Sqrt(posT2.y()), TMath::Sqrt(posT2.z()));
      StarMagField::Instance()->BField(fTracklet->posG.xyz(),fTracklet->BG.xyz());
      // Go to weighterd average
      L /= W; // < w_i * xyz_i >
      D /= W; // < w_i * z_i * xyz_i >
      Double_t det = (D.z() - L.z()*L.z()); // sigma_z^2
      if (TMath::Abs(det) < 4) continue; // sigma_Z < 2 cm
      Double_t x0 =   (L.x()*D.z() - D.x()*L.z())/det;
      Double_t tX =   (      D.x() - L.z()*L.x())/det;
      Double_t y0 =   (L.y()*D.z() - D.y()*L.z())/det;
      Double_t tY =   (      D.y() - L.z()*L.y())/det;
      if (y0 < 58 || y0 > 192 ||
	  TMath::Abs(x0) > 50) {
	if (Debug()) {
	  cout << "Illegal y0 = " << y0 << " x0 " << x0 << " tY " << tY << " tX " << tX << endl;
	}
	continue;
      }
      fTracklet->x0 = x0;
      fTracklet->tX = tX;
      fTracklet->y0 = y0;
      fTracklet->tY = tY;
      fTracklet->dirL = StThreeVectorD(tX,tY,1.);
      StTpcLocalSectorDirection dirLS(fTracklet->dirL,sec, fTracklet->row);
      StTpcLocalDirection       dirS2L;
      tran(dirLS,dirS2L);
      fTracklet->dirST = dirS2L.position();
      Double_t detT = (W     *DT.z() - LT.z()*LT.z());
      if (TMath::Abs(detT) < 1e-7) continue;
      Double_t x0T =   (LT.x()*DT.z() - DT.x()*LT.z())/detT;
      Double_t tXT =   (W     *DT.x() - LT.z()*LT.x())/detT;
      Double_t y0T =   (LT.y()*DT.z() - DT.y()*LT.z())/detT;
      Double_t tYT =   (W     *DT.y() - LT.z()*LT.y())/detT;
      fTracklet->x0T = x0T;
      fTracklet->tXT = tXT;
      fTracklet->y0T = y0T;
      fTracklet->tYT = tYT;
      fTracklet->dirT = StThreeVectorD(tXT,tYT,1.);
      StGlobalDirection globD(fTracklet->BG);
      tran(globD,locD,sec, TMath::Nint(avRow) );
      fTracklet->BL = locD.position();
      tran(globD,locDT,sec, TMath::Nint(avRow) );
      fTracklet->BT = locDT.position();
      if (TrackletTree)	TrackletTree->Fill();
      if (Debug()) {
	  cout << " x0 = " << x0 << " tX " << tX << " y0 " << y0 << "tY " << tY << endl;
	c2->cd(2);
	secXZ->Fit("pol1");
	secXZ->Draw("colz");
	c2->cd(3);
	secYZ->Fit("pol1");
	secYZ->Draw("colz");
	c2->Update();
	if (! gROOT->IsBatch() && Ask()) return kStEOF;
      }
    }
  }
  return kStOK;
}
