/***************************************************************************
 *
 * $Id: StTpcAvClusterMaker.cxx,v 1.5 2018/10/17 20:45:27 fisyak Exp $
 *
 **************************************************************************/
#include <assert.h>
#include "StTpcAvClusterMaker.h"
#include "StEvent.h"
#include "StEvent/StTpcHitCollection.h"
#include "StEvent/StTpcHit.h"
#include "StDetectorDbMaker/St_tpcPadConfigC.h"
//_____________________________________________________________
Int_t StTpcAvClusterMaker::InitRun(Int_t runnumber) {
  static Bool_t Done = kFALSE;
  SetAttr("minSector",1);
  SetAttr("maxSector",24);
  SetAttr("minRow",1);
  Int_t NoRows = St_tpcPadConfigC::instance()->numberOfRows(20);
  SetAttr("maxRow",NoRows);
  if (! Done) {
    TFile *f = GetTFile();
    if (! f) {
      gMessMgr->Error() << "with Tpx/Tpc AvLaser you must provide TFile as the 5-th parameter in bfc.C macro" << endm; 
      assert(0);
    }
    f->cd();
    enum {NoDim = 3};
    const Char_t *NameV[NoDim] = {     "row", "pad","time"};
    const Double_t xMin[NoDim] = {0.5       ,   0.5,  -0.5};
    const Double_t xMax[NoDim] = {0.5+NoRows, 182.5, 399.5};
    Int_t  nBins[NoDim]  = {    NoRows,   8*182,   8*400};
    fAvLaser = new THnSparseF *[24];
    for (Int_t s = 1; s <= 24; s++) {
      fAvLaser[s-1] = new THnSparseF(Form("AvLaser_%02i",s), 
				     Form("Averaged laser event for sector %02i",s), 
				     NoDim, nBins, xMin, xMax);
      fAvLaser[s-1]->CalculateErrors(kTRUE);
      for (Int_t i = 0; i < NoDim; i++) { 
	fAvLaser[s-1]->GetAxis(i)->SetName(NameV[i]);
      }
      f->Add(fAvLaser[s-1]);
    }
    Done = kTRUE;
  }
  // write event header for AvLaser
  StEvtHddr *header = GetEvtHddr();
  if (header) {
    TFile *f = GetTFile();
    if (! f) {
      gMessMgr->Error() << "with Tpx/Tpc AvLaser you must provide TFile as the 5-th parameter in bfc.C macro" << endm; 
      assert(0);
    }
    f->cd();
    header->Write();
  }
  return kStOK;
}
//_____________________________________________________________
Int_t  StTpcAvClusterMaker::Finish() {
#if 0
  if (GetTFile() && fAvLaser) {
    for (Int_t sector = 1; sector <= 24; sector++) {
      if (fAvLaser[sector-1]) {
	THnSparseF *hnew = CompressTHn(fAvLaser[sector-1]);
	GetTFile()->Remove(fAvLaser[sector-1]);
	delete fAvLaser[sector-1];
	fAvLaser[sector-1] = hnew;
	GetTFile()->Add(fAvLaser[sector-1]);
      }
    }
  }
#endif
  return StMaker::Finish();
}
//_____________________________________________________________
Int_t StTpcAvClusterMaker::Make() {
  StEvent *pEvent = dynamic_cast<StEvent *> (GetInputDS("StEvent"));
  if (Debug()) {LOG_INFO << "StTpcHitMaker::Make : StEvent has been retrieved " <<pEvent<< endm;}
  if (! pEvent) {LOG_INFO << "StTpcHitMaker::Make : StEvent has not been found " << endm; return kStWarn;}
  StTpcHitCollection *TpcHitCollection = pEvent->tpcHitCollection();
  if (! TpcHitCollection) {LOG_INFO << "StTpcHitMaker::Make : Tpc hit Collection has not been found " << endm; return kStWarn;}
  struct pixl_t {
    Double_t sector, row, pad, time, adc;
  };
  pixl_t pixel;
  
  UInt_t numberOfSectors = 24;
  for (UInt_t sec = 1; sec <= numberOfSectors; sec++) {
    assert(fAvLaser[sec-1]);
    if (fAvLaser[sec-1]->GetNbins() > 1000000) {
      THnSparseF *hnew = CompressTHn(fAvLaser[sec-1]);
      GetTFile()->Remove(fAvLaser[sec-1]);
      delete fAvLaser[sec-1];
      fAvLaser[sec-1] = hnew;
      GetTFile()->Add(fAvLaser[sec-1]);
    }
    StTpcSectorHitCollection* sectorCollection = TpcHitCollection->sector(sec-1);
    if (sectorCollection) {
      UInt_t numberOfPadrows = sectorCollection->numberOfPadrows();
      for (UInt_t row = 1; row <= numberOfPadrows; row++) {
	StTpcPadrowHitCollection *rowCollection = TpcHitCollection->sector(sec-1)->padrow(row-1);
	if (rowCollection) {
	  UInt_t NoHits = rowCollection->hits().size();
	  for (UInt_t k = 0; k < NoHits; k++) {
	    StTpcHit* kHit = TpcHitCollection->sector(sec-1)->padrow(row-1)->hits().at(k);
	    pixel.sector = sec;
	    pixel.row    = row;
	    pixel.pad    = kHit->pad();
	    pixel.time   = kHit->timeBucket();
	    pixel.adc    = kHit->adc();
	    fAvLaser[sec-1]->Fill(&pixel.row);// ,pixel.adc);
	  }
	}
      }
    }
  }
  return kStOK;
}
//________________________________________________________________________________
THnSparseF *StTpcAvClusterMaker::CompressTHn(THnSparseF *hist, Double_t compress) { 
  if (! hist) return 0;
  Int_t nd = hist->GetNdimensions();
  Int_t *nbins = new  Int_t[nd];
  for (Int_t i = 0; i < nd; i++) nbins[i] = hist->GetAxis(i)->GetNbins();
  THnSparseF *hnew = new THnSparseF(hist->GetName(),hist->GetTitle(),nd, nbins, 0, 0);//, hist->GetChunkSize());
  hnew->CalculateErrors(kTRUE);
  delete [] nbins;
  for (Int_t i = 0; i < nd; i++) {
    TAxis *ax = hist->GetAxis(i);
    if (ax->IsVariableBinSize()) hnew->GetAxis(i)->Set(ax->GetNbins(), ax->GetXbins()->GetArray());
    else                         hnew->GetAxis(i)->Set(ax->GetNbins(), ax->GetXmin(), ax->GetXmax());
  }
  Int_t *bins = new Int_t[nd];
  Double_t *x = new Double_t[nd];
  Long64_t N = hist->GetNbins(); cout << hist->GetName() << " has " << N << " bins before compression." << endl;
  Double_t max = -1;
  for (Long64_t i = 0; i < N; ++i) {
    Double_t cont = hist->GetBinContent(i, bins);
    if (cont > max) max = cont;
  }
  for (Long64_t i = 0; i < N; ++i) {
    Double_t cont = hist->GetBinContent(i, bins);
    if (cont < max/compress) continue;
    //    Long64_t bin = hnew->GetBin(bins);
    for (Int_t d = 0; d < nd; ++d) {x[d] = hist->GetAxis(d)->GetBinCenter(bins[d]);}
    hnew->Fill(x,cont);
  }
  delete [] bins;
  delete [] x;
  cout << hnew->GetName() << " has " << hnew->GetNbins() << " bins after compression." << endl;
  return hnew;
}
