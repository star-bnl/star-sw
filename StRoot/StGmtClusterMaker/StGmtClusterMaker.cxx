//
// First Cluster Maker
// \class StGmtClusterMaker
// \authors K.S. Engle and Richard Witt (witt@usna.edu)
// based on StFgtClusterMaker

#include "StGmtClusterMaker.h"
#include "StRoot/StEvent/StEvent.h"
#include "StRoot/StEvent/StGmtCollection.h"
#include "StEvent/StGmtHit.h"
#include "StRoot/StGmtUtil/geometry/StGmtGeom.h"
#include "TSystem.h"
#include "StMessMgr.h" 
ClassImp(StGmtClusterMaker);

Int_t StGmtClusterMaker::gmtStat = 0;
const UInt_t         CLUS_BINS = 128;
const Double_t       CLUS_MIN  = 0.0;
const Double_t       CLUS_MAX  = 128*0.08;
const UInt_t         MAX_PEAKS = 20;
//________________________________________________________________________________
inline Double_t MyGaus(Double_t x, Double_t mean, Double_t sigma, Double_t delta) {
  return TMath::Freq((x-mean+delta/2)/sigma)-TMath::Freq((x-mean-delta/2)/sigma);
}
//________________________________________________________________________________
Double_t fpeaks(Double_t *x, Double_t *par) {
  Float_t result=0.0;
  UInt_t nPar=(UInt_t)par[0];
  for (UInt_t p=0;p<par[0];p++) {
    Double_t norm  = TMath::Exp(par[3*p+1]);
    Double_t mean  = par[3*p+2];
    Double_t sigma = par[3*p+3];
    result += norm*MyGaus(x[0],mean,sigma,0.08); //norm*TMath::Gaus(x[0],mean,sigma,1);
  }
  return result;
}
//________________________________________________________________________________
TF1* StGmtClusterMaker::FindPeaks(TH1F* hist) {
  TSpectrum spect(MAX_PEAKS);
  TF1 back("poly","pol0",CLUS_MIN,CLUS_MAX);
  Double_t par[MAX_PEAKS*3+1];
  spect.Search(hist,3);
  float* xpeaks=spect.GetPositionX();
  
  hist->Fit(&back,"Q");
  UInt_t npx=0;
  UInt_t nfound = spect.GetNPeaks();
  for(UInt_t i=0; i < nfound; i++) {
    Double_t xp=xpeaks[i];
    int bin=hist->GetXaxis()->FindBin(xp);
    Double_t yp=hist->GetBinContent(bin);
    Double_t err=hist->GetBinError(bin);
    if(err<=0.0) continue;
    if(bin<=1) continue;
    if((yp-err*3) < back.GetParameter(0)) continue;
    Double_t yp_left=hist->GetBinContent(bin-1);
    Double_t yp_right=hist->GetBinContent(bin+1);
    Double_t err_left=hist->GetBinError(bin-1);
    Double_t err_right=hist->GetBinError(bin+1);
    Double_t yp_sum=yp+yp_left+yp_right;
    Double_t err_sum=TMath::Sqrt(err*err+err_left*err_left+err_right*err_right);
    if((yp_sum-3*err_sum) < back.GetParameter(0)) continue;
    
    par[3*npx+1]=TMath::Log(yp);
    par[3*npx+2]=xp;
    par[3*npx+3]=3*0.08; // sigma
    npx++;
  }
  if (Debug()) {LOG_INFO << hist->GetName() << " found " << nfound << " Accpeted " << npx << endm;}
  if (! npx) return 0;
  
  TString funcName=Form("Func%s",hist->GetName());
  TF1* fitFunc;
  if(fitFunc=(TF1*)gROOT->GetListOfFunctions()->FindObject(funcName)) delete fitFunc;
  fitFunc=new TF1(funcName,fpeaks,CLUS_MIN,CLUS_MAX,3*npx+1);
  
  for(UInt_t i=0; i < npx; i++) {fitFunc->SetParLimits(3*i+3,0.08*0.5,10*0.08);}
  fitFunc->SetParameters(par);
  fitFunc->FixParameter(0,(double)npx);
  fitFunc->SetNpx(1000);
  fitFunc->SetLineColor(kGreen);
  
  TVirtualFitter::SetDefaultFitter("Fumili");
  int isOk=hist->Fit(fitFunc);
  if(isOk) isOk=hist->Fit(fitFunc);
  if(isOk) return 0;
  
  return fitFunc;
}
//________________________________________________________________________________
void StGmtClusterMaker::ClusterBuilder(ULong_t events, UInt_t module, StGmtStripCollection& strips, StGmtHitCollection& hits) {
  static TCanvas* canv=0;
  static TH1F* histX=0;
  static TH1F* histY=0;
  static TProfile* profX[8]={0};
  static TProfile* profY[8]={0};
  
  StGmtStrip* pStrip;
  Float_t position;
  UInt_t stripsNum=strips.getNumStrips();
  int adc,adc_buf=0;
  TString name, title;
  TSpectrum spectX(MAX_PEAKS); TSpectrum spectY(MAX_PEAKS);
  TH1F* histPointer;
  TProfile* profPointer;

  if(!profX[module]) {
    name="PedestalX_"; name += module;
    profX[module]=new TProfile(name,name,CLUS_BINS,CLUS_MIN,CLUS_MAX,"s");
  }
  if(!profY[module]) {
    name = "PedestalY_"; name += module;
    profY[module]=new TProfile(name,name,CLUS_BINS,CLUS_MIN,CLUS_MAX,"s");
  }
  if(!histX) histX=new TH1F("ClusterX","ClusterX",CLUS_BINS,CLUS_MIN,CLUS_MAX);
  if(!histY) histY=new TH1F("ClusterY","ClusterY",CLUS_BINS,CLUS_MIN,CLUS_MAX);

  if(Debug()>3) {
    if(!canv) canv=new TCanvas("canv","Clusters",768,768);
    else      canv->Clear();
  }
  
  histX->Reset(); histY->Reset();
  TProfile profXold(*profX[module]); TProfile profYold(*profY[module]);
  for(UInt_t iStrip=0;iStrip<stripsNum;iStrip++) {
    adc=0;
    pStrip=strips.getSortedStrip(iStrip);
    if(!pStrip->isY()) { profPointer=profX[module]; histPointer=histX; }
    else               { profPointer=profY[module]; histPointer=histY; }
    for(UInt_t iTimeBin=0;iTimeBin<kGmtNumTimeBins;iTimeBin++) {
      adc_buf=pStrip->getAdc(iTimeBin);
      if(adc_buf>-999) adc+=adc_buf;
    }
    position=pStrip->getPosition();
    int bin=profPointer->Fill(position,adc);
    Float_t error=TMath::Sqrt(adc);
    histPointer->Fill(position,adc);
    histPointer->SetBinError(bin,error);
  }
  
  histX->Add(&profXold,-1.0); histY->Add(&profYold,-1.0);
  
  TF1 *fitX=0, *fitY=0;
  if(events) {
    fitX = FindPeaks(histX); fitY = FindPeaks(histY);
    UInt_t idx[MAX_PEAKS], idy[MAX_PEAKS];
    UInt_t nClusX=0, nClusY=0;
    if(fitX) {
      for(UInt_t i=0; i<fitX->GetParameter(0); i++) {
        if(fitX->GetParameter(3*i+3) <= 0.4) idx[nClusX++]=i;
      }
      if (nClusX) *profX[module]=profXold;      
      if (Debug()) {LOG_INFO << "######XPEAKS found =" << fitX->GetParameter(0) << ", Clusters fitted =" << nClusX << endm;}
    } else if (Debug()) {LOG_INFO << "######XNULL" <<  endm; }
    if(fitY) {
      for(UInt_t i=0; i<fitY->GetParameter(3*i+3); i++) {
	if(fitY->GetParameter(3*i+3) <= 0.4) idy[nClusY++]=i;
      }
      if (nClusY) *profY[module]=profYold;
      if (Debug()) {LOG_INFO << "######YPEAKS found =" << fitY->GetParameter(0) << ", Clusters fitted =" << nClusY << endm;}
    } else if (Debug()) {LOG_INFO << "######YNULL" << endm; }
    if(nClusX && nClusY) { 
      for(UInt_t nx=0; nx<nClusX; nx++) {
	for(UInt_t ny=0; ny<nClusY; ny++) {
	  StGmtHit* newCluster = new StGmtHit(
					      hits.getHitVec().size(),
					      module, 
					      TMath::Exp(fitX->GetParameter(3*nx+1)), // adcX
					      TMath::Exp(fitY->GetParameter(3*ny+1)), // adcY
					      fitX->GetParError(3*nx+1),              // error(adcX)
					      fitY->GetParError(3*ny+1),              // error(adcY)
					      fitX->GetParameter(3*nx+2), // meanX
					      fitY->GetParameter(3*ny+2), // meanY
					      fitX->GetParError(3*nx+2),  // error(meanX)
					      fitY->GetParError(3*ny+2),  // error(meanY)
					      fitX->GetParameter(3*nx+3), // sigmaX
					      fitY->GetParameter(3*ny+3), // sigmaY
					      fitX->GetParError(3*nx+3),  // error(sigmaX)
					      fitY->GetParError(3*ny+3)); // error(sigmaY)
	  hits.getHitVec().push_back(newCluster);
	}
      }
    }
  }
  
  if (Debug()>3) {
    canv->Divide(2,2);
    
    canv->cd(1);
    profX[module]->Draw();
    canv->cd(2);
    profY[module]->Draw();
    
    canv->cd(3);
    histX->Draw();
    canv->cd(4);
    histY->Draw();
    
    canv->Modified();
    canv->Update();
    canv->Draw();
    
    while (!gSystem->ProcessEvents()){gSystem->Sleep(200);}
  }
}
//________________________________________________________________________________
StGmtClusterMaker::StGmtClusterMaker( const Char_t* name ) : //StMaker(name),
  StRTSBaseMaker( "clustser", name ) {
  SetAttr("gmtCosmics"             ,kFALSE);
  SetAttr("gmtClusTree"            ,kFALSE);
};
//________________________________________________________________________________
Int_t StGmtClusterMaker::Make() {
  Int_t ierr = kStOk;
  static ULong_t nEvents=0;
  //StEvent* eventPtr = 0;
  //eventPtr = (StEvent*)GetInputDS("StEvent");
  StEvent* eventPtr = (StEvent*) (GetInputDS("StEvent"));
  if(!eventPtr) {
    LOG_ERROR << "Error getting pointer to StEvent from '" << ClassName() << "'" << endm;
    return kStErr;
  }
  StGmtCollection* gmtCollectionPtr = eventPtr->gmtCollection();
  if(!gmtCollectionPtr) {
    LOG_WARN << "Error getting pointer to StGmtCollection from '" << ClassName() << "'" << endm;
    return kStWarn;
  }
  for(UInt_t moduleIdx=0; moduleIdx<gmtCollectionPtr->getNumModules(); moduleIdx++) {
    if(Debug()) {
      LOG_INFO << "module: " << moduleIdx << " has strips: \t" <<  gmtCollectionPtr->getNumStrips(moduleIdx) << endm;
      LOG_INFO << "Collection =\t" << gmtCollectionPtr->getNumStrips()  << "\t"
	       << gmtCollectionPtr->getNumHits() << '\t' << gmtCollectionPtr->getNumPoints() << '\t' << endm;
    }
    Int_t nelements = gmtCollectionPtr->getNumStrips(moduleIdx);
    if(nelements < kGmtNumStripsPerModule) {
      if(Debug()) {
	LOG_WARN <<"StClusterMaker::Make(): no data for module " << moduleIdx << endm;
      }
      continue;
    }
    StGmtStripCollection *stripCollectionPtr = gmtCollectionPtr->getStripCollection(moduleIdx);
    StGmtHitCollection *hitCollectionPtr = gmtCollectionPtr->getHitCollection(moduleIdx);
    ClusterBuilder(nEvents,moduleIdx,*stripCollectionPtr,*hitCollectionPtr);
    if(stripCollectionPtr && hitCollectionPtr && hitCollectionPtr->getHitVec().size()) {
      if(Debug()) {
	LOG_INFO << "Cluster " << stripCollectionPtr->getNumStrips() << "strips\tin module" << stripCollectionPtr->getModule() << endm;
      }
    }
  }
  nEvents++;
  
  if(Debug()) {
    LOG_INFO << "End of gmt-clust-maker, print all strips & clusters: " << endm;
    LOG_INFO <<"  gmtCollnumModule=" << gmtCollectionPtr->getNumModules()<<", tot strip=" <<gmtCollectionPtr->getNumStrips()
	     <<"  totClust=" <<  gmtCollectionPtr->getNumHits() <<endm;
  }
  if (IAttr("gmtCosmics")) {
    if (! gmtCollectionPtr->getNumHits()) return kStERR;
  }
  if (Debug()) {
    UShort_t NumModules = gmtCollectionPtr->getNumModules();
    for (UShort_t m = 0; m < NumModules; m++) {
      const StGmtHitCollection *coll = gmtCollectionPtr->getHitCollection(m);
      if (! coll) continue;
      const StSPtrVecGmtHit &hits = coll->getHitVec();
      UInt_t NoHits = hits.size();
      for (UInt_t l = 0; l < NoHits; l++) {
	const StGmtHit *hit = hits[l];
	if (hit) {
	  hit->Print("");
	}
      }
    }
  }
  return ierr;
}
//________________________________________________________________________________
Int_t StGmtClusterMaker::Init() {
  if (IAttr("gmtCosmics")) SetAttr(".Privilege",kTRUE);
  return kStOk;
}
