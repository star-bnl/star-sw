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
#include "StGmtSimpleClusterAlgo.h"
#include "TSystem.h"
#include "StMessMgr.h" 
ClassImp(StGmtClusterMaker);

Int_t StGmtClusterMaker::gmtStat = 0;
const unsigned int CLUS_BINS = 128;
const double       CLUS_MIN  = 0.0;
const double       CLUS_MAX  = 128*0.08;
const unsigned int MAX_PEAKS = 20;
//________________________________________________________________________________
inline double MyGaus(double x, double mean, double sigma, double delta) {
  return TMath::Freq((x-mean+delta/2)/sigma)-TMath::Freq((x-mean-delta/2)/sigma);
}
//________________________________________________________________________________
double fpeaks(double *x, double *par) {
  float result=0.0;
  unsigned int nPar=(unsigned int)par[0];
  for (unsigned int p=0;p<par[0];p++) {
      double norm  = TMath::Exp(par[3*p+1]);
      double mean  = par[3*p+2];
      double sigma = par[3*p+3];
      result += norm*MyGaus(x[0],mean,sigma,0.08); //norm*TMath::Gaus(x[0],mean,sigma,1);
   }
   return result;
}
//________________________________________________________________________________
TF1* StGmtClusterMaker::FindPeaks(TH1F* hist) {
  TSpectrum spect(MAX_PEAKS);
  TF1 back("poly","pol0",CLUS_MIN,CLUS_MAX);
  double par[MAX_PEAKS*3+1];
  spect.Search(hist,3);
  float* xpeaks=spect.GetPositionX();

  hist->Fit(&back,"Q");
  unsigned int npx=0;
  for(unsigned int i=0; i < spect.GetNPeaks(); i++) {
    double xp=xpeaks[i];
    int bin=hist->GetXaxis()->FindBin(xp);
    double yp=hist->GetBinContent(bin);
    double err=hist->GetBinError(bin);
    if(err<=0.0) continue;
    if(bin<=1) continue;
    if((yp-err*3) < back.GetParameter(0)) continue;
    double yp_left=hist->GetBinContent(bin-1);
    double yp_right=hist->GetBinContent(bin+1);
    double err_left=hist->GetBinError(bin-1);
    double err_right=hist->GetBinError(bin+1);
    double yp_sum=yp+yp_left+yp_right;
    double err_sum=TMath::Sqrt(err*err+err_left*err_left+err_right*err_right);
    if((yp_sum-3*err_sum) < back.GetParameter(0)) continue;

    par[3*npx+1]=TMath::Log(yp);
    par[3*npx+2]=xp;
    par[3*npx+3]=3*0.08; // sigma
    npx++;
  }
  if (! npx) return 0;

  TString funcName=Form("Func%s",hist->GetName());
  TF1* fitFunc;
  if(fitFunc=(TF1*)gROOT->GetListOfFunctions()->FindObject(funcName)) delete fitFunc;
  fitFunc=new TF1(funcName,fpeaks,CLUS_MIN,CLUS_MAX,3*npx+1);

  for(unsigned int i=0; i < npx; i++) {fitFunc->SetParLimits(3*i+3,0.08*0.5,10*0.08);}
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
void StGmtClusterMaker::ClusterBuilder(unsigned long events, UInt_t module, StGmtStripCollection& strips, StGmtHitCollection& hits) {
  static TCanvas* canv=0;
  static TH1F* histX=0;
  static TH1F* histY=0;
  static TProfile* profX[8]={0};
  static TProfile* profY[8]={0};
  
  StGmtStrip* pStrip;
  float position;
  unsigned int stripsNum=strips.getNumStrips();
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
  for(unsigned int iStrip=0;iStrip<stripsNum;iStrip++) {
    adc=0;
    pStrip=strips.getSortedStrip(iStrip);
    if(!pStrip->isY()) { profPointer=profX[module]; histPointer=histX; }
    else               { profPointer=profY[module]; histPointer=histY; }
    for(unsigned int iTimeBin=0;iTimeBin<kGmtNumTimeBins;iTimeBin++) {
      adc_buf=pStrip->getAdc(iTimeBin);
      if(adc_buf>-999) adc+=adc_buf;
    }
    position=pStrip->getPosition();
    int bin=profPointer->Fill(position,adc);
    float error=TMath::Sqrt(adc);
    histPointer->Fill(position,adc);
    histPointer->SetBinError(bin,error);
  }
  
  histX->Add(&profXold,-1.0); histY->Add(&profYold,-1.0);

  TF1 *fitX=0, *fitY=0;
  if(events) {
    fitX = FindPeaks(histX); fitY = FindPeaks(histY);
    unsigned int idx[MAX_PEAKS], idy[MAX_PEAKS];
    unsigned int nClusX=0, nClusY=0;
    std::cout << "###CLUSTER BUILDER" << endl;
    if(fitX) {
      for(unsigned int i=0; i<fitX->GetParameter(0); i++) {
        if(fitX->GetParameter(3*i+3) <= 0.4) idx[nClusX++]=i;
      }
      if (nClusX) *profX[module]=profXold;      
      std::cout << "######XPEAKS=" << fitX->GetParameter(0) << ", Clusters found=" << nClusX << endl;
    } else { std::cout << "######XNULL" <<  endl; }
    if(fitY) {
      for(unsigned int i=0; i<fitY->GetParameter(3*i+3); i++) {
        if(fitY->GetParameter(3*i+3) <= 0.4) idy[nClusY++]=i;
      }
      if (nClusY) *profY[module]=profYold;
      std::cout << "######YPEAKS=" << fitY->GetParameter(0) << ", Clusters found=" << nClusY << endl;
    } else { std::cout << "######YNULL" << endl; }
    if(nClusX && nClusY) { 
      for(unsigned int nx=0; nx<nClusX; nx++) {
        for(unsigned int ny=0; ny<nClusY; ny++) {
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
  StRTSBaseMaker( "clustser", name ),						     
  mClusterAlgoPtr(0),
  ftriviahit(NULL) {
  ftriviatree=0;
  StGmtClusterMaker::gmtStat = 0;
  phClusXDebug=0;
  phClusYDebug=0;
  SetAttr("gmtCosmics"             ,kFALSE);
  SetAttr("gmtClusTree"             ,kFALSE);
};
//________________________________________________________________________________
Int_t StGmtClusterMaker::Make() {
   Int_t ierr = kStOk;
   StGmtClusterMaker::gmtStat = 0;
   static unsigned long nEvents=0;
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

   Int_t loc_ierr=0;
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
//     loc_ierr = mClusterAlgoPtr->doClustering(moduleIdx,*stripCollectionPtr,*hitCollectionPtr); //CLUSTERING
     if(stripCollectionPtr && hitCollectionPtr && hitCollectionPtr->getHitVec().size()) {
       if(Debug()) {
	 LOG_INFO << "Cluster " << stripCollectionPtr->getNumStrips() << "strips\tin module" << stripCollectionPtr->getModule() << endm;
       }
       StGmtHit* pGmtHit=0;
     }
   }

   if(Debug()) {
     LOG_INFO << "End of gmt-clust-maker, print all strips & clusters: " << endm;
     LOG_INFO <<"  gmtCollnumModule=" << gmtCollectionPtr->getNumModules()<<", tot strip=" <<gmtCollectionPtr->getNumStrips()
	      <<"  totClust=" <<  gmtCollectionPtr->getNumHits() <<endm;
   }
   StGmtClusterMaker::gmtStat = 0;
   if(gmtCollectionPtr->getNumStrips() > 0) {
     StGmtClusterMaker::gmtStat = (gmtCollectionPtr->getNumHits()>0 ? 20 + gmtCollectionPtr->getNumHits() : 10);
   }
   if(ftriviatree) {
     for(int iModule=0; iModule<(int)gmtCollectionPtr->getNumModules(); iModule++) {
       if(Debug()) {
	 // ..... print all strips ....
	 LOG_INFO <<"  content: iModule="<<iModule<< " # of : strips="<<gmtCollectionPtr->getNumStrips(iModule) <<"  hits=" <<gmtCollectionPtr->getNumHits(iModule)<<endm;
       }
       StGmtStripCollection *stripPtr= gmtCollectionPtr->getStripCollection(iModule);
       StSPtrVecGmtStrip &stripVec = stripPtr->getStripVec(); //  StGmtStrip* stripPtr = stripCollectionPtr->getStrip( geoId );   
       StGmtHitCollection *clustPtr= gmtCollectionPtr->getHitCollection(iModule);
       StSPtrVecGmtHit &clustVec = clustPtr->getHitVec();    
       if(clustVec.size()){
	 int ih1=0;
	 if(gmtCollectionPtr->getNumStrips(iModule))
	   //if(loc_ierr) //to record when the pedestal calc is ok
	   for(StSPtrVecGmtStripIterator it=stripVec.begin();it!=stripVec.end(); it++, ih1++) {
	     // details of strip localization, use output variables ending w/ X
	     //       Short_t moduleX,  stripX; Char_t  layerX;
	     Short_t moduleX,  stripX; Int_t  layerX;
	     StGmtGeom::decodeGeoId((*it)->getGeoId(), moduleX, layerX, stripX);
	     ftriviahit = new StGmtTrivia((*it)->getGeoId(), moduleX, layerX, stripX); //HERE
	     ftriviahit->setMaxAdc((*it)->getMaxAdc());
	     ftriviahit->setRunId(eventPtr->time());
	     ftriviahit->setMaxPedSubtractedAdc((*it)->getMaxPedSubtractedAdc());
	     ftriviahit->setCharge((*it)->getCharge());
	     ftriviahit->setPed((*it)->getPed());
	     ftriviahit->setPedErr((*it)->getPedErr());
	     ftriviahit->setMaxPedSubtractedAdcTB((*it)->getMaxPedSubtractedAdcTB());
	     ftriviahit->setPedDev((*it)->getPedStdDev());
	     ftriviahit->setCoor((*it)->getCoordNum());;
	     ftriviahit->setPos((*it)->getPosition());
	     int rdo = -99, arm=-99, apv=-99, chan=-99;  
	     (*it)->getElecCoords(rdo, arm, apv, chan); 
	     ftriviahit->setElecCoords(rdo, arm, apv, chan); 
	     for(int i=0;i<15;i++) ftriviahit->setAdc(i,(*it)->getPedSubtractedAdc(i));
	     //ClusterRelatedStuff
	     
	     if(clustVec.size() && ftriviahit){
	       StGmtHit* cl = clustVec.at(0);
	       ftriviahit->setCX(cl->getLocalX());
	       ftriviahit->setCY(cl->getLocalY());
	       ftriviahit->setCAX(cl->getAdcX());
	       ftriviahit->setCAY(cl->getAdcY());
	       //ftriviahit->setClustSize(clustVec.size());
	       ftriviahit->setClustSize((*it)->isC());
	     }
	     ftriviatree->Fill();
	   }
       }//end   if(clustVec.size()){
     }
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
   nEvents++;
   return ierr;
}
//________________________________________________________________________________
Int_t StGmtClusterMaker::setClusterAlgo(StGmtSimpleClusterAlgo* algo) {
  mClusterAlgoPtr=algo;
  return kStOk;
}
//________________________________________________________________________________
Int_t StGmtClusterMaker::Init() {
  //  cout <<"cluster init " <<endl;
  Int_t ierr = kStOk;
  if (IAttr("gmtCosmics")) SetAttr(".Privilege",kTRUE);
  //
  // Please, conside the Maker's SetMode() method (setting m_Mode but you can get
  // the value using Getmode() as well ... to switch between cluster agos.
  // Extrenal setting a-la gmtClusMkr->setClusterAlgo( seededClusAlgo ); is
  // not appropriate for a chain mades maker.
  //
  if( !mClusterAlgoPtr ){
    //      LOG_INFO << "No gmt cluster algorithm specified, using default seededAlgo" << endm;
    //      mClusterAlgoPtr=new StGmtSeededClusterAlgo();
    LOG_INFO << "No gmt cluster algorithm specified, using default simple algorithm" << endm;
    mClusterAlgoPtr=new StGmtSimpleClusterAlgo();
  }

  //
  // Cluster tree
  //
  pClusTree=new TTree("pClusTree","A tree with cluster information");
  pClusTree->SetAutoSave(10000);
  ftriviatree=0;
  if (IAttr("gmtClusTree")) {
    TFile *f = GetTFile();
    if (f) {
      f->cd();
      ftriviatree  = new TTree("tGMT","A Tree with Trivia");
      ftriviatree->SetAutoSave(1000000);
      ftriviahit = new StGmtTrivia();
      ftriviatree->Branch("Hits","StGmtTrivia", &ftriviahit);//t,32000,0);
    }
  }
  gMessMgr->SetLimit("StGmtClusterMaker:ERROR - GSLError : Error 18 in qags.c at 548 : cannot reach tolerance because of roundoff error",5);
  gMessMgr->SetLimit("GSLError : Error 18 in qags.c at 548 : cannot reach tolerance because of roundoff error",5);
  return mClusterAlgoPtr->Init();
}
    
