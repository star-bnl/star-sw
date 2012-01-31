 /***************************************************************************
 *
 * 
 * Author: Len K. Eun, Jan 2012
 *
 ***************************************************************************
 *
 * Description: 
 *
 ***************************************************************************
 *
 * 
 * 
 *
 *
 **************************************************************************/

#include <string>
#include "StFgtTimeShapeMaker.h"
#include "StRoot/StEvent/StFgtCollection.h"
#include "StRoot/StEvent/StFgtStrip.h"
#include "StRoot/StEvent/StEvent.h"
#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"

// Copied from StFgtPedMaker.cxx
// since this isn't defined elsewhere (yet)
// set to 8 so that it doesn't matter if bins are 0-6 or 1-7
const Int_t StFgtTimeShapeMaker::mMaxNumTimeBins = 8;

// constructor
StFgtTimeShapeMaker::StFgtTimeShapeMaker( const Char_t* name ) : StMaker( name ) {
  fname="hFgt";
  fitThresh=350;
  plotThresh=700;
  fixTau=false;  
  Ntimebin=7;
};

// initialize
Int_t StFgtTimeShapeMaker::Init(){
   Int_t ierr = kStOk;

   Int_t chstore[120];
   for(Int_t jch=0;jch<80;jch++)
     {
       chstore[jch]=-1;
     };
   igoodCnt=0;
   ibadCnt=0;
   iEvt=-1;
   hh=new TH1F("hh","hh",Ntimebin,0,Ntimebin);
   ierr=InitTree();
   
   return ierr;
};

StFgtTimeShapeMaker::~StFgtTimeShapeMaker(){
};

Int_t StFgtTimeShapeMaker::InitTree(){

  Int_t ierr = kStOk;
  if(fname.CompareTo("")==0){
    LOG_ERROR << "No output file name given for the TTree" << endm;
    ierr = kStErr;    
  }
  else{
    TString outName=fname+".tree.root";
    fFgt = new TFile(outName,"recreate");    
    tFgt = new TTree("tFgt","TTree for FGT time shape analysis");

    tFgt->Branch("iEvt",&iEvt,"iEvt/I");
    tFgt->Branch("rdo",&rdo,"rdo/I");
    tFgt->Branch("arm",&arm,"arm/I");
    tFgt->Branch("apv",&apv,"apv/I");
    tFgt->Branch("chn",&chn,"chn/I");
    tFgt->Branch("adc",adc,"adc[7]/I");
    tFgt->Branch("ped",&ped,"ped/I");
    tFgt->Branch("adcmax",&adcmax,"adcmax/I");
    tFgt->Branch("mmin",&mmin,"mmin/I");
    tFgt->Branch("mmax",&mmax,"mmax/I");
    tFgt->Branch("chi2",&chi2,"chi2/F");
    tFgt->Branch("fmax",&fmax,"fmax/F");
    tFgt->Branch("tau",&tau,"tau/F");
    tFgt->Branch("t0",&t0,"t0/F");
    tFgt->Branch("offset",&offset,"offset/F");
    tFgt->Branch("errCode",&errCode,"errCode/I");
  };
  return ierr;
};

Int_t StFgtTimeShapeMaker::Make(){
   iEvt++;
   Int_t ierr = kStOk;

   StEvent* eventPtr = 0;
   eventPtr = (StEvent*)GetInputDS("StEvent");

   if( !eventPtr ) {
      LOG_ERROR << "Error getting pointer to StEvent from '" << ClassName() << "'" << endm;
      ierr = kStErr;
   };

   StFgtCollection* fgtCollectionPtr = 0;

   if( eventPtr ) {
      fgtCollectionPtr=eventPtr->fgtCollection();
   };

   if( !fgtCollectionPtr) {
      LOG_ERROR << "Error getting pointer to StFgtCollection from '" << ClassName() << "'" << endm;
      ierr = kStErr;
   };

   FitFunc();
   if( !ierr ){
      for( UInt_t discIdx=0; discIdx<fgtCollectionPtr->getNumDiscs(); ++discIdx ){
         StFgtStripCollection *stripCollectionPtr = fgtCollectionPtr->getStripCollection( discIdx );
         if( stripCollectionPtr ){
            StSPtrVecFgtStrip& stripVec = stripCollectionPtr->getStripVec();
            StSPtrVecFgtStripIterator stripIter;
	    Int_t itimebinCnt=0;
	    rdo=0;arm=-1;apv=-1;chn=-1;		 
	    char hname[100];
            for( stripIter = stripVec.begin(); stripIter != stripVec.end(); ++stripIter ){	      
	      rdo=0;arm=-1;apv=-1;chn=-1;	
	      (*stripIter)->getElecCoords( rdo, arm, apv, chn );
	      printf("%d %d %d %d %d \n",iEvt,rdo,arm,apv,chn);
	      ped=(*stripIter)->getAdc(0);
	      for(Int_t is=0;is<Ntimebin;is++){
		adc[is]=(*stripIter)->getAdc(is);
		adc[is]-=ped;
		printf("%d ",adc[is]);
	      };		
	      printf("%d \n",(*stripIter)->getAdc());
	      Bool_t pass=true;
	      if(rdo==1 && arm==1 && (apv==0 || apv==1))pass=false;		 
	      if(pass){	
		chi2=-1.;tau=0.;t0=0.;offset=0.;errCode=0;
		Float_t pedsig=35.*ped/745.;		 	
		hh->Reset();
		for(Int_t is=0;is<Ntimebin;is++){
		  hh->SetBinContent(is+1,adc[is]);
		  hh->SetBinError(is+1,sqrt(2.)*pedsig);
		}
		sprintf(hname,"rdo%d_arm%d_apv%d_chn%d_%d",rdo,arm,apv,chn,iEvt);
		hh->SetTitle(hname);	 		            
		mmax=hh->GetMaximumBin()-1;
		mmin=hh->GetMinimumBin()-1;
		adcmax=hh->GetBinContent(mmax+1);
		
		Int_t highcnt=0;
		for(Int_t is=0;is<Ntimebin;is++){
		  if(adc[is]>adcmax*0.9)highcnt++;
		};
		
		if(adcmax>fitThresh){
		  Bool_t skipFit=false;
		  if(abs(mmax-mmin)==2 && mmin>0 && mmax>0 && mmin<6 && mmax<6){
		    Float_t middle1=(hh->GetBinContent(mmin)+hh->GetBinContent(mmin+2))/2.;
		    Float_t middle2=(hh->GetBinContent(mmax)+hh->GetBinContent(mmax+2))/2.;
		    if((middle1-hh->GetBinContent(mmin+1))/hh->GetBinError(mmin+1)>3. && (hh->GetBinContent(mmax+1)-middle2)/hh->GetBinError(mmax+1)>3.){skipFit=true;errCode=1;}
		  }
		  if(highcnt>3)errCode=2;
		  if(!skipFit){
		    InitFX();
		    if(fixTau){
		      //Float_t tau0=htau->GetBinContent(iapv+1);
		      //if(tau0>0){InitFX(tau0);}
		      //else{InitFX();}
		    };
		    hh->Fit(FX,"Q","",0.,Ntimebin);		       
		    chi2=FX->GetChisquare();
		    fmax=FX->GetMaximumX();			     
		    t0=FX->GetParameter(4);
		    tau=FX->GetParameter(1);
		    offset=FX->GetParameter(3);
		    if(chi2<20. && igoodCnt<120 && adcmax>plotThresh){
		      hGood[igoodCnt]=new TH1F(*hh);	
		      fGood[igoodCnt]=new TF1(*FX);
		      sprintf(hname,"fgood%d",igoodCnt);
		      fGood[igoodCnt]->SetTitle(hname);
		      igoodCnt++;
		    };
		    if(chi2>100. && ibadCnt<120 && adcmax>plotThresh){
		      hBad[ibadCnt]=new TH1F(*hh);	
		      fBad[ibadCnt]=new TF1(*FX);
		      sprintf(hname,"fbad%d",ibadCnt);
		      fBad[ibadCnt]->SetTitle(hname);
		      ibadCnt++;
		    };
		  };
		  tFgt->Fill();//save only fitted events		 
		};
	      };
	    };
         };
      };
   };

   return ierr;
};


Int_t StFgtTimeShapeMaker::Finish(){
  
  TCanvas* tcv1=new TCanvas("tcv1","tcv1",1600,800);
  tcv1->Divide(8,5);
  TCanvas* tcv2=new TCanvas("tcv2","tcv2",1600,800);
  tcv2->Divide(8,5);

  for(Int_t i=0;i<igoodCnt;i++){
    if(i==40 || i==80){
      if(i==40){tcv1->Print("fits_good(","pdf");}
      else{tcv1->Print("fits_good","pdf");};
      tcv1->Clear();
      tcv1->Divide(8,5);
    };
    tcv1->GetPad(i%40+1)->SetGridx(0);
    tcv1->GetPad(i%40+1)->SetGridy(0);
    tcv1->cd(i%40+1);
    hGood[i]->Draw();
    fGood[i]->Draw("same");
  }
  for(Int_t i=0;i<ibadCnt;i++){
    if(i==40 || i==80){
      if(i==40){tcv2->Print("fits_bad(","pdf");}
      else{tcv2->Print("fits_bad","pdf");};
      tcv2->Clear();
      tcv2->Divide(8,5);
    };
    tcv2->GetPad(i%40+1)->SetGridx(0);
    tcv2->GetPad(i%40+1)->SetGridy(0);
    tcv2->cd(i%40+1);
    hBad[i]->Draw();
    fBad[i]->Draw("same");
  }
  
  tcv1->Print("fits_good)","pdf");
  tcv2->Print("fits_bad)","pdf");

  tFgt->Print();
  fFgt->cd();
  tFgt->Write();
  fFgt->Close();
  cout << "StFgtTimeShapeMaker::Finish()" << endl;
   
  return StMaker::Finish();
};


struct StFgtTimeShapeMaker::MyFunc { 
  //par[0]=normalization
  //par[1]=tau
  //par[2]=exponent: fixed at 2
  //par[3]=y-offset: fixed at 0 
  //par[4]=t-offset
   MyFunc(TF1 * f): fFunc(f) {}
   double Evaluate (double *x, double * par) const { 
     fFunc->SetParameter(0,par[0]);
     fFunc->SetParameter(1,par[1]);
     fFunc->SetParameter(2,par[2]);
     fFunc->SetParameter(3,par[3]);
     Float_t val=0;
     if(x[0]-par[4]<0.){val=par[3];}
     else{val=fFunc->Eval(x[0]-par[4]);};
     return val;
   }
   TF1 * fFunc; 
};


void StFgtTimeShapeMaker::FitFunc()
{  
  fs = new TF1("fs","[0]*x**[2]*exp(-x/[1])+[3]",-100,100);
  MyFunc * mf = new MyFunc(fs);
  FX = new TF1("FX",mf,&MyFunc::Evaluate,-10.,10.,5);
  FX->SetLineWidth(1);
  FX->SetLineColor(2);
  InitFX();
};

void StFgtTimeShapeMaker::InitFX()
{
  FX->SetParameter(0,200.);
  FX->SetParLimits(0,0.,100000.);
  FX->SetParameter(1,1.);
  FX->SetParLimits(1,0.1,10.);
  FX->FixParameter(2,2.);
  FX->FixParameter(3,0.);
  FX->SetParameter(4,0.);
  FX->SetParLimits(4,-10.,17.);
};

void StFgtTimeShapeMaker::InitFX(Float_t tau)
{
  FX->SetParameter(0,200.);
  FX->SetParLimits(0,0.,100000.);
  FX->SetParameter(1,tau);
  FX->SetParLimits(1,tau,tau);
  FX->FixParameter(2,2.);
  FX->FixParameter(3,0.);
  FX->SetParameter(4,0.);
  FX->SetParLimits(4,-10.,17.);
};


ClassImp( StFgtTimeShapeMaker );
