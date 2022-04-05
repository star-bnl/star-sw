 /***************************************************************************
 *
 * 
 * Author: Len K. Eun, Jan 2012
 *
 ***************************************************************************
 *
 * Description: Time shape fitter for the raw daq files. Output is a TTree that 
 * contain the original spectrum, mapping information, and the fit results.
 *
 ***************************************************************************
 * No version history yet
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
#include "StRoot/StFgtDbMaker/StFgtDbMaker.h"

// Copied from StFgtPedMaker.cxx

// constructor
StFgtTimeShapeMaker::StFgtTimeShapeMaker( const Char_t* name , const Char_t* dbMkrName  ) : StMaker( name ), mDbMkrName( dbMkrName ), mFgtDbMkr(0) {
  fname="hFgt";
  fitThresh=350;
  plotThresh=700;
  fixTau=false;  
  Ntimebin=7;
};

// initialize
Int_t StFgtTimeShapeMaker::Init(){
   Int_t ierr = kStOk;

   ierr=InitTree();

   mFgtDbMkr = static_cast< StFgtDbMaker* >( GetMaker( mDbMkrName.data() ) );
   
   if( !ierr && !mFgtDbMkr ){
     LOG_FATAL << "Error finding mFgtDbMkr named '" << mDbMkrName << "'" << endm;
     ierr = kStFatal;
   };
   
   if( !mFgtDbMkr->InheritsFrom("StFgtDbMaker") ){
     LOG_FATAL << "StFgtDbMkr does not inherit from StFgtDbMaker" << endm;
     LOG_FATAL << "Name is '" << mFgtDbMkr->GetName() << "', class is '" << mFgtDbMkr->ClassName() << endm;
     ierr = kStFatal;
   };

   if( !mFgtDbMkr ){
     mFgtDbMkr = static_cast< StFgtDbMaker* >( GetMakerInheritsFrom( "StFgtDbMaker" ) );
     if( !mFgtDbMkr ){
       LOG_FATAL << "StFgtDbMaker name not provided and error finding StFgtDbMaker" << endm;
       ierr = kStFatal;
     };
   };

   if(fixTau)
     {
       TFile ftau("htau.root","read");
       if(ftau.IsOpen()){
	 htau=(TH1F*)ftau.Get("htau");     
       }
       else{
	 LOG_FATAL << "Tau parameter fix requested, but htau.root not found" << endm;
	 ierr = kStFatal;
       };
     };
   
   LOG_INFO << "Using date and time " << mFgtDbMkr->GetDateTime().GetDate() << ", "
	    << mFgtDbMkr->GetDateTime().GetTime() << endm;

   igoodCnt=0;
   ibadCnt=0;
   iEvt=-1;
   hh=new TH1F("hh","hh",Ntimebin,0,Ntimebin);
   
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
    tFgt->Branch("disk",&disk,"disk/S");
    tFgt->Branch("quad",&quad,"quad/S");
    tFgt->Branch("strip",&strip,"strip/S");
    tFgt->Branch("stat",&stat,"stat/S");
    tFgt->Branch("ordinate",&ordinate,"ordinate/D");
    tFgt->Branch("lowerSpan",&lowerSpan,"lowerSpan/D");
    tFgt->Branch("upperSpan",&upperSpan,"upperSpan/D");
    tFgt->Branch("layer",&layer,"layer/B");
    tFgt->Branch("adc",adc,"adc[7]/I");
    tFgt->Branch("ped",&ped,"ped/D");
    tFgt->Branch("pedSig",&pedSig,"pedSig/D");
    tFgt->Branch("adcmax",&adcmax,"adcmax/I");
    tFgt->Branch("mmin",&mmin,"mmin/I");
    tFgt->Branch("mmax",&mmax,"mmax/I");
    tFgt->Branch("chi2",&chi2,"chi2/F");
    tFgt->Branch("fmax",&fmax,"fmax/F");
    tFgt->Branch("norm",&norm,"norm/F");
    tFgt->Branch("tau",&tau,"tau/F");
    tFgt->Branch("t0",&t0,"t0/F");
    tFgt->Branch("beta",&beta,"beta/F");
    tFgt->Branch("offset",&offset,"offset/F");
    tFgt->Branch("errCode",&errCode,"errCode/I");
  };
  return ierr;
};

Int_t StFgtTimeShapeMaker::Make(){
   iEvt++;
   Int_t ierr = kStOk;
   if(iEvt%10==0)cout << "iEvt = " << iEvt << endl;
   StFgtDb *fgtTables = 0;
   if( !mFgtDbMkr ){
     LOG_FATAL << "Pointer to Fgt DB Maker is null" << endm;
     ierr = kStFatal;
     return ierr;
   };
   if( !ierr ){
     fgtTables = mFgtDbMkr->getDbTables();
     
     if( !fgtTables ){
       LOG_FATAL << "Pointer to Fgt DB Tables is null" << endm;
       ierr = kStFatal;
       return ierr;
     };
   };
   
   StEvent* eventPtr = 0;
   eventPtr = (StEvent*)GetInputDS("StEvent");

   if( !eventPtr ) {
      LOG_ERROR << "Error getting pointer to StEvent from '" << ClassName() << "'" << endm;
      ierr = kStErr;
   };
   
   mFgtCollectionPtr = 0;

   if( eventPtr ) {
      mFgtCollectionPtr=eventPtr->fgtCollection();
   };

   if( !mFgtCollectionPtr) {
      LOG_ERROR << "Error getting pointer to StFgtCollection from '" << ClassName() << "'" << endm;
      ierr = kStErr;
   };

   if( pedSelect<0 || pedSelect>2 ) {
     LOG_ERROR << "Error no pedestal type selected " << pedSelect << endm;
     ierr = kStErr;
   };	      

   FitFunc();
   if( !ierr ){
      for( UInt_t discIdx=0; discIdx<mFgtCollectionPtr->getNumDiscs(); ++discIdx ){
         StFgtStripCollection *stripCollectionPtr = mFgtCollectionPtr->getStripCollection( discIdx );
         if( stripCollectionPtr ){
            StSPtrVecFgtStrip& stripVec = stripCollectionPtr->getStripVec();
            StSPtrVecFgtStripIterator stripIter;
	    rdo=0;arm=-1;apv=-1;chn=-1;		 
	    char hname[100];
            for( stripIter = stripVec.begin(); stripIter != stripVec.end(); ++stripIter ){	      
	      rdo=0;arm=apv=chn=stat=-1;	
	      disk=quad=strip=-1.;layer=' ';
	      ordinate=lowerSpan=upperSpan=-1.;
	      Int_t dof=Ntimebin-4;

	      (*stripIter)->getElecCoords( rdo, arm, apv, chn );      
	      stat=fgtTables->getStatusFromElecCoord(rdo,arm,apv,chn);
	      if(stat>0)continue;//stat=0 is good. Otherwise, skip.

	      int geoId=fgtTables->getGeoIdFromElecCoord(rdo, arm, apv, chn);
	      if (geoId>0){
		StFgtGeom::decodeGeoId(geoId,disk,quad,layer,strip);
		StFgtGeom::getPhysicalCoordinate(geoId,disk,quad,layer,ordinate,lowerSpan,upperSpan);
	      }
	      else{continue;};//strip was not mapped , we readout in the daq file some '0' form non-existing APVs.
	      //printf("%d %d %d %d %d %d \n",iEvt,rdo,arm,apv,chn,geoId);

	      ped=99999.;pedSig=0.;
	      switch(pedSelect){	    
	      case 0:{
		ped=(*stripIter)->getAdc(0);
		pedSig=35.*ped/745.;
	      } break;
	      case 1:{
		for(Int_t is=0;is<Ntimebin;is++){
		  if(ped>(*stripIter)->getAdc(is)){ped=(*stripIter)->getAdc(is);};
		};
		pedSig=35.*ped/745.;
		//dof++;//when using adcmin as pedestal, we fix the offset parameter.
	      } break;
	      case 2:{
		ped=fgtTables->getPedestalFromElecCoord(rdo,arm,apv,chn);
		pedSig=fgtTables->getPedestalSigmaFromElecCoord(rdo,arm,apv,chn);
	      }	break;
	      default:cout << "O_o This should never print" << endl;       
	      }
	      if(ped<=0.)continue;
	      for(Int_t is=0;is<7;is++){adc[is]=0.;};
	      for(Int_t is=0;is<Ntimebin;is++){
		adc[is]=(*stripIter)->getAdc(is);
		adc[is]-=ped;
	      };			     
	      Bool_t pass=true;
	      //if(rdo==1 && arm==3 && apv==9)pass=false;		 
	      //if(rdo==1 && arm==4 && apv==21)pass=false;		 
	      if(rdo==1 && arm==0 && apv>-1 && apv<10){}
	      else if(rdo==2 && arm==0 && apv>-1 && apv<10){} 
	      else{pass=false;};

	      if(pass){	
		chi2=-1.;tau=0.;t0=0.;beta=0.;offset=0.;errCode=0;
		hh->Reset();
		for(Int_t is=0;is<Ntimebin;is++){
		  hh->SetBinContent(is+1,adc[is]);
		  hh->SetBinError(is+1,pedSig);
		}
		sprintf(hname,"rdo%d_arm%d_apv%d_chn%d_%d",rdo,arm,apv,chn,iEvt);
		hh->SetTitle(hname);	 		            
		mmax=hh->GetMaximumBin()-1;
		mmin=hh->GetMinimumBin()-1;
		adcmax=hh->GetBinContent(mmax+1);
		if(0){
		  for(Int_t is=0;is<Ntimebin;is++){
		    printf("%d ",(*stripIter)->getAdc(is));
		  };		
		  printf("ped=%f \n",ped);
		};
		if(adcmax>fitThresh){
		  if(abs(mmax-mmin)==2 && mmin>0 && mmax>0 && mmin<Ntimebin-1 && mmax<Ntimebin-1){
		    Float_t middle1=(hh->GetBinContent(mmin)+hh->GetBinContent(mmin+2))/2.;
		    Float_t middle2=(hh->GetBinContent(mmax)+hh->GetBinContent(mmax+2))/2.;
		    if((middle1-hh->GetBinContent(mmin+1))/hh->GetBinError(mmin+1)>3. && (hh->GetBinContent(mmax+1)-middle2)/hh->GetBinError(mmax+1)>3.){errCode=1;}
		  }
		  Int_t highcnt=0;
		  for(Int_t is=0;is<Ntimebin;is++){
		    if(adc[is]>adcmax*0.95 && adcmax>20.*pedSig)highcnt++;
		  };		  
		  if(highcnt>2){errCode=2;}

		  if(!errCode){
		    InitFX();
		    if(fixTau){
		      Int_t binAPV = disk*40 + quad*10 + (apv%12);
		      Float_t tau0=htau->GetBinContent(binAPV+1);
		      if(tau0>0){InitFX(tau0);}
		      else{InitFX();}
		      dof++;//for fixing the tau parameter
		    };

		    hh->Fit(FX,"Q","",0.,Ntimebin);		       
		    chi2=FX->GetChisquare()/(Float_t)dof;
		    fmax=FX->GetMaximumX();			     
		    norm=FX->GetParameter(0);
		    tau=FX->GetParameter(1);
		    beta=FX->GetParameter(2);
		    offset=FX->GetParameter(3);
		    t0=FX->GetParameter(4);
		    if(chi2<1. && igoodCnt<120 && adcmax>plotThresh){
		      hGood[igoodCnt]=new TH1F(*hh);	
		      fGood[igoodCnt]=new TF1(*FX);
		      sprintf(hname,"fgood%d",igoodCnt);
		      fGood[igoodCnt]->SetTitle(hname);
		      igoodCnt++;
		    };
		    if(chi2>20. && ibadCnt<120 && adcmax>plotThresh){
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
  
  TCanvas* tcv1=new TCanvas("tcv1","tcv1",850,1100);
  tcv1->Divide(5,8);
  TCanvas* tcv2=new TCanvas("tcv2","tcv2",850,1100);
  tcv2->Divide(5,8);

  tcv1->Print("fits_good.pdf(","pdf");
  tcv2->Print("fits_bad.pdf(","pdf");

  for(Int_t i=0;i<igoodCnt;i++){
    tcv1->GetPad(i%40+1)->SetGridx(0);
    tcv1->GetPad(i%40+1)->SetGridy(0);
    tcv1->cd(i%40+1);
    hGood[i]->Draw();
    fGood[i]->Draw("same");
    if(i%40==39){
      tcv1->Print("fits_good.pdf","pdf");
      tcv1->Clear();
      tcv1->Divide(5,8);
    };
  }
  for(Int_t i=0;i<ibadCnt;i++){
    tcv2->GetPad(i%40+1)->SetGridx(0);
    tcv2->GetPad(i%40+1)->SetGridy(0);
    tcv2->cd(i%40+1);
    hBad[i]->Draw();
    fBad[i]->Draw("same");
    if(i%40==39){
      tcv2->Print("fits_bad.pdf","pdf");
      tcv2->Clear();
      tcv2->Divide(5,8);
    };
  }
  
  tcv1->Print("fits_good.pdf)","pdf");
  tcv2->Print("fits_bad.pdf)","pdf");

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
  //par[3]=y-offset: fixed at 0 for pedSelect==1 
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
  //FX->SetParameter(2,2.);
  //FX->SetParLimits(2,1.,3.);
  //if(pedSelect==1)FX->FixParameter(3,0.);
  FX->SetParameter(4,0.);
  FX->SetParLimits(4,-10.,17.);
};

void StFgtTimeShapeMaker::InitFX(Float_t ftau)
{
  FX->SetParameter(0,200.);
  FX->SetParLimits(0,0.,100000.);
  FX->SetParameter(1,ftau);
  FX->SetParLimits(1,ftau,ftau);
  FX->FixParameter(2,2.);
  //if(pedSelect==1)FX->FixParameter(3,0.);
  FX->SetParameter(4,0.);
  FX->SetParLimits(4,-10.,17.);
};


ClassImp( StFgtTimeShapeMaker );
