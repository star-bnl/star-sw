#include "StFgtLenTreeMaker.h"
#include "StRoot/StEvent/StFgtCollection.h"
#include "StRoot/StEvent/StFgtHitCollection.h"
#include "StRoot/StEvent/StFgtHit.h"
#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"
#include "StRoot/StEvent/StEvent.h"
#include "StRoot/StEvent/StEventInfo.h"
#include <TH2D.h>
#include <TROOT.h>
#include <TStyle.h>
#include <TCanvas.h>


Int_t StFgtLenTreeMaker::Make()
{
   StEvent* eventPtr = 0;
   eventPtr = (StEvent*)GetInputDS("StEvent");
   //(*outTxtFile) <<endl<<endl<<" ------new event: " << eventPtr->info()->id() << "----------------------" << " running nr: " << runningEvtNr << "------" << endl;
   
   Bool_t anycluster=false;
   Int_t iTrk=0;

   for(int iD=0;iD<kFgtNumDiscs;iD++)
     {
       Ncl[iD]=0;
       for(int iCl=0;iCl<20;iCl++)
	 {
	   cl_geoId[iD][iCl]=-1;
	   cl_seedType[iD][iCl]=-1;
	   cl_quad[iD][iCl]=-1;
	   cl_z[iD][iCl]=0.;cl_ez[iD][iCl]=0.;
	   cl_phi[iD][iCl]=0;cl_ephi[iD][iCl]=0;
	   cl_r[iD][iCl]=-1;cl_er[iD][iCl]=-1;
	   cl_charge[iD][iCl]=-1;cl_echarge[iD][iCl]=-1;
	   cl_numStrips[iD][iCl]=-1;
	   cl_tQuad[iD][iCl]=-1;
	   cl_tStrip[iD][iCl]=-1;
	   cl_layer[iD][iCl]=' ';
	   cl_key[iD][iCl]=-1;
	   cl_pointers[iD][iCl]=0;
	   maxadc[iD][iCl]=-1;seedadc[iD][iCl]=-1;
	 }
     };

   Ntrk=0;
   for(int iTr=0;iTr<4;iTr++)
     {
       tr_phi[iTr]=0.;tr_slope[iTr]=0.;tr_vtx[iTr]=0.;
       tr_chi2[iTr]=0.;tr_ncluster[iTr]=0;
       trkArray[iTr].phi=0.;trkArray[iTr].slope=0.;trkArray[iTr].vtx=0.;
       trkArray[iTr].chi2=0.;trkArray[iTr].ncluster=0;
       for(int iD=0;iD<6;iD++)
	 {
	   tr_iCl[iTr][iD]=-1;
	   trkArray[iTr].clArray[iD]=0;
	 };
     }
   Int_t ierr = StFgtQaMaker::Make();
   cout <<"in plotter make " << endl;
   for(int iD=0;iD<kFgtNumDiscs;iD++)
     {
       cout <<"trying to get clusters in disc " << iD << endl;
       StFgtHitCollection* clusterCol=mFgtCollectionPtr->getHitCollection(iD);
       
       if(clusterCol)
	 {
	   anycluster=true;
	   Int_t iCl=0;
	   cout <<"got collection, looking at hits ..  " <<endl;
	   const StSPtrVecFgtHit &hitVec=clusterCol->getHitVec();
	   StSPtrVecFgtHitConstIterator hitIter;
	   for(hitIter=hitVec.begin();hitIter != hitVec.end();hitIter++)
	     {
	       Int_t iq=(*hitIter)->getQuad();
	       Float_t phi=(*hitIter)->getPositionPhi();
	       Float_t ephi=(*hitIter)->getErrorPhi();
	       Float_t r=(*hitIter)->getPositionR();
	       Float_t er=(*hitIter)->getErrorR();
	       Float_t z=(*hitIter)->getPositionZ();
	       Float_t ez=(*hitIter)->getErrorZ();
	       Int_t geoId=(*hitIter)->getCentralStripGeoId();
	       Float_t charge=(*hitIter)->charge();
	       Float_t echarge=(*hitIter)->getChargeUncert();
	       Int_t numStrips=(*hitIter)->getStripWeightMap().size();
	       ///check if at least one of the strips has a valid seed)
	       Int_t seedType=-99;
	       Bool_t containsSeed=false;
	       Int_t maxA=0;
	       Int_t seedA=0;
	       for(stripWeightMap_t::iterator it=(*hitIter)->getStripWeightMap().begin();it!=(*hitIter)->getStripWeightMap().end();it++)
		 {
		   if(it->first->getClusterSeedType()==kFgtSeedType1 || it->first->getClusterSeedType()==kFgtSeedType2 ||it->first->getClusterSeedType()==kFgtSeedType3) //require  1,2,3 strips
		     {
		       containsSeed=true;		     
		       if(seedA < it->first->getMaxAdc())seedA=it->first->getMaxAdc();
		       if(it->first->getClusterSeedType()==kFgtSeedType1)seedType=1;
		       if(it->first->getClusterSeedType()==kFgtSeedType2 && seedType!=1)seedType=2;
		       if(it->first->getClusterSeedType()==kFgtSeedType3 && seedType!=1 && seedType!=2)seedType=3;
		     }
		   if(maxA < it->first->getMaxAdc())maxA=it->first->getMaxAdc();
		 }
	       if(iCl<20){
		 maxadc[iD][iCl]=maxA;
		 seedadc[iD][iCl]=seedA;
	       };
	       if(containsSeed)
		 {
		   cout <<"cluster contains a seed  " <<endl;
		   if((*hitIter)->getLayer()!='R')
		     {
		       continue;///go to next cluster
		     }
		   
		   Short_t tDisc, tQuad,tStrip;
		   Char_t tLayer;
		   StFgtGeom::decodeGeoId(geoId,tDisc,tQuad,tLayer,tStrip);     
		   if(iCl<20){
		     cout <<"filling cluster information  " <<endl;
		     cl_geoId[iD][iCl]=geoId;
		     cl_seedType[iD][iCl]=seedType;
		     cl_quad[iD][iCl]=iq;
		     cl_z[iD][iCl]=z;cl_ez[iD][iCl]=ez;
		     cl_phi[iD][iCl]=phi;cl_ephi[iD][iCl]=ephi;
		     cl_r[iD][iCl]=r;cl_er[iD][iCl]=er;
		     cl_charge[iD][iCl]=charge;cl_echarge[iD][iCl]=echarge;
		     cl_numStrips[iD][iCl]=numStrips;
		     cl_tQuad[iD][iCl]=tQuad;
		     cl_tStrip[iD][iCl]=tStrip;
		     cl_layer[iD][iCl]=(*hitIter)->getLayer();
		     cl_key[iD][iCl]=(*hitIter)->getKey();
		     cl_pointers[iD][iCl]=(*hitIter);
		   }
		   iCl++;
		 }
	     }
	   Ncl[iD]=iCl;
	 }
     }
   
   
   for(Int_t iphi=0;iphi<2;iphi++)
     {
       htrk->Reset();
       //Float_t minphi=;
       //Float_t maxphi=;
       Int_t trNcl=0;
       Float_t maxA[6]={0.,0.,0.,0.,0.,0.};
       Int_t kcl[6]={-1,-1,-1,-1,-1,-1};
       
       for(Int_t iD=0;iD<6;iD++)
	 {
	   Int_t Ntot=Ncl[iD];
	   if(Ntot>10)Ntot=10;
	   Float_t maxz=-1;
	   Float_t maxr=-1;
	   Float_t emaxr=-1;
	   for(Int_t iC=0;iC<Ntot;iC++)
	     {
	       //if(maxadc[iD][iC]>0. && cl_phi[iD][iC]>minphi && cl_phi[iD][iC]<maxphi) 
	       if(maxadc[iD][iC]>maxA[iD] && cl_quad[iD][iC]==iphi) 
		 {
		   //printf("%f %f \n",cl_z[iD][iC],cl_r[iD][iC]);
		   maxz=cl_z[iD][iC];
		   maxr=cl_r[iD][iC];emaxr=cl_er[iD][iC];
		   maxA[iD]=maxadc[iD][iC];
		   kcl[iD]=iC;
		 }
	     }
	   if(maxA[iD]>0.)
	     {
	       trNcl++;
	       Int_t zbin=htrk->FindBin(maxz);
	       htrk->SetBinContent(zbin,maxr);
	       Float_t errR=1.;
	       htrk->SetBinError(zbin,errR);
	     };
	 }
       if(trNcl>2)
	 {
	   cout <<"Track found, fitting ..  " <<endl;
	   f0->SetParameter(0,0);
	   f0->SetParameter(1,0);
	   htrk->SetStats(0);
	   htrk->Fit(f0,"Q");
	   htrk->SetMaximum(50);
	   trkArray[iTrk].slope=f0->GetParameter(1);
	   trkArray[iTrk].vtx=-f0->GetParameter(0)/f0->GetParameter(1);
	   trkArray[iTrk].chi2=f0->GetChisquare()/(trNcl-2);
	   trkArray[iTrk].ncluster=trNcl;
	   trkArray[iTrk].phi=iphi;
	   printf("slope=%f vtx=%f chi2=%f \n",trkArray[iTrk].slope,trkArray[iTrk].vtx,trkArray[iTrk].chi2);
	   tr_phi[iTrk]=trkArray[iTrk].phi;
	   tr_slope[iTrk]=trkArray[iTrk].slope;
	   tr_vtx[iTrk]=trkArray[iTrk].vtx;
	   tr_chi2[iTrk]=trkArray[iTrk].chi2;
	   tr_ncluster[iTrk]=trkArray[iTrk].ncluster;
	   for(Int_t iD=0;iD<6;iD++)
	     {
	       printf("%d \n",kcl[iD]);
	       tr_iCl[iTrk][iD]=kcl[iD];	   
	       if(kcl[iD]>-1)
		 {
		   trkArray[iTrk].clArray[iD]=cl_pointers[iD][kcl[iD]];
		   Int_t qq=trkArray[iTrk].clArray[iD]->getQuad();
		   Float_t pp=trkArray[iTrk].clArray[iD]->getPositionPhi();
		   Float_t rr=trkArray[iTrk].clArray[iD]->getPositionR();
		   printf("********##########*************** iTrk=%d ncluster=%d iD=%d quad=%d phi=%f r=%f",iTrk,trkArray[iTrk].ncluster,iD,qq,pp,rr);
		 }
	     };
	   iTrk++;
	 };
     };
   Ntrk=iTrk;
   iEvt=runningEvtNr;
   if(anycluster){
     printf("*************** FILLING THE TREE %d **********************\n", iEvt);
     tCl->Fill();
   };

   FitFunc();
   for(int iTr=0;iTr<Ntrk;iTr++)
     {
       for(int iD=0;iD<6;iD++)
	 {
	   if(trkArray[iTr].clArray[iD]>0)
	     {
	       char hname[100];
	       StFgtHit* clTr=trkArray[iTr].clArray[iD];
	       for(stripWeightMap_t::iterator it=clTr->getStripWeightMap().begin();it!=clTr->getStripWeightMap().end();it++)
		 {
		   rdo=0;arm=apv=chn=stat=-1;	
		   disk=quad=strip=-1.;layer=' ';
		   ordinate=lowerSpan=upperSpan=-1.;
		   Int_t dof=Ntimebin-4;
		   
		   it->first->getElecCoords( rdo, arm, apv, chn );      
		   stat=0;//how to get status?
		   if(stat>0)continue;//stat=0 is good. Otherwise, skip.
		   
		   int geoId=it->first->getGeoId();
		   if (geoId>0){
		     StFgtGeom::decodeGeoId(geoId,disk,quad,layer,strip);
		     StFgtGeom::getPhysicalCoordinate(geoId,disk,quad,layer,ordinate,lowerSpan,upperSpan);
		   }
		   else{continue;};//strip was not mapped , we readout in the daq file some '0' form non-existing APVs.
		   //printf("%d %d %d %d %d %d \n",iEvt,rdo,arm,apv,chn,geoId);
		   
		   ped=it->first->getPed();
		   pedSig=it->first->getPedErr();
		   if(ped<=0.)continue;
		   for(Int_t is=0;is<7;is++){adc[is]=0.;};
		   for(Int_t is=0;is<Ntimebin;is++){
		     adc[is]=it->first->getAdc(is);
		     //adc[is]-=ped;//already subtracted
		   };		
		   Bool_t pass=true;
		   //if(rdo==1 && arm==3 && apv==9)pass=false;		 
		   //if(rdo==1 && arm==4 && apv==21)pass=false;		 
		   //if(rdo==1 && arm==0 && apv>-1 && apv<10){}
		   //else if(rdo==2 && arm==0 && apv>-1 && apv<10){} 
		   //else{pass=false;};
		   
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
			 printf("%d ",it->first->getAdc(is));
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
		       //if(highcnt>2){errCode=2;}
		       
		       if(!errCode){
			 InitFX();
			 hh->Fit(FX,"QI","",0.,Ntimebin);		       
			 chi2=FX->GetChisquare()/(Float_t)dof;
			 fmax=FX->GetMaximumX();			     
			 norm=FX->GetParameter(0);
			 tau=FX->GetParameter(1);
			 beta=FX->GetParameter(2);
			 offset=FX->GetParameter(3);
			 t0=FX->GetParameter(4);
		       };
		       tFgt->Fill();//save only fitted events		 
		     };
		   };		   
		 }
	     }
	 }
     }
   runningEvtNr++; 
   return ierr;
};

 StFgtLenTreeMaker::StFgtLenTreeMaker( const Char_t* name):runningEvtNr(0)
{
  StFgtQaMaker( name, 0,0, "qName" );
  fname="hFgt";
  fitThresh=350;
  Ntimebin=7;
};

StFgtLenTreeMaker::~StFgtLenTreeMaker()
{
  //delete histogram arrays
};


Int_t StFgtLenTreeMaker::Finish(){
  //outTxtFile->close();
  gStyle->SetPalette(1);
  cout <<"cluster tree maker finish funciton " <<endl;
   Int_t ierr = kStOk;

  tCl->Print();
  fFgt->cd();
  ierr=tCl->Write();

  tFgt->Print();
  fFgt->cd();
  tFgt->Write();

  fFgt->Close();
  cout << "StFgtLenTreeMaker::Finish()" << endl;
  
  return ierr;
};

Int_t StFgtLenTreeMaker::Init(){
  
  Int_t ierr = kStOk;
  ierr=InitTree();

  htrk=new TH1F("htrk","Hits in FGT Discs;z (cm);r (cm)",300,-100,200);
  f0=new TF1("f0","[0]+[1]*x",-500,500);
  hh=new TH1F("hh","hh",Ntimebin,0,Ntimebin);
    
  return ierr;
};

Int_t StFgtLenTreeMaker::InitTree(){

  Int_t ierr = kStOk;
  if(fname.CompareTo("")==0){
    LOG_ERROR << "No output file name given for the TTree" << endm;
    ierr = kStErr;    
  }
  else{
    TString outName=fname+".tree.root";
    fFgt = new TFile(outName,"recreate");    
    tCl = new TTree("tCl","TTree for FGT cluster analysis");
    tCl->Branch("iEvt",&iEvt,"iEvt/I");
    tCl->Branch("Ncl",Ncl,"Ncl[6]/I");   
    tCl->Branch("cl_geoId",cl_geoId,"cl_geoId[6][20]/I");
    tCl->Branch("cl_seedType",cl_seedType,"cl_seedType[6][20]/I");
    tCl->Branch("cl_quad",cl_quad,"cl_quad[6][20]/I");
    tCl->Branch("cl_z",cl_z,"cl_z[6][20]/F");
    tCl->Branch("cl_ez",cl_ez,"cl_ez[6][20]/F");
    tCl->Branch("cl_phi",cl_phi,"cl_phi[6][20]/F");  
    tCl->Branch("cl_ephi",cl_ephi,"cl_ephi[6][20]/F");
    tCl->Branch("cl_r",cl_r,"cl_r[6][20]/F");
    tCl->Branch("cl_er",cl_er,"cl_er[6][20]/F");
    tCl->Branch("cl_charge",cl_charge,"cl_charge[6][20]/F");
    tCl->Branch("cl_echarge",cl_echarge,"cl_echarge[6][20]/F");
    tCl->Branch("cl_numStrips",cl_numStrips,"cl_numStrips[6][20]/I");
    tCl->Branch("cl_tStrip",cl_tStrip,"cl_tStrip[6][20]/I");
    tCl->Branch("cl_layer",cl_layer,"cl_layer[6][20]/B");
    tCl->Branch("cl_key",cl_key,"cl_key[6][20]/I");
    tCl->Branch("maxadc",maxadc,"maxadc[6][20]/I");
    tCl->Branch("seedadc",seedadc,"seedadc[6][20]/I");
    tCl->Branch("Ntrk",&Ntrk,"Ntrk/I");  
    tCl->Branch("tr_phi",tr_phi,"tr_phi[4]/F"); 
    tCl->Branch("tr_slope",tr_slope,"tr_slope[4]/F"); 
    tCl->Branch("tr_vtx",tr_vtx,"tr_vtx[4]/F");   
    tCl->Branch("tr_chi2",tr_chi2,"tr_chi2[4]/F"); 
    tCl->Branch("tr_ncluster",tr_ncluster,"tr_ncluster[4]/I"); 
    tCl->Branch("tr_iCl",tr_iCl,"tr_iCl[4][6]/I"); 

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

struct StFgtLenTreeMaker::MyFunc { 
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


void StFgtLenTreeMaker::FitFunc()
{  
  fs = new TF1("fs","[0]*x**[2]*exp(-x/[1])+[3]",-100,100);
  MyFunc * mf = new MyFunc(fs);
  FX = new TF1("FX",mf,&MyFunc::Evaluate,-10.,10.,5);
  FX->SetLineWidth(1);
  FX->SetLineColor(2);
  InitFX();
};

void StFgtLenTreeMaker::InitFX()
{
  FX->SetParameter(0,200.);
  FX->SetParLimits(0,0.,100000.);
  FX->SetParameter(1,1.);
  FX->SetParLimits(1,0.1,10.);
  FX->FixParameter(2,2.);
  FX->SetParameter(4,0.);
  FX->SetParLimits(4,-10.,17.);
};


ClassImp(StFgtLenTreeMaker);
