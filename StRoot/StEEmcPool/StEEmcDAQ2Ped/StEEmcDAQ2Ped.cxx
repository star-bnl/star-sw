#include <TF1.h>

#include "StEEmcDAQ2Ped.h"
#include "StEEmcUtil/database/StEEmcDb.h"
#include "StEEmcUtil/database/EEmcDbItem.h"

ClassImp(StEEmcDAQ2Ped)

//---------------------------
//---------------------------
//---------------------------
StEEmcDAQ2Ped::StEEmcDAQ2Ped(const char* name, TFile* file)
    : StMaker(name) {
  mHList=0;
  mDAQHistos=file;
  cout <<"StEEmcDAQ2Ped::StEEmcDAQ2Ped()"<<endl;
}

//---------------------------
//---------------------------
//---------------------------

StEEmcDAQ2Ped::~StEEmcDAQ2Ped()
{
  cout <<"StEEmcDAQ2Ped::~StEEmcDAQ2Ped()"<<endl;
}

//---------------------------
//---------------------------
//---------------------------

Int_t 
StEEmcDAQ2Ped::Init() {
  mEeDb = (StEEmcDb*)this->GetDataSet("StEEmcDb");
  assert(mEeDb); // eemcDB must be in the chain, fix it
  return kStOk;
}

//------------------------------------
//------------------------------------

Int_t StEEmcDAQ2Ped::InitRun(int runNo){
  
  initHistos(); //initialize esmd histos

  assert(mEeDb);
  assert(mDAQHistos);
  
  // ...................  histo for individual pixels ...
  int icrate,ichan=0;

  // loop over tower crates
  for(icrate=0;icrate<MaxTwCrateID;icrate++) { 
    char tt0[100];
    sprintf(tt0,"ETOW_%d",icrate+1);
    h2D=(TH2F*)mDAQHistos->Get(tt0); assert(h2D);
    for (ichan=0;ichan<=MaxTwCrateCh;ichan++) {
      const  EEmcDbItem *x=mEeDb->getByCrate(icrate+1,ichan);
      if(x==0) continue;
      // initialize histos only for pixels acquired from DB
      char  tt1[100],tt2[200];
      sprintf(tt1,"a%s",x->name);
      sprintf(tt2,"ADC for %s,  cr/chan=%3.3d/%3.3d,  tube=%s; ADC",x->name,x->crate,x->chan,x->tube);
      //cout<<tt2<<endl;
      //x->print();
      TH1F* h=new TH1F(tt1,tt2,5000,-20.5,4979.5);
      // Fill 1D histos
      TAxis* axY=h2D->GetYaxis();
      int nbY=axY->GetNbins();
      //if(ichan == 0)
      //  cout<<tt2<<" crate/chan "<<icrate<<"/"<<ichan<<endl;
      for(int i=1;i<=nbY;i++) //weight entries in 1D histos
	h->SetBinContent(i+20,h2D->GetBinContent(x->chan+1,i));
      mHList->Add(h);
    
      //now find slope and plot in 2D histo
      if(x->fail) continue;
      float entries=h2D->GetEntries()/160;
      float xInt1=0; float xInt2=0; int xbin=-1;
      int xHigh = -1; int xLow = -1;
      for(xbin=h2D->GetNbinsY();xbin>0;xbin--){
	xInt1 += h2D->GetBinContent(ichan+1,xbin);
	//cout<<"xbin="<<xbin<<" xInt1="<<xInt1<<endl;
	if(xInt1 > 0.00036*entries){
	  xHigh = xbin; break;}
      }
      for(xbin=h2D->GetNbinsY();xbin>0;xbin--){
	xInt2 += h2D->GetBinContent(ichan+1,xbin);
	if(xInt2 > 0.0027*entries){
	  xLow = xbin; break;}
      }
      float ped=x->ped; float sigPed=x->sigPed;
      if(xLow < (ped+(5*sigPed)))
	cout<<"ichan= "<<ichan<<" x->name= "<<x->name<<" x->tube= "<<x->tube<<endl;
      xLowEtow->Fill(xLow-ped);
      xHighEtow->Fill(xHigh-ped);
      xDiffEtow->Fill(xHigh-xLow);
      xCorrEtow->Fill(xLow-ped,xHigh-ped);
      TF1* f1=new TF1("f1","exp([0]+[1]*x)",xLow,xHigh);
      f1->SetLineColor(2);
      h->Fit(f1,"Q","",xLow,xHigh);
      float slope = f1->GetParameter(1);
      float slopeErr = f1->GetParError(1);
      //if(slope<0 && slope>-0.1){ //don't plot outliers
      etow[icrate]->SetBinContent(ichan+1,slope);
      etow[icrate]->SetBinError(ichan+1,slopeErr);
      
      //if(icrate==0) cout<<"ichan= "<<ichan<<" x->name= "<<x->name<<" x->tube= "<<x->tube<<endl;
    }
  }
  
  //ascii output file for mapping
  ofstream outfile(mappingFile.Data());

  // loop over Mapmt crates
  for(icrate=MinMapmtCrateID;icrate<=MaxMapmtCrateID;icrate++) {
    char tt0[100];
    sprintf(tt0,"ESMD_%d",icrate-63);//daq counts crates from 1
    h2D=(TH2F*)mDAQHistos->Get(tt0); assert(h2D);
    for (ichan=0;ichan<MaxMapmtCrateCh;ichan++) {
      const  EEmcDbItem *x=mEeDb->getByCrate(icrate,ichan);
      if(x==0) continue;
      // initialize histos only for pixels acquired from DB
      char  tt1[100],tt2[200];
      sprintf(tt1,"a%s",x->name);
      sprintf(tt2,"ADC for %s,  cr/chan=%3.3d/%3.3d,  tube=%s; ADC",x->name,x->crate,x->chan,x->tube);
      TH1F* h=new TH1F(tt1,tt2,5000,-20.5,4979.5);
      // Fill 1D histos
      TAxis* axY=h2D->GetYaxis();
      int nbY=axY->GetNbins();
      for(int i=1;i<=nbY;i++) //weight entries in 1D histos
	h->SetBinContent(i+20,h2D->GetBinContent(x->chan+1,i));
      mHList->Add(h);
      
      outfile<<icrate<<","<<ichan<<","<<x->name<<endl;
      //now find slope and plot in 2D histo
      if(x->fail) continue;
      float entries=h2D->GetEntries()/192;
      float xInt1=0; float xInt2=0; int xbin=-1;
      int xHigh = -1; int xLow = -1;
      for(xbin=h2D->GetNbinsY();xbin>0;xbin--){
	xInt1 += h2D->GetBinContent(ichan+1,xbin);
	//cout<<"xbin="<<xbin<<" xInt1="<<xInt1<<endl;
	if(xInt1 > 0.0003*entries){
	  xHigh = xbin; break;}
      }
      for(xbin=h2D->GetNbinsY();xbin>0;xbin--){
	xInt2 += h2D->GetBinContent(ichan+1,xbin);
	if(xInt2 > 0.0015*entries){
	  xLow = xbin; break;}
      }
      float ped=x->ped; //float sigPed=x->sigPed;
      float stopper=ped; stopper +=25;
      if(xLow < stopper){
	xTestEsmd->Fill(xLow-ped-stopper);
	//cout<<"ichan= "<<ichan<<" x->name= "<<x->name<<" [xLow,xHigh] ["<<xLow<<","<<xHigh<<"] ped+25 = "<<stopper;
	xLow=(int)stopper+1;
	//cout<<" new xLow "<<xLow<<endl;
	//continue;
      }
      xDiffEsmd->Fill(xHigh-xLow);
      if(xHigh - xLow < 10) {
	//cout<<"Not enough channels fit ichan="<<ichan<<" x->name="<<x->name<<" # chan's fit="<<xHigh-xLow<<" # counts past xLow="<<h2D->Integral(ichan+1,ichan+1,xLow,1024)/entries<<endl; 
	continue;
      }
      xLowEsmd->Fill(xLow-ped);
      xHighEsmd->Fill(xHigh-ped);
      xCorrEsmd->Fill(xLow-ped,xHigh-ped);
      TF1* f1=new TF1("f1","exp([0]+[1]*x)",xLow,xHigh);
      f1->SetLineColor(2);
      h->Fit(f1,"Q","",xLow,xHigh);
      float slope = f1->GetParameter(1);
      float slopeErr = f1->GetParError(1);
      //fill histos by sector
      int sec=x->sec; int plane=3; int strip=x->strip;
      if(x->plane=='U') plane=0;
      if(x->plane=='V') plane=1;
      //printf("Sec %d plane %c=%d strip %d : name %s\n",sec,x->plane,plane,strip,x->name);
      if(plane==0 || plane==1){
	esmdSec[sec-1][plane]->SetBinContent(strip,slope);
	esmdSec[sec-1][plane]->SetBinError(strip,slopeErr);
      }
      //fill histos by crate
      if(plane!=0 && plane !=1){//eprs first
	esmd[icrate-63]->SetBinContent(ichan+1,slope);
	esmd[icrate-63]->SetBinError(ichan+1,slopeErr);
      }
      else if(strip<221 && strip>10){//then esmd for long strips
	esmd[icrate-63]->SetBinContent(ichan+1,slope);
	esmd[icrate-63]->SetBinError(ichan+1,slopeErr);
      }
      //end gain stuff
      
    }
  }
  
  printf("StEEmcDAQ2Ped::InitRun() \n");

  return kStOk;
}

//------------------------------------
//------------------------------------
void 
StEEmcDAQ2Ped::initHistos()
{

  int icrate=0;
  for(icrate=MinMapmtCrateID;icrate<=MaxMapmtCrateID;icrate++){
    //need to initialize slope histos for each crate
    char name[100]; char title[100];
    sprintf(name,"ESMD_%d",icrate-63);
    sprintf(title,"ESMD FEE %d",icrate);
    esmd[icrate-63] = new TH1F(name,title,192,-0.5,192-0.5);
    esmd[icrate-63]->SetMarkerStyle(20+mSet);//TCD25=21 and TCD63=22
    esmd[icrate-63]->SetMarkerColor(mSet+1);//TCD25=2 and TCD63=3
    mHList->Add(esmd[icrate-63]);
  }
  for(icrate=0;icrate<MaxTwCrateID;icrate++) {
    //need to initialize slope histos for each crate
    char nameT[100]; char titleT[100];
    sprintf(nameT,"ETOW_%d",icrate+1);
    sprintf(titleT,"ETOW FEE %d",icrate+1);
    etow[icrate] = new TH1F(nameT,titleT,120,-0.5,119.5);
    etow[icrate]->SetMarkerStyle(20+mSet);//TCD25=21 and TCD63=22
    etow[icrate]->SetMarkerColor(mSet+1);//TCD25=2 and TCD63=3
    mHList->Add(etow[icrate]);
  }
  for(int isector=0;isector<12;isector++){
    for(int iuv=0;iuv<2;iuv++){
      //need to initialize slope histos for each sector + plane
      char name[100]; char title[100];
      sprintf(name,"ESMD_sector%d_%cplane",isector+1,'U'+iuv);
      sprintf(title,"ESMD Sector%d %c-Plane",isector+1,'U'+iuv);
      esmdSec[isector][iuv] =new TH1F(name,title,288,0.5,288.5);
      esmdSec[isector][iuv]->SetMarkerStyle(20+mSet);//TCD25=21 and TCD63=22
      esmdSec[isector][iuv]->SetMarkerColor(mSet+1);//TCD25=2 and TCD63=3
      mHList->Add(esmdSec[isector][iuv]);
    }
  }
  xLowEtow = new TH1F("xLowEtow","Fit range minimum ETOW;xLow",60,0.0,60.0); mHList->Add(xLowEtow);
  xHighEtow = new TH1F("xHighEtow","Fit range maximum ETOW;xHigh",100,0.0,100.0); mHList->Add(xHighEtow);
  xDiffEtow = new TH1F("xDiffEtow","Fit range difference ETOW;xHigh-xLow",80,0.0,80.0); mHList->Add(xDiffEtow);
  xLowEsmd = new TH1F("xLowEsmd","Fit range minimum ESMD;xLow",150,0.0,150.0); mHList->Add(xLowEsmd);
  xHighEsmd = new TH1F("xHighEsmd","Fit range maximum ESMD;xHigh",300,0.0,300.0); mHList->Add(xHighEsmd);
  xDiffEsmd = new TH1F("xDiffEsmd","Fit range difference ESMD;xHigh-xLow",200,0.0,200.0); mHList->Add(xDiffEsmd);
  xCorrEtow = new TH2F("xCorrEtow","ETOW xHigh vs xLow;xLow;xHigh",60,0.0,60.0,100,0.0,100.0); mHList->Add(xCorrEtow);
  xCorrEsmd = new TH2F("xCorrEsmd","ESMD xHigh vs xLow;xLow;xHigh",50,0.0,200.0,100,0.0,400.0); mHList->Add(xCorrEsmd);
  xTestEsmd = new TH1F("xTestEsmd","ESMD xLow - (ped+5*sigPed)",60,-3.0,3.0); mHList->Add(xTestEsmd);

}



//------------------------------------
//------------------------------------
Int_t 
StEEmcDAQ2Ped::Finish()
{ 

  TH1F* hSlope = new TH1F("hSlope","Average slope by crate;Crate;Slope",48,63.5,111.5);
  hSlope->SetMarkerStyle(20+mSet);//TCD25=23 and TCD63=22
  hSlope->SetMarkerColor(mSet+1);//TCD25=4 and TCD63=5
  int icrate=0;
  for(icrate=MinMapmtCrateID;icrate<=MaxMapmtCrateID;icrate++){
    TF1* f=new TF1("f","[0]");
    f->SetLineColor(5+mSet); //TCD25=4 and TCD63=5
    if((icrate-63)%4 != 0) //SMD crates
      esmd[icrate-63]->Fit(f,"Q","",0,192);
    else {//PRS crates
      esmd[icrate-63]->Fit(f,"Q","",60,192);
      continue;
    }
    float constant = f->GetParameter(0);
    float constantErr = f->GetParError(0);
    hSlope->SetBinContent(icrate-63,constant);
    hSlope->SetBinError(icrate-63,constantErr);
  }
  mHList->Add(hSlope);

  return kStOk;
}


//------------------------------------
//------------------------------------
Int_t 
StEEmcDAQ2Ped::Make(){

  return kStOk;
}


