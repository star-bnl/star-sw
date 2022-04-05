#include <Riostream.h>
#include <TString.h>
#include <TMath.h>
#include <assert.h>

#include <StEmcPool/StPhotonCommon/MyEvent.h> 
#include <StEmcPool/StPhotonCommon/MyPoint.h> 
//#include <StEmcPool/StPhotonCommon/MyMcTrack.h> 

#include "AnaCuts.h"
using namespace std;
ClassImp(AnaCuts)

AnaCuts::AnaCuts(const char* flag)
{
  cout<<"constructing CUTS for "<<flag<<endl;

  if(strcmp(flag,"dAu")==0){

    vertexxCUT=5.0;
    vertexyCUT=5.0;
    vertexzCUT=60.;
    neutralCUT=5.;//10, note this is cm's, 1 strip = 1.7cm or so.
    photonCUT=15.;
    etaHitsCUT=2;//2;
    phiHitsCUT=1;//2;
    energyRatioCUT=0.;//0
  
    etaMinCUT=.1;//.1
    etaMaxCUT=.9;//.9
    rapidityMinCUT=.1;//0.
    rapidityMaxCUT=.9;//1.
    rapPionMinCUT=.1;//.1
    rapPionMaxCUT=.9;//.9
    asymmetryCUT=0.7;//.7
    ratioCUT=0.8;//.8

    softTrigHT1=0.;
    softTrigHT2=0.;

    nPtBinsMB=8;
    ptBinsMB.Set(nPtBinsMB+1);
    for(Int_t i=0;i<=nPtBinsMB;i++) ptBinsMB[i]=(Double_t)0.5*i;
    ptBinsMB[7]=4.;
    ptBinsMB[8]=5.;

    nPtBinsHT1=14;
    ptBinsHT1.Set(nPtBinsHT1+1);
    for(Int_t i=0;i<=nPtBinsHT1;i++) ptBinsHT1[i]=(Double_t)1.*i;
    ptBinsHT1[13]=(Double_t)13.5;
    ptBinsHT1[14]=(Double_t)15.;

    nPtBinsHT2=14;
    ptBinsHT2.Set(nPtBinsHT2+1);
    for(Int_t i=0;i<=nPtBinsHT2;i++) ptBinsHT2[i]=(Double_t)1.*i;
    ptBinsHT2[13]=(Double_t)13.5;
    ptBinsHT2[14]=(Double_t)15.;


    nPtBinsEffMB=nPtBinsMB;//MB
    ptBinsEffMB=ptBinsMB;//MB
    nPtBinsEffHT1=nPtBinsHT1;
    ptBinsEffHT1=ptBinsHT1;
    nPtBinsEffHT2=nPtBinsHT2;
    ptBinsEffHT2=ptBinsHT2;
     
    nMinvBinsMB=500;
    mInvLowMB=0.0;
    mInvHighMB=5.0;
    nMinvBinsHT1=250;
    mInvLowHT1=0.0;
    mInvHighHT1=5.0;
    nMinvBinsHT2=250;
    mInvLowHT2=0.0;
    mInvHighHT2=5.0;

    nPtBinsEff=40;
    ptBinsEff.Set(nPtBinsEff+1);
    for(Int_t i=0;i<=nPtBinsEff;i++){
      ptBinsEff[i]=i*0.5;
    }

    ht1AdcCUT=287;
    ht2AdcCUT=447;

    isHot.Set(4800);
    isHot.Reset();
    Int_t badIdsDAU[]={82, 156, 240, 264, 719, 733, 818, 853, 931, 1289, 1355, 1358, 1359, 1472,
                       1527, 1759, 1943, 1965, 2005, 2011, 2025, 2090, 2130, 2131, 2152, 2170, 2369, -1};
    Int_t entryDAU=0;
    while(badIdsDAU[entryDAU]>-1) {isHot[badIdsDAU[entryDAU]-1]=1; entryDAU++;}
    
    timesSigma=2.0;
  }
  else if(strcmp(flag,"pp05")==0 || strcmp(flag,"pythia")==0){

    vertexxCUT=5.0;
    vertexyCUT=5.0;
    vertexzCUT=60.;//60.
    neutralCUT=5.;//10, note this is cm's, 1 strip = 1.7cm or so.
    photonCUT=15.;
    etaHitsCUT=2;//2;
    phiHitsCUT=1;//1;
    energyRatioCUT=0.;//0
  
    etaMinCUT=.2;//.1
    etaMaxCUT=.8;//.9
    rapidityMinCUT=.1;//0.
    rapidityMaxCUT=.9;//1.
    rapPionMinCUT=.1;//.1
    rapPionMaxCUT=.9;//.9
    asymmetryCUT=.7;//.7
    ratioCUT=1.;

    softTrigHT1=0.;
    softTrigHT2=0.;

    nPtBinsMB=8;
    ptBinsMB.Set(nPtBinsMB+1);
    for(Int_t i=0;i<=nPtBinsMB;i++) ptBinsMB[i]=(Double_t)0.5*i;
    ptBinsMB[7]=4.0;
    ptBinsMB[8]=5.0;


    nPtBinsHT1=14;
    ptBinsHT1.Set(nPtBinsHT1+1);
    for(Int_t i=0;i<=nPtBinsHT1;i++) ptBinsHT1[i]=(Double_t)1.*i;
    ptBinsHT1[13]=(Double_t)13.5;
    ptBinsHT1[14]=(Double_t)15.0;

    nPtBinsHT2=14;
    ptBinsHT2.Set(nPtBinsHT2+1);
    for(Int_t i=0;i<=nPtBinsHT2;i++) ptBinsHT2[i]=(Double_t)1.*i;
    ptBinsHT2[13]=(Double_t)13.5;
    ptBinsHT2[14]=(Double_t)15.0;

    nPtBinsEffMB=nPtBinsMB;//MB
    ptBinsEffMB=ptBinsMB;//MB
    nPtBinsEffHT1=nPtBinsHT1;
    ptBinsEffHT1=ptBinsHT1;
    nPtBinsEffHT2=nPtBinsHT2;
    ptBinsEffHT2=ptBinsHT2;
        
    nMinvBinsMB=500;
    mInvLowMB=0.0;
    mInvHighMB=5.0;
    nMinvBinsHT1=250;
    mInvLowHT1=0.0;
    mInvHighHT1=5.0;
    nMinvBinsHT2=250;
    mInvLowHT2=0.0;
    mInvHighHT2=5.0;

    nPtBinsEff=40;
    ptBinsEff.Set(nPtBinsEff+1);
    for(Int_t i=0;i<=nPtBinsEff;i++){
      ptBinsEff[i]=i*0.5;
    }

    ht1AdcCUT=0;//420;//prelim. rejection of obvious bad triggers
    ht2AdcCUT=0;//550;//but for pp05 trigger must be simulated.
    
    isHot.Set(4800);
    isHot.Reset();
    Int_t badIdsPP[]={750, 757, 762, 768, 812, -1};
    Int_t entryPP=0;
    while(badIdsPP[entryPP]>-1) {isHot[badIdsPP[entryPP]-1]=1; entryPP++;}
    
    timesSigma=2.0;
  }
  else{
    cout<<"error constructing cuts"<<endl;
    assert(0);
  }


  if(0){
    nPtBinsEffMB=40;
    ptBinsEffMB.Set(nPtBinsEffMB+1);
    for(Int_t i=0;i<=nPtBinsEffMB;i++) ptBinsEffMB[i]=(Double_t).5*i;
    nPtBinsEffHT1=40;
    ptBinsEffHT1.Set(nPtBinsEffHT1+1);
    for(Int_t i=0;i<=nPtBinsEffHT1;i++) ptBinsEffHT1[i]=(Double_t).5*i;
    nPtBinsEffHT2=40;
    ptBinsEffHT2.Set(nPtBinsEffHT2+1);
    for(Int_t i=0;i<=nPtBinsEffHT2;i++) ptBinsEffHT2[i]=(Double_t).5*i;
  }

}
AnaCuts::~AnaCuts()
{
  cout<<"cuts destructed!"<<endl;
}
Bool_t AnaCuts::isPointOK(MyPoint *p,TVector3 vpos)
{
  TVector3 pos=p->position();
  TVector3 mom=pos-vpos;
  mom.SetMag(p->energy());
  if(p->distanceToTrack()<neutralCUT) return kFALSE;
  if(!(p->nHitsEta()>=etaHitsCUT && p->nHitsPhi()>=phiHitsCUT))
    {
      if(!(p->nHitsEta()>=phiHitsCUT && p->nHitsPhi()>=etaHitsCUT))
	{
	  return kFALSE;
	}
    }
  if(pos.PseudoRapidity()<etaMinCUT) return kFALSE;
  if(pos.PseudoRapidity()>etaMaxCUT) return kFALSE;

  //new
  if((p->energyEta()+p->energyPhi())/p->energy()<energyRatioCUT) return kFALSE;
 
  return kTRUE;
}

Bool_t AnaCuts::isEventOK(MyEvent *ev, const char *flag)
{
  TVector3 vPos=ev->vertex();
  Float_t ratioE=TMath::Abs(ev->energyInBarrel()+ev->momentumInTpc())>0. ?
    ev->energyInBarrel()/(ev->energyInBarrel()+ev->momentumInTpc()) : 0.;
  //event cuts
  if(strcmp(flag,"dAu")==0){
    if(TMath::Abs(vPos.Z())>vertexzCUT) return kFALSE;
  }
  if(strcmp(flag,"pp05")==0){
    if(TMath::Abs(ev->bbcVertexZ())>vertexzCUT) return kFALSE;
  }
  if(TMath::Abs(vPos.X())>vertexxCUT) return kFALSE;
  if(TMath::Abs(vPos.Y())>vertexyCUT) return kFALSE;

  if(ev->trigger()<1) return kFALSE;
  //hot towers and beam bg:
  if(ev->trigger()&2 || ev->trigger()&4){
    if(ev->highTowerId()>0 && isHot[ev->highTowerId() - 1]){
      if(ev->trigger()&1) ev->setTrigger(1);
      else return false;
    }
    if(ev->highTowerEnergy()<0.001){
      if(ev->trigger()&1) ev->setTrigger(1);
      else return false;
    }
    if(ratioE>ratioCUT){
      if(ev->trigger()&1) ev->setTrigger(1);
      else return false;
    }
  }
  if(ev->trigger()&4 && ev->highTowerAdc()<=ht2AdcCUT){
    ev->setTrigger(ev->trigger()-4);
  }
  if(ev->trigger()&2 && ev->highTowerAdc()<=ht1AdcCUT){
    ev->setTrigger(1);
  }

  return true;
}
void AnaCuts::printCuts()
{
  cout<<"----- event cuts ------"<<endl;
  cout<<"vertexxCUT="<<vertexxCUT<<endl;
  cout<<"vertexyCUT="<<vertexyCUT<<endl;
  cout<<"vertexzCUT="<<vertexzCUT<<endl;
  cout<<"ratioCUT="<<ratioCUT<<endl<<endl;

  cout<<"----- point cuts ------"<<endl;
  cout<<"neutralCUT="<<neutralCUT<<endl;
  cout<<"chargedCUT="<<photonCUT<<endl;
  cout<<"etaHitsCUT="<<etaHitsCUT<<endl;
  cout<<"phiHitsCUT="<<phiHitsCUT<<endl;
  cout<<"etaMinCUT="<<etaMinCUT<<endl;
  cout<<"etaMaxCUT="<<etaMaxCUT<<endl;
  cout<<"rapidityMinCUT="<<rapidityMinCUT<<endl;
  cout<<"rapidityMaxCUT="<<rapidityMaxCUT<<endl;
  cout<<"rapPionMinCUT="<<rapPionMinCUT<<endl;
  cout<<"rapPionMaxCUT="<<rapPionMaxCUT<<endl;
  cout<<"asymmetryCUT="<<asymmetryCUT<<endl;
  cout<<"ht1AdcCUT="<<ht1AdcCUT<<endl;
  cout<<"ht2AdcCUT="<<ht2AdcCUT<<endl<<endl;


  cout<<endl<<"------ binning ------"<<endl;
  
  //bins
  cout<<"mb bins={";
  for(Int_t i=0;i<=nPtBinsMB;i++) cout<<ptBinsMB[i]<<",";
  cout<<"};"<<endl;
  cout<<"ht1 bins={";
  for(Int_t i=0;i<=nPtBinsHT1;i++) cout<<ptBinsHT1[i]<<",";
  cout<<"};"<<endl;
  cout<<"ht2 bins={";
  for(Int_t i=0;i<=nPtBinsHT2;i++) cout<<ptBinsHT2[i]<<",";
  cout<<"};"<<endl;
  cout<<"Eff bins={";
  for(Int_t i=0;i<=nPtBinsEff;i++) cout<<ptBinsEff[i]<<",";
  cout<<"};"<<endl;

  cout<<"inv mass bins mb: "<<nMinvBinsMB<<" "<<mInvLowMB<<" "<<mInvHighMB<<endl;
  cout<<"inv mass bins ht1: "<<nMinvBinsHT1<<" "<<mInvLowHT1<<" "<<mInvHighHT1<<endl;
  cout<<"inv mass bins ht2: "<<nMinvBinsHT2<<" "<<mInvLowHT2<<" "<<mInvHighHT2<<endl;  

  //hot towers:
  cout<<endl<<endl<<"------- hot towers -------"<<endl;
  for(Int_t k=1;k<=4800;k++)
    {
      if(isHot[k-1]) cout<<k<<", ";
    }
  cout<<endl<<"-----------------------"<<endl<<endl<<endl;
}
