#include <TMultiGraph.h>
#include <TStyle.h>
#include <TMinuit.h>
#include <TTree.h>
#include <TF1.h>
#include <TFile.h>
#include <TCanvas.h>
#include <TVector3.h>
#include <Riostream.h>
#include <TMath.h>

#include <StEmcPool/StPhotonCommon/MyEvent.h> 
#include <StEmcPool/StPhotonCommon/MyPoint.h> 
//#include <StEmcPool/StPhotonCommon/MyMcTrack.h> 

#include "GainAnalysis.h"
using namespace std;

ClassImp(GainAnalysis)

GainAnalysis::GainAnalysis(const char *psout,const char * /*flag*/)
{
  gStyle->SetOptDate(0);
  ps=new TPostScript(psout,-111);
  cout<<"DEFAULT CONSTRUCTOR FOR PI0ANALYSIS!!!!"<<endl;
  cout<<"storing ps in: "<<psout<<endl;
}
GainAnalysis::~GainAnalysis()
{
  cout<<endl<<"GainAnalysis destructed!"<<endl<<endl;
}
Int_t GainAnalysis::init(const char *output)
{

  mFileOut=new TFile(output,"RECREATE");
  h_minvPt=new TH2F("h_minvPt","invariant mass vs pT",40,0.,10.,200,0.,1.);
  h_minvId=new TH2F("h_minvId","inv. mass vs ID",2400,0.5,2400.5,200,0.,1.);
  
  return 1; 
}
Int_t GainAnalysis::make(Int_t evmax,const char *input)
{
  mFile=new TFile(input,"OPEN");
  myEventTree=(TTree*)mFile->Get("mEventTree");
  ev=new MyEvent();
  myEventTree->SetBranchAddress("branch",&ev);
  
  Int_t i=0;
  while(myEventTree->GetEntry(i)){
    if(evmax&&i>evmax){
      cout<<"reached evmax,"<<endl;
      cout<<"abort loop!"<<endl;
      break;      
    }
    if(i%100000==0) cout<<"processing "<<input<<" evt: "<<i<<endl;

    TVector3 vPos=ev->vertex();
    //just mb:
    if(!(ev->trigger()&1)&&!(ev->trigger()&8)){
      i++;
      continue;
    }
    //select vertex:
    if(TMath::Abs(vPos.Z())<0.0000001 || TMath::Abs(vPos.Z())>50.){
      i++;
      continue;
    }
    //loop on points
    MyPoint *p;
    MyPoint *pp;
    TClonesArray *clA=(TClonesArray*)ev->getPointArray();
    for(Int_t j=0;j<clA->GetEntries();j++){
      p=(MyPoint*)clA->At(j);
      TVector3 pPos=p->position();
      TVector3 pMom=pPos - vPos;
      pMom.SetMag(p->energy());
      
      if(!isPointOK(p)) continue;      

      for(Int_t jj=0;jj<clA->GetEntries();jj++){
	if(jj<=j) continue;
	pp=(MyPoint*)clA->At(jj);
	TVector3 ppPos=pp->position();
	TVector3 ppMom=ppPos - vPos;
	ppMom.SetMag(pp->energy());
	
	if(!isPointOK(pp)) continue;

	//two neutrals
	TVector3 pi0Mom=pMom+ppMom;
	Float_t angle=pMom.Angle(ppMom);


	//Float_t Epion=p->energy()+pp->energy();
	//Float_t asymm=TMath::Abs(p->energy()-pp->energy())/Epion;
	Float_t minv=TMath::Sqrt(2.*p->energy()*pp->energy()*(1. - TMath::Cos(angle)));
	Float_t pTpion=pi0Mom.Pt();
	
	if(ev->trigger()&1||ev->trigger()&8){

	  //get id:
	  Int_t ID=0;
	  Float_t EN=0.;
	  for(Int_t i_id=0;i_id<4;i_id++){
	    if(p->towerClusterEnergy(i_id)>EN){
	      EN=p->towerClusterEnergy(i_id);
	      ID=p-> towerClusterId(i_id);
	    }
	  }
	  //get id2:
          Int_t ID2=0;
          Float_t EN2=0.;
          for(Int_t i_id=0;i_id<4;i_id++){
            if(pp->towerClusterEnergy(i_id)>EN2){
              EN2=p->towerClusterEnergy(i_id);
              ID2=p-> towerClusterId(i_id);
            }
          }

	  //if(asymm>0.7) continue;

	  h_minvPt->Fill(pTpion,minv);
	  if(pTpion>.75){
	    h_minvId->Fill(ID,minv);
	    if(ID!=ID2) h_minvId->Fill(ID2,minv);
	  }
	}
      

      }
    }

      
    i++;
  }
  
  return 1;
}
Bool_t GainAnalysis::isPointOK(MyPoint *p)
{
  Bool_t ret=kTRUE;
  if(p->nHitsEta()<1 && p->nHitsPhi()<1) ret=kFALSE;
  return ret;
}


Int_t GainAnalysis::finish()
{
  mFileOut->Write();

  return 1;
}
