
#include <assert.h>

#include <TColor.h>
#include <TMultiGraph.h>
#include <TGraphErrors.h>
#include <TStyle.h>
#include <TMinuit.h>
#include <TRandom.h>
#include <TTree.h>
#include <TF1.h>
#include <TFile.h>
#include <TCanvas.h>
#include <TVector3.h>
#include <TLorentzVector.h>
#include <TMath.h>
#include <Riostream.h>

#include <StEmcPool/StPhotonCommon/MyEvent.h> 
#include <StEmcPool/StPhotonCommon/MyPoint.h> 
#include <StEmcPool/StPhotonCommon/MyMcTrack.h> 

#include "AnaCuts.h"
#include "Pi0Analysis.h"

#include "Efficiency.h"
using namespace std;
ClassImp(Efficiency)

Efficiency::Efficiency(const char *input,const char *dir,const char *flag)
{
  gStyle->SetOptDate(0);

  dirout=TString(dir);

  mFile=new TFile(input,"OPEN");
  myEventTree=(TTree*)mFile->Get("mEventTree");
  ev=new MyEvent();
  myEventTree->SetBranchAddress("branch",&ev);

  pseff=new TPostScript((dirout+"_eff.ps").Data(),-111);
  //init cuts
  cuts=new AnaCuts(flag);
  cuts->printCuts();

  USEWEIGHT=kTRUE;
  USEPYTHIAWEIGHT=kFALSE;
  USEBBCSPREAD=kTRUE;

  mFlag=flag;
  isMC=kFALSE;
  isPythia=kFALSE;
  isDAU=kFALSE;
  isPP05=kFALSE;
  if(strcmp(mFlag,"dAu")==0) isDAU=kTRUE;
  if(strcmp(mFlag,"pp05")==0) isPP05=kTRUE;

  NEUTRONS=kFALSE;
  ANTINEUTRONS=kFALSE;
  PIONS=kFALSE;
  ETAS=kFALSE;
  CHARGEDPIONS=kFALSE;
  PHOTONS=kFALSE;
  KZEROLONG=kFALSE;

  nNeutrons=0;

  htitle=TString("eff. for ");

  cout<<endl<<"Efficiency constructed!"<<endl<<endl;
}

Efficiency::~Efficiency()
{
  cout<<endl<<"Efficiency destructed!"<<endl<<endl;
}
Int_t Efficiency::init()
{
  //acceptance
  h_genMB=new TH2F("h_genMB","generated vs y and p_{T}",20,0.,20.,30,-0.5,1.5);
  h_accMB=new TH2F("h_accMB","acceptance vs y and p_{T}",20,0.,20.,30,-0.5,1.5);

  //conversions
  h_convGen=new TH1F("h_convGen","generated vs #eta",45,-0.2,1.3);
  h_convConv=new TH1F("h_convConv","converted vs #eta",45,-0.2,1.3);
  h_convConvSSD=new TH1F("h_convConvSSD","converted vs rapidity",45,-0.2,1.3);
  h_convConvSVT=new TH1F("h_convConvSVT","converted vs rapidity",45,-0.2,1.3);
  h_convConvIFC=new TH1F("h_convConvIFC","converted vs rapidity",45,-0.2,1.3);

  h_convConvNotCtb=new TH1F("h_convConvNotCtb","converted vs #eta before CTB",45,-0.2,1.3);
  h_stopRadius=new TH1F("h_stopRadius","conversion radius",1000,0.,450.);


  //ht
  h_HT1adc_id=new TH2F("h_HT1adc_id","ht-1 adc vs id",2400,.5,2400.5,800,200.5,1000.5);
  h_HT2adc_id=new TH2F("h_HT2adc_id","ht-2 adc vs id",2400,.5,2400.5,800,200.5,1000.5);

  //split clusters:
  h_splitClusAll=new TH1F("h_splitClusAll","h_splitClusAll",20,0.0,10.0);
  h_splitClus=new TH1F("h_splitClus","h_splitClus",20,0.0,10.0);

  //neutrons:
  h_nbarDet=new TH1F("h_nbarDet","h_nbarDet",cuts->nPtBinsEffMB,cuts->ptBinsEffMB.GetArray());
  h_nbarIn=new TH1F("h_nbarIn","h_nbarIn",cuts->nPtBinsEffMB,cuts->ptBinsEffMB.GetArray());

  //QA
  h_mcdist=new TH1F("h_mcdist","distance to mctrack",1000,0.,5.);
  h_mcdist2D=new TH2F("h_mcdist2D","distance to mctrack vs pT",20,0.,20.,100,0.,.25);
  h_dist=new TH1F("h_dist","distance to charged track",1000,0.,200.);
  h_dist2D=new TH2F("h_dist2D","distance to charged track vs pT",20,0.,20.,200,0.,50.);
  //eff:
  h_inputMB=new TH1F("h_inputMB","mc input;p_{T};dN/dp_{T}",cuts->nPtBinsEffMB,cuts->ptBinsEffMB.GetArray());
  h_inputHT1=new TH1F("h_inputHT1","mc input;p_{T};dN/dp_{T}",cuts->nPtBinsEffHT1,cuts->ptBinsEffHT1.GetArray());
  h_inputHT2=new TH1F("h_inputHT2","mc input;p_{T};dN/dp_{T}",cuts->nPtBinsEffHT2,cuts->ptBinsEffHT2.GetArray());

  h_inputDaughtersMB=new TH1F("h_inputDaughtersMB","mc inputDaughters;p_{T};dN/dp_{T}",cuts->nPtBinsEffMB,cuts->ptBinsEffMB.GetArray());
  h_inputDaughtersHT1=new TH1F("h_inputDaughtersHT1","mc inputDaughters;p_{T};dN/dp_{T}",cuts->nPtBinsEffHT1,cuts->ptBinsEffHT1.GetArray());
  h_inputDaughtersHT2=new TH1F("h_inputDaughtersHT2","mc inputDaughters;p_{T};dN/dp_{T}",cuts->nPtBinsEffHT2,cuts->ptBinsEffHT2.GetArray());

  h_recoMB=new TH1F("h_recoMB","reco.;p_{T};dN/dp_{T}",cuts->nPtBinsEffMB,cuts->ptBinsEffMB.GetArray());
  h_recoHT1=new TH1F("h_recoHT1","reco.;p_{T};dN/dp_{T}",cuts->nPtBinsEffHT1,cuts->ptBinsEffHT1.GetArray());
  h_recoHT2=new TH1F("h_recoHT2","reco.;p_{T};dN/dp_{T}",cuts->nPtBinsEffHT2,cuts->ptBinsEffHT2.GetArray());

  h_recoDaughtersMB=new TH1F("h_recoDaughtersMB","recoDaughters.;p_{T};dN/dp_{T}",cuts->nPtBinsEffMB,cuts->ptBinsEffMB.GetArray());
  h_recoDaughtersHT1=new TH1F("h_recoDaughtersHT1","recoDaughters.;p_{T};dN/dp_{T}",cuts->nPtBinsEffHT1,cuts->ptBinsEffHT1.GetArray());
  h_recoDaughtersHT2=new TH1F("h_recoDaughtersHT2","recoDaughters.;p_{T};dN/dp_{T}",cuts->nPtBinsEffHT2,cuts->ptBinsEffHT2.GetArray());

  h_minvMB=new TH2F("h_minvMB","reco.;p_{T};m_{inv}",
		    cuts->nPtBinsEffMB,cuts->ptBinsEffMB.GetArray(),cuts->nMinvBinsMB,cuts->mInvLowMB,cuts->mInvHighMB);
  h_minvHT1=new TH2F("h_minvHT1","reco.;p_{T};m_{inv}",
		     cuts->nPtBinsEffHT1,cuts->ptBinsEffHT1.GetArray(),cuts->nMinvBinsHT1,cuts->mInvLowHT1,cuts->mInvHighHT1);
  h_minvHT2=new TH2F("h_minvHT2","reco.;p_{T};m_{inv}",
		     cuts->nPtBinsEffHT2,cuts->ptBinsEffHT2.GetArray(),cuts->nMinvBinsHT2,cuts->mInvLowHT2,cuts->mInvHighHT2);

  h_matrixMB=new TH2F("h_matrixMB","pT gen. vs reco. MB",80,0.,20.,80,0.,20.);

  h_etaphi=new TH2F("h_etaphi","eta/phi neutral points",300,0.,1.,1800,-TMath::Pi(),TMath::Pi());
  h_pythiaPions=new TH1F("h_pythiaPions","pythia pions vs pT",200,0.,50.);
  h_pythiaPhotons=new TH1F("h_pythiaPhotons","pythia photons vs pT",200,0.,50.);
  h_pythiaPartonPt=new TH1F("h_pythiaPartonPt","pT of pythia process",200,0.,50);

  h_clusterWidth=new TH2F("h_clusterWidth","width BSMD eta+phi",20,0.,20.,100,0.,0.05);
  h_energyRatio=new TH2F("h_energyRatio","energy ratio BSMD/BTOW",20,0.,20.,160,0.,8.);
  h_towclusRatio=new TH2F("h_towclusRatio","hightow/cluster",20,0.,20.,50,0.,1.);

  h_vzMB=new TH1F("h_vzMB","z-vertex MB",480,-120.,120.);
  h_vzMB->Sumw2();

  h_asymmMB=new TH2F("h_asymmMB","asymmetry of inv mass pairs MB",cuts->nPtBinsMB,cuts->ptBinsMB.GetArray(),40,0.0,1.0);
  h_asymmMB->Sumw2();
  h_asymmHT1=new TH2F("h_asymmHT1","asymmetry of inv mass pairs HT1",cuts->nPtBinsHT1,cuts->ptBinsHT1.GetArray(),40,0.0,1.0);
  h_asymmHT1->Sumw2();
  h_asymmHT2=new TH2F("h_asymmHT2","asymmetry of inv mass pairs HT2",cuts->nPtBinsHT2,cuts->ptBinsHT2.GetArray(),40,0.0,1.0);
  h_asymmHT2->Sumw2();

  h_smdeta1=new TH2F("h_smdeta1","n eta strips in cluster vs p_{T}",20,0.,10.,10,-0.5,9.5);
  h_smdphi1=new TH2F("h_smdphi1","n phi strips in cluster vs p_{T}",20,0.,10.,10,-0.5,9.5);
  h_smdeta2=new TH2F("h_smdeta2","eta energy ratio in cluster vs p_{T}",20,0.,10.,100,0.0,10.);
  h_smdphi2=new TH2F("h_smdphi2","phi energy ratio in cluster vs p_{T}",20,0.,10.,100,0.0,10.);

  h_energyeta=new TH1F("h_energyeta","energy BSMDE",40,0.,10.);
  h_energyphi=new TH1F("h_energyeta","energy BSMDP",40,0.,10.);

  h_pionsVsEtaMB=new TH1F("h_pionsVsEta","dN_{#pi^{0}}/dy data",40,-0.5,1.5);
  h_pionsVsEtaMB->Sumw2();


  h_smdeta1->Sumw2();
  h_smdphi1->Sumw2();
  h_smdeta2->Sumw2();
  h_smdphi2->Sumw2();


  h_pythiaPartonPt->Sumw2();
  h_inputMB->Sumw2();
  h_inputHT1->Sumw2();
  h_inputHT2->Sumw2();
  h_inputDaughtersMB->Sumw2();
  h_inputDaughtersHT1->Sumw2();
  h_inputDaughtersHT2->Sumw2();
  h_recoMB->Sumw2();
  h_recoHT1->Sumw2();
  h_recoHT2->Sumw2();
  h_recoDaughtersMB->Sumw2();
  h_recoDaughtersHT1->Sumw2();
  h_recoDaughtersHT2->Sumw2();
  h_minvMB->Sumw2();
  h_minvHT1->Sumw2();
  h_minvHT2->Sumw2();
  h_matrixMB->Sumw2();
  h_pythiaPions->Sumw2();
  h_clusterWidth->Sumw2();
  h_energyRatio->Sumw2();
  h_towclusRatio->Sumw2();



  return 1; 
}
Int_t Efficiency::make(Int_t evmax)
{
  Float_t WEIGHT=0.;
  Int_t pid=0;
  Int_t i=0;
  while(myEventTree->GetEntry(i))
    {
      if(evmax&&i>evmax)
	{
	  cout<<"reached evmax,"<<endl;
	  cout<<"abort loop!"<<endl;
	  break;      
	}
      if(i%10000==0) cout<<"processing event: "<<i<<endl;


      MyMcTrack *mcTr=0;
      Float_t ptInput=0;
      Float_t rapInput=0;
      if(!isPythia){
	mcTr=ev->getMcTrack();
	ptInput=mcTr->momentum().Pt();
	rapInput=mcTr->momentum().PseudoRapidity();
      }

      if(i==0 && !isPythia){
	cout<<"not pythia, plain MC"<<endl;
	pid=mcTr->id();
	if(pid==111){PIONS=kTRUE;htitle=TString("pions");}
	else if(pid==221) {ETAS=kTRUE;htitle=TString("etas");}     
	else if(pid==2112) {NEUTRONS=kTRUE;htitle=TString("neutrons");}
	else if(pid==-2112) {ANTINEUTRONS=kTRUE;htitle=TString("antineutrons");}
	else if(pid==211) {CHARGEDPIONS=kTRUE;htitle=TString("piplus");}
	else if(pid==22) {PHOTONS=kTRUE;htitle=TString("single gamma");}
	else if(pid==130) {KZEROLONG=kTRUE;htitle=TString("single kzeroL");}
	else assert(0);

	htitle+=TString(" in ");
	htitle+=TString(mFlag);
      }
      else if(isPythia){
	htitle=TString("pions pythia");
      }

      if(NEUTRONS||ANTINEUTRONS){
	TLorentzVector lv(mcTr->momentum().X(),mcTr->momentum().Y(),mcTr->momentum().Z(),sqrt(mcTr->momentum().Mag2()+1.));
	rapInput=lv.Rapidity();
      }
      
      //get vertex
      TVector3 vPos=ev->vertex();
      //get vertex weight dAu + pp separately:
      WEIGHT=getWeightVertex(vPos.Z());
      h_vzMB->Fill(vPos.Z(),WEIGHT);


      //add spread from bbc:
      if(USEBBCSPREAD && isPP05 && gRandom->Rndm()<0.36){
	double bbc_spread=gRandom->Gaus(0.,43.5);
	vPos.SetZ(vPos.Z()+bbc_spread);
      }

      //set trigger from simu for dAu
      if(isDAU){
	Int_t tr=1;
	if(ev->highTowerAdc()>cuts->ht1AdcCUT) tr+=2;
	if(ev->highTowerAdc()>cuts->ht2AdcCUT) tr+=4;
	ev->setTrigger(tr);
      }
      else if(!isPP05){
	cout<<"wrong flag"<<endl;
	assert(0);
      }

      if(ev->trigger()&2) h_HT1adc_id->Fill(ev->highTowerId(),ev->highTowerAdc());
      if(ev->trigger()&4) h_HT2adc_id->Fill(ev->highTowerId(),ev->highTowerAdc());

      
      //event cuts
      ev->setMomentumInTpc(999999.);

      if(isDAU){
	if(!cuts->isEventOK(ev,"dAu")) {i++;continue;}
      }
      else if(isPP05){
	if(!cuts->isEventOK(ev,"pp05")) {i++;continue;}
      }
      else assert(0);


      //event ok, count neutrons:
      nNeutrons=nNeutrons+1.;
      float pt_rnd=10.*gRandom->Rndm();
      h_nbarIn->Fill(pt_rnd,exp(-0.5*pt_rnd));
      h_nbarDet->Fill(mcTr->momentum().Pt());

      if(ETAS||PIONS){
	if(ev->numberOfMcPhotons()!=2 && !isPythia){
	  i++;
	  continue;
	}
      }
      
      //fill input:
      if(isPythia && 0){
        Float_t pypT=ev->partonPt();
        if(pypT<15.){
          WEIGHT=1.;
        }
        else if(pypT<25.){
          WEIGHT=(216000/208000)*(0.0003895/0.002228);
        }
        else if(pypT>25.){
          WEIGHT=(216000/208000)*(1.016e-005/0.002228);
        }
      }
      if(isPythia){
        h_pythiaPartonPt->Fill(ev->partonPt(),WEIGHT);
        for(Int_t it=0;it<ev->getMcPionArray()->GetEntries();it++){
          MyMcTrack *pyt=(MyMcTrack*)ev->getMcPionArray()->At(it);
          if(pyt->momentum().PseudoRapidity()<cuts->rapPionMinCUT||
             pyt->momentum().PseudoRapidity()>cuts->rapPionMaxCUT) continue;
          h_pythiaPions->Fill(pyt->momentum().Pt(),WEIGHT);
          if(ev->trigger()&1){
            h_inputMB->Fill(pyt->momentum().Pt(),WEIGHT);
          }
          if(ev->trigger()&1){
            h_inputHT1->Fill(pyt->momentum().Pt(),WEIGHT);
          }
          if(ev->trigger()&1){
            h_inputHT2->Fill(pyt->momentum().Pt(),WEIGHT);
          }
        }
	for(Int_t it=0;it<ev->getMcPhotonArray()->GetEntries();it++){
          MyMcTrack *pyt=(MyMcTrack*)ev->getMcPhotonArray()->At(it);
          if(pyt->momentum().PseudoRapidity()<cuts->rapPionMinCUT||
             pyt->momentum().PseudoRapidity()>cuts->rapPionMaxCUT) continue;
          h_pythiaPhotons->Fill(pyt->momentum().Pt(),WEIGHT);
          if(ev->trigger()&1){
            h_inputDaughtersMB->Fill(pyt->momentum().Pt(),WEIGHT);
          }
          if(ev->trigger()&1){
            h_inputDaughtersHT1->Fill(pyt->momentum().Pt(),WEIGHT);
          }
          if(ev->trigger()&1){
            h_inputDaughtersHT2->Fill(pyt->momentum().Pt(),WEIGHT);
          }
        }
      }



      if(NEUTRONS){
	WEIGHT*=getWeightNeutrons(ptInput);	
	if(rapInput>cuts->rapidityMinCUT&&rapInput<cuts->rapidityMaxCUT){
	  if(ev->trigger()&1) h_inputMB->Fill(mcTr->momentum().Pt(),WEIGHT);
	  if(ev->trigger()&1) h_inputHT1->Fill(mcTr->momentum().Pt(),WEIGHT);
	  if(ev->trigger()&1) h_inputHT2->Fill(mcTr->momentum().Pt(),WEIGHT);
	}
      }

      if(ANTINEUTRONS || KZEROLONG){
	WEIGHT*=getWeightAntiNeutrons(ptInput);
	if(rapInput>cuts->rapidityMinCUT&&rapInput<cuts->rapidityMaxCUT){
          if(ev->trigger()&1) h_inputMB->Fill(mcTr->momentum().Pt(),WEIGHT);
          if(ev->trigger()&1) h_inputHT1->Fill(mcTr->momentum().Pt(),WEIGHT);
          if(ev->trigger()&1) h_inputHT2->Fill(mcTr->momentum().Pt(),WEIGHT);
	}
      }

      if(PIONS||ETAS){
	if(PIONS) WEIGHT*=getWeightPions(ptInput);
	else if(ETAS) WEIGHT*=getWeightEtas(ptInput);

	//digamma
	if(rapInput>cuts->rapPionMinCUT&&rapInput<cuts->rapPionMaxCUT){
	  if(ev->trigger()&1) h_inputMB->Fill(mcTr->momentum().Pt(),WEIGHT);
	  if(ev->trigger()&1) h_inputHT1->Fill(mcTr->momentum().Pt(),WEIGHT);
	  if(ev->trigger()&1) h_inputHT2->Fill(mcTr->momentum().Pt(),WEIGHT);
	}
	MyMcTrack *daughterA=(MyMcTrack*)ev->getMcPhotonArray()->At(0);
	MyMcTrack *daughterB=(MyMcTrack*)ev->getMcPhotonArray()->At(1);

	//if(TMath::Abs(daughterA->energy()-daughterB->energy())/(daughterA->energy()+daughterB->energy())<0.7){
	//  i++; 
	//  continue;
	//}

	//for acceptance
        h_genMB->Fill(mcTr->momentum().Pt(),mcTr->momentum().PseudoRapidity());
	if(daughterA->position().PseudoRapidity() > cuts->etaMinCUT &&
           daughterA->position().PseudoRapidity() < cuts->etaMaxCUT){
	  if(daughterB->position().PseudoRapidity() > cuts->etaMinCUT &&
	     daughterB->position().PseudoRapidity() < cuts->etaMaxCUT){
	    h_accMB->Fill(mcTr->momentum().Pt(),mcTr->momentum().PseudoRapidity());
	  }
	}

	if(daughterA->momentum().PseudoRapidity() > cuts->rapidityMinCUT &&
	   daughterA->momentum().PseudoRapidity() < cuts->rapidityMaxCUT){
	  if(ev->trigger()&1) h_inputDaughtersMB->Fill(daughterA->momentum().Pt(),WEIGHT);
	  if(ev->trigger()&1) h_inputDaughtersHT1->Fill(daughterA->momentum().Pt(),WEIGHT);
	  if(ev->trigger()&1) h_inputDaughtersHT2->Fill(daughterA->momentum().Pt(),WEIGHT);
	}
	if(daughterB->momentum().PseudoRapidity() > cuts->rapidityMinCUT &&
	   daughterB->momentum().PseudoRapidity() < cuts->rapidityMaxCUT){
	  if(ev->trigger()&1) h_inputDaughtersMB->Fill(daughterB->momentum().Pt(),WEIGHT);
	  if(ev->trigger()&1) h_inputDaughtersHT1->Fill(daughterB->momentum().Pt(),WEIGHT);
	  if(ev->trigger()&1) h_inputDaughtersHT2->Fill(daughterB->momentum().Pt(),WEIGHT);
	}
      }
      
      if(CHARGEDPIONS){
	WEIGHT*=getWeightPions(ptInput);
	if(rapInput>cuts->rapidityMinCUT&&rapInput<cuts->rapidityMaxCUT){
          if(ev->trigger()&1) h_inputMB->Fill(mcTr->momentum().Pt(),WEIGHT);
          if(ev->trigger()&1) h_inputHT1->Fill(mcTr->momentum().Pt(),WEIGHT);
          if(ev->trigger()&1) h_inputHT2->Fill(mcTr->momentum().Pt(),WEIGHT);
        }
      }

      if(PHOTONS){
	WEIGHT*=getWeightPhotons(ptInput);
        if(rapInput>cuts->rapidityMinCUT&&rapInput<cuts->rapidityMaxCUT){
          if(ev->trigger()&1) h_inputMB->Fill(mcTr->momentum().Pt(),WEIGHT);
          if(ev->trigger()&1) h_inputHT1->Fill(mcTr->momentum().Pt(),WEIGHT);
          if(ev->trigger()&1) h_inputHT2->Fill(mcTr->momentum().Pt(),WEIGHT);

	  h_stopRadius->Fill(mcTr->stopRadius());
	  h_convGen->Fill(mcTr->momentum().PseudoRapidity(),getWeightVertex(vPos.Z()));
	  if(mcTr->stopRadius()>0&&mcTr->stopRadius()<223.5){
	    h_convConv->Fill(mcTr->momentum().PseudoRapidity(),getWeightVertex(vPos.Z()));
	  }
	  if(mcTr->stopRadius()>0&&mcTr->stopRadius()<200.){
            h_convConvNotCtb->Fill(mcTr->momentum().PseudoRapidity(),getWeightVertex(vPos.Z()));
          }
	  //SVT
	  if(mcTr->stopRadius()>1.&&mcTr->stopRadius()<20.){
	    h_convConvSVT->Fill(mcTr->momentum().PseudoRapidity(),getWeightVertex(vPos.Z()));
	  }
	  //SSD
          if(mcTr->stopRadius()>20.&&mcTr->stopRadius()<40.){
	    h_convConvSSD->Fill(mcTr->momentum().PseudoRapidity(),getWeightVertex(vPos.Z()));
          }
	  //IFC
	  if(mcTr->stopRadius()>40.&&mcTr->stopRadius()<55.){
	    h_convConvIFC->Fill(mcTr->momentum().PseudoRapidity(),getWeightVertex(vPos.Z()));
          }
	}
      }

      //check split clusters:
      int nclusters=0;

      MyPoint *p;
      MyPoint *pp;
      TClonesArray *clA=(TClonesArray*)ev->getPointArray();
      for(Int_t j=0;j<clA->GetEntries();j++)
        {
          p=(MyPoint*)clA->At(j);
          TVector3 pPos=p->position();
          TVector3 pMom=pPos-vPos;
          pMom.SetMag(p->energy());

	  h_dist2D->Fill(pMom.Pt(),TMath::Abs(p->distanceToTrack()));

          if(!cuts->isPointOK(p,vPos)) continue;

	  //chaeck split clusters:
	  nclusters++;
	  if(ev->trigger()&1 && pMom.Pt()>1.){
	    h_etaphi->Fill(pPos.PseudoRapidity(),pPos.Phi(),WEIGHT);
	  }
	  Float_t PdistMC=9999.;

	  h_dist->Fill(TMath::Abs(p->distanceToTrack()));

	  MyMcTrack *closestTrack=0;
	  if(PIONS||ETAS){
	    PdistMC=9999.;//reset
	    MyMcTrack *trA=(MyMcTrack*)ev->getMcPhotonArray()->At(0);
	    MyMcTrack *trB=(MyMcTrack*)ev->getMcPhotonArray()->At(1);
	    TVector3 mcPosA=trA->position();
	    TVector3 mcPosB=trB->position();
	    Float_t An=mcPosA.PseudoRapidity();
	    Float_t Ap=mcPosA.Phi();
	    Float_t Bn=mcPosB.PseudoRapidity();
	    Float_t Bp=mcPosB.Phi();
	    //point p:
	    Float_t Pn=pPos.PseudoRapidity();
	    Float_t Pp=pPos.Phi();
	    Float_t dphiAP=TMath::Abs(Ap-Pp);
	    while(dphiAP>2.*TMath::Pi()) dphiAP-=2.*TMath::Pi();
	    Float_t dphiBP=TMath::Abs(Bp-Pp);
	    while(dphiBP>2.*TMath::Pi()) dphiBP-=2.*TMath::Pi();
	    Float_t dpA=sqrt(pow(An-Pn,2)+pow(dphiAP,2));
	    Float_t dpB=sqrt(pow(Bn-Pn,2)+pow(dphiBP,2));
	    //distance to mc track:
	    PdistMC=dpA<dpB ? dpA : dpB;
	    closestTrack=dpA<dpB ? trA : trB;

	    h_mcdist->Fill(PdistMC);
	    h_mcdist2D->Fill(pMom.Pt(),PdistMC);
	  }
	  
	  //gamma:
	  if(p->distanceToTrack()>cuts->photonCUT){

	    if(pMom.PseudoRapidity()>cuts->rapidityMinCUT && pMom.PseudoRapidity()<cuts->rapidityMaxCUT){
	      if(ETAS||PIONS||isPythia){
		
		if(ev->trigger()&1) h_recoDaughtersMB->Fill(pMom.Pt(),WEIGHT);
		if(ev->trigger()&2) h_recoDaughtersHT1->Fill(pMom.Pt(),WEIGHT);
		if(ev->trigger()&4) h_recoDaughtersHT2->Fill(pMom.Pt(),WEIGHT);
		
	      }
	      else{
		if(ev->trigger()&1) h_recoMB->Fill(pMom.Pt(),WEIGHT);
		if(ev->trigger()&2) h_recoHT1->Fill(pMom.Pt(),WEIGHT);
		if(ev->trigger()&4) h_recoHT2->Fill(pMom.Pt(),WEIGHT);

		if(ev->trigger()&1) h_matrixMB->Fill(pMom.Pt(),ptInput);
	      }
	      
	      if(ev->trigger()&1){
		h_clusterWidth->Fill(pMom.Pt(),sqrt(p->widthEta()*p->widthEta()+p->widthPhi()*p->widthPhi()),WEIGHT);
		h_energyRatio->Fill(pMom.Pt(),(p->energyEta()+p->energyPhi())/p->energy(),WEIGHT);
		h_towclusRatio->Fill(pMom.Pt(),p->towerClusterEnergy(0)/p->energy(),WEIGHT);
		
		if(p->distanceToTrack()>30.){
		  h_smdeta1->Fill(pMom.Pt(),p->nHitsEta(),WEIGHT);
		  h_smdphi1->Fill(pMom.Pt(),p->nHitsPhi(),WEIGHT);
		  h_smdeta2->Fill(pMom.Pt(),p->energyEta()/p->energy(),WEIGHT);
		  h_smdphi2->Fill(pMom.Pt(),p->energyPhi()/p->energy(),WEIGHT);
		  
		  h_energyeta->Fill(p->energyEta());
		  h_energyphi->Fill(p->energyPhi());
		}
	      }


	    }
	 
	  }
	  
	  for(Int_t jj=0;(ETAS||PIONS||PHOTONS||isPythia)&&jj<clA->GetEntries();jj++)
            {
              if(jj<=j) continue;
              pp=(MyPoint*)clA->At(jj);
              TVector3 ppPos=pp->position();
              TVector3 ppMom=ppPos-vPos;
              ppMom.SetMag(pp->energy());
              if(!cuts->isPointOK(pp,vPos)) continue;

	      /*Float_t PPdistMC=9999.;
	      if(!isPythia){
	      MyMcTrack *trA=(MyMcTrack*)ev->getMcPhotonArray()->At(0);
	      MyMcTrack *trB=(MyMcTrack*)ev->getMcPhotonArray()->At(1);
	      TVector3 mcPosA=trA->position();
	      TVector3 mcPosB=trB->position();
	      Float_t An=mcPosA.PseudoRapidity();
	      Float_t Ap=mcPosA.Phi();
	      Float_t Bn=mcPosB.PseudoRapidity();
	      Float_t Bp=mcPosB.Phi();  
	      
	      //point pp:
	      Float_t PPn=ppPos.PseudoRapidity();
	      Float_t PPp=ppPos.Phi();
	      Float_t dphiAPP=TMath::Abs(Ap-PPp);
	      while(dphiAPP>2.*TMath::Pi()) dphiAPP-=2.*TMath::Pi();
	      Float_t dphiBPP=TMath::Abs(Bp-PPp);
	      while(dphiBPP>2.*TMath::Pi()) dphiBPP-=2.*TMath::Pi();
	      Float_t dppA=sqrt(pow(An-PPn,2)+pow(dphiAPP,2));
	      Float_t dppB=sqrt(pow(Bn-PPn,2)+pow(dphiBPP,2));
	      PPdistMC=dppA<dppB ? dppA : dppB;
	      }*/
	      
	      //two neutrals:
	      TVector3 pi0Mom=pMom+ppMom;
              Float_t angle=pMom.Angle(ppMom);
              Float_t minv=sqrt(2.*p->energy()*pp->energy()*(1. - cos(angle)));
              Float_t pTpion=pi0Mom.Pt();
              Float_t etapion=pi0Mom.PseudoRapidity();
              Float_t asymm=TMath::Abs(p->energy()-pp->energy())/(p->energy()+pp->energy());

	      if(etapion<cuts->rapPionMinCUT||etapion>cuts->rapPionMaxCUT) continue;
	      
	      if(ev->trigger()&1){
		if(asymm<=cuts->asymmetryCUT) h_minvMB->Fill(pTpion,minv,WEIGHT);
		if(pTpion>1.){
		  if(minv>0.08 && minv<0.20){
		    h_asymmMB->Fill(pTpion,asymm,WEIGHT);
		    if(asymm<=cuts->asymmetryCUT){
		      h_pionsVsEtaMB->Fill(pi0Mom.PseudoRapidity(),WEIGHT);
		      h_matrixMB->Fill(pTpion,ptInput,WEIGHT);
		    }
		  }
		}
	      }
	      if(ev->trigger()&2){
		if(asymm<=cuts->asymmetryCUT) h_minvHT1->Fill(pTpion,minv,WEIGHT);
		if(pTpion>4.){
		  if(minv>0.10 && minv<0.20){
		    h_asymmHT1->Fill(pTpion,asymm,WEIGHT);
		  }
		}
	      }
	      if(ev->trigger()&4){
		if(asymm<=cuts->asymmetryCUT) h_minvHT2->Fill(pTpion,minv,WEIGHT);
		if(pTpion>6.){
		  if(minv>0.10 && minv<0.2){
		    h_asymmHT2->Fill(pTpion,asymm,WEIGHT);
		  }
		}
	      }
	    

	    }
	

	}

      if(PHOTONS && nclusters>0) h_splitClusAll->Fill(mcTr->momentum().Pt());
      if(PHOTONS && nclusters>1) h_splitClus->Fill(mcTr->momentum().Pt());
            
      i++;
    }



  if(ETAS||PIONS||isPythia){//extract yield with regular routine:
    Pi0Analysis *pi0=new Pi0Analysis((const char*)(dirout+"_effbins.ps").Data(),"/dev/null",mFlag);
    pi0->init("/dev/null");
    if(!isPythia) pi0->setMC(kTRUE);
    h_recoMB=new TH1F(*pi0->getYield(h_minvMB,"mb"));
    h_recoHT1=new TH1F(*pi0->getYield(h_minvHT1,"ht1"));
    h_recoHT2=new TH1F(*pi0->getYield(h_minvHT2,"ht2"));
    pi0->storeCanvases((dirout+"_canvases.root").Data());
    delete pi0;
    // !!!! calculate from dN/dpt to #/bin:
    for(Int_t i=1;i<=h_recoMB->GetNbinsX();i++)
      {
	h_recoMB->SetBinContent(i,h_recoMB->GetBinContent(i)*h_recoMB->GetBinWidth(i));
	h_recoMB->SetBinError(i,h_recoMB->GetBinError(i)*h_recoMB->GetBinWidth(i));
      }
    for(Int_t i=1;i<=h_recoHT1->GetNbinsX();i++)
      {
	h_recoHT1->SetBinContent(i,h_recoHT1->GetBinContent(i)*h_recoHT1->GetBinWidth(i));
	h_recoHT1->SetBinError(i,h_recoHT1->GetBinError(i)*h_recoHT1->GetBinWidth(i));
      }
    for(Int_t i=1;i<=h_recoHT2->GetNbinsX();i++)
      {
	h_recoHT2->SetBinContent(i,h_recoHT2->GetBinContent(i)*h_recoHT2->GetBinWidth(i));
	h_recoHT2->SetBinError(i,h_recoHT2->GetBinError(i)*h_recoHT2->GetBinWidth(i));
      }
  }

  gStyle->SetPalette(1);
  gStyle->SetStatStyle(0);
  
  pseff->On();
  TCanvas *c=new TCanvas("c","c",600,800);
  c->Divide(3,4);
  c->cd(1);
  gPad->SetLogy();
  h_inputMB->SetLineColor(1);
  h_inputMB->SetLineWidth(2);
  h_inputMB->SetTitle((htitle+TString(" MB")).Data());
  h_inputMB->Draw("hist");
  h_recoMB->SetLineColor(1);
  h_recoMB->SetLineWidth(2);
  h_recoMB->SetLineStyle(2);
  if(h_recoMB->GetMinimum()>0.) h_inputMB->SetMinimum(h_recoMB->GetMinimum()/10. );
  h_recoMB->Draw("histsame");
  c->cd(2);
  gPad->SetLogy();
  h_inputHT1->SetLineColor(TColor::GetColor(24,101,24));
  h_inputHT1->SetLineWidth(2);
  h_inputHT1->SetTitle((htitle+TString(" HT1")).Data());
  h_inputHT1->Draw("hist");
  h_recoHT1->SetLineColor(TColor::GetColor(24,101,24));
  h_recoHT1->SetLineWidth(2);
  h_recoHT1->SetLineStyle(2);
  if(h_recoHT1->GetMinimum()>0.) h_inputHT1->SetMinimum(h_recoHT1->GetMinimum()/10. );
  h_recoHT1->Draw("histsame");
  c->cd(3);
  gPad->SetLogy();
  h_inputHT2->SetLineColor(TColor::GetColor(24,28,174));
  h_inputHT2->SetLineWidth(2);
  h_inputHT2->SetTitle((htitle+TString(" HT2")).Data());
  h_inputHT2->Draw("hist");  
  h_recoHT2->SetLineColor(TColor::GetColor(24,28,174));
  h_recoHT2->SetLineWidth(2);
  h_recoHT2->SetLineStyle(2);
  if(h_recoHT2->GetMinimum()>0.) h_inputHT2->SetMinimum(h_recoHT2->GetMinimum()/10. );
  h_recoHT2->Draw("histsame");

  c->cd(4);
  if(NEUTRONS||ANTINEUTRONS||KZEROLONG){
    gPad->SetLogy();
  }
  h_effMB=new TH1F(*h_recoMB);
  h_effMB->SetNameTitle("h_effMB","efficiency MB;p_{T};#epsilon");
  h_effMB->SetLineStyle(1);
  h_effMB->Divide(h_inputMB);
  h_effMB->Draw("pe");
  c->cd(5);
  if(NEUTRONS||ANTINEUTRONS||KZEROLONG){
    gPad->SetLogy();
  }
  h_effHT1=new TH1F(*h_recoHT1);
  h_effHT1->SetNameTitle("h_effHT1","efficiency HT1;p_{T};#epsilon");
  h_effHT1->SetLineStyle(1);
  h_effHT1->Divide(h_inputHT1);
  h_effHT1->Draw("pe");
  c->cd(6);
  if(NEUTRONS||ANTINEUTRONS||KZEROLONG){
    gPad->SetLogy();
  }
  h_effHT2=new TH1F(*h_recoHT2);
  h_effHT2->SetNameTitle("h_effHT2","efficiency HT2;p_{T};#epsilon");
  h_effHT2->SetLineStyle(1);
  h_effHT2->Divide(h_inputHT2);
  h_effHT2->Draw("pe");

  c->cd(7);
  gPad->SetLogy();
  h_inputDaughtersMB->SetLineColor(1);
  h_inputDaughtersMB->SetLineWidth(2);
  h_inputDaughtersMB->Draw("hist");
  h_recoDaughtersMB->SetLineColor(1);
  h_recoDaughtersMB->SetLineWidth(2);
  h_recoDaughtersMB->SetLineStyle(2);
  h_recoDaughtersMB->Draw("histsame");

  c->cd(8);
  gPad->SetLogy();
  h_inputDaughtersHT1->SetLineColor(TColor::GetColor(24,101,24));
  h_inputDaughtersHT1->SetLineWidth(2);
  h_inputDaughtersHT1->Draw("hist");
  h_recoDaughtersHT1->SetLineColor(TColor::GetColor(24,101,24));
  h_recoDaughtersHT1->SetLineWidth(2);
  h_recoDaughtersHT1->SetLineStyle(2);
  h_recoDaughtersHT1->Draw("histsame");
  c->cd(9);
  gPad->SetLogy();
  h_inputDaughtersHT2->SetLineColor(TColor::GetColor(24,28,174));
  h_inputDaughtersHT2->SetLineWidth(2);
  h_inputDaughtersHT2->Draw("hist");
  h_recoDaughtersHT2->SetLineColor(TColor::GetColor(24,28,174));
  h_recoDaughtersHT2->SetLineWidth(2);
  h_recoDaughtersHT2->SetLineStyle(2);
  h_recoDaughtersHT2->Draw("histsame");

  c->cd(10);
  h_effDaughtersMB=new TH1F(*h_recoDaughtersMB);
  h_effDaughtersMB->SetNameTitle("h_effDaughtersMB","efficiency daughters MB;p_{T};#epsilon");
  h_effDaughtersMB->SetLineStyle(1);
  h_effDaughtersMB->Divide(h_inputDaughtersMB);
  h_effDaughtersMB->Draw("pe");
  c->cd(11);
  h_effDaughtersHT1=new TH1F(*h_recoDaughtersHT1);
  h_effDaughtersHT1->SetNameTitle("h_effDaughtersHT1","efficiency daughters HT1;p_{T};#epsilon");
  h_effDaughtersHT1->SetLineStyle(1);
  h_effDaughtersHT1->Divide(h_inputDaughtersHT1);
  h_effDaughtersHT1->Draw("pe");
  c->cd(12);
  h_effDaughtersHT2=new TH1F(*h_recoDaughtersHT2);
  h_effDaughtersHT2->SetNameTitle("h_effDaughtersHT2","efficiency daughters HT2;p_{T};#epsilon");
  h_effDaughtersHT2->SetLineStyle(1);
  h_effDaughtersHT2->Divide(h_inputDaughtersHT2);
  h_effDaughtersHT2->Draw("pe");

  
  c->cd(0);
  c->Update();
  c->SaveAs((dirout+"_effplots.root").Data());
  pseff->Close();
    
  
  return 1;
}
Int_t Efficiency::finish()
{
  //save histograms
  mFileOut=new TFile((dirout+"_eff.root").Data(),"RECREATE");
  mFileOut->cd();

  h_genMB->Write();
  h_accMB->Write();

  h_HT1adc_id->Write();
  h_HT2adc_id->Write();

  h_splitClus->Write();
  h_splitClusAll->Write();

  h_convGen->Write();
  h_convConv->Write();
  h_convConvSSD->Write();
  h_convConvSVT->Write();
  h_convConvIFC->Write();

  h_convConvNotCtb->Write();
  h_stopRadius->Write();

  h_nbarDet->Write();
  h_nbarIn->Write();

  h_clusterWidth->Write();
  h_energyRatio->Write();
  h_towclusRatio->Write();

  h_smdeta1->Write();
  h_smdphi1->Write();
  h_smdeta2->Write();
  h_smdphi2->Write();


  h_pythiaPions->Write();
  h_pythiaPhotons->Write();
  h_pythiaPartonPt->Write();
  //
  h_mcdist->Write();
  h_mcdist2D->Write();
  h_dist->Write();
  h_dist2D->Write();

  h_effMB->Write();
  h_effHT1->Write();
  h_effHT2->Write();

  h_effDaughtersMB->Write();
  h_effDaughtersHT1->Write();
  h_effDaughtersHT2->Write();

  h_matrixMB->Write();

  h_etaphi->Write();

  h_minvMB->Write();
  h_minvHT1->Write();
  h_minvHT2->Write();

  h_asymmMB->Write();
  h_asymmHT1->Write();
  h_asymmHT2->Write();

  h_pionsVsEtaMB->Write();

  h_vzMB->Write();

  h_energyeta->Write();
  h_energyphi->Write();

  //
  mFileOut->Close();
  return 1;
}
Float_t Efficiency::getWeightPions(Float_t pT)
{
  if(pT<0.2) pT=0.2;
  float weight=1.;
  if(isPP05){
    float p[]={592.,-9.31,5.03,-7.55,-1.7,6.06};
    float WS=1. - 1./(1.+TMath::Exp(TMath::Abs(p[4])*(pT-p[5])));
    weight=WS*p[2]*pow(pT,p[3]);
    weight+=(1.- WS)*p[0]*pow(1.+pT,p[1]);
    weight*=pT;
    weight=7.0e+05*pT*pow(1.+pT,-9.3);
  }
  else if(isDAU){
    float p[]={39.3,-8.58,0.828,-7.27,0.835,5.18};
    float WS=1. - 1./(1.+exp(p[4]*(pT-p[5])));
    weight=WS*p[2]*pow(1.+pT,p[3]);
    weight+=(1.- WS)*p[0]*pow(1.+pT,p[1]);
    weight*=pT;
    weight=7.0e+05*pT*pow(1.+pT,-9.3);
  }

  if(!USEWEIGHT) weight=1.;
  if(USEPYTHIAWEIGHT) weight=exp(-0.401*pT);
  return weight;
}
Float_t Efficiency::getWeightEtas(Float_t pT)
{
  Float_t ME=0.548;//eta meson mass
  Float_t MP=0.135;//pi0 mass
  Float_t weight=0.45*pT/sqrt(pT*pT+ME*ME-MP*MP)*getWeightPions(sqrt(pT*pT+ME*ME-MP*MP));
  if(!USEWEIGHT) weight=1.;
  return weight;
}
Float_t Efficiency::getWeightAntiNeutrons(Float_t pT)
{
  //hadron data for Levy function, nucl-ex/0601033:
  //-->  d^2N/(2*pi*pT*N*dpT*dy) = B/((1+((mT - m0)/nT))^n)
  // {p-dAu; pbar-dAu; p-pp; pbar-pp} and m0 = m_neutron = 1.0 GeV.
  //Double_t B[]={0.3,0.23,0.072,0.061};//->[0]
  //Double_t eB[]={0.01,0.01,0.005,0.005};
  Double_t T[]={0.205,0.215,0.179,0.173};//->[1]
  //Double_t eT[]={0.004,0.005,0.006,0.006};
  Double_t n[]={11.00,12.55,10.87,10.49};//->[2]
  //Double_t en[]={0.29,0.41,0.43,0.40};
  
  if(pT<0.5) pT=0.5;
  Float_t weight=0.;
  Int_t bin=-1;
  if(isDAU) bin=1;
  if(isPP05) bin=3;
  weight=pT/pow((1.+(sqrt(pT*pT+1.) - 1.)/(T[bin]*n[bin])),n[bin]);
  if(isPP05 && ANTINEUTRONS) weight*=exp(0.5*pT);
  if(KZEROLONG) weight=getWeightPions(pT);
  return weight;
}
Float_t Efficiency::getWeightNeutrons(Float_t pT)
{
  //hadron data for Levy function, nucl-ex/0601033:
  //-->  d^2N/(2*pi*pT*N*dpT*dy) = B/((1+((mT - m0)/nT))^n)
  // {p-dAu; pbar-dAu; p-pp; pbar-pp} and m0 = m_neutron = 1.0 GeV.
  //Double_t B[]={0.3,0.23,0.072,0.061};//->[0]
  //Double_t eB[]={0.01,0.01,0.005,0.005};
  Double_t T[]={0.205,0.215,0.179,0.173};//->[1]
  //Double_t eT[]={0.004,0.005,0.006,0.006};
  Double_t n[]={11.00,12.55,10.87,10.49};//->[2]
  //Double_t en[]={0.29,0.41,0.43,0.40};

  if(pT<0.5) pT=0.5;
  Float_t weight=0.;
  Int_t bin=-1;
  if(isDAU) bin=0;
  if(isPP05) bin=2;
  weight=pT/pow((1.+(sqrt(pT*pT+1.) - 1.)/(T[bin]*n[bin])),n[bin]);
  return weight;
}
Float_t Efficiency::getWeightPhotons(Float_t pT)
{
  if(pT<0.2) pT=0.2;
  return getWeightPions(pT);  
}
Float_t Efficiency::getWeightVertex(Float_t z)
{
  Float_t weight=1.;
  if(isDAU){
    weight=1.04 + 0.00547*z - 0.00016*z*z - 2.8e-06*z*z*z;
    weight=weight + 4.7e-08*z*z*z*z + 4.5e-10*z*z*z*z*z;
  }
  if(isPP05){
    float sigma_data2=50.*50.;
    float sigma_mc2=60.*60.;
    float factor=(1./sigma_data2)-(1./sigma_mc2);
    weight=exp(-0.5*z*z*factor);    
  }
  return weight;
}
