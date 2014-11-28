#include <stdio.h>
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
#include <StEmcPool/StPhotonCommon/MyMcTrack.h> 

#include "AnaCuts.h"
#include "EventMixer.h"

#include "Pi0Analysis.h"
using namespace std;

//temp solution
Bool_t MINBIAS=kTRUE;
Bool_t EXCLUDERANGE=kTRUE;
Bool_t EXCLUDEETA=kFALSE;
Bool_t doNEUTRONS=kFALSE;
Float_t rightPI=0.22;
Float_t leftPI=0.08;//was 0.12
Double_t CONSTANT=0.;
Double_t combFit(Double_t *x,Double_t *par)
{
  //old settings:
  //leftPI=0.08;
  //rightPI=0.24;

  if( (x[0]>leftPI&&x[0]<rightPI) && EXCLUDERANGE )
    {
      TF1::RejectPoint();
      return 0;
    }
  if( (x[0]>0.5&&x[0]<0.65) && EXCLUDEETA )//thesis result: 0.48
    {
      TF1::RejectPoint();
      return 0;
    }
 
  
  Double_t ret=par[0]*x[0]+par[1]*x[0]*x[0]+par[2]*x[0]*x[0]*x[0]+CONSTANT;
  
  return ret;
}
Double_t sumFit(Double_t *x,Double_t *par)
{
  Double_t ret=par[0]*x[0]+par[1]*x[0]*x[0]+par[2]*x[0]*x[0]*x[0];
  Double_t w=x[0]-par[4];
  Double_t s=par[5];
  Double_t ret2=par[3]*TMath::Exp(-0.5*(w/s)*(w/s));
  return ret+ret2;
}

ClassImp(Pi0Analysis)

Pi0Analysis::Pi0Analysis(const Char_t *psout, const Char_t *psout2, const Char_t *flag)
{
  gStyle->SetOptDate(0);
  ps=new TPostScript(psout,-111);
  ps2=new TPostScript(psout2,-111);

  c_array=new TObjArray(100);


  cout<<"DEFAULT CONSTRUCTOR FOR PI0ANALYSIS!!!!"<<endl;
  cout<<"storing ps in: "<<psout<<endl;

  //init cuts
  cuts=new AnaCuts(flag);
  //cuts->printCuts();

  //eventmixer
  mixer=new EventMixer(flag);
  
  runPrev=0;
  startdatePrev=0;
  starttimePrev=0;
  numberOfMB=0;
  nMBinRun=0;
  numberOfHT1=0;
  nHT1inRun=0;
  numberOfHT2=0;
  nHT2inRun=0;
  psMB=0;
  psHT1=0;
  psHT2=0;

  psHT1_eff=0;
  psHT1_eff2=0;
  nMB_eff=0;

  isMC=kFALSE;
  isPythia=kFALSE;
  isDAU=kFALSE;
  isPP05=kFALSE;
  isAUAU200=kFALSE;
  isHIJING=kFALSE;
  if(strcmp(flag,"dAu")==0){
    isDAU=kTRUE;
  }
  if(strcmp(flag,"pp05")==0){
    isPP05=kTRUE;
  }
  if(strcmp(flag,"auau200")==0){
    isAUAU200=kTRUE;
  }

  noMINBIAS=kFALSE;

  WEIGHT=1.;
  iev_0=0;
  iev_1=0;
  iev_2=0;

}
Pi0Analysis::~Pi0Analysis()
{
  cout<<endl<<"Pi0Analysis destructed!"<<endl<<endl;
}
Int_t Pi0Analysis::init(const Char_t *output)
{

  mFileOut=new TFile(output,"RECREATE");

  h_bbcVsTpc=new TH2F("h_bbcVsTpc","BBC vs TPC vertex z",481,-240.5,240.5,480,-240.,240.);
  h_bbcVsTpcCorr=new TH2F("h_bbcVsTpcCorr","corrected BBC vs TPC vertex z",481,-240.5,240.5,480,-240.,240.);
  h_bbcRes=new TH1F("h_bbcres","bbc resolution",100,-2.,2.);

  h_smdeta1=new TH2F("h_smdeta1","n eta strips in cluster vs p_{T}",20,0.,10.,10,0.5,10.5);
  h_smdphi1=new TH2F("h_smdphi1","n phi strips in cluster vs p_{T}",20,0.,10.,10,0.5,10.5);
  h_smdeta2=new TH2F("h_smdeta2","eta energy ratio in cluster vs p_{T}",20,0.,10.,100,0.0,10.);
  h_smdphi2=new TH2F("h_smdphi2","phi energy ratio in cluster vs p_{T}",20,0.,10.,100,0.0,10.);

  h_ratioMB=new TH1F("ratioMB","E neutral over E total",60,-.2,1.);
  h_ratioMB->Sumw2();
  h_ratioHT1=new TH1F("ratioHT1","E neutral over E total",60,-.2,1.);
  h_ratioHT1->Sumw2();
  h_ratioHT2=new TH1F("ratioHT2","E neutral over E total",60,-.2,1.);
  h_ratioHT2->Sumw2();
  h_vzMB=new TH1F("h_vzMB","z-vertex MB",480,-120.,120.);
  h_vzMB->Sumw2();
  h_vzHT1=new TH1F("h_vzHT1","z-vertex HT1",480,-120.,120.);
  h_vzHT1->Sumw2();
  h_vzHT2=new TH1F("h_vzHT2","z-vertex HT2",480,-120.,120.);
  h_vzHT2->Sumw2();
  h_trigidHT1=new TH1F("h_trigidHT1","id of trigger HT1",4800,0.5,4800.5);
  h_trigidHT1->Sumw2();
  h_trigidHT2=new TH1F("h_trigidHT2","id of trigger HT2",4800,0.5,4800.5);
  h_trigidHT2->Sumw2();
  h_trigidHT1aftercut=new TH1F("h_trigidHT1aftercut","id of trigger HT1 after cut",4800,0.5,4800.5);
  h_trigidHT1aftercut->Sumw2();
  h_trigidHT2aftercut=new TH1F("h_trigidHT2aftercut","id of trigger HT2 after cut",4800,0.5,4800.5);
  h_trigidHT2aftercut->Sumw2();
  h_events=new TH1F("h_events","trigger counts: mb=+1,ht1=+2,ht2=+4,sn=+8",21,-0.5,20.5);
  h_events->Sumw2();

  h_HT1adc_id=new TH2F("h_HT1adc_id","ht-1 adc vs id",2400,.5,2400.5,800,200.5,1000.5);
  h_HT2adc_id=new TH2F("h_HT2adc_id","ht-2 adc vs id",2400,.5,2400.5,800,200.5,1000.5);

  h_etaphi=new TH2F("h_etaphi","eta/phi of neutral points",300,0.0,1.0,1800,-3.14,3.14);
  h_etaphi->Sumw2();
  h_rapphi=new TH2F("h_rapphi","rap/phi of neutral points",300,-0.2,1.2,1800,-3.14,3.14);
  h_rapphi->Sumw2();
  h_dist=new TH1F("h_dist","distance of point to track",1000,0.0,200.0);
  h_dist->Sumw2();

  h_dist2DMB=new TH2F("h_dist2DMB","dist. vs pt MB",cuts->nPtBinsMB,cuts->ptBinsMB.GetArray(),1000,0.0,1000.0);
  h_dist2DHT1=new TH2F("h_dist2DHT1","dist. vs pt HT1",cuts->nPtBinsHT1,cuts->ptBinsHT1.GetArray(),1000,0.0,1000.0);
  h_dist2DHT2=new TH2F("h_dist2DHT2","dist. vs pt HT2",cuts->nPtBinsHT2,cuts->ptBinsHT2.GetArray(),1000,0.0,1000.0);

  h_dist2DMBpions=new TH2F("h_dist2DMBpions","dist. vs pt MB",cuts->nPtBinsMB,cuts->ptBinsMB.GetArray(),1000,0.0,1000.0);
  h_dist2DHT1pions=new TH2F("h_dist2DHT1pions","dist. vs pt HT1",cuts->nPtBinsHT1,cuts->ptBinsHT1.GetArray(),1000,0.0,1000.0);
  h_dist2DHT2pions=new TH2F("h_dist2DHT2pions","dist. vs pt HT2",cuts->nPtBinsHT2,cuts->ptBinsHT2.GetArray(),1000,0.0,1000.0);
  
  h_asymmMB=new TH2F("h_asymmMB","asymmetry of inv mass pairs MB",cuts->nPtBinsMB,cuts->ptBinsMB.GetArray(),40,0.0,1.0);
  h_asymmMB->Sumw2();
  h_asymmHT1=new TH2F("h_asymmHT1","asymmetry of inv mass pairs HT1",cuts->nPtBinsHT1,cuts->ptBinsHT1.GetArray(),40,0.0,1.0);
  h_asymmHT1->Sumw2();
  h_asymmHT2=new TH2F("h_asymmHT2","asymmetry of inv mass pairs HT2",cuts->nPtBinsHT2,cuts->ptBinsHT2.GetArray(),40,0.0,1.0);
  h_asymmHT2->Sumw2();
  h_asymmMBbg=new TH2F("h_asymmMBbg","asymmetry of inv mass pairs MB (bg)",cuts->nPtBinsMB,cuts->ptBinsMB.GetArray(),40,0.0,1.0);
  h_asymmMBbg->Sumw2();
  h_asymmHT1bg=new TH2F("h_asymmHT1bg","asymmetry of inv mass pairs HT1 (bg)",cuts->nPtBinsHT1,cuts->ptBinsHT1.GetArray(),40,0.0,1.0);
  h_asymmHT1bg->Sumw2();
  h_asymmHT2bg=new TH2F("h_asymmHT2bg","asymmetry of inv mass pairs HT2 (bg)",cuts->nPtBinsHT2,cuts->ptBinsHT2.GetArray(),40,0.0,1.0);
  h_asymmHT2bg->Sumw2();

  h_minvMB=new TH2F("h_minvMB","invariant mass vs pT MB",cuts->nPtBinsMB,cuts->ptBinsMB.GetArray(),cuts->nMinvBinsMB,cuts->mInvLowMB,cuts->mInvHighMB);
  h_minvMB->Sumw2();
  h_minvHT1=new TH2F("h_minvHT1","invariant mass vs pT HT1",cuts->nPtBinsHT1,cuts->ptBinsHT1.GetArray(),cuts->nMinvBinsHT1,cuts->mInvLowHT1,cuts->mInvHighHT1);
  h_minvHT1->Sumw2();
  h_minvHT2=new TH2F("h_minvHT2","invariant mass vs pT HT2",cuts->nPtBinsHT2,cuts->ptBinsHT2.GetArray(),cuts->nMinvBinsHT2,cuts->mInvLowHT2,cuts->mInvHighHT2);
  h_minvHT2->Sumw2();

  h_pionsVsEtaMB=new TH1F("h_pionsVsEta","dN_{#pi^{0}}/dy data",40,-0.5,1.5);

  h_gammaMB=new TH1F("h_gammaMB","photon yield vs pT MB",cuts->nPtBinsMB,cuts->ptBinsMB.GetArray());
  h_gammaMB->Sumw2();
  h_gammaHT1=new TH1F("h_gammaHT1","photon yield vs pT HT1",cuts->nPtBinsHT1,cuts->ptBinsHT1.GetArray());
  h_gammaHT1->Sumw2();
  h_gammaHT2=new TH1F("h_gammaHT2","photon yield vs pT HT2",cuts->nPtBinsHT2,cuts->ptBinsHT2.GetArray());
  h_gammaHT2->Sumw2();

  h_pythiaPartonPt=new TH1F("h_pythiaPartonPt","pT of pythia process",200,0.,50);  
  h_pythiaPartonPt->Sumw2();
  h_pythiaPions=new TH1F("h_pythiaPions","pythia pions vs pT",200,0.,50.);
  h_pythiaPions->Sumw2();
  h_pythiaPionsMB=new TH1F("h_pythiaPionsMB","pythia pions vs pT MB",cuts->nPtBinsMB,cuts->ptBinsMB.GetArray());
  h_pythiaPionsMB->Sumw2();
  h_pythiaPionsHT1=new TH1F("h_pythiaPionsHT1","pythia pions vs pT HT1",cuts->nPtBinsHT1,cuts->ptBinsHT1.GetArray());
  h_pythiaPionsHT1->Sumw2();
  h_pythiaPionsHT2=new TH1F("h_pythiaPionsHT2","pythia pions vs pT HT2",cuts->nPtBinsHT2,cuts->ptBinsHT2.GetArray());
  h_pythiaPionsHT2->Sumw2();

  isHot=(TArrayI)cuts->isHot;

  h_adcHT1=new TH1F("h_adcHT1","highest adc en>0. for HT1 after cuts",1000,0.,1000.);
  h_adcHT2=new TH1F("h_adcHT2","highest adc en>0. for HT2 after cuts",1000,0.,1000.);

  h_mcneutronsMB=new TH1F("h_mcneutronsMB","gen. neutrons vs pT MB",cuts->nPtBinsEffMB,cuts->ptBinsEffMB.GetArray());
  h_mcneutronsHT1=new TH1F("h_mcneutronsHT1","gen. neutrons vs pT HT1",cuts->nPtBinsEff,cuts->ptBinsEff.GetArray());
  h_mcneutronsHT2=new TH1F("h_mcneutronsHT2","gen. neutrons vs pT HT2",cuts->nPtBinsEff,cuts->ptBinsEff.GetArray());
  h_mcneutronsWeightMB=new TH1F("h_mcneutronsWeightMB","gen. neutrons vs pT (weighted) MB",cuts->nPtBinsEff,cuts->ptBinsEff.GetArray());
  h_mcneutronsWeightHT1=new TH1F("h_mcneutronsWeightHT1","gen. neutrons vs pT (weighted) HT1",cuts->nPtBinsEff,cuts->ptBinsEff.GetArray());
  h_mcneutronsWeightHT2=new TH1F("h_mcneutronsWeightHT2","gen. neutrons vs pT (weighted) HT2",cuts->nPtBinsEff,cuts->ptBinsEff.GetArray());
  h_neutronsMB=new TH1F("h_neutronsMB","reco. neutrons vs pT MB",cuts->nPtBinsEff,cuts->ptBinsEff.GetArray());
  h_neutronsHT1=new TH1F("h_neutronsHT1","reco. neutrons vs pT HT1",cuts->nPtBinsEff,cuts->ptBinsEff.GetArray());
  h_neutronsHT2=new TH1F("h_neutronsHT2","reco. neutrons vs pT HT2",cuts->nPtBinsEff,cuts->ptBinsEff.GetArray());

  h_EvsE=new TH2F("h_EvsE","E neutral vs E in TPC",80,0.,40,80,0.,40.);

  h_nstripsETA=new TH2F("h_nstripsETA","n strips vs pt bsmde",15,0.,15.,6,0.,6.);
  h_nstripsPHI=new TH2F("h_nstripsPHI","n strips vs pt bsmdp",15,0.,15.,6,0.,6.);

  h_clusterWidth=new TH2F("h_clusterWidth","width BSMD eta+phi",20,0.,20.,100,0.,0.05);
  h_clusterWidth->Sumw2();
  h_energyRatio=new TH2F("h_energyRatio","energy ratio BSMD/BTOW",20,0.,20.,160,0.,8.);
  h_energyRatio->Sumw2();

  if(isPP05){
    fout_mb.open("timestamps/pp_timestamps_mb.list");
    fout_ht1.open("timestamps/pp_timestamps_ht1.list");
    fout_ht2.open("timestamps/pp_timestamps_ht2.list");
  }
  if(isDAU){
    fout_mb.open("timestamps/dau_timestamps_mb.list");
    fout_ht1.open("timestamps/dau_timestamps_ht1.list");
    fout_ht2.open("timestamps/dau_timestamps_ht2.list");
  }

  return 1; 
}
Int_t Pi0Analysis::make(Int_t evmax, const Char_t *input)
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
    if(i%500000==0) cout<<"processing "<<input<<" evt: "<<i<<endl;

    if(i==0){
      startdatePrev=ev->date();
      starttimePrev=ev->time();
    }

    //assign trigger for MC event:
    Int_t mcTr=1;//mb
    if(isMC&&!isPythia){
      if(ev->highTowerAdc()>cuts->ht1AdcCUT) mcTr+=2;
      if(ev->highTowerAdc()>cuts->ht2AdcCUT) mcTr+=4;
      ev->setTrigger(mcTr);
    }

    TVector3 vPos=ev->vertex();

    float bbc_vertz=(ev->bbcVertexZ()-10.77)/2.82;
    //now bbc_vert is west-east, then from correlation:
    bbc_vertz+=4.04;
    bbc_vertz/=0.354;
    ev->setBbcVertexZ(bbc_vertz);

    if(ev->trigger()&1 && TMath::Abs(vPos.Z())>0.00000001){
      h_bbcVsTpc->Fill(vPos.Z(),ev->bbcVertexZ());
    }
   
    if(ev->trigger()&1) {h_vzMB->Fill(vPos.Z());}
    if(ev->trigger()&2) {h_vzHT1->Fill(vPos.Z());h_trigidHT1->Fill(ev->highTowerId());}
    if(ev->trigger()&4) {h_vzHT2->Fill(vPos.Z());h_trigidHT2->Fill(ev->highTowerId());}

    if(ev->trigger()&4 && TMath::Abs(vPos.Z())<60.){h_EvsE->Fill(ev->momentumInTpc(),ev->energyInBarrel());}
    
    //set tpc momentum for simu:
    if(isMC) ev->setMomentumInTpc(99999.);

    //exclude depleted minbias
    if(noMINBIAS && ev->trigger()&1){
      int trigg=ev->trigger();
      ev->setTrigger(trigg-1);
    }

    //event cuts + hot tower:
    if(isDAU && !cuts->isEventOK(ev,"dAu")) {i++;continue;}
    if(isPP05 && !cuts->isEventOK(ev,"pp05")) {i++;continue;}    



    //fill mixer
    //if(isDAU || isPP05) mixer->addEvent(ev);

    //check resolution bbc:
    if(isPP05 && TMath::Abs(vPos.Z())>0.){
      float bbcres=(ev->bbcVertexZ()-vPos.Z())/vPos.Z();
      h_bbcRes->Fill(bbcres);
    }

    Float_t ratioE=TMath::Abs(ev->energyInBarrel()+ev->momentumInTpc())>0. ? ev->energyInBarrel()/(ev->energyInBarrel()+ev->momentumInTpc()) : -.1;

    if(i>0 && ev->runId()!=runPrev){
      psHT1_eff+=psHT1*nHT1inRun;
      nMB_eff+=psMB*nMBinRun;

      fout_mb<<runPrev<<" "<<startdatePrev<<" "<<starttimePrev<<" "<<nMBinRun<<endl;
      fout_ht1<<runPrev<<" "<<startdatePrev<<" "<<starttimePrev<<" "<<nHT1inRun<<endl;
      fout_ht2<<runPrev<<" "<<startdatePrev<<" "<<starttimePrev<<" "<<nHT2inRun<<endl;
  
      nMBinRun=0;
      nHT1inRun=0;
      nHT2inRun=0;
      startdatePrev=ev->date();
      starttimePrev=ev->time();
    }
    if(ev->trigger()&1) {nMBinRun++;}
    if(ev->trigger()&2) {nHT1inRun++;}
    if(ev->trigger()&4) {nHT2inRun++;}
    psMB=(ev->prescale(0) > ev->prescale(1)) ? (Float_t)ev->prescale(0) : (Float_t)ev->prescale(1);
    psHT1=(Float_t)ev->prescale(2);
    psHT2=(Float_t)ev->prescale(3);
    
    //get neutrons:
    for(Int_t j=0;j<ev->numberOfMcPions();j++){
      MyMcTrack *pyt=(MyMcTrack*)ev->getMcPionArray()->At(j);
      //check if neutron in rapidity bite:
      //change this to rapidity...
      if(pyt->momentum().PseudoRapidity()<cuts->rapidityMinCUT||pyt->momentum().PseudoRapidity()>cuts->rapidityMaxCUT) continue;
      h_mcneutronsMB->Fill(pyt->momentum().Pt());
      h_mcneutronsHT1->Fill(pyt->momentum().Pt());
      h_mcneutronsHT2->Fill(pyt->momentum().Pt());
      Float_t weight=getWeight(pyt->momentum().Pt());
      h_mcneutronsWeightMB->Fill(pyt->momentum().Pt(),weight);
      h_mcneutronsWeightHT1->Fill(pyt->momentum().Pt(),weight);
      h_mcneutronsWeightHT2->Fill(pyt->momentum().Pt(),weight);
    }

    if(ev->trigger()&1){
      h_ratioMB->Fill(ratioE);
      numberOfMB++;
    }
    if(ev->trigger()&2){
      h_ratioHT1->Fill(ratioE);
      numberOfHT1++;
      h_trigidHT1aftercut->Fill(ev->highTowerId());

      h_adcHT1->Fill(ev->highTowerAdc());
      h_HT1adc_id->Fill(ev->highTowerId(),ev->highTowerAdc());
      }
    if(ev->trigger()&4){
      h_ratioHT2->Fill(ratioE);
      numberOfHT2++;
      h_trigidHT2aftercut->Fill(ev->highTowerId());

      h_adcHT2->Fill(ev->highTowerAdc());
      h_HT2adc_id->Fill(ev->highTowerId(),ev->highTowerAdc());
    }

    h_events->Fill(ev->trigger());

    if(isPythia){
      Float_t pypT=ev->partonPt();
      if(pypT<15.){
	WEIGHT=1.;
	iev_0++;
      }
      else if(pypT<25.){
	WEIGHT=(216000/208000)*(0.0003895/0.002228);
	iev_1++;
      }
      else if(pypT>25.){
	iev_2++;
	WEIGHT=(216000/208000)*(1.016e-005/0.002228);
      }
      h_pythiaPartonPt->Fill(ev->partonPt(),WEIGHT);
      for(Int_t it=0;it<ev->getMcPionArray()->GetEntries();it++){
	MyMcTrack *pyt=(MyMcTrack*)ev->getMcPionArray()->At(it);
	if(pyt->momentum().PseudoRapidity()<cuts->rapPionMinCUT||pyt->momentum().PseudoRapidity()>cuts->rapPionMaxCUT) continue;
	h_pythiaPions->Fill(pyt->momentum().Pt(),WEIGHT);
	if(ev->trigger()&1){
	  h_pythiaPionsMB->Fill(pyt->momentum().Pt(),WEIGHT);
	}
	if(ev->trigger()&2){
          h_pythiaPionsHT1->Fill(pyt->momentum().Pt(),WEIGHT);
        }
	if(ev->trigger()&4){
          h_pythiaPionsHT2->Fill(pyt->momentum().Pt(),WEIGHT);
        }
      }
    }

    runPrev=ev->runId();
    //loop on points
    MyPoint *p;
    MyPoint *pp;
    TClonesArray *clA=(TClonesArray*)ev->getPointArray();
    for(Int_t j=0;j<clA->GetEntries();j++){
      p=(MyPoint*)clA->At(j);
      TVector3 pPos=p->position();
      TVector3 pMom=pPos - vPos;
      pMom.SetMag(p->energy());

      if(!cuts->isPointOK(p,vPos)) continue;
      
      h_dist->Fill(p->distanceToTrack());
      if(ev->trigger()&1) h_dist2DMB->Fill(pMom.Pt(),p->distanceToTrack());
      if(ev->trigger()&2) h_dist2DHT1->Fill(pMom.Pt(),p->distanceToTrack());
      if(ev->trigger()&4) h_dist2DHT2->Fill(pMom.Pt(),p->distanceToTrack());

      if(ev->trigger()&1 && pMom.Pt()>1.){
	h_etaphi->Fill(pPos.PseudoRapidity(),pPos.Phi());
	h_rapphi->Fill(pMom.PseudoRapidity(),pMom.Phi());
      }

      if(pMom.PseudoRapidity()>cuts->rapidityMinCUT && pMom.PseudoRapidity()<cuts->rapidityMaxCUT){
	//first neutrons:
	if(doNEUTRONS){
	  Float_t D=1000000.;
	  Float_t pt4weight=10.;
	  for(Int_t j=0;j<ev->numberOfMcPions();j++){
	    MyMcTrack *pyt=(MyMcTrack*)ev->getMcPionArray()->At(j);
	    TVector3 mcPos=pyt->position();
	    Float_t n=mcPos.PseudoRapidity();
	    Float_t p=mcPos.Phi();
	    Float_t nn=pPos.PseudoRapidity();
	    Float_t pp=pPos.Phi();
	    Float_t d=sqrt(pow(p-pp,2)+pow(n-nn,2));
	    if(d<D){
	      D=d;
	      pt4weight=pyt->momentum().Pt();
	    }
	  }
	  if(D<0.025){
	    if(ev->trigger()&1) h_neutronsMB->Fill(pMom.Pt(),getWeight(pt4weight));
	    if(ev->trigger()&2) h_neutronsHT1->Fill(pMom.Pt(),getWeight(pt4weight));
	    if(ev->trigger()&4) h_neutronsHT2->Fill(pMom.Pt(),getWeight(pt4weight));
	  }
	}
	
	//fill photons:
	if(p->distanceToTrack()>cuts->photonCUT){
	  if(ev->trigger()&1) h_gammaMB->Fill(pMom.Pt(),WEIGHT);
	  if(ev->trigger()&2) h_gammaHT1->Fill(pMom.Pt(),WEIGHT);
	  if(ev->trigger()&4) h_gammaHT2->Fill(pMom.Pt(),WEIGHT);
	  
	  if(ev->trigger()&1){
	    h_clusterWidth->Fill(pMom.Pt(),sqrt(p->widthEta()*p->widthEta()+p->widthPhi()*p->widthPhi()));
	    h_energyRatio->Fill(pMom.Pt(),(p->energyEta()+p->energyPhi())/p->energy());
	    h_nstripsETA->Fill(pMom.Pt(),p->nHitsEta(),WEIGHT);
	    h_nstripsPHI->Fill(pMom.Pt(),p->nHitsPhi(),WEIGHT);
	    //super isolated:
	    if(p->distanceToTrack()>30.){
	      h_smdeta1->Fill(pMom.Pt(),p->nHitsEta());
	      h_smdphi1->Fill(pMom.Pt(),p->nHitsPhi());
	      h_smdeta2->Fill(pMom.Pt(),p->energyEta()/p->energy());
	      h_smdphi2->Fill(pMom.Pt(),p->energyPhi()/p->energy());
	    }
	  }
	}
	
      }
      
      for(Int_t jj=0;jj<clA->GetEntries();jj++){
	if(jj<=j) continue;
	pp=(MyPoint*)clA->At(jj);
	TVector3 ppPos=pp->position();
	TVector3 ppMom=ppPos - vPos;
	ppMom.SetMag(pp->energy());
	if(!cuts->isPointOK(pp,vPos)) continue;

	if(p->distanceToTrack()<pp->distanceToTrack()){
	  if(ev->trigger()&1) h_dist2DMBpions->Fill(pMom.Pt(),p->distanceToTrack());
	  if(ev->trigger()&2) h_dist2DHT1pions->Fill(pMom.Pt(),p->distanceToTrack());
	  if(ev->trigger()&4) h_dist2DHT2pions->Fill(pMom.Pt(),p->distanceToTrack());
	}
	else{
	  if(ev->trigger()&1) h_dist2DMBpions->Fill(pMom.Pt(),pp->distanceToTrack());
          if(ev->trigger()&2) h_dist2DHT1pions->Fill(pMom.Pt(),pp->distanceToTrack());
          if(ev->trigger()&4) h_dist2DHT2pions->Fill(pMom.Pt(),pp->distanceToTrack());
	}
	   
	//two neutrals
	TVector3 pi0Mom=pMom+ppMom;
	Float_t angle=pMom.Angle(ppMom);
	Float_t Epion=p->energy()+pp->energy();
	Float_t asymm=TMath::Abs(p->energy()-pp->energy())/Epion;
	Float_t minv=sqrt(2.*p->energy()*pp->energy()*(1. - cos(angle)));
	Float_t pTpion=pi0Mom.Pt();
	
	//select rapidity bite / asymm cut for pions:
	if(pi0Mom.PseudoRapidity()<cuts->rapPionMinCUT||pi0Mom.PseudoRapidity()>cuts->rapPionMaxCUT) continue;
		
	if(ev->trigger()&1){
	  if(asymm<=cuts->asymmetryCUT) h_minvMB->Fill(pTpion,minv,WEIGHT);
	  if(pTpion>1.){
	    if(minv>0.08 && minv<0.20){
	    h_asymmMB->Fill(pTpion,asymm);
	    if(asymm<=cuts->asymmetryCUT) h_pionsVsEtaMB->Fill(pi0Mom.PseudoRapidity());
	    }
	    else h_asymmMBbg->Fill(pTpion,asymm);
	  }
	}
	if(ev->trigger()&2){
	  if(asymm<=cuts->asymmetryCUT) h_minvHT1->Fill(pTpion,minv,WEIGHT);
	  if(pTpion>4.){
	    if(minv>0.10 && minv<0.20){
	      h_asymmHT1->Fill(pTpion,asymm);
	    }
	    else h_asymmHT1bg->Fill(pTpion,asymm);
	  }
	}
	if(ev->trigger()&4){
	  if(asymm<=cuts->asymmetryCUT) h_minvHT2->Fill(pTpion,minv,WEIGHT);
	  if(pTpion>6.){
	    if(minv>0.10 && minv<0.2){
              h_asymmHT2->Fill(pTpion,asymm);
	    }
	    else h_asymmHT2bg->Fill(pTpion,asymm);
	  }
	}
      
      }


    }
    
    
    i++;
  }
  
  return 1;
}

void Pi0Analysis::printPrescales()
{
  cout<<"--------------------------------"<<endl;
  cout<<"      stats processed:          "<<endl;
  cout<<"                                "<<endl;
  cout<<" number of mb:  "<<numberOfMB<<endl;
  cout<<" number of ht1: "<<numberOfHT1<<endl;
  cout<<" number of ht2: "<<numberOfHT2<<endl;
  cout<<"--------------------------------"<<endl;
  cout<<" prescale corrections:          "<<endl;
  cout<<"                                "<<endl;
  cout<<" psHT1_eff "<<psHT1_eff/(1.*numberOfHT1)<<endl;
  cout<<endl;
  cout<<" N_mb*ps_mb: "<<nMB_eff<<endl;
  cout<<"--------------------------------"<<endl;
}
Int_t Pi0Analysis::finish()
{
  h_minvMB_mixed=new TH2F(*(TH2F*)mixer->getMinvMB());
  mFileOut->Add(h_minvMB_mixed);
  mFileOut->Write();

  return 1;
}

void Pi0Analysis::getYield()
{
  h_yieldMB=new TH1F(*getYield(NULL,"mb"));
  h_yieldHT1=new TH1F(*getYield(NULL,"ht1"));
  h_yieldHT2=new TH1F(*getYield(NULL,"ht2"));
}

//new routine:
TH1F *Pi0Analysis::getYield(TH2F *h, const Char_t *flag)
{
  ps->On();
  if(!h){
    cout<<"zero pointer"<<endl;
    if(strcmp(flag,"mb")==0) h=new TH2F(*h_minvMB);
    if(strcmp(flag,"ht1")==0) h=new TH2F(*h_minvHT1);
    if(strcmp(flag,"ht2")==0) h=new TH2F(*h_minvHT2);
  }
  TString retname(h->GetName());
  retname.Append("_yield");
  TString rettitle(h->GetTitle());
  rettitle.Append("_yield");
  TH1F *retYield=new TH1F(*((TH1F*)h->ProjectionX()));
  retYield->Reset();
  retYield->Sumw2();
  retYield->SetNameTitle(retname.Data(),rettitle.Data());

  Char_t canvasname[100];
  int page=0;
  TCanvas *c_copy;

  Float_t NSIGMAHI=3.;
  Float_t NSIGMALO=3.;

  const int nx=h->GetNbinsX();
  Float_t *x=new Float_t[nx];
  Float_t *ex=new Float_t[nx];
  Float_t *y=new Float_t[nx];
  Float_t *ey=new Float_t[nx];
  Float_t *s=new Float_t[nx];
  Float_t *es=new Float_t[nx];
  Float_t *m=new Float_t[nx];
  Float_t *em=new Float_t[nx];
  
  cout<<"------------- "<<flag<<" --------------"<<endl;

  Int_t color=1;
  Int_t cbgcolor=1;
  Int_t peakcolor=1;
  Int_t fillcolor=16;
  MINBIAS=kTRUE;
  if(strcmp(flag,"ht1")==0) {color=4; cbgcolor=2; peakcolor=1; fillcolor=38; MINBIAS=kFALSE;}
  if(strcmp(flag,"ht2")==0) {color=2; cbgcolor=4; peakcolor=1; fillcolor=46; MINBIAS=kFALSE;}

  TCanvas *c=new TCanvas(flag,flag,600,600);
  c->Divide(3,3);

  TF1 *combbg[50];
  Char_t combname[100];

  TF1 *pi0peak[50];
  Char_t peakname[100];
  TF1 *sumfit[50];
  Char_t sumname[100];

  TH1D *ptslice[50];
  Char_t name[100];

  TH1D *ptslice_bg[50];

  TH1D *ptslice_bgsub[50];
  Char_t subname[100];
  Char_t title[100];
  
  for(Int_t i=1;i<=nx;i++){
    if((i-1)%9==0){
      if(i>1){
	c->Update();
	sprintf(canvasname,"c_%s_page_%d",flag,page++);
	c_copy=(TCanvas*)c->Clone();
	c_copy->SetName(canvasname);
	c_array->Add(c_copy);
      }
      ps->NewPage();
    }
    c->cd((i-1)%9 + 1);
      
    sprintf(combname,"comb_%d_%s",i,flag);
   
    sprintf(peakname,"gaus_%d_%s",i,flag);
    sprintf(sumname,"sum_%d_%s",i,flag);
    sprintf(name,"ptslice_%d_%s",i,flag);
    sprintf(subname,"subslice_%d_%s",i,flag);
    sprintf(title,"%.2f < p_{T} < %.2f",h->GetXaxis()->GetBinLowEdge(i),h->GetXaxis()->GetBinUpEdge(i));
    
    //get bin
    ptslice[i]=h->ProjectionY(name,i,i,"e");

    //for next iter.
    ptslice_bg[i]=new TH1D(*ptslice[i]);


    //set fitfunction
    combbg[i]=new TF1(combname,combFit,0.0,1.0,3);


    sumfit[i]=new TF1(sumname,sumFit,0.0,1.0,6);
   

    float firstbin=0;
    for(int ib=1;ib<=ptslice[i]->GetNbinsX();ib++){
      if(ptslice[i]->GetBinContent(ib)>0.){
        firstbin=(float)ptslice[i]->GetXaxis()->GetBinCenter(ib);
        break;
      }
    }

    combbg[i]->SetParameters(1.0,-1.0,1.0);
    
    EXCLUDERANGE=kTRUE;//leave out pi0 range, interval:
    EXCLUDEETA=kTRUE;
    if(strcmp(flag,"mb")==0) rightPI=isDAU ? 0.2 : 0.2;//dau was 0.26 : 0.22
    if(strcmp(flag,"ht1")==0) rightPI=isDAU ? 0.281 : 0.281;
    if(strcmp(flag,"ht2")==0) rightPI=isDAU ? 0.26 : 0.26;
    
    if(isDAU && strcmp(flag,"ht1")==0){
      if(h->GetXaxis()->GetBinCenter(i)>9.){
	combbg[i]->FixParameter(2,0.);//was 9.
	//rightPI=0.3;
      }
    }
    if(isDAU && strcmp(flag,"ht2")==0){
      if(h->GetXaxis()->GetBinCenter(i)>11.){//was 11.
        combbg[i]->FixParameter(2,0.);//was 9.
	//rightPI=0.3;
      }
    }

    if(isPP05 && h->GetXaxis()->GetBinCenter(i)>9.) combbg[i]->FixParameter(2,0.);

    combbg[i]->SetRange(0.0,0.8);
    if(isPP05){
      combbg[i]->SetRange(0.28,0.8);//was 0.0-0.8 (24/01/08)
      if(h->GetXaxis()->GetBinCenter(i)<7. && strcmp(flag,"ht2")==0){
      combbg[i]->SetRange(0.3,0.8);
      }
    }
    if(isDAU) combbg[i]->SetRange(0.28,0.8);    

    sumfit[i]->SetRange(0.0,0.8);//was 0.0-0.8, this to show background
    
    
    //MARKER    
    if(isPP05 && strcmp(flag,"mb")==0){
      combbg[i]->SetRange(0.0,0.4);//0.8
      if(h->GetXaxis()->GetBinCenter(i)<2.){
	combbg[i]->SetRange(0.05,0.4);
	leftPI=0.081;
        rightPI=0.19;
      }
      if(h->GetXaxis()->GetBinCenter(i)>2.){
	combbg[i]->FixParameter(2,0.);
	combbg[i]->SetRange(0.05,0.4);
	leftPI=0.10;
        rightPI=0.22;
      }
      if(h->GetXaxis()->GetBinCenter(i)>3.){
	leftPI=0.06;
	rightPI=0.22;
      }
      sumfit[i]->SetRange(0.0,0.4);
    }
    if(isDAU && strcmp(flag,"mb")==0){
      combbg[i]->SetRange(0.0,0.4);//0.8
      if(h->GetXaxis()->GetBinCenter(i)<2.){
        combbg[i]->SetRange(0.0,0.4);
        leftPI=0.0;
        rightPI=0.2;
      }
      if(h->GetXaxis()->GetBinCenter(i)>2.){
	leftPI=0.0;
        rightPI=0.22;
      }
      if(h->GetXaxis()->GetBinCenter(i)>4.){
	combbg[i]->FixParameter(2,0.);
	leftPI=0.0;
        rightPI=0.22;
      }
      sumfit[i]->SetRange(0.0,0.4);
    }


    if(!isMC||isPythia) ptslice[i]->Fit(combname,"QR0");
    EXCLUDERANGE=kFALSE;
    EXCLUDEETA=kFALSE;
    combbg[i]->SetRange(0.0,0.8);
    if(strcmp(flag,"mb")==0) combbg[i]->SetRange(0.0,0.4);
    //cout<<gMinuit->fCstatu.Data()<<endl;
    
    ptslice[i]->SetMarkerStyle(4);
    ptslice[i]->SetMarkerSize(.7);
    ptslice[i]->SetMarkerColor(color);
    ptslice[i]->SetLineColor(color);
    ptslice[i]->Draw();
    combbg[i]->SetLineStyle(2);
    combbg[i]->SetLineWidth(1);
    combbg[i]->SetLineColor(cbgcolor);
    sumfit[i]->SetLineWidth(1);
    sumfit[i]->SetLineColor(cbgcolor);


    ptslice_bgsub[i]=new TH1D(*ptslice[i]);
    ptslice_bgsub[i]->SetName(subname);
    if(!isMC||isPythia){
      ptslice_bgsub[i]->Add(combbg[i],-1.0);
      combbg[i]->Draw("same");
    }
    
    //fit peak
    pi0peak[i]=new TF1(peakname,"gaus",0.05,0.2);
    pi0peak[i]->SetParLimits(1,.1,.2);
    
    if(!isMC||isPythia) ptslice_bgsub[i]->Fit(peakname,"QR0");
    else ptslice[i]->Fit(peakname,"QR0");
    pi0peak[i]->SetLineColor(peakcolor); 
    pi0peak[i]->SetLineStyle(3);
    pi0peak[i]->SetLineWidth(2);
    pi0peak[i]->SetRange(0.0,0.8);

    //draw sum of comb. + sign.
    Double_t params[6];
    combbg[i]->GetParameters(&params[0]);
    pi0peak[i]->GetParameters(&params[3]);
    sumfit[i]->SetParameters(params);
    if(!isMC||isPythia){
      sumfit[i]->Draw("same");
    }

    ptslice[i]->SetTitle(title);
    TAxis *ax=ptslice[i]->GetXaxis();
    ax->SetRangeUser(0.0,.8);
    if(strcmp(flag,"mb")==0) ax->SetRangeUser(0.0,0.4);

    if(!isMC||isPythia){
      Double_t MINIMUM=ptslice_bgsub[i]->GetMinimum();
      ptslice[i]->SetMinimum(MINIMUM);
    }
    
    //get 1st yield
    Float_t mean=pi0peak[i]->GetParameter(1);
    Float_t meanerr=pi0peak[i]->GetParError(1);
    Float_t sigma=pi0peak[i]->GetParameter(2);
    Float_t sigmaerr=pi0peak[i]->GetParError(2);
    
    Float_t yield=pi0peak[i]->Integral(mean-2.*sigma,mean+2.*sigma);
    yield/=ptslice_bgsub[i]->GetBinWidth(1);
    Float_t erryield=0.;
    
    if(yield>0.) erryield=sqrt(yield);
    else {yield=0.;erryield=0.;}
    
    x[i-1]=h->GetXaxis()->GetBinCenter(i);
    ex[i-1]=0.;
    y[i-1]=yield/h->GetXaxis()->GetBinWidth(i);// divide dpt
    ey[i-1]=erryield/h->GetXaxis()->GetBinWidth(i);
    m[i-1]=mean;
    em[i-1]=meanerr;
    s[i-1]=sigma;
    es[i-1]=sigmaerr;
    
  }
  c->Update();
  sprintf(canvasname,"c_%s_page_%d",flag,page++);
  c_copy=(TCanvas*)c->Clone();
  c_copy->SetName(canvasname);
  c_array->Add(c_copy);
  ps->NewPage();
  
  Float_t pTmin=1.;
  Float_t pTmax=7.;
  Float_t thres=0;
  if(strcmp(flag,"ht1")==0) {pTmin=3.;pTmax=11.;thres=2.5;}
  if(strcmp(flag,"ht2")==0) {pTmin=5.;pTmax=15.;thres=4.5;}
  //fit mean
  TF1 *meanfit=new TF1("meanfit","[0]*(1.0 - exp(-[1]*(x-[2])))");
  meanfit->SetParameter(0,.145);
  meanfit->SetParameter(1,2.);
  meanfit->SetParameter(2,thres);
  meanfit->SetParLimits(2,thres-2.0,10.);
  meanfit->SetRange(pTmin,pTmax);
  meanfit->SetLineColor(color);
  meanfit->SetLineStyle(2);
  //fit sigma
  TF1 *sigmafit=new TF1("sigmafit","[0]+[1]*exp(-[2]*(x-[3]))");
  sigmafit->SetParameter(0,0.02);
  sigmafit->SetParameter(1,0.05);
  sigmafit->SetParameter(2,1.0);
  sigmafit->SetParameter(3,thres);
  sigmafit->SetParLimits(3,thres-2.0,10.);
  sigmafit->SetRange(pTmin,pTmax);
  sigmafit->SetLineColor(color);
  sigmafit->SetLineStyle(2);

  TCanvas *c2=new TCanvas(TString("c2_")+TString(flag),TString("c2_")+TString(flag),600,600);
  c2->Divide(2,2);
  c2->cd(1);

  //draw mean,sigma,yield vs pT
  TGraphErrors *gry=new TGraphErrors(nx,x,y,ex,ey);
  gry->SetName((TString(flag)+"_yieldfit").Data());
  gPad->SetLogy();
  gry->SetMarkerStyle(25);
  gry->SetMarkerSize(.9);
  gry->SetMarkerColor(color);
  gry->GetXaxis()->SetRangeUser(pTmin,pTmax);
  gry->Draw("ap");
  gry->SetMaximum(10.*gry->GetHistogram()->GetMaximum());
  Float_t minimum=.1*gry->GetHistogram()->GetMinimum();
  gry->SetMinimum(minimum > 1. ? minimum : 1.);

  c2->cd(2);
  TGraphErrors *grm=new TGraphErrors(nx,x,m,ex,em);
  grm->SetName((TString(flag)+"_meanfit").Data());
  grm->SetMarkerStyle(25);
  grm->SetMarkerSize(.9);
  grm->SetMarkerColor(color);
  grm->GetXaxis()->SetRangeUser(pTmin,pTmax);
  grm->Draw("ap");
  grm->Fit("meanfit","QR");
  meanfit->Draw("same");
  c2->cd(3);
  TGraphErrors *grs=new TGraphErrors(nx,x,s,ex,es);
  grs->SetName((TString(flag)+"_sigmafit").Data());
  grs->SetMarkerStyle(25);
  grs->SetMarkerSize(.9);
  grs->SetMarkerColor(color);
  grs->SetMaximum(0.15);
  grs->SetMinimum(0.);
  grs->GetXaxis()->SetRangeUser(pTmin,pTmax);
  grs->Draw("ap");
  grs->Fit("sigmafit","QR");
  c2->cd(0);

  //c2->Update();
  //ps->NewPage();
  delete c2;

  TCanvas *cr=new TCanvas(TString("cr_")+TString(flag),TString("cr_")+TString(flag),1000,1000);
  cr->Divide(4,5);

  //re-integrate peak region (with correct error..):
  Float_t *x2=new Float_t[nx];
  Float_t *ex2=new Float_t[nx];
  Float_t *y2=new Float_t[nx];
  Float_t *ey2=new Float_t[nx];
  Float_t *m2=new Float_t[nx];
  Float_t *em2=new Float_t[nx];
  Float_t *s2=new Float_t[nx];
  Float_t *es2=new Float_t[nx];
  Float_t *bgy=new Float_t[nx];


  TF1 *finalpeak[100];
  TH1D *intregion[100];
  Char_t newname[100];
  Char_t newname2[100];
  Char_t newtitle[100];
  Char_t newfit[100];
  for(Int_t g=1;g<=nx;g++)
    {
      sprintf(newname,"ptbin_%d_%s",g,flag);
      sprintf(newname,"ptbin2_%d_%s",g,flag);
      sprintf(newtitle,"%.2f < p_{T} < %.2f",h->GetXaxis()->GetBinLowEdge(g),h->GetXaxis()->GetBinUpEdge(g));
      sprintf(newfit,"fit_peak_%d_%s",g,flag);
      finalpeak[g]=new TF1(newfit,"gaus");
      //get pT
      Float_t xval=h->GetXaxis()->GetBinCenter(g);
      //limit peak to +/- 2 sigma from extrapolation
      Float_t left=meanfit->Eval(xval)-2.*sigmafit->Eval(xval);
      Float_t right=meanfit->Eval(xval)+2.*sigmafit->Eval(xval);
      
      finalpeak[g]->SetParLimits(1,left,right);
      //define fit range +/- 3*sigma
      left=meanfit->Eval(xval)-3*sigmafit->Eval(xval);
      right=meanfit->Eval(xval)+3*sigmafit->Eval(xval);

      finalpeak[g]->SetRange(left,right);
      finalpeak[g]->SetLineColor(color);
      finalpeak[g]->SetLineStyle(2);
      finalpeak[g]->SetLineWidth(1);
      cr->cd(g);
      ptslice_bgsub[g]->SetName(newname);
      ptslice_bgsub[g]->SetTitle(newtitle);
      ptslice_bgsub[g]->GetXaxis()->SetTitle("M_{inv} (GeV/c^{2})");
      ptslice_bgsub[g]->GetYaxis()->SetTitle("counts (10 MeV/c^{2})^{-1}");
      if(strcmp(flag,"mb")!=0){
	ptslice_bgsub[g]->GetYaxis()->SetTitle("counts (20 MeV/c^{2})^{-1}");
      }
      ptslice_bgsub[g]->SetFillColor(0);
      ptslice_bgsub[g]->SetLineColor(color);
      ptslice_bgsub[g]->Draw("hist");
      ptslice_bgsub[g]->GetXaxis()->SetRangeUser(0.0,.8);
      if(strcmp(flag,"mb")==0){
	ptslice_bgsub[g]->GetXaxis()->SetRangeUser(0.0,0.4);
	//fit residual:
      }

      ptslice_bgsub[g]->Fit(newfit,"QR0");
      finalpeak[g]->SetRange(0.0,1.0);
      finalpeak[g]->Draw("same");


      //get integration region
      intregion[g]=new TH1D(*ptslice_bgsub[g]);
      intregion[g]->SetName(newname2);
      intregion[g]->SetFillColor(fillcolor);
      Float_t LEFT=finalpeak[g]->GetParameter(1)-NSIGMALO*finalpeak[g]->GetParameter(2);
      Float_t RIGHT=finalpeak[g]->GetParameter(1)+(NSIGMAHI)*finalpeak[g]->GetParameter(2);
      if(isMC) RIGHT=finalpeak[g]->GetParameter(1)+(NSIGMAHI+3)*finalpeak[g]->GetParameter(2);
      if(!isMC){
	if(isDAU){
	  if(strcmp(flag,"mb")==0){
	    if(RIGHT>0.2) RIGHT=0.2;
	    if(LEFT<0.08) LEFT=0.081;
	  }
	  if(strcmp(flag,"ht1")==0&&xval>9.){
	    LEFT=0.081;
            RIGHT=0.26;
	  }
	  if(strcmp(flag,"ht2")==0&&xval>9.){
	    LEFT=0.081;
	    if(xval>12.) RIGHT=0.3;
	  }
	}
	
	if(isPP05){
	  if(strcmp(flag,"mb")==0){
            //if(RIGHT>0.2) RIGHT=0.19;
	    if(xval>2.) RIGHT=0.199;
            if(LEFT<0.08) LEFT=0.081;
          }
	  if(strcmp(flag,"ht1")==0&&xval>9.){
            LEFT=0.081;
	    RIGHT=0.241;
	  }
	  if(strcmp(flag,"ht2")==0&&xval>10.){
            LEFT=0.081;
          }


	}
      
      }
      
      
      x2[g-1]=xval;
      ex2[g-1]=0.;
            
      Float_t dpt=h->GetXaxis()->GetBinWidth(g);
      intregion[g]->GetXaxis()->SetRangeUser(LEFT,RIGHT);
    

      ptslice_bg[g]->GetXaxis()->SetRangeUser(LEFT,RIGHT);
      
      y2[g-1]=intregion[g]->Integral();
      bgy[g-1]=ptslice_bg[g]->Integral() - intregion[g]->Integral();
      if(!isMC) ey2[g-1]=sqrt(y2[g-1]+2.*bgy[g-1]);
      else{
	//calculate montecarlo error:
	ey2[g-1]=0;
	for(Int_t i=1;i<=intregion[g]->GetNbinsX();i++){
	  ey2[g-1]+=pow(intregion[g]->GetBinError(i),2);
	}
	ey2[g-1]=sqrt(ey2[g-1]);
      } 
      y2[g-1]/=dpt;
      ey2[g-1]/=dpt;
      //cout<<xval<<"\t"<<y2[g-1]<<" +/- "<<ey2[g-1]<<endl;
      if(strcmp(flag,"mb")==0) cout<<xval<<"\t"<<dpt*y2[g-1]/bgy[g-1]<<"\t"<<ey2[g-1]/y2[g-1]<<endl;

      intregion[g]->Draw("histsame");

      //get mean from final gaus fit
      m2[g-1]=finalpeak[g]->GetParameter(1);
      em2[g-1]=finalpeak[g]->GetParError(1);
      s2[g-1]=finalpeak[g]->GetParameter(2);
      es2[g-1]=finalpeak[g]->GetParError(2);

      retYield->SetBinContent(g,y2[g-1]);
      retYield->SetBinError(g,ey2[g-1]);

    }
  cr->cd(0);
  cr->Update();
  sprintf(canvasname,"c_%s_page_%d",flag,page++);
  c_copy=(TCanvas*)cr->Clone();
  c_copy->SetName(canvasname);
  c_array->Add(c_copy);
  ps->NewPage();  
  

  TCanvas *c4=new TCanvas(TString("c4_")+TString(flag),TString("c4_")+TString(flag),600,900);
  c4->Divide(2,3);
  c4->cd(1);
  TGraphErrors *regry=new TGraphErrors(nx,x2,y2,ex2,ey2);
  regry->SetName((TString(flag)+"_yield").Data());
  gPad->SetLogy();
  regry->SetMarkerStyle(21);
  regry->SetMarkerSize(.9);
  regry->SetMarkerColor(color);
  regry->GetXaxis()->SetRangeUser(pTmin,pTmax);
  gry->GetXaxis()->SetRangeUser(pTmin,pTmax);
  Float_t maxy=gry->GetHistogram()->GetMaximum();
  Float_t miny=gry->GetHistogram()->GetMinimum() + 1.;
  if(isMC) miny=gry->GetHistogram()->GetMinimum() + 0.001;
  if(isPythia) miny=gry->GetHistogram()->GetMinimum();
  regry->SetMaximum(maxy);
  regry->SetMinimum(miny);
  gry->SetMaximum(maxy);
  gry->SetMinimum(miny);
  regry->Draw("ap");
  c4->cd(2);
  gPad->SetLogy();
  gry->Draw("ap");
  c4->cd(3);
  TGraphErrors *regrm=new TGraphErrors(nx,x2,m2,ex2,em2);
  regrm->SetName((TString(flag)+"_mean").Data());
  regrm->SetMarkerStyle(21);
  regrm->SetMarkerSize(.9);
  regrm->SetMarkerColor(color);
  regrm->GetXaxis()->SetRangeUser(pTmin,pTmax);
  grm->GetXaxis()->SetRangeUser(pTmin,pTmax);
  regrm->SetMaximum(.2);
  regrm->SetMinimum(.1);
  grm->SetMaximum(.2);
  grm->SetMinimum(.1);
  regrm->Draw("ap");
  c4->cd(4);
  grm->Draw("ap");
  c4->cd(5);
  TGraphErrors *regrs=new TGraphErrors(nx,x2,s2,ex2,es2);
  regrs->SetName((TString(flag)+"_sigma").Data());
  regrs->SetMarkerStyle(21);
  regrs->SetMarkerSize(.9);
  regrs->SetMarkerColor(color);
  regrs->GetXaxis()->SetRangeUser(pTmin,pTmax);
  grs->GetXaxis()->SetRangeUser(pTmin,pTmax);
  regrs->SetMaximum(.05);
  regrs->SetMinimum(0.);
  grs->SetMaximum(.05);
  grs->SetMinimum(0.);
  regrs->Draw("ap");
  c4->cd(6);
  grs->Draw("ap");
  c4->cd(0);
  c4->Update();

  sprintf(canvasname,"c_%s_page_%d",flag,page++);
  c_copy=(TCanvas*)c4->Clone();
  c_copy->SetName(canvasname);
  c_array->Add(c_copy);

  ps->NewPage();

  if(strcmp(flag,"ht2")==0) ps->Close();

  return retYield;
}

Float_t Pi0Analysis::getWeight(Float_t pT){
  //weight for neutrons (Levy function):
  if(pT<0.5) pT=0.5;
  Float_t weight=pT/pow((1.+(sqrt(pT*pT+1.) - 1.)/(0.215*12.55)),12.55);
  return weight;
}

void Pi0Analysis::storeCanvases(const Char_t *f_c){
  TFile *f_canvas=new TFile(f_c,"RECREATE");
  c_array->Write();
  f_canvas->Close();
}
