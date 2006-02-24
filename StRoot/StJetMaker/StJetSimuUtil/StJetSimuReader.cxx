//M.L. Miller
//MIT Software
//6/04

//std
#include <map>
#include <string>
#include <algorithm>
#include <iostream>
#include <math.h>

//StEvent
#include "StEvent/StEmcDetector.h"
#include "StEvent/StEmcCollection.h"
#include "StEvent/StEmcRawHit.h"
#include "StEvent/StEmcModule.h"

//StEmc
#include "StEmcClusterCollection.h"
#include "StEmcPoint.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/others/emcDetectorName.h"
#include "StEmcADCtoEMaker/StBemcData.h"
#include "StEmcRawMaker/defines.h"
#include "StEmcRawMaker/StBemcRaw.h"
#include "StEmcADCtoEMaker/StEmcADCtoEMaker.h"
#include "StEmcRawMaker/StBemcTables.h"

//root
#include "TTree.h"
#include "TFriendElement.h"
#include "TFile.h"
#include "TH1F.h"
#include "TH2F.h"
#include "TH3F.h"
#include "TProfile.h"
#include "StThreeVectorF.hh"

//StMuDst
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuEmcCollection.h"

//StJetMaker
#include "StJetMaker/StJet.h"
#include "StJetMaker/StJets.h"
#include "StJetMaker/StJetSimuUtil/StJetSimuReader.h"

ClassImp(StJetSimuReader)


double gDeltaPhi(double p1, double p2);

string idString(TrackToJetIndex* t);

bool verifyJet(StJets* stjets, int ijet);

double getTheta(double eta){
  return 2*atan(exp(-eta));
}

double getEta(double theta){
  return -log(tan(theta/2));
}

double getEmcTheta(double z,double thetaj){
  const double SMDR = 2.2625; //radius of BSMD
  const double pi=3.141592654;
  double hold;
  double denom;
  if (thetaj==pi/2) { // if Jet Theta = 90 then tan is undefined
    
    if (z==0) {
      hold = pi/2;
    }
    else {
      hold = atan2(SMDR,z);
    }
    
  }
  else 
    {
      denom = (SMDR/tan(thetaj))+z;
      if (denom==0.0) {
	hold = pi/2;
      }
      if (denom!=0.0) {
	hold = atan2(SMDR,denom);
      }
      
    }
  return hold;
}

bool verifySimu(int dst, int stored){
  if (dst==stored) return true;
  else return false;
}

Char_t getCond(float partPt){

  char cond;
  if (partPt<3) cond='A';
  if ((partPt>=3)&&(partPt<7)) cond='B';
  if ((partPt>=7)&&(partPt<15)) cond='C';
  if (partPt>=15.0) cond='D';

  return cond;
}

void dumpProtojetToStream(int event, ostream& os, StJets* stjets);

StJetSimuReader::StJetSimuReader(const char* name, StMuDstMaker* uDstMaker)
    : StMaker(name), mFile(0), mTree(0), mDstMaker(uDstMaker), mCounter(0), mOfstream(0)
{
    cout <<"StJetSimuReader::StJetSimuReader()"<<endl;
    HList=0;
    SimuSwitch=0;
    EventSwitch=0;
    ConeYieldSwitch=0;
    myTable = new StBemcTables();
}

StJetSimuReader::~StJetSimuReader()
{
  delete myTable;
  cout <<"StJetSimuReader::~StJetSimuReader()"<<endl;
}


Int_t StJetSimuReader::Init()
{
  primTrack = new StMuTrack();

  if (SimuSwitch) {
    SimuHisto2005();
    cout <<"Called SimuHisto2005 "<<endl;
  }

  if (EventSwitch) {
    EventHisto();
    cout <<"Called EventHisto "<<endl;
  }

  if (ConeYieldSwitch) {
    ConeYieldHisto();
    cout <<"Called ConeYieldHisto "<<endl;
  }
  
  if (EnergyScaleSwitch) {
    ScaleHisto();
    cout<<"Called ScaleHisto "<<endl;
  }

  if (CheckPDFSwitch) {
    CheckPDFHisto();
    cout<<"Called CheckPDFHisto()"<<endl;
  }

  if (AsymBiasSwitch){
    AsymBiasHisto();
    cout<<"Called AsymBiasHisto()"<<endl;
  }
   
  return kStOk;
}


void StJetSimuReader::CheckPDFHisto()
{
  Q2hist=new TH1F("Q2","Q2",1600,0,1600);
  HList->Add(Q2hist);

  pol[0]=new TH2F("xdu","xdu",300,0.001,1,5000,0,0.5);
  pol[1]=new TH2F("xdd","xdd",300,0.001,1,5000,-0.5,0.0);
  pol[2]=new TH2F("xds","xds",300,0.001,1,5000,-0.25,0.25);
  pol[3]=new TH2F("xdg","xdg",300,0.001,1,5000,0,0.5);
  pol[4]=new TH2F("xdub","xdub",300,0.001,1,5000,-0.25,0.25);
  pol[5]=new TH2F("xddb","xddb",300,0.001,1,5000,-0.25,0.25);
  pol[6]=new TH2F("xdsb","xdsb",300,0.001,1,5000,-0.25,0.25);

  unpol[0]=new TH2F("xu","xu",300,0.001,1,15000,0,1.5);
  unpol[1]=new TH2F("xd","xd",300,0.001,1,15000,0.0,1.5);
  unpol[2]=new TH2F("xs","xs",300,0.001,1,15000,0.0,1.5);
  unpol[3]=new TH2F("xg","xg",300,0.001,1,15000,0.0,15.0);
  unpol[4]=new TH2F("xub","xub",300,0.001,1,15000,0.0,1.5);
  unpol[5]=new TH2F("xdb","xdb",300,0.001,1,15000,0.0,1.5);
  unpol[6]=new TH2F("xsb","xsb",300,0.001,1,15000,0.0,1.5);
  
  part[0]=new TH2F("part0","Partonic A_LL vs costh 0",360,0,180,40,-1,1);
  part[1]=new TH2F("part1","Partonic A_LL vs costh 1",360,0,180,40,-1,1);
  part[2]=new TH2F("part2","Partonic A_LL vs costh 2",360,0,180,40,-1,1);
  part[3]=new TH2F("part3","Partonic A_LL vs costh 3",360,0,180,40,-1,1);
  part[4]=new TH2F("part4","Partonic A_LL vs costh 4",360,0,180,40,-1,1);
  
  for (int xx=0;xx<7;xx++){
    HList->Add(pol[xx]);
    HList->Add(unpol[xx]);
    if (xx<5) HList->Add(part[xx]);
  }  
  
 
}

void StJetSimuReader::ScaleHisto()
{
  TScale[0]=new TH2F("Tscale0","PYTHIA vs GEANT (trigJet)",50,0,50,50,0,50);
  TScale[1]=new TH2F("Tscale1","PNEU vs GEANT (trigJet)",50,0,50,50,0,50);
  TScale[2]=new TH2F("Tscale2","PYTHIA-PNEU vs GEANT (trigJet)",50,0,50,50,0,50);
  qqTScale[0]=new TH2F("qqTscale0","PYTHIA vs GEANT (qqtrigJet)",50,0,50,50,0,50);
  qqTScale[1]=new TH2F("qqTscale1","PNEU vs GEANT (qqtrigJet)",50,0,50,50,0,50);
  qqTScale[2]=new TH2F("qqTscale2","PYTHIA-PNEU vs GEANT (qqtrigJet)",50,0,50,50,0,50);
  qgTScale[0]=new TH2F("qgTscale0","PYTHIA vs GEANT (qgtrigJet)",50,0,50,50,0,50);
  qgTScale[1]=new TH2F("qgTscale1","PNEU vs GEANT (qgtrigJet)",50,0,50,50,0,50);
  qgTScale[2]=new TH2F("qgTscale2","PYTHIA-PNEU vs GEANT (qgtrigJet)",50,0,50,50,0,50);
  ggTScale[0]=new TH2F("ggTscale0","PYTHIA vs GEANT (ggtrigJet)",50,0,50,50,0,50);
  ggTScale[1]=new TH2F("ggTscale1","PNEU vs GEANT (ggtrigJet)",50,0,50,50,0,50);
  ggTScale[2]=new TH2F("ggTscale2","PYTHIA-PNEU vs GEANT (ggtrigJet)",50,0,50,50,0,50);

  AScale[0]=new TH2F("Ascale0","PYTHIA vs GEANT (trigJet)",50,0,50,50,0,50);
  AScale[1]=new TH2F("Ascale1","PNEU vs GEANT (trigJet)",50,0,50,50,0,50);
  AScale[2]=new TH2F("Ascale2","PYTHIA-PNEU vs GEANT (trigJet)",50,0,50,50,0,50);
  qqAScale[0]=new TH2F("qqAscale0","PYTHIA vs GEANT (qqtrigJet)",50,0,50,50,0,50);
  qqAScale[1]=new TH2F("qqAscale1","PNEU vs GEANT (qqtrigJet)",50,0,50,50,0,50);
  qqAScale[2]=new TH2F("qqAscale2","PYTHIA-PNEU vs GEANT (qqtrigJet)",50,0,50,50,0,50);
  qgAScale[0]=new TH2F("qgAscale0","PYTHIA vs GEANT (qgtrigJet)",50,0,50,50,0,50);
  qgAScale[1]=new TH2F("qgAscale1","PNEU vs GEANT (qgtrigJet)",50,0,50,50,0,50);
  qgAScale[2]=new TH2F("qgAscale2","PYTHIA-PNEU vs GEANT (qgtrigJet)",50,0,50,50,0,50);
  ggAScale[0]=new TH2F("ggAscale0","PYTHIA vs GEANT (ggtrigJet)",50,0,50,50,0,50);
  ggAScale[1]=new TH2F("ggAscale1","PNEU vs GEANT (ggtrigJet)",50,0,50,50,0,50);
  ggAScale[2]=new TH2F("ggAscale2","PYTHIA-PNEU vs GEANT (ggtrigJet)",50,0,50,50,0,50);

  js[0]=new TH1F("dR","dR",1000,0,10);
  js[1]=new TH1F("jet","Assoc Jet pT",50,0,50);
  js[2]=new TH1F("qqjet","qq Assoc Jet pT",50,0,50);
  js[3]=new TH1F("qgjet","qg Assoc Jet pT",50,0,50);
  js[4]=new TH1F("ggjet","gg Assoc Jet pT",50,0,50);
  js[5]=new TH1F("Tjet","Trigger Jet pT",50,0,50);
  js[6]=new TH1F("qqTjet","qq Trigger Jet pT",50,0,50);
  js[7]=new TH1F("qgTjet","qg Trigger Jet pT",50,0,50);
  js[8]=new TH1F("ggTjet","gg Trigger Jet pT",50,0,50);
  js[9]=new TH1F("Ajet","Away Jet pT",50,0,50);
  js[10]=new TH1F("qqAjet","qq Away Jet pT",50,0,50);
  js[11]=new TH1F("qgAjet","qg Away Jet pT",50,0,50);
  js[12]=new TH1F("ggAjet","gg Away Jet pT",50,0,50);

  for (int xx=0;xx<13;xx++){
    HList->Add(js[xx]);
  }

  for (int xx=0;xx<3;xx++){

    HList->Add(AScale[xx]);
    HList->Add(qqAScale[xx]);
    HList->Add(qgAScale[xx]);
    HList->Add(ggAScale[xx]);

    HList->Add(TScale[xx]);
    HList->Add(qqTScale[xx]);
    HList->Add(qgTScale[xx]);
    HList->Add(ggTScale[xx]);
  }

}


void StJetSimuReader::ConeYieldHisto()
{
  jpTA[0]=new TH1F("pTA","Jet pT (R=0.2) ",50,0,50);
  jpTB[0]=new TH1F("pTB","Jet pT (R=0.4) ",50,0,50); 
  jpTC[0]=new TH1F("pTC","Jet pT  (R=0.6) ",50,0,50); 
  jpTD[0]=new TH1F("pTD","Jet pT  (R=0.8) ",50,0,50);
  jpTE[0]=new TH1F("pTE","Jet pT  (R=1.0) ",50,0,50);

  jpTA[1]=new TH1F("HTpTA","HT Jet pT (R=0.2) ",50,0,50);
  jpTB[1]=new TH1F("HTpTB","HT Jet pT (R=0.4) ",50,0,50); 
  jpTC[1]=new TH1F("HTpTC","HT Jet pT  (R=0.6) ",50,0,50); 
  jpTD[1]=new TH1F("HTpTD","HT Jet pT  (R=0.8) ",50,0,50); 
  jpTE[1]=new TH1F("HTpTE","HT Jet pT  (R=1.0) ",50,0,50);

  PYjpTA[0]=new TH1F("PYpTA","Jet pT (R=0.2) ",50,0,50);
  PYjpTB[0]=new TH1F("PYpTB","Jet pT (R=0.4) ",50,0,50); 
  PYjpTC[0]=new TH1F("PYpTC","Jet pT  (R=0.6) ",50,0,50); 
  PYjpTD[0]=new TH1F("PYpTD","Jet pT  (R=0.8) ",50,0,50);
  PYjpTE[0]=new TH1F("PYpTE","Jet pT  (R=1.0) ",50,0,50);

  PYjpTA[1]=new TH1F("PYHTpTA","HT Jet pT (R=0.2) ",50,0,50);
  PYjpTB[1]=new TH1F("PYHTpTB","HT Jet pT (R=0.4) ",50,0,50); 
  PYjpTC[1]=new TH1F("PYHTpTC","HT Jet pT  (R=0.6) ",50,0,50); 
  PYjpTD[1]=new TH1F("PYHTpTD","HT Jet pT  (R=0.8) ",50,0,50); 
  PYjpTE[1]=new TH1F("PYHTpTE","HT Jet pT  (R=1.0) ",50,0,50);
 
  Count[0]=new TH1F("HTtrig","HTtrig",2,-0.5,1.5);

  for (int xx=0;xx<2;xx++){

    HList->Add(jpTA[xx]);
    HList->Add(jpTB[xx]);
    HList->Add(jpTC[xx]);
    HList->Add(jpTD[xx]);
    HList->Add(jpTE[xx]);

    HList->Add(PYjpTA[xx]);
    HList->Add(PYjpTB[xx]);
    HList->Add(PYjpTC[xx]);
    HList->Add(PYjpTD[xx]);
    HList->Add(PYjpTE[xx]);

    HList->Add(Count[0]);
  }
}

void StJetSimuReader::EventHisto()
{

  trig=new TH2F("trg","Trigger Condition vs PID",100,0,100,10,0,10);
  HList->Add(trig);
  
  py[0]=new TH1F("hard_pt","partonic PT (GeV)",100,0,100);
  py[1]=new TH1F("x1","partonic x1",100,0,1);
  py[2]=new TH1F("x2","partonic x2",100,0,1);
  py[3]=new TH1F("s","partonic Mand s",100,0,1000);
  py[4]=new TH1F("t","partonic Mand t", 10000,-500,500);
  py[5]=new TH1F("u","partonic Mand u", 4100,-40000,1000);
  py[6]=new TH1F("cos_theta","partonic cos(theta)",100,0,1);
  py[7]=new TH1F("MaxEt","Et for Max Et Particle",500,0,50);
  py[8]=new TH1F("MaxpT","pT for Max Et Particle",500,0,50);
  py[9]=new TH1F("MaxEta","Eta for Max Et Particle",200,-10,10);
  py[10]=new TH1F("MaxID","PID for Max Et Particle",4600,-2300,2300);
  py[11]=new TH1F("SumEt","Et Sum of Stable Particles",1000,0,100);
  py[12]=new TH1F("SumPt","Pt Sum of Stable Particles",1000,0,100);
  py[13]=new TH1F("Multi","Multiplicity of Particles with E>0.5 GeV",100,0,100);
  py[14]=new TH1F("MaxEtA","Et for Max Et Anti-Nucleon",500,0,50);
  py[15]=new TH1F("MaxpTA","pT for Max Et Anti-Nucleon",500,0,50);
  py[16]=new TH1F("MaxEtaA","Eta for Max Et Anti-Nucleon",200,-10,10);
  py[17]=new TH1F("MaxIDA","PID for Max Et Anti-Nucleon",4600,-2300,2300);
  py[18]=new TH1F("MaxEtC","Et for Max Et Charged Hadron",500,0,50);
  py[19]=new TH1F("MaxpTC","pT for Max Et Charged Hadron",500,0,50);
  py[20]=new TH1F("MaxEtaC","Eta for Max Et Charged Hadron",200,-10,10);
  py[21]=new TH1F("MaxIDC","PID for Max Et Charged hadron",4600,-2300,2300);
  py[22]=new TH1F("MaxEtN","Et for Max Et Neutral hadron",500,0,50);
  py[23]=new TH1F("MaxpTN","pT for Max Et Neutral hadron",500,0,50);
  py[24]=new TH1F("MaxEtaN","Eta for Max Et Neutral hadron",200,-10,10);
  py[25]=new TH1F("MaxIDN","PID for Max Et Neutral hadron",4600,-2300,2300);
  py[26]=new TH1F("MaxEtP","Et for Max Et pion+gamma",500,0,50);
  py[27]=new TH1F("MaxpTP","pT for Max Et pion+gamma",500,0,50);
  py[28]=new TH1F("MaxEtaP","Eta for Max Et pion+gamma",200,-10,10);
  py[29]=new TH1F("MaxIDP","PID for Max Et pion+gamma",4600,-2300,2300);
  py[30]=new TH1F("PyJet","pT of Pythia Jet",50,0,50);

  pyB[0]=new TH1F("Bhard_pt","partonic PT (GeV)",100,0,100);
  pyB[1]=new TH1F("Bx1","partonic x1",100,0,1);
  pyB[2]=new TH1F("Bx2","partonic x2",100,0,1);
  pyB[3]=new TH1F("Bs","partonic Mand s",100,0,1000);
  pyB[4]=new TH1F("Bt","partonic Mand t", 10000,-500,500);
  pyB[5]=new TH1F("Bu","partonic Mand u", 4100,-40000,1000);
  pyB[6]=new TH1F("Bcos_theta","partonic cos(theta)",100,0,1);
  pyB[7]=new TH1F("BMaxEt","Et for Max Et Particle",500,0,50);
  pyB[8]=new TH1F("BMaxpT","pT for Max Et Particle",500,0,50);
  pyB[9]=new TH1F("BMaxEta","Eta for Max Et Particle",200,-10,10);
  pyB[10]=new TH1F("BMaxID","PID for Max Et Particle",4600,-2300,2300);
  pyB[11]=new TH1F("BSumEt","Et Sum of Stable Particles",1000,0,100);
  pyB[12]=new TH1F("BSumPt","Pt Sum of Stable Particles",1000,0,100);
  pyB[13]=new TH1F("BMulti","Multiplicity of Particles with E>0.5 GeV",100,0,100);
  pyB[14]=new TH1F("BMaxEtA","Et for Max Et Anti-Nucleon",500,0,50);
  pyB[15]=new TH1F("BMaxpTA","pT for Max Et Anti-Nucleon",500,0,50);
  pyB[16]=new TH1F("BMaxEtaA","Eta for Max Et Anti-Nucleon",200,-10,10);
  pyB[17]=new TH1F("BMaxIDA","PID for Max Et Anti-Nucleon",4600,-2300,2300);
  pyB[18]=new TH1F("BMaxEtC","Et for Max Et Charged Hadron",500,0,50);
  pyB[19]=new TH1F("BMaxpTC","pT for Max Et Charged Hadron",500,0,50);
  pyB[20]=new TH1F("BMaxEtaC","Eta for Max Et Charged Hadron",200,-10,10);
  pyB[21]=new TH1F("BMaxIDC","PID for Max Et Charged hadron",4600,-2300,2300);
  pyB[22]=new TH1F("BMaxEtN","Et for Max Et Neutral hadron",500,0,50);
  pyB[23]=new TH1F("BMaxpTN","pT for Max Et Neutral hadron",500,0,50);
  pyB[24]=new TH1F("BMaxEtaN","Eta for Max Et Neutral hadron",200,-10,10);
  pyB[25]=new TH1F("BMaxIDN","PID for Max Et Neutral hadron",4600,-2300,2300);
  pyB[26]=new TH1F("BMaxEtP","Et for Max Et pion+gamma",500,0,50);
  pyB[27]=new TH1F("BMaxpTP","pT for Max Et pion+gamma",500,0,50);
  pyB[28]=new TH1F("BMaxEtaP","Eta for Max Et pion+gamma",200,-10,10);
  pyB[29]=new TH1F("BMaxIDP","PID for Max Et pion+gamma",4600,-2300,2300);
  pyB[30]=new TH1F("BPyJet","pT of Pythia Jet",50,0,50);


  pyH[0]=new TH1F("Hhard_pt","partonic PT (GeV)",100,0,100);
  pyH[1]=new TH1F("Hx1","partonic x1",100,0,1);
  pyH[2]=new TH1F("Hx2","partonic x2",100,0,1);
  pyH[3]=new TH1F("Hs","partonic Mand s",100,0,1000);
  pyH[4]=new TH1F("Ht","partonic Mand t", 200,-1200,200);
  pyH[5]=new TH1F("Hu","partonic Mand u", 4100,-40000,1000);
  pyH[6]=new TH1F("Hcos_theta","partonic cos(theta)",100,0,1);
  pyH[7]=new TH1F("HMaxEt","Et for Max Et Particle",500,0,50);
  pyH[8]=new TH1F("HMaxpT","pT for Max Et Particle",500,0,50);
  pyH[9]=new TH1F("HMaxEta","Eta for Max Et Particle",200,-10,10);
  pyH[10]=new TH1F("HMaxID","PID for Max Et Particle",4600,-2300,2300);
  pyH[11]=new TH1F("HSumEt","Et Sum of Stable Particles",1000,0,100);
  pyH[12]=new TH1F("HSumPt","Pt Sum of Stable Particles",1000,0,100);
  pyH[13]=new TH1F("HMulti","Multiplicity of Particles with E>0.5 GeV",100,0,100);
  pyH[14]=new TH1F("HMaxEtA","Et for Max Et Anti-Nucleon",500,0,50);
  pyH[15]=new TH1F("HMaxpTA","pT for Max Et Anti-Nucleon",500,0,50);
  pyH[16]=new TH1F("HMaxEtaA","Eta for Max Et Anti-Nucleon",200,-10,10);
  pyH[17]=new TH1F("HMaxIDA","PID for Max Et Anti-Nucleon",4600,-2300,2300);
  pyH[18]=new TH1F("HMaxEtC","Et for Max Et Charged Hadron",500,0,50);
  pyH[19]=new TH1F("HMaxpTC","pT for Max Et Charged Hadron",500,0,50);
  pyH[20]=new TH1F("HMaxEtaC","Eta for Max Et Charged Hadron",200,-10,10);
  pyH[21]=new TH1F("HMaxIDC","PID for Max Et Charged hadron",4600,-2300,2300);
  pyH[22]=new TH1F("HMaxEtN","Et for Max Et Neutral hadron",500,0,50);
  pyH[23]=new TH1F("HMaxpTN","pT for Max Et Neutral hadron",500,0,50);
  pyH[24]=new TH1F("HMaxEtaN","Eta for Max Et Neutral hadron",200,-10,10);
  pyH[25]=new TH1F("HMaxIDN","PID for Max Et Neutral hadron",4600,-2300,2300);
  pyH[26]=new TH1F("HMaxEtP","Et for Max Et pion+gamma",500,0,50);
  pyH[27]=new TH1F("HMaxpTP","pT for Max Et pion+gamma",500,0,50);
  pyH[28]=new TH1F("HMaxEtaP","Eta for Max Et pion+gamma",200,-10,10);
  pyH[29]=new TH1F("HMaxIDP","PID for Max Et pion+gamma",4600,-2300,2300);
  pyH[30]=new TH1F("HPyJet","pT of Pythia Jet",50,0,50);


  pyBH[0]=new TH1F("BHhard_pt","partonic PT (GeV)",100,0,100);
  pyBH[1]=new TH1F("BHx1","partonic x1",100,0,1);
  pyBH[2]=new TH1F("BHx2","partonic x2",100,0,1);
  pyBH[3]=new TH1F("BHs","partonic Mand s",100,0,1000);
  pyBH[4]=new TH1F("BHt","partonic Mand t", 10000,-500,500);
  pyBH[5]=new TH1F("BHu","partonic Mand u", 4100,-40000,1000);
  pyBH[6]=new TH1F("BHcos_theta","partonic cos(theta)",100,0,1);
  pyBH[7]=new TH1F("BHMaxEt","Et for Max Et Particle",500,0,50);
  pyBH[8]=new TH1F("BHMaxpT","pT for Max Et Particle",500,0,50);
  pyBH[9]=new TH1F("BHMaxEta","Eta for Max Et Particle",200,-10,10);
  pyBH[10]=new TH1F("BHMaxID","PID for Max Et Particle",4600,-2300,2300);
  pyBH[11]=new TH1F("BHSumEt","Et Sum of Stable Particles",1000,0,100);
  pyBH[12]=new TH1F("BHSumPt","Pt Sum of Stable Particles",1000,0,100);
  pyBH[13]=new TH1F("BHMulti","Multiplicity of Particles with E>0.5 GeV",100,0,100);
  pyBH[14]=new TH1F("BHMaxEtA","Et for Max Et Anti-Nucleon",500,0,50);
  pyBH[15]=new TH1F("BHMaxpTA","pT for Max Et Anti-Nucleon",500,0,50);
  pyBH[16]=new TH1F("BHMaxEtaA","Eta for Max Et Anti-Nucleon",200,-10,10);
  pyBH[17]=new TH1F("BHMaxIDA","PID for Max Et Anti-Nucleon",4600,-2300,2300);
  pyBH[18]=new TH1F("BHMaxEtC","Et for Max Et Charged Hadron",500,0,50);
  pyBH[19]=new TH1F("BHMaxpTC","pT for Max Et Charged Hadron",500,0,50);
  pyBH[20]=new TH1F("BHMaxEtaC","Eta for Max Et Charged Hadron",200,-10,10);
  pyBH[21]=new TH1F("BHMaxIDC","PID for Max Et Charged hadron",4600,-2300,2300);
  pyBH[22]=new TH1F("BHMaxEtN","Et for Max Et Neutral hadron",500,0,50);
  pyBH[23]=new TH1F("BHMaxpTN","pT for Max Et Neutral hadron",500,0,50);
  pyBH[24]=new TH1F("BHMaxEtaN","Eta for Max Et Neutral hadron",200,-10,10);
  pyBH[25]=new TH1F("BHMaxIDN","PID for Max Et Neutral hadron",4600,-2300,2300);
  pyBH[26]=new TH1F("BHMaxEtP","Et for Max Et pion+gamma",500,0,50);
  pyBH[27]=new TH1F("BHMaxpTP","pT for Max Et pion+gamma",500,0,50);
  pyBH[28]=new TH1F("BHMaxEtaP","Eta for Max Et pion+gamma",200,-10,10);
  pyBH[29]=new TH1F("BHMaxIDP","PID for Max Et pion+gamma",4600,-2300,2300);
  pyBH[30]=new TH1F("BHPyJet","pT of Pythia Jet",50,0,50);

  pyBHJ[0]=new TH1F("BHJhard_pt","partonic PT (GeV)",100,0,100);
  pyBHJ[1]=new TH1F("BHJx1","partonic x1",100,0,1);
  pyBHJ[2]=new TH1F("BHJx2","partonic x2",100,0,1);
  pyBHJ[3]=new TH1F("BHJs","partonic Mand s",100,0,1000);
  pyBHJ[4]=new TH1F("BHJt","partonic Mand t", 10000,-500,500);
  pyBHJ[5]=new TH1F("BHJu","partonic Mand u", 4100,-40000,1000);
  pyBHJ[6]=new TH1F("BHJcos_theta","partonic cos(theta)",100,0,1);
  pyBHJ[7]=new TH1F("BHJMaxEt","Et for Max Et Particle",500,0,50);
  pyBHJ[8]=new TH1F("BHJMaxpT","pT for Max Et Particle",500,0,50);
  pyBHJ[9]=new TH1F("BHJMaxEta","Eta for Max Et Particle",200,-10,10);
  pyBHJ[10]=new TH1F("BHJMaxID","PID for Max Et Particle",4600,-2300,2300);
  pyBHJ[11]=new TH1F("BHJSumEt","Et Sum of Stable Particles",1000,0,100);
  pyBHJ[12]=new TH1F("BHJSumPt","Pt Sum of Stable Particles",1000,0,100);
  pyBHJ[13]=new TH1F("BHJMulti","Multiplicity of Particles with E>0.5 GeV",100,0,100);
  pyBHJ[14]=new TH1F("BHJMaxEtA","Et for Max Et Anti-Nucleon",500,0,50);
  pyBHJ[15]=new TH1F("BHJMaxpTA","pT for Max Et Anti-Nucleon",500,0,50);
  pyBHJ[16]=new TH1F("BHJMaxEtaA","Eta for Max Et Anti-Nucleon",200,-10,10);
  pyBHJ[17]=new TH1F("BHJMaxIDA","PID for Max Et Anti-Nucleon",4600,-2300,2300);
  pyBHJ[18]=new TH1F("BHJMaxEtC","Et for Max Et Charged Hadron",500,0,50);
  pyBHJ[19]=new TH1F("BHJMaxpTC","pT for Max Et Charged Hadron",500,0,50);
  pyBHJ[20]=new TH1F("BHJMaxEtaC","Eta for Max Et Charged Hadron",200,-10,10);
  pyBHJ[21]=new TH1F("BHJMaxIDC","PID for Max Et Charged hadron",4600,-2300,2300);
  pyBHJ[22]=new TH1F("BHJMaxEtN","Et for Max Et Neutral hadron",500,0,50);
  pyBHJ[23]=new TH1F("BHJMaxpTN","pT for Max Et Neutral hadron",500,0,50);
  pyBHJ[24]=new TH1F("BHJMaxEtaN","Eta for Max Et Neutral hadron",200,-10,10);
  pyBHJ[25]=new TH1F("BHJMaxIDN","PID for Max Et Neutral hadron",4600,-2300,2300);
  pyBHJ[26]=new TH1F("BHJMaxEtP","Et for Max Et pion+gamma",500,0,50);
  pyBHJ[27]=new TH1F("BHJMaxpTP","pT for Max Et pion+gamma",500,0,50);
  pyBHJ[28]=new TH1F("BHJMaxEtaP","Eta for Max Et pion+gamma",200,-10,10);
  pyBHJ[29]=new TH1F("BHJMaxIDP","PID for Max Et pion+gamma",4600,-2300,2300);
  pyBHJ[30]=new TH1F("BHJPyJet","pT of Pythia Jet",50,0,50);

  
  for (int i=0;i<31;i++){
    HList->Add(py[i]);
    HList->Add(pyH[i]);
    HList->Add(pyB[i]);
    HList->Add(pyBH[i]);
    HList->Add(pyBHJ[i]);
   }
  
  jet[0]=new TH1F("JetpT","Jet pT",50,0,50);
  jet[1]=new TH1F("JetEta","Jet Phi",360,-180,180);
  jet[2]=new TH1F("JetPhi","Jet Eta",40,-2,2);
  for (int i=0;i<3;i++){
    HList->Add(jet[i]);
  }

  pidH=new TH1F("pid","pid",100,0,100);
  HList->Add(pidH);

  tk[0]=new TH1F("trkPt","track pT",100,0,10);
  tk[1]=new TH1F("trkEta","track Eta",600,-3,3);
  tk[2]=new TH1F("trkPhi","track Phi",360,-180,180);
  tk[3]=new TH1F("trkQ","track Charge",5,-2.5,2.5);
  tk[4]=new TH1F("trkHits","track nHits",50,0,50);
  tk[5]=new TH1F("trkZ","Z vertex of event",400,-200,200);
  tk[6]=new TH1F("trkX","X vertex of event",400,-20,20);
  tk[7]=new TH1F("trkY","Y vertex of event",400,-20,20);
  tk[8]=new TH1F("trkN","Track Multiplicity",100,0,100);
  tk[9]=new TH1F("trkN1","Track Multiplicity",100,0,100);
  tk[10]=new TH1F("trkN2","Track Multiplicity",100,0,100);
  tk[11]=new TH1F("trkN3","Track Multiplicity",100,0,100);
  tk[12]=new TH1F("trkN4","Track Multiplicity",100,0,100);

  tkA[0]=new TH1F("AtrkPt","track pT (0.2<pT<0.5)",100,0,10);
  tkA[1]=new TH1F("AtrkEta","track Eta (0.2<pT<0.5)",600,-3,3);
  tkA[2]=new TH1F("AtrkPhi","track Phi (0.2<pT<0.5)",360,-180,180);
  tkA[3]=new TH1F("AtrkQ","track Charge (0.2<pT<0.5)",5,-2.5,2.5);
  tkA[4]=new TH1F("AtrkHits","track nHits (0.2<pT<0.5)",50,0,50);
  tkA[5]=new TH1F("AtrkZ","Z vertex of event (0.2<pT<0.5)",400,-200,200);
  tkA[6]=new TH1F("AtrkX","X vertex of event (0.2<pT<0.5)",400,-20,20);
  tkA[7]=new TH1F("AtrkY","Y vertex of event (0.2<pT<0.5)",400,-20,20);

  tkB[0]=new TH1F("BtrkPt","track pT (0.5<pT<1.0)",100,0,10);
  tkB[1]=new TH1F("BtrkEta","track Eta (0.5<pT<1.0)",600,-3,3);
  tkB[2]=new TH1F("BtrkPhi","track Phi (0.5<pT<1.0)",360,-180,180);
  tkB[3]=new TH1F("BtrkQ","track Charge (0.5<pT<1.0)",5,-2.5,2.5);
  tkB[4]=new TH1F("BtrkHits","track nHits (0.5<pT<1.0)",50,0,50);
  tkB[5]=new TH1F("BtrkZ","Z vertex of event (0.5<pT<1.0)",400,-200,200);
  tkB[6]=new TH1F("BtrkX","X vertex of event (0.5<pT<1.0)",400,-20,20);
  tkB[7]=new TH1F("BtrkY","Y vertex of event (0.5<pT<1.0)",400,-20,20);
 
  tkC[0]=new TH1F("CtrkPt","track pT (1.0<pT<2.0)",100,0,10);
  tkC[1]=new TH1F("CtrkEta","track Eta (1.0<pT<2.0)",600,-3,3);
  tkC[2]=new TH1F("CtrkPhi","track Phi (1.0<pT<2.0)",360,-180,180);
  tkC[3]=new TH1F("CtrkQ","track Charge (1.0<pT<2.0)",5,-2.5,2.5);
  tkC[4]=new TH1F("CtrkHits","track nHits (1.0<pT<2.0)",50,0,50);
  tkC[5]=new TH1F("CtrkZ","Z vertex of event (1.0<pT<2.0)",400,-200,200);
  tkC[6]=new TH1F("CtrkX","X vertex of event (1.0<pT<2.0)",400,-20,20);
  tkC[7]=new TH1F("CtrkY","Y vertex of event (1.0<pT<2.0)",400,-20,20);

  tkD[0]=new TH1F("DtrkPt","track pT (pT>2.0)",100,0,10);
  tkD[1]=new TH1F("DtrkEta","track Eta (pT>2.0)",600,-3,3);
  tkD[2]=new TH1F("DtrkPhi","track Phi (pT>2.0)",360,-180,180);
  tkD[3]=new TH1F("DtrkQ","track Dharge (pT>2.0)",5,-2.5,2.5);
  tkD[4]=new TH1F("DtrkHits","track nHits (pT>2.0)",50,0,50);
  tkD[5]=new TH1F("DtrkZ","Z vertex of event (pT>2.0)",400,-200,200);
  tkD[6]=new TH1F("DtrkX","X vertex of event (pT>2.0)",400,-20,20);
  tkD[7]=new TH1F("DtrkY","Y vertex of event (pT>2.0)",400,-20,20);

  for (int i=0;i<9;i++){
    HList->Add(tk[i]);
    HList->Add(tkA[i]);
    HList->Add(tkB[i]);
    HList->Add(tkC[i]);
    HList->Add(tkD[i]);
  }
  HList->Add(tk[9]);
  HList->Add(tk[10]);
  HList->Add(tk[11]);
  HList->Add(tk[12]);

 
}


void StJetSimuReader::SimuHisto2005(){


  //joanna histos
  jo[0]=new TH1F("joneu","Jet Neu (BBConly)",100,0,1);
  jo[1]=new TH1F("joneuHT","Jet Neu (HTonly)",100,0,1);
  jo[2]=new TH1F("joneuJP","Jet Neu (JPonly)",100,0,1);
  jo[3]=new TH1F("joneuHTJP","Jet Neu (JP&&HT)",100,0,1);
  for (int i=0;i<4;i++){
    HList->Add(jo[i]);
  }

  //Single Track and Single Tower Jets
  jpT[0]=new TH1F("1pT","Jet pT (1 track) ",50,0,50);
  jpT[1]=new TH1F("1eta","Jet Eta (all) ",30,-1,2);
  jpT[2]=new TH1F("1Deta","Jet Detector Eta (all) ",10,0,1);
  jpT[3]=new TH1F("1phi","Jet Phi (all) ",60,-180,180);
  jpT[4]=new TH1F("1pTtrkSum","Track pT sum (all) ",100,0,25);
  jpT[5]=new TH1F("1ETowSum","Tow E sum (all) ",100,0,25);

  jpTrk[0]=new TH1F("pTtrk","Jet pT (1 track) ",50,0,50);
  jpTrk[1]=new TH1F("etaTrk","Jet Eta (1 track) ",30,-1,2);
  jpTrk[2]=new TH1F("DetaTrk","Jet Detector Eta (1 track) ",10,0,1);
  jpTrk[3]=new TH1F("phiTrk","Jet Phi (1 track) ",60,-180,180);
  jpTrk[4]=new TH1F("TrkPt","Track pT (1 track) ",100,0,25);

  jpTow[0]=new TH1F("pTtow","Jet pT (1 tow) ",50,0,50);
  jpTow[1]=new TH1F("etaTow","Jet Eta (1 tow) ",30,-1,2);
  jpTow[2]=new TH1F("DetaTow","Jet Eta (1 tow) ",10,0,1);
  jpTow[3]=new TH1F("phiTow","Jet Phi (1 tow) ",60,-180,180);
  jpTow[4]=new TH1F("TowE","Tow E (1 tow) ",100,0,25);

  for (int i=0;i<5;i++){
    HList->Add(jpTrk[i]);
    HList->Add(jpTow[i]);
    HList->Add(jpT[i]);
  }
  HList->Add(jpT[5]);

  float nbins0[nbins+1]={0.0,5.0,6.15,7.5645,9.30434,11.4443,14.0765,17.3141,21.2964,26.1945,32.2193,39.6297,48.7446,59.9558};

  //Trigger Bias	       
  HTfreq=new TH1F("HTfreq","Frequency of HT trig vs Eta",20,0,20);
  Count[0]=new TH1F("HTtrig","HTtrig",2,-0.5,1.5);
  Count[1]=new TH1F("BBCtrig","BBCtrig",2,0,2);
  Count[2]=new TH1F("Zver","Zver",400,-200,200);
  Count[3]=new TH1F("ZverBBC","ZverBBC",400,-200,200);
  Count[4]=new TH1F("JetBBCtrig","JetBBCtrig",2,0,2);
  Count[5]=new TH1F("JetZver","JetZver",400,-200,200);
  Count[6]=new TH1F("JetZverBBC","JetZverBBC",400,-200,200);
  Count[7]=new TH1F("allBBC","allBBC",2,0,2);
  Count[8]=new TH1F("JPtrig","JPtrig",2,-0.5,1.5);
  Count[9]=new TH1F("AHTtrig","AHTtrig",2,-0.5,1.5);
  Count[10]=new TH1F("HTid","HTid",4800,0,4800);
  Count[11]=new TH1F("JPid","JPid",6,0,6);
  Count[12]=new TH1F("AHTid","AHTid",4800,0,4800);
  Count[13]=new TH1F("HTdsm","HTdsm",100,0,100);
  Count[14]=new TH1F("JPdsm","JPdsm",200,0,200);
  Count[15]=new TH1F("AHTdsm","AHTdsm",100,0,100);
  
  HList->Add(HTfreq);  
  for (int i=0;i<16;i++){
    HList->Add(Count[i]);
  }
  
  partpT=new TH1F("partpT","Jet pT",nbins,nbins0);
  HTpartpT=new TH1F("HTpartpT","Jet pT (HT)",nbins,nbins0);
  JPpartpT=new TH1F("JPpartpT","Jet pT (JP)",nbins,nbins0);
  HList->Add(partpT);
  HList->Add(HTpartpT);
  HList->Add(JPpartpT);

  jpTA[0]=new TH1F("pT","Jet pT",nbins,nbins0);
  jpTA[1]=new TH1F("eta","Jet Eta",30,-3,3);
  jpTA[2]=new TH1F("phi","Jet Phi",60,-180,180);
  jpTA[3]=new TH1F("neu","Jet Neutral",100,0,1);

  qqjpTA[0]=new TH1F("qqpT","Jet pT (qq) ",nbins,nbins0);
  qqjpTA[1]=new TH1F("qqeta","Jet Eta (qq) ",30,-3,3);
  qqjpTA[2]=new TH1F("qqphi","Jet Phi (qq) ",60,-180,180);
  qqjpTA[3]=new TH1F("qqneu","Jet Neutral (qq) ",100,0,1);
  qgjpTA[0]=new TH1F("qgpT","Jet pT (qg) ",nbins,nbins0);
  qgjpTA[1]=new TH1F("qgeta","Jet Eta (qg) ",30,-3,3);
  qgjpTA[2]=new TH1F("qgphi","Jet Phi (qg) ",60,-180,180);
  qgjpTA[3]=new TH1F("qgneu","Jet Neutral (qg) ",100,0,1);
  ggjpTA[0]=new TH1F("ggpT","Jet pT (gg) ",nbins,nbins0);
  ggjpTA[1]=new TH1F("ggeta","Jet Eta (gg) ",30,-3,3);
  ggjpTA[2]=new TH1F("ggphi","Jet Phi (gg) ",60,-180,180);
  ggjpTA[3]=new TH1F("ggneu","Jet Neutral (gg) ",100,0,1);
  for (int i=0;i<4;i++){
    HList->Add(jpTA[i]);
    HList->Add(qqjpTA[i]);
    HList->Add(qgjpTA[i]);
    HList->Add(ggjpTA[i]);
  }



 //PYTHIA no BBC no HT/JP trig
  PYjpTA[0]=new TH1F("PYpT","Jet pT  ",nbins,nbins0);
  PYjpTA[1]=new TH1F("PYeta","Jet Eta  ",30,-3,3);
  PYjpTA[2]=new TH1F("PYphi","Jet Phi  ",60,-180,180);
  PYjpTA[3]=new TH1F("PYneu","Jet Neutral  ",100,0,1);
  qqPYjpTA[0]=new TH1F("qqPYpT","Jet pT (qq) ",nbins,nbins0);
  qqPYjpTA[1]=new TH1F("qqPYeta","Jet Eta (qq) ",30,-3,3);
  qqPYjpTA[2]=new TH1F("qqPYphi","Jet Phi (qq) ",60,-180,180);
  qqPYjpTA[3]=new TH1F("qqPYneu","Jet Neutral (qq) ",100,0,1);
  qgPYjpTA[0]=new TH1F("qgPYpT","Jet pT (qg) ",nbins,nbins0);
  qgPYjpTA[1]=new TH1F("qgPYeta","Jet Eta (qg) ",30,-3,3);
  qgPYjpTA[2]=new TH1F("qgPYphi","Jet Phi (qg) ",60,-180,180);
  qgPYjpTA[3]=new TH1F("qgPYneu","Jet Neutral (qg) ",100,0,1);
  ggPYjpTA[0]=new TH1F("ggPYpT","Jet pT (gg) ",nbins,nbins0);
  ggPYjpTA[1]=new TH1F("ggPYeta","Jet Eta (gg) ",30,-3,3);
  ggPYjpTA[2]=new TH1F("ggPYphi","Jet Phi (gg) ",60,-180,180);
  ggPYjpTA[3]=new TH1F("ggPYneu","Jet Neutral (gg) ",100,0,1);
  for (int i=0;i<4;i++){
    HList->Add(PYjpTA[i]);
    HList->Add(qqPYjpTA[i]);
    HList->Add(qgPYjpTA[i]);
    HList->Add(ggPYjpTA[i]);
  }



  //RAW no BBC no HT/JP trig
  RAWjpTA[0]=new TH1F("RAWpT","Jet pT  ",nbins,nbins0);
  RAWjpTA[1]=new TH1F("RAWeta","Jet Eta  ",30,-3,3);
  RAWjpTA[2]=new TH1F("RAWphi","Jet Phi  ",60,-180,180);
  RAWjpTA[3]=new TH1F("RAWneu","Jet Neutral  ",100,0,1);
  qqRAWjpTA[0]=new TH1F("qqRAWpT","Jet pT (qq) ",nbins,nbins0);
  qqRAWjpTA[1]=new TH1F("qqRAWeta","Jet Eta (qq) ",30,-3,3);
  qqRAWjpTA[2]=new TH1F("qqRAWphi","Jet Phi (qq) ",60,-180,180);
  qqRAWjpTA[3]=new TH1F("qqRAWneu","Jet Neutral (qq) ",100,0,1);
  qgRAWjpTA[0]=new TH1F("qgRAWpT","Jet pT (qg) ",nbins,nbins0);
  qgRAWjpTA[1]=new TH1F("qgRAWeta","Jet Eta (qg) ",30,-3,3);
  qgRAWjpTA[2]=new TH1F("qgRAWphi","Jet Phi (qg) ",60,-180,180);
  qgRAWjpTA[3]=new TH1F("qgRAWneu","Jet Neutral (qg) ",100,0,1);
  ggRAWjpTA[0]=new TH1F("ggRAWpT","Jet pT (gg) ",nbins,nbins0);
  ggRAWjpTA[1]=new TH1F("ggRAWeta","Jet Eta (gg) ",30,-3,3);
  ggRAWjpTA[2]=new TH1F("ggRAWphi","Jet Phi (gg) ",60,-180,180);
  ggRAWjpTA[3]=new TH1F("ggRAWneu","Jet Neutral (gg) ",100,0,1);
  for (int i=0;i<4;i++){
    HList->Add(RAWjpTA[i]);
    HList->Add(qqRAWjpTA[i]);
    HList->Add(qgRAWjpTA[i]);
    HList->Add(ggRAWjpTA[i]);
  }

  //HT Trig
  HTjpTA[0]=new TH1F("HTpT","Jet pT  ",nbins,nbins0);
  HTjpTA[1]=new TH1F("HTeta","Jet Eta  ",30,-3,3);
  HTjpTA[2]=new TH1F("HTphi","Jet Phi  ",60,-180,180);
  HTjpTA[3]=new TH1F("HTneu","Jet Neutral  ",100,0,1);
  HTjpTA[4]=new TH1F("HTneu1","Jet Neutral (Et=5-7)",100,0,1);
  HTjpTA[5]=new TH1F("HTneu2","Jet Neutral (Et=7-9=10)",100,0,1);
  HTjpTA[6]=new TH1F("HTneu3","Jet Neutral (Et=10-15)",100,0,1);
  HTjpTA[7]=new TH1F("HTneu4","Jet Neutral (Et=>15)",100,0,1);
  qqHTjpTA[0]=new TH1F("qqHTpT","Jet pT (qq) ",nbins,nbins0);
  qqHTjpTA[1]=new TH1F("qqHTeta","Jet Eta (qq) ",30,-3,3);
  qqHTjpTA[2]=new TH1F("qqHTphi","Jet Phi (qq) ",60,-180,180);
  qqHTjpTA[3]=new TH1F("qqHTneu","Jet Neutral (qq) ",100,0,1);
  qgHTjpTA[0]=new TH1F("qgHTpT","Jet pT (qg) ",nbins,nbins0);
  qgHTjpTA[1]=new TH1F("qgHTeta","Jet Eta (qg) ",30,-3,3);
  qgHTjpTA[2]=new TH1F("qgHTphi","Jet Phi (qg) ",60,-180,180);
  qgHTjpTA[3]=new TH1F("qgHTneu","Jet Neutral (qg) ",100,0,1);
  ggHTjpTA[0]=new TH1F("ggHTpT","Jet pT (gg) ",nbins,nbins0);
  ggHTjpTA[1]=new TH1F("ggHTeta","Jet Eta (gg) ",30,-3,3);
  ggHTjpTA[2]=new TH1F("ggHTphi","Jet Phi (gg) ",60,-180,180);
  ggHTjpTA[3]=new TH1F("ggHTneu","Jet Neutral (gg) ",100,0,1);
  for (int i=0;i<4;i++){
    HList->Add(HTjpTA[i]);
    HList->Add(qqHTjpTA[i]);
    HList->Add(qgHTjpTA[i]);
    HList->Add(ggHTjpTA[i]);
  }

  //JP trig
  JPjpTA[0]=new TH1F("JPpT","Jet pT  ",nbins,nbins0);
  JPjpTA[1]=new TH1F("JPeta","Jet Eta  ",30,-3,3);
  JPjpTA[2]=new TH1F("JPphi","Jet Phi  ",60,-180,180);
  JPjpTA[3]=new TH1F("JPneu","Jet Neutral  ",100,0,1);
  qqJPjpTA[0]=new TH1F("qqJPpT","Jet pT (qq) ",nbins,nbins0);
  qqJPjpTA[1]=new TH1F("qqJPeta","Jet Eta (qq) ",30,-3,3);
  qqJPjpTA[2]=new TH1F("qqJPphi","Jet Phi (qq) ",60,-180,180);
  qqJPjpTA[3]=new TH1F("qqJPneu","Jet Neutral (qq) ",100,0,1);
  qgJPjpTA[0]=new TH1F("qgJPpT","Jet pT (qg) ",nbins,nbins0);
  qgJPjpTA[1]=new TH1F("qgJPeta","Jet Eta (qg) ",30,-3,3);
  qgJPjpTA[2]=new TH1F("qgJPphi","Jet Phi (qg) ",60,-180,180);
  qgJPjpTA[3]=new TH1F("qgJPneu","Jet Neutral (qg) ",100,0,1);
  ggJPjpTA[0]=new TH1F("ggJPpT","Jet pT (gg) ",nbins,nbins0);
  ggJPjpTA[1]=new TH1F("ggJPeta","Jet Eta (gg) ",30,-3,3);
  ggJPjpTA[2]=new TH1F("ggJPphi","Jet Phi (gg) ",60,-180,180);
  ggJPjpTA[3]=new TH1F("ggJPneu","Jet Neutral (gg) ",100,0,1);
  for (int i=0;i<4;i++){
    HList->Add(JPjpTA[i]);
    HList->Add(qqJPjpTA[i]);
    HList->Add(ggJPjpTA[i]);
    HList->Add(qgJPjpTA[i]);
  }

  //JP&&HT
  jpTB[0]=new TH1F("HTJPpT","Jet pT  ",nbins,nbins0);
  jpTB[1]=new TH1F("HTJPeta","Jet Eta  ",30,-3,3);
  jpTB[2]=new TH1F("HTJPphi","Jet Phi  ",60,-180,180);
  jpTB[3]=new TH1F("HTJPneu","Jet Neutral  ",100,0,1);
  qqjpTB[0]=new TH1F("qqHTJPpT","Jet pT (qq) ",nbins,nbins0);
  qqjpTB[1]=new TH1F("qqHTJPeta","Jet Eta (qq) ",30,-3,3);
  qqjpTB[2]=new TH1F("qqHTJPphi","Jet Phi (qq) ",60,-180,180);
  qqjpTB[3]=new TH1F("qqHTJPneu","Jet Neutral (qq) ",100,0,1);
  qgjpTB[0]=new TH1F("qgHTJPpT","Jet pT (qg) ",nbins,nbins0);
  qgjpTB[1]=new TH1F("qgHTJPeta","Jet Eta (qg) ",30,-3,3);
  qgjpTB[2]=new TH1F("qgHTJPphi","Jet Phi (qg) ",60,-180,180);
  qgjpTB[3]=new TH1F("qgHTJPneu","Jet Neutral (qg) ",100,0,1);
  ggjpTB[0]=new TH1F("ggHTJPpT","Jet pT (gg) ",nbins,nbins0);
  ggjpTB[1]=new TH1F("ggHTJPeta","Jet Eta (gg) ",30,-3,3);
  ggjpTB[2]=new TH1F("ggHTJPphi","Jet Phi (gg) ",60,-180,180);
  ggjpTB[3]=new TH1F("ggHTJPneu","Jet Neutral (gg) ",100,0,1);
  for (int i=0;i<4;i++){
    HList->Add(jpTB[i]);
    HList->Add(qqjpTB[i]);
    HList->Add(qgjpTB[i]);
    HList->Add(ggjpTB[i]);
  }

  //hardpT
  hhard[0]=new TH2F("hardpT","hard vt Jet pT",nbins,nbins0,nbins,nbins0);
  RAWhard[0]=new TH2F("RAWhardpT","hard vs Jet pT",nbins,nbins0,nbins,nbins0); 
  HThard[0]=new TH2F("HThard","hard vs Jet pT (HT)",nbins,nbins0,nbins,nbins0);
  JPhard[0]=new TH2F("JPhard","hard vs Jet pT (JP)",nbins,nbins0,nbins,nbins0); 
  qqhard[0]=new TH2F("qqhard","hard vs Jet pT (qq)",nbins,nbins0,nbins,nbins0); 
  qqRAWhard[0]=new TH2F("qqRAWhard","hard vs Jet pT (qq)",nbins,nbins0,nbins,nbins0);  
  qqHThard[0]=new TH2F("qqHThard","hard vs Jet pT (qq HT)",nbins,nbins0,nbins,nbins0);  
  qqJPhard[0]=new TH2F("qqJPhard","hard vs Jet pT (qq JP)",nbins,nbins0,nbins,nbins0);  
  qghard[0]=new TH2F("qghard","hard vs Jet pT (qg)",nbins,nbins0,nbins,nbins0);  
  qgRAWhard[0]=new TH2F("qgRAWhard","hard vs Jet pT (qg)",nbins,nbins0,nbins,nbins0);  
  qgHThard[0]=new TH2F("qgHThard","hard vs Jet pT (qg HT)",nbins,nbins0,nbins,nbins0);  
  qgJPhard[0]=new TH2F("qgJPhard","hard vs Jet pT (qg JP)",nbins,nbins0,nbins,nbins0);  
  gghard[0]=new TH2F("gghard","hard vs Jet pT (gg)",nbins,nbins0,nbins,nbins0);  
  ggRAWhard[0]=new TH2F("ggRAWhard","s vs Jet pT (gg)",nbins,nbins0,nbins,nbins0);  
  ggHThard[0]=new TH2F("ggHThard","hard vs Jet pT (gg HT)",nbins,nbins0,nbins,nbins0);  
  ggJPhard[0]=new TH2F("ggJPhard","hard vs Jet pT (gg JP)",nbins,nbins0,nbins,nbins0);  
  for (int i=0;i<1;i++){
    HList->Add(hhard[i]);
    HList->Add(RAWhard[i]);
    HList->Add(HThard[i]);
    HList->Add(JPhard[i]);
    HList->Add(qqhard[i]);
    HList->Add(qqRAWhard[i]);
    HList->Add(qqHThard[i]);
    HList->Add(qqJPhard[i]);
    HList->Add(qghard[i]);
    HList->Add(qgRAWhard[i]);
    HList->Add(qgHThard[i]);
    HList->Add(qgJPhard[i]);
    HList->Add(gghard[i]);
    HList->Add(ggRAWhard[i]);
    HList->Add(ggHThard[i]);
    HList->Add(ggJPhard[i]);
  }

  //u
  /*  hu[0]=new TH2F("upT","u vt Jet pT",nbins,nbins0,4100,-40000,1000);
  RAWu[0]=new TH2F("RAWupT","u vs Jet pT",nbins,nbins0,4100,-40000,1000); 
  HTu[0]=new TH2F("HTu","u vs Jet pT (HT)",nbins,nbins0,4100,-40000,1000);
  JPu[0]=new TH2F("JPu","u vs Jet pT (JP)",nbins,nbins0,4100,-40000,1000); 
  qqu[0]=new TH2F("qqu","u vs Jet pT (qq)",nbins,nbins0,4100,-40000,1000); 
  qqRAWu[0]=new TH2F("qqRAWu","u vs Jet pT (qq)",nbins,nbins0,4100,-40000,1000);  
  qqHTu[0]=new TH2F("qqHTu","u vs Jet pT (qq HT)",nbins,nbins0,4100,-40000,1000);  
  qqJPu[0]=new TH2F("qqJPu","u vs Jet pT (qq JP)",nbins,nbins0,4100,-40000,1000);  
  qgu[0]=new TH2F("qgu","u vs Jet pT (qg)",nbins,nbins0,4100,-40000,1000);  
  qgRAWu[0]=new TH2F("qgRAWu","u vs Jet pT (qg)",nbins,nbins0,4100,-40000,1000);  
  qgHTu[0]=new TH2F("qgHTu","u vs Jet pT (qg HT)",nbins,nbins0,4100,-40000,1000);  
  qgJPu[0]=new TH2F("qgJPu","u vs Jet pT (qg JP)",nbins,nbins0,4100,-40000,1000);  
  ggu[0]=new TH2F("ggu","u vs Jet pT (gg)",nbins,nbins0,4100,-40000,1000);  
  ggRAWu[0]=new TH2F("ggRAWu","s vs Jet pT (gg)",nbins,nbins0,4100,-40000,1000);  
  ggHTu[0]=new TH2F("ggHTu","u vs Jet pT (gg HT)",nbins,nbins0,4100,-40000,1000);  
  ggJPu[0]=new TH2F("ggJPu","u vs Jet pT (gg JP)",nbins,nbins0,4100,-40000,1000);  
  for (int i=0;i<1;i++){
    HList->Add(hu[i]);
    HList->Add(RAWu[i]);
    HList->Add(HTu[i]);
    HList->Add(JPu[i]);
    HList->Add(qqu[i]);
    HList->Add(qqRAWu[i]);
    HList->Add(qqHTu[i]);
    HList->Add(qqJPu[i]);
    HList->Add(qgu[i]);
    HList->Add(qgRAWu[i]);
    HList->Add(qgHTu[i]);
    HList->Add(qgJPu[i]);
    HList->Add(ggu[i]);
    HList->Add(ggRAWu[i]);
    HList->Add(ggHTu[i]);
    HList->Add(ggJPu[i]);
  }
  
  //t
  ht[0]=new TH2F("tpT","t vt Jet pT",nbins,nbins0,2000,-10000,10000);
  RAWt[0]=new TH2F("RAWtpT","t vs Jet pT",nbins,nbins0,2000,-10000,10000); 
  HTt[0]=new TH2F("HTt","t vs Jet pT (HT)",nbins,nbins0,2000,-10000,10000);
  JPt[0]=new TH2F("JPt","t vs Jet pT (JP)",nbins,nbins0,2000,-10000,10000); 
  qqt[0]=new TH2F("qqt","t vs Jet pT (qq)",nbins,nbins0,2000,-10000,10000); 
  qqRAWt[0]=new TH2F("qqRAWt","t vs Jet pT (qq)",nbins,nbins0,2000,-10000,10000);  
  qqHTt[0]=new TH2F("qqHTt","t vs Jet pT (qq HT)",nbins,nbins0,2000,-10000,10000);  
  qqJPt[0]=new TH2F("qqJPt","t vs Jet pT (qq JP)",nbins,nbins0,2000,-10000,10000);  
  qgt[0]=new TH2F("qgt","t vs Jet pT (qg)",nbins,nbins0,2000,-10000,10000);  
  qgRAWt[0]=new TH2F("qgRAWt","t vs Jet pT (qg)",nbins,nbins0,2000,-10000,10000);  
  qgHTt[0]=new TH2F("qgHTt","t vs Jet pT (qg HT)",nbins,nbins0,2000,-10000,10000);  
  qgJPt[0]=new TH2F("qgJPt","t vs Jet pT (qg JP)",nbins,nbins0,2000,-10000,10000);  
  ggt[0]=new TH2F("ggt","t vs Jet pT (gg)",nbins,nbins0,2000,-10000,10000);  
  ggRAWt[0]=new TH2F("ggRAWt","s vs Jet pT (gg)",nbins,nbins0,2000,-10000,10000);  
  ggHTt[0]=new TH2F("ggHTt","t vs Jet pT (gg HT)",nbins,nbins0,2000,-10000,10000);  
  ggJPt[0]=new TH2F("ggJPt","t vs Jet pT (gg JP)",nbins,nbins0,2000,-10000,10000);  
  for (int i=0;i<1;i++){
    HList->Add(ht[i]);
    HList->Add(RAWt[i]);
    HList->Add(HTt[i]);
    HList->Add(JPt[i]);
    HList->Add(qqt[i]);
    HList->Add(qqRAWt[i]);
    HList->Add(qqHTt[i]);
    HList->Add(qqJPt[i]);
    HList->Add(qgt[i]);
    HList->Add(qgRAWt[i]);
    HList->Add(qgHTt[i]);
    HList->Add(qgJPt[i]);
    HList->Add(ggt[i]);
    HList->Add(ggRAWt[i]);
    HList->Add(ggHTt[i]);
    HList->Add(ggJPt[i]);
  }
  

  //s
  hs[0]=new TH2F("spT","s vs Jet pT",nbins,nbins0,1100,0,11000);
  RAWs[0]=new TH2F("RAWspT","s vs Jet pT",nbins,nbins0,1100,0,11000); 
  HTs[0]=new TH2F("HTs","s vs Jet pT (HT)",nbins,nbins0,1100,0,11000);
  JPs[0]=new TH2F("JPs","s vs Jet pT (JP)",nbins,nbins0,1100,0,11000); 
  qqs[0]=new TH2F("qqs","s vs Jet pT (qq)",nbins,nbins0,1100,0,11000); 
  qqRAWs[0]=new TH2F("qqRAWs","s vs Jet pT (qq)",nbins,nbins0,1100,0,11000);  
  qqHTs[0]=new TH2F("qqHTs","s vs Jet pT (qq HT)",nbins,nbins0,1100,0,11000);  
  qqJPs[0]=new TH2F("qqJPs","s vs Jet pT (qq JP)",nbins,nbins0,1100,0,11000);  
  qgs[0]=new TH2F("qgs","s vs Jet pT (qg)",nbins,nbins0,1100,0,11000);  
  qgRAWs[0]=new TH2F("qgRAWs","s vs Jet pT (qg)",nbins,nbins0,1100,0,11000);  
  qgHTs[0]=new TH2F("qgHTs","s vs Jet pT (qg HT)",nbins,nbins0,1100,0,11000);  
  qgJPs[0]=new TH2F("qgJPs","s vs Jet pT (qg JP)",nbins,nbins0,1100,0,11000);  
  ggs[0]=new TH2F("ggs","s vs Jet pT (gg)",nbins,nbins0,1100,0,11000);  
  ggRAWs[0]=new TH2F("ggRAWs","s vs Jet pT (gg)",nbins,nbins0,1100,0,11000);  
  ggHTs[0]=new TH2F("ggHTs","s vs Jet pT (gg HT)",nbins,nbins0,1100,0,11000);  
  ggJPs[0]=new TH2F("ggJPs","s vs Jet pT (gg JP)",nbins,nbins0,1100,0,11000);  
  for (int i=0;i<1;i++){
    HList->Add(hs[i]);
    HList->Add(RAWs[i]);
    HList->Add(HTs[i]);
    HList->Add(JPs[i]);
    HList->Add(qqs[i]);
    HList->Add(qqRAWs[i]);
    HList->Add(qqHTs[i]);
    HList->Add(qqJPs[i]);
    HList->Add(qgs[i]);
    HList->Add(qgRAWs[i]);
    HList->Add(qgHTs[i]);
    HList->Add(qgJPs[i]);
    HList->Add(ggs[i]);
    HList->Add(ggRAWs[i]);
    HList->Add(ggHTs[i]);
    HList->Add(ggJPs[i]);
  }
  


  //x1&x2
  hx2[0]=new TH2F("x2pT","x2 vs Jet pT",nbins,nbins0,100,0,1); 
  //hx2[1]=new TH2F("x2Eta","x2 vs Jet Eta",30,-3,3,100,0,1); 
  //hx2[2]=new TH2F("x2Phi","x2 vs Jet Phi",60,-180,180,100,0,1); 
  //hx2[3]=new TH2F("x2Neu","x2 vs Jet Neu",100,0,1,100,0,1);
  RAWx2[0]=new TH2F("RAWx2pT","x2 vs Jet pT",nbins,nbins0,100,0,1); 
  //RAWx2[1]=new TH2F("RAWx2Eta","x2 vs Jet Eta",30,-3,3,100,0,1); 
  //RAWx2[2]=new TH2F("RAWx2Phi","x2 vs Jet Phi",60,-180,180,100,0,1); 
  //RAWx2[3]=new TH2F("RAWx2Neu","x2 vs Jet Neu",100,0,1,100,0,1);
  HTx2[0]=new TH2F("HTx2","x2 vs Jet pT (HT)",nbins,nbins0,100,0,1);  
  //HTx2[1]=new TH2F("HTx2Eta","x2 vs Jet Eta (HT)",30,-3,3,100,0,1); 
  //HTx2[2]=new TH2F("HTx2Phi","x2 vs Jet Phi (HT)",60,-180,180,100,0,1); 
  //HTx2[3]=new TH2F("HTx2Neu","x2 vs Jet Neu (HT)",100,0,1,100,0,1); 
  JPx2[0]=new TH2F("JPx2","x2 vs Jet pT (JP)",nbins,nbins0,100,0,1);  
  //JPx2[1]=new TH2F("JPx2Eta","x2 vs Jet Eta (JP)",30,-3,3,100,0,1); 
  //JPx2[2]=new TH2F("JPx2Phi","x2 vs Jet Phi (JP)",60,-180,180,100,0,1); 
  //JPx2[3]=new TH2F("JPx2Neu","x2 vs Jet Neu (JP)",100,0,1,100,0,1); 
  qqx2[0]=new TH2F("qqx2","x2 vs Jet pT (qq)",nbins,nbins0,100,0,1);  
  //qqx2[1]=new TH2F("qqx2Eta","x2 vs Jet Eta (qq)",30,-3,3,100,0,1); 
  //qqx2[2]=new TH2F("qqx2Phi","x2 vs Jet Phi (qq)",60,-180,180,100,0,1); 
  //qqx2[3]=new TH2F("qqx2Neu","x2 vs Jet Neu (qq)",100,0,1,100,0,1);  
  qqRAWx2[0]=new TH2F("qqRAWx2","x2 vs Jet pT (qq)",nbins,nbins0,100,0,1);  
  //qqRAWx2[1]=new TH2F("qqRAWx2Eta","x2 vs Jet Eta (qq)",30,-3,3,100,0,1); 
  //qqRAWx2[2]=new TH2F("qqRAWx2Phi","x2 vs Jet Phi (qq)",60,-180,180,100,0,1); 
  //qqRAWx2[3]=new TH2F("qqRAWx2Neu","x2 vs Jet Neu (qq)",100,0,1,100,0,1); 
  qqHTx2[0]=new TH2F("qqHTx2","x2 vs Jet pT (qq HT)",nbins,nbins0,100,0,1);  
  //qqHTx2[1]=new TH2F("qqHTx2Eta","x2 vs Jet Eta (qq HT)",30,-3,3,100,0,1); 
  //qqHTx2[2]=new TH2F("qqHTx2Phi","x2 vs Jet Phi (qq HT)",60,-180,180,100,0,1); 
  //qqHTx2[3]=new TH2F("qqHTx2Neu","x2 vs Jet Neu (qq HT)",100,0,1,100,0,1); 
  qqJPx2[0]=new TH2F("qqJPx2","x2 vs Jet pT (qq JP)",nbins,nbins0,100,0,1);  
  //qqJPx2[1]=new TH2F("qqJPx2Eta","x2 vs Jet Eta (qq JP)",30,-3,3,100,0,1); 
  //qqJPx2[2]=new TH2F("qqJPx2Phi","x2 vs Jet Phi (qq JP)",60,-180,180,100,0,1); 
  //qqJPx2[3]=new TH2F("qqJPx2Neu","x2 vs Jet Neu (qq JP)",100,0,1,100,0,1);
  qgx2[0]=new TH2F("qgx2","x2 vs Jet pT (qg)",nbins,nbins0,100,0,1);  
  ///qgx2[1]=new TH2F("qgx2Eta","x2 vs Jet Eta (qg)",30,-3,3,100,0,1); 
  //qgx2[2]=new TH2F("qgx2Phi","x2 vs Jet Phi (qg)",60,-180,180,100,0,1); 
  //qgx2[3]=new TH2F("qgx2Neu","x2 vs Jet Neu (qg)",100,0,1,100,0,1); 
  qgRAWx2[0]=new TH2F("qgRAWx2","x2 vs Jet pT (qg)",nbins,nbins0,100,0,1);  
  //qgRAWx2[1]=new TH2F("qgRAWx2Eta","x2 vs Jet Eta (qg)",30,-3,3,100,0,1); 
  //qgRAWx2[2]=new TH2F("qgRAWx2Phi","x2 vs Jet Phi (qg)",60,-180,180,100,0,1); 
  //qgRAWx2[3]=new TH2F("qgRAWx2Neu","x2 vs Jet Neu (qg)",100,0,1,100,0,1); 
  qgHTx2[0]=new TH2F("qgHTx2","x2 vs Jet pT (qg HT)",nbins,nbins0,100,0,1);  
  //qgHTx2[1]=new TH2F("qgHTx2Eta","x2 vs Jet Eta (qg HT)",30,-3,3,100,0,1); 
  //qgHTx2[2]=new TH2F("qgHTx2Phi","x2 vs Jet Phi (qg HT)",60,-180,180,100,0,1); 
  //qgHTx2[3]=new TH2F("qgHTx2Neu","x2 vs Jet Neu (qg HT)",100,0,1,100,0,1); 
  qgJPx2[0]=new TH2F("qgJPx2","x2 vs Jet pT (qg JP)",nbins,nbins0,100,0,1);  
  //qgJPx2[1]=new TH2F("qgJPx2Eta","x2 vs Jet Eta (qg JP)",30,-3,3,100,0,1); 
  //qgJPx2[2]=new TH2F("qgJPx2Phi","x2 vs Jet Phi (qg JP)",60,-180,180,100,0,1); 
  //qgJPx2[3]=new TH2F("qgJPx2Neu","x2 vs Jet Neu (qg JP)",100,0,1,100,0,1);
  ggx2[0]=new TH2F("ggx2","x2 vs Jet pT (gg)",nbins,nbins0,100,0,1);  
  //ggx2[1]=new TH2F("ggx2Eta","x2 vs Jet Eta (gg)",30,-3,3,100,0,1); 
  //ggx2[2]=new TH2F("ggx2Phi","x2 vs Jet Phi (gg)",60,-180,180,100,0,1); 
  //ggx2[3]=new TH2F("ggx2Neu","x2 vs Jet Neu (gg)",100,0,1,100,0,1);
  ggRAWx2[0]=new TH2F("ggRAWx2","x2 vs Jet pT (gg)",nbins,nbins0,100,0,1);  
  //ggRAWx2[1]=new TH2F("ggRAWx2Eta","x2 vs Jet Eta (gg)",30,-3,3,100,0,1); 
  //ggRAWx2[2]=new TH2F("ggRAWx2Phi","x2 vs Jet Phi (gg)",60,-180,180,100,0,1); 
  //ggRAWx2[3]=new TH2F("ggRAWx2Neu","x2 vs Jet Neu (gg)",100,0,1,100,0,1); 
  ggHTx2[0]=new TH2F("ggHTx2","x2 vs Jet pT (gg HT)",nbins,nbins0,100,0,1);  
  //ggHTx2[1]=new TH2F("ggHTx2Eta","x2 vs Jet Eta (ggHT)",30,-3,3,100,0,1); 
  //ggHTx2[2]=new TH2F("ggHTx2Phi","x2 vs Jet Phi (ggHT)",60,-180,180,100,0,1); 
  //ggHTx2[3]=new TH2F("ggHTx2Neu","x2 vs Jet Neu (ggHT)",100,0,1,100,0,1); 
  ggJPx2[0]=new TH2F("ggJPx2","x2 vs Jet pT (gg JP)",nbins,nbins0,100,0,1);  
  //ggJPx2[1]=new TH2F("ggJPx2Eta","x2 vs Jet Eta (ggJP)",30,-3,3,100,0,1); 
  //ggJPx2[2]=new TH2F("ggJPx2Phi","x2 vs Jet Phi (ggJP)",60,-180,180,100,0,1); 
  //ggJPx2[3]=new TH2F("ggJPx2Neu","x2 vs Jet Neu (ggJP)",100,0,1,100,0,1); 
  for (int i=0;i<1;i++){
    HList->Add(hx2[i]);
    HList->Add(RAWx2[i]);
    HList->Add(HTx2[i]);
    HList->Add(JPx2[i]);
    HList->Add(qqx2[i]);
    HList->Add(qqRAWx2[i]);
    HList->Add(qqHTx2[i]);
    HList->Add(qqJPx2[i]);
    HList->Add(qgx2[i]);
    HList->Add(qgRAWx2[i]);
    HList->Add(qgHTx2[i]);
    HList->Add(qgJPx2[i]);
    HList->Add(ggx2[i]);
    HList->Add(ggRAWx2[i]);
    HList->Add(ggHTx2[i]);
    HList->Add(ggJPx2[i]);
  }
  
  hx1[0]=new TH2F("x1pT","x1 vs Jet pT",nbins,nbins0,100,0,1); 
  //hx1[1]=new TH2F("x1Eta","x1 vs Jet Eta",30,-3,3,100,0,1); 
  //hx1[2]=new TH2F("x1Phi","x1 vs Jet Phi",60,-180,180,100,0,1); 
  //hx1[3]=new TH2F("x1Neu","x1 vs Jet Neu",100,0,1,100,0,1); 
  RAWx1[0]=new TH2F("RAWx1pT","x1 vs Jet pT",nbins,nbins0,100,0,1); 
  //RAWx1[1]=new TH2F("RAWx1Eta","x1 vs Jet Eta",30,-3,3,100,0,1); 
  //RAWx1[2]=new TH2F("RAWx1Phi","x1 vs Jet Phi",60,-180,180,100,0,1); 
  //RAWx1[3]=new TH2F("RAWx1Neu","x1 vs Jet Neu",100,0,1,100,0,1); 
  HTx1[0]=new TH2F("HTx1","x1 vs Jet pT (HT)",nbins,nbins0,100,0,1);  
  //HTx1[1]=new TH2F("HTx1Eta","x1 vs Jet Eta (HT)",30,-3,3,100,0,1); 
  //HTx1[2]=new TH2F("HTx1Phi","x1 vs Jet Phi (HT)",60,-180,180,100,0,1); 
  //HTx1[3]=new TH2F("NTx1Neu","x1 vs Jet Neu (HT)",100,0,1,100,0,1); 
  JPx1[0]=new TH2F("JPx1","x1 vs Jet pT (JP)",nbins,nbins0,100,0,1);  
  //JPx1[1]=new TH2F("JPx1Eta","x1 vs Jet Eta (JP)",30,-3,3,100,0,1); 
  //JPx1[2]=new TH2F("JPx1Phi","x1 vs Jet Phi (JP)",60,-180,180,100,0,1); 
  //JPx1[3]=new TH2F("JPx1Neu","x1 vs Jet Neu (JP)",100,0,1,100,0,1); 
  qqx1[0]=new TH2F("qqx1","x1 vs Jet pT (qq)",nbins,nbins0,100,0,1);  
  //qqx1[1]=new TH2F("qqx1Eta","x1 vs Jet Eta (qq)",30,-3,3,100,0,1); 
  //qqx1[2]=new TH2F("qqx1Phi","x1 vs Jet Phi (qq)",60,-180,180,100,0,1); 
  //qqx1[3]=new TH2F("qqx1Neu","x1 vs Jet Neu (qq)",100,0,1,100,0,1); 
  qqRAWx1[0]=new TH2F("qqRAWx1","x1 vs Jet pT (qq)",nbins,nbins0,100,0,1);  
  //qqRAWx1[1]=new TH2F("qqRAWx1Eta","x1 vs Jet Eta (qq)",30,-3,3,100,0,1); 
  //qqRAWx1[2]=new TH2F("qqRAWx1Phi","x1 vs Jet Phi (qq)",60,-180,180,100,0,1); 
  //qqRAWx1[3]=new TH2F("qqRAWx1Neu","x1 vs Jet Neu (qq)",100,0,1,100,0,1); 
  qqHTx1[0]=new TH2F("qqHTx1","x1 vs Jet pT (qq HT)",nbins,nbins0,100,0,1);  
  //qqHTx1[1]=new TH2F("qqHTx1Eta","x1 vs Jet Eta (qq HT)",30,-3,3,100,0,1); 
  //qqHTx1[2]=new TH2F("qqHTx1Phi","x1 vs Jet Phi (qq HT)",60,-180,180,100,0,1); 
  //qqHTx1[3]=new TH2F("qqHTx1Neu","x1 vs Jet Neu (qq HT)",100,0,1,100,0,1); 
  qqJPx1[0]=new TH2F("qqJPx1","x1 vs Jet pT (qq JP)",nbins,nbins0,100,0,1);  
  //qqJPx1[1]=new TH2F("qqJPx1Eta","x1 vs Jet Eta (qq JP)",30,-3,3,100,0,1); 
  //qqJPx1[2]=new TH2F("qqJPx1Phi","x1 vs Jet Phi (qq JP)",60,-180,180,100,0,1); 
  //qqJPx1[3]=new TH2F("qqJPx1Neu","x1 vs Jet Neu (qq JP)",100,0,1,100,0,1); 
  qgx1[0]=new TH2F("qgx1","x1 vs Jet pT (qg)",nbins,nbins0,100,0,1);  
  //qgx1[1]=new TH2F("qgx1Eta","x1 vs Jet Eta (qg)",30,-3,3,100,0,1); 
  //qgx1[2]=new TH2F("qgx1Phi","x1 vs Jet Phi (qg)",60,-180,180,100,0,1); 
  //qgx1[3]=new TH2F("qgx1Neu","x1 vs Jet Neu (qg)",100,0,1,100,0,1);
  qgRAWx1[0]=new TH2F("qgRAWx1","x1 vs Jet pT (qg)",nbins,nbins0,100,0,1);  
  //qgRAWx1[1]=new TH2F("qgRAWx1Eta","x1 vs Jet Eta (qg)",30,-3,3,100,0,1); 
  //qgRAWx1[2]=new TH2F("qgRAWx1Phi","x1 vs Jet Phi (qg)",60,-180,180,100,0,1); 
  //qgRAWx1[3]=new TH2F("qgRAWx1Neu","x1 vs Jet Neu (qg)",100,0,1,100,0,1);
  qgHTx1[0]=new TH2F("qgHTx1","x1 vs Jet pT (qg HT)",nbins,nbins0,100,0,1);  
  //qgHTx1[1]=new TH2F("qgHTx1Eta","x1 vs Jet Eta (qg HT)",30,-3,3,100,0,1); 
  //qgHTx1[2]=new TH2F("qgHTx1Phi","x1 vs Jet Phi (qg HT)",60,-180,180,100,0,1); 
  //qgHTx1[3]=new TH2F("qgHTx1Neu","x1 vs Jet Neu (qg HT)",100,0,1,100,0,1); 
  qgJPx1[0]=new TH2F("qgJPx1","x1 vs Jet pT (qg JP)",nbins,nbins0,100,0,1);  
  //qgJPx1[1]=new TH2F("qgJPx1Eta","x1 vs Jet Eta (qg JP)",30,-3,3,100,0,1); 
  //qgJPx1[2]=new TH2F("qgJPx1Phi","x1 vs Jet Phi (qg JP)",60,-180,180,100,0,1); 
  //qgJPx1[3]=new TH2F("qgJPx1Neu","x1 vs Jet Neu (qg JP)",100,0,1,100,0,1);
  ggx1[0]=new TH2F("ggx1","x1 vs Jet pT (gg)",nbins,nbins0,100,0,1);  
  //ggx1[1]=new TH2F("ggx1Eta","x1 vs Jet Eta (gg)",30,-3,3,100,0,1); 
  //ggx1[2]=new TH2F("ggx1Phi","x1 vs Jet Phi (gg)",60,-180,180,100,0,1); 
  //ggx1[3]=new TH2F("ggx1Neu","x1 vs Jet Neu (gg)",100,0,1,100,0,1); 
  ggRAWx1[0]=new TH2F("ggRAWx1","x1 vs Jet pT (gg)",nbins,nbins0,100,0,1);  
  //ggRAWx1[1]=new TH2F("ggRAWx1Eta","x1 vs Jet Eta (gg)",30,-3,3,100,0,1); 
  //ggRAWx1[2]=new TH2F("ggRAWx1Phi","x1 vs Jet Phi (gg)",60,-180,180,100,0,1); 
  //ggRAWx1[3]=new TH2F("ggRAWx1Neu","x1 vs Jet Neu (gg)",100,0,1,100,0,1); 
  ggHTx1[0]=new TH2F("ggHTx1","x1 vs Jet pT (gg HT)",nbins,nbins0,100,0,1);  
  //ggHTx1[1]=new TH2F("ggHTx1Eta","x1 vs Jet Eta (ggHT)",30,-3,3,100,0,1); 
  //ggHTx1[2]=new TH2F("ggHTx1Phi","x1 vs Jet Phi (ggHT)",60,-180,180,100,0,1); 
  //ggHTx1[3]=new TH2F("ggHTx1Neu","x1 vs Jet Neu (ggHT)",100,0,1,100,0,1); 
  ggJPx1[0]=new TH2F("ggJPx1","x1 vs Jet pT (gg JP)",nbins,nbins0,100,0,1);  
  //ggJPx1[1]=new TH2F("ggJPx1Eta","x1 vs Jet Eta (ggJP)",30,-3,3,100,0,1); 
  //ggJPx1[2]=new TH2F("ggJPx1Phi","x1 vs Jet Phi (ggJP)",60,-180,180,100,0,1); 
  //ggJPx1[3]=new TH2F("ggJPx1Neu","x1 vs Jet Neu (ggJP)",100,0,1,100,0,1); 
  for (int i=0;i<1;i++){
    HList->Add(hx1[i]);
    HList->Add(RAWx1[i]);
    HList->Add(HTx1[i]);
    HList->Add(JPx1[i]);
    HList->Add(qqx1[i]);
    HList->Add(qqRAWx1[i]);
    HList->Add(qqHTx1[i]);
    HList->Add(qqJPx1[i]);
    HList->Add(qgx1[i]);
    HList->Add(qgRAWx1[i]);
    HList->Add(qgHTx1[i]);
    HList->Add(qgJPx1[i]);
    HList->Add(ggx1[i]);
    HList->Add(ggRAWx1[i]);
    HList->Add(ggHTx1[i]);
    HList->Add(ggJPx1[i]);
  }
  
  costh[0]=new TH2F("costhpT","costh vs Jet pT",50,0.,50.,100,0.,1.); 
  //costh[1]=new TH2F("costhEta","costh vs Jet Eta",30,-3.,3.,100,0.,1.); 
  //costh[2]=new TH2F("costhPhi","costh vs Jet Phi",60,-180.,180.,100,0.,1.); 
  //costh[3]=new TH2F("costhNeu","costh vs Jet Neu",100,0.,1.,100,0.,1.);
  RAWcosth[0]=new TH2F("RAWcosthpT","costh vs Jet pT",50,0.,50.,100,0.,1.); 
  //RAWcosth[1]=new TH2F("RAWcosthEta","costh vs Jet Eta",30,-3.,3.,100,0.,1.); 
  //RAWcosth[2]=new TH2F("RAWcosthPhi","costh vs Jet Phi",60,-180.,180.,100,0.,1.); 
  //RAWcosth[3]=new TH2F("RAWcosthNeu","costh vs Jet Neu",100,0.,1.,100,0.,1.);
  HTcosth[0]=new TH2F("HTcosthpT","costh vs Jet pT (HT)",nbins,nbins0,100,0,1); 
  //HTcosth[1]=new TH2F("HTcosthEta","costh vs Jet Eta",30,-3,3,100,0,1); 
  //HTcosth[2]=new TH2F("HTcosthPhi","costh vs Jet Phi",60,-180,180,100,0,1); 
  //HTcosth[3]=new TH2F("HTcosthNeu","costh vs Jet Neu",100,0,1,100,0,1); 
  JPcosth[0]=new TH2F("JPcosthpT","costh vs Jet pT (JP)",nbins,nbins0,100,0,1); 
  //JPcosth[1]=new TH2F("JPcosthEta","costh vs Jet Eta",30,-3,3,100,0,1); 
  //JPcosth[2]=new TH2F("JPcosthPhi","costh vs Jet Phi",60,-180,180,100,0,1); 
  //JPcosth[3]=new TH2F("JPcosthNeu","costh vs Jet Neu",100,0,1,100,0,1);
  qqcosth[0]=new TH2F("qqcosthpT","costh vs Jet pT (qq)",nbins,nbins0,100,0,1);  
  //qqcosth[1]=new TH2F("qqcosthEta","costh vs Jet Eta",30,-3,3,100,0,1); 
  //qqcosth[2]=new TH2F("qqcosthPhi","costh vs Jet Phi",60,-180,180,100,0,1); 
  //qqcosth[3]=new TH2F("qqcosthNeu","costh vs Jet Neu",100,0,1,100,0,1); 
  qqRAWcosth[0]=new TH2F("qqRAWcosthpT","costh vs Jet pT",50,0.,50.,100,0.,1.); 
  //qqRAWcosth[1]=new TH2F("qqRAWcosthEta","costh vs Jet Eta",30,-3.,3.,100,0.,1.); 
  //qqRAWcosth[2]=new TH2F("qqRAWcosthPhi","costh vs Jet Phi",60,-180.,180.,100,0.,1.); 
  //qqRAWcosth[3]=new TH2F("qqRAWcosthNeu","costh vs Jet Neu",100,0.,1.,100,0.,1.);
  qqHTcosth[0]=new TH2F("qqHTcosthpT","costh vs Jet pT (qq HT)",nbins,nbins0,100,0,1);   
  //qqHTcosth[1]=new TH2F("qqHTcosthEta","costh vs Jet Eta",30,-3,3,100,0,1); 
  //qqHTcosth[2]=new TH2F("qqHTcosthPhi","costh vs Jet Phi",60,-180,180,100,0,1); 
  //qqHTcosth[3]=new TH2F("qqHTcosthNeu","costh vs Jet Neu",100,0,1,100,0,1); 
  qqJPcosth[0]=new TH2F("qqJPcosthpT","costh vs Jet pT (qq JP)",nbins,nbins0,100,0,1);   
  //qqJPcosth[1]=new TH2F("qqJPcosthEta","costh vs Jet Eta",30,-3,3,100,0,1); 
  //qqJPcosth[2]=new TH2F("qqJPcosthPhi","costh vs Jet Phi",60,-180,180,100,0,1); 
  //qqJPcosth[3]=new TH2F("qqJPcosthNeu","costh vs Jet Neu",100,0,1,100,0,1); 
  qgcosth[0]=new TH2F("qgcosthpT","costh vs Jet pT (qg)",nbins,nbins0,100,0,1);   
  //qgcosth[1]=new TH2F("qgcosthEta","costh vs Jet Eta",30,-3,3,100,0,1); 
  //qgcosth[2]=new TH2F("qgcosthPhi","costh vs Jet Phi",60,-180,180,100,0,1); 
  //qgcosth[3]=new TH2F("qgcosthNeu","costh vs Jet Neu",100,0,1,100,0,1); 
  qgRAWcosth[0]=new TH2F("qgRAWcosthpT","costh vs Jet pT",50,0.,50.,100,0.,1.); 
  //qgRAWcosth[1]=new TH2F("qgRAWcosthEta","costh vs Jet Eta",30,-3.,3.,100,0.,1.); 
  //qgRAWcosth[2]=new TH2F("qgRAWcosthPhi","costh vs Jet Phi",60,-180.,180.,100,0.,1.); 
  //qgRAWcosth[3]=new TH2F("qgRAWcosthNeu","costh vs Jet Neu",100,0.,1.,100,0.,1.);
  qgHTcosth[0]=new TH2F("qgHTcosthpT","costh vs Jet pT (qg HT)",nbins,nbins0,100,0,1);   
  //qgHTcosth[1]=new TH2F("qgHTcosthEta","costh vs Jet Eta",30,-3,3,100,0,1); 
  //qgHTcosth[2]=new TH2F("qgHTcosthPhi","costh vs Jet Phi",60,-180,180,100,0,1); 
  //qgHTcosth[3]=new TH2F("qgHTcosthNeu","costh vs Jet Neu",100,0,1,100,0,1); 
  qgJPcosth[0]=new TH2F("qgJPcosthpT","costh vs Jet pT (qg JP)",nbins,nbins0,100,0,1);   
  //qgJPcosth[1]=new TH2F("qgJPcosthEta","costh vs Jet Eta",30,-3,3,100,0,1); 
  //qgJPcosth[2]=new TH2F("qgJPcosthPhi","costh vs Jet Phi",60,-180,180,100,0,1); 
  //qgJPcosth[3]=new TH2F("qgJPcosthNeu","costh vs Jet Neu",100,0,1,100,0,1); 
  ggcosth[0]=new TH2F("ggcosthpT","costh vs Jet pT (gg)",nbins,nbins0,100,0,1);   
  //ggcosth[1]=new TH2F("ggcosthEta","costh vs Jet Eta",30,-3,3,100,0,1); 
  //ggcosth[2]=new TH2F("ggcosthPhi","costh vs Jet Phi",60,-180,180,100,0,1); 
  //ggcosth[3]=new TH2F("ggcosthNeu","costh vs Jet Neu",100,0,1,100,0,1); 
  ggRAWcosth[0]=new TH2F("ggRAWcosthpT","costh vs Jet pT",50,0.,50.,100,0.,1.); 
  //ggRAWcosth[1]=new TH2F("ggRAWcosthEta","costh vs Jet Eta",30,-3.,3.,100,0.,1.); 
  //ggRAWcosth[2]=new TH2F("ggRAWcosthPhi","costh vs Jet Phi",60,-180.,180.,100,0.,1.); 
  //ggRAWcosth[3]=new TH2F("ggRAWcosthNeu","costh vs Jet Neu",100,0.,1.,100,0.,1.);
  ggHTcosth[0]=new TH2F("ggHTcosthpT","costh vs Jet pT (gg HT)",nbins,nbins0,100,0,1);   
  //ggHTcosth[1]=new TH2F("ggHTcosthEta","costh vs Jet Eta",30,-3,3,100,0,1); 
  //ggHTcosth[2]=new TH2F("ggHTcosthPhi","costh vs Jet Phi",60,-180,180,100,0,1); 
  //ggHTcosth[3]=new TH2F("ggHTcosthNeu","costh vs Jet Neu",100,0,1,100,0,1); 
  ggJPcosth[0]=new TH2F("ggJPcosthpT","costh vs Jet pT (gg JP)",nbins,nbins0,50,100,0,1);   
  //ggJPcosth[1]=new TH2F("ggJPcosthEta","costh vs Jet Eta",30,-3,3,100,0,1); 
  //ggJPcosth[2]=new TH2F("ggJPcosthPhi","costh vs Jet Phi",60,-180,180,100,0,1); 
  //ggJPcosth[3]=new TH2F("ggJPcosthNeu","costh vs Jet Neu",100,0,1,100,0,1); 
  for (int i=0;i<1;i++){
    HList->Add(costh[i]);
    HList->Add(RAWcosth[i]);
    HList->Add(JPcosth[i]);
    HList->Add(HTcosth[i]);
    HList->Add(qqcosth[i]);
    HList->Add(qqRAWcosth[i]);
    HList->Add(qqHTcosth[i]);
    HList->Add(qqJPcosth[i]);
    HList->Add(qgcosth[i]);
    HList->Add(qgRAWcosth[i]);
    HList->Add(qgHTcosth[i]); 
    HList->Add(qgJPcosth[i]);
    HList->Add(ggcosth[i]);
    HList->Add(ggRAWcosth[i]);
    HList->Add(ggJPcosth[i]);
    HList->Add(ggHTcosth[i]);
  }

  //Shape histo
  HTshapeTow=new TH2F("HTshapeTow","Jet Shape (HT)",9,0.0,4.5,100,0,1);
  qqHTshapeTow=new TH2F("qqHTshapeTow","Jet Shape (qqHT)",9,0.0,4.5,100,0,1);
  qgHTshapeTow=new TH2F("qgHTshapeTow","Jet Shape (qgHT)",9,0.0,4.5,100,0,1);
  ggHTshapeTow=new TH2F("ggHTshapeTow","Jet Shape (ggHT)",9,0.0,4.5,100,0,1);
  HTshapeTrk=new TH2F("HTshapeTrk","Jet Shape (HT)",9,0.0,4.5,100,0,1);
  qqHTshapeTrk=new TH2F("qqHTshapeTrk","Jet Shape (qqHT)",9,0.0,4.5,100,0,1);
  qgHTshapeTrk=new TH2F("qgHTshapeTrk","Jet Shape (qgHT)",9,0.0,4.5,100,0,1);
  ggHTshapeTrk=new TH2F("ggHTshapeTrk","Jet Shape (ggHT)",9,0.0,4.5,100,0,1);
  JPshapeTow=new TH2F("JPshapeTow","Jet Shape (JP)",9,0.0,4.5,100,0,1);
  qqJPshapeTow=new TH2F("qqJPshapeTow","Jet Shape (qqJP)",9,0.0,4.5,100,0,1);
  qgJPshapeTow=new TH2F("qgJPshapeTow","Jet Shape (qgJP)",9,0.0,4.5,100,0,1);
  ggJPshapeTow=new TH2F("ggJPshapeTow","Jet Shape (ggJP)",9,0.0,4.5,100,0,1);
  JPshapeTrk=new TH2F("JPshapeTrk","Jet Shape (JP)",9,0.0,4.5,100,0,1);
  qqJPshapeTrk=new TH2F("qqJPshapeTrk","Jet Shape (qqJP)",9,0.0,4.5,100,0,1);
  qgJPshapeTrk=new TH2F("qgJPshapeTrk","Jet Shape (qgJP)",9,0.0,4.5,100,0,1);
  ggJPshapeTrk=new TH2F("ggJPshapeTrk","Jet Shape (ggJP)",9,0.0,4.5,100,0,1);
  HList->Add(HTshapeTow);
  HList->Add(qqHTshapeTow);
  HList->Add(qgHTshapeTow);
  HList->Add(ggHTshapeTow);
  HList->Add(HTshapeTrk);
  HList->Add(qqHTshapeTrk);
  HList->Add(qgHTshapeTrk);
  HList->Add(ggHTshapeTrk);
  HList->Add(JPshapeTow);
  HList->Add(qqJPshapeTow);
  HList->Add(qgJPshapeTow);
  HList->Add(ggJPshapeTow);
  HList->Add(JPshapeTrk);
  HList->Add(qqJPshapeTrk);
  HList->Add(qgJPshapeTrk);
  HList->Add(ggJPshapeTrk);
  */
}

  

void StJetSimuReader::SimuHisto2004()
{
  
  //Single Track and Single Tower Jets
  jpT[0]=new TH1F("pT","Jet pT (1 track) ",50,0,50);
  jpT[1]=new TH1F("eta","Jet Eta (all) ",30,-1,2);
  jpT[2]=new TH1F("Deta","Jet Detector Eta (all) ",10,0,1);
  jpT[3]=new TH1F("phi","Jet Phi (all) ",60,-180,180);
  jpT[4]=new TH1F("pTtrkSum","Track pT sum (all) ",100,0,25);
  jpT[5]=new TH1F("ETowSum","Tow E sum (all) ",100,0,25);

  jpTrk[0]=new TH1F("pTtrk","Jet pT (1 track) ",50,0,50);
  jpTrk[1]=new TH1F("etaTrk","Jet Eta (1 track) ",30,-1,2);
  jpTrk[2]=new TH1F("DetaTrk","Jet Detector Eta (1 track) ",10,0,1);
  jpTrk[3]=new TH1F("phiTrk","Jet Phi (1 track) ",60,-180,180);
  jpTrk[4]=new TH1F("TrkPt","Track pT (1 track) ",100,0,25);

  jpTow[0]=new TH1F("pTtow","Jet pT (1 tow) ",50,0,50);
  jpTow[1]=new TH1F("etaTow","Jet Eta (1 tow) ",30,-1,2);
  jpTow[2]=new TH1F("DetaTow","Jet Eta (1 tow) ",10,0,1);
  jpTow[3]=new TH1F("phiTow","Jet Phi (1 tow) ",60,-180,180);
  jpTow[4]=new TH1F("TowE","Tow E (1 tow) ",100,0,25);

  for (int i=0;i<5;i++){
    HList->Add(jpTrk[i]);
    HList->Add(jpTow[i]);
    HList->Add(jpT[i]);
  }
  HList->Add(jpT[5]);

  //Trigger Bias	       
  HTfreq=new TH1F("HTfreq","Frequency of HT trig vs Eta",20,0,20);
  Count[0]=new TH1F("HTtrig","HTtrig",2,-0.5,1.5);
  Count[1]=new TH1F("BBCtrig","BBCtrig",2,0,2);
  Count[2]=new TH1F("Zver","Zver",400,-200,200);
  Count[3]=new TH1F("ZverBBC","ZverBBC",400,-200,200);
  Count[4]=new TH1F("JetBBCtrig","JetBBCtrig",2,0,2);
  Count[5]=new TH1F("JetZver","JetZver",400,-200,200);
  Count[6]=new TH1F("JetZverBBC","JetZverBBC",400,-200,200);
  Count[7]=new TH1F("allBBC","allBBC",2,0,2);
  HList->Add(HTfreq);  
  for (int i=0;i<8;i++){
    HList->Add(Count[i]);
  }



  jpTA[0]=new TH1F("pTA","Jet pT (Case A) ",50,0,50);
  jpTB[0]=new TH1F("pTB","Jet pT (Case B) ",50,0,50);
  jpTC[0]=new TH1F("pTC","Jet pT (Case C) ",50,0,50);
  jpTD[0]=new TH1F("pTD","Jet pT (Case D) ",50,0,50);
  jpTA[1]=new TH1F("etaA","Jet Eta (Case A) ",30,-3,3);
  jpTB[1]=new TH1F("etaB","Jet Eta (Case B) ",30,-3,3);
  jpTC[1]=new TH1F("etaC","Jet Eta (Case C) ",30,-3,3);
  jpTD[1]=new TH1F("etaD","Jet Eta (Case D) ",30,-3,3);
  jpTA[2]=new TH1F("phiA","Jet Phi (Case A) ",60,-180,180);
  jpTB[2]=new TH1F("phiB","Jet Phi (Case B) ",60,-180,180);
  jpTC[2]=new TH1F("phiC","Jet Phi (Case C) ",60,-180,180);
  jpTD[2]=new TH1F("phiD","Jet Phi (Case D) ",60,-180,180);
  jpTA[3]=new TH1F("neuA","Jet Neutral (Case A) ",100,0,1);
  jpTB[3]=new TH1F("neuB","Jet Neutral (Case B) ",100,0,1);
  jpTC[3]=new TH1F("neuC","Jet Neutral (Case C) ",100,0,1);
  jpTD[3]=new TH1F("neuD","Jet Neutral (Case D) ",100,0,1);
  for (int i=0;i<4;i++){
    HList->Add(jpTA[i]);
    HList->Add(jpTB[i]);
    HList->Add(jpTC[i]);
    HList->Add(jpTD[i]);
  }

  HTjpTA[0]=new TH1F("HTpTA","Jet pT (Case A) ",50,0,50);
  HTjpTB[0]=new TH1F("HTpTB","Jet pT (Case B) ",50,0,50);
  HTjpTC[0]=new TH1F("HTpTC","Jet pT (Case C) ",50,0,50);
  HTjpTD[0]=new TH1F("HTpTD","Jet pT (Case D) ",50,0,50);
  HTjpTA[1]=new TH1F("HTetaA","Jet Eta (Case A) ",30,-3,3);
  HTjpTB[1]=new TH1F("HTetaB","Jet Eta (Case B) ",30,-3,3);
  HTjpTC[1]=new TH1F("HTetaC","Jet Eta (Case C) ",30,-3,3);
  HTjpTD[1]=new TH1F("HTetaD","Jet Eta (Case D) ",30,-3,3);
  HTjpTA[2]=new TH1F("HTphiA","Jet Phi (Case A) ",60,-180,180);
  HTjpTB[2]=new TH1F("HTphiB","Jet Phi (Case B) ",60,-180,180);
  HTjpTC[2]=new TH1F("HTphiC","Jet Phi (Case C) ",60,-180,180);
  HTjpTD[2]=new TH1F("HTphiD","Jet Phi (Case D) ",60,-180,180);
  HTjpTA[3]=new TH1F("HTneuA","Jet Neutral (Case A) ",100,0,1);
  HTjpTB[3]=new TH1F("HTneuB","Jet Neutral (Case B) ",100,0,1);
  HTjpTC[3]=new TH1F("HTneuC","Jet Neutral (Case C) ",100,0,1);
  HTjpTD[3]=new TH1F("HTneuD","Jet Neutral (Case D) ",100,0,1);
  for (int i=0;i<4;i++){
    HList->Add(HTjpTA[i]);
    HList->Add(HTjpTB[i]);
    HList->Add(HTjpTC[i]);
    HList->Add(HTjpTD[i]);
  }

  qqjpTA[0]=new TH1F("qqpTA","Jet pT (qq Case A) ",50,0,50);
  qqjpTB[0]=new TH1F("qqpTB","Jet pT (qq Case B) ",50,0,50);
  qqjpTC[0]=new TH1F("qqpTC","Jet pT (qq Case C) ",50,0,50);
  qqjpTD[0]=new TH1F("qqpTD","Jet pT (qq Case D) ",50,0,50);
  qqjpTA[1]=new TH1F("qqetaA","Jet Eta (qq Case A) ",30,-3,3);
  qqjpTB[1]=new TH1F("qqetaB","Jet Eta (qq Case B) ",30,-3,3);
  qqjpTC[1]=new TH1F("qqetaC","Jet Eta (qq Case C) ",30,-3,3);
  qqjpTD[1]=new TH1F("qqetaD","Jet Eta (qq Case D) ",30,-3,3);
  qqjpTA[2]=new TH1F("qqphiA","Jet Phi (qq Case A) ",60,-180,180);
  qqjpTB[2]=new TH1F("qqphiB","Jet Phi (qq Case B) ",60,-180,180);
  qqjpTC[2]=new TH1F("qqphiC","Jet Phi (qq Case C) ",60,-180,180);
  qqjpTD[2]=new TH1F("qqphiD","Jet Phi (qq Case D) ",60,-180,180);
  qqjpTA[3]=new TH1F("qqneuA","Jet Neutral (qq Case A) ",100,0,1);
  qqjpTB[3]=new TH1F("qqneuB","Jet Neutral (qq Case B) ",100,0,1);
  qqjpTC[3]=new TH1F("qqneuC","Jet Neutral (qq Case C) ",100,0,1);
  qqjpTD[3]=new TH1F("qqneuD","Jet Neutral (qq Case D) ",100,0,1);
  for (int i=0;i<4;i++){
    HList->Add(qqjpTA[i]);
    HList->Add(qqjpTB[i]);
    HList->Add(qqjpTC[i]);
    HList->Add(qqjpTD[i]);
  }

  qqHTjpTA[0]=new TH1F("qqHTpTA","Jet pT (qq Case A) ",50,0,50);
  qqHTjpTB[0]=new TH1F("qqHTpTB","Jet pT (qq Case B) ",50,0,50);
  qqHTjpTC[0]=new TH1F("qqHTpTC","Jet pT (qq Case C) ",50,0,50);
  qqHTjpTD[0]=new TH1F("qqHTpTD","Jet pT (qq Case D) ",50,0,50);
  qqHTjpTA[1]=new TH1F("qqHTetaA","Jet Eta (qq Case A) ",30,-3,3);
  qqHTjpTB[1]=new TH1F("qqHTetaB","Jet Eta (qq Case B) ",30,-3,3);
  qqHTjpTC[1]=new TH1F("qqHTetaC","Jet Eta (qq Case C) ",30,-3,3);
  qqHTjpTD[1]=new TH1F("qqHTetaD","Jet Eta (qq Case D) ",30,-3,3);
  qqHTjpTA[2]=new TH1F("qqHTphiA","Jet Phi (qq Case A) ",60,-180,180);
  qqHTjpTB[2]=new TH1F("qqHTphiB","Jet Phi (qq Case B) ",60,-180,180);
  qqHTjpTC[2]=new TH1F("qqHTphiC","Jet Phi (qq Case C) ",60,-180,180);
  qqHTjpTD[2]=new TH1F("qqHTphiD","Jet Phi (qq Case D) ",60,-180,180);
  qqHTjpTA[3]=new TH1F("qqHTneuA","Jet Neutral (qq Case A) ",100,0,1);
  qqHTjpTB[3]=new TH1F("qqHTneuB","Jet Neutral (qq Case B) ",100,0,1);
  qqHTjpTC[3]=new TH1F("qqHTneuC","Jet Neutral (qq Case C) ",100,0,1);
  qqHTjpTD[3]=new TH1F("qqHTneuD","Jet Neutral (qq Case D) ",100,0,1);
  for (int i=0;i<4;i++){
    HList->Add(qqHTjpTA[i]);
    HList->Add(qqHTjpTB[i]);
    HList->Add(qqHTjpTC[i]);
    HList->Add(qqHTjpTD[i]);
  }

  qgjpTA[0]=new TH1F("qgpTA","Jet pT (qg Case A) ",50,0,50);
  qgjpTB[0]=new TH1F("qgpTB","Jet pT (qg Case B) ",50,0,50);
  qgjpTC[0]=new TH1F("qgpTC","Jet pT (qg Case C) ",50,0,50);
  qgjpTD[0]=new TH1F("qgpTD","Jet pT (qg Case D) ",50,0,50);
  qgjpTA[1]=new TH1F("qgetaA","Jet Eta (qg Case A) ",30,-3,3);
  qgjpTB[1]=new TH1F("qgetaB","Jet Eta (qg Case B) ",30,-3,3);
  qgjpTC[1]=new TH1F("qgetaC","Jet Eta (qg Case C) ",30,-3,3);
  qgjpTD[1]=new TH1F("qgetaD","Jet Eta (qg Case D) ",30,-3,3);
  qgjpTA[2]=new TH1F("qgphiA","Jet Phi (qg Case A) ",60,-180,180);
  qgjpTB[2]=new TH1F("qgphiB","Jet Phi (qg Case B) ",60,-180,180);
  qgjpTC[2]=new TH1F("qgphiC","Jet Phi (qg Case C) ",60,-180,180);
  qgjpTD[2]=new TH1F("qgphiD","Jet Phi (qg Case D) ",60,-180,180);
  qgjpTA[3]=new TH1F("qgneuA","Jet Neutral (qg Case A) ",100,0,1);
  qgjpTB[3]=new TH1F("qgneuB","Jet Neutral (qg Case B) ",100,0,1);
  qgjpTC[3]=new TH1F("qgneuC","Jet Neutral (qg Case C) ",100,0,1);
  qgjpTD[3]=new TH1F("qgneuD","Jet Neutral (qg Case D) ",100,0,1);
  for (int i=0;i<4;i++){
    HList->Add(qgjpTA[i]);
    HList->Add(qgjpTB[i]);
    HList->Add(qgjpTC[i]);
    HList->Add(qgjpTD[i]);
  }

  qgHTjpTA[0]=new TH1F("qgHTpTA","Jet pT (qg Case A) ",50,0,50);
  qgHTjpTB[0]=new TH1F("qgHTpTB","Jet pT (qg Case B) ",50,0,50);
  qgHTjpTC[0]=new TH1F("qgHTpTC","Jet pT (qg Case C) ",50,0,50);
  qgHTjpTD[0]=new TH1F("qgHTpTD","Jet pT (qg Case D) ",50,0,50);
  qgHTjpTA[1]=new TH1F("qgHTetaA","Jet Eta (qg Case A) ",30,-3,3);
  qgHTjpTB[1]=new TH1F("qgHTetaB","Jet Eta (qg Case B) ",30,-3,3);
  qgHTjpTC[1]=new TH1F("qgHTetaC","Jet Eta (qg Case C) ",30,-3,3);
  qgHTjpTD[1]=new TH1F("qgHTetaD","Jet Eta (qg Case D) ",30,-3,3);
  qgHTjpTA[2]=new TH1F("qgHTphiA","Jet Phi (qg Case A) ",60,-180,180);
  qgHTjpTB[2]=new TH1F("qgHTphiB","Jet Phi (qg Case B) ",60,-180,180);
  qgHTjpTC[2]=new TH1F("qgHTphiC","Jet Phi (qg Case C) ",60,-180,180);
  qgHTjpTD[2]=new TH1F("qgHTphiD","Jet Phi (qg Case D) ",60,-180,180);
  qgHTjpTA[3]=new TH1F("qgHTneuA","Jet Neutral (qg Case A) ",100,0,1);
  qgHTjpTB[3]=new TH1F("qgHTneuB","Jet Neutral (qg Case B) ",100,0,1);
  qgHTjpTC[3]=new TH1F("qgHTneuC","Jet Neutral (qg Case C) ",100,0,1);
  qgHTjpTD[3]=new TH1F("qgHTneuD","Jet Neutral (qg Case D) ",100,0,1);
  for (int i=0;i<4;i++){
    HList->Add(qgHTjpTA[i]);
    HList->Add(qgHTjpTB[i]);
    HList->Add(qgHTjpTC[i]);
    HList->Add(qgHTjpTD[i]);
  }

  ggjpTA[0]=new TH1F("ggpTA","Jet pT (gg Case A) ",50,0,50);
  ggjpTB[0]=new TH1F("ggpTB","Jet pT (gg Case B) ",50,0,50);
  ggjpTC[0]=new TH1F("ggpTC","Jet pT (gg Case C) ",50,0,50);
  ggjpTD[0]=new TH1F("ggpTD","Jet pT (gg Case D) ",50,0,50);
  ggjpTA[1]=new TH1F("ggetaA","Jet Eta (gg Case A) ",30,-3,3);
  ggjpTB[1]=new TH1F("ggetaB","Jet Eta (gg Case B) ",30,-3,3);
  ggjpTC[1]=new TH1F("ggetaC","Jet Eta (gg Case C) ",30,-3,3);
  ggjpTD[1]=new TH1F("ggetaD","Jet Eta (gg Case D) ",30,-3,3);
  ggjpTA[2]=new TH1F("ggphiA","Jet Phi (gg Case A) ",60,-180,180);
  ggjpTB[2]=new TH1F("ggphiB","Jet Phi (gg Case B) ",60,-180,180);
  ggjpTC[2]=new TH1F("ggphiC","Jet Phi (gg Case C) ",60,-180,180);
  ggjpTD[2]=new TH1F("ggphiD","Jet Phi (gg Case D) ",60,-180,180);
  ggjpTA[3]=new TH1F("ggneuA","Jet Neutral (gg Case A) ",100,0,1);
  ggjpTB[3]=new TH1F("ggneuB","Jet Neutral (gg Case B) ",100,0,1);
  ggjpTC[3]=new TH1F("ggneuC","Jet Neutral (gg Case C) ",100,0,1);
  ggjpTD[3]=new TH1F("ggneuD","Jet Neutral (gg Case D) ",100,0,1);
  for (int i=0;i<4;i++){
    HList->Add(ggjpTA[i]);
    HList->Add(ggjpTB[i]);
    HList->Add(ggjpTC[i]);
    HList->Add(ggjpTD[i]);
  }

  ggHTjpTA[0]=new TH1F("ggHTpTA","Jet pT (gg Case A) ",50,0,50);
  ggHTjpTB[0]=new TH1F("ggHTpTB","Jet pT (gg Case B) ",50,0,50);
  ggHTjpTC[0]=new TH1F("ggHTpTC","Jet pT (gg Case C) ",50,0,50);
  ggHTjpTD[0]=new TH1F("ggHTpTD","Jet pT (gg Case D) ",50,0,50);
  ggHTjpTA[1]=new TH1F("ggHTetaA","Jet Eta (gg Case A) ",30,-3,3);
  ggHTjpTB[1]=new TH1F("ggHTetaB","Jet Eta (gg Case B) ",30,-3,3);
  ggHTjpTC[1]=new TH1F("ggHTetaC","Jet Eta (gg Case C) ",30,-3,3);
  ggHTjpTD[1]=new TH1F("ggHTetaD","Jet Eta (gg Case D) ",30,-3,3);
  ggHTjpTA[2]=new TH1F("ggHTphiA","Jet Phi (gg Case A) ",60,-180,180);
  ggHTjpTB[2]=new TH1F("ggHTphiB","Jet Phi (gg Case B) ",60,-180,180);
  ggHTjpTC[2]=new TH1F("ggHTphiC","Jet Phi (gg Case C) ",60,-180,180);
  ggHTjpTD[2]=new TH1F("ggHTphiD","Jet Phi (gg Case D) ",60,-180,180);
  ggHTjpTA[3]=new TH1F("ggHTneuA","Jet Neutral (gg Case A) ",100,0,1);
  ggHTjpTB[3]=new TH1F("ggHTneuB","Jet Neutral (gg Case B) ",100,0,1);
  ggHTjpTC[3]=new TH1F("ggHTneuC","Jet Neutral (gg Case C) ",100,0,1);
  ggHTjpTD[3]=new TH1F("ggHTneuD","Jet Neutral (gg Case D) ",100,0,1);
  for (int i=0;i<4;i++){
    HList->Add(ggHTjpTA[i]);
    HList->Add(ggHTjpTB[i]);
    HList->Add(ggHTjpTC[i]);
    HList->Add(ggHTjpTD[i]);
  }

  hx1[0]=new TH2F("x1A","x1 vs Jet pT (CASE A)",50,0,50,100,0,1); 
  hx1[1]=new TH2F("x1B","x1 vs Jet pT (CASE B)",50,0,50,100,0,1); 
  hx1[2]=new TH2F("x1C","x1 vs Jet pT (CASE C)",50,0,50,100,0,1); 
  hx1[3]=new TH2F("x1D","x1 vs Jet pT (CASE D)",50,0,50,100,0,1);
  HTx1[0]=new TH2F("HTx1A","x1 vs Jet pT (HT CASE A)",50,0,50,100,0,1); 
  HTx1[1]=new TH2F("HTx1B","x1 vs Jet pT (HT CASE B)",50,0,50,100,0,1); 
  HTx1[2]=new TH2F("HTx1C","x1 vs Jet pT (HT CASE C)",50,0,50,100,0,1); 
  HTx1[3]=new TH2F("HTx1D","x1 vs Jet pT (HT CASE D)",50,0,50,100,0,1);
  qqx1[0]=new TH2F("qqx1A","x1 vs Jet pT (qq CASE A)",50,0,50,100,0,1); 
  qqx1[1]=new TH2F("qqx1B","x1 vs Jet pT (qq CASE B)",50,0,50,100,0,1); 
  qqx1[2]=new TH2F("qqx1C","x1 vs Jet pT (qq CASE C)",50,0,50,100,0,1); 
  qqx1[3]=new TH2F("qqx1D","x1 vs Jet pT (qq CASE D)",50,0,50,100,0,1);
  qqHTx1[0]=new TH2F("qqHTx1A","x1 vs Jet pT (qq HT CASE A)",50,0,50,100,0,1); 
  qqHTx1[1]=new TH2F("qqHTx1B","x1 vs Jet pT (qq HT CASE B)",50,0,50,100,0,1); 
  qqHTx1[2]=new TH2F("qqHTx1C","x1 vs Jet pT (qq HT CASE C)",50,0,50,100,0,1); 
  qqHTx1[3]=new TH2F("qqHTx1D","x1 vs Jet pT (qq HT CASE D)",50,0,50,100,0,1);
  qgx1[0]=new TH2F("qgx1A","x1 vs Jet pT (qg CASE A)",50,0,50,100,0,1); 
  qgx1[1]=new TH2F("qgx1B","x1 vs Jet pT (qgCASE B)",50,0,50,100,0,1); 
  qgx1[2]=new TH2F("qgx1C","x1 vs Jet pT (qg CASE C)",50,0,50,100,0,1); 
  qgx1[3]=new TH2F("qgx1D","x1 vs Jet pT (qg CASE D)",50,0,50,100,0,1);
  qgHTx1[0]=new TH2F("qgHTx1A","x1 vs Jet pT (qg HT CASE A)",50,0,50,100,0,1); 
  qgHTx1[1]=new TH2F("qgHTx1B","x1 vs Jet pT (qg HT CASE B)",50,0,50,100,0,1); 
  qgHTx1[2]=new TH2F("qgHTx1C","x1 vs Jet pT (qg HT CASE C)",50,0,50,100,0,1);
  qgHTx1[3]=new TH2F("qgHTx1D","x1 vs Jet pT (qg HT CASE D)",50,0,50,100,0,1); 
  ggx1[0]=new TH2F("ggx1A","x1 vs Jet pT (gg CASE A)",50,0,50,100,0,1); 
  ggx1[1]=new TH2F("ggx1B","x1 vs Jet pT (gg CASE B)",50,0,50,100,0,1); 
  ggx1[2]=new TH2F("ggx1C","x1 vs Jet pT (gg CASE C)",50,0,50,100,0,1); 
  ggx1[3]=new TH2F("ggx1D","x1 vs Jet pT (gg CASE D)",50,0,50,100,0,1);
  ggHTx1[0]=new TH2F("ggHTx1A","x1 vs Jet pT (gg HT CASE A)",50,0,50,100,0,1); 
  ggHTx1[1]=new TH2F("ggHTx1B","x1 vs Jet pT (gg HT CASE B)",50,0,50,100,0,1); 
  ggHTx1[2]=new TH2F("ggHTx1C","x1 vs Jet pT (gg HT CASE C)",50,0,50,100,0,1); 
  ggHTx1[3]=new TH2F("ggHTx1D","x1 vs Jet pT (gg HT CASE D)",50,0,50,100,0,1);

  hx2[0]=new TH2F("x2A","x2 vs Jet pT (CASE A)",50,0,50,100,0,1); 
  hx2[1]=new TH2F("x2B","x2 vs Jet pT (CASE B)",50,0,50,100,0,1); 
  hx2[2]=new TH2F("x2C","x2 vs Jet pT (CASE C)",50,0,50,100,0,1); 
  hx2[3]=new TH2F("x2D","x2 vs Jet pT (CASE D)",50,0,50,100,0,1);
  HTx2[0]=new TH2F("HTx2A","x2 vs Jet pT (HT CASE A)",50,0,50,100,0,1); 
  HTx2[1]=new TH2F("HTx2B","x2 vs Jet pT (HT CASE B)",50,0,50,100,0,1); 
  HTx2[2]=new TH2F("HTx2C","x2 vs Jet pT (HT CASE C)",50,0,50,100,0,1); 
  HTx2[3]=new TH2F("HTx2D","x2 vs Jet pT (HT CASE D)",50,0,50,100,0,1);
  qqx2[0]=new TH2F("qqx2A","x2 vs Jet pT (qq CASE A)",50,0,50,100,0,1); 
  qqx2[1]=new TH2F("qqx2B","x2 vs Jet pT (qq CASE B)",50,0,50,100,0,1); 
  qqx2[2]=new TH2F("qqx2C","x2 vs Jet pT (qq CASE C)",50,0,50,100,0,1); 
  qqx2[3]=new TH2F("qqx2D","x2 vs Jet pT (qq CASE D)",50,0,50,100,0,1);
  qqHTx2[0]=new TH2F("qqHTx2A","x2 vs Jet pT (qq HT CASE A)",50,0,50,100,0,1); 
  qqHTx2[1]=new TH2F("qqHTx2B","x2 vs Jet pT (qq HT CASE B)",50,0,50,100,0,1); 
  qqHTx2[2]=new TH2F("qqHTx2C","x2 vs Jet pT (qq HT CASE C)",50,0,50,100,0,1); 
  qqHTx2[3]=new TH2F("qqHTx2D","x2 vs Jet pT (qq HT CASE D)",50,0,50,100,0,1);
  qgx2[0]=new TH2F("qgx2A","x2 vs Jet pT (qg CASE A)",50,0,50,100,0,1); 
  qgx2[1]=new TH2F("qgx2B","x2 vs Jet pT (qgCASE B)",50,0,50,100,0,1); 
  qgx2[2]=new TH2F("qgx2C","x2 vs Jet pT (qg CASE C)",50,0,50,100,0,1); 
  qgx2[3]=new TH2F("qgx2D","x2 vs Jet pT (qg CASE D)",50,0,50,100,0,1);
  qgHTx2[0]=new TH2F("qgHTx2A","x2 vs Jet pT (qg HT CASE A)",50,0,50,100,0,1); 
  qgHTx2[1]=new TH2F("qgHTx2B","x2 vs Jet pT (qg HT CASE B)",50,0,50,100,0,1); 
  qgHTx2[2]=new TH2F("qgHTx2C","x2 vs Jet pT (qg HT CASE C)",50,0,50,100,0,1);
  qgHTx2[3]=new TH2F("qgHTx2D","x2 vs Jet pT (qg HT CASE D)",50,0,50,100,0,1); 
  ggx2[0]=new TH2F("ggx2A","x2 vs Jet pT (gg CASE A)",50,0,50,100,0,1); 
  ggx2[1]=new TH2F("ggx2B","x2 vs Jet pT (gg CASE B)",50,0,50,100,0,1); 
  ggx2[2]=new TH2F("ggx2C","x2 vs Jet pT (gg CASE C)",50,0,50,100,0,1); 
  ggx2[3]=new TH2F("ggx2D","x2 vs Jet pT (gg CASE D)",50,0,50,100,0,1);
  ggHTx2[0]=new TH2F("ggHTx2A","x2 vs Jet pT (gg HT CASE A)",50,0,50,100,0,1); 
  ggHTx2[1]=new TH2F("ggHTx2B","x2 vs Jet pT (gg HT CASE B)",50,0,50,100,0,1); 
  ggHTx2[2]=new TH2F("ggHTx2C","x2 vs Jet pT (gg HT CASE C)",50,0,50,100,0,1); 
  ggHTx2[3]=new TH2F("ggHTx2D","x2 vs Jet pT (gg HT CASE D)",50,0,50,100,0,1);

  costh[0]=new TH2F("costhA","costh vs Jet pT (CASE A)",50,0,50,100,0,1); 
  costh[1]=new TH2F("costhB","costh vs Jet pT (CASE B)",50,0,50,100,0,1); 
  costh[2]=new TH2F("costhC","costh vs Jet pT (CASE C)",50,0,50,100,0,1); 
  costh[3]=new TH2F("costhD","costh vs Jet pT (CASE D)",50,0,50,100,0,1);
  HTcosth[0]=new TH2F("HTcosthA","costh vs Jet pT (HT CASE A)",50,0,50,100,0,1); 
  HTcosth[1]=new TH2F("HTcosthB","costh vs Jet pT (HT CASE B)",50,0,50,100,0,1); 
  HTcosth[2]=new TH2F("HTcosthC","costh vs Jet pT (HT CASE C)",50,0,50,100,0,1); 
  HTcosth[3]=new TH2F("HTcosthD","costh vs Jet pT (HT CASE D)",50,0,50,100,0,1);
  qqcosth[0]=new TH2F("qqcosthA","costh vs Jet pT (qq CASE A)",50,0,50,100,0,1); 
  qqcosth[1]=new TH2F("qqcosthB","costh vs Jet pT (qq CASE B)",50,0,50,100,0,1); 
  qqcosth[2]=new TH2F("qqcosthC","costh vs Jet pT (qq CASE C)",50,0,50,100,0,1); 
  qqcosth[3]=new TH2F("qqcosthD","costh vs Jet pT (qq CASE D)",50,0,50,100,0,1);
  qqHTcosth[0]=new TH2F("qqHTcosthA","costh vs Jet pT (qq HT CASE A)",50,0,50,100,0,1); 
  qqHTcosth[1]=new TH2F("qqHTcosthB","costh vs Jet pT (qq HT CASE B)",50,0,50,100,0,1); 
  qqHTcosth[2]=new TH2F("qqHTcosthC","costh vs Jet pT (qq HT CASE C)",50,0,50,100,0,1); 
  qqHTcosth[3]=new TH2F("qqHTcosthD","costh vs Jet pT (qq HT CASE D)",50,0,50,100,0,1);
  qgcosth[0]=new TH2F("qgcosthA","costh vs Jet pT (qg CASE A)",50,0,50,100,0,1); 
  qgcosth[1]=new TH2F("qgcosthB","costh vs Jet pT (qgCASE B)",50,0,50,100,0,1); 
  qgcosth[2]=new TH2F("qgcosthC","costh vs Jet pT (qg CASE C)",50,0,50,100,0,1); 
  qgcosth[3]=new TH2F("qgcosthD","costh vs Jet pT (qg CASE D)",50,0,50,100,0,1);
  qgHTcosth[0]=new TH2F("qgHTcosthA","costh vs Jet pT (qg HT CASE A)",50,0,50,100,0,1); 
  qgHTcosth[1]=new TH2F("qgHTcosthB","costh vs Jet pT (qg HT CASE B)",50,0,50,100,0,1); 
  qgHTcosth[2]=new TH2F("qgHTcosthC","costh vs Jet pT (qg HT CASE C)",50,0,50,100,0,1);
  qgHTcosth[3]=new TH2F("qgHTcosthD","costh vs Jet pT (qg HT CASE D)",50,0,50,100,0,1); 
  ggcosth[0]=new TH2F("ggcosthA","costh vs Jet pT (gg CASE A)",50,0,50,100,0,1); 
  ggcosth[1]=new TH2F("ggcosthB","costh vs Jet pT (gg CASE B)",50,0,50,100,0,1); 
  ggcosth[2]=new TH2F("ggcsthC","costh vs Jet pT (gg CASE C)",50,0,50,100,0,1); 
  ggcosth[3]=new TH2F("ggcosthD","costh vs Jet pT (gg CASE D)",50,0,50,100,0,1);
  ggHTcosth[0]=new TH2F("ggHTcosthA","costh vs Jet pT (gg HT CASE A)",50,0,50,100,0,1); 
  ggHTcosth[1]=new TH2F("ggHTcosthB","costh vs Jet pT (gg HT CASE B)",50,0,50,100,0,1); 
  ggHTcosth[2]=new TH2F("ggHTcosthC","costh vs Jet pT (gg HT CASE C)",50,0,50,100,0,1); 
  ggHTcosth[3]=new TH2F("ggHTcosthD","costh vs Jet pT (gg HT CASE D)",50,0,50,100,0,1);

  for (int i=0;i<4;i++){
    HList->Add(hx1[i]);
    HList->Add(HTx1[i]);
    HList->Add(qqx1[i]);
    HList->Add(qqHTx1[i]);
    HList->Add(qgx1[i]);
    HList->Add(qgHTx1[i]);
    HList->Add(ggx1[i]);
    HList->Add(ggHTx1[i]);
    HList->Add(hx2[i]);
    HList->Add(HTx2[i]);
    HList->Add(qqx2[i]);
    HList->Add(qqHTx2[i]);
    HList->Add(qgx2[i]);
    HList->Add(qgHTx2[i]);
    HList->Add(ggx2[i]);
    HList->Add(ggHTx2[i]);
    HList->Add(costh[i]);
    HList->Add(HTcosth[i]);
    HList->Add(qqcosth[i]);
    HList->Add(qqHTcosth[i]);
    HList->Add(qgcosth[i]);
    HList->Add(qgHTcosth[i]);
    HList->Add(ggcosth[i]);
    HList->Add(ggHTcosth[i]);
  }

  //Unfolding
  AssocA[0]=new TH1F("dRA","Geant-Pythia dR (Case A)",1000,0,10);
  AssocA[1]=new TH1F("ratio_pTA","(Pythia-Geant)/Pythia pT (Case A) ",1000,-5,5);
  Assoc2A[0]=new TH2F("scalepTA","(Pythia-Geant)/Pythia pT vs Geant pT (Case A)",50,0,50,1000,-5.0,5.0);
  Assoc2A[1]=new TH2F("dRpTA","Geant-Pythia dR vs (Pythia-Geant)/Pythia pT (Case A)",100,0,10,1000,-5,5);
  AssocB[0]=new TH1F("dRB","Geant-Pythia dR (Case B)",1000,0,10);
  AssocB[1]=new TH1F("ratio_pTB","(Pythia-Geant)/Pythia pT (Case B) ",1000,-5,5);
  Assoc2B[0]=new TH2F("scalepTB","(Pythia-Geant)/Pythia pT vs Geant pT (Case B)",50,0,50,1000,-5.0,5.0);
  Assoc2B[1]=new TH2F("dRpTB","Geant-Pythia dR vs (Pythia-Geant)/Pythia pT (Case B)",100,0,10,1000,-5,5);
  AssocC[0]=new TH1F("dRC","Geant-Pythia dR (Case C)",1000,0,10);
  AssocC[1]=new TH1F("ratio_pTC","(Pythia-Geant)/Pythia pT (Case C) ",1000,-5,5);
  Assoc2C[0]=new TH2F("scalepTC","(Pythia-Geant)/Pythia pT vs Geant pT (Case C)",50,0,50,1000,-5.0,5.0);
  Assoc2C[1]=new TH2F("dRpTC","Geant-Pythia dR vs (Pythia-Geant)/Pythia pT (Case C)",100,0,10,1000,-5,5);
  AssocD[0]=new TH1F("dRD","Geant-Pythia dR (Case D)",1000,0,10);
  AssocD[1]=new TH1F("ratio_pTD","(Pythia-Geant)/Pythia pT (Case D) ",1000,-5,5);
  Assoc2D[0]=new TH2F("scalepTD","(Pythia-Geant)/Pythia pT vs Geant pT (Case D)",50,0,50,1000,-5.0,5.0);
  Assoc2D[1]=new TH2F("dRpTD","Geant-Pythia dR vs (Pythia-Geant)/Pythia pT (Case D)",100,0,10,1000,-5,5);
 
  for (int i=0;i<2;i++){
    HList->Add(AssocA[i]);
    HList->Add(Assoc2A[i]);
    HList->Add(AssocB[i]);
    HList->Add(Assoc2B[i]);
    HList->Add(AssocC[i]);
    HList->Add(Assoc2C[i]);
    HList->Add(AssocD[i]);
    HList->Add(Assoc2D[i]);
  }
}

void StJetSimuReader::AsymBiasHisto(){

  float nbins0[nbins+1]={0.0,5.0,6.15,7.5645,9.30434,11.4443,14.0765,17.3141,21.2964,26.1945,32.2193,39.6297,48.7446,59.9558};

  //Trigger Bias	       
  HTfreq=new TH1F("HTfreq","Frequency of HT trig vs Eta",20,0,20);
  Count[0]=new TH1F("HTtrig","HTtrig",2,-0.5,1.5);
  Count[1]=new TH1F("BBCtrig","BBCtrig",2,0,2);
  Count[2]=new TH1F("Zver","Zver",400,-200,200);
  Count[3]=new TH1F("ZverBBC","ZverBBC",400,-200,200);
  Count[4]=new TH1F("JetBBCtrig","JetBBCtrig",2,0,2);
  Count[5]=new TH1F("JetZver","JetZver",400,-200,200);
  Count[6]=new TH1F("JetZverBBC","JetZverBBC",400,-200,200);
  Count[7]=new TH1F("allBBC","allBBC",2,0,2);
  Count[8]=new TH1F("JPtrig","JPtrig",2,-0.5,1.5);
  Count[9]=new TH1F("AHTtrig","AHTtrig",2,-0.5,1.5);
  Count[10]=new TH1F("HTid","HTid",4800,0,4800);
  Count[11]=new TH1F("JPid","JPid",6,0,6);
  Count[12]=new TH1F("AHTid","AHTid",4800,0,4800);
  Count[13]=new TH1F("HTdsm","HTdsm",100,0,100);
  Count[14]=new TH1F("JPdsm","JPdsm",200,0,200);
  Count[15]=new TH1F("AHTdsm","AHTdsm",100,0,100);
  Count[16]=new TH1F("ZverBBCHT","ZverBBCHT",400,-200,200);
  Count[17]=new TH1F("JetZverBBCHT","JetZverBBCHT",400,-200,200);
  
  HList->Add(HTfreq);  
  for (int i=0;i<18;i++){
    HList->Add(Count[i]);
  }
  
  partpT=new TH1F("partpT","Jet pT",nbins,nbins0);
  HTpartpT=new TH1F("HTpartpT","Jet pT (HT)",nbins,nbins0);
  JPpartpT=new TH1F("JPpartpT","Jet pT (JP)",nbins,nbins0);
  HTverNeu=new TH2F("HTverNeu","Jet Neutral vs vertex",50,0,1,20,-100,100);
  dR=new TH1F("dR","dR of Associated Jet",10000,0,100);
  PYIN=new TH1F("PYindex","PYindex",30,-10,20);
  HList->Add(HTverNeu);
  HList->Add(partpT);
  HList->Add(HTpartpT);
  HList->Add(JPpartpT);
  HList->Add(dR);
  HList->Add(PYIN);

  //x1&&x2
  x1H[0]=new TH1F("PYx1","PYx1",100,0,1);
  x1H[1]=new TH1F("PYx1qq","PYx1qq",100,0,1);
  x1H[2]=new TH1F("PYx1qg","PYx1qg",100,0,1);
  x1H[3]=new TH1F("PYx1gg","PYx1gg",100,0,1);
  x1H[4]=new TH1F("RAWx1","RAWx1",100,0,1);
  x1H[5]=new TH1F("RAWx1qq","RAWx1qq",100,0,1);
  x1H[6]=new TH1F("RAWx1qg","RAWx1qg",100,0,1);
  x1H[7]=new TH1F("RAWx1gg","RAWx1gg",100,0,1);
  x1H[8]=new TH1F("BBCx1","BBCx1",100,0,1);
  x1H[9]=new TH1F("BBCx1qq","BBCx1qq",100,0,1);
  x1H[10]=new TH1F("BBCx1qg","BBCx1qg",100,0,1);
  x1H[11]=new TH1F("BBCx1gg","BBCx1gg",100,0,1);
  x1H[12]=new TH1F("HTx1","HTx1",100,0,1);
  x1H[13]=new TH1F("HTx1qq","HTx1qq",100,0,1);
  x1H[14]=new TH1F("HTx1qg","HTx1qg",100,0,1);
  x1H[15]=new TH1F("HTx1gg","HTx1gg",100,0,1);

  x2H[0]=new TH1F("PYx2","PYx2",100,0,1);
  x2H[1]=new TH1F("PYx2qq","PYx2qq",100,0,1);
  x2H[2]=new TH1F("PYx2qg","PYx2qg",100,0,1);
  x2H[3]=new TH1F("PYx2gg","PYx2gg",100,0,1);
  x2H[4]=new TH1F("RAWx2","RAWx2",100,0,1);
  x2H[5]=new TH1F("RAWx2qq","RAWx2qq",100,0,1);
  x2H[6]=new TH1F("RAWx2qg","RAWx2qg",100,0,1);
  x2H[7]=new TH1F("RAWx2gg","RAWx2gg",100,0,1);
  x2H[8]=new TH1F("BBCx2","BBCx2",100,0,1);
  x2H[9]=new TH1F("BBCx2qq","BBCx2qq",100,0,1);
  x2H[10]=new TH1F("BBCx2qg","BBCx2qg",100,0,1);
  x2H[11]=new TH1F("BBCx2gg","BBCx2gg",100,0,1);
  x2H[12]=new TH1F("HTx2","HTx2",100,0,1);
  x2H[13]=new TH1F("HTx2qq","HTx2qq",100,0,1);
  x2H[14]=new TH1F("HTx2qg","HTx2qg",100,0,1);
  x2H[15]=new TH1F("HTx2gg","HTx2gg",100,0,1);
  
  //Q2
  Q2H[0]=new TH1F("PYQ2","PYQ2",2000,0,2000);
  Q2H[1]=new TH1F("PYQ2qq","PYQ2qq",2000,0,2000);
  Q2H[2]=new TH1F("PYQ2qg","PYQ2qg",2000,0,2000);
  Q2H[3]=new TH1F("PYQ2gg","PYQ2gg",2000,0,2000);
  Q2H[4]=new TH1F("RAWQ2","RAWQ2",2000,0,2000);
  Q2H[5]=new TH1F("RAWQ2qq","RAWQ2qq",2000,0,2000);
  Q2H[6]=new TH1F("RAWQ2qg","RAWQ2qg",2000,0,2000);
  Q2H[7]=new TH1F("RAWQ2gg","RAWQ2gg",2000,0,2000);
  Q2H[8]=new TH1F("BBCQ2","BBCQ2",2000,0,2000);
  Q2H[9]=new TH1F("BBCQ2qq","BBCQ2qq",2000,0,2000);
  Q2H[10]=new TH1F("BBCQ2qg","BBCQ2qg",2000,0,2000);
  Q2H[11]=new TH1F("BBCQ2gg","BBCQ2gg",2000,0,2000);
  Q2H[12]=new TH1F("HTQ2","HTQ2",2000,0,2000);
  Q2H[13]=new TH1F("HTQ2qq","HTQ2qq",2000,0,2000);
  Q2H[14]=new TH1F("HTQ2qg","HTQ2qg",2000,0,2000);
  Q2H[15]=new TH1F("HTQ2gg","HTQ2gg",2000,0,2000);

  //aLL
  aLLH[0]=new TH1F("PYaLL","PYaLL",200,-1,1);
  aLLH[1]=new TH1F("PYaLLqq","PYaLLqq",200,-1,1);
  aLLH[2]=new TH1F("PYaLLqg","PYaLLqg",200,-1,1);
  aLLH[3]=new TH1F("PYaLLgg","PYaLLgg",200,-1,1);
  aLLH[4]=new TH1F("RAWaLL","RAWaLL",200,-1,1);
  aLLH[5]=new TH1F("RAWaLLqq","RAWaLLqq",200,-1,1);
  aLLH[6]=new TH1F("RAWaLLqg","RAWaLLqg",200,-1,1);
  aLLH[7]=new TH1F("RAWaLLgg","RAWaLLgg",200,-1,1);
  aLLH[8]=new TH1F("BBCaLL","BBCaLL",200,-1,1);
  aLLH[9]=new TH1F("BBCaLLqq","BBCaLLqq",200,-1,1);
  aLLH[10]=new TH1F("BBCaLLqg","BBCaLLqg",200,-1,1);
  aLLH[11]=new TH1F("BBCaLLgg","BBCaLLgg",200,-1,1);
  aLLH[12]=new TH1F("HTaLL","HTaLL",200,-1,1);
  aLLH[13]=new TH1F("HTaLLqq","HTaLLqq",200,-1,1);
  aLLH[14]=new TH1F("HTaLLqg","HTaLLqg",200,-1,1);
  aLLH[15]=new TH1F("HTaLLgg","HTaLLgg",200,-1,1);

  //df1/df2
  df1H[0]=new TH1F("PYdf1","PYdf1",200,-1,1);
  df1H[1]=new TH1F("PYdf1qq","PYdf1qq",200,-1,1);
  df1H[2]=new TH1F("PYdf1qg","PYdf1qg",200,-1,1);
  df1H[3]=new TH1F("PYdf1gg","PYdf1gg",200,-1,1);
  df1H[4]=new TH1F("RAWdf1","RAWdf1",200,-1,1);
  df1H[5]=new TH1F("RAWdf1qq","RAWdf1qq",200,-1,1);
  df1H[6]=new TH1F("RAWdf1qg","RAWdf1qg",200,-1,1);
  df1H[7]=new TH1F("RAWdf1gg","RAWdf1gg",200,-1,1);
  df1H[8]=new TH1F("BBCdf1","BBCdf1",200,-1,1);
  df1H[9]=new TH1F("BBCdf1qq","BBCdf1qq",200,-1,1);
  df1H[10]=new TH1F("BBCdf1qg","BBCdf1qg",200,-1,1);
  df1H[11]=new TH1F("BBCdf1gg","BBCdf1gg",200,-1,1);
  df1H[12]=new TH1F("HTdf1","HTdf1",200,-1,1);
  df1H[13]=new TH1F("HTdf1qq","HTdf1qq",200,-1,1);
  df1H[14]=new TH1F("HTdf1qg","HTdf1qg",200,-1,1);
  df1H[15]=new TH1F("HTdf1gg","HTdf1gg",200,-1,1);

  df2H[0]=new TH1F("PYdf2","PYdf2",200,-1,1);
  df2H[1]=new TH1F("PYdf2qq","PYdf2qq",200,-1,1);
  df2H[2]=new TH1F("PYdf2qg","PYdf2qg",200,-1,1);
  df2H[3]=new TH1F("PYdf2gg","PYdf2gg",200,-1,1);
  df2H[4]=new TH1F("RAWdf2","RAWdf2",200,-1,1);
  df2H[5]=new TH1F("RAWdf2qq","RAWdf2qq",200,-1,1);
  df2H[6]=new TH1F("RAWdf2qg","RAWdf2qg",200,-1,1);
  df2H[7]=new TH1F("RAWdf2gg","RAWdf2gg",200,-1,1);
  df2H[8]=new TH1F("BBCdf2","BBCdf2",200,-1,1);
  df2H[9]=new TH1F("BBCdf2qq","BBCdf2qq",200,-1,1);
  df2H[10]=new TH1F("BBCdf2qg","BBCdf2qg",200,-1,1);
  df2H[11]=new TH1F("BBCdf2gg","BBCdf2gg",200,-1,1);
  df2H[12]=new TH1F("HTdf2","HTdf2",200,-1,1);
  df2H[13]=new TH1F("HTdf2qq","HTdf2qq",200,-1,1);
  df2H[14]=new TH1F("HTdf2qg","HTdf2qg",200,-1,1);
  df2H[15]=new TH1F("HTdf2gg","HTdf2gg",200,-1,1);

  //f1/f2
  f1H[0]=new TH1F("PYf1","PYf1",100,0,1);
  f1H[1]=new TH1F("PYf1qq","PYf1qq",100,0,1);
  f1H[2]=new TH1F("PYf1qg","PYf1qg",100,0,1);
  f1H[3]=new TH1F("PYf1gg","PYf1gg",100,0,1);
  f1H[4]=new TH1F("RAWf1","RAWf1",100,0,1);
  f1H[5]=new TH1F("RAWf1qq","RAWf1qq",100,0,1);
  f1H[6]=new TH1F("RAWf1qg","RAWf1qg",100,0,1);
  f1H[7]=new TH1F("RAWf1gg","RAWf1gg",100,0,1);
  f1H[8]=new TH1F("BBCf1","BBCf1",100,0,1);
  f1H[9]=new TH1F("BBCf1qq","BBCf1qq",100,0,1);
  f1H[10]=new TH1F("BBCf1qg","BBCf1qg",100,0,1);
  f1H[11]=new TH1F("BBCf1gg","BBCf1gg",100,0,1);
  f1H[12]=new TH1F("HTf1","HTf1",100,0,1);
  f1H[13]=new TH1F("HTf1qq","HTf1qq",100,0,1);
  f1H[14]=new TH1F("HTf1qg","HTf1qg",100,0,1);
  f1H[15]=new TH1F("HTf1gg","HTf1gg",100,0,1);

  f2H[0]=new TH1F("PYf2","PYf2",100,0,1);
  f2H[1]=new TH1F("PYf2qq","PYf2qq",100,0,1);
  f2H[2]=new TH1F("PYf2qg","PYf2qg",100,0,1);
  f2H[3]=new TH1F("PYf2gg","PYf2gg",100,0,1);
  f2H[4]=new TH1F("RAWf2","RAWf2",100,0,1);
  f2H[5]=new TH1F("RAWf2qq","RAWf2qq",100,0,1);
  f2H[6]=new TH1F("RAWf2qg","RAWf2qg",100,0,1);
  f2H[7]=new TH1F("RAWf2gg","RAWf2gg",100,0,1);
  f2H[8]=new TH1F("BBCf2","BBCf2",100,0,1);
  f2H[9]=new TH1F("BBCf2qq","BBCf2qq",100,0,1);
  f2H[10]=new TH1F("BBCf2qg","BBCf2qg",100,0,1);
  f2H[11]=new TH1F("BBCf2gg","BBCf2gg",100,0,1);
  f2H[12]=new TH1F("HTf2","HTf2",100,0,1);
  f2H[13]=new TH1F("HTf2qq","HTf2qq",100,0,1);
  f2H[14]=new TH1F("HTf2qg","HTf2qg",100,0,1);
  f2H[15]=new TH1F("HTf2gg","HTf2gg",100,0,1);

  for (int i=0;i<16;i++){
    HList->Add(x1H[i]);
    HList->Add(x2H[i]);
    HList->Add(f1H[i]);
    HList->Add(f2H[i]);
    HList->Add(df1H[i]);
    HList->Add(df2H[i]);
    HList->Add(Q2H[i]);
    HList->Add(aLLH[i]);
  }

  //PYTHIA - no cuts -> use PYjpTA
  //PYTHIA - detector eta cuts
  PYETAjpTA[0]=new TH1F("PYETApT","Jet pT  ",nbins,nbins0);
  PYETAjpTA[1]=new TH1F("PYETAeta","Jet Eta  ",30,-3,3);
  PYETAjpTA[2]=new TH1F("PYETAphi","Jet Phi  ",60,-180,180);
  PYETAjpTA[3]=new TH1F("PYETAneu","Jet Neutral  ",100,0,1);
  PYETAjpTB[0]=new TH1F("PYETAwpT","Jet pT weight",nbins,nbins0);
  PYETAjpTB[1]=new TH1F("PYETAweta","Jet Eta weight",30,-3,3);
  PYETAjpTB[2]=new TH1F("PYETAwphi","Jet Phi weight",60,-180,180);
  PYETAjpTB[3]=new TH1F("PYETAwneu","Jet Neutral weight",100,0,1);
  PYETAjpTC[0]=new TH1F("PYETAw2pT","Jet pT weight2",nbins,nbins0);
  PYETAjpTC[1]=new TH1F("PYETAw2eta","Jet Eta weight2",30,-3,3);
  PYETAjpTC[2]=new TH1F("PYETAw2phi","Jet Phi weight2",60,-180,180);
  PYETAjpTC[3]=new TH1F("PYETAw2neu","Jet Neutral weight2",100,0,1);
  for (int i=0;i<4;i++){
    HList->Add(PYETAjpTA[i]);
    HList->Add(PYETAjpTB[i]);
    HList->Add(PYETAjpTC[i]);
  }  
  //PYTHIA - associated with detector jet with all detector cuts
  PYASCjpTA[0]=new TH1F("PYASCpT","Jet pT  ",nbins,nbins0);
  PYASCjpTA[1]=new TH1F("PYASCeta","Jet Eta  ",30,-3,3);
  PYASCjpTA[2]=new TH1F("PYASCphi","Jet Phi  ",60,-180,180);
  PYASCjpTA[3]=new TH1F("PYASCneu","Jet Neutral  ",100,0,1);
  PYASCjpTB[0]=new TH1F("PYASCwpT","Jet pT weight",nbins,nbins0);
  PYASCjpTB[1]=new TH1F("PYASCweta","Jet Eta weight",30,-3,3);
  PYASCjpTB[2]=new TH1F("PYASCwphi","Jet Phi weight",60,-180,180);
  PYASCjpTB[3]=new TH1F("PYASCwneu","Jet Neutral weight",100,0,1);
  PYASCjpTC[0]=new TH1F("PYASCw2pT","Jet pT weight2",nbins,nbins0);
  PYASCjpTC[1]=new TH1F("PYASCw2eta","Jet Eta weight2",30,-3,3);
  PYASCjpTC[2]=new TH1F("PYASCw2phi","Jet Phi weight2",60,-180,180);
  PYASCjpTC[3]=new TH1F("PYASCw2neu","Jet Neutral weight2",100,0,1);
  for (int i=0;i<4;i++){
    HList->Add(PYASCjpTA[i]);
    HList->Add(PYASCjpTB[i]);
    HList->Add(PYASCjpTC[i]);
  }  

  //PYTHIA - associated with detector jet with all detector cuts and diagonal
  PYDIAjpTA[0]=new TH1F("PYDIApT","Jet pT  ",nbins,nbins0);
  PYDIAjpTA[1]=new TH1F("PYDIAeta","Jet Eta  ",30,-3,3);
  PYDIAjpTA[2]=new TH1F("PYDIAphi","Jet Phi  ",60,-180,180);
  PYDIAjpTA[3]=new TH1F("PYDIAneu","Jet Neutral  ",100,0,1);
  PYDIAjpTB[0]=new TH1F("PYDIAwpT","Jet pT weight",nbins,nbins0);
  PYDIAjpTB[1]=new TH1F("PYDIAweta","Jet Eta weight",30,-3,3);
  PYDIAjpTB[2]=new TH1F("PYDIAwphi","Jet Phi weight",60,-180,180);
  PYDIAjpTB[3]=new TH1F("PYDIAwneu","Jet Neutral weight",100,0,1);
  PYDIAjpTC[0]=new TH1F("PYDIAw2pT","Jet pT weight2",nbins,nbins0);
  PYDIAjpTC[1]=new TH1F("PYDIAw2eta","Jet Eta weight2",30,-3,3);
  PYDIAjpTC[2]=new TH1F("PYDIAw2phi","Jet Phi weight2",60,-180,180);
  PYDIAjpTC[3]=new TH1F("PYDIAw2neu","Jet Neutral weight2",100,0,1);
  DIAjpTA[0]=new TH1F("DIApT","Jet pT  ",nbins,nbins0);
  DIAjpTA[1]=new TH1F("DIAeta","Jet Eta  ",30,-3,3);
  DIAjpTA[2]=new TH1F("DIAphi","Jet Phi  ",60,-180,180);
  DIAjpTA[3]=new TH1F("DIAneu","Jet Neutral  ",100,0,1);
  DIAjpTB[0]=new TH1F("DIAwpT","Jet pT weight",nbins,nbins0);
  DIAjpTB[1]=new TH1F("DIAweta","Jet Eta weight",30,-3,3);
  DIAjpTB[2]=new TH1F("DIAwphi","Jet Phi weight",60,-180,180);
  DIAjpTB[3]=new TH1F("DIAwneu","Jet Neutral weight",100,0,1);
  DIAjpTC[0]=new TH1F("DIAw2pT","Jet pT weight2",nbins,nbins0);
  DIAjpTC[1]=new TH1F("DIAw2eta","Jet Eta weight2",30,-3,3);
  DIAjpTC[2]=new TH1F("DIAw2phi","Jet Phi weight2",60,-180,180);
  DIAjpTC[3]=new TH1F("DIAw2neu","Jet Neutral weight2",100,0,1);
  UPDIAjpTA[0]=new TH1F("UPDIApT","Jet pT  ",nbins,nbins0);
  UPDIAjpTA[1]=new TH1F("UPDIAeta","Jet Eta  ",30,-3,3);
  UPDIAjpTA[2]=new TH1F("UPDIAphi","Jet Phi  ",60,-180,180);
  UPDIAjpTA[3]=new TH1F("UPDIAneu","Jet Neutral  ",100,0,1);
  UPDIAjpTB[0]=new TH1F("UPDIAwpT","Jet pT weight",nbins,nbins0);
  UPDIAjpTB[1]=new TH1F("UPDIAweta","Jet Eta weight",30,-3,3);
  UPDIAjpTB[2]=new TH1F("UPDIAwphi","Jet Phi weight",60,-180,180);
  UPDIAjpTB[3]=new TH1F("UPDIAwneu","Jet Neutral weight",100,0,1);
  UPDIAjpTC[0]=new TH1F("UPDIAw2pT","Jet pT weight2",nbins,nbins0);
  UPDIAjpTC[1]=new TH1F("UPDIAw2eta","Jet Eta weight2",30,-3,3);
  UPDIAjpTC[2]=new TH1F("UPDIAw2phi","Jet Phi weight2",60,-180,180);
  UPDIAjpTC[3]=new TH1F("UPDIAw2neu","Jet Neutral weight2",100,0,1);
  DWDIAjpTA[0]=new TH1F("DWDIApT","Jet pT  ",nbins,nbins0);
  DWDIAjpTA[1]=new TH1F("DWDIAeta","Jet Eta  ",30,-3,3);
  DWDIAjpTA[2]=new TH1F("DWDIAphi","Jet Phi  ",60,-180,180);
  DWDIAjpTA[3]=new TH1F("DWDIAneu","Jet Neutral  ",100,0,1);
  DWDIAjpTB[0]=new TH1F("DWDIAwpT","Jet pT weight",nbins,nbins0);
  DWDIAjpTB[1]=new TH1F("DWDIAweta","Jet Eta weight",30,-3,3);
  DWDIAjpTB[2]=new TH1F("DWDIAwphi","Jet Phi weight",60,-180,180);
  DWDIAjpTB[3]=new TH1F("DWDIAwneu","Jet Neutral weight",100,0,1);
  DWDIAjpTC[0]=new TH1F("DWDIAw2pT","Jet pT weight2",nbins,nbins0);
  DWDIAjpTC[1]=new TH1F("DWDIAw2eta","Jet Eta weight2",30,-3,3);
  DWDIAjpTC[2]=new TH1F("DWDIAw2phi","Jet Phi weight2",60,-180,180);
  DWDIAjpTC[3]=new TH1F("DWDIAw2neu","Jet Neutral weight2",100,0,1);

  for (int i=0;i<4;i++){
    HList->Add(PYDIAjpTA[i]);
    HList->Add(PYDIAjpTB[i]);
    HList->Add(PYDIAjpTC[i]);
    HList->Add(DIAjpTA[i]);
    HList->Add(DIAjpTB[i]);
    HList->Add(DIAjpTC[i]);
    HList->Add(UPDIAjpTA[i]);
    HList->Add(UPDIAjpTB[i]);
    HList->Add(UPDIAjpTC[i]);
    HList->Add(DWDIAjpTA[i]);
    HList->Add(DWDIAjpTB[i]);
    HList->Add(DWDIAjpTC[i]);
  }  

  //PYTHIA - associated with detector jet with all detector cuts + BBC
  PYBBCjpTA[0]=new TH1F("PYBBCpT","Jet pT  ",nbins,nbins0);
  PYBBCjpTA[1]=new TH1F("PYBBCeta","Jet Eta  ",30,-3,3);
  PYBBCjpTA[2]=new TH1F("PYBBCphi","Jet Phi  ",60,-180,180);
  PYBBCjpTA[3]=new TH1F("PYBBCneu","Jet Neutral  ",100,0,1);
  PYBBCjpTB[0]=new TH1F("PYBBCwpT","Jet pT weight",nbins,nbins0);
  PYBBCjpTB[1]=new TH1F("PYBBCweta","Jet Eta weight",30,-3,3);
  PYBBCjpTB[2]=new TH1F("PYBBCwphi","Jet Phi weight",60,-180,180);
  PYBBCjpTB[3]=new TH1F("PYBBCwneu","Jet Neutral weight",100,0,1);
  PYBBCjpTC[0]=new TH1F("PYBBCw2pT","Jet pT weight2",nbins,nbins0);
  PYBBCjpTC[1]=new TH1F("PYBBCw2eta","Jet Eta weight2",30,-3,3);
  PYBBCjpTC[2]=new TH1F("PYBBCw2phi","Jet Phi weight2",60,-180,180);
  PYBBCjpTC[3]=new TH1F("PYBBCw2neu","Jet Neutral weight2",100,0,1);
  for (int i=0;i<4;i++){
    HList->Add(PYBBCjpTA[i]);
    HList->Add(PYBBCjpTB[i]);
    HList->Add(PYBBCjpTC[i]);
  }  
  //PYHTIA - associated with detector jet with all detcctor cuts + BBC + HT

  PYHTjpTA[0]=new TH1F("PYHTpT","Jet pT  ",nbins,nbins0);
  PYHTjpTA[1]=new TH1F("PYHTeta","Jet Eta  ",30,-3,3);
  PYHTjpTA[2]=new TH1F("PYHTphi","Jet Phi  ",60,-180,180);
  PYHTjpTA[3]=new TH1F("PYHTneu","Jet Neutral  ",100,0,1);
  PYHTjpTB[0]=new TH1F("PYHTwpT","Jet pT weight",nbins,nbins0);
  PYHTjpTB[1]=new TH1F("PYHTweta","Jet Eta weight",30,-3,3);
  PYHTjpTB[2]=new TH1F("PYHTwphi","Jet Phi weight",60,-180,180);
  PYHTjpTB[3]=new TH1F("PYHTwneu","Jet Neutral weight",100,0,1);
  PYHTjpTC[0]=new TH1F("PYHTw2pT","Jet pT weight2",nbins,nbins0);
  PYHTjpTC[1]=new TH1F("PYHTw2eta","Jet Eta weight2",30,-3,3);
  PYHTjpTC[2]=new TH1F("PYHTw2phi","Jet Phi weight2",60,-180,180);
  PYHTjpTC[3]=new TH1F("PYHTw2neu","Jet Neutral weight2",100,0,1);
  for (int i=0;i<4;i++){
    HList->Add(PYHTjpTA[i]);
    HList->Add(PYHTjpTB[i]);
    HList->Add(PYHTjpTC[i]);
  }  

  //PYTHIA no detector cuts/BBC/HT trig
  PYjpTA[0]=new TH1F("PYpT","Jet pT  ",nbins,nbins0);
  PYjpTA[1]=new TH1F("PYeta","Jet Eta  ",30,-3,3);
  PYjpTA[2]=new TH1F("PYphi","Jet Phi  ",60,-180,180);
  PYjpTA[3]=new TH1F("PYneu","Jet Neutral  ",100,0,1);
  PYjpTB[0]=new TH1F("PYwpT","Jet pT weight",nbins,nbins0);
  PYjpTB[1]=new TH1F("PYweta","Jet Eta weight",30,-3,3);
  PYjpTB[2]=new TH1F("PYwphi","Jet Phi weight",60,-180,180);
  PYjpTB[3]=new TH1F("PYwneu","Jet Neutral weight",100,0,1);
  PYjpTC[0]=new TH1F("PYw2pT","Jet pT weight2",nbins,nbins0);
  PYjpTC[1]=new TH1F("PYw2eta","Jet Eta weight2",30,-3,3);
  PYjpTC[2]=new TH1F("PYw2phi","Jet Phi weight2",60,-180,180);
  PYjpTC[3]=new TH1F("PYw2neu","Jet Neutral weight2",100,0,1);
  for (int i=0;i<4;i++){
    HList->Add(PYjpTA[i]);
    HList->Add(PYjpTB[i]);
    HList->Add(PYjpTC[i]);
  }


  //RAW no BBC no HT/JP trig
  RAWjpTA[0]=new TH1F("RAWpT","Jet pT  ",nbins,nbins0);
  RAWjpTA[1]=new TH1F("RAWeta","Jet Eta  ",30,-3,3);
  RAWjpTA[2]=new TH1F("RAWphi","Jet Phi  ",60,-180,180);
  RAWjpTA[3]=new TH1F("RAWneu","Jet Neutral  ",100,0,1);
  RAWjpTB[0]=new TH1F("RAWwpT","Jet pT weight",nbins,nbins0);
  RAWjpTB[1]=new TH1F("RAWweta","Jet Eta weight",30,-3,3);
  RAWjpTB[2]=new TH1F("RAWwphi","Jet Phi weight",60,-180,180);
  RAWjpTB[3]=new TH1F("RAWwneu","Jet Neutral weight",100,0,1);
  RAWjpTC[0]=new TH1F("RAWw2pT","Jet pT weight2",nbins,nbins0);
  RAWjpTC[1]=new TH1F("RAWw2eta","Jet Eta weight2",30,-3,3);
  RAWjpTC[2]=new TH1F("RAWw2phi","Jet Phi weight2",60,-180,180);
  RAWjpTC[3]=new TH1F("RAWw2neu","Jet Neutral weight2",100,0,1);

  for (int i=0;i<4;i++){
    HList->Add(RAWjpTA[i]);
    HList->Add(RAWjpTB[i]);
    HList->Add(RAWjpTC[i]);
  }


  //BBC trigger
  jpTA[0]=new TH1F("pT","Jet pT",nbins,nbins0);
  jpTA[1]=new TH1F("eta","Jet Eta",30,-3,3);
  jpTA[2]=new TH1F("phi","Jet Phi",60,-180,180);
  jpTA[3]=new TH1F("neu","Jet Neutral",100,0,1);
  jpTB[0]=new TH1F("wpT","Jet pT weight",nbins,nbins0);
  jpTB[1]=new TH1F("weta","Jet Eta weight",30,-3,3);
  jpTB[2]=new TH1F("wphi","Jet Phi weight",60,-180,180);
  jpTB[3]=new TH1F("wneu","Jet Neutral weight",100,0,1);
  jpTC[0]=new TH1F("w2pT","Jet pT weight2",nbins,nbins0);
  jpTC[1]=new TH1F("w2eta","Jet Eta weight2",30,-3,3);
  jpTC[2]=new TH1F("w2phi","Jet Phi weight2",60,-180,180);
  jpTC[3]=new TH1F("w2neu","Jet Neutral weight2",100,0,1);

  for (int i=0;i<4;i++){
    HList->Add(jpTA[i]);
    HList->Add(jpTB[i]);
    HList->Add(jpTC[i]);
  }


  //HT Trig
  HTjpTA[0]=new TH1F("HTpT","Jet pT  ",nbins,nbins0);
  HTjpTA[1]=new TH1F("HTeta","Jet Eta  ",30,-3,3);
  HTjpTA[2]=new TH1F("HTphi","Jet Phi  ",60,-180,180);
  HTjpTA[3]=new TH1F("HTneu","Jet Neutral  ",100,0,1);
  HTjpTB[0]=new TH1F("HTwpT","Jet pT weight",nbins,nbins0);
  HTjpTB[1]=new TH1F("HTweta","Jet Eta weight",30,-3,3);
  HTjpTB[2]=new TH1F("HTwphi","Jet Phi weight",60,-180,180);
  HTjpTB[3]=new TH1F("HTwneu","Jet Neutral weight",100,0,1);
  HTjpTC[0]=new TH1F("HTw2pT","Jet pT weight2",nbins,nbins0);
  HTjpTC[1]=new TH1F("HTw2eta","Jet Eta weight2",30,-3,3);
  HTjpTC[2]=new TH1F("HTw2phi","Jet Phi weight2",60,-180,180);
  HTjpTC[3]=new TH1F("HTw2neu","Jet Neutral weight2",100,0,1);

  for (int i=0;i<4;i++){
    HList->Add(HTjpTA[i]);
    HList->Add(HTjpTB[i]);
    HList->Add(HTjpTC[i]);
  }
  
  float abins0[abins+1];
  for (int i=0;i<=abins;i++){
    abins0[i]=-1 + i*2/abins;
  }
  jet3D[0]=new TH2F("PYDETW","PYJETS vs DETJETS",nbins,nbins0,nbins,nbins0);
  jet3D[1]=new TH2F("PYBBCW","PYJETS vs BBCJETS",nbins,nbins0,nbins,nbins0);
  jet3D[2]=new TH2F("PYHTW","PYJETS vs HTJETS",nbins,nbins0,nbins,nbins0);
  jet3D[3]=new TH2F("PYDET","PYJETS vs DETJETS",nbins,nbins0,nbins,nbins0);
  jet3D[4]=new TH2F("PYBBC","PYJETS vs BBCJETS",nbins,nbins0,nbins,nbins0);
  jet3D[5]=new TH2F("PYHT","PYJETS vs HTJETS",nbins,nbins0,nbins,nbins0);
  for (int i=0;i<6;i++){
    HList->Add(jet3D[i]);
  }
}

void StJetSimuReader::InitFile(const char* file, const char* simufile)
{
    cout <<"StJetSimuReader::InitFile()"<<endl;

    cout <<"open file:\t"<<file<<"\tfor reading"<<endl;
    mFile = new TFile(file,"READ");
    assert(mFile);
    cout <<"open simufile:\t"<<simufile<<"\tfor reading"<<endl;
    sFile = new TFile(simufile,"READ");
    assert(sFile);

    cout <<"recover jet tree"<<endl;
    TObject* tree = mFile->Get("jet");
    TTree* t = dynamic_cast<TTree*>(tree);
    assert(t);
    mTree = t;

    cout<<"recover simu tree"<<endl;
    stree =(TTree *)sFile->Get("Event");
    assert(stree);

    cout <<"\tset tree pointer"<<endl;
    cout <<"Number of entries in jet tree:\t"<<t->GetEntries();
    cout <<"Number of entries in simu tree:\n"<<stree->GetEntries();
    
    cout <<"\tGet Jet Branches"<<endl;
    TObjArray* branches = t->GetListOfBranches();

    cout <<"\tGet Simu Branches"<<endl;
    stree->SetBranchAddress("evtid",&evtid);
    stree->SetBranchAddress("pid",&pid);
    stree->SetBranchAddress("TowHtEt",TowHtEt);
    stree->SetBranchAddress("bbc",&bbc);
    stree->SetBranchAddress("Badc",Badc);
    stree->SetBranchAddress("s",&ss);
    stree->SetBranchAddress("t",&tt);
    stree->SetBranchAddress("u",&uu);
    stree->SetBranchAddress("hard_p",&hard_p);
    stree->SetBranchAddress("cos_th",&cos_th);
    stree->SetBranchAddress("x1",&x1);
    stree->SetBranchAddress("x2",&x2);
    stree->SetBranchAddress("Alex_ht_Et",&Alex_ht_Et);
    stree->SetBranchAddress("Alex_ht_DSM",&Alex_ht_DSM);
    stree->SetBranchAddress("Alex_ht_id",&Alex_ht_id);
    stree->SetBranchAddress("JP1_2004",&JP1_2004);
    stree->SetBranchAddress("JP1_2004_Patch",&JP1_2004_Patch);
    stree->SetBranchAddress("JP1_2004_DSM",&JP1_2004_DSM);
    stree->SetBranchAddress("HT1_2004",&HT1_2004);
    stree->SetBranchAddress("HT1_2004_Tow",&HT1_2004_Tow);
    stree->SetBranchAddress("HT1_2004_DSM",&HT1_2004_DSM);
    stree->SetBranchAddress("weight",&weight);
    stree->SetBranchAddress("df1",&df1);
    stree->SetBranchAddress("df2",&df2);
    stree->SetBranchAddress("f1",&f1);
    stree->SetBranchAddress("f2",&f2);
    stree->SetBranchAddress("partonic_all",&partonic_all);
    stree->SetBranchAddress("Q2",&Q2);
    stree->SetBranchAddress("flavor1",&flavor1);
    stree->SetBranchAddress("flavor2",&flavor2);
    stree->SetBranchAddress("flavor3",&flavor3);
    stree->SetBranchAddress("flavor4",&flavor4);


    /*stree->SetBranchAddress("Row",Row);
    stree->SetBranchAddress("NeuEt",NeuEt);
    stree->SetBranchAddress("BHTmax",&BHTmax);
    stree->SetBranchAddress("BJPmax",&BJPmax);
    stree->SetBranchAddress("BJPsum",&BJPsum);
    stree->SetBranchAddress("EHTmax",&EHTmax);
    stree->SetBranchAddress("EJPmax",&EJPmax);
    stree->SetBranchAddress("EJPsum",&EJPsum);
    stree->SetBranchAddress("BJP",BJP);
    stree->SetBranchAddress("EJP",EJP);
    stree->SetBranchAddress("Max_Part_Et",&Max_Part_Et);
    stree->SetBranchAddress("Max_pT",&Max_pT);
    stree->SetBranchAddress("Max_eta",&Max_eta);
    stree->SetBranchAddress("Max_id",&Max_id);
    stree->SetBranchAddress("Max_A_Et",&Max_A_Et);
    stree->SetBranchAddress("Max_A_pT",&Max_A_pT);
    stree->SetBranchAddress("Max_A_id",&Max_A_id);
    stree->SetBranchAddress("Max_A_eta",&Max_A_eta);
    stree->SetBranchAddress("Max_C_Et",&Max_C_Et);
    stree->SetBranchAddress("Max_C_pT",&Max_C_pT);
    stree->SetBranchAddress("Max_C_id",&Max_C_id);
    stree->SetBranchAddress("Max_C_eta",&Max_C_eta);
    stree->SetBranchAddress("Max_P_Et",&Max_P_Et);
    stree->SetBranchAddress("Max_P_pT",&Max_P_pT);
    stree->SetBranchAddress("Max_P_id",&Max_P_id);
    stree->SetBranchAddress("Max_P_eta",&Max_P_eta);
    stree->SetBranchAddress("Max_N_Et",&Max_N_Et);
    stree->SetBranchAddress("Max_N_pT",&Max_N_pT);
    stree->SetBranchAddress("Max_N_id",&Max_N_id);
    stree->SetBranchAddress("Max_N_eta",&Max_N_eta);
    stree->SetBranchAddress("Sum_pT",&Sum_pT);
    stree->SetBranchAddress("Sum_Et",&Sum_Et);
    stree->SetBranchAddress("mult",&mult);*/
   
    EveNum1=0;
    EveNum2=0;
    EveNum3=0;
    EveNum4=0;
    EveNum5=0;
    EveNum6=0;

    if (!branches) {cout <<"StJetSimuReader::InitFile().  Null branches"<<endl; abort();}

    cout <<"\tLoop on branches"<<endl;
    
    for (int i=0; i<branches->GetLast()+1; ++i) {
	TBranch* branch = dynamic_cast<TBranch*>((*branches)[i]);
	if (!branch) {cout <<"StJetSimuReader::InitFile().  Null branch"<<endl; abort();}
	string bname( branch->GetName() );
	cout <<"\t--- Found branch:\t"<<bname<<endl;
	
	if ( (bname.find("Kt")!=bname.npos) || (bname.find("Cone")!=bname.npos) ) {
	    cout <<"\t\tcreate StJets object for branch:\t"<<bname<<endl;

	    //create StJets object here, put in map:
	    StJets* jets = new StJets();
	    jets->Clear();
	    mStJetsMap[bname] = jets;
	    cout <<"\t\tset branch address for branch:\t"<<bname.c_str()<<endl;
	    t->SetBranchStatus(bname.c_str(), 1);
	    t->SetBranchAddress(bname.c_str(), &jets);
	}
    }

    if (0) {
	string jetCheck(file);
	jetCheck += ".read.txt";
	mOfstream = new ofstream(jetCheck.c_str());
    }
    
    cout <<"\tfinished!"<<endl;

    return ;
}

void StJetSimuReader::InitTree(TTree* tree)
{
    cout <<"StJetSimuReader::InitTree()"<<endl;
    cout <<"\tset tree pointer"<<endl;
    cout <<"Number of entries in tree:\t"<<tree->GetEntries();

    TList* friendList = tree->GetListOfFriends();
    TTree* t=0;
    for (int j=0; j<friendList->GetSize()+1; ++j) {
	TFriendElement* fr = static_cast<TFriendElement*>( friendList->At(j) );
	string tree_name( fr->GetTreeName() );
	if (tree_name == "jet") {
	    t = fr->GetTree();
	    break;
	}
    }
    assert(t);
    
    cout <<"\tGet Branches"<<endl;
    TObjArray* branches = t->GetListOfBranches();
    if (!branches) {cout <<"StJetSimuReader::InitFile().  Null branches"<<endl; abort();}

    cout <<"\tLoop on branches"<<endl;

    for (int i=0; i<branches->GetLast()+1; ++i) {
	TBranch* branch = dynamic_cast<TBranch*>((*branches)[i]);
	if (!branch) {cout <<"StJetSimuReader::InitFile().  Null branch"<<endl; abort();}
	string bname( branch->GetName() );
	cout <<"\t--- Found branch:\t"<<bname<<endl;
	
	if ( (bname.find("jet")!=bname.npos) || (bname.find("Jet")!=bname.npos) ) {
	    cout <<"\t\tcreate StJets object for branch:\t"<<bname<<endl;

	    //create StJets object here, put in map:
	    StJets* jets = new StJets();
	    jets->Clear();
	    mStJetsMap[bname] = jets;
	    cout <<"\t\tset branch address for branch:\t"<<bname.c_str()<<endl;
	    t->SetBranchStatus(bname.c_str(), 1);
	    t->SetBranchAddress(bname.c_str(), &jets);
	}
    }

    cout <<"\tfinished!"<<endl;

    return ;
}

Int_t StJetSimuReader::Make()
{
    if (mTree) { //handle the reading ourselves...
	int status = mTree->GetEntry(mCounter++);
	if (status<0) {
	    cout <<"StJetSimuReader::getEvent(). ERROR:\tstatus<0.  return null"<<endl;
	}
    }
    else {
	//the MuDst did the reading...
    }
    if (mDstMaker) {//double check consistency:
	StMuDst* mudst = mDstMaker->muDst();
	for (JetBranchesMap::iterator it=mStJetsMap.begin(); it!=mStJetsMap.end(); ++it) {
	    StJets* j = (*it).second;
	    
	    if ( !j->isSameEvent(mudst) ) {
		cout <<"StJetSimuReader::Maker() ERROR:\tisSameEvent()==false.  abort"<<endl;
		abort();
	    }
	}
    }

    return kStOk;
}

Int_t StJetSimuReader::Finish()
{
    if (mOfstream) {
	delete mOfstream;
	mOfstream=0;
    }

    return kStOk;
}


void StJetSimuReader::CheckPDF(){

  const  float degr=57.29577951; //180/pi
  stree->GetEntry(EveNum5);
  EveNum5++;
  
  Q2hist->Fill(Q2);

  if (flavor1==1) {//d
    pol[1]->Fill(x1,x1*df1);
    unpol[1]->Fill(x1,x1*f1);
  }
  if (flavor1==2) {//u
    pol[0]->Fill(x1,x1*df1);
    unpol[0]->Fill(x1,x1*f1);
  }
  if (flavor1==-1) {//dbar
    pol[5]->Fill(x1,x1*df1);
    unpol[5]->Fill(x1,x1*f1);
  }
  if (flavor1==-2) {//ubar
    pol[4]->Fill(x1,x1*df1);
    unpol[4]->Fill(x1,x1*f1);
  }
  if (flavor1==21) {//g
    pol[3]->Fill(x1,x1*df1);
    unpol[3]->Fill(x1,x1*f1);
  }
  if (flavor1==3) {//s
    pol[2]->Fill(x1,x1*df1);
    unpol[2]->Fill(x1,x1*f1);
  }
  if (flavor1==-3) {//sbar
    pol[6]->Fill(x1,x1*df1);
    unpol[6]->Fill(x1,x1*f1);
  }


  if (flavor2==1) {//d
    pol[1]->Fill(x2,x2*df2);
    unpol[1]->Fill(x2,x2*f2);
  }
  if (flavor2==2) {//u
    pol[0]->Fill(x2,x2*df2);
    unpol[0]->Fill(x2,x2*f2);
  }
  if (flavor2==-1) {//dbar
    pol[5]->Fill(x2,x2*df2);
    unpol[5]->Fill(x2,x2*f2);
  }
  if (flavor2==-2) {//ubar
    pol[4]->Fill(x2,x2*df2);
    unpol[4]->Fill(x2,x2*f2);
  }
  if (flavor2==21) {//g
    pol[3]->Fill(x2,x2*df2);
    unpol[3]->Fill(x2,x2*f2);
  }
  if (flavor2==3) {//s
    pol[2]->Fill(x2,x2*df2);
    unpol[2]->Fill(x2,x2*f2);
  }
  if (flavor2==-3) {//sbar
    pol[6]->Fill(x2,x2*df2);
    unpol[6]->Fill(x2,x2*f2);
  }
 
  float theta=degr*acos(cos_th);

  if ((pid==11)&&(abs(flavor1)!=abs(flavor2)))  part[3]->Fill(theta,partonic_all);//qq'=qq',qqbar'=qqbar'
  if ((pid==11)&&(abs(flavor1)==abs(flavor2))) part[1]->Fill(theta,partonic_all);//qq->qq
  if ((pid==12)&&(abs(flavor1)!=abs(flavor3))) part[2]->Fill(theta,partonic_all);//qqbar->q'qbar'
  if ((pid==12)&&(abs(flavor1)==abs(flavor3))) part[0]->Fill(theta,partonic_all);//qqbar->qqbar
  if (pid==13) part[2]->Fill(theta,partonic_all);//qqbar->GG
  if (pid==53) part[2]->Fill(theta,partonic_all);//GG->qqbar
  if (pid==28) part[3]->Fill(theta,partonic_all);//qG->qG
  if (pid==68) part[4]->Fill(theta,partonic_all);//GG->GG 
  

}


void StJetSimuReader::ConeYieldAna()
{

  double thetaJET;//jet Theta
  double etaEMC;//eta of Jet in EMC frame
  double thetaEMC;//theta in emc frame

  stree->GetEntry(EveNum2);
  EveNum2++;
  StMuDst* muDst = 0;
  if (mDstMaker!=0) {
    muDst = mDstMaker->muDst();
  }

  StMuEvent* muEvent = muDst->event();
  double verX=muEvent->primaryVertexPosition().x();
  double verY=muEvent->primaryVertexPosition().y();
  double verZ=muEvent->primaryVertexPosition().z();

   //get event id from MuDst and test if it is the same as stored in SIMU tree
  StEventInfo &info=muEvent->eventInfo();
  int dstid=info.id();
  cout << "Event # = "<< info.id() << "evtID=  " << evtid <<endl;
  assert(verifySimu(dstid,evtid));
    
  //get trigger info
  int HTtrig=0;
  int JPtrig=0;
  int AHTtrig=0;
  if (Alex_ht_DSM>10) AHTtrig=1;
  HTtrig=HT1_2004;
  JPtrig=JP1_2004;

  Count[0]->Fill(HTtrig);

  if ((verX==0.0)&&(verY==0.0)&&(verZ==0.0)) return;

  for (JetBranchesMap::iterator it=mStJetsMap.begin(); it!=mStJetsMap.end(); ++it) {
    cout <<"Found\t"<<(*it).first<<endl;
    if ((*it).first!="MkConeR02") continue;

    StJets* stjets = (*it).second;
    int nJets = stjets->nJets();
    TClonesArray* jets = stjets->jets();
    
    //Only continue if BBC trigger condition met && nJet>0
    if (bbc==0) continue;
    if (nJets==0) continue;
    
    for (int ijet=0;ijet<nJets;ijet++){//loop over jets
      
      StJet* j = static_cast<StJet*>( (*jets)[ijet] );
      assert(j);
      assert(verifyJet(stjets, ijet));	    
      cout <<"jet:\t"<<ijet<<"\tPt:\t"<<j->Pt()<<"\tEta:\t"<<j->Eta()<<"\tPhi:\t"<<j->Phi()<<endl;
      
      //find eta of Jet in STAR BEMC frame
      thetaJET=getTheta(j->Eta());
      thetaEMC=getEmcTheta(verZ/100,thetaJET);
      etaEMC=getEta(thetaEMC);

      if (fabs(verZ)>60.0) continue;  
      if (etaEMC>0.8) continue;
      if (etaEMC<0.2) continue;	
      
      jpTA[0]->Fill(j->Pt());
      if (HTtrig==1){
	jpTA[1]->Fill(j->Pt());
      }
    }
  }

  for (JetBranchesMap::iterator it=mStJetsMap.begin(); it!=mStJetsMap.end(); ++it) {
    cout <<"Found\t"<<(*it).first<<endl;
    if ((*it).first!="MkConeR04") continue;

    StJets* stjets = (*it).second;
    int nJets = stjets->nJets();
    TClonesArray* jets = stjets->jets();
    
    //Only continue if BBC trigger condition met && nJet>0
    if (bbc==0) continue;
    if (nJets==0) continue;

    for (int ijet=0;ijet<nJets;ijet++){//loop over jets
      
      StJet* j = static_cast<StJet*>( (*jets)[ijet] );
      assert(j);
      assert(verifyJet(stjets, ijet));	    
      cout <<"jet:\t"<<ijet<<"\tPt:\t"<<j->Pt()<<"\tEta:\t"<<j->Eta()<<"\tPhi:\t"<<j->Phi()<<endl;
      
      //find eta of Jet in STAR BEMC frame
      thetaJET=getTheta(j->Eta());
      thetaEMC=getEmcTheta(verZ/100,thetaJET);
      etaEMC=getEta(thetaEMC);

      if (fabs(verZ)>60.0) continue;  
      if (etaEMC>0.8) continue;
      if (etaEMC<0.2) continue;	

      jpTB[0]->Fill(j->Pt());
      if (HTtrig==1){
	jpTB[1]->Fill(j->Pt());
      }   
    }
  }


  for (JetBranchesMap::iterator it=mStJetsMap.begin(); it!=mStJetsMap.end(); ++it) {
    cout <<"Found\t"<<(*it).first<<endl;
    if ((*it).first!="MkConeR06") continue;

    StJets* stjets = (*it).second;
    int nJets = stjets->nJets();
    TClonesArray* jets = stjets->jets();
    cout <<"Found\t"<<nJets<<"\tjets from:\t"<<(*it).first<<endl;
    
    //Only continue if BBC trigger condition met && nJet>0
    if (bbc==0) continue;
    if (nJets==0) continue;

    for (int ijet=0;ijet<nJets;ijet++){//loop over jets
      
      StJet* j = static_cast<StJet*>( (*jets)[ijet] );
      assert(j);
      assert(verifyJet(stjets, ijet));	    
      cout <<"jet:\t"<<ijet<<"\tPt:\t"<<j->Pt()<<"\tEta:\t"<<j->Eta()<<"\tPhi:\t"<<j->Phi()<<endl;
      
      //find eta of Jet in STAR BEMC frame
      thetaJET=getTheta(j->Eta());
      thetaEMC=getEmcTheta(verZ/100,thetaJET);
      etaEMC=getEta(thetaEMC);

      if (fabs(verZ)>60.0) continue;  
      if (etaEMC>0.8) continue;
      if (etaEMC<0.2) continue;	

      jpTC[0]->Fill(j->Pt());
      if (HTtrig==1){
	jpTC[1]->Fill(j->Pt());
      }
    }
  }


  for (JetBranchesMap::iterator it=mStJetsMap.begin(); it!=mStJetsMap.end(); ++it) {
    cout <<"Found\t"<<(*it).first<<endl;
    if ((*it).first!="MkConeR08") continue;

    StJets* stjets = (*it).second;
    int nJets = stjets->nJets();
    TClonesArray* jets = stjets->jets();
    
    //Only continue if BBC trigger condition met && nJet>0
    if (bbc==0) continue;
    if (nJets==0) continue;
    
    for (int ijet=0;ijet<nJets;ijet++){//loop over jets
      
      StJet* j = static_cast<StJet*>( (*jets)[ijet] );
      assert(j);
      assert(verifyJet(stjets, ijet));	    
      cout <<"jet:\t"<<ijet<<"\tPt:\t"<<j->Pt()<<"\tEta:\t"<<j->Eta()<<"\tPhi:\t"<<j->Phi()<<endl;
      
      //find eta of Jet in STAR BEMC frame
      thetaJET=getTheta(j->Eta());
      thetaEMC=getEmcTheta(verZ/100,thetaJET);
      etaEMC=getEta(thetaEMC);

      if (fabs(verZ)>60.0) continue;  
      if (etaEMC>0.8) continue;
      if (etaEMC<0.2) continue;	
      
      jpTD[0]->Fill(j->Pt());
      if (HTtrig==1){
	jpTD[1]->Fill(j->Pt());
      }
    }
  }


  for (JetBranchesMap::iterator it=mStJetsMap.begin(); it!=mStJetsMap.end(); ++it) {
    cout <<"Found\t"<<(*it).first<<endl;
    if ((*it).first!="MkConeR010") continue;

    StJets* stjets = (*it).second;
    int nJets = stjets->nJets();
    TClonesArray* jets = stjets->jets();
    
    //Only continue if BBC trigger condition met && nJet>0
    if (bbc==0) continue;
    if (nJets==0) continue;
    
    for (int ijet=0;ijet<nJets;ijet++){//loop over jets
      
      StJet* j = static_cast<StJet*>( (*jets)[ijet] );
      assert(j);
      assert(verifyJet(stjets, ijet));	    
      cout <<"jet:\t"<<ijet<<"\tPt:\t"<<j->Pt()<<"\tEta:\t"<<j->Eta()<<"\tPhi:\t"<<j->Phi()<<endl;
      
      //find eta of Jet in STAR BEMC frame
      thetaJET=getTheta(j->Eta());
      thetaEMC=getEmcTheta(verZ/100,thetaJET);
      etaEMC=getEta(thetaEMC);

      if (fabs(verZ)>60.0) continue;  
      if (etaEMC>0.8) continue;
      if (etaEMC<0.2) continue;	
      
      jpTE[0]->Fill(j->Pt());
      if (HTtrig==1){
	jpTE[1]->Fill(j->Pt());
      }
    }
  }


  for (JetBranchesMap::iterator it=mStJetsMap.begin(); it!=mStJetsMap.end(); ++it) {
    cout <<"Found\t"<<(*it).first<<endl;
    if ((*it).first!="PythiaConeR02") continue;

    StJets* stjets = (*it).second;
    int nJets = stjets->nJets();
    TClonesArray* jets = stjets->jets();
    
    //Only continue if BBC trigger condition met && nJet>0
    if (bbc==0) continue;
    if (nJets==0) continue;
    
    for (int ijet=0;ijet<nJets;ijet++){//loop over jets
      
      StJet* j = static_cast<StJet*>( (*jets)[ijet] );
      assert(j);
      assert(verifyJet(stjets, ijet));	    
      cout <<"jet:\t"<<ijet<<"\tPt:\t"<<j->Pt()<<"\tEta:\t"<<j->Eta()<<"\tPhi:\t"<<j->Phi()<<endl;
      
      //find eta of Jet in STAR BEMC frame
      thetaJET=getTheta(j->Eta());
      thetaEMC=getEmcTheta(verZ/100,thetaJET);
      etaEMC=getEta(thetaEMC);

      if (fabs(verZ)>60.0) continue;  
      if (etaEMC>0.8) continue;
      if (etaEMC<0.2) continue;	
      
      PYjpTA[0]->Fill(j->Pt());
      if (HTtrig==1){
	PYjpTA[1]->Fill(j->Pt());
      }
    }
  }

  for (JetBranchesMap::iterator it=mStJetsMap.begin(); it!=mStJetsMap.end(); ++it) {
    cout <<"Found\t"<<(*it).first<<endl;
    if ((*it).first!="PythiaConeR04") continue;

    StJets* stjets = (*it).second;
    int nJets = stjets->nJets();
    TClonesArray* jets = stjets->jets();
    
    //Only continue if BBC trigger condition met && nJet>0
    if (bbc==0) continue;
    if (nJets==0) continue;

    for (int ijet=0;ijet<nJets;ijet++){//loop over jets
      
      StJet* j = static_cast<StJet*>( (*jets)[ijet] );
      assert(j);
      assert(verifyJet(stjets, ijet));	    
      cout <<"jet:\t"<<ijet<<"\tPt:\t"<<j->Pt()<<"\tEta:\t"<<j->Eta()<<"\tPhi:\t"<<j->Phi()<<endl;
      
      //find eta of Jet in STAR BEMC frame
      thetaJET=getTheta(j->Eta());
      thetaEMC=getEmcTheta(verZ/100,thetaJET);
      etaEMC=getEta(thetaEMC);

      if (fabs(verZ)>60.0) continue;  
      if (etaEMC>0.8) continue;
      if (etaEMC<0.2) continue;	

      PYjpTB[0]->Fill(j->Pt());
      if (HTtrig==1){
	PYjpTB[1]->Fill(j->Pt());
      }   
    }
  }




  for (JetBranchesMap::iterator it=mStJetsMap.begin(); it!=mStJetsMap.end(); ++it) {
    cout <<"Found\t"<<(*it).first<<endl;
    if ((*it).first!="PythiaConeR06") continue;

    StJets* stjets = (*it).second;
    int nJets = stjets->nJets();
    TClonesArray* jets = stjets->jets();
    cout <<"Found\t"<<nJets<<"\tjets from:\t"<<(*it).first<<endl;
    
    //Only continue if BBC trigger condition met && nJet>0
    if (bbc==0) continue;
    if (nJets==0) continue;

    for (int ijet=0;ijet<nJets;ijet++){//loop over jets
      
      StJet* j = static_cast<StJet*>( (*jets)[ijet] );
      assert(j);
      assert(verifyJet(stjets, ijet));	    
      cout <<"jet:\t"<<ijet<<"\tPt:\t"<<j->Pt()<<"\tEta:\t"<<j->Eta()<<"\tPhi:\t"<<j->Phi()<<endl;
      
      //find eta of Jet in STAR BEMC frame
      thetaJET=getTheta(j->Eta());
      thetaEMC=getEmcTheta(verZ/100,thetaJET);
      etaEMC=getEta(thetaEMC);

      if (fabs(verZ)>60.0) continue;  
      if (etaEMC>0.8) continue;
      if (etaEMC<0.2) continue;	

      PYjpTC[0]->Fill(j->Pt());
      if (HTtrig==1){
	PYjpTC[1]->Fill(j->Pt());
      }
    }
  }


  for (JetBranchesMap::iterator it=mStJetsMap.begin(); it!=mStJetsMap.end(); ++it) {
    cout <<"Found\t"<<(*it).first<<endl;
    if ((*it).first!="PythiaConeR08") continue;

    StJets* stjets = (*it).second;
    int nJets = stjets->nJets();
    TClonesArray* jets = stjets->jets();
    
    //Only continue if BBC trigger condition met && nJet>0
    if (bbc==0) continue;
    if (nJets==0) continue;
    
    for (int ijet=0;ijet<nJets;ijet++){//loop over jets
      
      StJet* j = static_cast<StJet*>( (*jets)[ijet] );
      assert(j);
      assert(verifyJet(stjets, ijet));	    
      cout <<"jet:\t"<<ijet<<"\tPt:\t"<<j->Pt()<<"\tEta:\t"<<j->Eta()<<"\tPhi:\t"<<j->Phi()<<endl;
      
      //find eta of Jet in STAR BEMC frame
      thetaJET=getTheta(j->Eta());
      thetaEMC=getEmcTheta(verZ/100,thetaJET);
      etaEMC=getEta(thetaEMC);

      if (fabs(verZ)>60.0) continue;  
      if (etaEMC>0.8) continue;
      if (etaEMC<0.2) continue;	
      
      PYjpTD[0]->Fill(j->Pt());
      if (HTtrig==1){
	PYjpTD[1]->Fill(j->Pt());
      }
    }
  }


  for (JetBranchesMap::iterator it=mStJetsMap.begin(); it!=mStJetsMap.end(); ++it) {
    cout <<"Found\t"<<(*it).first<<endl;
    if ((*it).first!="PythiaConeR010") continue;

    StJets* stjets = (*it).second;
    int nJets = stjets->nJets();
    TClonesArray* jets = stjets->jets();
    
    //Only continue if BBC trigger condition met && nJet>0
    if (bbc==0) continue;
    if (nJets==0) continue;
    
    for (int ijet=0;ijet<nJets;ijet++){//loop over jets
      
      StJet* j = static_cast<StJet*>( (*jets)[ijet] );
      assert(j);
      assert(verifyJet(stjets, ijet));	    
      cout <<"jet:\t"<<ijet<<"\tPt:\t"<<j->Pt()<<"\tEta:\t"<<j->Eta()<<"\tPhi:\t"<<j->Phi()<<endl;
      
      //find eta of Jet in STAR BEMC frame
      thetaJET=getTheta(j->Eta());
      thetaEMC=getEmcTheta(verZ/100,thetaJET);
      etaEMC=getEta(thetaEMC);

      if (fabs(verZ)>60.0) continue;  
      if (etaEMC>0.8) continue;
      if (etaEMC<0.2) continue;	
      
      PYjpTE[0]->Fill(j->Pt());
      if (HTtrig==1){
	PYjpTE[1]->Fill(j->Pt());
      }
    }
  }
}

void StJetSimuReader::JetAssociator(){

  for (int g=0;g<10;g++){
    if (GpT[g]==0.0) continue;
    printf("Geant pT, Eta, Phi = %f,%f,%f\n",GpT[g],GEta[g],GPhi[g]);
    
    float hold_dR=100.0;
    float ratio_pT=0.0;
    float py_pT=0.0;
    float ge_pT=GpT[g];
    for (int p=0;p<10;p++){
      if (PpT[p]==0.0) continue;
      float dPhi=GPhi[g]-PPhi[p];
      float dEta=GEta[g]-PEta[p];
      float dR=sqrt(dPhi*dPhi + dEta*dEta);
      printf("Pythia pT,Eta, Phi = %f,%f,%f\n",PpT[p],PEta[p],PPhi[p]);

      if (dR<hold_dR){ 
	hold_dR=dR;
	ratio_pT=(PpT[p]-GpT[g])/PpT[p];
	py_pT=PpT[p];
	ge_pT=GpT[g];
      }
    }
    cout <<"Min dR = "<<hold_dR<<" Pythia/Geant pT= "<<ratio_pT<<"Pythia pT = "<<py_pT<<"Geant pT = "<<ge_pT<<endl;

    if (hold_dR<0.2){
      AssocA[0]->Fill(hold_dR);
      AssocA[1]->Fill(ratio_pT);
      Assoc2A[0]->Fill(ge_pT,ratio_pT);
      Assoc2A[1]->Fill(hold_dR,ratio_pT);
      matrix[0]->Fill(py_pT,ge_pT);
    }
      
    /*2004 simu
    cout<<"Condition = "<<cond<<endl;
    if (cond=='A'){
      AssocA[0]->Fill(hold_dR);
      AssocA[1]->Fill(ratio_pT);
      Assoc2A[0]->Fill(ge_pT,ratio_pT);
      Assoc2A[1]->Fill(hold_dR,ratio_pT);
      matrix[0]->Fill(py_pT,ge_pT);
    }
    if (cond=='B'){
      AssocB[0]->Fill(hold_dR);
      AssocB[1]->Fill(ratio_pT);
      Assoc2B[0]->Fill(ge_pT,ratio_pT);
      Assoc2B[1]->Fill(hold_dR,ratio_pT);
      matrix[1]->Fill(py_pT,ge_pT);
    }
    if (cond=='C'){
      AssocC[0]->Fill(hold_dR);
      AssocC[1]->Fill(ratio_pT);
      Assoc2C[0]->Fill(ge_pT,ratio_pT);
      Assoc2C[1]->Fill(hold_dR,ratio_pT);
      matrix[2]->Fill(py_pT,ge_pT);
    }
    if (cond=='D'){
      AssocD[0]->Fill(hold_dR);
      AssocD[1]->Fill(ratio_pT);
      Assoc2D[0]->Fill(ge_pT,ratio_pT);
      Assoc2D[1]->Fill(hold_dR,ratio_pT);
      matrix[3]->Fill(py_pT,ge_pT);
    }
    */
  }
}


void StJetSimuReader::PythiaAna(){

  cout<<"StJetSimuReader::PythiaAna()"<<endl;
  //Clear Associator Jet Array
  for (int c=0;c<10;c++){
    PPhi[c]=0;
    PEta[c]=0;
    PpT[c]=0;
    PNeu[c]=0;
  }

  for (JetBranchesMap::iterator it=mStJetsMap.begin(); it!=mStJetsMap.end(); ++it) {
    cout <<"Found\t"<<(*it).first<<endl;
    if ((*it).first!="PythiaConeR04") continue;

    StJets* stjets = (*it).second;
    int nJets = stjets->nJets();
    TClonesArray* jets = stjets->jets();
    cout<<" # PYTHIA JETS ="<<nJets<<endl;
    for (int ijet=0;ijet<nJets;ijet++){
      StJet* j = static_cast<StJet*>( (*jets)[ijet] );
      assert(j);
      assert(verifyJet(stjets, ijet));	    
      
      PEta[ijet]=j->Eta();
      PPhi[ijet]=j->Phi();
      PpT[ijet]=j->Pt(); 
      PEt[ijet]=j->Et();
      cout<<"Jet Pt="<<PpT[ijet]<<endl;

      int num=0;

      typedef vector<TrackToJetIndex*> TrackToJetVec;
      TrackToJetVec particles = stjets->particles(ijet);
      for (TrackToJetVec::iterator it=particles.begin(); it!=particles.end(); ++it) {
	TrackToJetIndex* t2j = (*it);
	assert(t2j);
	for (int j=0;j<50;j++){
	  if (Row[j]==t2j->trackIndex()) PNeu[ijet]+=t2j->Et();
	  if (Row[j]==t2j->trackIndex()) cout<<Row[j]<<" t2j->Et()="<<t2j->Et()<<" t2j->Eta()="<<t2j->Eta()<<" My Et="<<NeuEt[j]<<endl;
	}
	num++;
      }
    }
  }
}


void StJetSimuReader::EnergyScale(){

  PythiaAna();

  //const  float degr=57.29577951; //180/pi
  double thetaJET;//jet Theta
  double etaEMC;//eta of Jet in EMC frame
  double thetaEMC;//theta in emc frame


  stree->GetEntry(EveNum1);
  EveNum1++;
  StMuDst* muDst = 0;
  if (mDstMaker!=0) {
    muDst = mDstMaker->muDst();
  }

  StMuEvent* muEvent = muDst->event();
  double verX=muEvent->primaryVertexPosition().x();
  double verY=muEvent->primaryVertexPosition().y();
  double verZ=muEvent->primaryVertexPosition().z();

   //get event id from MuDst and test if it is the same as stored in SIMU tree
  StEventInfo &info=muEvent->eventInfo();
  int dstid=info.id();
  cout << "Event # = "<< info.id() << "evtID=  " << evtid <<endl;
  assert(verifySimu(dstid,evtid));
    
  //get trigger info
  int HTtrig=0;
  int JPtrig=0;
  int AHTtrig=0;
  int HTid=0;
  int JPid=0;
  if (Alex_ht_DSM>10) AHTtrig=1;
  HTtrig=HT1_2004;
  HTid=HT1_2004_Tow;
  JPtrig=JP1_2004;
  JPid=JP1_2004_Patch;

  if ((verX==0.0)&&(verY==0.0)&&(verZ==0.0)) return;

  for (JetBranchesMap::iterator it=mStJetsMap.begin(); it!=mStJetsMap.end(); ++it) {
    cout <<"Found\t"<<(*it).first<<endl;
    if ((*it).first!="MkConeR04") continue;

    StJets* stjets = (*it).second;
    int nJets = stjets->nJets();
    TClonesArray* jets = stjets->jets();
    cout <<"Found\t"<<nJets<<"\tjets from:\t"<<(*it).first<<endl;
    

    //Only continue if BBC trigger condition met && nJet>0
    if (bbc==0) continue;
    if (nJets==0) continue;
    
    for (int ijet=0;ijet<nJets;ijet++){//loop over jets
      
      StJet* j = static_cast<StJet*>( (*jets)[ijet] );
      assert(j);
      assert(verifyJet(stjets, ijet));	    
      cout <<"jet:\t"<<ijet<<"\tPt:\t"<<j->Pt()<<"\tEta:\t"<<j->Eta()<<"\tPhi:\t"<<j->Phi()<<endl;
      
      //find eta of Jet in STAR BEMC frame
      thetaJET=getTheta(j->Eta());
      thetaEMC=getEmcTheta(verZ/100,thetaJET);
      etaEMC=getEta(thetaEMC);

      if (fabs(verZ)>60.0) continue;  
      if (etaEMC>0.8) continue;
      if (etaEMC<0.2) continue;	


      //Association
      float hold_dR=100.0;
      int PYindex=-1;
      for (int p=0;p<10;p++){
	if (PpT[p]==0.0) continue;
	float dPhi=j->Phi()-PPhi[p];
	float dEta=j->Eta()-PEta[p];
	float dR=sqrt(dPhi*dPhi + dEta*dEta);
	printf("Pythia pT,Eta, Phi = %f,%f,%f\n",PpT[p],PEta[p],PPhi[p]);
	
	if (dR<hold_dR){ 
	  hold_dR=dR;
	  PYindex=p;
	}
      }
     


      //look at 4-momenta in the jet:
      int trkCounter=0;
      int towCounter=0;
      Double_t trkpT=0;
      Double_t towE=0;
      float EmcE=0;
      int trigJet=0;//default is trigger tower is not in jet

      typedef vector<TrackToJetIndex*> TrackToJetVec;
      TrackToJetVec particles = stjets->particles(ijet);
      for (TrackToJetVec::iterator it=particles.begin(); it!=particles.end(); ++it) {
	TrackToJetIndex* t2j = (*it); 
	assert(t2j);
	if (idString(t2j)=="kTpcId") {
	  trkCounter++;
	  trkpT+=t2j->Pt();

	}
	if (idString(t2j)=="kBarrelEmcTowerId") {
	  if (HTid==t2j->trackIndex()) trigJet=1;
	  towCounter++;
	  towE+=t2j->E();
	  EmcE=t2j->E()+EmcE; 

	}
      }
      EmcE=EmcE/j->E();

      js[0]->Fill(hold_dR);
      if ((HTtrig==1)&&(hold_dR<0.2)&&(bbc==1)){
	js[1]->Fill(j->Et());
	if (pid==11) js[2]->Fill(j->Et());
	if (pid==28) js[3]->Fill(j->Et());
	if ((pid==53)||(pid==68)) js[4]->Fill(j->Et());
	if (trigJet==1){//trigger Geant jet
	  js[5]->Fill(j->Et());
	  TScale[0]->Fill(j->Et(),PEt[PYindex]);
	  TScale[1]->Fill(j->Et(),PNeu[PYindex]);
	  TScale[2]->Fill(j->Et(),PEt[PYindex]-PNeu[PYindex]);
	  if (pid==11){
	    js[6]->Fill(j->Et());
	    qqTScale[0]->Fill(j->Et(),PEt[PYindex]);
	    qqTScale[1]->Fill(j->Et(),PNeu[PYindex]);
	    qqTScale[2]->Fill(j->Et(),PEt[PYindex]-PNeu[PYindex]);
	  }
	  if (pid==28){
	    js[7]->Fill(j->Et());
	    qgTScale[0]->Fill(j->Et(),PEt[PYindex]);
	    qgTScale[1]->Fill(j->Et(),PNeu[PYindex]);
	    qgTScale[2]->Fill(j->Et(),PEt[PYindex]-PNeu[PYindex]);
	  }
	  if ((pid==53)||(pid==68)){
	    js[8]->Fill(j->Et());
	    ggTScale[0]->Fill(j->Et(),PEt[PYindex]);
	    ggTScale[1]->Fill(j->Et(),PNeu[PYindex]);
	    ggTScale[2]->Fill(j->Et(),PEt[PYindex]-PNeu[PYindex]);
	  }
	}

	if (trigJet==0){//away side Geant jet
	  js[9]->Fill(j->Et());
	  AScale[0]->Fill(j->Et(),PEt[PYindex]);
	  AScale[1]->Fill(j->Et(),PNeu[PYindex]);
	  AScale[2]->Fill(j->Et(),PEt[PYindex]-PNeu[PYindex]);
	  if (pid==11){
	    js[10]->Fill(j->Et());
	    qqAScale[0]->Fill(j->Et(),PEt[PYindex]);
	    qqAScale[1]->Fill(j->Et(),PNeu[PYindex]);
	    qqAScale[2]->Fill(j->Et(),PEt[PYindex]-PNeu[PYindex]);
	  }
	  if (pid==28){
	    js[11]->Fill(j->Et());
	    qgAScale[0]->Fill(j->Et(),PEt[PYindex]);
	    qgAScale[1]->Fill(j->Et(),PNeu[PYindex]);
	    qgAScale[2]->Fill(j->Et(),PEt[PYindex]-PNeu[PYindex]);
	  }
	  if ((pid==53)||(pid==68)){
	    js[12]->Fill(j->Et());
	    ggAScale[0]->Fill(j->Et(),PEt[PYindex]);
	    ggAScale[1]->Fill(j->Et(),PNeu[PYindex]);
	    ggAScale[2]->Fill(j->Et(),PEt[PYindex]-PNeu[PYindex]);
	  }
	}
      }
    }
  }

}

void StJetSimuReader::SimuAna(){

  
  const  float degr=57.29577951; //180/pi
  double thetaJET;//jet Theta
  double etaEMC;//eta of Jet in EMC frame
  double thetaEMC;//theta in emc frame

  stree->GetEntry(EveNum3);
  EveNum3++;
  StMuDst* muDst = 0;
  if (mDstMaker!=0) {
    muDst = mDstMaker->muDst();
  }

  StMuEvent* muEvent = muDst->event();
  double verX=muEvent->primaryVertexPosition().x();
  double verY=muEvent->primaryVertexPosition().y();
  double verZ=muEvent->primaryVertexPosition().z();

   //get event id from MuDst and test if it is the same as stored in SIMU tree
  StEventInfo &info=muEvent->eventInfo();
  int dstid=info.id();
  cout << "Event # = "<< info.id() << "evtID=  " << evtid <<endl;
  assert(verifySimu(dstid,evtid));
    
  //get trigger info
  int HTtrig=0;
  int JPtrig=0;
  int AHTtrig=0;
  int HTid=0;
  int JPid=0;
  if (Alex_ht_DSM>10) AHTtrig=1;
  HTtrig=HT1_2004;
  HTid=HT1_2004_Tow;
  JPtrig=JP1_2004;
  JPid=JP1_2004_Patch;

  Count[0]->Fill(HTtrig);
  Count[7]->Fill(bbc);
  Count[8]->Fill(JPtrig);
  Count[9]->Fill(AHTtrig);
  Count[10]->Fill(HT1_2004_Tow);
  Count[11]->Fill(JP1_2004_Patch);
  Count[12]->Fill(Alex_ht_id);
  Count[13]->Fill(HT1_2004_DSM);
  Count[14]->Fill(JP1_2004_DSM);
  Count[15]->Fill(Alex_ht_DSM);

  if ((verX==0.0)&&(verY==0.0)&&(verZ==0.0)) return;
  Count[1]->Fill(bbc);
  Count[2]->Fill(verZ);
  if (bbc==1) Count[3]->Fill(verZ);


  for (JetBranchesMap::iterator it=mStJetsMap.begin(); it!=mStJetsMap.end(); ++it) {
    cout <<"Found\t"<<(*it).first<<endl;
    if ((*it).first!="MkConeR04") continue;

    StJets* stjets = (*it).second;
    int nJets = stjets->nJets();
    TClonesArray* jets = stjets->jets();
    cout <<"Found\t"<<nJets<<"\tjets from:\t"<<(*it).first<<endl;
    
    for (int ijet=0;ijet<nJets;ijet++){
      Count[4]->Fill(bbc);
      Count[5]->Fill(verZ);
      if (bbc==1) Count[6]->Fill(verZ);
    }

    //Only continue if BBC trigger condition met && nJet>0
    //if (bbc==0) continue;
    if (nJets==0) continue;
    
    for (int ijet=0;ijet<nJets;ijet++){//loop over jets
      
      StJet* j = static_cast<StJet*>( (*jets)[ijet] );
      assert(j);
      assert(verifyJet(stjets, ijet));	    
           
      //find eta of Jet in STAR BEMC frame
      thetaJET=getTheta(j->Eta());
      thetaEMC=getEmcTheta(verZ/100,thetaJET);
      etaEMC=getEta(thetaEMC);

      if (fabs(verZ)>60.0) continue;  
      if (etaEMC>0.8) continue;
      if (etaEMC<0.2) continue;	
      
      //look at 4-momenta in the jet:
      int trkCounter=0;
      int towCounter=0;
      Double_t trkpT=0;
      Double_t towE=0;
      Double_t trkDR=0.0;
      Double_t towDR=0.0;
      float EmcE=0;
      Double_t SumTrk[9],SumTow[9];
      for (int xx=0;xx<9;xx++){
	SumTrk[xx]=0;
	SumTow[xx]=0;
      }
      typedef vector<TrackToJetIndex*> TrackToJetVec;
      TrackToJetVec particles = stjets->particles(ijet);
      for (TrackToJetVec::iterator it=particles.begin(); it!=particles.end(); ++it) {
	TrackToJetIndex* t2j = (*it); 
	assert(t2j);
	if (idString(t2j)=="kTpcId") {
	  trkCounter++;
	  trkpT+=t2j->Pt();
	  trkDR=pow(pow(j->Eta()-t2j->Eta(),2)+pow(j->Phi()-t2j->Phi(),2),0.5);
	  if (trkDR<0.05) SumTrk[0]+=t2j->Pt();
	  if (trkDR<0.1) SumTrk[1]+=t2j->Pt();
	  if (trkDR<0.15) SumTrk[2]+=t2j->Pt();
	  if (trkDR<0.2) SumTrk[3]+=t2j->Pt();
	  if (trkDR<0.25) SumTrk[4]+=t2j->Pt();
	  if (trkDR<0.3) SumTrk[5]+=t2j->Pt();
	  if (trkDR<0.35) SumTrk[6]+=t2j->Pt();
	  if (trkDR<0.4) SumTrk[7]+=t2j->Pt();
	  if (trkDR>=0.4) SumTrk[8]+=t2j->Pt();
	}
	if (idString(t2j)=="kBarrelEmcTowerId") {
	  towCounter++;
	  towE+=t2j->Et();
	  EmcE=t2j->Et()+EmcE; 
	  towDR=pow(pow(j->Eta()-t2j->Eta(),2)+pow(j->Phi()-t2j->Phi(),2),0.5);
	  if (towDR<0.05) SumTow[0]+=t2j->Et();
	  if (towDR<0.1) SumTow[1]+=t2j->Et();
	  if (towDR<0.15) SumTow[2]+=t2j->Et();
	  if (towDR<0.2) SumTow[3]+=t2j->Et();
	  if (towDR<0.25) SumTow[4]+=t2j->Et();
	  if (towDR<0.3) SumTow[5]+=t2j->Et();
	  if (towDR<0.35) SumTow[6]+=t2j->Et();
	  if (towDR<0.4) SumTow[7]+=t2j->Et();
	  if (towDR>=0.4) SumTow[8]+=t2j->Et();
	}
      }
      EmcE=EmcE/j->Et();
      for (int xx=0;xx<10;xx++){
	SumTow[xx]/=j->Et();
	SumTrk[xx]/=j->Pt();
      }

      if (EmcE>0.9) continue;
      
      //trigger bias
      RAWjpTA[0]->Fill(j->Pt());
      RAWjpTA[1]->Fill(j->Eta());
      RAWjpTA[2]->Fill(j->Phi()*degr);
      RAWjpTA[3]->Fill(EmcE);
      //RAWhard[0]->Fill(j->Pt(),hard_p);
      //RAWs[0]->Fill(j->Pt(),ss);
      //RAWt[0]->Fill(j->Pt(),tt);
      //RAWu[0]->Fill(j->Pt(),uu);
      //RAWx1[0]->Fill(j->Pt(),x1);f
      //RAWx2[0]->Fill(j->Pt(),x2);
      //RAWcosth[0]->Fill(j->Pt(),cos_th);
      if (pid==11){
	qqRAWjpTA[0]->Fill(j->Pt());
	qqRAWjpTA[1]->Fill(j->Eta());
	qqRAWjpTA[2]->Fill(j->Phi()*degr);
	qqRAWjpTA[3]->Fill(EmcE);
	//qqRAWhard[0]->Fill(j->Pt(),hard_p);
	//qqRAWs[0]->Fill(j->Pt(),ss);
	//qqRAWt[0]->Fill(j->Pt(),tt);
	//qqRAWu[0]->Fill(j->Pt(),uu);
	///qqRAWx1[0]->Fill(j->Pt(),x1);
	//qqRAWx2[0]->Fill(j->Pt(),x2);
	//qqRAWcosth[0]->Fill(j->Pt(),cos_th);
      }
      if (pid==28){
	qgRAWjpTA[0]->Fill(j->Pt());
	qgRAWjpTA[1]->Fill(j->Eta());
	qgRAWjpTA[2]->Fill(j->Phi()*degr);
	qgRAWjpTA[3]->Fill(EmcE);
	//qgRAWhard[0]->Fill(j->Pt(),hard_p);
	//qgRAWs[0]->Fill(j->Pt(),ss);
	//qgRAWt[0]->Fill(j->Pt(),tt);
	//qgRAWu[0]->Fill(j->Pt(),uu);
	//qgRAWx1[0]->Fill(j->Pt(),x1);
	//qgRAWx2[0]->Fill(j->Pt(),x2);
	//qgRAWcosth[0]->Fill(j->Pt(),cos_th);
      }
      if ((pid==53)||(pid==68)){
	ggRAWjpTA[0]->Fill(j->Pt());
	ggRAWjpTA[1]->Fill(j->Eta());
	ggRAWjpTA[2]->Fill(j->Phi()*degr);
	ggRAWjpTA[3]->Fill(EmcE);
	//ggRAWhard[0]->Fill(j->Pt(),hard_p);
	//ggRAWs[0]->Fill(j->Pt(),ss);
	//ggRAWt[0]->Fill(j->Pt(),tt);
	//ggRAWu[0]->Fill(j->Pt(),uu);
	//ggRAWx1[0]->Fill(j->Pt(),x1);
	///ggRAWx2[0]->Fill(j->Pt(),x2);
	//ggRAWcosth[0]->Fill(j->Pt(),cos_th);
      }
    
    

      if (bbc==1){
	//joanna's plots
	if (j->Et()>5){
	  jo[0]->Fill(EmcE);
	  if ((HTtrig==1)&&(JPtrig!=1)) jo[1]->Fill(EmcE);
	  if ((JPtrig==1)&&(HTtrig!=1)) jo[2]->Fill(EmcE);
	  if ((HTtrig==1)&&(JPtrig==1)) jo[3]->Fill(EmcE);
	}
	
	//trigger bias
	partpT->Fill(hard_p);
	jpTA[0]->Fill(j->Pt());
	jpTA[1]->Fill(j->Eta());
	jpTA[2]->Fill(j->Phi()*degr);
	jpTA[3]->Fill(EmcE);
	//hhard[0]->Fill(j->Pt(),hard_p);
	//cout<<"ss="<<ss<<" tt="<<tt<<endl;
	//hs[0]->Fill(j->Pt(),ss);
	//ht[0]->Fill(j->Pt(),tt);
	//hu[0]->Fill(j->Pt(),uu);
	//hx1[0]->Fill(j->Pt(),x1);
	//hx1[1]->Fill(j->Eta(),x1);
	//hx1[2]->Fill(j->Phi()*degr,x1);
	//hx1[3]->Fill(EmcE,x1);
	//hx2[0]->Fill(j->Pt(),x2);
	//hx2[1]->Fill(j->Eta(),x2);
	//hx2[2]->Fill(j->Phi()*degr,x2);
	//hx2[3]->Fill(EmcE,x2);
	//costh[0]->Fill(j->Pt(),cos_th);
	//costh[1]->Fill(j->Eta(),cos_th);
	//costh[2]->Fill(j->Phi()*degr,cos_th);
	//costh[3]->Fill(EmcE,cos_th);
	if (pid==11){
	  qqjpTA[0]->Fill(j->Pt());
	  qqjpTA[1]->Fill(j->Eta());
	  qqjpTA[2]->Fill(j->Phi()*degr);
	  qqjpTA[3]->Fill(EmcE);
	  //qqhard[0]->Fill(j->Pt(),hard_p);
	  //qqs[0]->Fill(j->Pt(),ss);
	  //qqt[0]->Fill(j->Pt(),tt);
	  //qqu[0]->Fill(j->Pt(),uu);
	  //qqx1[0]->Fill(j->Pt(),x1);
	  //qqx1[1]->Fill(j->Eta(),x1);
	  //qqx1[2]->Fill(j->Phi()*degr,x1);
	  //qqx1[3]->Fill(EmcE,x1);
	  //qqx2[0]->Fill(j->Pt(),x2);
	  //qqx2[1]->Fill(j->Eta(),x2);
	  //qqx2[2]->Fill(j->Phi()*degr,x2);
	  //qqx2[3]->Fill(EmcE,x2);
	  //qqcosth[0]->Fill(j->Pt(),cos_th);
	  //qqcosth[1]->Fill(j->Eta(),cos_th);
	  //qqcosth[2]->Fill(j->Phi()*degr,cos_th);
	  //qqcosth[3]->Fill(EmcE,cos_th);
	}
	if (pid==28){
	  qgjpTA[0]->Fill(j->Pt());
	  qgjpTA[1]->Fill(j->Eta());
	  qgjpTA[2]->Fill(j->Phi()*degr);
	  qgjpTA[3]->Fill(EmcE);
	  //qghard[0]->Fill(j->Pt(),hard_p);
	  //qgs[0]->Fill(j->Pt(),ss);
	  //qgt[0]->Fill(j->Pt(),tt);
	  //qgu[0]->Fill(j->Pt(),uu);
	  //qgx1[0]->Fill(j->Pt(),x1);
	  //qgx1[1]->Fill(j->Eta(),x1);
	  //qgx1[2]->Fill(j->Phi()*degr,x1);
	  //qgx1[3]->Fill(EmcE,x1);
	  //qgx2[0]->Fill(j->Pt(),x2);
	  //qgx2[1]->Fill(j->Eta(),x2);
	  //qgx2[2]->Fill(j->Phi()*degr,x2);
	  //qgx2[3]->Fill(EmcE,x2);
	  //qgcosth[0]->Fill(j->Pt(),cos_th);
	  //qgcosth[1]->Fill(j->Eta(),cos_th);
	  //qgcosth[2]->Fill(j->Phi()*degr,cos_th);
	  //qgcosth[3]->Fill(EmcE,cos_th);
	}
	if ((pid==53)||(pid==68)){
	  ggjpTA[0]->Fill(j->Pt());
	  ggjpTA[1]->Fill(j->Eta());
	  ggjpTA[2]->Fill(j->Phi()*degr);
	  ggjpTA[3]->Fill(EmcE);
	  //gghard[0]->Fill(j->Pt(),hard_p);
	  //ggs[0]->Fill(j->Pt(),ss);
	  //ggt[0]->Fill(j->Pt(),tt);
	  //ggu[0]->Fill(j->Pt(),uu);
	  //ggx1[0]->Fill(j->Pt(),x1);
	  //ggx1[1]->Fill(j->Eta(),x1);
	  //ggx1[2]->Fill(j->Phi()*degr,x1);
	  //ggx1[3]->Fill(EmcE,x1);
	  //ggx2[0]->Fill(j->Pt(),x2);
	  //ggx2[1]->Fill(j->Eta(),x2);
	  //ggx2[2]->Fill(j->Phi()*degr,x2);
	  //ggx2[3]->Fill(EmcE,x2);
	  //ggcosth[0]->Fill(j->Pt(),cos_th);
	  //ggcosth[1]->Fill(j->Eta(),cos_th);
	  //ggcosth[2]->Fill(j->Phi()*degr,cos_th);
	  //ggcosth[3]->Fill(EmcE,cos_th);
	}
      }
      
      if ((HTtrig==1)&&(bbc==1)) {
	HTpartpT->Fill(hard_p);
	HTjpTA[0]->Fill(j->Pt());
	HTjpTA[1]->Fill(j->Eta());
	HTjpTA[2]->Fill(j->Phi()*degr);
	HTjpTA[3]->Fill(EmcE);
	if ((j->Et()>=5)&&(j->Et()<7)) HTjpTA[4]->Fill(EmcE);
	if ((j->Et()>=7)&&(j->Et()<10)) HTjpTA[5]->Fill(EmcE);
	if ((j->Et()>=10)&&(j->Et()<15)) HTjpTA[6]->Fill(EmcE);
	if (j->Et()>=15) HTjpTA[7]->Fill(EmcE);
	//HThard[0]->Fill(j->Pt(),hard_p);
	//HTs[0]->Fill(j->Pt(),ss);
	//HTt[0]->Fill(j->Pt(),tt);
	///HTu[0]->Fill(j->Pt(),uu);
	//HTx1[0]->Fill(j->Pt(),x1);
	//HTx1[1]->Fill(j->Eta(),x1);
	//HTx1[2]->Fill(j->Phi()*degr,x1);
	//HTx1[3]->Fill(EmcE,x1);
	//HTx2[0]->Fill(j->Pt(),x2);
	//HTx2[1]->Fill(j->Eta(),x2);
	//HTx2[2]->Fill(j->Phi()*degr,x2);
	//HTx2[3]->Fill(EmcE,x2);
	//HTcosth[0]->Fill(j->Pt(),cos_th);
	//HTcosth[1]->Fill(j->Eta(),cos_th);
	//HTcosth[2]->Fill(j->Phi()*degr,cos_th);
	//HTcosth[3]->Fill(EmcE,cos_th);
	//for (int xx=0;xx<10;xx++) HTshapeTow->Fill((xx+1)*0.05,SumTow[xx]);
	//for (int xx=0;xx<10;xx++) HTshapeTrk->Fill((xx+1)*0.05,SumTrk[xx]);
	if (pid==11){
	  qqHTjpTA[0]->Fill(j->Pt());
	  qqHTjpTA[1]->Fill(j->Eta());
	  qqHTjpTA[2]->Fill(j->Phi()*degr);
	  qqHTjpTA[3]->Fill(EmcE);
	  //qqHThard[0]->Fill(j->Pt(),hard_p);
	  //qqHTs[0]->Fill(j->Pt(),ss);
	  //qqHTt[0]->Fill(j->Pt(),tt);
	  //qqHTu[0]->Fill(j->Pt(),uu);
	  //qqHTx1[0]->Fill(j->Pt(),x1);
	  //qqHTx1[1]->Fill(j->Eta(),x1);
	  //qqHTx1[2]->Fill(j->Phi()*degr,x1);
	  //qqHTx1[3]->Fill(EmcE,x1);
	  //qqHTx2[0]->Fill(j->Pt(),x2);
	  //qqHTx2[1]->Fill(j->Eta(),x2);
	  //qqHTx2[2]->Fill(j->Phi()*degr,x2);
	  //qqHTx2[3]->Fill(EmcE,x2);
	  //qqHTcosth[0]->Fill(j->Pt(),cos_th);
	  //qqHTcosth[1]->Fill(j->Eta(),cos_th);
	  //qqHTcosth[2]->Fill(j->Phi()*degr,cos_th);
	  //qqHTcosth[3]->Fill(EmcE,cos_th);
	  //for (int xx=0;xx<10;xx++) qqHTshapeTow->Fill((xx+1)*0.05,SumTow[xx]);
	  //for (int xx=0;xx<10;xx++) qqHTshapeTrk->Fill((xx+1)*0.05,SumTrk[xx]);
	}
	if (pid==28){
	  qgHTjpTA[0]->Fill(j->Pt());
	  qgHTjpTA[1]->Fill(j->Eta());
	  qgHTjpTA[2]->Fill(j->Phi()*degr);
	  qgHTjpTA[3]->Fill(EmcE);
	  //qgHThard[0]->Fill(j->Pt(),hard_p);
	  //qgHTs[0]->Fill(j->Pt(),ss);
	  //qgHTt[0]->Fill(j->Pt(),tt);
	  //qgHTu[0]->Fill(j->Pt(),uu);
	  //qgHTx1[0]->Fill(j->Pt(),x1);
	  //qgHTx1[1]->Fill(j->Eta(),x1);
	  //qgHTx1[2]->Fill(j->Phi()*degr,x1);
	  //qgHTx1[3]->Fill(EmcE,x1);
	  //qgHTx2[0]->Fill(j->Pt(),x2);
	  //qgHTx2[1]->Fill(j->Eta(),x2);
	  // qgHTx2[2]->Fill(j->Phi()*degr,x2);
	  //qgHTx2[3]->Fill(EmcE,x2);
	  //qgHTcosth[0]->Fill(j->Pt(),cos_th);
	  //qgHTcosth[1]->Fill(j->Eta(),cos_th);
	  //qgHTcosth[2]->Fill(j->Phi()*degr,cos_th);
	  //qgHTcosth[3]->Fill(EmcE,cos_th);
	  //for (int xx=0;xx<10;xx++) qgHTshapeTow->Fill((xx+1)*0.05,SumTow[xx]);
	  //for (int xx=0;xx<10;xx++) qgHTshapeTrk->Fill((xx+1)*0.05,SumTrk[xx]);
	}
	if ((pid==53)||(pid==68)){
	  ggHTjpTA[0]->Fill(j->Pt());
	  ggHTjpTA[1]->Fill(j->Eta());
	  ggHTjpTA[2]->Fill(j->Phi()*degr);
	  ggHTjpTA[3]->Fill(EmcE);
	  //ggHThard[0]->Fill(j->Pt(),hard_p);
	  //ggHTs[0]->Fill(j->Pt(),ss);
	  //ggHTt[0]->Fill(j->Pt(),tt);
	  //ggHTu[0]->Fill(j->Pt(),uu);
	  //ggHTx1[0]->Fill(j->Pt(),x1);
	  //ggHTx1[1]->Fill(j->Eta(),x1);
	  //ggHTx1[2]->Fill(j->Phi()*degr,x1);
	  //ggHTx1[3]->Fill(EmcE,x1);
	  //ggHTx2[0]->Fill(j->Pt(),x2);
	  //ggHTx2[1]->Fill(j->Eta(),x2);
	  //ggHTx2[2]->Fill(j->Phi()*degr,x2);
	  //ggHTx2[3]->Fill(EmcE,x2);
	  //ggHTcosth[0]->Fill(j->Pt(),cos_th);
	  //ggHTcosth[1]->Fill(j->Eta(),cos_th);
	  //ggHTcosth[2]->Fill(j->Phi()*degr,cos_th);
	  //ggHTcosth[3]->Fill(EmcE,cos_th);
	  //for (int xx=0;xx<10;xx++) ggHTshapeTow->Fill((xx+1)*0.05,SumTow[xx]);
	  //for (int xx=0;xx<10;xx++) ggHTshapeTrk->Fill((xx+1)*0.05,SumTrk[xx]);
	}
      }


      if ((JPtrig==1)&&(bbc==1)) {
	JPpartpT->Fill(hard_p);
	JPjpTA[0]->Fill(j->Pt());
	JPjpTA[1]->Fill(j->Eta());
	JPjpTA[2]->Fill(j->Phi()*degr);
	JPjpTA[3]->Fill(EmcE);	
	//JPhard[0]->Fill(j->Pt(),hard_p);
	//JPs[0]->Fill(j->Pt(),ss);
	//JPt[0]->Fill(j->Pt(),tt);
	//JPu[0]->Fill(j->Pt(),uu);
	//JPx1[0]->Fill(j->Pt(),x1);
	//JPx1[1]->Fill(j->Eta(),x1);
	//JPx1[2]->Fill(j->Phi()*degr,x1);
	//JPx1[3]->Fill(EmcE,x1);
	//JPx2[0]->Fill(j->Pt(),x2);
	//JPx2[1]->Fill(j->Eta(),x2);
	//JPx2[2]->Fill(j->Phi()*degr,x2);
	//JPx2[3]->Fill(EmcE,x2);
	//JPcosth[0]->Fill(j->Pt(),cos_th);
	//JPcosth[1]->Fill(j->Eta(),cos_th);
	//JPcosth[2]->Fill(j->Phi()*degr,cos_th);
	//JPcosth[3]->Fill(EmcE,cos_th);
	if (pid==11){
	  qqJPjpTA[0]->Fill(j->Pt());
	  qqJPjpTA[1]->Fill(j->Eta());
	  qqJPjpTA[2]->Fill(j->Phi()*degr);
	  qqJPjpTA[3]->Fill(EmcE);	
	  //qqJPhard[0]->Fill(j->Pt(),hard_p);
	  //qqJPs[0]->Fill(j->Pt(),ss);
	  //qqJPt[0]->Fill(j->Pt(),tt);
	  //qqJPu[0]->Fill(j->Pt(),uu);
	  //qqJPx1[0]->Fill(j->Pt(),x1);
	  //qqJPx1[1]->Fill(j->Eta(),x1);
	  //qqJPx1[2]->Fill(j->Phi()*degr,x1);
	  //qqJPx1[3]->Fill(EmcE,x1);
	  //qqJPx2[0]->Fill(j->Pt(),x2);
	  //qqJPx2[1]->Fill(j->Eta(),x2);
	  //qqJPx2[2]->Fill(j->Phi()*degr,x2);
	  //qqJPx2[3]->Fill(EmcE,x2);
	  //qqJPcosth[0]->Fill(j->Pt(),cos_th);
	  //qqJPcosth[1]->Fill(j->Eta(),cos_th);
	  //qqJPcosth[2]->Fill(j->Phi()*degr,cos_th);
	  //qqJPcosth[3]->Fill(EmcE,cos_th);
	}
	if (pid==28){
	  qgJPjpTA[0]->Fill(j->Pt());
	  qgJPjpTA[1]->Fill(j->Eta());
	  qgJPjpTA[2]->Fill(j->Phi()*degr);
	  qgJPjpTA[3]->Fill(EmcE);	
	  //qgJPhard[0]->Fill(j->Pt(),hard_p);
	  //qgJPs[0]->Fill(j->Pt(),ss);
	  //qgJPt[0]->Fill(j->Pt(),tt);
	  //qgJPu[0]->Fill(j->Pt(),uu);
	  //qgJPx1[0]->Fill(j->Pt(),x1);
	  //qgJPx1[1]->Fill(j->Eta(),x1);
	  //qgJPx1[2]->Fill(j->Phi()*degr,x1);
	  //qgJPx1[3]->Fill(EmcE,x1);
	  //qgJPx2[0]->Fill(j->Pt(),x2);
	  //qgJPx2[1]->Fill(j->Eta(),x2);
	  //qgJPx2[2]->Fill(j->Phi()*degr,x2);
	  //qgJPx2[3]->Fill(EmcE,x2);
	  //qgJPcosth[0]->Fill(j->Pt(),cos_th);
	  //qgJPcosth[1]->Fill(j->Eta(),cos_th);
	  //qgJPcosth[2]->Fill(j->Phi()*degr,cos_th);
	  //qgJPcosth[3]->Fill(EmcE,cos_th);
	}
	if ((pid==53)||(pid==68)){
	  ggJPjpTA[0]->Fill(j->Pt());
	  ggJPjpTA[1]->Fill(j->Eta());
	  ggJPjpTA[2]->Fill(j->Phi()*degr);
	  ggJPjpTA[3]->Fill(EmcE);	
	  //ggJPhard[0]->Fill(j->Pt(),hard_p);
	  //ggJPs[0]->Fill(j->Pt(),ss);
	  //ggJPt[0]->Fill(j->Pt(),tt);
	  //ggJPu[0]->Fill(j->Pt(),uu);
	  //ggJPx1[0]->Fill(j->Pt(),x1);
	  //ggJPx1[1]->Fill(j->Eta(),x1);
	  //ggJPx1[2]->Fill(j->Phi()*degr,x1);
	  //ggJPx1[3]->Fill(EmcE,x1);
	  //ggJPx2[0]->Fill(j->Pt(),x2);
	  //ggJPx2[1]->Fill(j->Eta(),x2);
	  //ggJPx2[2]->Fill(j->Phi()*degr,x2);
	  //ggJPx2[3]->Fill(EmcE,x2);
	  //ggJPcosth[0]->Fill(j->Pt(),cos_th);
	  //ggJPcosth[1]->Fill(j->Eta(),cos_th);
	  //ggJPcosth[2]->Fill(j->Phi()*degr,cos_th);
	  //ggJPcosth[3]->Fill(EmcE,cos_th);
	}
      }

      if ((JPtrig==1)&&(HTtrig==1)&&(bbc==1)){  
	jpTB[0]->Fill(j->Pt());
	jpTB[1]->Fill(j->Eta());
	jpTB[2]->Fill(j->Phi()*degr);
	jpTB[3]->Fill(EmcE);
	if (pid==11){
	  qqjpTB[0]->Fill(j->Pt());
	  qqjpTB[1]->Fill(j->Eta());
	  qqjpTB[2]->Fill(j->Phi()*degr);
	  qqjpTB[3]->Fill(EmcE);
	}
	if (pid==28){
	  qgjpTB[0]->Fill(j->Pt());
	  qgjpTB[1]->Fill(j->Eta());
	  qgjpTB[2]->Fill(j->Phi()*degr);
	  qgjpTB[3]->Fill(EmcE);
	}
	if ((pid==53)||(pid==68)){
	  ggjpTB[0]->Fill(j->Pt());
	  ggjpTB[1]->Fill(j->Eta());
	  ggjpTB[2]->Fill(j->Phi()*degr);
	  ggjpTB[3]->Fill(EmcE);
	}
      }
      




      //trigger bias for 2004 simu
      /*cond=getCond(hard_p);
	cout <<"First Cond = "<<cond<<endl;
	cout <<"PID = "<<pid<<endl;
	if (cond=='A'){
	jpTA[0]->Fill(j->Pt());
	jpTA[1]->Fill(j->Eta());
	jpTA[2]->Fill(j->Phi()*degr);
	jpTA[3]->Fill(EmcE);
	hx1[0]->Fill(j->Pt(),x1);
	hx2[0]->Fill(j->Pt(),x2);
	costh[0]->Fill(j->Pt(),cos_th);
	if (pid==11){
	qqjpTA[0]->Fill(j->Pt());
	qqjpTA[1]->Fill(j->Eta());
	qqjpTA[2]->Fill(j->Phi()*degr);
	qqjpTA[3]->Fill(EmcE);
	qqx1[0]->Fill(j->Pt(),x1);
	qqx2[0]->Fill(j->Pt(),x2);
	qqcosth[0]->Fill(j->Pt(),cos_th);
	}
	  if (pid==28){
	  qgjpTA[0]->Fill(j->Pt());
	  qgjpTA[1]->Fill(j->Eta());
	  qgjpTA[2]->Fill(j->Phi()*degr);
	  qgjpTA[3]->Fill(EmcE);
	  qgx1[0]->Fill(j->Pt(),x1);
	  qgx2[0]->Fill(j->Pt(),x2);
	  qgcosth[0]->Fill(j->Pt(),cos_th);
	  }
	if ((pid==53)||(pid==68)){
	  ggjpTA[0]->Fill(j->Pt());
	  ggjpTA[1]->Fill(j->Eta());
	  ggjpTA[2]->Fill(j->Phi()*degr);
	  ggjpTA[3]->Fill(EmcE);
	  ggx1[0]->Fill(j->Pt(),x1);
	  ggx2[0]->Fill(j->Pt(),x2);
	  ggcosth[0]->Fill(j->Pt(),cos_th);
	}
      }
      if (cond=='B'){
	jpTB[0]->Fill(j->Pt());
	jpTB[1]->Fill(j->Eta());
	jpTB[2]->Fill(j->Phi()*degr);
	jpTB[3]->Fill(EmcE);
	hx1[1]->Fill(j->Pt(),x1);
	hx2[1]->Fill(j->Pt(),x2);
	costh[1]->Fill(j->Pt(),cos_th);
	if (pid==11){
	  qqjpTB[0]->Fill(j->Pt());
	  qqjpTB[1]->Fill(j->Eta());
	  qqjpTB[2]->Fill(j->Phi()*degr);
	  qqjpTB[3]->Fill(EmcE);
	  qqx1[1]->Fill(j->Pt(),x1);
	  qqx2[1]->Fill(j->Pt(),x2);
	  qqcosth[1]->Fill(j->Pt(),cos_th);
	}
	if (pid==28){
	  qgjpTB[0]->Fill(j->Pt());
	  qgjpTB[1]->Fill(j->Eta());
	  qgjpTB[2]->Fill(j->Phi()*degr);
	  qgjpTB[3]->Fill(EmcE);
	  qgx1[1]->Fill(j->Pt(),x1);
	  qgx2[1]->Fill(j->Pt(),x2);
	  qgcosth[1]->Fill(j->Pt(),cos_th);
	}
	if ((pid==53)||(pid==68)){
	  ggjpTB[0]->Fill(j->Pt());
	  ggjpTB[1]->Fill(j->Eta());
	  ggjpTB[2]->Fill(j->Phi()*degr);
	  ggjpTB[3]->Fill(EmcE);
	  ggx1[1]->Fill(j->Pt(),x1);
	  ggx2[1]->Fill(j->Pt(),x2);
	  ggcosth[1]->Fill(j->Pt(),cos_th);
	}
      }
      if (cond=='C'){
	jpTC[0]->Fill(j->Pt());
	jpTC[1]->Fill(j->Eta());
	jpTC[2]->Fill(j->Phi()*degr);
	jpTC[3]->Fill(EmcE);
	hx1[2]->Fill(j->Pt(),x1);
	hx2[2]->Fill(j->Pt(),x2);
	costh[2]->Fill(j->Pt(),cos_th);
	if (pid==11){
	  qqjpTC[0]->Fill(j->Pt());
	  qqjpTC[1]->Fill(j->Eta());
	  qqjpTC[2]->Fill(j->Phi()*degr);
	  qqjpTC[3]->Fill(EmcE);
	  qqx1[2]->Fill(j->Pt(),x1);
	  qqx2[2]->Fill(j->Pt(),x2);
	  qqcosth[2]->Fill(j->Pt(),cos_th);
	}
	if (pid==28){
	  qgjpTC[0]->Fill(j->Pt());
	  qgjpTC[1]->Fill(j->Eta());
	  qgjpTC[2]->Fill(j->Phi()*degr);
	  qgjpTC[3]->Fill(EmcE);
	  qgx1[2]->Fill(j->Pt(),x1);
	  qgx2[2]->Fill(j->Pt(),x2);
	  qgcosth[2]->Fill(j->Pt(),cos_th);
	}
	if ((pid==53)||(pid==68)){
	  ggjpTC[0]->Fill(j->Pt());
	  ggjpTC[1]->Fill(j->Eta());
	  ggjpTC[2]->Fill(j->Phi()*degr);
	  ggjpTC[3]->Fill(EmcE);
	  ggx1[2]->Fill(j->Pt(),x1);
	  ggx2[2]->Fill(j->Pt(),x2);
	  ggcosth[2]->Fill(j->Pt(),cos_th);
	}
      }
      if (cond=='D'){
	jpTD[0]->Fill(j->Pt());
	jpTD[1]->Fill(j->Eta());
	jpTD[2]->Fill(j->Phi()*degr);
	jpTD[3]->Fill(EmcE);
	hx1[3]->Fill(j->Pt(),x1);
	hx2[3]->Fill(j->Pt(),x2);
	costh[3]->Fill(j->Pt(),cos_th);
	if (pid==11){
	  qqjpTD[0]->Fill(j->Pt());
	  qqjpTD[1]->Fill(j->Eta());
	  qqjpTD[2]->Fill(j->Phi()*degr);
	  qqjpTD[3]->Fill(EmcE);
	  qqx1[3]->Fill(j->Pt(),x1);
	  qqx2[3]->Fill(j->Pt(),x2);
	  qqcosth[3]->Fill(j->Pt(),cos_th);
	}
	if (pid==28){
	  qgjpTD[0]->Fill(j->Pt());
	  qgjpTD[1]->Fill(j->Eta());
	  qgjpTD[2]->Fill(j->Phi()*degr);
	  qgjpTD[3]->Fill(EmcE);
	  qgx1[3]->Fill(j->Pt(),x1);
	  qgx2[3]->Fill(j->Pt(),x2);
	  qgcosth[3]->Fill(j->Pt(),cos_th);
	}
	if ((pid==53)||(pid==68)){
	  ggjpTD[0]->Fill(j->Pt());
	  ggjpTD[1]->Fill(j->Eta());
	  ggjpTD[2]->Fill(j->Phi()*degr);
	  ggjpTD[3]->Fill(EmcE);
	  ggx1[3]->Fill(j->Pt(),x1);
	  ggx2[3]->Fill(j->Pt(),x2);
	  ggcosth[3]->Fill(j->Pt(),cos_th);
	}
      }	

      if (HTtrig!=1) continue;
      if (cond=='A'){
	HTjpTA[0]->Fill(j->Pt());
	HTjpTA[1]->Fill(j->Eta());
	HTjpTA[2]->Fill(j->Phi()*degr);
	HTjpTA[3]->Fill(EmcE);
	HTx1[0]->Fill(j->Pt(),x1);
	HTx2[0]->Fill(j->Pt(),x2);
	HTcosth[0]->Fill(j->Pt(),cos_th);
	if (pid==11){
	  qqHTjpTA[0]->Fill(j->Pt());
	  qqHTjpTA[1]->Fill(j->Eta());
	  qqHTjpTA[2]->Fill(j->Phi()*degr);
	  qqHTjpTA[3]->Fill(EmcE);
	  qqHTx1[0]->Fill(j->Pt(),x1);
	  qqHTx2[0]->Fill(j->Pt(),x2);
	  qqHTcosth[0]->Fill(j->Pt(),cos_th);
	}
	if (pid==28){
	  qgHTjpTA[0]->Fill(j->Pt());
	  qgHTjpTA[1]->Fill(j->Eta());
	  qgHTjpTA[2]->Fill(j->Phi()*degr);
	  qgHTjpTA[3]->Fill(EmcE);
	  qgHTx1[0]->Fill(j->Pt(),x1);
	  qgHTx2[0]->Fill(j->Pt(),x2);
	  qgHTcosth[0]->Fill(j->Pt(),cos_th);
	}
	if ((pid==53)||(pid==68)){
	  ggHTjpTA[0]->Fill(j->Pt());
	  ggHTjpTA[1]->Fill(j->Eta());
	  ggHTjpTA[2]->Fill(j->Phi()*degr);
	  ggHTjpTA[3]->Fill(EmcE);
	  ggHTx1[0]->Fill(j->Pt(),x1);
	  ggHTx2[0]->Fill(j->Pt(),x2);
	  ggHTcosth[0]->Fill(j->Pt(),cos_th);
	}
      }
      if (cond=='B'){
	HTjpTB[0]->Fill(j->Pt());
	HTjpTB[1]->Fill(j->Eta());
	HTjpTB[2]->Fill(j->Phi()*degr);
	HTjpTB[3]->Fill(EmcE);
	HTx1[1]->Fill(j->Pt(),x1);
	HTx2[1]->Fill(j->Pt(),x2);
	HTcosth[1]->Fill(j->Pt(),cos_th);
	if (pid==11){
	  qqHTjpTB[0]->Fill(j->Pt());
	  qqHTjpTB[1]->Fill(j->Eta());
	  qqHTjpTB[2]->Fill(j->Phi()*degr);
	  qqHTjpTB[3]->Fill(EmcE);
	  qqHTx1[1]->Fill(j->Pt(),x1);
	  qqHTx2[1]->Fill(j->Pt(),x2);
	  qqHTcosth[1]->Fill(j->Pt(),cos_th);
	}
	if (pid==28){
	  qgHTjpTB[0]->Fill(j->Pt());
	  qgHTjpTB[1]->Fill(j->Eta());
	  qgHTjpTB[2]->Fill(j->Phi()*degr);
	  qgHTjpTB[3]->Fill(EmcE);
	  qgHTx1[1]->Fill(j->Pt(),x1);
	  qgHTx2[1]->Fill(j->Pt(),x2);
	  qgHTcosth[1]->Fill(j->Pt(),cos_th);
	}
	if ((pid==53)||(pid==68)){
	  ggHTjpTB[0]->Fill(j->Pt());
	  ggHTjpTB[1]->Fill(j->Eta());
	  ggHTjpTB[2]->Fill(j->Phi()*degr);
	  ggHTjpTB[3]->Fill(EmcE);
	  ggHTx1[1]->Fill(j->Pt(),x1);
	  ggHTx2[1]->Fill(j->Pt(),x2);
	  ggHTcosth[1]->Fill(j->Pt(),cos_th);
	}
      }
      if (cond=='C'){
	HTjpTC[0]->Fill(j->Pt());
	HTjpTC[1]->Fill(j->Eta());
	HTjpTC[2]->Fill(j->Phi()*degr);
	HTjpTC[3]->Fill(EmcE);
	HTx1[2]->Fill(j->Pt(),x1);
	HTx2[2]->Fill(j->Pt(),x2);
	HTcosth[2]->Fill(j->Pt(),cos_th);
	if (pid==11){
	  qqHTjpTC[0]->Fill(j->Pt());
	  qqHTjpTC[1]->Fill(j->Eta());
	  qqHTjpTC[2]->Fill(j->Phi()*degr);
	  qqHTjpTC[3]->Fill(EmcE);
	  qqHTx1[2]->Fill(j->Pt(),x1);
	  qqHTx2[2]->Fill(j->Pt(),x2);
	  qqHTcosth[2]->Fill(j->Pt(),cos_th);
	}
	if (pid==28){
	  qgHTjpTC[0]->Fill(j->Pt());
	  qgHTjpTC[1]->Fill(j->Eta());
	  qgHTjpTC[2]->Fill(j->Phi()*degr);
	  qgHTjpTC[3]->Fill(EmcE);
	  qgHTx1[2]->Fill(j->Pt(),x1);
	  qgHTx2[2]->Fill(j->Pt(),x2);
	  qgHTcosth[2]->Fill(j->Pt(),cos_th);
	}
	if ((pid==53)||(pid==68)){
	  ggHTjpTC[0]->Fill(j->Pt());
	  ggHTjpTC[1]->Fill(j->Eta());
	  ggHTjpTC[2]->Fill(j->Phi()*degr);
	  ggHTjpTC[3]->Fill(EmcE);
	  ggHTx1[2]->Fill(j->Pt(),x1);
	  ggHTx2[2]->Fill(j->Pt(),x2);
	  ggHTcosth[2]->Fill(j->Pt(),cos_th);
	}
      }
      if (cond=='D'){
	HTjpTD[0]->Fill(j->Pt());
	HTjpTD[1]->Fill(j->Eta());
	HTjpTD[2]->Fill(j->Phi()*degr);
	HTjpTD[3]->Fill(EmcE);
	HTx1[3]->Fill(j->Pt(),x1);
	HTx2[3]->Fill(j->Pt(),x2);
	HTcosth[3]->Fill(j->Pt(),cos_th);
	if (pid==11){
	  qqHTjpTD[0]->Fill(j->Pt());
	  qqHTjpTD[1]->Fill(j->Eta());
	  qqHTjpTD[2]->Fill(j->Phi()*degr);
	  qqHTjpTD[3]->Fill(EmcE);
	  qqHTx1[3]->Fill(j->Pt(),x1);
	  qqHTx2[3]->Fill(j->Pt(),x2);
	  qqHTcosth[3]->Fill(j->Pt(),cos_th);
	}
	if (pid==28){
	  qgHTjpTD[0]->Fill(j->Pt());
	  qgHTjpTD[1]->Fill(j->Eta());
	  qgHTjpTD[2]->Fill(j->Phi()*degr);
	  qgHTjpTD[3]->Fill(EmcE);
	  qgHTx1[3]->Fill(j->Pt(),x1);
	  qgHTx2[3]->Fill(j->Pt(),x2);
	  qgHTcosth[3]->Fill(j->Pt(),cos_th);
	}
	if ((pid==53)||(pid==68)){
	  ggHTjpTD[0]->Fill(j->Pt());
	  ggHTjpTD[1]->Fill(j->Eta());
	  ggHTjpTD[2]->Fill(j->Phi()*degr);
	  ggHTjpTD[3]->Fill(EmcE);
	  ggHTx1[3]->Fill(j->Pt(),x1);
	  ggHTx2[3]->Fill(j->Pt(),x2);
	  ggHTcosth[3]->Fill(j->Pt(),cos_th);
	}
      }*/

    }
  }

 //PYTHIA jets
  for (JetBranchesMap::iterator it=mStJetsMap.begin(); it!=mStJetsMap.end(); ++it) {
    cout <<"Found\t"<<(*it).first<<endl;
    if ((*it).first!="PythiaConeR04") continue;
                                                                                                 
    StJets* stjets = (*it).second;
    int nJets = stjets->nJets();
    TClonesArray* jets = stjets->jets();
    cout <<"Found\t"<<nJets<<"\tjets from:\t"<<(*it).first<<endl;
    if (nJets==0) continue;
                                                                                                 
    for (int ijet=0;ijet<nJets;ijet++){//loop over jets
      StJet* j = static_cast<StJet*>( (*jets)[ijet] );
      assert(j);
      assert(verifyJet(stjets, ijet));                                                                                          
      //PYTHIA
      PYjpTA[0]->Fill(j->Pt());
      PYjpTA[1]->Fill(j->Eta());
      PYjpTA[2]->Fill(j->Phi()*degr);
      //PYjpTA[3]->Fill(EmcE);
      if (pid==11){
        qqPYjpTA[0]->Fill(j->Pt());
        qqPYjpTA[1]->Fill(j->Eta());
        qqPYjpTA[2]->Fill(j->Phi()*degr);
        //qqPYjpTA[3]->Fill(EmcE);
      }
      if (pid==28){
        qgPYjpTA[0]->Fill(j->Pt());
        qgPYjpTA[1]->Fill(j->Eta());
        qgPYjpTA[2]->Fill(j->Phi()*degr);
        //qgPYjpTA[3]->Fill(EmcE);
      }
      if ((pid==53)||(pid==68)){
        ggPYjpTA[0]->Fill(j->Pt());
        ggPYjpTA[1]->Fill(j->Eta());
        ggPYjpTA[2]->Fill(j->Phi()*degr);
        //ggPYjpTA[3]->Fill(EmcE);
      }
    }
  }
}

void StJetSimuReader::EventAna(){
   
 
  const  float degr=57.29577951; //180/pi
  stree->GetEntry(EveNum4);
  EveNum4++;

  pidH->Fill(pid);

  int HTtrig=0;
  if (Alex_ht_DSM>10) HTtrig=1;
  cout <<"Alex_ht_DSM="<<Alex_ht_DSM<<" HTtrg="<<HTtrig<<endl;
  StMuDst* muDst = 0;
  if (mDstMaker!=0) {
    muDst = mDstMaker->muDst();
  }

  //get vertex
  StMuEvent* muEvent = muDst->event();

  double verX=muEvent->primaryVertexPosition().x();
  double verY=muEvent->primaryVertexPosition().y();
  double verZ=muEvent->primaryVertexPosition().z();   

  //get event id from MuDst and test if it is the same as stored in SIMU tree
  StEventInfo &info=muEvent->eventInfo();
  int dstid=info.id();
  cout << "Event # = "<< info.id() << "evtID=  " << evtid <<endl;
  assert(verifySimu(dstid,evtid));
  
  double PyJetPt=0;
  int PnJets=0;
  if (verZ!=0.000){
    for (JetBranchesMap::iterator it=mStJetsMap.begin(); it!=mStJetsMap.end(); ++it) {
      if ((*it).first!="PythiaConeR04") continue;
      StJets* stjets = (*it).second;
      PnJets = stjets->nJets();
      TClonesArray* jets = stjets->jets();
      cout <<"Found\t"<<PnJets<<"\tjets from:\t"<<(*it).first<<endl;
      
      for (int ijet=0;ijet<PnJets;ijet++){
	StJet* j = static_cast<StJet*>( (*jets)[ijet] );
	assert(j);
	assert(verifyJet(stjets, ijet));	    
	float thetaJET=getTheta(j->Eta());
	float thetaEMC=getEmcTheta(verZ/100,thetaJET);
	float etaEMC=getEta(thetaEMC);
	if ((etaEMC<1)&&(etaEMC>0)) {
	  if (j->Pt()>PyJetPt)  PyJetPt=j->Pt();
	  cout<<"EtaEMC = "<<etaEMC<<" Pythia Pt ="<<PyJetPt<<endl;
	}
       }
    }
  }

  int nJets=0;
  if (verZ!=0.000){
    for (JetBranchesMap::iterator it=mStJetsMap.begin(); it!=mStJetsMap.end(); ++it) {
      if ((*it).first!="MkConeR04") continue;
      StJets* stjets = (*it).second;
      nJets = stjets->nJets();
      cout <<"Found\t"<<nJets<<"\tjets from:\t"<<(*it).first<<endl;
    }
  }

  trig->Fill(pid,0);
  if (bbc==1) trig->Fill(pid,1);
  if (HTtrig==1) trig->Fill(pid,2);
  if (nJets) trig->Fill(pid,3);
  if ((bbc==1)&&(HTtrig==1)) trig->Fill(pid,4);
  if ((bbc==1)&&(HTtrig==1)&&(nJets)) trig->Fill(pid,5);
  

  //if ((verZ!=0.000)&&(fabs(verZ)<60.0)){
  py[0]->Fill(hard_p);
  py[1]->Fill(x1);
  py[2]->Fill(x2);
  py[3]->Fill(ss);
  py[4]->Fill(tt);
  py[5]->Fill(uu);
  py[6]->Fill(cos_th);
  py[7]->Fill(Max_Part_Et);
  py[8]->Fill(Max_pT);
  py[9]->Fill(Max_eta);
  py[10]->Fill(Max_id);
  py[11]->Fill(Sum_Et);
  py[12]->Fill(Sum_pT);
  py[13]->Fill(mult);
  py[14]->Fill(Max_A_Et);
  py[15]->Fill(Max_A_pT);
  py[16]->Fill(Max_A_eta);
  py[17]->Fill(Max_A_id);
  py[18]->Fill(Max_C_Et);
  py[19]->Fill(Max_C_pT);
  py[20]->Fill(Max_C_eta);
  py[21]->Fill(Max_C_id);
  py[22]->Fill(Max_N_Et);
  py[23]->Fill(Max_N_pT);
  py[24]->Fill(Max_N_eta);
  py[25]->Fill(Max_N_id);
  py[26]->Fill(Max_P_Et);
  py[27]->Fill(Max_P_pT);
  py[28]->Fill(Max_P_eta);
  py[29]->Fill(Max_P_id);
  py[30]->Fill(PyJetPt);
  multEtSum[0]->Fill(mult,Sum_Et);
  
  if (bbc==1){
    
    pyB[0]->Fill(hard_p);
    pyB[1]->Fill(x1);
    pyB[2]->Fill(x2);
    pyB[3]->Fill(ss);
    pyB[4]->Fill(tt);
    pyB[5]->Fill(uu);
    pyB[6]->Fill(cos_th);
    pyB[7]->Fill(Max_Part_Et);
    pyB[8]->Fill(Max_pT);
    pyB[9]->Fill(Max_eta);
    pyB[10]->Fill(Max_id);
    pyB[11]->Fill(Sum_Et);
    pyB[12]->Fill(Sum_pT);
    pyB[13]->Fill(mult);
    pyB[14]->Fill(Max_A_Et);
    pyB[15]->Fill(Max_A_pT);
    pyB[16]->Fill(Max_A_eta);
    pyB[17]->Fill(Max_A_id);
    pyB[18]->Fill(Max_C_Et);
    pyB[19]->Fill(Max_C_pT);
    pyB[20]->Fill(Max_C_eta);
    pyB[21]->Fill(Max_C_id);
    pyB[22]->Fill(Max_N_Et);
    pyB[23]->Fill(Max_N_pT);
    pyB[24]->Fill(Max_N_eta);
    pyB[25]->Fill(Max_N_id);
    pyB[26]->Fill(Max_P_Et);
    pyB[27]->Fill(Max_P_pT);
    pyB[28]->Fill(Max_P_eta);
    pyB[29]->Fill(Max_P_id);
    pyB[30]->Fill(PyJetPt);
    multEtSum[1]->Fill(mult,Sum_Et);
  }
  
  
  if (HTtrig==1){
    
    pyH[0]->Fill(hard_p);
    pyH[1]->Fill(x1);
    pyH[2]->Fill(x2);
    pyH[3]->Fill(ss);
    pyH[4]->Fill(tt);
    pyH[5]->Fill(uu);
    pyH[6]->Fill(cos_th);
    pyH[7]->Fill(Max_Part_Et);
    pyH[8]->Fill(Max_pT);
    pyH[9]->Fill(Max_eta);
    pyH[10]->Fill(Max_id);
    pyH[11]->Fill(Sum_Et);
    pyH[12]->Fill(Sum_pT);
    pyH[13]->Fill(mult);
    pyH[14]->Fill(Max_A_Et);
    pyH[15]->Fill(Max_A_pT);
    pyH[16]->Fill(Max_A_eta);
    pyH[17]->Fill(Max_A_id);
    pyH[18]->Fill(Max_C_Et);
    pyH[19]->Fill(Max_C_pT);
    pyH[20]->Fill(Max_C_eta);
    pyH[21]->Fill(Max_C_id);
    pyH[22]->Fill(Max_N_Et);
    pyH[23]->Fill(Max_N_pT);
    pyH[24]->Fill(Max_N_eta);
    pyH[25]->Fill(Max_N_id);
    pyH[26]->Fill(Max_P_Et);
    pyH[27]->Fill(Max_P_pT);
    pyH[28]->Fill(Max_P_eta);
    pyH[29]->Fill(Max_P_id);
    pyH[30]->Fill(PyJetPt);
    multEtSum[2]->Fill(mult,Sum_Et);
  }
  
  
  if ((bbc==1)&&(HTtrig==1)){
    
    pyBH[0]->Fill(hard_p);
    pyBH[1]->Fill(x1);
    pyBH[2]->Fill(x2);
    pyBH[3]->Fill(ss);
    pyBH[4]->Fill(tt);
    pyBH[5]->Fill(uu);
    pyBH[6]->Fill(cos_th);
    pyBH[7]->Fill(Max_Part_Et);
    pyBH[8]->Fill(Max_pT);
    pyBH[9]->Fill(Max_eta);
    pyBH[10]->Fill(Max_id);
    pyBH[11]->Fill(Sum_Et);
    pyBH[12]->Fill(Sum_pT);
    pyBH[13]->Fill(mult);
    pyBH[14]->Fill(Max_A_Et);
    pyBH[15]->Fill(Max_A_pT);
    pyBH[16]->Fill(Max_A_eta);
    pyBH[17]->Fill(Max_A_id);
    pyBH[18]->Fill(Max_C_Et);
    pyBH[19]->Fill(Max_C_pT);
    pyBH[20]->Fill(Max_C_eta);
    pyBH[21]->Fill(Max_C_id);
    pyBH[22]->Fill(Max_N_Et);
    pyBH[23]->Fill(Max_N_pT);
    pyBH[24]->Fill(Max_N_eta);
    pyBH[25]->Fill(Max_N_id);
    pyBH[26]->Fill(Max_P_Et);
    pyBH[27]->Fill(Max_P_pT);
    pyBH[28]->Fill(Max_P_eta);
    pyBH[29]->Fill(Max_P_id);
    pyBH[30]->Fill(PyJetPt);
    multEtSum[3]->Fill(mult,Sum_Et);
    
    if (nJets){
      pyBHJ[0]->Fill(hard_p);
      pyBHJ[1]->Fill(x1);
      pyBHJ[2]->Fill(x2);
      pyBHJ[3]->Fill(ss);
      pyBHJ[4]->Fill(tt);
      pyBHJ[5]->Fill(uu);
      pyBHJ[6]->Fill(cos_th);
      pyBHJ[7]->Fill(Max_Part_Et);
      pyBHJ[8]->Fill(Max_pT);
      pyBHJ[9]->Fill(Max_eta);
      pyBHJ[10]->Fill(Max_id);
      pyBHJ[11]->Fill(Sum_Et);
      pyBHJ[12]->Fill(Sum_pT);
      pyBHJ[13]->Fill(mult);
      pyBHJ[14]->Fill(Max_A_Et);
      pyBHJ[15]->Fill(Max_A_pT);
      pyBHJ[16]->Fill(Max_A_eta);
      pyBHJ[17]->Fill(Max_A_id);
      pyBHJ[18]->Fill(Max_C_Et);
      pyBHJ[19]->Fill(Max_C_pT);
      pyBHJ[20]->Fill(Max_C_eta);
      pyBHJ[21]->Fill(Max_C_id);
      pyBHJ[22]->Fill(Max_N_Et);
      pyBHJ[23]->Fill(Max_N_pT);
      pyBHJ[24]->Fill(Max_N_eta);
      pyBHJ[25]->Fill(Max_N_id);
      pyBHJ[26]->Fill(Max_P_Et);
      pyBHJ[27]->Fill(Max_P_pT);
      pyBHJ[28]->Fill(Max_P_eta);
      pyBHJ[29]->Fill(Max_P_id);
      pyBHJ[30]->Fill(PyJetPt);
      multEtSum[4]->Fill(mult,Sum_Et);
    }
  }
  // }
  
  if ((HTtrig==1)&&(bbc==1)) {
    nTrk1=0;
    nTrk2=0;
    nTrk3=0;
    nTrk4=0;
    int nTracks=mDstMaker->muDst()->numberOfPrimaryTracks();
    tk[8]->Fill(nTracks);
    
    for (int i=0;i<nTracks;i++){
      
      primTrack=mDstMaker->muDst()->primaryTracks(i);
      float ptTrack=primTrack->pt();
      float etaTrack=primTrack->eta();
      float phiTrack=primTrack->phi();
      int qTrack=primTrack->charge();
      int nHits=primTrack->nHits();
      
      if (primTrack->flag()<1) continue;
      if (primTrack->topologyMap().trackFtpcEast()==true || primTrack->topologyMap().trackFtpcWest()==true) continue;
      tk[0]->Fill(ptTrack);
      tk[1]->Fill(etaTrack);
      tk[2]->Fill(phiTrack*degr);
      tk[3]->Fill(qTrack);
      tk[4]->Fill(nHits);
      tk[5]->Fill(verZ);
      tk[6]->Fill(verX);
      tk[7]->Fill(verY);
      if ((ptTrack<0.5)&&(ptTrack>=0.2)){
	tkA[0]->Fill(ptTrack);
	tkA[1]->Fill(etaTrack);
	tkA[2]->Fill(phiTrack*degr);
	tkA[3]->Fill(qTrack);
	tkA[4]->Fill(nHits);
	tkA[5]->Fill(verZ);
	tkA[6]->Fill(verX);
	tkA[7]->Fill(verY);
      }
      if ((ptTrack<1.0)&&(ptTrack>=0.5)){
	tkB[0]->Fill(ptTrack);
	tkB[1]->Fill(etaTrack);
	tkB[2]->Fill(phiTrack*degr);
	tkB[3]->Fill(qTrack);
	tkB[4]->Fill(nHits);
	tkB[5]->Fill(verZ);
	tkB[6]->Fill(verX);
	tkB[7]->Fill(verY);
      }
      if ((ptTrack<2.0)&&(ptTrack>=1.0)){
	tkC[0]->Fill(ptTrack);
	tkC[1]->Fill(etaTrack);
	tkC[2]->Fill(phiTrack*degr);
	tkC[3]->Fill(qTrack);
	tkC[4]->Fill(nHits);
	tkC[5]->Fill(verZ);
	tkC[6]->Fill(verX);
	tkC[7]->Fill(verY);
      }
      if (ptTrack>=2.0){
	tkD[0]->Fill(ptTrack);
	tkD[1]->Fill(etaTrack);
	tkD[2]->Fill(phiTrack*degr);
	tkD[3]->Fill(qTrack);
	tkD[4]->Fill(nHits);
	tkD[5]->Fill(verZ);
	tkD[6]->Fill(verX);
	tkD[7]->Fill(verY);
      }
      
      if (nHits<15) continue;
      if ((ptTrack<20.0)&&(ptTrack>0.2)) {
	
	if (fabs(etaTrack )<0.5) {
	  nTrk1++;
	}
	if ((fabs(etaTrack)<1.0)&&(fabs(etaTrack)>0.5)) {
	  nTrk2++;
	}
	if ((fabs(etaTrack)<1.5)&&(fabs(etaTrack)>1.0)) {
	  nTrk3++;
	}
	if (fabs(etaTrack)>1.5) {
	  nTrk4++;
	}
	
      }
    }
    tk[9]->Fill(nTrk1);
    tk[10]->Fill(nTrk2);
    tk[11]->Fill(nTrk3);
    tk[12]->Fill(nTrk4);
  }
}


void StJetSimuReader::AsymBias(){

  const  float degr=57.29577951; //180/pi
  double thetaJET;//jet Theta
  double etaEMC;//eta of Jet in EMC frame
  double thetaEMC;//theta in emc frame
  float nbins0[nbins+1]={0.0,5.0,6.15,7.5645,9.30434,
			 11.4443,14.0765,17.3141,21.2964,
			 26.1945,32.2193,39.6297,48.7446,59.9558};
  int jimflag=0;
  

  stree->GetEntry(EveNum6);
  EveNum6++;
  StMuDst* muDst = 0;
  if (mDstMaker!=0) {
    muDst = mDstMaker->muDst();
  }

  //Store Pythia jets in an array
  PythiaAna();

  //vertex
  StMuEvent* muEvent = muDst->event();
  double verX=muEvent->primaryVertexPosition().x();
  double verY=muEvent->primaryVertexPosition().y();
  double verZ=muEvent->primaryVertexPosition().z();

   //get event id from MuDst and test if it is the same as stored in SIMU tree
  StEventInfo &info=muEvent->eventInfo();
  int dstid=info.id();
  cout << "Event # = "<< info.id() << "evtID=  " << evtid <<endl;
  assert(verifySimu(dstid,evtid));
    
  //get trigger info
  int HTtrig=0;
  int JPtrig=0;
  int HTid=0;
  int JPid=0;
  HTtrig=HT1_2004;
  HTid=HT1_2004_Tow;
  JPtrig=JP1_2004;
  JPid=JP1_2004_Patch;

  Count[0]->Fill(HTtrig);
  Count[7]->Fill(bbc);
  Count[8]->Fill(JPtrig);
  Count[10]->Fill(HT1_2004_Tow);
  Count[11]->Fill(JP1_2004_Patch);
  Count[13]->Fill(HT1_2004_DSM);
  Count[14]->Fill(JP1_2004_DSM);
 
 //Require a vertex is found
  if ((verX==0.0)&&(verY==0.0)&&(verZ==0.0)) return;

  Count[1]->Fill(bbc);
  Count[2]->Fill(verZ);
  if (bbc==1) Count[3]->Fill(verZ);
  if ((bbc==1)&&(HTtrig==1))Count[16]->Fill(verZ);



  //Fill PYTHIA jets
  for (int p=0;p<10;p++){
    if (PpT[p]==0.0) continue;

    //PYTHIA
    //Counts
    PYjpTA[0]->Fill(PpT[p]);
    PYjpTA[1]->Fill(PEta[p]);
    PYjpTA[2]->Fill(PPhi[p]*degr);
    //B=weight*counts
    PYjpTB[0]->Fill(PpT[p],weight);
    PYjpTB[1]->Fill(PEta[p],weight);
    PYjpTB[2]->Fill(PPhi[p]*degr,weight);
    //C=weight^2*counts
    PYjpTC[0]->Fill(PpT[p],weight*weight);
    PYjpTC[1]->Fill(PEta[p],weight*weight);
    PYjpTC[2]->Fill(PPhi[p]*degr,weight*weight);      
    
    //find eta of Jet in STAR BEMC frame
    thetaJET=getTheta(PEta[p]);
    thetaEMC=getEmcTheta(verZ/100,thetaJET);
    etaEMC=getEta(thetaEMC);
    
    if (fabs(verZ)>60.0) continue;  
    if (etaEMC>0.8) continue;
    if (etaEMC<0.2) continue;	
    
    
    //PYTHIA+DET ETA cuts
    //Counts
    PYETAjpTA[0]->Fill(PpT[p]);
    PYETAjpTA[1]->Fill(PEta[p]);
    PYETAjpTA[1]->Fill(PPhi[p]*degr);
    //B=weight*counts
    PYETAjpTB[0]->Fill(PpT[p],weight);
    PYETAjpTB[1]->Fill(PEta[p],weight);
    PYETAjpTB[2]->Fill(PPhi[p]*degr,weight);
    //C=weight^2*counts
    PYETAjpTC[0]->Fill(PpT[p],weight*weight);
    PYETAjpTC[1]->Fill(PEta[p],weight*weight);
    PYETAjpTC[2]->Fill(PPhi[p]*degr,weight*weight);
    
  }

  for (JetBranchesMap::iterator it=mStJetsMap.begin(); it!=mStJetsMap.end(); ++it) {
    cout <<"Found\t"<<(*it).first<<endl;
    if ((*it).first!="MkConeR04") continue;

    StJets* stjets = (*it).second;
    int nJets = stjets->nJets();
    TClonesArray* jets = stjets->jets();
    cout <<"Found\t"<<nJets<<"\tjets from:\t"<<(*it).first<<endl;
    
    for (int ijet=0;ijet<nJets;ijet++){
      Count[4]->Fill(bbc);
      Count[5]->Fill(verZ);
      if (bbc==1) Count[6]->Fill(verZ);
    }

    if (nJets==0) continue;    
    for (int ijet=0;ijet<nJets;ijet++){//loop over jets
      
      StJet* j = static_cast<StJet*>( (*jets)[ijet] );
      assert(j);
      assert(verifyJet(stjets, ijet));	    
      
      //PYTHIA/detector Association
      float hold_dR=100.0;
      int PYindex=-1;
      for (int p=0;p<10;p++){
	if (PpT[p]==0.0) continue;
	float dPhi=j->Phi()-PPhi[p];
	float dEta=j->Eta()-PEta[p];
	float dR=sqrt(dPhi*dPhi + dEta*dEta);
	
	if (dR<hold_dR){ 
	  hold_dR=dR;
	  PYindex=p;
	  printf("Pythia pT, Eta, Phi, dR = %f,%f,%f,%f\n",PpT[p],PEta[p],PPhi[p],dR);
	}
      }
      dR->Fill(hold_dR);
      if (PYindex==-1) cout<<"NO ASSOCIATED JET!"<<endl;
      PYIN->Fill(PYindex);
     
      //look at 4-momenta in the jet:
      int trkCounter=0;
      int towCounter=0;
      Double_t trkpT=0;
      Double_t towE=0;
      Double_t trkDR=0.0;
      Double_t towDR=0.0;
      float EmcE=0;
      Double_t SumTrk[9],SumTow[9];
      for (int xx=0;xx<9;xx++){
	SumTrk[xx]=0;
	SumTow[xx]=0;
      }
      typedef vector<TrackToJetIndex*> TrackToJetVec;
      TrackToJetVec particles = stjets->particles(ijet);
      for (TrackToJetVec::iterator it=particles.begin(); it!=particles.end(); ++it) {
	TrackToJetIndex* t2j = (*it); 
	assert(t2j);
	if (idString(t2j)=="kTpcId") {
	  trkCounter++;
	  trkpT+=t2j->Pt();
	  trkDR=pow(pow(j->Eta()-t2j->Eta(),2)+pow(j->Phi()-t2j->Phi(),2),0.5);
	  if (trkDR<0.05) SumTrk[0]+=t2j->Pt();
	  if (trkDR<0.1) SumTrk[1]+=t2j->Pt();
	  if (trkDR<0.15) SumTrk[2]+=t2j->Pt();
	  if (trkDR<0.2) SumTrk[3]+=t2j->Pt();
	  if (trkDR<0.25) SumTrk[4]+=t2j->Pt();
	  if (trkDR<0.3) SumTrk[5]+=t2j->Pt();
	  if (trkDR<0.35) SumTrk[6]+=t2j->Pt();
	  if (trkDR<0.4) SumTrk[7]+=t2j->Pt();
	  if (trkDR>=0.4) SumTrk[8]+=t2j->Pt();
	}
	if (idString(t2j)=="kBarrelEmcTowerId") {
	  towCounter++;
	  towE+=t2j->Et();
	  EmcE=t2j->Et()+EmcE; 
	  towDR=pow(pow(j->Eta()-t2j->Eta(),2)+pow(j->Phi()-t2j->Phi(),2),0.5);
	  if (towDR<0.05) SumTow[0]+=t2j->Et();
	  if (towDR<0.1) SumTow[1]+=t2j->Et();
	  if (towDR<0.15) SumTow[2]+=t2j->Et();
	  if (towDR<0.2) SumTow[3]+=t2j->Et();
	  if (towDR<0.25) SumTow[4]+=t2j->Et();
	  if (towDR<0.3) SumTow[5]+=t2j->Et();
	  if (towDR<0.35) SumTow[6]+=t2j->Et();
	  if (towDR<0.4) SumTow[7]+=t2j->Et();
	  if (towDR>=0.4) SumTow[8]+=t2j->Et();
	}
      }
      EmcE=EmcE/j->Et();
      for (int xx=0;xx<10;xx++){
	SumTow[xx]/=j->Et();
	SumTrk[xx]/=j->Pt();
      }

      if ((HTtrig==1)&&(bbc==1)) {
	HTverNeu->Fill(EmcE,verZ);
	Count[17]->Fill(verZ);
      }

      //find eta of Jet in STAR BEMC frame
      thetaJET=getTheta(j->Eta());
      thetaEMC=getEmcTheta(verZ/100,thetaJET);
      etaEMC=getEta(thetaEMC);

      //Apply standard cuts
      if (fabs(verZ)>60.0) continue;  
      if (etaEMC>0.8) continue;
      if (etaEMC<0.2) continue;	
      if (EmcE>0.9) continue;

      //print outs for Jim
      if (j->Pt()>nbins0[4]){
	jimflag=1;
	printf("JIMPRINT EVTID=%d\n",evtid);
	printf("JIMPRINT DETpT =%f  DETeta=%f  DETphi=%f NUMtow=%d  SUMtow=%f  NUMtrks=%d SUMtrks=%f\n",j->Pt(),j->Eta(),j->Phi()*degr,towCounter,towE,trkCounter,trkpT);
	printf("JIMPRINT ASCPYpT=%f PYeta=%f PYphi=%f\n",PpT[PYindex],PEta[PYindex],PPhi[PYindex]*degr);
      }

      // FILL ASSOC. PYTHIA
      //Counts
      PYASCjpTA[0]->Fill(PpT[PYindex]);
      PYASCjpTA[1]->Fill(PEta[PYindex]);
      PYASCjpTA[2]->Fill(PPhi[PYindex]*degr);
      //B=weight*counts
      PYASCjpTB[0]->Fill(PpT[PYindex],weight);
      PYASCjpTB[1]->Fill(PEta[PYindex],weight);
      PYASCjpTB[2]->Fill(PPhi[PYindex]*degr,weight);
      //C=weight^2*counts
      PYASCjpTC[0]->Fill(PpT[PYindex],weight*weight);
      PYASCjpTC[1]->Fill(PEta[PYindex],weight*weight);
      PYASCjpTC[2]->Fill(PPhi[PYindex]*degr,weight*weight);

      x1H[0]->Fill(x1);
      x2H[0]->Fill(x2);
      Q2H[0]->Fill(Q2);
      df1H[0]->Fill(df1);
      df2H[0]->Fill(df2);
      f1H[0]->Fill(f1);
      f2H[0]->Fill(f2);
      aLLH[0]->Fill(partonic_all);
      if (pid==11){
	x1H[1]->Fill(x1);
	x2H[1]->Fill(x2);
	Q2H[1]->Fill(Q2);
	df1H[1]->Fill(df1);
	df2H[1]->Fill(df2);
	f1H[1]->Fill(f1);
	f2H[1]->Fill(f2);
	aLLH[1]->Fill(partonic_all);
      }
      if (pid==28){
	x1H[2]->Fill(x1);
	x2H[2]->Fill(x2);
	Q2H[2]->Fill(Q2);
	df1H[2]->Fill(df1);
	df2H[2]->Fill(df2);
	f1H[2]->Fill(f1);
	f2H[2]->Fill(f2);
	aLLH[2]->Fill(partonic_all);
      }
      if ((pid==53)||(pid==68)){
	x1H[3]->Fill(x1);
	x2H[3]->Fill(x2);
	Q2H[3]->Fill(Q2);
	df1H[3]->Fill(df1);
	df2H[3]->Fill(df2);
	f1H[3]->Fill(f1);
	f2H[3]->Fill(f2);
	aLLH[3]->Fill(partonic_all);
      }

      if (bbc==1){
	// FILL ASSOC. PYTHIA+BBC
	//Counts
	PYBBCjpTA[0]->Fill(PpT[PYindex]);
	PYBBCjpTA[1]->Fill(PEta[PYindex]);
	PYBBCjpTA[2]->Fill(PPhi[PYindex]*degr);
	//B=weight*counts
	PYBBCjpTB[0]->Fill(PpT[PYindex],weight);
	PYBBCjpTB[1]->Fill(PEta[PYindex],weight);
	PYBBCjpTB[2]->Fill(PPhi[PYindex]*degr,weight);
	//C=weight^2*counts
	PYBBCjpTC[0]->Fill(PpT[PYindex],weight*weight);
	PYBBCjpTC[1]->Fill(PEta[PYindex],weight*weight);
	PYBBCjpTC[2]->Fill(PPhi[PYindex]*degr,weight*weight);
      }

      if ((bbc==1)&&(HTtrig==1)){
      // FILL ASSOC. PYTHIA+BBC+HT
      //Counts
	PYHTjpTA[0]->Fill(PpT[PYindex]);
	PYHTjpTA[1]->Fill(PEta[PYindex]);
	PYHTjpTA[2]->Fill(PPhi[PYindex]*degr);
	//B=weight*counts
	PYHTjpTB[0]->Fill(PpT[PYindex],weight);
	PYHTjpTB[1]->Fill(PEta[PYindex],weight);
	PYHTjpTB[2]->Fill(PPhi[PYindex]*degr,weight);
	//C=weight^2*counts
	PYHTjpTC[0]->Fill(PpT[PYindex],weight*weight);
	PYHTjpTC[1]->Fill(PEta[PYindex],weight*weight);
	PYHTjpTC[2]->Fill(PPhi[PYindex]*degr,weight*weight);
      }
           
  
      //Identify ASSOC PY jet in DETECTOR pT bin
      int jetbin=-1;
      for (int b=0;b<nbins;b++){
	if ((j->Pt()<=nbins0[b+1])&&(j->Pt()>nbins0[b])) jetbin=b;
	if (j->Pt()>nbins0[nbins]) jetbin=nbins;
      }
      
      cout<<"PYTHIA pT="<<PpT[PYindex]<<" DET pT="<<j->Pt()<<"Bin="<<jetbin<<endl;
      if (jetbin==nbins){
	if (PpT[PYindex]>nbins0[nbins]){
	  cout<<"DIAG"<<endl;
	  //Counts
	  PYDIAjpTA[0]->Fill(PpT[PYindex]);
	  PYDIAjpTA[1]->Fill(PEta[PYindex]);
	  PYDIAjpTA[2]->Fill(PPhi[PYindex]*degr);
	  //B=weight*counts
	  PYDIAjpTB[0]->Fill(PpT[PYindex],weight);
	  PYDIAjpTB[1]->Fill(PEta[PYindex],weight);
	  PYDIAjpTB[2]->Fill(PPhi[PYindex]*degr,weight);
	  //C=weight^2*counts
	  PYDIAjpTC[0]->Fill(PpT[PYindex],weight*weight);
	  PYDIAjpTC[1]->Fill(PEta[PYindex],weight*weight);
	  PYDIAjpTC[2]->Fill(PPhi[PYindex]*degr,weight*weight);
	  
	  //Counts
	  DIAjpTA[0]->Fill(j->Pt());
	  DIAjpTA[1]->Fill(j->Eta());
	  DIAjpTA[2]->Fill(j->Phi()*degr);
	  //B=weight*counts
	  DIAjpTB[0]->Fill(j->Pt(),weight);
	  DIAjpTB[1]->Fill(j->Eta(),weight);
	  DIAjpTB[2]->Fill(j->Phi()*degr,weight);
	  //C=weight^2*counts
	  DIAjpTC[0]->Fill(j->Pt(),weight*weight);
	  DIAjpTC[1]->Fill(j->Eta(),weight*weight);
	  DIAjpTC[2]->Fill(j->Phi()*degr,weight*weight);
	}
      }

      if (jetbin!=nbins){
	if ((PpT[PYindex]<=nbins0[jetbin+1])&&(PpT[PYindex]>nbins0[jetbin])){
	  cout<<"DIAG"<<endl;
	  //Counts
	  PYDIAjpTA[0]->Fill(PpT[PYindex]);
	  PYDIAjpTA[1]->Fill(PEta[PYindex]);
	  PYDIAjpTA[2]->Fill(PPhi[PYindex]*degr);
	  //B=weight*counts
	  PYDIAjpTB[0]->Fill(PpT[PYindex],weight);
	  PYDIAjpTB[1]->Fill(PEta[PYindex],weight);
	  PYDIAjpTB[2]->Fill(PPhi[PYindex]*degr,weight);
	  //C=weight^2*counts
	  PYDIAjpTC[0]->Fill(PpT[PYindex],weight*weight);
	  PYDIAjpTC[1]->Fill(PEta[PYindex],weight*weight);
	  PYDIAjpTC[2]->Fill(PPhi[PYindex]*degr,weight*weight);
	  
	  //Counts
	  DIAjpTA[0]->Fill(j->Pt());
	  DIAjpTA[1]->Fill(j->Eta());
	  DIAjpTA[2]->Fill(j->Phi()*degr);
	  //B=weight*counts
	  DIAjpTB[0]->Fill(j->Pt(),weight);
	  DIAjpTB[1]->Fill(j->Eta(),weight);
	  DIAjpTB[2]->Fill(j->Phi()*degr,weight);
	  //C=weight^2*counts
	  DIAjpTC[0]->Fill(j->Pt(),weight*weight);
	  DIAjpTC[1]->Fill(j->Eta(),weight*weight);
	  DIAjpTC[2]->Fill(j->Phi()*degr,weight*weight);
	}
      }
	
      if (PpT[PYindex]<=nbins0[jetbin]){
	cout<<"LOW"<<endl;
	//Counts
	DWDIAjpTA[0]->Fill(j->Pt());
	DWDIAjpTA[1]->Fill(j->Eta());
	DWDIAjpTA[2]->Fill(j->Phi()*degr);
	//B=weight*counts
	DWDIAjpTB[0]->Fill(j->Pt(),weight);
	DWDIAjpTB[1]->Fill(j->Eta(),weight);
	DWDIAjpTB[2]->Fill(j->Phi()*degr,weight);
	//C=weight^2*counts
	DWDIAjpTC[0]->Fill(j->Pt(),weight*weight);
	DWDIAjpTC[1]->Fill(j->Eta(),weight*weight);
	DWDIAjpTC[2]->Fill(j->Phi()*degr,weight*weight);
      }
	
      if (jetbin!=nbins){
	if (PpT[PYindex]>nbins0[jetbin+1]){
	  cout<<"UPPER"<<endl;
	  //Counts
	  UPDIAjpTA[0]->Fill(j->Pt());
	  UPDIAjpTA[1]->Fill(j->Eta());
	  UPDIAjpTA[2]->Fill(j->Phi()*degr);
	  //B=weight*counts
	  UPDIAjpTB[0]->Fill(j->Pt(),weight);
	  UPDIAjpTB[1]->Fill(j->Eta(),weight);
	  UPDIAjpTB[2]->Fill(j->Phi()*degr,weight);
	  //C=weight^2*counts
	  UPDIAjpTC[0]->Fill(j->Pt(),weight*weight);
	  UPDIAjpTC[1]->Fill(j->Eta(),weight*weight);
	  UPDIAjpTC[2]->Fill(j->Phi()*degr,weight*weight);
	}
      }
    
      
   
      //DETECTOR JETS
      jet3D[0]->Fill(j->Pt(),PpT[PYindex],weight);
      jet3D[3]->Fill(j->Pt(),PpT[PYindex]);
      //Counts
      RAWjpTA[0]->Fill(j->Pt());
      RAWjpTA[1]->Fill(j->Eta());
      RAWjpTA[2]->Fill(j->Phi()*degr);
      RAWjpTA[3]->Fill(EmcE);
      //B=weight*counts
      RAWjpTB[0]->Fill(j->Pt(),weight);
      RAWjpTB[1]->Fill(j->Eta(),weight);
      RAWjpTB[2]->Fill(j->Phi()*degr,weight);
      RAWjpTB[3]->Fill(EmcE,weight);;
      //C=weight^2*counts
      RAWjpTC[0]->Fill(j->Pt(),weight*weight);
      RAWjpTC[1]->Fill(j->Eta(),weight*weight);
      RAWjpTC[2]->Fill(j->Phi()*degr,weight*weight);
      RAWjpTC[3]->Fill(EmcE,weight*weight);

      x1H[4]->Fill(x1);
      x2H[4]->Fill(x2);
      Q2H[4]->Fill(Q2);
      df1H[4]->Fill(df1);
      df2H[4]->Fill(df2);
      f1H[4]->Fill(f1);
      f2H[4]->Fill(f2);
      aLLH[4]->Fill(partonic_all);
      if (pid==11){
	x1H[5]->Fill(x1);
	x2H[5]->Fill(x2);
	Q2H[5]->Fill(Q2);
	df1H[5]->Fill(df1);
	df2H[5]->Fill(df2);
	f1H[5]->Fill(f1);
	f2H[5]->Fill(f2);
	aLLH[5]->Fill(partonic_all);
      }
      if (pid==28){
	x1H[6]->Fill(x1);
	x2H[6]->Fill(x2);
	Q2H[6]->Fill(Q2);
	df1H[6]->Fill(df1);
	df2H[6]->Fill(df2);
	f1H[6]->Fill(f1);
	f2H[6]->Fill(f2);
	aLLH[6]->Fill(partonic_all);
      }
      if ((pid==53)||(pid==68)){
	x1H[7]->Fill(x1);
	x2H[7]->Fill(x2);
	Q2H[7]->Fill(Q2);
	df1H[7]->Fill(df1);
	df2H[7]->Fill(df2);
	f1H[7]->Fill(f1);
	f2H[7]->Fill(f2);
	aLLH[7]->Fill(partonic_all);
      }      
      
      
      if (bbc==1){
	jet3D[1]->Fill(j->Pt(),PpT[PYindex],weight);
	jet3D[4]->Fill(j->Pt(),PpT[PYindex]);
	//A=counts
	partpT->Fill(hard_p);
	jpTA[0]->Fill(j->Pt());
	jpTA[1]->Fill(j->Eta());
	jpTA[2]->Fill(j->Phi()*degr);
	jpTA[3]->Fill(EmcE);
	//B=weight*counts
	jpTB[0]->Fill(j->Pt(),weight);
	jpTB[1]->Fill(j->Eta(),weight);
	jpTB[2]->Fill(j->Phi()*degr,weight);
	jpTB[3]->Fill(EmcE,weight);;
	//C=weight^2*counts
	jpTC[0]->Fill(j->Pt(),weight*weight);
	jpTC[1]->Fill(j->Eta(),weight*weight);
	jpTC[2]->Fill(j->Phi()*degr,weight*weight);
	jpTC[3]->Fill(EmcE,weight*weight);
   
	x1H[8]->Fill(x1);
	x2H[8]->Fill(x2);
	Q2H[8]->Fill(Q2);
	df1H[8]->Fill(df1);
	df2H[8]->Fill(df2);
	f1H[8]->Fill(f1);
	f2H[8]->Fill(f2);
	aLLH[8]->Fill(partonic_all);
	if (pid==11){
	  x1H[9]->Fill(x1);
	  x2H[9]->Fill(x2);
	  Q2H[9]->Fill(Q2);
	  df1H[9]->Fill(df1);
	  df2H[9]->Fill(df2);
	  f1H[9]->Fill(f1);
	  f2H[9]->Fill(f2);
	  aLLH[9]->Fill(partonic_all);
	}
	if (pid==28){
	  x1H[10]->Fill(x1);
	  x2H[10]->Fill(x2);
	  Q2H[10]->Fill(Q2);
	  df1H[10]->Fill(df1);
	  df2H[10]->Fill(df2);
	  f1H[10]->Fill(f1);
	  f2H[10]->Fill(f2);
	  aLLH[10]->Fill(partonic_all);
	}
	if ((pid==53)||(pid==68)){
	  x1H[11]->Fill(x1);
	  x2H[11]->Fill(x2);
	  Q2H[11]->Fill(Q2);
	  df1H[11]->Fill(df1);
	  df2H[11]->Fill(df2);
	  f1H[11]->Fill(f1);
	  f2H[11]->Fill(f2);
	  aLLH[11]->Fill(partonic_all);
	}
      }
      
      if ((HTtrig==1)&&(bbc==1)) {
	jet3D[2]->Fill(j->Pt(),PpT[PYindex],weight);
	jet3D[5]->Fill(j->Pt(),PpT[PYindex]);
	//A=counts
	HTpartpT->Fill(hard_p);
	HTjpTA[0]->Fill(j->Pt());
	HTjpTA[1]->Fill(j->Eta());
	HTjpTA[2]->Fill(j->Phi()*degr);
	HTjpTA[3]->Fill(EmcE);
	//B=weight*counts
	HTjpTB[0]->Fill(j->Pt(),weight);
	HTjpTB[1]->Fill(j->Eta(),weight);
	HTjpTB[2]->Fill(j->Phi()*degr,weight);
	HTjpTB[3]->Fill(EmcE,weight);;
	//C=weight^2*counts
	HTjpTC[0]->Fill(j->Pt(),weight*weight);
	HTjpTC[1]->Fill(j->Eta(),weight*weight);
	HTjpTC[2]->Fill(j->Phi()*degr,weight*weight);
	HTjpTC[3]->Fill(EmcE,weight*weight);
	
	x1H[12]->Fill(x1);
	x2H[12]->Fill(x2);
	Q2H[12]->Fill(Q2);
	df1H[12]->Fill(df1);
	df2H[12]->Fill(df2);
	f1H[12]->Fill(f1);
	f2H[12]->Fill(f2);
	aLLH[12]->Fill(partonic_all);
	if (pid==11){
	  x1H[13]->Fill(x1);
	  x2H[13]->Fill(x2);
	  Q2H[13]->Fill(Q2);
	  df1H[13]->Fill(df1);
	  df2H[13]->Fill(df2);
	  f1H[13]->Fill(f1);
	  f2H[13]->Fill(f2);
	  aLLH[13]->Fill(partonic_all);
	}
	if (pid==28){
	  x1H[14]->Fill(x1);
	  x2H[14]->Fill(x2);
	  Q2H[14]->Fill(Q2);
	  df1H[14]->Fill(df1);
	  df2H[14]->Fill(df2);
	  f1H[14]->Fill(f1);
	  f2H[14]->Fill(f2);
	  aLLH[14]->Fill(partonic_all);
	}
	if ((pid==53)||(pid==68)){
	  x1H[15]->Fill(x1);
	  x2H[15]->Fill(x2);
	  Q2H[15]->Fill(Q2);
	  df1H[15]->Fill(df1);
	  df2H[15]->Fill(df2);
	  f1H[15]->Fill(f1);
	  f2H[15]->Fill(f2);
	  aLLH[15]->Fill(partonic_all);
	}
      }
    }
  }
}


void StJetSimuReader::setSoftTrig(float thresh){
  HTETthres1=thresh;
  cout <<"HTETthres1=="<<HTETthres1<<endl;
}

void StJetSimuReader::setBemc2004Trig(int dsm){
  
  float EtAdc[20];//2004 Et/ADC calibration
  EtAdc[0]=0.006962;
  EtAdc[1]=0.006980;  
  EtAdc[2]=0.007015;
  EtAdc[3]=0.007067;  
  EtAdc[4]=0.007137;
  EtAdc[5]=0.007225;  
  EtAdc[6]=0.007331;
  EtAdc[7]=0.007455;  
  EtAdc[8]=0.007598;
  EtAdc[9]=0.007760;  
  EtAdc[10]=0.007942;
  EtAdc[11]=0.008143;  
  EtAdc[12]=0.008364;
  EtAdc[13]=0.008607;  
  EtAdc[14]=0.008871;  
  EtAdc[15]=0.009157;
  EtAdc[16]=0.009466;
  EtAdc[17]=0.009799;
  EtAdc[18]=0.010156;
  EtAdc[19]=0.010499;

  // Now calculate Hardware ADC threshold = dsm * 2^5 (five bits dropped)
  int AdcThr=dsm*32;
  
  //Calculate effective Et threshold for each eta bin  EtThr(eta)=hardware thr * Et/adc(eta) for 2004
  for (int n=0;n<20;n++){
    EtThr[n]=AdcThr*EtAdc[n];
    printf("eff Et Thresh =%f for etabin=%d \n",EtThr[n],n);
  } 
}

void StJetSimuReader::getSoftTrig(float et[20], int *trig){
  *trig=0;
  for (int n=0;n<20;n++){
    //cout <<"et ="<<et[n]<<endl;
    if (et[n]>HTETthres1) {
      *trig=1;
      HTfreq->Fill(n);
      printf("Bin=%d, MaxTowEt=%f, HTETthres1=%f,trig=%d\n",n,et[n],HTETthres1,*trig);
    }
  }
}


void StJetSimuReader::getBemc2004Trig(float et[20], int *trig){
  *trig=0;
  for (int n=0;n<20;n++){
    if (et[n]>EtThr[n]) {
      *trig=1;
      HTfreq->Fill(n);
      printf("Bin=%d, MaxTowEt=%f, EtThres=%f,trig=%d\n",n,et[n],EtThr[n],*trig);
    }
  }
}

void StJetSimuReader::SetSimuSwitch(int i){
  SimuSwitch=i;
  cout<<"User set SimuSwitch to "<<i<<" SimuSwitch = "<<SimuSwitch<<endl;
}


void StJetSimuReader::SetEventSwitch(int i){
  EventSwitch=i;
  cout<<"User set EventSwitch to "<<i<<" EventSwitch = "<<EventSwitch<<endl;
}


void StJetSimuReader::SetEnergyScaleSwitch(int i){
  EnergyScaleSwitch=i;
  cout<<"User set EventSwitch to "<<i<<" EventSwitch = "<<EventSwitch<<endl;
}

void StJetSimuReader::SetCheckPDFSwitch(int i){
  CheckPDFSwitch=i;
  cout<<"User set CheckPDFSwitch to "<<i<<" CheckPDFSwitch = "<<EventSwitch<<endl;
}

void StJetSimuReader::SetAsymBiasSwitch(int i){
  AsymBiasSwitch=i;
  cout<<"User set AsymBiasSwitch to "<<i<<" AsymBiasSwitch = "<<EventSwitch<<endl;
}
