//*-- Author : Renee Fatemi 
// $Id: StJetSimuWeightMaker.cxx,v 1.4 2006/02/10 18:08:32 mmiller Exp $

#include "TFile.h"
#include "StJetMaker/StJetSimuUtil/StJetSimuWeightMaker.h"
#include "StChain.h"
#include "StJetMaker/StJetSimuUtil/StJetSimuTrigMaker.h"
#include "StMcEventMaker/StMcEventMaker.h"
#include "StMcEventTypes.hh"
#include "StMcEvent.hh"
#include "St_DataSet.h"
#include "St_DataSetIter.h"
#include "tables/St_g2t_event_Table.h"
#include "tables/St_particle_Table.h"
#include "tables/St_g2t_pythia_Table.h"

ClassImp(StJetSimuWeightMaker)

StJetSimuWeightMaker::StJetSimuWeightMaker(const char *name):StMaker(name){
}

StJetSimuWeightMaker::~StJetSimuWeightMaker(){
}

Int_t StJetSimuWeightMaker::Init(){
 

  mcEventMaker=(StMcEventMaker *)GetMaker("StMcEvent");
  trigMaker=(StJetSimuTrigMaker *)GetMaker("SimuTrig");
  assert(trigMaker);

  //set id=0 and flag=0 for first round to load correct file
  polid=0;
  pol_id_flag=0;
  unpolid=0;
  unpol_id_flag=0;


  return StMaker::Init();
}

void StJetSimuWeightMaker::Zero(){
  pid=-10;
  s= 0;
  t= 0;
  u= 0;
  hard_p= -10;
  cos_th= -10;
  x1= -10;
  x2= -10;
}

//_____________________________________________________________________________
/// Make - this method is called in loop for each event
Int_t StJetSimuWeightMaker::Make(){

  Zero();

  if (pol_id_flag==0) polid=0;
  if (pol_id_flag==1) polid=1;
  if (unpol_id_flag==0) unpolid=0;
  if (unpol_id_flag==1) unpolid=1;

  evtid=trigMaker->evtID;

  //SubProcess ID from StMcEventMaker
  StMcEvent* mcEvent = (StMcEvent*) GetDataSet("StMcEvent"); 
  if(!mcEvent)  {
    printf("No McEvent!!!");
    return kStErr;
  }
  pid=mcEvent->subProcessId();
  
  //GET PYTHIA RECORD
  TDataSet *Event = GetDataSet("geant");
  if (print) Event->ls(3);
  TDataSetIter geantDstI(Event);
  particleTabPtr = (St_particle  *) geantDstI("particle");
  particle_st* particleTable = particleTabPtr->GetTable();
  if (print) particleTabPtr->Print();
  Pg2t_event=(St_g2t_event *) geantDstI("g2t_event");
  if (print) Pg2t_event->Print();
  g2t_event_st *g2t_event1=Pg2t_event->GetTable();
  geantID= g2t_event1->n_event; 
  geantPID= g2t_event1->subprocess_id;

  //Get s,t,u, parontic pT, cos(theta),xb1,xb2
  Pg2t_pythia=(St_g2t_pythia *) geantDstI("g2t_pythia");
  assert(Pg2t_pythia);
  if (print) Pg2t_pythia->Print();
  g2t_pythia_st *g2t_pythia1=Pg2t_pythia->GetTable();
  assert(g2t_pythia1);
  
  s= g2t_pythia1-> mand_s;
  t= g2t_pythia1-> mand_t;
  u= g2t_pythia1-> mand_u;
  hard_p= g2t_pythia1->hard_p;
  cos_th= g2t_pythia1->cos_th;
  x1= g2t_pythia1->bjor_1;
  x2= g2t_pythia1->bjor_2;
  print=1;
  if (print) printf("s=%f,u=%f,t=%f,hard_p=%f,cos_th=%f,x1=%f,x2=%f\n",s,u,t,hard_p,cos_th,x1,x2);
  print=0;

  //get flavor of partons after intial radiation before scattering and then after scattering
  flavor1=particleTable[4].idhep;
  flavor2=particleTable[5].idhep;
  flavor3=particleTable[6].idhep;
  flavor4=particleTable[7].idhep;

  parton1[0]=particleTable[6].idhep;// particle id
  parton1[1]=particleTable[6].phep[0];//px
  parton1[2]=particleTable[6].phep[1];//py
  parton1[3]=particleTable[6].phep[2];//pz
  parton1[4]=particleTable[6].phep[3];//E
  parton1[5]=particleTable[6].phep[4];//m
  parton1[6]=particleTable[6].isthep;//status
  parton1[7]=particleTable[6].jmohep[0];//moth1
  parton1[8]=particleTable[6].jmohep[1];//moth2
  parton1[9]=particleTable[6].jdahep[0];//daughter1
  parton1[10]=particleTable[6].jdahep[1];//daughter2
  parton2[0]=particleTable[7].idhep;// particle id
  parton2[1]=particleTable[7].phep[0];//px
  parton2[2]=particleTable[7].phep[1];//py
  parton2[3]=particleTable[7].phep[2];//pz
  parton2[4]=particleTable[7].phep[3];//E
  parton2[5]=particleTable[7].phep[4];//m
  parton2[6]=particleTable[7].isthep;//status
  parton2[7]=particleTable[7].jmohep[0];//moth1
  parton2[8]=particleTable[7].jmohep[1];//moth2
  parton2[9]=particleTable[7].jdahep[0];//daughter1
  parton2[10]=particleTable[7].jdahep[1];//daughter2

  printf("PID/evtid from McEvent = %d,%d; PID/evtid from Table = %d,%d:\n",pid,evtid,geantPID,geantID);
 
  if (print){
    printf("PID/evtid from McEvent = %d,%d; PID/evtid from Table = %d,%d:\n",pid,evtid,geantPID,geantID);
    printf("row |   id   |   px   |   py   |   pz   |   E   |   m   | status | moth1 | moth2 | daught1 | daught2 |\n");
    for (int i=0; i<particleTabPtr->GetNRows();++i) {
      printf("  %d,  %d,  %f,   %f,   %f,   %f,   %f,   %d,   %d,   %d,   %d,   %d\n",i,particleTable[i].idhep, particleTable[i].phep[0], particleTable[i].phep[1], particleTable[i].phep[2] , particleTable[i].phep[3], particleTable[i].phep[4], particleTable[i].isthep , particleTable[i].jmohep[0], particleTable[i].jmohep[1], particleTable[i].jdahep[0], particleTable[i].jdahep[1]);}
  }
  
  
  //TEST that geantPID==McEventPID and geantID==McEventID
  assert(pid==geantPID);
  assert(evtid==geantID);

  //Get partonic a_LL, polarized/unpolarized pdfs using Q2 = partonic_pT^2
  Q2=hard_p*hard_p;
  df1=getPolPDF(flavor1,x1,Q2);
  df2=getPolPDF(flavor2,x2,Q2);
  f1=getUnPolPDF(flavor1,x1,Q2);
  f2=getUnPolPDF(flavor2,x2,Q2);
  partonic_all=getPartonicALL(s,t,u,pid,flavor1,flavor2,flavor3,flavor4);
  weight=(df1*df2*partonic_all)/(f1*f2);

  if (print) {
    cout<<" 1st parton pol:"<<df1<<" unpol= "<<f1<<endl;
    cout<<" 2nd parton pol:"<<df2<<" unpol= "<<f2<<endl;
    cout<<" Partonic A_LL:"<<partonic_all<<endl;
  }
  pol_id_flag=1;
  unpol_id_flag=1;


  return kStOK;   
}


//returns polarized delta(PDF) 
Double_t StJetSimuWeightMaker::getPolPDF(int flavor, double x, double Q2){
  if (print) cout<<"getPolPDF: flavor="<<flavor<<" x="<<x<<" Q2="<<Q2<<" id="<<polid<<endl;

  double parpol[6]={0.0,0.0,0.0,0.0,0.0,0.0};
  double pdf=1000;

  polar_(&polset, &x, &Q2, parpol, &polid);
  if (print) cout <<"getPolPDF:  U="<<parpol[0]<<" D="<<parpol[1]<<" UB="<<parpol[2]<<" DB="<<parpol[3]<<" ST="<<parpol[4]<<" GL="<<parpol[5]<<endl;

  if (flavor==1) pdf=parpol[1];      //dv + dsea quark
  if (flavor==2) pdf=parpol[0];      //uv + usea quark
  if (flavor==-1) pdf=parpol[3];     //dbar==dsea quark
  if (flavor==-2) pdf=parpol[2];     //ubar==usea quark
  if (abs(flavor)==3) pdf=parpol[4]; //s==sbar quark
  if (flavor==21) pdf=parpol[5];     //gluon
  if ((abs(flavor)>=4)&&(abs(flavor)<=6)) pdf=parpol[4];
  
  return pdf;
}
/*
//returns unpolarized grv PDF 
Double_t StJetSimuWeightMaker::getUnPolPDF(int flavor, double x, double Q2){
  if (print) cout<<"getUnPolPDF: flavor="<<flavor<<" x="<<x<<" Q2="<<Q2<<" id="<<unpolid<<endl;

  double parpol[6]={0.0,0.0,0.0,0.0,0.0,0.0};
  double pdf=1000;

  unpolar_(&unpolset, &x, &Q2, parpol, &unpolid);
  if (print) cout <<"getUnPolPDF:  U="<<parpol[0]<<" D="<<parpol[1]<<" UB="<<parpol[2]<<" DB="<<parpol[3]<<" ST="<<parpol[4]<<" GL="<<parpol[5]<<endl;

  if (flavor==1) pdf=parpol[1]+parpol[3];      //dv + dsea
  if (flavor==2) pdf=parpol[0]+parpol[2];      //uv + usea
  if (flavor==-1) pdf=parpol[3];               //dbar=dsea quark
  if (flavor==-2) pdf=parpol[2];               //ubar=usea quark
  if (abs(flavor)==3) pdf=parpol[4];           //s==sbar quark
  if (flavor==21) pdf=parpol[5];               //gluon
  if ((abs(flavor)>=4)&&(abs(flavor)<=6)) pdf=parpol[4];
  
  return pdf;
}
*/

/* Returns unpolarized CTEQ*/
Double_t StJetSimuWeightMaker::getUnPolPDF(int flavor, double x, double Q2){
  if (print) cout<<"getUnPolPDF: flavor="<<flavor<<" x="<<x<<" Q2="<<Q2<<" id="<<unpolid<<endl;

  Double_t pdf=0.0;
  Int_t iset=3;
  Int_t er=0;
  Int_t fl=10;
  
  if (flavor==1) fl=2;
  if (flavor==2) fl=1;
  if (flavor==-1) fl=-2;
  if (flavor==-2) fl=-1;
  if (flavor==21) fl=0;
  if (flavor==3) fl=3;
  if (flavor==-3) fl=-3;
  if (flavor==4) fl=4;
  if (flavor==-4) fl=-4;
  if (flavor==5) fl=5;
  if (flavor==-5) fl=-5;

  double Q=pow(Q2,0.5);
  pdf=ctq5pd_(&iset,&fl,&x,&Q,&er);
  if (er!=0) pdf=0.0;

  return pdf;
}

Double_t StJetSimuWeightMaker::getPartonicALL(double s, double t, double u, int sub, int inA, int inB, int outA, int outB){

  //Werner definitions:
  //1: qq'->qq' (qqbar'->qqbar') 2: qq->qq   3: qqbar->q'qbar'  4: qqbar->qqbar 5: qqbar->gg   6: gg->qqbar  7: qg->qg   8: gg->gg
  //PYTHIA definitions:
  //1: 11a                       2:11b       3: 12a             4:12b           5: 13          6: 53         7: 28       8: 68
  //NOTES:
  // 3==5==6==-1  1==7  1!=2 and 1!=4

  double N1,N2,N3,N4,N5,N6,N7,N8;
  double D1,D2,D3,D4,D5,D6,D7,D8;
  double all=-10;

  num_(&s,&t,&u,&N1,&N2,&N3,&N4,&N5,&N6,&N7,&N8);
  denom_(&s,&t,&u,&D1,&D2,&D3,&D4,&D5,&D6,&D7,&D8);
  if (print){
    cout<<"s="<<s<<" t="<<t<<" u="<<u<<" sub="<<sub<<" inA="<<inA<<" inB="<<inB<<" outA="<<outA<<" outB="<<outB<<endl;
    cout<<" 1="<<N1<<" "<<D1<<endl;
    cout<<" 2="<<N2<<" "<<D2<<endl;
    cout<<" 3="<<N3<<" "<<D3<<endl;
    cout<<" 4="<<N4<<" "<<D4<<endl;
    cout<<" 5="<<N5<<" "<<D5<<endl;
    cout<<" 6="<<N6<<" "<<D6<<endl;
    cout<<" 7="<<N7<<" "<<D7<<endl;
    cout<<" 8="<<N8<<" "<<D8<<endl;
  }

  
  if ((sub==11)&&(abs(inA)!=abs(inB))) all=N1/D1;
  if ((sub==11)&&(abs(inA)==abs(inB))) all=N2/D2;
  if ((sub==12)&&(abs(inA)!=abs(outA))) all=N3/D3;
  if ((sub==12)&&(abs(inA)==abs(outA))) all=N4/D4;
  if (sub==13) all=N5/D5;
  if (sub==53) all=N6/D6;
  if (sub==28) all=N7/D7;
  if (sub==68) all=N8/D8;
	   
  return all;
}


















