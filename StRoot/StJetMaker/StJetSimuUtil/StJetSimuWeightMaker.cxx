//*-- Author : Renee Fatemi 
// $Id: StJetSimuWeightMaker.cxx,v 1.2 2005/09/28 18:18:12 mmiller Exp $

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
 
  //get Makers
  mcEventMaker=(StMcEventMaker *)GetMaker("StMcEvent");
  assert(mcEventMaker);
  trigMaker=(StJetSimuTrigMaker *)GetMaker("SimuTrig");
  assert(trigMaker);

  return StMaker::Init();
}


//_____________________________________________________________________________
/// Make - this method is called in loop for each event
Int_t StJetSimuWeightMaker::Make(){

  //SubProcess ID from StMcEventMaker
    StMcEvent* mcEvent = (StMcEvent*) GetDataSet("StMcEvent"); 
    //StMcEvent* mcEvent = (StMcEvent*) mcEventMaker->currentMcEvent();
  if(!mcEvent)  {
    printf("No McEvent!!!");
    return kStErr;
  }
  pid=mcEvent->subProcessId();
  evtid=trigMaker->evtID;
  
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
  
  s= g2t_pythia1->mand_s;
  t= g2t_pythia1->mand_t;
  u= g2t_pythia1->mand_u;
  hard_p= g2t_pythia1->hard_p;
  cos_th= g2t_pythia1->cos_th;
  x1= g2t_pythia1->bjor_1;
  x2= g2t_pythia1->bjor_2;
  if (print) printf("s=%f,u=%f,t=%f,hard_p=%f,cos_th=%f,x1=%f,x2=%f\n",s,u,t,hard_p,cos_th,x1,x2);
  

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
 
  if (print){
    printf("PID/evtid from McEvent = %d,%d; PID/evtid from Table = %d,%d:\n",pid,evtid,geantPID,geantID);
    printf("row |   id   |   px   |   py   |   pz   |   E   |   m   | status | moth1 | moth2 | daught1 | daught2 |\n");
    for (int i=0; i<particleTabPtr->GetNRows();++i) {
      //for (int i=0; i<6;++i) {
      printf("  %d,  %d,  %f,   %f,   %f,   %f,   %f,   %d,   %d,   %d,   %d,   %d\n",i,particleTable[i].idhep, particleTable[i].phep[0], particleTable[i].phep[1], particleTable[i].phep[2] , particleTable[i].phep[3], particleTable[i].phep[4], particleTable[i].isthep , particleTable[i].jmohep[0], particleTable[i].jmohep[1], particleTable[i].jdahep[0], particleTable[i].jdahep[1]);}
  }
    
  //TEST that geantPID==McEventPID and geantID==McEventID
   assert(pid==geantPID);
  assert(evtid==geantID);
 
  return kStOK;   
}











