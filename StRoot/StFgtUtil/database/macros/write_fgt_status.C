{
#include <iomanip>
#include "fgtStatus.h"

  Int_t runnumber =72;

  gSystem->Setenv("DB_ACCESS_MODE", "write");

  gROOT->Macro("LoadLogger.C");
  gSystem->Load("St_base.so");
  gSystem->Load("libStDb_Tables.so");
  gSystem->Load("StDbLib.so");
    
  // Initialize db manager
  StDbManager* mgr = StDbManager::Instance();
  StDbConfigNode* node = mgr->initConfig("Calibrations_fgt");
  StDbTable* dbtable = node->addDbTable("fgtStatus");

  TString storeTime = "2012-11-08 00:00:00";
  cout <<"The RTS Start date and time is " << storeTime<<endl;
  mgr->setStoreTime(storeTime.Data());
  
  // Create your c-struct
  fgtStatus_st table;
  
  TString infile = "Cosmic_";
  infile +=runnumber;
  infile +=".FGT-stat-DB.dat";
  cout<<" Opening File "<<infile<<endl;

  std::ifstream in(infile);
  if (!in.is_open()) {
    cout << "Can't find file!\n"; 
    exit(0); 
  } 


  Int_t electId,stat;//electronic Id
  Int_t zero = 128;//if this channel does not have a determined pedestal set it to bad
  Int_t counter=0;
  Char_t status;//status
  TString statread;

  //initialize all entries to 0 ie good
  for (int i = 0; i < 51200; i++) {
    table.Status[i] = (Char_t)zero;
  }

  //read in file of electId and status
  while(in >> electId >>statread){

    statread.Remove(0,2);
    stat = statread.Atoi();
    status = (Char_t)stat;
    cout<<counter++<<" electId="<<electId<<" stat ="<<statread<<" status="<<(Short_t)status<<endl;
    table.Status[electId] = status;
  }
  
  // Store data to the StDbTable
  dbtable->SetTable((char*)&table, 1);
  
  // Store table to database
  mgr->storeDbTable(dbtable);
}
