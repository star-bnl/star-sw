{
#include <iomanip>
#include "fgtPedestal.h"
   
  Int_t runnumber = 72;
 
  gSystem->Setenv("DB_ACCESS_MODE", "write");
  gROOT->Macro("LoadLogger.C");
  gSystem->Load("St_base.so");
  gSystem->Load("libStDb_Tables.so");
  gSystem->Load("StDbLib.so");

  TString storeTime = "2012-11-08 00:00:00";
  cout <<"The RTS Start date and time is " << storeTime<<endl;  

  StDbManager* mgr = StDbManager::Instance();
  StDbConfigNode* node = mgr->initConfig("Calibrations_fgt");
  StDbTable* dbtable = node->addDbTable("fgtPedestal");
  mgr->setStoreTime(storeTime.Data());
 
  //open up file to read in pedesstals
  TString infile = "Cosmic_";
  infile +=runnumber;
  infile +=".FGT-ped-DB.dat";
  cout<<" Opening File "<<infile<<endl;

  std::ifstream in(infile);
  if (!in.is_open()) {
    cout << "Can't find file!\n"; 
    exit(0); 
  } 
  in.precision(10);
  std::cout << setprecision(10);

  Double_t ped,stdev;
  Int_t electId, timebin;
  fgtPedestal_st table;
  
 
  for (int i = 0; i < 51200; i++) {
    table.AdcPedestal[i] = 0.0;
    table.AdcPedestalRMS[i] = 0.0;
    table.Status[i] = '0';
  }
  
  while (!in.eof()){
    Int_t counter;
    in >> electId >> timebin >> ped >> stdev;
    cout<<counter++<<" electId="<<electId<<" ped ="<<ped<<" stdev="<<stdev<<endl;
    table.AdcPedestal[electId]=ped;
    table.AdcPedestalRMS[electId]=stdev;
  }
  
  in.close();
 
   
  // Store data to the StDbTable
  dbtable->SetTable((char*)&table, 1);
  
  // Store table to database
  mgr->storeDbTable(dbtable);
  

};

