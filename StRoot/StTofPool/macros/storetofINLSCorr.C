// macro to upload tofr5 INL tables to database
//
// based on
//  http://www.star.bnl.gov/STAR/comp/db/StoreDbTable.cc.html
//
// Jing Liu, 02/18/2005 
//

 
// #include "StDbLib/StDbManager.hh"
// #include "StDbLib/StDbConfigNode.hh"
// #include "StDbLib/StDbTable.h"
// #include "StDbLib/StDbDefs.hh"

#include <iostream>
#include <fstream>
#include <string>
using namespace std;


void storetofINLSCorr()
//int main(int argc, char *argv[])

{

  const Int_t NMAX = 1200;
  const Int_t NTDC = 3;  // # of tdcs per board
  const Int_t NCHAN = 8;  // # of channels per tdc

  //-- load dBase and Table definition libraries
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("StUtilities");
  gSystem->Load("St_Tables.so");

  gSystem->Load("StDbLib.so");
  gSystem->Load("libStDb_Tables.so");

  //-- get the singleton manager
  StDbManager* dbManager = StDbManager::Instance();

  //-- connect to the db & get an empty container
  StDbConfigNode* configNode = dbManager->initConfig("Calibrations_tof");

  //----------------------------------------
  TString ZStoreTime = "2008-11-01 00:00:00";

  //-- add table to the container with descriptor given by Database
  StDbTable* tofINLSCorr = configNode->addDbTable("tofINLSCorr");

  //-- fill structures & store times
  tofINLSCorr_st *inlcorr= new tofINLSCorr_st[NMAX*NTDC*NCHAN];

  Int_t NFILL = 0;
  Int_t NBOARD = 0;
  // read in inltable file by file.

  TH1S *hINLSCorr[NMAX][NTDC][NCHAN];
  Bool_t *hFlag[NMAX];
  for(int i=0;i<NMAX;i++) hFlag[i] = false;

  TFile *f = new TFile("INL_test.root");

  for(int iboard=0;iboard<NMAX;iboard++) {    
    if(iboard%100==0) cout << " Processing board Id = " << iboard << endl;

    for(int itdc=0;itdc<NTDC;itdc++){
      for(int ichan=0;ichan<NCHAN;ichan++) {

	char hisname[512];
	sprintf(hisname,"b_%d_tdc_%d_chan_%d",iboard,itdc,ichan);

        hINLSCorr[iboard][itdc][ichan] = (TH1S *)f->Get(hisname);

        if(!hINLSCorr[iboard][itdc][ichan]) {
//          cout << " Histogram board " << iboard << " itdc " << itdc << " ichan " << ichan << " doesn't exist!" << endl;
          continue;
        }

	inlcorr[NFILL].tdigId = (Short_t)iboard;
	inlcorr[NFILL].tdcChanId = (Short_t)(itdc*NCHAN+ichan);	
	
	for(int j=0;j<1024;j++) {
	  inlcorr[NFILL].INLCorr[j]=(Short_t)(hINLSCorr[iboard][itdc][ichan]->GetBinContent(j+1));
	}
      

	NFILL++;
      } // end of loop channel
    }  // end of loop TDC

    NBOARD++;
    hFlag[iboard] = true;

  } // end of loop board
  cout<<" prepare to upload data to DB NFILL="<<NFILL<<" NBOARD="<<NBOARD<<endl;

/*
  char rootname[100];
  sprintf(rootname,"INL_test.root");
  TFile *fout = new TFile(rootname,"recreate");
  int index = 0;
  for(int iboard=0;iboard<NMAX;iboard++) {
    if(!hFlag[iboard]) continue;

    for(int itdc=0;itdc<NTDC;itdc++){
      for(int ichan=0;ichan<NCHAN;ichan++) {
	if(!hINLSCorr[iboard][itdc][ichan]) continue;
	hINLSCorr[iboard][itdc][ichan]->Write();

	cout << " Ids = " << iboard << " " << itdc << " " << ichan << endl;
	cout << " IN Ids = " << inlcorr[index].tdigId << " " << inlcorr[index].tdcChanId << endl;
	for(int j=0;j<1024;j++) {
	  if(j%100==0)
	    cout << " INL j = " << j << " " << inlcorr[index].INLCorr[j] << endl;
	}
	index++;
      }
    }    
  }
  fout->Close();
*/

  //- store data in table
  tofINLSCorr->SetTable((char*)inlcorr, NFILL);
  //- set store time
  dbManager->setStoreTime(ZStoreTime.Data());
  //- store table in dBase
  cout<<" here "<<endl;
  dbManager->storeDbTable(tofINLSCorr);
  cout<<"uploaded"<<endl;
      //    return 0;
}

