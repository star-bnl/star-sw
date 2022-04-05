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


void storetofDaqMap()
//int main(int argc, char *argv[])

{

  const Int_t NCHAN = 192;
  const Int_t NVPD = 19;

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
  StDbTable* tofDaqMap = configNode->addDbTable("tofDaqMap");

  //-- fill structures & store times
  tofDaqMap_st *daqmap = new tofDaqMap_st[1];

  Short_t Chan2M[24] = {1, 3, 2, 3, 2, 1, 2, 1,
			3, 4, 1, 2, 4, 4, 1, 1,
			4, 4, 3, 3, 2, 4, 3, 2};
//  Short_t Chan2C[24] = {3, 5, 4, 3, 2, 5, 6, 1,
//			1, 6, 4, 5, 4, 2, 6, 2,
//			1, 3, 2, 4, 3, 5, 6, 1};
  Short_t Chan2C[24] = {4, 2, 3, 4, 5, 2, 1, 6,
                        6, 1, 3, 2, 3, 5, 1, 5,
                        6, 4, 5, 3, 4, 2, 1, 6};

  Short_t Chan2MRPC[24], MRPC2Chan[24];
//  = { 02, 16, 09, 14, 07, 04, 11, 00,
// 			    12, 23, 03, 10, 21, 19, 05, 01,
// 			    18, 20, 13, 15, 08, 22, 17, 06};

//  Short_t PMT2B[19] = {5, 5, 4, 4, 1, 1, 0, 0, 5, 4, 1,
//		       5, 5, 4, 4, 1, 1, 0, 0};
//  Short_t PMT2LeT[19] = {2, 0, 2, 0, 2, 0, 2, 0, 0, 0, 0,
//			 2, 0, 2, 0, 2, 0, 2, 0};
//  Short_t PMT2LeC[19] = {6, 2, 6, 2, 6, 2, 6, 2, 0, 0, 0,
//			 0, 3, 0, 3, 0, 3, 0, 3};
//  Short_t PMT2TeT[19] = {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
//			 1, 1, 1, 1, 1, 1, 1, 1};
//  Short_t PMT2TeC[19] = {1, 3, 1, 3, 1, 3, 1, 3, 7, 7, 7,
//			 5, 4, 5, 4, 5, 4, 5, 4};

//  Short_t PMT2LeChan[19];
//  Short_t PMT2TeChan[19];
// Run 8
/*
  Short_t PMT2LeChan[38] = {142,122,118,98,46,26,22,2,120,96,
                            24,136,123,112,99,40,27,16,3,   //west
                            142,122,118,98,46,26,22,2,112,101,
                            24,136,123,120,99,40,27,16,3};  //east;
  Short_t PMT2TeChan[38] = {129,131,105,107,33,35,9,11,135,111,
                            39,133,132,109,108,37,36,13,12,  //west
                            129,131,105,107,33,35,9,11,109,110,
                            39,133,132,135,108,37,36,13,12}; //east;
*/
 // Run 9 E/W symmetric
  Short_t PMT2LeChan[38] = {142,122,118,98,46,26,22,2,112,101,
                            24,136,123,120,99,40,27,16,3,   //west
                            142,122,118,98,46,26,22,2,112,101,
                            24,136,123,120,99,40,27,16,3};  //east;
  Short_t PMT2TeChan[38] = {129,131,105,107,33,35,9,11,109,110,
                            39,133,132,135,108,37,36,13,12,  //west
                            129,131,105,107,33,35,9,11,109,110,
                            39,133,132,135,108,37,36,13,12}; //east;

  for(int i=0;i<24;i++) {
    Chan2MRPC[i] = (Chan2M[i] - 1)*6 + (Chan2C[i] - 1);
    MRPC2Chan[Chan2MRPC[i]] = i;
  }

  for(int i=0;i<24;i++) {
    if(i%8==0) cout << endl;
    cout << " " << MRPC2Chan[i];
  }
  cout << endl;

/*
  for(int i=0;i<19;i++) {
    PMT2LeChan[i] = PMT2B[i] * 24 + PMT2LeT[i] * 8 + PMT2LeC[i];
    PMT2TeChan[i] = PMT2B[i] * 24 + PMT2TeT[i] * 8 + PMT2TeC[i];
  }
*/
  cout << " West PMT 2 leading Tdc " << endl;
  for(int i=0;i<19;i++) cout << " " << PMT2LeChan[i];
  cout << endl;
  cout << " West PMT 2 trailing Tdc " << endl;
  for(int i=0;i<19;i++) cout << " " << PMT2TeChan[i];
  cout << endl;

  cout << " East PMT 2 leading Tdc " << endl;
  for(int i=19;i<38;i++) cout << " " << PMT2LeChan[i];
  cout << endl;
  cout << " West PMT 2 trailing Tdc " << endl;
  for(int i=19;i<38;i++) cout << " " << PMT2TeChan[i];
  cout << endl;
  

  for(int i=0;i<NCHAN;i++) {
    int iboard = i/24;
    int ii = i%24;
//    daqmap[0].MRPC2TDIGChanMap[i] = iboard*24 + Chan2MRPC[ii];
    daqmap[0].MRPC2TDIGChanMap[i] = iboard*24 + MRPC2Chan[ii];
  }

  for(int i=0;i<50;i++) {
    if(i<NVPD*2) {
      daqmap[0].PMT2TDIGLeChanMap[i] = PMT2LeChan[i];
      daqmap[0].PMT2TDIGTeChanMap[i] = PMT2TeChan[i];
    } else {
      daqmap[0].PMT2TDIGLeChanMap[i] = -1;
      daqmap[0].PMT2TDIGTeChanMap[i] = -1;
    }
  }


  //- store data in table
  tofDaqMap->SetTable((char*)daqmap, 1);
  //- set store time
  dbManager->setStoreTime(ZStoreTime.Data());
  //- store table in dBase
  cout<<" here "<<endl;
  dbManager->storeDbTable(tofDaqMap);
  cout<<"uploaded"<<endl;
      //    return 0;
}

