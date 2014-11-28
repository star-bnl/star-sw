// macro to upload tofr5 INL tables to database
//
// based on
//  http://www.star.bnl.gov/STAR/comp/db/StoreDbTable.cc.html
//
// Jing Liu, 02/18/2005 
// FG 01/13/2012 -- Updated to allow for arbitray number of TDIGs per tray

 
#include <iostream>
#include <fstream>
#include <string>
#include <iomanip>
using namespace std;


void storetofTDIGOnTray() {
  const Bool_t Debug = kFALSE;
  const Int_t NBOARD = 8;
//-- year8
  // const Int_t NTRAY = 5;
//-- year9
  //  const Int_t NTRAY = 94;
  //  const Int_t NVPDTRAY = 2;
  //  const Int_t NMAX = 120;
//-- year10/year11
  const Int_t NTRAY = 120;
  const Int_t NVPDTRAY = 2;
  const Int_t NMAX = 300; //123;

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
  //  TString ZStoreTime = "2007-11-01 00:00:03";
  //  TString ZStoreTime = "2008-11-01 00:00:00";
  //  TString ZStoreTime = "2009-11-01 00:00:00";
  //  TString ZStoreTime = "2010-12-20 00:00:01";
  //  TString ZStoreTime = "2011-04-20 00:00:00";
  //  TString ZStoreTime = "2011-12-20 00:00:00";
  TString ZStoreTime = "2012-12-20 00:00:00";

//-- add table to the container with descriptor given by Database
  StDbTable* tofTDIGOnTray = configNode->addDbTable("tofTDIGOnTray");

//-- fill structures & store times
  tofTDIGOnTray_st *tdig = new tofTDIGOnTray_st[NMAX+NVPDTRAY];

//-- year8
  //  Short_t trayId[NTRAY+NVPDTRAY] = {76, 77, 78, 79, 80, 121, 122};
  //  Short_t tdigId[NTRAY+NVPDTRAY][NBOARD] = {
  //    36, 26, 27, 29, 25, 23, 22, 21,
  //    30, 31, 33, 35, 28, 32, 34, 37,
  //    74, 63, 49, 72, 64, 53, 59, 52,
  //    70, 51, 75, 65, 66, 50, 71, 68,
  //    46, 62, 72, 69, 61, 58, 42, 43,
  //    89, 91,  0,  0, 84, 82,  0,  0,
  //    85, 79,  0,  0, 80, 83,  0,  0
  //  };
// ------------------------------
//-- year10
  //  Short_t trayId[NMAX] = { 68, 58, 63,  6,106, 67, 61, 50, 57, 60,
  //                           47, 46, 87,133, 64, 72, 62, 42, 14, 11,
  //                           28,  7, 25, 33, 19, 17, 37, 21, 30, 35,
  //                            8, 20, 31, 27, 12,  9, 10, 29, 34, 13,
  //                           16, 77, 89, 36, 39, 23, 32, 15, 24, 18,
  //                           59, 66, 70, 38, 41, 45, 52, 54, 53, 55,
  //                           84,105, 94, 91, 95, 48, 44, 22, 26, 49,
  //                            5,  2, 75, 40,117,  4,121, 99,120,122,
  //                          111,116,118,119,109,101,100,107, 98, 56,
  //                           88,115,102,108,112,110,113,114,123, 90,
  //                           93, 65, 51, 73, 76, 83, 69, 80, 82, 74,
  //                           97, 79, 43, 86, 85,132,104, 92, 96,103};
  //                              // B-132;  C-133  renamed
// ------------------------------
//-- year11
  // ifstream infile("data/run11/INL/tdigs_01122011.txt");
// VPD boards until April 2011
  //  Short_t tdigId_Vpd[NVPDTRAY][NBOARD] = { 89, 91, 1102, 0, 885, 82, 0, 0,
  //                                           85, 79, 1101, 0,  80, 83, 0, 0};
  //  
  //  Short_t trayId[NMAX] = { 68, 58, 63,  6,106, 67, 61, 50, 57, 71,
  //                           47, 46, 87,133, 64, 72, 62, 42, 14, 11,
  //                           28,  7, 25, 33, 19, 17, 37, 21, 30, 35,
  //                            8, 20, 31, 27, 12,  9, 10, 29, 34, 13,
  //                           16, 77, 89, 36, 39, 23, 32, 15, 24, 18,
  //                           59, 66, 70, 38, 41, 45, 52, 54, 53, 55,
  //                           84,105, 94, 91, 95, 48, 44, 22, 26, 49,
  //                            5,  2, 75, 40,117,  4,121, 99,120,122,
  //                          111,116,118,119,109,101,100,107, 98, 56,
  //                           88,115,102,108,112,110,113,114,123, 90,
  //                           93, 65, 51, 73, 76, 83, 69, 80, 82, 74,
  //                           97, 79, 43, 86, 85,132,104, 92, 96,103};
  //                              // B-132;  C-133  renamed
// ------------------------------
//-- year12
//  changed trays at W8, W23, E93
//  ifstream infile("data/run12/INL/tdigs_120106.txt");
//  Short_t trayId[NTRAY] = { 68, 58, 63,  6,106, 67, 61, 81, 57, 71,
//			    47, 46, 87,133, 64, 72, 62, 42, 14, 11,
//			    28,  7,  3, 33, 19, 17, 37, 21, 30, 35,
//			     8, 20, 31, 27, 12,  9, 10, 29, 34, 13,
//			    16, 77, 89, 36, 39, 23, 32, 15, 24, 18,
//			    59, 66, 70, 38, 41, 45, 52, 54, 53, 55,
//			    84,105, 94, 91, 95, 48, 44, 22, 26, 49,
//			     5,  2, 75, 40,117,  4,121, 99,120,122,
//			   111,116,118,119,109,101,100,107, 98, 56,
//			    88,115, 78,108,112,110,113,114,123, 90,
//			    93, 65, 51, 73, 76, 83, 69, 60, 82, 74,
//			    97, 79, 43, 86, 85,132,104, 92, 96,103};
//                            // B-132;  C-133  renamed
// ------------------------------
//-- year13
//  changed trays at W8, W23, W38, E93, E95, E109
// see http://www.star.bnl.gov/HyperNews-star/protected/get/startof/2675.html
  ifstream infile("data/run13/INL/tdigs_20130115.txt");
  Short_t trayId[NTRAY] = { 68, 58, 63,  6,106, 67, 61, 50, 57, 71,
			    47, 46, 87,133, 64, 72, 62, 42, 14, 11,
			    28,  7, 25, 33, 19, 17, 37, 21, 30, 35,
			     8, 20, 31, 27, 12,  9, 10,  3, 34, 13,
			    16, 77, 89, 36, 39, 23, 32, 15, 24, 18,
			    59, 66, 70, 38, 41, 45, 52, 54, 53, 55,
			    84,105, 94, 91, 95, 48, 44, 22, 26, 49,
			     5,  2, 75, 40,117,  4,121, 99,120,122,
			   111,116,118,119,109,101,100,107, 98, 56,
			    88,115,102,108, 81,110,113,114,123, 90,
			    93, 65, 51, 73, 76, 83, 69, 80, 82, 74,
			    97, 79, 43, 86, 85,132,104, 92, 96,103};
                            // B-132;  C-133  renamed
// ------------------------------
  Short_t tdigId[NTRAY][NBOARD];

//-- run 11 AuAu@18GeV, run 12, run 13
  Short_t tdigId_Vpd[NVPDTRAY][NBOARD] = { 89, 91,  495, 0, 885, 82, 0, 0,
                                           85, 79, 1101, 0,  80, 83, 0, 0};

  Short_t tId[NMAX], boardId[NMAX][NBOARD];  // two additional trays
  int i(0),j(0);  // clear counters
  int trayNum, oldTrayNum, iboard, boardNum;

  cout << "Reading UT-Austin database info ... "  << endl;
  if (infile) {
    // read first entry
    infile >> trayNum >> iboard >> boardNum;
    tId[i] = (Short_t)trayNum;
    boardId[i][iboard-1] = (Short_t)boardNum;      
    oldTrayNum = trayNum;
    // increment general counter (total# of TDIGs, only for debugging) and dump data
    if (Debug) cout << j++ << " " << i << "  " << trayNum << " "<< iboard << "  " << boardNum << endl;

    // loop in the remaining entries
    while ( infile >> trayNum >> iboard >> boardNum ) {
      // increment i only when trayNum incrememts ...
      if (trayNum != oldTrayNum) {
	tId[++i] = (Short_t)trayNum;
	oldTrayNum = trayNum;
      }
      // increment general counter (total# of TDIGs, only for debugging) and dump data
      if (Debug) cout << j++ << " " << i << "  " << trayNum << " "<< iboard << "  " << boardNum << endl;

      boardId[i][iboard-1] = (Short_t)boardNum;
    }
  }
  infile.close();
  cout << "... done reading database info" << endl;

  for(int i=0;i<NTRAY;i++) {
    for(int j=0;j<NBOARD;j++) { tdigId[i][j] = 0; }

    Short_t thisTray = trayId[i];
    int index = -1;
    for(int j=0;j<NMAX;j++) {
      if(thisTray==tId[j]) {
        index = j;
        break;
      }
    }
    if (Debug) cout << "pos = " << i+1 << " tray = " << thisTray << " index = " << index << " : ";
    if(index<0||index>=NMAX) { cout<< "ERROR: unexpected index "<< index << endl; continue;}
    for(int j=0;j<NBOARD;j++) {
      tdigId[i][j] = boardId[index][j];
      if (Debug) cout << tdigId[i][j] << " ";
    }
    if (Debug) cout << endl;
  }

/*
  {
    89, 91,  0,  0, 84, 82,  0,  0,
    85, 79,  0,  0, 80, 83,  0,  0
  };
*/
//-- include the VPD TDIG mapping 
  for(int i=0;i<NTRAY+NVPDTRAY;i++) {
    tdig[i].trayId = i+1;
    for(int j=0;j<NBOARD;j++) {
      if(i<NTRAY) {
        tdig[i].tdigId[j] = tdigId[i][j];
      } else {
        tdig[i].tdigId[j] = tdigId_Vpd[i-NTRAY][j];
      }
    }
  }

//-- create output file for test/verification purposes
  ofstream outData("testTDIGMap.dat");
  for(int i=0;i<NTRAY+NVPDTRAY;i++) {
    for(int j=0;j<NBOARD;j++) {
      outData << setw(5) << tdig[i].tdigId[j];
    }
    outData << endl;
  }
  outData.close();

//-- store data in table
  tofTDIGOnTray->SetTable((char*)tdig, NTRAY+NVPDTRAY);
//- set store time
  dbManager->setStoreTime(ZStoreTime.Data());
//- store table in dBase
  cout<<"Preparing to upload ... "<<endl;
  dbManager->storeDbTable(tofTDIGOnTray);
  cout << "... done" << endl;
}

