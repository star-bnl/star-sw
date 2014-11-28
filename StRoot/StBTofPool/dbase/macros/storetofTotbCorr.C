// $Id: storetofTotbCorr.C,v 1.4 2014/11/24 22:18:54 geurts Exp $
// macro to upload tofr5 INL tables to database
// based on http://www.star.bnl.gov/STAR/comp/db/StoreDbTable.cc.html
//
// Xin Dong, 02/18/2005 
// ---
// $Log: storetofTotbCorr.C,v $
// Revision 1.4  2014/11/24 22:18:54  geurts
// Add striciter protection against non-existing files (bail out), and reduce excessive std output
//
// Revision 1.3  2011/06/01 00:44:31  geurts
// bug fix in boardId calculation
//
// Revision 1.2  2011/05/31 22:53:28  geurts
// Store board-based input files as cell-based database entries
//
// Revision 1.1  2010/12/14 19:27:28  geurts
// *** empty log message ***
//
// ---

#include <iostream>
#include <fstream>
#include <string>
#include "iomanip.h"
using namespace std;

void storetofTotbCorr(const Bool_t mTest = 1)
{
  const int mNTray = 120;  // for test
  const int mNTDIG = 8;
  const int mNVPD = 19;
  const int mNMODULE = 32;
  const int mNCELL = 6;
  const int mNMODPERBOARD = 4;

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
//  TString StoreTime = "2008-02-01 00:00:03";
//  TString StoreTime = "2008-03-04 16:00:01";
//  TString StoreTime = "2009-03-15 00:00:00";
//  TString StoreTime = "2009-11-01 00:00:00";  // 200 GeV preliminary
//  TString StoreTime = "2010-03-18 18:00:00";   // 62 GeV preliminary
//  TString StoreTime = "2010-04-08 15:00:00";   // 39 GeV preliminary
  ifstream inTime;
  inTime.open("input/timestamp");
  string time;
  if (inTime.is_open()) {
    getline(inTime, time);
    inTime.close();
  } else {
    cout << " Unable to open the TimeStamp file! EXIT! " << endl;
    return;
  }
  TString StoreTime = time;
  cout << " Store Time " << StoreTime.Data() << endl;

  //-- add table to the container with descriptor given by Database
  StDbTable* tofTotbCorr = configNode->addDbTable("tofTotbCorr");

  //-- fill structures & store times
  //tofTotbCorr_st *totcorr= new tofTotbCorr_st[mNTray*mNTDIG];
  tofTotbCorr_st *totcorr= new tofTotbCorr_st[23040];

  //=======================================
  // read in pvpdTot file by file.

  Double_t diff[mNVPD*2];
  Double_t toffset;
  ifstream infile;

  //========================================
  //Double_t X[mNTray][mNTDIG][60];
  //Double_t Y[mNTray][mNTDIG][60];
  Double_t X[mNTray][mNMODULE][mNCELL][60];
  Double_t Y[mNTray][mNMODULE][mNCELL][60];

  infile.open("input/totCali_4DB.dat");
  if (!infile.is_open()){
    cerr <<" unable to open input/totCali_4DB.dat; bailing out ..." << endl;
    exit(-1);
  }

  int calibSize;
  infile >> calibSize;
  cout << "reading in " << calibSize << " calibration records ... " << endl;
  switch (calibSize) {
   case 960 : // TDIG BOARD BASED
    for(int i=0;i<mNTray;i++) {
    for(int j=0;j<mNTDIG;j++) {
      int tray, board, nnn;
      infile >> tray >> board;
      infile >> nnn;
      //cout << " tray = " << tray << " board = " << board << endl;
      for(int k=0;k<60;k++) {
	if(nnn>0&&k<nnn+1) {
	  infile >> X[tray-1][board-1][0][k];
	} else {
	  X[tray-1][board-1][0][k] = 0.0;
	}
      }
      for(int k=0;k<60;k++) {
	if(nnn>0&&k<nnn+1) {
	  infile >> Y[tray-1][board-1][0][k];
	} else {
	  Y[tray-1][board-1][0][k] = 0.0;
	}
      }
    }
  }
  break;
  case 23040 : // CELL BASED
    for(int i=0;i<mNTray;i++) {
    for(int j=0;j<mNMODULE;j++) {
    for(int jj=0;jj<mNCELL;jj++) {
      int tray, module, cell, nnn;
      infile >> tray >> module >> cell;
      infile >> nnn;
      //cout << " tray = " << tray << " module = " << module << " cell = " << cell << endl;
      for(int k=0;k<60;k++) {
	if(nnn>0&&k<nnn+1) {
	  infile >> X[tray-1][module-1][cell-1][k];
	} else {
	  X[tray-1][module-1][cell-1][k] = 0.0;
	}
      }
      for(int k=0;k<60;k++) {
	if(nnn>0&&k<nnn+1) {
	  infile >> Y[tray-1][module-1][cell-1][k];
	} else {
	  Y[tray-1][module-1][cell-1][k] = 0.0;
	}
      }
    } //cell
    } //module
    } //tray
  break;
  default:  // DON'T KNOW -- BAIL OUT
    cerr<< "Unknown calib-size " << calibSize << "; bailing out ... " << endl;
    exit(-2);} // switch

  infile.close();

// prepare database records
  cout << "preparing database records ... " << endl;
switch (calibSize) {
case 960:
  int index=-1;
  for (int tray=1;tray<mNTray+1;tray++){
  for (int module=1;module<mNMODULE+1;module++){
  for (int cell=1;cell<mNCELL+1;cell++){
    index++;
    totcorr[index].trayId = (Short_t)tray;
    totcorr[index].moduleId = (Short_t)module;
    totcorr[index].cellId = (Short_t)cell;
    totcorr[index].tdcId = 0;
    int board = ((module-1)/mNMODPERBOARD) + 1 ;
    for(int j=0;j<60;j++) {
      totcorr[index].tot[j] = X[tray-1][board-1][0][j];
      totcorr[index].corr[j] = Y[tray-1][board-1][0][j];   
    }
  } // cell
  } // module
  } // tray
//  for(int i=0;i<mNTray*mNTDIG;i++) {
//    int tray = i/mNTDIG + 1;
//    int board = i%mNTDIG + 1;
//    int module = (i%mNTDIG) * 4 + 1;  // set to be the first module to this board
//    int cell = 1;  // set to 1
//    totcorr[i].trayId = (Short_t)tray;
//    totcorr[i].moduleId = (Short_t)module;
//    totcorr[i].cellId = (Short_t)cell;
//    totcorr[i].tdcId = 0;
//    for(int j=0;j<60;j++) {
//      totcorr[i].tot[j] = X[tray-1][board-1][0][j];
//      totcorr[i].corr[j] = Y[tray-1][board-1][0][j];
//    }
//  }
  break;
case 23040:
  //for(int i=0;i<mNTray*mNMODULE*mNCELL;i++) {
  int index=-1;
  for (int tray=1;tray<mNTray+1;tray++){
  for (int module=1;module<mNMODULE+1;module++){
  for (int cell=1;cell<mNCELL+1;cell++){
    index++;
    totcorr[index].trayId = (Short_t)tray;
    totcorr[index].moduleId = (Short_t)module;
    totcorr[index].cellId = (Short_t)cell;
    totcorr[index].tdcId = 0;
    for(int j=0;j<60;j++) {
      totcorr[index].tot[j] = X[tray-1][module-1][cell-1][j];
      totcorr[index].corr[j] = Y[tray-1][module-1][cell-1][j];
    }
  } // cell
  } // module
  } // tray
  break;
}// switch


// Store records in test file
 cout << "Storing records in totCorr_test.dat (this may take a long time) ... " << endl;
//  int nRow = mNTray * mNTDIG;
//  int nRow = calibSize;
 int nRow = 23040;
  ofstream outData;
  outData.open("totCorr_test.dat");
  for(int i=0;i<nRow;i++) {
    outData << setw(6) << totcorr[i].trayId << setw(6) << totcorr[i].moduleId << setw(6) << totcorr[i].cellId << setw(6) << totcorr[i].tdcId << endl;
    for(int j=0;j<60;j++) {
        outData << setw(15) << totcorr[i].tot[j];
    }
    outData << endl;
    for(int j=0;j<60;j++) {
        outData << setw(15) << totcorr[i].corr[j];
    }
    outData << endl;
  }
  outData.close();

// Store records in databse
  if(!mTest) {
    cout<<" prepare to upload data to DB"<<endl;
    //- store data in table
    tofTotbCorr->SetTable((char*)totcorr, nRow);
    //- set store time
    dbManager->setStoreTime(StoreTime.Data());
    //- store table in dBase
    dbManager->storeDbTable(tofTotbCorr);
    cout<<"uploaded"<<endl;
    //    return 0;
  }

}
