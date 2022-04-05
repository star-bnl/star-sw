// $Id: storevpdTotCorr.C,v 1.3 2019/01/16 16:18:36 geurts Exp $
// macro to upload tofr5 INL tables to database
//
// based on http://www.star.bnl.gov/STAR/comp/db/StoreDbTable.cc.html
//
// Xin Dong, 02/18/2005 
// ---
// $Log: storevpdTotCorr.C,v $
// Revision 1.3  2019/01/16 16:18:36  geurts
// minor updates
//
// Revision 1.2  2014/11/24 22:18:54  geurts
// Add striciter protection against non-existing files (bail out), and reduce excessive std output
//
// Revision 1.1  2010/12/14 19:27:28  geurts
// *** empty log message ***
//
// --

#include <iostream>
#include <fstream>
#include <string>
#include "iomanip.h"
using namespace std;


void storevpdTotCorr(const Bool_t mTest = 1)
{
  const int mNTray = 120;  // for test
  const int mNTDIG = 8;
  const int mNVPD = 19;

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
//  TString StoreTime = "2010-04-24 03:50:00";   // 7.7GeV preliminary
  int corralgo = 1; // 1:enable self-calibrated BTOF; 0=use VPD
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
  StDbTable* vpdTotCorr = configNode->addDbTable("vpdTotCorr");

  //-- fill structures & store times
  vpdTotCorr_st *totcorr= new vpdTotCorr_st[2*mNVPD];

  //=======================================
  // read in pvpdTot file by file.

  Double_t diff[mNVPD*2];
  Double_t toffset;
  Double_t XS[mNVPD*2][128];
  Double_t YS[mNVPD*2][128];
  ifstream infile;

  infile.open("input/vpdCali_4DB.dat");
  if (!infile.is_open()){
    cerr <<" unable to open input/vpdCali_4DB.dat; bailing out ..." << endl;
    exit(-1);
  }

  for(int i=0;i<mNVPD*2;i++) {
    int tubeId, nbins;
    infile>>tubeId;
    infile>>nbins;
    for(int j=0;j<128;j++) {
      if(j<nbins+1) infile>>XS[tubeId-1][j];
      else XS[tubeId-1][j] = 0.0;
    }
    for(int j=0;j<128;j++) {
      if(j<nbins+1) infile>>YS[tubeId-1][j];
      else YS[tubeId-1][j] = 0.0;
    }
  }
  infile.close();

  for(int i=0;i<2*mNVPD;i++) {
    int tubeId = i+1;
    totcorr[i].tubeId = (Short_t)tubeId;
    totcorr[i].corralgo = corralgo;
    for(int j=0;j<128;j++) {
      totcorr[i].tot[j] = (Float_t)XS[i][j];
      totcorr[i].corr[j] = (Float_t)YS[i][j];
    }
  }

  int nRow = 2*mNVPD;
  ofstream outData;
  outData.open("vpdTotCorr_test.dat");
  for(int i=0;i<nRow;i++) {
    outData << setw(6) << totcorr[i].tubeId << endl;
    cout << totcorr[i].corralgo << " ";
    for(int j=0;j<128;j++) {
      if(totcorr[i].tot[j]>1.e-4)
        outData << setw(15) << totcorr[i].tot[j];
    }
    outData << endl;
    for(int j=0;j<128;j++) {
      if(totcorr[i].tot[j]>1.e-4)
        outData << setw(15) << totcorr[i].corr[j];
    }
    outData << endl;
    cout << endl;
  }
  outData.close();

  if(!mTest) {
    cout<<" prepare to upload data to DB"<<endl;
    //- store data in table
    vpdTotCorr->SetTable((char*)totcorr, nRow);
    //- set store time
    dbManager->setStoreTime(StoreTime.Data());
    //- store table in dBase
    dbManager->storeDbTable(vpdTotCorr);
    cout<<"uploaded"<<endl;
      //    return 0;
  }

}
