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
#include <iomanip>
using namespace std;


void storetofINLSCorrStep1(int i1=0, int i2=300)
//int main(int argc, char *argv[])

{

  const Int_t NMAX = 1400;
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
  TString ZStoreTime = "2009-09-01 00:00:00";

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

  ofstream testData;
  char testname[100];
  sprintf(testname,"write_%d_%d.list",i1,i2);
  testData.open(testname);

  ofstream inputData;
  char inputname[100];
  sprintf(inputname,"input_%d_%d.list",i1,i2);
  inputData.open(inputname);
//  for(int iboard=0;iboard<NMAX;iboard++) {
  for(int iboard=i1;iboard<i2;iboard++) {

    //char *dirpfx = "/gpfs01/star/scratch/geurts/inl/Run10/dbase/INL";
    //char *dirpfx = "/gpfs01/star/i_rice/geurts/database/Run13/INL";
    char *dirpfx = "/gpfs01/star/i_rice/geurts/database/Run15/";
    char fulldir[200];
    char dirname[100];
    char sfx[9][10] = {"",".R",".RC",".RRC",".redo",".n","redo","n",".RRR"};

    int iv = -1;
    for(int j=0;j<9;j++) {
      sprintf(dirname,"tdig-ser%d%s",iboard,sfx[j]);
      sprintf(fulldir,"%s/%s",dirpfx,dirname);
      void *dir = gSystem->OpenDirectory(gSystem->ExpandPathName(fulldir));
      if(!dir) continue;

      iv = j;
      break;
    }

    if(iv<0||iv>=9) {
      cout << " No available table for board # " << iboard << endl;
      continue;
    }
    sprintf(dirname,"tdig-ser%d%s",iboard,sfx[iv]);
    sprintf(fulldir,"%s/%s",dirpfx,dirname);

    cout << " Opening directory " << dirname << " for input... " << endl;
    inputData << iboard << "\t" << dirname << endl;

    Short_t corr[NTDC*NCHAN*1024];

    for(int itdc=0;itdc<NTDC;itdc++){
      for(int ichan=0;ichan<NCHAN;ichan++) {

	char filename[512];
	sprintf(filename,"%s/inl/%s.tdc%d.ch%d.inl",fulldir,dirname,itdc,ichan);

	char hisname[512];
	sprintf(hisname,"b_%d_tdc_%d_chan_%d",iboard,itdc,ichan);
	hINLSCorr[iboard][itdc][ichan] = new TH1S(hisname,hisname,1024,0.,1024.);

	inlcorr[NFILL].tdigId = (Short_t)iboard;
	inlcorr[NFILL].tdcChanId = (Short_t)(itdc*NCHAN+ichan);

	ifstream infile(filename);
	if(!infile) {
	  cout<<"Can not open "<<filename<<" Please check!!!"<<endl;
	  return -1;
	}
        cout << " +++ Open file " << filename << " for input " << endl;

	for(int j=0;j<1024;j++) {
	  float bin, data;
	  infile >> bin >> data;
	  inlcorr[NFILL].INLCorr[j]=(Short_t)(data*100.);
          corr[itdc*NCHAN*1024+ichan*1024+j] = (Short_t)(data*100.);
	  if(fabs(j-bin)>0.6) {
	    cout << " weird!!! " << endl;
	    return;
	  }
	  //	  cout<<"readin bin="<<bin<<" ibin="<<j<<" data="<<data<<endl;
	  hINLSCorr[iboard][itdc][ichan]->SetBinContent(j+1,inlcorr[NFILL].INLCorr[j]);
	}

	infile.close();

	NFILL++;
      } // end of loop channel
    }  // end of loop TDC

    char outname[200];
    sprintf(outname,"%s/inl/%s_24.inl",fulldir,dirname);
    ofstream outfile(outname);
    cout << " Writing to file " << outname << endl;
    for(int ic=0;ic<1024;ic++) {
      for(int it=0;it<NTDC*NCHAN;it++) {
        outfile << setw(10) << corr[it*1024+ic];
      }
      outfile << endl;
    }

    outfile.close();

    cout<<"================================================="<<endl;
    NBOARD++;
    hFlag[iboard] = true;
    testData << iboard << "\t" << dirname << endl;

  } // end of loop board
  testData.close();
  inputData.close();
  cout<<" prepare to upload data to DB NFILL="<<NFILL<<" NBOARD="<<NBOARD<<endl;

  char rootname[100];
  sprintf(rootname,"INL_test_%d_%d.root",i1,i2);
  TFile *fout = new TFile(rootname,"recreate");
  int index = 0;
  for(int iboard=0;iboard<NMAX;iboard++) {
    if(!hFlag[iboard]) continue;

    for(int itdc=0;itdc<NTDC;itdc++){
      for(int ichan=0;ichan<NCHAN;ichan++) {
	if(!hINLSCorr[iboard][itdc][ichan]) continue;
	hINLSCorr[iboard][itdc][ichan]->Write();
/*
	cout << " Ids = " << iboard << " " << itdc << " " << ichan << endl;
	cout << " IN Ids = " << inlcorr[index].tdigId << " " << inlcorr[index].tdcChanId << endl;
	for(int j=0;j<1024;j++) {
	  if(j%100==0)
	    cout << " INL j = " << j << " " << inlcorr[index].INLCorr[j] << endl;
	}
*/
	index++;
      }
    }
  }
  fout->Close();

  //- store data in table
/*
  tofINLSCorr->SetTable((char*)inlcorr, NFILL);
  //- set store time
  dbManager->setStoreTime(ZStoreTime.Data());
  //- store table in dBase
  cout<<" here "<<endl;
  dbManager->storeDbTable(tofINLSCorr);
  cout<<"uploaded"<<endl;
      //    return 0;
*/
}
