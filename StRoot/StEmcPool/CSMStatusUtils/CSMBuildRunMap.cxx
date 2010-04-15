#include "CSMBuildRunMap.h"

#include "TH2.h"
#include "TAxis.h"
#include "TROOT.h"
#include "TKey.h"
#include "TIterator.h"
#include "TFile.h"
#include "TTree.h"
#include "TSystem.h"
#include "TF1.h"
#include "TCanvas.h"
#include "TStyle.h"

#include <iostream>
#include <fstream>
#include <iomanip>
#include <map>
#include <set>
#include <string>
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

using namespace std;

//this opens ALL(!!) of the files in the directory specified
//and looks for histograms containing the phrase bemcStatusAdc
//It then adds up all of the 2d histograms to make one 2d histogram
//for that particular run  FOR EACH DETECTOR
//and puts all of those 2d histograms in an output file called
//directory/run##.calorimeters.hist.root

Int_t 
CSMBuildRunMap::buildRunMap(const Char_t *directory, const Char_t* filter) {

  cout << "CSMBuilRunMap::buildRunMap" << endl;

  std::map<Int_t,std::set<std::string> > runFileMap;
  if (!directory || !filter) return 0;
  void *dir = NULL;
  if ((dir = gSystem->OpenDirectory(directory)) != NULL) {
    const Char_t *dirEntry;
    while ((dirEntry = gSystem->GetDirEntry(dir)) != NULL) {
      Char_t buffer[2048];
      strcpy(buffer,directory);
      if (buffer[strlen(buffer)-1] != '/') strcat(buffer,"/");
      strcat(buffer,dirEntry);
      if (!strstr(buffer,filter)) continue;
      TFile *file = new TFile(buffer,"READ");
      if (file) {
	cout << "Read File buffer Initiatiated" << endl;
        TList *keys = file->GetListOfKeys();
        if (keys) {
          TIterator* iter = keys->MakeIterator();
          TKey* key = NULL;
          while ((key = static_cast<TKey*>(iter->Next())) != NULL) {
            if (strstr(key->GetName(),"bemcStatusAdc") != NULL) {
              Char_t name[2048];
              Short_t keyCycle;
              file->DecodeNameCycle(key->GetName(),name,keyCycle);
              Char_t *runNumberString = name+14;
              cout << runNumberString << endl;
              string s(name);
              Int_t runNumber = atoi(runNumberString);
              runFileMap[runNumber].insert(string(buffer));
            }
          }
        }
      }
      file->Close();
      delete file;
    }
  }
//this just couts the entire map you just acquired
  cout << "found histograms for " << runFileMap.size() << " runs" << endl;
  cout << "dumping run->file map" << endl;
  for (std::map<Int_t,std::set<std::string> >::const_iterator iter = runFileMap.begin();
       iter != runFileMap.end(); ++iter) {
    cout << "run number " << iter->first << endl;
    for (set<string>::const_iterator files = iter->second.begin();
	 files != iter->second.end(); ++files) {
      cout << *files << endl;
    }
  }
  
  Int_t runDate, runTime, minirunDate, minirunTime;
  Float_t runFillNumber, minirunFillNumber;
  for (std::map<Int_t,set<string> >::const_iterator iter = runFileMap.begin();
        iter != runFileMap.end(); ++iter) {
    cout << "Creating status histogram for run " << iter->first << endl;
    Char_t buffer[2048];
    Char_t tempola[100];
    strcpy(buffer,directory);
    sprintf(tempola,"/run%d.cal.total.hist.root",iter->first);
    strcat(buffer,tempola);
    TFile* outFile = new TFile(buffer,"RECREATE");
    if (outFile) cout << "outFile buffer initiated" << endl;
    TTree* runTree = new TTree("calinfo","Extraneous Information");
    runTree->Branch("fillnum",&runFillNumber,"fillnum/F");
    runTree->Branch("thedate",&runDate,"thedate/I");
    runTree->Branch("thetime",&runTime,"thetime/I");
    runDate = 99999999;
    runTime = 999999;
    runFillNumber = 0;
    sprintf(buffer,"bemcStatusAdc_%d",iter->first);
    string BEMChistName(buffer);
    sprintf(buffer,"bemcStatusEnergy_%d",iter->first);
    string BEMCenergyHistName(buffer);
    sprintf(buffer,"eemcStatusAdc_%d",iter->first);
    string EEMChistName(buffer);
    sprintf(buffer,"eemcStatusEnergy_%d",iter->first);
    string EEMCenergyHistName(buffer);
    //cout << "reading " << bemchistName << " and " << bemcenergyHistName << endl;
    //    TH2F* myEEMCRunHist = NULL; //D.Staszak
    //    TH2F* myEEMCEnergyRunHist = NULL;
    TH2F* myBEMCRunHist = NULL;
    //    TH2F* myBEMCEnergyRunHist = NULL;
    for (set<string>::const_iterator filenames = iter->second.begin();
          filenames != iter->second.end(); ++filenames) {
      TFile *file = new TFile(filenames->c_str(),"READ");
      if (file->IsOpen()) {
        TTree* minirunTree = dynamic_cast<TTree*>(file->Get("calinfo"));
        if(minirunTree) {
          minirunTree->SetBranchAddress("thedate",&minirunDate);
          minirunTree->SetBranchAddress("thetime",&minirunTime);
          minirunTree->SetBranchAddress("fillnum",&minirunFillNumber);
          minirunTree->GetEvent(0);
        } else {
          assert(minirunTree);  //uh, where's the tree?
        }
        if(runDate > minirunDate) {
          runDate = minirunDate;
          runTime = minirunTime;
        } else if(runDate == minirunDate && runTime > minirunTime) {
          runTime = minirunTime;
        }
        runFillNumber = minirunFillNumber;
        TH2F* BEMChist = dynamic_cast<TH2F*>(file->Get(BEMChistName.c_str()));
        if (BEMChist) {
	  if (BEMChist->Integral(3,BEMChist->GetXaxis()->GetNbins(),1,2) >1e5) cout << "File " << filenames->c_str() << "  has a ton of 0's" << endl; // D.Staszak
          if (!myBEMCRunHist) {
            outFile->cd();
            myBEMCRunHist = new TH2F(BEMChistName.c_str(),BEMChistName.c_str(),
                                 BEMChist->GetXaxis()->GetNbins(),
                                 BEMChist->GetXaxis()->GetXmin(),
                                 BEMChist->GetXaxis()->GetXmax(),
                                 BEMChist->GetYaxis()->GetNbins(),
                                 BEMChist->GetYaxis()->GetXmin(),
                                 BEMChist->GetYaxis()->GetXmax());
          }
          myBEMCRunHist->Add(BEMChist);
        } else {
          cerr << "Didn't find histogram " << BEMChistName << " ???" << endl;
          cerr << "in file " << filenames->c_str() << " ???" << endl;
        }
	/*
        TH2F* BEMCenergyHist = dynamic_cast<TH2F*>(file->Get(BEMCenergyHistName.c_str()));
        if (BEMCenergyHist) {
          if (!myBEMCEnergyRunHist) {
            outFile->cd();
            myBEMCEnergyRunHist = new TH2F(BEMCenergyHistName.c_str(),BEMCenergyHistName.c_str(),
                                       BEMCenergyHist->GetXaxis()->GetNbins(),
                                       BEMCenergyHist->GetXaxis()->GetXmin(),
                                       BEMCenergyHist->GetXaxis()->GetXmax(),
                                       BEMCenergyHist->GetYaxis()->GetNbins(),
                                       BEMCenergyHist->GetYaxis()->GetXmin(),
                                       BEMCenergyHist->GetYaxis()->GetXmax());
          }
          myBEMCEnergyRunHist->Add(BEMCenergyHist);
        } else {
          cerr << "Didn't find histogram " << BEMCenergyHistName << " ???" << endl;
        }
        TH2F* EEMChist = dynamic_cast<TH2F*>(file->Get(EEMChistName.c_str()));
        if (EEMChist) {
          if (!myEEMCRunHist) {
            outFile->cd();
            myEEMCRunHist = new TH2F(EEMChistName.c_str(),EEMChistName.c_str(),
                                 EEMChist->GetXaxis()->GetNbins(),
                                 EEMChist->GetXaxis()->GetXmin(),
                                 EEMChist->GetXaxis()->GetXmax(),
                                 EEMChist->GetYaxis()->GetNbins(),
                                 EEMChist->GetYaxis()->GetXmin(),
                                 EEMChist->GetYaxis()->GetXmax());
          }
          myEEMCRunHist->Add(EEMChist);
        } else {
          cerr << "Didn't find histogram " << EEMChistName << " ???" << endl;
	  }
	*/
      }
      file->Close();
      delete file;
    }
    runTree->Fill();
    cout << "Writing myBEMCRunHist" << endl;
    myBEMCRunHist->Write();
    //    cout << "Writing myEEMCRunHist" << endl;
    //    myEEMCRunHist->Write();
    cout << "Writing outFile" << endl;
    outFile->Write();
    outFile->Close();
    delete outFile;
  } 
  return 0;
}

ClassImp(CSMBuildRunMap)
