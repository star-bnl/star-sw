//Contrary to its name, this code does not make histograms.  Rather, it skims the trees looking for electrons
//and writes them to a slimmer tree
#include <iostream>
#include <fstream>
#include <set>
#include <pair>
#include <map>

using namespace std;

void electron_tree_maker(const char* file_list="ctest3.list",const char* skimfile="electronskimfile.root") 
{
  gROOT->Macro("LoadLogger.C");
  gROOT->Macro("loadMuDst.C");
  gSystem->Load("StTpcDb");
  gSystem->Load("StDaqLib");
  gSystem->Load("StDetectorDbMaker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StDbUtilities");
  gSystem->Load("StEmcRawMaker");
  gSystem->Load("StMcEvent");
  gSystem->Load("StMcEventMaker");//***
  gSystem->Load("StEmcSimulatorMaker");//***
  gSystem->Load("StEmcADCtoEMaker");
  gSystem->Load("StEpcMaker");
  gSystem->Load("StDbBroker");
  gSystem->Load("StEmcUtil");
  gSystem->Load("StAssociationMaker");
  gSystem->Load("StEmcTriggerMaker");
  gSystem->Load("StTriggerUtilities");
  gSystem->Load("StEmcOfflineCalibrationMaker");

		//chain all input files together
  char file[300];
  TChain* calib_tree = new TChain("calibTree");
  ifstream filelist(file_list);
  while(1){
    filelist >> file;
    if(!filelist.good()) break;
    cout<<file<<endl;
    calib_tree->Add(file);
  }
  
  char* dbtime = "2009-03-28 00:00:00";
  StEmcOfflineCalibrationElectronAnalyzer* ana = new StEmcOfflineCalibrationElectronAnalyzer;
  
  //2011
  /*ana->HTtrigs.push_back(320500);
  ana->HTtrigs.push_back(320501);
  ana->HTtrigs.push_back(320503);
  ana->HTtrigs.push_back(320504);
  ana->HTtrigs.push_back(320514);
  ana->HTtrigs.push_back(320524);
  ana->HTtrigs.push_back(330501);
  ana->HTtrigs.push_back(330503);
  ana->HTtrigs.push_back(330524);
  ana->HTtrigs.push_back(320801);
  ana->HTtrigs.push_back(330801);

  ana->TOFtrigs.push_back(320300);
  ana->TOFtrigs.push_back(320301);
  ana->TOFtrigs.push_back(320302);
  ana->TOFtrigs.push_back(320311);
  ana->TOFtrigs.push_back(330311);
  ana->TOFtrigs.push_back(330322);
  ana->TOFtrigs.push_back(320312);
  ana->TOFtrigs.push_back(320322);*/

  //2009
  ana->HTtrigs.push_back(240530);
  ana->HTtrigs.push_back(240540);
  ana->HTtrigs.push_back(240550);
  ana->HTtrigs.push_back(240560);
  ana->HTtrigs.push_back(240570);
  
  
  /*2008
    ana->HTtrigs.push_back(220500);
    ana->HTtrigs.push_back(220510);
    ana->HTtrigs.push_back(220520);
  */
  /*2006
    ana->HTtrigs.push_back(117211);
    ana->HTtrigs.push_back(117212);
    ana->HTtrigs.push_back(117611);
    ana->HTtrigs.push_back(117821);
    ana->HTtrigs.push_back(127212);
    ana->HTtrigs.push_back(127213);
    ana->HTtrigs.push_back(127611);
    ana->HTtrigs.push_back(127821);
    ana->HTtrigs.push_back(137213);
    ana->HTtrigs.push_back(137611);
    ana->HTtrigs.push_back(137821);
  */
  ana->analyzeTree(calib_tree,skimfile,dbtime);
  
  
}
