#include <iostream>
#include <fstream>
#include <set>
using namespace std;

void mip_histogram_maker(const char* file_list="", const char* skimfile="mipskimfile.root") 
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
  gSystem->Load("StEEmcUtil");
  gSystem->Load("StAssociationMaker");
  //gSystem->Load("StEmcTriggerMaker");
  gSystem->Load("StTriggerUtilities");
  gSystem->Load("StEmcOfflineCalibrationMaker");

  const int ntowers=4800;
  const bool lookForSwaps=false;

  cout<<"input filelist:  "<<file_list<<endl;
  cout<<"histogram file:  "<<skimfile<<endl;
	
	//chain all input files together
  char file[300];
  TChain* calib_tree = new TChain("calibTree");
  ifstream filelist(file_list);
  TFile *test_file;
  while(1){
    filelist >> file;
    if(!filelist.good()) break;
    cout<<file<<endl;
    calib_tree->Add(file);
  }
	
  StEmcOfflineCalibrationEvent* myEvent = new StEmcOfflineCalibrationEvent();
  calib_tree->SetBranchAddress("event_branch",&myEvent);
  StEmcOfflineCalibrationTrack* mip;
	
	//create the 4800 mip histograms
  TH2F* mapcheck = new TH2F("mapcheck","check mapping",4800,0.5,4800.5,4800,0.5,4800.5);
  mapcheck->SetXTitle("Track Projection ID");
  mapcheck->SetYTitle("Tower hit above 5 rms");
  TH1* mip_histo[ntowers];
  char name[100];
  for(int k=0; k<ntowers; k++){
    sprintf(name,"mip_histo_%i",k+1);
    mip_histo[k] = new TH1D(name,name,250,-50.5,199.5);
  }
	
	//keep track of all hit towers and exclude any with >1 track/tower
  set<int> track_towers;
  set<int> excluded_towers;
  int ntracks = 0;
  int ngood = 0;
  unsigned int nentries = calib_tree->GetEntries();
  for(unsigned int i=0; i<nentries; i++){
    if(i%100000 == 0) cout<<"processing "<<i<<" of "<<nentries<<endl;
		
    track_towers.clear();
    excluded_towers.clear();
		
    calib_tree->GetEntry(i);
	
		//we're only working with events from vertexIndex==0
    if(TMath::Abs(myEvent->vz[0]) > 30.)	continue;
		
		//do a quick loop over tracks to get excluded towers
    for(int j=0; j<myEvent->tracks->GetEntries(); j++){
      mip = (StEmcOfflineCalibrationTrack*)myEvent->tracks->At(j);
      int id = mip->tower_id[0];

      if(track_towers.find(id) != track_towers.end()){
	excluded_towers.insert(id);
      }
      else{
	track_towers.insert(id);
      }
    }
		
		//select on runnumbers to look for stability
		//if(myEvent->run < 7135000) continue;
    //if(myEvent->tracks->GetEntries() > 0)cout<<"Processing event "<<i<<" with "<<myEvent->tracks->GetEntries()<<" tracks"<<endl;
    for(int j=0; j<myEvent->tracks->GetEntries(); j++){
      mip = (StEmcOfflineCalibrationTrack*)myEvent->tracks->At(j);
      ntracks++;
      double pedsub = mip->tower_adc[0] - mip->tower_pedestal[0];
			
      //cout<<mip->tower_id[0]<<" "<<mip->tower_id_exit<<" "<<pedsub<<" "<<mip->tower_pedestal_rms[0]<<" "<<mip->p<<" "<<mip->highest_neighbor<<" "<<mip->vertexIndex<<endl;
      if(excluded_towers.find(mip->tower_id[0]) != excluded_towers.end()) continue;
      if(mip->p < 1.) continue;
      //if(mip->preshower_status[0] != 1) continue;
      if(mip->tower_id[0] != mip->tower_id_exit) continue;

      for(int k = 0; k < 9; k++){
	if(mip->tower_adc[k] - mip->tower_pedestal[k] < 5*mip->tower_pedestal_rms[k])continue;
	mapcheck->Fill(mip->tower_id[0],mip->tower_id[k]);
      }

      if(mip->highest_neighbor > 2.) continue;
      if(pedsub < 1.5*mip->tower_pedestal_rms[0])continue;
      //if(mip->vertexIndex > 0) continue;
			
      int index = mip->tower_id[0];
      mip_histo[index-1]->Fill(pedsub);
      ngood++;
      //cout<<"track found in tower "<<index<<endl;
    }
  }
	
  cout<<"Added "<<ngood<<" tracks of "<<ntracks<<endl;
  TFile* output_file = new TFile(skimfile,"RECREATE");
  for(int k=0; k<ntowers; k++) mip_histo[k]->Write();
  mapcheck->Write();
  output_file->Close();
}
