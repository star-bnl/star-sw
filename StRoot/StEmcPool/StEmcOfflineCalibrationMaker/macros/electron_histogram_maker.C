//Contrary to its name, this code does not make histograms.  Rather, it skims the trees looking for electrons
//and writes them to a slimmer tree
#include <iostream>
#include <fstream>
#include <set>
using namespace std;

void electron_histogram_maker(const char* file_list="",const char* skimfile="electronskimfile.root") 
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

  gSystem->Load("StEmcOfflineCalibrationMaker");
        cout<<"input filelist:  "<<file_list<<endl;
	cout<<"skimmed tree:   "<<skimfile<<endl;
	
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
	
	StEmcOfflineCalibrationEvent* myEvent = new StEmcOfflineCalibrationEvent();
	calib_tree->SetBranchAddress("event_branch",&myEvent);
	StEmcOfflineCalibrationCluster* cluster = new StEmcOfflineCalibrationCluster();
	StEmcOfflineCalibrationTrack* track = new StEmcOfflineCalibrationTrack();
	StEmcOfflineCalibrationTrack* dumtrack = new StEmcOfflineCalibrationTrack();
	
	TFile* skim_file = new TFile(skimfile,"RECREATE");
	TTree *electron_tree = new TTree("skimTree","electron tracks");
	electron_tree->Branch("clusters",&cluster);
	
	//keep track of all hit towers and exclude any with >1 track/tower
	set<int> track_towers;
	set<int> excluded_towers;
		
	unsigned int nAccept = 0;
	unsigned int nGoodEvents = 0;
	unsigned int nentries = calib_tree->GetEntries();
	for(unsigned int i=0; i<nentries; i++){
		if(i%100000 == 0) cout<<"processing "<<i<<" of "<<nentries<<endl;
		//cout<<i<<endl;
		calib_tree->GetEntry(i);
		
		//event level cuts
		if(TMath::Abs(myEvent->vz[0]) > 60.) continue;

		//do a quick loop over tracks to get excluded towers
		track_towers.clear();
		excluded_towers.clear();
		
		for(int j=0; j<myEvent->tracks->GetEntries(); j++){
			track = (StEmcOfflineCalibrationTrack*)myEvent->tracks->At(j);
			int id = track->tower_id[0];
			
			if(track_towers.find(id) != track_towers.end()){
				excluded_towers.insert(id);
			}
			else{
				track_towers.insert(id);
			}
		}   
		
		//now we loop again and look for electrons / hadrons
		for(int j=0; j<myEvent->tracks->GetEntries(); j++){
			track = (StEmcOfflineCalibrationTrack*)myEvent->tracks->At(j);
			
			if(j==0) nGoodEvents++;
			
			double dR = TMath::Sqrt(track->deta*track->deta + track->dphi*track->dphi);
			//if(dR > 0.03) continue;
			//cout<<track->nSigmaElectron<<" "<<track->p<<" "<<track->nHits<<" "<<track->tower_id[0]<<" "<<track->tower_id_exit<<endl;
			float squarefid = 0.03/TMath::Sqrt(2.0);
			if(TMath::Abs(track->deta) > squarefid || TMath::Abs(track->dphi) > squarefid)continue;
			
			if(track->p < 1.5) continue;
			if(track->p > 20.) continue;
			if(track->tower_status[0] != 1)					continue;
			if(track->tower_id[0] != track->tower_id_exit)	continue;
			if(track->nHits < 10)							continue;
			//if(track->vertexIndex != 0)						continue;
			
			if(excluded_towers.find(track->tower_id[0]) != excluded_towers.end()) continue;

			//looking for tracks at surrounding towers
			//cout<<"passed basic cuts"<<endl;

			if(track->nSigmaElectron > -1.){
			  //create start using cluster
			  cluster->centralTrack = *track;

			  for (int k = 1; k < 9; k++)//loop over surrounding towers
			  {
			    if(track_towers.find(track->tower_id[k]) != track_towers.end())//if there's a tower with a track
			      {
				for(int q = 0; q < myEvent->tracks->GetEntries(); q++)//loop over all of the tracks in the event
				  {
				    if(q == j)continue;//continue if we're looking at the same track
				    dumtrack = (StEmcOfflineCalibrationTrack*)myEvent->tracks->At(q);
				    if(dumtrack->tower_id[0] == track->tower_id[k])
				      { 
					cluster->addTrack(dumtrack);
					//cout<<"Track "<<q<<" added to track "<<j<<" of event "<<i<<"."<<endl;
					//continue;//track found so exit from this for loop
				      }
				  }
			      }
			  }
				nAccept++;
				electron_tree->Fill();
				track->Clear();
				cluster->Clear();
			}
		}
		myEvent->Clear();
	}
	
	cout<<"found "<<nGoodEvents<<" events with at least one good track"<<endl;
	cout<<"accepted "<<nAccept<<" electrons"<<endl;
	
	skim_file->cd();
	skim_file->Write();
	skim_file->Close();
}
