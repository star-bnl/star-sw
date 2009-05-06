//Contrary to its name, this code does not make histograms.  Rather, it skims the trees looking for electrons
//and writes them to a slimmer tree
#include <iostream>
#include <fstream>
#include <set>
#include <pair>
#include <map>

using namespace std;

void electron_ntuple_maker(const char* file_list="",const char* skimfile="electronskimfile.root") 
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

	char* dbtime = "2008-02-28 00:00:00";
	StEmcOfflineCalibrationElectronAnalyzer* ana = new StEmcOfflineCalibrationElectronAnalyzer;

	ana->HTtrigs.push_back(220500);
	ana->HTtrigs.push_back(220510);
	ana->HTtrigs.push_back(220520);

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
	ana->analyze(calib_tree,skimfile,dbtime);

	/*
  StEmcGeom* emcgeom = StEmcGeom::instance("bemc");
  StBemcTablesWriter* bemctables = new StBemcTablesWriter();
  bemctables->loadTables("2006-05-31 00:00:00","ofl");
        cout<<"input filelist:  "<<file_list<<endl;
	cout<<"skimmed tree:   "<<skimfile<<endl;

	StEmcOfflineCalibrationEvent* myEvent = new StEmcOfflineCalibrationEvent();
	calib_tree->SetBranchAddress("event_branch",&myEvent);
	StEmcOfflineCalibrationTrack* track = new StEmcOfflineCalibrationTrack();
	StEmcOfflineCalibrationTrack* dumtrack = new StEmcOfflineCalibrationTrack();
	
	TFile* skim_file = new TFile(skimfile,"RECREATE");
	TNtuple *ntuple = new TNtuple("nt","electron ntuple","p:teta:tphi:dedx:np:id:idx:eta:energy:r:vz:ht:en3x3:enHN:nTracks");
	
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
		float vz = myEvent->vz[0];

		int ht = 0;
		for(map<int, int>::iterator iter = (map<int, int>::iterator)myEvent->triggerResult.begin(); iter != (map<int, int>::iterator)myEvent->triggerResult.end(); ++iter){
		  if((*iter).first < 200000 && (*iter).second == 1)ht = 1;
		}

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
			//if(TMath::Abs(track->deta) > squarefid || TMath::Abs(track->dphi) > squarefid)continue;
			
			float p = track->p;
			float teta = track->track.pseudoRapidity();
			float tphi = track->track.phi();
			float np = track->nHits;
			float dedx = track->dEdx;
			float energy = (track->tower_adc[0]-track->tower_pedestal[0])*bemctables->calib(1,track->tower_id[0]);
			if(energy < 0)energy = 0;
			if(track->tower_status[0] != 1)					continue;
			float id = track->tower_id[0];
			float idx = track->tower_id_exit;
			float eta;
			emcgeom->getEta(id,eta);
			if(excluded_towers.find(track->tower_id[0]) != excluded_towers.end()) continue;

			//looking for tracks at surrounding towers
			//cout<<"passed basic cuts"<<endl;

			float nnt = 0;
			float en3x3 = energy;
			float enHN = 0;
			if(track->nSigmaElectron > -5.){
			  //create start using cluster

			  for (int k = 1; k < 9; k++)//loop over surrounding towers
			  {
			    float tenergy = (track->tower_adc[k]-track->tower_pedestal[k])*bemctables->calib(1,track->tower_id[k]);
			    if(tenergy > 0)en3x3+=tenergy;
			    if(tenergy > enHN)enHN = tenergy;
			    if(track_towers.find(track->tower_id[k]) != track_towers.end())//if there's a tower with a track
			      {
				nnt++;
			      }
			  }
			  nt->Fill(p,teta,tphi,dedx,np,id,idx,eta,energy,dR,vz,ht,en3x3,enHN,nnt);
				nAccept++;
				track->Clear();
			}
		}
		myEvent->Clear();
	}
	
	
	cout<<"found "<<nGoodEvents<<" events with at least one good track"<<endl;
	cout<<"accepted "<<nAccept<<" electrons"<<endl;
	
	skim_file->cd();
	skim_file->Write();
	skim_file->Close();
*/
}
