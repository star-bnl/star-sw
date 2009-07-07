#include "StEmcOfflineCalibrationElectronAnalyzer.h"
#include "TMath.h"
#include "TChain.h"
#include "TNtuple.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/database/StBemcTablesWriter.h"
#include "TFile.h"
#include "StEmcOfflineCalibrationEvent.h"
#include <set>
#include <map>
#include <vector>

ClassImp(StEmcOfflineCalibrationElectronAnalyzer)

void StEmcOfflineCalibrationElectronAnalyzer::analyze(TChain* calib_tree, char* skimfile,char* dbTime)
{

	TFile* skim_file = new TFile(skimfile,"RECREATE");
	TNtuple *ntuple = new TNtuple("nt","electron ntuple","p:teta:tphi:dedx:np:id:rn:eta:energy:r:vz:ht:en3x3:enHN:nTracks");

  StEmcGeom* emcgeom = StEmcGeom::instance("bemc");
  StBemcTablesWriter* bemctables = new StBemcTablesWriter();
  bemctables->loadTables(dbTime,"ofl");


	StEmcOfflineCalibrationEvent* myEvent = new StEmcOfflineCalibrationEvent();
	calib_tree->SetBranchAddress("event_branch",&myEvent);
	StEmcOfflineCalibrationTrack* track = new StEmcOfflineCalibrationTrack();
	
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
		int rn = (int)myEvent->run;
		int ht = 0;
		int oht = 0;
		int nht = 0;
		for(unsigned int k = 0; k < myEvent->triggerIds.size(); k++){
		  int isht = 0;
		  for(unsigned int l = 0; l < HTtrigs.size(); l++){
		    if((int)myEvent->triggerIds[k] == HTtrigs[l]){
		      isht = 1;
		      break;
		    }
		  }
		  if(isht)ht=1;
		  else nht=1;
		}
		//if(nht == 0 && ht == 1) oht=1;
		if(ht == 1)oht=1;
		map<unsigned int, vector< pair<int,int> > >tws;
		tws = myEvent->towersAboveThreshold;
		map<unsigned int, vector< pair<int,int> > >::iterator miter;
		for(miter = tws.begin(); miter != tws.end(); miter++){
		  vector< pair<int,int> > vs;
		  vector< pair<int,int> >::iterator viter;
		  vs = (*miter).second;
		  unsigned int trigID = (*miter).first;
		  for(viter = vs.begin(); viter != vs.end(); viter++){
		    cout<<trigID<<" "<<(*viter).first<<" "<<(*viter).second<<endl;
		  }
		}

		/*
		for(map<unsigned int, unsigned int>::iterator iter = myEvent->triggerResult.begin(); iter != myEvent->triggerResult.end(); ++iter){
		  if((*iter).first < 200000 && (*iter).second == 1)ht = 1;
		}
		*/
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
			//float squarefid = 0.03/TMath::Sqrt(2.0);
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
			if(id!=idx)continue;
			float eta;
			emcgeom->getEta((int)id,eta);
			if(excluded_towers.find(track->tower_id[0]) != excluded_towers.end()) continue;

			//looking for tracks at surrounding towers
			//cout<<"passed basic cuts"<<endl;

			float nnt = 0;
			float en3x3 = energy;
			float enHN = 0;
			if(track->nSigmaElectron > -2.){
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
			  ntuple->Fill(p,teta,tphi,dedx,np,id,rn,eta,energy,dR,vz,oht,en3x3,enHN,nnt);
				nAccept++;
				track->Clear();
			}
		}
		myEvent->Clear();
	}
	skim_file->cd();
	skim_file->Write();
	skim_file->Close();

}
