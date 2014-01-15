#include "StEmcOfflineCalibrationElectronAnalyzer.h"
#include "TMath.h"
#include "TChain.h"
#include "TNtuple.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/database/StBemcTablesWriter.h"
#include "TFile.h"
#include "TH2.h"
#include "StEmcOfflineCalibrationEvent.h"
#include <set>
#include <map>
#include <vector>
#include "TMath.h"
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
    if(i > 1000)break;
    if(i%100000 == 0) cout<<"processing "<<i<<" of "<<nentries<<endl;
    //cout<<i<<endl;
    calib_tree->GetEntry(i);
    
    //event level cuts
    float vz = myEvent->vz[0];
    int rn = (int)myEvent->run;
    int ht = 0;
    int oht = 0;
    int nht = 0;
    //cout<<i<<" "<<myEvent->towersAboveThreshold.size()<<endl;
    for(unsigned int k = 0; k < myEvent->triggerIds.size(); k++){
      int isht = 0;
      cout<<i<<" "<<myEvent->triggerIds[k]<<endl;
      for(unsigned int l = 0; l < HTtrigs.size(); l++){
	cout<<i<<" "<<HTtrigs[l]<<" "<<myEvent->triggerResult[HTtrigs[l]]<<" "<<myEvent->towersAboveThreshold[HTtrigs[l]].size()<<endl;
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
      //cout<<i<<" "<<trigID<<" "<<vs.size()<<endl;
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
    cout<<i<<" "<<nAccept<<endl;
    myEvent->Clear();
  }
  skim_file->cd();
  skim_file->Write();
  skim_file->Close();
  
}


void StEmcOfflineCalibrationElectronAnalyzer::analyzeTree(TChain* calib_tree, char* skimfile,char* dbTime)
{

  StEmcGeom* emcgeom = StEmcGeom::instance("bemc");
  StBemcTablesWriter* bemctables = new StBemcTablesWriter();
  bemctables->loadTables(dbTime,"ofl");

  StEmcOfflineCalibrationEvent* myEvent = new StEmcOfflineCalibrationEvent();
  calib_tree->SetBranchAddress("event_branch",&myEvent);
  StEmcOfflineCalibrationCluster* cluster = new StEmcOfflineCalibrationCluster();
  StEmcOfflineCalibrationTrack* track = new StEmcOfflineCalibrationTrack();
  StEmcOfflineCalibrationTrack* dumtrack = new StEmcOfflineCalibrationTrack();

  TFile* skim_file = new TFile(skimfile,"RECREATE");

  TTree *electron_tree = new TTree("skimTree","electron tracks");
  electron_tree->Branch("clusters",&cluster);
  
  TH2F* dEdxvsp = new TH2F("dEdxvsp","",100,0.0,10.0,100,0.0,1e-5);

  //keep track of all hit towers and exclude any with >1 track/tower
  set<int> track_towers;
  set<int> excluded_towers;
  
  unsigned int nAccept = 0;
  unsigned int nGoodEvents = 0;
  unsigned int nentries = calib_tree->GetEntries();
  int ngoodhit = 0;
  int nplt10 = 0;
  int nnosis = 0;
  int nfinal = 0;
  int nbsmdgood = 0;
  int nnottrig = 0;
  int nfidu = 0;
  int nenterexit = 0;
  for(unsigned int i=0; i<nentries; i++){
    if(i%100000 == 0) cout<<"processing "<<i<<" of "<<nentries<<endl;
    //cout<<i<<endl;
    calib_tree->GetEntry(i);
    
    //event level cuts
    if(TMath::Abs(myEvent->vz[0]) > 60.) continue;
    

    // identify HT triggers
    int nht = 0;
    int yht = 0;
    //cout << myEvent->triggerIds.size() << ":  ";
    for(unsigned int h = 0; h < myEvent->triggerIds.size(); h++){
      int isht = 0;
      //cout << myEvent->triggerIds[h] << " "; //debug
      for(unsigned int f = 0; f < HTtrigs.size(); f++){
	if(HTtrigs[f] == (int)myEvent->triggerIds[h])isht = 1;
      }
      if(!isht)nht = 1;
      if(isht)yht = 1;
    }
    //cout << "   " << nht << " " << yht << endl;

    vector<int> HTtowers;

    if(yht){
      map<unsigned int, vector< pair<int,int> > >tws;
      tws = myEvent->towersAboveThreshold;
      map<unsigned int, vector< pair<int,int> > >::iterator miter;
      for(miter = tws.begin(); miter != tws.end(); miter++){
	vector< pair<int,int> > vs;
	vector< pair<int,int> >::iterator viter;
	vs = (*miter).second;
	unsigned int trigID = (*miter).first;
	//cout<<i<<" "<<trigID<<" "<<vs.size()<<endl;
	for(viter = vs.begin(); viter != vs.end(); viter++){
	  HTtowers.push_back((*viter).first);
	  //cout<<trigID<<" "<<(*viter).first<<" "<<(*viter).second<<endl;
	}
      }
    }

    // identify TOF triggers
    int istoftrig = 0;
    for(unsigned int h = 0; h < myEvent->triggerIds.size(); h++){
      int istof = 0;
      for(unsigned int f = 0; f < TOFtrigs.size(); f++){
	if(TOFtrigs[f] == (int)myEvent->triggerIds[h])istof = 1;
      }
      if(istof)istoftrig = 1;
    }



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
      dEdxvsp->Fill(track->p,track->dEdx);
      if(j==0) nGoodEvents++;
      if(myEvent->ranking[track->vertexIndex] < 0)continue;
      double dR = TMath::Sqrt(track->deta*track->deta + track->dphi*track->dphi);
      float squarefid = 0.03/TMath::Sqrt(2.0);
      
      if(track->p < 1.5) continue;
      if(track->p > 20.) continue;
      ngoodhit++;
      if(track->tower_status[0] != 1)					continue;
      nenterexit++;
      //if(track->tower_id[0] != track->tower_id_exit)	continue;
      if(track->nHits < 10)							continue;
      nplt10++;
      //if(track->vertexIndex != 0)						continue;
      
      if(excluded_towers.find(track->tower_id[0]) != excluded_towers.end()) continue;
      nfidu++;
      if((track->tower_adc[0] - track->tower_pedestal[0]) < 1.5 * track->tower_pedestal_rms[0])continue;
      nnottrig++;
      //cout<<track->dEdx<<endl;
      if(track->dEdx < 3.0e-6)continue;
      nbsmdgood++;
      //looking for tracks at surrounding towers
      //cout<<"passed basic cuts"<<endl;
      
      //if(track->nSigmaElectron > -2.){
      //create start using cluster
      //cluster->centralTrack = *track;

      float toweta,towphi;
      emcgeom->getEtaPhi(track->tower_id[0],toweta,towphi);
      
      // HT triggers
      int tr = 0;
      if(yht){
	for(int ttt = 0; ttt < (int)HTtowers.size(); ttt++){
	  float trigeta,trigphi;
	  emcgeom->getEtaPhi(HTtowers[ttt],trigeta,trigphi);
	  float tcdeta = fabs(toweta - trigeta);
	  float tcdphi = fabs(towphi - trigphi);
	  if(tcdphi > TMath::Pi())tcdphi = 2*TMath::Pi() - tcdphi;
	  float tcdR = sqrt(pow(tcdeta,2) + pow(tcdphi,2));
	  if(tcdR < 0.7)tr = 1;
	  //cout<<i<<" "<<trigeta<<" "<<trigphi<<" "<<tcdR<<" "<<yht<<" "<<tr<<" "<<nht<<endl;
	}
      }

      
      track->htTrig = yht + tr;
      track->nonhtTrig = nht;
      //(cluster->centralTrack).htTrig = yht + tr;
      //(cluster->centralTrack).nonhtTrig = nht;
      //cout<<track->htTrig<<endl;

      track->tofTrig = 0;
      if(istoftrig)track->tofTrig = 1;

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
      //cout<<i<<" "<<yht<<" "<<tr<<" "<<nht<<endl;
      track->Clear();
      cluster->Clear();
      //}
    }
    myEvent->Clear();
  }
  
  cout<<"found "<<nGoodEvents<<" events with at least one good track"<<endl;
  cout<<"accepted "<<nAccept<<" electrons"<<endl;
  //cout<<"ngoodhit: "<<ngoodhit<<endl;
  //cout<<"nenterexit: "<<nenterexit<<endl;
  //cout<<"n p < 10: "<<nplt10<<endl;
  //cout<<"n in fidu: "<<nfidu<<endl;
  //cout<<"n not trig: "<<nnottrig<<endl;
  //cout<<"nbsmdhit: "<<nbsmdgood<<endl;
  //cout<<"n no sis: "<<nnosis<<endl;
  //cout<<"n final: "<<nfinal<<endl;
  skim_file->cd();
  skim_file->Write();
  skim_file->Close();

}
