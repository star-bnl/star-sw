//Contrary to its name, this code does not make histograms.  Rather, it skims the trees looking for electrons
//and writes them to a slimmer tree
#include <iostream>
#include <fstream>
#include <vector>
#include <set>
//#include <StTriggerUtilities/L2Emulator/L2wAlgo/L2wResult2009.h>
using namespace std;

struct L2wResult2009 { // must be N*4 bytes
  enum {mySizeChar=8};// negotiate size w/ Ross before extending 
  unsigned char seedEt;      // seed Et with 60Gev Max.  bits=Et*256/60
  unsigned char clusterEt;   // cluster Et with 60Gev Max.  bits=Et*256/60
  unsigned char seedEtaBin;  // iEta bin 
  unsigned char seedPhiBin;  // iPhi bin 

  unsigned char trigger;     // bit0=rnd, bit1=ET>thr
  unsigned char dum1,dum2,dum3;
};


void electron_histogram_maker(const char* file_list="ctest.list",const char* skimfile="electronskimfile.root") 
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
  //gSystem->Load("StEmcTriggerMaker");
  gSystem->Load("StTriggerUtilities");
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
  
  StEmcGeom* geom = StEmcGeom::instance("bemc");
  
  
  vector<unsigned int> htTriggers;
  htTriggers.push_back(240530);
  htTriggers.push_back(240530);
  htTriggers.push_back(240550);
  htTriggers.push_back(240560);
  htTriggers.push_back(240570);
  
  /*
    htTriggers.push_back(230531);
    htTriggers.push_back(230601);
  */
  
  /*
    htTriggers.push_back(220500);
    htTriggers.push_back(220510);
    htTriggers.push_back(220520);
  */
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
    if(i%1000000 == 0) cout<<"processing "<<i<<" of "<<nentries<<endl;
    //cout<<i<<endl;
    calib_tree->GetEntry(i);
    
    //event level cuts
    if(TMath::Abs(myEvent->vz[0]) > 60.) continue;
    
    //TArrayI& l2Array = myEvent->l2Result;
    //unsigned int *l2res=(unsigned int *)l2Array.GetArray();
    //printf(" L2-jet online results below:\n");
    const int BEMCW_off=20; // valid only for 2009 run
    //L2wResult2009* l2wresult = NULL;
    //l2wresult = (L2wResult2009*) &l2res[BEMCW_off];
    //int l2wbit = (l2wresult->trigger&2)>0;
    //int l2wrnd = (l2wresult->trigger&1)>0;
    float trigeta = -999;
    float trigphi = -999;
    
    //cout<<i<<" "<<l2wbit<<" "<<l2wrnd<<endl;

    /*    
    if(l2wbit || l2wrnd){
      trigeta = (float)(l2wresult->seedEtaBin - 1)*0.05-1 + 0.025;
      int kPhi = l2wresult->seedPhiBin;
      if(kPhi > 24)kPhi -= 120;
      trigphi = (float)(24 - kPhi)*TMath::Pi()/60.;
      if(trigphi > TMath::Pi())trigphi -= 2*TMath::Pi();
    }
    */
    int nht = 0;
    int yht = 0;
    for(unsigned int h = 0; h < myEvent->triggerIds.size(); h++){
      int isht = 0;
      for(unsigned int f = 0; f < htTriggers.size(); f++){
	if(htTriggers[f] == myEvent->triggerIds[h])isht = 1;
      }
      if(!isht)nht = 1;
      if(isht)yht = 1;
      //cout<<i<<" "<<myEvent->triggerIds[h]<<endl;
    }
    //cout<<i<<" "<<nht<<" "<<yht<<endl;
    //if(!nht && yht)continue;
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
      dEdxvsp->Fill(track->p,track->dEdx);
      if(j==0) nGoodEvents++;
			
      double dR = TMath::Sqrt(track->deta*track->deta + track->dphi*track->dphi);
      //if(dR > 0.03) continue;
      //cout<<track->nSigmaElectron<<" "<<track->p<<" "<<track->nHits<<" "<<track->tower_id[0]<<" "<<track->tower_id_exit<<endl;
      float squarefid = 0.03/TMath::Sqrt(2.0);
      //if(TMath::Abs(track->deta) > squarefid || TMath::Abs(track->dphi) > squarefid)continue;
      
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
      cluster->centralTrack = *track;

      float toweta,towphi;
      geom->getEtaPhi(track->tower_id[0],toweta,towphi);
      
      int tr = 0;
      /*
      if(yht){
	float tcdeta = fabs(toweta - trigeta);
	float tcdphi = fabs(towphi - trigphi);
	if(tcdphi > TMath::Pi())tcdphi -= TMath::Pi();
	float tcdR = sqrt(pow(tcdeta,2) + pow(tcdphi,2));
	if(tcdR < 0.7)tr = 1;
	//cout<<i<<" "<<trigeta<<" "<<trigphi<<" "<<tcdR<<" "<<yht<<" "<<tr<<" "<<nht<<endl;
      }
      */
      
      track->htTrig = yht + tr;
      track->nonhtTrig = nht;
      //cout<<track->htTrig<<endl;
      
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
