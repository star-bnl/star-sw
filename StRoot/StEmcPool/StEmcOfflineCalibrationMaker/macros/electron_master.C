//copied from /star/u/staremc/2009/pp200/StRoot/StEmcPool/StEmcOfflineCalibrationMaker/macros/electron_master.C

#include <iostream>
#include <fstream>
using namespace std;

#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "TF1.h"
#include "TMath.h"
#include "TPostScript.h"
#include "TCanvas.h"
#include "TStyle.h"
#include "TLatex.h"
#include "TTree.h"
#include "TLine.h"

#include "CalibrationHelperFunctions.h"
#include "CalibrationHelperFunctions.cxx"

void drawTower(TH1* h, TF1* f, int id, CalibrationHelperFunctions* helper);
double fit_function(double *x, double *par);
double fit_function2(double *x, double *par);
double background_only_fit(double *x, double *par);
int lookup_crate(float,float);

//void electron_master(const char* file_list="checkP10ic/electrons.list",const char* output="checkP10ic/electroncalib.root", const char* dbDate="2009-04-10 00:11:00", const char* geantfile="geant_func.root", const char* gfname="checkP10ic/mip.gains", const char* ngname="checkP10ic/electron.gains", const char* rgname="matrel.gains",const char* ffname="checkP10ic/electron.fits"){
void electron_master(const char* file_list="checkP11id/electrons.list",const char* output="checkP11id/electroncalib2.root", const char* dbDate="2009-04-10 00:11:00", const char* gfname="checkP11id/mip.gains", const char* ngname="checkP11id/electron2.gains",const char* ffname="checkP11id/electron2.fits"){
  //**********************************************//
  //Load Libraries                                //
  //**********************************************//
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
  gSystem->Load("StEmcTriggerMaker");
  gSystem->Load("StTriggerUtilities");
  gSystem->Load("StEmcOfflineCalibrationMaker");

  cout<<"input filelist: "<<file_list<<endl;
  cout<<"output:          "<<output<<endl;
  cout<<"db Date:        "<<dbDate<<endl;


  //**********************************************//
  //Initialize Stuff                              //
  //**********************************************//

  CalibrationHelperFunctions *helper = new CalibrationHelperFunctions();
	
  StBemcTablesWriter *bemctables = new StBemcTablesWriter();
  bemctables->loadTables(dbDate,"sim");
  StEmcDecoder* decoder = bemctables->getDecoder();


  //chain all input files together
  char file[300];
  TChain* tree = new TChain("skimTree");
  ifstream filelist(file_list);
  while(1){
    filelist >> file;
    if(!filelist.good()) break;
    cout<<file<<endl;
    tree->Add(file);
  }

  const int ntowers = 4800;
  const int nrings = 40;
  const int ncrates = 30;
  float gains[ntowers];
  int status[ntowers];
  float peaks[ntowers];
  float peakerr[ntowers];
  float gainerr[ntowers];

  ifstream gainfile(gfname);
  while(1){
    int id,stat;
    float peak,err,gain,eta,theta;
    gainfile >> id >> peak >> err >> stat;
    if(!gainfile.good())break;
    eta = helper->getEta(id);
    theta = helper->getTheta(id);
    gain = 0.264*(1+0.056*eta*eta)/(sin(theta)*peak);
    peaks[id-1] = peak;
    gains[id-1] = gain;
    if(status[id-1] != 1 || stat != 1) status[id-1] = stat;
    peakerr[id-1] = err;
    //cout<<id<<" "<<gain;
  }


  TFile* geant_file = new TFile("geant_fits.root","READ");
  TF1* geant_fits[20];
  for(int i = 0; i < 20; i++){
    TString fname = "fit_";
    fname += i;
    geant_fits[i] = (TF1*)geant_file->Get(fname);
  }


  TFile outfile(output,"RECREATE");

  TH1 *crate_histo[ncrates];
  TF1 *crate_fit[ncrates];
  TH1 *electron_histo[ntowers];
  TF1 *fit[ntowers];
  TH1 *prs_histo[ntowers];
  TH1 *ring_histo[nrings];
  TH1 *ring2_histo[nrings/2];
  TF1 *ring2fit[nrings/2];

  TH1 *etacrate_histo[ncrates*20];
  TH1 *etacrate_histo_p[ncrates*20];
  TH1 *etacrate_histo_n[ncrates*20];

  TH1 *bghisto[21];
  TH2F* drvsep = new TH2F("drvsep","drvsep",60,0,3.0,100,0.0,0.03);

  TF1 *ringfit[nrings];
  TH2F* ring_pve[nrings];
  TH2F* jan_pve[6];
  float eta;
  int etaindex;
  TH1F* ringprec = new TH1F("ringprec","",40,-1.0,1.0);
  TH1F* ringprec2 = new TH1F("ringprec2","",40,-1.0,1.0);
  ringprec->SetYTitle("E/p");
  ringprec->SetXTitle("#eta");
  ringprec2->SetYTitle("E/p");
  ringprec2->SetXTitle("#eta");
  TH1F* ring2prec = new TH1F("ring2prec","",nrings/2,-1.0,1.0);
  TH1F* ring2prec2 = new TH1F("ring2prec2","",nrings/2,-1.0,1.0);
  ring2prec->SetYTitle("E/p");
  ring2prec->SetXTitle("#eta");
  ring2prec2->SetYTitle("E/p");
  ring2prec2->SetXTitle("#eta");
  double ew[nrings];

  TH2F* trgcheck = new TH2F("trgcheck","trgcheck",2,-0.5,1.5,3,-0.5,2.5);

  TH1F* crateprec = new TH1F("crateprec","",30,0.5,30.5);
  crateprec->SetXTitle("Crate");
  crateprec->SetYTitle("E/p");

  TH2F *energyleak = new TH2F("energyleak","",20,0.0,0.03,20,0.0,1.0);
  TH2F *findbg = new TH2F("findbg","",20,0.0,0.03,30,0.0,5.0);
  TH1F *energymean = new TH1F("energymean","",20,0.0,0.03);
  TH1F *leakmean = new TH1F("leakmean","",20,0.0,0.03);
  energyleak->SetXTitle("#DeltaR");
  energyleak->SetYTitle("leaked energy / total energy");
  findbg->SetXTitle("#DeltaR");
  findbg->SetYTitle("Total energy / track momentum");
  TH2F *tevsp = new TH2F("tevsp","",50,0.0,20.0,50,0.0,30.0);
  TH1F *pmean = new TH1F("pmean","",20,0.0,15.0);
  tevsp->SetXTitle("track momentum (GeV)");
  tevsp->SetYTitle("Total Energy (GeV)");
  TH2F *tevspcent = new TH2F("tevspcent","",20,0.0,15.0,20,0.0,15.0);
  tevspcent->SetXTitle("track momentum (GeV)");
  tevspcent->SetYTitle("Energy in central tower (GeV)");
  TH1F *cmean = new TH1F("cmain","",20,0.0,15.0);
  TH2F *sistertracks = new TH2F("sistertracks","",20,0.0,8.0,20,0.0,8.0);
  sistertracks->SetXTitle("Track momentum (GeV)");
  sistertracks->SetYTitle("Neighbor momentum (GeV)");
  TH2F* dEdxvsp = new TH2F("dEdxvsp","",100,0.15,1.3,100,-5.7,-5.);
  dEdxvsp->SetXTitle("Log(p)");
  dEdxvsp->SetYTitle("Log(dE/dx)");
  TH2F* dEdxvsp_east = new TH2F("dEdxvsp_east","",100,0.15,1.3,100,-5.7,-5.0);
  dEdxvsp_east->SetXTitle("Log(p)");
  dEdxvsp_east->SetYTitle("Log(dE/dx)");
  TH2F* dEdxvsp_west = new TH2F("dEdxvsp_west","",100,0.15,1.3,100,-5.7,-5.0);
  dEdxvsp_west->SetXTitle("Log(p)");
  dEdxvsp_west->SetYTitle("Log(dE/dx)");
  TH2F* energyleak2 = new TH2F("energyleak2","",20,0.0,0.03,20,0.0,1.0);
  TH1F* energymean2 = new TH1F("energymean2","",20,0.0,0.03);
  TH1F* towermult = new TH1F("towermult","",9,0.0,9.0);
  energyleak2->SetXTitle("#DeltaR");
  energyleak2->SetYTitle("leaked energy/total energy");
  towermult->SetXTitle("Neighbors with energy");
  TH2F* multvsp = new TH2F("multvsp","",20,0.0,20.0,9,0.0,9.0);
  multvsp->SetXTitle("Track momentum (GeV)");
  multvsp->SetYTitle("Neighbors with energy");
  TH1F* multmean = new TH1F("multmean","",20,0.0,20.0);
  TH2F* tep3 = new TH2F("tep3","2 < p < 3",20,0.0,0.03,20,0.0,4.0);
  TH2F* tep5 = new TH2F("tep5","3 < p < 5",20,0.0,0.03,20,0.0,4.0);
  TH2F* tep10 = new TH2F("tep10","5 < p < 10",20,0.0,0.03,20,0.0,4.0);
  tep3->SetXTitle("DeltaR");
  tep3->SetYTitle("Total energy / track momentum");
  tep5->SetXTitle("DeltaR");
  tep5->SetYTitle("Total energy / track momentum");
  tep10->SetXTitle("DeltaR");
  tep10->SetYTitle("Total energy / track momentum");
  TH1F* tep3mean = new TH1F("tep3mean","",20,0,0.03);
  TH1F* tep5mean = new TH1F("tep5mean","",20,0,0.03);
  TH1F* tep10mean = new TH1F("tep10mean","",20,0,0.03);
  TH1F* multen = new TH1F("multen","",40,0.0,1.0);
  multen->SetXTitle("Energy in neighbor towers (GeV)");
  TH1F* east_histo = new TH1F("east_histo","Electron E/p in East",40,0.0,2.0);
  TH1F* west_histo = new TH1F("west_histo","Electron E/p in West",40,0.0,2.0);
  TH1F* all_histo = new TH1F("all_histo","Electron E/p",40,0.0,2.0);
  TH1F* notrg = new TH1F("notrg","Electron E/p",40,0.0,2.0);
  TH2F* pvsep = new TH2F("pvsep","Electron p vs E/p",120,0,3.0,20,0,20.0);
  pvsep->SetYTitle("p (Gev)");
  pvsep->SetXTitle("E/p");
  TH2F* pvsep0 = new TH2F("pvsep0","Electron p vs E/p",120,0,3.0,20,0,20.0);
  pvsep0->SetYTitle("p (Gev)");
  pvsep0->SetXTitle("E/p");
  TH2F* evsep = new TH2F("evsep","Electron E vs E/p",120,0,3.0,20,0,20.0);
  evsep->SetYTitle("E (GeV)");
  evsep->SetXTitle("E/p");

  TH1F* bsmde = new TH1F("bsmde","BSMDE ADC TOT",1500,0,1500);
  TH1F* bsmdp = new TH1F("bsmdp","BSMDP ADC TOT",1500,0,1500);

  TH1F* bsmde_central = new TH1F("bsmde_central","BSMDE ADC TOT",100,0,1500);
  TH1F* bsmde_mid = new TH1F("bsmde_mid","BSMDE ADC TOT",100,0,1500);
  TH1F* bsmde_outer = new TH1F("bsmde_outer","BSMDE ADC TOT",100,0,1500);

  TH2F* bsmdep = new TH2F("bsmdep","BSMDE v BSMDP",100,0,1500,100,0,1500);

  TH2F* bsmdevp = new TH2F("bsmdevp","BSMDE v p",100,1.0,15.0,100,0,1500);
  TH2F* bsmdpvp = new TH2F("bsmdpvp","BSMDP v p",100,1.0,15.0,100,0,1500);
  TH2F* bsmdevep = new TH2F("bsmdevep","BSMDE v E/p",100,0.0,2.0,100,0,1500);
  TH2F* bsmdpvep = new TH2F("bsmdpvep","BSMDP v E/p",100,0.0,2.0,100,0,1500);

  TH2F* bsmdeve = new TH2F("bsmdeve","BSMDE v E",100,1.0,30.0,100,0,1500);
  TH2F* bsmdpve = new TH2F("bsmdpve","BSMDP v E",100,1.0,30.0,100,0,1500);

  TH1F* httrig = new TH1F("httrig","HT Trigger",5,-0.5,4.5);

  TH1F* pplus = new TH1F("pplus","e+ p",100,0,20);
  TH1F* pminus = new TH1F("pminus","e- p",100,0,20);

  TH1F* posep = new TH1F("posep","e+ E/p",60,0,3.0);
  TH1F* negep = new TH1F("negep","e- E/p",60,0,3.0);


   //create the tower histograms
  char name1[100];

  for(int i = 0; i < ncrates; i++){
    for(int j = 0; j < 20; j++){
      TString ecname,ecnamep,ecnamen;
      ecname += "etacrate_";
      int cr = i+1;
      int et = j;
      ecname += cr;
      ecname += "_";
      ecname += et;
      ecnamep += ecname;
      ecnamen += ecname;
      ecnamep.ReplaceAll("te_","te_p_");
      ecnamen.ReplaceAll("te_","te_n_");
      etacrate_histo[i*20+j] = new TH1F(ecname.Data(),ecname.Data(),60,0.0,3.0);
      etacrate_histo_p[i*20+j] = new TH1F(ecnamep.Data(),ecnamep.Data(),60,0.0,3.0);
      etacrate_histo_n[i*20+j] = new TH1F(ecnamen.Data(),ecnamen.Data(),60,0.0,3.0);

    }
  }

  for(int i=0; i<ntowers; i++){
    char nameeh[100];
    sprintf(nameeh,"electron_histo_%i",i+1);
    electron_histo[i] = new TH1D(nameeh,"",60,0.,3.0);
    electron_histo[i]->SetXTitle("E/p");
    //sprintf(name1,"prs_histo_%i",i+1);
    //prs_histo[i] = new TH1D(name1,"",60,0.,500.);
    //prs_histo[i]->SetXTitle("ADC");
  }

  for(int i = 0; i < 21; i++){
    TString namebg;
    namebg += "bg_";
    namebg += i+25;
    bghisto[i] = new TH1F(namebg.Data(),namebg.Data(),60,0,3.0);
  }

  for(int i = 0; i < ncrates; i++){
    char name[100];
    char title[100];
    sprintf(name,"crate_%i",i+1);
    sprintf(title,"E/p for Crate %i",i+1);
    crate_histo[i] = new TH1F(name,title,60,0.,3.0);
    crate_histo[i]->SetXTitle("E/p");
  }

  for(int i = 0; i < nrings/2; i++){
    char name[100];
    sprintf(name,"ring2_histo_%i",i);
    ring2_histo[i] = new TH1F(name,"",60,0.,3.0);
    ring2_histo[i]->SetXTitle("E/p");
  }

  char name2[100];
  for(int i=0; i<nrings;i++)
    {
      sprintf(name2,"ring_histo_%i",i);
      //ring_histo[i] = new TH1D(name2,"",30,0.,140.0);
      //ring_histo[i]->SetXTitle("ADC / GeV Sin(#theta)");
      ring_histo[i] = new TH1D(name2,"",60,0.,3.0);
      ring_histo[i]->SetXTitle("E/p");
      char namerpve[100];
      sprintf(namerpve,"ring_pve_%i",i);
      ring_pve[i] = new TH2F(namerpve,"",20,0,20.0,20,0,20.0);
      ring_pve[i]->SetXTitle("E (GeV)");
      ring_pve[i]->SetYTitle("p (GeV)");
    }

  for(int i = 0; i < 6; i++){
    char jname[100];
    sprintf(jname,"jan_pve_%i",i);
    jan_pve[i] = new TH2F(jname,"",120,0,3.0,20,0,20.0);
    jan_pve[i]->SetXTitle("E/p");
    jan_pve[i]->SetYTitle("p (GeV)");
  }
  //global graphics functions
  gStyle->SetOptStat("oue");
  gStyle->SetOptFit(111);
  gStyle->SetCanvasColor(10);
  gStyle->SetCanvasBorderMode(0);
  gStyle->SetOptTitle(0);
  gStyle->SetPalette(1);
  gStyle->SetStatColor(0);

  StEmcOfflineCalibrationTrack* track = new StEmcOfflineCalibrationTrack();
  StEmcOfflineCalibrationCluster* cluster = new StEmcOfflineCalibrationCluster();
  tree->SetBranchAddress("clusters",&cluster);

  //**********************************************//
  //Loop Over Tracks, Fill Histograms             //
  //**********************************************//

  int nentries = tree->GetEntries();
  cout<<nentries<<endl;
  int ngoodhit = 0;
  int nplt10 = 0;
  int nnosis = 0;
  int nfinal = 0;
  int nbsmdgood = 0;
  int nnottrig = 0;
  int nfidu = 0;
  int nenterexit = 0;
  for(int j=0; j<nentries; j++){
    tree->GetEntry(j);
    track = &(cluster->centralTrack);
    TClonesArray *tracks = cluster->tracks;
    if(j % 500000 == 0) cout<<"reading "<<j<<" of "<<nentries<<endl;

    httrig->Fill((float)track->htTrig);

    if(track->charge > 0)pplus->Fill(track->p);
    if(track->charge < 0)pminus->Fill(track->p);

    int bsmdeadctot = 0;
    int bsmdpadctot = 0;
    for(int i = 0; i < 11; i++){
      if(track->smde_adc[i] > track->smde_pedestal[i])bsmdeadctot += track->smde_adc[i] - track->smde_pedestal[i];
      if(track->smdp_adc[i] > track->smdp_pedestal[i])bsmdpadctot += track->smdp_adc[i] - track->smdp_pedestal[i];
    }

    double dR = TMath::Sqrt(track->deta*track->deta + track->dphi*track->dphi);


    double scaled_adc = (track->tower_adc[0] - track->tower_pedestal[0]) / track->p;

    int index = track->tower_id[0];

    //figure out eta and etaindex
    eta = helper->getEta(index);
    if(TMath::Abs(eta) > 0.968) eta += 0.005 * TMath::Abs(eta)/eta;
    etaindex = ((TMath::Nint(eta * 1000.0) + 25)/50 + 19);
    int geantetaindex = ((TMath::Nint(fabs(eta) * 1000.0) + 25)/50 - 1);

    double geant_scale = geant_fits[geantetaindex]->Eval(dR);
    scaled_adc /= geant_scale;
    //double geant_scale = geant_fit->Eval(dR);
    //scaled_adc *= geant_scale;
    //cout<<scaled_adc<<endl;

    //now rescale dR for last ring to make cuts work
    if(geantetaindex == 19)dR *= 0.025/0.017;

    //double tgain = bemctables->calib(1,track->tower_id[0])*gains[index-1];
    double tgain = gains[index-1];


    if((track->tower_adc[0] - track->tower_pedestal[0]) < 2.5 * track->tower_pedestal_rms[0])continue;
    ngoodhit++;
    dEdxvsp->Fill(TMath::Log10(track->p),TMath::Log10(track->dEdx));
    if(track->tower_id[0] <= 2400)dEdxvsp_west->Fill(TMath::Log10(track->p),TMath::Log10(track->dEdx));
    if(track->tower_id[0] > 2400)dEdxvsp_east->Fill(TMath::Log10(track->p),TMath::Log10(track->dEdx));

    trgcheck->Fill(track->nonhtTrig,track->htTrig);

    if(track->tower_id[0] != track->tower_id_exit)continue;
    nenterexit++;

    if(status[index-1]!=1)continue;

    pvsep0->Fill(scaled_adc*tgain,track->p);
    if(track->p > 10)continue;	
    nplt10++;		

    //if(track->p < 1.5)continue;
    //if(track->p < 3.0)continue;
    if(track->htTrig == 2 && track->nonhtTrig == 0)continue;
    nnottrig++;
    //change the fiducial cut to a square with diagonal = 0.06 in deta, dphi space
    float squarefid = 0.02;//0.03/TMath::Sqrt(2.0);
    //if(TMath::Abs(track->deta) > squarefid || TMath::Abs(track->dphi) > squarefid)continue;

    //calculate geant scaled, pedestal subtracted adc
    //if(dR > 0.0125)continue;
    int numsis = tracks->GetEntries();
    float totalbtow = 0;
    float maxEt = 0;
    int maxId = -1;
    for(int i = 0; i < 9; i++){
      if(track->tower_adc[i] - track->tower_pedestal[i] < 0)continue;
      float theta = helper->getTheta(track->tower_id[i]);
      float nextEt = (track->tower_adc[i] - track->tower_pedestal[i]) * bemctables->calib(1,track->tower_id[i])*sin(theta);
      totalbtow += nextEt;
      if(nextEt > maxEt){
	maxEt = nextEt;
	maxId = i;
      }
    }

    if(track->dEdx > 3.5e-6 && track->dEdx <5.0e-6 && numsis ==0 &&maxId == 0)drvsep->Fill(scaled_adc*tgain,dR);
    if(dR > 0.02)continue;
    nfidu++;
    //if(track->p > 6.0)continue;				
    //if(track->p > 15)continue;
    //cout<<track->dEdx<<endl;


    //if(track->dEdx*1000000 > 4.5 || track->dEdx*1000000 < 3.5)continue;

    //cout<<track->htTrig<<endl;

    for(int i = 0; i < 21; i++){
      if(numsis > 0)break;
      if(maxId != 0)break;
      if(track->dEdx*1e7 > 25 + i && track->dEdx*1e7 < 26+i)bghisto[i]->Fill(scaled_adc*tgain);
    }

    //if(track->dEdx < 3.5e-6 || track->dEdx > 5.0e-6)continue;
    if(track->dEdx < 3.5e-6 || track->dEdx > 4.5e-6)continue;
    //if((bsmdeadctot > -1 && bsmdeadctot < 50) && (bsmdpadctot < 50 && bsmdpadctot > -1))continue;
    nbsmdgood++;
    //if(bsmdeadctot < 500) continue;

    //if(bsmdeadctot < 84.*track->p)continue;
    //if(bsmdeadctot > 200.*track->p + 1500)continue;

    //if(bsmdpadctot < 800)continue;

    if(numsis > 0)continue;

    nnosis++;
    if(maxId != 0) continue;


    nfinal++;
    if(track->htTrig!=2)notrg->Fill(scaled_adc*tgain);

    //if(!track->nonhtTrig)continue;
    //if(track->nHits < 25)continue;
    //if(status[index-1]==1)ring_histo[etaindex]->Fill(scaled_adc*gains[index-1]);

    //scaled_adc = totalbtow/track->p;
    //tgain = 1.0;

    float phi = helper->getPhi(index);
    int crate,sequence;
    decoder->GetCrateFromTowerId(index,crate,sequence);
    etacrate_histo[(crate-1)*20+geantetaindex]->Fill(scaled_adc*tgain);
    if(track->charge > 0)etacrate_histo_p[(crate-1)*20+geantetaindex]->Fill(scaled_adc*tgain);
    if(track->charge < 0)etacrate_histo_n[(crate-1)*20+geantetaindex]->Fill(scaled_adc*tgain);

    electron_histo[index-1]->Fill(scaled_adc*tgain);
    ring_histo[etaindex]->Fill(scaled_adc*tgain);
    if(etaindex == 0 || etaindex == 39){
      //cout<<etaindex<<" "<<tgain<<" "<<track->p<<" "<<scaled_adc*tgain*track->p<<" "<<scaled_adc*tgain<<endl;
    }
    //cout<<index<<" "<<gains[index-1]<<" "<<scaled_adc*track->p<<" "<<track->p<<" "<<status[index-1]<<endl;
    ring_pve[etaindex]->Fill(scaled_adc*tgain*track->p,track->p);

    dEdxvsp->Fill(TMath::Log10(track->p),TMath::Log10(track->dEdx));

    float abseta = TMath::Abs(eta);
    if(abseta > 0.95){
      jan_pve[5]->Fill(scaled_adc*tgain,track->p);
    }else if(abseta > 0.9){
      jan_pve[4]->Fill(scaled_adc*tgain,track->p);
    }else if(abseta > 0.6){
      jan_pve[3]->Fill(scaled_adc*tgain,track->p);
    }else if(abseta > 0.3){
      jan_pve[2]->Fill(scaled_adc*tgain,track->p);
    }else if(abseta > 0.05){
      jan_pve[1]->Fill(scaled_adc*tgain,track->p);
    }else{
      jan_pve[0]->Fill(scaled_adc*tgain,track->p);
    }

    all_histo->Fill(scaled_adc*tgain);
    pvsep->Fill(scaled_adc*tgain,track->p);
    evsep->Fill(scaled_adc*tgain,track->p*scaled_adc*tgain);
    tevsp->Fill(track->p,scaled_adc*tgain*track->p);

    if(track->charge > 0)posep->Fill(scaled_adc*tgain);
    if(track->charge < 0)negep->Fill(scaled_adc*tgain);

    //if(scaled_adc*tgain < 0.7 || scaled_adc*tgain > 5.0)continue;
    bsmde->Fill(bsmdeadctot);
    bsmdp->Fill(bsmdpadctot);
    bsmdep->Fill(bsmdeadctot,bsmdpadctot);

    if(abseta > 0.6){
      bsmde_outer->Fill(bsmdeadctot);
    }else if(abseta > 0.3){
      bsmde_mid->Fill(bsmdeadctot);
    }else{
      bsmde_central->Fill(bsmdeadctot);
    }

    bsmdevp->Fill(track->p,bsmdeadctot);
    bsmdpvp->Fill(track->p,bsmdpadctot);
    bsmdevep->Fill(scaled_adc*tgain,bsmdeadctot);
    bsmdpvep->Fill(scaled_adc*tgain,bsmdpadctot);

    bsmdeve->Fill(scaled_adc*tgain*track->p,bsmdeadctot);
    bsmdpve->Fill(scaled_adc*tgain*track->p,bsmdpadctot);

    if(dR > 0.015)continue;
    if(track->tower_id[0] <= 2400){
      west_histo->Fill(scaled_adc*tgain);
    }
    if(track->tower_id[0] > 2400){
      east_histo->Fill(scaled_adc*tgain);
    }

  }
  cout<<"processed electron tree"<<endl;
  cout<<"ngoodhit: "<<ngoodhit<<endl;
  cout<<"nenterexit: "<<nenterexit<<endl;
  cout<<"n not trig: "<<nnottrig<<endl;
  cout<<"n p < 10: "<<nplt10<<endl;
  cout<<"n in fidu: "<<nfidu<<endl;
  cout<<"nbsmdhit: "<<nbsmdgood<<endl;
  cout<<"n no sis: "<<nnosis<<endl;
  cout<<"n final: "<<nfinal<<endl;

  //double ew[21];
  for(int h=0;h<21;h++){
    TH1D* projection = energyleak->ProjectionY("projection",h,h);
    float mean = projection->GetMean();
    energymean->SetBinContent(h,mean);
    TH1D* projection1 = findbg->ProjectionY("projection1",h,h);
    float mean1 = projection1->GetMean();
    leakmean->SetBinContent(h,mean1);
    TH1D* projection2 = tevsp->ProjectionY("projection2",h,h);
    float mean2 = projection2->GetMean();
    pmean->SetBinContent(h,mean2);
    //ew[h] = projection2->GetRMS();
    TH1D* projection3 = tevspcent->ProjectionY("projection3",h,h);
    float mean3 = projection3->GetMean();
    cmean->SetBinContent(h,mean3);
    TH1D* projection4 = energyleak2->ProjectionY("projection4",h,h);
    float mean4 = projection4->GetMean();
    energymean2->SetBinContent(h,mean4);
    TH1D* projection5 = multvsp->ProjectionY("projection5",h,h);
    float mean5 = projection5->GetMean();
    multmean->SetBinContent(h,mean5);
    TH1D* projection6 = tep3->ProjectionY("projection6",h,h);
    float mean6 = projection6->GetMean();
    tep3mean->SetBinContent(h,mean6);
    TH1D* projection7 = tep3->ProjectionY("projection7",h,h);
    float mean7 = projection7->GetMean();
    tep5mean->SetBinContent(h,mean7);
    TH1D* projection8 = tep3->ProjectionY("projection8",h,h);
    float mean8 = projection8->GetMean();
    tep10mean->SetBinContent(h,mean8);
  }

  TF1* fitleak = new TF1("fitleak","[0]",0,0.03);
  fitleak->SetLineWidth(0.1);
  leakmean->Fit(fitleak,"rq");


  //**********************************************//
  //Fit Tower Histograms                          //
  //**********************************************//
  /*
  for(int i=0; i<ntowers; i++){
	  
    if(i%600 == 0) cout<<"fitting tower "<<i+1<<" of "<<ntowers<<endl;
		
    sprintf(name,"fit_%i",i+1);
		
		//this fit is for the electron tree
    fit[i] = new TF1(name,fit_function,0.,140.,6);
    fit[i]->SetParameter(1,65.);
    fit[i]->SetParameter(2,10.);
    fit[i]->SetParameter(3,10.); //relative height of peak to bg
    fit[i]->SetParameter(4,10.);
    fit[i]->SetParameter(5,3.);
    fit[i]->SetParNames("Constant","Mean","Sigma","Peak Ratio","Bg Mean","Bg Sigma");
		
    fit[i]->SetLineColor(kGreen);
    fit[i]->SetLineWidth(0.6);
		
    electron_histo[i]->Fit(fit[i],"rq");
  }
  */
  //**********************************************//
  //Fit Ring Histograms                           //
  //**********************************************//

  for(int i=0; i<nrings; i++){
	  
    //cout<<"fitting ring "<<i+1<<" of "<<nrings<<endl;
		
    sprintf(name,"ring_fit_%i",i);
    /*
    ringfit[i] = new TF1(name,fit_function,0.,140.,6);
    ringfit[i]->SetParameter(1,1.);
    ringfit[i]->SetParameter(2,0.2);
    ringfit[i]->SetParameter(3,1.5); //relative height of peak to bg
    ringfit[i]->SetParameter(4,0.15);
    ringfit[i]->SetParameter(5,0.8);
    ringfit[i]->SetParNames("Constant","Mean","Sigma","Peak Ratio","Bg Mean","Bg Sigma");
    */		
    //TF1* ffff = new TF1("ffff","expo(0) + gaus(2)",0.4,1.7);

    ring_histo[i]->Sumw2();
    //ring_histo[i]->Rebin(3);

    ringfit[i] = new TF1(name,"pol1(0) + gaus(2)");
    ringfit[i]->SetParLimits(0,0,10.0*ring_histo[i]->GetBinContent(1));
    ringfit[i]->SetParLimits(1,-10000,0);
    ringfit[i]->SetParLimits(2,0,10.0*ring_histo[i]->GetMaximum());
    ringfit[i]->SetParLimits(3,0,10);
    ringfit[i]->SetParameter(0,ring_histo[i]->GetBinContent(1));
    ringfit[i]->SetParameter(1,-ring_histo[i]->GetBinContent(1)/6.0);
    ringfit[i]->SetParameter(2,ring_histo[i]->GetMaximum());
    ringfit[i]->SetParameter(3,0.95);
    ringfit[i]->SetParameter(4,0.15);
    ringfit[i]->SetParNames("constant1","Slope","constant2","Mean","Sigma");
    /*
    ringfit[i] = new TF1(name,fit_function2,0.1,2.5,8);
    ringfit[i]->SetParameter(1,1.);
    ringfit[i]->SetParameter(2,0.2);
    ringfit[i]->SetParameter(3,1.5); //relative height of peak to bg
    ringfit[i]->SetParameter(4,0.25);
    ringfit[i]->SetParameter(5,0.15);
    ringfit[i]->SetParameter(7,0.8);
    ringfit[i]->SetParNames("Constant","Mean","Sigma","Peak Ratio","Bg Mean","Bg Sigma","Bg2 constant","Bg2 decay");
    	
    */
    ringfit[i]->SetLineColor(kBlue);
    ringfit[i]->SetLineWidth(0.6);
    
    ring_histo[i]->Fit(ringfit[i],"rql","",0.2,1.7);
		
    ringprec->SetBinContent(i+1,(ringfit[i]->GetParameter(3)));
    ringprec->SetBinError(i+1,ringfit[i]->GetParameter(4));
    ringprec2->SetBinContent(i+1,(ringfit[i]->GetParameter(3)));
    ringprec2->SetBinError(i+1,ringfit[i]->GetParError(3));
    //ew[i] = 4066/(60*(fit[i]->GetParameter(2))*(fit[i]->GetParameter(2)));

    float mean = ringfit[i]->GetParameter(3);
    float merr = ringfit[i]->GetParError(3);
    cout<<"ring "<<i<<" "<<mean<<" "<<merr/mean<<" "<<ring_histo[i]->GetEntries()<<endl;
  }

  for(int i = 0; i < nrings/2; i++){
    ring2_histo[i]->Add(ring_histo[2*i]);
    ring2_histo[i]->Add(ring_histo[2*i+1]);
    sprintf(name,"ring2_fit_%i",i);
    ring2fit[i] = new TF1(name,"pol1(0) + gaus(2)",0.3,1.7);

    ring2_histo[i]->Rebin(3);

    ring2fit[i]->SetParLimits(0,0,10.0*ring2_histo[i]->GetBinContent(1));
    ring2fit[i]->SetParLimits(1,-10000,0);
    ring2fit[i]->SetParLimits(2,0,10.0*ring2_histo[i]->GetMaximum());
    ring2fit[i]->SetParLimits(3,0,10);
    ring2fit[i]->SetParLimits(4,0.17,0.175);
    ring2fit[i]->SetParameter(0,ring2_histo[i]->GetBinContent(1));
    ring2fit[i]->SetParameter(1,-ring2_histo[i]->GetBinContent(1)/6.0);
    ring2fit[i]->SetParameter(2,ring2_histo[i]->GetMaximum());
    ring2fit[i]->SetParameter(3,0.95);
    ring2fit[i]->SetParameter(4,0.11245);
    ring2fit[i]->SetParNames("constant1","Slope","constant2","Mean","Sigma");

    ring2_histo[i]->Fit(ring2fit[i],"rql","",0.3,1.7);

    ring2prec->SetBinContent(i+1,(ring2fit[i]->GetParameter(3)));
    ring2prec->SetBinError(i+1,ring2fit[i]->GetParameter(4));
    ring2prec2->SetBinContent(i+1,(ring2fit[i]->GetParameter(3)));
    ring2prec2->SetBinError(i+1,ring2fit[i]->GetParError(3));

    cout<<"ring2 "<<i<<" "<<ring2fit[i]->GetParameter(3)<<" "<<ring2fit[i]->GetParError(3)<<endl;
  }

  for(int i = 0; i < ntowers; i++){
    char name[100];
    sprintf(name,"electron_fit_%i",i+1);
    /*
    fit[i] = new TF1(name,"pol1(0)+gaus(2)");
    fit[i]->SetParLimits(0,0,10.0*electron_histo[i]->GetBinContent(1));
    fit[i]->SetParLimits(1,-10000,0);
    fit[i]->SetParLimits(2,0,10.0*electron_histo[i]->GetMaximum());
    fit[i]->SetParLimits(3,0,10);
    fit[i]->SetParameter(0,electron_histo[i]->GetBinContent(1));
    fit[i]->SetParameter(1,-electron_histo[i]->GetBinContent(1)/6.0);
    fit[i]->SetParameter(2,electron_histo[i]->GetMaximum());
    fit[i]->SetParameter(3,0.95);
    fit[i]->SetParameter(4,0.15);
    fit[i]->SetParNames("constant1","Slope","constant2","Mean","Sigma");
    */
    fit[i] = new TF1(name,"pol0(0)+gaus(1)");
    fit[i]->SetParLimits(0,0,10.0*electron_histo[i]->GetBinContent(1));
    fit[i]->SetParLimits(1,0,10.0*electron_histo[i]->GetMaximum());
    fit[i]->SetParLimits(2,0,10);
    fit[i]->SetParameter(0,electron_histo[i]->GetBinContent(1));
    fit[i]->SetParameter(1,electron_histo[i]->GetMaximum());
    fit[i]->SetParameter(2,0.95);
    fit[i]->SetParameter(3,0.15);
    fit[i]->SetParNames("constant1","constant2","Mean","Sigma");

    electron_histo[i]->Fit(fit[i],"rql","",0.3,1.7);
  }

  ofstream fitfile(ffname);

  TF1* etacrate_fit[ncrates*20];
  for(int ii = 0; ii < ncrates; ii++){
    for(int j = 0; j < 20; j++){
      TString ecname;
      ecname += "fit";
      int cr = ii+1;
      int et = j;
      int i = ii*20 + j;
      //ecname += cr;
      //ecname += "_";
      //ecname += et;
      etacrate_fit[i] = new TF1(ecname.Data(),"pol1(0) + gaus(2)",0.25,1.6);
      //etacrate_fit[i]->SetParLimits(0,0,10.0*etacrate_histo[i]->GetBinContent(1));
      etacrate_histo[i]->Rebin();
      etacrate_fit[i]->SetParLimits(1,-10000,0);
      etacrate_fit[i]->SetParLimits(2,0,10.0*etacrate_histo[i]->GetMaximum());
      etacrate_fit[i]->SetParLimits(3,0,10);
      etacrate_fit[i]->SetParameter(0,etacrate_histo[i]->GetBinContent(2));
      etacrate_fit[i]->SetParameter(1,-etacrate_histo[i]->GetBinContent(2)/3.0);
      etacrate_fit[i]->SetParameter(2,etacrate_histo[i]->GetMaximum());
      etacrate_fit[i]->SetParameter(3,0.96134);
      etacrate_fit[i]->SetParameter(4,0.141123);
      etacrate_fit[i]->SetParNames("constant1","Slope","constant2","Mean","Sigma");
      
      etacrate_histo[i]->Fit(etacrate_fit[i],"rql","",0.25,1.5);
      etacrate_histo_p[i]->Fit(etacrate_fit[i],"rql","",0.25,1.5);
      etacrate_histo_n[i]->Fit(etacrate_fit[i],"rql","",0.25,1.5);
      fitfile << i << " " << etacrate_histo[i]->GetFunction("fit")->GetParameter(3) << " " << etacrate_histo_p[i]->GetFunction("fit")->GetParameter(3) << " " << etacrate_histo_n[i]->GetFunction("fit")->GetParameter(3) << endl;

    }
  }

  ofstream newgain(ngname);
  

  float gains2[ntowers];
  float gerr2[ntowers];
  for(int i = 0; i < ntowers; i++){
    float eta = helper->getEta(i+1);
    int crate,sequence;
    decoder->GetCrateFromTowerId(i+1,crate,sequence);
    int geantetaindex = ((TMath::Nint(fabs(eta) * 1000.0) + 25)/50 - 1);
    TString ecname;
    ecname += "fit";
    //int cr = ii+1;
    //int et = j;
    //int i = ii*20 + j;
    //ecname += crate;
    //ecname += "_";
    //ecname += geantetaindex;
    if(TMath::Abs(eta) > 0.968) eta += 0.005 * TMath::Abs(eta)/eta;
    int etaindex = ((TMath::Nint(eta * 1000.0) + 25)/50 + 19);
    //float adjust = ring2fit[(int)etaindex/2]->GetParameter(3);
    //cout<<etaindex<<" "<<(int)etaindex/2<<" "<<adjust<<endl;
    //float adjust = fit[i]->GetParameter(3);
    float ng = 0;
    float ne = 0;
    if(status[i] == 1){
      //float og = bemctables->calib(1,i+1)*gains[i];
      float og = gains[i];
      //float aerr = ring2fit[(int)etaindex/2]->GetParError(3);
      float adjust = etacrate_histo[(crate-1)*20+geantetaindex]->GetFunction("fit")->GetParameter(3);
      float aerr = etacrate_histo[(crate-1)*20+geantetaindex]->GetFunction("fit")->GetParError(3);
      if(geantetaindex == 19){
	adjust = ringfit[etaindex]->GetParameter(3);
	aerr = ringfit[etaindex]->GetParError(3);
      }
      ng = og/adjust;
      float gerr = peakerr[i]*gains[i]/peaks[i];
      ne = sqrt(pow(og*aerr/(adjust*adjust),2) + pow(gerr/adjust,2));
    }
    newgain << i+1 << " " << ng << " " << ne << " " << status[i] << endl;
    gains2[i] = ng;
    gerr2[i] = ne;


  }

  newgain.close();

    ///////////////////////////////////////
   //Using new gains regenerate by crate//
  ///////////////////////////////////////


  /*
  for(int j=0; j<nentries; j++){
    tree->GetEntry(j);
    track = &(cluster->centralTrack);
    TClonesArray *tracks = cluster->tracks;

    int bsmdeadctot = 0;
    int bsmdpadctot = 0;
    for(int i = 0; i < 11; i++){
      if(track->smde_adc[i] > track->smde_pedestal[i])bsmdeadctot += track->smde_adc[i] - track->smde_pedestal[i];
      if(track->smdp_adc[i] > track->smdp_pedestal[i])bsmdpadctot += track->smdp_adc[i] - track->smdp_pedestal[i];
    }

    double dR = TMath::Sqrt(track->deta*track->deta + track->dphi*track->dphi);


    double scaled_adc = (track->tower_adc[0] - track->tower_pedestal[0]) / track->p;
    double geant_scale = geant_fit->Eval(dR);
    scaled_adc *= geant_scale;
    //cout<<scaled_adc<<endl;
    int index = track->tower_id[0];

    //figure out eta and etaindex
    eta = helper->getEta(index);
    if(TMath::Abs(eta) > 0.968) eta += 0.005 * TMath::Abs(eta)/eta;
    etaindex = ((TMath::Nint(eta * 1000.0) + 25)/50 + 19);

    double tgain = bemctables->calib(1,track->tower_id[0])*gains[index-1]*gains2[index-1];
    //double tgain = gains[index-1];


    if((track->tower_adc[0] - track->tower_pedestal[0]) < 2.5 * track->tower_pedestal_rms[0])continue;

    if(track->tower_id[0] != track->tower_id_exit)continue;

    if(track->htTrig == 2)continue;
    if(track->p > 6)continue;	

    if(dR > 0.025)continue;

    if(track->dEdx < 3.4e-6)continue;
    if((bsmdeadctot > -1 && bsmdeadctot < 50) && (bsmdpadctot < 50 && bsmdpadctot > -1))continue;

    int numsis = tracks->GetEntries();
    if(numsis > 0)continue;

    float totalbtow = 0;
    float maxEt = 0;
    int maxId = -1;
    for(int i = 0; i < 9; i++){
      if(track->tower_adc[i] - track->tower_pedestal[i] < 0)continue;
      float theta = helper->getTheta(track->tower_id[i]);
      float nextEt = (track->tower_adc[i] - track->tower_pedestal[i]) * bemctables->calib(1,track->tower_id[i])*sin(theta);
      totalbtow += nextEt;
      if(nextEt > maxEt){
	maxEt = nextEt;
	maxId = i;
      }
    }
    if(maxId != 0) continue;

    eta = helper->getEta(index);
    float phi = helper->getPhi(index);
    int crate = lookup_crate(eta,phi);
    if(status[index-1]==1)crate_histo[crate-1]->Fill(scaled_adc*tgain);
  }

  for(int i = 0; i < ncrates; i++){
    sprintf(name,"crate_fit_%i",i);
    crate_histo[i]->Sumw2();	
    crate_histo[i]->Rebin(4);	

    crate_fit[i] = new TF1(name,"pol1(0) + gaus(2)",0.3,1.7);
    crate_fit[i]->SetParLimits(0,0,10.0*crate_histo[i]->GetBinContent(1));
    crate_fit[i]->SetParLimits(1,-10000,0);
    crate_fit[i]->SetParLimits(2,0,10.0*crate_histo[i]->GetMaximum());
    crate_fit[i]->SetParLimits(3,0,10);
    crate_fit[i]->SetParameter(0,crate_histo[i]->GetBinContent(1));
    crate_fit[i]->SetParameter(1,-crate_histo[i]->GetBinContent(1)/6.0);
    crate_fit[i]->SetParameter(2,-crate_histo[i]->GetMaximum());
    crate_fit[i]->SetParameter(3,0.929);
    crate_fit[i]->SetParameter(4,0.156);
    crate_fit[i]->SetParNames("constant1","Slope","constant2","Mean","Sigma");
    crate_fit[i]->SetLineColor(kBlue);
    crate_fit[i]->SetLineWidth(0.6);


    crate_histo[i]->Fit(crate_fit[i],"rql","",0.3,1.8);
    float mean = crate_histo[i]->GetFunction(name)->GetParameter(3);
    float merr = crate_histo[i]->GetFunction(name)->GetParError(3);
    crateprec->SetBinContent(i+1,mean);
    crateprec->SetBinError(i+1,merr);
    cout<<"crate "<<i+1<<" "<<mean<<" "<<merr/mean<<endl;
  }
  
  ofstream newgain(ngname);

  float gains3[ntowers];
  float gerr3[ntowers];
  for(int i = 0; i < ntowers; i++){
    float eta = helper->getEta(i+1);
    float phi = helper->getPhi(i+1);
    int crate = lookup_crate(eta,phi);
    float adjust = crate_fit[crate-1]->GetParameter(3);
    float og = bemctables->calib(1,i)*gains[i]*gains2[i];
    float ng = og;
    float aerr = crate_fit[crate-1]->GetParError(3);
    float ne = sqrt(pow(gains[i]*gerr2[i],2) + pow(gains2[i]*gainerr[i],2));
    if(fabs(adjust-1)/aerr > 1.5){
      ne = sqrt(pow(ne/(adjust),2)+pow(og*aerr/(adjust*adjust),2));
      ng /= adjust;
    }
    newgain << i+1 << " " << ng << " " << ne << " " << status[i] << endl;
    gains3[i] = ng;
    gerr3[i] = ne;
  }

  newgain.close();
  */
  outfile.Write();
  outfile.Close();

}

//constrained double-Gaussian to fit the background
double background_only_fit(double *x, double *par){
  double par3 = par[0]/1.5;
  double par4 = par[1] + 10.;
  double par5 = par[2] * 6.5;
	
  double fitval = 0;
  if(par[2] != 0){
    double arg1 = (x[0]-par[1])/par[2];
    double arg2 = (x[0]-par4)/par5;
    fitval = par[0]*TMath::Exp(-0.5*arg1*arg1) + par3*TMath::Exp(-0.5*arg2*arg2);
  }
	
  return fitval;
}

double fit_function(double *x, double *par){
	//6-parameter fit includes
	//3 param electron Gaussian
	//par3 = relative height of peak/bg ~ 10
	//par4 = mean of main bg Gaussian ~ 10
	//par5 = width of main bg Gaussian ~ 3
	
  double par3 = 0;
  if(par[3] != 0)par3 = par[0] / par[3];
  double par6 = par3/1.5;
  double par7 = par[4] + 10.;
  double par8 = par[5] * 6.5;
	
  double fitval = 0;
  if(par[2] != 0 && par[5] != 0){
    double arg1 = (x[0]-par[1])/par[2];
    double arg2 = (x[0]-par[4])/par[5];
    double arg3 = (x[0]-par7)/par8;
    fitval = par[0]*TMath::Exp(-0.5*arg1*arg1) + par3*TMath::Exp(-0.5*arg2*arg2) + par6*TMath::Exp(-0.5*arg3*arg3);
  }
	
  return fitval;
}

double fit_function2(double *x, double *par){
	//6-parameter fit includes
	//3 param electron Gaussian
	//par3 = relative height of peak/bg ~ 10
	//par4 = mean of main bg Gaussian ~ 10
	//par5 = width of main bg Gaussian ~ 3
	
  double par3 = 0;
  if(par[3] != 0)par3 = par[0] / par[3];
  double par6 = par[6];
  double par7 = par[4] + 10.;
  double par8 = par[7];
	
  double fitval = 0;
  if(par[2] != 0 && par[5] != 0 && par8 != 0){
    double arg1 = (x[0]-par[1])/par[2];
    double arg2 = (x[0]-par[4])/par[5];
    double arg3 = (x[0])/par8;
    fitval = par[0]*TMath::Exp(-0.5*arg1*arg1) + par3*TMath::Exp(-0.5*arg2*arg2) + par6*TMath::Exp(-arg3);
  }
  return fitval;
}

int lookup_crate(float eta,float phi)
{
    if (eta < 0){
      if (phi < -2.72) return 5;
      else if (phi < -2.30) return 6;
      else if (phi < -1.88) return 7;
      else if (phi < -1.46) return 8;
      else if( phi < -1.04) return 9;
      else if( phi < -0.62) return 10;
      else if( phi < -0.20) return 11;
      else if( phi <  0.22) return 12;
      else if( phi <  0.64) return 13;
      else if( phi <  1.06) return 14;
      else if( phi <  1.48) return 15;
      else if( phi <  1.90) return 1;
      else if( phi <  2.32) return 2;
      else if( phi <  2.74) return 3;
      else             return 4;
    }else{
      if   (phi < -2.72) return 20;
      else if( phi < -2.30) return 21;
      else if( phi < -1.88) return 22;
      else if( phi < -1.46) return 23;
      else if( phi < -1.04) return 24;
      else if( phi < -0.62) return 25;
      else if( phi < -0.20) return 26;
      else if( phi <  0.22) return 27;
      else if( phi <  0.64) return 28;
      else if( phi <  1.06) return 29;
      else if( phi <  1.48) return 30;
      else if( phi <  1.90) return 16;
      else if( phi <  2.32) return 17;
      else if( phi <  2.74) return 18;
      else             return 19;
    }
}
