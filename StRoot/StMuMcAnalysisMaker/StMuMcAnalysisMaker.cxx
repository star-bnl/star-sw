//*-- Author : Yuri Fisyak 02/02/2016
#include "StMuMcAnalysisMaker.h"
#include "TDirectory.h"
#include "TROOT.h"
ClassImp(StMuMcAnalysisMaker);
//                  [gp]     [type]           [particle] [pm]         [x]         [i]                  
static TH3F *fHistsT[kTotalT][kTotalMatchType][kPartypeT][kTotalSigns][kVariables][kTotalQAll] = {0};
static TH1F *GiD[4] = {0};
static TH2F *McRcHit = 0;
static const Char_t *TitleTrType[kTotalT] = {"Global", "Primary"};
static const Char_t *TitleCharge[kTotalSigns] = {"(+)", "(-)"};	  
static const Char_t *NamesF[NHYPS]    = {"electron","antiproton","kaon-","pion-","muon-","dbar","tbar","He3Bar","alphabar"
					 "positron","proton"    ,"kaon+","pion+","muon+","deuteron"   ,"triton"   ,"He3"    ,"alpha"};
static const Char_t *Names[NHYPS]     = {"e-","pbar","K-","pi-","mu-","dbar","tbar","He3Bar","alphabar"
					 "e+","p"   ,"K+","pi+","mu+","d"   ,"t"   ,"He3"    ,"alpha"};
static const Double_t Masses[NHYPS] = {0.51099907e-3,0.93827231,0.493677,0.13956995,0.1056584,1.875613,2.80925, 2.80923,3.727417,
				       0.51099907e-3,0.93827231,0.493677,0.13956995,0.1056584,1.875613,2.80925, 2.80923,3.727417};
static const Int_t GEANTiD[NHYPS]    = { 3, 15, 12,  9, 6, 53, 50046, 50049, 50047, // GEANT part Id
					 2, 14, 11,  8, 5, 45,    46,    49,   47};
static const Char_t *HistNames[NHYPS] = {"eNzB","protonNzB","kaonNzB","piNzB","muNzB","deuteronNzB","tritonNzB","He3NzB","alphaNzB",
				  "ePzB","protonPzB","kaonPzB","piPzB","muPzB","deuteronPzB","tritonPzB","HePzB","alphaPzB"};
static const Char_t *HistNames70[NHYPS] = {"eN70B","protonN70B","kaonN70B","piN70B","muN70B","deuteronN70B","tritonN70B","He3N70B","alphaN70B",
				    "eP70B","protonP70B","kaonP70B","piP70B","muP70B","deuteronP70B","tritonP70B","He3P70B","alphaP70B"};
static const Char_t *HistNameP[NHYPS] = {"eNzB","protonNzB","kaonNzB","piNzB","muNzB","deuteronNzB","tritonNzB","He3NzB","alphaNzB",
				  "ePzB","protonPzB","kaonPzB","piPzB","muPzB","deuteronPzB","tritonPzB","He3PzB","alphaPzB"};
static const Char_t *HitName = "vs NoFitPnts and no. bad hits";
static const Char_t *KinName = "vs   #eta and pT/|q|";
static const Char_t *KinPionName = "vs   #eta and pT/|q| for pion";
static const Char_t *proj[5] = {"zx","zy","x","y","yx"};
static TProfile3D *PdEdx[2][NHYPS] = {0};
static TH3F *LdEdx[2][NHYPS] = {0};
static Int_t nPng = 0;
//_____________________________________________________________________________
Int_t StMuMcAnalysisMaker::Init(){
  assert(StMuDstMaker::instance());
  StMuDstMaker::instance()->SetStatus("*",0);
  const Char_t *ActiveBranches[] = {
    "MuEvent"
    ,"PrimaryVertices"
    ,"PrimaryTracks"
    ,"GlobalTracks"
    ,"StStMuMcVertex"
    ,"StStMuMcTrack"
    ,"CovPrimTrack"
    ,"CovGlobTrack"
    ,"StStMuMcVertex"
    ,"StStMuMcTrack"
    ,"KFTracks"
    ,"KFVertices"
    ,"StBTofHit"
    ,"StBTofHeader"
  }; 
  Int_t Nb = sizeof(ActiveBranches)/sizeof(Char_t *);
  for (Int_t i = 0; i < Nb; i++) StMuDstMaker::instance()->SetStatus(ActiveBranches[i],1); // Set Active braches
  TFile *f = GetTFile();
  if (f) {
    f->cd();
    BookTrackPlots();
    BookVertexPlots();
  }
  return StMaker::Init();
}
//_____________________________________________________________________________
void StMuMcAnalysisMaker::BookTrackPlots(){
#if 1
  Int_t    npT    = 104;
  //  Double_t pTMax =   10;
  const Double_t ptBins[105] = {
    0.07, 0.08, 0.11, 0.14, 0.16, 0.17, 0.19, 0.21, 0.22, 0.23,
    0.24, 0.26, 0.27, 0.28, 0.29, 0.30, 0.31, 0.32, 0.33, 0.34,
    0.35, 0.36, 0.37, 0.38, 0.39, 0.40, 0.41, 0.42, 0.43, 0.44,
    0.45, 0.46, 0.47, 0.48, 0.49, 0.50, 0.51, 0.53, 0.54, 0.55,
    0.56, 0.57, 0.58, 0.60, 0.61, 0.62, 0.63, 0.65, 0.66, 0.67,
    0.69, 0.70, 0.72, 0.73, 0.75, 0.76, 0.78, 0.80, 0.81, 0.83,
    0.85, 0.87, 0.89, 0.91, 0.93, 0.96, 0.98, 1.01, 1.03, 1.06,
    1.09, 1.12, 1.16, 1.19, 1.23, 1.27, 1.31, 1.35, 1.40, 1.45,
    1.51, 1.57, 1.64, 1.71, 1.80, 1.89, 2.00, 2.11, 2.24, 2.39,
    2.57, 2.78, 3.05, 3.38, 3.80, 4.28, 4.96, 5.88, 7.25, 8.55,
    10.00, 25.0, 50., 75., 100.
  };
#else
  Int_t    npT    = 100;
  Double_t ptBins[101];
  ptBins[0] = 0;
  for (Int_t i = 1; i <= npT; i++) ptBins[i] = i;
#endif
  enum {nphiM = 10};
  Double_t dphiMask[nphiM] = {0,  5., 10., 12., 14., 15., 16., 18., 20., 25.};
  Int_t    nphi   = 12*nphiM;
  Double_t *phiBins = new Double_t [nphi+1];
  Int_t i = 0;
  for (Int_t sec = 0; sec < 12; sec++) {
    Double_t phi = -180 + 30*sec;
    for (Int_t j = 0; j < nphiM; j++, i++) {
      phiBins[i] = phi +  dphiMask[j];
    }
  }
  phiBins[nphi] = 180.;
#ifdef __DEVT__
  Int_t    neta   = 100; 
  Double_t etamax = 2.5;
#else
  Int_t    neta   =  60; 
  Double_t etamax = 1.5;
#endif
  Double_t deta = 2*etamax/neta;
  Double_t *etaBins = new Double_t [neta+1];
  for (i = 0; i <= neta; i++) {etaBins[i] = -etamax + deta*i;}
  TDirectory *dirs[7] = {0};
  const Char_t *TracksVertices[2] = {"Tracks","Vertices"};
  dirs[0] = TDirectory::CurrentDirectory(); assert(dirs[0]);
  dirs[0]->cd();
  if (! dirs[0]->GetDirectory(TracksVertices[0])) {
    dirs[0]->mkdir(TracksVertices[0]);
  }
  dirs[1] = dirs[0]->GetDirectory(TracksVertices[0]); assert(dirs[1]);
  dirs[1]->cd();
  for (Int_t gp = kGlobal; gp < kTotalT; gp++) {
    const  PlotName_t plotNameMatch[kTotalMatchType] = {
      {kMcTk,    "Mc",    "Mc tracks All"},
      {kMcTpcTk, "Tpc",   Form("Mc tracks which have >= %i Mc Tpc Hits",StMuDst::MinNoTpcMcHits)},
      {kRecoTk,  "Rec",   "Rc tracks matched with only Mc track"},
      {kCloneTk, "Clone", "Mc tracks matched with > 1 Rc track (Clone)"},
      {kGhostTk, "Ghost", "Rc tracks without Mc partner"},
      {kLostTk,  "Lost",  "Mc tracks without reconstructed one"},
      {kMcHftTk, "Hft",   Form("Mc tracks which have >= %i Mc Tpc and >= Hft Hits",StMuDst::MinNoTpcMcHits)},
      {kRecoHftTk,  "RecHft",   "Rc tracks matched with only Mc track"},
      {kCloneHftTk, "CloneHft", "Mc tracks matched with > 1 Rc track (Clone)"},
      {kGhostHftTk, "GhostHft", "Rc tracks without Mc partner"},
      {kLostHftTk,  "LostHft",  "Mc tracks without reconstructed one"}
    };
    if (! dirs[1]->GetDirectory(TitleTrType[gp])) {
      dirs[1]->mkdir(TitleTrType[gp]);
    }
    if (! dirs[1]->GetDirectory(TitleTrType[gp])) {
      dirs[1]->mkdir(TitleTrType[gp]);
    }
    dirs[2] = dirs[1]->GetDirectory(TitleTrType[gp]); assert(dirs[2]);
    dirs[2]->cd();
    for (Int_t type = kMcTk; type < kTotalMatchType; type++) {
      if (! dirs[2]->GetDirectory(plotNameMatch[type].Name)) {
	dirs[2]->mkdir(plotNameMatch[type].Name);
      }
      dirs[3] = dirs[2]->GetDirectory(plotNameMatch[type].Name); assert(dirs[3]);
      dirs[3]->cd();
      const Char_t *ParticleType[2] = {"All","Pion"};
      for (Int_t particle = 0; particle < kPartypeT; particle++) {
	if (! dirs[3]->GetDirectory(ParticleType[particle])) {
	  dirs[3]->mkdir(ParticleType[particle]);
	}
	dirs[4] = dirs[3]->GetDirectory(ParticleType[particle]); assert(dirs[4]);
	dirs[4]->cd();
	for (Int_t pm = kPositive; pm < kTotalSigns; pm++) {
	  if (! dirs[4]->GetDirectory(TitleCharge[pm])) {
	    dirs[4]->mkdir(TitleCharge[pm]);
	  }
	  dirs[5] = dirs[4]->GetDirectory(TitleCharge[pm]); assert(dirs[5]);
	  dirs[5]->cd();
	  const Char_t *VarSet[kVariables] = {"NoHits","EtapT"};
	  for (Int_t x = 0; x < kVariables; x++) {
	    if (x == 0 && type != kRecoTk) continue;
	    if (! dirs[5]->GetDirectory(VarSet[x])) {
	      dirs[5]->mkdir(VarSet[x]);
	    }
	    dirs[6] = dirs[5]->GetDirectory(VarSet[x]); assert(dirs[6]);
	    dirs[6]->cd();
	    //                /GlobalTracks/Mc/All/(+)/NoHits
	    TString dir(Form("/%s/%s/%s/%s/%s/%s",TracksVertices[0],TitleTrType[gp],plotNameMatch[type].Name,ParticleType[particle],TitleCharge[pm],VarSet[x]));
	    const VarName_t plotVar[kTotalQAll] = {         //no.fit                      no.bad,                               
	      {"ChiSqXY",   "#chi^{2}_{Track}/NDF",          noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, 100,  0.,  10., 0.000, 6.000, 1},
	      {"ChiSqZ",    "#chi^{2}_{Vx} ",                noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, 100,  0., 100., 0.000,10.000,-1},
	      {"dDcaXY",    "difference in Dca_{XY}",        noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, 100, -5.,   5., -.250, 1.500, 0},
	      {"dDcaZ",     "difference in Dca_{Z}",         noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, 100, -5.,   5., -.250, 1.500, 0},
	      {"dPsi",      "difference in  #Psi ",          noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, 100, -0.1, 0.1, -.004, 0.040, 1},
	      {"dPti" ,     "difference in q/pT",            noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, 100, -0.1, 0.1, -.020, 0.200, 1},
	      {"dPtiR" ,    "difference in relative q/pT",   noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, 100, -0.1, 0.1, -.004, 0.040, 1},
	      {"dTanL",     "difference in tan( #lambda )",  noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, 100, -0.1, 0.1, -.004, 0.040, 1},
	      {"deta",      "difference in  #eta",           noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, 100, -0.1, 0.1, -.002, 0.025,-1},
	      {"pDcaXY",    "pull in Dca_{XY}",              noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, 100, -10., 10., -.300, 3.000, 1},
	      {"pDcaZ",     "pull in Dca_{Z}",               noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, 100, -10., 10., -.100,10.000, 1},
	      {"pPsi",      "pull in  #Psi ",                noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, 100, -10., 10., -.300, 3.000, 1},
	      {"pPti" ,     "pull in q/pT",                  noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, 100, -10., 10., -.250, 2.500, 1},
	      {"pPtiR" ,    "pull in relative q/pT",         noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, 100, -10., 10., -.230, 3.500, 1},
	      {"pTanL",     "pull in tan( #lambda )",        noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, 100, -10., 10., -.600, 6.000, 1},
	      {"peta",      "pull for  #eta",                noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5, 100, -10., 10.,-1.000,10.000,-1},
	      {"Phi",       "#phi (degrees)",                      0,   0,           0,  0,   0,    0, 120,-180.,180.,-1.000,10.000, 1}
	    };
	    Int_t i1 = 0;
	    if (type != kRecoTk) i1 = kTotalQA;
	    for (Int_t i = i1; i < kTotalQAll; i++) {
	      if (gp == kGlobal && plotVar[i].GlobalOnly <  0) continue;
	      if (gp == kPrimary && plotVar[i].GlobalOnly == 0) continue;
	      if (fHistsT[gp][type][particle][pm][x][i] = (TH3F *) dirs[6]->Get(plotVar[i].Name)) continue;
	      if (! x) {// No.Hits
		if (i == kTotalQA) continue;
		if (! fHistsT[gp][type][particle][pm][x][i])
		fHistsT[gp][type][particle][pm][x][i] = new TH3F(plotVar[i].Name,
								 Form("%s for %s %s %s %s %s", plotVar[i].Title,
								      TitleTrType[gp],plotNameMatch[type].Name,ParticleType[particle],TitleCharge[pm],VarSet[x]),
								 noFit-9, 9.5, noFit + 0.5, 11,-0.5, 10.5,
								 plotVar[i].nz, plotVar[i].zmin, plotVar[i].zmax);
		fHistsT[gp][type][particle][pm][x][i]->GetXaxis()->SetTitle("No. of Fit Points");
		fHistsT[gp][type][particle][pm][x][i]->GetYaxis()->SetTitle("No. of Bad Points");
		fHistsT[gp][type][particle][pm][x][i]->GetZaxis()->SetTitle(plotVar[i].Title);
		fHistsT[gp][type][particle][pm][x][i]->SetMarkerColor(pm+1); 
		fHistsT[gp][type][particle][pm][x][i]->SetLineColor(pm+1); 
		for (Int_t l = 1; l < 7; l++) cout << "/" << dirs[l]->GetName();
		cout << "\t" << "fHistsT[" << gp << "][" << type << "][" << particle << "][" << pm << "][" << x << "][" << i << "]";
		cout << " = " << fHistsT[gp][type][particle][pm][x][i]->GetName() << "\t" << fHistsT[gp][type][particle][pm][x][i]->GetTitle() << endl;
	      } else { // eta & pT
		if (i != kTotalQA) {
		  Double_t *zBins = new Double_t[plotVar[i].nz+1];
		  Double_t dz = (plotVar[i].zmax - plotVar[i].zmin)/plotVar[i].nz;
		  for (Int_t j = 0; j <= plotVar[i].nz; j++) zBins[j] = plotVar[i].zmin + dz*j;
		  fHistsT[gp][type][particle][pm][x][i] = new TH3F(plotVar[i].Name,
								   Form("%s for %s %s %s %s %s", plotVar[i].Title,
								      TitleTrType[gp],plotNameMatch[type].Name,ParticleType[particle],TitleCharge[pm],VarSet[x]),
								   neta, etaBins,
								   npT, ptBins,
								   plotVar[i].nz, zBins);
		  delete [] zBins;
		} else {
		  fHistsT[gp][type][particle][pm][x][i] = new TH3F(plotVar[i].Name,
								   Form("%s for %s %s %s %s %s", plotVar[i].Title,
								      TitleTrType[gp],plotNameMatch[type].Name,ParticleType[particle],TitleCharge[pm],VarSet[x]),
								   neta, etaBins,
								   npT, ptBins,
								   nphi, phiBins);
		}
		fHistsT[gp][type][particle][pm][x][i]->GetXaxis()->SetTitle("  #eta");
		fHistsT[gp][type][particle][pm][x][i]->GetYaxis()->SetTitle("pT/|q| (GeV/c)");

		fHistsT[gp][type][particle][pm][x][i]->GetZaxis()->SetTitle(plotVar[i].Title);
		fHistsT[gp][type][particle][pm][x][i]->SetMarkerColor(pm+1); 
		fHistsT[gp][type][particle][pm][x][i]->SetLineColor(pm+1); 
		for (Int_t l = 1; l < 7; l++) cout << "/" << dirs[l]->GetName();
		cout << "\t" << "fHistsT[" << gp << "][" << type << "][" << particle << "][" << pm << "][" << x << "][" << i << "]";
		cout << " = " << fHistsT[gp][type][particle][pm][x][i]->GetName() << "\t" << fHistsT[gp][type][particle][pm][x][i]->GetTitle() << endl;
	      }
	      if (fHistsT[gp][type][particle][pm][x][i]) {
		for (Int_t l = 1; l < 7; l++) cout << "/" << dirs[l]->GetName();
		cout << "\t" << "fHistsT[" << gp << "][" << type << "][" << particle << "][" << pm << "][" << x << "][" << i << "]";
		cout << " = " << fHistsT[gp][type][particle][pm][x][i]->GetName() << "\t" << fHistsT[gp][type][particle][pm][x][i]->GetTitle() << endl;
		ofstream out;
		TString Out("Hist.list");
		if (gSystem->AccessPathName(Out)) out.open(Out, ios::out); //"Results.list",ios::out | ios::app);
		else                              out.open(Out, ios::app);
		out << "\t" << "fHistsT[" << gp << "][" << type << "][" << particle << "][" << pm << "][" << x << "][" << i << "]";
		out << " = " << fHistsT[gp][type][particle][pm][x][i]->GetName() << "\t" << fHistsT[gp][type][particle][pm][x][i]->GetTitle() << endl;
		out.close();
	      }
	    }
	  }
	}
      }
    }
  }
  delete [] phiBins;
  delete [] etaBins;
  dirs[1]->cd();
  // dE/dx block for matched primary tracks
  const Char_t *dEdxTypes[2] = {"I70","Fit"};
  for (Int_t h = 0; h < NHYPS; h++) {
    for (i = 0; i < 2; i++) {
      PdEdx[i][h] = (TProfile3D *) dirs[1]->Get(Form("Zav%s%s",dEdxTypes[i],HistNames[h]));
      if (! PdEdx[i][h]) {
	PdEdx[i][h] = new TProfile3D(Form("Zav%s%s",dEdxTypes[i],HistNames[h]),
				     Form("< z_{%s} > versus  #phi,  #eta,  p_{T}/|q| for %s",dEdxTypes[i],Names[h]),
				     90, -TMath::Pi(), TMath::Pi(),
				     60, -1.2, 1.2,
				     npT, 0, 10, "S");
	PdEdx[i][h]->GetXaxis()->SetTitle("#phi (rad)");
	PdEdx[i][h]->GetYaxis()->SetTitle("  #eta");         
	PdEdx[i][h]->GetZaxis()->SetTitle(Form("Z_{%s}{%s}",dEdxTypes[i],HistNames[h]));   
	PdEdx[i][h]->GetZaxis()->Set(npT,ptBins);
      }
      LdEdx[i][h] = (TH3F *) dirs[1]->Get(Form("Z%s%s",dEdxTypes[i],HistNames[h]));
      if (! LdEdx[i][h]) {
	LdEdx[i][h] = new TH3F(Form("Z%s%s",dEdxTypes[i],HistNames[h]),
			       Form(" z_{%s}  versus TpcTrackLength and log_{10} (#beta #gamma) for %s",dEdxTypes[i],Names[h]),
			       110, 0, 220, 220,-1,10, 100, -1, 1); 
	LdEdx[i][h]->GetXaxis()->SetTitle("TpcTrackLength (cm)");
	LdEdx[i][h]->GetYaxis()->SetTitle("log_{10} (#beta #gamma)");         
	LdEdx[i][h]->GetZaxis()->SetTitle(Form(" z_{%s}{%s}",dEdxTypes[i],Names[h]));   
      }
    }
  }
  GiD[0] = (TH1F *)  dirs[1]->Get("GiD");
  if (! GiD[0]) GiD[0] = new TH1F("GiD","Geant ID for all MC tracks",50,0.5,50.5);
  GiD[1] = (TH1F *)  dirs[1]->Get("GiDG");
  if (! GiD[1]) GiD[1] = new TH1F("GiDG",Form("Geant ID for MC tracks with >= %i Tpc MC hits",StMuDst::MinNoTpcMcHits),50,0.5,50.5);
  GiD[2] = (TH1F *)  dirs[1]->Get("GiDPr");
  if (! GiD[2]) GiD[2] = new TH1F("GiDPr","Geant ID for all primary MC tracks",50,0.5,50.5);
  GiD[3] = (TH1F *)  dirs[1]->Get("GiDPrG");
  if (! GiD[3]) GiD[3] = new TH1F("GiDPrG",Form("Geant ID for primary MC tracks with >= %i Tpc MC hits",StMuDst::MinNoTpcMcHits),50,0.5,50.5);
  McRcHit = (TH2F *)   dirs[1]->Get("McRcHit");
  if (! McRcHit) McRcHit = new TH2F("McRcHit","No. RC hits in TPC versus No. MC ones",80,-0.5,79.5,80,-0.5,79.5);
}
//_____________________________________________________________________________
void StMuMcAnalysisMaker::BookVertexPlots(){
}
//_____________________________________________________________________________
Int_t StMuMcAnalysisMaker::Make(){
  StMuDstMaker *muDstMaker = StMuDstMaker::instance();
  if (! muDstMaker) return kStFatal;
  muDst = muDstMaker->muDst();
#if 0
  TObjectSet *muSet = (TObjectSet *) GetDataSet("muDst");
  if (! muSet) return kStFatal;
  muDst = (StMuDst *) muSet->GetObject();
#endif
  if (! muDst) return kStOK;
    if (Debug()) {
      muDst->Print();
      muDst->printVertices();
      muDst->printPrimaryTracks();
      muDst->printGlobalTracks();
      muDst->printKFVertices();
      muDst->printKFTracks();
      muDst->printMcVertices();
      muDst->printMcTracks();
    }
  FillTrackPlots();
  FillVertexPlots();
  return kStOK;
}
//_____________________________________________________________________________
void StMuMcAnalysisMaker::FillTrackPlots(){
  if (! muDst || ! muDst->event()) return;
  const Double_t field = muDst->event()->magneticField()*kilogauss;
  map<Int_t,Int_t> &Gl2Pr = muDst->IdGlobalId2IdPrimaryTrack(); // map global to primary track Ids from vertex with idTruth == 1
  multimap<Int_t,Int_t> &Mc2RcTracks = muDst->IdMc2IdRcTracks(); // map between global and Mc tracks from primary Mc vertex
  // =============  Build map between  Rc and Mc vertices 
  multimap<Int_t,Int_t> Mc2RcVertices = muDst->IdMc2IdRcVertices(); // Reconstructable !
  // Loop over Mc Tracks
  for (Int_t m = 0; m < muDst->numberOfMcTracks(); m++) {
    StMuMcTrack *mcTrack = muDst->MCtrack(m);
    if (! mcTrack) continue;
    // Select only Triggered Mc Vertex
    Int_t IdVx = mcTrack->IdVx();
    while (IdVx != 1) { // Find parent vertex 
      StMuMcVertex *mcVertex = muDst->MCvertex(IdVx-1);
      Int_t idMcTrack = mcVertex->IdParTrk();
      if (! idMcTrack) break;
      StMuMcTrack *mcTrackP = muDst->MCtrack(idMcTrack-1);
      IdVx = mcTrackP->IdVx();
      if (! IdVx) break;
    }
    if (IdVx != 1) continue;
    Bool_t McTpc = mcTrack->No_tpc_hit() >= StMuDst::MinNoTpcMcHits;
    Bool_t McHft = mcTrack->No_pix_hit() >= 2 && mcTrack->No_ist_hit()+mcTrack->No_ssd_hit() >= 1;
    GiD[0]->Fill(mcTrack->GePid());
    if (McTpc) GiD[1]->Fill(mcTrack->GePid());
    if (IdVx == 1) {
      GiD[2]->Fill(mcTrack->GePid());
      if (McTpc) GiD[3]->Fill(mcTrack->GePid());
    }
    if (! mcTrack->Charge()) continue;
    EChargeType pm = kPositive;
    if (mcTrack->Charge() < 0) pm = kNegative;
    Int_t NPart = kallP;
    if (mcTrack->GePid() == 8 || mcTrack->GePid() == 9) NPart = kPion;
    for (Int_t particle = 0; particle <= NPart; particle++) {
      fHistsT[kGlobal][kMcTk][particle][pm][1][kTotalQA]->Fill(mcTrack->Pxyz().pseudoRapidity(),mcTrack->Pxyz().perp(), TMath::RadToDeg()*mcTrack->Pxyz().phi());
      if (IdVx == 1)
	fHistsT[kPrimary][kMcTk][particle][pm][1][kTotalQA]->Fill(mcTrack->Pxyz().pseudoRapidity(),mcTrack->Pxyz().perp(), TMath::RadToDeg()*mcTrack->Pxyz().phi());
      if (! McTpc) continue; 
      fHistsT[kGlobal][kMcTpcTk][particle][pm][1][kTotalQA]->Fill(mcTrack->Pxyz().pseudoRapidity(),mcTrack->Pxyz().perp(), TMath::RadToDeg()*mcTrack->Pxyz().phi());
      if (IdVx == 1)
      fHistsT[kPrimary][kMcTpcTk][particle][pm][1][kTotalQA]->Fill(mcTrack->Pxyz().pseudoRapidity(),mcTrack->Pxyz().perp(), TMath::RadToDeg()*mcTrack->Pxyz().phi());
      if (McHft) {
	fHistsT[kGlobal][kMcHftTk][particle][pm][1][kTotalQA]->Fill(mcTrack->Pxyz().pseudoRapidity(),mcTrack->Pxyz().perp(), TMath::RadToDeg()*mcTrack->Pxyz().phi());
	if (IdVx == 1)
	  fHistsT[kPrimary][kMcHftTk][particle][pm][1][kTotalQA]->Fill(mcTrack->Pxyz().pseudoRapidity(),mcTrack->Pxyz().perp(), TMath::RadToDeg()*mcTrack->Pxyz().phi());
      }
      // kNotDefined, kLostTk, kRecoTk, kCloneTk
      TrackMatchType type    = TrackType(mcTrack,Mc2RcTracks);
      // kNotDefined, kLostHftTk, kRecoHftTk, kCloneHftTk
      TrackMatchType typeHft = TrackType(mcTrack,Mc2RcTracks,kTRUE);
      if (typeHft == kLostHftTk && ! McHft) typeHft = kNotDefined;
      if (typeHft == kRecoHftTk && ! McHft) typeHft = kGhostHftTk;
      if (type != kNotDefined) {
	fHistsT[kGlobal][type][particle][pm][1][kTotalQA]->Fill(mcTrack->Pxyz().pseudoRapidity(),mcTrack->Pxyz().perp(), TMath::RadToDeg()*mcTrack->Pxyz().phi());
	fHistsT[kPrimary][type][particle][pm][1][kTotalQA]->Fill(mcTrack->Pxyz().pseudoRapidity(),mcTrack->Pxyz().perp(), TMath::RadToDeg()*mcTrack->Pxyz().phi());
      }
      if (typeHft != kNotDefined) {
	fHistsT[kGlobal][typeHft][particle][pm][1][kTotalQA]->Fill(mcTrack->Pxyz().pseudoRapidity(),mcTrack->Pxyz().perp(), TMath::RadToDeg()*mcTrack->Pxyz().phi());
	fHistsT[kPrimary][typeHft][particle][pm][1][kTotalQA]->Fill(mcTrack->Pxyz().pseudoRapidity(),mcTrack->Pxyz().perp(), TMath::RadToDeg()*mcTrack->Pxyz().phi());
      }
      if (type == kRecoTk || typeHft == kRecoHftTk) {
	Int_t Id = mcTrack->Id()-1;
	pair<multimap<Int_t,Int_t>::iterator,multimap<Int_t,Int_t>::iterator> ret = Mc2RcTracks.equal_range(Id);
	multimap<Int_t,Int_t>::iterator it;
	Int_t kg = -1;
	Int_t count = 0;
	for (it = ret.first; it != ret.second; ++it, ++count) {
	  kg = (*it).second;
	}
	assert(count == 1);
	// Track QA
	StMuTrack *gTrack = muDst->globalTracks(kg);
	Int_t kgc = gTrack->index2Cov();
	if (kgc < 0) continue;
	StDcaGeometry *dcaG = (StDcaGeometry *) muDst->covGlobTrack()->UncheckedAt(kgc);
	StMuMcVertex *mcVertex = muDst->MCvertex(IdVx-1);
	if (type    == kRecoTk)    FillQAGl(type   ,gTrack, mcTrack, dcaG, mcVertex);
	if (typeHft == kRecoHftTk) FillQAGl(typeHft,gTrack, mcTrack, dcaG, mcVertex);
	Int_t k = Gl2Pr[kg];
	if (! k) continue;
	StMuTrack *pTrack = (StMuTrack *) muDst->array(muPrimary)->UncheckedAt(k);
	Int_t kpc = pTrack->index2Cov();
	if (kpc >= 0) {
	  StMuPrimaryTrackCovariance *cov = (StMuPrimaryTrackCovariance *) muDst->covPrimTrack()->UncheckedAt(kpc);
	  if (type    == kRecoTk)    FillQAPr(type   , pTrack, mcTrack, cov);
	  if (typeHft == kRecoHftTk) FillQAPr(typeHft, pTrack, mcTrack, cov);
	} else {
	  KFParticle *particle = 0;
	  if (k < muDst->numberOfKFTracks()) particle = muDst->KFtrack(k);
	  if (type    == kRecoTk)    FillQAPr(type   ,pTrack, mcTrack, particle);
	  if (typeHft == kRecoHftTk) FillQAPr(typeHft,pTrack, mcTrack, particle);
	}
	// dE/dx block
	const StMuProbPidTraits &PiD = pTrack->probPidTraits();
	Double_t I[2] = {PiD.dEdxTruncated(), PiD.dEdxFit()};
	Double_t TrackLength = PiD.dEdxTrackLength();
	Int_t Gid = mcTrack->GePid();
	static Bichsel *m_Bichsel = Bichsel::Instance();
	Double_t pMomentum = pTrack->helix().momentum(field).mag();
	//	const StThreeVectorF &pVx  = pTrack->momentum();
	for (Int_t h = 0; h < NHYPS; h++) {
	  if (GEANTiD[h] == Gid) {
	    Double_t bghyp = TMath::Log10(pMomentum/Masses[h]);
	    Double_t Pred[2]  = {1.e-6*m_Bichsel->GetI70(bghyp,1.0),
				 1.e-6*TMath::Exp(m_Bichsel->GetMostProbableZ(bghyp,1.0))};
	    for (Int_t mm = 0; mm < 2; mm++) {
	      if (I[mm] <= 0 || Pred[mm] <= 0) continue;
	      Double_t z = TMath::Log(I[mm]/Pred[mm]);
	      PdEdx[mm][h]->Fill(pTrack->phi(), pTrack->eta(), pTrack->pt(), z);
	      LdEdx[mm][h]->Fill(TrackLength, bghyp, z);
	    }
	    break;
	  }
	}
      }
    }
    // check for ghost
    for (Int_t kg = 0; kg < muDst->numberOfGlobalTracks(); kg++) {
      StMuTrack *gTrack = muDst->globalTracks(kg);
      if ( ! AcceptGhost(gTrack)) continue;
      if ( gTrack->idTruth()) continue;
      EChargeType pm = kPositive;
      if (gTrack->charge() < 0) pm = kNegative;
      fHistsT[kGlobal][kGhostTk][0][pm][1][kTotalQA]->Fill(gTrack->eta(),(gTrack->charge()*gTrack->pt()),TMath::RadToDeg()*gTrack->phi());
    }
    for (Int_t l = 0; l < muDst->numberOfPrimaryVertices(); l++) {
      StMuPrimaryVertex *Vtx = muDst->primaryVertex(l);
      if (Vtx->idTruth() != 1) continue;
      for (Int_t k = 0; k < muDst->numberOfPrimaryTracks(); k++) {
	StMuTrack *pTrack = (StMuTrack *) muDst->array(muPrimary)->UncheckedAt(k);
	if (! pTrack) continue;
        if (pTrack->vertexIndex() != l) continue;
	if (! AcceptGhost(pTrack)) continue;
	if (pTrack->idParentVx() == 1) continue;
	EChargeType pm = kPositive;
	if (pTrack->charge() < 0) pm = kNegative;
	fHistsT[kPrimary][kGhostTk][0][pm][1][kTotalQA]->Fill(pTrack->eta(),pTrack->pt(),TMath::RadToDeg()*pTrack->phi());
      }
    }
  }
}
//_____________________________________________________________________________
void StMuMcAnalysisMaker::FillVertexPlots(){
#if 0
    // Loop over KF Vetrices and KF particles
    // Map between Id and position in Clones Array
    map<Int_t,Int_t> VerId2k;
    for (Int_t l = 0; l < NoKFVertices; l++) {
      const KFVertex *vertex = (const KFVertex *) KFVertices->UncheckedAt(l);
      if (! vertex) continue;
      Int_t Id = vertex->GetID();
      VerId2k[Id] = l;
    }
    map<Int_t,Int_t> ParId2k;
    for (Int_t k = 0; k < muDst->numberOfKFTracks(); k++) {
      const KFParticle *particle = (const KFParticle *) KFTracks->UncheckedAt(k);
      if (! particle) continue;
      Int_t Id = particle->GetID();
      ParId2k[Id] = k;
    }
    for (Int_t l = 0; l < NoKFVertices; l++) {
      const KFVertex *vertex = (const KFVertex *) KFVertices->UncheckedAt(l);
      cout << *vertex << endl;
      if (vertex->IdTruth()) {
	StMuMcVertex *mcVertex = muDst->MCtrack(vertex->IdTruth()-1);
	if (mcVertex) cout << "Mc Vertex:" << *mcVertex << endl;
      }
      Int_t IdPtrk = vertex->GetParentID(); //reconstructed parent track
      if (IdPtrk) {
	Int_t k = ParId2k[IdPtrk];
	const KFParticle *particle = (const KFParticle *) KFTracks->UncheckedAt(k);
	if (particle) cout << "Parent Track:" << *particle << endl;
      }
      Int_t m = vertex->IdParentMcVx(); // MC parent track
      if (m) {
	StMuMcTrack *mcTrack = muDst->MCtrack(m-1);
	if (! mcTrack) continue;
	cout << "Parent Mc Track:" << *mcTrack << endl;
      }
    }
    cout << "-----------------------------------" << endl;
    for (Int_t k = 0; k < muDst->numberOfKFTracks(); k++) {
      const KFParticle *particle = (const KFVertex *) KFTracks->UncheckedAt(k);
      cout << *particle << endl;
      if (! particle->GetID()) {cout << "beam" << endl; continue;}
      Int_t IdPVx = particle->GetParentID(); //reconstructed parent vertex
      if (particle->IdTruth()) {
	StMuMcTrack *mcTrack = muDst->MCtrack(particle->IdTruth()-1);
	if (mcTrack) cout << "Mc Track:" << *mcTrack << endl;
      }
      if (IdPVx) {
	Int_t l = VerId2k[IdPVx];
	const KFVertex *vertex = (const KFVertex *) KFVertices->UncheckedAt(l);
	if (vertex) cout << "Parent Vertex:" << *vertex << endl;
      }
      Int_t m = particle->IdParentMcVx(); // MC parent vertex
      if (m) {
	StMuMcVertex *mcVertex = muDst->MCtrack(m-1);
	if (! mcVertex) continue;
	cout << "Parent Mc Vertex:" << *mcVertex << endl;
      }
      
    }
    cout << "===================================" << endl;
#endif
}
//________________________________________________________________________________
void StMuMcAnalysisMaker::FillQAGl(TrackMatchType type,const StMuTrack *gTrack, const StMuMcTrack *mcTrack, const StDcaGeometry *dcaG, const StMuMcVertex *mcVertex) {
  if (! gTrack || ! mcTrack) return;
  if (! dcaG   || ! mcVertex) return;
  EChargeType pm = kPositive;
  if (mcTrack->Charge() < 0) pm = kNegative;
  Var_t var; memset(&var.ChiSqXY, 0, sizeof(var));
  TRSymMatrix Cov(5,dcaG->errMatrix());
  Double_t vtx[3] = {mcVertex->XyzV().x(), mcVertex->XyzV().y(), mcVertex->XyzV().z()};
  THelixTrack     thelix =  dcaG->thelix();
  Double_t ermx[3];
  Double_t pars[2];
  thelix.Dca(vtx,pars[0],pars[1],ermx,2);
  var.ChiSqXY = gTrack->chi2xy();
  var.dDcaXY  = pars[0];
  var.dDcaZ   = pars[1];
  Double_t *Dir = thelix.Dir();
  Double_t phi =  TMath::ATan2(Dir[1],Dir[0]);
  var.dPsi    = phi - mcTrack->Pxyz().phi(); 
  var.Phi = TMath::RadToDeg()*mcTrack->Pxyz().phi();
  Double_t pTqRC = gTrack->pt()/gTrack->charge();
  Double_t pTqMC = mcTrack->pT()/mcTrack->Charge();
  var.dPti    = 1./pTqRC - 1./pTqMC;
  var.dPtiR   = pTqMC/pTqRC - 1;
  Double_t    tanDip =  mcTrack->Pxyz().z()/mcTrack->Pxyz().perp();
  var.dTanL   = thelix.GetTan() - tanDip;
  if (ermx[0] <= 0 || ermx[2] <= 0) {
    gTrack->Print();
    mcTrack->Print();
    dcaG->Print("");
    mcVertex->Print();
    thelix.Print();
  } else {
    var.pDcaXY  = var.dDcaXY/TMath::Sqrt(ermx[0]);
    var.pDcaZ   = var.dDcaZ /TMath::Sqrt(ermx[2]);
  }
  if (Cov(2,2) <= 0 || Cov(3,3) <= 0 || Cov(4,4) <= 0) {
    gTrack->Print();
    mcTrack->Print();
    dcaG->Print("");
    mcVertex->Print();
  } else {
    var.pPsi    = var.dPsi  /TMath::Sqrt(Cov(2,2));
    var.pPti    = var.dPti  /TMath::Sqrt(Cov(3,3));
    var.pPtiR   = var.dPtiR /TMath::Sqrt(Cov(3,3)) / mcTrack->Pxyz().perp();
    var.pTanL   = var.dTanL /TMath::Sqrt(Cov(4,4));
  }
  Double_t *x = &var.ChiSqXY;
  Int_t Npart = 1;
  if (mcTrack->GePid() == 8 || mcTrack->GePid() == 9) Npart = 2;
  for (Int_t particle = 0; particle < Npart; particle++) {
    for (Int_t i = 0; i < kTotalQA; i++) {
      if (fHistsT[kGlobal][type][particle][pm][0][i])
	fHistsT[kGlobal][type][particle][pm][0][i]->Fill(gTrack->nHitsFit(), gTrack->nHitsFit()*(100.-gTrack->qaTruth())/100., x[i]);
      if (fHistsT[kGlobal][type][particle][pm][1][i])
	fHistsT[kGlobal][type][particle][pm][1][i]->Fill(mcTrack->Pxyz().pseudoRapidity(), mcTrack->Pxyz().perp(), x[i]);
    }
  }
  McRcHit->Fill(gTrack->nHitsFit(),mcTrack->No_tpc_hit());
}
//________________________________________________________________________________
void StMuMcAnalysisMaker::FillQAPr(TrackMatchType type,const StMuTrack *pTrack, const StMuMcTrack *mcTrack, const StMuPrimaryTrackCovariance *cov) {
  if (! pTrack || ! mcTrack) return;
  if (! mcTrack->Charge()) return;
  EChargeType pm = kPositive;
  if (mcTrack->Charge() < 0) pm = kNegative;
  Var_t var; memset(&var.ChiSqXY, 0, sizeof(var));
  var.ChiSqXY = pTrack->chi2xy();
  var.ChiSqZ  = pTrack->chi2z();
  Double_t Eta = mcTrack->Pxyz().pseudoRapidity();
  var.deta    = pTrack->eta() - Eta;
  var.dPsi    = pTrack->phi() - mcTrack->Pxyz().phi();
  var.Phi = TMath::RadToDeg()*mcTrack->Pxyz().phi();
  Double_t pTqRC = pTrack->pt()/pTrack->charge();
  Double_t pTqMC = mcTrack->pT()/mcTrack->Charge();
  var.dPti    = 1./pTqRC - 1./pTqMC;
  var.dPtiR   = pTqMC/pTqRC - 1;
  var.peta    = 999.;
  var.pPsi    = 999.;
  var.pPti    = 999.;
  var.pPtiR   = 999.;
  if (cov) {
    TRSymMatrix Cov(3,cov->errMatrix());
    if (Cov(0,0) <= 0 || Cov(1,1) <= 0 || Cov(2,2) <= 0) {
      pTrack->Print();
      mcTrack->Print();
      cov->Print();
      Cov.Print();
    } else {
      var.peta    = var.deta / TMath::Sqrt(Cov(0,0)) / TMath::CosH(Eta);
      var.pPsi    = var.dPsi / TMath::Sqrt(Cov(1,1));
      var.pPti    = var.dPti / TMath::Sqrt(Cov(2,2)); 
      var.pPtiR   = var.dPtiR/ TMath::Sqrt(Cov(2,2)) / (mcTrack->pT()/mcTrack->Charge());
    }			
  }	
  Double_t *x = &var.ChiSqXY;
  Int_t Npart = 1;
  if (mcTrack->GePid() == 8 || mcTrack->GePid() == 9) Npart = 2;
  for (Int_t particle = 0; particle < Npart; particle++) {
    for (Int_t i = 0; i < kTotalQAll; i++) {
      if (fHistsT[kPrimary][type][particle][pm][0][i])
	fHistsT[kPrimary][type][particle][pm][0][i]->Fill(pTrack->nHitsFit(), pTrack->nHitsFit()*(100.-pTrack->qaTruth())/100., x[i]);
      if (fHistsT[kPrimary][type][particle][pm][1][i])
       fHistsT[kPrimary][type][particle][pm][1][i]->Fill(mcTrack->Pxyz().pseudoRapidity(), mcTrack->Pxyz().perp(), x[i]);
    }
  }
}
//________________________________________________________________________________
void StMuMcAnalysisMaker::FillQAPr(TrackMatchType type,const StMuTrack *pTrack, const StMuMcTrack *mcTrack, const KFParticle *particle) {
  if (! pTrack || ! mcTrack) return;
  if (! mcTrack->Charge()) return;
  EChargeType pm = kPositive;
  if (mcTrack->Charge() < 0) pm = kNegative;
  Var_t var; memset(&var.ChiSqXY, 0, sizeof(var));
  var.ChiSqXY = pTrack->chi2xy();
  var.ChiSqZ  = pTrack->chi2z();
  Double_t Eta = mcTrack->Pxyz().pseudoRapidity();
  var.deta    = pTrack->eta() - Eta;
  var.dPsi    = pTrack->phi() - mcTrack->Pxyz().phi();
  var.Phi = TMath::RadToDeg()*mcTrack->Pxyz().phi();
  Double_t pTqRC = pTrack->pt()/pTrack->charge();
  Double_t pTqMC = mcTrack->pT()/mcTrack->Charge();
  var.dPti    = 1./pTqRC - 1./pTqMC;
  var.dPtiR   = pTqMC/pTqRC - 1;
  var.peta    = 999.;
  var.pPsi    = 999.;
  var.pPti    = 999.;
  var.pPtiR   = 999.;
  if (particle) {
    var.ChiSqZ  = particle->GetChi2();
    Float_t pT, dpT;
    Float_t Eta, dEta;
    Float_t Phi, dPhi;
    if (! particle->GetPt(pT,dpT) && ! particle->GetEta(Eta,dEta) && ! particle->GetPhi(Phi,dPhi)) {
      Float_t dpTi = dpT/(pTqRC*pTqRC);
      var.peta    = var.deta / dEta;
      var.pPsi    = var.dPsi / dPhi;
      var.pPti    = var.dPti / dpTi;
      var.pPtiR   = var.dPtiR/ dpTi / (mcTrack->pT()/mcTrack->Charge());
    }	
  }
  Double_t *x = &var.ChiSqXY;
  Int_t Npart = 1;
  if (mcTrack->GePid() == 8 || mcTrack->GePid() == 9) Npart = 2;
  for (Int_t particle = 0; particle < Npart; particle++) {
    for (Int_t i = 0; i < kTotalQAll; i++) {
      if (fHistsT[kPrimary][type][particle][pm][0][i])
	fHistsT[kPrimary][type][particle][pm][0][i]->Fill(pTrack->nHitsFit(), pTrack->nHitsFit()*(100.-pTrack->qaTruth())/100., x[i]);
      if (fHistsT[kPrimary][type][particle][pm][1][i])
	fHistsT[kPrimary][type][particle][pm][1][i]->Fill(mcTrack->Pxyz().pseudoRapidity(), mcTrack->Pxyz().perp(), x[i]);
    }
  }
}
//________________________________________________________________________________
Bool_t StMuMcAnalysisMaker::Accept(const StMuTrack *gTrack) {
  if (! gTrack)            return kFALSE;
  if (! gTrack->idTruth()) return kFALSE;
  if (! gTrack->charge())  return kFALSE;
  if (  gTrack->flag() < 100 ||  gTrack->flag()%100 == 11) return kFALSE; // bad fit or short track pointing to EEMC
  if (  gTrack->flag() > 1000) return kFALSE;  // pile up track in TPC
  if (  gTrack->nHitsFit() < 15) return kFALSE;
  //  if (  gTrack->qaTruth() < 90) return kFALSE;
  return kTRUE;
}
//________________________________________________________________________________
Bool_t StMuMcAnalysisMaker::AcceptGhost(const StMuTrack *gTrack) {
  if (! gTrack)            return kFALSE;
  // if (  gTrack->idTruth()) return kFALSE;
  if (! gTrack->charge())  return kFALSE;
  if (  gTrack->flag() < 100 ||  gTrack->flag()%100 == 11) return kFALSE; // bad fit or short track pointing to EEMC
  if (  gTrack->flag() > 1000) return kFALSE;  // pile up track in TPC
  if (  gTrack->nHitsFit() < 10) return kFALSE;
  //  if (  gTrack->qaTruth() < 90) return kFALSE;
  return kTRUE;
}
//________________________________________________________________________________
Bool_t StMuMcAnalysisMaker::AcceptVX(const StMuPrimaryVertex *Vtx) {
  if (! Vtx) return kFALSE;
  if (! Vtx->idTruth())  return kFALSE;
  //  if (  Vtx->qaTruth() < 90) return kFALSE;
  return kTRUE;
}
//________________________________________________________________________________
TrackMatchType StMuMcAnalysisMaker::TrackType(const StMuMcTrack *mcTrack, multimap<Int_t,Int_t> &Mc2RcTracks, Bool_t CheckHft) {
  Int_t Id = mcTrack->Id()-1;
  pair<multimap<Int_t,Int_t>::iterator,multimap<Int_t,Int_t>::iterator> ret = Mc2RcTracks.equal_range(Id);
  Int_t count = 0;
  Int_t countHft = 0;
  for (multimap<Int_t,Int_t>::iterator it = ret.first; 
       it != ret.second; 
       ++it, ++count) 
    {
      Int_t kg = (*it).second;
      StMuTrack *gTrack = muDst->globalTracks(kg);
      StTrackTopologyMap topologyMap = gTrack->topologyMap();
      UInt_t noPxlHits = topologyMap.numberOfHits(kPxlId); // 0-3
      UInt_t noIstHits = topologyMap.numberOfHits(kIstId); // 0-2
      UInt_t noSsdHits = topologyMap.numberOfHits(kSsdId); // 0-2
      UInt_t noHftHits = noPxlHits + noIstHits + noSsdHits;
      if (noPxlHits < 2 || noIstHits < 1) continue;
      countHft++;
    }

  TrackMatchType iok = kNotDefined;
  if (! CheckHft) {
    if      (count == 0) { iok = kLostTk;}
    else {
      if (count == 1) iok = kRecoTk;
      else            iok = kCloneTk;
    }
    //  cout << " Marked as " << NameTrackMcName[iok] << endl;
  } else {
    if      (countHft == 0) { iok = kLostHftTk;}
    else {
      if (countHft == 1) iok = kRecoHftTk;
      else               iok = kCloneHftTk;
    }
  }    
  return iok;
}
//________________________________________________________________________________
void StMuMcAnalysisMaker::ForceAnimate(unsigned int times, int msecDelay) {
  unsigned int  counter = times;
  while( (!times || counter) && !gSystem->ProcessEvents()) { --counter; if (msecDelay) gSystem->Sleep(msecDelay);} 
}
//________________________________________________________________________________
void StMuMcAnalysisMaker::DrawPng(TCanvas *c) {
  TString pngName("");
  if (c) {
    c->Update(); pngName = c->GetName();
    pngName.ReplaceAll(" ","_");
    pngName.ReplaceAll("(","_");
    pngName.ReplaceAll(")","_");
    pngName.ReplaceAll("{","_");
    pngName.ReplaceAll("}","_");
    pngName.ReplaceAll("<","lt");
    pngName.ReplaceAll(">","gt");
    pngName.ReplaceAll(".","_");
    pngName.ReplaceAll("/","_");
    pngName.ReplaceAll("^","_");
    pngName.ReplaceAll("__","_");
    pngName.ReplaceAll("__","_");
    pngName += ".png"; 
    TVirtualX::Instance()->WritePixmap(c->GetCanvasID(),-1,-1,(Char_t *)pngName.Data());
    nPng++;
    cout << "Draw #\t" << nPng << "\t" << pngName << endl;
  }
}
//________________________________________________________________________________
void StMuMcAnalysisMaker::MinMax(TH1 *h, Double_t &min, Double_t &max, Double_t amax) {
  if (! h) return;
  Int_t n = h->GetNbinsX();
  Int_t imax = n;
  Bool_t entry = kFALSE;
  for (Int_t i = n; i > 0; i--) {
    Double_t y = h->GetBinContent(i);
    Double_t dy = h->GetBinError(i);
    if (dy == 0.0 && ! entry) {
      imax = i;
    } else {
      entry = kTRUE;
    }
    if (TMath::Abs(y+dy) > amax) continue;
    if (TMath::Abs(y-dy) > amax) continue;
    if (y > 0 && y < 3*dy) continue;
    if (y < 0 && y >  -2*dy) continue;
    if (y + dy > max) max = y + dy;
    if (y - dy < min) min = y - dy;
  }
  if (min < -0.5*max) min = -0.5*max;
  if (imax < n) h->GetXaxis()->SetRange(0,imax+1);
}
//________________________________________________________________________________
void StMuMcAnalysisMaker::DrawH3s(TH3F *h3s[2], Int_t animate, Double_t min, Double_t max, Int_t np) {
  if (! h3s[0] || ! h3s[1]) return;
  if (h3s[0]->GetEntries() < 100 || h3s[1]->GetEntries()) {
    cout << "Histograms " << h3s[0]->GetName() << " has " << h3s[0]->GetEntries() << "entries and " 
	 << h3s[1]->GetName() << " has " << h3s[1]->GetEntries() << " entries. To few for analysis. Skip" << endl;
    return;
  }
  TH2 *h2[2] = {0,0};
  TH1 *h1[2] = {0,0};
  TH1 *s1[2] = {0,0};
  TString path = DirPath(h3s[0]);
  for (Int_t p = 0; p < np; p++) {// zx, zy, x, y, yx
    for (Int_t pm = kPositive; pm < kTotalSigns; pm++) {
      TH3 *h3 = h3s[pm];
      if (! h3) continue;
      h3->GetDirectory()->cd();
      h2[pm] = (TH2 *) h3->Project3D(proj[p]);
      TString Title(h2[pm]->GetTitle());
      Title.ReplaceAll("(+) ","");
      Title.ReplaceAll("(-) ","");
      Title.ReplaceAll(Form("%s projection",proj[p]),"");
      //      Title.ReplaceAll("  Pr"," Primary tracks");
      //      Title.ReplaceAll("  Gl"," Global tracks");
      Title.ReplaceAll(HitName,"");
      Title.ReplaceAll(KinPionName,"");
      Title.ReplaceAll(KinName,"");
#if 0
      h2[pm]->SetTitle(Title);
      h2[pm]->GetYaxis()->SetTitle(h3->GetZaxis()->GetTitle()); 
      if ( p == 0 || p == 2 || p == 4) {h2[pm]->GetXaxis()->SetTitle(h3->GetXaxis()->GetTitle()); }
      else                             {h2[pm]->GetXaxis()->SetTitle(h3->GetYaxis()->GetTitle()); }
      if ( p == 4)                     {h2[pm]->GetYaxis()->SetTitle(h3->GetYaxis()->GetTitle()); }
#endif
      h2[pm]->SetMarkerColor(h3->GetMarkerColor());
      h2[pm]->SetLineColor(h3->GetLineColor());
      TString NameH(h3->GetName());
      cout << "Histogram: " << NameH.Data() << "\t" << Title.Data() << endl;
      if (NameH.Contains("ChiSq",TString::kIgnoreCase)) {
	if (p < 2 || p == 4) {
	  h1[pm] = (TH1 *) h2[pm]->ProfileX();
	  h1[pm]->SetTitle("");
	  h1[pm]->GetYaxis()->SetTitle(h2[pm]->GetYaxis()->GetTitle());
	  h1[pm]->SetStats(0);
	  h1[pm]->SetMarkerColor(h2[pm]->GetMarkerColor());
	  h1[pm]->SetLineColor(h2[pm]->GetLineColor());
	  h1[pm]->GetXaxis()->SetTitle(h2[pm]->GetXaxis()->GetTitle());
	} else {
	  h1[pm] = (TH1 *) h2[pm];
	  h1[pm]->GetYaxis()->SetTitle("");
	  h1[pm]->SetTitle("");
	}
	MinMax(h1[pm],min,max,500);
      } else {
	h2[pm]->FitSlicesY(0,0,-1,10,"qeg3s");
	h1[pm] = (TH1 *) TDirectory::CurrentDirectory()->Get(Form("%s_1",h2[pm]->GetName()));
	if (h1[pm]) {
	  h1[pm]->SetTitle(Form("Fitted %s",Title.Data()));
	  h1[pm]->SetStats(0);
	  h1[pm]->SetMarkerColor(h2[pm]->GetMarkerColor());
	  h1[pm]->SetLineColor(h2[pm]->GetLineColor());
	  h1[pm]->GetXaxis()->SetTitle(h2[pm]->GetXaxis()->GetTitle());
	  h1[pm]->GetYaxis()->SetTitle(h2[pm]->GetYaxis()->GetTitle());
	  MinMax(h1[pm],min,max,10);
	  s1[pm] = (TH1 *) TDirectory::CurrentDirectory()->Get(Form("%s_2",h2[pm]->GetName()));
	  if (s1[pm]) {
	    s1[pm]->SetTitle(Form("#sigma %s",Title.Data()));
	    s1[pm]->SetMarkerStyle(21);
	    s1[pm]->SetMarkerColor(h2[pm]->GetMarkerColor());
	    s1[pm]->SetLineColor(h2[pm]->GetLineColor());
	    MinMax(s1[pm],min,max,10);
	    if (min > -0.1*max) min = -0.1*max;
	  }
	}
      }
    }
    if (h1[0] && h1[1]) {
      Double_t yy = 0.3;
      if (! s1[0] || ! s1[1]) yy = 0.2; 
      //      TLegend *l = new TLegend(0.7,0.1,0.9,0.1+yy);
      TString Name = path; Name += "/"; Name += h1[0]->GetName();
      Name.ReplaceAll("/(+)","");
      Name.ReplaceAll("/(-)","");
      Name.ReplaceAll("/","_");
      TCanvas *c = new TCanvas(Name.Data(),Name.Data(),400,400);
      if (max > 0) max *= 1.1;
      else         max *= 0.9;
      if (min > 0) min *= 0.9;
      else         min *= 1.1;
      TString xName(h1[0]->GetXaxis()->GetTitle());
#if 1
      if (xName.Contains("pT",TString::kIgnoreCase) ||
	  xName.Contains("pT",TString::kIgnoreCase)) c->SetLogx(1);
      if (p < 2) {
	h1[0]->SetMinimum(min);
	h1[0]->SetMaximum(max);
      }
#endif
      for (Int_t pm = kPositive; pm < kTotalSigns; pm++) {
	if (pm == kPositive) h1[pm]->Draw(); 
	else                h1[pm]->Draw("same"); 
	if (! s1[pm]) {
	  //	  l->AddEntry(h1[pm], Form("averaged %s",TitleCharge[pm]));
	} else {
	  //	  l->AddEntry(h1[pm], Form("%s #mu",TitleCharge[pm]));
	  s1[pm]->Draw("same");
	  //	  l->AddEntry(s1[pm], Form("%s #sigma",TitleCharge[pm]));
	}
      }
      //      l->Draw();
      if (animate) ForceAnimate(0,200);
      c->Update();
      DrawPng(c);
      delete c;
    }
  }
}
//_____________________________________________________________________________
Int_t StMuMcAnalysisMaker::Finish(){
  if (! Check()) return kStOk;
  DrawQA();
  DrawEff();
  DrawdEdx();
  return kStOK;
}
//________________________________________________________________________________
TString StMuMcAnalysisMaker::DirPath(const TH1 * hist) {
  TString path;
  if (hist && hist->GetDirectory()) {
    TString FullPath(hist->GetDirectory()->GetPathStatic());
    Int_t index = FullPath.Index(":");
    path = TString(FullPath.Data()+index+2);
  }
  return path;
}
//________________________________________________________________________________
Bool_t StMuMcAnalysisMaker::Check() {
  if (! TDirectory::CurrentDirectory() ) {cout << "There is no input file. Exit" << endl; return kFALSE;}
  if (! fHistsT[kGlobal][kRecoTk][kallP][kPositive][1][0]) {cout << "There are no input histograms. Exit" << endl; return kFALSE;}
  return kTRUE;
}
//________________________________________________________________________________
void StMuMcAnalysisMaker::DrawQA(Int_t gp, Int_t pp, Int_t xx, Int_t ii) {// versus Nhits
  if (! Check()) return;
  Int_t animate = 0;
  TrackMatchType type = kRecoTk;
  Int_t k1 = kGlobal, k2 = kPrimary;
  if (gp >= 0 && gp <= kPrimary) {k1 =  k2 = gp;}
  for (Int_t k = k1; k <= k2; k++) {
    Int_t i1 = 0, i2 = kTotalQAll - 1;
    if (ii >= 0 && ii < kTotalQAll) {i1 = i2 = ii;}
    for (Int_t i = i1; i <= i2; i++) {
      TH3F *h3s[2];
      Int_t p1 = 0, p2 = 1;
      if (pp >= 0 && pp < kPartypeT) {p1 = p2 = pp;}
      Int_t x1 = 0; Int_t x2 = kVariables - 1;
      if (xx >= 0 && xx < kVariables) {x1 = x2 = xx;}
      for (Int_t particle = 0; particle <= p2; particle++) {
	for (Int_t x = x1; x <= x2; x++) {
	  h3s[0] = fHistsT[k][type][particle][kPositive][x][i];
	  h3s[1] = fHistsT[k][type][particle][kNegative][x][i];
	  if (! h3s[0] || ! h3s[1]) {cout << "No. Plots" << endl; continue;}
	  cout << h3s[0]->GetName() << "\t" << h3s[1]->GetName() << endl;
	  Double_t min =  1e9;
	  Double_t max = -1e9;
#if 0
	  if (k == kPrimary) {min = plotPrVar[i].min;  max = plotPrVar[i].max;}
	  else          {min = plotGlVar[i].min;  max = plotGlVar[i].max;}
#endif
	  if (i == 0) DrawH3s(h3s, animate, min, max, 5);
	  else        DrawH3s(h3s, animate, min, max);
	}
      }
    }
  }
}
//________________________________________________________________________________
void StMuMcAnalysisMaker::DrawEff(Double_t ymax, Double_t pTmin, Int_t animate) {// Efficiencies
  if (! Check()) return;
  struct Eff_t {
    const Char_t *Name;
    const Char_t *Title;
    TrackMatchType kDividend;
    TrackMatchType kDivider;
    Double_t min, max;
  };
  enum Effiencies {kEffTotal = 9};
  static Int_t Break = 0;
  Eff_t eff[kEffTotal] = {
    {"GeomA", "Geometrical acceptance",kMcTpcTk, kMcTk,    0.0, 100.0},
    {"EffA",  "Effeciency over all",   kRecoTk,  kMcTk,    0.0, 100.0},
    {"EffG",  "Effeciency wrt Geom",   kRecoTk,  kMcTpcTk, 0.0, 100.0},
    {"CloneA","Clone  over all",       kCloneTk, kMcTk,    0.0,  25.0},
    {"CloneG","Clone wrt Geom",        kCloneTk, kMcTpcTk, 0.0,  60.0},
    {"LostA", "Lost over all",         kLostTk,  kMcTk,    0.0,  50.0},
    {"LostG", "Lost wrt Geom",         kLostTk,  kMcTpcTk, 0.0,  70.0},
    {"GhostA","Ghost over all",        kGhostTk, kMcTk,    0.0, 110.0},
    {"GhostG","Ghost wrt Geom",        kGhostTk, kMcTpcTk, 0.0, 110.0}
  };
  const Double_t pTmins[4] = {0.11, 0.5, 1.01, 2.0};
  TCanvas *c1 = 0;
  if (Debug()) {
    c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c1");
    if (c1) c1->Clear();
    else    c1 = new TCanvas();
  }
  //                       Phi Eta pT
  const Char_t *proj3[3] = {"x","y","z"};
  
  for (Int_t gp = kGlobal; gp < kTotalT; gp++) {
    for (Int_t particle = 0; particle < kPartypeT; particle++) {
      for (Int_t i = 0; i < kEffTotal; i++) {
	TrackMatchType type1 = eff[i].kDividend;
	TrackMatchType type2 = eff[i].kDivider;
	for (Int_t p = 0; p < 3; p++) { // projections
	  TString Name(eff[i].Name);
	  TString Title(eff[i].Title);
	  TH1 *heff[8]; memset(heff, 0, sizeof(heff));
	  Double_t min = eff[i].min;
	  Double_t max = eff[i].max;
	  Int_t NS = kTotalSigns;
	  if (pTmin < 0 && p != 1) NS *= 4;
	  TString path;
	  for (Int_t l = kPositive; l < NS; l++) {
	    Int_t pm = l%kTotalSigns;
	    TH3F *Dividend = fHistsT[gp][type1][particle][pm][1][kTotalQA];
	    path = DirPath(Dividend);
	    TH3F *Divider  = fHistsT[gp][type2][particle][pm][1][kTotalQA];
	    Int_t nbinsX = Dividend->GetNbinsX();
	    Int_t nbinsY = Dividend->GetNbinsY();
	    Int_t nbinsZ = Dividend->GetNbinsZ();
	    Int_t binX1 = 1, binX2 = nbinsX;
	    Int_t binY1 = 1, binY2 = nbinsY;
	    Int_t binZ1 = 1, binZ2 = nbinsZ;
	    Dividend->GetXaxis()->SetRange(binX1,binX2);
	    Divider->GetXaxis()->SetRange(binX1,binX2);
	    Dividend->GetYaxis()->SetRange(binY1,binY2);
	    Divider->GetYaxis()->SetRange(binY1,binY2);
	    cout << "Sum " << Divider->GetName() << "\tentries = " << Divider->GetEntries() << endl;
	    cout << "Eff " << Dividend->GetName() << "\tentries = " << Dividend->GetEntries() << endl;
	    if (p != 0) { // ! eta
	      binX1 = Dividend->GetXaxis()->FindBin(-ymax);
	      binX2 = Dividend->GetXaxis()->FindBin( ymax);
	      Dividend->GetXaxis()->SetRange(binX1,binX2);
	      Divider->GetXaxis()->SetRange(binX1,binX2);
	    } 
	    if (p != 1) { // ! pT
	      if (NS == kTotalSigns) {
		binY1 = Dividend->GetYaxis()->FindBin(pTmin);
	      } else {
		binY1 = Dividend->GetYaxis()->FindBin(pTmins[l/2]);
	      }
	      Dividend->GetYaxis()->SetRange(binY1,binY2);
	      Divider->GetYaxis()->SetRange(binY1,binY2);
	    }
	    heff[l] = Dividend->Project3D(proj3[p]); 
	    if      (p == 0) heff[l]->SetXTitle(Dividend->GetXaxis()->GetTitle());
	    else if (p == 1) heff[l]->SetXTitle(Dividend->GetYaxis()->GetTitle());
	    else if (p == 2) heff[l]->SetXTitle(Dividend->GetZaxis()->GetTitle());
	    if (l == 0) heff[l]->SetName(Form("%s%s",eff[i].Name,heff[l]->GetName()));
	    else        heff[l]->SetName(Form("%s%s_%i",eff[i].Name,heff[l]->GetName(),l));
	    heff[l]->SetTitle(Form("%s for %s vs %s",eff[i].Title,TitleTrType[gp],heff[l]->GetXaxis()->GetTitle()));
	    heff[l]->SetYTitle(Form("%s (%)",eff[i]));
	    heff[l]->SetStats(0);
	    heff[l]->SetMarkerColor(l+1);
	    heff[l]->SetLineColor(l+1);
	    Title = heff[l]->GetTitle();
	    if (binX1 != binX2) Title += Form(" at |  #eta | <= %3.1f",ymax);
	    if (binY1 > 0)      Title += Form(" at pT > %3.2f",pTmins[l/2]);
	    heff[l]->SetTitle(Title);   
	    TH1 *temp =Divider->Project3D(proj3[p]); 
	    cout << heff[l]->GetName() << "\t" << heff[l]->GetEntries() << " sum " << temp->GetEntries() << endl;
	    if (c1) {
	      c1->cd(); 
	      temp->Draw();
	      heff[l]->Draw();
	      c1->Update();
	    }
	    if (temp->GetEntries() < 1) continue;
	    if (temp->GetNbinsX() != heff[l]->GetNbinsX()) {
	      cout << "No. of bins in " <<  heff[l]->GetName() << " and " << temp->GetName() << " is different. Ignore these histograms" << endl;
	      delete heff[l]; heff[l] = 0;
	      delete temp;
	      continue;
	    }
	    Double_t Val = 0;
	    Double_t Sum = 0;
	    for (Int_t bin = heff[l]->GetXaxis()->GetFirst(); bin <= heff[l]->GetXaxis()->GetLast(); bin++) {
	      Double_t val = heff[l]->GetBinContent(bin); Val += val;
	      Double_t sum = temp->GetBinContent(bin);    Sum += sum;
	      Double_t err = 0;
	      if (sum < 1.e-7 || val > sum) {val = 1.05;}
	      else { val /= sum;     err = TMath::Sqrt(val*(1.-val)/sum);}
	      heff[l]->SetBinContent(bin,100*val);
	      heff[l]->SetBinError(bin,100*err);
	    }
	    cout << heff[l]->GetName() 
		 << "[" << binX1 << "," << binX2 << "]"
		 << "[" << binX1 << "," << binX2 << "]"
		 << "[" << binZ1 << "," << binZ2 << "]"
		 << " Val = " << Val << "\tSum = " << Sum << endl;
	    MinMax(heff[l],min,max,200);
	    if (c1) {
	      c1->cd(); 
	      heff[l]->Draw();
	      c1->Update();
	    }
	  }
	  if (heff[0] && heff[1]) {
	    Name = path; Name += "/";
	    Name += heff[0]->GetName();
	    Name.ReplaceAll("/(+)","");
	    Name.ReplaceAll("/(-)","");
	    Name.ReplaceAll("/","_");
	    TCanvas *c = new TCanvas(Name.Data(),Name.Data(),400,400);
	    if (p == 1) c->SetLogx(1);
	    TLegend *l = 0;
	    if (NS > kTotalSigns) l = new TLegend(0.1,0.4,0.4,0.6);
	    for (Int_t pm = kPositive; pm < NS; pm++) {
	      if (pm == kPositive) {heff[pm]->SetMinimum(min); heff[pm]->SetMaximum(max); heff[pm]->Draw();}
	      else                 heff[pm]->Draw("same");
	      if (l) l->AddEntry(heff[pm],Form("%s with pT/|q| > %3.1f",TitleCharge[pm%2],pTmins[pm/2]));
	      if (l && pm == kPositive) l->Draw();
	      c->Update();
	    }
	    if (animate) ForceAnimate(0,200);
	    DrawPng(c);
	    if (Break) return;
	    delete c;
	  }
	}
      }
    }
  }
}
//________________________________________________________________________________
void StMuMcAnalysisMaker::DrawdEdx() {
  if (Check()) return;
  TH3F *ZI70piPzB = (TH3F *) TDirectory::CurrentDirectory()->Get("ZI70piPzB");
  if (! ZI70piPzB) return;
  TH3F *ZI70pizB = new TH3F(*ZI70piPzB);
  ZI70pizB->SetName("ZI70pizB");
  TH3F *ZI70piNzB = (TH3F *) TDirectory::CurrentDirectory()->Get("ZI70piNzB"); 
  if (ZI70piNzB) {
    ZI70pizB->Add(ZI70piNzB);
  }
  TString Name(ZI70pizB->GetName());
  TCanvas *c = new TCanvas(Name.Data(),Name.Data(),400,400);
  ZI70pizB->Project3D("zx")->Draw("colz");
  TH2 *ZI70pizB_zx = (TH2 *) TDirectory::CurrentDirectory()->Get("ZI70pizB_zx");
  if (! ZI70pizB_zx) return;
  ZI70pizB_zx->FitSlicesY(0,0,-1,10,"qeg3s");
  TH1 *ZI70pizB_zx_2 = (TH1 *) TDirectory::CurrentDirectory()->Get("ZI70pizB_zx_2");
  if (! ZI70pizB_zx_2) return;
  ZI70pizB_zx_2->SetAxisRange(20,220);
  ZI70pizB_zx_2->SetMaximum(0.16);
  ZI70pizB_zx_2->SetMinimum(0.04);
  ZI70pizB_zx_2->SetStats(0);
  ZI70pizB_zx_2->SetTitle("dE/dx resolution versus track length in active TPC");
  ZI70pizB_zx_2->SetXTitle("track length in active TPC (cm)");
  TF1 *pl3 = TPolynomial::MakePol(3);
  ZI70pizB_zx_2->Fit(pl3,"er","",20,220);
  TLegend *l = new TLegend(0.15,0.8,0.85,0.9);
  l->AddEntry(ZI70pizB_zx_2,Form("#sigma(@76cm) = %5.2f%\% : #sigma(@128cm) = %5.2f%\%",
				 100*pl3->Eval(76),100*pl3->Eval(128)));
  l->Draw();
  c->Update();
  DrawPng(c);
  delete c;
}
// 
// $Id: StMuMcAnalysisMaker.cxx,v 1.18 2007/10/27 17:42:59 fine Exp $
// $Log: StMuMcAnalysisMaker.cxx,v $
