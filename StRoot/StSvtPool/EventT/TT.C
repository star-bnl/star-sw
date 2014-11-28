// #ifndef __CINT__
#define T_cxx 1
#include <assert.h>
#include "TH1.h"
#include "TH2.h"
#include "TStyle.h"
#include "TCanvas.h"
#include "TF1.h"
#include "TT.h"

//ClassImp(TreeClass);
// #endif

TreeClass *TreeClass::Create() { 
  printf("Hello Create\n");
//  return ::new TreeClass((TTree*)0);
  return ::new TreeClass();
}
void TT() { printf("Hello Van\n"); TreeClass::Create(); printf("bye  Van\n");}

//empty constructor *************************************************

  TreeClass::TreeClass() : TBase() {}
 
//constructor (*file) ***********************************************

   TreeClass::TreeClass(TFile *f) : TBase() {
     fOutFileName = "Out.root";
    if (f) {
      cout << f->GetName() << " file has been found" << endl;
      fOutFileName = f->GetName();
      fOutFileName.ReplaceAll("/","_");
      fOutFileName.ReplaceAll(".root","");
      fOutFileName += "Out.root";
      TTree *tree = (TTree*) f->Get("T");
      if (tree) { MyInit(tree); }
      else cout << "no TTree found" << endl;
    }
  }

//constructor ( *tree) *********************************************************

  TreeClass::TreeClass(TTree *tree) : TBase(tree)  {
    // if parameter tree is not specified (or zero): 
    // try to make the Tree using available files
    if (tree == 0) {
      TCollection *files = gROOT->GetListOfFiles();
      if (! files) { cout << "no root files" << endl;  return; } 
      TIter next(files);
      TFile *f = 0;
//      TChain * chain = new TChain("T");
      Int_t Files = 0;
      ULong64_t Events = 0;
      ULong64_t TotalE = 0;
      while ( (f = (TFile *) next()) ) {   
	tree = (TTree*) f->Get("T");
	if (tree) {
	  cout << f->GetName() << " file hase been found" << endl;
	  fOutFileName = f->GetName();
	  fOutFileName.ReplaceAll("/","_");
	  fOutFileName.ReplaceAll(".root","");
	  fOutFileName += "Out.root";
      Files++;
      Events = tree->GetEntries();
      TotalE += Events;
      cout << "#\t" << Files << "\t" << f->GetName() << "\t" << Events << endl;
//      chain->Add(f->GetName());
	  break;
	}
      }
    }
    if (tree) {MyInit(tree);}
  }
  
//*********************************************************************
void TreeClass::Loop(Int_t Nevents) {  

struct Geometry_t {
  Int_t Barrel;
  Int_t Layer;
  Int_t NoLadders;
  Int_t NoWafers;
};
const Int_t NSvtLayers = 6;
// Barrel, Layer  ladder wafer
const Geometry_t SvtSsdConfig[] = 
  {    {1,     1,      8,   4}, // even
       {1,     2,      8,   4}, // odd
       {2,     3,     12,   6}, // event
       {2,     4,     12,   6}, // odd
       {3,     5,     16,   7}, // even
       {3,     6,     16,   7}, // odd
       {4,     7,     20,  16}  // Ssd
  };
//const Int_t BL[4] = {8, 12, 16, 20}; // ladders in barrel

  struct PlotPar_t {
    Char_t *Name;
    Char_t *Title;
    Int_t    nx;
    Int_t    ny;
    Double_t xmin; 
    Double_t xmax; 
    Double_t ymin;
    Double_t ymax;  
  };
  const  PlotPar_t plotTB = // plots for time bins and anodes
    { "timeB","time for 80 anodes", 256, 3, 0.,128., 0.,3. };

  const  PlotPar_t plotUP = // plots for uP
    { "uP","track u", 320, 3, -5.,5., 0.,3. };

  const  PlotPar_t plotDu = // plots for u-uP
    { "Du","Du before cut", 250, 3, -2.,2., 0.,3. };

  const  PlotPar_t plotDuv = // plots for du & dv
    { "Du","Du cuts", 200, 3, -1.,1., 0.,3. };

  TFile *fOut = new TFile(fOutFileName,"recreate");

    Xdcor X1;
    X1.InitSptr();
  
  TString Name;
  TString Title;
  TString uName;
  TString uTitle;
  const Int_t NB =  3;
  const Int_t NL = 16;
  const Int_t NW =  7;
  const Int_t NH =  2;
  const Int_t NA =  3;
  //              B   L   W   H   A
  TH1F *LocPlots[NB][NL][NW][NH][NA];
  TH1F *  uPlots[NB][NL];
  TH1F *  uPlBLW[NB][NL][NW];
  TH1F *    hpT = new TH1F(   "Pt",   "pt", 200, -2., 2.);
  TH1F *    hpM = new TH1F( "Ptot", "ptot", 200,  0., 5.);
  TH1F * LocAll = new TH1F(  "All",  "all", plotTB.nx, plotTB.xmin, plotTB.xmax);
  TH1F * uPAll  = new TH1F("UPall","uPall", plotUP.nx, plotUP.xmin, plotUP.xmax);
  TH1F * uAll   = new TH1F("Uall", "ua",    plotUP.nx, plotUP.xmin, plotUP.xmax);
  TH1F * duB[NB][2];
  TH1F * dvB[NB];
  TH1F * uCuts[NB][NL];
  TH1F * xCuts[NB][NL];
  TH1F * uCut   = new TH1F("Ucut","uc", plotDu.nx, plotDu.xmin, plotDu.xmax);
  TH1F * vCut   = new TH1F("Vcut","vc", 200, -3., 3.);
  TH2F * dMin   = new TH2F("DMin","vumin",100,-0.75,0.75,100,-0.75,0.75); 
  TH1F * vMin   = new TH1F("VMin","vmin", plotDuv.nx, plotDuv.xmin, plotDuv.xmax);
  TH1F * uMin   = new TH1F("UMin","umin", plotDuv.nx, plotDuv.xmin, plotDuv.xmax);
  TH1F * uMinC  = new TH1F("UMinC","umC", plotDuv.nx, plotDuv.xmin, plotDuv.xmax);
   memset(LocPlots,0,NB*NL*NW*NH*sizeof(TH1F *));
   memset(  uPlots,0,NB*NL*sizeof(TH1F *));
   for (int B = 0; B < 3; B++) {// over svt Barrels
     uName  = Form("UBarrel%i", B+1);
     uTitle = Form("du for B%i", B+1);
     duB[B][0] =  new TH1F(uName, uTitle, plotDuv.nx, plotDuv.xmin, plotDuv.xmax );
     uName  = Form("VBarrel%i", B+1);
     uTitle = Form("dv for B%i", B+1);
     dvB[B] =  new TH1F(uName, uTitle, plotDuv.nx, plotDuv.xmin, plotDuv.xmax );
     uName  = Form("UBarrel%iVcut", B+1);
     uTitle = Form("du for B%i after Vcut", B+1);
     duB[B][1] =  new TH1F(uName, uTitle, plotDuv.nx, plotDuv.xmin, plotDuv.xmax );
   }
   for (Int_t L = 0; L < NSvtLayers; L++) {// over Layers
     Int_t barrel    = SvtSsdConfig[L].Barrel;
     Int_t layer     = SvtSsdConfig[L].Layer;
     Int_t NoLadders = SvtSsdConfig[L].NoLadders;
     Int_t NoWafers  = SvtSsdConfig[L].NoWafers;
     for (Int_t ladder = 1; ladder <= NoLadders; ladder++) {
       if (barrel <= 3 && (ladder-1)%2 != layer%2) continue;
         uName  = plotUP.Name;
	 uName += Form("L%02iB%i", ladder, barrel);
	 uTitle = Form("uP for layer %i B %i L %i", layer, barrel, ladder);
	 uPlots[barrel-1][ladder-1] = 
	 new TH1F(uName, uTitle, plotUP.nx, plotUP.xmin, plotUP.xmax );

         uName  = Form("%sL%02iB%i", plotDu.Name, ladder, barrel);
	 uTitle = Form("u-uP for B %i L %i", barrel, ladder);
         uCuts[barrel-1][ladder-1]= 
	 new TH1F(uName, uTitle, plotDu.nx, plotDu.xmin, plotDu.xmax );
         uName  = Form("%sxL%02iB%i", plotDu.Name, ladder, barrel);
	 uTitle = Form("u-uP corr B %i L %i", barrel, ladder);
         xCuts[barrel-1][ladder-1]= 
	 new TH1F(uName, uTitle, plotDu.nx, plotDu.xmin, plotDu.xmax );
       for (Int_t wafer = 1; wafer <= NoWafers; wafer++) {// wafer == 0 for whole ladder
         uName  = plotUP.Name;
	 uName += Form("L%02iB%iW%i", ladder, barrel, wafer);
	 uTitle = Form("uP for layer %i B %i L %i W %i", layer, barrel, ladder, wafer);
         uPlBLW[barrel-1][ladder-1][wafer-1] = 
	 new TH1F(uName, uTitle, plotUP.nx, plotUP.xmin, plotUP.xmax );
	 for (Int_t hybrid = 1; hybrid <= 2; hybrid++) {
	   for (Int_t anode =1; anode <=3; anode++) {
	     Name = plotTB.Name;
	     Name += Form("L%02iB%iW%02iH%iA%i", ladder, barrel, wafer, hybrid, anode);
	     Title = Form("%s for layer %i B %i L %i W %i H %i G %i",
			  plotTB.Title, layer, barrel, ladder, wafer, hybrid, anode);
	     LocPlots[barrel-1][ladder-1][wafer-1][hybrid-1][anode-1] = 
	       new TH1F(Name, Title, plotTB.nx, plotTB.xmin, plotTB.xmax );
           }	   
	 }
       }
     }
   }
   Long64_t nentries = fChain->GetEntriesFast();
   if (Nevents > 0 && nentries > Nevents) nentries = Nevents;
   Long64_t nbytes = 0, nb = 0;
   Int_t TreeNo = -1;
   TString currentFile("");

   for (Long64_t jentry=0; jentry<nentries;jentry++) {
     Long64_t ientry = LoadTree(jentry);
     if (ientry < 0) break;
     nb = fChain->GetEntry(jentry);   nbytes += nb;
     if (! jentry%1000 || TreeNo != fChain->GetTreeNumber()) {
       cout << "Read event \t" << jentry 
	    << " so far, switch to file " << fChain->GetCurrentFile()->GetName() 
	    << endl;
       cout << " current TreeNo: " << TreeNo
            <<  " new TreeNo: " << fChain->GetTreeNumber() << endl;
       TreeNo = fChain->GetTreeNumber();
     }
//     if (VertexZCut > 0 && TMath::Abs(fVertex[2]) > VertexZCut) continue;
     UInt_t Ntrack = fNPTracks; 
//     int k_used[100000] = {0};    
     for (UInt_t trk = 0; trk < Ntrack; trk++) {
      int hitFlag[3][16][7] = { { { 0 } } };
      Int_t Npoints = fTracks_fNpoint[trk];
       if (minNoFitPoints > 0 && Npoints%100 < minNoFitPoints) continue;
       if (UseSsd && Npoints < 1000) continue; 
       if (UseSvt && Npoints <  100) continue; 
       Int_t Nsp = fTracks_fNsp[trk];
       double dvmin = 1000.;
       double dumin = 1000.;
       int kmin;
       for (Int_t hit = 0; hit < Nsp; hit++) {
	 Int_t k = fTracks_fIdHitT[trk][hit] - 1;
//	 assert(k>=0);
	 if ( k < 0) cout <<" k <0:"<<k<<" hit="<<hit<<" Nsp="<<Nsp<< endl;
	 if ( k < 0) continue;
	 Int_t layer  = fHits_layer[k];
	 if (layer <= NSvtLayers ){
	   Int_t barrel = fHits_barrel[k];
	   Int_t ladder = fHits_ladder[k];
	   Int_t wafer  = fHits_wafer[k];
	   Int_t hybrid = fHits_hybrid[k];

	   Double32_t u = fHits_u[k];       
	   Double32_t v = fHits_v[k];
	   Double32_t uP = fHits_uP[k];       
	   Double32_t vP = fHits_vP[k];
	   Double32_t du = u - uP;
	   Double32_t dv = v - vP;
	   Double32_t anode = fHits_anode[k];
	   Double32_t timeb = fHits_timeb[k];
	   
           hpT->Fill(fHits_pT[k]);
           hpM->Fill(fHits_pMom[k]);
	   if (TMath::Abs(fHits_pT[k]) < 0.2) continue;
	   if ( TMath::Abs(dv) < TMath::Abs(dvmin) ) {dvmin = dv; dumin= du; kmin = k;}
	   
           if ( hitFlag[barrel-1][ladder-1][wafer-1] == 0) {
	        hitFlag[barrel-1][ladder-1][wafer-1] = 1;
	     uPAll->Fill( uP );
             uPlots[barrel-1][ladder-1]->Fill( uP );
	     uPlBLW[barrel-1][ladder-1][wafer-1] ->Fill( uP );
	   }
	   
           duB[barrel-1][0]->Fill(du);
           dvB[barrel-1]->Fill(dv);
	   vCut->Fill(dv);
	   if (TMath::Abs(dv) > rCut ) continue;
	   uCut->Fill(du);
           uCuts[barrel-1][ladder-1]->Fill(du);
	   
	   int hid = X1.GetHid (barrel, ladder, wafer, hybrid);
	   double dux = uP - X1.Sptr[hid]->Coord( timeb );
	   xCuts[barrel-1][ladder-1]->Fill(dux);
	   
           duB[barrel-1][1]->Fill(du);
	   if (TMath::Abs(du) > 2.*rCut) continue;
	   
	   Int_t   group = ((Int_t)anode)/80;
	   if (group >= 3) group = 2;
	   if (group <  0) group = 0;
	   LocPlots[barrel-1][ladder-1][wafer-1][hybrid-1][group]->Fill( timeb );
           LocAll->Fill( timeb );
           uAll->Fill( u );
	   X1.Sptr[hid]->addStat( timeb, u, uP);
	   
	 } //layer if
       } //hits loop
       if (TMath::Abs(dvmin) < 1000.) {
         dMin->Fill(dvmin,dumin);
	 vMin->Fill(dvmin);
	 uMin->Fill(dumin);
	 if (TMath::Abs(dvmin) < rCut ) uMinC->Fill(dumin);
       }	 
     } //track loop
   } //jentry loop (event loop)
        X1.solve();
	fOut->Write();
} //end of Loop()


