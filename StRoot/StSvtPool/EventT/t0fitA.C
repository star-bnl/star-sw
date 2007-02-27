#include <stdlib.h>
#include <stdio.h>
#include <iostream>

#include "TF1.h"
#include "TH1F.h"
#include "TFile.h"
#include "TMath.h"
#include "TCanvas.h"
#include "TTree.h"
#include "TSystem.h"
#include "tfit_CuCu.C"

// par[]
// 0 - amplitude of step function
// 1 - edge of the step
// 2 - 1/sigma of step function
// 3 - amplitude of Gaus
// 4 - center of Gaus
// 5 - 1/sigma of Gaus
// 6 - constant

Double_t  tmin ( Double_t *v, Double_t *par )  
{
   Double_t x = v[0];
   Double_t argS = (x - par[1])*par[2];
   Double_t argG = (x - par[1])*par[2];
   Double_t fitval = par[0]*TMath::Freq( argS )+
                     par[3]*TMath::Exp(-0.5*argG*argG); // +  
//		     par[4];
   return fitval;
}
const int Npar = 4; 

// par[]
// 0 - amplitude of step function
// 1 - edge of the step
// 2 - 1/sigma of step function
// 3 - constant when needed

Double_t  tmax ( Double_t *v, Double_t *par )  
{
   Double_t x = v[0];
   Double_t argS = (par[1] - x )*par[2];
   Double_t fitval = par[0]*TMath::Freq( argS );
//		     par[3];
   return fitval;
}
const int NpaM = 3; 

class FitTree : public TObject {
public:
    char * fName;
    char * hName;
    int    barrel;
    int    ladder;
    int    wafer;
    int    hybrid;
    double Entries;
    int    fitflag;
    char * comment;
    int    fitID;
    int    Nparr;
    double chi2; 
    double prr[Npar]; 
    double err[Npar];
    
    int    ftMflag;
    char * commentM;
    int    ftMID;
    int    NparM;
    double chi2M; 
    double prM[NpaM]; 
    double erM[NpaM];
    
    FitTree(): Nparr(Npar),
               NparM(NpaM){}
      
    ClassDef(FitTree,1)
};
 
  ClassImp(FitTree)
   
  struct PlotPar_t {
    char * Name;
    char * Title;
    int    nx;
    int    ny;
    double xmin; 
    double xmax; 
    double ymin;
    double ymax;  
  };
  const  PlotPar_t plotTB = // plots for time bins and anodes
    { "timeB","time for 80 anodes", 256, 3, 0.,128., 0.,3. };


int t0fitA ( void) {

// files *******************************************************
//  char * fName = "afs_rhic.bnl.gov_star_users_fisyak_work_SvtSsdAlignment_Pass103_CuCuNoField_TpcOnlyPlotsTBNFP25rCut0.5cm.root";
  char * fName = "afs_rhic.bnl.gov_star_users_fisyak_work_SvtSsdAlignment_Pass102_CuCu62PlotsTBNFP25rCut0.5cm.root";
  TFile * f  = new TFile( fName );

  char * outName = "tfit_CuCu62_L3";  // default file: fit2_par.root
  char   ftName[128] = "";
  char   foName[128] = "";
  strcat(ftName, outName); strcat(ftName, ".root");
  strcat(foName, outName); strcat(foName, ".txt");
  cout << ftName << endl;
  cout << foName << endl;
  TFile fout(ftName,"RECREATE","fit parameters tree");
  FILE * pFile = fopen (foName,"wt");
   if (pFile == 0) return -1;

// create a tree ********************************************************

  FitTree FitOut;
  FitTree * pFitOut = &FitOut;;
  TTree * tree = new TTree("Fit0T","t0 fit output tree");
  tree->Branch("fit_res",&pFitOut,8000,99);

// canvas ***************************************************************

  TCanvas *c1 = new TCanvas("fit", "t0fit", 300,10,700,650);

//loops over barrels, ladders, wafers, hybrids

const int NL[4] = {8, 12, 16, 20}; // ladders in the barrel
const int NW[4] = {4,  6,  7, 16}; // wafers on the ladder

for (int barrel = 1; barrel <= 3;            barrel++){
for (int ladder = 1; ladder <= NL[barrel-1]; ladder++){
for (int wafer  = 1; wafer  <= NW[barrel-1]; wafer++) {
for (int hybrid = 1; hybrid <= 2;            hybrid++){

// get histogram ***********************************************
   
  char nome[64];
  sprintf( nome,"B%iL%02iW%iH%i",barrel,ladder,wafer,hybrid);
   
  TH1F *hs = new TH1F(Form("hybrid %i_%i_%i",ladder,wafer,hybrid),
                      nome,  
//	              Form("B%i L%02i W%02i H%i",barrel,ladder,wafer,hybrid),
		      plotTB.nx, plotTB.xmin, plotTB.xmax );
  hs->SetDirectory(0);  // to disconnect this histogram from any files

  for (Int_t anode =1; anode <=3; anode++) {
    TString Name("timeB");
    Name += Form("L%02iB%iW%02iH%iA%i", ladder, barrel, wafer, hybrid, anode);
//    TH1F *hist = (TH1F *) gDirectory->Get(Name);
    TH1F *hist = (TH1F *) f->Get(Name);
    if (! hist) continue;
//	  hist->SetLineColor(anode);
    hs->Add(hist,1);
  }	   
  c1->Clear();
  c1->cd(1);
  hs->Draw("");

// hs is filled up ------------------------------------------------------

  FitOut.fName   = fName;
  FitOut.hName   = nome;
  FitOut.barrel  = barrel;
  FitOut.ladder  = ladder;
  FitOut.wafer   = wafer;
  FitOut.hybrid  = hybrid;
  FitOut.Entries = hs->GetEntries(); // stats[0];
  cout <<endl;
  cout << " ********************************************************** " << endl; 
  cout << " Entries = " << FitOut.Entries << endl;
  if ( FitOut.Entries < 100 ) cout << " histogram is empty: " << nome << endl;
  if ( FitOut.Entries < 100 ) continue;
  cout << " B " << FitOut.barrel 
       << " L " << FitOut.ladder
       << " W " << FitOut.wafer
       << " H " << FitOut.hybrid
       << "   nome: " << FitOut.hName << ""
       << endl;
   
// set initial values for parameters

  double par0 = FitOut.Entries/220.;
  cout << endl;
  int idx = barrel*10000 + ladder*100 + wafer*10 + hybrid;
  double t0_init = 10.;
  double tmax_init = 120.;
  double d_length = 29928.; //in microns
  int init = InitEdge ( idx, t0_init, tmax_init, d_length); 
  cout << " Init: " << init 
       << " t0 = " << t0_init
       << " tmax = " << tmax_init
       << " par0 = " << par0 
       << endl; 
  cout << endl;
   
// t0 fit **************************************** 

  TF1 * fit = new TF1("fit0",tmin, 0., 16., Npar);
  fit->SetParameter(0,par0);
  fit->SetParLimits(0,0.,2000.);
  fit->SetParameter(1,t0_init);  
  fit->SetParameter(2,12.3);
  fit->SetParLimits(2,0.,1000.);
  fit->SetParameter(3,par0*2.);
  fit->SetParLimits(3,0.,2000.);
//  fit->SetParameter(4,1.0);  
//  fit->SetParameter(4,2.0); 
// constant background
//  fit->SetParameter(4,8.); 
//  fit->SetParLimits(6,0.,100.);


  FitOut.fitflag = hs->Fit("fit0","LIBr");
  cout << " fit status = " << FitOut.fitflag << endl;

  if ( FitOut.fitflag != 0 ) cout << "Fit failed: " << FitOut.fitflag << endl;
  
  FitOut.chi2 = fit->GetChisquare();
  cout << " Chi**2 = " << FitOut.chi2  << endl;
  cout << " Npar = "   << FitOut.Nparr << endl;
  for (int jpar = 0; jpar < Npar; jpar++){
    FitOut.prr[jpar]=fit->GetParameter(jpar);
    FitOut.err[jpar]=fit->GetParError(jpar);
    cout << " par " << jpar 
         << " = "   << FitOut.prr[jpar]
         <<  " +- " << FitOut.err[jpar]
	 << endl;
    if ( FitOut.prr[jpar] < 0.) cout << " WARNING! " << FitOut.prr[jpar] << endl;
  }

  gPad->Update();
  gPad->Draw();
  c1->Update();
  c1->Draw();

  char in;
  cin >> in;
  if ( in == 's') { cout << " operator intervention " << endl; return -3;}
  if ( in == 'g') {FitOut.comment  = "good "; FitOut.fitID  =  1;}
  else            {FitOut.comment  = "refit"; FitOut.fitID  = -1;}
  cout << " " << FitOut.hName
       << " comment : " << FitOut.comment
       << " fit ID : "  << FitOut.fitID
       << endl;
  cout << endl;

   
// t_max fit **************************************** 

  double Rmin = 116.;
  if (init == 0) Rmin = tmax_init - 3.;
  TF1 * ftM = new TF1("fitM",tmax, Rmin, 128., NpaM);
  ftM->SetParameter(0, par0);
  ftM->SetParLimits(0, 0.,2000.);
  ftM->SetParameter(1, tmax_init);  
  ftM->SetParameter(2, 12.2);
// constant background
//  ftM->SetParameter(3, 8.); 
//  ftM->SetParLimits(6,0.,100.);


  FitOut.ftMflag = hs->Fit("fitM","LIBr");
  cout << " fit status = " << FitOut.ftMflag << endl;

  if ( FitOut.ftMflag != 0 ) cout << "Fit failed: " << FitOut.ftMflag << endl;
  
  FitOut.chi2M = ftM->GetChisquare();
  cout << " Chi**2 = " << FitOut.chi2M  << endl;
  cout << " Npar = "   << FitOut.NparM << endl;
  for (int jpar = 0; jpar < NpaM; jpar++){
    FitOut.prM[jpar]=ftM->GetParameter(jpar);
    FitOut.erM[jpar]=ftM->GetParError(jpar);
    cout << " par " << jpar 
         << " = "   << FitOut.prM[jpar]
         <<  " +- " << FitOut.erM[jpar]
	 << endl;
    if ( FitOut.prM[jpar] < 0.) cout << " WARNING! " << FitOut.prM[jpar] << endl;
  }

  gPad->Update();
  gPad->Draw();
  c1->Update();
  c1->Draw();

//  char in;
  cin >> in;
  if ( in == 'q') { cout << " quit " << endl; return -3;}
  if ( in == 's') { cout << " stopped " << endl; fout.Write(); fout.Close(); fclose (pFile); return -4;}
  if ( in == 'g') {FitOut.commentM  = "good "; FitOut.ftMID  =  1;}
  else            {FitOut.commentM  = "refit"; FitOut.ftMID  = -1;}
  cout << " " << FitOut.hName
       << " comment M: " << FitOut.commentM
       << " fit ID M: "  << FitOut.ftMID
       << endl;
//  cout << endl;

  tree->Fill();

// fill the txt file ***********************************************************

const double timeb = 40.; // in ns
const double d_veldef = 6.75;    //"default" velosity constant, micron/ns 
      double d_velosity;

  int   type = 0;  
//  int   rdx = 0;   // row index
  int   nrows = 216*2; // total no. of real rows in the table; For Db interface (where nrows = 50)
  int   npr =0;
//  double tmin;  
//  double tmax;  
  double v[10] ={0}; 
  char row_name[32];
   
  if ( FitOut.fitID>0 && 
       FitOut.ftMID>0 && 
      (FitOut.prM[1] - FitOut.prr[1])> 0. )
      d_velosity = d_length/( FitOut.prM[1] - FitOut.prr[1])/timeb;
  else d_velosity = d_veldef;
  v[0] = d_velosity;
      
  sprintf(row_name,"B%iL%02iW%iH%i", barrel, ladder, wafer, hybrid);
          
  fprintf (pFile," { %i, %4i, %i, %i, %i, %2i, %i, %i, ",
	           type, idx, nrows, npr, barrel, ladder, wafer, hybrid);
  fprintf (pFile,"%7.3f, %6.3f, %7.3f, %6.3f, %7.1f, ", 
	         FitOut.prr[1], FitOut.err[1], FitOut.prM[1], FitOut.erM[1], d_length );
  fprintf (pFile,"{%6.4f, %3.1f, %3.1f, %3.1f, %3.1f, %3.1f, %3.1f, %3.1f, %3.1f, %3.1f }}, ",
                    v[0],v[1],v[2],v[3],v[4],v[5],v[6],v[7],v[8],v[9] );
  fprintf (pFile," // %2i %2i %2i %2i  %s %s %s \n", 
                  FitOut.fitflag, FitOut.fitID, FitOut.ftMflag, FitOut.ftMID, 
		  FitOut.comment, FitOut.commentM, row_name );
		  
//******************************************************************************
  delete hs;
  
} //hybrid loop
} //wafer loop
} //ladder loop
} //barrel loop

  fout.Write();
  fout.Close();
  
  fclose (pFile);

  return 0;
   
} //end macro
