// File:    mipcalib.C
// Author:  Piotr A. Zolnierczuk, IUCF
// Date:    June 15-21, 2003
// Update:  Jan  7, 2004
// Purpose: generate EEMC Tower PMT calibration
// Usage:  
//      root './mipcalib.C(sector,rootfile)' 
// 	sector     is a sector number 5,6,7 or 8 (0 stands for all four)
//      rootfile  'ntuple' tree root file (wildcards allowed)
// Output: 
//      stderr             - fitting log file
//      sectorXX.cal       - eemc dbase input files(s)
//	mip$$T#EE.eps      - an eps plot for each tower
//      mipcalib.ps        - a single ps of the above
//      mipcalib.hist.root - a root histo file
//      
// Notes(s): 
// 1. cuts are nicely hidden :) in miptower()
// 2. best to run it under bash where one can redirect 
// stdout and stderr separately e.g.
// root './mipcalib.C(0,"root/R*.root")' 2> mipcalib.log
// 

#ifndef __CINT__
#include <iostream>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cctype>

#include <getopt.h>
#include <signal.h>

#include <TROOT.h>
#include <TSystem.h>
#include <TError.h>
#include <TStyle.h>
#include <TApplication.h>
#include <TRint.h>
#include <TList.h>
#include <TCanvas.h>
#include <TBox.h>
#include <TLine.h>
#include <TPave.h>
#include <TLegend.h>
#include <TPaveLabel.h>
#include <TPolyLine.h>
#include <TGraph.h>

#include <TString.h>
#include <TH1.h>
#include <TH2.h>
#include <TF1.h>

#include <TTree.h>
#include <TFile.h>
#include <TChain.h>
#include <TBranch.h>

using namespace std;

#endif




void mystat(TH1 *h, Double_t x1, Double_t x2,
            Double_t& xmean, Double_t& emean, Double_t& dint);
int  miptower( TH1   **hadc, 
	       const int   MinEta,
	       const int   MaxEta,
	       const char *dname= "eta",
               const char *func = "landau",
	       const float xmin = 10.0,
	       const float xmax = 80.0);

// ===========================================================================
// the main routine
// ===========================================================================
// WHAT A MESS!!!
void 
mipcalib(
	 const int   secnum  = 0,  // counting from 1!! (0==all)
	 const char *fname   = "*.root",
	 const char *outname = "mipcalib", 
	 const char *fitfunc = "landau"   ,
	 const float xmin    = 10.0,
	 const float xmax    = 80.0,
	 const bool  dosort  = false,
	 const int   trig    = 0   // select trigger, 0==all 
	)
{
  gROOT->Reset();
  gErrorIgnoreLevel=1000;


  const int   FirstSec= 5-1;
  const int   LastSec = 8-1;
  const int   MaxSec  = 12;
  const int   MaxSSec =  5;
  const int   MinEta  = 5-1;
  const int   MaxEta  = 12;
  const int   MaxHist = MaxEta*MaxSec*MaxSSec;

  const float MinPt   = 0.500;
  const float MaxDEta = 0.020;
  const float MaxDPhi = 0.026;
    

  // track info
  const int     MaxTracks = 1024;
  int   numtracks;
  int   sector[MaxTracks];
  int   subsec[MaxTracks];
  int   etabin[MaxTracks];
  float adcval[MaxTracks];
  //
  int   nhits [MaxTracks];
  float pt    [MaxTracks];
  float ptot  [MaxTracks];
  float length[MaxTracks];
  float dedx  [MaxTracks];
  //
  float detasmd[MaxTracks];
  float dphismd[MaxTracks];

  // trigger Info
  const  int   MaxTrigger  = 32;
  int   numtrig;
  int   trigid[MaxTrigger];
  int   daqbits;
  //


  TH1*   hadc[MaxHist];
  TH1*   heta[MaxEta];
  for(int i=0; i<MaxHist; i++) hadc[i]=NULL;
  for(int i=0; i<MaxEta ; i++) heta[i]=NULL;

  int      nentries=0;
  long     ntracks =0;
  TChain  *chain   =NULL;



  if(dosort) { 
    cout << fname   << " sorting (trigger=" << trig << ") to " << outname << endl;
    chain = new TChain("track");
    chain->Add(fname);
    nentries =  (int)chain->GetEntries();

    chain->SetBranchAddress("ntracks" ,&numtracks);
    
    chain->SetBranchAddress("sec"     , sector  );
    chain->SetBranchAddress("ssec"    , subsec  );
    chain->SetBranchAddress("eta"     , etabin  );
    chain->SetBranchAddress("adc"     , adcval  );
    
    chain->SetBranchAddress("pt"      , pt      );
    chain->SetBranchAddress("ptot"    , ptot    );
    chain->SetBranchAddress("nhits"   , nhits   );
    chain->SetBranchAddress("length"  , length  );
    chain->SetBranchAddress("dedx"    , dedx    );
    
    chain->SetBranchAddress("detasmd" , detasmd );
    chain->SetBranchAddress("dphismd" , dphismd );
    
    chain->SetBranchAddress("ntrig"   ,&numtrig );
    chain->SetBranchAddress("trigid"  , trigid  );
    chain->SetBranchAddress("daqbits" ,&daqbits );
    
  } else {
    cout << outname << " fitting " << fitfunc  << endl;
  }


  int sec1=(secnum>0) ? secnum-1 : FirstSec;
  int sec2=(secnum>0) ? secnum-1 : LastSec; 

  TFile    *histfile = new TFile(TString(outname)+".hist.root",dosort ? "RECREATE" : "");

  // histograms
  // individual towers
  for(int sec=sec1;sec<=sec2 ;sec++) {
    for( int ssec=0; ssec<MaxSSec; ssec++)   {
      char dname[256];
      sprintf(dname,"%02dT%1c",sec+1,ssec+'A');
      if(dosort) histfile->mkdir(dname);
      histfile->cd(dname);
      for( int eta=0; eta<MaxEta; eta++ ) {
	char name[256],titl[256];
	sprintf(titl,"ADC(%02dT%1c%02d)",sec+1,ssec+'A',eta+1);
	int hidx=(sec*MaxSSec+ssec)*MaxEta+eta;
	if(hidx<0 || MaxHist<=hidx) continue;
	if(dosort) { 
	  sprintf(name,"%02dT%1c%02d"           ,sec+1,ssec+'A',eta+1);
	  hadc[hidx] = new TH1F(name,titl,60,0.0,120.0);
	} else {
	  sprintf(name,"%s/%02dT%1c%02d;1",dname,sec+1,ssec+'A',eta+1);
	  hadc[hidx] = (TH1F *)histfile->Get(name);
	}
	//cerr << name << " " << hidx << " " << sec << " " << ssec << " " << eta << endl;    
      }
    }
  }
  // summed over eta
  {
    char dname[256];
    sprintf(dname,"eta");
    if(dosort) histfile->mkdir(dname);
    histfile->cd(dname);
    for( int eta=0; eta<MaxEta; eta++ ) {
      char name[256],titl[256];
      sprintf(titl,"ADC(ETA%02d)",eta+1);
      if(dosort) { 
	sprintf(name,"ETA%02d",eta+1);
	heta[eta] = new TH1F(name,titl,60,0.0,120.0);
      } else {
	sprintf(name,"%s/ETA%02d;1",dname,eta+1);
	heta[eta] = (TH1F *)histfile->Get(name);
      }
    }
  }

  if(dosort) {     // sort data
    int ie=0;
    for(ie=0;ie<nentries;ie++) {
      ntracks += numtracks;
      chain->GetEntry(ie);
      if(ie%100==0)fprintf(stdout ,"Entry %d/%d (%.2f%%)\r",ie,nentries,(ie*100.0)/nentries);
      if(trig>0) {
	int  it=0;
	int *trigword=trigid;
	while(it<numtrig && *trigword>0 && *trigword!=trig ) { it++; trigword++; };
	if(*trigword!=trig) continue;
      }

      for(int t=0;t<numtracks; t++) {
	// select the tracks 
	if(pt[t]            < MinPt   ) continue;
	if(fabs(detasmd[t]) > MaxDEta ) continue;
	if(fabs(dphismd[t]) > MaxDPhi ) continue;

	
	int sec  = sector[t];
	int ssec = subsec[t];
	int eta  = etabin[t];
	int hidx=(sec*MaxSSec+ssec)*MaxEta+eta;
	if(hidx<0 || MaxHist<=hidx) continue;
	if(hadc[hidx]!=NULL) hadc[hidx]->Fill(adcval[t]);
	if(heta[eta ]!=NULL) heta[eta ]->Fill(adcval[t]);
     }
    }
    fprintf(stdout ,"Entry %d/%d (%.2f%%)\n",ie,nentries,(ie*100.0)/nentries);
    fprintf(stdout ,"Totals tracks %ld\n",ntracks);
    histfile->Write();
  } else {        // plot
    int nFitOK=0;
    TCanvas  *c1      = new TCanvas(outname,"MIP CALIB",800,0,1024,1024);    
    c1->Print(TString(outname)+".ps[");
    bool go_on = true;
    for(int sec=sec1;sec<=sec2 && go_on;sec++) {
      for(int ssec=0;ssec<MaxSSec && go_on;ssec++) {
	char dname[256];
	sprintf(dname,"%02dT%1c",sec+1,ssec+'A');
	c1->Clear();
	nFitOK += miptower(hadc+(sec*MaxSSec+ssec)*MaxEta,MinEta,MaxEta,dname,fitfunc,xmin,xmax);
	c1->Update();
	c1->Print(TString(outname)+".ps");
	//c1->Print(epsfn,"eps");
	
	// a lousy loop break for interactive session
	if(!gROOT->IsBatch()) { 
	  while(true) {
	    char   cmd[256];
	    char   c;
	    int    k=0;
	    cmd[k]=0x00;
	    cout << "fit> "; 
	    do {
	      c=getchar();
	      if(c==0x0A) { cmd[k++]=0x00; break; }
	      cmd[k++]=c;
	    } while(k<255);
	    if(cmd[0]==0x0A)                         { break;
	    } else if(strncasecmp(cmd,".cont" ,2)==0 ||
	              strncasecmp(cmd, "cont" ,1)==0)  { break;
	    } else if(strncasecmp(cmd,".quit" ,2)==0 ||
	              strncasecmp(cmd, "quit" ,1)==0)  { go_on=false; break; 
	    } else if(strncasecmp(cmd,".help" ,2)==0 || 
		      strncasecmp(cmd, "help" ,1)==0) {
	      cerr << "commands (may be abbreviated):\n";
	      cerr << " .c  cont  -  to continue with fitting\n" ;
	      cerr << " .q  quit  -  to quit fitting loop \n" ;
	      cerr << " .h  help  -  this help \n" << endl;
	    } else { 
	      cerr << "unkown command:" << cmd << " type help for command list" << endl;
	    }
	  }
	}
      }
    }
    cout << "TOTAL FIT OK " << nFitOK << endl;
    c1->Clear();
    (void) miptower(heta,MinEta,MaxEta,"ETA",fitfunc,xmin,xmax);
    c1->Update();
    c1->Print(TString(outname)+".ps");
    c1->Print(TString(outname)+".ps]");
  }
}

// ===========================================================================
// fit/plot/get calib for one tower (sec,ssec,eta=4..11)
// WARNING: here we count from 0 
// ===========================================================================
int
miptower( TH1 **hadc, 
	  const int   MinEta,
	  const int   MaxEta,
	  const char *dname,
	  const char *func , 
	  const float xmin ,
	  const float xmax )
{
  const Double_t MinCounts = 50.0; // King's constants or the cuts
  const Double_t MinPeakV  = 10.0;
  const Double_t MaxPeakV  = 30.0;
  const Double_t MaxPeakE  = 6.0 ;
  const Double_t MinChi2   = 0.0 ;
  const Double_t MaxChi2   =10.0 ; 
  //
  const int   MaxPad =  MaxEta-MinEta;
  
  int nFitOK=0;

  // root mumbo-jumbo
  TString gltit("EEMC TOWERS ");
  TString pz   ("Piotr A. Zolnierczuk (IU) ");
  TPaveLabel *tlab = new TPaveLabel(0.005,0.955,0.990,0.985,gltit+dname);
  TDatime    *now  = new TDatime;
  TPaveLabel *date = new TPaveLabel(0.555,0.005,0.995,0.045,pz+now->AsString());
  TPad       *gpad = new TPad("Graphs","Graphs",0.005,0.05,0.995,0.95);

  gStyle->SetOptStat(101111);
  gStyle->SetOptFit(1);
  tlab->Draw();
  date->Draw();
  gpad->Draw();
  gpad->Divide(2,MaxPad/2);
  gpad->cd(1);
  gpad->Update();

  if(gROOT->IsBatch()) cout << "TOWERS " << dname << " " << flush;

  //TFile      *f    = gDirectory->GetFile();
  //f->mkdir(dname);
  //f->cd(dname);

  
  for(int eta=0,pad=0; pad<MaxPad && eta<MaxEta; eta++) {
    Double_t xint;
    Double_t xmean , xmnerr;
    Double_t xpeak , xpkerr;
    Double_t par[20];
    Double_t chi2;
    Int_t    ndf;
    char     name[256];
    sprintf(name,"%s%02d",dname,eta+1);

    if(hadc[eta]==NULL) continue;
    
    // set initial parameter values
    par[0] = par[2] = par[3] = par[4]= 0.0;
    par[1] = 20.0;
    //

    TF1 *fit1 = new TF1("fit1" ,func ,0.0,120.0); //xmin+0.0,xmax);
    fit1->SetParameters(par); 
    fit1->SetLineWidth(2);    
    fit1->SetLineColor(kRed); 
    Float_t xMin = (xmin<0.0) ? (2.0*int((eta+0.5)/2)+1) : xmin; 
    Float_t xMax = (xmax<0.0) ? 100.0                    : xmax;
    hadc[eta]->Fit("fit1","Q0","",xMin,xMax);

    mystat(hadc[eta],xMin,xMax,xmean,xmnerr,xint);

    xpeak   = fit1->GetParameter(1);
    xpkerr  = fit1->GetParError(1);
    chi2    = fit1->GetChisquare();
    ndf     = fit1->GetNDF();
    chi2    = (ndf>=1) ? chi2/ndf : -1.0;
    xpkerr *= (chi2>0.0) ? sqrt(chi2) : 1.0 ;
    
    // print all the stuff
    fprintf(stderr,"%6s  %8.3g %8.3g %8.3g  %6.0f",
	    name,xpeak,xpkerr,chi2,xint); 

    // now get the error message
    char    *errmsg = NULL;
    if     (xint<MinCounts                    ) errmsg = "not enough statistics";
    else if(xpeak<MinPeakV ||  MaxPeakV<xpeak ) errmsg = "bad fit value";  
    else if( MaxPeakE < xpkerr                ) errmsg = "fit error too large";
    else if(chi2<MinChi2 || MaxChi2<chi2      ) errmsg = "chi2 too large";

    if(errmsg==NULL) fprintf(stderr," fit %s ok \n",func);
    else             fprintf(stderr," *** %s ***\n",errmsg);
    if(eta<MinEta) continue;

    // plot 
    gpad->cd(++pad);
    Double_t hmax = hadc[eta]->GetMaximum();
    hadc[eta]->SetMaximum( (hmax>40.0) ? 1.2*hmax : 50.0 );
    hadc[eta]->GetXaxis()->SetTitle("ADC");
    hadc[eta]->Draw("E");
    fit1->Draw("SAME");

    if(errmsg!=NULL) {
      hmax   = hadc[eta]->GetMaximum();
      TPaveLabel* badfitlabel = new TPaveLabel(70,0.25*hmax,118,0.40*hmax,errmsg);
      badfitlabel->SetFillColor(kRed   );
      badfitlabel->SetTextColor(kYellow   );
      badfitlabel->Draw();
    } else {
      nFitOK++;
    }
    gpad->Update();
  }
  fprintf(stderr,"\n");
  if(gROOT->IsBatch()) cout << " (" << nFitOK << ") DONE " << endl;
  return nFitOK;
}



// ===========================================================================
// get some statistics for a H1F 
// returns mean, its error and the integral between x1 and x2
// ===========================================================================
void 
mystat(TH1 *h, 
      Double_t x1, Double_t x2, 
      Double_t& xmean, Double_t& emean, Double_t& dint)
{
  TAxis* xax  = h->GetXaxis();
  Int_t  i1 = xax->FindBin(x1);
  Int_t  i2 = xax->FindBin(x2);
  
  Float_t sw=0.0,sw2=0.0,swx=0.0,swx2=0.0;
  Int_t   n=0;
  xmean=emean=0.0;
  for(Int_t i=i1;i<=i2;i++) {
    Double_t x = h->GetBinCenter(i);
    Double_t w = h->GetBinContent(i);
    sw  += w;
    sw2 += w*w;
    swx += w*x;
    swx2+= w*x*x;
    n++;
  }
  dint  = sw;
  xmean = (sw>0.0)    ?  swx/sw                  : 0.0 ;
  emean = (sw>0.0)    ? (swx2/sw - xmean*xmean ) : 0.0 ;
  emean = (emean>0.0) ?  sqrt(emean/n) : 0.0;
}







#ifndef __CINT__

int   sector  =  0;
char *fname   = "ntuple.root";
char *outname = "mipcalib";
char *fitfunc = "landau";

int   trig    = 0;
float xmin    =   9.0;
float xmax    = 100.0;



void
usage(char *name)
{
  cerr << "usage: "  << name << "   [options]                         \n";
  cerr << "       -s <sector>         - (0 == all)                    \n";
  cerr << "       -t <trigger_id>     - (0 == any)                    \n";
  cerr << "       -f <inpfile(s)>     - input TTree file (ntuple.root)\n";
  cerr << "       -o <histogram file> - .hist.root file   (mipcalib)\n";
  cerr << "       -L                  - fit Landau/Gauss distributions\n";
  cerr << "       -G                  - fit Landau/Gauss distributions\n";
  cerr << "       -x <xmin>           - fit lower bound\n";
  cerr << "       -X <xmax>           - fit upper bound\n";
  cerr << "       -n                  - do not sort (just use the existing histogram file)\n";
  cerr << "       -b                  - run in batch mode without graphics \n";
  cerr << "       -h                  - this help\n";
  cerr << endl;
}


int
main(int argc, char **argv)
{
  extern char *optarg;
  char         optchar;
  bool         dosort=true;

  while((optchar = getopt(argc, argv, "s:t:f:o:LGx:X:bnh")) != EOF) {
    switch(optchar) {
    case 's': sector  = atoi(optarg);  break;
    case 'f': fname   = optarg ;       break;
    case 'o': outname = optarg ;       break;
    case 't': trig    = atoi(optarg);  break;
    case 'L': fitfunc = "landau";      break;
    case 'G': fitfunc = "gaus"  ;      break;
    case 'x': xmin    = atof(optarg);  break;
    case 'X': xmax    = atof(optarg);  break;
    case 'n': dosort  = false;         break;
    case 'b': gROOT->SetBatch(kTRUE);  break; 
    case 'h': usage(argv[0]); exit(0); break;

    default:  usage(argv[0]); exit(-1);break;
    }
  }

  TApplication *myApp;
  if(gROOT->IsBatch()) 
    myApp = new TApplication("mipcalib",&argc,argv);
  else
    myApp = new TRint("mipcalib",&argc,argv);

  // first sort data
  if(dosort) mipcalib(sector,fname,outname,fitfunc,xmin,xmax,true  ,trig);
  
  // then plot them
  mipcalib(sector,fname,outname,fitfunc,xmin,xmax,false ,trig);

  // Here we don't return from the eventloop. "Exit ROOT" will quit the app.
  if(!gROOT->IsBatch() ) { 
    cout << "close root window to exit" << endl;
    myApp->Run();
  }

  return 0;
}

#endif
