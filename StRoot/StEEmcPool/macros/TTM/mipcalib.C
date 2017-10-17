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

const float etaBinTable[] = {
  2.0000,1.9008,1.8065,1.7168,1.6317,1.5507,1.4738,
  1.4007,1.3312,1.2651,1.2023,1.1427,1.0860,-1.0 };

const float SamplingFraction    = 0.05;       // 5%
const float EnergyLossMip       = 0.0018*9.6; // 0.020 GeV at eta=1.3
const short AdcRange            = 4076;       // 4096 - 20(pedestals)
const float EnergyTransverseMax = 60.0;       // 60 GeV

const int   MaxSec  = 12;
const int   MaxSSec =  5;
const int   MaxEta  = 12;

const int   FirstSec= 0;
const int   LastSec =11;
const int   MinEta  = 4;

//const float MaxCTB  = 1000;
float MaxCTB = 1000.0;
const float MinPt   = 0.500;
const float MaxDEta = 0.035; //0.020;
const float MaxDPhi = 0.028; //0.026;


// PHIHW     0.0873/2.0
// ETAHW 12: 0.0992/2.0
//       10: 0.0897/2.0 
//        8: 0.0810/2.0
//        6: 0.0731/2.0
//        4: 0.0661/2.0
//        1: 0.0567/2.0



void mystat(TH1 *h, Double_t x1, Double_t x2,
            Double_t& xmean, Double_t& emean, Double_t& dint, 
	    Double_t& xmax , Double_t& xlo  , Double_t& xhi);
int  miptower( TH1   **hadc, 
	       const int   MinEta,
	       const int   MaxEta,
	       const char *dname= "eta",
               const char *func = "landau",
	       const float xmin = 10.0,
	       const float xmax = 80.0,
	       FILE  *outfd     = stderr);


// ===========================================================================
// the main routine
// ===========================================================================
// WHAT A MESS!!!
void 
mipcalib(
	 const int   secNum  = 0,           // counting from 1!! (0==all)
	 const char *fName   = "",          // if fName=="", use root global file list
	 const char *histName= "calib.root",// histogram file name 
	 const char *fitFunc = "landau"    ,// function to fit (gaus or landau)
	 const float xMin    = 10.0,        // lower limit of the fit 
	 const float xMax    = 80.0,        // upper limit of the fit 
	 const int   trig    = 0,           // select trigger, 0==all 
	 const bool  doSort  = false        // a flag that decides whether we sort or plot the data
	)
{
  gROOT->Reset();
  gErrorIgnoreLevel=1000;

  const int   MaxHist    = MaxEta*MaxSec*MaxSSec;
  const int   MaxTracks  = 1024;
  const int   MaxTrigger =   32;
    
  // track info
  int   numtracks;
  int   sector[MaxTracks];
  int   subsec[MaxTracks];
  int   etabin[MaxTracks];
  float adcval[MaxTracks];
  int   ntrack[MaxTracks];
  //
  float pt    [MaxTracks];
  float ptot  [MaxTracks];
  //float length[MaxTracks];
  //float dedx  [MaxTracks];
  //int   nhits [MaxTracks];
  float  etatrk[MaxTracks];
  //
  float detasmd [MaxTracks];
  float dphismd [MaxTracks];
  float detapres[MaxTracks];
  float dphipres[MaxTracks];
  float detapost[MaxTracks];
  float dphipost[MaxTracks];

  // trigger Info
  int   numtrig;
  int   trigid[MaxTrigger];
  int   daqbits;
  int   ctbsum;

  // histogram list
  TH1*   hadc[MaxHist];   for(int i=0; i<MaxHist; i++) hadc[i]=NULL;
  TH1*   heta[MaxEta];    for(int i=0; i<MaxEta ; i++) heta[i]=NULL;

  int      nentries= 0;
  long     ntracks = 0;
  TChain  *chain   = NULL;

  if(doSort) { 

    chain = new TChain("track");
    if( fName!="" ) { 
      chain->Add(fName);
    } else {
      TFile          *f   = NULL;
      TSeqCollection *seq = gROOT->GetListOfFiles();
      TIter next(seq);
      while ( ( f = (TFile *)next() ) != NULL ) chain->Add(f->GetName());
      fName = "attached files";
    }
    cout << "sorting " << fName   << " (trigger=" << trig << ") to " << histName << endl;
    cout << "MaxCTB: " << MaxCTB  << endl;
    nentries =  (int)chain->GetEntries();

    chain->SetBranchAddress("ntracks" ,&numtracks);
    
    chain->SetBranchAddress("sec"     , sector  );
    chain->SetBranchAddress("ssec"    , subsec  );
    chain->SetBranchAddress("eta"     , etabin  );
    chain->SetBranchAddress("adc"     , adcval  );
    chain->SetBranchAddress("track"   , ntrack  );
    
    chain->SetBranchAddress("pt"      , pt      );
    chain->SetBranchAddress("ptot"    , ptot    );
    //chain->SetBranchAddress("nhits"   , nhits   );
    //chain->SetBranchAddress("length"  , length  );
    //chain->SetBranchAddress("dedx"    , dedx    );
    //chain->SetBranchAddress("etatrk"    , etatrk  );
    
    chain->SetBranchAddress("detasmd" , detasmd );
    chain->SetBranchAddress("dphismd" , dphismd );
  
    chain->SetBranchAddress("detapres", detapres );
    chain->SetBranchAddress("dphipres", dphipres );

    chain->SetBranchAddress("detapost", detapost );
    chain->SetBranchAddress("dphipost", dphipost );
  
    chain->SetBranchAddress("ntrig"   ,&numtrig );
    chain->SetBranchAddress("trigid"  , trigid  );
    chain->SetBranchAddress("daqbits" ,&daqbits );
    chain->SetBranchAddress("ctbsum"  ,&ctbsum  );
  } else {
    cout << histName << " fitting " << fitFunc  << endl;
  }

  int sec1=(secNum>0) ? secNum-1 : FirstSec;
  int sec2=(secNum>0) ? secNum-1 : LastSec; 

  TFile    *histfile = new TFile(histName,doSort ? "RECREATE" : "READ");
  TString   outName  = TString(histName).ReplaceAll(".root","");

  // histograms
  // individual towers
  char dir[256];
  char name[256],titl[256];
  for(int sec=sec1;sec<=sec2 ;sec++) {
    for( int ssec=0; ssec<MaxSSec; ssec++)   {
      sprintf(dir,"%02dT%1c",sec+1,ssec+'A');
      if(doSort) histfile->mkdir(dir);
      histfile->cd(dir);
      for( int eta=0; eta<MaxEta; eta++ ) {
	sprintf(titl,"ADC(%02dT%1c%02d)",sec+1,ssec+'A',eta+1);
	sprintf(name,"%02dT%1c%02d"     ,sec+1,ssec+'A',eta+1);
	int hidx=(sec*MaxSSec+ssec)*MaxEta+eta;
	if(hidx<0 || MaxHist<=hidx) continue;
	if(doSort) hadc[hidx] = new TH1F(name,titl,40,0.0,120.0);
	else 	   hadc[hidx] = (TH1F *)gDirectory->Get(name);

      }
    }
  }
  // summed over eta
  {
    sprintf(dir,"eta");
    if(doSort) histfile->mkdir(dir);
    histfile->cd(dir);
    for( int eta=0; eta<MaxEta; eta++ ) {
      sprintf(titl,"ADC(ETA%02d)",eta+1);
      sprintf(name,"ETA%02d"     ,eta+1);
      if(doSort) heta[eta] = new TH1F(name,titl,60,0.0,120.0);
      else       heta[eta] = (TH1F *)gDirectory->Get(name);
    }
  }

  if(doSort) {     // sort data
    int ie=0;
    for(ie=0;ie<nentries;ie++) {
      ntracks += numtracks;
      chain->GetEntry(ie);
      if(ie%100==0)fprintf(stdout ,"Entry %d/%d (%.2f%%)\r",ie,nentries,(ie*100.0)/nentries);
      if(ctbsum>MaxCTB) continue;
      if(trig>0) {
	int  it=0;
	int *trigword=trigid;
	while(it<numtrig && *trigword>0 && *trigword!=trig ) { it++; trigword++; };
	if(*trigword!=trig) continue;
      }

      for(int t=0;t<numtracks; t++) {
	// select the tracks 
	if(ntrack[t]         > 1       ) continue; // reject multiple tracks
	if(pt[t]             < MinPt   ) continue;
	if(fabs(detapres[t]) > MaxDEta ) continue;
	if(fabs(dphipres[t]) > MaxDPhi ) continue;
	if(fabs(detasmd[t])  > MaxDEta ) continue;
	if(fabs(dphismd[t])  > MaxDPhi ) continue;
	if(fabs(detapost[t]) > MaxDEta ) continue;
	if(fabs(dphipost[t]) > MaxDPhi ) continue;
	
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
    TCanvas  *c1      = new TCanvas(outName,"MIP CALIB",800,0,1024,1024);    
    c1->Print(outName+".ps[");
    bool go_on = true;
    for(int sec=sec1;sec<=sec2 && go_on;sec++) {
      char outfn[256];
      sprintf(outfn,"sector%02d.cal",sec+1);
      FILE *outfd = fopen(outfn,"w");
      for(int ssec=0;ssec<MaxSSec && go_on;ssec++) {
	sprintf(dir,"%02dT%1c",sec+1,ssec+'A');
	c1->Clear();
	nFitOK += miptower(hadc+(sec*MaxSSec+ssec)*MaxEta,MinEta,MaxEta,dir,fitFunc,xMin,xMax,outfd);
	c1->Update();
	c1->Print(outName+".ps");
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
      fclose(outfd);
    }
    cout << "TOTAL FIT OK " << nFitOK << endl;
    c1->Clear();
    (void) miptower(heta,MinEta,MaxEta,"ETA",fitFunc,xMin,xMax);
    c1->Update();
    c1->Print(outName+".ps");
    c1->Print(outName+".ps]");
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
	  const char *dir,
	  const char *func , 
	  const float xMin ,
	  const float xMax ,
	  FILE  *outfd)
{
  const Double_t MinCounts = 30.0; // King's constants or the cuts
  const Double_t MinPeakV  =  8.0;
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
  TPaveLabel *tlab = new TPaveLabel(0.005,0.955,0.990,0.985,gltit+dir);
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

  if(gROOT->IsBatch()) cout << "TOWERS " << dir << " " << flush;

  //TFile      *f    = gDirectory->GetFile();
  //f->mkdir(dir);
  //f->cd(dir);

  
  for(int eta=0,pad=0; pad<MaxPad && eta<MaxEta; eta++) {
    Double_t xint;
    Double_t xmean , xmnerr;
    Double_t xpeak , xpkerr;
    Double_t xgain , xgnerr;
    Double_t par[20];
    Double_t chi2;
    Int_t    ndf;
    char     name[256];
    sprintf(name,"%s%02d",dir,eta+1);

    if(hadc[eta]==NULL) continue;

    double xlim1, xlim2;
    mystat(hadc[eta],xMin,xMax,xmean,xmnerr,xint,xpeak,xlim1,xlim2);
    
    // set initial parameter values
    par[0] = hadc[eta]->GetMaximum();
    par[1] = xpeak;
    par[2] = xmnerr;
    //

    TF1 *fit1 = new TF1("fit1" ,func ,0.0,120.0); //xmin+0.0,xmax);
    fit1->SetParameters(par); 
    fit1->SetLineWidth(2);    
    fit1->SetLineColor(kRed); 

    // FIXME hack
    if(strncmp(func,"gaus",4)==0) {
      //cerr << xpeak << " (" << xlim1 << "," << xlim2 << ")" << endl;
      hadc[eta]->Fit("fit1","Q0","",xlim1,xlim2);
    } else {
      hadc[eta]->Fit("fit1","Q0","",xMin,xMax);
    }




    xpeak   = fit1->GetParameter(1);
    xpkerr  = fit1->GetParError(1);
    chi2    = fit1->GetChisquare();
    ndf     = fit1->GetNDF();
    chi2    = (ndf>=1) ? chi2/ndf : -1.0;
    xpkerr *= (chi2>0.0) ? sqrt(chi2) : 1.0 ;

    // calculate gain and gain error
    Double_t xeta    = 0.5*(etaBinTable[eta-1]+etaBinTable[eta]);
    Double_t xscale  = SamplingFraction/EnergyLossMip*TMath::TanH(xeta);
    xgain   = xpeak  * xscale;  
    xgnerr  = xpkerr * xscale;
    
    // print all the stuff
    fprintf(outfd,"%6s  %8.3f %8.3f 0.0   # %8.3f  %6.0f",
	    name,xgain,xgnerr,chi2,xint); 

    // now get the error message
    char    *errmsg = NULL;
    if     (xint<MinCounts                    ) errmsg = "not enough statistics";
    else if(xpeak<MinPeakV ||  MaxPeakV<xpeak ) errmsg = "bad fit value";  
    else if( MaxPeakE < xpkerr                ) errmsg = "fit error too large";
    else if(chi2<MinChi2 || MaxChi2<chi2      ) errmsg = "chi2 too large";

    if(errmsg==NULL) fprintf(outfd," fit %s ok \n",func);
    else             fprintf(outfd," *** %s ***\n",errmsg);
    if(eta<MinEta) continue;

    // plot 
    gpad->cd(++pad);
    Double_t hmax = hadc[eta]->GetMaximum();
    hadc[eta]->SetMaximum( (hmax>10.0) ? 1.2*hmax : 12.0 );
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
  fprintf(outfd,"\n");
  if(gROOT->IsBatch()) cout << " (" << nFitOK << ") DONE " << endl;
  return nFitOK;
}



// ===========================================================================
// get some statistics for a H1F 
// returns mean, its error and the integral between x1 and x2
// ===========================================================================
void 
mystat(TH1 *h,
       Double_t  x1   , Double_t  x2,
       Double_t& xmean, Double_t& emean, Double_t& dint,
       Double_t& xmax , Double_t& xlo  , Double_t& xhi)
{
  TAxis* xax  = h->GetXaxis();
  Int_t  i1 = xax->FindBin(x1);
  Int_t  i2 = xax->FindBin(x2);
  
  Float_t sw=0.0,sw2=0.0,swx=0.0,swx2=0.0,wmax=0.0;
  Int_t   n=0;
  Int_t   imax=0;
  xmean=emean=xmax=0.0;
  for(Int_t i=i1;i<=i2;i++) {
    Double_t x = h->GetBinCenter(i);
    Double_t w = h->GetBinContent(i);
    if(w>wmax) { xmax=x; wmax=w; imax=i; }
    sw  += w;
    sw2 += w*w;
    swx += w*x;
    swx2+= w*x*x;
    n++;
  }
  
  xlo = h->GetBinCenter(i1);
  xhi = h->GetBinCenter(i2);
 
  //cerr << "MAX " << imax << " (" << i1 << "," << i2 << ") " << wmax << endl;
  //cerr << "LO" << endl;
  wmax *= 0.4;
  for(Int_t i=imax;i>=i1;i--) {
    Double_t w = h->GetBinContent(i);
    //cerr << i << "," << w << endl;
    if(w<wmax) { 
      xlo = h->GetBinCenter(i);
      break;
    }
  }
  //cerr << "HI" << endl;
  for(Int_t i=imax;i<=i2;i++) {
    Double_t w = h->GetBinContent(i);
    //cerr << i << "," << w << endl;
    if(w<wmax) { 
      xhi = h->GetBinCenter(i);
      break;
    }
  }
  
  dint  = sw;
  xmean = (sw>0.0)    ?  swx/sw                  : 0.0 ;
  emean = (sw>0.0)    ? (swx2/sw - xmean*xmean ) : 0.0 ;
  emean = (emean>0.0) ?  sqrt(emean/n) : 0.0;
}


#ifndef __CINT__
int   sector  =  0;
char *outname = "mipcalib.hist.root";
char *fitfunc = "landau";

int   trig    =     0;
float xmin    =   9.0;
float xmax    = 100.0;

void
usage(char *name)
{
  cerr << "usage: "  << name << "   [options]   rootfile(s)   \n";
  cerr << "       -s <sector>         - (0 == all)       \n";
  cerr << "       -t <trigger_id>     - (0 == any)       \n";
  cerr << "       -o <histogram file> - .hist.root file  \n";
  cerr << "       -L                  - fit Landau/Gauss distributions\n";
  cerr << "       -G                  - fit Landau/Gauss distributions\n";
  cerr << "       -x <xmin>           - fit lower bound\n";
  cerr << "       -X <xmax>           - fit upper bound\n";
  cerr << "       -b                  - run in batch mode without graphics \n";
  cerr << "       -h                  - this help\n";
  cerr << endl;
}


int
main(int argc, char **argv)
{
  extern char *optarg;
  extern int   optind;
  char         optchar;
  cerr << "#===============================================\n";
  cerr << "#         ******* WARNING *******               \n";
  cerr << "# A LOUSY PROGRAM THAT GREW OUT OF A ROOT MACRO \n";
  cerr << "#  needs to be rewritten                        \n";
  cerr << "#===============================================\n" << endl;


  while((optchar = getopt(argc, argv, "s:t:o:LGx:X:c:bqnlh")) != EOF) {
    switch(optchar) {
    case 's': sector  = atoi(optarg);  break;
    case 'o': outname = optarg ;       break;
    case 't': trig    = atoi(optarg);  break;
    case 'L': fitfunc = "landau";      break;
    case 'G': fitfunc = "gaus"  ;      break;
    case 'x': xmin    = atof(optarg);  break;
    case 'X': xmax    = atof(optarg);  break;
    case 'c': MaxCTB  = atof(optarg);  break;
    case 'b': gROOT->SetBatch(kTRUE);  // fall down
    case 'q':                          // pass
    case 'n':                          // pass
    case 'l': break; // targv[targc++]=optarg;   break; 
    case 'h': usage(argv[0]); exit(0); break;
    case '?': 
    default:  usage(argv[0]); exit(-1);break;
    }
  }


  // attach root files from the in the list
  for(int k=optind;k<argc; k++) new TFile(argv[k],"");  
  argc=optind;


  TApplication *myApp;
  if(gROOT->IsBatch()) 
    myApp = new TApplication("mipcalib",&argc,argv);
  else
    myApp = new TRint       ("mipcalib",&argc,argv);

  mipcalib(sector,"",outname,fitfunc,xmin,xmax,trig,true );   // sort 
  mipcalib(sector,"",outname,fitfunc,xmin,xmax,trig,false);   // fit and plot 

  if(!gROOT->IsBatch() ) myApp->Run(); // "Exit ROOT" will quit the application

  return 0;
}
#endif
