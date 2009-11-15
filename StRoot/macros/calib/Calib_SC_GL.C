/////////////////////////////////////////////////////////////////
//
// Calib_SC_GL.C
//   Macro for calibrating the SpaceCharge and GridLeak
//   distortion corrections.
//
//   Author: G. Van Buren, BNL
//           Jan 6, 2006
//
// Parameters:
//   1) Input file with info on data to be calibrated
//   2) Any additional cuts to help calibrate (i.e. "run!=6082047")
//   3) A specific scaler identifier (if you know which scaler you want to use)
//   4) Debug mode
//
// Details:
// The input file should contain lines with the following info:
//   - dataset file specification
//   - scaler detector used in that dataset (see dets strings below)
//   - SpaceCharge rate used in that dataset
//   - GridLeak multiplier used in that dataset
//
// There should be at least 3 datasets with different GridLeaks!
// Example 3 dataset file which used bbce+bbcw would look like this:
//    histsSetA/* 4 1.7e-8 9.0
//    histsSetB/* 4 1.7e-8 12.0
//    histsSetC/* 4 1.7e-8 15.0
// If this file is named input.dat, it could be analyzed with:
//    root Calib_SC_GL.C("input.dat")
// If you knew you wanted to use only bbce+bbcw:
//    root Calib_SC_GL.C("input.dat","",4)
//
/////////////////////////////////////////////////////////////////

#ifndef __CINT__
#include "TCanvas.h"
#include "TChain.h"
#include "TGraphErrors.h"
#include "TF1.h"
#include "TProfile.h"
#include "TCut.h"
#include <Stiostream.h>
#endif


void Calib_SC_GL(const char* input, const char* cuts=0, int scaler=-1, int debug=0);
int  FitLine(TTree* SC, const char* v0, const char* v1, double window,
     double& p0, double& p1, double& e0, double& e1, int debug=0);


const int nfip=128;
const int nsca=5;
const int npos=nfip*nsca;
TCanvas* c1 = 0;
TChain* SCi[nfip];
TGraphErrors* gr_lk=0;
TGraphErrors* gr_sc=0;
TGraphErrors* gr_so=0;
TGraphErrors* gr_s2=0;
TCut cut;


//////////////////////////////////////////////////



void Calib_SC_GL(const char* input, const char* cuts, int scaler, int debug) {


TString dets[nsca];
dets[0] = "bbce+bbcw-4*(bbcyb+bbcbb)"; // Just for fun.
dets[1] = "bbcx"     ;
dets[2] = "zdcx"     ;
dets[3] = "zdce+zdcw";
dets[4] = "bbce+bbcw";

int minsca = 0;
int maxsca = nsca;
if (scaler>=0) { minsca = scaler; maxsca = scaler+1; }

int i,j,indx,jmin,nfi,result;
TString fis[nfip];// file names
int det[nfip];    // detector used (see dets above)
double usc[nfip]; // SC used
double ugl[nfip]; // GL used

char fname[128];
Bool_t allZeros = kTRUE;

ifstream dats(input);
nfi = nfip;
for (i=0;i<nfi;i++) {
  dats >> fname;
  if (dats.eof()) { nfi=i; continue; }
  fis[i] = fname;
  dats >> det[i] >> usc[i] >> ugl[i];
  if (usc[i]) allZeros = kFALSE;
}
printf("Found %d dataset specifications.\n",nfi);

cut = ((cuts) ? cuts : "");

if (c1==0) c1 = new TCanvas("c1","Calib SC and GL");

double rms[npos];
double lum[npos];
double off[npos];
double glk[npos];
double lumE[npos];
double offE[npos];
double glkE[npos];

double rmin=1;
jmin=-1;
for (i=0;i<nfi;i++) { // run
  SCi[i] = new TChain("SC");
  SCi[i]->Add(fis[i].Data());
  SCi[i]->SetMarkerStyle(7);
  SCi[i]->SetMarkerColor(2);
  for (j=minsca;j<maxsca;j++) { // scaler
    indx = i + j*nfi;
    double pr,pg,pl,po,pgE,plE,poE,temp,temp2;
    const char* dt = dets[j].Data();
  
    // Determine GridLeak
    result = FitLine(SCi[i],"gapf",dt,0.005,temp,pg,temp2,pgE,debug);
    if (result<0) return;

    // Determine SpaceCharge:
    result = FitLine(SCi[i],"sc",dt,0.0003,po,pl,poE,plE,debug);
    if (result<0) return;
  
    TString plot = Form("sc-(%5.3g+(%5.3g)*(%s))",po,pl,dt);
    SCi[i]->Draw(plot.Data(),cut);
    pr = SCi[i]->GetHistogram()->GetRMS();
    if (debug>0) printf("RMS = %5.3g for %s\n",pr,dt);

    rms[indx] = pr;
    glk[indx] = pg;
    lum[indx] = pl;
    off[indx] = po;
    glkE[indx] = pgE;
    lumE[indx] = plE;
    offE[indx] = poE;

    if (pr < rmin) { rmin=pr; jmin=j; } // Select best scaler
  }
}
if (jmin<0) { printf("ERROR - no good scaler found\n"); return; }
const char* detbest = dets[jmin].Data();
if (scaler<0) printf("*** Best scaler = %s  [ID = %d]\n\n",detbest,jmin);



gr_lk = new TGraphErrors(nfi);
gr_sc = new TGraphErrors(nfi);
gr_so = new TGraphErrors(nfi);
gr_s2 = new TGraphErrors(nfi);
gr_lk->SetTitle("Leak vs. SC*GL");
gr_sc->SetTitle("SC vs. GL");
gr_so->SetTitle("SO vs. GL");
gr_s2->SetTitle("SO vs. SC");
gr_lk->SetMarkerStyle(27);
gr_sc->SetMarkerStyle(27);
gr_so->SetMarkerStyle(27);
gr_s2->SetMarkerStyle(27);
gr_lk->SetMarkerColor(4);
gr_sc->SetMarkerColor(4);
gr_so->SetMarkerColor(4);
gr_s2->SetMarkerColor(4);

for (i=0; i<nfi; i++) {
  indx = i + jmin*nfi;
  int uindx = i + det[i]*nfi; // for the detector used in the dataset
  if (debug>2) printf("RMS=%5.3g Lum=%5.3g Glk=%5.3g\n",
    rms[indx],lum[indx],glk[indx]);
  gr_lk->SetPoint(i,usc[i]*ugl[i],glk[uindx]); // must use same detector
  gr_sc->SetPoint(i,ugl[i],lum[indx]);
  gr_so->SetPoint(i,ugl[i],off[indx]);
  gr_s2->SetPoint(i,lum[indx],off[indx]);
  gr_lk->SetPointError(i,0,glkE[indx]);
  gr_sc->SetPointError(i,0,lumE[indx]);
  gr_so->SetPointError(i,0,offE[indx]);
  gr_s2->SetPointError(i,lumE[indx],offE[indx]);
}

c1->Clear();
c1->Divide(2,2,0.01,0.025);

// For zero space charge run, do not do the full fits,
//  just report back the SpaceCharge quantities to try
//  for each of the GridLeaks used:
if (allZeros) {
  printf("\n*** Try the following calibration values: ***\n");
  for (i=0; i<nfi; i++) {
    indx = i + jmin*nfi;
    double sop = -off[indx]/lum[indx];
    printf("SC = %5.3g * ((%s) - (%5.3g))",lum[indx],detbest,sop);
    printf(" with GL = %4.1f\n\n",ugl[i]);
  }
  c1->cd(1);
  gr_sc->Draw("AP");
  c1->cd(2);
  gr_so->Draw("AP");
  return;
}


// Otherwise, step through the following:
// 1) Determine constraint on SpaceCharge rate * GridLeak
// 2) Determine SpaceCharge rate dependence
// 3) Determine SpaceCharge offset

double p0,p1,p2;

// Step 1) Determine constraint on SpaceCharge rate * GridLeak
// --
// We assume here that the observed leakage is directly proportional
//   to the amount of correction:
//     GLK(obs) = GLK(real) - C*(SC*GL)
//     GLK(obs) = p0 + p1*(SC*GL)
// GLK(obs) = 0  when  SC*GL = -p0/p1
c1->cd(1);
gr_lk->Fit("pol1","Q");
gr_lk->Draw("AP");
TF1* lk_fit = gr_lk->GetFunction("pol1");
p0 = lk_fit->GetParameter(0);
p1 = lk_fit->GetParameter(1);
double scXgl = -p0/p1;
printf("* Constraint on SC x GL = %5.3g\n",scXgl);

// Step 2) Determine SpaceCharge rate dependence
// --
// Here, we make a first guess on SC by finding the dependence of SC on GL
//   to first order, inserting the constraint on SC*GL, and then finding
//   the root of the equation:
//     SC = p0 + p1*GL
//     SC = p0 + p1*(SC*GL)/SC
//      0 = -SC^2 + p0*SC + p1*(SC*GL)
// =>  SC = (p0 + sqrt(p0^2 + 4*p1*(SC*GL)))*0.5
// Next, we try to determine the dependence between SC and GL to 2nd order:
//     SC = p0 + p1*GL + p2*GL^2
//     SC = p0 + p1*(SC*GL)/SC + p2*(SC*GL)^2/SC^2
//      0 = -SC^3 + p0*SC^2 + p1*(SC*GL)*SC + p2*(SC*GL)^2
// We determine the roots of this cubic equation using TMath,
//   and pick the root which is closest to the first order solution.
//   GL then is given by the SC*GL constraint.
c1->cd(2);
gr_sc->Fit("pol1","Q");
gr_sc->Draw("AP");
TF1* sc_fit = gr_sc->GetFunction("pol1");
p0 = sc_fit->GetParameter(0);
p1 = sc_fit->GetParameter(1);
double scp = (p0 + TMath::Sqrt(p0*p0+4*p1*scXgl))*0.5;

gr_sc->Fit("pol2","Q");
gr_sc->Draw("AP");
TF1* sc_fi2 = gr_sc->GetFunction("pol2");
p0 = sc_fi2->GetParameter(0);
p1 = sc_fi2->GetParameter(1);
p2 = sc_fi2->GetParameter(2);
double coefs[4];
coefs[3] = -1.0;
coefs[2] = p0;
coefs[1] = p1*scXgl;
coefs[0] = p2*scXgl*scXgl;
double s0,s1,s2;
TMath::RootsCubic(coefs,s0,s1,s2); // Find root of cubic polynomial
printf("* Guesses on SC = %5.3g , %5.3g , %5.3g , %5.3g\n",scp,s0,s1,s2);
double diff0 = TMath::Abs(1-(s0/scp));
double diff1 = TMath::Abs(1-(s1/scp));
double diff2 = TMath::Abs(1-(s2/scp));
scp = (diff0 < diff1 ? s0 : s1);
scp = (diff2 < TMath::Min(diff0,diff1) ? s2 : scp);
double glp = scXgl/scp;


// Step 3) Determine SpaceCharge offset
// --
// As before, determine SO dependence on GL to second order,
//   and plug in the value of GL already determined.
// Also, determine SO dependence on SC to second order,
//   and plug in the value of SC already determined.
// Take the average of the two guesses.
c1->cd(4);
gr_so->Fit("pol2","Q");
gr_so->Draw("AP");
TF1* so_fit = gr_so->GetFunction("pol2");
double sop = -(so_fit->Eval(glp))/scp;

c1->cd(3);
gr_s2->Fit("pol2","Q");
gr_s2->Draw("AP");
TF1* s2_fit = gr_s2->GetFunction("pol2");
double s2p = -(s2_fit->Eval(scp))/scp;
printf("* Guesses on SO = %5.3g , %5.3g\n",sop,s2p);
sop = 0.5*(sop+s2p);


// Done
printf("\n*** FINAL CALIBRATION VALUES: ***\n");
printf("SC = %5.3g * ((%s) - (%5.3g))",scp,detbest,sop);
printf(" with GL = %4.1f\n\n",glp);

c1->Update(); c1->Draw();

}



//////////////////////////////////////////////////

int FitLine(TTree* SC, const char* v0, const char* v1, double window,
     double& p0, double& p1, double& e0, double& e1, int debug) {
// Find a good line fit by removing outliers iteratively

  double mindif = 0.01;
  double dif = 1.0;
  TF1 fline("fline","[0]+[1]*x");
  fline.SetParameters(0,0);
  p0 = 0; p1 = 0;

  TCut cut2 = "";
  TString plot = Form("%s:%s",v0,v1);
  TString prof = plot + ">>hsca";
  SC->Draw(v1,cut);
  TH1* HSC = SC->GetHistogram();
  TProfile hsca("hsca","hschist",10,
      HSC->GetXaxis()->GetXmin(),HSC->GetXaxis()->GetXmax());

  int count=0;
  double oldp1[256];
  while (dif>mindif && count<256) {
    if (count>0)
      cut2 = Form("abs(%s-(%5.3g+(%5.3g)*(%s)))<%5.3g",v0,p0,p1,v1,window);
    SC->Draw(prof.Data(),cut&&cut2,"profs");
    hsca.Fit(&fline,"Q");
    SC->Draw(plot.Data(),cut&&cut2,"same");
    if (debug>4) { c1->Update(); c1->Draw(); }
    if (debug>5) {
      int tempi;
      printf("Waiting...\n");
      cin >> tempi;
      if (tempi<0) return -1;
    }
    p0 = fline.GetParameter(0); p1 = fline.GetParameter(1);
    e0 = fline.GetParError(0) ; e1 = fline.GetParError(1);
    dif = 1.0;
    for (int cmp=0; cmp<count; cmp++) {
      double olddif = TMath::Abs(1.0-(p1/oldp1[cmp]));
      dif = TMath::Min(dif,olddif);
    }
    oldp1[count]=p1;
    if (debug>3) printf("Finishing iteration %d\n",count);
    count++;
  }
  if (debug>1) printf("Linear fit within %s\n",cut2.GetTitle());
  return count;
}


/////////////////////////////////////////////////////////////////
// $Id: Calib_SC_GL.C,v 1.1 2006/05/23 16:14:42 genevb Exp $
// $Log: Calib_SC_GL.C,v $
// Revision 1.1  2006/05/23 16:14:42  genevb
// Moved macro to calib directory
//
// Revision 3.2  2006/01/11 19:02:35  genevb
// Better documentation
//
// Revision 3.1  2006/01/07 00:35:14  genevb
// Introduce macro
//
