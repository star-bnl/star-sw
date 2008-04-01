/////////////////////////////////////////////////////////////////
//
// Calib_SC_GL.C
//   Macro for calibrating the SpaceCharge and GridLeak
//   distortion corrections.
//
//   Author: G. Van Buren, BNL
//           Jan 6, 2006
//
// Synopsis:
// The general idea is to
//   1) Find the SpaceCharge * GridLeak which gives zero leakage distortions.
//   2) Fit the relationship between SpaceCharge and GridLeak.
//   3) The above give two equations and two unknowns, which can be solved.
//   4) Find the SpaceCharge offset for the desired SpaceCharge and GridLeak.
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
// A # at the beginning of a file spec will cause that dataset to be skipped:
//    #histsSetC/* 4 0 12.0
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
#include "TPolyMarker.h"
#include <Stiostream.h>
#endif


// Main routine:
void Calib_SC_GL(const char* input, const char* cuts=0, int scaler=-1, int debug=0);

// Helper functions:
int  FitLine(TTree* SC, const char* v0, const char* v1, double window,
     double& p0, double& p1, double& e0, double& e1, int debug=0);
int  Waiting();
double Differ(double s0, double s1, double* co);
void FindMinMax(TH1* h, double& min, double& max);


// Global parameters:
const int nfip=128;
const int nsca=5;
const int npos=nfip*nsca;
TCanvas* c1 = 0;
TChain* SCi[nfip];
TGraphErrors* gr_lk=0;
TGraphErrors* gr_sc=0;
TGraphErrors* gr_so=0;
TGraphErrors* gr_s2=0;
TGraphErrors* gr_go=0;
TGraphErrors* gr_temp=0;
TF1* scgl_fit=0;
TCut cut;

// The following parameters define the windows used to remove
// outliers from the distributions. No good ways to determine
// this a priori, so set by hand for now

//double MAX_GAPD = 0.012; // 0.005
//double MAX_SC = 0.0010; // 0.0003
double MAX_GAPD = 0.015; // 0.005
double MAX_SC = 0.0010; // 0.0003
//double MAX_GAPD = 0.010; // 0.005
//double MAX_SC = 0.0010; // 0.0003

//////////////////////////////////////////////////



void Calib_SC_GL(const char* input, const char* cuts, int scaler, int debug) {


// Define useful luminosity scaler detectors
TString dets[nsca];
dets[0] = "bbce+bbcw-4*(bbcyb+bbcbb)"; // Just for fun.
dets[1] = "bbcx"     ;
dets[2] = "zdcx"     ;
dets[3] = "zdce+zdcw";
dets[4] = "bbce+bbcw";

int minsca = 0;
int maxsca = nsca;
if (scaler>=0) { minsca = scaler; maxsca = scaler+1; }

int i,j,indx,uindx,jmin,nfi,result;
TString fis[nfip];// file names
int det[nfip];    // detector used (see dets above)
double usc[nfip]; // SC used
double ugl[nfip]; // GL used
double uso[nfip]; // SO used
double sca[nfip]; // Scale factor to compare detectors

char fname[128];
Bool_t allZeros = kTRUE;

// Read input file
ifstream dats(input);
nfi = nfip;
for (i=0;i<nfi;i++) {
  dats >> fname;
  if (dats.eof()) { nfi=i; continue; }
  fis[i] = fname;
  dats >> det[i] >> usc[i] >> ugl[i];
  if (usc[i]) allZeros = kFALSE;

  if (fis[i].Contains("!")) { // Indicates that the used SO value is to be read in
    dats >> uso[i];
    fis[i].Remove(fis[i].First('!'),1);
  } else {
    uso[i] = 0;
  }
 
  if (fname[0]=='#') i--; // Skip lines beginning with #
}
printf("Found %d dataset specifications.\n",nfi);

cut = ((cuts) ? cuts : "");

if (c1==0) c1 = new TCanvas("c1","Calib SC and GL");

// Data arrays
double rms[npos];
double lum[npos];
double off[npos];
double glk[npos];
double glo[npos];
double go[npos];
double lumE[npos];
double offE[npos];
double glkE[npos];
double gloE[npos];
//double goE[npos];

double rmin=1;
jmin=-1;
SCi[nfip-1] = new TChain("SC","ChainAll");
for (i=0;i<nfi;i++) { // run

  // Build TChains from histogram files
  SCi[i] = new TChain("SC",Form("Chain%d : %s",i,fis[i].Data()));
  int added_files=0;
  if ((added_files = SCi[i]->Add(fis[i].Data())) < 1)
    printf("Warning: no files added from %s\n",fis[i].Data());
  else {
    printf("%d files added from %s\n",added_files,fis[i].Data());
    SCi[nfip-1]->Add(fis[i].Data());
  }
  SCi[i]->SetMarkerStyle(7);
  SCi[i]->SetMarkerColor(2);

  // Loop over available scaler detectors
  for (j=minsca;j<maxsca;j++) {
    indx = i + j*nfi;
    double pr,pg,pl,po,pgE,plE,poE,pgo,pgoE;
    const char* dt = dets[j].Data();
  
    // Determine GridLeak
    result = FitLine(SCi[i],"gapd",dt,MAX_GAPD,pgo,pg,pgoE,pgE,debug);
    if (result<0) return;

    // Determine SpaceCharge
    result = FitLine(SCi[i],"sc",dt,MAX_SC,po,pl,poE,plE,debug);
    if (result<0) return;
  
    TString plot = Form("sc-(%5.3g+(%5.3g)*(%s))",po,pl,dt);
    SCi[i]->Draw(plot.Data(),cut);
    pr = SCi[i]->GetHistogram()->GetRMS();
    if (debug>0) printf("RMS = %5.3g for %s\n",pr,dt);

    rms[indx] = pr;
    glk[indx] = pg;
    lum[indx] = pl;
    off[indx] = po;
    glo[indx] = pgo;
    glkE[indx] = pgE;
    lumE[indx] = plE;
    offE[indx] = poE;
    gloE[indx] = pgoE;
    go[indx] = pgo + pg*uso[i];
    //goE[indx] = TMath::Sqrt(((pgoE*pgoE)/(pgo*pgo))+((pgE*pgE*uso[i]*uso[i])/(pg*pg)));

    if (pr < rmin) { rmin=pr; jmin=j; } // Select best scaler
  }
}
if (jmin<0) { printf("ERROR - no good scaler found\n"); return; }
const char* detbest = dets[jmin].Data();
if (scaler<0) { // were looking for the best scaler
  printf("*** Best scaler = %s  [ID = %d]\n\n",detbest,jmin);
}


// Define useful graphs
gr_lk = new TGraphErrors(nfi);
gr_sc = new TGraphErrors(nfi);
gr_so = new TGraphErrors(nfi);
gr_s2 = new TGraphErrors(nfi);
gr_go = new TGraphErrors(nfi);
gr_lk->SetTitle("Leak vs. SC*GL");
gr_sc->SetTitle("SC vs. GL");
gr_so->SetTitle("SO vs. GL");
gr_s2->SetTitle("SO vs. SC");
gr_go->SetTitle("Leak @uso vs. uso");
gr_lk->SetMarkerStyle(27);
gr_sc->SetMarkerStyle(27);
gr_so->SetMarkerStyle(27);
gr_s2->SetMarkerStyle(27);
gr_go->SetMarkerStyle(27);
gr_lk->SetMarkerColor(1);
gr_sc->SetMarkerColor(4);
gr_so->SetMarkerColor(4);
gr_s2->SetMarkerColor(4);
gr_go->SetMarkerColor(1);
TPolyMarker mark;
mark.SetMarkerColor(2);
mark.SetMarkerStyle(28);
double zero=0;

for (i=0; i<nfi; i++) {
  indx = i + jmin*nfi;
  if (det[i] != jmin) {
    for (j=0; j<i; j++) {
      if (det[i] == det[j]) { sca[i] = sca[j]; break; }
    }
    if (j==i) {
      // determine scale factors between the scaler quantities
      double temp1,temp2,temp3;
      result = FitLine(SCi[nfip-1],dets[det[i]],dets[jmin],1e10,temp1,sca[i],temp2,temp3,debug);
      if (result<0) return;
    }
    uindx = i + det[i]*nfi; // for the detector used in the dataset
  } else {
    sca[i] = 1.0;
    uindx = indx;
  }
  if (debug>2) printf("RMS=%5.3g Lum=%5.3g Glk=%5.3g\n",
    rms[indx],lum[indx],glk[indx]);

  // Fill the graphs which will be fit
  gr_lk->SetPoint(i,usc[i]*sca[i]*ugl[i],glk[indx]); // must use same detector
  gr_sc->SetPoint(i,ugl[i],lum[indx]);
  gr_so->SetPoint(i,ugl[i],off[indx]);
  gr_s2->SetPoint(i,lum[indx],off[indx]);
  gr_go->SetPoint(i,usc[i]*ugl[i],glo[indx]);
  gr_lk->SetPointError(i,0,glkE[indx]);
  gr_sc->SetPointError(i,0,lumE[indx]);
  gr_so->SetPointError(i,0,offE[indx]);
  gr_s2->SetPointError(i,lumE[indx],offE[indx]);
  gr_go->SetPointError(i,0,gloE[indx]);
  //printf("UO=%f    GLO=%f +/- %f    GO=%f +/- %f\n",uso[i],glo[indx],gloE[indx],go[indx],goE[indx]);
}

c1->Clear();
c1->Divide(2,2,0.01,0.025);

double p0,p1,p2,p3;

gr_go->Fit("pol1","0Q");
TF1* go_fit = gr_go->GetFunction("pol1");
go_fit->SetLineColor(4);
p0 = go_fit->GetParameter(0);
p1 = go_fit->GetParError(0);
p2 = go_fit->GetParameter(1);
p3 = go_fit->GetParError(1);
//printf("\nGap at zero correction = (%5.3g +/- %5.3g) + zdc*(%5.3g +/- %5.3g)\n",p0,p1,p2,p3);


//////////////////////////////////////////
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


//////////////////////////////////////////
// Step 1) Determine constraint on SpaceCharge rate * GridLeak
// --
// We assume here that the observed leakage is directly proportional
//   to the amount of correction:
//     GLK(obs) = GLK(real) - C*(SC*GL)
//     GLK(obs) = p0 + p1*(SC*GL)
// GLK(obs) = 0  when  SC*GL = -p0/p1
c1->cd(1);
gr_lk->Fit("pol1","Q");
TF1* lk_fit = gr_lk->GetFunction("pol1");
lk_fit->SetLineColor(4);
gr_lk->Draw("AP");
p0 = lk_fit->GetParameter(0);
p1 = lk_fit->GetParameter(1);
double scXgl = -p0/p1;
printf("* Constraint on SC x GL = %5.3g\n",scXgl);

if (!scgl_fit) scgl_fit = new TF1("scgl_fit","[0]/x",-5.,100.);
scgl_fit->SetParameter(0,scXgl);
scgl_fit->SetLineColor(4);
scgl_fit->SetLineWidth(1);
mark.DrawPolyMarker(1,&scXgl,&zero);


//////////////////////////////////////////
// Step 2) Determine SpaceCharge rate dependence
// --
// Here, we make a first guess on SC by finding the dependence of SC on GL
//   to first order, inserting the constraint on SC*GL, and then finding
//   the root of the equation:
//     SC = p0 + p1*GL
//     SC = p0 + p1*(SC*GL)/SC
//      0 = -SC^2 + p0*SC + p1*(SC*GL)
// =>  SC = (p0 + sqrt(p0^2 + 4*p1*(SC*GL)))*0.5
// ...or multiplying by GL...
//      0 = -(SC*GL) +p0*GL + p1*GL^2
// =>  GL = (-p0 + sqrt(p0^2 + 4*p1*(SC*GL)))*0.5/p1, but this is identical
//
//   If there is no intersection between the line and hyperbola, which we
//   we can identify by a negative value to take sqrt, then we take the
//   point of closest approach, which is where the derivatives match:
//     SC = (SC*GL)/GL
//     d(SC)/d(GL) = -(SC*GL)/GL^2 = p1
//     p1 = -SC^2/(SC*GL)
// =>  SC = sqrt(-p1*(SC*GL))
//
// Next, we try to determine the dependence between SC and GL to second order:
//     SC = p0 + p1*GL + p2*GL^2
//     SC = p0 + p1*(SC*GL)/SC + p2*(SC*GL)^2/SC^2
//      0 = -SC^3 + p0*SC^2 + p1*(SC*GL)*SC + p2*(SC*GL)^2
// ...or multiplying by GL...
//      0 = -(SC*GL) + p0*GL + p1*GL^2 + p2*GL^3
// We determine the roots of these cubic equations using TMath, and pick
//   the positive root which is closest to the first order solution.
//   GL or SC then is given by the SC*GL constraint.
// Note that we will end up with three guesses:
//   one from the first order solution
//   one from the second order solution against SC
//   one from the second order solution against GL
// The second order solution closest to the first order solution
//   is chosen as the final solution
double scp,glp,scp1,glp1,scp2,glp2;
double s0,s1,s2,diff0,diff1,diff2;
Bool_t one_sol;

// Find via straight line
c1->cd(2);
gr_sc->Fit("pol1","Q");
gr_sc->Draw("AP");
TF1* sc_fit = gr_sc->GetFunction("pol1");
p0 = sc_fit->GetParameter(0);
p1 = sc_fit->GetParameter(1);
scp = p0*p0+4*p1*scXgl;
if (scp>=0) scp = (p0 + TMath::Sqrt(scp))*0.5;
else {
  scp = TMath::Sqrt(-p1*scXgl);
  printf("Using closest approach on first guess...\n");
}
glp = scXgl/scp;

// Find via SC-cubic
gr_sc->Fit("pol2","Q");
gr_sc->Draw("AP");
scgl_fit->Draw("same");
TF1* sc_fi2 = gr_sc->GetFunction("pol2");
p0 = sc_fi2->GetParameter(0);
p1 = sc_fi2->GetParameter(1);
p2 = sc_fi2->GetParameter(2);
double coefs[4];
coefs[3] = -1.0;
coefs[2] = p0;
coefs[1] = p1*scXgl;
coefs[0] = p2*scXgl*scXgl;
one_sol = TMath::RootsCubic(coefs,s0,s1,s2); // Find root of cubic polynomial
if (one_sol) {
  printf("* Guesses on SC = %5.3g , %5.3g\n",scp,s0);
  scp1 = s0;
} else {
  printf("* Guesses on SC = %5.3g , %5.3g , %5.3g , %5.3g\n",scp,s0,s1,s2);
  diff0 = Differ(scp,s0,coefs);
  diff1 = Differ(scp,s1,coefs);
  diff2 = Differ(scp,s2,coefs);
  scp1 = (diff0 < diff1 ? s0 : s1);
  scp1 = (diff2 < TMath::Min(diff0,diff1) ? s2 : scp1);
}
glp1 = scXgl/scp1;

// Find via GL-cubic
coefs[3] = p2;
coefs[2] = p1;
coefs[1] = p0;
coefs[0] = -scXgl;
one_sol = TMath::RootsCubic(coefs,s0,s1,s2); // Find root of cubic polynomial
if (one_sol) {
  printf("* Guesses on GL = %5.3g , %5.3g\n",glp,s0);
  glp2 = s0;
} else {
  printf("* Guesses on GL = %5.3g , %5.3g , %5.3g , %5.3g\n",glp,s0,s1,s2);
  diff0 = Differ(glp,s0,coefs);
  diff1 = Differ(glp,s1,coefs);
  diff2 = Differ(glp,s2,coefs);
  glp2 = (diff0 < diff1 ? s0 : s1);
  glp2 = (diff2 < TMath::Min(diff0,diff1) ? s2 : glp2);
}
scp2 = scXgl/glp2;

// Decide which of the second order solutions is best
diff1 = TMath::Sqrt(pow((scp1/scp)-1,2)+pow((glp1/glp)-1,2));
diff2 = TMath::Sqrt(pow((scp2/scp)-1,2)+pow((glp2/glp)-1,2));
if (diff1 < diff2) {
  scp = scp1; glp = glp1;
} else {
  scp = scp2; glp = glp2;
}
mark.DrawPolyMarker(1,&glp,&scp);


//////////////////////////////////////////
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

double som = -scp*sop;
mark.DrawPolyMarker(1,&scp,&som);
c1->cd(4);
mark.DrawPolyMarker(1,&glp,&som);


//////////////////////////////////////////
// Done
printf("\n*** FINAL CALIBRATION VALUES: ***\n");
printf("SC = %5.3g * ((%s) - (%5.3g))",scp,detbest,sop);
printf(" with GL = %5.2f\n\n",glp);


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
  int i,j,nbinsc = 12;
  if (!gr_temp) {
    gr_temp = new TGraphErrors(nbinsc);
    gr_temp->SetMarkerStyle(25);
    gr_temp->SetMarkerColor(4);
  }

  TCut cut2 = "";
  TString plot = Form("%s:%s",v0,v1);
  TString prof = plot + ">>hsca";
  TString profp = Form("%s:%s>>hscap",v1,v1);
  if (debug>2) printf("FitLine for \"%s\" from \"%s\"\n",plot.Data(),SC->GetTitle());
  SC->Draw(v1,cut);
  TH1* HSC = SC->GetHistogram();
  if (!HSC) printf("Problem creating hist of \"%s\" from \"%s\"\n",v1,SC->GetTitle());
  double hmin,hmax;
  FindMinMax(HSC,hmin,hmax);
  TProfile hsca("hsca","hschist",nbinsc,hmin,hmax);
  TProfile hscap("hscap","hschistp",nbinsc,hmin,hmax);

  int count=0;
  double oldp1[256];
  while (dif>mindif && count<256) {
    if (count>0)
      cut2 = Form("abs(%s-(%5.3g+(%5.3g)*(%s)))<%5.3g",v0,p0,p1,v1,window);
    SC->Draw(prof.Data(),cut&&cut2,"prof"); // prof vs. profs doesn't seem to make a difference
    SC->Draw(profp.Data(),cut&&cut2,"prof");
    i = 0;
    for (j=1; j<=nbinsc; j++) {
      if (TMath::Abs(hscap.GetBinError(j)) > 1e-10) {
        gr_temp->SetPoint(i,hscap.GetBinContent(j),hsca.GetBinContent(j));
        gr_temp->SetPointError(i,hscap.GetBinError(j),hsca.GetBinError(j));
        i++;
      }
    }
    gr_temp->Set(i);
    //hsca.Fit(&fline,"C");
    gr_temp->Fit(&fline,"QC");
    if (debug>4) {
      c1->Clear();
      gr_temp->Draw("AP");
      //hsca.Draw("same");

      SC->Draw(plot.Data(),cut&&cut2,"same");
      c1->Update(); c1->Draw();
      if (debug>5 && Waiting()<0) return -1;
    }
    p0 = fline.GetParameter(0); p1 = fline.GetParameter(1);
    e0 = fline.GetParError(0) ; e1 = fline.GetParError(1);
    dif = 1.0;
    for (int cmp=0; cmp<count; cmp++) {
      double olddif = 1e10;
      if (oldp1[cmp]!=0) olddif = TMath::Abs(1.0-(p1/oldp1[cmp]));
      dif = TMath::Min(dif,olddif);
    }
    oldp1[count]=p1;
    if (debug>3) printf("Finishing iteration %d\n",count);
    count++;
  }
  if (debug>1) printf("Linear fit within %s\n",cut2.GetTitle());
  return count;
}

int Waiting() {
  int tempi;
  printf("Waiting...\n");
  cin >> tempi;
  return tempi;
}

double Differ(double s0, double s1, double* co) {
  // Must have at least the same sign
  if (s1*s0 < 0) return 1e15;
  double eval = co[0]+s1*co[1]+s1*s1*co[2]+s1*s1*s1*co[3];
  // Make sure it is in fact a root solution
  if (TMath::Abs(eval) > 0.01*TMath::Abs(co[0])) return 2e15;
  return TMath::Abs(1.0-(s1/s0));
}

void FindMinMax(TH1* h, double& min, double& max) {
  // Account for the default histograms having 10% margins
  min = h->GetXaxis()->GetXmin();
  max = h->GetXaxis()->GetXmax();
  //return;

  double margin = 0.075*(max-min);
  min -= margin;
  max += margin;
}

/////////////////////////////////////////////////////////////////
// $Id: Calib_SC_GL.C,v 1.2 2008/04/01 22:17:52 genevb Exp $
// $Log: Calib_SC_GL.C,v $
// Revision 1.2  2008/04/01 22:17:52  genevb
// Improvements in documentation, fitting, graphing
//
// Revision 1.1  2006/05/23 16:14:42  genevb
// Moved macro to calib directory
//
// Revision 3.2  2006/01/11 19:02:35  genevb
// Better documentation
//
// Revision 3.1  2006/01/07 00:35:14  genevb
// Introduce macro
//
