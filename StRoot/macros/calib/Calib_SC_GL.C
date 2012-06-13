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
//   1) Determine the luminosity scaler which gives the smallest variance
//        in a linear fit to measured SpaceCharge
//   2) Fit GridLeak using the selected luminosity scaler
//   3) Use the two distinct constraints on SC & GL from these two fits
//        to arrive at initial values
//   4) Fit SpaceCharge & GridLeak simultaneously
//
// Parameters:
//   1) Input file with info on data to be calibrated
//   2) Any additional cuts to help calibrate (i.e. "run!=6082047")
//   3) A specific scaler identifier (if you know which scaler you want to use)
//      Possible values:
//      -1    : automatically determine which of the predefined scalers is best
//      0..n  : force the calibration to use specific scaler with this id
//              (see code below, or StDetectorDbMaker/St_spaceChargeCorC.cxx)
//      -n    : perform principal components analysis with up to 'n' scalers
//      "det" : force the calibration to use a specific scaler, e.g. "zdcx"
//      "det1:det2:..." : perform principal components analysis using a specific
//              set of scalers, e.g. "zdcx:zdcx*zdcx" allows a function of
//              sc = A*(zdcx-offset)+B*(zdcx^2)
//   4) Debug mode
//
// Details:
//
// The macro now only runs compiled, e.g. execute as Calib_SC_GL.C+(...)
//
// PCA does not determine errors on individual terms, so errors are reported
//   on the overall scale, e.g. (1.0 +/- error)*(...)
//
// The input file should contain lines with the following info:
//   - dataset file specification
//   - scaler detector used in that dataset (see dets strings below)
//   - SpaceCharge rate used in that dataset
//   - GridLeak multiplier used in that dataset
//   - (optional) SpaceCharge offset used in that dataset (set "!" below)
//
// There should be at least 3 datasets with different GridLeaks!
// Example 3 dataset file which used bbce+bbcw would look like this:
//    histsSetA/* 4 1.7e-8 9.0
//    histsSetB/* 4 1.7e-8 12.0
//    histsSetC/* 4 1.7e-8 15.0
// A ! at the beginning of a file spec will cause the optional SpaceCharge
// offset to be read in on that line:
//    !histsSetC/* 4 0 12.0 550
// A # at the beginning of a file spec will cause that dataset to be skipped:
//    #histsSetC/* 4 0 12.0
// If this file is named input.dat, it could be analyzed with:
//    root Calib_SC_GL.C+("input.dat")
// If you knew you wanted to use only bbce+bbcw:
//    root Calib_SC_GL.C+("input.dat","",4)
//
/////////////////////////////////////////////////////////////////

#ifndef __CINT__
#include "TCanvas.h"
#include "TChain.h"
#include "TGraphErrors.h"
#include "TF3.h"
#include "TH2.h"
#include "TProfile.h"
#include "TArrayD.h"
#include "TCut.h"
#include "TMarker.h"
#include "TPolyMarker.h"
#include "TEllipse.h"
#include "TFitter.h"
#include "TMinuit.h"
#include "TMath.h"
#include "TStyle.h"
#include "TLine.h"
#include "TString.h"
#include "TPrincipal.h"
#include "TROOT.h"
#include <Stiostream.h>
#endif


Bool_t USE_OLD_STDDEV = kTRUE; // method for determining STDDEV
Bool_t FORCE_GLO_SO = kFALSE; // force GLO = SO
Bool_t NO_PLOTS = kFALSE;
double MAX_DEV = 3.0;

// Main routine:
void Calib_SC_GL(const char* input, const char* cuts=0, int scaler=-1, int debug=0);
void Calib_SC_GL(const char* input, const char* cuts, const char* scalerstr, int debug=0);

// Fitting functions:
Double_t funcGapf(Double_t* x, Double_t* pars);
Double_t funcSC(Double_t* x, Double_t* pars);
void fnchGapf(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag);
void fnchSC(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag);
void fnchSCGapf(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag);

// Helper functions:
void Init(const char* input);
int  Waiting();
void SetMinMax(int n, Double_t* ar, double& min, double& max, double inclusion, double margin=0.075);
void SetMinuitPars(const int n, TString* names, Double_t* starts, Double_t* steps, int debug);
void Log2Lin(int i);
int  FitWithOutlierRemoval(int debug);
void DrawErrorContours();
TString PCA(int Nmax=32, int debug=0);
void PrintResult(double scp, double escp, double sop, double esop,
                 double glp, double eglp, const char* det);

// Global parameters:
const int nLimit=150;
const int nfip=128;
const int nsca=32;
const int npos=nfip*nsca;
const int nMeasuresMax=nfip*256;
int nMeasures = 0;
int nMeasuresI = 0;
int nfi = 0;

// Global inputs
TCut cut;
TChain* SCi[nfip];
TChain* SCall = 0;
int det[nfip];    // detector used (see dets above)
Bool_t allZeros = kTRUE;
TString scastr;

// Graphics
TCanvas* cSummary = 0;
TCanvas* conSC[nsca];
TCanvas* conGL = 0;
TCanvas* conSCGL = 0;
TCanvas* cSC = 0;
TCanvas* cGL = 0;

// Fitting
TF1* scgl_fit = 0;
TVirtualFitter* minuit = 0;
TMinuit* minuit2 = 0;
double arglist[10];
Double_t* fitPars = 0;
Double_t* fitParErrs = 0;
char** parName = 0;
Double_t fitParsSC[3];
Double_t fitParErrsSC[3];
Double_t fitParsGL[3];
Double_t fitParErrsGL[3];
Double_t fitParsSCGL[6];
Double_t fitParErrsSCGL[6];
char* parNameSC[3];
char* parNameGL[3];
char* parNameSCGL[6];

// Global fit data and parameters
Double_t m_sc[nMeasuresMax];
Double_t m_gapf[nMeasuresMax];
Double_t m_L[nMeasuresMax];
Double_t m_L2[nMeasuresMax];
Int_t m_set[nMeasuresMax];
Double_t devsSC[nMeasuresMax];
Double_t devsGL[nMeasuresMax];
Double_t devs_set[nfip];
Double_t* devs;
Bool_t outliers0[nMeasuresMax];
Bool_t outliersSC[nMeasuresMax];
Bool_t outliersGL[nMeasuresMax];
Bool_t* outliers;
Double_t nno[nfip]; // number of non-outliers per set

double STDDEV = 1.0;
double CHISQ = 1.0;
double VAR = 1.0;
double VAR_all = 1.0;
double STDDEV_SC = 0.001;
double STDDEV_GL = 0.035;

// Data for each chain
double uso[nfip]; // SO used
double usc[nfip]; // SC used
double ugl[nfip]; // GL used
double usg[nfip]; // SC*GL used
double ugo[nfip]; // SO*SC*GL used

// Plot data:
double xgl[nMeasuresMax];
double ygl[nMeasuresMax];
double xgo[nMeasuresMax];
double ygo[nMeasuresMax];
double xsc[nMeasuresMax];
double ysc[nMeasuresMax];
double yso[nMeasuresMax];

// PCA globals
TString pcadets[nsca];
double pcacoef[nsca];
double pcaoffset;
int pcaN = -1;
const int maxp = 512;
TPrincipal* ppl[maxp];
Bool_t ITER0 = kFALSE;



//////////////////////////////////////////
// Main routine
//////////////////////////////////////////

void Calib_SC_GL(const char* input, const char* cuts, const char* scalerstr, int debug) {
  scastr = scalerstr;
  if (scastr.Contains(":")) Calib_SC_GL(input,cuts,  -999,debug);
  else                      Calib_SC_GL(input,cuts,nsca-1,debug);
}

void Calib_SC_GL(const char* input, const char* cuts, int scaler, int debug) {

  // Define useful luminosity scaler detectors
  TString dets[nsca];
  dets[0] = "bbce+bbcw-4*(bbcyb+bbcbb)"; // Just for fun.
  dets[1] = "bbcx"     ;
  dets[2] = "zdcx"     ;
  dets[3] = "zdce+zdcw";
  dets[4] = "bbce+bbcw";
  //dets[11] = "zdcc"    ;
  //dets[12] = "bbcc"    ;
  // reserve dets[nsca-1] for special uses (PCA, manually provided scalers)

  Init(input);

  Bool_t DO_PCA = (scaler < -1);

  if (DO_PCA) { // Doing a PCA analysis
    if (!ITER0) {
      Bool_t no_plots = NO_PLOTS;
      NO_PLOTS = kTRUE;
      ITER0 = kTRUE;
      printf("\n*** Running PCA iteration 0 ***\n\n");
      // First iteration uses a single input dataset to define PCA
      Calib_SC_GL(input,cuts,scaler,debug);
      // Second pass uses a all input datasets to define PCA
      NO_PLOTS = no_plots;
      ITER0 = kFALSE;
      printf("\n*** Running PCA iteration 1 ***\n\n");
      delete minuit;
    } else {
      fitPars = fitParsSCGL;
    }
    dets[nsca-1] = PCA(1-scaler,debug);
    scaler = nsca-1;
  } else {
    if (scastr.Length()) {
      dets[nsca-1] = scastr;
      scaler = nsca - 1;
    }
  }


  cut = ((cuts) ? cuts : "");
  int i,j,k,status;
  double temp1,temp2,vsc_min = 1e10;
  int jmin=-1;


  // Data arrays
  double glk[nfip];
  double glo[nfip];
  double spc[nfip];
  double spo[nfip];
  double sce[nsca];
  double sof[nsca];
  double ggl[nsca];
  double sceE[nsca];
  double sofE[nsca];
  double gglE[nsca];
  double vsc[nsca];
  double ssc[nsca];
  double vgl;

  // Define useful graphics tools
  TMarker mark;
  mark.SetMarkerColor(2);
  mark.SetMarkerStyle(28);
  TPolyMarker mark2;
  TEllipse ellip;
  ellip.SetLineColor(2);
  ellip.SetLineStyle(2);
  ellip.SetFillStyle(0);
  TLine line;
  line.SetLineStyle(2);
  line.SetLineColor(4);
  double zero = 0;


  // Prepare for SC fit
  devs = devsSC;
  outliers = outliers0;
  fitPars = fitParsSC;
  fitParErrs = fitParErrsSC;
  parName = parNameSC;



  // Loop over available scaler detectors
  for (j=0;j<nsca;j++) {
    if (scaler>=0 && j!=scaler) continue;
    if (dets[j].Length()<1) continue;
    const char* dt = dets[j].Data();


    //////////////////////////////////////////
    // 2D fit for SpaceCharge

    // Set dimensional variables
    nMeasures = 0;
    for (i=0;i<nfi;i++) { // parameter sets
      SCi[i]->Draw(Form("sc:%s",dt),cut,"goff");
      nMeasuresI = SCi[i]->GetSelectedRows();
      memcpy(&(m_sc[nMeasures]),SCi[i]->GetV1(),nMeasuresI*sizeof(Double_t));
      memcpy(&(m_L [nMeasures]),SCi[i]->GetV2(),nMeasuresI*sizeof(Double_t));
      for (k=0; k<nMeasuresI; k++) m_set[k+nMeasures] = i;
      nMeasures += nMeasuresI;
    }
    STDDEV = STDDEV_SC;

    // Set starting values and step sizes for parameters
    double sce_init = 25.0; // approximate guess on (g5 + GL)
    if (DO_PCA) {
      if (!ITER0) sce_init = fitParsSCGL[1]+fitParsSCGL[4];
    } else {
      SCi[nfi-1]->Draw(Form("sc/(%s)",dt),cut,"goff");
      TH1* htemp = SCi[nfi-1]->GetHistogram();
      sce_init *= (htemp->GetMean());
    }
    if (debug) printf("\nUsing initial value for SCe of %g\n",sce_init);

    TString sname[3] = {"SO","log(SCe)","log(g5)"};
    Double_t sstart[3] = {100., TMath::Log(sce_init), TMath::Log(15.0)};
    Double_t sstep[3] = {1000., 0.5, 0.1};
    if (DO_PCA) sstart[0] = 0;
    SetMinuitPars(3,sname,sstart,sstep,debug);
    minuit->SetFCN(fnchSC);

    // Perform the fit
    printf("\nSpaceCharge fit results (scaler: %s):\n",dt);
    status = FitWithOutlierRemoval(debug);
    if (status) {
      printf("Fit failed for sc, scaler = %s, err = %d\nTrying next scaler (if any)...\n\n",dt,status);
      Waiting();
      continue;
    }
    if (!NO_PLOTS && debug>0) {
      if (conSC[j] ==0) {
        conSC[j] = new TCanvas(Form("conSC_%d",j),Form("SC fit contours for %s",dt),600,400);
        conSC[j]->Divide(2,2);
      } else conSC[j]->cd();
      DrawErrorContours();
    }
    Log2Lin(1);
    Log2Lin(2);
    sof[j] = fitPars[0];
    sce[j] = fitPars[1];
    ggl[j] = fitPars[2];
    sofE[j] = fitParErrs[0];
    sceE[j] = fitParErrs[1];
    gglE[j] = fitParErrs[2];
    vsc[j] = VAR;
    ssc[j] = STDDEV;

    if (debug>0) printf("VAR_all = %6.4g for %s\n",VAR_all,dt);
    if (VAR_all < vsc_min) { // Select best scaler
      vsc_min = VAR_all;
      jmin = j;
      memcpy(outliersSC,outliers0,nMeasures*sizeof(Double_t));
    }

  }

  //////////////////////////////////////////
  // Set up for using the best scaler

  if (jmin<0) { printf("ERROR - no good scaler found\n"); return; }
  const char* detbest = dets[jmin].Data();
  if (scaler<0) { // were looking for the best scaler
    printf("*** Best scaler = %s  [ID = %d]\n\n",detbest,jmin);
  }
  fitParsSC[0] = sof[jmin];
  fitParsSC[1] = sce[jmin];
  fitParsSC[2] = ggl[jmin];
  fitParErrsSC[0] = sofE[jmin];
  fitParErrsSC[1] = sceE[jmin];
  fitParErrsSC[2] = gglE[jmin];
  STDDEV_SC = ssc[jmin];


  //////////////////////////////////////////
  // 3D fit for GridLeak

  // Prepare for GL fit
  devs = devsGL;
  outliers = outliersGL;
  fitPars = fitParsGL;
  fitParErrs = fitParErrsGL;
  parName = parNameGL;
  STDDEV = STDDEV_GL;
  
  // Set dimensional variables
  nMeasures = 0;
  for (i=0;i<nfi;i++) {
    SCi[i]->Draw(Form("gapf:%s:%s",detbest,dets[det[i]].Data()),cut,"goff");
    nMeasuresI = SCi[i]->GetSelectedRows();
    memcpy(&(m_gapf[nMeasures]),SCi[i]->GetV1(),nMeasuresI*sizeof(Double_t));
    memcpy(&(m_L   [nMeasures]),SCi[i]->GetV2(),nMeasuresI*sizeof(Double_t));
    memcpy(&(m_L2  [nMeasures]),SCi[i]->GetV3(),nMeasuresI*sizeof(Double_t));
    for (k=0; k<nMeasuresI; k++) m_set[k+nMeasures] = i;
    nMeasures += nMeasuresI;
  }

  // Set starting values and step sizes for parameters
  TString gname[3] = {"g2","gGL/g2 = SCxGL","GLO"};
  Double_t gstart[3] = {1.0, sce[jmin]*0.4, 0.}; // guess GL/(g5+gL) ~=(10/(15+10)) ~= 0.4
  Double_t gstep[3]  = {0.1, sceE[jmin]*0.4, 100.0};
  if (DO_PCA) {
    gstart[1] = 10.0; // gues GL ~= 10.0
    gstep[1] = 1.0;
  }
  SetMinuitPars(3,gname,gstart,gstep,debug);
  minuit->SetFCN(fnchGapf);

  // Perform the fit
  printf("\nGridLeak fit results (scaler: %s):\n",detbest);
  status = FitWithOutlierRemoval(debug);
  if (status) {
    printf("Fit failed for gapf, err = %d\n",status);
    return;
  }
  if (!NO_PLOTS && debug>0) {
    if (conGL ==0) {
      conGL = new TCanvas("conGL","GL fit contours",600,400);
      conGL->Divide(2,2);
    } else conGL->cd();
    DrawErrorContours();
  }

  
  double scXgl = fitPars[1];
  double escXgl = fitParErrs[1];
  double GLO = fitPars[2];
  double eGLO = fitParErrs[2];
  vgl = VAR;
  STDDEV_GL = STDDEV;


  //////////////////////////////////////////
  // Combining SC & GL fits...we have our first guesses of solutions!!!

  double scp = (sce[jmin]-scXgl)/ggl[jmin];
  double escp = TMath::Sqrt(sceE[jmin]*sceE[jmin] +
                            escXgl*escXgl +
                            scp*scp*gglE[jmin]*gglE[jmin]
                            ) / ggl[jmin];

  double sop = sof[jmin];
  double esop = sofE[jmin];

  double glp1 = ((sce[jmin]/scXgl) - 1.0);
  double glp = ggl[jmin]/glp1;
  double glp2 = glp/glp1;
  double eglp = TMath::Sqrt(TMath::Power(glp*gglE[jmin]/ggl[jmin],2) +
                            TMath::Power(glp2*escXgl*sce[jmin]/(scXgl*scXgl),2) +
                            TMath::Power(glp2*sceE[jmin]*scXgl,2));

  if (debug>0) {
    printf("\n*** FIRST PASS CALIBRATION VALUES: ***\n");
    PrintResult(scp, escp, sop, esop, glp, eglp, (DO_PCA ? 0 : detbest));
    printf("USING STDDEV SC = %f :: GL = %f\n",STDDEV_SC,STDDEV_GL);
  }
  
  //////////////////////////////////////////
  // Try again with one unified full fit
  // No further outlier removal (use already-determined outliers)

  // Prepare for SC+GL fit
  fitPars = fitParsSCGL;
  fitParErrs = fitParErrsSCGL;
  parName = parNameSCGL;

  // Set starting values and step sizes for parameters
  int npar = 6;
  TString fname[6] = {"g2","log(g5)","log(SC)","SO","log(GL)","GLO"};
  Double_t fstart[6] = {fitParsGL[0], TMath::Log(fitParsSC[2]), TMath::Log(scp),
    sop, TMath::Log(9.0), GLO};
  Double_t fstep[6]  = {fitParErrsGL[0], fitParErrsSC[2]/fitParsSC[2], escp/scp,
    esop, 0.1, eGLO};
  SetMinuitPars(6,fname,fstart,fstep,debug);
  minuit->SetFCN(fnchSCGapf);
  if (FORCE_GLO_SO) {
    minuit->SetParameter(5, "GLO    ", 0, 0, 0, 0);
    minuit->FixParameter(5);
    npar--;
  }

  // Perform the fit
  printf("\nSpaceCharge & GridLeak fit results (scaler: %s):\n",detbest);
  status = minuit->ExecuteCommand("MIGRAD", arglist ,1);
  if (status) {
    printf("Fit failed for sc+gl, err = %d\n",status);
    return;
  }
  for (k=0;k<npar;k++) {
    for (i=0;i<npar;i++) if (i!=k) minuit->FixParameter(i);
    status = minuit->ExecuteCommand("MIGRAD", arglist ,1);
    if (status) {
      printf("Fit failed for sc+gl, err = %d\n",status);
      return;
    }
    minuit->GetParameter(k,parName[k],fitPars[k],fitParErrs[k],temp1,temp2);
    printf("%s\t:\t%g\t+/- %g\n",parName[k],fitPars[k],fitParErrs[k]);
    for (i=0;i<npar;i++) if (i!=k) minuit->ReleaseParameter(i);
  }
  if (!NO_PLOTS && debug>0) {
    if (conSCGL ==0) {
      conSCGL = new TCanvas("conSCGL","SCGL fit contours",600,900);
      conSCGL->Divide(3,5);
    } else conSCGL->cd();
    DrawErrorContours();
  }
  Log2Lin(1);
  Log2Lin(2);
  Log2Lin(4);
  scp = fitPars[2];
  sop = fitPars[3];
  glp = fitPars[4];
  escp = fitParErrs[2];
  esop = fitParErrs[3];
  eglp = fitParErrs[4];
  if (FORCE_GLO_SO) {
    GLO = sop;
    eGLO = esop;
    fitPars[5] = GLO;
    fitParErrs[5] = eGLO;
  } else {
    GLO = fitPars[5];
    eGLO = fitParErrs[5];
  }
  printf("\n*** FINAL CALIBRATION VALUES: ***\n");
  PrintResult(scp, escp, sop, esop, glp, eglp, (DO_PCA ? 0 : detbest));

  if (NO_PLOTS) return;


  //////////////////////////////////////////
  // Individual datasets compared to their fits
  //   Magenta lines show outlier exclusion zones
  //   gapf is presented as if no correction was applied,
  //   because it would otherwise depend on both the
  //   used luminosity scaler and the estimated best one


  double sc_min,sc_max,gapf_min,gapf_max,Lmin,Lmax;
  SetMinMax(nMeasures,m_sc,sc_min,sc_max,0);
  SetMinMax(nMeasures,m_gapf,gapf_min,gapf_max,0,0.25);
  SetMinMax(nMeasures,m_L,Lmin,Lmax,m_L[0]);
  gapf_max += fitParsSCGL[0]*scp*glp*(Lmax-sop);
  // Offset is minimum of either 0.2*(max-min) or 5*MAX_DEV*VAR, 
  double sc_offset = 0.002*TMath::Max(1,
                                      TMath::Max(TMath::Nint((sc_max-sc_min)*0.2/0.002),
                                                 TMath::Nint(MAX_DEV*vsc[jmin]*5./0.002)));
  sc_max += (nfi-1)*sc_offset;
  double gapf_offset = 0.05*TMath::Max(1,
                                       TMath::Max(TMath::Nint((gapf_max-gapf_min)*0.2/0.05),
                                                  TMath::Nint(MAX_DEV*vgl*5./0.05)));
  gapf_max += (nfi-1)*gapf_offset;

  // Same as funcGapf and funcSC except non-luminosity dimensions become
  //   exra parameters, and we're pushing gapf back to uncorrected values
  TF1* fnGapf = new TF1("fnGapf","[0] * ( ([2]*[4]) * (x - [5]) ) + [8]",0,Lmax);
  TF1* fnSC = new TF1("fnSC","([2]*([1]+[4])) * (x - [3]) / ([6] + [1]) + [8]",0,Lmax);
  fnGapf->SetParameters(fitParsSCGL);
  fnSC->SetParameters(fitParsSCGL);
  fnGapf->SetLineWidth(1);
  fnSC->SetLineWidth(1);
  if (!cGL) cGL = new TCanvas("cGL","GridLeak Fits",30,30,500,500);
  TH2D* htGL = new TH2D("htGL",Form("gapf vs. %s for all sets offset by %4.2f",detbest,gapf_offset),
                        1,Lmin,Lmax,1,gapf_min,gapf_max);
  htGL->Draw();
  if (!cSC) cSC = new TCanvas("cSC","SpaceCharge Fits",60,60,500,500);
  TH2D* htSC = new TH2D("htSC",Form("sc vs. %s for all sets offset by %5.3f",detbest,sc_offset),
                        1,Lmin,Lmax,1,sc_min,sc_max);
  htSC->Draw();
  for (i=0; i<nfi; i++) {
    cGL->cd();
    SCi[i]->Draw(Form("gapf+(%g)*(%g)*((%s)-(%g))+%g:%s",fitParsSCGL[0],usg[i],dets[det[i]].Data(),uso[i],
                      i*gapf_offset,detbest),cut,"same");
    fnGapf->SetParameter(6,usg[i]);
    fnGapf->SetParameter(7,uso[i]);
    fnGapf->SetParameter(8,0+i*gapf_offset);
    fnGapf->SetLineColor(1);
    fnGapf->DrawCopy("same");
    fnGapf->SetParameter(8,MAX_DEV*vgl+i*gapf_offset);
    fnGapf->SetLineColor(6);
    fnGapf->DrawCopy("same");
    fnGapf->SetParameter(8,-MAX_DEV*vgl+i*gapf_offset);
    fnGapf->DrawCopy("same");

    cSC->cd();
    SCi[i]->Draw(Form("sc+%g:%s",i*sc_offset,detbest),cut,"same");
    fnSC->SetParameter(6,ugl[i]);
    fnSC->SetParameter(8,0+i*sc_offset);
    fnSC->SetLineColor(1);
    fnSC->DrawCopy("same");
    fnSC->SetParameter(8,MAX_DEV*vsc[jmin]+i*sc_offset);
    fnSC->SetLineColor(6);
    fnSC->DrawCopy("same");
    fnSC->SetParameter(8,-MAX_DEV*vsc[jmin]+i*sc_offset);
    fnSC->DrawCopy("same");
  }


  //////////////////////////////////////////
  // For zero space charge run, do not do the full fits,
  //  just report back the SpaceCharge quantities to try
  //  for each of the GridLeaks used:

  if (allZeros) {
    printf("\n\n*** Try the following calibration values: ***\n");
    for (i=0; i<nfi; i++) {
      printf("SC = %6.4g * ((%s) - (%6.4g))",sce[jmin],detbest,sof[jmin]);
      printf(" with GL = %5.2f\n\n",ugl[i]);
    }
    return;
  }


  if (cSummary==0) cSummary = new TCanvas("cSummary","Calib SC and GL");
  cSummary->Divide(2,2,0.01,0.025);

  Double_t min,max,ymin,ymax,ymin2,ymax2,dev,dev2;
  Double_t xx[4];
  
  //////////////////////////////////////////
  // Plot for LeakS vs. SC*GL

  TF1* myGL = new TF1("LeakS_of_SCGL","[0]*([1]-x)",0,5e-5);
  myGL->SetParameters(fitParsGL);
  myGL->SetLineWidth(1);
  Double_t asg[nfip]; // approximated usg converted from used lum scaler to best
  memset(asg,0,nfip*sizeof(Double_t));
  memset(nno,0,nfip*sizeof(Double_t));
  memset(devs,0,nMeasures*sizeof(Double_t));
  for (i=0;i<nMeasures;i++) { // parameter sets
    if (!outliersGL[i]) {
      int ifi = m_set[i];
      asg[ifi] += m_L2[i]/m_L[i];
      nno[ifi]++;
    }
  }
  for (i=0;i<nfi;i++) {
    asg[i] *= usg[i]/nno[i];
    glk[i] = myGL->Eval(asg[i]);
  }
  k = 0;
  for (i=0;i<nMeasures;i++) { // parameter sets
    if (outliersGL[i]) continue;
    int ifi = m_set[i];
    xx[0] = m_L[i];
    xx[1] = m_L2[i];
    xx[2] = uso[ifi];
    xx[3] = usg[ifi];
    xgl[k] = usg[ifi]*m_L2[i]/m_L[i]; // more realistic than using asg[ifi]
    // gapf = Leak_I + Leak_S * L
    // Leak_S = (gapf - Leak_I) / L
    // del(Leak_S) = del(gapf)/L
    dev = (m_gapf[i] - funcGapf(xx,fitParsGL))/m_L[i];
    ygl[k] = glk[ifi] + dev;
    devs[ifi] += dev;
    k++;
  }
  for (i=0;i<nfi;i++) {
    glk[i] += devs[i]/nno[i];
  }
  SetMinMax(k,xgl,min,max,scXgl);
  SetMinMax(k,ygl,ymin,ymax,zero);
  myGL->SetRange(min,max);

  cSummary->cd(1);
  myGL->Draw();
  myGL->GetHistogram()->SetTitle("Leak_S vs. SC*GL");
  myGL->GetHistogram()->SetMinimum(ymin);
  myGL->GetHistogram()->SetMaximum(ymax);
  mark2.SetMarkerColor(1);
  mark2.SetMarkerStyle(1);
  mark2.DrawPolyMarker(k,xgl,ygl);
  mark2.SetMarkerColor(4);
  mark2.SetMarkerStyle(27);
  mark2.DrawPolyMarker(nfi,asg,glk);

  ellip.DrawEllipse(scXgl,zero,escXgl,0.004*(ymax-ymin),0,360,0);
  mark.DrawMarker(scXgl,zero);


  //////////////////////////////////////////
  // Plot for LeakI vs. SO*SC*GL*g2/gGL => GLO @ zero crossing
  
  TF1* myGO = new TF1("LeakI_of_SOSCGL","[0]*[1]*(x-[2])",0,5e-6);
  myGO->SetParameters(fitParsGL);
  myGO->SetLineWidth(1);
  memset(devs,0,nMeasures*sizeof(Double_t));
  for (i=0;i<nfi;i++) {
    ugo[i] /= scXgl; // Use 
    glo[i] = myGO->Eval(ugo[i]);
  }
  k = 0;
  for (i=0;i<nMeasures;i++) { // parameter sets
    if (outliersGL[i]) continue;
    int ifi = m_set[i];
    xx[0] = m_L[i];
    xx[1] = m_L2[i];
    xx[2] = uso[ifi];
    xx[3] = usg[ifi];
    xgo[k] = ugo[ifi];
    // gapf = Leak_I + Leak_S * L
    // Leak_I = gapf - (Leak_S / L)
    // del(Leak_I) = del(gapf), (Leak_S / L) cancels out
    dev = m_gapf[i] - funcGapf(xx,fitParsGL);
    ygo[k] = glo[ifi] + dev;
    devs[ifi] += dev;
    k++;
  }
  for (i=0;i<nfi;i++) {
    glo[i] += devs[i]/nno[i];
  }
  SetMinMax(nfi,ugo,min,max,GLO);
  SetMinMax(k,ygo,ymin,ymax,zero);
  myGO->SetRange(min,max);

  cSummary->cd(3);
  myGO->Draw();
  myGO->GetHistogram()->SetTitle("Leak_I vs. GLO");
  myGO->GetHistogram()->SetMinimum(ymin);
  myGO->GetHistogram()->SetMaximum(ymax);
  mark2.SetMarkerColor(1);
  mark2.SetMarkerStyle(1);
  mark2.DrawPolyMarker(nMeasures,xgo,ygo);
  mark2.SetMarkerColor(4);
  mark2.SetMarkerStyle(27);
  mark2.DrawPolyMarker(nfi,ugo,glo);

  ellip.DrawEllipse(GLO,zero,eGLO,0.004*(ymax-ymin),0,360,0);
  mark.DrawMarker(GLO,zero);


  //////////////////////////////////////////
  // Plots for SC and SO vs. GL

  TF1* mySO = new TF1("SO_of_GL","[0]",0,100);
  TF1* mySC = new TF1("SC_of_GL","[1]/(x+[2])",0,100);
  mySO->SetParameters(fitParsSC);
  mySC->SetParameters(fitParsSC);
  mySO->SetLineWidth(1);
  mySC->SetLineWidth(1);
  memset(nno,0,nfip*sizeof(Double_t));
  memset(devs,0,nMeasures*sizeof(Double_t));
  for (i=0;i<nfi;i++) {
    spo[i] = mySO->Eval(ugl[i]);
    spc[i] = mySC->Eval(ugl[i]);
  }
  k = 0;
  for (i=0;i<nMeasures;i++) { // parameter sets
    if (outliersSC[i]) continue;
    int ifi = m_set[i];
    xx[0] = m_L[i];
    xx[1] = ugl[ifi];
    xsc[k] = ugl[ifi];
    // sc = SC*(L-SO)
    // SO = L-sc/SC, , del(SO) = del(sc)/SC    , L cancels
    // SC = sc/(L-SO), del(SC) = del(sc)/(L-SO)
    double dsc = m_sc[i] - funcSC(xx,fitParsSC);
    dev = dsc/spc[ifi];
    dev2 = dsc/(m_L[i]-spo[ifi]);
    yso[k] = spo[ifi] + dev;
    ysc[k] = spc[ifi] + dev2;
    devs[ifi] += dev;
    devs[ifi+nfi] += dev2;
    nno[ifi]++;
    k++;
  }
  j = 0;
  for (i=0;i<nfi;i++) {
    spo[i] += devs[i]/nno[i];
    dev2 = devs[i+nfi]/nno[i];
    spc[i] += dev2;
    if (dev2 > 0) j++;
  }
  i = (nfi>5 ? 1 : 0);
  if (j<=i || j>=nfi-i) {
    // too many spc[] points above or below the curve
    printf("WARNING! Symptoms of non-linearity in chosen luminosity scaler!\n");
    printf("  Suggestion: use ");
    if (DO_PCA) printf("more PCA variables\n\n");
    else printf("PCA method\n\n");
  }
  SetMinMax(nfi,ugl,min,max,glp);
  SetMinMax(k,yso,ymin,ymax,sop);
  SetMinMax(k,ysc,ymin2,ymax2,scp);
  mySO->SetRange(min,max);
  mySC->SetRange(min,max);

  cSummary->cd(2);
  mySC->Draw();
  mySC->GetHistogram()->SetTitle("SC vs. GL");
  mySC->GetHistogram()->SetMinimum(ymin2);
  mySC->GetHistogram()->SetMaximum(ymax2);
  mark2.SetMarkerColor(1);
  mark2.SetMarkerStyle(1);
  mark2.DrawPolyMarker(k,xsc,ysc);
  mark2.SetMarkerColor(4);
  mark2.SetMarkerStyle(27);
  mark2.DrawPolyMarker(nfi,ugl,spc);


  if (!scgl_fit) scgl_fit = new TF1("scgl_fit","[0]/x",-5.,100.);
  scgl_fit->SetParameter(0,scXgl);
  scgl_fit->SetLineColor(4);
  scgl_fit->SetLineWidth(1);
  scgl_fit->DrawCopy("same");
  scgl_fit->SetLineStyle(7);
  scgl_fit->SetParameter(0,scXgl+escXgl);
  scgl_fit->DrawCopy("same");
  scgl_fit->SetParameter(0,scXgl-escXgl);
  scgl_fit->DrawCopy("same");

  ellip.DrawEllipse(glp,scp,eglp,escp,0,360,0);
  mark.DrawMarker(glp,scp);
  
  cSummary->cd(4);
  mySO->Draw();
  mySO->GetHistogram()->SetTitle("SO vs. GL");
  mySO->GetHistogram()->SetMinimum(ymin);
  mySO->GetHistogram()->SetMaximum(ymax);
  mark2.SetMarkerColor(1);
  mark2.SetMarkerStyle(1);
  mark2.DrawPolyMarker(k,xsc,yso);
  mark2.SetMarkerColor(4);
  mark2.SetMarkerStyle(27);
  mark2.DrawPolyMarker(nfi,ugl,spo);

  ellip.DrawEllipse(glp,sop,eglp,esop,0,360,0);
  mark.DrawMarker(glp,sop);

  //////////////////////////////////////////
  // Done


  // Set colors/markers for any further fun
  for (i=0;i<nfi;i++) {
    int color = 60 + (int) (40.0*((float) i)/((float) (nfi-1)));
    SCi[i]->SetMarkerColor(color);
    SCi[i]->SetLineColor(color);
    SCi[i]->SetMarkerStyle(22+i);
  }


}

//////////////////////////////////////////
// Fitting functions
//////////////////////////////////////////

Double_t funcGapf(Double_t* x, Double_t* pars) {
  // x[0] = Luminosity to use
  // x[1] = Luminosity used
  // x[2] = uso
  // x[3] = usg
  return pars[0] * ( pars[1] * (x[0] - pars[2]) - x[3] * (x[1] - x[2]) );
}

Double_t funcSC(Double_t* x, Double_t* pars) {
  // x[0] = Luminosity
  // x[1] = ugl
  return pars[1] * (x[0] - pars[0]) / (x[1] + pars[2]);
}

void fnchGapf(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag) {
  //calculate chisquare
  gin = 0; iflag = 0;
  double chisq = 0;
  Double_t x[4];
  nMeasuresI = 0;
  for (int i=0;i<nMeasures;i++) {
    if (outliers[i]) continue;
    int ifi = m_set[i];
    x[0] = m_L[i];
    x[1] = m_L2[i];
    x[2] = uso[ifi];
    x[3] = usg[ifi];
    devs[i] = m_gapf[i] - funcGapf(x,par);
    chisq += devs[i]*devs[i];
    nMeasuresI++;
  }
  CHISQ = (chisq/(STDDEV*STDDEV))/(nMeasuresI-npar); // chisq/dof
  f = CHISQ;  
}

void fnchSC(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag) {
  //calculate chisquare
  gin = 0; iflag = 0;
  double chisq = 0;
  Double_t x[2];
  Double_t pars[3];
  pars[0] = par[0];
  pars[1] = TMath::Exp(par[1]);
  pars[2] = TMath::Exp(par[2]);
  nMeasuresI = 0;
  for (int i=0;i<nMeasures;i++) {
    if (outliers[i]) continue;
    int ifi = m_set[i];
    x[0] = m_L[i];
    x[1] = ugl[ifi];
    devs[i] = m_sc[i] - funcSC(x,pars); // par
    chisq += devs[i]*devs[i];
    nMeasuresI++;
  }
  CHISQ = (chisq/(STDDEV*STDDEV))/(nMeasuresI-npar); // chisq/dof
  f = CHISQ;
}

void fnchSCGapf(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag) {
  //calculate chisquare
  gin = 0; iflag = 0;
  double chisq = 0;
  Double_t x[4];
  nMeasuresI = 0;
  Double_t parGL[3];
  Double_t parSC[3];
  Double_t SC = TMath::Exp(par[2]);
  Double_t GL = TMath::Exp(par[4]);
  Double_t g5 = TMath::Exp(par[1]);
  parGL[0] = par[0]; // g2
  parGL[1] = SC*GL; // gGL/g2 = SC * GL
  parGL[2] = (FORCE_GLO_SO ? par[3] : par[5]); // GLO
  parSC[0] = par[3]; // SO
  parSC[1] = SC*(g5 + GL); // SCe = SC * (g5 + GL)
  parSC[2] = g5; // g5
  for (int i=0;i<nMeasures;i++) {
    int ifi = m_set[i];
    x[0] = m_L[i];
    x[1] = m_L2[i];
    x[2] = uso[ifi];
    x[3] = usg[ifi];
    if (!outliersGL[i]) {
      devsGL[i] = m_gapf[i] - funcGapf(x,parGL);
      chisq += TMath::Power(devsGL[i]/STDDEV_GL,2);
      nMeasuresI++;
    }

    x[1] = ugl[ifi];
    if (!outliersSC[i]) {
      devsSC[i] = m_sc[i] - funcSC(x,parSC);
      chisq += TMath::Power(devsSC[i]/STDDEV_SC,2);
      nMeasuresI++;
    }
  }
  CHISQ = chisq/(nMeasuresI-npar); // chisq/dof
  f = CHISQ;
}

//////////////////////////////////////////
// Helper functions
//////////////////////////////////////////


void Init(const char* input) {
  if (SCall) return;

  gStyle->SetGridColor(kSpring+8);
  gStyle->SetOptStat(0);
  gStyle->SetPalette(1);

  int i,k;
  char fname[128];
  TString fis[nfip];// file names

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

    usg[i] = usc[i] * ugl[i];
    ugo[i] = usg[i] * uso[i];

    if (fname[0]=='#') i--; // Skip lines beginning with #
  }
  printf("Found %d dataset specifications.\n",nfi);
  
  SCall = new TChain("SC","ChainAll");
  for (i=0;i<nfi;i++) { // parameter sets
    
    // Build TChains from histogram files
    SCi[i] = new TChain("SC",Form("Chain%d : %s",i,fis[i].Data()));
    int added_files = 0;
    if ((added_files = SCi[i]->Add(fis[i].Data())) < 1)
      printf("Warning: no files added from %s\n",fis[i].Data());
    else {
      printf("%d files added from %s\n",added_files,fis[i].Data());
      SCall->Add(fis[i].Data());
    }
    SCi[i]->SetMarkerStyle(7);
    SCi[i]->SetMarkerColor(2);
  }

  memset(conSC,0,nsca*sizeof(TCanvas*));

  // Minimization items
  for (k=0;k<3;k++) parNameSC[k] = new char[16];
  for (k=0;k<3;k++) parNameGL[k] = new char[16];
  for (k=0;k<6;k++) parNameSCGL[k] = new char[16];
  arglist[0] = 5000;
  
}

int Waiting() {
  int tempi;
  printf("Waiting...\n");
  cin >> tempi;
  return tempi;
}

void SetMinMax(int n, Double_t* ar, double& min, double& max,
               double inclusion, double margin) {
  min = 1e10;
  max = -1e10;
  for (int i=0; i<n; i++) {
    if (min > ar[i]) min = ar[i];
    if (max < ar[i]) max = ar[i];
  }
  if (min > inclusion) min = inclusion;
  else if (max < inclusion) max = inclusion;
  double margins = margin*(max-min);
  min -= margins;
  max += margins;
}

void SetMinuitPars(const int n, TString* names, Double_t* starts, Double_t* steps,int debug) {
  minuit = TVirtualFitter::Fitter(0,26);
  minuit2 = ((TFitter*) minuit)->GetMinuit();
  minuit2->SetPrintLevel(debug - 2);
    
  int i, maxlen = 0;
  for (i=0; i<n; i++) if (names[i].Length() > maxlen) maxlen = names[i].Length();
  for (i=0; i<n; i++) {
    names[i].Append(' ',maxlen-names[i].Length());
    minuit->SetParameter(i, names[i].Data(), starts[i], steps[i], 0, 0);
  }
}


void Log2Lin(int i) {
  fitPars[i] = TMath::Exp(fitPars[i]);
  fitParErrs[i] *= fitPars[i];
}

int FitWithOutlierRemoval(int debug) {
  // Iterates the fit, until a stable set of
  //  outliers at > MAX_DEV*VAR are removed
  int i,k,status,nOutliers;
  double temp1,temp2;
  Double_t VAR_sets,VAR_single;
  Double_t nSet[nfip];
  Double_t VAR_singles[nfip];
  int iter = 0;
  int nBytes = nMeasuresMax*sizeof(Bool_t);
  int nBytes2 = nfip*sizeof(Double_t);
  memset(outliers,0,nBytes);
  Bool_t outliers1[nMeasuresMax];
  Bool_t outliers2[nMeasuresMax];
  memset(devs,0,nMeasuresMax*sizeof(Double_t));

  while (1) {
    if (iter>1) memcpy(outliers2,outliers1,nBytes);
    if (iter>0) memcpy(outliers1,outliers,nBytes);
    status = minuit->ExecuteCommand("MIGRAD", arglist ,1);
    if (status) return status;

    // One more call to the fnch to get the deviations correct
    for (k=0;k<3;k++)
      minuit->GetParameter(k,parName[k],fitPars[k],fitParErrs[k],temp1,temp2);
    (*(minuit->GetFCN()))(k,&temp1,temp2,fitPars,status);

    // Calculate apprpriate errors
    VAR = 0;
    VAR_sets = 0;
    VAR_single = 0;
    memset(devs_set,0,nBytes2);
    memset(nSet,0,nBytes2);
    memset(VAR_singles,0,nBytes2);
    for (i=0; i<nMeasures; i++) {
      if (outliers[i]) continue;
      VAR += devs[i]*devs[i];
      int ifi = m_set[i];
      nSet[ifi]++;
      devs_set[ifi] += devs[i];
    }
    for (i=0; i<nfi; i++) {
      devs_set[i] /= nSet[i];
      VAR_sets += devs_set[i]*devs_set[i];
    }
    for (i=0; i<nMeasures; i++) {
      if (outliers[i]) continue;
      int ifi = m_set[i];
      devs[i] -= devs_set[ifi];
      VAR_singles[ifi] += devs[i]*devs[i];
    }
    for (i=0; i<nfi; i++) {
      VAR_singles[i] = TMath::Sqrt(VAR_singles[i] / nSet[i]);
      VAR_single += VAR_singles[i];
    }
    // VAR_single is the average variance of the single sets:
    VAR_single /= ((Double_t) nfi);
    // VAR_set is the vaiance of one set to another
    VAR_sets = TMath::Sqrt(VAR_sets / ((Double_t) nfi-1));
    // VAR is the combined observed variance of all data
    VAR = TMath::Sqrt(VAR/((Double_t) nMeasuresI-1));

    // Use an equivalent variance^2 = (nfi-2) * VAR_single^2 + (<n_per_set> -2) * VAR_sets^2
    double N_sets = nfi - 2.0;
    double N_sing = (((Double_t) nMeasuresI)/((Double_t) nfi)) - 2.0;
    if (N_sets <= 0 || N_sing <=0) {
      printf("WARNING: could not estimate point-to-point errors due to limited samples\n");
    } else {
      STDDEV = TMath::Sqrt(N_sets* VAR_single*VAR_single + N_sing * VAR_sets*VAR_sets);
    }
    if (USE_OLD_STDDEV) STDDEV = VAR;

    // Determine outliers from this pass and compare to previous passes
    nOutliers = 0;
    for (i=0; i<nMeasures; i++) {
      if (TMath::Abs(devs[i]) > MAX_DEV*VAR) { outliers[i] = kTRUE; nOutliers++; }
      else outliers[i] = kFALSE;
    }
    if (iter>0 && memcmp(outliers,outliers1,nBytes) == 0) break;
    if (iter>1 && memcmp(outliers,outliers2,nBytes) == 0) break;
    if (iter > nMeasures/10) {
      printf("\nWARNING: Outlier removal at 10 percent of data points...stopping here.\n");
      break;
    }
    iter++;
  }
  if (debug>0) printf("Finished outlier removal: %d (of %d) data points removed (%d iterations)\n",
                      nOutliers,nMeasures,iter+1);

  // Constrain errors by fixing other parameters
  for (k=0;k<3;k++) {
    for (i=0;i<3;i++) if (i!=k) minuit->FixParameter(i);
    status = minuit->ExecuteCommand("MIGRAD", arglist ,1);
    if (status) return status;
    minuit->GetParameter(k,parName[k],fitPars[k],fitParErrs[k],temp1,temp2);
    printf("%s\t:\t%g\t+/- %g\n",parName[k],fitPars[k],fitParErrs[k]);
    for (i=0;i<3;i++) if (i!=k) minuit->ReleaseParameter(i);
  }

  // One more determination of variance using final fit,
  //   without outlier removal
  memcpy(outliers1,outliers,nBytes);
  memset(outliers,0,nBytes);
  (*(minuit->GetFCN()))(k,&temp1,temp2,fitPars,status);
  VAR_all = 0;
  for (i=0; i<nMeasures; i++) VAR_all += devs[i]*devs[i];
  VAR_all = TMath::Sqrt(VAR_all/((Double_t) nMeasures));
  memcpy(outliers,outliers1,nBytes);

  return status;
}

void DrawErrorContours() {
  int i,j,k,l,status = 0;
  Double_t* gin = 0;
  Double_t x0,y0,chi;
  Int_t indx = 0;
  Double_t lows[128];
  Double_t highs[128];
  Int_t npar = minuit->GetNumberTotalParameters();
  TString pName[32];
  for (k=0;k<npar;k++) {
    lows [k] = fitPars[k]-2.5*fitParErrs[k];
    highs[k] = fitPars[k]+2.5*fitParErrs[k];
    pName[k] = parName[k];
    pName[k].Remove(TString::kTrailing,' ');
  }
  TVirtualPad* origPad = gPad;
  double plmin = (USE_OLD_STDDEV ? 0.75 : 1.0/nfi);
  double plmax = plmin*100.;
  for (i=0;i<npar;i++) {
    x0 = fitPars[i];
    for (j=0;j<i;j++) {
      y0 = fitPars[j];
      TH2D* plot = new TH2D(Form("h%s_%d_%d",origPad->GetName(),i,j),
                               Form("%s vs. %s",pName[j].Data(),pName[i].Data()),
                               40,lows[i],highs[i],40,lows[j],highs[j]);
      for (k=0;k<40;k++) {
        fitPars[i] = lows[i]+(((double) k)+0.5)*(highs[i]-lows[i])/40.;
        for (l=0;l<40;l++) {
          fitPars[j] = lows[j]+(((double) l)+0.5)*(highs[j]-lows[j])/40.;
          (*(minuit->GetFCN()))(npar, gin, chi, fitPars, status);
          plot->SetBinContent(k+1,l+1,chi);
        }
      }
      fitPars[j] = y0;
      origPad->cd(indx+1)->SetLogz();
      plot->SetMinimum(plmin);
      plot->SetMaximum(plmax);
      plot->Draw("zcol");
      indx++;
    }
    fitPars[i] = x0;
  }
}

TString PCA(int Nmax, int debug) {
  printf("Running principal components analysis...\n");

  TString scl[maxp];
  TString scb[maxp];
  Double_t rmsl[maxp];
  long long rmsi[maxp];
  TPrincipal* pp = 0;

  int map[nsca];
  Double_t coef[nsca];
  Double_t x[nsca];
  Double_t* xvals[nsca];
  Double_t p[nsca];
  TString delim=":";
  TString scas = (scastr.Length() ? scastr :
                  "zdcx:zdcw:zdce:bbcx:bbcw:bbce:bbcyb:bbcbb");
  scas += ":sc";
  TObjArray* scaA = scas.Tokenize(delim);
  int i,n,N1 = scaA->GetEntries();
  TH1F rmsp("rmsp","rmsp",500,-0.01,0.01);

  if (pcaN >= 0) for (i=0;i<maxp;i++) if (ppl[i]) delete ppl[i];
  memset(ppl,0,maxp*sizeof(TPrincipal*));

  // Read the file once and store the values in a dummy TPrincipal
  //ppl[0] = SCpca->Principal(scas.Data(),cut);
  ppl[0] = new TPrincipal(N1,"D");
  ppl[0]->SetName("principal0");
  for (n=0; n<N1; n++) {
    xvals[n] = new Double_t[nMeasuresMax];
    if (ITER0) {
      SCi[nfi-1]->Draw(scaA->At(n)->GetName(),cut,"goff");
      nMeasures = SCi[nfi-1]->GetSelectedRows();
      memcpy(xvals[n],SCi[nfi-1]->GetV1(),nMeasures*sizeof(Double_t));
    } else {
      nMeasures = 0;
      for (i=0; i<nfi; i++) {
        SCi[i]->Draw(Form("%s*%g",scaA->At(n)->GetName(),(fitPars[1]+ugl[i])/(fitPars[1]+fitPars[4])),cut,"goff");
        nMeasuresI = SCi[i]->GetSelectedRows();
        memcpy(&(xvals[n][nMeasures]),SCi[i]->GetV1(),nMeasuresI*sizeof(Double_t));
        nMeasures += nMeasuresI;
      }
    }
  }
  int count,Count = nMeasures;
  for (count=0; count<Count;count++) {
    for (n=0; n<N1; n++) x[n] = xvals[n][count];
    ppl[0]->AddRow(x);
  }
  unsigned int combo = (unsigned int) (TMath::Power(2,N1-1));
  if (debug>0) printf("Npoints = %d, Nsca = %d, Possible combinations = %d\n",Count,N1-1,combo-1);


  // Try all combinations
  unsigned int l,m,one = 1;
  l = 0;
  rmsl[l] = 1e28;
  double rmsmin = rmsl[l];
  for (m=1;m<combo;m++) {

    int N = 0;
    for (n=0;n<N1;n++) if ((n==N1-1) || ((m>>n) & one)) { map[N]=n; N++; }
    if (N>Nmax) continue;
    l++;

    pp = new TPrincipal(N,"D");
    pp->SetName(Form("principal%d",l));
    ppl[l] = pp;
    for (count=0;count<Count;count++) {
      for (n=0; n<N; n++) x[n] = xvals[map[n]][count];
      pp->AddRow(x);
    }
    pp->MakePrincipals();
    pp->MakeHistograms(Form("PCA_SC%d",l),"xp");
    TH1F* pca_sc = (TH1F*) (gROOT->FindObject(Form("PCA_SC%d_p%03d",l,N-1)));

    const Double_t* mean = pp->GetMeanValues()->GetMatrixArray();
    const Double_t* evals = pp->GetEigenValues()->GetMatrixArray();
    const Double_t* evecs = pp->GetEigenVectors()->GetMatrixArray();
    const Double_t* cova = pp->GetCovarianceMatrix()->GetMatrixArray();
    for (n=0; n<N-1; n++) coef[n] = -evecs[(n+1)*N - 1]/evecs[N*N-1];

    double trace = 0;
    for (n=0; n<N; n++) trace += cova[n*(N+1)];

    Double_t val_at_means = 0;
    for (n=0;n<N-1;n++) val_at_means += mean[n]*coef[n];
    Double_t zero_offset = val_at_means - mean[N-1];
    Double_t converted = zero_offset / coef[0];

    rmsl[l] = pca_sc->GetRMS();

    if (rmsl[l]<1e-8) {
      printf("TRYING TO FIND MY OWN RMS...%g == ",rmsl[l]);
      rmsp.Reset();
      for (int row=0;row<count;row++) {
        pp->X2P(pp->GetRow(row),p);
        rmsp.Fill(p[N-1]);
      }
      rmsl[l] = rmsp.GetRMS();
      printf("%g\n",rmsl[l]);
    }

    scl[l] = Form("RMS = %g :: ",rmsl[l]);
    scl[l] += Form("EV = %g :: ",evals[N-1]);
    scl[l] += Form("\n  %s = ",scaA->At(N1-1)->GetName());
    scb[l] = Form("(%g*(%s",coef[0],scaA->At(map[0])->GetName());
    if (zero_offset!=0) {
      scb[l] += (converted>0 ? Form("-%g",converted) : Form("+%g",-converted));
    }
    scb[l] += "))";
    for (n=1;n<N-1;n++) scb[l] += Form("+(%g*(%s))",coef[n],scaA->At(map[n])->GetName());
    scl[l] += scb[l];

    if (rmsl[l] > 0 && rmsl[l] < rmsmin) {
      rmsmin = rmsl[l];
      pcaoffset = zero_offset;
      pcaN = N-1;
      for (n=0;n<N-1;n++) {
        pcadets[n] = scaA->At(map[n])->GetName();
        pcacoef[n] = coef[n];
      }
    }
    
  } // m loop
  if (debug>0) printf("Max limit on Nsca = %d, Explored combinations = %d\n",
                      TMath::Min(Nmax,N1)-1,l);
  l++;

  TMath::Sort((long long) l,rmsl,rmsi);

  if (debug>1) {
    for (m=1;m<l;m++) {
      //printf("%d :: %d :: %s\n",l,rmsi[l],scl[rmsi[l]].Data());
      printf("%s\n",scl[rmsi[m]].Data());
    }
  }

  for (n=0; n<N1; n++) delete xvals[n];


  return scb[rmsi[l-1]];
}

void PrintResult(double scp, double escp, double sop, double esop,
                 double glp, double eglp, const char* det) {
  printf("SC = (");
  if (det) {
    printf("%6.4g +/- %6.4g) * ((%s) - (%6.4g +/- %6.4g)",scp,escp,det,sop,esop);
  } else {
    double lsop = (sop + pcaoffset)/pcacoef[0];
    double lesop = esop/pcacoef[0];
    printf("1.0 +/- %6.4g)*(%6.4g*(%s-(%6.4g +/- %6.4g))",escp/scp,pcacoef[0]/scp,
           pcadets[0].Data(),lsop,lesop);
    for (int n=1;n<pcaN;n++) printf("+(%6.4g*(%s))",pcacoef[n]/scp,pcadets[n].Data());
  }
  printf(")\n  with GL = %5.2f +/- %5.2f\n\n",glp,eglp);
}

/////////////////////////////////////////////////////////////////
// $Id: Calib_SC_GL.C,v 2.0 2012/06/13 04:45:02 genevb Exp $
// $Log: Calib_SC_GL.C,v $
// Revision 2.0  2012/06/13 04:45:02  genevb
// Switch to universal fits, add PCA and flexible mixtures of scalers, chisq contout plots
//
// Revision 1.5  2010/01/06 17:56:43  genevb
// Improvements to the fitter
//
// Revision 1.4  2008/04/30 14:54:38  genevb
// Use gapf; get initial guess on linear fit
//
// Revision 1.3  2008/04/07 19:41:33  genevb
// Minor updates to documentation, graphing
//
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
