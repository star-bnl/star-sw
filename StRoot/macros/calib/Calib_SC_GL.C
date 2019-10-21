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
//   5) Plot the fits
//
// Parameters:
//   1) Input specification with info on data to be calibrated
//   2) Any additional cuts to help calibrate (i.e. "run!=6082047")
//   3) A specific scaler identifier (if you know which scaler you want to use)
//      Possible values:
//      -1    : automatically determine which of the predefined scalers is best
//      0..n  : force the calibration to use specific scaler with this id (see
//              'dets' in code below, or StDetectorDbMaker/St_spaceChargeCorC.cxx)
//      -n    : perform principal components analysis with up to 'n' scalers
//      "det" : force the calibration to use a specific scaler, e.g. "zdcx"
//      "det1:det2:..." : perform principal components analysis using a specific
//              set of scalers, e.g. "zdcx:zdcx*zdcx" allows a function of
//              sc = A*(zdcx-offset)+B*(zdcx^2)
//   4) Debug mode
//      0 : default, limited output
//      1 : outlier removal info and chisquare contours
//      2 : detailed fit results
//      3 : more detailed fit results
//   5) If specified, the cuts for including data in the plots versus luminosity;
//        same as parameter 2 if omitted. A looser cut here than in parameter 2
//        allows to see the predictive power of whether the fits describe data
//        outside what was used in the calibration
//
// Details:
//
// The macro now only runs compiled, e.g. execute as Calib_SC_GL.C+(...)
//
// PCA does not determine errors on individual terms, so errors are reported
//   on the overall scale, e.g. (1.0 +/- error)*(...)
//
// Plots show:
//   - small brown points     : single file (ntuple) results from data
//   - violet diamonds        : averages for each dataset
//   - grey lines             : independent fits for SpaceCharge & GridLeak
//   - black lines            : simultaneous SpaceCharge & GridLeak fits
//                              (sometimes obscures the grey lines)
//   - red crosses & ellipses : final values and 1-sigma ellipse
//   - blue lines             : constraint on SCxGL and 1-sigma bands
//   - pink lines             : outlier removal bands
//
// There are three input specification methods. 
//   1) 1 An empty (or 0) input argument will use data files already opened
//      in ROOT, and the used calibrations are read from each data file
//      and used to determine the different input parameter sets:
//        root hists*/*
//        .x Calib_SC_GL.C+
//   2) A '@' at the first character of the input argument will open all files
//      matching a simple wildcard pattern (does not work well with multiple
//      wildcards), and the used calibrations are read from each data file
//      and used to determine the different input parameter sets:
//        root Calib_SC_GL.C+("@hists/*")
//   3) The input specification can be the name of an input file, which should
//      contain lines with the path/name (simple wildcards allowed) of dataset
//      files. Each line beginning with a '@' character will be assumed to be
//      a distinct dataset with common used calibrations:
//        @histsSetA/*
//        @histsSetB/*
//        @histsSetC/*
//      A # at the beginning of a line will cause that dataset to be skipped:
//        #@histsSetC/*
//      If this file is named input.dat, it could be analyzed with:
//        root Calib_SC_GL.C+("input.dat")
//      If you know you want to use only bbce+bbcw:
//        root Calib_SC_GL.C+("input.dat","",4)
//
// Note: Older input file formats are supported for backward compatibility
// with hist files that do not have the used corrections stored in them.
// For these, each line of the file must contain the following info:
//   - dataset file specification
//   - scaler detector used in that dataset (see dets strings below)
//   - SpaceCharge rate used in that dataset
//   - GridLeak multiplier used in that dataset
//   - (optional) SpaceCharge offset used in that dataset (set "!" below)
// Example 3 dataset file which used bbce+bbcw would look like this:
//    histsSetA/* 4 1.7e-8 9.0
//    histsSetB/* 4 1.7e-8 12.0
//    histsSetC/* 4 1.7e-8 15.0
// A ! at the beginning of a file spec will cause the optional SpaceCharge
// offset to be read in on that line:
//    !histsSetC/* 4 1.7e-8 15.0 550
// A # can be used to skip these old format lines as well
//
/////////////////////////////////////////////////////////////////

#if !defined(__CINT__) || defined(__MAKECINT__)
#include "TFile.h"
#include "TKey.h"
#include "TCanvas.h"
#include "TChain.h"
#include "TChainElement.h"
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
#include "TString.h"
#include "TPrincipal.h"
#include "TROOT.h"
#include <Stiostream.h>
#endif

// Switches for users? Needs better arrangement
Int_t EWmode = 0; // -1 east, 0 both, 1 west
Int_t EW_ASYMMETRY = kTRUE;

// Switches and constants for experts
Bool_t USE_OLD_STDDEV = kTRUE; // method for determining STDDEV
Bool_t FORCE_GLO_SO   = kTRUE; // force GLO = SO
Bool_t FORCE_g3_same  = kTRUE;
Bool_t FORCE_g4_same  = kTRUE;
Bool_t FORCE_g5_same  = kTRUE; // if g4_same, then g3_same and g5_same are equivalent
Bool_t NO_PLOTS       = kFALSE;
Bool_t BY_SECTOR      = kFALSE;
double GUESS_g5       = 15.0;
double MAX_DEV        = 4.0;
double REF_CONST1     = 0.318;
double RUN_CORRELATE  = 0.70;  // Assume that RUN_CORRELATE fraction of variance
                               // among results from same run/file is correlated
                               // (lower values may help with failed fits)

// Main routine:
void Calib_SC_GL(const char* input=0, const char* cuts=0, int scaler=-1, int debug=0, const char* gcuts=0);
void Calib_SC_GL(const char* input, const char* cuts, const char* scalerstr, int debug=0, const char* gcuts=0);

// Fitting functions:
Double_t funcGapf(Double_t* x, Double_t* pars);
Double_t funcSC(Double_t* x, Double_t* pars);
Double_t funcSC2(Double_t* x, Double_t* pars);
void fnchGapf(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag);
void fnchSC(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag);
void fnchSCGapf(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag);
void fnchSCGapfSec(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag);

// Helper functions:
int  Init(const char* input);
int  Waiting();
void SetMinMax(int n, Double_t* ar, double& min, double& max, double inclusion, double margin=0.075);
int  SetMinuitPars(const int n, TString* names, Double_t* starts, Double_t* steps, int debug, Bool_t* fixing=0);
void Log2Lin(int i);
int  FitWithOutlierRemoval(int debug);
void DrawErrorContours(int npar, Bool_t* fix=0);
TString PCA(int Nmax=32, int debug=0);
void PrintResult(double scp, double escp, double sop, double esop,
                 double glp, double eglp, double ewp, double eewp, const char* det);
void PrintResult(double scp, double escp, double sop, double esop,
                 double* glp, double* eglp, const char* det);

// Global parameters:
const int nLimit=150;
const int nfip=128;
const int nsca=32;
const int npos=nfip*nsca;
const int nMeasuresMax=nfip*256;
const int nMeasuresHalf=nMeasuresMax/2;
int nMeasures = 0;
int nMeasuresI = 0;
int nfi = 0;

// Global inputs
TCut cut,gcut;
TChain* SCi[nfip];
TChain* SCall = 0;
Bool_t allZeros = kTRUE;
TString scastr;

// Graphics
TCanvas* cSummary = 0;
TCanvas* conSC[nsca];
TCanvas* conGL = 0;
TCanvas* conSCGL = 0;
TCanvas* cSC = 0;
TCanvas* cGL = 0;
TH1* histo = 0;

// Colors
Int_t colorData = kOrange+2;
Int_t colorMiFit = kGray;
Int_t colorFit = kBlack;
Int_t colorConstraint = kBlue;
Int_t colorGrid = kSpring+8;
Int_t colorAggregate = kViolet+1;
Int_t colorOutlier = kMagenta-9;
Int_t colorFinal = kRed;

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
Double_t fitParsSCGL[9];
Double_t fitParErrsSCGL[9];
Double_t fitParsSCGLsec[19];
Double_t fitParErrsSCGLsec[19];
char* parNameSC[3];
char* parNameGL[3];
char* parNameSCGL[9];
char* parNameSCGLsec[19];
Bool_t unfittableSec[12];

// Global fit data and parameters
Double_t m_sc[nMeasuresMax];
Double_t m_scS[12][nMeasuresMax];
Double_t m_sc2[nMeasuresMax];
Double_t m_sc2S[12][nMeasuresMax];
Double_t m_usc[nMeasuresMax];
Double_t m_ugl[nMeasuresMax];
Double_t m_gapf[nMeasuresMax];
Double_t m_gapfS[12][nMeasuresMax];
Double_t m_runs[nMeasuresMax];
Double_t m_L[nMeasuresMax];
Double_t m_c1[nMeasuresMax];
Int_t m_set[nMeasuresMax];
Int_t m_runIdx[nMeasuresMax];
Double_t devsSC[nMeasuresMax];
Double_t devsGL[nMeasuresMax];
Double_t maxvarSC[nMeasuresMax];
Double_t maxvarGL[nMeasuresMax];
Double_t devsSCsec[12][nMeasuresMax];
Double_t devsGLsec[12][nMeasuresMax];
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
int glmode[nfip]; // mode of obtaining used GL
double ugl[nfip]; // GL used


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
const int maxp = 1024;
TPrincipal* ppl[maxp];
Bool_t DO_PCA = kFALSE;
Bool_t ITER0 = kFALSE;



//////////////////////////////////////////
// Main routine
//////////////////////////////////////////

void Calib_SC_GL(const char* input, const char* cuts, const char* scalerstr, int debug, const char* gcuts) {
  scastr = scalerstr;
  if (scastr.Contains(":")) Calib_SC_GL(input,cuts,  -999,debug,gcuts);
  else                      Calib_SC_GL(input,cuts,nsca-1,debug,gcuts);
}

void Calib_SC_GL(const char* input, const char* cuts, int scaler, int debug, const char* gcuts) {

  // Define useful luminosity scaler detectors
  //   copied from StRoot/StDetectorDbMaker/St_spaceChargeCorC.cxx
  TString dets[nsca];
  dets[0] = "vpdx"     ;
  dets[1] = "bbcx"     ;
  dets[2] = "zdcx"     ;
  dets[3] = "zdce+zdcw";
  dets[4] = "bbce+bbcw";
  dets[5] = "zdce"     ;
  dets[6] = "zdcw"     ;
  dets[7] = "bbce"     ;
  dets[8] = "bbcw"     ;
  dets[9] = "bbcyb"    ;
  dets[10]= "bbcbb"    ;
  dets[11]= "vpde"     ;
  dets[12]= "vpdw"     ;
  dets[13]= "zdcxnk"   ;
  dets[14]= "zdcenk"   ;
  dets[15]= "zdcwnk"   ;
  //dets[21] = "zdcc"    ;
  //dets[22] = "bbcc"    ;
  //dets[23] = "bbce+bbcw-4*(bbcyb+bbcbb)"; // Just for fun.
  // reserve dets[nsca-1] for special uses (PCA, manually provided scalers)

  if (BY_SECTOR && EWmode==0) {
    printf("Error: must do east or west only when processing by sector!");
    return;
  }
  if (EW_ASYMMETRY && EWmode!=0) {
    printf("Error: must do both east and west when processing full asymmetry!");
    return;
  }
  if (EW_ASYMMETRY && BY_SECTOR) {
    printf("Error: by sector not yet implemented when processing full asymmetry!");
    return;
  }
  

  if (Init(input)) return;
  
  cut = ((cuts) ? cuts : "");
  gcut = ((gcuts) ? gcuts : cut.GetTitle());

  DO_PCA = (scaler < -1);

  if (DO_PCA) { // Doing a PCA analysis
    if (!ITER0) {
      Bool_t no_plots = NO_PLOTS;
      NO_PLOTS = kTRUE;
      ITER0 = kTRUE;
      printf("\n*** Running PCA iteration 0 ***\n\n");
      // First iteration uses a single input dataset to define PCA
      Calib_SC_GL(input,cuts,scaler,debug,gcuts);
      // Second pass uses all input datasets to define PCA
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

  int i,j,k,status;
  double temp1,temp2,temp3,vsc_min = 1e10;
  int jmin=-1;

  // Asymmetrical calibration mode variables
  TString     EWstr = (EWmode < 0 ? " EAST" : (EWmode > 0 ? " WEST" :   ""));
  TString  scvarstr = (EWmode < 0 ?   "sce" : (EWmode > 0 ?   "scw" : (EW_ASYMMETRY ? "scw" : "sc")));
  TString uscvarstr = (EWmode < 0 ?  "usce" : "usc"); // "usc" is same for west
  const char*  scvar =  scvarstr.Data();
  const char* uscvar = uscvarstr.Data();
  if (EWmode != 0) printf("\nAsymmetrical calibration mode:%s\n\n",EWstr.Data());


  // Data arrays
  double asg[nfip];
  double glk[nfip];
  double ago[nfip];
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
  mark.SetMarkerColor(colorFinal);
  mark.SetMarkerStyle(28);
  TPolyMarker mark2;
  TEllipse ellip;
  ellip.SetLineColor(colorFinal);
  ellip.SetLineStyle(2);
  ellip.SetFillStyle(0);
  double zero = 0;


  // Prepare for SC fit
  devs = devsSC;
  outliers = outliers0;
  fitPars = fitParsSC;
  fitParErrs = fitParErrsSC;
  parName = parNameSC;


  // Set some dimensional variables
  Int_t fittableCnt[12];
  memset(fittableCnt,0,12*sizeof(Int_t));
  int startSec = (EWmode < 0 ? 13 : 1);
  nMeasures = 0;
  for (i=0;i<nfi;i++) { // parameter sets
    TString SCvarstr = (EW_ASYMMETRY ? "usc:usce:scw:sce" : Form("%s:%s:gapf",uscvar,scvar));
    nMeasuresI = SCi[i]->Draw(SCvarstr.Data(),cut,"goff");
    if (EW_ASYMMETRY) {
      // east values offset by nMeasuresHalf
      memcpy(&(m_usc [nMeasures              ]),SCi[i]->GetV1(),nMeasuresI*sizeof(Double_t));
      memcpy(&(m_usc [nMeasures+nMeasuresHalf]),SCi[i]->GetV2(),nMeasuresI*sizeof(Double_t));
      memcpy(&(m_sc  [nMeasures              ]),SCi[i]->GetV3(),nMeasuresI*sizeof(Double_t));
      memcpy(&(m_sc  [nMeasures+nMeasuresHalf]),SCi[i]->GetV4(),nMeasuresI*sizeof(Double_t));
      Bool_t useGapfew = (SCi[i]->Draw("1",cut&&"gapfw!=0||gapfe!=0","goff") > 0);
      SCi[i]->Draw("gapfw:gapfe:gapf",cut,"goff");
      if (useGapfew) {
        // gapfe & gapfw are filled (non-zero)
        memcpy(&(m_gapf[nMeasures              ]),SCi[i]->GetV1(),nMeasuresI*sizeof(Double_t));
        memcpy(&(m_gapf[nMeasures+nMeasuresHalf]),SCi[i]->GetV2(),nMeasuresI*sizeof(Double_t));
      } else {
        printf("Set %3d: no e/w gapf, using global gapf\n",i);
        memcpy(&(m_gapf[nMeasures              ]),SCi[i]->GetV3(),nMeasuresI*sizeof(Double_t));
        memcpy(&(m_gapf[nMeasures+nMeasuresHalf]),SCi[i]->GetV3(),nMeasuresI*sizeof(Double_t));
      }
    } else {
      memcpy(&(m_usc [nMeasures]),SCi[i]->GetV1(),nMeasuresI*sizeof(Double_t));
      memcpy(&(m_sc  [nMeasures]),SCi[i]->GetV2(),nMeasuresI*sizeof(Double_t));
      memcpy(&(m_gapf[nMeasures]),SCi[i]->GetV3(),nMeasuresI*sizeof(Double_t));
    }
    if (BY_SECTOR) {
      for (j=0;j<12;j++) {
        k = startSec + j;
        SCvarstr = Form("sc%d:gapf%d",k,k);
        TCut testFittable = Form("sc%d!=0&&gapf%d!=0",k,k);
        Int_t fittableCntS = SCi[i]->Draw(SCvarstr.Data(),cut&&testFittable,"goff");
        if (fittableCntS > 0) {
          fittableCnt[j] += fittableCntS;
          SCi[i]->Draw(SCvarstr.Data(),cut,"goff");
          memcpy(&(m_scS  [j][nMeasures]),SCi[i]->GetV1(),nMeasuresI*sizeof(Double_t));
          memcpy(&(m_gapfS[j][nMeasures]),SCi[i]->GetV2(),nMeasuresI*sizeof(Double_t));
        } else {
          if (debug>0) printf("Set %3d : Unfittable sector: %d\n",i,k);
          memset(&(m_scS  [j][nMeasures]),0,nMeasuresI*sizeof(Double_t));
          memset(&(m_gapfS[j][nMeasures]),0,nMeasuresI*sizeof(Double_t));
        }
      }
    }
    SCi[i]->Draw("const1:run",cut,"goff");
    memcpy(&(m_c1  [nMeasures]),SCi[i]->GetV1(),nMeasuresI*sizeof(Double_t));
    memcpy(&(m_runs[nMeasures]),SCi[i]->GetV2(),nMeasuresI*sizeof(Double_t));
    // possibly improve to (event+run*1e-6) as a more unique ID
    //   for the case of two files from the same run
    if (glmode[i] == 2) {
      SCi[i]->Draw("ugl",cut,"goff");
      memcpy(&(m_ugl[nMeasures]),SCi[i]->GetV1(),nMeasuresI*sizeof(Double_t));
      ugl[i] = m_ugl[nMeasures];
    } else for (k=0; k<nMeasuresI; k++) m_ugl[k+nMeasures] = ugl[i]; // if not in the ntuple
    for (k=0; k<nMeasuresI; k++) {
      m_set[k+nMeasures] = i;
      m_runIdx[k+nMeasures] = k+nMeasures;
      for (j=0; j<k+nMeasures; j++) { // find identical input runs/files
        if (m_runs[k+nMeasures] == m_runs[j]) { m_runIdx[k+nMeasures] = j; break; }
      }
    }
    nMeasures += nMeasuresI;
  }
  for (i=0;BY_SECTOR && i<12;i++) {
    unfittableSec[i] = (fittableCnt[i] < 8);
    if (unfittableSec[i]) printf("Unfittable sector: %d\n",startSec + i);
  }
  
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
      SCi[i]->Draw(dt,cut,"goff");
      nMeasuresI = SCi[i]->GetSelectedRows();
      memcpy(&(m_L[nMeasures]),SCi[i]->GetV1(),nMeasuresI*sizeof(Double_t));
      nMeasures += nMeasuresI;
    }
    STDDEV = STDDEV_SC;

    // Set starting values and step sizes for parameters
    double sce_init = 25.0; // approximate guess on (g5 + GL)
    if (DO_PCA) {
      if (!ITER0) sce_init = fitParsSCGL[1]+fitParsSCGL[4];
    } else {
      SCall->Draw(Form("%s/(%s)",scvar,dt),cut,"goff");
      histo = SCall->GetHistogram();
      sce_init *= (histo->GetMean());
    }
    if (debug) printf("\nUsing initial value for SCe of %g\n",sce_init);

    TString sname[3] = {"SO","log(SCe)","log(g5)"};
    Double_t sstart[3] = {100., TMath::Log(sce_init), TMath::Log(GUESS_g5)};
    Double_t sstep[3] = {1000., TMath::Abs(0.01*TMath::Log(sce_init)), 0.01};
    if (DO_PCA) { sstart[0] = 1e-3; sstep[0] = 1e-5; }
    SetMinuitPars(3,sname,sstart,sstep,debug);
    minuit->SetFCN(fnchSC); // Keep g3r=g4r=1 to stabilize initial fit

    // Perform the fit
    printf("\nSpaceCharge fit results {scaler: %s}:\n",dt);
    status = FitWithOutlierRemoval(debug);
    if (status==4) {
      if (debug) printf("Fit failed with initial g5=15. Trying 12...\n");
      GUESS_g5 = 12.0;
      sstart[2] = TMath::Log(GUESS_g5);
      SetMinuitPars(3,sname,sstart,sstep,debug);
      status = FitWithOutlierRemoval(debug);
    }
    if (status==4) {
      if (debug) printf("Fit failed with initial g5=12. Trying 20...\n");
      GUESS_g5 = 20.0;
      sstart[2] = TMath::Log(GUESS_g5);
      SetMinuitPars(3,sname,sstart,sstep,debug);
      status = FitWithOutlierRemoval(debug);
    }
    if (status) {
      printf("Fit failed for sc, err = %d\nTrying next scaler (if any)...\n\n",status);
      Waiting();
      continue;
    }
    if (!NO_PLOTS && debug>0) {
      if (conSC[j] ==0) {
        conSC[j] = new TCanvas(Form("conSC_%d",j),Form("SC fit contours for %s",dt),600,400);
        conSC[j]->Divide(2,2);
      } else conSC[j]->cd();
      DrawErrorContours(3);
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
    SCi[i]->Draw(detbest,cut,"goff");
    nMeasuresI = SCi[i]->GetSelectedRows();
    memcpy(&(m_L[nMeasures]),SCi[i]->GetV1(),nMeasuresI*sizeof(Double_t));
    nMeasures += nMeasuresI;
  }

  // Set starting values and step sizes for parameters
  TString gname[3] = {"g2","g1/g2 = SCxGL","GLO"};
  Double_t gstart[3] = {1.0, sce[jmin]*0.4, 0.}; // guess GL/(g5+GL) ~=(10/(15+10)) ~= 0.4
  Double_t gstep[3]  = {0.01, sceE[jmin]*0.4, 100.0};
  if (DO_PCA) {
    gstart[1] = 10.0; // gues GL ~= 10.0
    gstep[1] = 1.0;
  }
  SetMinuitPars(3,gname,gstart,gstep,debug);
  minuit->SetFCN(fnchGapf);

  // Perform the fit
  printf("\nGridLeak fit results {scaler: %s}:\n",detbest);
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
    DrawErrorContours(3);
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
  double ewp = 1.0;
  double eewp = 0.0;

  double glp1 = ((sce[jmin]/scXgl) - 1.0);
  double glp = ggl[jmin]/glp1;
  double glp2 = glp/glp1;
  double eglp = TMath::Sqrt(TMath::Power(glp*gglE[jmin]/ggl[jmin],2) +
                            TMath::Power(glp2*escXgl*sce[jmin]/(scXgl*scXgl),2) +
                            TMath::Power(glp2*sceE[jmin]*scXgl,2));

  if (debug>0) {
    printf("\n*** FIRST PASS CALIBRATION VALUES: ***\n");
    PrintResult(scp, escp, sop, esop, glp, eglp, ewp, eewp, detbest);
    printf("USING STDDEV sc => %f :: gapf => %f\n",STDDEV_SC,STDDEV_GL);
  }
  
  //////////////////////////////////////////
  // Try again with one unified full 3D fit
  // No further outlier removal (use already-determined outliers)

  // Prepare for SC+GL fit
  fitPars = fitParsSCGL;
  fitParErrs = fitParErrsSCGL;
  parName = parNameSCGL;

  // Set starting values and step sizes for parameters
  int npar = 9;
  TString fname[9] = {"g2","log(g5)","log(SC)","SO","log(GL)","GLO","log(g5r)","log(g4r)","log(ewratio)"};
  Double_t fstart[9] = {fitParsGL[0], TMath::Log(fitParsSC[2]), TMath::Log(scp),
    sop, TMath::Log(9.0), GLO, 0, 0, TMath::Log(ewp)};
  Double_t fstep[9]  = {fitParErrsGL[0], fitParErrsSC[2]/fitParsSC[2], escp/scp,
    esop, 0.1, eGLO, 0.001, 0.001, 0.001};
  Bool_t ffix[9] = {kFALSE, kFALSE, kFALSE, kFALSE, kFALSE,
    FORCE_GLO_SO, (FORCE_g3_same || FORCE_g5_same), FORCE_g4_same, !EW_ASYMMETRY};
  SetMinuitPars(9,fname,fstart,fstep,debug,ffix);
  minuit->SetFCN(fnchSCGapf);
  
  // Perform the fit
  printf("\nSpaceCharge & GridLeak fit results {scaler: %s}:\n",detbest);
  status = minuit->ExecuteCommand("MINIMIZE", arglist, 1);
  if (status) {
    printf("Fit failed for sc+gl, err = %d\n",status);
    return;
  }
  // covarAdjust is a fudge factor for how the covariance (which is negative)
  // between pars 2 and 4 modifies the variance on the product of their exponentials
  Double_t covarAdjust = 1;
  if (!(ffix[2] || ffix[4]))
    covarAdjust += (minuit->GetCovarianceMatrixElement(2,4) /
                    (minuit->GetCovarianceMatrixElement(2,2) +
                     minuit->GetCovarianceMatrixElement(4,4)));
  status = minuit->ExecuteCommand("SET LIM", 0, 0); // Remove limits before MINOS
  if (status) {
    printf("SetLimit failed for sc+gl, err = %d\n",status);
    return;
  }
  double eplus,eminus,eparab,gcc;
  for (k=0;k<npar;k++) {
    if (ffix[k]) { fitPars[k] = fstart[k]; fitParErrs[k] = 0; continue; }
    minuit->GetParameter(k,parName[k],fitPars[k],fitParErrs[k],temp1,temp2);
  }
  for (k=0;k<npar;k++) {
    if (ffix[k]) { fitPars[k] = fstart[k]; fitParErrs[k] = 0; continue; }
    for (i=0;i<npar;i++) if (i!=k && !ffix[i]) minuit->FixParameter(i);
    status = minuit->ExecuteCommand("MINIMIZE", arglist, 1);
    if (status) {
      printf("Fit failed for sc+gl, err = %d\n",status);
      return;
    }
    arglist[1] = (double) k+1;
    status = minuit->ExecuteCommand("MINOS", arglist, 2);
    if (status) {
      printf("Fit errors failed for sc+gl, err = %d\n",status);
      return;
    }
    minuit->GetParameter(k,parName[k],fitPars[k],temp1,temp2,temp3);
    minuit2->mnerrs(k,eplus,eminus,eparab,gcc);
    if (eplus!=0.0 && eminus!=0.0) fitParErrs[k] = 0.5*(eplus-eminus);
    else if (eplus!=0.0 || eminus!=0.0) fitParErrs[k] = eparab;
    if (debug>0) printf("%s\t:\t%g\t+%g/%g or +/- %g\n",parName[k],fitPars[k],eplus,eminus,eparab);
    printf("%s\t:\t%g\t+/- %g\n",parName[k],fitPars[k],fitParErrs[k]);
    for (i=0;i<npar;i++) if (i!=k && !ffix[i]) minuit->ReleaseParameter(i);
  }
  if (!NO_PLOTS && debug>0) {
    if (conSCGL ==0) {
      conSCGL = new TCanvas("conSCGL","SCGL fit contours",600,900);
      conSCGL->Divide(3,5);
    } else conSCGL->cd();
    DrawErrorContours(npar,ffix);
  }
  Double_t bstart[19] = {fitPars[0], fitPars[1], fitPars[2], fitPars[3],
    fitPars[4], fitPars[4], fitPars[4], fitPars[4], fitPars[4], fitPars[4],
    fitPars[4], fitPars[4], fitPars[4], fitPars[4], fitPars[4], fitPars[4],
    fitPars[5], fitPars[6], fitPars[7]};
  Double_t bstep[19]  = {fitParErrs[0], fitParErrs[1], fitParErrs[2],
    fitParErrs[3], fitParErrs[4], fitParErrs[4], fitParErrs[4],
    fitParErrs[4], fitParErrs[4], fitParErrs[4], fitParErrs[4],
    fitParErrs[4], fitParErrs[4], fitParErrs[4], fitParErrs[4],
    fitParErrs[4], fitParErrs[5], fitParErrs[6], fitParErrs[7]};
  Log2Lin(1);
  Log2Lin(2);
  Log2Lin(4);
  Log2Lin(6);
  Log2Lin(7);
  Log2Lin(8);
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
  if (FORCE_g4_same) {
    fitPars[7] = 1.0;
    fitParErrs[7] = 0.0;
  }
  if (FORCE_g5_same) {
    fitPars[6] = 1.0;
    fitParErrs[6] = 0.0;
  } else if (FORCE_g3_same) {
    fitPars[6] = 1.0/fitPars[7];
    fitParErrs[6] = fitPars[6]*(fitParErrs[7]/fitPars[7]);
  }
  if (EW_ASYMMETRY) {
    ewp = fitPars[8];
    eewp = fitParErrs[8];
  } else {
    ewp = 1.0;
    eewp = 0.0;
  }
  double scXgl_final = scp*glp;
  double escXgl_final = scXgl_final*TMath::Sqrt(covarAdjust*
                                                (TMath::Power(fitParErrs[4]/fitPars[4],2)+
                                                 TMath::Power(fitParErrs[2]/fitPars[2],2)));
  printf(Form("\n***%s FINAL CALIBRATION VALUES: ***\n",EWstr.Data()));
  PrintResult(scp, escp, sop, esop, glp, eglp, ewp, eewp, detbest);

  
  
  ///////////////////////////////////////////////////////
  // FIT GL BY SECTOR
  //////////////////////////////////////////

  double glpS[12];
  double eglpS[12];
  double scpS, sopS, escpS, esopS, GLOS, eGLOS;
  scpS = 0;
  sopS = 0;
  if (BY_SECTOR) {

    // Try again with one unified full 3D fit
    // No further outlier removal (use already-determined outliers)
    
    // Prepare for SC+GL fit
    fitPars = fitParsSCGLsec;
    fitParErrs = fitParErrsSCGLsec;
    parName = parNameSCGLsec;
    
    // Set starting values and step sizes for parameters
    npar = 19;
    TString bname[19] = {"g2","log(g5)","log(SC)","SO","log(GLa)","log(GLb)",
      "log(GLc)", "log(GLd)","log(GLe)","log(GLf)","log(GLg)","log(GLh)",
      "log(GLi)","log(GLj)","log(GLk)","log(GLl)","GLO","log(g5r)","log(g4r)"};
    Bool_t bfix[19] = {kFALSE, kFALSE, kFALSE, kFALSE, kFALSE, kFALSE, kFALSE,
      kFALSE, kFALSE, kFALSE, kFALSE, kFALSE, kFALSE, kFALSE, kFALSE, kFALSE,
      FORCE_GLO_SO, (FORCE_g3_same || FORCE_g5_same), FORCE_g4_same};
    for (k=0;k<12;k++) bfix[k+4] = unfittableSec[k];
    SetMinuitPars(19,bname,bstart,bstep,debug,bfix);
    minuit->SetFCN(fnchSCGapfSec);
    
    // Perform the fit
    printf("\nSpaceCharge & GridLeak fit by sector results {scaler: %s}:\n",detbest);
    status = minuit->ExecuteCommand("MINIMIZE", arglist, 1);
    if (status) {
      printf("Fit failed for sc+gl by sector, err = %d\n",status);
      return;
    }
    status = minuit->ExecuteCommand("SET LIM", 0, 0); // Remove limits before MINOS
    if (status) {
      printf("SetLimit failed for sc+gl by sector, err = %d\n",status);
      return;
    }
    for (k=0;k<npar;k++) {
      if (bfix[k]) { fitPars[k] = bstart[k]; fitParErrs[k] = 0; continue; }
      minuit->GetParameter(k,parName[k],fitPars[k],fitParErrs[k],temp1,temp2);
    }
    for (k=0;k<npar;k++) {
      if (bfix[k]) { fitPars[k] = bstart[k]; fitParErrs[k] = 0; continue; }
      for (i=0;i<npar;i++) if (i!=k && !bfix[i]) minuit->FixParameter(i);
      status = minuit->ExecuteCommand("MINIMIZE", arglist, 1);
      if (status) {
        printf("Fit failed for sc+gl by sector, err = %d\n",status);
        return;
      }
      arglist[1] = (double) k+1;
      status = minuit->ExecuteCommand("MINOS", arglist, 2);
      if (status) {
        printf("Fit errors failed for sc+gl by sector, err = %d\n",status);
        return;
      }
      minuit->GetParameter(k,parName[k],fitPars[k],temp1,temp2,temp3);
      minuit2->mnerrs(k,eplus,eminus,eparab,gcc);
      if (eplus!=0.0 && eminus!=0.0) fitParErrs[k] = 0.5*(eplus-eminus);
      else if (eplus!=0.0 || eminus!=0.0) fitParErrs[k] = eparab;
      if (debug>0) printf("%s\t:\t%g\t+%g/%g or +/- %g\n",parName[k],fitPars[k],eplus,eminus,eparab);
      printf("%s\t:\t%g\t+/- %g\n",parName[k],fitPars[k],fitParErrs[k]);
      for (i=0;i<npar;i++) if (i!=k && !bfix[i]) minuit->ReleaseParameter(i);
    }
    Log2Lin(1);
    Log2Lin(2);
    Log2Lin(4);
    Log2Lin(5);
    Log2Lin(6);
    Log2Lin(7);
    Log2Lin(8);
    Log2Lin(9);
    Log2Lin(10);
    Log2Lin(11);
    Log2Lin(12);
    Log2Lin(13);
    Log2Lin(14);
    Log2Lin(15);
    Log2Lin(17);
    Log2Lin(18);

    scpS = fitPars[2];
    sopS = fitPars[3];
    for (i=0;i<12;i++) {
      glpS[i]=fitPars[4+i];
      eglpS[i]=fitParErrs[4+i];
    }
    escpS = fitParErrs[2];
    esopS = fitParErrs[3];
    if (FORCE_GLO_SO) {
      GLOS = sop;
      eGLOS = esop;
      fitPars[16] = GLOS;
      fitParErrs[16] = eGLOS;
    } else {
      GLOS = fitPars[16];
      eGLOS = fitParErrs[16];
    }
    if (FORCE_g4_same) {
      fitPars[18] = 1.0;
      fitParErrs[18] = 0.0;
    }
    if (FORCE_g5_same) {
      fitPars[17] = 1.0;
      fitParErrs[17] = 0.0;
    } else if (FORCE_g3_same) {
      fitPars[17] = 1.0/fitPars[18];
      fitParErrs[17] = fitPars[17]*(fitParErrs[18]/fitPars[18]);
    }
    printf(Form("\n***%s FINAL BY-SECTOR CALIBRATION VALUES: ***\n",EWstr.Data()));
    PrintResult(scpS, escpS, sopS, esopS, glpS, eglpS, detbest);
  
  
  
  }
  
  
  /////////////////////////
  
  
  
  
  
  
  
  if (NO_PLOTS) return;


  //////////////////////////////////////////
  // Individual datasets compared to their fits
  //   Magenta lines show outlier exclusion zones
  //   gapf is presented as if no correction was applied,
  //   because it would otherwise depend on both the
  //   used luminosity scaler and the estimated best one

  if (fitParsSCGL[6] == 1.0 && fitParsSCGL[7] == 1.0) {
    memcpy(m_sc2,m_sc,nMeasures*sizeof(Double_t));
    for (k=0; k<12; k++)
      memcpy(m_sc2S[k],m_scS[k],nMeasures*sizeof(Double_t));
  } else {
    // Need to shift data points for display purposes
    //   (cannot shift the fit curves point-by-point)
    for (i=0; i<nMeasures; i++) {
      m_sc2[i] = m_sc[i] - m_usc[i] *
                 (1.0 - (                (               fitParsSCGL[1] + m_ugl[i])/
                         (fitParsSCGL[7]*(fitParsSCGL[6]*fitParsSCGL[1] + m_ugl[i]))));
      if (!BY_SECTOR) continue;
      for (k=0; k<12; k++)
        m_sc2S[k][i] = m_scS[k][i] - m_usc[i] *
                   (1.0 - (                (               fitParsSCGLsec[1] + m_ugl[i])/
                           (fitParsSCGLsec[18]*(fitParsSCGLsec[17]*fitParsSCGLsec[1] + m_ugl[i]))));
    }
  }    
  
  double sc_min[12],sc_max[12],sc_offset[12],gapf_min[12],gapf_max[12],gapf_offset[12],Lmin,Lmax;
  SetMinMax(nMeasures,m_L,Lmin,Lmax,m_L[0]);
  for (k=0; k<(BY_SECTOR?12:1); k++) {
    SetMinMax(nMeasures,(BY_SECTOR?m_sc2S[k]:m_sc2),sc_min[k],sc_max[k],0);
    // Offset is minimum of either 0.2*(max-min) or 5*MAX_DEV*VAR, 
    sc_offset[k] = 0.002*TMath::Max(1,
                                      TMath::Max(TMath::Nint((sc_max[k]-sc_min[k])*0.2/0.002),
                                                 TMath::Nint(MAX_DEV*vsc[jmin]*5./0.002)));
    sc_max[k] += (nfi-1)*sc_offset[k];
    SetMinMax(nMeasures,(BY_SECTOR?m_gapfS[k]:m_gapf),gapf_min[k],gapf_max[k],0,0.25);
    gapf_max[k] += (BY_SECTOR ?
                    fitParsSCGLsec[0]*scpS*glpS[k]*(Lmax-sopS) :
                    fitParsSCGL[0]*scp*glp*(Lmax-sop) );
    gapf_offset[k] = 0.05*TMath::Max(1,
                                      TMath::Max(TMath::Nint((gapf_max[k]-gapf_min[k])*0.2/0.05),
                                                  TMath::Nint(MAX_DEV*vgl*5./0.05)));
    gapf_max[k] += (nfi-1)*gapf_offset[k];
  }

  // Same as funcGapf and funcSC except non-luminosity dimensions become
  //   extra parameters, and we're adjusting gapf back to uncorrected values
  TF1* fnGapf = 0;
  TF1* fnSC = 0;
  if (BY_SECTOR) {
    fnGapf = new TF1("fnGapf","[0] * ( ([2]*[4]) * (x - [16]) ) + [10]",0,Lmax);
    fnSC = new TF1("fnSC","([2]*([1] + [4])) * (x - [3]) / ([18]*([17]*[1] + [8])) + [10]",0,Lmax);
    fnGapf->SetParameters(fitParsSCGLsec);
    fnSC->SetParameters(fitParsSCGLsec);
  } else {
    fnGapf = new TF1("fnGapf","[0] * ( ([2]*[4]) * (x - [5]) ) + [10]",0,Lmax);
    fnSC = new TF1("fnSC","([2]*([1] + [4])) * (x - [3]) / ([7]*([6]*[1] + [8])) + [10]",0,Lmax);
    fnGapf->SetParameters(fitParsSCGL);
    fnSC->SetParameters(fitParsSCGL);
  }
  fnGapf->SetLineWidth(1);
  fnSC->SetLineWidth(1);
  TF1* fiGapf = new TF1("fiGapf","[0] * ( ([1]) * (x - [2]) ) + [10]",0,Lmax);
  TF1* fiSC = new TF1("fiSC","[1] * (x - [0]) / ([2] + [8]) + [10]",0,Lmax);
  fiGapf->SetParameters(fitParsGL);
  fiSC->SetParameters(fitParsSC);
  fiGapf->SetLineWidth(1);
  fiSC->SetLineWidth(1);
  fiGapf->SetLineColor(colorMiFit);
  fiSC->SetLineColor(colorMiFit);
  if (BY_SECTOR) {
    if (!cGL) {
      cGL = new TCanvas("cGL","GridLeak Fits by Sector",30,30,1000,750);
      cGL->Divide(4,3);
    }
    if (!cSC) {
      cSC = new TCanvas("cSC","SpaceCharge Fits",60,60,1000,750);
      cSC->Divide(4,3);
    }
    TH2D* htGLsec[12];
    TH2D* htSCsec[12];
    for (k=0; k<12; k++) {
      if (unfittableSec[k]) continue;
      htGLsec[k] = new TH2D(Form("htGL%d",k),
                            Form("Sector %d: adjusted #font[32]{gapf} vs. %s for all sets, offset by %4.2f",
                                 k+startSec,detbest,gapf_offset[k]),
                            1,Lmin,Lmax,1,gapf_min[k],gapf_max[k]);
      cGL->cd(k+1);
      htGLsec[k]->Draw();
      htSCsec[k] = new TH2D(Form("htSC%d",k),
                            Form("Sector %d: #font[32]{sc} vs. %s for all sets, offset by %5.3f",
                                 k+startSec,detbest,sc_offset[k]),
                            1,Lmin,Lmax,1,sc_min[k],sc_max[k]);
      cSC->cd(k+1);
      htSCsec[k]->Draw();

      fnGapf->SetParameter(4,fitParsSCGLsec[4+k]);
      fnSC->SetParameter(4,fitParsSCGLsec[4+k]);
      for (i=0; i<nfi; i++) {
        cGL->cd(k+1);
        SCi[i]->Draw(Form("gapf%d+%s*(%g)*%s+%g:%s",
                      k+startSec,
                      (glmode[i] == 2 ? "ugl" : Form("(%g)",ugl[i])),
                      fitParsSCGLsec[0],uscvar,i*gapf_offset[k],detbest),
                      gcut,"same");
        // should use fitParsSCGL[0] to match fnGapf and outlier bands,
        //   but fitParsGL[0] to match fiGapf
        fiGapf->SetParameter(10,0+i*gapf_offset[k]);
        fiGapf->DrawCopy("same");
        fnGapf->SetParameter(10,0+i*gapf_offset[k]);
        fnGapf->SetLineColor(colorFit);
        fnGapf->DrawCopy("same");
        fnGapf->SetParameter(10,MAX_DEV*vgl+i*gapf_offset[k]);
        fnGapf->SetLineColor(colorOutlier);
        fnGapf->DrawCopy("same");
        fnGapf->SetParameter(10,-MAX_DEV*vgl+i*gapf_offset[k]);
        fnGapf->DrawCopy("same");

        cSC->cd(k+1);
        if (fitParsSCGL[6] == 1.0 && fitParsSCGL[7] == 1.0) {
          SCi[i]->Draw(Form("%s+%g:%s",scvar,i*sc_offset[k],detbest),gcut,"same");
        } else {
          SCi[i]->Draw(Form("%s-%s*(1-(%g+ugl)/(%g*(%g+ugl)))+%g:%s",scvar,uscvar,
                            fitParsSCGLsec[1],fitParsSCGLsec[18],fitParsSCGLsec[17]*fitParsSCGLsec[1],
                            i*sc_offset[k],detbest),gcut,"same");
        }
        fiSC->SetParameter(8,ugl[i]);
        fiSC->SetParameter(10,0+i*sc_offset[k]);
        fiSC->DrawCopy("same");
        fnSC->SetParameter(8,ugl[i]);
        fnSC->SetParameter(10,0+i*sc_offset[k]);
        fnSC->SetLineColor(colorFit);
        fnSC->DrawCopy("same");
        fnSC->SetParameter(10,MAX_DEV*vsc[jmin]+i*sc_offset[k]);
        fnSC->SetLineColor(colorOutlier);
        fnSC->DrawCopy("same");
        fnSC->SetParameter(10,-MAX_DEV*vsc[jmin]+i*sc_offset[k]);
        fnSC->DrawCopy("same");
      }
    }
  } else {
    if (!cGL) cGL = new TCanvas("cGL","GridLeak Fits",30,30,500,500);
    TH2D* htGL = new TH2D("htGL",
                          Form("adjusted #font[32]{gapf} vs. %s for all sets, offset by %4.2f",
                               detbest,gapf_offset[0]),
                          1,Lmin,Lmax,1,gapf_min[0],gapf_max[0]);
    cGL->cd();
    htGL->Draw();
    if (!cSC) cSC = new TCanvas("cSC","SpaceCharge Fits",60,60,500,500);
    TH2D* htSC = new TH2D("htSC",
                          Form("#font[32]{sc} vs. %s for all sets, offset by %5.3f",
                               detbest,sc_offset[0]),
                          1,Lmin,Lmax,1,sc_min[0],sc_max[0]);
    cSC->cd();
    htSC->Draw();
    for (i=0; i<nfi; i++) {
      cGL->cd();
      SCi[i]->Draw(Form("gapf+%s*(%g)*%s+%g:%s",
                        (glmode[i] == 2 ? "ugl" : Form("(%g)",ugl[i])),
                        fitParsSCGL[0],uscvar,i*gapf_offset[0],detbest),
                        gcut,"same");
      // should use fitParsSCGL[0] to match fnGapf and outlier bands,
      //   but fitParsGL[0] to match fiGapf
      fiGapf->SetParameter(10,0+i*gapf_offset[0]);
      fiGapf->DrawCopy("same");
      fnGapf->SetParameter(10,0+i*gapf_offset[0]);
      fnGapf->SetLineColor(colorFit);
      fnGapf->DrawCopy("same");
      fnGapf->SetParameter(10,MAX_DEV*vgl+i*gapf_offset[0]);
      fnGapf->SetLineColor(colorOutlier);
      fnGapf->DrawCopy("same");
      fnGapf->SetParameter(10,-MAX_DEV*vgl+i*gapf_offset[0]);
      fnGapf->DrawCopy("same");

      cSC->cd();
      if (fitParsSCGL[6] == 1.0 && fitParsSCGL[7] == 1.0) {
        SCi[i]->Draw(Form("%s+%g:%s",scvar,i*sc_offset[0],detbest),gcut,"same");
      } else {
        SCi[i]->Draw(Form("%s-%s*(1-(%g+ugl)/(%g*(%g+ugl)))+%g:%s",scvar,uscvar,
                          fitParsSCGL[1],fitParsSCGL[7],fitParsSCGL[6]*fitParsSCGL[1],
                          i*sc_offset[0],detbest),gcut,"same");
      }
      fiSC->SetParameter(8,ugl[i]);
      fiSC->SetParameter(10,0+i*sc_offset[0]);
      fiSC->DrawCopy("same");
      fnSC->SetParameter(8,ugl[i]);
      fnSC->SetParameter(10,0+i*sc_offset[0]);
      fnSC->SetLineColor(colorFit);
      fnSC->DrawCopy("same");
      fnSC->SetParameter(10,MAX_DEV*vsc[jmin]+i*sc_offset[0]);
      fnSC->SetLineColor(colorOutlier);
      fnSC->DrawCopy("same");
      fnSC->SetParameter(10,-MAX_DEV*vsc[jmin]+i*sc_offset[0]);
      fnSC->DrawCopy("same");
    }
  }



  //////////////////////////////////////////
  // For zero space charge run, do not do the full fits,
  //  just report back the SpaceCharge quantities to try
  //  for each of the GridLeaks used:

  if (allZeros) {
    printf("\n\n*** The following calibration values may not be trusted at this time... ***");
    printf("\n\n*** Try the following calibration values: ***\n");
    for (i=0; i<nfi; i++) {
      printf("sc = %6.4g * ((%s) - (%6.4g))",sce[jmin],detbest,sof[jmin]);
      printf(" with GL = %5.2f\n\n",ugl[i]); // glmode[i]==2 ?
    }
    return;
  }


  if (cSummary==0) cSummary = new TCanvas("cSummary","Calib SC and GL");
  cSummary->Divide(2,2,0.01,0.025);

  Double_t min,max,ymin,ymax,ymin2,ymax2;
  Double_t xx[2];
  
  //////////////////////////////////////////
  // Plot for gapf/(L-GLO) vs. usc*GLu/(L-GLO) (~SC*GL) => g1/g2 @ zero crossing

  TF1* miGL = new TF1("igapfL_of_SCGL","[0]*([1]-x)",0,5e-5);
  miGL->SetParameters(fitParsGL);
  miGL->SetLineWidth(1);
  miGL->SetLineColor(colorMiFit);
  TF1* myGL = new TF1("gapfL_of_SCGL","[0]*([2]*[4]-x)",0,5e-5);
  myGL->SetParameters(fitParsSCGL);
  myGL->SetLineWidth(1);
  myGL->SetLineColor(colorFit);
  // average used SC*GL converted from used lum scaler to best
  memset(asg,0,nfip*sizeof(Double_t));
  memset(glk,0,nfip*sizeof(Double_t));
  memset(nno,0,nfip*sizeof(Double_t));
  k = 0;
  for (i=0;i<nMeasures;i++) { // parameter sets
    if (outliersGL[i]) continue;
    int ifi = m_set[i];
    // gapf = g5*(L-GLO) - g2*usc*GLu = g2*(SC*GL*(L-GLO) - usc*GLu)
    // SC*GL = usc*GLu/(L-GLO) @ gapf = 0
    xgl[k] = m_usc[i]*m_ugl[i]/(m_L[i]-fitParsSCGL[5]);
    ygl[k] = m_gapf[i]/(m_L[i]-fitParsSCGL[5]);
    asg[ifi] += xgl[k];
    glk[ifi] += ygl[k];
    nno[ifi]++;
    k++;
  }
  for (i=0;i<nfi;i++) {
    asg[i] /= nno[i];
    glk[i] /= nno[i];
  }
  SetMinMax(k,xgl,min,max,scXgl_final);
  SetMinMax(k,ygl,ymin,ymax,zero);
  miGL->SetRange(min,max);
  myGL->SetRange(min,max);

  cSummary->cd(1);
  myGL->Draw();
  histo = myGL->GetHistogram();
  histo->SetTitle("#font[32]{gapf}/#font[32]{L} vs. SC#timesGL");
  histo->SetYTitle("#font[32]{gapf} / (#font[32]{L}_{best} - GLO)");
  histo->SetXTitle("#font[32]{sc}_{used} GL_{used} / (#font[32]{L}_{best} - GLO)");
  histo->GetYaxis()->SetTitleOffset(1.25);
  histo->SetMinimum(ymin);
  histo->SetMaximum(ymax);
  miGL->Draw("same");
  myGL->Draw("same");
  mark2.SetMarkerColor(colorData);
  mark2.SetMarkerStyle(1);
  mark2.DrawPolyMarker(k,xgl,ygl);
  mark2.SetMarkerColor(colorAggregate);
  mark2.SetMarkerStyle(27);
  mark2.DrawPolyMarker(nfi,asg,glk);

  ellip.DrawEllipse(scXgl_final,zero,escXgl_final,0.004*(ymax-ymin),0,360,0);
  mark.DrawMarker(scXgl_final,zero);


  //////////////////////////////////////////
  // Plot for gapf vs. L - usc*GL*g2/g1 => GLO @ zero crossing
  
  TF1* miGO = new TF1("igapf_of_GLO","[0]*[1]*(x-[2])",0,5e-6);
  miGO->SetParameters(fitParsGL);
  miGO->SetLineWidth(1);
  miGO->SetLineColor(colorMiFit);
  TF1* myGO = new TF1("gapf_of_GLO","[0]*[2]*[4]*(x-[5])",0,5e-6);
  myGO->SetParameters(fitParsSCGL);
  myGO->SetLineWidth(1);
  myGO->SetLineColor(colorFit);
  // average used GLO converted from used lum scaler to best
  memset(ago,0,nfip*sizeof(Double_t));
  memset(glo,0,nfip*sizeof(Double_t));
  k = 0;
  for (i=0;i<nMeasures;i++) { // parameter sets
    if (outliersGL[i]) continue;
    int ifi = m_set[i];
    // GLO = L - (usc*GLu)/(SC*GL) @ gapf = 0
    xgo[k] = m_L[i] - (m_usc[i]*m_ugl[i])/(fitParsSCGL[2]*fitParsSCGL[4]);
    ygo[k] = m_gapf[i];
    ago[ifi] += xgo[k];
    glo[ifi] += ygo[k];
    k++;
  }
  for (i=0;i<nfi;i++) {
    ago[i] /= nno[i];
    glo[i] /= nno[i];
  }
  SetMinMax(nfi,ago,min,max,GLO);
  SetMinMax(k,ygo,ymin,ymax,zero);
  miGO->SetRange(min,max);
  myGO->SetRange(min,max);

  cSummary->cd(3);
  myGO->Draw();
  histo = myGO->GetHistogram();
  histo->SetTitle(Form("#font[32]{gapf} vs. GLO%s",(FORCE_GLO_SO ? " #equiv SO" : "")));
  histo->SetYTitle("#font[32]{gapf}");
  histo->SetXTitle("#font[32]{L}_{best} - (#font[32]{sc}_{used} GL_{used} g_{2} / g_{GL})");
  histo->GetYaxis()->SetTitleOffset(1.25);
  histo->SetMinimum(ymin);
  histo->SetMaximum(ymax);
  miGO->Draw("same");
  myGO->Draw("same");
  mark2.SetMarkerColor(colorData);
  mark2.SetMarkerStyle(1);
  mark2.DrawPolyMarker(nMeasures,xgo,ygo);
  mark2.SetMarkerColor(colorAggregate);
  mark2.SetMarkerStyle(27);
  mark2.DrawPolyMarker(nfi,ago,glo);

  ellip.DrawEllipse(GLO,zero,eGLO,0.004*(ymax-ymin),0,360,0);
  mark.DrawMarker(GLO,zero);


  //////////////////////////////////////////
  // Plots for SC and SO vs. GL

  TF1* miSO = new TF1("iSO_of_GL","[0]",0,100);
  TF1* miSC = new TF1("iSC_of_GL","[1]/([2]+[0])",0,100);
  miSO->SetParameters(fitParsSC);
  miSC->SetParameters(fitParsSC);
  miSC->SetParameter(0,fitParsSCGL[4]);
  miSO->SetLineWidth(1);
  miSC->SetLineWidth(1);
  miSO->SetLineColor(colorMiFit);
  miSC->SetLineColor(colorMiFit);
  TF1* mySO = new TF1("SO_of_GL","[3]",0,100);
  TF1* mySC = new TF1("SC_of_GL","[2]",0,100);
  mySO->SetParameters(fitParsSCGL);
  mySC->SetParameters(fitParsSCGL);
  mySO->SetLineWidth(1);
  mySC->SetLineWidth(1);
  mySO->SetLineColor(colorFit);
  mySC->SetLineColor(colorFit);
  memset(nno,0,nfip*sizeof(Double_t));
  memset(spo,0,nfip*sizeof(Double_t));
  memset(spc,0,nfip*sizeof(Double_t));
  k = 0;
  for (i=0;i<nMeasures;i++) { // parameter sets
    if (outliersSC[i]) continue;
    int ifi = m_set[i];
    xx[0] = m_L[i];
    xx[1] = m_ugl[i];
    xsc[k] = m_ugl[i];
    // Basically, sc = SC*(L-SO), so...
    // SO = L-sc/SC 
    // SC = sc/(L-SO)
    // More explicitly,
    // sc_real = [(sc_obs - sc_used) * g4r * (g5r * g5 + ugl) + sc_used * (g5 + ugl)] / (g5 + GL)
    temp1 = ((m_sc[i] - m_usc[i]) * fitParsSCGL[7] * (fitParsSCGL[6] * fitParsSCGL[1] + m_ugl[i]) +
                        m_usc[i]  *                  (                 fitParsSCGL[1] + m_ugl[i])) /
                                                     (                 fitParsSCGL[1] + fitParsSCGL[4]);
    yso[k] = m_L[i] - (temp1 / fitParsSCGL[2]);
    ysc[k] = temp1 / (m_L[i] - fitParsSCGL[3]);
    spo[ifi] += yso[k];
    spc[ifi] += ysc[k];
    nno[ifi]++;
    k++;
  }
  j = 0;
  for (i=0;i<nfi;i++) {
    spo[i] /= nno[i];
    spc[i] /= nno[i];
    if (spc[i] - miSC->Eval(ugl[i]) > 0) j++;
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
  miSO->SetRange(min,max);
  miSC->SetRange(min,max);
  mySO->SetRange(min,max);
  mySC->SetRange(min,max);

  cSummary->cd(2);
  mySC->Draw();
  histo = mySC->GetHistogram();
  histo->SetTitle("SC vs. GL");
  histo->SetYTitle("[#font[32]{sc} / (#font[32]{L}_{best} - SO)] (g_{5} + GL_{used}) / (g_{5} + GL)");
  histo->SetXTitle("GL_{used}");
  histo->GetYaxis()->SetTitleOffset(1.25);
  histo->SetMinimum(ymin2);
  histo->SetMaximum(ymax2);
  miSC->Draw("same");
  mySC->Draw("same");
  mark2.SetMarkerColor(colorData);
  mark2.SetMarkerStyle(1);
  mark2.DrawPolyMarker(k,xsc,ysc);
  mark2.SetMarkerColor(colorAggregate);
  mark2.SetMarkerStyle(27);
  mark2.DrawPolyMarker(nfi,ugl,spc);


  if (!scgl_fit) scgl_fit = new TF1("scgl_fit","[0]/x",-5.,100.);
  scgl_fit->SetParameter(0,scXgl_final);
  scgl_fit->SetLineColor(colorConstraint);
  scgl_fit->SetLineWidth(1);
  scgl_fit->DrawCopy("same");
  scgl_fit->SetLineStyle(7);
  scgl_fit->SetParameter(0,scXgl_final+escXgl_final);
  scgl_fit->DrawCopy("same");
  scgl_fit->SetParameter(0,scXgl_final-escXgl_final);
  scgl_fit->DrawCopy("same");

  ellip.DrawEllipse(glp,scp,eglp,escp,0,360,0);
  mark.DrawMarker(glp,scp);
  
  cSummary->cd(4);
  mySO->Draw();
  histo = mySO->GetHistogram();
  histo->SetTitle("SO vs. GL");
  histo->SetYTitle("#font[32]{L}_{best} - [#font[32]{sc} (g_{5} + GL_{used}) / (g_{5} + GL)]");
  histo->SetXTitle("GL_{used}");
  histo->GetYaxis()->SetTitleOffset(1.25);
  histo->SetMinimum(ymin);
  histo->SetMaximum(ymax);
  miSO->Draw("same");
  mySO->Draw("same");
  mark2.SetMarkerColor(colorData);
  mark2.SetMarkerStyle(1);
  mark2.DrawPolyMarker(k,xsc,yso);
  mark2.SetMarkerColor(colorAggregate);
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
  // x[1] = usc*ugl
  // x[2] = const1
  // pars[0] = g2
  // pars[1] = g1 / g2 = SC * GL
  // pars[2] = GLO
  return (x[2] / REF_CONST1) * pars[0] * ( pars[1] * (x[0] - pars[2]) - x[1] );
}

Double_t funcSC(Double_t* x, Double_t* pars) {
  // x[0] = Luminosity
  // x[1] = ugl
  // pars[0] = SO
  // pars[1] = SCe = SC * (g5 + GL)
  // pars[2] = g5
  return pars[1] * (x[0] - pars[0]) / (x[1] + pars[2]);
}

Double_t funcSC2(Double_t* x, Double_t* pars) {
  // x[0] = Luminosity
  // x[1] = ugl
  // x[2] = usc
  // pars[0] = SO
  // pars[1] = SCe = SC * (g5 + GL)
  // pars[2] = g5
  // pars[3] = g5r = g5' / g5
  // pars[4] = g4r = g4' / g4
  return x[2] +
    (pars[1] * (x[0] - pars[0]) - (pars[2] + x[1]) * x[2]) /
    (pars[4] * (pars[3] * pars[2] + x[1]));
}

void fnchGapf(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag) {
  //calculate chisquare
  gin = 0; iflag = 0;
  double chisq = 0;
  Double_t x[3];
  nMeasuresI = 0;
  for (int i=0;i<nMeasures;i++) {
    if (outliers[i]) continue;
    if (m_gapf[i] == 0) { outliers[i]=kTRUE; devs[i] = 0; continue; }
    x[0] = m_L[i];
    x[1] = m_usc[i] * m_ugl[i];
    x[2] = m_c1[i];
    devs[i] = m_gapf[i] - funcGapf(x,par);
    maxvarGL[i] = devs[i]*devs[i];
    int k = m_runIdx[i];
    if (k!=i) {
      if (maxvarGL[i]>maxvarGL[k]) {
        for (int j=k;j<i;j++) {
          if (m_runIdx[j]==k) maxvarGL[j] = maxvarGL[i];
        }
      } else maxvarGL[i] = maxvarGL[k];
    }
  }
  for (int i=0;i<nMeasures;i++) {
    if (outliers[i]) continue;
    chisq += RUN_CORRELATE*maxvarGL[i] + (1.0-RUN_CORRELATE)*devs[i]*devs[i];
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
    if (m_sc[i] == 0) { outliers[i]=kTRUE; devs[i] = 0; continue; }
    x[0] = m_L[i];
    x[1] = m_ugl[i];
    devs[i] = m_sc[i] - funcSC(x,pars);
    maxvarSC[i] = devs[i]*devs[i];
    int k = m_runIdx[i];
    if (k!=i) {
      if (maxvarSC[i]>maxvarSC[k]) {
        for (int j=k;j<i;j++) {
          if (m_runIdx[j]==k) maxvarSC[j] = maxvarSC[i];
        }
      } else maxvarSC[i] = maxvarSC[k];
    }
  }
  for (int i=0;i<nMeasures;i++) {
    if (outliers[i]) continue;
    chisq += RUN_CORRELATE*maxvarSC[i] + (1.0-RUN_CORRELATE)*devs[i]*devs[i];
    nMeasuresI++;
  }
  CHISQ = (chisq/(STDDEV*STDDEV))/(nMeasuresI-npar); // chisq/dof
  f = CHISQ;
}

void fnchSCGapf(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag) {
  //calculate chisquare
  gin = 0; iflag = 0;
  double chisqGL = 0;
  double chisqSC = 0;
  Double_t x[3];
  nMeasuresI = 0;
  Double_t parGL[3];
  Double_t parSC[5];
  Double_t SC = TMath::Exp(par[2]);
  Double_t GL = TMath::Exp(par[4]);
  Double_t g5 = TMath::Exp(par[1]);
  Double_t g5r= TMath::Exp(par[6]); // g5r = g5' / g5
  Double_t g4r= TMath::Exp(par[7]); // g4r = g4' / g4
  Double_t ewr= TMath::Exp(par[8]);
  parGL[0] = par[0]; // g2
  parGL[1] = SC*GL; // g1 / g2 = SC * GL
  parGL[2] = (FORCE_GLO_SO ? par[3] : par[5]); // GLO
  parSC[0] = par[3]; // SO
  parSC[1] = SC*(g5 + GL); // SCe = SC * (g5 + GL)
  parSC[2] = g5; // g5
  parSC[3] = (FORCE_g5_same ? 1. : (FORCE_g3_same ? 1./g4r : g5r));
  parSC[4] = (FORCE_g4_same ? 1. : g4r);
  for (int l=0;l<(EW_ASYMMETRY ? 2 : 1);l++) {
    if (l) { // east-side
      parGL[1] *= ewr;
      parSC[1] *= ewr;
    }

    for (int i=0;i<nMeasures;i++) {
      int m = i + l*nMeasuresHalf;
      x[0] = m_L[i];
      x[1] = m_usc[m] * m_ugl[i];
      x[2] = m_c1[i];
      if (!outliersGL[i]) {
        devsGL[i] = m_gapf[m] - funcGapf(x,parGL);
        maxvarGL[i] = devsGL[i]*devsGL[i];
        int k = m_runIdx[i];
        if (k!=i) {
          if (maxvarGL[i]>maxvarGL[k]) {
            for (int j=k;j<i;j++) {
              if (m_runIdx[j]==k) maxvarGL[j] = maxvarGL[i];
            }
          } else maxvarGL[i] = maxvarGL[k];
        }
      }
      
      x[1] = m_ugl[i];
      x[2] = m_usc[m];
      if (!outliersSC[i]) {
        devsSC[i] = m_sc[m] - funcSC2(x,parSC);
        maxvarSC[i] = devsSC[i]*devsSC[i];
        int k = m_runIdx[i];
        if (k!=i) {
          if (maxvarSC[i]>maxvarSC[k]) {
            for (int j=k;j<i;j++) {
              if (m_runIdx[j]==k) maxvarSC[j] = maxvarSC[i];
            }
          } else maxvarSC[i] = maxvarSC[k];
        }
      }
    }
    for (int i=0;i<nMeasures;i++) {
      if (!outliersGL[i]) {
        chisqGL += RUN_CORRELATE*maxvarGL[i] + (1.0-RUN_CORRELATE)*devsGL[i]*devsGL[i];
        nMeasuresI++;
      }
      if (!outliersSC[i]) {
        chisqSC += RUN_CORRELATE*maxvarSC[i] + (1.0-RUN_CORRELATE)*devsSC[i]*devsSC[i];
        nMeasuresI++;
      }
    }
  }
  CHISQ = ((chisqGL/(STDDEV_GL*STDDEV_GL))+(chisqSC/(STDDEV_SC*STDDEV_SC)))/(nMeasuresI-npar); // chisq/dof
  f = CHISQ;
}

void fnchSCGapfSec(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag) {
  //calculate chisquare
  gin = 0; iflag = 0;
  double chisqGL = 0;
  double chisqSC = 0;
  Double_t x[3];
  nMeasuresI = 0;
  Double_t parGL[3];
  Double_t parSC[5];
  Double_t SC = TMath::Exp(par[2]);
  Double_t g5 = TMath::Exp(par[1]);
  Double_t g5r= TMath::Exp(par[17]); // g5r = g5' / g5
  Double_t g4r= TMath::Exp(par[18]); // g4r = g4' / g4
  parGL[0] = par[0]; // g2
  parGL[2] = (FORCE_GLO_SO ? par[3] : par[16]); // GLO
  parSC[0] = par[3]; // SO
  parSC[2] = g5; // g5
  parSC[3] = (FORCE_g5_same ? 1. : (FORCE_g3_same ? 1./g4r : g5r));
  parSC[4] = (FORCE_g4_same ? 1. : g4r);
  for (int sec = 0; sec<12; sec++) {
    if (unfittableSec[sec]) continue;
    
    Double_t GL = TMath::Exp(par[4+sec]);
    parGL[1] = SC*GL; // g1 / g2 = SC * GL
    parSC[1] = SC*(g5 + GL); // SCe = SC * (g5 + GL)

    for (int i=0;i<nMeasures;i++) {
      x[0] = m_L[i];
      x[1] = m_usc[i] * m_ugl[i];
      x[2] = m_c1[i];
      if (!outliersGL[i] && m_gapfS[sec][i]!=0.0) {
        devsGLsec[sec][i] = m_gapfS[sec][i] - funcGapf(x,parGL);
        maxvarGL[i] = devsGLsec[sec][i]*devsGLsec[sec][i];
        int k = m_runIdx[i];
        if (k!=i) {
          if (maxvarGL[i]>maxvarGL[k]) {
            for (int j=k;j<i;j++) {
              if (m_runIdx[j]==k) maxvarGL[j] = maxvarGL[i];
            }
          } else maxvarGL[i] = maxvarGL[k];
        }
      }
      
      x[1] = m_ugl[i];
      x[2] = m_usc[i];
      if (!outliersSC[i] && m_scS[sec][i]!=0.0) {
        devsSCsec[sec][i] = m_scS[sec][i] - funcSC2(x,parSC);
        maxvarSC[i] = devsSCsec[sec][i]*devsSCsec[sec][i];
        int k = m_runIdx[i];
        if (k!=i) {
          if (maxvarSC[i]>maxvarSC[k]) {
            for (int j=k;j<i;j++) {
              if (m_runIdx[j]==k) maxvarSC[j] = maxvarSC[i];
            }
          } else maxvarSC[i] = maxvarSC[k];
        }
      }
    }
    for (int i=0;i<nMeasures;i++) {
      if (!outliersGL[i] && m_gapfS[sec][i]!=0.0) {
        chisqGL += RUN_CORRELATE*maxvarGL[i] + (1.0-RUN_CORRELATE)*devsGLsec[sec][i]*devsGLsec[sec][i];
        nMeasuresI++;
      }
      if (!outliersSC[i] && m_scS[sec][i]!=0.0) {
        chisqSC += RUN_CORRELATE*maxvarSC[i] + (1.0-RUN_CORRELATE)*devsSCsec[sec][i]*devsSCsec[sec][i];
        nMeasuresI++;
      }
    }
  }
  CHISQ = ((chisqGL/(STDDEV_GL*STDDEV_GL))+(chisqSC/(STDDEV_SC*STDDEV_SC)))/(nMeasuresI-npar); // chisq/dof
  f = CHISQ;
}

//////////////////////////////////////////
// Helper functions
//////////////////////////////////////////


int Init(const char* input) {
  if (SCall) return 0;

  gStyle->SetGridColor(colorGrid);
  gStyle->SetOptStat(0);
  gStyle->SetPalette(1);

  int i,k;
  char fname[256];
  double temp;
  TString fis[nfip];// file names
  SCall = new TChain("SC","ChainAll");

  if (!input || input[0] == 0 || input[0] == '@') {
    
    nfi = 0;
    int nfii[nfip];
    memset(nfii,0,nfip*sizeof(int));
    TSeqCollection* listOfFiles = 0;
    TObjArray SCGLcombos;
    TFile* fileI = 0;
    if (!input) { // Use already opened files
      listOfFiles = gROOT->GetListOfFiles();
    } else { // Unified file specification
      fis[0] = input;
      fis[0].Remove(0,1);
      SCall->Add(fis[0].Data());
      listOfFiles = SCall->GetListOfFiles();
    }
    Int_t nfiles = (listOfFiles ? listOfFiles->GetEntries() : 0);
    if (nfiles == 0) {
      printf("Error: no input specified!\n");
      return 1;
    }
    for (i=nfiles-1;i>=0;i--) {
      if (!input) {
        fileI = (TFile*) (listOfFiles->At(i));
        SCall->Add(fileI->GetName());
      } else {
        TChainElement* elem = (TChainElement*) (listOfFiles->At(i));
        if (elem->GetEntries() == 0) continue;
        fileI = new TFile(elem->GetTitle());
      }
      TKey* corrKey = fileI->GetKey("SCcorrection");
      if (!corrKey) {
        printf("File is missing its corrections (excluding from grouping): %s\n",fileI->GetName());
        continue;
      } else {
        TString corrStr = corrKey->ReadObj()->GetTitle();
        corrKey = fileI->GetFile()->GetKey("GLcorrection");
        (corrStr += ":") += corrKey->ReadObj()->GetTitle();
        corrKey = fileI->GetFile()->GetKey("SCEWRatio");
        if (corrKey)
         (corrStr += ":") += corrKey->ReadObj()->GetTitle();
        TObject* existing = SCGLcombos.FindObject(corrStr.Data());
        if (existing) {
          k = SCGLcombos.IndexOf(existing);
          int kent = SCi[k]->GetEntries();
          SCi[k]->Add(fileI->GetName());
          if (SCi[k]->GetEntries() == kent)
            printf("Skipping file with 0 ntuple entries: %s\n",fileI->GetName());
          else nfii[k]++;
        } else {
          SCi[nfi] = new TChain("SC",Form("Chain%d : %s",nfi,corrStr.Data()));
          SCi[nfi]->Add(fileI->GetName());
          if (SCi[nfi]->GetEntries()>0) {
            SCGLcombos.AddLast(new TNamed(corrStr.Data(),corrStr.Data()));
            nfii[nfi] = 1;
            nfi++;
            if (nfi>nfip) {
              printf("Warning! Exceeding maximum number of sets! Excluding the rest from grouping.\n");
              break;
            }
          } else {
            delete SCi[nfi];
            printf("Skipping file with 0 ntuple entries: %s\n",fileI->GetName());
          }
        }
      }
      fileI->Close();
    }
    for (i=0;i<nfi;i++) {
      printf("Set %3d: %d files added with...\n",i,nfii[i]);
      TString corrStr = SCGLcombos.At(i)->GetName();
      corrStr.Remove(corrStr.First(':'));
      if (corrStr.Length()) allZeros = kFALSE;
      printf("  used sc = %s\n",corrStr.Data());
      corrStr = SCGLcombos.At(i)->GetName();
      corrStr.Remove(0,corrStr.First(':')+1);
      if (corrStr.CountChar(':')) {
        corrStr.Remove(0,corrStr.First(':')+1);
        printf("  used ewratio = %s\n",corrStr.Data());
        corrStr = SCGLcombos.At(i)->GetName();
        corrStr.Remove(0,corrStr.First(':')+1);
        corrStr.Remove(corrStr.First(':'));
      }
      ugl[i] = corrStr.Atof();
      glmode[i] = 1;
      printf("  used GL = %g\n",ugl[i]);
    }
  } else {
    // Read input file
    nfi = nfip;
    ifstream dats(input);
    if (!(dats.good())) {
      printf("Error: problems opening input file specified!\n");
      return 2;
    }
    for (i=0;i<nfi && dats.good();i++) {
      dats >> fname;
      if (dats.eof()) { nfi=i; continue; }
      fis[i] = fname;
      if (fis[i].Contains('@')) {
        fis[i].Remove(fis[i].First('@'),1);
      } else { // depracated; supported only to read in ugl
        dats >> temp >> temp >> ugl[i];
        if (ugl[i]) allZeros = kFALSE;
        if (fis[i].Contains('!')) { // Defunct
          dats >> temp;
          fis[i].Remove(fis[i].First('!'),1);
        }
      }

      // Skip lines beginning with #
      if (fname[0]=='#') { i--; continue; }

      // Build TChains from histogram files
      SCi[i] = new TChain("SC",Form("Chain%d : %s",i,fis[i].Data()));
      int added_files = 0;
      if ((added_files = SCi[i]->Add(fis[i].Data())) < 1) {
        printf("Warning: no files added from %s (Set %d)\n",fis[i].Data(),i);
      } else {
        printf("Set %3d: %d files added from %s\n",i,added_files,fis[i].Data());
        TKey* corrKey = SCi[i]->GetFile()->GetKey("SCcorrection");
        if (corrKey) {
          TString corrStr = corrKey->ReadObj()->GetTitle();
          if (corrStr.Length()) allZeros = kFALSE;
          printf("  used sc = %s\n",corrStr.Data());
          corrKey = SCi[i]->GetFile()->GetKey("SCEWRatio");
          if (corrKey) {
            corrStr = corrKey->ReadObj()->GetTitle();
            printf("  used ewratio = %s\n",corrStr.Data());
          }
          corrKey = SCi[i]->GetFile()->GetKey("GLcorrection");
          corrStr = corrKey->ReadObj()->GetTitle();
          ugl[i] = corrStr.Atof();
        }
        printf("  used GL = %g\n",ugl[i]);
        // glmode: from ntuple (2), from file (1), from input.dat (0)
        glmode[i] = (SCi[i]->GetBranch("ugl") ? 2 :
                     (corrKey ? 1 : 0));
        SCall->Add(fis[i].Data());
      }
    }
  }
  printf("Found %d dataset specifications.\n",nfi);

  for (i=0;i<nfi;i++) {
    if (BY_SECTOR && !(SCi[i]->GetBranch("sc1"))) {
      printf("ERROR: BY_SECTOR specified, but not possible with Set %d!\n",i);
      return 3;
    }
    SCi[i]->SetMarkerStyle(7);
    SCi[i]->SetMarkerColor(colorData);
  }
  SCall->SetMarkerStyle(7);

  memset(conSC,0,nsca*sizeof(TCanvas*));

  // Minimization items
  for (k=0;k<3;k++) parNameSC[k] = new char[16];
  for (k=0;k<3;k++) parNameGL[k] = new char[16];
  for (k=0;k<9;k++) parNameSCGL[k] = new char[16];
  for (k=0;k<19;k++) parNameSCGLsec[k] = new char[16];
  arglist[0] = 5000;
  
  return 0;
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

int SetMinuitPars(const int n, TString* names, Double_t* starts, Double_t* steps, int debug, Bool_t* fixing) {
  TVirtualFitter::SetDefaultFitter("Minuit");
  TVirtualFitter::SetMaxIterations(500000000);
  minuit = TVirtualFitter::Fitter(0,26);
  minuit2 = ((TFitter*) minuit)->GetMinuit();
  minuit2->SetPrintLevel(debug - 2);
    
  int i, maxlen = 0;
  for (i=0; i<n; i++) if (names[i].Length() > maxlen) maxlen = names[i].Length();
  for (i=0; i<n; i++) {
    names[i].Append(' ',maxlen-names[i].Length());
    if (fixing && fixing[i]) {
      minuit->SetParameter(i, names[i].Data(), starts[i],        0,     0,     0);
      minuit->FixParameter(i);
    } else {
      // Rules for limits (default is no limits):
      double lower = 0.;
      double upper = 0.;
      if (names[i].BeginsWith("log(g5")) {
        lower = starts[i] - 1.0;
        upper = starts[i] + 1.0;
      } else if (names[i].BeginsWith("log")) {
        lower = starts[i] - 3.0;
        upper = starts[i] + 3.0;
      } else if (names[i].Contains("g2")) {
        lower = starts[i] * 1e-1;
        upper = starts[i] * 1e1;
      }
      minuit->SetParameter(i, names[i].Data(), starts[i], steps[i], lower, upper);
    }
  }
  return maxlen;
}


void Log2Lin(int i) {
  fitPars[i] = TMath::Exp(fitPars[i]);
  fitParErrs[i] *= fitPars[i];
}

int FitWithOutlierRemoval(int debug) {
  // Iterates the fit, until a stable set of
  //  outliers at > MAX_DEV*VAR are removed
  int i,k,status,nOutliers;
  double temp1,temp2,temp3;
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
    status = minuit->ExecuteCommand("MINIMIZE", arglist, 1);
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
      VAR_singles[ifi] += (devs[i]-devs_set[ifi])*(devs[i]-devs_set[ifi]);
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
      if (devs[i] == 0 || TMath::Abs(devs[i]) > MAX_DEV*VAR) { outliers[i] = kTRUE; nOutliers++; }
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
  double eplus,eminus,eparab,gcc;
  status = minuit->ExecuteCommand("SET LIM", 0, 0);
  if (status) return status;
  for (k=0;k<3;k++) minuit->GetParameter(k,parName[k],fitPars[k],fitParErrs[k],temp1,temp2);
  for (k=0;k<3;k++) {
    for (i=0;i<3;i++) if (i!=k) minuit->FixParameter(i);
    status = minuit->ExecuteCommand("MINIMIZE", arglist, 1);
    if (status) return status;
    arglist[1] = (double) k+1;
    status = minuit->ExecuteCommand("MINOS", arglist, 2);
    if (status) return status;
    minuit->GetParameter(k,parName[k],fitPars[k],temp1,temp2,temp3);
    minuit2->mnerrs(k,eplus,eminus,eparab,gcc);
    if (debug>0) printf("%s\t:\t%g\t+%g/%g or +/- %g\n",parName[k],fitPars[k],eplus,eminus,eparab);
    if (eplus!=0.0 && eminus!=0.0) fitParErrs[k] = 0.5*(eplus-eminus);
    else if (eplus!=0.0 || eminus!=0.0) fitParErrs[k] = eparab;
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

void DrawErrorContours(int npar, Bool_t* fix) {
  int i,j,k,l,status = 0;
  Double_t* gin = 0;
  Double_t x0,y0,chi;
  Int_t indx = 0;
  Double_t lows[128];
  Double_t highs[128];
  TString pName[32];
  for (k=0;k<npar;k++) {
    if (fix && fix[k]) continue;
    lows [k] = fitPars[k]-2.5*fitParErrs[k];
    highs[k] = fitPars[k]+2.5*fitParErrs[k];
    pName[k] = parName[k];
    pName[k].Remove(TString::kTrailing,' ');
  }
  TVirtualPad* origPad = gPad;
  double plmin = (USE_OLD_STDDEV ? 0.75 : 1.0/nfi);
  double plmax = plmin*100.;
  for (i=0;i<npar;i++) {
    if (fix && fix[i]) continue;
    x0 = fitPars[i];
    for (j=0;j<i;j++) {
      if (fix && fix[j]) continue;
      if (indx==15) {
        printf("Stopping error contour drawing at limit (5 parameters)!\n");
        return;
      }
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
      fitPars[i] = x0;
      fitPars[j] = y0;
      indx++;
      origPad->cd(indx)->SetLogz();
      plot->SetMinimum(plmin);
      plot->SetMaximum(plmax);
      plot->Draw("zcol");
    }
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
                  "zdcx:zdcw:zdce:bbcx:bbcw:bbce:bbcyb:bbcbb:vpdx:vpde:vpdw:zdcxnk:zdcwnk:zdcenk");
  scas += ":sc";
  TObjArray* scaA = scas.Tokenize(delim);
  int i,n,N1 = scaA->GetEntries();
  TH1F rmsp("rmsp","rmsp",500,-0.01,0.01);
  int iter0fi = nfi-1;

  if (pcaN >= 0) for (i=0;i<maxp;i++) if (ppl[i]) delete ppl[i];
  memset(ppl,0,maxp*sizeof(TPrincipal*));

  // Read the file once and store the values in a dummy TPrincipal
  //ppl[0] = SCpca->Principal(scas.Data(),cut);
  ppl[0] = new TPrincipal(N1,"D");
  ppl[0]->SetName("principal0");
  for (n=0; n<N1; n++) {
    xvals[n] = new Double_t[nMeasuresMax];
    if (ITER0) {
      SCi[iter0fi]->Draw(scaA->At(n)->GetName(),cut,"goff");
      nMeasures = SCi[iter0fi]->GetSelectedRows();
      memcpy(xvals[n],SCi[iter0fi]->GetV1(),nMeasures*sizeof(Double_t));
    } else {
      nMeasures = 0;
      for (i=0; i<nfi; i++) {
        SCi[i]->Draw(Form("%s*%g",scaA->At(n)->GetName(),
                          (n < N1-1 ? 1.0 : (fitPars[1]+ugl[i])/(fitPars[1]+fitPars[4]))),
                     cut,"goff");
        /* Using fitPars[6] and fitPars[7] can be dangerous if first iteration fit wasn't very good
        if (n < N1-1)
          SCi[i]->Draw(Form("%s",scaA->At(n)->GetName()),cut,"goff");
        else if (fitPars[6] == 1.0 && fitPars[7] == 1.0)
          SCi[i]->Draw(Form("sc*%g",(fitPars[1]+ugl[i])/(fitPars[1]+fitPars[4])),cut,"goff");
        else
          SCi[i]->Draw(Form("(sc-usc)*(%g)+usc*(%g)",
                            fitPars[7]*(fitPars[6]*fitPars[1]+ugl[i])/(fitPars[1]+fitPars[4]),
                            (fitPars[1]+ugl[i])/(fitPars[1]+fitPars[4])),
                       cut,"goff");
        */ 
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

  // For bug: https://sft.its.cern.ch/jira/browse/ROOT-8238
  if (debug>1 && Count<100 && gROOT->GetVersionInt()<60900)
    printf("Please ignore any warnings about nbins from TH1::TH1 ...\n");

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
    if (l == (unsigned int) maxp) {
      printf("Error! Maximum number of combinations %d exceeded!\n",maxp);
      break;
    }

    pp = new TPrincipal(N,"D");
    pp->SetName(Form("principal%d",l));
    ppl[l] = pp;
    for (count=0;count<Count;count++) {
      for (n=0; n<N; n++) x[n] = xvals[map[n]][count];
      pp->AddRow(x);
    }
    pp->MakePrincipals();
    pp->MakeHistograms(Form("PCA_SC%d",l),(debug>1 ? "xp" : "p"));
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

  if (debug<1) {
    for (m=0;m<l;m++) {
      delete ppl[m]; // cleans up histograms too
      ppl[m]=0;
    }
  }

  for (n=0; n<N1; n++) delete xvals[n];


  return scb[rmsi[l-1]];
}

void PrintResult(double scp, double escp, double sop, double esop,
                 double glp, double eglp, double ewp, double eewp, const char* det) {
  printf("sc = (");
  if (!DO_PCA && det) {
    printf("%6.4g +/- %6.4g) * ((%s) - (%6.4g +/- %6.4g)",scp,escp,det,sop,esop);
  } else {
    double lsop = (sop + pcaoffset)/pcacoef[0];
    double lesop = esop/pcacoef[0];
    printf("1.0 +/- %6.4g)*(%6.4g*(%s-(%6.4g +/- %6.4g))",escp/scp,pcacoef[0]*scp,
           pcadets[0].Data(),lsop,lesop);
    for (int n=1;n<pcaN;n++) printf("+(%6.4g*(%s))",pcacoef[n]*scp,pcadets[n].Data());
  }
  printf(")\n");
  if (EW_ASYMMETRY) printf("  with EWratio = %5.3f +/-%5.3f\n",ewp,eewp);
  printf("  with GL = %5.2f +/- %5.2f\n\n",glp,eglp);
}

void PrintResult(double scp, double escp, double sop, double esop,
                 double* glp, double* eglp, const char* det) {
  printf("sc = (");
  if (!DO_PCA && det) {
    printf("%6.4g +/- %6.4g) * ((%s) - (%6.4g +/- %6.4g)",scp,escp,det,sop,esop);
  } else {
    double lsop = (sop + pcaoffset)/pcacoef[0];
    double lesop = esop/pcacoef[0];
    printf("1.0 +/- %6.4g)*(%6.4g*(%s-(%6.4g +/- %6.4g))",escp/scp,pcacoef[0]*scp,
           pcadets[0].Data(),lsop,lesop);
    for (int n=1;n<pcaN;n++) printf("+(%6.4g*(%s))",pcacoef[n]*scp,pcadets[n].Data());
  }
  printf(")\n  with...\n");
  for (int n=0;n<12; n++) {
    if (!unfittableSec[n])
      printf("GL (%2d) = %5.2f +/- %5.2f\n",n+(EWmode<0 ? 13 : 1),glp[n],eglp[n]);
  }
  printf("\n");
}

/////////////////////////////////////////////////////////////////
// $Id: Calib_SC_GL.C,v 2.10 2019/10/21 15:20:57 genevb Exp $
// $Log: Calib_SC_GL.C,v $
// Revision 2.10  2019/10/21 15:20:57  genevb
// Avoid Minuit complaint when asking about a fixed parameter
//
// Revision 2.9  2019/10/16 15:41:01  genevb
// Fix issues with constant (fixed) parameter values
//
// Revision 2.8  2019/09/06 15:29:12  genevb
// Include no-killer ZDC scalers in PCA
//
// Revision 2.7  2016/06/22 20:51:55  genevb
// PCA: delete classes for debug=0, create x hists for debug>1 only
//
// Revision 2.6  2014/11/19 22:11:53  genevb
// Print used ewratio from input files
//
// Revision 2.5  2014/11/19 19:19:56  genevb
// Introduce EW asymmetry, and GL by sector
//
// Revision 2.4  2014/08/12 18:59:02  genevb
// Introduce correlations between input datasets, MIGRAD=>MINIMIZE
//
// Revision 2.3  2014/06/10 19:16:16  genevb
// Fix previous commit log: Introduce basic asymmetry, note of caution on 'guesses'
//
// Revision 2.2  2014/06/10 19:10:24  genevb
// Introduce basic asymmetry, note of caution on 'guesses'
//
// Revision 2.1  2013/03/29 23:54:22  genevb
// Modified fits with additional flexibility, use self-documented files for input,  more scalers, bug fixes
//
// Revision 2.0  2012/06/13 04:45:02  genevb
// Switch to universal fits, add PCA and flexible mixtures of scalers, chisq contour plots
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
