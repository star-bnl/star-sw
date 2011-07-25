///////////////////////////////////////////////////////////////////////////////
//
// $Id: doFlowSumAll.C,v 1.6 2011/07/25 15:54:50 posk Exp $
//
// Makes root.files.<cenNo> files containing lists of flow.hist.root files
// in the specified subdirectory/link of outDir (which is a link in this directory).
// Adds the histograms together for all files for all specified centralities.
// The CenNo is actually the baseRunNo + the centrality.
// First adds all histograms, then, for some, does it again for weighted averages,
//  for LYZ including in the error the spread from the different batch jobs.
// Thus, for LYZ do only one centrality at a time
// Last file is the output file.
// This macro must be in your working directory.
// if just AnalysisMaker set nNames = 6
//
// by Art Poskanzer and Kirill Filimonov
//
///////////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <fstream.h>
#include "TObject.h"
#include "TH1.h"
#include "TH2.h"
#include "TFile.h"
#include "TKey.h"
#include "TChain.h"

void doFlowSumAll(Int_t firstCenNo, Int_t lastCenNo, char*  dirName = "", Int_t outputRunNo=99) {

  const  int nSels = 2;
  const  int nHars = 4; // 4
  bool LYZ    = kFALSE;
  bool reCent = kFALSE;

  char   rootFileName[80];
  char   logFileName[80];
  char   listFileName[80];
  char   rootOutName[80];
  char   logOutName[80];
  char   logTitle[80];
  char   lsCommand[80];
  char   cpCommand[80];
  char   logTitleCommand[80];
  char   logCommand[80];
  char   firstPassCommand[80];
  char   secondPassCommand[80];
  char   rmCommand[80];
  TFile* histFile[1000];
  Float_t j01 = 2.405;
  
  // names of histograms to be added with weighting (not including Obs hists)
  const char* baseName[] = { 
    "Flow_Res_",
    "Flow_v2D_",   // must be before vEta and vPt
    "Flow_vEta_",
    "Flow_vPt_",
    "Flow_v_",
    "Flow_q_",
    "Flow_v2D_ScalarProd_",
    "Flow_vEta_ScalarProd_",
    "Flow_vPt_ScalarProd_",
    "Flow_v_ScalarProd_",
    "Flow_Cumul_vEta_Order2_",
    "Flow_Cumul_vPt_Order2_",
    "Flow_Cumul_v_Order2_",
    "Flow_Cumul_vEta_Order4_",
    "Flow_Cumul_vPt_Order4_",
    "Flow_Cumul_v_Order4_",
    "FlowLYZ_r0theta_",
    "FlowLYZ_Vtheta_", // must come after r0theta
    "FlowLYZ_V_",
    "FlowLYZ_vr0_",
    "FlowLYZ_vEta_",
    "FlowLYZ_vPt_",
    "FlowLYZ_v_",
    "FlowLYZ_Gtheta0_",
    "FlowLYZ_Gtheta1_",
    "FlowLYZ_Gtheta2_",
    "FlowLYZ_Gtheta3_",
    "FlowLYZ_Gtheta4_"
  };
  //const int nNames = sizeof(baseName) / sizeof(char*);
  const int nNames = 6; // if just AnalysisMaker

  // open the output log file
  sprintf(logOutName, "ana%2d.log", outputRunNo);
  sprintf(logTitle, "  Combination of centralities %2d to %2d", firstCenNo, lastCenNo);
  sprintf(logTitleCommand, "echo %s > %s", logTitle, logOutName);
  system(logTitleCommand);

  // open the input files
  Int_t nFile = 0;
  for (int nc = firstCenNo; nc <= lastCenNo; nc++) {
    sprintf(listFileName, "root.files.%2d", nc);
    //sprintf(lsCommand, "ls -1 outDir/*-%2d/flow.hist.root > %s", nc, listFileName);
    sprintf(lsCommand, "ls -1 outDir/%s*-%2d/flow.hist.root > %s", dirName, nc, listFileName);
    system(lsCommand);
    sprintf(cpCommand, "cat %s >> %s", listFileName, logOutName);
    system(cpCommand);
    fstream ascii_in;
    ascii_in.open(listFileName, ios::in);

    while(!ascii_in.eof()) {
      ascii_in >> rootFileName;
      if (strstr(rootFileName,".root")==0) continue;
      histFile[nFile] = new TFile(rootFileName);
      char* cp = strstr(rootFileName, "flow.");
      if (cp) *cp = '\0';                                  // truncate to directory name
      sprintf(firstPassCommand, "test -f %sflowPhiWgtNew.hist.root", rootFileName);
      sprintf(secondPassCommand, "test -f %sflowPhiWgt.hist.root", rootFileName);
      if (LYZ) sprintf(secondPassCommand, "test -f %sflow.firstPassLYZNew.root", rootFileName);
      if (reCent) {
	sprintf(firstPassCommand, "test -f %sflow.reCentAnaNew.root", rootFileName);
	sprintf(secondPassCommand, "test -f %sflow.reCentAna.root", rootFileName);
      }
      if (system(firstPassCommand) && system(secondPassCommand)) {
	cout << "####################################" << endl;
 	cout << "### No 2nd pass for " << rootFileName << endl;
	cout << "####################################" << endl;
	delete histFile[nFile];
	continue;
      }
      cout << nFile+1 << " directory name = " << rootFileName << endl;
      sprintf(logFileName, "%sana.log", rootFileName);
      sprintf(logCommand, "cat %s >> %s", logFileName, logOutName);
      system(logCommand);
      nFile++;
    }
  }
  const  Int_t nFiles = nFile;
  cout << endl;
  cout << "input files: " << nFiles << endl;
  if (!nFiles) {
    cout << "#### no files" << endl;
    return;
  }

  TH2*    yieldPartHist[nFiles];
  TH1*    hist[nFiles+1];
  TH1*    yieldPartHistPt[nFiles];
  TH1*    yieldPartHistEta[nFiles];
  Float_t yieldTotal[nFiles];
  TH1*    hist_r0theta[nSels][nHars];

  // open the output file
  sprintf(rootOutName, "ana%2d.root", outputRunNo);
  cout << " output file name = " << rootOutName << endl;
  histFile[nFiles] = new TFile(rootOutName, "RECREATE"); // last file is new out file
  cout << "    log file name = " << logOutName << endl << endl;
    
  // add all histograms with no weighting
  // iterate over the histograms in file[0] and add all the others to it
  // write output to file[nFiles]
  TKey*    key;
  TObject* obj;
  const char* objName = 0;
  TIter nextkey(histFile[0]->GetListOfKeys());  
  while ((key = (TKey*)nextkey())) {
    histFile[0]->cd();
    //key->ls();
    obj = key->ReadObj();
    if (obj->InheritsFrom("TH1")) { // TH1 or TProfile
      hist[0] = (TH1*)obj;
      objName = key->GetName();
      cout << "  hist name= " << objName << endl;
      for (int n = 1; n < nFiles; n++) {
	hist[1] = (TH1*)histFile[n]->Get(objName);
	hist[0]->Add(hist[1]);
      }
    }
    histFile[nFiles]->cd();
    obj->Write(objName);
    delete obj;
    obj = NULL;
  }
   
  // get yield histograms
  for (int n = 0; n < nFiles; n++) {
    yieldPartHist[n] = dynamic_cast<TH2*>(histFile[n]->Get("Flow_YieldPart2D"));
    yieldPartHistPt[n] = dynamic_cast<TH1*>(histFile[n]->Get("FlowLYZ_YieldPartPt"));
    yieldPartHistEta[n] = dynamic_cast<TH1*>(histFile[n]->Get("FlowLYZ_YieldPartEta"));
    if (yieldPartHistPt[n]) { yieldTotal[n] = yieldPartHistPt[n]->Integral(); }
  }

  cout << endl << "with weighting" << endl;
  TH1F* yieldY;
  TH1F* yieldPt;
  for (int pageNumber = 0; pageNumber < nNames; pageNumber++ ) {
  nextPage:
    bool twoD = kFALSE;
    if (strstr(baseName[pageNumber],"v2D")) twoD = kTRUE;

    for (int selN = 0; selN < nSels; selN++) {
      
      // no harmonics
      bool noHars = kFALSE;
      int nHar = nHars;
      if (strstr(baseName[pageNumber],"_v_")!=0 ||
	  strcmp(baseName[pageNumber],"Flow_Cos_")==0 ||
	  strstr(baseName[pageNumber],"Res")!=0 ||
	  strstr(baseName[pageNumber],"_V_")!=0 ||
	  strstr(baseName[pageNumber],"_vr0_")!=0) {
	noHars = kTRUE;
	nHar = 1;
      }

      // two harmonics
      if (strstr(baseName[pageNumber],"theta_")!=0) {
	nHar = 2;
      }
 
      for (int harN = 0; harN < nHar; harN++) {	
	
	// construct histName
	TString histName(baseName[pageNumber]);
	histName += "Sel";
	histName += selN+1;
	if (!noHars) {	
	  histName += "_Har";
	  histName += harN+1;
	}
		
	if (strstr(histName.Data(), "_Gtheta") && harN > 1) { continue; }

	// get the histograms
	for (int n = 0; n < nFiles+1; n++) {
	  hist[n] = dynamic_cast<TH1*>(histFile[n]->Get(histName));
	  if (!hist[n]) {
	    if (pageNumber < nNames) {
	      pageNumber++;
	      goto nextPage;
	    } else {
	      cout << endl;
	      cout << "*** Error: Correct nNames in macro" << endl;
	    }
	  } else if (n == 0) {
	    cout << "  hist name= " << histName << endl;
	  }
	}

	// get the r0theta output hist
	if (strstr(histName.Data(), "r0theta")) {
	  hist_r0theta[selN][harN] = dynamic_cast<TH1*>(histFile[nFiles]->Get(histName));
	}

	// get the number of bins for this hist
	if (!hist[0]) { continue; }
	int nBins;      // set by 2D
	int xBins = hist[0]->GetNbinsX();
	int yBins = 0.;
	if (twoD) {
	  yBins = hist[0]->GetNbinsY();
	  nBins = xBins + (xBins + 2) * yBins;
	  float yMax  = hist[0]->GetXaxis()->GetXmax();
	  float yMin  = hist[0]->GetXaxis()->GetXmin();
	  yieldY = new TH1F("Yield_Y", "Yield_Y", xBins, yMin, yMax);
	  yieldY->SetXTitle("Rapidity");
	  yieldY->SetYTitle("Counts");
	  float ptMax  = hist[0]->GetYaxis()->GetXmax();
	  yieldPt = new TH1F("Yield_Pt", "Yield_Pt", yBins, 0., ptMax);
	  yieldPt->SetXTitle("Pt (GeV/c)");
	  yieldPt->SetYTitle("Counts");
	} else {
	  nBins = xBins + 2;
	}
	
	// loop over the bins
	if (strstr(baseName[pageNumber],"LYZ")) {	
	  cout << " with LYZ yield weighted averaging" << endl;
	  double  v, yield, content, error, Vtheta, VthetaErr, r0, r0Err;
	  double vSum, v2Sum, error2Sum, yieldSum, yieldSum2, yield2Sum;
	  //for (int bin = 0; bin < nBins; bin++) {
	  for (int bin = 1; bin < nBins-1; bin++) {
	    v         = 0.;
	    vSum      = 0.;
	    v2Sum     = 0.;
	    content   = 0.;
	    error     = 0.;
	    error2Sum = 0.;
	    yield     = 0.;
	    yieldSum  = 0.;
	    yield2Sum = 0.;
	    for (int n = 0; n < nFiles; n++) {
	      if (hist[n]) {
		if (strstr(histName.Data(), "vEta")) {
		  yield = yieldPartHistEta[n]->GetBinContent(bin);
		} else if (strstr(histName.Data(), "vPt")) {
		  yield = yieldPartHistPt[n]->GetBinContent(bin);
		} else {                               // r0theta, Vtheta, _V_, vr0, _v_, Gtheta
		  yield = yieldTotal[n];
		}
		v = hist[n]->GetBinContent(bin);
		if (v != 0 && yield > 1.) { // error is wrong for one count
		  yieldSum  += yield;
		  yield2Sum += yield*yield;
		  vSum      += yield * v;
		  v2Sum     += yield * v*v;
		  error2Sum += pow(yield * hist[n]->GetBinError(bin), 2.);
		}
	      }
	    } // nFiles
	    if (yieldSum) {
	      content   = vSum / yieldSum;
	      v2Sum     /= yieldSum;
	      yieldSum2 = yieldSum*yieldSum;
	      yield2Sum /= yieldSum2;
	      error2Sum /= yieldSum2;
	      if (strstr(baseName[pageNumber],"_G")) {
		error = 0.;
	      } else if (nFiles > 1 && yield2Sum != 1.) {
		error = sqrt(error2Sum + (v2Sum - content*content) / (1/yield2Sum - 1));
	      } else {
		error = sqrt(error2Sum); // only 1st term
	      }
	      if (strstr(histName.Data(), "v")) {
// 		cout << histName << " 1st, 2nd: " << sqrt(error2Sum) << ", " << sqrt(error*error - error2Sum) << endl;
	      }
 	      //error = sqrt(error2Sum); // only 1st term
	    }
	    hist[nFiles]->SetBinContent(bin, content);
	    hist[nFiles]->SetBinError(bin, error);
	    if (strstr(histName.Data(), "Vtheta")) {
	      //cout << content << ", " << error2Sum << ", " << v2Sum << ", " << content*content << ", " << yield2Sum << ", " << error << endl;
	      Vtheta = hist[nFiles]->GetBinContent(bin);
	      VthetaErr = hist[nFiles]->GetBinError(bin);
	      if (VthetaErr > 0.001) { // calculate r0theta from <Vtheta>
	        //float perCentErr = Vtheta ? 100. * VthetaErr / Vtheta : 0.;
	        //cout << "Vtheta= " << Vtheta << " +/- " << perCentErr << "%" << endl;
		r0 = Vtheta ? j01 / Vtheta : 0.; // Eq. 9
		r0Err = Vtheta ? r0 * VthetaErr / Vtheta : 0.;
		//cout << hist_r0theta[selN][harN]->GetBinContent(bin) << ", " << r0 << endl;
		hist_r0theta[selN][harN]->SetBinContent(bin, r0);
		hist_r0theta[selN][harN]->SetBinError(bin, r0Err);
		//cout << "r0= " << r0 << " +/- " << r0Err << endl;
	      }
	    }
	  } // bins
	} else if ((strstr(baseName[pageNumber],"Cos")) ||
		   (strstr(baseName[pageNumber],"Res"))) { // with error weighting
	  cout << "  With error weighting" << endl;
	  double content, error, errorSq, meanContent, meanError, weight;
	  for (int bin = 0; bin < nBins; bin++) {
	    meanContent = 0.;
	    meanError   = 0.;
	    weight      = 0.;
	    for (int n = 0; n < nFiles; n++) {
	      if (hist[n]) {
		content = hist[n]->GetBinContent(bin);
		error   = hist[n]->GetBinError(bin);
		errorSq = error * error;
		if (errorSq > 0.) {
		  meanContent += content / errorSq;
		  weight      += 1. / errorSq;
		}
	      }
	    } // nFiles
	    if (weight > 0.) {
	      meanContent /= weight;
	      meanError = sqrt(1. / weight);
	      hist[nFiles]->SetBinContent(bin, meanContent);
	      hist[nFiles]->SetBinError(bin, meanError);
	    }
	  } // bins
	} else {                                   // with yield weighting
	  cout << "  with yield weighted averaging" << endl;
	  double v, yield, content, error, y, pt;
	  double vSum, error2sum, yieldSum;
	  for (int bin = 0; bin < nBins; bin++) {
	    v         = 0.;
	    vSum      = 0.;
	    content   = 0.;
	    error     = 0.;
	    error2sum = 0.;
	    yield     = 0.;
	    yieldSum  = 0.;
	    for (int n = 0; n < nFiles; n++) {
	      if (hist[n]) {
		if (strstr(histName.Data(),"v2D")) {
		  yield = yieldPartHist[n]->GetBinContent(bin);
		} else if (strstr(histName.Data(),"vEta")) {
		  yield = yieldPartHist[n]->Integral(bin, bin, 1, yBins);
		  if (selN==0 && harN==0) {
		    y = yieldPartHist[n]->GetXaxis()->GetBinCenter(bin);
		    yieldY->Fill(y, yield);
		  }
		} else if (strstr(histName.Data(),"vPt")) {
		  yield = yieldPartHist[n]->Integral(1, xBins, bin, bin);
		  if (selN==0 && harN==0) {
		    pt = yieldPartHist[n]->GetYaxis()->GetBinCenter(bin);
		    yieldPt->Fill(pt, yield);
		  }
		} else {                                        // _v_ and _q_
		  yield = yieldPartHist[n]->Integral();
		}
		v = hist[n]->GetBinContent(bin);
		  if (v != 0. && yield > 1.) { // error is wrong for one count
		  yieldSum  += yield;
		  vSum      += yield * v;
		  error2sum += pow(yield *  hist[n]->GetBinError(bin), 2.);
		}
	      }
	    } // nFiles
	    if (yieldSum) {
	      content = vSum / yieldSum;
	      error   = sqrt(error2sum) / yieldSum;
	    }
	    hist[nFiles]->SetBinContent(bin, content);
	    hist[nFiles]->SetBinError(bin, error);
	  } // bin
	} // else 
      } // harN
    } // selN
    cout << endl;
  } // pageNumber

  cout << endl;
  //histFile[nFiles]->ls();
  histFile[nFiles]->Write(0, TObject::kOverwrite);
  histFile[nFiles]->Close();
  delete histFile[nFiles];
  sprintf(rmCommand, "rm %s", listFileName);
  system(rmCommand);  
}

///////////////////////////////////////////////////////////////////////////////
//
// $Log: doFlowSumAll.C,v $
// Revision 1.6  2011/07/25 15:54:50  posk
// Added correction for non-flatness of event plane.
//
// Revision 1.5  2011/03/10 18:56:34  posk
// Added histogram for laboratory azimuthal distribution of particles.
//
// Revision 1.4  2010/09/30 19:28:21  posk
// Instead of reversing the weight for negative pseudrapidity for odd harmonics,
// it is now done only for the first harmonic.
// Recentering is now done for all harmonics.
//
// Revision 1.3  2007/02/06 19:00:50  posk
// In Lee Yang Zeros method, introduced recentering of Q vector.
// Reactivated eta symmetry cut.
//
// Revision 1.1  2006/03/22 21:59:51  posk
// Macro and shell script to sum the outputs of the second pass.
//
//
///////////////////////////////////////////////////////////////////////////////
