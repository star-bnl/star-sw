///////////////////////////////////////////////////////////////////////////////
//
// $Id: doFlowSumFirstPass.C,v 1.2 2006/07/06 16:58:37 posk Exp $
//
// Makes temporary root.files.<cenNo> containing lists of flow.firstPassLYZNew.root files
//   in all subdirectories of outDir.
// outDir is a link in this directory.
// The cenNo is the firstCenNo + cen.
// The link argument is the prefix of the subdirectory name.
// Adds the histograms together by yield weighted averaging for all files for this centrality.
// Writes out files called flow.firstPassLYZ<cenNo>.root.
// Copies them back to the subdirectories as flow.firstPassLYZNew.root.
// This macro must be in your directory containing the outDir link
//   and run between the first and second passes.
//
// by Art Poskanzer, Feb 2006
//
///////////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <iomanip.h>
#include <fstream.h>
#include "TH1.h"
#include "TFile.h"
#include "TComplex.h"

void doFlowSumFirstPass(char*  link, int firstCenNo) {

  int    nCens = 9; // 9
  Bool_t fromG = kTRUE; // the prefered setting

  char    rootFileName[80];
  char    rootDirName[80];
  char    listFileName[80];
  char    rootOutName[80];
  char    lsCommand[80];
  char    firstPassCommand[80];
  char    rmCommand[80];
  char    cpCommand[80];
  TFile*  histFile[1000];
  Float_t j01 = 2.405;
  int     maxTheta;
  const   int nSels = 2;
  const   int nHars = 2;

  // names of histograms to be averaged with weighting (no profiles and no 2D hists)
  const char* baseName[] = { 
    "FlowLYZ_r0theta_",
    "FlowLYZ_Vtheta_", // must come after r0theta
    "FlowLYZ_Gtheta",  // first G
    "FlowReGtheta",
    "FlowImGtheta"     // after ReG
  };
  const int nNames = sizeof(baseName) / sizeof(char*);
  
  for (int cen = 0; cen < nCens; cen++) {
    int cenNo = firstCenNo + cen;

    // open the input files
    Int_t nFile = 0;
    sprintf(listFileName, "root.files.%2d", cenNo);
    sprintf(lsCommand, "ls -1 outDir/%s-*-%2d/flow.firstPassLYZNew.root > %s", link, cenNo,
	    listFileName);
    //sprintf(lsCommand, "ls -1 flow.firstPassLYZNew.root > %s", listFileName);
    system(lsCommand);
    fstream ascii_in;
    ascii_in.open(listFileName, ios::in);
    ascii_in >> rootFileName;
    while(!ascii_in.eof()) {
      if (strstr(rootFileName,".root")==0) continue;
      histFile[nFile] = new TFile(rootFileName);
      //if (nFile==0) { histFile[nFile]->ls(); }
      strcpy(rootDirName, rootFileName);
      char* cp = strstr(rootDirName, "flow.");
      if (cp) *cp = '\0';                    // truncate to directory name
      sprintf(firstPassCommand, "test -f %s", rootFileName);
      if (system(firstPassCommand)) {
	cout << "### No first pass for " << rootDirName << endl;
	//delete histFile[nFile];
	continue;
      }
      cout << nFile+1 << " directory name = " << rootDirName << endl;
      ascii_in >> rootFileName;
      nFile++;
    }
    const Int_t nFiles = nFile;
    cout << "input files: " << nFiles << endl << endl;
    if (nFiles == 0) { return; }
    
    // the following 3 lines will prevent compiling this macro
    TH1D*    hist[nFiles+1];  // last file is the output
    TH1D*    hist_r0theta[nSels][nHars];
    TH1F*    yieldMultHist[nFiles+1];
    Float_t  yieldTotal[nFiles];
    
    // open the output file
    sprintf(rootOutName, "flow.firstPassLYZ%2d.root", cenNo);
    cout << "tempory output file name = " << rootOutName << endl << endl ;
    histFile[nFiles] = new TFile(rootOutName, "RECREATE"); // last file is new out file
        
    // get multiplicity histograms
    for (int n = 0; n < nFiles; n++) {
      yieldMultHist[n] = dynamic_cast<TH1F*>(histFile[n]->Get("FlowLYZ_Mult"));
      if (!yieldMultHist[n]) {
	cout << "### dynamic cast can't find histogram FlowLYZ_Mult" << endl;
	return;
      }
      yieldTotal[n] = yieldMultHist[n]->GetEntries() * yieldMultHist[n]->GetMean();
      cout << n+1 << ": " << setprecision(7) << yieldTotal[n] << " particles" << endl;
    }

    // add mult hists together for output
    histFile[nFiles]->cd();
    yieldMultHist[nFiles] = (TH1F*)yieldMultHist[0]->Clone();
    for (int n = 1; n < nFiles; n++) {
      yieldMultHist[nFiles]->Add(yieldMultHist[n]);
    }
    cout << endl;

    // Get maxTheta
    if (cen==0) {
      TString histNameNtheta("FlowLYZ_r0theta_Sel1_Har2");
      TH1D* histNtheta = (TH1D*)histFile[0]->Get(histNameNtheta);
      if (!histNtheta) {
	cout << "### Can't find file " << histNameNtheta << endl;
	return;
      }
      maxTheta = histNtheta->GetNbinsX();
      cout << "maxTheta = " << maxTheta << endl << endl;
      const int thetas = maxTheta + 2;
    }
    TH1D*    histReG[nSels][nHars][thetas];
    TH1D*    histG[nSels][nHars][thetas];

    //if (fromG) { cout << "r0:\t\t\t from V \t from G \t change" << endl; }
    float r0V[nSels][nHars][thetas];
    float Xlast, X0, Xnext;
    double reG, imG, Glast, G0, Gnext, GnextNext;
    TComplex Gtheta;

    // with yield weighted averaging
    for (int pageNumber = 0; pageNumber < nNames; pageNumber++ ) {
      for (int selN = 0; selN < nSels; selN++) { 
	for (int harN = 0; harN < nHars; harN++) {
	  for (int thetaN = 0; thetaN < maxTheta; thetaN++) {	
	  
	    // construct histName
	    TString histName(baseName[pageNumber]);
	    if (strstr(histName.Data(), "G")) {
	      histName += thetaN;
	      histName += "_Sel";
	      histName += selN+1;
	      histName += "_Har";
	      histName += harN+1;
	    } else {
	      histName += "Sel";
	      histName += selN+1;
	      histName += "_Har";
	      histName += harN+1;
	    }
	    //cout << "hist name= " << histName << endl;
	    
	    // get the histograms
	    for (int n = 0; n < nFiles; n++) {
	      hist[n] = dynamic_cast<TH1D*>(histFile[n]->Get(histName));
	      if (!hist[n]) {
		cout << "### dynamic cast can't find file " << n << ": histogram " <<
		  histName << endl;
		return;
	      }
	    }
	    
	    // make the output hists
	    histFile[nFiles]->cd();
	    hist[nFiles] = new TH1D(*(hist[0]));
	    
	    // get pointer to the r0 output hist
	    if (strstr(histName.Data(), "r0theta")) {
	      hist_r0theta[selN][harN] = dynamic_cast<TH1D*>(histFile[nFiles]->Get(histName));
	      if (!hist_r0theta[selN][harN]) {
		cout << "### dynamic castan't find hist " << histName << endl;
		return;
	      }
	    }
	    
	    // get pointer to the G output hist
	    if (strstr(histName.Data(), "_G")) {
	      histG[selN][harN][thetaN] = dynamic_cast<TH1D*>(histFile[nFiles]->Get(histName));
	      if (!histG[selN][harN][thetaN]) {
		cout << "### dynamic cast can't find hist " << histName << endl;
		return;
	      }
	    }

	    // get the number of bins for this hist
	    if (!hist[0]) continue;
	    int nBins = hist[0]->GetNbinsX() + 2;
	    
	    // loop over the bins
	    float  yield, r0fromV;
	    double content, error, Vtheta, VthetaErr, r0, r0VErr;
	    double v, vSum, v2Sum, error2Sum, yieldSum, yieldSum2, yield2Sum;
	    for (int bin = 0; bin < nBins; bin++) {
	      v         = 0.;
	      vSum      = 0.;
	      v2Sum     = 0.;
	      content   = 0.;
	      error     = 0.;
	      error2Sum = 0.;
	      yield     = 0.;
	      yieldSum  = 0.;
	      yield2Sum = 0.;
	      for (int n = 0; n < nFiles; n++) { // loop over the files
		if (hist[n]) {
		  yield = yieldTotal[n];
		  v = hist[n]->GetBinContent(bin);
		  if (v != 0. && yield > 1.) { // error is wrong for one count
		    yieldSum  += yield;
		    yield2Sum += yield*yield;
		    vSum      += yield * v;
		    v2Sum     += yield*yield * v*v;
		    error2Sum += pow(yield * hist[n]->GetBinError(bin), 2.);
		  }
		}
	      } // nFiles
	      if (yieldSum) {
		content   = vSum / yieldSum;
		yieldSum2 = yieldSum*yieldSum;
		v2Sum     /= yieldSum2;
		yield2Sum /= yieldSum2;
		error2Sum /= yieldSum2;
		if (strstr(histName.Data(), "G")) {
		  error = 0.;
		} else { 
		  error = sqrt(error2Sum);
		}
	      }
	      hist[nFiles]->SetBinContent(bin, content);
	      hist[nFiles]->SetBinError(bin, error);
	      if (strstr(histName.Data(), "Vtheta")) { // calculate r0theta from Vtheta
		Vtheta = hist[nFiles]->GetBinContent(bin);
		VthetaErr = hist[nFiles]->GetBinError(bin);
		r0V[selN][harN][bin] = Vtheta ? j01 / Vtheta : 0.;
		r0VErr = Vtheta ? r0V[selN][harN][bin] * VthetaErr / Vtheta : 0.;
		hist_r0theta[selN][harN]->SetBinContent(bin, r0V[selN][harN][bin]);
		hist_r0theta[selN][harN]->SetBinError(bin, r0VErr);
	      }

	      if (fromG && strstr(histName.Data(), "ImGtheta")) {
		reG = histReG[selN][harN][thetaN]->GetBinContent(bin);
		imG = hist[nFiles]->GetBinContent(bin);
		Gtheta(reG, imG);
		histG[selN][harN][thetaN]->SetBinContent(bin, Gtheta.Rho2());
	      }

	    } // bin
	    if (!strstr(histName.Data(), "G")) { break; } // only once if no thetas

	    if (fromG && strstr(histName.Data(), "ReGtheta")) {
	      histReG[selN][harN][thetaN] = (TH1D*)hist[nFiles]->Clone();
	    }

	    if ((fromG) && pageNumber == nNames-1) { // calculate r0theta from Gtheta
	      // Find first minimum of the square of the modulus of G for each theta
	      r0 = hist[nFiles]->GetBinCenter(nBins-1); // default value if no minimum
	      for (int N = 2; N < nBins-2; N++) {
		G0        = histG[selN][harN][thetaN]->GetBinContent(N);
		Gnext     = histG[selN][harN][thetaN]->GetBinContent(N+1);
		GnextNext = histG[selN][harN][thetaN]->GetBinContent(N+2);
		
		if (Gnext > G0 && GnextNext > G0) { // lowest point
		  Glast     = histG[selN][harN][thetaN]->GetBinContent(N-1);
		  Xlast     = histG[selN][harN][thetaN]->GetBinCenter(N-1);
		  X0        = histG[selN][harN][thetaN]->GetBinCenter(N);
		  Xnext     = histG[selN][harN][thetaN]->GetBinCenter(N+1);
		  r0 = X0;
		  r0 -= ((X0 - Xlast)*(X0 - Xlast)*(G0 - Gnext) - (X0 - Xnext)*(X0 - Xnext)*(G0 - Glast)) /
		    (2.*((X0 - Xlast)*(G0 - Gnext) - (X0 - Xnext)*(G0 - Glast))); // intopolated minimum
		  break;
		} // if
	      } // N
	      hist_r0theta[selN][harN]->SetBinContent(thetaN+1, r0);
	      hist_r0theta[selN][harN]->SetBinError(thetaN+1, 0.);

	      r0fromV = r0V[selN][harN][thetaN+1];
// 	      if (r0fromV != 0.) { cout << "sel, har, theta= " << selN << ", " << harN << ", "
// 					<< thetaN << ": " << setprecision(4) << r0fromV << " \t"
// 					<< r0 << " \t\t" << setprecision(2)
// 					<< 100. * (r0-r0fromV)/r0fromV << "%" << endl; }
	    } // if last page
	  } // thetaN
	} // harN
      } // selN
    } // pageNumber

    // write the output file
    //histFile[nFiles]->ls();
    histFile[nFiles]->Write(0, TObject::kOverwrite);
    histFile[nFiles]->Close();
    delete histFile[nFiles];

    // copy the output back
    nFile = 0;
    fstream ascii_out;
    ascii_out.open(listFileName, ios::in);
    ascii_out >> rootFileName;
    while(!ascii_out.eof()) {
      if (strstr(rootFileName,".root")==0) continue;
      cout << nFile+1 << " file name = " << rootFileName << endl;
      sprintf(cpCommand, "cp flow.firstPassLYZ%2d.root %s", cenNo, rootFileName);
      system(cpCommand);
      ascii_out >> rootFileName;
      nFile++;
    }
    cout << "####" << endl << endl;

    // clean up
    sprintf(rmCommand, "rm %s", listFileName);
    system(rmCommand);
    sprintf(rmCommand, "rm flow.firstPassLYZ%2d.root", cenNo);
    system(rmCommand);
    
  } // cenNo
}

///////////////////////////////////////////////////////////////////////////////
//
// $Log: doFlowSumFirstPass.C,v $
// Revision 1.2  2006/07/06 16:58:37  posk
// Calculation of v1 for LYZ selection=2 is done with mixed harmonics.
//
// Revision 1.1  2006/03/22 21:57:46  posk
// Macro to sum the Generating Functions between the two passes.
//
//
///////////////////////////////////////////////////////////////////////////////
