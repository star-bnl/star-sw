///////////////////////////////////////////////////////////////////////////////
//
// $Id: vProj.C,v 1.6 2001/10/24 21:46:36 posk Exp $
//
// Author:       Art Poskanzer, May 2000
// Description:  Projects v(y,pt) on the y and Pt axes
//                 with yield or cross section weighting.
//               Also makes the min. bias histograms
//                 and v as a function of centrality.
//               Saves output histograms to a file.
//               The cross sections come from dNdydPt.C .
//               Centrality = 0 is min. bias
//               Centralites 7, 8, and 9 are the 
//                 combined 1-2, 3-4, and 5-6 centralities.
//               Makes momentum conserevation correction if pCons non-zero.
//
//
///////////////////////////////////////////////////////////////////////////////
//
// $Log: vProj.C,v $
// Revision 1.6  2001/10/24 21:46:36  posk
// Added conservation of momentum correction. Calculate triply integrated v values.
//
// Revision 1.4  2001/05/14 23:20:29  posk
// Uses cross section weighting for all projections.
//
// Revision 1.3  2001/03/16 22:35:10  posk
// plotGraphs.C makes the final graphs.
//
// $Log: vProj.C,v $
// Revision 1.6  2001/10/24 21:46:36  posk
// Added conservation of momentum correction. Calculate triply integrated v values.
//
// Revision 1.4  2001/05/14 23:20:29  posk
// Uses cross section weighting for all projections.
//
// Revision 1.2  2001/03/06 17:33:04  posk
// All macros now work.
//
//
// Revision 1.1  2001/02/23 00:58:19  posk
// NA49 version of STAR software.
//
///////////////////////////////////////////////////////////////////////////////
#include <iostream.h>
#include "TString.h"

TCanvas*  can;
char      runName[6];
char      temp[30];
TPad*     graphPad; 
const int nCens = 6;

void vProj(char* part = "pion") {
  
  int       eBeam = 158; //select full beam energy
  //int       eBeam = 40;  //select 40Gev beam energy
  bool      pion = kFALSE;
  if (strcmp(part, "pion")==0) pion = kTRUE;
  bool   crossSection = kTRUE;
  //bool   crossSection = kFALSE;     // use yield weighting

  const int nPlots = 1 + nCens + nCens/2;
  const int nHars = 2;
  int       sel   = 2;
  float     yLow  = 2.92;             // for pt proj.
  float     yLowY = 2.;               // for y proj.
  float     yUp   = 5.;               // for both projs.
  float     ptUp  = 2.;               // for both projs.
  //float     pCons = 0.;               // no p cons. correction
  float     pCons = 1.;               // adjustable constant for p cons.
  float     pConsErr = 0.1;           // error in adjustable constant
  float     sumPt2All[nCens] = {773.,623.,455.,305.,203.,122.}; // from Glenn
  float     meanMul[nCens] = {135.,184.,152.,108.,75.,46.}; // for har 1
  float     meanPt[nCens] = {0.166,0.255,0.288,0.282,0.279,0.275}; // for har 1
  double    frac[nCens];
  double    sqrtPiOver2 = 0.886;      // sqrt(pi) / 2
  double    sqrt2 = 1.414;
  double    res;
  double    chiSV;
  double    chiFact;
  TFile*    anaFile[nCens];
  TH2*      v2D[nCens][nHars];
  TH2*      vObs2D[nCens][nHars];
  TH1*      resHist[nCens];
  TH2*      yieldPartHist[nCens];
  TH1F*     vY[nPlots][nHars];
  TH1F*     vPt[nPlots][nHars];
  TH1F*     vCen[nHars];
  TH1F*     vMB;
  TH1F*     yieldCen;
  TH1F*     yieldY[nPlots];
  TH1F*     yieldPt[nPlots];
  TString*  histName;
  char      histTitle[30];
  double    yCM = 2.92;
  int       firstRunNumber = 0;
  char      fileName[30];
  int       cent;
  TAxis*    xAxis;
  TAxis*    yAxis;
  int       xBins;
  int       yBins;
  double    y;
  double    pt;
  double    yield;
  double    yieldSum;
  double    v;
  double    vErr;
  double    deltaV;
  double    vSum;
  double    err2Sum;
  float     content;
  float     error;
  char      selText[2];
  char      cenText[2];
  char      harText[2];
  float     vYMax  = 7.;
  float     vYMin  = -5.;
  float     vPtMax = 20.;
  float     vPtMin = -10.;
 
  if (eBeam == 40) {
    yLow   =  2.24;             // for pt proj.
    yLowY  =  1.;               // for y proj.
    yUp    =  4.5;              // for both projs.
    ptUp   =  2.;               // for both projs.
    yCM    =  2.24;
    vYMax  =  5.;
    vYMin  = -5.;
    vPtMax =  5.;
    vPtMin = -5.;
  }
   
  // set style
  gROOT->SetStyle("Bold");                              
  gROOT->ForceStyle();
  gStyle->SetOptStat(0);

  // load the cross section macro
  if (crossSection) {
    gROOT->LoadMacro("dNdydPt.C");
  }

  // convert integer to text
  sprintf(selText, "%d", sel);

  // input the first run number
  if (firstRunNumber == 0) {
    cout << "     first run number? ";
    fgets(temp, sizeof(temp), stdin);
    firstRunNumber = atoi(temp);
    sprintf(runName, "ana%2d", firstRunNumber);             // add ana prefix
    cout << " first run name = " << runName << endl;
  }

  // construct out file name
  TString* outName = new TString(part);
  if (pCons != 0.) outName->Append("Pcons");
  if (!crossSection) outName->Append("Yield");
  outName->Append(".root");
  char outFile[255] = outName->Data();
  cout << " out file = " << outFile << endl;
  delete outName;

  for (int n = 0; n < nCens; n++) {
    int runNumber = firstRunNumber + n;
    cout << "Cent " << n+1 << endl;

    // open the files
    sprintf(fileName, "ana%d.root", runNumber);
    cout << " file name= " << fileName << endl;
    anaFile[n] = new TFile(fileName);
    if (!anaFile[n]) {
      cout << "### Can't find file " << fileName << endl;
      return;
    }
    cent = runNumber % 10;                             // unit digit = cent
    if (cent <= 0 || cent > nCens) {
      cout << " Not valid centrality = " << cent << endl;
      return;
    }
    sprintf(cenText, "%d", cent);
    
    // calculate frac[n] for Olli's momentum conservation correction
    if (pCons != 0.) {
      frac[n] = meanMul[n] * meanPt[n] * meanPt[n] / sumPt2All[n];
      cout << " fraction= " << frac[n] << endl;
    }
    
    //get the resolutions
    if (pCons != 0.) {
      histName = new TString("Flow_Res_Sel");
      histName->Append(*selText);
      resHist[n] = dynamic_cast<TH1*>(anaFile[n]->Get(histName->Data()));
      if (!resHist[n]) {
	cout << "### Can't find histogram " << histName->Data() << endl;
	return;
      }
      delete histName;
    }      

    // get the 2D v histograms
    for (int j = 0; j < nHars; j++) {
      sprintf(harText, "%d", j+1);
      histName = new TString("Flow_v2D_Sel");
      histName->Append(*selText);
      histName->Append("_Har");
      histName->Append(*harText);
      v2D[n][j] = dynamic_cast<TH2*>(anaFile[n]->Get(histName->Data()));
      if (!v2D[n][j]) {
	cout << "### Can't find histogram " << histName->Data() << endl;
	return;
      }
      delete histName;
      if (n==0 && j==0) {
	yAxis = v2D[0][0]->GetYaxis();
	yBins = yAxis->GetNbins();
	xAxis = v2D[0][0]->GetXaxis();
	xBins = xAxis->GetNbins();
      }
      res = resHist[n]->GetBinContent(j+1);
      if (pCons !=0. && j==0) {        // conservation of momentum correction
	histName = new TString("Flow_vObs2D_Sel");
	histName->Append(*selText);
	histName->Append("_Har");
	histName->Append(*harText);
	vObs2D[n][j] = dynamic_cast<TH2*>(anaFile[n]->Get(histName->Data()));
	if (!vObs2D[n][j]) {
	  cout << "### Can't find histogram " << histName->Data() << endl;
	  return;
	}
	delete histName;
	v2D[n][j]->Reset();
	chiSV = chi(res);
	chiFact = F(chiSV/1.414);
	cout << " ChiSV= " << chiSV << endl;
	cout << " F= " << chiFact << endl;
	for (int yBin=1; yBin<=yBins; yBin++) {
	  pt = yAxis->GetBinCenter(yBin);
	  for (int xBin=1; xBin<=xBins; xBin++) {
	    v = vObs2D[n][j]->GetCellContent(xBin, yBin);
	    vErr = vObs2D[n][j]->GetCellError(xBin, yBin);
	    //deltaV = pCons * 100. * sqrtPiOver2 * (1. - res) * pt * sqrt(frac[n]/(1.-frac[n])) /  sqrt(sumPt2All[n]);
	    //deltaV = pCons * 100. * (sqrtPiOver2 - chiSV/sqrt2) * pt * sqrt(frac[n]/(1.-frac[n])) / sqrt(sumPt2All[n]);
	    deltaV = pCons * 100. * chiFact * pt * sqrt(frac[n]/(1.-frac[n])) / 
	      sqrt(sumPt2All[n]);
	    v += deltaV;
	    v2D[n][j]->SetCellContent(xBin, yBin, v);
	    vErr = sqrt(vErr*vErr + pConsErr*deltaV*pConsErr*deltaV);
	    v2D[n][j]->SetCellError(xBin, yBin, vErr);
	  }
	}
	if (res != 0.) {
	  v2D[n][j]->Scale(1. / res);
	} else {
	  cout << "resolution of the " << j+1 << "th harmonic was zero." << endl;
	  v2D[n][j]->Reset();
	}
      }
      v2D[n][j]->SetMaximum(20.);
      v2D[n][j]->SetMinimum(-20.);
    }

    // Get the 2D yield histograms
    yieldPartHist[n] = dynamic_cast<TH2*>(anaFile[n]->Get("Flow_YieldPart2D"));
    if (!yieldPartHist[n]) {
      cout << "### Can't find yield part histogram Flow_YieldPart2D"
	   << endl;
      return;
    }
  }
  
  // Get the axes properties
  float  xMin  = xAxis->GetXmin();
  float  xMax  = xAxis->GetXmax();
  float  yMin  = yAxis->GetXmin();
  float  yMax  = yAxis->GetXmax();
 
  cout << "Please wait" << endl << endl;

  for (int n = 0; n < nPlots; n++) {
    sprintf(cenText, "%d", n);

    // Create 1D Yield histograms
    histName = new TString("Flow_YieldY_Sel");
    histName->Append(*selText);
    histName->Append("_Cen");
    histName->Append(*cenText);
    yieldY[n] = new TH1F(histName->Data(), histName->Data(), xBins, xMin, xMax);
    yieldY[n]->SetXTitle("Rapidity");
    yieldY[n]->SetYTitle("Yield");
    delete histName;
    
    histName = new TString("Flow_YieldPt_Sel");
    histName->Append(*selText);
    histName->Append("_Cen");
    histName->Append(*cenText);
    yieldPt[n] = new TH1F(histName->Data(), histName->Data(), yBins, yMin, yMax);
    yieldPt[n]->SetXTitle("Pt (GeV)");
    yieldPt[n]->SetYTitle("Yield");
    delete histName;
    
    for (int j = 0; j < nHars; j++) {
      sprintf(harText, "%d", j+1);

      // Create the 1D v histograms
      histName = new TString("Flow_vY_Sel");
      histName->Append(*selText);
      histName->Append("_Har");
      histName->Append(*harText);
      histName->Append("_Cen");
      histName->Append(*cenText);
      vY[n][j] = new TH1F(histName->Data(), histName->Data(), xBins, xMin, xMax);
      vY[n][j]->SetXTitle("Rapidity");
      vY[n][j]->SetYTitle("Flow (%)");
      vY[n][j]->SetMaximum(vYMax);
      vY[n][j]->SetMinimum(vYMin);
      delete histName;
      
      histName = new TString("Flow_vPt_Sel");
      histName->Append(*selText);
      histName->Append("_Har");
      histName->Append(*harText);
      histName->Append("_Cen");
      histName->Append(*cenText);
      vPt[n][j] = new TH1F(histName->Data(), histName->Data(), yBins, yMin, yMax);
      vPt[n][j]->SetXTitle("Pt (GeV)");
      vPt[n][j]->SetYTitle("Flow (%)");
      vPt[n][j]->SetMaximum(vPtMax);
      vPt[n][j]->SetMinimum(vPtMin);
      delete histName;

    }
  }
      
  // Create the histograms for v as a function of centrality
  histName = new TString("Flow_Yield_Sel");
  histName->Append(*selText);
  yieldCen = new TH1F(histName->Data(), histName->Data(), nCens, 0.5, nCens+0.5);
  yieldCen->SetXTitle("Centrality");
  yieldCen->SetYTitle("Yield");
  delete histName;
  for (int j = 0; j < nHars; j++) {
    sprintf(harText, "%d", j+1);
      histName = new TString("Flow_v_Sel");
      histName->Append(*selText);
      histName->Append("_Har");
      histName->Append(*harText);
      vCen[j] = new TH1F(histName->Data(), histName->Data(), nCens, 0.5, 
			 nCens+0.5);
      vCen[j]->SetXTitle("Centrality");
      vCen[j]->SetYTitle("Flow (%)");
      vCen[j]->SetMaximum(5.);
      vCen[j]->SetMinimum(-5.);
      delete histName;
  }

  // Create the histogram for the triply integrated v
  histName = new TString("Flow_vMB_Sel");
  histName->Append(*selText);
  vMB = new TH1F(histName->Data(), histName->Data(), nHars, 0.5, 
		 nHars+0.5);
  vMB->SetXTitle("Harmonic");
  vMB->SetYTitle("Flow (%)");
  vMB->SetMaximum(3.);
  vMB->SetMinimum(-2.);
  delete histName;
  
  for (int n = 1; n <= nCens; n++) {
    for (int j = 0; j < nHars; j++) {
      
      // fill the rapidity projections -----------------------------
      for (int xBin=1; xBin<=xBins; xBin++) {
	yieldSum = 0.;
	vSum     = 0.;
	err2Sum  = 0.;
	content  = 0.;
	error    = 0.;
	y = xAxis->GetBinCenter(xBin);
	if (y > yLowY && y < yUp) {
	  for (int yBin=1; yBin<=yBins; yBin++) {
	    pt = yAxis->GetBinCenter(yBin);
	    if (pt < ptUp) {
	      if (crossSection) {
		if (pion) {
		  //yield = 1.;
		  yield = dNdydPt(0, y - yCM, pt, n) +
		    dNdydPt(1, y - yCM, pt, n);               // pi+ + pi-
		} else {
		  yield = dNdydPt(4, y - yCM, pt, n);         // p
		}
	      } else {
		yield = yieldPartHist[n-1]->GetCellContent(xBin, yBin);
	      }
	      v = v2D[n-1][j]->GetCellContent(xBin, yBin);
	      if(v != 0.0) {
		yieldSum += yield;
		vSum     += yield * v;
		err2Sum  += pow(yield * v2D[n-1][j]->GetCellError(xBin, yBin), 
				2.);
	      }
	    }
	  }
	  if (yieldSum) {
	    content = vSum / yieldSum;
	    error   = sqrt(err2Sum) / yieldSum;
	  }
	  yieldY[n]->SetBinContent(xBin, yieldSum);
	  vY[n][j]->SetBinContent(xBin, content);
	  vY[n][j]->SetBinError(xBin, error);
	}
      }
      
      // fill the pt projections -------------------------------------
      for (int yBin=1; yBin<=yBins; yBin++) {
	yieldSum = 0.;
	vSum     = 0.;
	err2Sum  = 0.;
	content  = 0.;
	error    = 0.;
	pt = yAxis->GetBinCenter(yBin);
	for (int xBin=1; xBin<=xBins; xBin++) {
	  y = xAxis->GetBinCenter(xBin);
	  if (y > yLow && y < yUp) {
	    v = v2D[n-1][j]->GetCellContent(xBin, yBin);
	    if (j % 2 == 1 && y < yCM) v *= -1; // backward particles for odd har
	    if (crossSection) {
	      if (pion) {
		//yield = 1.;
		yield = dNdydPt(0, y - yCM, pt, n) +
		  dNdydPt(1, y - yCM, pt, n);                 // pi+ + pi-
	      } else {
		yield = dNdydPt(4, y - yCM, pt, n);           // p
	      }
	    } else {
	      yield = yieldPartHist[n-1]->GetCellContent(xBin, yBin);
	    }
	    if(v != 0.0) {
	      yieldSum += yield;
	      vSum     += yield * v;
	      err2Sum  += pow(yield * v2D[n-1][j]->GetCellError(xBin, yBin), 2.);
	    }
	  }
	}
	if (yieldSum) {
	  content = vSum / yieldSum;
	  error   = sqrt(err2Sum) / yieldSum;
	}
	yieldPt[n]->SetBinContent(yBin, yieldSum);
	vPt[n][j]->SetBinContent(yBin, content);
	vPt[n][j]->SetBinError(yBin, error);
      }
    }
  }
  
  // fill the min. bias projections (cen = 0) --------------------------
  for (int j = 0; j < nHars; j++) {
    for (int xBin=1; xBin<=xBins; xBin++) {
      yieldSum = 0.;
      vSum     = 0.;
      err2Sum  = 0.;
      content  = 0.;
      error    = 0.;
      for (int n = 1; n <= nCens; n++) {
	v     = vY[n][j] ->GetBinContent(xBin);
	yield = yieldY[n]->GetBinContent(xBin);
	if(v != 0.0) {
	  yieldSum += yield;
	  vSum     += yield * v;
	  err2Sum  += pow(yield * vY[n][j]->GetBinError(xBin), 2.);
	}
      }	
      if (yieldSum) {
	content = vSum / yieldSum;
	error   = sqrt(err2Sum) / yieldSum;
      }
      if (j==0) yieldY[0]->SetBinContent(xBin, yieldSum);
      vY[0][j]->SetBinContent(xBin, content);
      vY[0][j]->SetBinError(xBin, error);
    }

    for (int xBin=1; xBin<=yBins; xBin++) {
      yieldSum = 0.;
      vSum     = 0.;
      err2Sum  = 0.;
      content  = 0.;
      error    = 0.;
      for (int n = 1; n <= nCens; n++) {
	v     = vPt[n][j] ->GetBinContent(xBin);
	yield = yieldPt[n]->GetBinContent(xBin);
	if(v != 0.0) {
	  yieldSum += yield;
	  vSum     += yield * v;
	  err2Sum  += pow(yield * vPt[n][j]->GetBinError(xBin), 2.);
	}
      }	
      if (yieldSum) {
	content = vSum / yieldSum;
	error   = sqrt(err2Sum) / yieldSum;
      }
      if (j==0) yieldPt[0]->SetBinContent(xBin, yieldSum);
      vPt[0][j]->SetBinContent(xBin, content);
      vPt[0][j]->SetBinError(xBin, error);
    }
  }

  // fill the projections for two centralities at a time (cent = 7, 8, 9) ----
  int nOut = 6;
  for (int nLow = 1; nLow < nCens; nLow+=2) {
    int nUp  = nLow + 1;
    nOut++;
    for (int j = 0; j < nHars; j++) {
      for (int xBin=1; xBin<=xBins; xBin++) {
	yieldSum = 0.;
	vSum     = 0.;
	err2Sum  = 0.;
	content  = 0.;
	error    = 0.;
	for (int n = nLow; n <= nUp; n++) {
	  v     = vY[n][j] ->GetBinContent(xBin);
	  yield = yieldY[n]->GetBinContent(xBin);
	  if(v != 0.0) {
	    yieldSum += yield;
	    vSum     += yield * v;
	    err2Sum  += pow(yield * vY[n][j]->GetBinError(xBin), 2.);
	  }
	}	
	if (yieldSum) {
	  content = vSum / yieldSum;
	  error   = sqrt(err2Sum) / yieldSum;
	}
	if (j==0) yieldY[nOut]->SetBinContent(xBin, yieldSum);
	vY[nOut][j]->SetBinContent(xBin, content);
	vY[nOut][j]->SetBinError(xBin, error);
      }
    
      for (int xBin=1; xBin<=yBins; xBin++) {
	yieldSum = 0.;
	vSum     = 0.;
	err2Sum  = 0.;
	content  = 0.;
	error    = 0.;
	for (int n = nLow; n <= nUp; n++) {
	  v     = vPt[n][j] ->GetBinContent(xBin);
	  yield = yieldPt[n]->GetBinContent(xBin);
	  if(v != 0.0) {
	    yieldSum += yield;
	    vSum     += yield * v;
	    err2Sum  += pow(yield * vPt[n][j]->GetBinError(xBin), 2.);
	  }
	}	
	if (yieldSum) {
	  content = vSum / yieldSum;
	  error   = sqrt(err2Sum) / yieldSum;
	}
	if (j==0) yieldPt[nOut]->SetBinContent(xBin, yieldSum);
	vPt[nOut][j]->SetBinContent(xBin, content);
	vPt[nOut][j]->SetBinError(xBin, error);
      }
    }
  }

  // fill the centrality histograms ---------------------------
  for (int j = 0; j < nHars; j++) {
    for (int n = 1; n <= nCens; n++) {
      yieldSum = 0.;
      vSum     = 0.;
      err2Sum  = 0.;
      content  = 0.;
      error    = 0.;
      for (int xBin=1; xBin<=yBins; xBin++) {
	v     = vPt[n][j] ->GetBinContent(xBin);
	yield = yieldPt[n]->GetBinContent(xBin);
	if(v != 0.0) {
	  yieldSum += yield;
	  vSum     += yield * v;
	  err2Sum  += pow(yield * vPt[n][j]->GetBinError(xBin), 2.);
	}
      }
      if (yieldSum) {
	content = vSum / yieldSum;
	error   = sqrt(err2Sum) / yieldSum;
      }
      if (j==0) yieldCen->SetBinContent(n, yieldSum);
      vCen[j]->SetBinContent(n, content);
      vCen[j]->SetBinError(n, error);
    }
  }
  
  // fill the triply integrated histogram ---------------------------
  for (int j = 0; j < nHars; j++) {
    yieldSum = 0.;
    vSum     = 0.;
    err2Sum  = 0.;
    content  = 0.;
    error    = 0.;
    for (int n = 1; n <= nCens; n++) {
      v     = vCen[j] ->GetBinContent(n);
      yield = yieldCen->GetBinContent(n);
      if(v != 0.0) {
	yieldSum += yield;
	vSum     += yield * v;
	err2Sum  += pow(yield * vCen[j]->GetBinError(n), 2.);
      }
    }
    if (yieldSum) {
      content = vSum / yieldSum;
      error   = sqrt(err2Sum) / yieldSum;
    }
    cout<<" Min Bias v"<<j+1<<" = "<<content<<" +/- "<<error<<endl<<endl;
    vMB->SetBinContent(j+1, content);
    vMB->SetBinError(j+1, error);
  }
  
  // save Histograms to file  
  TFile *file = new TFile(outFile,"RECREATE");
  file->cd();
  for (int j = 0; j < nHars; j++) {
    for (int n = 0; n < nPlots; n++) {
      vY[n][j] ->Write();
      vPt[n][j]->Write();
    }
    vCen[j]->Write();
  }
  file->Close();
  cout << "file closed" << endl << endl;
  
  //return;
  
  // plots
  TLine* lineZeroY   = new TLine(xMin, 0., xMax, 0.);
  TLine* lineZeroPt  = new TLine(yMin, 0., yMax, 0.);
  TLine* lineZeroCen = new TLine(0.5, 0., nCens+0.5, 0.);
  TLine* lineZeroHar = new TLine(0.5, 0., nHars+0.5, 0.);
  TLine* lineYcm     = new TLine(yCM, vYMin, yCM, vYMax);
  
  const char* tmp;
  tmp = yieldY[1]->GetName();
  tmp[strlen(tmp) - 5] = '\0';   // truncate
  NewCanvas(tmp);
  float max;
  TLine* lineYield;
  for (int n = 1; n <= nCens; n++) {
    graphPad->cd(n);
    sprintf(histTitle,"Centrality %d",n);
    cout << "centrality= " << n << endl;
    yieldY[n]->SetTitle(histTitle);
    yieldY[n]->Draw();
    max = 1.05 * yieldY[n]->GetMaximum();
    lineYield = new TLine(yCM, 0., yCM, max);
    lineYield->Draw();
  }
  if (!Pause()) return;
  delete lineYield;
  
  tmp = yieldPt[1]->GetName();
  tmp[strlen(tmp) - 5] = '\0';   // truncate
  NewCanvas(tmp);
  for (int n = 1; n <= nCens; n++) {
    graphPad->cd(n);
    gPad->SetLogy(1);
    sprintf(histTitle,"Centrality %d",n);
    cout << "centrality= " << n << endl;
    yieldPt[n]->SetTitle(histTitle);
    yieldPt[n]->Draw();
  }
  if (!Pause()) return;
  gPad->SetLogy(0);
  
  for (int j = 0; j < nHars; j++) {
    NewCanvas(v2D[1][j]->GetName());
    for (int n = 0; n < nCens; n++) {
      graphPad->cd(n+1);
      sprintf(histTitle,"Centrality %d",n+1);
      cout << "centrality= " << n+1 << endl;
      v2D[n][j]->SetTitle(histTitle);
      v2D[n][j]->Draw("COLZ");
    }
    if (!Pause()) return;
    
    tmp = vY[1][j]->GetName();
    tmp[strlen(tmp) - 5] = '\0';   // truncate
    NewCanvas(tmp);
    for (int n = 1; n <= nCens; n++) {
      graphPad->cd(n);
      sprintf(histTitle,"Centrality %d",n);
      cout << "centrality= " << n << endl;
      vY[n][j]->SetTitle(histTitle);
      vY[n][j]->Rebin();
      vY[n][j]->Scale(0.5);
      vY[n][j]->SetXTitle("Rapidity");
      vY[n][j]->Draw();
      lineZeroY->Draw();
      lineYcm  ->Draw();
    }
    if (!Pause()) return;
    
    tmp = vPt[1][j]->GetName();
    tmp[strlen(tmp) - 5] = '\0';   // truncate
    NewCanvas(tmp);
    for (int n = 1; n <= nCens; n++) {
      graphPad->cd(n);
      sprintf(histTitle,"Centrality %d",n);
      cout << "centrality= " << n << endl;
      vPt[n][j]->SetTitle(histTitle);
      vPt[n][j]->Rebin();
      vPt[n][j]->Scale(0.5);
      vPt[n][j]->SetXTitle("Pt (GeV)");
      vPt[n][j]->Draw();
      lineZeroPt->Draw();
    }
    if (!Pause()) return;
  }
  
  // min. bias plots
  NewCanvas(yieldY[0]->GetName());
  graphPad->cd(1);
  yieldY[0] ->Draw();
  float max = 1.05 * yieldY[0]->GetMaximum();
  TLine* lineYield = new TLine(yCM, 0., yCM, max);
  lineYield ->Draw();
  
  graphPad->cd(2);
  gPad->SetLogy(1);
  yieldPt[0]->Draw();
  
  for (int j = 0; j < nHars; j++) {
    graphPad->cd(3+j);
    vY[0][j] ->Draw();
    lineZeroY->Draw();
    lineYcm  ->Draw();
    
    graphPad->cd(5+j);
    vPt[0][j] ->Draw();
    lineZeroPt->Draw();
  }
  if (!Pause()) return;
  delete lineYield;
  
  // plots summed two a time
  TLine* lineYield;
  for (n = nCens+1; n < nPlots; n++) {
    NewCanvas(yieldY[n]->GetName());
    graphPad->cd(1);
    yieldY[n]->Draw();
    max = 1.05 * yieldY[n]->GetMaximum();
    lineYield = new TLine(yCM, 0., yCM, max);
    lineYield->Draw();
    
    graphPad->cd(2);
    gPad->SetLogy(1);
    yieldPt[n]->Draw();
    
    for (int j = 0; j < nHars; j++) {
      graphPad->cd(3+j);
      vY[n][j]->Draw();
      lineZeroY->Draw();
      lineYcm  ->Draw();
      
      graphPad->cd(5+j);
      vPt[n][j] ->Draw();
      lineZeroPt->Draw();
    }
    if (!Pause()) return;
  }
  delete lineYield;
  
  // centrality plots
  NewCanvas(yieldCen->GetName());
  yieldCen->Draw();
  if (!Pause()) return;
  
  for (int j = 0; j < nHars; j++) {
    NewCanvas(vCen[j]->GetName());
    vCen[j]->Draw();
    lineZeroCen->Draw();
    if (!Pause()) return;
  }
  
  // triply integrated plot
  NewCanvas(vMB->GetName());
  vMB->Draw();
  lineZeroHar->Draw();
  if (!Pause()) return;
  
}


void NewCanvas(char* canvasName) {

  // delete old canvas
  TCanvas* cOld = (TCanvas*)gROOT->GetListOfCanvases();
  if (cOld) cOld->Delete();
    
  // make new canvas
  can = new TCanvas(canvasName, canvasName, 600, 780);
  can->ToggleEventStatus();
  TPaveLabel* title = new TPaveLabel(0.1,0.96,0.9,0.99,canvasName);
  title->Draw();
  TPaveLabel* run = new TPaveLabel(0.1,0.01,0.2,0.03,runName);  
  run->Draw();
  TDatime now;
  TPaveLabel* date = new TPaveLabel(0.7,0.01,0.9,0.03,now->AsString());
  date->Draw();
  graphPad = new TPad("Graphs","Graphs",0.01,0.05,0.97,0.95);
  graphPad->Draw();
  graphPad->cd();
  graphPad->Divide(2,nCens/2);
  cout << canvasName << endl;

  return;
}


bool Pause() {
  can->Update();
  cout << "save? y/[n], quit? q" << endl;
  fgets(temp, sizeof(temp), stdin);
  if (strstr(temp,"y")!=0) can->Print(".ps");
  else if (strstr(temp,"q")!=0) return kFALSE;

  return kTRUE;
}

//-----------------------------------------------------------------------

static Double_t resEventPlane(double chi) {
  // Calculates the event plane resolution as a function of chi

  double con = 0.626657;                   // sqrt(pi/2)/2
  double arg = chi * chi / 4.;

  Double_t res = con * chi * exp(-arg) * (TMath::BesselI0(arg) + 
					  TMath::BesselI1(arg)); 

  return res;
}

//-----------------------------------------------------------------------

static Double_t chi(double res) {
  // Calculates chi from the event plane resolution

  double chi   = 2.0;
  double delta = 1.0;

  for (int i = 0; i < 15; i++) {
    chi = (resEventPlane(chi) < res) ? chi + delta : chi - delta;
    delta = delta / 2.;
  }

  return chi;
}

//-----------------------------------------------------------------------

static Double_t F(double chi) {
  // Calculates the F function for momentum conservation as a function of chi_JYO

  double sqrtPiOver2 = 0.886;      // sqrt(pi) / 2
  double chi2 = chi * chi;

  Double_t F = sqrtPiOver2 * exp(-chi2/2.) * ((1. + chi2) * TMath::BesselI0(chi2/2.) + chi2 * TMath::BesselI1(chi2/2.)) - chi; 

  return F;
}

//-----------------------------------------------------------------------
