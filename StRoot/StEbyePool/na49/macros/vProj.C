///////////////////////////////////////////////////////////////////////////////
//
// $Id: vProj.C,v 1.13 2003/01/24 23:10:26 posk Exp $
//
// Author:       Art Poskanzer, May 2000
// Description:  Projects v(y,pt) on the y and Pt axes
//                 with yield or cross section weighting.
//               Also makes the min. bias histograms
//                 and v as a function of centrality
//                 from these projections.
//               Saves output histograms to a file.
//               The cross sections come from dNdydPt.C times
//                 the fraction of events at each centrality.
//               Min. bias only sums centralities 1 to 5
//                 and uses the fraction of geometric cross section.
//               Centrality = 0 is min. bias
//               Centralites 7, 8, and 9 are the 
//                 combined 1-2, 3-4, and 5-6 centralities.
//               Makes momentum conserevation correction if pCons is TRUE.
//               Calculates <px> if pTFlow is TRUE.
//
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
  
  //int   eBeam = 158; // select full beam energy
  int   eBeam = 40;  // select 40Gev beam energy

  bool  pion = kFALSE;
  if (strcmp(part, "pion")==0) pion = kTRUE;

  bool  crossSection = kTRUE;
  //bool  crossSection = kFALSE;       // use yield weighting
  bool  pCons = kTRUE;               // do momentum consevation corr.
  //bool  pCons = kFALSE;
  //bool  pTFlow = kTRUE;      // calculate Flow weighted with pt
  bool  pTFlow = kFALSE;

  const int nPlots = 1 + nCens + nCens/2;
  const int nHars = 2;
  int       sel   = 2;
  double    sqrtPiOver2 = sqrt(TMath::Pi()) / 2.;
  double    sqrt2 = sqrt(2.);

  if (eBeam == 158) {
    double yCM    = 2.92;
    float  yLow   = yCM;              // for pt proj.
    float  yLowY  = 2.;               // for y proj.
    float  yUp    = 5.;               // for both projs.
    float  ptUp   = 2.;               // for both projs.
    float  vYMax  = 7.;
    float  vYMin  = -5.;
    float  vPtMax = 20.;
    float  vPtMin = -10.;
    float  sumPt2All[nCens] = {773.,623.,455.,305.,203.,122.}; // from Glenn
    float  meanMul[nCens] = {119.,181.,154.,110.,78.,46.}; // for har 1
    float  meanPt[nCens] = {0.166,0.254,0.293,0.288,0.284,0.279}; // for har 1
    Double_t fracEvents[nCens] = {0.231, 0.189, 0.134, 0.134, 0.115, 0.197};
  } else if (eBeam == 40) {
    double yCM    =  2.24;
    float  yLow   =  yCM;              // for pt proj.
    float  yLowY  =  1.;               // for y proj.
    float  yUp    =  4.;               // for both projs.
    float  ptUp   =  2.;               // for both projs.
    float  vYMax  =  5.;
    float  vYMin  = -5.;
    float  vPtMax =  5.;
    float  vPtMin = -5.;
    float  sumPt2All[nCens] = {579.,467.,344..,231.,155.,94.}; // from Glenn
    float  meanMul[nCens] = {89.3,76.4,59.5,42.2,29.7,17.2}; // for har 1
    float  meanPt[nCens] = {0.284,0.281,0.273,0.270,0.265,0.258}; // for har 1
    Double_t fracEvents[nCens] = {0.084, 0.127, 0.189, 0.175, 0.168, 0.258};
  } else {
    cout << " Not valid beam energy" << endl;
    return;
  }
  Double_t fracGeom[nCens]   = {0.05, 0.075, 0.11, 0.10, 0.10, 0.57};
 
  TFile*    anaFile[nCens];
  TH2*      v2D[nCens][nHars];
  TH2*      vObs2D[nCens][nHars];
  TH1*      resHist[nCens];
  TH2*      yieldPartHist[nCens];
  TH1F*     vY[nPlots][nHars];
  TH1F*     vPt[nPlots][nHars];
  TH1F*     vCen[nHars];
  TH1F*     _v;
  TH1F*     yieldCen;
  TH1F*     yieldY[nPlots];
  TH1F*     yieldPt[nPlots];
  TString*  histName;
  char      histTitle[30];
  char      fileName[30];
  int       firstRunNumber = 0;
  TAxis*    xAxis;
  TAxis*    yAxis;
  int       xBins;
  int       yBins;
  double    y;
  double    pt;
  double    yield;
  double    yieldReal;
  double    yieldSum;
  double    v;
  double    vErr;
  double    vSum;
  double    err2Sum;
  float     content;
  float     error;
  char      selText[2];
  char      cenText[2];
  char      harText[2];
    
  // set style
  gROOT->SetStyle("Bold");                              
  gROOT->ForceStyle();
  gStyle->SetOptStat(0);

  // load the cross section macro
  if (crossSection) {
    gROOT->LoadMacro("dNdydPt.C");
  }

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
  if (pCons) outName->Append("Pcons");
  if (!crossSection) outName->Append("Yield");
  outName->Append(".root");
  char outFile[255] = outName->Data();
  cout << " out file = " << outFile << endl;
  delete outName;

  // convert integer to text
  sprintf(selText, "%d", sel);

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
    int cent = runNumber % 10;                             // unit digit = cent
    if (cent <= 0 || cent > nCens) {
      cout << " Not valid centrality = " << cent << endl;
      return;
    }
    sprintf(cenText, "%d", cent);
    
    // calculate frac[n] for momentum conservation correction
    // assuming weights of all particles used for event plane are +1 (forward hemisphere)
    if (pCons) {
      double frac        = sqrt(meanMul[n] / sumPt2All[n]) * meanPt[n];
      double frac2       = frac * frac;
      double fracFact    = frac / sqrt(1. - frac2);
      double fullSubFact = sqrt((2. - frac2) / (1. - frac2));
      cout << " fraction= " << frac << ", fraction factor = " << fracFact 
	   << ", sub-to-full-fact/sqrt(2) = " << fullSubFact/sqrt2 << endl;
    }
    
    // get the resolution histogram
    histName = new TString("Flow_Res_Sel");
    histName->Append(*selText);
    resHist[n] = dynamic_cast<TH1*>(anaFile[n]->Get(histName->Data()));
    if (!resHist[n]) {
      cout << "### Can't find histogram " << histName->Data() << endl;
      return;
    }
    delete histName;
    

    for (int j = 0; j < nHars; j++) {
      // get the 2D v histograms
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
      v2D[n][j]->SetMaximum(20.);
      v2D[n][j]->SetMinimum(-20.);
      if (n==0 && j==0) {
	yAxis = v2D[0][0]->GetYaxis();
	yBins = yAxis->GetNbins();
	xAxis = v2D[0][0]->GetXaxis();
	xBins = xAxis->GetNbins();
      }

      // Conservation of momentum correction for v1     
      if (pCons && j==0) {

	// get the 2D vObs histograms
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

	// get the resolution and calculate chi
	double res = resHist[n]->GetBinContent(j+1);
	double chi = chi(res);
	double chiSub = chi / sqrt(2.);
	double resCorr;
	double chiFactCorr;
	if (chiSub < 0.1 && frac < 0.1) {
	  // approximate solution
	  double chiApprox = sqrt(chi*chi + frac2);
	  double resCorr = resEventPlane(chiApprox);
	  double chiFactCorr = F(chiApprox);
	  cout << " chiApprox= " << chiApprox << ", corr.= " <<
	    chiApprox/chi << endl;
	  cout << " resApprox= " << resCorr << ", corr.= " << resCorr/res << endl;
	  cout << " F approx= " << chiFactCorr << ", corr.= " <<
	    chiFactCorr/sqrtPiOver2 << endl;
	} else {
	  // starting from resSub
	  double resSub = resEventPlane(chiSub);
	  double chiSubCorr = chi(resSub, frac);
	  double chiCorr = chiSubCorr * fullSubFact;
	  resCorr = resEventPlane(chiCorr);
	  chiFactCorr = F(chiCorr);
	  cout << " chiCorr= " << chiCorr << ", corr.= " << 
	    chiCorr/chi << endl;
	  cout << " resCorr= " << resCorr << ", corr.= " << resCorr/res << endl;
	  cout << " F corr= " << chiFactCorr << ", corr.= " << chiFactCorr/sqrtPiOver2
	       << endl;
	}

	for (int yBin=1; yBin<=yBins; yBin++) {
	  pt = yAxis->GetBinCenter(yBin);
	  for (int xBin=1; xBin<=xBins; xBin++) {
	    v = vObs2D[n][j]->GetCellContent(xBin, yBin);
	    vErr = vObs2D[n][j]->GetCellError(xBin, yBin);
	    v += 100. * chiFactCorr * pt * fracFact / sqrt(sumPt2All[n]);
	    v2D[n][j]->SetCellContent(xBin, yBin, v);
	    v2D[n][j]->SetCellError(xBin, yBin, vErr);
	  }
	}
	if (resCorr != 0.) {
	  v2D[n][j]->Scale(1. / resCorr);
	} else {
	  cout << "resolution of the " << j+1 << "th harmonic was zero." << endl;
	  v2D[n][j]->Reset();
	}
      }
    }
    
    // get the 2D yield histograms
    yieldPartHist[n] = dynamic_cast<TH2*>(anaFile[n]->Get("Flow_YieldPart2D"));
    if (!yieldPartHist[n]) {
      cout << "### Can't find yield part histogram Flow_YieldPart2D"
	   << endl;
      return;
    }

    // get the histograms to print the number of events and <Eveto>.
    TH1* multHist = dynamic_cast<TH1*>(anaFile[n]->Get("Flow_Mult"));
    if (!multHist) {
      cout << "### Can't find histogram Flow_Mult"<< endl;
      return;
    }
    Float_t events = multHist->GetEntries();

    TH1* vetoHist = dynamic_cast<TH1*>(anaFile[n]->Get("Flow_EVeto"));
    if (!vetoHist) {
      cout << "### Can't find histogram Flow_EVeto"<< endl;
      return;
    }
    Float_t vetoMean = vetoHist->GetMean();

    // get the resolution of the second harmonic event plane.
    Float_t res2 = resHist[n]->GetBinContent(2);

    // get the histograms to print <M> and <pt> for momentum conservation corr.
    TH1* mulHist = dynamic_cast<TH1*>(anaFile[n]->Get("Flow_Mul_Sel2_Har1"));
    if (!multHist) {
      cout << "### Can't find histogram Flow_Mul_Sel2_Har1"<< endl;
      return;
    }
    Float_t meanMult = mulHist->GetMean();

    TH2* yieldHist = dynamic_cast<TH2*>(anaFile[n]->Get("Flow_Yield2D_Sel2_Har1"));
    if (!yieldHist) {
      cout << "### Can't find histogram Flow_Yield2D_Sel2_Har1"<< endl;
      return;
    }
    TH1* ptHist = yieldHist->ProjectionY("pt", 0, 9999, "E");
    Float_t ptMean  = ptHist->GetMean();

    cout << " events= " << events << " EVeto= " << vetoMean 
	 << " mult= " << meanMult << " pt= " << ptMean 
	 << " res. har. 2= " << res2 << endl;

  }
  // get the axes properties
  float  xMin  = xAxis->GetXmin();
  float  xMax  = xAxis->GetXmax();
  float  yMin  = yAxis->GetXmin();
  float  yMax  = yAxis->GetXmax();
 
  cout << "Please wait" << endl << endl;

  for (int n = 0; n < nPlots; n++) {
    sprintf(cenText, "%d", n);

    // create 1D Yield histograms
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

      // create the 1D v histograms
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
      
  // create the histograms for v as a function of centrality
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

  // create the histogram for the triply integrated v
  histName = new TString("Flow__v_Sel");
  histName->Append(*selText);
  _v = new TH1F(histName->Data(), histName->Data(), nHars, 0.5, 
		 nHars+0.5);
  _v->SetXTitle("Harmonic");
  _v->SetYTitle("Flow (%)");
  _v->SetMaximum(3.);
  _v->SetMinimum(-2.);
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
	      yieldReal = yieldPartHist[n-1]->GetCellContent(xBin, yBin);
	      if (crossSection) {
		if (pion) {
		  yield = dNdydPt(0, y - yCM, pt, n, eBeam) +
		    dNdydPt(1, y - yCM, pt, n, eBeam);               // pi+ + pi-
		} else {
		  yield = dNdydPt(4, y - yCM, pt, n, eBeam);         // p
		}
		yield *= fracEvents[n-1];
	      } else {
		yield = yieldReal;
	      }
	      v = v2D[n-1][j]->GetCellContent(xBin, yBin);
	      if (v != 0.0 && yieldReal > 1.) { // error is wrong for one count
		yieldSum += yield;
		if (pTFlow) {
		  vSum     += yield * v *pt;
		  err2Sum  += pow(pt * yield * v2D[n-1][j]->GetCellError(xBin, yBin), 
				  2.);
		} else {
		  vSum     += yield * v;
		  err2Sum  += pow(yield * v2D[n-1][j]->GetCellError(xBin, yBin), 
				  2.);
                }
	      }
	    }
	  }
	  if (yieldSum) {
	    content = vSum / yieldSum;
	    error   = sqrt(err2Sum) / yieldSum;
	  }
	  if(j != 1 || n != 1 || eBeam != 40) // 40 GeV v2 cen 1 res. zero
	    {yieldY[n]->SetBinContent(xBin, yieldSum);}
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
	    if ((j+1) % 2 && y < yCM) v *= -1; // backward particles for odd har
	    yieldReal = yieldPartHist[n-1]->GetCellContent(xBin, yBin);
	    if (crossSection) {
	      if (pion) {
		yield = dNdydPt(0, y - yCM, pt, n, eBeam) +
		  dNdydPt(1, y - yCM, pt, n, eBeam);                 // pi+ + pi-
	      } else {
		yield = dNdydPt(4, y - yCM, pt, n, eBeam);           // p
	      }
	      yield *= fracEvents[n-1];
	    } else {
	      yield = yieldReal;
	    }
	    if (v != 0.0 && yieldReal > 1.) { // error is wrong for one count
	      yieldSum += yield;
	      if (pTFlow) {
		vSum += yield * v *pt;
	      } else {
		vSum += yield * v;
	      }
	      err2Sum += pow(yield * v2D[n-1][j]->GetCellError(xBin, yBin), 2.);
	    }
	  }
	}
	if (yieldSum) {
	  content = vSum / yieldSum;
	  error   = sqrt(err2Sum) / yieldSum;
          if (pTFlow) error *= pt;
	}
	if(j != 1 || n != 1 || eBeam != 40) // 40 GeV v2 cen 1 res. zero
	  {yieldPt[n]->SetBinContent(yBin, yieldSum);}
	vPt[n][j]->SetBinContent(yBin, content);
	vPt[n][j]->SetBinError(yBin, error);
      }
    }
  }
  
  // fill the min. bias projections (cen = 0) --------------------------
  // only for centralities 1 to 5
  // but using the fraction of geometric cross section
  for (int j = 0; j < nHars; j++) {
    for (int xBin=1; xBin<=xBins; xBin++) {
      yieldSum = 0.;
      vSum     = 0.;
      err2Sum  = 0.;
      content  = 0.;
      error    = 0.;
      for (int n = 1; n < nCens; n++) {
	v     = vY[n][j] ->GetBinContent(xBin);
	yield = (yieldY[n]->GetBinContent(xBin)) * fracGeom[n-1]/fracEvents[n-1];
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
      for (int n = 1; n < nCens; n++) {
	v     = vPt[n][j] ->GetBinContent(xBin);
	yield = yieldPt[n]->GetBinContent(xBin) * fracGeom[n-1]/fracEvents[n-1];
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
      if(j != 1 || n != 1 || eBeam != 40)  // 40 GeV v2 cen 1 res. zero
	yieldPt[0]->SetBinContent(xBin, yieldSum);
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
  // only for centralities 1 to 5
  // but using the fraction of geometric cross section
  for (int j = 0; j < nHars; j++) {
    yieldSum = 0.;
    vSum     = 0.;
    err2Sum  = 0.;
    content  = 0.;
    error    = 0.;
    for (int n = 1; n < nCens; n++) {
      v     = vCen[j] ->GetBinContent(n);
      yield = (yieldCen->GetBinContent(n)) * fracGeom[n-1]/fracEvents[n-1];
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
    _v->SetBinContent(j+1, content);
    _v->SetBinError(j+1, error);
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
  NewCanvas(_v->GetName());
  _v->Draw();
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
  // Calculates the event plane resolution as a function of chiJYO

  double con = sqrt(TMath::Pi() / 2.) / 2.;
  chi *= sqrt(2.);
  double arg = chi * chi / 4.;
  
  Double_t res = con * chi * exp(-arg) * (TMath::BesselI0(arg) + 
					  TMath::BesselI1(arg)); 

  return res;
}

//-----------------------------------------------------------------------

static Double_t chi(double res) {
  // Calculates chiJYO from the event plane resolution

  double chi   = 2.0;
  double delta = 1.0;

  for (int i = 0; i < 15; i++) {
    chi = (resEventPlane(chi) < res) ? chi + delta : chi - delta;
    delta = delta / 2.;
  }

  return chi;
}

//-----------------------------------------------------------------------

static Double_t resEventPlane(double chi, double f) {
  // Calculates the subevent plane resolution as a function of chi_subJYO
  // and the fraction f

  double con = sqrt(TMath::Pi()) / 2.;
  double arg = chi * chi / 2.;
  
  Double_t I0 = TMath::BesselI0(arg);
  Double_t I1 = TMath::BesselI1(arg);
  Double_t term1 = chi * chi * (I0 + I1) * (I0 + I1);
  Double_t term2 = (f * f / 2.) * ((I0 * I0)  + (I1 * I1));
  Double_t res =  con * exp(-arg) * sqrt(term1 - term2);

  return res;
}

//-----------------------------------------------------------------------

static Double_t chi(double res, double f) {
  // Calculates chi_subJYO from the subevent plane resolution
  // including momentum conservatiuon

  double chi   = 2.0;
  double delta = 1.0;

  for (int i = 0; i < 15; i++) {
    chi = (resEventPlane(chi, f) < res) ? chi + delta : chi - delta;
    delta = delta / 2.;
  }

  return chi;
}

//-----------------------------------------------------------------------

static Double_t F(double chi) {
  // Calculates the F function for momentum conservation as a function of chi_JYO
  // The corr. = -pt * F * fracFact / sumPt2All

  double sqrtPiOver2 = sqrt(TMath::Pi()) / 2.;
  double chi2Over2   = chi*chi/2.;

  Double_t F = sqrtPiOver2 * exp(-chi2Over2) * TMath::BesselI0(chi2Over2); 

  return F;
}

//-----------------------------------------------------------------------

///////////////////////////////////////////////////////////////////////////
//
// $Log: vProj.C,v $
// Revision 1.13  2003/01/24 23:10:26  posk
// For version 37 of the paper.
//
// Revision 1.11  2002/11/15 22:35:17  posk
// Require > 1 count/bin. Use number of events or frac. of crosssection when combining centralities.
//
// Revision 1.10  2002/03/26 17:48:42  posk
// Corrected sqrt(2) mistake.
//
// Revision 1.9  2002/03/23 21:46:20  posk
// More 40 GeV compatability.
//
// Revision 1.8  2002/01/16 18:21:49  posk
// Fit q in plot.C. Updated momentum conservation corr. in vProj.C.
//
// Revision 1.7  2001/11/06 18:02:54  posk
// 40 GeV compatability.
//
// Revision 1.6  2001/10/24 21:46:36  posk
// Added conservation of momentum correction. Calculate triply integrated v values.
//
// Revision 1.4  2001/05/14 23:20:29  posk
// Uses cross section weighting for all projections.
//
// Revision 1.3  2001/03/16 22:35:10  posk
// plotGraphs.C makes the final graphs.
//
// Revision 1.2  2001/03/06 17:33:04  posk
// All macros now work.
//
// Revision 1.1  2001/02/23 00:58:19  posk
// NA49 version of STAR software.
//
///////////////////////////////////////////////////////////////////////////////
