///////////////////////////////////////////////////////////////////////////////
//
// $Id: minBias.C,v 1.11 2004/03/01 22:43:42 posk Exp $
//
// Author:       Art Poskanzer and Alexander Wetzler, Mar 2001
//                 Kirill Filimonov treated the one count case
// Description:  Macro to add histograms together.
//               The v histograms will be added with yield weighting.
//               First file anaXX.root given by first run number XX.
//               Output file given by output run number.
//
//
///////////////////////////////////////////////////////////////////////////////
#ifndef __CINT__
#include <fstream.h>
#include "TSystem.h"
#include <TFile.h>
#include "TH1.h"
#include "TH2.h"
#include "TProfile.h"
#include "TKey.h"
#include "TObject.h"
#endif

void minBias(Int_t firstRunNo, Int_t outputRunNo=99) {

  const  int nCens = 9;
  int    nSels = 2;
  const  int nHars = 4;
  char   fileName[80];
  TFile* histFile[nCens+1];
  TH1*   hist[nCens+1];
  TH2*   yieldPartHist[nCens];
  
  // names of histograms to be added with weighting
  const char* baseName[] = { 
    "Flow_Res_",
    "Flow_v2D_",   // must be first
    "Flow_vEta_",
    "Flow_vPt_",
    "Flow_v_",
    "Flow_v2D_ScalarProd_",
    "Flow_vEta_ScalarProd_",
    "Flow_vPt_ScalarProd_",
    "Flow_v_ScalarProd_",
    "Flow_Cumul_vEta_Order2_",
    "Flow_Cumul_vPt_Order2_",
    "Flow_Cumul_v_Order2_",
    "Flow_Cumul_vEta_Order4_",
    "Flow_Cumul_vPt_Order4_",
    "Flow_Cumul_v_Order4_"
  };
  const int nNames = sizeof(baseName) / sizeof(char*);

  // open the files
  for (int n = 0; n < nCens; n++) {
    sprintf(fileName, "ana%2d.root", firstRunNo + n);
    cout << " file name = " << fileName << endl;
    histFile[n] = new TFile(fileName);
    if (!histFile[n]) {
      cout << "### Can't find file " << fileName << endl;
      return;
    }
  }
  sprintf(fileName, "ana%2d.root", outputRunNo);
  cout << " output file name = " << fileName << endl << endl;
  histFile[nCens] = new TFile(fileName, "RECREATE");
    
  // add all histograms with no weighting
  TKey*    key;
  TObject* obj;
  TIter nextkey(histFile[0]->GetListOfKeys());  
  while (key = (TKey*)nextkey()) {
    histFile[0]->cd();
    //key->ls();
    obj = key->ReadObj();
    const char* objName;
    if (obj->InheritsFrom("TH1")) { // TH1 or TProfile
      hist[0] = (TH1*)obj;
      objName = key->GetName();
      cout << "hist name= " << objName << endl;
      for (int n = 1; n < nCens; n++) {
	hist[1] = (TH1*)histFile[n]->Get(objName);
	hist[0]->Add(hist[1]);
      }
    }
    histFile[nCens]->cd();
    obj->Write(objName);
    delete obj;
    obj = NULL;
  }
   
  // get yield histogram
  cout<<endl;
  for (int n = 0; n < nCens; n++) {
    yieldPartHist[n] = dynamic_cast<TH2*>(histFile[n]->Get("Flow_YieldPart2D"));
    if (!yieldPartHist[n]) {
      cout << "### Can't find yield part histogram Flow_YieldPart2D"
	   << endl;
      return;
    }
  }

  for (int pageNumber = 0; pageNumber < nNames; pageNumber++ ) {
    bool twoD = kFALSE;
    if (strstr(baseName[pageNumber],"v2D")) twoD = kTRUE;

    for (int selN = 0; selN < nSels; selN++) {
      
      // no harmonics
      bool noHars = kFALSE;
      int nHar = nHars;
      if (strstr(baseName[pageNumber],"_v_")!=0 ||
	  strstr(baseName[pageNumber],"Res")!=0) {
	noHars = kTRUE;
	nHar = 1;
      } 
      for (int harN = 0; harN < nHar; harN++) {	
	
	// construct histName
	char sel[2];
	sprintf(sel,"%d",selN+1);
	char har[2];
	sprintf(har,"%d",harN+1);
	TString* histName = new TString(baseName[pageNumber]);
	histName->Append("Sel");
	histName->Append(*sel);
	if (!noHars) {	
	  histName->Append("_Har");
	  histName->Append(*har);
	}
	cout << "hist name= " << histName->Data() << endl;
		
	// get the histograms
	for (int n = 0; n < nCens+1; n++) {
	  hist[n] = dynamic_cast<TH1*>(histFile[n]->Get(histName->Data()));
	  if (!hist[n]) {
	    cout << "### Can't find histogram " << histName->Data() << endl;
	    //return;
	  }
	}

	const int lastHist = 3;
	int nBins;      // set by 2D of centrality lastHist
	if (!hist[lastHist]) continue;
	int xBins = hist[lastHist]->GetNbinsX();
	int yBins;
	TH1F *yieldY, *yieldPt;
	if (twoD) {
	  yBins = hist[lastHist]->GetNbinsY();
	  nBins = xBins + (xBins + 2) * yBins;
	  float yMax  = hist[lastHist]->GetXaxis()->GetXmax();
	  float yMin  = hist[lastHist]->GetXaxis()->GetXmin();
	  yieldY = new TH1F("Yield_Y", "Yield_Y", xBins, yMin, yMax);
	  yieldY->SetXTitle("Rapidity");
	  yieldY->SetYTitle("Counts");
	  float ptMax  = hist[lastHist]->GetYaxis()->GetXmax();
	  yieldPt = new TH1F("Yield_Pt", "Yield_Pt", yBins, 0., ptMax);
	  yieldPt->SetXTitle("Pt (GeV/c)");
	  yieldPt->SetYTitle("Counts");
	} else {
	  nBins = xBins + 2;
	}
	
	// loop over the bins
	if (strstr(baseName[pageNumber],"Res")) { // with error weighting
	  cout<<"  With error weighting"<<endl;
	  float content;
	  float error;
	  float errorSq;
	  float meanContent;
	  float meanError;
	  float weight;
	  for (int bin = 0; bin < nBins; bin++) {
	    meanContent = 0.;
	    meanError   = 0.;
	    weight      = 0.;
	    for (int n = 0; n < nCens; n++) {
	      if (hist[n]) {
		content = hist[n]->GetBinContent(bin);
		error   = hist[n]->GetBinError(bin);
		errorSq = error * error;
		if (errorSq > 0.) {
		  meanContent += content / errorSq;
		  weight      += 1. / errorSq;
		}
	      }
	    }
	    if (weight > 0.) {
	    meanContent /= weight;
	    meanError = sqrt(1. / weight);
	    hist[nCens]->SetBinContent(bin, meanContent);
	    hist[nCens]->SetBinError(bin, meanError);
	    }
	  }
	} else {                                   // with yield weighting
	  cout<<"  With yield weighting"<<endl;
	  float v;
	  float verr;
	  float vSum;
	  double vSum2;
	  float content;
	  float error;
	  float error2sum;
	  float yield;
	  float yieldSum;
	  float y;
	  float pt;
	  double vRms;
	  double verrRms;
	  double vSumRms;
	  double yieldSumRms;
	  double vSumRms2;
	  for (int bin = 0; bin < nBins; bin++) {
	    v         = 0.;
	    verr      = 0.;
	    vSum      = 0.;
	    vSum2     = 0.;
	    content   = 0.;
	    error     = 0.;
	    error2sum = 0.;
	    yield     = 0.;
	    yieldSum  = 0.;
	    vRms        = 0.;
	    verrRms     = 0.;
	    vSumRms     = 0.;
	    yieldSumRms = 0.;
	    vSumRms2    = 0.;
	    for (int n = 0; n < nCens; n++) {
	      if (hist[n]) {
		if (strstr(histName->Data(),"v2D")) {
		  yield = yieldPartHist[n]->GetBinContent(bin);
		} else if (strstr(histName->Data(),"vEta")) {
		  yield = yieldPartHist[n]->Integral(bin, bin, 1, yBins);
		  if (selN==0 && harN==0) {
		    y = yieldPartHist[n]->GetXaxis()->GetBinCenter(bin);
		    yieldY->Fill(y, yield);
		  }
		} else if (strstr(histName->Data(),"vPt")) {
		  yield = yieldPartHist[n]->Integral(1, xBins, bin, bin);
		  if (selN==0 && harN==0) {
		    pt = yieldPartHist[n]->GetYaxis()->GetBinCenter(bin);
		    yieldPt->Fill(pt, yield);
		  }
		} else {                                        // _v
		  yield = yieldPartHist[n]->Integral();
		}
		v = hist[n]->GetBinContent(bin);
		if(yield==1) { // special case to calculate the correct error
		  vSumRms     += v;
		  yieldSumRms += yield;
		  vSumRms2    += v*v;
		} else {
		  verr = hist[n]->GetBinError(bin);
		  if (v != 0 ) {
		    yieldSum  += yield;
		    vSum      += yield * v;
		    vSum2     += v * v * yield;
		    error2sum += pow(yield * verr, 2.);
		  }
		}
	      }
	    }
	    
	    if(yieldSumRms) vRms = vSumRms / yieldSumRms;
	    if(yieldSumRms>1) {
	      verrRms    = sqrt(vSumRms2 - vSumRms*vSumRms / yieldSumRms)
		/ yieldSumRms;
	      yieldSum  += yieldSumRms;
	      vSum      += vSumRms;
	      error2sum += pow(yieldSumRms * verrRms, 2.);
	      vSum2     += vRms * vRms * yieldSumRms;
	    }
	    
	    if (yieldSum) {
	      content = vSum / yieldSum;
	      if (yieldSumRms==1) {
		error = sqrt(error2sum + vSum2 - vSum*vSum / yieldSum) / yieldSum;
		error = yieldSum / (yieldSum+1) * sqrt(error*error + 
		   (vRms - content)*(vRms - content) / (yieldSum * (yieldSum+1)));
	      } else {
		error = sqrt(error2sum + vSum2 - vSum*vSum/yieldSum) / yieldSum;
	      }
	      hist[nCens]->SetBinContent(bin, content);
	      hist[nCens]->SetBinError(bin, error);
	    }
	  }
	} 
	delete histName;
      }
    }
  }
  
  //histFile[nCens]->ls();
  histFile[nCens]->Write(0, TObject::kOverwrite);
  histFile[nCens]->Close();
  delete histFile[nCens];
  
}

///////////////////////////////////////////////////////////////////////////////
//
// $Log: minBias.C,v $
// Revision 1.11  2004/03/01 22:43:42  posk
// Changed some "->" to ".".
//
// Revision 1.10  2003/08/26 21:10:12  posk
// Calculates v8 if nHars=8.
//
// Revision 1.9  2003/06/27 21:25:44  posk
// v4 and v6 are with repect to the 2nd harmonic event plane.
//
// Revision 1.8  2003/03/11 23:03:07  posk
// Includes scalar product and cumulant hists.
//
// Revision 1.7  2002/06/11 21:54:15  posk
// Kirill's further correction to minBias.C for bins with one count.
//
// Revision 1.6  2002/05/21 18:42:18  posk
// Kirill's correction to minBias.C for bins with one count.
//
// Revision 1.5  2001/11/09 21:15:00  posk
// Switched from CERNLIB to TMath. Using global dca instead of dca.
//
// Revision 1.4  2001/05/22 20:05:47  posk
// Now outputs a hist.root file.
// The v values are averaged with yield weighting.
//
// Revision 1.3  2000/09/29 22:53:17  posk
// More histograms.
//
// Revision 1.2  2000/09/26 20:54:11  posk
// Updated documentation.
//
// Revision 1.1  2000/09/26 00:19:43  posk
// New macro to add centrality-selected histograms with proper weights, to make
// minimum bias histogram.
//
//
///////////////////////////////////////////////////////////////////////////////
