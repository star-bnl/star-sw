///////////////////////////////////////////////////////////////////////////////
//
// $Id: addHists.C,v 1.8 2003/01/24 23:10:24 posk Exp $
//
// Author:       Art Poskanzer, Feb 2001
// Description:  Macro to add histograms together with weighting.
//               First file anaXX.root given by first run number XX.
//               Second file given by second run number.
//               Output file given by output run number.
//
//
///////////////////////////////////////////////////////////////////////////////
#include <iostream.h>

// // pions
// .x addHists.C(11,21,31,41) 
// .x addHists.C(12,22,32,42)
// .x addHists.C(13,23,33,43)
// .x addHists.C(14,24,44)
// .x addHists.C(15,25,45)
// .x addHists.C(16,26,46)

// // protons
// .x addHists.C(51,61,71,81)
// .x addHists.C(52,62,72,82)
// .x addHists.C(53,63,73,83)
// .x addHists.C(54,64,84)
// .x addHists.C(55,65,85)
// .x addHists.C(56,66,86)

// at 40 GeV there are no ana3 or ana7

Int_t addHists(Int_t firstRunNo, Int_t secondRunNo, Int_t thirdRunNo,
	       Int_t outputRunNo) {
  // Combines three files.
  Int_t tempRunNo = 100 + firstRunNo % 10;
  addHists(firstRunNo, secondRunNo, tempRunNo);
  addHists(tempRunNo, thirdRunNo, outputRunNo);

}

Int_t addHists(Int_t firstRunNo, Int_t secondRunNo, Int_t outputRunNo=99) {

  bool   stripes = kFALSE;
  //bool   stripes = kTRUE;
  TFile* histFile[3];
  char   firstRunName[30];
  char   secondRunName[30];
  char   outputRunName[30];
  int    nSels = 2;
  int    nHars = 3;
  TH1*   hist[2];
  TH2*   yieldPartHist[2];
  
  // names of histograms to be added with weighting
  const char* baseName[] = { 
    "Flow_Res",
    "Flow_v2D",   // must be first of the v
    "Flow_vY",
    "Flow_vPt",
    "Flow_v"
  };
  const int nNames = sizeof(baseName) / sizeof(char*);

  // add ana prefix and open the files
  sprintf(firstRunName, "ana%2d", firstRunNo);
  cout << " first run name  = " << firstRunName << endl;
  sprintf(firstRunName, "ana%2d.root", firstRunNo);
  histFile[0] = new TFile(firstRunName);
  
  sprintf(secondRunName, "ana%2d", secondRunNo);
  cout << " second run name = " << secondRunName << endl;
  sprintf(secondRunName, "ana%2d.root", secondRunNo);
  histFile[1] = new TFile(secondRunName);
  
  sprintf(outputRunName, "ana%2d", outputRunNo); 
  cout << " output run name = " << outputRunName << endl;
  sprintf(outputRunName, "ana%2d.root", outputRunNo);
  histFile[2] = new TFile(outputRunName, "RECREATE");
    
  // add all histograms with no weighting
  TKey*    key;
  TObject* obj;
  TIter nextkey(histFile[0]->GetListOfKeys());  
  while (key = (TKey*)nextkey()) {
    histFile[0]->cd();
    //key->ls();
    obj = key->ReadObj();
    if (obj->InheritsFrom("TH1")) { // TH1 or TProfile
      hist[0] = (TH1*)obj;
      char* objName = key->GetName();
      if (!stripes || strstr(objName,"Corr")
	  || strstr(objName,"Psi_Subs")
	  || strstr(objName,"Yield2D")
	  || strstr(objName,"YieldPart2D") 
	  || (strstr(objName,"Flow_Cos") && !strstr(objName,"Lab"))) {
	cout << " hist name= " << objName << endl;
	hist[1] = (TH1*)histFile[1]->Get(objName);
	hist[0]->Add(hist[1]);
      }
      histFile[2]->cd();
      obj->Write(objName);
      delete obj;
      obj = NULL;
    }
  }
  
  //get the yield hist
  cout<<endl<<"  With weighting"<<endl;
  for (int n = 0; n < 2; n++) {
    yieldPartHist[n] = dynamic_cast<TH2*>(histFile[n]->Get("Flow_YieldPart2D"));
    if (!yieldPartHist[n]) {
      cout << "### Can't find yield part histogram Flow_YieldPart2D"
	   << endl;
      return;
    }
  }

  // add histograms with error or yield weighting
  for (int pageNumber = 0; pageNumber < nNames; pageNumber++ ) {
    bool twoD = kFALSE;
    if (strstr(baseName[pageNumber],"v2D")) twoD = kTRUE;

    for (int selN = 0; selN < nSels; selN++) {
      
      // special cases
      Bool_t noHars = kFALSE;
      int nHar = nHars;
      if (strcmp(baseName[pageNumber],"Flow_v")==0 ||
	  strstr(baseName[pageNumber],"Res")) {
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
	histName->Append("_Sel");
	histName->Append(*sel);
	if (!noHars) {	
	  histName->Append("_Har");
	  histName->Append(*har);
	}
	cout << " hist name= " << histName->Data() << endl;
	
	// get the histograms
	hist[0] = dynamic_cast<TH1*>(histFile[0]->Get(histName->Data()));
	hist[1] = (TH1*)histFile[1]->Get(histName->Data());
	histOut = (TH1*)histFile[2]->Get(histName->Data());
	if (!hist[0]) {
	  cout << "### Can't find histogram " << histName->Data() << endl;
	  return;
	}
	int nBins;      // set by 2D
	int xBins = hist[0]->GetNbinsX();
	if (twoD) {
	  int yBins = hist[0]->GetNbinsY();
	  nBins = xBins + (xBins + 2) * yBins;
	} else {
	  nBins = xBins + 2;
	}
	
	// loop over the bins
	if (strstr(baseName[pageNumber],"Res")) {
	  // with error weighting
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
	    for (int n = 0; n < 2; n++) {
	      content = hist[n]->GetBinContent(bin);
	      error   = hist[n]->GetBinError(bin);
	      errorSq = error * error;
	      if (errorSq > 0.) {
		meanContent += content / errorSq;
		weight      += 1. / errorSq;
	      }
	    }
	    if (weight > 0.) {
	    meanContent /= weight;
	    meanError = sqrt(1. / weight);
	    histOut->SetBinContent(bin, meanContent);
	    histOut->SetBinError(bin, meanError);
	    }
	  }
	} else {
	  // with yield weighting
	  float v;
	  float verr;
	  float vSum;
	  float content;
	  float error;
	  float error2sum;
	  float yield;
	  float yieldSum;
	  for (int bin = 0; bin < nBins; bin++) {
	    v         = 0.;
	    verr      = 0.;
	    vSum      = 0.;
	    content   = 0.;
	    error     = 0.;
	    error2sum = 0.;
	    yield     = 0.;
	    yieldSum  = 0.;
	    for (int n = 0; n < 2; n++) {
	      if (strstr(histName->Data(),"v2D")) {
		yield   = yieldPartHist[n]->GetBinContent(bin);
	      } else if (strstr(histName->Data(),"vY")) {
		yield = yieldPartHist[n]->Integral(bin, bin, 1, yBins);
	      } else if (strstr(histName->Data(),"vPt")) {
		yield = yieldPartHist[n]->Integral(1, xBins, bin, bin);
	      } else {                                        // _v
		yield = yieldPartHist[n]->Integral();
	      }
	      v = hist[n]->GetBinContent(bin);
	      verr = hist[n]->GetBinError(bin);
	      if (v != 0. && yield > 1.) { // error is wrong for one count
		yieldSum  += yield;
		vSum      += yield * v;
		error2sum += pow(yield * verr, 2.);
	      }
	    }
	    if (yieldSum) {
	      content = vSum / yieldSum;
	      error = sqrt(error2sum) / yieldSum;
	    }
	    histOut->SetBinContent(bin, content);
	    histOut->SetBinError(bin, error);
	  }	  
	}
	delete histName;	
      }
    }
  }

  //histFile[2]->ls();
  histFile[2]->Write(0, TObject::kOverwrite);
  histFile[2]->Close();
  delete histFile[2];
  
}

///////////////////////////////////////////////////////////////////////////////
//
// $Log: addHists.C,v $
// Revision 1.8  2003/01/24 23:10:24  posk
// For version 37 of the paper.
//
// Revision 1.7  2002/11/15 22:38:14  posk
// updates.
//
// Revision 1.6  2002/03/23 21:45:51  posk
// More 40 GeV compatability.
//
// Revision 1.5  2001/08/17 22:14:40  posk
// Updated to also do 40 GeV.
//
// Revision 1.4  2001/05/14 23:18:22  posk
// Uses yield weighting.
//
// Revision 1.3  2001/03/16 22:34:59  posk
// plotGraphs.C makes the final graphs.
//
// Revision 1.2  2001/03/06 17:32:51  posk
// All macros now work.
//
// Revision 1.1  2001/02/26 23:07:01  posk
// Rearranged macros.
//
// Revision 1.1  2001/02/23 00:58:19  posk
// NA49 version of STAR software.
//
///////////////////////////////////////////////////////////////////////////////
