///////////////////////////////////////////////////////////////////////////////
//
// Author:       Art Poskanzer, Feb 2001
// Description:  Macro to add histograms together with weighting.
//               First file anaXX.root given by first run number XX.
//               Second file given by second run number.
//               Output file given by output run number.
//
//
///////////////////////////////////////////////////////////////////////////////

gROOT->Reset();

Int_t addStripes(Int_t firstRunNo, Int_t secondRunNo, Int_t outputRunNo=99) {

  TFile* histFile[3];
  char   firstRunName[6];
  char   firstRunName[30];
  char   secondRunName[6];
  char   secondRunName[30];
  char   outputRunName[6];
  char   outputRunName[30];
  int    nSels = 2;
  int    nHars = 6;
  //int    nHars = 2;
  
  // names of histograms to be added
  const char* baseName[] = { 
    "Flow_Cos",
    "Flow_Res",
    "Flow_Psi_Sub_Corr",
    "Flow_Psi_Sub_Corr_Diff",
    "Flow_Phi_Corr",
    "Flow_Yield2D",
    "Flow_v2D",
    "Flow_vY",
    "Flow_vPt"
  };
  const int nNames = sizeof(baseName) / sizeof(char*);
  
  // add ana prefix
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
    
  for (int pageNumber = 0; pageNumber < nNames; pageNumber++ ) {
    for (int selN = 0; selN < nSels; selN++) {
      
      // Cos and Res
      Bool_t noHars = kFALSE;
      int nHar = nHars;
      if (strstr(baseName[pageNumber],"Cos")!=0 || 
	  strstr(baseName[pageNumber],"Res")!=0) {
	noHars = kTRUE;
	nHar = 1;
      } 
      if (strstr(baseName[pageNumber],"Diff")!=0) {
	nHar = nHars - 1;
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
	Bool_t TwoD;
	if (strstr(histName->Data(),"2D")==0) {          // 1D
	  TwoD = kFALSE;
	  TH1* hist[2];
	  hist[0] = dynamic_cast<TH1*>(histFile[0].Get(histName->Data()));
	  hist[1] = (TH1*)histFile[1].Get(histName->Data());
	  if (!hist[0]) {
	    cout << "### Can't find histogram " << histName->Data() << endl;
	    return;
	  }
	} else {
	  TwoD = kTRUE;                                  // 2D
	  TH2* hist2D[2];
	  hist2D[0] = dynamic_cast<TH2*>(histFile[0].Get(histName->Data()));
	  hist2D[1] = (TH2*)histFile[1].Get(histName->Data());
	  if (!hist2D[0]) {
	    cout << "### Can't find histogram " << histName->Data() << endl;
	    return;
	  }
	}
	
	// book the output histogram
	if (!TwoD) {                                     // 1D
	  int nBins     = hist[0]->GetNbinsX();
	  TAxis* xAxis  = hist[0]->GetXaxis();
	  float xMin    = xAxis->GetXmin();
	  float xMax    = xAxis->GetXmax();
	  char* xTitle  = xAxis->GetTitle();
	  char* yTitle  = hist[0]->GetYaxis()->GetTitle();
	  TH1F* histOut = new TH1F(histName->Data(), histName->Data(), 
				   nBins, xMin, xMax);
	  histOut->SetXTitle(xTitle);
	  histOut->SetYTitle(yTitle);
	} else {                                          // 2D
	  int nBinsX   = hist2D[0]->GetNbinsX();
	  int nBinsY   = hist2D[0]->GetNbinsY();
	  int nBins    = (nBinsX + 2) * (nBinsY + 1);
	  TAxis* xAxis = hist2D[0]->GetXaxis();
	  TAxis* yAxis = hist2D[0]->GetYaxis();
	  float xMin   = xAxis->GetXmin();
	  float xMax   = xAxis->GetXmax();
	  float yMin   = yAxis->GetXmin();
	  float yMax   = yAxis->GetXmax();
	  char* xTitle = xAxis->GetTitle();
	  char* yTitle = hist2D[0]->GetYaxis()->GetTitle();
	  TH2F* histOut2D = new TH2F(histName->Data(), histName->Data(), 
				     nBinsX, xMin, xMax, nBinsY, yMin, yMax);
	  histOut2D->SetXTitle(xTitle);
	  histOut2D->SetYTitle(yTitle);
	}
	
	// loop over the bins
	if (strstr(histName->Data(),"_v")!=0 || noHars) {   // weighted sum
	  float content, error, errorSq;
	  float meanContent, meanError, weight;
	  for (int bin = 0; bin < nBins; bin++) {
	    meanContent = 0.;
	    meanError   = 0.;
	    weight      = 0.;
	    for (int n = 0; n < 2; n++) {
	      if (!TwoD) {                                 // 1D
		content = hist[n]->GetBinContent(bin);
		error   = hist[n]->GetBinError(bin);
	      } else {
		content = hist2D[n]->GetBinContent(bin);
		error   = hist2D[n]->GetBinError(bin);
	      }
	      errorSq = error * error;
	      if (errorSq > 0.) {
		meanContent += content / errorSq;
		weight      += 1. / errorSq;
	      }
	    }
	    if (weight > 0.) {
	      meanContent /= weight;
	      meanError = sqrt(1. / weight);
	      if (!TwoD) {                                 // 1D
		histOut->SetBinContent(bin, meanContent);
		histOut->SetBinError(bin, meanError);
	      } else {
		histOut2D->SetBinContent(bin, meanContent);
		histOut2D->SetBinError(bin, meanError);
	      }
	    }
	  }
	} else {                                    // simple add
	  if (!TwoD) {
	    histOut->Add(hist[0], hist[1]);
	  } else {
	    histOut2D->Add(hist2D[0], hist2D[1]);
	  }
	}
	delete histName;	
      }
    }
  }
  
  //histFile[2]->ls();
  histFile[2]->Write();
  histFile[2]->Close();
  delete histFile[2];
  
}
