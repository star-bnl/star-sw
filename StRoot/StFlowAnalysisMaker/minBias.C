///////////////////////////////////////////////////////////////////////////////
//
// $Id: minBias.C,v 1.5 2001/11/09 21:15:00 posk Exp $
//
// Author:       Art Poskanzer and Alexander Wetzler, Mar 2001
// Description:  Macro to add histograms together.
//               The v histograms will be added with yield weighting.
//               First file anaXX.root given by first run number XX.
//               Output file given by output run number.
//
//
///////////////////////////////////////////////////////////////////////////////

void minBias(Int_t firstRunNo, Int_t outputRunNo=99) {

  const  int nCens = 8;
  int    nSels = 2;
  const  int nHars = 6;
  float  yCM   = 0.0;
  char   fileName[30];
  TFile* histFile[nCens+1];
  TH1*   hist[nCens+1];
  TH2*   yieldPartHist[nCens];
  
  // names of histograms to be added with weighting
  const char* baseName[] = { 
    "Flow_Res",
    "Flow_v2D",   // must be first
    "Flow_vEta",
    "Flow_vPt",
    "Flow_v"
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
    if (obj->InheritsFrom("TH1")) { // TH1 or TProfile
      hist[0] = (TH1*)obj;
      char* objName = key->GetName();
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
      if (strcmp(baseName[pageNumber],"Flow_v")==0 ||
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
	histName->Append("_Sel");
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
	    return;
	  }
	}

	int nBins;      // set by 2D
	int xBins = hist[0]->GetNbinsX();
	int yBins;
	if (twoD) {
	  yBins = hist[0]->GetNbinsY();
	  nBins = xBins + (xBins + 2) * yBins;
	  float yMax  = hist[0]->GetXaxis()->GetXmax();
	  float yMin  = hist[0]->GetXaxis()->GetXmin();
	  TH1F* yieldY = new TH1F("Yield_Y", "Yield_Y", xBins, yMin, yMax);
	  yieldY->SetXTitle("Rapidity");
	  yieldY->SetYTitle("Counts");
	  float ptMax  = hist[0]->GetYaxis()->GetXmax();
	  TH1F* yieldPt = new TH1F("Yield_Pt", "Yield_Pt", yBins, 0., ptMax);
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
	    hist[nCens]->SetBinContent(bin, meanContent);
	    hist[nCens]->SetBinError(bin, meanError);
	    }
	  }
	} else {                                   // with yield weighting
	  cout<<"  With yield weighting"<<endl;
	  float v;
	  float vSum;
	  float content;
	  float error;
	  float error2sum;
	  float yield;
	  float yieldSum;
	  float y;
	  float pt;
	  for (int bin = 0; bin < nBins; bin++) {
	    v         = 0.;
	    vSum      = 0.;
	    content   = 0.;
	    error     = 0.;
	    error2sum = 0.;
	    yield     = 0.;
	    yieldSum  = 0.;
	    for (int n = 0; n < nCens; n++) {
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
	      if (v != 0) {
		yieldSum  += yield;
		vSum      += yield * v;
		error2sum += pow(yield *  hist[n]->GetBinError(bin), 2.);
	      }
	    }
	    if (yieldSum) {
	      content = vSum / yieldSum;
	      error   = sqrt(error2sum) / yieldSum;
	    }
	    hist[nCens]->SetBinContent(bin, content);
	    hist[nCens]->SetBinError(bin, error);
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
