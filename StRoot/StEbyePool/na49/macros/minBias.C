///////////////////////////////////////////////////////////////////////////////
//
// $Id: minBias.C,v 1.4 2001/03/16 22:35:01 posk Exp $
//
// Author:       Art Poskanzer, Mar 2001
// Description:  Macro to add histograms together.
//               Some will be added with statistics weighting.
//               The _v2D histogram can be done with cross section weighting.
//               First file anaXX.root given by first run number XX.
//               Output file given by output run number.
//
//
///////////////////////////////////////////////////////////////////////////////
//
// $Log: minBias.C,v $
// Revision 1.4  2001/03/16 22:35:01  posk
// plotGraphs.C makes the final graphs.
//
// Revision 1.3  2001/03/06 17:32:57  posk
// All macros now work.
//
//
///////////////////////////////////////////////////////////////////////////////

gROOT->Reset();

void minBias(Int_t firstRunNo, Int_t outputRunNo=99) {

  bool   crossSection = kTRUE;   // cross section weighting
  //bool   crossSection = kFALSE;  // statistics weighting
  const  int nCens = 6;
  TFile* histFile[nCens+1];
  TH1*   hist[nCens+1];
  char   fileName[30];
  int    nSels =    2;
  int    nHars =    6;
  float  yCM   = 2.92;
  bool   twoD;
  
  // names of histograms to be added with statistics weighting
  const char* baseName[] = { 
    "Flow_Res",
    "Flow_v2D",
    "Flow_vY",
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
      cout << " hist name= " << objName << endl;
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
   
  // add histograms with weighting
  cout<<endl<<"  With weighting"<<endl;
  gROOT->LoadMacro("dNdydPt.C");
  for (int pageNumber = 0; pageNumber < nNames; pageNumber++ ) {
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
	cout << " hist name= " << histName->Data() << endl;
	
	// get the histograms
	for (int n = 0; n < nCens+1; n++) {
	  hist[n] = dynamic_cast<TH1*>(histFile[n]->Get(histName->Data()));
	  if (!hist[n]) {
	    cout << "### Can't find histogram " << histName->Data() << endl;
	    return;
	  }
	}
	if (strstr(histName->Data(),"v2D")!=0) twoD = kTRUE;      // 2D
	else twoD = kFALSE;

	int nBins;
	int xBins = hist[0]->GetNbinsX();
	int yBins;
	if (twoD) {
	  yBins = hist[0]->GetNbinsY();
	  nBins = (xBins + 2) * (yBins + 1);
	} else {
	  nBins = xBins;
	}
	
	// loop over the bins
	if (!crossSection || !twoD) {

	  // with statistics weighting
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
	} else {

	  // 2D with cross section weighting
	  cout << "Please wait" << endl;
	  double y;
	  double pt;
	  double yield;
	  double yieldSum;
	  double v;
	  double vSum;
	  double err2Sum;
	  float  content;
	  float  error;
	  for (int xBin=1; xBin<=xBins; xBin++) {
	    y = hist[0]->GetXaxis()->GetBinCenter(xBin);
	    for (int yBin=1; yBin<=yBins; yBin++) {
	      pt = hist[0]->GetYaxis()->GetBinCenter(yBin);
	      yieldSum = 0.;
	      vSum     = 0.;
	      err2Sum  = 0.;
	      content  = 0.;
	      error    = 0.;
	      for (int n = 0; n < nCens; n++) {
		yield     = dNdydPt(0, y - yCM, pt, n+1) +
		  dNdydPt(1, y - yCM, pt, n+1);               // pi+ + pi-
		yieldSum += yield;
		vSum     += yield * hist[n]->GetCellContent(xBin, yBin);
		err2Sum  += pow(yield * hist[n]->GetCellError(xBin, yBin), 2.);
	      }
	      if (yieldSum) {
		content = vSum / yieldSum;
		error   = sqrt(err2Sum) / yieldSum;
	      }
	      hist[nCens]->SetCellContent(xBin, yBin, content);
	      hist[nCens]->SetCellError(xBin, yBin, error);
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
