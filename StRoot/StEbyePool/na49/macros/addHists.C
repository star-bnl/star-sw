///////////////////////////////////////////////////////////////////////////////
//
// $Id: addHists.C,v 1.2 2001/03/06 17:32:51 posk Exp $
//
// Author:       Art Poskanzer, Feb 2001
// Description:  Macro to add histograms together with weighting.
//               First file anaXX.root given by first run number XX.
//               Second file given by second run number.
//               Output file given by output run number.
//
//
///////////////////////////////////////////////////////////////////////////////
//
// $Log: addHists.C,v $
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

gROOT->Reset();

Int_t addHists(Int_t firstRunNo, Int_t secondRunNo, Int_t thirdRunNo,
	       Int_t outputRunNo) {
  // Combines three files.

  addHists(firstRunNo, secondRunNo);
  addHists(99, thirdRunNo, outputRunNo);

}

Int_t addHists(Int_t firstRunNo, Int_t secondRunNo, Int_t outputRunNo=99) {

  bool     stripes = kFALSE;
  //bool     stripes = kTRUE;
  TFile*   histFile[3];
  char     firstRunName[6];
  char     firstRunName[30];
  char     secondRunName[6];
  char     secondRunName[30];
  char     outputRunName[6];
  char     outputRunName[30];
  int      nSels = 2;
  int      nHars = 6;
  TH1*     hist[2];
  
  // names of histograms to be added with weighting
  const char* baseName[] = { 
    "Flow_Res",
    "Flow_v2D",
    "Flow_vY",
    "Flow_vPt",
    "Flow_v"
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
    
  // add all histograms
  TKey*    key;
  TObject* obj;
  TIter nextkey(histFile[0]->GetListOfKeys());  
  while (key = (TKey*)nextkey()) {
    histFile[0]->cd();
    //key->ls();
    obj = key->ReadObj();
    hist[0] = (TH1*)obj;
    char* objName = key->GetName();
    if (!stripes || strstr(objName,"Corr")!=0
	|| strstr(objName,"Yield2D")!=0 
	|| (strstr(objName,"Flow_Cos")!=0 && strstr(objName,"Lab")==0)) {
      cout << " hist name= " << objName << endl;
      hist[1] = (TH1*)histFile[1]->Get(objName);
      hist[0]->Add(hist[1]);
    }
    histFile[2]->cd();
    obj->Write(objName);
    delete obj;
    obj = NULL;
  }
  
  // add histograms with weighting
  cout<<endl<<"  With weighting"<<endl;
  for (int pageNumber = 0; pageNumber < nNames; pageNumber++ ) {
    for (int selN = 0; selN < nSels; selN++) {
      
      // special cases
      Bool_t noHars = kFALSE;
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
	hist[0] = dynamic_cast<TH1*>(histFile[0]->Get(histName->Data()));
	hist[1] = (TH1*)histFile[1].Get(histName->Data());
	histOut = (TH1*)histFile[2].Get(histName->Data());
	if (!hist[0]) {
	  cout << "### Can't find histogram " << histName->Data() << endl;
	  return;
	}
	int nBins = hist[0]->GetNbinsX();

	if (strstr(histName->Data(),"2D")!=0) {          // 2D
	  int nBinsY   = hist[0]->GetNbinsY();
	  nBins        = (nBins + 2) * (nBinsY + 1);
	}
	
	// loop over the bins
	float content, error, errorSq;
	float meanContent, meanError, weight;
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
	delete histName;	
      }
    }
  }
  
  //histFile[2]->ls();
  histFile[2]->Write(0, TObject::kOverwrite);
  histFile[2]->Close();
  delete histFile[2];
  
}
