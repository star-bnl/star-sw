#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <iostream>
#include <fstream>

#include "TProfile.h"
#include "TProfile2D.h"
#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "TCanvas.h"
#include "TMath.h"
#include "TFitResultPtr.h"

using namespace std;


void readdata(char const * inputfile, char const * histname){

  //load the root file name
  char * infile0 = new char[400];
  sprintf(infile0,"%s",inputfile);

  //load the hist name
  TFile * f = new TFile(infile0);
  sprintf(infile0,"%s",histname);
  
  //prepare the output file
  char * outfile0 = new char[400];
  sprintf(outfile0,"runInfo_%s.txt",histname);

  TProfile *myHist = (TProfile *)f->Get(infile0);

  double runID;
  double content;
  double content_err;
  double entries;
  long long runIDInt;
  std::ofstream cout(outfile0);

  cout << "runID " << histname << " " << histname << "_Err " << "entries" << endl;
  for(long long  i=1;i<=myHist->GetNbinsX();i++){
    runID = myHist->GetBinCenter(i);
    content = myHist->GetBinContent(i);
    content_err = myHist->GetBinError(i);
    entries = myHist->GetBinEntries(i);
    runIDInt = runID;

    if(content_err!=0){
      cout << runIDInt << " " << content << " " << content_err << " " << entries << endl; 
      }
    
  }
}  






