#ifndef COMPARE_ROOTFILES_HH
#define COMPARE_ROOTFILES_HH

#include "TH1.h"
#include "TFile.h"
#include "TTree.h"
#include "TCanvas.h"

#include <string>
#include <vector>

using std::string;
using std::vector;

class compareRootFiles {

public:

  compareRootFiles(string decayName, string newFileName, 
		   string oldFileName, string description);
  ~compareRootFiles();

  void getNDaugPlots();
  void getAllIdPlots();
  void getPartIdPlots();
  void getMtmPlots();

private:

  TH1D* getDaugHist(TFile* theFile, string histName);    
  TH1D* getAllIdHist(TFile* theFile, string histName);
  TH1D* getPartIdHist(TFile* theFile, string histName);
  TH1D* getMtmHist(TFile* theFile, string histName, vector<int> groupInts);

  TH1D* _emptyHist;

  int getPartGroup(int PDGId);

  string _decayName, _newFileName, _oldFileName, _description;

  TFile* _newFile;
  TFile* _oldFile;

  TCanvas* _theCanvas;
  int _nGroupMax;

};

#endif
