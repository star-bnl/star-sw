#ifndef CSMSTATUSUTILS_H
#define CSMSTATUSUTILS_H

#include "TObject.h"
#include "TH2.h"

#include <map>
#include <set>
#include <vector>
#include <string>

class CSMStatusUtils : public TObject {
  public:
  // analysis functions
  Int_t createStatusHistograms(const char* directory);
  Int_t analyseStatusHistogram(TH2F* hist);
  Int_t analyseStatusHistogram(TH2F* hist, std::vector<Short_t>& statusVector,
	   		       const Char_t* directory, TH1F* hHotTower=0, TH1F* hPedMean=0, TH1F* hPedWidth=0);
  Int_t initializeHistFileFromDir(const Char_t* directory, const Char_t* filter);
  Int_t saveStatusTablesToASCII(const Char_t* directory, int index=0);
  Int_t saveAbbreviatedStatusTablesToASCII(const Char_t* directory);
  Int_t readTablesFromASCII(const Char_t* directory, const Char_t* filter);
  
  Int_t getNumberOfChangedTowers(Int_t runnumber);
  
  TH2F* makeStatusVersusTimePlot();
  Int_t makeCombinedStatusTable(std::vector<Short_t>& statusVector);
  
  Int_t makeStatusPlots(const Char_t* plotDir);
  
  void setDetectorFlavor(TString flavor="bemc");
  
  void writeHtmlHeaderBadTowerList(std::ofstream& out,Int_t runnumber);
  void writeHtmlFooterBadTowerList(std::ofstream& out);
  void writeHtmlHeaderSummary(std::ofstream& out);
  void writeHtmlFooterSummary(std::ofstream& out);
  
  private:
  std::map<Int_t,std::string> mHistFileMap; //!
  std::map<Int_t,std::vector<Short_t>*> mEEMCRunStatusMap; //!
  std::map<Int_t,std::vector<Short_t>*> mBEMCRunStatusMap; //!
  std::map<Int_t,std::vector<Short_t>*>* mRunStatusMapPtr; //!
  TString mDetectorFlavor;
  int mDetectorSize;
  
  ClassDef(CSMStatusUtils,0)
};

#endif
