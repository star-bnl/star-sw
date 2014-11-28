
#ifndef CSMSTATUSUTILS_H
#define CSMSTATUSUTILS_H

#include "TObject.h"
#include "TH2.h"
#include "TTree.h"

#include <map>
#include <set>
#include <vector>
#include <string>

//#include "StEEmcUtil/database/StEEmcDb.h" 
class StEEmcDb;

class CSMStatusUtils : public TObject {
  public:
  CSMStatusUtils() {mZerobit=512;}
  ~CSMStatusUtils() {}
      
  // analysis functions
  Int_t createStatusHistograms(TString directory);
  Int_t analyseStatusHistogram(TH2F* hist);
  Int_t initializeHistFileFromDir(TString directory, TString filter);
  Int_t saveStatusTablesToASCII(TString directory,int runnumber=0);
  Int_t saveAbbreviatedStatusTablesToASCII(TString directory);
  Int_t readTablesFromASCII(TString directory, TString filter);
  
  Int_t getNumberOfChangedTowers(Int_t runnumber);
  
  TH2F* makeStatusVersusTimePlot();
  void plotAllStatuses(TString rootfiledir="/star/data05/scratch/relyea/2005/DONE/",int year=2005,int runstart=0);
  Int_t makeCombinedStatusTable(std::vector<Short_t>& statusVector);
  
  Int_t makeStatusPlots(TString plotDir);
  
  void setDetectorFlavor(TString flavor="bemc");
  void  setDb( StEEmcDb *db ) { m_eedb = db;}
  
  void writeHtmlHeaderBadTowerList(std::ofstream& out,Int_t runnumber);
  void writeHtmlFooterBadTowerList(std::ofstream& out);
  void writeHtmlHeaderSummary(std::ofstream& out);
  void writeHtmlFooterSummary(std::ofstream& out);
  
  private:
  std::map<Int_t,std::string> mHistFileMap; //!
  std::map<Int_t,std::vector<Short_t>*> mEEMCRunStatusMap; //!
  std::map<Int_t,std::vector<Short_t>*> mBEMCRunStatusMap; //!
  std::map<Int_t,Int_t> mRunTimestampMap; //!
  std::map<Int_t,Int_t> mRunDatestampMap; //!
  std::map<Int_t,Bool_t> mFillEndMap; //!
  std::map<Int_t,std::vector<Short_t>*>* mRunStatusMapPtr; //!
  TString mDetectorFlavor;
  int mDetectorSize;
  int mDetectorActualSize;
  int eemcCrateMap[6][120];
  StEEmcDb *m_eedb;
  
  Short_t mZerobit;

  TString getDateTimeString(int runnumber,TTree* ttree=0);
  void findFillEnds();
  void setDateTimeInfo(int runnumber,TTree* ttree);
  void writePedestals(Int_t runNumber, TString directory,
                        std::vector<Short_t>& statusVector,
                        std::vector<Float_t>& pedestalmean,
                        std::vector<Float_t>& pedestalwidth,
                        std::vector<Float_t>& pedestalchi);
  Int_t analyseStatusHistogram(TH2F* hist, TString directory, 
            Float_t& averageNumberOfHitsPerChannel,
            Int_t dateStamp,
            Int_t timeStamp,
            std::vector<Short_t>& statusVector,
            std::vector<Float_t>& pedestalmean,
            std::vector<Float_t>& pedestalwidth,
            std::vector<Float_t>& pedestalchi,
  		      TH1F* hHotTower=0, TH1F* hPedMean=0, TH1F* hPedWidth=0);
  
  ClassDef(CSMStatusUtils,0)
};

#endif
