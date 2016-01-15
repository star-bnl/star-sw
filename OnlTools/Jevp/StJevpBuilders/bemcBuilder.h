#include <stdio.h>
#include <stdlib.h>

#include "JevpBuilder.h"
#include "DAQ_READER/daqReader.h"
#include <TH1F.h>
#include <TH2F.h>

#include <math.h>

class StEmcDecoder;

class bemcBuilder : public JevpBuilder {
public:
  int run;

  bemcBuilder(JevpServer *parent=NULL); 
  ~bemcBuilder();
  

  void initialize(int argc, char *argv[]);
  void startrun(daqReader *rdr);
  void stoprun(daqReader *rdr);
  void event(daqReader *rdr);
  
  static void main(int argc, char *argv[]);

 private:
  char statusfile[255];
  
  void addCrateIDs(JevpPlot *plot, Int_t numBoxes, Int_t *min, Int_t *max, const char *labelFormat);
  void addPatchSumIndex(JevpPlot *plot);
  void addTowerIndex(JevpPlot *plot);
  void addDsmL1TowerIndex(JevpPlot *plot);
  void addDsmL1PatchIndex(JevpPlot *plot);
  void addDsmL2TowerIndex(JevpPlot *plot);
  void addDsmL2PatchIndex(JevpPlot *plot);
  void addDsmL2PatchSumIndex(JevpPlot *plot);
  void addSmdFeeSumIndex(JevpPlot *plot);
  void addPsdFeeSumIndex(JevpPlot *plot);

  static Int_t linesColor;
  static Int_t crateMinSoftId[];
  static Int_t crateMaxSoftId[];
  static Int_t pmtbxMinSoftId[];
  static Int_t pmtbxMaxSoftId[];

  Int_t numPmtBoxes;
  Int_t numBoxes;

  StEmcDecoder *BEMCDecoderPresenter;

  ClassDef(bemcBuilder, 1);
};
