#include <stdio.h>
#include <stdlib.h>

#include "JevpBuilder.h"
#include "DAQ_READER/daqReader.h"
#include <TH1F.h>
#include <TH2F.h>

#include <math.h>

class StEmcDecoder;

struct Def {
  const char *name;
  const char *opts;
  int logx;
  int logy;
  int logz;
  int optstat;
};
    
class eemcBuilder : public JevpBuilder {
public:
  int run;

  eemcBuilder(JevpServer *parent=NULL); 
  ~eemcBuilder();
  
  void initialize(int argc, char *argv[]);
  void startrun(daqReader *rdr);
  void stoprun(daqReader *rdr);
  void event(daqReader *rdr);
  
  static void main(int argc, char *argv[]);

 private:
  char statusfile[255];
  
  int MAPMHits;
  int MAPMHitsCopy;

  //StEmcDecoder *EEMCDecoderPresenter;

  void eeJpQaMinMax(TH1 *hh);
  JevpPlot *plots[400];

  ClassDef(eemcBuilder, 1);
};
