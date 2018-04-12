#include <stdio.h>
#include <stdlib.h>

#include "JevpBuilder.h"
#include "DAQ_READER/daqReader.h"
#include <TH1F.h>
#include <TH2F.h>

#include <math.h>
#include <map>



class etofBuilder : public JevpBuilder {
  public:
    //RunStatus status;
    int run;

    etofBuilder(JevpServer *parent=NULL); 
    ~etofBuilder();


    void initialize(int argc, char *argv[]);
    void startrun(daqReader *rdr);
    void stoprun(daqReader *rdr);
    void event(daqReader *rdr);

    static void main(int argc, char *argv[]);

  private:
    int disable_builder;

    union {
      TH1* array[];
      struct {
        TH1* nHits;
        TH1* nEpochMismatch;
        TH1* nEpochLoss;
        TH1* nEpochDataLoss;
        TH1* nEpochSync;
        TH1* totAll;
        TH1* hitTimeToTrigger;
        TH1* hitMap;
        TH1* hitMapChannelId;
        TH1* fineTime[20];
      };
    } contents;

    JevpPlot **plots;
#ifndef __CINT__
    // ifndef block needed to get root to work
    map<uint16_t, uint16_t> rocMap;
    map<uint16_t, uint16_t> rocrMap;
#endif


    ClassDef(etofBuilder, 1);
};
