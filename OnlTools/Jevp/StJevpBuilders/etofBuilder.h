#include <stdio.h>
#include <stdlib.h>

#include "JevpBuilder.h"
#include "DAQ_READER/daqReader.h"
#include <TH1F.h>
#include <TH2F.h>

#include <math.h>
#include <map>
#include <vector>


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
 	TH1* nHitsVsTofTrgMult;
        TH1* nEpochMismatch;
        TH1* nEpochLoss;
        TH1* nEpochDataLoss;
        TH1* nEpochSync;
        TH1* totAll;
        TH1* hitTimeToTrigger;
        TH1* hitMap;
        TH1* hitMapChannelId;
  	TH1* hitMapCounter[9];
        TH1* fineTimeAll;
	TH1* coarseTimeAll;
	TH1* fineTimeGet4card[18];
        TH1* coarseTimeGet4card[18];
        TH1* fineTime[20];
      };
    } contents;

    JevpPlot **plots;
#ifndef __CINT__
    // ifndef block needed to get root to work
    map<uint16_t, uint16_t> rocMap;
    map<uint16_t, uint16_t> rocrMap;
    vector<int> Get4ToPadi;
    map<int, int> counterPlotMap;
    map<int, int> counterPlotrMap;
#endif

    
    unsigned int NrOfChannelsPerGet4;
    unsigned int NrOfGet4PerFeb;
    unsigned int NrOfChannelsPerCard;



    ClassDef(etofBuilder, 1);
};
