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
    // -------------------------------
    void processMessages( uint64_t* messageBuffer, size_t nFullMessagesToRead, map< unsigned int, unsigned short >& nDigisInGdpb );

    unsigned int hardwareMapElinkGet4Id( const unsigned int& chip );
    unsigned int hardwareMapChannelNumberInFee( const unsigned int& chip, const unsigned int& chan );
    unsigned int hardwareMapFeeNumber( const unsigned int& chip );
    unsigned int hardwareMapChannelNumber( const unsigned int& chip, const unsigned int& chan );
    
    void processMessages2018( uint64_t* messageBuffer, size_t nFullMessagesToRead, map< unsigned int, unsigned short >& nDigisInGdpb );
    // --------------------------------

    int disable_builder;

    union {
      TH1* array[];
      struct {
        TH1* nDigis;
        TH1* nDigisVsTofTrgMult;
        
        TH1* digiTot;
        TH1* digiTimeToTrigger;

        TH1* digiFineTs;
        TH1* digiCoarseTs;


        TH1* nDigisPerGdpb[ 12 ];
        TH1* digiTotPerGdpb[ 12 ];
        TH1* digiMappedChannelNumberPerGdpb[ 12 ];
        
        /*
        TH1* hitMap;
        TH1* hitMapChannelId;
        TH1* hitMapCounter[9];
        TH1* fineTimeAll;
        TH1* coarseTimeAll;
        TH1* fineTimeGet4card[18];
        TH1* coarseTimeGet4card[18];
        TH1* fineTime[20];

        TH1* nEpochMismatch;
        TH1* nEpochLoss;
        TH1* nEpochDataLoss;
        TH1* nEpochSync;
        */
      };
    } contents;

    JevpPlot **plots;

#ifndef __CINT__
    // ifndef block needed to get root to work
    std::map< uint16_t, unsigned int > gdpbMap2018;
    std::map< uint16_t, unsigned int > gdpbMap2019;
    std::map< unsigned int, uint16_t > gdpbRevMap2018;
    std::map< unsigned int, uint16_t > gdpbRevMap2019;

    std::map< uint16_t, unsigned int > gdpbMap;
    std::map< unsigned int, uint16_t > gdpbRevMap;

    std::vector< unsigned int > get4ToPadi;
    std::vector< unsigned int > eLinkToGet4;
#endif

    unsigned int nrOfGdpbInSys;
    unsigned int nrOfGbtxPerGdpb;
    unsigned int nrOfFeePerGbtx;
    unsigned int nrOfGet4PerFee;
    unsigned int nrOfChannelsPerGet4;

    unsigned int nrOfChannelsPerFee;
    unsigned int nrOfGet4PerGbtx;
    unsigned int nrOfChannelsPerGdpb;

    unsigned int year;

    ClassDef( etofBuilder, 1 );
};
