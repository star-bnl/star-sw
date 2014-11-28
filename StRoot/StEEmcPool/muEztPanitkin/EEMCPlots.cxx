#ifndef NEW_DAQ_READER
#	include "evpReader.hh"
#	include "emcReader.h"
#	include "trgReader.h"
#else
#	include "DAQ_READER/daqReader.h"
#	include "DAQ_READER/daq_dta.h"
#	include "DAQ_EMC/daq_emc.h"
#	include "DAQ_ETOW/daq_etow.h"
#	include "DAQ_ESMD/daq_esmd.h"
#	include "DAQ_TRG/daq_trg.h"
#	include "DAQ_READER/evpReaderClass.h"
#	include "DAQ_EMC/emcReader.h"
#	include <RTS/include/daqFormats.h>
#endif

#include <TObjArray.h>

#include "StEEmcUtil/database/StEEmcDb.h"

#include "EEMCPlots.h"
#include "EEqaSorter.h"
#include "Tonko2Ezt.h"

EEMCPlots *EEMCPlotsInstance = 0;

//-------------------------------------------------------------------
void EEMCPlots::initHisto(TObjArray *list, const char *eemcDbDump, const char *eemcPathIn, const char *eemcPathOut) {
    if (EEMCPlotsInstance) delete EEMCPlotsInstance; EEMCPlotsInstance = 0;
    EEMCPlotsInstance = new EEMCPlots(list, eemcDbDump, eemcPathIn, eemcPathOut);
    if (EEMCPlotsInstance) {
	EEMCPlotsInstance->resetHistograms();
    }
}
//-------------------------------------------------------------------
void EEMCPlots::resetHisto() {
    if (EEMCPlotsInstance) {
	EEMCPlotsInstance->resetHistograms();
    }
}
//-------------------------------------------------------------------
void EEMCPlots::saveHisto(TFile *hfile) {
    if (EEMCPlotsInstance) {
	EEMCPlotsInstance->saveHistograms(hfile);
    }
}
//-------------------------------------------------------------------
void EEMCPlots::fillHisto(    char *rdr
                    	    , const unsigned char * dsm0inp
                    	    , const unsigned short int  * dsm1inp
                    	    , const unsigned short int  * dsm2inp
                    	    , const unsigned short int  * dsm3inp
                	    ) {
    if (EEMCPlotsInstance) {
	EEMCPlotsInstance->processEvent(rdr, dsm0inp, dsm1inp, dsm2inp, dsm3inp);
    }
}
//-------------------------------------------------------------------
EEMCPlots::EEMCPlots(TObjArray *list, const char *eemcDbDump, const char *eemcPathIn, const char *eemcPathOut)
    : eeqa(0), eeDb(0)
{
  eeDb = new StEEmcDb(); 
  eeDb->setSectors(1, 12);
  eeDb->setAsciiDatabase(eemcDbDump); // use ASCII dump as input

  eeqa = new EEqaSorter(eeDb); // creates EEMC related histos
  eeqa->setPath(eemcPathIn, eemcPathOut);
  eeqa->initHisto(list, 200,1000); // nBins, maxAdc

  //Akio said it is not Ok for Run 9
  //eeqa->initSpy(list, 60, 3); // time constant/sec & mode,   //mode: 1=balewski@rcf,3=operator@evp
}
//-------------------------------------------------------------------
EEMCPlots::~EEMCPlots() {
    if (eeqa) delete eeqa;
    if (eeDb) delete eeDb;
}
//-------------------------------------------------------------------
//void EEMCPlots::init(unsigned int date, unsigned int time, const char *eemcDbDump, const char *eemcPathIn, const char *eemcPathOut) {}
//-------------------------------------------------------------------
void EEMCPlots::resetHistograms() {
    if (eeqa) eeqa->resetHisto();
}
//-------------------------------------------------------------------
void EEMCPlots::saveHistograms(TFile *hfile) {
    if (eeqa) eeqa->saveHisto(hfile);
}
//-------------------------------------------------------------------
void EEMCPlots::processEvent( char *datap
                    	    , const unsigned char * dsm0inp
                    	    , const unsigned short int  * dsm1inp
                    	    , const unsigned short int  * dsm2inp
                    	    , const unsigned short int  * dsm3inp
                 	    ) {
    if (eeqa) {
#ifdef NEW_DAQ_READER
    daqReader *rdr = (daqReader*)(datap);
#else
    int ret = emcReader(datap);
#endif

#ifdef NEW_DAQ_READER
    unsigned int runId = rdr->run;
    unsigned int token = rdr->token;
//    unsigned int eventId = rdr->seq;
//    int isEvp = rdr->IsEvp();
#else
    evpReader *evp = (evpReader*)(datap);
    unsigned int runId = evp->run;
    unsigned int token = evp->token;
//    unsigned int eventId = evp->seq;
//    int isEvp = evp->isevp;
#endif

	Tonko2Ezt ezt(datap);
	eeqa->clear();
	eeqa->sort(ezt.eETowPresent ? &ezt.eETow : 0, ezt.eESmdPresent ? &ezt.eESmd : 0, runId, token, TRG_VERSION, dsm0inp, dsm1inp, dsm2inp, dsm3inp);
        //if (isEvp) eeqa->spy(&ezt.eETow, &ezt.eESmd, runId, eventId);
    }
}
//-------------------------------------------------------------------
