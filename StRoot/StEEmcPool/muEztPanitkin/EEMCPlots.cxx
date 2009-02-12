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
	EEMCPlotsInstance->clear();
    }
}
//-------------------------------------------------------------------
void EEMCPlots::resetHisto() {
    if (EEMCPlotsInstance) {
	EEMCPlotsInstance->clear();
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

  if (!list) list = new TObjArray(0);
  eeqa = new EEqaSorter(list, eeDb); // creates EEMC related histos
  eeqa->setPath(eemcPathIn, eemcPathOut);
  eeqa->initHisto(200,1000); // nBins, maxAdc

  //Akio said it is not Ok for Run 9
  //eeqa->initSpy(60,3); // time constant/sec & mode,   //mode: 1=balewski@rcf,3=operator@evp
}
//-------------------------------------------------------------------
EEMCPlots::~EEMCPlots() {
    this->clear();
}
//-------------------------------------------------------------------
void EEMCPlots::init(unsigned int date, unsigned int time, const char *eemcDbDump, const char *eemcPathIn, const char *eemcPathOut) {
    this->clear();
}
//-------------------------------------------------------------------
void EEMCPlots::clear() {
}
//-------------------------------------------------------------------
void EEMCPlots::saveHistograms(TFile *hfile) {
    if (eeqa) eeqa->saveHistoAdd(hfile);
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
/*
    if (!dsm0inp || !dsm1inp || !dsm2inp || !dsm3inp) {
#ifdef NEW_DAQ_READER
	daq_dta *dd_trg = rdr ? (rdr->det("trg")->get("legacy")) : 0;
        if (dd_trg) while (dd_trg->iterate()) {
    	    trg_t *d = (trg_t *) dd_trg->Void;
    	    if (d) {
        	dsm0inp = &(d->EEMC[0]);
    		dsm1inp = &(d->EEMC_l1[0]);
        	dsm2inp = ((unsigned short*)d->trg_sum ? (((TrgSumData*)d->trg_sum)->DSMdata.EMC) : 0);
        	dsm3inp = ((unsigned short*)d->trg_sum ? (((TrgSumData*)d->trg_sum)->DSMdata.lastDSM) : 0);
            }
	}
#else
	trgReader(datap);
	dsm0inp = ((unsigned char*)&trg.EEMC);
	dsm1inp = ((unsigned short*)trg.EEMC_l1);
	dsm2inp = ((unsigned short*)trg.trg_sum ? (((TrgSumData*)trg.trg_sum)->DSMdata.EMC) : 0);
	dsm3inp = ((unsigned short*)trg.trg_sum ? (((TrgSumData*)trg.trg_sum)->DSMdata.lastDSM) : 0);
#endif
    }
*/
#ifdef NEW_DAQ_READER
    unsigned int runId = rdr->run;
    unsigned int token = rdr->token;
    unsigned int eventId = rdr->seq;
    int isEvp = rdr->IsEvp();
#else
    evpReader *evp = (evpReader*)(datap);
    unsigned int runId = evp->run;
    unsigned int token = evp->token;
    unsigned int eventId = evp->seq;
    int isEvp = evp->isevp;
#endif

	Tonko2Ezt ezt(datap);
	eeqa->clear();
	eeqa->sort(&ezt.eETow, &ezt.eESmd, runId, token, TRG_VERSION, dsm0inp, dsm1inp, dsm2inp, dsm3inp);
        if (isEvp) eeqa->spy(runId, eventId);
    }
}
//-------------------------------------------------------------------
