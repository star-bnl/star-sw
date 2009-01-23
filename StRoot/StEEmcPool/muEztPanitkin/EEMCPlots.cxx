#ifndef NEW_DAQ_READER
#include "evpReader.hh"
#include "emcReader.h"
#else
#  include "DAQ_READER/daqReader.h"
#  include "DAQ_READER/evpReaderClass.h"
#  include "DAQ_EMC/emcReader.h"
#  include <RTS/include/daqFormats.h>
#endif

#include <TObjArray.h>

#include "EEMCPlots.h"
#include "EEmcDb.h"
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
  eeDb=new EEmcDb(); // use ASCII dump as input
  eeDb->readAsciiDataBase(eemcDbDump,1,12);

  if (!list) list = new TObjArray(0);
  eeqa=new EEqaSorter(list, eeDb); // creates EEMC related histos
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
//#ifdef NEW_DAQ_READER
    evpReader *evp = (evpReader*)(datap);
//#endif
	Tonko2Ezt ezt;
	eeqa->clear();
	eeqa->sort(&ezt.eETow, &ezt.eESmd,
            evp->run, evp->token, TRG_VERSION,
            dsm0inp, dsm1inp, dsm2inp, dsm3inp);
        if(emc.etow_in && evp->isevp ) {
            eeqa->spy(evp->run,evp->event_number);
        }
    }
}
//-------------------------------------------------------------------
