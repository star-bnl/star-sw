// $Id: Tonko2Ezt.cxx,v 1.6 2009/05/26 08:45:12 ogrebeny Exp $
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include  <iostream>
using namespace std;

#include <TObjArray.h>

#include "Tonko2Ezt.h"

#ifndef NEW_DAQ_READER
#       include "evpReader.hh"
#       include "emcReader.h"
#else
#       include "DAQ_READER/daqReader.h"
#       include "DAQ_READER/daq_dta.h"
#       include "DAQ_EMC/daq_emc.h"
#       include "DAQ_ETOW/daq_etow.h"
#       include "DAQ_ESMD/daq_esmd.h"
#endif

//-------------------------------------------
//-------------------------------------------
Tonko2Ezt::Tonko2Ezt(char *rdrc) {
    eETowPresent = false;
    eESmdPresent = false;
#ifdef NEW_DAQ_READER
    daqReader *rdr = (daqReader*)(rdrc);
#endif

#ifdef NEW_DAQ_READER
    daq_dta *dd_etow = rdr ? (rdr->det("etow")->get("adc")) : 0;
    if (dd_etow) while (dd_etow->iterate()) {
        etow_t *d = (etow_t *) dd_etow->Void;
        if (d) {
#else
    if (emc.etow_in) {
	{
#endif
	    for(int ib = 0;ib < ETOW_MAXFEE;ib++) {
    		eETow.createBank(ib,ETOW_PRESIZE,ETOW_DATSIZE);
#ifdef NEW_DAQ_READER
    		eETow.setHeader(ib,(unsigned short*)&(d->preamble[ib][0]));
    		eETow.setData(ib,(unsigned short*)&(d->adc[ib][0]));
#else
    		eETow.setHeader(ib,emc.etow_pre[ib]);
    		eETow.setData(ib,emc.etow[ib]);
#endif
		eETowPresent = true;
	    }
    	}
    }
#ifdef NEW_DAQ_READER
    daq_dta *dd_esmd = rdr ? (rdr->det("esmd")->get("adc")) : 0;
    if (dd_esmd) while (dd_esmd->iterate()) {
        esmd_t *d = (esmd_t *) dd_esmd->Void;
        if (d) {
#else
    if (emc.esmd_in) {
	{
#endif
	    for(int ib = 0;ib < ESMD_MAXFEE;ib++) {
    		eESmd.createBank(ib,ESMD_PRESIZE,ESMD_DATSIZE);
#ifdef NEW_DAQ_READER
    		eESmd.setHeader(ib,(unsigned short*)&(d->preamble[ib][0]));
    		eESmd.setData(ib,(unsigned short*)&(d->adc[ib][0]));
#else
    		eESmd.setHeader(ib,emc.esmd_pre[ib]);
    		eESmd.setData(ib,emc.esmd[ib]);
#endif
		eESmdPresent = true;
    		//eESmd.print(ib,0);
    		//printf("%x %x %x %x \n", emc.esmd_pre[ib][0], emc.esmd_pre[ib][1], emc.esmd_pre[ib][2], emc.esmd_pre[ib][3]);
	    }
	}
    }
}

// $Log: Tonko2Ezt.cxx,v $
// Revision 1.6  2009/05/26 08:45:12  ogrebeny
// Bug fix to disable EEMC plots if the detector is not in the run
//
// Revision 1.5  2009/01/24 03:47:43  ogrebeny
// Fuxed bug - can see ETOW data now
//
// Revision 1.4  2009/01/24 01:14:35  ogrebeny
// Now uses new DAQ reader
//
// Revision 1.3  2009/01/13 16:31:05  fine
// Add StEEmcPool package to the daqReader dependants
//
// Revision 1.2  2008/12/19 17:59:27  fine
// Add NEW_DAQ_READER flag
//
// Revision 1.1  2005/04/28 20:54:47  balewski
// start
//

  
