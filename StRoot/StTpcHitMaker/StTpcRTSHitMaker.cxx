/***************************************************************************
 *
 * $Id: StTpcRTSHitMaker.cxx,v 1.11 2009/11/25 21:34:04 fisyak Exp $
 *
 * Author: Valeri Fine, BNL Feb 2007
 ***************************************************************************
 *
 * Description:  Make clusters from StTpcRawData and fill the StEvent      */
#include <assert.h>
#include <sys/types.h>
#include <stdio.h>

#include "StTpcRTSHitMaker.h"

#include "TString.h"

#include "StTpcRawData.h"
#include "StEvent/StTpcRawData.h"
#include "StEvent/StTpcHit.h"
#include "StEvent/StEvent.h"
#include "StEvent/StTpcHitCollection.h"
#include "StThreeVectorF.hh"
#include "StTpcDb/StTpcDb.h"
#include "StDbUtilities/StCoordinates.hh"
#include "StDetectorDbMaker/St_tss_tssparC.h"
#include "StDetectorDbMaker/St_tpcPadGainT0C.h"
#include "StMessMgr.h" 

#ifndef NEW_DAQ_READER
#  include "RTS/include/rtsLog.h"
#  include "RTS/src/RTS_READER/rts_reader.h"
#  include "RTS/src/RTS_READER/daq_dta.h"
#  include "RTS/src/DAQ_TPX/daq_tpx.h"
#else /* NEW_DAQ_READER */
#  include "StDAQMaker/StDAQReader.h"
#  include "StRtsTable.h"
#  include "DAQ_TPX/daq_tpx.h"
#  include "DAQ_READER/daq_dta.h"
#  include "DAQ_READER/daqReader.h"
#endif /* NEW_DAQ_READER */


ClassImp(StTpcRTSHitMaker); 
//________________________________________________________________________________
StTpcRTSHitMaker::~StTpcRTSHitMaker() {
  SafeDelete(fTpx);
}
//________________________________________________________________________________
Int_t StTpcRTSHitMaker::Init() {
  SetAttr("minSector",1);
  SetAttr("maxSector",24);
  SetAttr("minRow",1);
  SetAttr("maxRow",45);
  return kStOK;
}
//________________________________________________________________________________
Int_t StTpcRTSHitMaker::InitRun(Int_t runnumber) {
  SafeDelete(fTpx);
  fTpx = new daq_tpx() ; 
  // do gains example; one loads them from database but I don't know how...
  daq_dta *dta  = fTpx->put("gain");
  for(Int_t sector=1;sector<=24;sector++) {
    for(Int_t row=1;row<=45;row++) {
      daq_det_gain *gain = (daq_det_gain *) dta->request(183);	// max pad+1		
      assert(gain);
      gain[0].gain = 0.0;	// kill pad0 just in case..
      
      for(Int_t pad = 1; pad <= StTpcDigitalSector::numberOfPadsAtRow(row); pad++) {
	if (m_Mode == 2) {
	  if (St_tpcPadGainT0C::instance()->Gain(sector,row,pad) > 0) 
	    gain[pad].gain = 1.;
	  else 
	    gain[pad].gain = .0;
	  gain[pad].t0   = 0.;
	} else {
         gain[pad].gain = St_tpcPadGainT0C::instance()->Gain(sector,row,pad);
         gain[pad].t0   = St_tpcPadGainT0C::instance()->T0(sector,row,pad);
	}
      }
      dta->finalize(183,sector,row);
    }
  }
  /*
    InitRun will setup the internal representations of gain 
    and other necessary structures but if the gains have not
    been previously loaded as shown in the example above they
    will be set to 1.0!
  */
  //  LOG_FATAL << " new reader has no r.InitRun(runnumber); method. Ask Tonko. " << endm;
  fTpx->InitRun(runnumber);
  return kStOK;
}
//________________________________________________________________________________
Int_t StTpcRTSHitMaker::Make() {
  static  Short_t ADCs[__MaxNumberOfTimeBins__];
  static UShort_t IDTs[__MaxNumberOfTimeBins__];
  StEvent*   rEvent      = (StEvent*)    GetInputDS("StEvent");
  if (! rEvent) {
    LOG_WARN << "There is no StEvent" << endm;
    return kStWarn;
  }
  StTpcHitCollection *hitCollection = rEvent->tpcHitCollection();
  TDataSet*  tpcRawEvent =               GetInputDS("Event");
  if (! tpcRawEvent) {
    LOG_WARN << "There is not Tpc Raw Event" << endm;
    return kStWarn;
  }
  StTpcRawData *tpcRawData = (StTpcRawData *) tpcRawEvent->GetObject();
  if (! tpcRawData) {
    LOG_WARN << "There is not Tpc Raw Data" << endm;
    return kStWarn;
  }
  // create (or reuse) the adc_sim bank...
  // add a bunch of adc data for a specific sector:row:pad
  Int_t minSector = IAttr("minSector");
  Int_t maxSector = IAttr("maxSector");
  Int_t minRow    = IAttr("minRow");
  Int_t maxRow    = IAttr("maxRow");
  Int_t Id = 0;
  for (Int_t sec = minSector; sec <= maxSector; sec++) {
    StTpcDigitalSector *digitalSector = tpcRawData->GetSector(sec);
    if (! digitalSector) continue;
    for (Int_t row = minRow; row <= maxRow; row++) {
      Int_t NoAdcs = 0;
      Double_t gain = (row<=13) ? St_tss_tssparC::instance()->gain_in() : St_tss_tssparC::instance()->gain_out();
      Double_t wire_coupling = (row<=13) ? 
	St_tss_tssparC::instance()->wire_coupling_in() : 
	St_tss_tssparC::instance()->wire_coupling_out();
      Double_t ADC2GeV = ((Double_t) St_tss_tssparC::instance()->ave_ion_pot() * 
			  (Double_t) St_tss_tssparC::instance()->scale())/(gain*wire_coupling) ;
      Int_t Npads = digitalSector->numberOfPadsInRow(row);
      if (! Npads) continue;
      daq_dta *dta = fTpx->put("adc_sim"); // used for any kind of data; transparent pointer
      Int_t nup = 0;
      for(Int_t pad = 1; pad <= Npads; pad++) {
	UInt_t ntimebins = digitalSector->numberOfTimeBins(row,pad);
	if (! ntimebins) continue;
	// allocate space for at least 512 pixels (timebins)
	daq_sim_adc_tb *d = (daq_sim_adc_tb *) dta->request(__MaxNumberOfTimeBins__);
	// add adc data for this specific sector:row:pad
	memset (ADCs, 0, sizeof(ADCs));
	memset (IDTs, 0, sizeof(IDTs));
	digitalSector->getTimeAdc(row,pad,ADCs,IDTs);
	UInt_t l = 0;
	for (UInt_t k = 0; k < __MaxNumberOfTimeBins__; k++) {
	  if (ADCs[k]) {
	    d[l].adc = ADCs[k];
	    d[l].tb  = k;
	    d[l].track_id = IDTs[k];
	    l++;
	  }
	}
	if (l > 0) {
	  dta->finalize(l,sec,row,pad);
	  NoAdcs += l;
	}
      }
      if (! NoAdcs) continue;
      if (Debug() > 1) {
	// verify data!
	dta = fTpx->get("adc_sim");
	while(dta && dta->iterate()) {
	  LOG_INFO << Form("*** sec %2d, row %2d, pad %3d: %3d pixels",dta->sec,dta->row,dta->pad,dta->ncontent) << endm;
	  for(UInt_t i=0;i<dta->ncontent;i++) {
	    LOG_INFO << Form("    %2d: adc %4d, tb %3d: track %4d",i,
			     dta->sim_adc[i].adc,
			     dta->sim_adc[i].tb,
			     dta->sim_adc[i].track_id
			     ) << endm;
	  }
	}
      }
      
      static StTpcCoordinateTransform transform(gStTpcDb);
      static StTpcLocalSectorCoordinate local;
      static StTpcLocalCoordinate global;
      static StThreeVectorF hard_coded_errors;
      //      fTpx->put("cld_sim");       // clean up clusters
      dta = fTpx->get("cld_sim"); // rerun the cluster finder on the simulated data...
      
      while(dta && dta->iterate()) {
	if (Debug() > 1) {
	  LOG_INFO << Form("CLD sec %2d: row %2d: %d clusters",dta->sec, dta->row, dta->ncontent) << endm;
	}
	if (sec != dta->sec || row != dta->row) continue;
	for(UInt_t i=0;i<dta->ncontent;i++) {
	  if (Debug() > 1) {
	    LOG_INFO << Form("    pad %f[%d:%d], tb %f[%d:%d], cha %d, fla 0x%X, Id %d, Q %d ",
			     dta->sim_cld[i].cld.pad,
			     dta->sim_cld[i].cld.p1,
			     dta->sim_cld[i].cld.p2,
			     dta->sim_cld[i].cld.tb,
			     dta->sim_cld[i].cld.t1,
			     dta->sim_cld[i].cld.t2,
			     dta->sim_cld[i].cld.charge,
			     dta->sim_cld[i].cld.flags,
			     dta->sim_cld[i].track_id,
			     dta->sim_cld[i].quality
			     ) << endm;
	  }
	  if (! dta->sim_cld[i].cld.pad) continue;
	  if (dta->sim_cld[i].cld.tb >= __MaxNumberOfTimeBins__) continue;
	  if (! hitCollection )  {
	    hitCollection = new StTpcHitCollection();
	    rEvent->setTpcHitCollection(hitCollection);
	  }
	  StTpcPadCoordinate padcoord(dta->sec, dta->row, dta->sim_cld[i].cld.pad, dta->sim_cld[i].cld.tb);
	  transform(padcoord,local,kFALSE);
	  transform(local,global);
	  
	  UInt_t hw = 1;   // detid_tpc
	  hw += dta->sec << 4;     // (row/100 << 4);   // sector
	  hw += dta->row << 9;     // (row%100 << 9);   // row
	  
	  Int_t npads = TMath::Abs(dta->sim_cld[i].cld.p2 - dta->sim_cld[i].cld.p1) + 1;
	  hw += (npads   << 15);  // npads
	  
	  Int_t ntmbk = TMath::Abs(dta->sim_cld[i].cld.t2 - dta->sim_cld[i].cld.t1) + 1;
	  hw += (ntmbk << 22);  // ntmbks...
	  Double_t q = ADC2GeV*dta->sim_cld[i].cld.charge;
	  Id++;
	  StTpcHit *hit = new StTpcHit(global.position(),hard_coded_errors,hw,q
				       , (unsigned char ) 0  // counter 
				       , (unsigned short) dta->sim_cld[i].track_id  // idTruth=0
				       , (unsigned short) dta->sim_cld[i].quality   // quality=0,
				       , (unsigned short) Id  // id =0,
				       , dta->sim_cld[i].cld.p1 //  mnpad
				       , dta->sim_cld[i].cld.p2 //  mxpad
				       , dta->sim_cld[i].cld.t1 //  mntmbk
				       , dta->sim_cld[i].cld.t2 //  mxtmbk
				       , dta->sim_cld[i].cld.pad
				       , dta->sim_cld[i].cld.tb );
	  hit->setFlag(dta->sim_cld[i].cld.flags);
	  hitCollection->addHit(hit);
	}
      }
      for(Int_t pad = 1; pad <= Npads; pad++) {
	UInt_t ntimebins = digitalSector->numberOfTimeBins(row,pad);
	if (! ntimebins) continue;
	digitalSector->getTimeAdc(row,pad,ADCs,IDTs);
	// Update pixels if any
	dta = fTpx->get("adc_sim",sec);
	Int_t Updated = 0;
	while(dta && dta->iterate()) {
	  Int_t secC  = dta->sec;
	  Int_t rowC  = dta->row;
	  Int_t padC  = dta->pad;
	  if (secC != sec || rowC != row || padC != pad) continue;
	  for(UInt_t i=0;i<dta->ncontent;i++) {
	    Int_t tb = dta->sim_adc[i].tb;
	    if (ADCs[tb] != dta->sim_adc[i].adc ||
		IDTs[tb] != dta->sim_adc[i].track_id) {
	      if (Debug() > 1) {
		LOG_INFO << Form("tb %3d    adc %4d => %4d, track %4d => %4d",tb,
				 ADCs[tb],dta->sim_adc[i].adc,
				 IDTs[tb],dta->sim_adc[i].track_id) << endm;
	      }
	      ADCs[tb] = dta->sim_adc[i].adc;
	      IDTs[tb] = dta->sim_adc[i].track_id;
	      Updated++;
	    }
	  }
	}
	if (Updated) {
	  digitalSector->putTimeAdc(row,pad,ADCs,IDTs);
	  nup += Updated;
	}
      }
      if (nup && Debug() > 1) {
	LOG_INFO << "Update total " << nup << " pixels from Sector / row = " << sec << " / " << row <<   endm;
      }
    }
  }
  return kStOK;
}
