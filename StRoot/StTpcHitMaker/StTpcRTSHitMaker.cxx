/***************************************************************************
 *
 * $Id: StTpcRTSHitMaker.cxx,v 1.40 2015/04/09 19:54:03 genevb Exp $
 *
 * Author: Valeri Fine, BNL Feb 2007
 ***************************************************************************
 *
 * Description:  Make clusters from StTpcRawData and fill the StEvent      */
#include <assert.h>
#include <sys/types.h>
#include <stdio.h>
#include "StTpcHitMaker.h"
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
#include "StDetectorDbMaker/St_tpcPadGainT0BC.h"
#include "StDetectorDbMaker/St_tpcAnodeHVavgC.h"
#include "StDetectorDbMaker/St_tpcMaxHitsC.h"
#include "StDetectorDbMaker/StDetectorDbTpcRDOMasks.h"
#include "StDetectorDbMaker/St_tpcPadPlanesC.h"
#include "StMessMgr.h" 
#  include "StDAQMaker/StDAQReader.h"
#  include "StRtsTable.h"
#  include "DAQ_TPX/daq_tpx.h"
#  include "DAQ_READER/daq_dta.h"
#  include "DAQ_READER/daqReader.h"
#include "RTS/src/DAQ_TPX/tpxFCF_flags.h" // for FCF flag definition
ClassImp(StTpcRTSHitMaker); 
#define __DEBUG__
#ifdef __DEBUG__
#define PrPP(A,B) if (Debug()%10 > 1) {LOG_INFO << "StTpcRTSHitMaker::" << (#A) << "\t" << (#B) << " = \t" << (B) << endm;}
#else
#define PrPP(A,B)
#endif
//________________________________________________________________________________
StTpcRTSHitMaker::~StTpcRTSHitMaker() {
  SafeDelete(fTpx);
  if (mTpx_RowLen) delete [] mTpx_RowLen;
}
//________________________________________________________________________________
Int_t StTpcRTSHitMaker::Init() {
  memset(maxHits,0,sizeof(maxHits));
  maxBin0Hits = 0;
  bin0Hits = 0;
  return kStOK;
}
//________________________________________________________________________________
Int_t StTpcRTSHitMaker::InitRun(Int_t runnumber) {
  SetAttr("minSector",1);
  SetAttr("maxSector",24);
  SetAttr("minRow",1);
  NoInnerPadRows = St_tpcPadPlanesC::instance()->innerPadRows();
  Int_t NoRowsOuter = St_tpcPadPlanesC::instance()->outerPadRows();
  NoRows = NoInnerPadRows + NoRowsOuter;
  if (NoRows != 45) {
    // Fill no. of pad per row 
    mTpx_RowLen = new UChar_t[NoRows+1];
    mTpx_RowLen[0] = 0;
    for (Int_t i = 1; i <= NoRows; i++) {
      mTpx_RowLen[i] = St_tpcPadPlanesC::instance()->padsPerRow(i);
    }
  }
  SetAttr("maxRow",NoRows);
  SafeDelete(fTpx);
  fTpx = new daq_tpx() ; 
  if (GetDate() >= 20091215) fTpx->fcf_run_compatibility = 10 ;
  if (GetDate() <= 20090101) fminCharge = 40;
  StMaker* maskMk = GetMakerInheritsFrom("StMtdTrackingMaskMaker");
  unsigned int mask = (maskMk ? maskMk->UAttr("TpcSectorsByMtd") : ~0U); // 24 bit masking for sectors 1..24
  // do gains example; one loads them from database but I don't know how...
  if (NoRows <= 45) { // hack for now take Tonko's defaults for iTpx
    daq_dta *dta  = fTpx->put("gain");
    
    // Prepare scaled hit maxima
    
    // No hit maxima if these DB params are 0
    Int_t maxHitsPerSector = St_tpcMaxHitsC::instance()->maxSectorHits();
    Int_t maxBinZeroHits = St_tpcMaxHitsC::instance()->maxBinZeroHits();
    Int_t livePads = 0;
    Int_t totalPads = 0;
    Float_t liveFrac = 1;
    for(Int_t sector=1;sector<=24;sector++) {
      if (!((1U<<(sector-1)) & mask)) continue; // sector masking
      Int_t liveSecPads = 0;
      Int_t totalSecPads = 0;
      for(Int_t row=1;row<=NoRows;row++) {
	Int_t numPadsAtRow = St_tpcPadPlanesC::instance()->padsPerRow(row);
	daq_det_gain *gain = (daq_det_gain *) dta->request(183);	// max pad+1		
	assert(gain);
	gain[0].gain = 0.0;	// kill pad0 just in case..
	gain[0].t0   = 0.0;
	for(Int_t pad = 1; pad <= numPadsAtRow; pad++) {
	  gain[pad].gain = 0.; // be sure that dead pads are killed
	  gain[pad].t0   = 0.;
	  if (m_Mode == 2) {
	    if (St_tpcPadGainT0BC::instance()->Gain(sector,row,pad) > 0) gain[pad].gain = 1.;
	    gain[pad].t0   = 0.;
	  } else {
	    if (St_tpcPadGainT0BC::instance()->Gain(sector,row,pad) <= 0) continue;
	    gain[pad].gain = St_tpcPadGainT0BC::instance()->Gain(sector,row,pad);
	    gain[pad].t0   = St_tpcPadGainT0BC::instance()->T0(sector,row,pad);
	  }
	}
	dta->finalize(183,sector,row);
	if (maxHitsPerSector > 0 || maxBinZeroHits > 0) {
	  totalSecPads += numPadsAtRow;
	  if (StDetectorDbTpcRDOMasks::instance()->isOn(sector,
            StDetectorDbTpcRDOMasks::instance()->rdoForPadrow(row)) &&
	      St_tpcAnodeHVavgC::instance()->livePadrow(sector,row))
	    liveSecPads += numPadsAtRow;
	}
      }
      livePads += liveSecPads;
      totalPads += totalSecPads;
      if (maxHitsPerSector > 0) {
	liveFrac = TMath::Max((Float_t) 0.1,
			      ((Float_t) liveSecPads) / ((Float_t) totalSecPads));
	maxHits[sector-1] = (Int_t) (liveFrac * maxHitsPerSector);
	if (Debug()) {LOG_INFO << "maxHits in sector " << sector
			       << " = " << maxHits[sector-1] << endm;}
      } else {
	maxHits[sector-1] = 0;
	if (Debug()) {LOG_INFO << "No maxHits in sector " << sector << endm;}
      }
    }
    if (maxBinZeroHits > 0) {
      liveFrac = TMath::Max((Float_t) 0.1,
			    ((Float_t) livePads) / ((Float_t) totalPads));
      maxBin0Hits = (Int_t) (liveFrac * maxBinZeroHits);
      if (Debug()) {LOG_INFO << "maxBinZeroHits " << maxBin0Hits << endm;}
    } else {
      maxBin0Hits = 0;
      if (Debug()) {LOG_INFO << "No maxBinZeroHits" << endm;}
    }
  }
  /*
    InitRun will setup the internal representations of gain 
    and other necessary structures but if the gains have not
    been previously loaded as shown in the example above they
    will be set to 1.0!
  */
  if (NoRows <= 45) { // hack for now take Tonko's defaults for iTpx
    fTpx->InitRun(runnumber);
  }
  PrintAttr();
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
  if (Debug()) tpcRawEvent->ls();
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

  bin0Hits = 0;
  daq_dta *dta = 0;
  for (Int_t sec = minSector; sec <= maxSector; sec++) {
    StTpcDigitalSector *digitalSector = tpcRawData->GetSector(sec);
    if (! digitalSector) continue;
    UShort_t Id = 0;
    if (NoRows != 45) dta = fTpx->put("adc_sim",0,NoRows+1,0,mTpx_RowLen); // used for any kind of data; transparent pointer
    else              dta = fTpx->put("adc_sim");
    Int_t hitsAdded = 0;
    Int_t nup = 0;
    Int_t NoAdcs = 0;
    for (Int_t row = minRow; row <= maxRow; row++) {
      if (! St_tpcPadGainT0BC::instance()->livePadrow(sec,row)) continue;
      Int_t Npads = digitalSector->numberOfPadsInRow(row);
      if (! Npads) continue;
      for(Int_t pad = 1; pad <= Npads; pad++) {
	UInt_t ntimebins = digitalSector->numberOfTimeBins(row,pad);
	if (! ntimebins) continue;
	// allocate space for at least 512 pixels (timebins)
	daq_sim_adc_tb *d = (daq_sim_adc_tb *) dta->request(__MaxNumberOfTimeBins__);
	// add adc data for this specific sector:row:pad
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
    }      
    if (! NoAdcs) continue;
    if (Debug() > 1) {
      // verify data!
      dta = fTpx->get("adc_sim");
      while(dta && dta->iterate()) {
	LOG_INFO << Form("*** sec %2d, row %2d, pad %3d: %3d pixels",dta->sec,dta->row,dta->pad,dta->ncontent) << endm;
	for(UInt_t i=0;i<dta->ncontent;i++) {
	  if (Debug() > 1 || dta->sim_adc[i].track_id) {
	    LOG_INFO << Form("    %2d: adc %4d, tb %3d: track %4d",i,
			     dta->sim_adc[i].adc,
			     dta->sim_adc[i].tb,
			     dta->sim_adc[i].track_id
			     ) << endm;
	  }
	}
      }
    }
    static StTpcCoordinateTransform transform(gStTpcDb);
    static StThreeVectorF hard_coded_errors;
    //      fTpx->put("cld_sim");       // clean up clusters
    if (IAttr("TpxClu2D")) {
      dta = fTpx->get("cld_2d_sim"); // rerun the 2D cluster finder on the simulated data...
    } else {
      dta = fTpx->get("cld_sim"); // rerun the cluster finder on the simulated data...
  }
    Double_t ADC2GeV = 0;
    Int_t rowOld = -1;
    static Int_t iBreak = 0;
    while(dta && dta->iterate()) {
      if (Debug() > 0) {
	LOG_INFO << Form("CLD sec %2d: row %2d: %d clusters",dta->sec, dta->row, dta->ncontent) << endm;
      }
      for(UInt_t i=0;i<dta->ncontent;i++) {
	if (Debug()) {
	  //	  if (Debug() > 1 || ( dta->sim_cld[i].cld.p2 - dta->sim_cld[i].cld.p1 <= 1 )) {
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
	    iBreak++;
	    //	  }
	}
	if (dta->sim_cld[i].cld.p1 > dta->sim_cld[i].cld.p2) continue;
	if (dta->sim_cld[i].cld.t1 > dta->sim_cld[i].cld.t2) continue;
	if (dta->sim_cld[i].cld.tb >= __MaxNumberOfTimeBins__) continue;
	if (dta->sim_cld[i].cld.charge < fminCharge) continue;
	/*tpxFCF.h
	  #define FCF_ONEPAD              1
	  #define FCF_DOUBLE_PAD          2       // offline: merged
	  #define FCF_MERGED              2
	  #define FCF_BIG_CHARGE          8
	  #define FCF_ROW_EDGE           16      // 0x10 touched end of row
	  #define FCF_BROKEN_EDGE        32      // 0x20 touches one of the mezzanine edges
	  #define FCF_DEAD_EDGE          64      // 0x40 touches a dead pad 
	*/
	if ( dta->sim_cld[i].cld.flags &&
	    (dta->sim_cld[i].cld.flags & ~(FCF_ONEPAD | FCF_MERGED | FCF_BIG_CHARGE))) continue;
	if (! hitCollection )  {
	  hitCollection = new StTpcHitCollection();
	  rEvent->setTpcHitCollection(hitCollection);
	}
	StTpcPadCoordinate Pad(dta->sec, dta->row, dta->sim_cld[i].cld.pad, dta->sim_cld[i].cld.tb); PrPP(Make,Pad);
	static StTpcLocalSectorCoordinate LS;
	static StTpcLocalCoordinate L;
	transform(Pad,LS,kFALSE,kTRUE); PrPP(Make,LS); // don't useT0, useTau                  
	transform(LS,L);                                                                             PrPP(Make,L);
	if (dta->row != rowOld) {
	  rowOld = dta->row;
	  Double_t gain = (dta->row<=NoInnerPadRows) ? St_tss_tssparC::instance()->gain_in() : St_tss_tssparC::instance()->gain_out();
	  Double_t wire_coupling = (dta->row<=NoInnerPadRows) ? 
	    St_tss_tssparC::instance()->wire_coupling_in() : 
	    St_tss_tssparC::instance()->wire_coupling_out();
	  ADC2GeV = ((Double_t) St_tss_tssparC::instance()->ave_ion_pot() * 
		     (Double_t) St_tss_tssparC::instance()->scale())/(gain*wire_coupling) ;
	}
	UInt_t hw = 1;   // detid_tpc
	hw += dta->sec << 4;     // (row/100 << 4);   // sector
	hw += dta->row << 9;     // (row%100 << 9);   // row
#if 0	
	Int_t npads = TMath::Abs(dta->sim_cld[i].cld.p2 - dta->sim_cld[i].cld.p1) + 1;
	hw += (npads   << 16);  // npads
	
	Int_t ntmbk = TMath::Abs(dta->sim_cld[i].cld.t2 - dta->sim_cld[i].cld.t1) + 1;
	hw += (ntmbk << 23);  // ntmbks...
#endif
	Double_t q = ADC2GeV*dta->sim_cld[i].cld.charge;
	UShort_t idTruth = 0;
	UShort_t quality = 0;
	//yf 12/26/13 bug 2741	if (dta->sim_cld[i].track_id < 10000) {
	idTruth = dta->sim_cld[i].track_id;
	quality = dta->sim_cld[i].quality;
	//yf 12/26/13 bug 2741 }
	Id++;
	StTpcHit *hit = StTpcHitMaker::StTpcHitFlag(L.position(),hard_coded_errors,hw,q
						    , (UChar_t ) 0  // counter 
						    , idTruth
						    , quality
						    , Id                                   // id =0,
						    , dta->sim_cld[i].cld.p1 //  mnpad
						    , dta->sim_cld[i].cld.p2 //  mxpad
						    , dta->sim_cld[i].cld.t1 //  mntmbk
						    , dta->sim_cld[i].cld.t2 //  mxtmbk
						    , dta->sim_cld[i].cld.pad
						    , dta->sim_cld[i].cld.tb 
						    , dta->sim_cld[i].cld.charge
						    , dta->sim_cld[i].cld.flags);
	assert(dta->sim_cld[i].cld.pad >  0 && dta->sim_cld[i].cld.pad <= 182 && 
	       dta->sim_cld[i].cld.tb  >= 0 && dta->sim_cld[i].cld.tb  <  512);
	hitsAdded++;
        if (hit->minTmbk() == 0) bin0Hits++;
	if (Debug()) hit->Print();
	hitCollection->addHit(hit);
      }
    }
    // Set IdTruth
    for (Int_t row = minRow; row <= maxRow; row++) {
      Int_t Npads = digitalSector->numberOfPadsInRow(row);
      for(Int_t pad = 1; pad <= Npads; pad++) {
	UInt_t ntimebins = digitalSector->numberOfTimeBins(row,pad);
	if (! ntimebins) continue;
	digitalSector->getTimeAdc(row,pad,ADCs,IDTs);
	// Update pixels if any (for data)
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
    if (maxHits[sec-1] && hitsAdded > maxHits[sec-1]) {
      LOG_ERROR << "Too many hits (" << hitsAdded << ") in one sector ("
                << sec << "). Skipping event." << endm;
      return kStSkip;
    }
  }
  if (maxBin0Hits && bin0Hits > maxBin0Hits) {
    LOG_ERROR << "Too many hits (" << bin0Hits
              << ") starting at time bin 0. Skipping event." << endm;
    return kStSkip;
  }
  StTpcHitMaker::AfterBurner(hitCollection);
  return kStOK;
}
