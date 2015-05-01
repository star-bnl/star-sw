/*
 * StMtdEvtFilterApplyMaker
 *
 * Skip production of events using MTD criteria stored in tags.root
 * under the MtdTrackFilterTag structure, which is filled in the
 * StMtdEvtFilterMaker class
 *
 */

#include "StMessMgr.h"
#include "StMtdEvtFilterApplyMaker.h"
#include <bitset>

ClassImp(StMtdEvtFilterApplyMaker)

//____________________________________________________________________________________________________
StMtdEvtFilterApplyMaker::StMtdEvtFilterApplyMaker(const Char_t *name) : StTagFilterMaker(name) 
{
  // Colon-separated list of variables (formulas, as can be used in TTree::Draw() method)
  // to use in the determination of skipping events.
  // Individual variable values will be available as GetVal(0),GetVal(1),GetVal(2) ...
  // Individual variable names  will be available as GetVar(0),GetVar(1),GetVar(2) ...

  // StMtdEvtFilterApplyMaker uses tags stored under the MtdTrackFilterTag structure,
  // which is filled in the StMtdEvtFilterMaker class
  
  SetVarList("MtdTrackFilterTag.isRejectEvent:MtdTrackFilterTag.shouldHaveRejectEvent:MtdTrackFilterTag.tpcSectors");
}

//____________________________________________________________________________________________________
bool StMtdEvtFilterApplyMaker::SkipEvent() 
{
  LOG_INFO << "Found isReject = " << GetVal(0)
	   << " and shouldHaveReject = " << GetVal(1)
	   << " and tpcSectors = " << (bitset<24>)(GetVal(2))  <<  endm; 

  /// Skip the event if MtdTrackFilterTag.isRejectEvent is not zero
  if (GetVal(0) != 0) return true; // true means skip

  /// The values of MtdTrackFilterTag.shouldHaveRejectEvent and MtdTrackFilterTag.tpcSectors
  /// are made globally available as "MtdShouldHaveRejectEvent" and "TpcSectorsByMtd" datasets
  /// if the event is accepted. The logic for publishing only for accepted events would need
  /// revisited if there is a reason to use this maker only to flag events it wants skipped,
  /// not being deterministic on its own of whether an event should be skipped.
  TDataSet* ds1 = new TDataSet("MtdShouldHaveRejectEvent");
  TDataSet* ds2 = new TDataSet("TpcSectorsByMtd");
  ds1->SetTitle(Form("%d",(int) GetVal(1)));
  ds2->SetTitle(Form("%u",(unsigned int) GetVal(2)));
  AddData(ds1);
  AddData(ds2);

  return false; // false means accept
}

/* -------------------------------------------------------------------------
 * $Id: StMtdEvtFilterApplyMaker.cxx,v 1.1 2015/05/01 21:25:57 jeromel Exp $
 * $Log: StMtdEvtFilterApplyMaker.cxx,v $
 * Revision 1.1  2015/05/01 21:25:57  jeromel
 * First version of the DataFiler + one imp: MTD. Code from GVB reviewed & closed (VP+JL))
 *
 *
 * -------------------------------------------------------------------------
 */

