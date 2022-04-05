// $Id: StDetectorDbTriggerID.h,v 1.14 2007/08/10 15:57:45 fisyak Exp $
//
// $Log: StDetectorDbTriggerID.h,v $
// Revision 1.14  2007/08/10 15:57:45  fisyak
// Fix bug in chairs destractors, clean up, add debug print outs
//
// Revision 1.13  2007/07/12 19:23:15  fisyak
// Provide access to Db tables by demand only
//
// Revision 1.12  2007/05/11 05:30:33  dunlop
// Add in the additionalTriggerID table
//
// Revision 1.11  2006/05/04 17:44:34  dunlop
// moved $LOG
//
// Revision 1.10  2006/05/04 17:39:57  dunlop
// Doxygenized and cvs commented
//

/*!
 * \class StDetectorDbTriggerID
 * \author Jon Gans
 * \brief  Accessor to the database for trigger id information
 
 * This class provides access to the database for various pieces
 * of information about trigger ids.  Importantly, it also provides
 * a method to grab prescales f'or trigger ids from the database.
 * Used by StEventMaker to fill StEvent::StTriggerIdCollection
 */
#ifndef StDetectorDbTriggerID_h
#define StDetectorDbTriggerID_h

#include <map>

#include "St_triggerIDC.h"
#include "St_trigPrescalesC.h"
#include "St_L0TriggerInfoC.h"
#include "St_defaultTrgLvlC.h"
#include "St_trigL3ExpandedC.h"
#include "St_dsmPrescalesC.h"
#include "St_additionalTriggerIDC.h"



enum { kDbTriggerBadID = 999 };

class StDetectorDbTriggerID{
 public:
  static StDetectorDbTriggerID*  instance() {if (! fgInstance) fgInstance = new StDetectorDbTriggerID(); return fgInstance;};
  virtual ~StDetectorDbTriggerID() {fgInstance = 0;}
  /*!
    Table RunLog/onl triggerID 
  */
  UInt_t getIDNumRows()                      {return St_triggerIDC::instance()->getNumRows();}
  UInt_t getIDRunNumber()                    {return St_triggerIDC::instance()->runNumber();}
  UInt_t getIdxTrg(UInt_t entry = 0)         {return St_triggerIDC::instance()->idxTrg(entry);}
  UInt_t getDaqTrgId(UInt_t entry = 0)       {return St_triggerIDC::instance()->daqTrgId(entry);}
  UInt_t getOfflineTrgId(UInt_t entry = 0)   {return St_triggerIDC::instance()->offlineTrgId(entry);}
  UInt_t getTrgNameVersion(UInt_t entry = 0) {return St_triggerIDC::instance()->trgNameVersion(entry);}
  UInt_t getTrgVersion(UInt_t entry = 0)     {return St_triggerIDC::instance()->trgVersion(entry);}
  UInt_t getThreashVersion(UInt_t entry = 0) {return St_triggerIDC::instance()->threashVersion(entry);}
  UInt_t getPsVersion(UInt_t entry = 0)      {return St_triggerIDC::instance()->psVersion(entry);}
  
  /*!
    Table RunLog/onl trigPrescales.  
    These are prescales applied at L1,L2,L3; usually 1. 
  */
 
  UInt_t  getSNumRows()                   {return St_trigPrescalesC::instance()->getNumRows();}
  Int_t   getSRunNumber()                 {return St_trigPrescalesC::instance()->runNumber();}
  Int_t   getIdxTrigger(UInt_t entry = 0) {return St_trigPrescalesC::instance()->idxTrigger(entry);}
  Int_t   getIdxLevel(UInt_t entry = 0)   {return St_trigPrescalesC::instance()->idxLevel(entry);}
  Int_t   getId(UInt_t entry = 0)         {return St_trigPrescalesC::instance()->id(entry);}
  Float_t getPs(UInt_t entry = 0)         {return St_trigPrescalesC::instance()->ps(entry);}

  /*!
    Table RunLog/onl L0TriggerInfo.  
    This holds various information, including prescale at L0. 
  */

  UInt_t  getL0NumRows()                           {return St_L0TriggerInfoC::instance()->getNumRows();}
  Int_t   getL0RunNumber()                         {return St_L0TriggerInfoC::instance()->runNumber();}
  Int_t   getL0DaqTrgId(UInt_t entry = 0)          {return St_L0TriggerInfoC::instance()->daqTriggerId(entry);}
  Int_t   getL0OfflineTrgId(UInt_t entry = 0)      {return St_L0TriggerInfoC::instance()->offlineTriggerId(entry);}
  Int_t   getPsL0(UInt_t entry = 0)                {return St_L0TriggerInfoC::instance()->psL0(entry);}
  Char_t* getName(UInt_t entry = 0)                {return St_L0TriggerInfoC::instance()->name(entry);}
  UInt_t  getDetectorLiveOnBits(UInt_t entry = 0)  {return St_L0TriggerInfoC::instance()->detectorLiveOnBits(entry);}
  UInt_t  getDetectorLiveOffBits(UInt_t entry = 0) {return St_L0TriggerInfoC::instance()->detectorLiveOffBits(entry);}
  UInt_t  getDetectorRequest(UInt_t entry = 0)     {return St_L0TriggerInfoC::instance()->detectorRequest(entry);}
  
  /*!
    Table RunLog/onl trigL3Expanded.  
    New table for run 6, allows to unpack multiple meanings of L2 trigger ids.
    Also holds prescales applied for certain algorithm results in L2. 
  */
  UInt_t  getTrigL3ExpandedNumRows()                           {return St_trigL3ExpandedC::instance()->getNumRows();}
  Int_t   getTrigL3ExpandedRunNumber()                         {return St_trigL3ExpandedC::instance()->runNumber();}
  Char_t* getTrigL3ExpandedL2TriggerResultType(UInt_t entry=0) {return St_trigL3ExpandedC::instance()->l2TriggerResultType(entry);}
  Int_t   getTrigL3ExpandedL3TrgId(UInt_t entry=0)             {return St_trigL3ExpandedC::instance()->l3TrgId(entry);}
  Int_t   getTrigL3ExpandedL3ExpandedTrgId(UInt_t entry=0)     {return St_trigL3ExpandedC::instance()->l3ExpandedTrgId(entry);}
  Int_t   getTrigL3ExpandedL2Algo(UInt_t entry=0)              {return St_trigL3ExpandedC::instance()->l2Algo(entry);}
  Float_t getTrigL3ExpandedL2Ps(UInt_t entry=0)                {return St_trigL3ExpandedC::instance()->l2Ps(entry);}
  Char_t* getTrigL3ExpandedName(UInt_t entry=0)                {return (Char_t *) St_trigL3ExpandedC::instance()->name(entry);}

        
  /*!
    Table RunLog/onl dsmPrescales.
    New table for run 6.  Holds the prescale applied at DSM level, before L0.
  */
  UInt_t getDsmPrescalesNumRows()                   {return St_dsmPrescalesC::instance()->getNumRows();}
  Int_t  getDsmPrescalesRunNumber()                 {return St_dsmPrescalesC::instance()->runNumber();}
  Int_t  getDsmPrescalesTrgId(UInt_t entry=0)       {return St_dsmPrescalesC::instance()->trgId(entry);}
  Int_t  getDsmPrescalesDsmPrescale(UInt_t entry=0) {return St_dsmPrescalesC::instance()->dsmPrescale(entry);}
  
  /*! 
    Table Calibrations/trg defaultTrgLvl.
    Holds the level to which nominal() trigger ids should be applied (i.e. L1,L2,L3).
    With the exception of early on in run 3, always will poInt_t to L3.
    Note that this is superseded in run 6 with L3Expanded 
  */
  UInt_t getDefaultTriggerLevel() {return St_defaultTrgLvlC::instance()->level();}
  
  /*! 
    These two functions allow one to get the total prescale,
    i.e. DSM*L0*L1*L2*L3*L3Expanded
  */
  Float_t       getTotalPrescaleByTrgId(Int_t trgId); /**< This will multiply the prescales at dsm*L0*L2; should be used by everybody */
  map<Int_t,Float_t>            getTotalPrescales(); /**< This returns all prescales active in the run */
  
  /*!
    Table RunLog/onl additionalTriggerID
    This table is new in 2007, allows to push back additional trigger ids
    onto the stack of trigger ids in order to fix the trigger id problem 
    in runs 3-6, in which the prescale was applied deterministically counting
    from 0, and so would deplete rare overlap events 
    
  */
  UInt_t getAdditionalTriggerIDNumRows()                        {return St_additionalTriggerIDC::instance()->getNumRows();}
  UInt_t getAdditionalTriggerIDRunNumber(UInt_t entry=0)        {return St_additionalTriggerIDC::instance()->runNumber(entry);}
  UInt_t getAdditionalTriggerIDEventNumber(UInt_t entry = 0)    {return St_additionalTriggerIDC::instance()->eventNumber(entry);}
  UInt_t getAdditionalTriggerIDIdxTrg(UInt_t entry = 0)         {return St_additionalTriggerIDC::instance()->idxTrg(entry);}
  UInt_t getAdditionalTriggerIDDaqTrgId(UInt_t entry = 0)       {return St_additionalTriggerIDC::instance()->daqTrgId(entry);}
  UInt_t getAdditionalTriggerIDOfflineTrgId(UInt_t entry = 0)   {return St_additionalTriggerIDC::instance()->offlineTrgId(entry);}
  UInt_t getAdditionalTriggerIDTrgNameVersion(UInt_t entry = 0) {return St_additionalTriggerIDC::instance()->trgNameVersion(entry);}
  UInt_t getAdditionalTriggerIDTrgVersion(UInt_t entry = 0)     {return St_additionalTriggerIDC::instance()->trgVersion(entry);}
  UInt_t getAdditionalTriggerIDThreashVersion(UInt_t entry = 0) {return St_additionalTriggerIDC::instance()->threashVersion(entry);}
  UInt_t getAdditionalTriggerIDPsVersion(UInt_t entry = 0)      {return St_additionalTriggerIDC::instance()->psVersion(entry);}
 private:
  static StDetectorDbTriggerID* fgInstance;
};
#endif
