/***************************************************************************
 *
 * $Id: StBemcTables.cxx,v 1.14 2015/03/02 20:25:51 jkadkins Exp $
 * Author: Alexandre A. P. Suaide
 * Maintainer: Adam Kocoloski, MIT, kocolosk@mit.edu
 *
 ***************************************************************************/

#include "StBemcTables.h"
#include "Stiostream.h"
#include "StEmcUtil/others/emcDetectorName.h"
#include "StDaqLib/EMC/StEmcDecoder.h"
#include "StMaker.h"
#include "St_db_Maker/St_db_Maker.h"
#include "StEmcRawMaker/defines.h"

ClassImp(StBemcTables)

StBemcTables::StBemcTables(Bool_t btowMapFix, Bool_t bprsMapFix):TObject() {
  mBtowP  = NULL;
  mBprsP  = NULL;
  mSmdeP  = NULL;
  mSmdpP  = NULL;
  mBtowS  = NULL;
  mBprsS  = NULL;
  mSmdeS  = NULL;
  mSmdpS  = NULL;
  mBtowC  = NULL;
  mBprsC  = NULL;
  mSmdeC  = NULL;
  mSmdpC  = NULL;
  mBtowG  = NULL;
  mBprsG  = NULL;
  mSmdeG  = NULL;
  mSmdpG  = NULL;
  mTrigS  = NULL;
  mTrigP  = NULL;
  mTrigL  = NULL;
  mTrigP4 = NULL;
  
  mBtowMapFix = btowMapFix;
  mBprsMapFix = bprsMapFix;
  mDecoder = new StEmcDecoder();
}

StBemcTables::~StBemcTables() {
    delete mBtowP;
    delete mBprsP;
    delete mSmdeP;
    delete mSmdpP;
    delete mBtowS;
    delete mBprsS;
    delete mSmdeS;
    delete mSmdpS;
    delete mBtowC;
    delete mBprsC;
    delete mSmdeC;
    delete mSmdpC;
    delete mBtowG;
    delete mBprsG;
    delete mSmdeG;
    delete mSmdpG;
    delete mTrigS;
    delete mTrigP;
    delete mTrigL;
    delete mTrigP4;
    
    if(mDecoder) delete mDecoder;
}

//-----------------------------------------------------------------------------

void StBemcTables::loadTables(StMaker* maker) {
    if(mBtowP) mBtowP = NULL;
    if(mBtowS) mBtowS = NULL;
    if(mBtowC) mBtowC = NULL;
    if(mBtowG) mBtowG = NULL;
                             
    if(mBprsP) mBprsP = NULL;
    if(mBprsS) mBprsS = NULL;
    if(mBprsC) mBprsC = NULL;
    if(mBprsG) mBprsG = NULL;
                             
    if(mSmdeP) mSmdeP = NULL;
    if(mSmdeS) mSmdeS = NULL;
    if(mSmdeC) mSmdeC = NULL;
    if(mSmdeG) mSmdeG = NULL;
                             
    if(mSmdpP) mSmdpP = NULL;
    if(mSmdpS) mSmdpS = NULL;
    if(mSmdpC) mSmdpC = NULL;
    if(mSmdpG) mSmdpG = NULL;
    
    int date = (maker->GetDBTime()).GetDate();
    int time = (maker->GetDBTime()).GetTime();
    mDecoder->SetDateTime(date, time);
    
    // the BTOW map fix should be used *ONLY* for runs before 2006
    // For runs after 2006-01-01 the database is supposed to be fixed
    if(date >= 20060101) mBtowMapFix = kFALSE;
    
    // same comment for BPRS in Run 8
    if(date >= 20080101) mBprsMapFix = kFALSE;
    
    TDataSet * DB = NULL;
    
    DB = maker->GetInputDB("Calibrations/emc/y3bemc");
    if(DB) {
        St_emcPed* bemcPed = (St_emcPed*)DB->Find("bemcPed");
        if(bemcPed) {
            mBtowP = bemcPed->GetTable();
            updateValidity(maker, bemcPed);
        }
        
        St_emcStatus* bemcStatus = (St_emcStatus*)DB->Find("bemcStatus");
        if(bemcStatus) {
            mBtowS = bemcStatus->GetTable();
            updateValidity(maker, bemcStatus);
        }
        
        St_emcCalib* bemcCalib = (St_emcCalib*)DB->Find("bemcCalib");
        if(bemcCalib) {
            mBtowC = bemcCalib->GetTable();
            updateValidity(maker, bemcCalib);
        }
        
        St_emcGain* bemcGain = (St_emcGain*)DB->Find("bemcGain");
        if(bemcGain) {
            mBtowG = bemcGain->GetTable();
            updateValidity(maker, bemcGain);
        }
    }
    
    DB = maker->GetInputDB("Calibrations/emc/y3bprs");
    if(DB) {
        St_emcPed* bprsPed = (St_emcPed*)DB->Find("bprsPed");
        if(bprsPed) {
            mBprsP = bprsPed->GetTable();
            updateValidity(maker, bprsPed);
        }
        
        St_emcStatus* bprsStatus = (St_emcStatus*)DB->Find("bprsStatus");
        if(bprsStatus) {
            mBprsS = bprsStatus->GetTable();
            updateValidity(maker, bprsStatus);
        }
        
        St_emcCalib* bprsCalib = (St_emcCalib*)DB->Find("bprsCalib");
        if(bprsCalib) {
            mBprsC = bprsCalib->GetTable();
            updateValidity(maker, bprsCalib);
        }
        
        St_emcGain* bprsGain = (St_emcGain*)DB->Find("bprsGain");
        if(bprsGain) {
            mBprsG = bprsGain->GetTable();
            updateValidity(maker, bprsGain);
        }
    }
    
    DB = maker->GetInputDB("Calibrations/emc/y3bsmde");
    if(DB) {
        St_smdPed* bsmdePed = (St_smdPed*)DB->Find("bsmdePed");
        if(bsmdePed) {
            mSmdeP = bsmdePed->GetTable();
            updateValidity(maker, bsmdePed);
        }
        
        St_smdStatus* bsmdeStatus = (St_smdStatus*)DB->Find("bsmdeStatus");
        if(bsmdeStatus) {
            mSmdeS = bsmdeStatus->GetTable();
            updateValidity(maker, bsmdeStatus);
        }
        
        St_smdCalib* bsmdeCalib = (St_smdCalib*)DB->Find("bsmdeCalib");
        if(bsmdeCalib) {
            mSmdeC = bsmdeCalib->GetTable();
            updateValidity(maker, bsmdeCalib);
        }
        
        St_smdGain* bsmdeGain = (St_smdGain*)DB->Find("bsmdeGain");
        if(bsmdeGain) {
            mSmdeG = bsmdeGain->GetTable();
            updateValidity(maker, bsmdeGain);
        }
    }
    
    DB = maker->GetInputDB("Calibrations/emc/y3bsmdp");
    if(DB) {
        St_smdPed* bsmdpPed = (St_smdPed*)DB->Find("bsmdpPed");
        if(bsmdpPed) {
            mSmdpP = bsmdpPed->GetTable();
            updateValidity(maker, bsmdpPed);
        }
        
        St_smdStatus* bsmdpStatus = (St_smdStatus*)DB->Find("bsmdpStatus");
        if(bsmdpStatus) {
            mSmdpS = bsmdpStatus->GetTable();
            updateValidity(maker, bsmdpStatus);
        }
        
        St_smdCalib* bsmdpCalib = (St_smdCalib*)DB->Find("bsmdpCalib");
        if(bsmdpCalib) {
            mSmdpC = bsmdpCalib->GetTable();
            updateValidity(maker, bsmdpCalib);
        }
        
        St_smdGain* bsmdpGain = (St_smdGain*)DB->Find("bsmdpGain");
        if(bsmdpGain) {
            mSmdpG = bsmdpGain->GetTable();
            updateValidity(maker, bsmdpGain);
        }
    }
    
    DB = maker->GetInputDB("Calibrations/emc/trigger");
    if(DB) {
        St_emcTriggerStatus* bemcTriggerStatus = (St_emcTriggerStatus*)DB->Find("bemcTriggerStatus");
        if(bemcTriggerStatus) {
            mTrigS = bemcTriggerStatus->GetTable();
            updateValidity(maker, bemcTriggerStatus);
        }
        
        St_emcTriggerPed* bemcTriggerPed = (St_emcTriggerPed*)DB->Find("bemcTriggerPed");
        if(bemcTriggerPed) {
            mTrigP = bemcTriggerPed->GetTable();
            updateValidity(maker, bemcTriggerPed);
        }

        St_bemcTriggerPed4* bemcTriggerPed4 = (St_bemcTriggerPed4*)DB->Find("bemcTriggerPed4");
        if(bemcTriggerPed4) {
	    mTrigP4 = bemcTriggerPed4->GetTable();
            updateValidity(maker, bemcTriggerPed4);
        }
        
        St_emcTriggerLUT* bemcTriggerLUT = (St_emcTriggerLUT*)DB->Find("bemcTriggerLUT");
        if(bemcTriggerLUT) {
            mTrigL = bemcTriggerLUT->GetTable();
            updateValidity(maker, bemcTriggerLUT);
        }
    }
}  

void StBemcTables::updateValidity(StMaker* maker, TTable* table) {
    TDatime datime[2];
    St_db_Maker::GetValidity(table,datime);
    string tableName = table->GetName();
    string beginTime = datime[0].AsSQLString();
    string endTime   = datime[1].AsSQLString();
    
    map<string, pair<string, string> >::iterator iter = mValidRanges.find(tableName);
    if(iter == mValidRanges.end()) {
        mValidRanges[tableName] = make_pair(beginTime, endTime);
        LOG_INFO << Form("loaded a new %20s table with beginTime %s and endTime %s", tableName.c_str(), beginTime.c_str(), endTime.c_str()) << endm; 
    }
    else if( beginTime != (iter->second).first ) {
        (iter->second).first    = beginTime;
        (iter->second).second   = endTime;
        LOG_INFO << Form("loaded a new %20s table with beginTime %s and endTime %s", tableName.c_str(), beginTime.c_str(), endTime.c_str()) << endm; 
    }
}

const char* StBemcTables::beginTime(const char * tableName) const {
    string t = tableName;
    map<string, pair<string, string> >::const_iterator iter = mValidRanges.find(t);
    if(iter == mValidRanges.end()) {
        LOG_WARN << "Couldn't find a table named " << tableName << endm;
        return NULL;
    }
    else {
        return (iter->second).first.c_str();
    }
}

const char* StBemcTables::endTime(const char * tableName) const {
    string t = tableName;
    map<string, pair<string, string> >::const_iterator iter = mValidRanges.find(t);
    if(iter == mValidRanges.end()) {
        LOG_WARN << "Couldn't find a table named " << tableName << endm;
        return NULL;
    }
    else {
        return (iter->second).second.c_str();
    }
}

//-----------------------------------------------------------------------------

Int_t StBemcTables::getOldId(int det, Int_t newId) const {
    Int_t  shift = 0;
    if(det == BTOW) { 
        mDecoder->GetTowerBugCorrectionShift(newId, shift);
    }
    if(det == BPRS) {
        mDecoder->GetPreshowerBugCorrectionShift(newId, shift);
    }
    return newId + shift;
}

//-----------------------------------------------------------------------------

void StBemcTables::getPedestal(Int_t det, Int_t id, Int_t CAP,Float_t& P, Float_t& R) const {
  P = 0;
  R = 0;
  if(det==BTOW && mBtowP) 
  {
    Int_t id1 = id;
    if(mBtowMapFix) id1 = getOldId(BTOW, id);
    P = ((Float_t)mBtowP[0].AdcPedestal[id1-1])/100;
    R = ((Float_t)mBtowP[0].AdcPedestalRMS[id1-1])/100;
    return;
  }
  if(det==BPRS && mBprsP) 
  {
    Int_t id1 = id;
    if(mBprsMapFix) id1 = getOldId(BPRS, id);
    P = ((Float_t)mBprsP[0].AdcPedestal[id1-1])/100;
    R = ((Float_t)mBprsP[0].AdcPedestalRMS[id1-1])/100;
    return;
  }
  if(det==BSMDE && mSmdeP) 
  {
    Int_t C = 0;
    if(CAP==CAP1) C = 1;
    if(CAP==CAP2) C = 2;
    P = ((Float_t)mSmdeP[0].AdcPedestal[id-1][C])/100;
    R = ((Float_t)mSmdeP[0].AdcPedestalRMS[id-1][C])/100;
    return;
  }
  if(det==BSMDP && mSmdpP) 
  {
    Int_t C = 0;
    if(CAP==CAP1) C = 1;
    if(CAP==CAP2) C = 2;
    P = ((Float_t)mSmdpP[0].AdcPedestal[id-1][C])/100;
    R = ((Float_t)mSmdpP[0].AdcPedestalRMS[id-1][C])/100;
    return;
  }
  return;  
}

void StBemcTables::getStatus(Int_t det, Int_t id, Int_t& S, const char *option) const {
  S = STATUS_OK;
  if(det==BTOW && mBtowS) 
  {    
      Int_t id1 = id;
        if(mBtowMapFix) id1 = getOldId(BTOW, id);
        if(!strcmp(option, "calib"))    { S = (Int_t)mBtowC[0].Status[id1-1]; return; }
        if(!strcmp(option, "pedestal")) { S = (Int_t)mBtowP[0].Status[id1-1]; return; }
        if(!strcmp(option, "gain"))     { S = (Int_t)mBtowG[0].Status[id1-1]; return; }        
        S = (Int_t)mBtowS[0].Status[id1-1];
        return;
  }
  if(det==BPRS && mBprsS) {
      Int_t id1 = id;
      if(mBprsMapFix) id1 = getOldId(BPRS, id);
      if(!strcmp(option, "calib"))    { S = (Int_t)mBprsC[0].Status[id1-1]; return; }
      if(!strcmp(option, "pedestal")) { S = (Int_t)mBprsP[0].Status[id1-1]; return; }
      if(!strcmp(option, "gain"))     { S = (Int_t)mBprsG[0].Status[id1-1]; return; }
      S = (Int_t)mBprsS[0].Status[id1-1];return;
  }
  if(det==BSMDE && mSmdeS) { 
      if(!strcmp(option, "calib"))    { S = (Int_t)mSmdeC[0].Status[id-1]; return; }
      if(!strcmp(option, "pedestal")) { S = (Int_t)mSmdeP[0].Status[id-1]; return; }
      if(!strcmp(option, "gain"))     { S = (Int_t)mSmdeG[0].Status[id-1]; return; }
      S = (Int_t)mSmdeS[0].Status[id-1];return;
  }
  if(det==BSMDP && mSmdpS) { 
      if(!strcmp(option, "calib"))    { S = (Int_t)mSmdpC[0].Status[id-1]; return; }
      if(!strcmp(option, "pedestal")) { S = (Int_t)mSmdpP[0].Status[id-1]; return; }
      if(!strcmp(option, "gain"))     { S = (Int_t)mSmdpG[0].Status[id-1]; return; }
      S = (Int_t)mSmdpS[0].Status[id-1];return;
  }
  return;  
}

void StBemcTables::getGain(Int_t det, Int_t id, Float_t& G) const {
  G = 1;
  if(det==BTOW && mBtowG) 
  { 
    Int_t id1 = id;
    if(mBtowMapFix) id1 = getOldId(BTOW, id);
    G = (Float_t)mBtowG[0].Gain[id1-1];
    return;
  }
  if(det==BPRS && mBprsG) { 
      Int_t id1 = id;
      if(mBprsMapFix) id1 = getOldId(BPRS, id);
      G = (Float_t)mBprsG[0].Gain[id1-1];return;
  }
  if(det==BSMDE && mSmdeG) { G = (Float_t)mSmdeG[0].Gain[id-1];return;}
  if(det==BSMDP && mSmdpG) { G = (Float_t)mSmdpG[0].Gain[id-1];return;}
  return;  
}

void StBemcTables::getCalib(Int_t det, Int_t id, Int_t power, Float_t& C) const {
  C = 0;
  if(det==BTOW && mBtowC) 
  { 
    Int_t id1 = id;
    if(mBtowMapFix) id1 = getOldId(BTOW, id);
    C = (Float_t)mBtowC[0].AdcToE[id1-1][power];
    return;
  }
  if(det==BPRS && mBprsC) { 
      Int_t id1 = id;
      if(mBprsMapFix) id1 = getOldId(BPRS, id);
      C = (Float_t)mBprsC[0].AdcToE[id1-1][power];return;
  }
  if(det==BSMDE && mSmdeC) { C = (Float_t)mSmdeC[0].AdcToE[id-1][power];return;}
  if(det==BSMDP && mSmdpC) { C = (Float_t)mSmdpC[0].AdcToE[id-1][power];return;}
  return;  
}

//-----------------------------------------------------------------------------

void StBemcTables::getTriggerPatchStatus(Int_t patch, Int_t& STATUS) const {
  STATUS = 0;
  if(mTrigS && patch>=0 && patch<NBEMCTRIGGERTOWER) 
    STATUS = (Int_t)mTrigS[0].PatchStatus[patch];
}

void StBemcTables::getTriggerHighTowerStatus(Int_t hightower, Int_t& STATUS) const {
  STATUS = 0;
  if(mTrigS && hightower>=0 && hightower<NBEMCTRIGGERTOWER) 
    STATUS = (Int_t)mTrigS[0].HighTowerStatus[hightower];
}

void StBemcTables::getTriggerTowerStatus(Int_t crate,Int_t index, Int_t& STATUS) const {
  STATUS = 0;
  if(mTrigS && crate>0 && crate<=MAXCRATES && index>=0 && index<NTOWERSPERCRATE) 
    STATUS = (Int_t)mTrigS[0].TowerStatus[crate-1][index];
}

void StBemcTables::getTriggerPedestal(Int_t crate,Int_t index, Float_t& PEDESTAL) const {
  PEDESTAL = 0;
  if(mTrigP && crate>0 && crate<=MAXCRATES && index>=0 && index<NTOWERSPERCRATE) 
    PEDESTAL = ((Float_t)mTrigP[0].Ped[crate-1][index])/100.0;
}

void StBemcTables::getTriggerBitConv(Int_t crate,Int_t patch, Int_t& BIT) const {
    BIT = 0;
    if(mTrigP && crate>0 && crate<=MAXCRATES && patch>=0 && patch<NPATCHESPERCRATE) {
        BIT = (Int_t)mTrigP[0].BitConversionMode[crate-1][patch];
    }
}

void StBemcTables::getTriggerPedestalShift(Int_t& pedestalShift) const {
  if (mTrigP) pedestalShift = (Int_t)mTrigP->PedShift / 100;
}

void StBemcTables::getTriggerPed4(Int_t softId, Short_t& PED4) const {
  PED4 = 0;
  if (mTrigP4 && softId>0 && softId<=4800) 
    PED4 = (Short_t)mTrigP4->Ped4[softId];
}

void StBemcTables::getTriggerFormulaTag(Int_t crate, Int_t index, Int_t& formula) const {
  if (mTrigL) formula = (Int_t)mTrigL->FormulaTag[crate-1][index];
}

void StBemcTables::getTriggerFormulaParameters(Int_t crate, Int_t index, Int_t* parameters) const {
  if (mTrigL) {
    parameters[0] = (Int_t)mTrigL->FormulaParameter0[crate-1][index];
    parameters[1] = (Int_t)mTrigL->FormulaParameter1[crate-1][index];
    parameters[2] = (Int_t)mTrigL->FormulaParameter2[crate-1][index];
    parameters[3] = (Int_t)mTrigL->FormulaParameter3[crate-1][index];
    parameters[4] = (Int_t)mTrigL->FormulaParameter4[crate-1][index];
    parameters[5] = (Int_t)mTrigL->FormulaParameter5[crate-1][index];
  }
}

//-----------------------------------------------------------------------------

float StBemcTables::calib(int det, int softId, int power) const {
    float val;
    getCalib(det, softId, power, val);
    return val;
}

float StBemcTables::pedestal(int det, int softId, int cap) const {
    float val, junk;
    getPedestal(det, softId, cap, val, junk);
    return val;
}

float StBemcTables::pedestalRMS(int det, int softId, int cap) const {
    float val, junk;
    getPedestal(det, softId, cap, junk, val);
    return val;
}

float StBemcTables::gain(int det, int softId) const {
    float val;
    getGain(det, softId, val);
    return val;
}

int StBemcTables::status(int det, int softId, const char *option) const {
    int val;
    getStatus(det, softId, val, option);
    return val;
}

//-----------------------------------------------------------------------------

int StBemcTables::triggerPatchStatus(int triggerPatchId) const {
    int val;
    getTriggerPatchStatus(triggerPatchId,val);
    return val;
}

int StBemcTables::triggerHighTowerStatus(int triggerPatchId) const {
    int val;
    getTriggerHighTowerStatus(triggerPatchId,val);
    return val;
}

int StBemcTables::triggerTowerStatus(int crate, int sequence) const {
    int val;
    getTriggerTowerStatus(crate,sequence,val);
    return val;
}

float StBemcTables::triggerPedestal(int crate, int sequence) const {
    float val;
    getTriggerPedestal(crate,sequence,val);
    return val;
}

int StBemcTables::triggerBitConversion(int crate, int patchSequence) const {
    int val;
    getTriggerBitConv(crate,patchSequence,val);
    return val;
}

int StBemcTables::triggerPedestalShift() const {
    int val;
    getTriggerPedestalShift(val);
    return val;
}

short StBemcTables::triggerPed4(int softId) const {
  short val;
  getTriggerPed4(softId,val);
  return val;
}

int StBemcTables::triggerFormulaTag(int crate, int patchSequence) const {
    int val;
    getTriggerFormulaTag(crate,patchSequence,val);
    return val;
}

int* StBemcTables::triggerFormulaParameters(int crate, int patchSequence) const {
    int * params = new int[6];
    getTriggerFormulaParameters(crate,patchSequence,params);
    return params;
}

//-----------------------------------------------------------------------------

int StBemcTables::triggerPatchStatusByID(int softId) const {
    int patchId;
    if(mDecoder->GetTriggerPatchFromTowerId(softId, patchId)) {
        return this->triggerPatchStatus(patchId);
    }
    LOG_ERROR << "Problem with StEmcDecoder::GetTriggerPatchFromTowerId for softId " << softId << endm;
    return -999;
}

int StBemcTables::triggerHighTowerStatusByID(int softId) const {
    int patchId;
    if(mDecoder->GetTriggerPatchFromTowerId(softId, patchId)) {
        return this->triggerHighTowerStatus(patchId);
    }
    LOG_ERROR << "Problem with StEmcDecoder::GetTriggerPatchFromTowerId for softId " << softId << endm;
    return -999;
}

int StBemcTables::triggerTowerStatusByID(int softId) const {
    int crate, sequence;
    if(mDecoder->GetCrateFromTowerId(softId, crate, sequence)) {
        return this->triggerTowerStatus(crate, sequence);
    }
    LOG_ERROR << "Problem with StEmcDecoder::GetCrateFromTowerId for softId " << softId << endm;
    return -999;
}

float StBemcTables::triggerPedestalByID(int softId) const {
    int crate, sequence;
    if(mDecoder->GetCrateFromTowerId(softId, crate, sequence)) {
        return this->triggerPedestal(crate, sequence);
    }
    LOG_ERROR << "Problem with StEmcDecoder::GetCrateFromTowerId for softId " << softId << endm;
    return -999;
}

int StBemcTables::triggerBitConversionByID(int softId) const {
    int patchId, crate, patchSequence;
    if(mDecoder->GetTriggerPatchFromTowerId(softId, patchId)) {
        if(mDecoder->GetCrateAndSequenceFromTriggerPatch(patchId, crate, patchSequence)) {
            if( (patchSequence%16 != 0) || (patchId%10 != patchSequence/16) ) {
                LOG_ERROR << "Please ask Adam what the heck is going on" << endm;
            }
            return this->triggerBitConversion(crate, patchSequence/16);
        }
        LOG_ERROR << "Problem with StEmcDecoder::GetCrateAndSequenceFromTriggerPatch for patchId " << patchId << endm;
    }
    LOG_ERROR << "Problem with StEmcDecoder::GetTriggerPatchFromTowerId for softId " << softId << endm;
    return -999;
}

int StBemcTables::triggerFormulaTagByID(int softId) const {
    int patchId, crate, patchSequence;
    if(mDecoder->GetTriggerPatchFromTowerId(softId, patchId)) {
        if(mDecoder->GetCrateAndSequenceFromTriggerPatch(patchId, crate, patchSequence)) {
            if( (patchSequence%16 != 0) || (patchId%10 != patchSequence/16) ) {
                LOG_ERROR << "Please ask Adam what the heck is going on" << endm;
            }
            return this->triggerFormulaTag(crate, patchSequence/16);
        }
        LOG_ERROR << "Problem with StEmcDecoder::GetCrateAndSequenceFromTriggerPatch for patchId " << patchId << endm;
    }
    LOG_ERROR << "Problem with StEmcDecoder::GetTriggerPatchFromTowerId for softId " << softId << endm;
    return -999;
}

int* StBemcTables::triggerFormulaParametersByID(int softId) const {
    int patchId, crate, patchSequence;
    if(mDecoder->GetTriggerPatchFromTowerId(softId, patchId)) {
        if(mDecoder->GetCrateAndSequenceFromTriggerPatch(patchId, crate, patchSequence)) {
            if( (patchSequence%16 != 0) || (patchId%10 != patchSequence/16) ) {
                LOG_ERROR << "Please ask Adam what the heck is going on" << endm;
            }
            return this->triggerFormulaParameters(crate, patchSequence/16);
        }
        LOG_ERROR << "Problem with StEmcDecoder::GetCrateAndSequenceFromTriggerPatch for patchId " << patchId << endm;
    }
    LOG_ERROR << "Problem with StEmcDecoder::GetTriggerPatchFromTowerId for softId " << softId << endm;
    return NULL;
}

/***************************************************************************
 *
 * $Log: StBemcTables.cxx,v $
 * Revision 1.14  2015/03/02 20:25:51  jkadkins
 * Updates to include "bemcTriggerPed4" for BEMC online trigger monitoring
 *
 * Revision 1.13  2010/01/27 21:39:51  perev
 * GetValidity now is static
 *
 * Revision 1.12  2009/01/02 02:56:18  kocolosk
 * use StMaker::GetDBTime() instead of GetDateTime()
 *
 * Revision 1.11  2007/11/08 11:12:39  kocolosk
 * remove debugging statements
 *
 * Revision 1.10  2007/10/01 19:53:43  kocolosk
 * another bugfix from Renee:  patchSequence is 0-9, not 0-159
 *
 * Revision 1.9  2007/10/01 15:03:35  kocolosk
 * bugfix in trigger DB methods that used StEmcDecoder, thanks to R. Fatemi
 *
 * Revision 1.8  2007/09/18 19:41:46  kocolosk
 * added an optional argument to status methods to get status for calib, pedestal, and gain tables
 *
 * Revision 1.7  2007/09/10 21:23:22  kocolosk
 * specify kTRUE as 2nd ctor argument to implement swapping corrections for 06/07 BPRS DB
 *
 * Revision 1.6  2007/08/21 18:39:22  kocolosk
 * added methods to get beginTime / endTime of a given DB table
 *
 *
 **************************************************************************/
