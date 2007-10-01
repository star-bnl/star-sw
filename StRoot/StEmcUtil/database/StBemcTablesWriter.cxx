/***************************************************************************
 *
 * $Id: StBemcTablesWriter.cxx,v 1.2 2007/10/01 16:57:29 kocolosk Exp $
 * Author:      Adam Kocoloski, MIT, kocolosk@mit.edu
 *
 ***************************************************************************/

#include "StBemcTablesWriter.h"

#include "TSystem.h"
#include "TFile.h"
#include "TDatime.h"

#include "TUnixTime.h"
#include "StDaqLib/EMC/StEmcDecoder.h"
#include "StMessMgr.h"

#include "StEmcRawMaker/defines.h"

ClassImp(StBemcTablesWriter)

StBemcTablesWriter::StBemcTablesWriter() : StBemcTables() {
    mDbTables["bemcPed"]          = NULL;
    mDbTables["bprsPed"]          = NULL;
    mDbTables["bsmdePed"]         = NULL;
    mDbTables["bsmdpPed"]         = NULL;
    
    mDbTables["bemcStatus"]       = NULL;
    mDbTables["bprsStatus"]       = NULL;
    mDbTables["bsmdeStatus"]      = NULL;
    mDbTables["bsmdpStatus"]      = NULL;
    
    mDbTables["bemcCalib"]        = NULL;
    mDbTables["bprsCalib"]        = NULL;
    mDbTables["bsmdeCalib"]       = NULL;
    mDbTables["bsmdpCalib"]       = NULL;
    
    mDbTables["bemcGain"]         = NULL;
    mDbTables["bprsGain"]         = NULL;
    mDbTables["bsmdeGain"]        = NULL;
    mDbTables["bsmdpGain"]        = NULL;
    
    mDbTables["bemcTriggerPed"]   = NULL;
    mDbTables["bemcTriggerStatus"]= NULL;
    mDbTables["bemcTriggerLUT"]   = NULL;
    
    mDbHandler = new StEmcDbHandler();
}

StBemcTablesWriter::~StBemcTablesWriter() {
    for(map<string, StDbTable*>::iterator it=mDbTables.begin(); it!=mDbTables.end(); it++) {
        delete it->second;
    }
    mDbTables.clear();
    
    if(mDbHandler) delete mDbHandler;
}

void StBemcTablesWriter::loadTables(const char * sqlTime, const char * flavor) {
    TDatime dt(sqlTime);
    TUnixTime ut(dt, 1); //sqlTime must be in GMT
    
    mDecoder->SetDateTime(dt.GetDate(), dt.GetTime());
    
    // the BTOW map fix should be used *ONLY* for runs before 2006
    // For runs after 2006-01-01 the database is supposed to be fixed
    if(dt.GetDate() >= 20060101) mBtowMapFix = kFALSE;
    
    for(map<string, StDbTable*>::iterator it = mDbTables.begin(); it!=mDbTables.end(); it++) {
        string tableName = it->first;
        StDbTable * t = it->second;
        if( (t==NULL) || (ut() < t->getBeginTime()) || (ut() >= t->getEndTime()) ) {
            mDbHandler->setTimeStamp(sqlTime);
            mDbHandler->setFlavor(flavor);
            mDbHandler->setTableName((char*)tableName.c_str());
            it->second = mDbHandler->getDbTable();
            t = it->second;
            
            string beginTime = StEmcDbHandler::timeToSqlTime(t->getBeginDateTime());
            string endTime   = StEmcDbHandler::timeToSqlTime(t->getEndDateTime());
            
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
    }
    
    if(mBtowP) delete mBtowP;
    mBtowP = (emcPed_st*) mDbTables["bemcPed"]->GetTableCpy();
    
    if(mBprsP) delete mBprsP;
    mBprsP = (emcPed_st*) mDbTables["bprsPed"]->GetTableCpy();
    
    if(mSmdeP) delete mSmdeP;
    mSmdeP = (smdPed_st*) mDbTables["bsmdePed"]->GetTableCpy();
    
    if(mSmdpP) delete mSmdpP;
    mSmdpP = (smdPed_st*) mDbTables["bsmdpPed"]->GetTableCpy();
    
    
    if(mBtowS) delete mBtowS;
    mBtowS = (emcStatus_st*) mDbTables["bemcStatus"]->GetTableCpy();
    
    if(mBprsS) delete mBprsS;
    mBprsS = (emcStatus_st*) mDbTables["bprsStatus"]->GetTableCpy();
    
    if(mSmdeS) delete mSmdeS;
    mSmdeS = (smdStatus_st*) mDbTables["bsmdeStatus"]->GetTableCpy();
    
    if(mSmdpS) delete mSmdpS;
    mSmdpS = (smdStatus_st*) mDbTables["bsmdpStatus"]->GetTableCpy();
    
    
    if(mBtowC) delete mBtowC;
    mBtowC = (emcCalib_st*) mDbTables["bemcCalib"]->GetTableCpy();
    
    if(mBprsC) delete mBprsC;
    mBprsC = (emcCalib_st*) mDbTables["bprsCalib"]->GetTableCpy();
    
    if(mSmdeC) delete mSmdeC;
    mSmdeC = (smdCalib_st*) mDbTables["bsmdeCalib"]->GetTableCpy();
    
    if(mSmdpC) delete mSmdpC;
    mSmdpC = (smdCalib_st*) mDbTables["bsmdpCalib"]->GetTableCpy();
    

    if(mBtowG) delete mBtowG;
    mBtowG = (emcGain_st*) mDbTables["bemcGain"]->GetTableCpy();
    
    if(mBprsG) delete mBprsG;
    mBprsG = (emcGain_st*) mDbTables["bprsGain"]->GetTableCpy();
    
    if(mSmdeG) delete mSmdeG;
    mSmdeG = (smdGain_st*) mDbTables["bsmdeGain"]->GetTableCpy();
    
    if(mSmdpG) delete mSmdpG;
    mSmdpG = (smdGain_st*) mDbTables["bsmdpGain"]->GetTableCpy();
    
    
    if(mTrigS) delete mTrigS;
    mTrigS = (emcTriggerStatus_st*) mDbTables["bemcTriggerStatus"]->GetTableCpy();
    
    if(mTrigP) delete mTrigP;
    mTrigP = (emcTriggerPed_st*) mDbTables["bemcTriggerPed"]->GetTableCpy();
    
    if(mTrigL) delete mTrigL;
    mTrigL = (emcTriggerLUT_st*) mDbTables["bemcTriggerLUT"]->GetTableCpy();
}

void StBemcTablesWriter::setCalib(int det, int softId, int power, float val) {
    switch(det) {
        case BTOW:
            mBtowC->AdcToE[softId-1][power] = val;
            break;
        
        case BPRS:
            mBprsC->AdcToE[softId-1][power] = val;
            break;
        
        case BSMDE:
            mSmdeC->AdcToE[softId-1][power] = val;
            break;
        
        case BSMDP:
            mSmdpC->AdcToE[softId-1][power] = val;
            break;
    }
}

void StBemcTablesWriter::setPedestal(int det, int softId, int cap, float val) {
    short packedValue = static_cast<short>(val*100.0);
    switch(det) {
        case BTOW:
            mBtowP->AdcPedestal[softId-1] = packedValue;
            break;
        
        case BPRS:
            mBprsP->AdcPedestal[softId-1] = packedValue;
            break;
        
        case BSMDE:
            mSmdeP->AdcPedestal[softId-1][cap] = packedValue;
            break;
            
        case BSMDP:
            mSmdpP->AdcPedestal[softId-1][cap] = packedValue;
            break;
    }
}

void StBemcTablesWriter::setPedestalRMS(int det, int softId, int cap, float val) {
    short packedValue = static_cast<short>(val*100.0);
    switch(det) {
        case BTOW:
            mBtowP->AdcPedestalRMS[softId-1] = packedValue;
            break;
        
        case BPRS:
            mBprsP->AdcPedestalRMS[softId-1] = packedValue;
            break;
        
        case BSMDE:
            mSmdeP->AdcPedestalRMS[softId-1][cap] = packedValue;
            break;
            
        case BSMDP:
            mSmdpP->AdcPedestalRMS[softId-1][cap] = packedValue;
            break;
    }
}

void StBemcTablesWriter::setGain(int det, int softId, float val) {
    switch(det) {
        case BTOW:
            mBtowG->Gain[softId-1] = val;
            break;
        
        case BPRS:
            mBprsG->Gain[softId-1] = val;
            break;
        
        case BSMDE:
            mSmdeG->Gain[softId-1] = val;
            break;
        
        case BSMDP:
            mSmdpG->Gain[softId-1] = val;
            break;
    }
}

void StBemcTablesWriter::setStatus(int det, int softId, unsigned short val) {
    if(val > 255) {
        cout << "status must be between 0 and 255 -- you supplied " << val << endl;
        return;
    }
    
    switch(det) {
        case BTOW:
            mBtowS->Status[softId-1] = val;
            break;
        
        case BPRS:
            mBprsS->Status[softId-1] = val;
            break;
        
        case BSMDE:
            mSmdeS->Status[softId-1] = val;
            break;
        
        case BSMDP:
            mSmdpS->Status[softId-1] = val;
            break;
    }
}

void StBemcTablesWriter::setCalibStatus(int det, int softId, unsigned short val) {
    if(val > 255) {
        cout << "status must be between 0 and 255 -- you supplied " << val << endl;
        return;
    }
    
    switch(det) {
        case BTOW:
            mBtowC->Status[softId-1] = val;
            break;
        
        case BPRS:
            mBprsC->Status[softId-1] = val;
            break;
        
        case BSMDE:
            mSmdeC->Status[softId-1] = val;
            break;
        
        case BSMDP:
            mSmdpC->Status[softId-1] = val;
            break;
    }
}

void StBemcTablesWriter::setPedestalStatus(int det, int softId, unsigned short val) {
    if(val > 255) {
        cout << "status must be between 0 and 255 -- you supplied " << val << endl;
        return;
    }
    
    switch(det) {
        case BTOW:
            mBtowP->Status[softId-1] = val;
            break;
        
        case BPRS:
            mBprsP->Status[softId-1] = val;
            break;
        
        case BSMDE:
            mSmdeP->Status[softId-1] = val;
            break;
        
        case BSMDP:
            mSmdpP->Status[softId-1] = val;
            break;
    }
}

void StBemcTablesWriter::setGainStatus(int det, int softId, unsigned short val) {
    if(val > 255) {
        cout << "status must be between 0 and 255 -- you supplied " << val << endl;
        return;
    }
    
    switch(det) {
        case BTOW:
            mBtowG->Status[softId-1] = val;
            break;
        
        case BPRS:
            mBprsG->Status[softId-1] = val;
            break;
        
        case BSMDE:
            mSmdeG->Status[softId-1] = val;
            break;
        
        case BSMDP:
            mSmdpG->Status[softId-1] = val;
            break;
    }
}

void StBemcTablesWriter::writeToDb(const char * tableName, const char * timeStamp, const char * flavor) {
    mDbHandler->setTableName(tableName);
    mDbHandler->setTimeStamp(timeStamp);
    mDbHandler->setFlavor(flavor);
    
    if(!strcmp(tableName, "bemcCalib"))   { mDbHandler->writeToDb(reinterpret_cast<char*>(mBtowC)); return; }
    if(!strcmp(tableName, "bprsCalib"))   { mDbHandler->writeToDb(reinterpret_cast<char*>(mBprsC)); return; }
    if(!strcmp(tableName, "bsmdeCalib"))  { mDbHandler->writeToDb(reinterpret_cast<char*>(mSmdeC)); return; }
    if(!strcmp(tableName, "bsmdpCalib"))  { mDbHandler->writeToDb(reinterpret_cast<char*>(mSmdpC)); return; }
                                                                                                            
    if(!strcmp(tableName, "bemcPed"))     { mDbHandler->writeToDb(reinterpret_cast<char*>(mBtowP)); return; }
    if(!strcmp(tableName, "bprsPed"))     { mDbHandler->writeToDb(reinterpret_cast<char*>(mBprsP)); return; }
    if(!strcmp(tableName, "bsmdePed"))    { mDbHandler->writeToDb(reinterpret_cast<char*>(mSmdeP)); return; }
    if(!strcmp(tableName, "bsmdpPed"))    { mDbHandler->writeToDb(reinterpret_cast<char*>(mSmdpP)); return; }
                                                                                                            
    if(!strcmp(tableName, "bemcGain"))    { mDbHandler->writeToDb(reinterpret_cast<char*>(mBtowG)); return; }
    if(!strcmp(tableName, "bprsGain"))    { mDbHandler->writeToDb(reinterpret_cast<char*>(mBprsG)); return; }
    if(!strcmp(tableName, "bsmdeGain"))   { mDbHandler->writeToDb(reinterpret_cast<char*>(mSmdeG)); return; }
    if(!strcmp(tableName, "bsmdpGain"))   { mDbHandler->writeToDb(reinterpret_cast<char*>(mSmdpG)); return; }
    
    if(!strcmp(tableName, "bemcStatus"))  { mDbHandler->writeToDb(reinterpret_cast<char*>(mBtowS)); return; }
    if(!strcmp(tableName, "bprsStatus"))  { mDbHandler->writeToDb(reinterpret_cast<char*>(mBprsS)); return; }
    if(!strcmp(tableName, "bsmdeStatus")) { mDbHandler->writeToDb(reinterpret_cast<char*>(mSmdeS)); return; }
    if(!strcmp(tableName, "bsmdpStatus")) { mDbHandler->writeToDb(reinterpret_cast<char*>(mSmdpS)); return; }
    
    if(!strcmp(tableName, "bemcTriggerPed"))    { mDbHandler->writeToDb(reinterpret_cast<char*>(mTrigP)); return; }
    if(!strcmp(tableName, "bemcTriggerStatus")) { mDbHandler->writeToDb(reinterpret_cast<char*>(mTrigS)); return; }
    if(!strcmp(tableName, "bemcTriggerLUT"))    { mDbHandler->writeToDb(reinterpret_cast<char*>(mTrigL)); return; }
    
    
    
    LOG_ERROR << "Can't upload a table with name = " << tableName << endm;
}

void StBemcTablesWriter::writeToFile(const char * fileName) {
    TFile *f = new TFile(fileName,"recreate");
    string baseName = gSystem->BaseName(fileName);
    
    
    if(baseName.find("bemcCalib") == 0) {
        St_emcCalib* table = new St_emcCalib("bemcCalib",1);
        table->AddAt(static_cast<void*>(mBtowC),0);
        table->Write();
    }
    else if(baseName.find("bprsCalib") == 0) {
        St_emcCalib* table = new St_emcCalib("bprsCalib",1);
        table->AddAt(static_cast<void*>(mBprsC),0);
        table->Write();
    }
    else if(baseName.find("bsmdeCalib") == 0) {
        St_smdCalib* table = new St_smdCalib("bsmdeCalib",1);
        table->AddAt(static_cast<void*>(mSmdeC),0);
        table->Write();
    }
    else if(baseName.find("bsmdpCalib") == 0) {
        St_smdCalib* table = new St_smdCalib("bsmdpCalib",1);
        table->AddAt(static_cast<void*>(mSmdpC),0);
        table->Write();
    }
    
    else if(baseName.find("bemcPed") == 0) {
        St_emcPed* table = new St_emcPed("bemcPed",1);
        table->AddAt(static_cast<void*>(mBtowP),0);
        table->Write();
    }
    else if(baseName.find("bprsPed") == 0) {
        St_emcPed* table = new St_emcPed("bprsPed",1);
        table->AddAt(static_cast<void*>(mBprsP),0);
        table->Write();
    }
    else if(baseName.find("bsmdePed") == 0) {
        St_smdPed* table = new St_smdPed("bsmdePed",1);
        table->AddAt(static_cast<void*>(mSmdeP),0);
        table->Write();
    }
    else if(baseName.find("bsmdpPed") == 0) {
        St_smdPed* table = new St_smdPed("bsmdpPed",1);
        table->AddAt(static_cast<void*>(mSmdpP),0);
        table->Write();
    }
    
    else if(baseName.find("bemcGain") == 0) {
        St_emcGain* table = new St_emcGain("bemcGain",1);
        table->AddAt(static_cast<void*>(mBtowG),0);
        table->Write();
    }
    else if(baseName.find("bprsGain") == 0) {
        St_emcGain* table = new St_emcGain("bprsGain",1);
        table->AddAt(static_cast<void*>(mBprsG),0);
        table->Write();
    }
    else if(baseName.find("bsmdeGain") == 0) {
        St_smdGain* table = new St_smdGain("bsmdeGain",1);
        table->AddAt(static_cast<void*>(mSmdeG),0);
        table->Write();
    }
    else if(baseName.find("bsmdpGain") == 0) {
        St_smdGain* table = new St_smdGain("bsmdpGain",1);
        table->AddAt(static_cast<void*>(mSmdpG),0);
        table->Write();
    }
    
    else if(baseName.find("bemcStatus") == 0) {
        St_emcStatus* table = new St_emcStatus("bemcStatus",1);
        table->AddAt(static_cast<void*>(mBtowS),0);
        table->Write();
    }
    else if(baseName.find("bprsStatus") == 0) {
        St_emcStatus* table = new St_emcStatus("bprsStatus",1);
        table->AddAt(static_cast<void*>(mBprsS),0);
        table->Write();
    }
    else if(baseName.find("bsmdeStatus") == 0) {
        St_smdStatus* table = new St_smdStatus("bsmdeStatus",1);
        table->AddAt(static_cast<void*>(mSmdeS),0);
        table->Write();
    }
    else if(baseName.find("bsmdpStatus") == 0) {
        St_smdStatus* table = new St_smdStatus("bsmdpStatus",1);
        table->AddAt(static_cast<void*>(mSmdpS),0);
        table->Write();
    }
    
    f->Close();
}

void StBemcTablesWriter::loadTableFromFile(TFile *f) {
    string fileName = gSystem->BaseName(f->GetName());
    
    if(fileName.find("bemcPed") == 0) {
        if(mBtowP) delete mBtowP;
        mBtowP = ((St_emcPed*)f->Get("bemcPed"))->GetTable();
    }
    else if(fileName.find("bprsPed") == 0) {
        if(mBprsP) delete mBprsP;
        mBprsP = ((St_emcPed*)f->Get("bprsPed"))->GetTable();
    }
    else if(fileName.find("bsmdePed") == 0) {
        if(mSmdeP) delete mSmdeP;
        mSmdeP = ((St_smdPed*)f->Get("bsmdePed"))->GetTable();
    }
    else if(fileName.find("bsmdpPed") == 0) {
        if(mSmdpP) delete mSmdpP;
        mSmdpP = ((St_smdPed*)f->Get("bsmdpPed"))->GetTable();
    }
    else if(fileName.find("bemcStatus") == 0) {
        if(mBtowS) delete mBtowS;
        mBtowS = ((St_emcStatus*)f->Get("bemcStatus"))->GetTable();
    }
    else if(fileName.find("bprsStatus") == 0) {
        if(mBprsS) delete mBprsS;
        mBprsS = ((St_emcStatus*)f->Get("bprsStatus"))->GetTable();
    }
    else if(fileName.find("bsmdeStatus") == 0) {
        if(mSmdeS) delete mSmdeS;
        mSmdeS = ((St_smdStatus*)f->Get("bsmdeStatus"))->GetTable();
    }
    else if(fileName.find("bsmdpStatus") == 0) {
        if(mSmdpS) delete mSmdpS;
        mSmdpS = ((St_smdStatus*)f->Get("bsmdpStatus"))->GetTable();
    }
    else if(fileName.find("bemcCalib") == 0) {
        if(mBtowC) delete mBtowC;
        mBtowC = ((St_emcCalib*)f->Get("bemcCalib"))->GetTable();
    }
    else if(fileName.find("bprsCalib") == 0) {
        if(mBprsC) delete mBprsC;
        mBprsC = ((St_emcCalib*)f->Get("bprsCalib"))->GetTable();
    }
    else if(fileName.find("bsmdeCalib") == 0) {
        if(mSmdeC) delete mSmdeC;
        mSmdeC = ((St_smdCalib*)f->Get("bsmdeCalib"))->GetTable();
    }
    else if(fileName.find("bsmdpCalib") == 0) {
        if(mSmdpC) delete mSmdpC;
        mSmdpC = ((St_smdCalib*)f->Get("bsmdpCalib"))->GetTable();
    }
    else if(fileName.find("bemcGain") == 0) {
        if(mBtowG) delete mBtowG;
        mBtowG = ((St_emcGain*)f->Get("bemcGain"))->GetTable();
    }
    else if(fileName.find("bprsGain") == 0) {
        if(mBprsG) delete mBprsG;
        mBprsG = ((St_emcGain*)f->Get("bprsGain"))->GetTable();
    }
    else if(fileName.find("bsmdeGain") == 0) {
        if(mSmdeG) delete mSmdeG;
        mSmdeG = ((St_smdGain*)f->Get("bsmdeGain"))->GetTable();
    }
    else if(fileName.find("bsmdpGain") == 0) {
        if(mSmdpG) delete mSmdpG;
        mSmdpG = ((St_smdGain*)f->Get("bsmdpGain"))->GetTable();
    }
    else if(fileName.find("bemcTriggerStatus") == 0) {
        if(mTrigS) delete mTrigS;
        mTrigS = ((St_emcTriggerStatus*)f->Get("bemcTriggerStatus"))->GetTable();
    }
    else if(fileName.find("bemcTriggerPed") == 0) {
        if(mTrigP) delete mTrigP;
        mTrigP = ((St_emcTriggerPed*)f->Get("bemcTriggerPed"))->GetTable();
    }
    else if(fileName.find("bemcTriggerLUT") == 0) {
        if(mTrigL) delete mTrigL;
        mTrigL = ((St_emcTriggerLUT*)f->Get("bemcTriggerLUT"))->GetTable();
    }
}

/***************************************************************************
 *
 * $Log: StBemcTablesWriter.cxx,v $
 * Revision 1.2  2007/10/01 16:57:29  kocolosk
 * allow to uploaded bemcTrigger* tables too
 *
 * Revision 1.1  2007/09/08 01:22:37  kocolosk
 * StBemcTablesWriter provides common interface for inserting DB tables
 *
 *
 **************************************************************************/
