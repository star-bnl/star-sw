// $Id: StEmcMappingDb.cxx,v 1.1 2009/01/08 02:16:18 kocolosk Exp $

#include "StEmcMappingDb.h"

#include "StMessMgr.h"

#include "StDbLib/StDbManager.hh"
#include "StDbLib/StDbConfigNode.hh"
#include "StDbLib/StDbTable.h"
#include "StDbLib/StDataBaseI.hh"

#include "tables/St_bemcMap_Table.h"
#include "tables/St_bprsMap_Table.h"
#include "tables/St_bsmdeMap_Table.h"
#include "tables/St_bsmdpMap_Table.h"

#include "St_db_Maker/St_db_Maker.h"

ClassImp(StEmcMappingDb)

StEmcMappingDb::StEmcMappingDb(int date, int time) : mBemcMap(NULL), 
    mBprsMap(NULL), mSmdeMap(NULL), mSmdpMap(NULL), mGlobalDirty(true)
{
    StMaker *test = new StMaker();
    mDbMk = static_cast<St_db_Maker*>(test->GetMakerInheritsFrom("St_db_Maker"));
    delete test;
    
    if(!mDbMk || !mDbMk->TestBIT(StMaker::kInitEnd)) {
        StDbManager *mgr = StDbManager::Instance();
        StDbConfigNode *db = mgr->initConfig("Calibrations_emc");
        mBemcTable = db->addDbTable("bemcMap");
        mSmdeTable = db->addDbTable("bsmdeMap");
        mSmdpTable = db->addDbTable("bsmdpMap");
        mBprsTable = db->addDbTable("bprsMap");
        
        int elements[18000]; for(int i=0; i<18000; i++) elements[i] = i+1;
        
        mBemcTable->resizeElementID(4800);
        mBemcTable->setElementID(elements, 4800);
        
        mBprsTable->resizeElementID(4800);
        mBprsTable->setElementID(elements, 4800);
        
        mSmdeTable->resizeElementID(18000);
        mSmdeTable->setElementID(elements, 18000);
        
        mSmdpTable->resizeElementID(18000);
        mSmdpTable->setElementID(elements, 18000);
        
        mDBI = mgr->findDb("Calibrations_emc");
    }
    
    SetDateTime(date, time);
}

StEmcMappingDb::~StEmcMappingDb() {}

void StEmcMappingDb::SetDateTime(int date, int time) {
    if(mDbMk) {
        if(mDbMk->GetDate() != date || mDbMk->GetTime() != time) {
            LOG_ERROR << "StEmcMappingDb::SetDateTime(" << date << "," << time
                << ") doesn't match St_db_Maker: " << mDbMk->GetDate() << '\t'
                << mDbMk->GetTime() << endm;
            LOG_ERROR << "StEmcMappingDb will use St_db_Maker values" << endm;
        }
    }
    else {
        mBeginTime.Set(date, time); 
    }
    reload();
}

void StEmcMappingDb::SetFlavor(const char *flavor, const char *tablename) {
    if(mDbMk) {
        if(tablename){
            mDbMk->SetFlavor(flavor, tablename);
        }
        else {
            LOG_WARN << "calling St_db_Maker::SetFlavor(" << flavor 
                << ") from StEmcMappingDb" << endm;
            mDbMk->SetFlavor(flavor);
        }
    }
    else {
        if(tablename) {
            if(!strcmp(tablename, "bemcMap"))  mBemcTable->setFlavor(flavor);
            if(!strcmp(tablename, "bprsMap"))  mBprsTable->setFlavor(flavor);
            if(!strcmp(tablename, "bsmdeMap")) mSmdeTable->setFlavor(flavor);
            if(!strcmp(tablename, "bsmdpMap")) mSmdpTable->setFlavor(flavor);
        }
        else {
            mBemcTable->setFlavor(flavor);
            mBprsTable->setFlavor(flavor);
            mSmdeTable->setFlavor(flavor);
            mSmdpTable->setFlavor(flavor);
        }
        mGlobalDirty = true;
    }
    reload();
}

void StEmcMappingDb::SetMaxEntryTime(int date, int time) {
    if(mDbMk) {
        LOG_WARN << "calling St_db_Maker::SetMaxEntryTime(" << date << "," 
            << time << ") from StEmcMappingDb" << endm;
        mDbMk->SetMaxEntryTime(date, time);
    }
    else {
        unsigned unixMax = mDBI->getUnixTime(TDatime(date,time).AsSQLString());
        mBemcTable->setProdTime(unixMax);
        mBprsTable->setProdTime(unixMax);
        mSmdeTable->setProdTime(unixMax);
        mSmdpTable->setProdTime(unixMax);
        mGlobalDirty = true;
    }
    reload();
}

// struct declaration auto-generated at $STAR/include/bemcMap.h
const bemcMap_st & StEmcMappingDb::bemc(int softId) const {
    return mBemcMap[softId-1];
}

// struct declaration auto-generated at $STAR/include/bprsMap.h
const bprsMap_st & StEmcMappingDb::bprs(int softId) const {
    return mBprsMap[softId-1];
}

// struct declaration auto-generated at $STAR/include/bsmdeMap.h
const bsmdeMap_st & StEmcMappingDb::bsmde(int softId) const {
    return mSmdeMap[softId-1];
}

// struct declaration auto-generated at $STAR/include/bsmdpMap.h
const bsmdpMap_st & StEmcMappingDb::bsmdp(int softId) const {
    return mSmdpMap[softId-1];
}

int StEmcMappingDb::softIdFromMES(StDetectorId det, int m, int e, int s) const {
    switch(det) {
        case kBarrelEmcTowerId:
        for(int i=0; i<4800; i++) {
            if(mBemcMap[i].m == m && mBemcMap[i].e == e && mBemcMap[i].s == s) {
                return i+1;
            }
        }
        break;
        case kBarrelEmcPreShowerId:
        for(int i=0; i<4800; i++) {
            if(mBprsMap[i].m == m && mBprsMap[i].e == e && mBprsMap[i].s == s) {
                return i+1;
            }
        }
        break;
        case kBarrelSmdEtaStripId:
        for(int i=0; i<18000; i++) {
            if(mSmdeMap[i].m == m && mSmdeMap[i].e == e && mSmdeMap[i].s == s) {
                return i+1;
            }
        }
        break;
        case kBarrelSmdPhiStripId:
        for(int i=0; i<18000; i++) {
            if(mSmdpMap[i].m == m && mSmdpMap[i].e == e && mSmdpMap[i].s == s) {
                return i+1;
            }
        }
        default: break;
    }
    return 0;
}

int StEmcMappingDb::
softIdFromCrate(StDetectorId det, int crate, int channel) const {
    if(det == kBarrelEmcTowerId) {
        for(int i=0; i<4800; i++) {
            if(mBemcMap[i].crate == crate) {
                if(mBemcMap[i].crateChannel == channel) return i+1;
            }
        }
    }
    return 0;
}

int StEmcMappingDb::softIdFromDaqId(StDetectorId det, int daqId) const {
    if(det == kBarrelEmcTowerId) {
        for(int i=0; i<4800; i++) {
            if(mBemcMap[i].daqID == daqId) return i+1;
        }
    }
    return 0;
}

int StEmcMappingDb::
softIdFromTDC(StDetectorId det, int TDC, int channel) const {
    if(det == kBarrelEmcTowerId) {
        for(int i=0; i<4800; i++) {
            if(mBemcMap[i].TDC == TDC) {
                if(mBemcMap[i].crateChannel == channel) return i+1;
            }
        }
    }
    return 0;
}

int StEmcMappingDb::
softIdFromRDO(StDetectorId det, int rdo, int channel) const {
    switch(det) {
        case kBarrelEmcPreShowerId:
        for(int i=0; i<4800; i++) {
            if(mBprsMap[i].rdo == rdo && mBprsMap[i].rdoChannel == channel) {
                return i+1;
            }
        }
        break;
        case kBarrelSmdEtaStripId:
        for(int i=0; i<18000; i++) {
            if(mSmdeMap[i].rdo == rdo && mSmdeMap[i].rdoChannel == channel) {
                return i+1;
            }
        }
        break;
        case kBarrelSmdPhiStripId:
        for(int i=0; i<18000; i++) {
            if(mSmdpMap[i].rdo == rdo && mSmdpMap[i].rdoChannel == channel) {
                return i+1;
            }
        }
        default: break;
    }
    return 0;
}

/* Private methods used for caching SQL query results */
 
void StEmcMappingDb::reload() {
    if(mDbMk && mDbMk->TestBIT(StMaker::kInitEnd)) {
        LOG_DEBUG << "(re)loading StEmcMappingDb using St_db_Maker" << endm;
        TDataSet *DB = mDbMk->GetInputDB("Calibrations/emc/map");
        
        St_bemcMap *a = (St_bemcMap*)DB->Find("bemcMap");
        mBemcMap = (bemcMap_st*)a->GetTable();
        
        St_bprsMap *b = (St_bprsMap*)DB->Find("bprsMap");
        mBprsMap = (bprsMap_st*)b->GetTable();
        
        St_bsmdeMap *c = (St_bsmdeMap*)DB->Find("bsmdeMap");
        mSmdeMap = (bsmdeMap_st*)c->GetTable();
        
        St_bsmdpMap *d = (St_bsmdpMap*)DB->Find("bsmdpMap");
        mSmdpMap = (bsmdpMap_st*)d->GetTable();
    }
    else {
        LOG_DEBUG << "(re)loading StEmcMappingDb using StDbManager" << endm;
        StDbManager *mgr = StDbManager::Instance();
        mgr->setVerbose(false);
        mgr->setRequestTime(mBeginTime.AsSQLString());
        
        if(isDirty(mBemcTable)) {
            mgr->fetchDbTable(mBemcTable);
            mBemcMap = (bemcMap_st*)(mBemcTable->GetTable());
        }
        if(isDirty(mBprsTable)) {
            mgr->fetchDbTable(mBprsTable);
            mBprsMap = (bprsMap_st*)(mBprsTable->GetTable());
        }
        if(isDirty(mSmdeTable)) {
            mgr->fetchDbTable(mSmdeTable);
            mSmdeMap = (bsmdeMap_st*)(mSmdeTable->GetTable());
        }
        if(isDirty(mSmdpTable)) {
            mgr->fetchDbTable(mSmdpTable);
            mSmdpMap = (bsmdpMap_st*)(mSmdpTable->GetTable());
        }
        
        mGlobalDirty = false;
    }
}

bool StEmcMappingDb::isDirty(StDbTable *table) {
    if(mGlobalDirty) return true;
    
    unsigned unixTS = mDBI->getUnixTime(mBeginTime.AsSQLString());
    return (unixTS < table->getBeginTime()) || (unixTS > table->getEndTime());
}

/*****************************************************************************
 * $Log: StEmcMappingDb.cxx,v $
 * Revision 1.1  2009/01/08 02:16:18  kocolosk
 * move StEmcMappingDb/StEmcDecoder to StEmcUtil/database
 *
 * Revision 2.1  2008/12/05 19:05:32  kocolosk
 * new DB-backed implementation of StEmcDecoder
 *
 *****************************************************************************/
