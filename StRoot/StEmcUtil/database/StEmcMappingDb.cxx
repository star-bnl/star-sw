// $Id: StEmcMappingDb.cxx,v 1.4 2009/01/31 20:15:22 kocolosk Exp $

#include "StEmcMappingDb.h"

#include "TUnixTime.h"
#include "StMessMgr.h"
#include "StMaker.h"

#include "StDbLib/StDbManager.hh"
#include "StDbLib/StDbConfigNode.hh"
#include "StDbLib/StDbTable.h"

#include "tables/St_bemcMap_Table.h"
#include "tables/St_bprsMap_Table.h"
#include "tables/St_bsmdeMap_Table.h"
#include "tables/St_bsmdpMap_Table.h"

ClassImp(StEmcMappingDb)

StEmcMappingDb::StEmcMappingDb(int date, int time) : mBemcTTable(NULL),
    mBprsTTable(NULL), mSmdeTTable(NULL), mSmdpTTable(NULL), mBemcValidity(-2),
    mBprsValidity(-2), mSmdeValidity(-2), mSmdpValidity(-2), mBemcTable(NULL),
    mBprsTable(NULL), mSmdeTable(NULL), mSmdpTable(NULL), mBemcDirty(true),
    mBprsDirty(true), mSmdeDirty(true), mSmdpDirty(true)
{
    mChain = StMaker::GetChain();
    
    if(!mChain) {
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
        
        mBemcTTable = new St_bemcMap();
        mBemcTTable->Adopt(4800, mBemcTable->GetTable());
        
        mBprsTTable = new St_bprsMap();
        mBprsTTable->Adopt(4800, mBprsTable->GetTable());
        
        mSmdeTTable = new St_bsmdeMap();
        mSmdeTTable->Adopt(18000, mSmdeTable->GetTable());
        
        mSmdpTTable = new St_bsmdpMap();
        mSmdpTTable->Adopt(18000, mSmdpTable->GetTable());
        
        SetDateTime(date, time);
    }
}

StEmcMappingDb::~StEmcMappingDb() {
    // I'm intentionally not deleting the TTables created on the heap above.  If
    // I tried to delete them I'd end up with a double-delete of the mapping 
    // memory, since the StDbTables already own it.  I don't want to copy the
    // data because it's ~6 MB or so.  Each TTable only has 2 Long_ts and an
    // Int_t; I think this should be OK.
}

void StEmcMappingDb::SetDateTime(int date, int time) {
    if(mChain) {
        LOG_ERROR << "StEmcMappingDb::SetDateTime is illegal in a chain" << endm;
    }
    else {
        mBeginTime.Set(date, time);
        unsigned unix = TUnixTime::Convert(mBeginTime, true);
        if( !(mBemcTable->getBeginTime() < unix < mBemcTable->getEndTime()) )
            mBemcDirty = true;
        if( !(mBprsTable->getBeginTime() < unix < mBprsTable->getEndTime()) )
            mBprsDirty = true;
        if( !(mSmdeTable->getBeginTime() < unix < mSmdeTable->getEndTime()) )
            mSmdeDirty = true;
        if( !(mSmdpTable->getBeginTime() < unix < mSmdpTable->getEndTime()) )
            mSmdpDirty = true;
    }
}

void StEmcMappingDb::SetFlavor(const char *flavor, const char *tablename) {
    if(mChain) {
        LOG_ERROR << "StEmcMappingDb::SetFlavor is illegal in a chain" << endm;
    }
    else {
        if(!tablename || !strcmp(tablename, "bemcMap"))  {
            if(strcmp(mBemcTable->getFlavor(), flavor)) {
                mBemcTable->setFlavor(flavor);
                mBemcDirty = true;
            }
        }
        if(!tablename || !strcmp(tablename, "bprsMap")) {
            if(strcmp(mBprsTable->getFlavor(), flavor)) {
                mBprsTable->setFlavor(flavor);
                mBprsDirty = true;
            }
        }
        if(!tablename || !strcmp(tablename, "bsmdeMap")) {
            if(strcmp(mSmdeTable->getFlavor(), flavor)) {
                mSmdeTable->setFlavor(flavor);
                mSmdeDirty = true;
            }
        }
        if(!tablename || !strcmp(tablename, "bsmdpMap")) {
            if(strcmp(mSmdpTable->getFlavor(), flavor)) {
                mSmdpTable->setFlavor(flavor);
                mSmdpDirty = true;
            }
        }
    }
}

void StEmcMappingDb::SetMaxEntryTime(int date, int time) {
    if(mChain) {
        LOG_ERROR << "StEmcMappingDb::SetMaxEntryTime is illegal in a chain" << endm;
    }
    else {
        unsigned unixMax = TUnixTime::Convert(TDatime(date,time), true);
        if(mBemcTable->getProdTime() != unixMax) {
            mBemcTable->setProdTime(unixMax);
            mBemcDirty = true;
        }
        if(mBprsTable->getProdTime() != unixMax) {
            mBprsTable->setProdTime(unixMax);
            mBprsDirty = true;
        }
        if(mSmdeTable->getProdTime() != unixMax) {
            mSmdeTable->setProdTime(unixMax);
            mSmdeDirty = true;
        }
        if(mSmdpTable->getProdTime() != unixMax) {
            mSmdpTable->setProdTime(unixMax);
            mSmdpDirty = true;
        }        
    }
}

// struct declaration auto-generated at $STAR/include/bemcMap.h
const bemcMap_st* StEmcMappingDb::bemc() const {
    maybe_reload(kBarrelEmcTowerId);
    return mBemcTTable->GetTable();
}

// struct declaration auto-generated at $STAR/include/bprsMap.h
const bprsMap_st* StEmcMappingDb::bprs() const {
    maybe_reload(kBarrelEmcPreShowerId);
    return mBprsTTable->GetTable();
}

// struct declaration auto-generated at $STAR/include/bsmdeMap.h
const bsmdeMap_st* StEmcMappingDb::bsmde() const {
    maybe_reload(kBarrelSmdEtaStripId);
    return mSmdeTTable->GetTable();
}

// struct declaration auto-generated at $STAR/include/bsmdpMap.h
const bsmdpMap_st* StEmcMappingDb::bsmdp() const {
    maybe_reload(kBarrelSmdPhiStripId);
    return mSmdpTTable->GetTable();
}

int 
StEmcMappingDb::softIdFromMES(StDetectorId det, int m, int e, int s) const {
    switch(det) {
        case kBarrelEmcTowerId:
        const bemcMap_st* tow = bemc();
        for(int i=0; i<4800; i++) {
            if(tow[i].m == m && tow[i].e == e && tow[i].s == s) {
                return i+1;
            }
        }
        break;
        case kBarrelEmcPreShowerId:
        const bprsMap_st* prs = bprs();
        for(int i=0; i<4800; i++) {
            if(prs[i].m == m && prs[i].e == e && prs[i].s == s) {
                return i+1;
            }
        }
        break;
        case kBarrelSmdEtaStripId:
        const bsmdeMap_st* smde = bsmde();
        for(int i=0; i<18000; i++) {
            if(smde[i].m == m && smde[i].e == e && smde[i].s == s) {
                return i+1;
            }
        }
        break;
        case kBarrelSmdPhiStripId:
        const bsmdpMap_st* smdp = bsmdp();
        for(int i=0; i<18000; i++) {
            if(smdp[i].m == m && smdp[i].e == e && smdp[i].s == s) {
                return i+1;
            }
        }
        default: break;
    }
    return 0;
}

int 
StEmcMappingDb::softIdFromCrate(StDetectorId det, int crate, int channel) const {
    if(det == kBarrelEmcTowerId) {
        const bemcMap_st* map = bemc();
        for(int i=0; i<4800; i++) {
            if(map[i].crate == crate) {
                if(map[i].crateChannel == channel) 
                    return i+1;
            }
        }
    }
    return 0;
}

int 
StEmcMappingDb::softIdFromDaqId(StDetectorId det, int daqId) const {
    if(det == kBarrelEmcTowerId) {
        const bemcMap_st* map = bemc();
        for(int i=0; i<4800; ++i) {
            if(map[i].daqID == daqId) return i+1;
        }
    }
    return 0;
}

int 
StEmcMappingDb::softIdFromTDC(StDetectorId det, int TDC, int channel) const {
    if(det == kBarrelEmcTowerId) {
        const bemcMap_st* map = bemc();
        for(int i=0; i<4800; i++) {
            if(map[i].TDC == TDC) {
                if(map[i].crateChannel == channel) return i+1;
            }
        }
    }
    return 0;
}

int 
StEmcMappingDb::softIdFromRDO(StDetectorId det, int rdo, int channel) const {
    switch(det) {
        case kBarrelEmcPreShowerId:
        const bprsMap_st* prs = bprs();
        for(int i=0; i<4800; i++) {
            if(prs[i].rdo == rdo && prs[i].rdoChannel == channel) {
                return i+1;
            }
        }
        break;
        case kBarrelSmdEtaStripId:
        const bsmdeMap_st *smde = bsmde();
        for(int i=0; i<18000; i++) {
            if(smde[i].rdo == rdo && smde[i].rdoChannel == channel) {
                return i+1;
            }
        }
        break;
        case kBarrelSmdPhiStripId:
        const bsmdpMap_st *smdp = bsmdp();
        for(int i=0; i<18000; i++) {
            if(smdp[i].rdo == rdo && smdp[i].rdoChannel == channel) {
                return i+1;
            }
        }
        default: break;
    }
    return 0;
}

/* Private methods used for caching SQL query results */
void StEmcMappingDb::maybe_reload(StDetectorId det) const {
    switch(det) {
        case kBarrelEmcTowerId:
        if(mChain) {
            if(!mBemcTTable) {
                TDataSet *DB = mChain->GetInputDB("Calibrations/emc/map");
                if(DB) mBemcTTable = static_cast<St_bemcMap*>(DB->Find("bemcMap"));
            }
        } else {
            if(mBemcDirty) reload_dbtable(mBemcTable);
            mBemcDirty = false;
        }
        break;
        
        case kBarrelEmcPreShowerId:
        if(mChain) {
            if(!mBprsTTable) {
                TDataSet *DB = mChain->GetInputDB("Calibrations/emc/map");
                if(DB) mBprsTTable = static_cast<St_bprsMap*>(DB->Find("bprsMap"));
            }
        } else {
            if(mBprsDirty) reload_dbtable(mBprsTable);
            mBprsDirty = false;
        }
        break;
        
        case kBarrelSmdEtaStripId:
        if(mChain) {
            if(!mSmdeTTable) {
                TDataSet *DB = mChain->GetInputDB("Calibrations/emc/map");
                if(DB) mSmdeTTable = static_cast<St_bsmdeMap*>(DB->Find("bsmdeMap"));
            }
        } else {
            if(mSmdeDirty) reload_dbtable(mSmdeTable);
            mSmdeDirty = false;
        }
        break;
        
        case kBarrelSmdPhiStripId:
        if(mChain) {
            if(!mSmdpTTable) {
                TDataSet *DB = mChain->GetInputDB("Calibrations/emc/map");
                if(DB) mSmdpTTable = static_cast<St_bsmdpMap*>(DB->Find("bsmdpMap"));
            }
        } else {
            if(mSmdpDirty) reload_dbtable(mSmdpTable);
            mSmdpDirty = false;
        }
        default: break;
    }
}

void StEmcMappingDb::reload_dbtable(StDbTable* table) const {
    LOG_DEBUG << "(re)loading mapping table using StDbManager" << endm;
    StDbManager *mgr = StDbManager::Instance();
    mgr->setVerbose(false);
    mgr->setRequestTime(mBeginTime.AsSQLString());
    mgr->fetchDbTable(table);
}

/*****************************************************************************
 * $Log: StEmcMappingDb.cxx,v $
 * Revision 1.4  2009/01/31 20:15:22  kocolosk
 * be much lazier about pulling data from DB
 *
 * Revision 1.1  2009/01/08 02:16:18  kocolosk
 * move StEmcMappingDb/StEmcDecoder to StEmcUtil/database
 *
 * Revision 2.1  2008/12/05 19:05:32  kocolosk
 * new DB-backed implementation of StEmcDecoder
 *
 *****************************************************************************/
