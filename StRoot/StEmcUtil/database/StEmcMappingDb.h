#ifndef STAR_StEmcMappingDb
#define STAR_StEmcMappingDb

// $Id: StEmcMappingDb.h,v 1.4 2009/02/01 17:34:52 kocolosk Exp $

/*****************************************************************************
 * @class StEmcMappingDb
 * @author Adam Kocoloski
 *
 * Wrapper around b*Map tables in Calibrations_emc. Relies on St_db_Maker to 
 * choose the correct tables if the Maker is present, otherwise it calls 
 * StDbManager directly.
 *****************************************************************************/

#include "TObject.h"
#include "TDatime.h"

#include "StEvent/StEnumerations.h"

#include "bemcMap.h"
#include "bprsMap.h"
#include "bsmdeMap.h"
#include "bsmdpMap.h"

class St_bemcMap;
class St_bsmdeMap;
class St_bsmdpMap;
class St_bprsMap;
 
class StDbTable;
class StMaker;

class StEmcMappingDb : public TObject 
{
public:
    StEmcMappingDb(int date=20330101, int time=0);
    virtual ~StEmcMappingDb();
    
    static StEmcMappingDb* instance();
    
    /// St_db_Maker-compatible interface
    void SetDateTime(int date, int time);
    void SetFlavor(const char *flavor, const char *tablename=NULL);
    void SetMaxEntryTime(int date, int time);
    
    const bemcMap_st* bemc() const;
    const bemcMap_st& bemc(int softId) const;
    
    const bprsMap_st* bprs() const;
    const bprsMap_st& bprs(int softId) const;
    
    const bsmdeMap_st* bsmde() const;
    const bsmdeMap_st& bsmde(int softId) const;

    const bsmdpMap_st* bsmdp() const;
    const bsmdpMap_st& bsmdp(int softId) const;
    
    int softIdFromMES(StDetectorId det, int m, int e, int s) const;
    
    // these are for towers
    int softIdFromCrate(StDetectorId det, int crate, int channel) const;
    int softIdFromDaqId(StDetectorId det, int daqId) const;
    int softIdFromTDC(StDetectorId det, int TDC, int channel) const;
    
    // this is for SMD/PRS
    int softIdFromRDO(StDetectorId det, int rdo, int channel) const;

private:
    static StEmcMappingDb* mInstance;
    
    // DB tables provided by St_db_Maker -- prefer to use these
    mutable St_bemcMap  *mBemcTTable;
    mutable St_bprsMap  *mBprsTTable;
    mutable St_bsmdeMap *mSmdeTTable;
    mutable St_bsmdpMap *mSmdpTTable;
    
    // version info from StMaker::GetValidity -- used to expire caches
    mutable Int_t mBemcValidity;
    mutable Int_t mBprsValidity;
    mutable Int_t mSmdeValidity;
    mutable Int_t mSmdpValidity;
    
    // DB tables provided by StDbManager in standalone mode
    StDbTable *mBemcTable;
    StDbTable *mBprsTable;
    StDbTable *mSmdeTable;
    StDbTable *mSmdpTable;
    
    // ensure caches expire if beginTime, flavor, or maxEntryTime changes
    mutable bool mBemcDirty;
    mutable bool mBprsDirty;
    mutable bool mSmdeDirty;
    mutable bool mSmdpDirty;
    
    StMaker *mChain;
    
    TDatime mBeginTime;
    void maybe_reload(StDetectorId) const;
    void reload_dbtable(StDbTable*) const;
    
    bool maybe_reset_cache(StDetectorId det) const;
    void reset_bemc_cache() const;
    void reset_bprs_cache() const;
    void reset_smde_cache() const;
    void reset_smdp_cache() const;
    
    mutable short mCacheCrate[30][160];
    mutable short mCacheDaqId[4800];
    mutable short mCacheTDC[30][160];
    mutable short mCacheBprsRdo[4][4800];
    mutable short mCacheSmdRdo[8][4800];
    
    ClassDef(StEmcMappingDb, 2)
};

inline const bemcMap_st& 
StEmcMappingDb::bemc(int softId) const { return bemc()[softId-1]; }

inline const bprsMap_st& 
StEmcMappingDb::bprs(int softId) const { return bprs()[softId-1]; }

inline const bsmdeMap_st& 
StEmcMappingDb::bsmde(int softId) const { return bsmde()[softId-1]; }

inline const bsmdpMap_st& 
StEmcMappingDb::bsmdp(int softId) const { return bsmdp()[softId-1]; }

#endif

/*****************************************************************************
 * $Log: StEmcMappingDb.h,v $
 * Revision 1.4  2009/02/01 17:34:52  kocolosk
 * more caching and optimization.
 *
 * Last StEmcMapping commit was bad and left header, implementation
 * inconsistent.  This commit fixes the AutoBuild.
 *
 * Revision 1.1  2009/01/08 02:16:19  kocolosk
 * move StEmcMappingDb/StEmcDecoder to StEmcUtil/database
 *
 * Revision 2.2  2009/01/02 03:34:34  kocolosk
 * use default date==20330101 like St_db_Maker to suppress spurious error msgs
 *
 * Revision 2.1  2008/12/05 19:05:32  kocolosk
 * new DB-backed implementation of StEmcDecoder
 *
 *****************************************************************************/
