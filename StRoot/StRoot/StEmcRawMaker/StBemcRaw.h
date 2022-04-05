// $Id: StBemcRaw.h,v 1.17 2010/12/22 22:58:57 stevens4 Exp $
// $Log: StBemcRaw.h,v $
// Revision 1.17  2010/12/22 22:58:57  stevens4
// Patch for BSMDE mapping problem in P10ih and P10ij productions (RT #2043)
//
// Revision 1.16  2009/02/26 12:00:40  mattheww
// added token check to BTOW header check
//
// Revision 1.15  2009/02/04 21:05:42  kocolosk
// Refactor StEEmcDb(Maker), new location for StEmcDecoder. Fixes RT #1388.
//
// Revision 1.14  2009/01/28 15:42:44  mattheww
// Put back some obsolete methods to satisfy StBemcData
//
// Revision 1.13  2009/01/27 19:58:36  mattheww
// Updates to StEmcRawMaker to be compatible with 2009 DAQ Format
//
// Revision 1.12  2008/10/24 18:19:07  mattheww
// Added option to throw out all hits in an event if any crates are corrupted
//
// Revision 1.11  2008/07/03 20:58:49  mattheww
// Added checking of every status table for each hit. Status table checks can be toggled using an option added to setCheckStatus. Also fixed a small bug.
//
// Revision 1.10  2007/09/10 22:21:41  kocolosk
// Support for new BPRS swap fixes (off by default for 06/07 production, on for analysis).
// StBemcTables now matches map fixes in case end users want to use this copy.
//
// Revision 1.9  2006/01/16 11:12:00  suaide
// tower map bug fixed and astyle run
//
// Revision 1.8  2005/01/07 20:33:18  suaide
// created a new method to correct for the PSD map problem
//
// Revision 1.7  2004/12/21 12:53:49  suaide
// moved StBemcTables to StEmcUtil
// corrected for y2005 PSD data banks
//
// Revision 1.6  2004/12/14 11:32:11  suaide
// added histograms for status tables creation
//
// Revision 1.5  2004/11/22 12:46:22  suaide
// added new flags for hit reconstruction. Status are not checked
// dureing production anymore in order to avoid bad status loaded in
// DB
//
// Revision 1.4  2004/10/21 00:01:50  suaide
// small changes in histogramming and messages for BEMC
// Complete version for EEMC done by Jan Balewski
//
// Revision 1.3  2004/10/19 17:53:00  suaide
// code clean up
//
// Revision 1.2  2004/10/19 13:10:38  suaide
// small fix
//
// Revision 1.1  2004/10/18 18:20:07  suaide
// New Maker. Will replace StEmcADCtoEMaker in production.
// It reads only DAQ structures. Output is StEvent.
//
//
/*!\class StBemcRaw
\author Alexandre A. P. Suaide
 
This class does the hit selection and calibration for the
Barrel EMC. It uses only DAQ structures as input. The
output is a StEmcCollection filled with StEmcRawHits.
bemcRawData is also filled in the StEmcCollection.
*/

#ifndef STAR_StBemcRaw
#define STAR_StBemcRaw

#include "TObject.h"
#include "TDataSet.h"
#include "StEmcUtil/database/StBemcTables.h"

#include "tables/St_controlADCtoE_Table.h"
#include "defines.h"
#include "TH2.h"
#include "StChain/StRTSBaseMaker.h"
class StEvent;
class StEmcRawData;
class StEmcCollection;
class StEmcRawMaker;
class StEmcDecoder;

class StBemcRaw : public TObject
{
protected:
    enum {kZero, kCrate, kStatus, kRms, kPed, kEn, kCalib, kOK};

    // BARREL HISTOGRAMS
    TH2F*                    mBarrelNHitHist;
    TH2F*                    mBarrelEtotHist;
    TH2F*                    mBarrelAdcSumHist;
    TH2F*                    mBarrelNCratesHist;
    TH2F*                    mBarrelCrateStatusHist;
    TH2F*                    mBarrelQAHisto[MAXDETBARREL];

    StEmcDecoder*            mDecoder;
    StBemcTables*            mTables;
    controlADCtoE_st*        mControlADCtoE;

    Bool_t                   mSaveAllStEvent;
    Bool_t                   mPsdMapBug;
    Bool_t                   mPsdMapBug2;
    Bool_t                   mTowerMapBug;
    Bool_t                   mSmdMapBug;
    string                   mProdVer;

    Int_t                    mDate;
    Int_t                    mTime;
    Int_t                    mNZ[MAXDETBARREL];
    Int_t                    mNCRATE[MAXDETBARREL];
    Int_t                    mNSTATUS[MAXDETBARREL];
    Int_t                    mNRMS[MAXDETBARREL];
    Int_t                    mNPED[MAXDETBARREL];
    Int_t                    mNOK[MAXDETBARREL];
    Int_t                    mNTOTAL[MAXDETBARREL];
    Int_t                    mADCSUM[MAXDETBARREL];
    Int_t                    mNCRATESOK[MAXDETBARREL];
    Float_t                  mTOTALE[MAXDETBARREL];
    Int_t                    mCrateStatus[MAXDETBARREL][MAXCRATES];
    Bool_t                   mIsCorrupted[MAXDETBARREL];
    Int_t                    mCheckStatus[MAXDETBARREL][4];
    Int_t                    mCrateVeto;
    Bool_t                   mAnyCorrupted;
public:

    StBemcRaw(); ///< StBemcRaw constructor
    virtual                   ~StBemcRaw(); ///< StBemcRaw destructor

    void                      initHisto();
    void                      initQAHisto();
    void                      fillHisto();

    Bool_t make(StEmcRawMaker * TheData, StEvent* event);
    Bool_t convertFromDaq(StEmcRawMaker * DAQ, StEmcRawData* RAW);
    //make(TDataSet*,StEvent*)made obsolete in 2009 with new DAQ format
    Bool_t                    make(TDataSet*,StEvent*); ///< Make the BEMC detector from DAQ
    Bool_t                    make(StEmcRawData*,StEvent*); ///< Make the BEMC detector from StEmcRaw
    //convertFromDaq(TDataSet*,StEmcRawData*)made obsolete in 2009 with new DAQ format
    Bool_t                    convertFromDaq(TDataSet*, StEmcRawData*); ///< Convert DAQ format into StEmcRawData format
    Int_t                     getBemcADCRaw(Int_t, Int_t, StEmcRawData*, Int_t&, Int_t&); ///< get ADC from StEmcRawData structure
    void                      checkHeaders(StEmcRawData*,StEvent*);///<Check all BEMC detector headers
    void                      checkBtowCrates(StEmcRawData*,StEvent*); ///< check tower crates
    void                      emptyEmcCollection(StEmcCollection*); ///< empty current emcCollection
    Int_t                     makeHit(StEmcCollection*, Int_t, Int_t, Int_t, Int_t, Int_t, Float_t&); ///< make StEmcRawHit
    void                      createDecoder(Int_t,Int_t); ///< Create new StEmcDecoder
    Bool_t                    isCorrupted(Int_t det)
    {
        return mIsCorrupted[det-1];
    } // return kTRUE if the event is corrupted for that detector

    ///setCheckStatus toggles checking status for adding hits to StEmcCollection
    ///accepts options="status","pedestal","calib","gain" to toggle individual tables
    ///default is all tables are checked
    void                      setCheckStatus(Int_t det, Int_t flag, const char* option = "");
    void                      setCrateVeto(Int_t flag);
    void                      clearStats(Int_t); ///< Clear statistics for detector 'det'
    void                      updateStats(Int_t,Int_t,Int_t, Float_t); ///< Update statistics for detector 'det'
    void                      printStats(Int_t); ///< Print statistics for detector 'det'
    void                      printConf(); ///< Print configuration

    void                      setDate(Int_t d)
    {
        mDate = d;
    } ///<Set event date.
    void                      setTime(Int_t t)
    {
        mTime = t;
    } ///<Set event time.
    void                      setProdVer(string prodVer)
    {
        mProdVer = prodVer;
    } ///<Set event date.
    void                      saveAllStEvent(Bool_t a)
    {
        mSaveAllStEvent = a;
    } ///< Set to kTRUE if all hits are to be saved on StEvent
    void                      psdMapBug(Bool_t a)
    {
        mPsdMapBug = a;
    } ///< Set to ktrue to correct PSD map inthe P04* productions
    void                      psdMapBug2(Bool_t a)
    { 
        mPsdMapBug2 = a;
        delete mTables;
        mTables = new StBemcTables(mTowerMapBug, mPsdMapBug2);
    } ///< Set to ktrue to correct PSD swaps in 2006 and 2007 data
    void                      towerMapBug(Bool_t a)
    {
        mTowerMapBug = a;
        delete mTables;
        mTables = new StBemcTables(mTowerMapBug, mPsdMapBug2);
    } ///< Set to ktrue to correct for the tower map bug (only runs before 2006)
    void                      smdMapBug(Bool_t a)
    { 
        mSmdMapBug = a;
    } ///< Set to ktrue to correct SMD swaps

    StBemcTables*             getTables()
    {
        return mTables;
    } ///< Return the StBemcTable pointer
    StEmcDecoder*             getDecoder()
    {
        return mDecoder;
    }///< Return the StEmcDecoder pointer
    controlADCtoE_st*         getControlTable()
    {
        return mControlADCtoE;
    } ///< Return Control table (NULL)

    Int_t                     getCrateStatus(Int_t det,Int_t c)
    {
        return mCrateStatus[det-1][c-1];
    } ///< Return the status of the crate C
    Int_t                     getTotalHits(Int_t det)
    {
        return mNTOTAL[det-1];
    }///< Return the Total number of hits for the detector 'det'
    Int_t                     getTotalSaved(Int_t det)
    {
        return mNOK[det-1];
    }///< Return the Total number of SAVED hits for the detector 'det'
    Int_t                     getTotalADC(Int_t det)
    {
        return mADCSUM[det-1];
    }///< Return the Total ADC sum for the detector 'det'
    Float_t                   getTotalE(Int_t det)
    {
        return mTOTALE[det-1];
    }///< Return the Total Energy Sum for the detector 'det'
    Int_t                     getRejectPed(Int_t det)
    {
        return mNRMS[det-1]+mNPED[det-1];
    }///< Return the Total number of hits rejected by pedestal issues for the detector 'det'
    Int_t                     getRejectZero(Int_t det)
    {
        return mNZ[det-1];
    }///< Return the Total number of hits rejected because of zero ADC for the detector 'det'
    Int_t                     getRejectStatus(Int_t det)
    {
        return mNSTATUS[det-1];
    }///< Return the Total number of hits rejected because of STATUS issues for the detector 'det'
    Int_t                     getRejectCrate(Int_t det)
    {
        return mNCRATE[det-1];
    }///< Return the Total number of hits rejected because of BAD crates for the detector 'det' (only tower)
    Int_t                     getNCratesOK(Int_t det)
    {
        return mNCRATESOK[det-1];
    }///< Return the Total number of good crates for the detector 'det' (only tower)
    ClassDef(StBemcRaw, 1)
};

#endif
