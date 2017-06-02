/*!\class StEmcADCtoEMaker
\author Alexandre A. P. Suaide
 
This class copies the BEMC raw data from DAQ file into StEvent.
This make should run only in production or when reading DAQ
files. The tasks performed by this maker are:
 
  1. Get EMC data from DAQ/MuDst/StEvent files
  2. does calibration and fill StEmcCollection
  3. Fills some QA histograms
*/

#ifndef STAR_StEmcADCtoEMaker
#define STAR_StEmcADCtoEMaker

#include "StMaker.h"
#include "TH1.h"
#include "TH2.h"
#include "tables/St_controlADCtoE_Table.h"
#include "StBemcData.h"
#include "StEmcRawMaker/defines.h"
#include "StEvent/StEnumerations.h"


class StEmcCollection;
class StEmcDecoder;
class StEmcGeom;
class StEmcRawData;
class StEvent;

class StEmcADCtoEMaker : public StMaker
{
public:
    StEmcADCtoEMaker(const char *name="Eread"); ///< StEmcADCtoEMaker constructor
    virtual                   ~StEmcADCtoEMaker(); ///< StEmcADCtoEMaker destructor
    virtual Int_t             Init(); ///< Init function. This method initializes the histograms
    virtual Int_t             InitRun(Int_t); ///< InitRun function
    virtual Int_t             Make(); ///< Process each event
    virtual Int_t             Finish(); ///< Finish function.

    /// require raw hits to have status == 1 in order to be saved in the StEmcCollection.
    /// Default is true.    
    ///Now added checking of all status tables and options to turn checking off
    ///setCheckStatus accepts options="status","pedestal","calib","gain"
    ///Default is to check all tables
    void setCheckStatus(StDetectorId det, int flag, const char* option="");
    
    /// only save hits above pedestal.  Default is false for BTOW, true for others.
    /// set this flag to 2 if you want to save hits from bad capacitors in BSMD, BPRS
    void setDoZeroSuppression(StDetectorId det, int flag) { getControlTable()->DeductPedestal[det-kBarrelEmcTowerId] = flag; }
    
    /// suppress hits if (adc-ped) < nRMS*pedRMS.  Default is 1.5 for BSMD, BPRS.
    void setPedestalCut(StDetectorId det, float nRMS) { getControlTable()->CutOff[det-kBarrelEmcTowerId] = nRMS; }
    
    /// suppress hits if crate fails corruption check.  Default is true.
    void setCheckCrateHeaderCorruption(StDetectorId det, int flag) { getControlTable()->CheckCrate[det-kBarrelEmcTowerId] = flag; }
    
    inline StBemcData*               getBemcData()
    {
        return mBemcData;
    } ///< Return the StBemcData pointer
    inline controlADCtoE_st*         getControlTable()
    {
        return mBemcData->getControlTable();
    } ///< Return Control table (NULL)
    StEmcCollection*          getEmcCollection();  ///< Return emcCollection
    Bool_t                    isCorrupted(); ///< Returns if BTOW is corrupted or not

	void                      setPrint(Bool_t);  ///< Obsolete function; users can control messages with logger config file.
    inline void                      setEmbeddingMode(Bool_t a)
    {
        mEmbed = a;
    } ///< Set embedding mode (default is kFALSE)
    inline void                      saveAllStEvent(Bool_t a)
    {
        mBemcData->saveAllStEvent(a);
    } ///< Set to kTRUE if all hits are to be saved on StEvent

    void                      printMap(Int_t,char*); ///< print map for an EMC detector

    virtual const char *      GetCVS() const
    {
        static const char cvs[]="Tag $Name:  $ $Id: StEmcADCtoEMaker.h,v 1.54 2017/06/02 16:41:57 jlzhang Exp $ built " __DATE__ " " __TIME__ ;
        return cvs;
    }

private:

    StEvent*                 mEvent;
    StBemcData*              mBemcData;

    Bool_t                   mMyStEvent;
    Bool_t                   mEmbed;
    Bool_t                   mIsCorrupted;
    Bool_t                   mTestedCorruption;

    virtual Bool_t            prepareEnvironment();///< Prepare the StEvent environment to fill the EMC data
    virtual Bool_t            makeBemc(); ///< Make the Barrel-EMC detector
    virtual void              fillHistograms();///<Fill QA histograms*
    virtual void              testCorruption();///<Test BTOW for corruption


    ClassDef(StEmcADCtoEMaker, 3)
};

#endif
