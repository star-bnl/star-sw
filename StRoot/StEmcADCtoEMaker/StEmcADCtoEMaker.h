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

class StEmcCollection;
class StEmcDecoder;
class StEmcGeom;
class StEmcRawData;
class StEvent;

class StEmcADCtoEMaker : public StMaker 
{
 protected: 
   
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

 public:                  
                            StEmcADCtoEMaker(const char *name="Eread"); ///< StEmcADCtoEMaker constructor
  virtual                   ~StEmcADCtoEMaker(); ///< StEmcADCtoEMaker destructor
  virtual Int_t             Init(); ///< Init function. This method initializes the histograms
  virtual Int_t             InitRun(Int_t); ///< InitRun function
  virtual Int_t             Make(); ///< Process each event
  virtual Int_t             Finish(); ///< Finish function. 
  
  StBemcData*               getBemcData()       { return mBemcData;} ///< Return the StBemcData pointer
  controlADCtoE_st*         getControlTable()   { return mBemcData->getControlTable();} ///< Return Control table (NULL)
  StEmcCollection*          getEmcCollection();  ///< Return emcCollection
  void                      clearStEventStaf()  { /*NOP*/}
  Bool_t                    isCorrupted(); ///< Returns if BTOW is corrupted or not
  
  void                      setPrint(Bool_t);  ///< Set it to kFALSE if youdo not want to print messages
  void                      setEmbeddingMode(Bool_t a) {mEmbed = a; } ///< Set embedding mode (default is kFALSE)
  void                      saveAllStEvent(Bool_t a)   { mBemcData->saveAllStEvent(a);} ///< Set to kTRUE if all hits are to be saved on StEvent
  
  void                      printMap(Int_t,char*); ///< print map for an EMC detector
  
  virtual const char *      GetCVS() const {static const char cvs[]="Tag $Name:  $ $Id: StEmcADCtoEMaker.h,v 1.48 2005/07/05 19:05:16 suaide Exp $ built "__DATE__" "__TIME__ ; return cvs;}

  ClassDef(StEmcADCtoEMaker, 3)  
};

#endif
