// $Id: StBemcRaw.h,v 1.3 2004/10/19 17:53:00 suaide Exp $
// $Log: StBemcRaw.h,v $
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
#include "StBemcTables.h"
#include "StDaqLib/EMC/StEmcDecoder.h"

#include "tables/St_controlADCtoE_Table.h"
#include "defines.h"

class StEvent;
class StEmcRawData;
class StEmcCollection;

class StBemcRaw : public TObject 
{
 protected: 
   enum {kZero, kCrate, kStatus, kRms, kPed, kEn, kCalib, kOK};
   
   StEmcDecoder*            mDecoder;
   StBemcTables*            mTables;
   controlADCtoE_st*        mControlADCtoE;
   
   Bool_t                   mPrint;
   Bool_t                   mSaveAllStEvent;
   
   Int_t                    mDate;
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
          
 public: 
                 
                            StBemcRaw(); ///< StBemcRaw constructor
  virtual                   ~StBemcRaw(); ///< StBemcRaw destructor
  
  Bool_t                    make(TDataSet*,StEvent*); ///< Make the BEMC detector from DAQ
  Bool_t                    make(StEmcRawData*,StEvent*); ///< Make the BEMC detector from StEmcRaw
  Bool_t                    convertFromDaq(TDataSet*, StEmcRawData*); ///< Convert DAQ format into StEmcRawData format
  Int_t                     getBemcADCRaw(Int_t, Int_t, StEmcRawData*, Int_t&, Int_t&); ///< get ADC from StEmcRawData structure
  void                      checkHeaders(StEmcRawData*);///<Check all BEMC detector headers
  void                      checkBtowCrates(StEmcRawData*); ///< check tower crates
  void                      emptyEmcCollection(StEmcCollection*); ///< empty current emcCollection
  Int_t                     makeHit(StEmcCollection*, Int_t, Int_t, Int_t, Int_t, Int_t, Float_t&); ///< make StEmcRawHit
  void                      createDecoder(Int_t,Int_t); ///< Create new StEmcDecoder

  void                      clearStats(Int_t); ///< Clear statistics for detector 'det'
  void                      updateStats(Int_t,Int_t,Int_t, Float_t); ///< Update statistics for detector 'det'
  void                      printStats(Int_t); ///< Print statistics for detector 'det'
  
  void                      setPrint(Bool_t a)         { mPrint = a; } ///< Set it to kFALSE if you do not want to print messages
  void                      setDate(Int_t d)           { mDate = d;} ///<Set event date.
  void                      saveAllStEvent(Bool_t a)   { mSaveAllStEvent = a;} ///< Set to kTRUE if all hits are to be saved on StEvent
  
  StBemcTables*             getTables()                { return mTables;} ///< Return the StBemcTable pointer
  StEmcDecoder*             getDecoder()               { return mDecoder;}///< Return the StEmcDecoder pointer
  controlADCtoE_st*         getControlTable()          { return mControlADCtoE;} ///< Return Control table (NULL)

  Int_t                     getCrateStatus(Int_t det,Int_t c)    { return mCrateStatus[det-1][c-1];} ///< Return the status of the crate C
  Int_t                     getTotalHits(Int_t det)    { return mNTOTAL[det-1];}///< Return the Total number of hits for the detector 'det'
  Int_t                     getTotalSaved(Int_t det)   { return mNOK[det-1];}///< Return the Total number of SAVED hits for the detector 'det'
  Int_t                     getTotalADC(Int_t det)     { return mADCSUM[det-1];}///< Return the Total ADC sum for the detector 'det'
  Float_t                   getTotalE(Int_t det)       { return mTOTALE[det-1];}///< Return the Total Energy Sum for the detector 'det'
  Int_t                     getRejectPed(Int_t det)    { return mNRMS[det-1]+mNPED[det-1];}///< Return the Total number of hits rejected by pedestal issues for the detector 'det'
  Int_t                     getRejectZero(Int_t det)   { return mNZ[det-1];}///< Return the Total number of hits rejected because of zero ADC for the detector 'det'
  Int_t                     getRejectStatus(Int_t det) { return mNSTATUS[det-1];}///< Return the Total number of hits rejected because of STATUS issues for the detector 'det'
  Int_t                     getRejectCrate(Int_t det)  { return mNCRATE[det-1];}///< Return the Total number of hits rejected because of BAD crates for the detector 'det' (only tower)
  Int_t                     getNCratesOK(Int_t det)    { return mNCRATESOK[det-1];}///< Return the Total number of good crates for the detector 'det' (only tower)
  ClassDef(StBemcRaw, 1)  
};

#endif
