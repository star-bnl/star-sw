// $Id: StEmcADCtoEMaker.h,v 1.39 2004/03/17 21:07:13 fisyak Exp $
// $Log: StEmcADCtoEMaker.h,v $
// Revision 1.39  2004/03/17 21:07:13  fisyak
// icc does like to have the same variable for different parameters
//
// Revision 1.38  2003/10/14 13:36:42  suaide
// small change
//
// Revision 1.37  2003/10/14 13:27:32  suaide
// new methods added in order to select either energy of pedestal cut for the SMD
//
// Revision 1.36  2003/10/10 13:38:29  suaide
// change to allow RMS or Energy CUT. Default is now RMS.
//
// Revision 1.35  2003/10/03 21:13:15  suaide
// some histograms are created only in debug mode to save memory
//
// Revision 1.34  2003/10/03 16:02:57  suaide
// version changed
//
// Revision 1.33  2003/10/03 14:02:23  suaide
// NULL points initialization fixed
//
// Revision 1.32  2003/09/19 14:34:39  suaide
// Removed muDST option from StEmcADCtoEMaker. Will find another solution for that
//
// Revision 1.31  2003/09/18 17:16:20  suaide
// Modifications in the way StEmcADCtoEMaker handles the database requests,
// fixed a bug related to GetDate() that was making the maker extremely slow,
// added the option of reading muDst events directly (need to use StEmcPreDbMaker
// in order to set the correct timestamp for database)
//
// StEmcPreDbMaker just sets the correct timestamp for St_db_Maker when reading
// muDst files without convertion to StEvent. Should run before St_db_Maker
//
// Revision 1.30  2003/09/12 22:03:16  jeromel
// No changes (ident)
//
// Revision 1.29  2003/09/11 20:40:08  suaide
// Removed SMD capacitors 124 and 125 from data for dAu and pp Y2003 runs only.
// It is timestamp flagged so it will work only for this data.
// The point is the fact the the ped subtracted SMD data looks strange for these
// two capacitors values
//
// Revision 1.27  2003/09/08 20:56:52  suaide
// Patch to fix problem with SMD-phi pedestals saved on database
//
// Revision 1.26  2003/04/03 13:18:07  suaide
// option to turn off message log was included
//
// Revision 1.25  2003/02/06 21:28:10  suaide
// bugs fixed
//
// Revision 1.24  2003/01/23 03:24:54  jeromel
// GetCVS() added
//
// Revision 1.23  2003/01/23 03:09:05  jeromel
// Include modif
//
// Revision 1.22  2003/01/17 23:02:25  suaide
// small modification
//
// Revision 1.21  2002/12/02 21:20:14  suaide
// modifications for new DB scheme
//
// Revision 1.20  2002/09/19 21:32:23  suaide
// Modifications to use a new internal data format
//
// Revision 1.19  2002/05/22 22:04:24  suaide
// small bug fixed to reconstruct micro DST's
//
// Revision 1.18  2002/05/15 15:05:28  suaide
// bugs fixed to recalibrate EMC after production
//
// Revision 1.17  2002/02/24 21:19:21  suaide
// clean up and modifications on the settings that allow to save only hits
// above a given threshold that can be defined for each sub detector.
//
// Revision 1.16  2001/12/28 22:38:37  suaide
// fixed documentation
//
// Revision 1.15  2001/12/28 15:03:09  suaide
// fixed documentation
//
// Revision 1.14  2001/12/27 17:45:36  suaide
// removed obsolete files and updated documentation
//
// Revision 1.13  2001/12/26 19:25:34  suaide
// Added documentation and few modifications
//
// Revision 1.12  2001/12/06 17:50:08  suaide
// changes to save ADC without pedestal subtraction
//
// Revision 1.11  2001/12/05 22:31:12  suaide
// Modifications to include SMD
//
// Revision 1.10  2001/12/04 22:05:50  suaide
// new QA histogram for tower
//
// Revision 1.9  2001/10/31 15:01:07  suaide
// modified to discard bad EMC events
//
// Revision 1.8  2001/10/25 21:31:36  suaide
// modifications to get new database tables
//
// Revision 1.7  2001/10/24 14:47:16  suaide
// type correction
//
// Revision 1.6  2001/10/24 14:41:45  suaide
// huge change on StEmcADCtoEMaker to a different software.
// The other version is kept as *.old for future debug
//
//

/*!\class StEmcADCtoEMaker
\author Alexandre A. P. Suaide

This class gets EMC raw ADC's and convert them to calibrated energy.<br><br>

*/

#ifndef STAR_StEmcADCtoEMaker
#define STAR_StEmcADCtoEMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif
#define MAXDETBARREL 4

#include "tables/St_emcCalib_Table.h"
#include "tables/St_smdCalib_Table.h"
#include "tables/St_emcPed_Table.h"
#include "tables/St_smdPed_Table.h"
#include "tables/St_emcStatus_Table.h"
#include "tables/St_smdStatus_Table.h"
#include "tables/St_emcGain_Table.h"
#include "tables/St_smdGain_Table.h"

#include "tables/St_controlADCtoE_Table.h"

#include "StEmcUtil/others/emcInternalDef.h"
#include "TH1.h"
#include "TH2.h"
#include "TString.h"

class StEmcCollection;
class StEmcDecoder;
class StEmcGeom;
class StBemcData;

class StEmcADCtoEMaker : public StMaker 
{
 private: 
  TH2F              *mNhit;           //! 
  TH2F              *mEtot;           //!
  TH2F              *mHits[MAXDETBARREL];   //!
  TH2F              *mAdc[MAXDETBARREL];    //!
  TH2F              *mEnergyHist[MAXDETBARREL]; //!
  TH2F              *mEnergySpec[MAXDETBARREL][3]; //!
  TH1F              *mAdc1d[MAXDETBARREL];  //!           
  TH1F              *mEn1d[MAXDETBARREL];  //!           
  TH2F              *mADCSpec[MAXDETBARREL];          //!           
  TH2F              *mSmdTimeBinHist; //!
  TH2F              *mValidEvents;    //!
           
  controlADCtoE_st  *mControlADCtoE; 
  
  Bool_t            mHasPed[MAXDETBARREL];
  Bool_t            mHasCalib[MAXDETBARREL];
  Bool_t            mHasGain[MAXDETBARREL];
  Float_t           mPed[MAXDETBARREL][18001][3];
  Float_t           mPedRMS[MAXDETBARREL][18001][3];
  Float_t           mGain[MAXDETBARREL][18001];
  Float_t           mCalib[MAXDETBARREL][18001][5];
					 
  TDataSet          *mDb;
  StEmcCollection   *mEmc;            
  StEmcDecoder      *mDecoder;          
  StBemcData        *mData;                      
  StEmcGeom         *mGeo[MAXDETBARREL]; 
  Int_t             mDBRunNumber;
  Int_t             mRunNumber;
           
  Bool_t            mEmbedd;
  Bool_t            mFromDaq;
  Bool_t            mPrint;
  Bool_t            mSave[MAXDETBARREL]; 
  Bool_t            mFillHisto;
  Bool_t            mDebug;
  Bool_t            mSMDPidMinus1Bug;
  
  TString           mMuName;
					 
					 
  void              zeroAll(); ///< Zero all temporary vectors
  Bool_t            getEmc(); ///< This method gets EMC hits from different sources.
  Bool_t            getTables(); ///< This method gets EMC tables from DB
  Bool_t            getStatus(Int_t); ///< This method gets the status tables 
  Bool_t            calibrate(Int_t); ///< This method applies the calibration constants to get the hit energy.
  Bool_t            fillHistograms(); ///< This method fills QA histograms
  Bool_t            fillStEvent(); ///< This method makes a clean up of StEvent
  Bool_t            getEmcFromDaq(TDataSet* daq);///< This method gets EMC collection from DAQ dataset.
  Bool_t            clearOldEmc(); ///< Clear old emc collection
  Bool_t            getEmcFromStEvent(StEmcCollection*); ///< This method creates a temporary ADC vector for each detector.
  Bool_t            saveHit(Int_t,Int_t,Int_t cap=0);///< Decide if a hit should be saved or not
 protected:    
    
 public: 
                 
                            StEmcADCtoEMaker(const char *name="Eread"); ///< StEmcADCtoEMaker constructor
  virtual                   ~StEmcADCtoEMaker(); ///< StEmcADCtoEMaker destructor
  virtual Int_t             Init(); ///< Init function. This method initializes the histograms
  virtual Int_t             Make(); ///< Process each event
  virtual Int_t             Finish(); ///< This method creates mean ADC and RMS histograms.
   
  controlADCtoE_st*         getControlTable()  {return mControlADCtoE;} ///< Return Control table (NULL)
  StEmcCollection*          getEmcCollection() {return mEmc;} ///< Return emcCollection
  StBemcData*               getBemcData()      {return mData;} ///< Return BemcData pointer
  void                      clearStEventStaf() {mEmc = NULL;} ///< Clear emcCollection (does not delete from memory)
  
  void                      setEmbeddingMode(Bool_t a) {mEmbedd = a; } ///< Set embedding mode (default is kFALSE)
  void                      setPrint(Bool_t a) {mPrint = a; } ///< Set it to kFALSE if you do not want to print messages
  void                      setFillHisto(Bool_t a) {mFillHisto = a;} ///< Turns on/off histogram filling
  void                      setSMDEnergyCut(Float_t a = 0.07,Float_t b = 0.07); ///< Turns on SMD hit cut based on energy and set the thresholds for eta and phi planes
  void                      setSMDRmsCut(Float_t a = 1.5,Float_t b = 1.5); ///< Turns on SMD hit cut based on pedestal RMS and set the thresholds for eta and phi planes
  void                      setSMDPhiIdMinus1Bug(Bool_t a = kFALSE) { mSMDPidMinus1Bug = a;} ///< Turns on the correction for the SMD-phi id-1 Pedestal bug if using old Pedestal tables for the y2003 d+Au and p+p runs
  
  virtual const char *      GetCVS() const {static const char cvs[]="Tag $Name:  $ $Id: StEmcADCtoEMaker.h,v 1.39 2004/03/17 21:07:13 fisyak Exp $ built "__DATE__" "__TIME__ ; return cvs;}

  ClassDef(StEmcADCtoEMaker, 2)  
};

#endif
