// $Id: StEmcRawMaker.h,v 1.3 2004/10/19 23:48:49 suaide Exp $
// $Log: StEmcRawMaker.h,v $
// Revision 1.3  2004/10/19 23:48:49  suaide
// Initial implementation of the endcap detector done by Jan Balewski
//
// Revision 1.2  2004/10/19 17:53:00  suaide
// code clean up
//
// Revision 1.1  2004/10/18 18:20:07  suaide
// New Maker. Will replace StEmcADCtoEMaker in production.
// It reads only DAQ structures. Output is StEvent.
//
//
/*!\class StEmcRawMaker
\author Alexandre A. P. Suaide

This class copies the BEMC raw data from DAQ file into StEvent.
This make should run only in production or when reading DAQ
files. The tasks performed by this maker are:

  1. Get EMC data from DAQ files
  2. Fills B+E emcRawData with the daq data
  3.a Check for BEMC corruption but does not remove BEMC sub-event.
  3.b Check for EEMC corruption and  does remove EEMC sub-event.
  4. Fills StEmcRawHits depending on the BEMC settings defined
     by the user. For EEMC works only daqReader->StEvent.
  5. Fills some QA histograms for BEMC & EEMC
*/

#ifndef STAR_StEmcRawMaker
#define STAR_StEmcRawMaker

#include "StMaker.h"
#include "TH1.h"
#include "TH2.h"
#include "tables/St_controlADCtoE_Table.h"
#include "StBemcRaw.h"
#include "defines.h"

class StEmcCollection;
class StEmcDecoder;
class StEmcGeom;
class StEmcRawData;
class StEvent;

class StEEmcDbMaker;
class StEemcRaw;

class StEmcRawMaker : public StMaker 
{
 protected: 
   // BARREL HISTOGRAMS
   TH2F*                    mBarrelNHitHist;            
   TH2F*                    mBarrelEtotHist;           
   TH2F*                    mBarrelAdcSumHist;           
   TH2F*                    mBarrelNCratesHist;           
   TH2F*                    mBarrelCrateStatusHist;            
   
   StEvent*                 mEvent; 
   StBemcRaw*               mBemcRaw;
   StEemcRaw*               mEemcRaw;
   
   Bool_t                   mPrint;
   void              fillHistograms();///<Fill QA histograms
  
   Bool_t            prepareEnvironment();///< Prepare the StEvent environment to fill the EMC data
   Bool_t            makeBemc(); ///< Make the Barrel-EMC detector
   Bool_t            makeEemc(); ///< Make the Endcap-EMC detector
   StEEmcDbMaker * eeStDb; ///< to assess EEMC DB
                      
 public:                  
                            StEmcRawMaker(const char *name="EmcRaw"); ///< StEmcRawMaker constructor
  virtual                   ~StEmcRawMaker(); ///< StEmcRawMaker destructor
  virtual Int_t             Init(); ///< Init function. This method initializes the histograms
  virtual Int_t             InitRun(Int_t runumber); ///< InitRun function
  virtual Int_t             Make(); ///< Process each event
  virtual Int_t             Finish(); ///< Finish function. 

  StBemcRaw*                getBemcRaw() { return mBemcRaw;} ///< Return the StBemcRaw pointer
  void                      setPrint(Bool_t a) { mPrint = a; mBemcRaw->setPrint(a);} ///< Set it to kFALSE if you do not want to print messages
    
  virtual const char *      GetCVS() const {static const char cvs[]="Tag $Name:  $ $Id: StEmcRawMaker.h,v 1.3 2004/10/19 23:48:49 suaide Exp $ built "__DATE__" "__TIME__ ; return cvs;}

  ClassDef(StEmcRawMaker, 1)  
};

#endif
