// $Id: StEmcRawMaker.h,v 1.2 2004/10/19 17:53:00 suaide Exp $
// $Log: StEmcRawMaker.h,v $
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
  2. Fills bemcRawData with the daq data
  3. Check for corruption but does not remove the event.
  4. Fills StEmcRawHits depending on the settings defined
     by the user. 
  5. Fills some QA histograms
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
   
   Bool_t                   mPrint;
                      
 public:                  
                            StEmcRawMaker(const char *name="EmcRaw"); ///< StEmcRawMaker constructor
  virtual                   ~StEmcRawMaker(); ///< StEmcRawMaker destructor
  virtual Int_t             Init(); ///< Init function. This method initializes the histograms
  virtual Int_t             InitRun(Int_t); ///< InitRun function
  virtual Int_t             Make(); ///< Process each event
  virtual Int_t             Finish(); ///< Finish function. 
  virtual void              fillHistograms();///<Fill QA histograms
  
  virtual Bool_t            prepareEnvironment();///< Prepare the StEvent environment to fill the EMC data
  virtual Bool_t            makeBemc(); ///< Make the Barrel-EMC detector
  
  StBemcRaw*                getBemcRaw()       { return mBemcRaw;} ///< Return the StBemcRaw pointer
  void                      setPrint(Bool_t a) { mPrint = a; mBemcRaw->setPrint(a);} ///< Set it to kFALSE if you do not want to print messages
    
  virtual const char *      GetCVS() const {static const char cvs[]="Tag $Name:  $ $Id: StEmcRawMaker.h,v 1.2 2004/10/19 17:53:00 suaide Exp $ built "__DATE__" "__TIME__ ; return cvs;}

  ClassDef(StEmcRawMaker, 1)  
};

#endif
