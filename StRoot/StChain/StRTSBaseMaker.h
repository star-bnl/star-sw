#ifndef STAR_StRTSBaseMaker_H
#define STAR_StRTSBaseMaker_H

/***************************************************************************
 *
 * $Id: StRTSBaseMaker.h,v 1.7 2009/10/09 20:30:08 fine Exp $
 * StRTSBaseMaker - class to fille the StEvewnt from DAQ reader
 *--------------------------------------------------------------------------
 *
 ***************************************************************************/

#include "StMaker.h"

class StRtsTable;

class StRTSBaseMaker : public StMaker
{
   private:
     static const char *fRTSRootDataset; // The name of the Root dataset
     StRtsTable   *fDaq_Dta;
     TString        fDetectorName;     // name of the detector

   protected:
      StRtsTable *GetNextDaqElement(const char *elementPath);
      StRtsTable *DaqDta() {return fDaq_Dta;}
      StRtsTable *GetNext(const char* bank);

      virtual StRtsTable *GetNextRaw();
      virtual StRtsTable *GetNextAdc();
      virtual StRtsTable *GetNextLegacy();

   public:

     StRTSBaseMaker(const char *detectorName,const char *makerName="");
     virtual ~StRTSBaseMaker() ;
 
     Int_t Sector () const;
     Int_t Pad () const;
     Int_t Rdo () const;
     Int_t Row () const;

     // DAQ Event raw data

     static UInt_t Token();      //< current token
     static UInt_t Trgcmd();     //< current trigger command
     static UInt_t Daqcmd();     //< current DAQ command
     static UInt_t Trgword();    //< the Trigger Word
     static UInt_t Phyword();    //< the Physics Word
     static UInt_t Daqbits();    //< "offline" bits aka L3 summary...
     static UInt_t Daqbits_l1(); //< triggers satisfying l1 
     static UInt_t Daqbits_l2(); //< triggers satisfying l2
     static UInt_t Evpgroups() ; //< evp groups aka L3 summary[2]     
  
     UInt_t Detectors()  const;  //< detectors present bit mask according to DAQ!
     
     const TString &DetectorName() const { return fDetectorName; }

     virtual Int_t Make() = 0;

  // cvs
  virtual const char *GetCVS() const
    {
      static const char cvs[]="Tag $Name:  $Id: built "__DATE__" "__TIME__ ; return cvs;
    }
  
  ClassDef(StRTSBaseMaker, 1)    //StRTSBaseMaker - class to fille the StEvewnt from DAQ reader
};


#endif
