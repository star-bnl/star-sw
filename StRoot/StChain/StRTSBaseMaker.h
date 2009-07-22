#ifndef STAR_StRTSBaseMaker_H
#define STAR_StRTSBaseMaker_H

/***************************************************************************
 *
 * $Id: StRTSBaseMaker.h,v 1.6 2009/07/22 21:42:52 fine Exp $
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

     UInt_t Token()      const;    // current token
     UInt_t Trgcmd()     const;    // current trigger command
     UInt_t Daqcmd()     const;    // current DAQ command
     UInt_t Trgword()    const;    // the Trigger Word
     UInt_t Phyword()    const;    // the Physics Word
     UInt_t Daqbits()    const;    // "offline" bits aka L3 summary...
     UInt_t Daqbits_l1() const;    // triggers satisfying l1 
     UInt_t Daqbits_l2() const;    // triggers satisfying l2
     UInt_t Evpgroups()  const;    // evp groups aka L3 summary[2]     
  
     UInt_t Detectors()  const;	  // detectors present bit mask according to DAQ!
     
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
