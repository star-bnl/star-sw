#ifndef STAR_StRTSBaseMaker_H
#define STAR_StRTSBaseMaker_H

/***************************************************************************
 *
 * $Id: StRTSBaseMaker.h,v 1.3 2008/12/02 22:55:15 fine Exp $
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

   protected:
      StRtsTable *GetNextDaqElement(const char *elementPath);
      StRtsTable *DaqDta() {return fDaq_Dta;}
      virtual StRtsTable *GetNext(const char* bank);
      virtual StRtsTable *GetNextRaw();
      virtual StRtsTable *GetNextAdc();
      virtual StRtsTable *GetNextLegacy();

   public:

     StRTSBaseMaker(const char *name);
     virtual ~StRTSBaseMaker() ;
 
     Int_t Sector () const;
     Int_t Pad () const;
     Int_t Rdo () const;
     Int_t Row () const;

     virtual Int_t Make() = 0;

  // cvs
  virtual const char *GetCVS() const
    {
      static const char cvs[]="Tag $Name:  $Id: built "__DATE__" "__TIME__ ; return cvs;
    }
  
  ClassDef(StRTSBaseMaker, 1)    //StRTSBaseMaker - class to fille the StEvewnt from DAQ reader
};


#endif
