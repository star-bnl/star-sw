#ifndef STAR_StRTSBaseMaker_H
#define STAR_StRTSBaseMaker_H

/***************************************************************************
 *
 * $Id: StRTSBaseMaker.h,v 1.1 2008/01/29 15:14:05 fine Exp $
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
      Int_t GetNextDaqElement(const char *elementPath);
      StRtsTable *DaqDta() {return fDaq_Dta;}

   public:

     StRTSBaseMaker(const char *name);
    ~StRTSBaseMaker() ;
    
     Int_t Sector () const;
     Int_t Pad () const;
     Int_t Rdo () const;
     Int_t Row () const;

     Int_t Make() = 0;

  // cvs
  virtual const char *GetCVS() const
    {
      static const char cvs[]="Tag $Name:  $Id: built "__DATE__" "__TIME__ ; return cvs;
    }
  
  ClassDef(StRTSBaseMaker, 1)    //StRTSBaseMaker - class to fille the StEvewnt from DAQ reader
};


#endif
