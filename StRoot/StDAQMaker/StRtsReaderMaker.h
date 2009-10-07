#ifndef STAR_StRtsReaderMaker_H
#define STAR_StRtsReaderMaker_H

/***************************************************************************
 *
 * $Id: StRtsReaderMaker.h,v 1.7 2009/10/07 00:52:32 fine Exp $
 * StRtsReaderMaker - class to fille the StEvewnt from DAQ reader
 *--------------------------------------------------------------------------
 *
 ***************************************************************************/

#include "StMaker.h"

class daqReader;
class daq_dta;
class StRtsTable;

class StRtsReaderMaker:public StMaker
{
   private:
     daqReader  *fRtsReader;
     StRtsTable *fRtsTable;
     TString     fLastQuery;
     daq_dta    *fBank;
     bool        fSlaveMode; // use  evpReader to advance the event

   protected:
      TDataSet   *FillTable();
      void        FillDaqHeader();
      StRtsTable *InitTable(const char *detName,const char *bankName);
      daqReader *InitReader();

   public:

     StRtsReaderMaker(const char *name="rts_reader");
    ~StRtsReaderMaker() ;
     TDataSet  *FindDataSet (const char* logInput,const StMaker *uppMk,
                                        const StMaker *dowMk) const;
     virtual void Clear(Option_t *option="");
     virtual Int_t Make();
     virtual Int_t Init();
     virtual Int_t InitRun(int run)  ;
     virtual void SetReader(daqReader *reader);

  // cvs
  virtual const char *GetCVS() const
    {
      static const char cvs[]="Tag $Name:  $Id: built "__DATE__" "__TIME__ ; return cvs;
    }
  
  ClassDef(StRtsReaderMaker, 0)    //StRtsReaderMaker - class to fill the StEvent from DAQ reader
};

#endif
