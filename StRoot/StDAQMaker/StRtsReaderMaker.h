#ifndef STAR_StRtsReaderMaker_H
#define STAR_StRtsReaderMaker_H

/***************************************************************************
 *
 * $Id: StRtsReaderMaker.h,v 1.5 2008/11/25 21:33:23 fine Exp $
 * StRtsReaderMaker - class to fille the StEvewnt from DAQ reader
 *--------------------------------------------------------------------------
 *
 ***************************************************************************/

#include "StMaker.h"

#if !defined(OLD_EVP_READER) && !defined(NEW_DAQ_READER)
class rts_reader; // new  2007 DAQ file reader
#elif defined(NEW_DAQ_READER)
class daqReader;
typedef daqReader rts_reader;
#endif

class daq_dta;
class StRtsTable;

class StRtsReaderMaker:public StMaker
{
   private:
     rts_reader *fRtsReader;
     StRtsTable *fRtsTable;
     TString     fLastQuery;
     daq_dta    *fBank;
     bool        fSlaveMode; // use  evpReader to advance the event

   protected:
      TDataSet   *FillTable();
      StRtsTable *InitTable(const char *detName,const char *bankName);
      rts_reader *InitReader();

   public:

     StRtsReaderMaker(const char *name="rts_reader");
    ~StRtsReaderMaker() ;
     TDataSet  *FindDataSet (const char* logInput,const StMaker *uppMk,
                                        const StMaker *dowMk) const;
     virtual void Clear(Option_t *option="");
     virtual Int_t Make();
     virtual Int_t Init();
     virtual Int_t InitRun(int run)  ;

  // cvs
  virtual const char *GetCVS() const
    {
      static const char cvs[]="Tag $Name:  $Id: built "__DATE__" "__TIME__ ; return cvs;
    }
  
  ClassDef(StRtsReaderMaker, 0)    //StRtsReaderMaker - class to fill the StEvent from DAQ reader
};

#endif
