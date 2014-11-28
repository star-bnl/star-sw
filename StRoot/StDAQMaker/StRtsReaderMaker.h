#ifndef STAR_StRtsReaderMaker_H
#define STAR_StRtsReaderMaker_H

/***************************************************************************
 *
 * $Id: StRtsReaderMaker.h,v 1.10 2014/08/06 11:42:55 jeromel Exp $
 * StRtsReaderMaker - class to fille the StEvewnt from DAQ reader
 *--------------------------------------------------------------------------
 *
 ***************************************************************************/

#include "StMaker.h"

class daqReader;
class daq_dta;
class StRtsTable;
class StStreamFile;

class StRtsReaderMaker:public StMaker
{
   private:
     daqReader     *fRtsReader;
     StStreamFile  *fDatReader;
     StRtsTable    *fRtsTable;
     TString        fLastQuery;
     daq_dta       *fBank;

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
     virtual void SetDaqReader(daqReader    *reader);
     virtual void SetDatReader(StStreamFile *reader);

  // cvs
  virtual const char *GetCVS() const
    {
      static const char cvs[]="Tag $Name:  $Id: built " __DATE__ " " __TIME__ ; return cvs;
    }
  
  ClassDef(StRtsReaderMaker, 0)    //StRtsReaderMaker - class to fill the StEvent from DAQ reader
};

#endif
