#ifndef STAR_StRtsReaderMaker_H
#define STAR_StRtsReaderMaker_H

/***************************************************************************
 *
 * $Id: StRtsReaderMaker.h,v 1.1 2008/04/08 20:03:52 fine Exp $
 * StRtsReaderMaker - class to fille the StEvewnt from DAQ reader
 *--------------------------------------------------------------------------
 *
 ***************************************************************************/

#include "StMaker.h"

class rts_reader;
class daq_dta;
class StRtsTable;

class StRtsReaderMaker:public StMaker
{
   private:
     rts_reader *fRtsReader;
     TString     fFileName;
     StRtsTable *fRtsTable;
     TString     fLastQuery;
     daq_dta    *fBank;
     bool        fSlaveMode; // use  evpReader to advance the event

   protected:
      TDataSet   *FillTable();
      StRtsTable *InitTable(const char *bankName);
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
     void  SetFileName (const char *fileName) {fFileName = fileName;}
     const TString &FileName() const { return fFileName;}
  
  // cvs
  virtual const char *GetCVS() const
    {
      static const char cvs[]="Tag $Name:  $Id: built "__DATE__" "__TIME__ ; return cvs;
    }
  
  ClassDef(StRtsReaderMaker, 0)    //StRtsReaderMaker - class to fill the StEvent from DAQ reader
};

#endif
