// $Id: St_tpcdaq_Maker.hh,v 1.1 1999/02/18 16:56:35 ward Exp $
// $Log: St_tpcdaq_Maker.hh,v $
// Revision 1.1  1999/02/18 16:56:35  ward
// There may be bugs. = Moshno oshibki.
//
// #define TRS_SIMPLE
#ifndef STAR_St_tpcdaq_Maker
#define STAR_St_tpcdaq_Maker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_tpcdaq_Maker virtual base class for Maker                         //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif

#include "StTpcRawDataEvent.hh"
// #include "StTrsRawDataEvent.hh"
// #include "StDacRawDataEvent.hh"
#ifdef TRS_SIMPLE
#include "StTrsSimpleMaker.h"
#else
#include "StTrsUnpacker.hh"
#endif
#include "StSequence.hh"

#include "St_raw_sec_m_Table.h"
#include "St_raw_row_Table.h"
#include "St_raw_pad_Table.h"
#include "St_raw_seq_Table.h"
#include "St_type_shortdata_Table.h"

class St_tpcdaq_Maker : public StMaker {
 private:
   Bool_t drawinit;
   StTpcRawDataEvent *mEvent; //!
#ifdef TRS_SIMPLE
   StTrsSimpleMaker *mUnpacker; //!
#else
   StTpcUnpacker *mUnpacker; //!
#endif
   Int_t GetEventAndDecoder();
 protected:
 public: 
                  St_tpcdaq_Maker(const char *name="tpcdaq", 
                           const char *title="tpcdaq_something");
   int mErr;
   
   void OrderTheSequences(int nseq,StSequence *los);
   void FatalError(int);
   void SeqWrite(St_raw_seq *raw_seq_gen,int rownum,
                  int startTimeBin,int numberOfBinsInSequence);
   void PixelWrite(St_type_shortdata *pixel_data_gen,
                    int rownum,char datum);
   void PadWrite(St_raw_pad *raw_pad_gen,int padR,int padOffset,
      int seqOffset,int nseq,int nSeqB4Offset,int pad);
   void RowWrite(St_raw_row *raw_row_gen,int rownumber,
          int pixSave, int iseqSave,int nPixelThisPadRow,
          int nSeqThisPadRow,int offsetIntoPadTable,
          int nPadsWithSignal,int pixTblWhere,int ipadrow);
   void MkTables(
      int isect,
      St_DataSet *sector,
      St_raw_row **raw_row_in,
      St_raw_row **raw_row_out,
      St_raw_pad **raw_pad_in,
      St_raw_pad **raw_pad_out,
      St_raw_seq **raw_seq_in,
      St_raw_seq **raw_seq_out,
      St_type_shortdata **pixel_data_in,
      St_type_shortdata **pixel_data_out);
   char *NameOfSector(int isect);
   void PrintErr(int,char);
   int Output();
   int tpcSectorZgetSector(int j1,int *j2); // junk for early debugging
   int tpcSectorZgetPadList(int ipad,char **padlist); // junk 4 early debugging
   int myDecoderZgetSequences(int ipadrow,int pad,int *nseq,
                 StSequence **listOfSequences); // junk for early debugging
   virtual       ~St_tpcdaq_Maker();
   virtual Int_t Init();
   virtual Int_t  Make();
   virtual void   PrintInfo();
// virtual void Set_mode       (Int_t   m =      2){m_mode       = m;} // *MENU*
   ClassDef(St_tpcdaq_Maker, 1)   //StAF chain virtual base class for Makers
};

#endif
