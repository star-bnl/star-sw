// $Id: St_tpcdaq_Maker.h,v 1.14 1999/11/19 19:59:53 ward Exp $
// $Log: St_tpcdaq_Maker.h,v $
// Revision 1.14  1999/11/19 19:59:53  ward
// Converted for new TRS ZeroSuppressed Reader.
//
// Revision 1.13  1999/09/10 20:59:33  fisyak
// fix declaration
//
// Revision 1.12  1999/08/13 21:30:34  ward
// Gain corrections.  And bug fix for TRS mode.
//
// Revision 1.11  1999/08/12 15:23:38  ward
// 8 to 10 bit conversion has been implemented
//
// Revision 1.10  1999/08/07 16:44:37  ward
// Default ctor from Yuri.
//
// Revision 1.9  1999/07/29 00:49:54  fisyak
// Add default ctor
//
// Revision 1.8  1999/07/27 17:30:40  ward
// Converted to StIOMaker.  Also noise suppression.
//
// Revision 1.7  1999/07/15 13:58:26  perev
// cleanup
//
// Revision 1.6  1999/06/22 21:50:28  ward
// Remove FatalError from class definition.
//
// Revision 1.5  1999/06/21 22:27:08  ward
// Prototype connection to StDaqLib.
//
// Revision 1.4  1999/04/28 19:46:13  ward
// QA histograms.
//
// Revision 1.3  1999/04/07 19:48:41  ward
// Fixed adc mis-cast and also mis-count of pixel offset.
//
// Revision 1.2  1999/03/15 03:24:14  perev
// New maker schema
//
// Revision 1.1  1999/02/19 16:32:21  fisyak
// rename h-file and access name to Trs
//
// Revision 1.1  1999/02/18 16:56:35  ward
// There may be bugs. = Moshno oshibki.
//
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

class St_DataSet;
class St_raw_row;
class St_raw_pad;
class St_raw_seq;
class St_type_shortdata;
class StTpcRawDataEvent;
class StTpcUnpacker;
class StSequence;
class TH1F;
#define NOISE_ELIM
#define GAIN_CORRECTION
#define MAXROWPADPERSECTOR 400
#define BINRANGE 3
#include "StDaqLib/GENERIC/EventReader.hh"
#include "StTrsMaker/include/StTrsDetectorReader.hh"
#include "StTrsMaker/include/StTrsZeroSuppressedReader.hh"
#ifdef NOISE_ELIM
typedef struct {
  int npad,row[MAXROWPADPERSECTOR],pad[MAXROWPADPERSECTOR];
  int nbin,low[BINRANGE],up[BINRANGE];
} tpcdaq_noiseElim; /* one of these for each of the 24 sectors */
#endif
class St_tpcdaq_Maker : public StMaker {
 private:
   Char_t            *gConfig; //!
   StTpcRawDataEvent *mEvent; //!
   void MakeHistograms();
   void SetGainCorrectionStuff(int);
#ifdef GAIN_CORRECTION
   float fGain[45][182];
#endif

   StTrsDetectorReader* mTdr; //!
   ZeroSuppressedReader* mZsr; //!

   Int_t GetEventAndDecoder();
 protected:
   TH1F *m_seq_startTimeBin;   // These names are more or less self-
   TH1F *m_seq_sequnceLength;  // explanatory.  For example, the first one
   TH1F *m_seq_padNumber;      // means "vertical axis is number of sequences,
   TH1F *m_seq_padRowNumber;   // and the horizontal axis is startTimeBin".
   TH1F *m_pad_numSeq;         // Happy sailing.
   TH1F *m_pix_AdcValue;       //
 public: 
   St_tpcdaq_Maker(const char *name="tpc_raw",char *daqInputFile="undefined"); // If
       // the 2nd arg (daqInputFile) is NULL, then we use TRS.
   int mErr;
   
   void OrderTheSequences(int nseq,StSequence *los);
   // void FatalError(int);
   void SeqWrite(St_raw_seq *raw_seq_gen,int rownum,
                  int startTimeBin,int numberOfBinsInSequence);
   void PixelWrite(St_type_shortdata *pixel_data_gen,
                    int rownum,unsigned short datum);
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
   int getSector(Int_t isect);
   int getPadList(int whichPadRow,unsigned char **padlist);
   int getSequences(int whichPadRow,int pad,int *nseq,StSequence **seqList);
   void SetNoiseEliminationStuff(tpcdaq_noiseElim*);
   void WriteStructToScreenAndExit(tpcdaq_noiseElim*);
   void SetConfig(Char_t *conf) {gConfig = conf;  
     printf("St_tpcdaq_Maker::SetConfig, getting data from %s.\n",gConfig);
   }
   Char_t   *GetConfig() {return gConfig;}
   virtual void SetMode(Int_t mode=0) {m_Mode=mode; mode ? SetConfig("trs"): SetConfig("daq");} 
   virtual       ~St_tpcdaq_Maker();
   virtual Int_t  Init();
   virtual Int_t  Make();
// virtual void Set_mode       (Int_t   m =      2){m_mode       = m;} // *MENU*
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: St_tpcdaq_Maker.h,v 1.14 1999/11/19 19:59:53 ward Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(St_tpcdaq_Maker, 1)   //StAF chain virtual base class for Makers
};

#endif
