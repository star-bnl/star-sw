// $Id: St_tpcdaq_Maker.h,v 1.23 2000/06/24 19:13:27 ward Exp $
// $Log: St_tpcdaq_Maker.h,v $
// Revision 1.23  2000/06/24 19:13:27  ward
// added SetMinMaxTimeBucket(int lo,int hi) for Dave H.
//
// Revision 1.22  2000/06/20 01:43:36  fisyak
// Change calibrations => Calibrations to match with MySQL Db
//
// Revision 1.21  2000/06/13 17:42:55  ward
// asic and noise attached to db, but not yet gains
//
// Revision 1.20  2000/03/28 20:34:57  fine
// Adjuested to ROOT 2.24
//
// Revision 1.19  2000/03/07 21:52:15  ward
// Converted from assert() to kStFatal.
//
// Revision 1.18  2000/02/23 21:31:33  ward
// Replaced the mErr mechanism with assert()s.
//
// Revision 1.17  2000/01/14 15:29:42  ward
// Implementation of ASICS thresholds for Iwona and Dave H.
//
// Revision 1.16  1999/11/23 22:26:45  ward
// forward declaration for daq ZeroSuppressedReader
//
// Revision 1.15  1999/11/23 20:32:48  ward
// forward declaration for StTrsDetectorReader & StTrsZeroSuppressedReader
//
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
#include "St_DataSet.h"

class St_raw_row;
class St_raw_pad;
class St_raw_seq;
class St_type_shortdata;
class StTpcRawDataEvent;
class StTpcUnpacker;
class StSequence;
class TH1F;
#define NOISE_ELIM 1
#define GAIN_CORRECTION
#define MAXROWPADPERSECTOR 400
#define BINRANGE 3
#define ASIC_THRESHOLDS
class ZeroSuppressedReader;
class StTrsDetectorReader;
class StTrsZeroSuppressedReader;
#ifdef NOISE_ELIM
typedef struct {
  int npad,row[MAXROWPADPERSECTOR],pad[MAXROWPADPERSECTOR];
  int nbin,low[BINRANGE],up[BINRANGE];
} tpcdaq_noiseElim; /* one of these for each of the 24 sectors */
#endif
class St_tpcdaq_Maker : public StMaker {
 private:
   Char_t            *gConfig; //!
   char               alreadySet; //!
   char               mErr; //!
   StTpcRawDataEvent *mEvent; //!
   void MakeHistograms();
   void SetGainCorrectionStuff(int);
#ifdef GAIN_CORRECTION
   float fGain[45][182];
#endif
#ifdef NOISE_ELIM
  tpcdaq_noiseElim noiseElim[24]; //!
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
   void SetMinMaxTimeBucket(int lo,int hi);
   St_tpcdaq_Maker(const char *name="tpc_raw",char *daqInputFile="undefined"); // If
       // the 2nd arg (daqInputFile) is NULL, then we use TRS.
   
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
#ifdef ASIC_THRESHOLDS
   int mNseqLo,mNseqHi,mThreshLo,mThreshHi; // ASICS parameters
   void AsicThresholds(float gain,int *nseq,StSequence **lst);
#endif
   int getSequences(float gain,int whichPadRow,int pad,int *nseq,StSequence **seqList);
   void SetNoiseEliminationStuff();
   void WriteStructToScreenAndExit();
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
  {static const char cvs[]="Tag $Name:  $ $Id: St_tpcdaq_Maker.h,v 1.23 2000/06/24 19:13:27 ward Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(St_tpcdaq_Maker, 1)   //StAF chain virtual base class for Makers
};

#endif
