/*!
 * \class St_tpcdaq_Maker 
 * \author Herbert Ward
 * \date started Feb 1 1999.
 *
 */

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
#include "TDataSet.h"

class St_raw_row;
class St_raw_pad;
class St_raw_seq;
class St_type_shortdata;
#include "StTpcRawDataEvent.hh"
class StTpcUnpacker;
class StSequence;
class TH1F;
#define MAXROWPADPERSECTOR 400            /*! \def MAXROWPADPERSECTOR SomeExplaination */
#define BINRANGE 3                        /*! \def BINRANGE SomeExplaination           */

class ZeroSuppressedReader;
class StTrsDetectorReader;
class StTrsZeroSuppressedReader;

typedef struct {
  int npad,row[MAXROWPADPERSECTOR],pad[MAXROWPADPERSECTOR];
  int nbin,low[BINRANGE],up[BINRANGE];
} tpcdaq_noiseElim; /* one of these for each of the 24 sectors */
class St_tpcdaq_Maker : public StMaker {
 private:
   int                mCorrectionMask; //!
   char               mMergeSequences; //!

   Char_t            *gConfig;         //!
   char               alreadySet;      //!
   char               mErr;            //!
   StTpcRawDataEvent *mEvent;          //!
   void MakeHistograms();
   void SetGainCorrectionStuff(int);
   float fGain[45][182];
   tpcdaq_noiseElim noiseElim[24];     //!

   StTrsDetectorReader* mTdr;          //!
   StTrsZeroSuppressedReader* mZsr;    //!
   Int_t daq_flag;                     //! 

   Int_t GetEventAndDecoder();
 protected:
   TH1F *m_seq_startTimeBin;   // These names are more or less self-
   TH1F *m_seq_sequnceLength;  // explanatory.  For example, the first one
   TH1F *m_seq_padNumber;      // means "vertical axis is number of sequences,
   TH1F *m_seq_padRowNumber;   // and the horizontal axis is startTimeBin".
   TH1F *m_pad_numSeq;         // Happy sailing.
   TH1F *m_pix_AdcValue;       //
 private:
   void PrepareSimulatedData(unsigned int sector,unsigned int *out);
   void DAQ100clOutput(const unsigned int *pTPCP); //!
   void DAQ100clTableOut(unsigned int,unsigned int,unsigned int, char,const unsigned int *); //!
 public: 
   void SetCorrection(int);
   int  GetCorrection(void);
   char SetSequenceMerging(char);
   char WhetherToSwap(unsigned int x);
   unsigned int Swap4(char,unsigned int x); //!
   unsigned short int Swap2(char,unsigned short int x); //!
   void ExcludeTheseTimeBins(int lo1,int hi1,int lo2,int hi2,int lo3,int hi3);
   St_tpcdaq_Maker(const char *name="tpc_raw",char *daqInputFile="undefined"); // If
       // the 2nd arg (daqInputFile) is NULL, then we use TRS.
   
   void OrderTheSequences(int nseq,StSequence *los);
   // void FatalError(int);
   void SeqWrite(St_raw_seq *raw_seq_gen,int rownum,
                  int startTimeBin,int numberOfBinsInSequence);
   void PixelWrite(St_type_shortdata *pixel_data_gen,St_type_shortdata *pixel_indx_gen,
                    int rownum,unsigned short datum,unsigned short id);
   void PadWrite(St_raw_pad *raw_pad_gen,int padR,int padOffset,
      int seqOffset,int nseq,int nSeqB4Offset,int pad);
   void RowWrite(St_raw_row *raw_row_gen,int rownumber,
          int pixSave, int iseqSave,int nPixelThisPadRow,
          int nSeqThisPadRow,int offsetIntoPadTable,
          int nPadsWithSignal,int pixTblWhere,int ipadrow);
   void MkTables(
      int isect,
      TDataSet *sector,
      St_raw_row **raw_row_in,
      St_raw_row **raw_row_out,
      St_raw_pad **raw_pad_in,
      St_raw_pad **raw_pad_out,
      St_raw_seq **raw_seq_in,
      St_raw_seq **raw_seq_out,
      St_type_shortdata **pixel_data_in,
      St_type_shortdata **pixel_data_out,
      St_type_shortdata **pixel_indx_in,
      St_type_shortdata **pixel_indx_out);
   char *NameOfSector(int isect);
   void PrintErr(int,char);
   int Output();
   int getSector(Int_t isect);
   int getPadList(int whichPadRow,unsigned char **padlist);
   int mNseqLo,mNseqHi,mThreshLo,mThreshHi; // ASICS parameters
   void AsicThresholds(float gain,int *nseq,StSequence **lst,UShort_t ***idt);
   int getSequences(float gain,int whichPadRow,int pad,int *nseq,StSequence **seqList, UShort_t ***listOfIds);
   void SetDAQFlag(Int_t);
   void SetNoiseEliminationStuff();
   void WriteStructToScreenAndExit();
   void SetConfig(Char_t *conf) {gConfig = conf;  
     printf("St_tpcdaq_Maker::SetConfig, getting data from %s.\n",gConfig);
   }
   Char_t   *GetConfig() {return gConfig;}
   virtual void SetMode(Int_t mode=0) {
     m_Mode=mode; 
     if (m_Mode == 1) SetConfig("trs");
     else {if (m_Mode == 2)  SetConfig("l3"); else  SetConfig("daq");}
   }  
   virtual       ~St_tpcdaq_Maker();
   virtual Int_t  Init();
   virtual Int_t  InitRun(Int_t );
   virtual Int_t  Make();
   // virtual void Set_mode       (Int_t   m =      2){m_mode       = m;} // *MENU*
   virtual const char *GetCVS() const {
     static const char cvs[]="Tag $Name:  $ $Id: St_tpcdaq_Maker.h,v 1.42 2008/06/20 14:58:08 fisyak Exp $ built "__DATE__" "__TIME__ ; 
     return cvs;
   }

   ClassDef(St_tpcdaq_Maker,0)   //StAF chain virtual base class for Makers
};

#endif



// $Id: St_tpcdaq_Maker.h,v 1.42 2008/06/20 14:58:08 fisyak Exp $
// $Log: St_tpcdaq_Maker.h,v $
// Revision 1.42  2008/06/20 14:58:08  fisyak
// Change interal presentation for ADC from UChat_t to Short_t
//
// Revision 1.41  2005/09/09 22:14:17  perev
// IdTruth added
//
// Revision 1.40  2004/03/10 05:49:52  jeromel
// This unfortunatly appears all diferent but main changes are
// - doxygenized
// - implement SetSequenceMerging()
//
// Revision 1.39  2004/01/02 17:53:18  ward
// Add receiver board and mezzanine to daq100cl output table.
//
// Revision 1.38  2003/12/24 13:44:55  fisyak
// Add (GEANT) track Id information in Trs; propagate it via St_tpcdaq_Maker; account interface change in StTrsZeroSuppressedReaded in StMixerMaker
//
// Revision 1.37  2003/10/28 20:35:54  ward
// Chain control of NOISE_ELIM GAIN_CORRECTION ASIC_THRESHOLDS.
//
// Revision 1.36  2003/09/10 19:47:51  perev
// ansi corrs
//
// Revision 1.35  2003/02/13 17:05:05  ward
// Restored noise elim on request from David Hardtke.
//
// Revision 1.34  2002/10/15 19:24:32  ward
// Stuff for pre-.daq file testing, and better handling of daq_flag.
//
// Revision 1.33  2002/10/13 20:43:38  ward
// Support for decoding DAQ100 data and writing it into a table.
//
// Revision 1.32  2002/10/11 18:10:54  jeromel
// Changes to accomodate for DAQ100 Cluster reading or raw hit reading
//
// Revision 1.31  2002/02/13 22:03:00  jeromel
// Swell, made incomplete prototype .. Now ** really ** fixed.
//
// Revision 1.30  2002/02/13 21:56:29  jeromel
// InitRun() function proto.
//
// Revision 1.29  2002/02/13 17:03:53  ward
// Turned off NOISE_ELIM.
//
// Revision 1.28  2001/02/15 22:25:30  fisyak
// Add l3 option
//
// Revision 1.27  2001/02/13 18:28:39  fisyak
// Step back with switching calibration
//
// Revision 1.25  2000/06/26 18:25:20  ward
// mulitple veto zones for ExcludeTheseTimeBins
//
// Revision 1.24  2000/06/24 19:26:31  ward
// changed the name of the function to ExcludeTheseTimeBins
//
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
