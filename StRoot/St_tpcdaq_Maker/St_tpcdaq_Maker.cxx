//  
// $Log: St_tpcdaq_Maker.cxx,v $
// Revision 1.52  2000/06/20 01:43:35  fisyak
// Change calibrations => Calibrations to match with MySQL Db
//
// Revision 1.51  2000/06/14 17:40:39  ward
// added db stuff for gains
//
// Revision 1.50  2000/06/13 17:42:55  ward
// asic and noise attached to db, but not yet gains
//
// Revision 1.49  2000/06/09 18:02:38  ward
// Removed the gets() for mErr.
//
// Revision 1.48  2000/03/07 21:52:14  ward
// Converted from assert() to kStFatal.
//
// Revision 1.47  2000/02/23 21:31:32  ward
// Replaced the mErr mechanism with assert()s.
//
// Revision 1.46  2000/01/14 15:29:42  ward
// Implementation of ASICS thresholds for Iwona and Dave H.
//
// Revision 1.43  1999/12/07 21:31:54  ward
// Eliminate 2 compile warnings, as requested by Lidia.
//
// Revision 1.42  1999/11/23 22:26:44  ward
// forward declaration for daq ZeroSuppressedReader
//
// Revision 1.41  1999/11/23 20:32:48  ward
// forward declaration for StTrsDetectorReader & StTrsZeroSuppressedReader
//
// Revision 1.40  1999/11/19 19:59:53  ward
// Converted for new TRS ZeroSuppressed Reader.
//
// Revision 1.39  1999/09/27 19:22:58  ward
// Ignore CVS comments in the noise file.
//
// Revision 1.38  1999/09/27 16:24:58  ward
// Handle CVS comments in gains file.
//
// Revision 1.37  1999/09/24 01:23:45  fisyak
// Reduced Include Path
//
// Revision 1.36  1999/09/23 16:22:00  ward
// Removed obsolete include file.
//
// Revision 1.35  1999/08/13 21:30:33  ward
// Gain corrections.  And bug fix for TRS mode.
//
// Revision 1.33  1999/08/12 15:23:37  ward
// 8 to 10 bit conversion has been implemented
//
// Revision 1.32  1999/08/07 16:44:37  ward
// Default ctor from Yuri.
//
// Revision 1.31  1999/07/29 23:07:05  ward
// Fixed bug in noise suppression.  Put gConfig back.
//
// Revision 1.30  1999/07/29 00:49:52  fisyak
// Add default ctor
//
// Revision 1.29  1999/07/27 17:30:39  ward
// Converted to StIOMaker.  Also noise suppression.
//
// Revision 1.28  1999/07/15 13:58:25  perev
// cleanup
//
// Revision 1.27  1999/06/22 19:21:43  ward
// Fix crash found by Lidia.
//
// Revision 1.26  1999/06/21 22:27:08  ward
// Prototype connection to StDaqLib.
//
// Revision 1.25  1999/05/01 03:39:52  ward
// raw_row col PadModBreak set per row instead of per half-sector
//
// Revision 1.24  1999/04/28 19:46:12  ward
// QA histograms.
//
// Revision 1.23  1999/04/09 23:30:04  ward
// Version tag, Alan Funt.
//
// Revision 1.22  1999/04/09 23:29:08  ward
// Does not waste huge amounts of table space.
//
// Revision 1.21  1999/04/08 17:21:46  ward
// Re-init nPixelPreviousPadRow at row 13, again.
//
// Revision 1.20  1999/04/08 16:40:51  ward
// Reduced table memory, will reduce more later.
//
// Revision 1.19  1999/04/07 21:42:33  ward
// Version tag, Desi and Lucy.
//
// Revision 1.18  1999/04/07 21:41:46  ward
// Incorporates nPixelThisPad fix from Bill Love.
//
// Revision 1.16  1999/04/07 19:48:40  ward
// Fixed adc mis-cast and also mis-count of pixel offset.
//
// Revision 1.13  1999/04/05 16:57:19  ward
// Updated version tag (Spock).
//
// Revision 1.12  1999/04/05 16:51:11  ward
// Now expects time bins 0-511 from Trs, and not 1-512.
//
// Revision 1.11  1999/04/02 22:45:21  ward
// Temp patch to prevent startTimeBin<1 or >512.
//
// Revision 1.9  1999/03/31 00:38:29  fisyak
// Replace search for Event and Decoder
//
// Revision 1.8  1999/03/25 22:39:10  ward
// getPadList does not set padlist when npad==0
//
// Revision 1.7  1999/03/15 03:24:14  perev
// New maker schema
//
// Revision 1.6  1999/03/10 19:18:17  ward
// Correctly fill raw_sec_m table.
//
// Revision 1.5  1999/03/03 20:52:16  ward
// Fix bug.  Pad number assignment was off by 1
//
// Revision 1.4  1999/02/21 22:30:53  ward
// small corrections
//
// Revision 1.3  1999/02/20 17:49:57  ward
// Fixed bug in setting of SeqModBreak.
//
// Revision 1.2  1999/02/19 16:32:21  fisyak
// rename h-file and access name to Trs
//
// Revision 1.1  1999/02/18 16:56:34  ward
// There may be bugs. = Moshno oshibki.
//
// BBB Yuri.  Is the above correctly initialized?
//////////////////////////////////////////////////////////////////////////
// St_tpcdaq_Maker class
// Herbert Ward, started Feb 1 1999.
//////////////////////////////////////////////////////////////////////////
#include <stdio.h>      // For binary file input (the DAQ data file).
#include <string.h>     // For binary file input (the DAQ data file).
#include <sys/types.h>  // For binary file input (the DAQ data file).
#include <sys/stat.h>   // For binary file input (the DAQ data file).
#include <fcntl.h>      // For binary file input (the DAQ data file).
///////////////////////////////////////////////////////////////////////////
#include "StDaqLib/GENERIC/EventReader.hh"
#include "StTrsMaker/include/StTrsDetectorReader.hh"
#include "StTrsMaker/include/StTrsZeroSuppressedReader.hh"
#include "St_tpcdaq_Maker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "St_ObjectSet.h"
#include "TH1.h"
#include "StTpcRawDataEvent.hh"
#include "StDaqLib/TPC/trans_table.hh"
#include "StSequence.hh"

#include "tables/St_raw_sec_m_Table.h"
#include "tables/St_raw_row_Table.h"
#include "tables/St_raw_pad_Table.h"
#include "tables/St_raw_seq_Table.h"
#include "tables/St_type_shortdata_Table.h"
#include "tables/St_asic_thresholds_Table.h"
#include "tables/St_noiseElim_Table.h"
#include "tables/St_tpcGain_Table.h"

ClassImp(St_tpcdaq_Maker)

#define PP printf(
#define ALLOC 1.2  // Must be > 1.0.  Larger runs faster, but wastes memory.
#define NSECT 24
#define NROW  45
#define DEBUG_ACTIVE_ROW 33
#define HISTOGRAMS

// We switched to the IO maker, so this is obsolete.  #include "StDaqLib/GENERIC/EventReader.hh"
#include "StDAQMaker/StDAQReader.h"
char gDAQ; /* This is TRUE if using DAQ, FALSE if using Trs. */
StDAQReader *victorPrelim;
StTPCReader *victor;
int gSector;
// obsolete since we are moving to StIOMaker ZeroSuppressedReader *gZsr;  
// obsolete since we are moving to StIOMaker DetectorReader *gDetectorReader;

St_tpcdaq_Maker::St_tpcdaq_Maker(const char *name,char *daqOrTrs):StMaker(name),gConfig(daqOrTrs)
{
  printf("This is St_tpcdaq_Maker, name = \"%s\".\n",name);
}
St_tpcdaq_Maker::~St_tpcdaq_Maker() {
}
Int_t St_tpcdaq_Maker::Init() {
  St_DataSet *herb; int junk;
#ifdef NOISE_ELIM
  // This is for noise elimination, added July 99 for Iwona.
  SetNoiseEliminationStuff();
  /*WriteStructToScreenAndExit();*/
#endif
#ifdef ASIC_THRESHOLDS
  TDataSet *tpc_calib  = GetDataBase("Calibrations/tpc");
  assert(tpc_calib);
  St_asic_thresholds *asic = (St_asic_thresholds *) tpc_calib->Find("asic_thresholds");
  assert(asic);
  St_asic_thresholds &kasic = *asic;
  mThreshLo=kasic[0].thresh_lo;
  mThreshHi=kasic[0].thresh_hi;
  mNseqLo=kasic[0].n_seq_lo;
  mNseqHi=kasic[0].n_seq_hi;
#endif
  junk=log10to8_table[0]; /* to eliminate the warnings from the compiler. */
  
  m_seq_startTimeBin  = new TH1F("tpcdaq_startBin" , 
                            "seq vs start bin" , 512 , 1.0 , 512.0 );
  m_seq_sequnceLength = new TH1F("tpcdaq_seqLen" , 
                            "seq vs seq len" , 100 , 1.0 , 100.0 );
  m_seq_padNumber     = new TH1F("tpcdaq_padNum" , 
                            "seq vs pad num" , 188 , 1.0 , 188.0 );
  m_seq_padRowNumber  = new TH1F("tpcdaq_padrowNum" , 
                            "seq vs padrow num" , 45 , 1.0 , 45.0 );
  m_pad_numSeq        = new TH1F("tpcdaq_numSeq" , 
                            "pad vs num seq" , 40 , 1.0 , 40.0 );
  m_pix_AdcValue      = new TH1F("tpcdaq_adcVal" , 
                            "pix vs ADC value" , 255 , 1.0 , 255.0 );
  if(!strcmp(GetConfig(),"daq")) { // Update this for embedding.
    gDAQ=7; 
    herb=GetDataSet("StDAQReader");
    assert(herb);
    victorPrelim=(StDAQReader*)(herb->GetObject());
    assert(victorPrelim);
  } else if(!strcmp(GetConfig(),"trs")) {
    gDAQ=0;
  } else {
     PP"-----------------------------------------------------------------\n");
     PP"The second argument of St_tpcdaq_Maker::St_tpcdaq_Maker() must be\n");
     PP"either \"daq\" or \"trs\".  Fatal error.  Please fix bfc.C.\n");
     exit(2); // Ctor called incorrectly. 
  }
  PP"end of St_tpcdaq_Maker::Init\n");
  return StMaker::Init();
}
void St_tpcdaq_Maker::PrintErr(int number,char letter) {
  printf("Severe error %d(%c) in St_tpcdaq_Maker.\n",number,letter);
}
char *St_tpcdaq_Maker::NameOfSector(int isect) {
  static char rv[16];
  sprintf(rv,"Sector_%i",isect);
  return rv;
}
void St_tpcdaq_Maker::MkTables(int isect,St_DataSet *sector,
      St_raw_row **raw_row_in,St_raw_row **raw_row_out,
      St_raw_pad **raw_pad_in,St_raw_pad **raw_pad_out, 
      St_raw_seq **raw_seq_in,St_raw_seq **raw_seq_out,
      St_type_shortdata **pixel_data_in,St_type_shortdata **pixel_data_out) {

  St_DataSetIter sect(sector);

  *raw_row_in=(St_raw_row*) sect("raw_row_in");
  if (!(*raw_row_in)) {
    *raw_row_in=new St_raw_row("raw_row_in",13); sect.Add(*raw_row_in);
  }

  *raw_row_out=(St_raw_row*) sect("raw_row_out");
  if (!(*raw_row_out)) {
    *raw_row_out=new St_raw_row("raw_row_out",32); sect.Add(*raw_row_out);
  }

  *raw_pad_in=(St_raw_pad*) sect("raw_pad_in");
  if (!(*raw_pad_in)) {
    *raw_pad_in=new St_raw_pad("raw_pad_in",3); sect.Add(*raw_pad_in);
  }

  *raw_pad_out=(St_raw_pad*) sect("raw_pad_out");
  if (!(*raw_pad_out)) {
    *raw_pad_out=new St_raw_pad("raw_pad_out",3); sect.Add(*raw_pad_out);
  }

  *raw_seq_in=(St_raw_seq*) sect("raw_seq_in");
  if (!(*raw_seq_in)) {
    *raw_seq_in=new St_raw_seq("raw_seq_in",3); sect.Add(*raw_seq_in);
  }

  *raw_seq_out=(St_raw_seq*) sect("raw_seq_out");
  if (!(*raw_seq_out)) {
    *raw_seq_out=new St_raw_seq("raw_seq_out",3); sect.Add(*raw_seq_out);
  }

  *pixel_data_in=(St_type_shortdata*) sect("pixel_data_in");
  if (!(*pixel_data_in)) {
    *pixel_data_in=new St_type_shortdata("pixel_data_in",100);
    sect.Add(*pixel_data_in);
  }

  *pixel_data_out=(St_type_shortdata*) sect("pixel_data_out");
  if (!(*pixel_data_out)) {
    *pixel_data_out=new St_type_shortdata("pixel_data_out",100);
    sect.Add(*pixel_data_out);
  }
}
void St_tpcdaq_Maker::PadWrite(St_raw_pad *raw_pad_gen,int padR,int padOffset,
      int seqOffset,int nseq,int timeWhere,int pad) {
  int nAlloc,nUsed;
  raw_pad_st singlerow;
  singlerow.PadOffset=padOffset;
  singlerow.SeqOffset=seqOffset;
  singlerow.nseq=nseq;
  singlerow.SeqModBreak=timeWhere;
  singlerow.PadId=pad;
  nAlloc=raw_pad_gen->GetTableSize(); nUsed=raw_pad_gen->GetNRows();
  if(nUsed>nAlloc-10) { raw_pad_gen->ReAllocate(Int_t(nAlloc*ALLOC+10)); }
  raw_pad_gen->AddAt(&singlerow,padR);
}
inline void St_tpcdaq_Maker::PixelWrite(St_type_shortdata *pixel_data_gen,
      int rownum,unsigned short datum) {
  int nAlloc,nUsed;
  type_shortdata_st singlerow;
  singlerow.data=datum;
  nAlloc=pixel_data_gen->GetTableSize(); nUsed=pixel_data_gen->GetNRows();
  if(nUsed>nAlloc-10) { pixel_data_gen->ReAllocate(Int_t(nAlloc*ALLOC+10)); }
  pixel_data_gen->AddAt(&singlerow,rownum);
}
void St_tpcdaq_Maker::SeqWrite(St_raw_seq *raw_seq_gen,int rownumber,
    int startTimeBin,int numberOfBinsInSequence) {
  int nAlloc,nUsed;
  raw_seq_st singlerow;
  if(startTimeBin>=0x100) mErr=1;
  singlerow.m=startTimeBin;
  singlerow.i=numberOfBinsInSequence-1;
  nAlloc=raw_seq_gen->GetTableSize(); nUsed=raw_seq_gen->GetNRows();
  if(nUsed>nAlloc-10) { raw_seq_gen->ReAllocate(Int_t(nAlloc*ALLOC+10)); }
  raw_seq_gen->AddAt(&singlerow,rownumber);
}
void St_tpcdaq_Maker::RowWrite(St_raw_row *raw_row_gen,int rownumber,
          int pixSave, int iseqSave,int nPixelPreviousPadRow,
          int nSeqThisPadRow,int offsetIntoPadTable,
          int nPadsWithSignal,int pixTblWhere,int ipadrow) {
  raw_row_st singlerow;
  singlerow.ipixel=pixSave;
  singlerow.iseq=iseqSave;
  singlerow.nseq=nSeqThisPadRow;
  singlerow.npixel=nPixelPreviousPadRow;
  singlerow.ipad=offsetIntoPadTable;
  singlerow.PadFirst=1;
  singlerow.npad=nPadsWithSignal;
  singlerow.PadModBreak=pixTblWhere;
  singlerow.PadRef='L';
  singlerow.RowId=ipadrow+1;
  raw_row_gen->AddAt(&singlerow,rownumber);
}
int St_tpcdaq_Maker::getSector(Int_t isect) {
  int rv=0;
  if(gDAQ) {         // Use DAQ.
    gSector=isect;
  } else {           // Use TRS.
    mZsr=mTdr->getZeroSuppressedReader(isect);
    if(!mZsr) rv=5; /* Either there are no hits for this sector, or there is an error. */
  }
  return rv; // 0 means there are hits and there is no error.
}
int St_tpcdaq_Maker::getPadList(int whichPadRow,unsigned char **padlist) {
  int rv; unsigned char *padlistPrelim;
  if(gDAQ) { // Use DAQ.
    rv=victor->getPadList(gSector,whichPadRow,padlistPrelim);
    *padlist=padlistPrelim;
    return rv;
  } else {           // Use TRS.
    assert(mZsr);
    rv=mZsr->getPadList(whichPadRow,padlist);
  }
  return rv;
}
#ifdef ASIC_THRESHOLDS
#define MSSPS 600 /* MSSPS = max sub sequences per sequence */
void St_tpcdaq_Maker::AsicThresholds(float gain,int *nseqOld,StSequence **lst) {
  static StSequence *pp=0; /* The new sequences are held here. */
  static int numberAllocated=0,call=0;
  unsigned char *pointerToAdc,*beg[MSSPS],*end[MSSPS],*tmp;
  int npp=0,iss,nss; /* nss = Number of SubSequences */
  int numberAboveThresh,npix,ipix,iseq,nseqNew=0,length;
  unsigned short conversion;
  char inSeq; /* boolean (true/false) value */
  if(mNseqLo<0) return; /* There is no asic.tpcdaq file. */
  call++;
  for(iseq=0;iseq<*nseqOld;iseq++) {
    npix=(*lst)[iseq].length; pointerToAdc=(*lst)[iseq].firstAdc; nss=0; inSeq=0;
    for(ipix=0;ipix<npix;ipix++) {
      conversion=gain*(*pointerToAdc);
      if(conversion> mThreshLo&&!inSeq) { inSeq=7; assert(nss<MSSPS); beg[nss  ]=pointerToAdc;   }
      if(conversion<=mThreshLo&& inSeq) { inSeq=0; assert(nss<MSSPS); end[nss++]=pointerToAdc-1; }
      pointerToAdc++;
    }
    if(inSeq) { inSeq=0; assert(nss<MSSPS); end[nss++]=pointerToAdc-1; }
    for(iss=0;iss<nss;iss++) { /* loop over candidate sequences */
      length=end[iss]-beg[iss]+1;
      if(length<=mNseqLo) continue; numberAboveThresh=0;
      for(tmp=beg[iss];tmp<=end[iss];tmp++) { if(gain*(*tmp)>mThreshHi) numberAboveThresh++; }
      if(numberAboveThresh<=mNseqHi) continue;
      if(npp>=numberAllocated) { /* allocate extra memory if necessary */
        numberAllocated=numberAllocated*1.3+5;
        pp=(StSequence*)realloc(pp,(size_t)(numberAllocated*sizeof(StSequence))); assert(pp);
      }
      pp[npp].length=length;
      pp[npp].firstAdc=beg[iss];
      pp[npp].startTimeBin=(*lst)[iseq].startTimeBin+(int)(beg[iss]-(*lst)[iseq].firstAdc);
      npp++;
    }
  }
  *nseqOld=npp; *lst=pp;
}
#endif
int St_tpcdaq_Maker::getSequences(float gain,int row,int pad,int *nseq,StSequence **lst) {
  int rv,nseqPrelim; TPCSequence *lstPrelim;
  if(gDAQ) { // Use DAQ.
    rv=victor->getSequences(gSector,row,pad,nseqPrelim,lstPrelim);
    *nseq=nseqPrelim;
    *lst=(StSequence*)lstPrelim;
  } else {           // Use TRS.
    assert(sizeof(Sequence)==sizeof(StSequence));
    rv=mZsr->getSequences(row,pad,nseq,(Sequence**)lst);
  }
#ifdef ASIC_THRESHOLDS
  AsicThresholds(gain,nseq,lst);
#endif
  return rv; // < 0 means serious error.
}
#ifdef GAIN_CORRECTION
#define GAIN_LINE_SIZE 1700
void St_tpcdaq_Maker::SetGainCorrectionStuff(int sector) { // www
  register int row,pad;

  TDataSet *tpc_calib  = GetDataBase("Calibrations/tpc"); assert(tpc_calib);

  St_tpcGain *gainObj = (St_tpcGain*) tpc_calib->Find("tpcGain"); assert(gainObj);

  assert(gainObj->GetNRows()==24);

  tpcGain_st *gains = gainObj->GetTable(); assert(gains);

  assert(sector>=1&&sector<=24);


  for(row=0;row<45;row++) {
    for(pad=0;pad<182;pad++) {
      fGain[row][pad]=gains[sector-1].Gain[row][pad];
    }
  }
}
#endif
#ifdef NOISE_ELIM
void St_tpcdaq_Maker::SetNoiseEliminationStuff() {
  int i,sector;

  for(sector=0;sector<24;sector++) { noiseElim[sector].npad=0; noiseElim[sector].nbin=0; }

  TDataSet *tpc_calib  = GetDataBase("Calibrations/tpc"); assert(tpc_calib);

  St_noiseElim *noiseObj = (St_noiseElim*) tpc_calib->Find("noiseElim"); assert(noiseObj);

  assert(noiseObj->GetNRows()==24);

  noiseElim_st *noise = noiseObj->GetTable(); assert(noise);

  for(sector=0;sector<24;sector++) {

    noiseElim[sector].npad=noise[sector].npad;
    assert(noise[sector].npad<=400); // Limit 400 is in both the St_tpcdaq_Maker.h and StDb/idl/noiseElim.idl.
    for(i=0;i<noise[sector].npad;i++) {
      noiseElim[sector].row[i]=noise[sector].row[i];
      noiseElim[sector].pad[i]=noise[sector].pad[i];
    }

    noiseElim[sector].nbin=noise[sector].nbin;
    assert(noise[sector].nbin<=3); // Limit (3) is in both the St_tpcdaq_Maker.h and StDb/idl/noiseElim.idl.
    for(i=0;i<noise[sector].nbin;i++) {
      noiseElim[sector].low[i]=noise[sector].low[i];
      noiseElim[sector].up[i]=noise[sector].up[i];
    }


  }
}
void St_tpcdaq_Maker::WriteStructToScreenAndExit() {
  int jj,ii;
  for(ii=0;ii<24;ii++) {
    PP"---------------------------------------------- sector %2d\n",ii+1);
    for(jj=0;jj<noiseElim[ii].nbin;jj++) {
      PP"Cut bins %3d to %3d.\n",noiseElim[ii].low[jj],noiseElim[ii].up[jj]);
    }
    for(jj=0;jj<noiseElim[ii].npad;jj++) {
      PP"Cut pad %3d of row %2d\n",noiseElim[ii].pad[jj],noiseElim[ii].row[jj]);
    }
  }
  exit(2); // This is in WriteStructToScreenAndExit().
}
#endif /* NOISE_ELIM */
int St_tpcdaq_Maker::Output() {
#ifdef NOISE_ELIM
  char skip; int hj,lgg;
#endif
  int pixCnt=0;
  St_raw_row *raw_row_in,*raw_row_out,*raw_row_gen;
  St_raw_pad *raw_pad_in,*raw_pad_out,*raw_pad_gen;
  St_raw_seq *raw_seq_in,*raw_seq_out,*raw_seq_gen;
  St_type_shortdata *pixel_data_in,*pixel_data_out,*pixel_data_gen;
  unsigned char *padlist;
  unsigned char *pointerToAdc;
  unsigned short conversion;
  char dataOuter[NSECT],dataInner[NSECT];
  St_DataSet *sector;
  St_DataSetIter raw_data_tpc(m_DataSet); // m_DataSet set from name in ctor
  raw_sec_m_st singlerow;
  int pad,sectorStatus,ipadrow,npad,ipad,seqStatus,iseq,nseq,startTimeBin,ibin;
  int numberOfUnskippedSeq,prevStartTimeBin,rowR,padR,seqR;  // row counters
  int iseqSave,pixTblWhere,seqLen,timeOff,numPadsWithSignal,pixOffset;
  int seqOffset,timeWhere;
  int nPixelThisPad,nSeqThisPadRow,offsetIntoPadTable;
  int nPixelPreviousPadRow;
  int isect,pixSave,pixR;
  unsigned long int nPixelThisPadRow;
  StSequence *listOfSequences;
  St_raw_sec_m  *raw_sec_m = (St_raw_sec_m *) raw_data_tpc("raw_sec_m");

  if(!raw_sec_m) {
    raw_sec_m=new St_raw_sec_m("raw_sec_m",NSECT); raw_data_tpc.Add(raw_sec_m);
  }


  // See "DAQ to Offline", section "Better example - access by padrow,pad",
  // modifications thereto in Brian's email, SN325, and Iwona's SN325 expl.
  for(isect=1;isect<=NSECT;isect++) {
#ifdef GAIN_CORRECTION
    SetGainCorrectionStuff(isect);
#endif
    dataOuter[isect-1]=0; dataInner[isect-1]=0;
    sector=raw_data_tpc(NameOfSector(isect));
    if(!sector) {
      raw_data_tpc.Mkdir(NameOfSector(isect));
      sector=raw_data_tpc(NameOfSector(isect));
    }
    MkTables(isect,sector,&raw_row_in,&raw_row_out,&raw_pad_in,&raw_pad_out, 
        &raw_seq_in,&raw_seq_out,&pixel_data_in,&pixel_data_out);
    sectorStatus=getSector(isect);
    if(sectorStatus) continue;
    raw_row_gen=raw_row_out; raw_pad_gen=raw_pad_out; rowR=0; padR=0;
    raw_seq_gen=raw_seq_out; pixel_data_gen=pixel_data_out; seqR=0; pixR=0;
    nPixelPreviousPadRow=0;
    for(ipadrow=NROW-1;ipadrow>=0;ipadrow--) {
      if(ipadrow==12) { // switch to the inner part of this sector
        raw_row_gen=raw_row_in; raw_pad_gen=raw_pad_in; rowR=0; padR=0;
        raw_seq_gen=raw_seq_in; pixel_data_gen=pixel_data_in; seqR=0; 
        pixR=0; nPixelPreviousPadRow=0;
      }
      pixSave=pixR; iseqSave=seqR; nPixelThisPadRow=0; nSeqThisPadRow=0;
      offsetIntoPadTable=padR; pixTblWhere=0; numPadsWithSignal=0;
      seqOffset=0; npad=getPadList(ipadrow+1,&padlist); pixOffset=0;
      // printf("BBB isect=%d ,ipadrow=%d ,npad=%d \n",isect,ipadrow,npad);
      if(npad>0) pad=padlist[0];
      for( ipad=0 ; ipad<npad ; pad=padlist[++ipad] ) {
#ifdef NOISE_ELIM
        skip=0;
        for(lgg=0;lgg<noiseElim[isect-1].npad;lgg++) {
          if(noiseElim[isect-1].row[lgg]==ipadrow+1&&noiseElim[isect-1].pad[lgg]==pad) { skip=7; break; }
        }
        if(skip) continue;
#endif
        nPixelThisPad=0;
        seqStatus=getSequences(fGain[ipadrow][pad-1],ipadrow+1,pad,&nseq,&listOfSequences);
        if(seqStatus<0) { PrintErr(seqStatus,'a'); mErr=2; return 1; }
        if(nseq) {
          numPadsWithSignal++; 
          if(ipadrow>=13) dataOuter[isect-1]=7; else dataInner[isect-1]=7;
        } else continue; // So we don't write meaningless rows in pad table.
        timeOff=0; timeWhere=0; prevStartTimeBin=-123;
#ifdef HISTOGRAMS
        m_pad_numSeq->Fill((Float_t)nseq);
#endif
        numberOfUnskippedSeq=0;
        for(iseq=0;iseq<nseq;iseq++) {
          startTimeBin=listOfSequences[iseq].startTimeBin;
          if(startTimeBin<0) startTimeBin=0;
          if(startTimeBin>511) startTimeBin=511;
          if(prevStartTimeBin>startTimeBin) { mErr=3; return 2; }
          prevStartTimeBin=startTimeBin; seqLen=listOfSequences[iseq].length;
#ifdef NOISE_ELIM
          skip=0;
          for(lgg=0;lgg<noiseElim[isect-1].nbin;lgg++) {
            hj=startTimeBin;
            if(hj>=(noiseElim[isect-1].low[lgg])&&hj<=(noiseElim[isect-1].up[lgg])) { skip=7; break; }
            hj=startTimeBin+seqLen-1;
            if(hj>=(noiseElim[isect-1].low[lgg])&&hj<=(noiseElim[isect-1].up[lgg])) { skip=7; break; }
          }
          if(skip) continue; // Skip this sequence.
#endif
          if(startTimeBin<=255) timeWhere=numberOfUnskippedSeq+1; else timeOff=0x100;
          SeqWrite(raw_seq_gen,seqR,(startTimeBin-timeOff),seqLen);
          nSeqThisPadRow++;
          pointerToAdc=listOfSequences[iseq].firstAdc;
#ifdef HISTOGRAMS
          m_seq_sequnceLength->Fill((Float_t)seqLen);
          m_seq_startTimeBin->Fill((Float_t)startTimeBin);
          m_seq_padNumber->Fill((Float_t)pad);
          m_seq_padRowNumber->Fill((Float_t)(ipadrow+1));
#endif
          numberOfUnskippedSeq++;
          for(ibin=0;ibin<seqLen;ibin++) {
            pixCnt++; conversion=log8to10_table[*(pointerToAdc++)]; 
#ifdef GAIN_CORRECTION
            assert(pad>0&&pad<=182);
            if(fGain[ipadrow][pad-1]>22.0) {
              printf("Fatal error in %s, line %d.\n",__FILE__,__LINE__);
              printf("ipadrow=%d, pad-1=%d, fgain=%g\n",ipadrow,pad-1,fGain[ipadrow][pad-1]);
              exit(2);
            }
            conversion=(short unsigned int)(0.5+fGain[ipadrow][pad-1]*conversion);
#endif
#ifdef HISTOGRAMS
            m_pix_AdcValue->Fill((Float_t)(conversion));
#endif
            PixelWrite(pixel_data_gen,pixR++,conversion);
            nPixelThisPadRow++; nPixelThisPad++;
          }
          seqR++;
        } // seq loop
        if(nPixelPreviousPadRow<0x10000) pixTblWhere++;
        PadWrite(raw_pad_gen,padR++,pixOffset,seqOffset,numberOfUnskippedSeq,timeWhere,pad);
        seqOffset+=numberOfUnskippedSeq; pixOffset+=nPixelThisPad;
      } // pad loop, don't confuse padR (table row #) with ipad (loop index)
      RowWrite(raw_row_gen,rowR++,pixSave,
          iseqSave,nPixelPreviousPadRow,nSeqThisPadRow,offsetIntoPadTable,
          numPadsWithSignal,pixTblWhere,ipadrow);
      nPixelPreviousPadRow=nPixelThisPadRow;
    }   // ipadrow loop
  }     // sector loop
  singlerow.tfirst=1; 
  singlerow.tlast=512;
  singlerow.TimeRef='S';
  for(isect=1;isect<=NSECT;isect++) {
    singlerow.SectorId=isect;
    if(dataInner[isect-1]) singlerow.RowRefIn ='R'; else singlerow.RowRefIn ='N';
    if(dataOuter[isect-1]) singlerow.RowRefOut='R'; else singlerow.RowRefOut='N';
    raw_sec_m->AddAt(&singlerow,isect-1);
  }
  printf("Pixel count = %d\n",pixCnt);
  return 0;
}
/*------------------------------------------------------------------------
name        init        set      used     columnName     comment
----        ----        ---      ----     ----------     -------
pixTblWhere padrow      PadWrite RowWrite PadModBreak(8) numPads, not tblrow#
pixTblOff   half sector PadWrite PadWrite PadOffset(1)   0x10000
timeWhere   pad         SeqWrite PadWrite SeqModBreak(4) numSeq
timeOff     pad         SeqWrite SeqWrite m              0x100
------------------------------------------------------------------------*/
// BBB Brian don't forget LinArray[] ("DAQ to Offline").
Int_t St_tpcdaq_Maker::GetEventAndDecoder() {
  if(gDAQ) return 0;
 St_ObjectSet *trsEvent=(St_ObjectSet*)GetDataSet("Event"); if(!trsEvent) return 1;
 mEvent=(StTpcRawDataEvent*)(trsEvent->GetObject());   if(!mEvent) return 3;
 mTdr = new StTrsDetectorReader(mEvent); assert(mTdr);
 return 0;
}
Int_t St_tpcdaq_Maker::Make() {
  int ii,errorCode;
  printf("I am Ronald McDonald. (Mar 7 2000).  St_tpcdaq_Maker::Make().\n"); 
  mErr=0;
  errorCode=GetEventAndDecoder();
  if(gDAQ) { victor=victorPrelim->getTPCReader(); assert(victor); }
  printf("GetEventAndDecoder() = %d\n",errorCode);
  if(errorCode) {
    printf("Error: St_tpcdaq_Maker no event from TRS (%d).\n",errorCode);
    return kStErr;
  }
  assert(!m_DataSet->GetList());
  Output();
  if(mErr) {
    PP"St_tpcdaq_Maker failed with error code %d.\n",mErr);
    return kStFatal;
  } else {
    printf("Got through St_tpcdaq_Maker OK.\n");
  }
  return kStOK;
}
