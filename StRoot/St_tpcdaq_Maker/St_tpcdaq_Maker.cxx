//  
// $Log: St_tpcdaq_Maker.cxx,v $
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
#include "St_tpcdaq_Maker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "St_ObjectSet.h"
#include "StTpcRawDataEvent.hh"
// #include "StTrsMaker/include/StTrsRawDataEvent.hh"
// #include "StTrsMaker/include/StDacRawDataEvent.hh"
#ifdef TRS_SIMPLE
#include "StTrsSimpleMaker/StTrsSimpleMaker.h"
#else
#include "StTrsMaker/include/StTrsUnpacker.hh"
#endif
#include "StSequence.hh"

#include "St_raw_sec_m_Table.h"
#include "St_raw_row_Table.h"
#include "St_raw_pad_Table.h"
#include "St_raw_seq_Table.h"
#include "St_type_shortdata_Table.h"

ClassImp(St_tpcdaq_Maker)

#define NSECT 24
#define NROW  45
#define DEBUG_ACTIVE_ROW 33
St_tpcdaq_Maker::St_tpcdaq_Maker(const char *name):StMaker(name) 
{
  printf("St_tpcdaq_Maker constructor.\n");
}
St_tpcdaq_Maker::~St_tpcdaq_Maker() {
}
Int_t St_tpcdaq_Maker::Init() {
  return StMaker::Init();
}
void St_tpcdaq_Maker::PrintErr(int number,char letter) {
  printf("Severe error %d(%c) in St_tpcdaq_Maker.\n",number,letter);
}
int St_tpcdaq_Maker::tpcSectorZgetPadList(int ipadrow,char **padlist) {
  static char rv[30];
  rv[0]=12;
  rv[1]=13;
  rv[2]=14;
  *padlist=rv;
  if(ipadrow==DEBUG_ACTIVE_ROW) return 3; else return 0;
}
int St_tpcdaq_Maker::myDecoderZgetSequences(int ipadrow,int pad,int *nseq,
                                  StSequence **listOfSequences) {
  static StSequence retval;
  static unsigned char vals[20];

  vals[0]= 20; vals[1]= 40; vals[2]=120; vals[3]= 40; vals[4]= 20;

  retval.startTimeBin=200;
  retval.length=5;
  retval.firstAdc=vals;

  *listOfSequences=&retval;
  if(ipadrow==DEBUG_ACTIVE_ROW) *nseq=1; else *nseq=0;

  return 0;
}
int St_tpcdaq_Maker::tpcSectorZgetSector(int j1,int *j2) {
  return 0;
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
    *raw_pad_in=new St_raw_pad("raw_pad_in",4000); sect.Add(*raw_pad_in);
  }

  *raw_pad_out=(St_raw_pad*) sect("raw_pad_out");
  if (!(*raw_pad_out)) {
    *raw_pad_out=new St_raw_pad("raw_pad_out",4000); sect.Add(*raw_pad_out);
  }

  *raw_seq_in=(St_raw_seq*) sect("raw_seq_in");
  if (!(*raw_seq_in)) {
    *raw_seq_in=new St_raw_seq("raw_seq_in",50000); sect.Add(*raw_seq_in);
  }

  *raw_seq_out=(St_raw_seq*) sect("raw_seq_out");
  if (!(*raw_seq_out)) {
    *raw_seq_out=new St_raw_seq("raw_seq_out",100000); sect.Add(*raw_seq_out);
  }

  *pixel_data_in=(St_type_shortdata*) sect("pixel_data_in");
  if (!(*pixel_data_in)) {
    *pixel_data_in=new St_type_shortdata("pixel_data_in",900000); // BBB Iwona
    sect.Add(*pixel_data_in);
  }

  *pixel_data_out=(St_type_shortdata*) sect("pixel_data_out");
  if (!(*pixel_data_out)) {
    *pixel_data_out=new St_type_shortdata("pixel_data_out",900000);
    sect.Add(*pixel_data_out);
  }
}
void St_tpcdaq_Maker::PadWrite(St_raw_pad *raw_pad_gen,int padR,int padOffset,
      int seqOffset,int nseq,int timeWhere,int pad) {
  raw_pad_st singlerow;
  singlerow.PadOffset=padOffset;
  singlerow.SeqOffset=seqOffset;
  singlerow.nseq=nseq;
  singlerow.SeqModBreak=timeWhere;
  singlerow.PadId=pad;
  raw_pad_gen->AddAt(&singlerow,padR);
}
inline void St_tpcdaq_Maker::PixelWrite(St_type_shortdata *pixel_data_gen,
      int rownum,unsigned char datum) {
  type_shortdata_st singlerow;
  singlerow.data=datum;
  pixel_data_gen->AddAt(&singlerow,rownum);
}
void St_tpcdaq_Maker::SeqWrite(St_raw_seq *raw_seq_gen,int rownumber,
    int startTimeBin,int numberOfBinsInSequence) {
  raw_seq_st singlerow;
  if(startTimeBin>=0x100) { 
    mErr=__LINE__; return; 
  }
  singlerow.m=startTimeBin;
  singlerow.i=numberOfBinsInSequence-1;
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
void St_tpcdaq_Maker::OrderTheSequences(int nseq,StSequence *los) {
#ifndef TRS_SIMPLE
  return;
#endif
  int ii,didASwap=7;
  StSequence saveValue;
  while(didASwap) {
    didASwap=0;
    for(ii=1;ii<nseq;ii++) {
      if(los[ii-1].startTimeBin>los[ii].startTimeBin) {
        saveValue.startTimeBin  = los[ii  ].startTimeBin;
        saveValue.length        = los[ii  ].length;
        saveValue.firstAdc      = los[ii  ].firstAdc;
        los[ii  ].startTimeBin  = los[ii-1].startTimeBin;
        los[ii  ].length        = los[ii-1].length;
        los[ii  ].firstAdc      = los[ii-1].firstAdc;
        los[ii-1].startTimeBin  = saveValue.startTimeBin;
        los[ii-1].length        = saveValue.length;
        los[ii-1].firstAdc      = saveValue.firstAdc;
        didASwap=7;
      }
    }
  }
}
int St_tpcdaq_Maker::Output() {
  St_raw_row *raw_row_in,*raw_row_out,*raw_row_gen;
  St_raw_pad *raw_pad_in,*raw_pad_out,*raw_pad_gen;
  St_raw_seq *raw_seq_in,*raw_seq_out,*raw_seq_gen;
  St_type_shortdata *pixel_data_in,*pixel_data_out,*pixel_data_gen;
  unsigned char *padlist;
  unsigned char *pointerToAdc;
  char dataOuter[NSECT],dataInner[NSECT];
  St_DataSet *sector;
  St_DataSetIter raw_data_tpc(m_DataSet); // m_DataSet set from name in ctor
  raw_sec_m_st singlerow;
  Int_t isect;
  int pad,sectorStatus,ipadrow,npad,ipad,seqStatus,iseq,nseq,startTimeBin,ibin;
  int prevStartTimeBin,rowR,padR,seqR;  // row counters
  int iseqSave,pixTblWhere,seqLen,timeOff,numPadsWithSignal,pixOffset;
  int seqOffset,timeWhere,nPixelThisPadRow;
  int nPixelThisPad,nSeqThisPadRow,offsetIntoPadTable;
  int nPixelPreviousPadRow;
  int pixSave,pixR,offIntoPixTbl,pixTblOff; // unsigned long
  StSequence *listOfSequences;
  St_raw_sec_m  *raw_sec_m = (St_raw_sec_m *) raw_data_tpc("raw_sec_m");

  if(!raw_sec_m) {
    raw_sec_m=new St_raw_sec_m("raw_sec_m",NSECT); raw_data_tpc.Add(raw_sec_m);
  }


  // See "DAQ to Offline", section "Better example - access by padrow,pad",
  // modifications thereto in Brian's email, SN325, and Iwona's SN325 expl.
  for(isect=1;isect<=NSECT;isect++) {
    dataOuter[isect-1]=0; dataInner[isect-1]=0;
    sector=raw_data_tpc(NameOfSector(isect));
    if(!sector) {
      raw_data_tpc.Mkdir(NameOfSector(isect));
      sector=raw_data_tpc(NameOfSector(isect));
    }
    MkTables(isect,sector,&raw_row_in,&raw_row_out,&raw_pad_in,&raw_pad_out, 
        &raw_seq_in,&raw_seq_out,&pixel_data_in,&pixel_data_out);
    sectorStatus=mUnpacker->getSector(isect,mEvent);
    if(sectorStatus) continue;
    raw_row_gen=raw_row_out; raw_pad_gen=raw_pad_out; rowR=0; padR=0;
    raw_seq_gen=raw_seq_out; pixel_data_gen=pixel_data_out; seqR=0; pixR=0;
    pixTblOff=0; nPixelPreviousPadRow=0;
    for(ipadrow=NROW-1;ipadrow>=0;ipadrow--) {
      if(ipadrow==12) { // switch to the inner part of this sector
        raw_row_gen=raw_row_in; raw_pad_gen=raw_pad_in; rowR=0; padR=0;
        raw_seq_gen=raw_seq_in; pixel_data_gen=pixel_data_in; seqR=0; 
        pixR=0; pixTblOff=0;
      }
      pixSave=pixR; iseqSave=seqR; nPixelThisPadRow=0; nSeqThisPadRow=0;
      offsetIntoPadTable=padR; pixTblWhere=0; numPadsWithSignal=0;
      seqOffset=0; npad=mUnpacker->getPadList(ipadrow+1,&padlist); pixOffset=0;
      // printf("BBB isect=%d ,ipadrow=%d ,npad=%d \n",isect,ipadrow,npad);
      if(npad>0) pad=padlist[0];
      for( ipad=0 ; ipad<npad ; pad=padlist[++ipad] ) {
        nPixelThisPad=0;
        seqStatus=mUnpacker->getSequences(ipadrow+1,pad,&nseq,&listOfSequences);
        OrderTheSequences(nseq,listOfSequences); // BBB writing on Brian's mem
        if(seqStatus<0) { PrintErr(seqStatus,'a'); mErr=__LINE__; return 7; }
        if(nseq) {
          numPadsWithSignal++; 
          if(ipadrow>=13) dataOuter[isect-1]=7; else dataInner[isect-1]=7;
        } else continue; // So we don't write meaningless rows in pad table.
        timeOff=0; offIntoPixTbl=pixR; timeWhere=0; prevStartTimeBin=-123;
        for(iseq=0;iseq<nseq;iseq++) {
          startTimeBin=listOfSequences[iseq].startTimeBin;
          if(startTimeBin<0) startTimeBin=0;
          if(startTimeBin>511) startTimeBin=511;
          if(prevStartTimeBin> startTimeBin) { mErr=__LINE__; return 7; }
          prevStartTimeBin=startTimeBin; seqLen=listOfSequences[iseq].length;
          if(startTimeBin<=255) timeWhere=iseq+1; else timeOff=0x100;
// printf("BBB startTimeBin=%3d, timeOff=%3d, diff = %x\n",startTimeBin,timeOff,
                        // startTimeBin-timeOff);
          SeqWrite(raw_seq_gen,seqR,(startTimeBin-timeOff),seqLen);
          nSeqThisPadRow++;
          pointerToAdc=listOfSequences[iseq].firstAdc;
          for(ibin=0;ibin<seqLen;ibin++) {
            PixelWrite(pixel_data_gen,pixR++,*(pointerToAdc++));
            nPixelThisPadRow++; nPixelThisPad++;
          }
          seqR++;
        } // seq loop
        if(offIntoPixTbl<0x10000) pixTblWhere++; else pixTblOff=0x10000;
        PadWrite(raw_pad_gen,padR++,pixOffset,seqOffset,nseq,timeWhere,pad);
        seqOffset+=nseq; pixOffset+=nPixelThisPad;
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
#ifdef TRS_SIMPLE
 mEvent=NULL;
 St_ObjectSet *decoder=(St_ObjectSet*)GetDataSet("Decoder"); if(!decoder) return 22;
 mUnpacker=(StTrsSimpleMaker*)(decoder->GetObject()); if(!mUnpacker) return 24;
#else
 St_ObjectSet *trsEvent=(St_ObjectSet*)GetDataSet("Event"); if(!trsEvent) return 1;
 St_ObjectSet *decoder=(St_ObjectSet*) GetDataSet("Decoder"); if(!decoder) return 2;
 mUnpacker=(StTrsUnpacker*)(decoder->GetObject());  if(!mUnpacker) return 4;
 mEvent=(StTpcRawDataEvent*)(trsEvent->GetObject());   if(!mEvent) return 3;
#endif
 return 0;
}
Int_t St_tpcdaq_Maker::Make() {
  int ii,errorCode;
  mErr=0;
  printf("I am Desi and Lucy. St_tpcdaq_Maker::Make().\n");
  errorCode=GetEventAndDecoder();
  printf("GetEventAndDecoder() = %d\n",errorCode);
  if(errorCode) {
    printf("Error: St_tpcdaq_Maker no event from DAQ/TRS (%d).\n",errorCode);
    return kStErr;
  }
  if (!m_DataSet->GetList()) Output(); else mErr=__LINE__;
  if(mErr) {
    for(ii=0;ii<5;ii++) {
      printf("\007Error Number %d in St_tpcdaq_Maker::Make().\n",mErr);
    }
    return kStErr;
  }
  printf("Got through St_tpcdaq_Maker OK.\n");
  return kStOK;
}
void St_tpcdaq_Maker::PrintInfo() {
  printf("**************************************************************\n");
  printf("St_tpcdaq_Maker, started by Herbert Ward on Feb 1 1999.\n");
  printf("Compiled on %s at  %s.\n",__DATE__,__TIME__);
  printf("* $Id: St_tpcdaq_Maker.cxx,v 1.19 1999/04/07 21:42:33 ward Exp $ \n");
  printf("**************************************************************\n");
  if(Debug()) StMaker::PrintInfo();
}
