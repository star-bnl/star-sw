// $Id: St_trg_Maker.cxx,v 1.11 2000/05/16 02:01:46 fisyak Exp $
// $Log: St_trg_Maker.cxx,v $
// Revision 1.11  2000/05/16 02:01:46  fisyak
// Correction for removed  trg_fillDst
//
// Revision 1.10  2000/05/04 22:25:21  ward
// New 3d DST tables, and some support for sim.
//
// Revision 1.9  2000/02/25 17:58:41  ward
// Changed array sizes for ctb and mwc.  Thx to Joakim Nystrand.
//
// Revision 1.8  2000/02/04 18:57:17  ward
// Added dst_L1_Trigger and dst_L2_Trigger to output.
//
// Revision 1.7  2000/01/26 18:55:37  ward
// Changed name of L0 table from TrgDet2 to L0_Trigger.
//
// Revision 1.6  2000/01/24 20:35:39  ward
// Access trigger data.
//
// Revision 1.5  1999/07/15 13:58:32  perev
// cleanup
//
// Revision 1.4  1999/07/11 19:32:42  druss
// Change DST table from dst_TriggerDetectors to dst_TrgDet.
// The classname can only be 19 letters long.
//
// also put the m_Dataset->add line before the PAM is called.
// SPIROS is checking in the change to the actual idl file (part of global)
//
// Revision 1.3  1999/06/24 18:02:46  druss
// add a line to make sure that the table exists...
//
// Revision 1.2  1999/03/14 00:25:40  perev
// New makers
//
// Revision 1.1  1999/02/06 01:51:22  yepes
// Add trg maker
//
// Revision 1.7  1998/10/31 00:25:45  fisyak
// Makers take care about branches
//
// Revision 1.6  1998/10/06 18:00:29  perev
// cleanup
//
// Revision 1.5  1998/10/02 13:46:08  fine
// DataSet->DataSetIter
//
// Revision 1.4  1998/08/14 15:25:58  fisyak
// add options
//
// Revision 1.3  1998/08/10 02:32:07  fisyak
// Clean up
//
// Revision 1.2  1998/07/20 15:08:15  fisyak
// Add tcl and tpt
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_trg_Maker class for Makers                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "St_trg_Maker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "StDAQMaker/StDAQReader.h"
#include "StDAQMaker/StTRGReader.h"
#include "tables/St_dst_L0_Trigger_Table.h" // 24dec99
#include "tables/St_dst_L1_Trigger_Table.h" // 02feb00
#include "tables/St_dst_L2_Trigger_Table.h" // 02feb00
#include "tables/St_dst_TrgDet_Table.h" // 24dec99
#include "tables/St_ctu_cor_Table.h"
#include "tables/St_mwc_raw_Table.h"
#include "tables/St_dst_TrgDet_Table.h"
#include "trgStructures.h" // From the STAR trigger group, and may need occasional updating.
// The structure MarilynMonroe_t is like the trigger group's TrgDataType, except that
// it does not include TrgEvtHeader, which they don't pass to DAQ.
#define PREPOST 11 // CAUTION:  this number is also in dst_TrgDet.idl
typedef struct {
  EvtDescData    EvtDesc;  /* L1 Event Descriptor Data */  
  TrgSumData     TrgSum;   /* summary data */
  RawTrgDet      RAW[PREPOST];      /* For simplicity, I assume that you don't want pre and post history. */
} MarilynMonroe_t;
MarilynMonroe_t *GraceSlick;


ClassImp(St_trg_Maker)
#define PP printf(

//_____________________________________________________________________________
St_trg_Maker::St_trg_Maker(const char *name):StMaker(name){
   drawinit=kFALSE;
}
//_____________________________________________________________________________
St_trg_Maker::~St_trg_Maker(){
}
//_____________________________________________________________________________
Int_t St_trg_Maker::Init(){
// Create tables
// Create Histograms    
  InitMwcArrays();
  InitCtbArrays();
  return StMaker::Init();
}
//_____________________________________________________________________________
void St_trg_Maker::SecondDstDaq(St_dst_L0_Trigger *dst2) {
  int i;
  dst_L0_Trigger_st *tt = dst2->GetTable();
  tt->TriggerActionWd  = GraceSlick->EvtDesc.TCU1.FIFO1.TrgActionWd;
  tt->TriggerWd        = GraceSlick->EvtDesc.TCU1.FIFO1.TrgToken;
  for(i=0;i<32;i++) tt->CPA[i]=GraceSlick->TrgSum.DSM.CPA[i];
  tt->MWC_CTB_mul      = GraceSlick->TrgSum.DSM.lastDSM[2]; // Per Hank Crawford, Jan 6 2000.
  tt->MWC_CTB_dipole   = 0;
  tt->MWC_CTB_topology = 0;
  tt->MWC_CTB_moment   = 0;
}
void St_trg_Maker::SecondDstSim(St_dst_L0_Trigger *dst2) {
  int i;
  dst_L0_Trigger_st *tt = dst2->GetTable();
  tt->TriggerActionWd  = 0;
  tt->TriggerWd        = 0;
  for(i=0;i<32;i++) tt->CPA[i]=0;
  tt->MWC_CTB_mul      = 0;
  tt->MWC_CTB_dipole   = 0;
  tt->MWC_CTB_topology = 0;
  tt->MWC_CTB_moment   = 0;
}
Int_t St_trg_Maker::Make(){

  St_dst_TrgDet     *dst1 = new St_dst_TrgDet("TrgDet",1);         if(!dst1) return kStWarn; dst1->SetNRows(1);
  St_dst_L0_Trigger *dst2 = new St_dst_L0_Trigger("L0_Trigger",1); if(!dst2) return kStWarn; dst2->SetNRows(1);
  St_dst_L1_Trigger *dst3 = new St_dst_L1_Trigger("L1_Trigger",1); if(!dst3) return kStWarn; dst3->SetNRows(1);
  St_dst_L2_Trigger *dst4 = new St_dst_L2_Trigger("L2_Trigger",1); if(!dst4) return kStWarn; dst4->SetNRows(1);
  m_DataSet->Add(dst1);
  m_DataSet->Add(dst2);
  m_DataSet->Add(dst3);
  m_DataSet->Add(dst4);

  St_DataSet *herb = GetDataSet("StDAQReader");
  if(herb) return Daq(herb,dst1,dst2,dst3,dst4); else return Sim(dst1,dst2,dst3,dst4);

}
void St_trg_Maker::CtbMwcDaq(St_dst_TrgDet *dst1) { // For sim data, use output of trg_fillDst module.
  int npre,npost,pp,i,tray,slat,subsector,sector;
  dst_TrgDet_st *tt = dst1->GetTable();

  npre=GraceSlick->EvtDesc.npre;
  npost=GraceSlick->EvtDesc.npost;
  tt->npre=npre;
  tt->npost=npost;
  for(pp=0;pp<1+npre+npost;pp++) {
    assert(pp<PREPOST);
    for(slat=0;slat<2;slat++) {
      for(tray=0;tray<120;tray++) {
        tt->nCtb[tray][slat][pp]=GraceSlick->RAW[pp].CTB[ctbmap[tray][slat]];
        tt->timeCtb[tray][slat][pp]=0;  // May be settable in some events from the LS bit, consult Hank.
      }
    }
    for(sector=0;sector<24;sector++) {
      for(subsector=0;subsector<4;subsector++) {
        tt->nMwc[sector][subsector][pp]=GraceSlick->RAW[pp].MWC[mwcmap[sector][subsector]];
      }
    }
    for(i=0;i<16;i++) tt->ctbaux[i][pp]=GraceSlick->RAW[pp].CTB[auxctbmap[i]];
    for(i=0;i<32;i++) tt->mwcaux[i][pp]=GraceSlick->RAW[pp].MWC[auxmwcmap[i]];
  }
}
void St_trg_Maker::SanityCheck() {
  int pp,lim;
  lim=1+GraceSlick->EvtDesc.npre+GraceSlick->EvtDesc.npost;
  for(pp=0;pp<lim;pp++) {
    assert(GraceSlick->RAW[pp].RawDetHeader[0]=='R');   // If one of these fails,
    assert(GraceSlick->RAW[pp].RawDetHeader[1]=='D');   // then the .daq file may
    assert(GraceSlick->RAW[pp].CTBdataHeader[0]=='C');  // be corrupted, the trigger
    assert(GraceSlick->RAW[pp].CTBdataHeader[1]=='T');  // data may be missing, or
    assert(GraceSlick->RAW[pp].MWCdataHeader[0]=='M');  // there may be trouble with StDaqLib
    assert(GraceSlick->RAW[pp].MWCdataHeader[1]=='W');  // or StDAQMaker.
    assert(GraceSlick->RAW[pp].EMCdataHeader[0]=='E');  // 
    assert(GraceSlick->RAW[pp].EMCdataHeader[1]=='M');  // 
  }
}
int St_trg_Maker::Daq(St_DataSet *herb,St_dst_TrgDet *dst1,St_dst_L0_Trigger *dst2,
      St_dst_L1_Trigger *dst3,St_dst_L2_Trigger *dst4) {

  char *ptr;
  fVictorPrelim=(StDAQReader*)(herb->GetObject()); assert(fVictorPrelim);
  fVictor=fVictorPrelim->getTRGReader(); assert(fVictor);
  assert(fVictor->thereIsTriggerData()); // We had bfc.C(" l0 "), but we have no TRG bank in
                                          // .daq file.
              // StTRGReader *St_trg_Maker::fVictor;
              // TRG_Reader  *StTRGReader::fTRGImpReader;
              // Bank_TRGD   *TRG_Reader::pBankTRGD;
  ptr=(char*)(fVictor->fTRGImpReader->pBankTRGD);
  assert(ptr);
  ptr+=40; /* skip header */
  GraceSlick=(MarilynMonroe_t*)ptr;
  SanityCheck();
  // dumpDataToScreenAndExit();
  VpdDaq(dst1);       // The function
  ZdcDaq(dst1);       // St_trg_Maker::Sim
  CtbMwcDaq(dst1);    // has four lines
  SecondDstDaq(dst2); // which are analogous to these four.
  TakeCareOfL1andL2Daq(dst3,dst4);

  return kStOK;
}
void St_trg_Maker::TakeCareOfL1andL2Daq(St_dst_L1_Trigger *dst3,St_dst_L2_Trigger *dst4) {
  int i;
  dst_L1_Trigger_st *tt1 = dst3->GetTable();
  dst_L2_Trigger_st *tt2 = dst4->GetTable();
  for(i=0;i<32;i++) {
    tt1->L1_result[i] = GraceSlick->TrgSum.L1Result[i];
    tt2->L2_result[i] = GraceSlick->TrgSum.L2Result[i];
  }
}
void St_trg_Maker::TakeCareOfL1andL2Sim(St_dst_L1_Trigger *dst3,St_dst_L2_Trigger *dst4) {
  // Like the rest of the sim stuff, this needs to be filled in.
}
int St_trg_Maker::HandleCtu(St_ctu_cor *ctu_cor,St_dst_TrgDet *dst1) {
  return 7;
}
void St_trg_Maker::Vladimir2Herbert(int input,int *sector,int *subsector) {
  int offset;
  assert(input>=1&&input<=96);
  if(input>48) { input-=48; offset=0; } else offset=12;
  *sector = (input-1)/4 + 1 ;
  *subsector = 4 + input - 4 * (*sector) ;
  *sector += offset;
}
int St_trg_Maker::HandleMwc(St_mwc_raw *mwc_raw,St_dst_TrgDet *dst1) {
  int sector,subsector,index,irow;
  if(!mwc_raw) { PP"Did not find the mwc_raw table mwc.\n"); return 7; }
  mwc_raw_st    *vladimir = mwc_raw->GetTable(); assert(vladimir);
  dst_TrgDet_st *herbert  = dst1->GetTable(); assert(herbert);
  herbert->npre=0; herbert->npost=0;
  for(irow=0;irow<mwc_raw->GetNRows();irow++) {
    index=vladimir[irow].sector;
    Vladimir2Herbert(index,&sector,&subsector);
    assert(sector>=1&&sector<=24);
    assert(subsector>=1&&subsector<=4);
    herbert[0].nMwc[sector-1][subsector-1][0]=vladimir[irow].count; // The "[0]" means "the triggered event (pre/post).
  }
  return 7;
}
int St_trg_Maker::Sim(St_dst_TrgDet *dst1,St_dst_L0_Trigger *dst2,St_dst_L1_Trigger *dst3,St_dst_L2_Trigger *dst4) {
  int rv=kStOK;

  St_DataSet *ctf = GetInputDS("ctf");
  St_DataSet *mwc = GetInputDS(".make/mwc/.data");
  if (!ctf || !mwc) return kStWarn;
  St_ctu_cor   *ctu_cor  = (St_ctu_cor   *) ctf->Find("ctb_cor");
  St_mwc_raw   *mwc_raw  = (St_mwc_raw   *) mwc->Find("raw");
  // May 3 2000.   if (!ctu_cor || !mwc_raw) return kStWarn;

  VpdSim(dst1); 
  ZdcSim(dst1);
  /* May 3 2000.   Int_t Res = trg_fillDst(ctu_cor,mwc_raw,dst1); if (Res != kSTAFCV_OK) return kStWarn; */
  if(!HandleMwc(mwc_raw,dst1)) rv=kStWarn;
  if(!HandleCtu(ctu_cor,dst1)) rv=kStWarn;
  SecondDstSim(dst2);
  TakeCareOfL1andL2Sim(dst3,dst4);

  return rv;
}
void St_trg_Maker::VpdDaq(St_dst_TrgDet *dst1) {
  int i;
  dst_TrgDet_st *tt = dst1->GetTable();
  for(i=0;i<48;i++) { 
    tt->adcVPD[i]=0;
    tt->timeVPD[i]=0;
  }
  tt->TimeEastVpd=0;
  tt->TimeWestVpd=0;
  tt->vertexZ=0;
}
void St_trg_Maker::ZdcDaq(St_dst_TrgDet *dst1) {
  int i;
  dst_TrgDet_st *tt = dst1->GetTable();
  for(i=0;i<16;i++) { 
    tt->adcZDC[i]=GraceSlick->TrgSum.DSM.ZDC[i];
    tt->tdcZDC[i]=0;
  }
  tt->adcZDCEast=GraceSlick->TrgSum.DSM.ZDC[3];
  tt->adcZDCWest=GraceSlick->TrgSum.DSM.ZDC[7];
  tt->adcZDCsum=GraceSlick->TrgSum.DSM.ZDC[3]+GraceSlick->TrgSum.DSM.ZDC[7];
}
void St_trg_Maker::VpdSim(St_dst_TrgDet *dst1) {
  int i;
  dst_TrgDet_st *tt = dst1->GetTable();
  for(i=0;i<48;i++) { 
    tt->adcVPD[i]=0;
    tt->timeVPD[i]=0;
  }
  tt->TimeEastVpd=0;
  tt->TimeWestVpd=0;
  tt->vertexZ=0;
}
void St_trg_Maker::ZdcSim(St_dst_TrgDet *dst1) {
  int i;
  dst_TrgDet_st *tt = dst1->GetTable();
  for(i=0;i<16;i++) { 
    tt->adcZDC[i]=0;
    tt->tdcZDC[i]=0;
  }
  tt->adcZDCEast=0;
  tt->adcZDCWest=0;
  tt->adcZDCsum=0;
}
//_____________________________________________________________________________
void St_trg_Maker::dumpDataToScreenAndExit() {
  int i;
                   printf("%15s      = %d\n","TCUdataBytes",GraceSlick->EvtDesc.TCUdataBytes);
                   printf("%15s      = %d\n","TCUEvtDesc",GraceSlick->EvtDesc.TCUEvtDesc);
                   printf("%15s      = %d\n","TrgDataFmtVer",GraceSlick->EvtDesc.TrgDataFmtVer);
                   printf("%15s      = %d\n","bunchXing_hi",GraceSlick->EvtDesc.bunchXing_hi);
                   printf("%15s      = %d\n","bunchXing_lo",GraceSlick->EvtDesc.bunchXing_lo);
                   printf("%15s      = %d\n","npre",GraceSlick->EvtDesc.npre);
                   printf("%15s      = %d\n","npost",GraceSlick->EvtDesc.npost);
                   printf("%15s      = %d\n","TrgSumBytes",GraceSlick->TrgSum.TrgSumBytes);
for(i=0;i<  2;i++) printf("%15s[%3d] = %d\n","TrgSumHeader",i,GraceSlick->TrgSum.TrgSumHeader[i]);
for(i=0;i<  2;i++) printf("%15s[%3d] = %d\n","L1Sum",i,GraceSlick->TrgSum.L1Sum[i]);
for(i=0;i<  2;i++) printf("%15s[%3d] = %d\n","L2Sum",i,GraceSlick->TrgSum.L2Sum[i]);
                   printf("%15s    = %d\n","L0SumBytes",GraceSlick->TrgSum.L0SumBytes);
for(i=0;i<  2;i++) printf("%15s[%3d] = %d\n","L0SumHeader",i,GraceSlick->TrgSum.L0SumHeader[i]);
                   printf("%15s      = %d\n","L1SumBytes",GraceSlick->TrgSum.L1SumBytes);
for(i=0;i<  2;i++) printf("%15s[%3d] = %d\n","L1SumHeader",i,GraceSlick->TrgSum.L1SumHeader[i]);
for(i=0;i< 32;i++) printf("%15s[%3d] = %d\n","L1Result",i,GraceSlick->TrgSum.L1Result[i]);
                   printf("%15s      = %d\n","L2SumBytes",GraceSlick->TrgSum.L2SumBytes);
for(i=0;i<  2;i++) printf("%15s[%3d] = %d\n","L2SumHeader",i,GraceSlick->TrgSum.L2SumHeader[i]);
for(i=0;i< 32;i++) printf("%15s[%3d] = %d\n","L2Result",i,GraceSlick->TrgSum.L2Result[i]);
                   printf("%15s      = %d\n","RawDetBytes",GraceSlick->RAW[0].RawDetBytes);
for(i=0;i<  2;i++) printf("%15s[%3d] = %d\n","RawDetHeader",i,GraceSlick->RAW[0].RawDetHeader[i]);
                   printf("%15s      = %d\n","CTBdataBytes",GraceSlick->RAW[0].CTBdataBytes);
for(i=0;i<  2;i++) printf("%15s[%3d] = %d\n","CTBdataHeader",i,GraceSlick->RAW[0].CTBdataHeader[i]);
for(i=0;i<256;i++) printf("%15s[%3d] = %d\n","CTB",i,GraceSlick->RAW[0].CTB[i]);
                   printf("%15s      = %d\n","MWCdataBytes",GraceSlick->RAW[0].MWCdataBytes);
for(i=0;i<  2;i++) printf("%15s[%3d] = %d\n","MWCdataHeader",i,GraceSlick->RAW[0].MWCdataHeader[i]);
for(i=0;i<128;i++) printf("%15s[%3d] = %d\n","MWC",i,GraceSlick->RAW[0].MWC[i]);
                   printf("%15s      = %d\n","EMCdataBytes",GraceSlick->RAW[0].EMCdataBytes);
for(i=0;i<  2;i++) printf("%15s[%3d] = %d\n","EMCdataHeader",i,GraceSlick->RAW[0].EMCdataHeader[i]);
exit(2);
}
void St_trg_Maker::InitCtbArrays(void) {
  static int call=0;
  call++;
  assert(call<=1); /* No sense doing this more than once. */
  auxctbmap[ 0] = 104;
  auxctbmap[ 1] = 120;
  auxctbmap[ 2] = 136;
  auxctbmap[ 3] = 152;
  auxctbmap[ 4] = 168;
  auxctbmap[ 5] = 184;
  auxctbmap[ 6] = 200;
  auxctbmap[ 7] = 216;
  auxctbmap[ 8] = 232;
  auxctbmap[ 9] = 24;
  auxctbmap[10] = 248;
  auxctbmap[11] = 40;
  auxctbmap[12] = 56;
  auxctbmap[13] = 72;
  auxctbmap[14] = 8;
  auxctbmap[15] = 88;
  ctbmap[0][0] = 7;
  ctbmap[0][1] = 23;
  ctbmap[1][0] = 6;
  ctbmap[1][1] = 22;
  ctbmap[3-1][0] = 5;
  ctbmap[3-1][1] = 21;
  ctbmap[4-1][0] = 4;
  ctbmap[4-1][1] = 20;
  ctbmap[5-1][0] = 3;
  ctbmap[5-1][1] = 19;
  ctbmap[6-1][0] = 2;
  ctbmap[6-1][1] = 18;
  ctbmap[7-1][0] = 1;
  ctbmap[7-1][1] = 17;
  ctbmap[8-1][0] = 0;
  ctbmap[8-1][1] = 16;
  ctbmap[9-1][0] = 15;
  ctbmap[9-1][1] = 31;
  ctbmap[10-1][0] = 14;
  ctbmap[10-1][1] = 30;
  ctbmap[11-1][0] = 13;
  ctbmap[11-1][1] = 29;
  ctbmap[12-1][0] = 12;
  ctbmap[12-1][1] = 28;
  ctbmap[13-1][0] = 11;
  ctbmap[13-1][1] = 27;
  ctbmap[14-1][0] = 10;
  ctbmap[14-1][1] = 26;
  ctbmap[15-1][0] = 9;
  ctbmap[15-1][1] = 25;
  ctbmap[16-1][0] = 39;
  ctbmap[16-1][1] = 55;
  ctbmap[17-1][0] = 38;
  ctbmap[17-1][1] = 54;
  ctbmap[18-1][0] = 37;
  ctbmap[18-1][1] = 53;
  ctbmap[19-1][0] = 36;
  ctbmap[19-1][1] = 52;
  ctbmap[20-1][0] = 35;
  ctbmap[20-1][1] = 51;
  ctbmap[21-1][0] = 34;
  ctbmap[21-1][1] = 50;
  ctbmap[22-1][0] = 33;
  ctbmap[22-1][1] = 49;
  ctbmap[23-1][0] = 32;
  ctbmap[23-1][1] = 48;
  ctbmap[24-1][0] = 47;
  ctbmap[24-1][1] = 63;
  ctbmap[25-1][0] = 46;
  ctbmap[25-1][1] = 62;
  ctbmap[26-1][0] = 45;
  ctbmap[26-1][1] = 61;
  ctbmap[27-1][0] = 44;
  ctbmap[27-1][1] = 60;
  ctbmap[28-1][0] = 43;
  ctbmap[28-1][1] = 59;
  ctbmap[29-1][0] = 42;
  ctbmap[29-1][1] = 58;
  ctbmap[30-1][0] = 41;
  ctbmap[30-1][1] = 57;
  ctbmap[31-1][0] = 71;
  ctbmap[31-1][1] = 87;
  ctbmap[32-1][0] = 70;
  ctbmap[32-1][1] = 86;
  ctbmap[33-1][0] = 69;
  ctbmap[33-1][1] = 85;
  ctbmap[34-1][0] = 68;
  ctbmap[34-1][1] = 84;
  ctbmap[35-1][0] = 67;
  ctbmap[35-1][1] = 83;
  ctbmap[36-1][0] = 66;
  ctbmap[36-1][1] = 82;
  ctbmap[37-1][0] = 65;
  ctbmap[37-1][1] = 81;
  ctbmap[38-1][0] = 64;
  ctbmap[38-1][1] = 80;
  ctbmap[39-1][0] = 79;
  ctbmap[39-1][1] = 95;
  ctbmap[40-1][0] = 78;
  ctbmap[40-1][1] = 94;
  ctbmap[41-1][0] = 77;
  ctbmap[41-1][1] = 93;
  ctbmap[42-1][0] = 76;
  ctbmap[42-1][1] = 92;
  ctbmap[43-1][0] = 75;
  ctbmap[43-1][1] = 91;
  ctbmap[44-1][0] = 74;
  ctbmap[44-1][1] = 90;
  ctbmap[45-1][0] = 73;
  ctbmap[45-1][1] = 89;
  ctbmap[46-1][0] = 103;
  ctbmap[46-1][1] = 119;
  ctbmap[47-1][0] = 102;
  ctbmap[47-1][1] = 118;
  ctbmap[48-1][0] = 101;
  ctbmap[48-1][1] = 117;
  ctbmap[49-1][0] = 100;
  ctbmap[49-1][1] = 116;
  ctbmap[50-1][0] = 99;
  ctbmap[50-1][1] = 115;
  ctbmap[51-1][0] = 98;
  ctbmap[51-1][1] = 114;
  ctbmap[52-1][0] = 97;
  ctbmap[52-1][1] = 113;
  ctbmap[53-1][0] = 96;
  ctbmap[53-1][1] = 112;
  ctbmap[54-1][0] = 111;
  ctbmap[54-1][1] = 127;
  ctbmap[55-1][0] = 110;
  ctbmap[55-1][1] = 126;
  ctbmap[56-1][0] = 109;
  ctbmap[56-1][1] = 125;
  ctbmap[57-1][0] = 108;
  ctbmap[57-1][1] = 124;
  ctbmap[58-1][0] = 107;
  ctbmap[58-1][1] = 123;
  ctbmap[59-1][0] = 106;
  ctbmap[59-1][1] = 122;
  ctbmap[60-1][0] = 105;
  ctbmap[60-1][1] = 121;
  ctbmap[61-1][0] = 135;
  ctbmap[61-1][1] = 151;
  ctbmap[62-1][0] = 134;
  ctbmap[62-1][1] = 150;
  ctbmap[63-1][0] = 133;
  ctbmap[63-1][1] = 149;
  ctbmap[64-1][0] = 132;
  ctbmap[64-1][1] = 148;
  ctbmap[65-1][0] = 131;
  ctbmap[65-1][1] = 147;
  ctbmap[66-1][0] = 130;
  ctbmap[66-1][1] = 146;
  ctbmap[67-1][0] = 129;
  ctbmap[67-1][1] = 145;
  ctbmap[68-1][0] = 128;
  ctbmap[68-1][1] = 144;
  ctbmap[69-1][0] = 143;
  ctbmap[69-1][1] = 159;
  ctbmap[70-1][0] = 142;
  ctbmap[70-1][1] = 158;
  ctbmap[71-1][0] = 141;
  ctbmap[71-1][1] = 157;
  ctbmap[72-1][0] = 140;
  ctbmap[72-1][1] = 156;
  ctbmap[73-1][0] = 139;
  ctbmap[73-1][1] = 155;
  ctbmap[74-1][0] = 138;
  ctbmap[74-1][1] = 154;
  ctbmap[75-1][0] = 137;
  ctbmap[75-1][1] = 153;
  ctbmap[76-1][0] = 167;
  ctbmap[76-1][1] = 183;
  ctbmap[77-1][0] = 166;
  ctbmap[77-1][1] = 182;
  ctbmap[78-1][0] = 165;
  ctbmap[78-1][1] = 181;
  ctbmap[79-1][0] = 164;
  ctbmap[79-1][1] = 180;
  ctbmap[80-1][0] = 163;
  ctbmap[80-1][1] = 179;
  ctbmap[81-1][0] = 162;
  ctbmap[81-1][1] = 178;
  ctbmap[82-1][0] = 161;
  ctbmap[82-1][1] = 177;
  ctbmap[83-1][0] = 160;
  ctbmap[83-1][1] = 176;
  ctbmap[84-1][0] = 175;
  ctbmap[84-1][1] = 191;
  ctbmap[85-1][0] = 174;
  ctbmap[85-1][1] = 190;
  ctbmap[86-1][0] = 173;
  ctbmap[86-1][1] = 189;
  ctbmap[87-1][0] = 172;
  ctbmap[87-1][1] = 188;
  ctbmap[88-1][0] = 171;
  ctbmap[88-1][1] = 187;
  ctbmap[89-1][0] = 170;
  ctbmap[89-1][1] = 186;
  ctbmap[90-1][0] = 169;
  ctbmap[90-1][1] = 185;
  ctbmap[91-1][0] = 199;
  ctbmap[91-1][1] = 215;
  ctbmap[92-1][0] = 198;
  ctbmap[92-1][1] = 214;
  ctbmap[93-1][0] = 197;
  ctbmap[93-1][1] = 213;
  ctbmap[94-1][0] = 196;
  ctbmap[94-1][1] = 212;
  ctbmap[95-1][0] = 195;
  ctbmap[95-1][1] = 211;
  ctbmap[96-1][0] = 194;
  ctbmap[96-1][1] = 210;
  ctbmap[97-1][0] = 193;
  ctbmap[97-1][1] = 209;
  ctbmap[98-1][0] = 192;
  ctbmap[98-1][1] = 208;
  ctbmap[99-1][0] = 207;
  ctbmap[99-1][1] = 223;
  ctbmap[100-1][0] = 206;
  ctbmap[100-1][1] = 222;
  ctbmap[101-1][0] = 205;
  ctbmap[101-1][1] = 221;
  ctbmap[102-1][0] = 204;
  ctbmap[102-1][1] = 220;
  ctbmap[103-1][0] = 203;
  ctbmap[103-1][1] = 219;
  ctbmap[104-1][0] = 202;
  ctbmap[104-1][1] = 218;
  ctbmap[105-1][0] = 201;
  ctbmap[105-1][1] = 217;
  ctbmap[106-1][0] = 231;
  ctbmap[106-1][1] = 247;
  ctbmap[107-1][0] = 230;
  ctbmap[107-1][1] = 246;
  ctbmap[108-1][0] = 229;
  ctbmap[108-1][1] = 245;
  ctbmap[109-1][0] = 228;
  ctbmap[109-1][1] = 244;
  ctbmap[110-1][0] = 227;
  ctbmap[110-1][1] = 243;
  ctbmap[111-1][0] = 226;
  ctbmap[111-1][1] = 242;
  ctbmap[112-1][0] = 225;
  ctbmap[112-1][1] = 241;
  ctbmap[113-1][0] = 224;
  ctbmap[113-1][1] = 240;
  ctbmap[114-1][0] = 239;
  ctbmap[114-1][1] = 255;
  ctbmap[115-1][0] = 238;
  ctbmap[115-1][1] = 254;
  ctbmap[116-1][0] = 237;
  ctbmap[116-1][1] = 253;
  ctbmap[117-1][0] = 236;
  ctbmap[117-1][1] = 252;
  ctbmap[118-1][0] = 235;
  ctbmap[118-1][1] = 251;
  ctbmap[119-1][0] = 234;
  ctbmap[119-1][1] = 250;
  ctbmap[120-1][0] = 233;
  ctbmap[120-1][1] = 249;
}
void St_trg_Maker::InitMwcArrays(void) {
  static int call=0;
  call++;
  assert(call<=1); /* No sense doing this more than once. */
  mwcmap[ 1-1][0] =  71;
  mwcmap[ 1-1][1] =  70;
  mwcmap[ 1-1][2] =  69;
  mwcmap[ 1-1][3] =  68;
  mwcmap[ 2-1][0] =  67;
  mwcmap[ 2-1][1] =  66;
  mwcmap[ 2-1][2] =  65;
  mwcmap[ 2-1][3] =  64;
  mwcmap[ 3-1][0] =  79;
  mwcmap[ 3-1][1] =  78;
  mwcmap[ 3-1][2] =  77;
  mwcmap[ 3-1][3] =  76;
  mwcmap[ 4-1][0] =  95;
  mwcmap[ 4-1][1] =  94;
  mwcmap[ 4-1][2] =  93;
  mwcmap[ 4-1][3] =  92;
  mwcmap[ 5-1][0] =  87;
  mwcmap[ 5-1][1] =  86;
  mwcmap[ 5-1][2] =  85;
  mwcmap[ 5-1][3] =  84;
  mwcmap[ 6-1][0] =  83;
  mwcmap[ 6-1][1] =  82;
  mwcmap[ 6-1][2] =  81;
  mwcmap[ 6-1][3] =  80;
  mwcmap[ 7-1][0] =  99;
  mwcmap[ 7-1][1] =  98;
  mwcmap[ 7-1][2] =  97;
  mwcmap[ 7-1][3] =  96;
  mwcmap[ 8-1][0] = 111;
  mwcmap[ 8-1][1] = 110;
  mwcmap[ 8-1][2] = 109;
  mwcmap[ 8-1][3] = 108;
  mwcmap[ 9-1][0] = 103;
  mwcmap[ 9-1][1] = 102;
  mwcmap[ 9-1][2] = 101;
  mwcmap[ 9-1][3] = 100;
  mwcmap[10-1][0] = 119;
  mwcmap[10-1][1] = 118;
  mwcmap[10-1][2] = 117;
  mwcmap[10-1][3] = 116;
  mwcmap[11-1][0] = 115;
  mwcmap[11-1][1] = 114;
  mwcmap[11-1][2] = 113;
  mwcmap[11-1][3] = 112;
  mwcmap[12-1][0] = 127;
  mwcmap[12-1][1] = 126;
  mwcmap[12-1][2] = 125;
  mwcmap[12-1][3] = 124;
  mwcmap[13-1][0] =   7;
  mwcmap[13-1][1] =   6;
  mwcmap[13-1][2] =   5;
  mwcmap[13-1][3] =   4;
  mwcmap[14-1][0] =   3;
  mwcmap[14-1][1] =   2;
  mwcmap[14-1][2] =   1;
  mwcmap[14-1][3] =   0;
  mwcmap[15-1][0] =  15;
  mwcmap[15-1][1] =  14;
  mwcmap[15-1][2] =  13;
  mwcmap[15-1][3] =  12;
  mwcmap[16-1][0] =  31;
  mwcmap[16-1][1] =  30;
  mwcmap[16-1][2] =  29;
  mwcmap[16-1][3] =  28;
  mwcmap[17-1][0] =  23;
  mwcmap[17-1][1] =  22;
  mwcmap[17-1][2] =  21;
  mwcmap[17-1][3] =  20;
  mwcmap[18-1][0] =  19;
  mwcmap[18-1][1] =  18;
  mwcmap[18-1][2] =  17;
  mwcmap[18-1][3] =  16;
  mwcmap[19-1][0] =  35;
  mwcmap[19-1][1] =  34;
  mwcmap[19-1][2] =  33;
  mwcmap[19-1][3] =  32;
  mwcmap[20-1][0] =  47;
  mwcmap[20-1][1] =  46;
  mwcmap[20-1][2] =  45;
  mwcmap[20-1][3] =  44;
  mwcmap[21-1][0] =  39;
  mwcmap[21-1][1] =  38;
  mwcmap[21-1][2] =  37;
  mwcmap[21-1][3] =  36;
  mwcmap[22-1][0] =  55;
  mwcmap[22-1][1] =  54;
  mwcmap[22-1][2] =  53;
  mwcmap[22-1][3] =  52;
  mwcmap[23-1][0] =  51;
  mwcmap[23-1][1] =  50;
  mwcmap[23-1][2] =  49;
  mwcmap[23-1][3] =  48;
  mwcmap[24-1][0] =  63;
  mwcmap[24-1][1] =  62;
  mwcmap[24-1][2] =  61;
  mwcmap[24-1][3] =  60;
  auxmwcmap[ 0] =  75;
  auxmwcmap[ 1] =  74;
  auxmwcmap[ 2] =  73;
  auxmwcmap[ 3] =  72;
  auxmwcmap[ 4] =  91;
  auxmwcmap[ 5] =  90;
  auxmwcmap[ 6] =  89;
  auxmwcmap[ 7] =  88;
  auxmwcmap[ 8] = 107;
  auxmwcmap[ 9] = 106;
  auxmwcmap[10] = 105;
  auxmwcmap[11] = 104;
  auxmwcmap[12] = 123;
  auxmwcmap[13] = 122;
  auxmwcmap[14] = 121;
  auxmwcmap[15] = 120;
  auxmwcmap[16] =  11;
  auxmwcmap[17] =  10;
  auxmwcmap[18] =   9;
  auxmwcmap[19] =   8;
  auxmwcmap[20] =  27;
  auxmwcmap[21] =  26;
  auxmwcmap[22] =  25;
  auxmwcmap[23] =  24;
  auxmwcmap[24] =  43;
  auxmwcmap[25] =  42;
  auxmwcmap[26] =  41;
  auxmwcmap[27] =  40;
  auxmwcmap[28] =  59;
  auxmwcmap[29] =  58;
  auxmwcmap[30] =  57;
  auxmwcmap[31] =  56;
}
