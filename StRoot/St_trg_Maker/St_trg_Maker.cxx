// $Id: St_trg_Maker.cxx,v 1.8 2000/02/04 18:57:17 ward Exp $
// $Log: St_trg_Maker.cxx,v $
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
#include "trg/St_trg_fillDst_Module.h"

#include "trgStructures.h" // From the STAR trigger group, and may need occasional updating.
// The structure MarilynMonroe_t is like the trigger group's TrgDataType, except that
// it does not include TrgEvtHeader, which they don't pass to DAQ.
typedef struct {
  EvtDescData    EvtDesc;  /* L1 Event Descriptor Data */  
  TrgSumData     TrgSum;   /* summary data */
  RawTrgDet      RAW;      /* For simplicity, I assume that you don't want pre and post history. */
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
  return StMaker::Init();
}
//_____________________________________________________________________________
void St_trg_Maker::SecondDstDaq(St_dst_L0_Trigger *dst2) {
  int i;
  dst_L0_Trigger_st *tt = dst2->GetTable();
  tt->TriggerActionWd  = GraceSlick->EvtDesc.TCU1.FIFO1.TrgActionWd;
  tt->TriggerWd        = GraceSlick->EvtDesc.TCU1.FIFO1.TrgToken;
  for(i=0;i<32;i++) tt->CPA[i]=GraceSlick->TrgSum.DSM.CPA[i];
  tt->MWC_CTB_mul      = GraceSlick->TrgSum.DSM.lastDSM[2]; // Per HANK CRAWFORD, Jan 6 2000.
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
void St_trg_Maker::CtbMwcDaq(St_dst_TrgDet *dst1) { // For real data, this takes the place of the trg_fillDst module.
  int i;
  dst_TrgDet_st *tt = dst1->GetTable();
  for(i=0;i<240;i++) tt->nCtb[i]=GraceSlick->RAW.CTB[i];
  for(i=0;i<240;i++) tt->timeCtb[i]=0; // bbb This may be settable in some event from the LS bit 
                                       //     of RAW.CTB. Hank is sending email to startrg-l.
  for(i=0;i< 96;i++) tt->nMwc[i]=GraceSlick->RAW.MWC[i];
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
  // bbb Like the rest of the sim stuff, this needs to be filled in.
}
int St_trg_Maker::Sim(St_dst_TrgDet *dst1,St_dst_L0_Trigger *dst2,St_dst_L1_Trigger *dst3,St_dst_L2_Trigger *dst4) {

  St_DataSet *ctf = GetInputDS("ctf");
  St_DataSet *mwc = GetInputDS("mwc");
  if (!ctf || !mwc) return kStWarn;
  St_ctu_cor   *ctu_cor  = (St_ctu_cor   *) ctf->Find("ctb_cor");
  St_mwc_raw   *mwc_raw  = (St_mwc_raw   *) mwc->Find("raw"    );
  if (!ctu_cor || !mwc_raw) return kStWarn;

  VpdSim(dst1); 
  ZdcSim(dst1);
  Int_t Res = trg_fillDst(ctu_cor,mwc_raw,dst1); if (Res != kSTAFCV_OK) return kStWarn;
  SecondDstSim(dst2);
  TakeCareOfL1andL2Sim(dst3,dst4);

  return kStOK;
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
    if(i<8) tt->adcZDC[i]=GraceSlick->TrgSum.DSM.ZDCDSM[i]; else tt->adcZDC[i]=0;
    tt->tdcZDC[i]=0;
  }
  tt->adcZDCEast=GraceSlick->TrgSum.DSM.ZDCDSM[3];
  tt->adcZDCWest=GraceSlick->TrgSum.DSM.ZDCDSM[7];
  tt->adcZDCsum=GraceSlick->TrgSum.DSM.ZDCDSM[3]+GraceSlick->TrgSum.DSM.ZDCDSM[7];
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
                   printf("%15s      = %d\n","RawDetBytes",GraceSlick->RAW.RawDetBytes);
for(i=0;i<  2;i++) printf("%15s[%3d] = %d\n","RawDetHeader",i,GraceSlick->RAW.RawDetHeader[i]);
                   printf("%15s      = %d\n","CTBdataBytes",GraceSlick->RAW.CTBdataBytes);
for(i=0;i<  2;i++) printf("%15s[%3d] = %d\n","CTBdataHeader",i,GraceSlick->RAW.CTBdataHeader[i]);
for(i=0;i<256;i++) printf("%15s[%3d] = %d\n","CTB",i,GraceSlick->RAW.CTB[i]);
                   printf("%15s      = %d\n","MWCdataBytes",GraceSlick->RAW.MWCdataBytes);
for(i=0;i<  2;i++) printf("%15s[%3d] = %d\n","MWCdataHeader",i,GraceSlick->RAW.MWCdataHeader[i]);
for(i=0;i<128;i++) printf("%15s[%3d] = %d\n","MWC",i,GraceSlick->RAW.MWC[i]);
                   printf("%15s      = %d\n","EMCdataBytes",GraceSlick->RAW.EMCdataBytes);
for(i=0;i<  2;i++) printf("%15s[%3d] = %d\n","EMCdataHeader",i,GraceSlick->RAW.EMCdataHeader[i]);
exit(2);
}
