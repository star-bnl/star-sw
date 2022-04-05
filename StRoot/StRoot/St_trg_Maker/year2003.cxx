#include "St_trg_Maker.h"
#include "StDaqLib/TRG/trgStructures2003.h"
#include "St_DataSetIter.h"
#include "StDAQMaker/StDAQReader.h"
#include "StDAQMaker/StTRGReader.h"
#include "tables/St_dst_L0_Trigger_Table.h"
#include "tables/St_dst_L1_Trigger_Table.h"
#include "tables/St_dst_L2_Trigger_Table.h"
#include "tables/St_dst_TrgDet_Table.h"
#include "tables/St_ctu_raw_Table.h"
#if 0
#include "tables/St_mwc_raw_Table.h"
#endif
#include "tables/St_dst_TrgDet_Table.h"
#define PREPOST 11

void St_trg_Maker::SecondDstDaq2003(St_dst_L0_Trigger *dst2) {
  int i;
  
  dst_L0_Trigger_st *tt = dst2->GetTable();
  tt->TrgToken         = mS2003->EvtDesc.TrgToken;
  tt->TriggerActionWd  = mActionWord;
  tt->DSMInput         = mS2003->EvtDesc.DSMInput;
  tt->DSMAddress       = mS2003->EvtDesc.DSMAddress;
  tt->TriggerWd        = mS2003->EvtDesc.TriggerWord;
  tt->DetectorBusy     = mS2003->EvtDesc.modifiedBusyStatus;
  tt->addBits          = mS2003->EvtDesc.addBits;
  for(i=0;i<32;i++) tt->CPA[i]=mS2003->TrgSum.DSMdata.CPA[i];
  tt->MWC_CTB_mul      = mS2003->TrgSum.DSMdata.lastDSM[2];
  tt->MWC_CTB_dipole   = 0;
  tt->MWC_CTB_topology = 0;
  tt->MWC_CTB_moment   = 0;
}

/// This method copies Ctb and Mwc trigger data to the dst_TrgDet_st table
void St_trg_Maker::CtbMwcDaq2003(St_dst_TrgDet *dst1) {
  int npre,npost,pp,tray,slat;
  dst_TrgDet_st *tt = dst1->GetTable();

  tt->bunchXing_lo=mS2003->EvtDesc.bunchXing_lo;
  tt->bunchXing_hi=(mS2003->EvtDesc.bunchXing_hi)&(0xFFFF);
  npre=mS2003->EvtDesc.npre;
  npost=mS2003->EvtDesc.npost;
  tt->npre=npre;
  tt->npost=npost;
  for(pp=0;pp<1+npre+npost;pp++) {
    assert(pp<PREPOST);
    // CTB information
    for(slat=0;slat<2;slat++) {
      for(tray=0;tray<120;tray++) {
        tt->nCtb[tray][slat][pp]=mS2003->rawTriggerDet[pp].CTB[ctbmap[tray][slat]];
        tt->timeCtb[tray][slat][pp]=0;
      }
    }
#if 0
    // MWC information 
    int i,subsector,sector;
    for(sector=0;sector<24;sector++) {
      for(subsector=0;subsector<4;subsector++) {
        tt->nMwc[sector][subsector][pp]=mS2003->rawTriggerDet[pp].MWC[mwcmap[sector][subsector]];
      }
    }
    for(i=0;i<16;i++) tt->ctbaux[i][pp]=mS2003->rawTriggerDet[pp].CTB[auxctbmap[i]];
    for(i=0;i<32;i++) tt->mwcaux[i][pp]=mS2003->rawTriggerDet[pp].MWC[auxmwcmap[i]];
#endif
  }
}

Int_t St_trg_Maker::SanityCheck2003() {
  unsigned short x;
  x=mS2003->TrgSum.L1SumBytes; assert(x==0x0084||x==0x8400);
  x=mS2003->TrgSum.L2SumBytes; assert(x==0x0084||x==0x8400);
  return kStOK;
}

///////////////////////////// Here begins the stuff from Jennifer Klay.  Oct 16 2001.
#ifdef LATER_THAN_YEAR_2000  // Year 2000 trgStructures.h does not have BEMC.
#define BYTESPERDSM 16  //The number of output bytes the 20 signals are packed into
typedef unsigned char byte;

int tower_map[10] = {0,1,2,3,4,5,6,7,8,9};  // Corrected per Jenn Klay in her email of Fri, 22 Feb 2002.
int swapByteOrder[16] = {7,6,5,4,3,2,1,0,15,14,13,12,11,10,9,8};

//dsm_to_patch follows the numbering scheme in the offline software, since
//it fills a table that is used in offline.  The numbers correspond to the 
//numbering of the EMC Crates with respect to the Tower ID's from Jose
//Riso, which seems to count backward.  i.e. Patch 1 contains Tower 4800,
//while Patch 300 contains Tower 1.		J.L. Klay 18-Feb-2002
////////
int AA_dsm_to_patch[5] = {0,0,15,16,17};  //AuAu 2001
int pp_dsm_to_patch[7] = {0,0,15,28,16,29,17};  //pp 2001-02: (After 1-Dec-2001)
////////
//See below - timestamp check determines which one to use

void St_trg_Maker::Emc2003(St_dst_TrgDet *dst1) {
  dst_TrgDet_st *tt = dst1->GetTable();

  int NUMDSM;

  //Check timestamp of events to decide which mapping to use
  //pp_dsm_to_patch[7] for after 01-Dec-2001, AA_dsm_to_patch[5] for before that date
  EventReader *er=fVictorPrelim->getEventReader();
  EventInfo info=er->getEventInfo();
  unsigned int UnixTime=info.UnixTime;
  struct tm *time=gmtime((time_t*) &UnixTime);
  int year=1900+time->tm_year;
  int month=1+time->tm_mon;
  int day=time->tm_mday;
  char text1[80];
  sprintf(text1,"%04d%02d%02d",year,month,day);
  unsigned int date=atoi(text1);
  if (date < 20011201) {	//Before 01-Dec-2001
    NUMDSM = 5;
  } else {
    NUMDSM = 7;
  }   
  byte dsmval[NUMDSM][BYTESPERDSM];
  byte val[BYTESPERDSM];
  byte hi_tower,tower_sum;
  int i,j,k,index,nt,patch,sort;

  for (i=0; i<NUMDSM ; i++) {		//Fill dsm info, including L1 & L2
    for (j=0; j < BYTESPERDSM; j++) {
      index = (i*16) + j;
      dsmval[i][j] = mS2003->rawTriggerDet[0].BEMC[index];
//      if (mS2003->rawTriggerDet[0].BEMC[index]!=0)
//	printf("dsmval[%2d][%2d]= %2d",i,j,dsmval[i][j]);
    }
  }

  for (i=2; i<NUMDSM; i++) {  //Loop through L0 DSMs, skipping L1 and L2
    //Convert the DSM output byte ordering to EMC software ordering
    if (date < 20011201) {
      patch = AA_dsm_to_patch[i];	//AuAu run had 5 boards
    } else {
      patch = pp_dsm_to_patch[i];	//pp run had 7 boards
    }
    for (j=0; j<BYTESPERDSM; j++) {
      k = swapByteOrder[j];
      val[k] = dsmval[i][j];
    }
    nt = 0;  //tower counter withing DSM - 10 max
    //Unpack the bits into 20 signals and store in table
    for (index=0; index<5; index++) {
      k=index*3;

      hi_tower = (val[k] & 0x3f);
      tower_sum = ((val[k]>>6) & 0x3) + ((val[k+1] & 0xF)<<2);

      sort = tower_map[nt] + 10*patch;
      assert(sort>=0&&sort<300); // The limit 300 is in the idl file for dst_TrgDet.
      tt->emcHiTower[sort] = hi_tower;
      tt->emcTrigPatch[sort] = tower_sum;
      //printf("emcHiTower[%2d] = %2d, ",sort,hi_tower);
      //printf("emcTowerSum[%2d] = %2d\n",sort,tower_sum);
      nt += 1;

      hi_tower = ((val[k+1]>>4) & 0xF) + ((val[k+2] & 0x3)<<4);
      tower_sum = ((val[k+2]>>2) & 0x3F);

      sort = tower_map[nt] + 10*patch;
      assert(sort>=0&&sort<300);
      tt->emcHiTower[sort] = hi_tower;   
      tt->emcTrigPatch[sort] = tower_sum;
      nt += 1;
      //printf("emcHiTower[%2d] = %2d, ",sort,hi_tower);
      //printf("emcTowerSum[%2d] = %2d\n",sort,tower_sum);
    }
  }
}
#endif // LATER_THAN_YEAR_2000
///////////////////////////// Here ends the stuff from Jennifer Klay.  Oct 16 2001.

// comment 77y:  "action word has 9 as first hex digit and last bit lit, and trigger word
// is 0xf200" from Bill Love (email Nov 9 2001).

int St_trg_Maker::Daq2003(St_DataSet *herb,St_dst_TrgDet *dst1,St_dst_L0_Trigger *dst2,
			  St_dst_L1_Trigger *dst3,St_dst_L2_Trigger *dst4) {

  char *oo,*ptr,isLaser=0,isPhysics=0,isPulser=0,thisEventOk=0;
  fVictorPrelim=(StDAQReader*)(herb->GetObject()); assert(fVictorPrelim);
  fVictor=fVictorPrelim->getTRGReader(); assert(fVictor);

  // No TRG bank in .daq file.  Perhaps a pedestal or laser run?
  assert(fVictor->thereIsTriggerData()); 

  // StTRGReader *St_trg_Maker::fVictor;
  // TRG_Reader  *StTRGReader::fTRGImpReader;
  // Bank_TRGD   *TRG_Reader::pBankTRGD;

  ptr=(char*)(fVictor->getData());
  assert(ptr);
  mS2003=(TrgDataType2003*)ptr;
  mActionWord=
    ( (unsigned short)(mS2003->EvtDesc.actionWdTrgCommand) * 16 * 16 * 16 ) +
    ( (unsigned short)(mS2003->EvtDesc.actionWdDaqCommand) * 16 * 16      ) +
    (                  mS2003->EvtDesc.actionWdDetectorBitMask & 0x00ff   );
  // Int_t Iret = SanityCheck2003();
  // if (Iret !=  kStOK) {
  //   printf("St_trg_Maker:: Daq2003 : failed L1/L2 summary sanity check.\n"); 
  //   return Iret;
  // }

  // printf("St_trg_Maker:: Daq2003 : passed L1/L2 summary sanity check.\n"); 
  LOG_INFO << Form("Daq2003 : ActionWrdCommand is 0x%0x TriggerWord 0x%0x",
	 mS2003->EvtDesc.actionWdTrgCommand,
	 mS2003->EvtDesc.TriggerWord) << endm;


  if(  (((mActionWord)&0xf000)==0x9000 ) &&
       (((mActionWord)&0x0001)==0x0001 ) //&&
       //( mS2003->EvtDesc.TriggerWord          ==0xf200 )
  ) isLaser=7; 

  // See comment 77y above.
  if(mActionWord>>12==4)                  isPhysics=7;
  if(mS2003->EvtDesc.TriggerWord==0xf101) isPulser=7;

  if((m_Mode  )==0)         thisEventOk=7;
  if((m_Mode&1)&&isPhysics) thisEventOk=7;
  if((m_Mode&2)&&isLaser)   thisEventOk=7;
  if((m_Mode&4)&&isPulser)  thisEventOk=7;


  oo = "unknown";
  if(isPhysics) oo="Physics"; 
  if(isLaser)   oo="Laser"; 
  if(isPulser)  oo="Pulser";

  LOG_INFO <<Form("Daq2003 : %s event.  TrgActionWd=0x%x.  TriggerWd=0x%0x. Returning %s. m_Mode=%d.\n",
	 oo,mActionWord,
	 mS2003->EvtDesc.TriggerWord,
	 thisEventOk?"kStOK":"kStErr",
	 m_Mode) << endm;

  if (!thisEventOk) return kStErr; // Skip this event.

  // dumpDataToScreenAndExit();
  VpdDaq2003(dst1);                // The function
  ZdcDaq2003(dst1);                // St_trg_Maker::Sim
  BbcDaq2003(dst1);                //   <--- this will be in as a hack
  CtbMwcDaq2003(dst1);             // has four lines
  SecondDstDaq2003(dst2);          // which are analogous to these four.
  TakeCareOfL1andL2Daq2003(dst3,dst4);
#ifdef LATER_THAN_YEAR_2000        // Year 2000 trgStructures.h does not have BEMC.
  Emc2003(dst1);
#endif

  // Request for a clear message
  //if(Debug() > 0)
  cout << "St_trg_Maker:: Daq2003 : Event has been accepted" << endl;
  return kStOK;
}


void St_trg_Maker::TakeCareOfL1andL2Daq2003(St_dst_L1_Trigger *dst3,St_dst_L2_Trigger *dst4) {
  int i;
  dst_L1_Trigger_st *tt1 = dst3->GetTable();
  dst_L2_Trigger_st *tt2 = dst4->GetTable();
  for(i=0;i<32;i++) {
    tt1->L1_result[i] = mS2003->TrgSum.L1Result[i];
    tt2->L2_result[i] = mS2003->TrgSum.L2Result[i];
  }
}
void St_trg_Maker::VpdDaq2003(St_dst_TrgDet *dst1) {
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
void St_trg_Maker::ZdcDaq2003(St_dst_TrgDet *dst1) {
  int i;
  dst_TrgDet_st *tt = dst1->GetTable();
  for(i=0;i<8;i++) tt->lastDSM[i]=mS2003->TrgSum.DSMdata.lastDSM[i];
  for(i=0;i<16;i++) { 
    tt->adcZDC[i]=mS2003->rawTriggerDet[0].ZDC[i];
    tt->tdcZDC[i]=0;
    tt->BCdata[i]=mS2003->TrgSum.DSMdata.BCdata[i];
  }
  tt->adcZDCEast=mS2003->rawTriggerDet[0].ZDC[13];
  tt->adcZDCWest=mS2003->rawTriggerDet[0].ZDC[10];
  tt->adcZDCsum=mS2003->rawTriggerDet[0].ZDC[13]+mS2003->rawTriggerDet[0].ZDC[10];
}


/// Following this layout of one routine per trigger to recover
void St_trg_Maker::BbcDaq2003(St_dst_TrgDet *dst1) {
  int i;
  dst_TrgDet_st *tt = dst1->GetTable();
  for(i=0; i < 80 ;i++) { 
    tt->BBC[i] = (unsigned short) mS2003->rawTriggerDet[0].BBC[i];
    //printf("DEBUG >>> %2.2d %d %c\n",i,tt->BBC[i],tt->BBC[i]);
  }
}

void St_trg_Maker::dumpDataToScreenAndExit2003() {
  //int i;
  exit(2);
}


