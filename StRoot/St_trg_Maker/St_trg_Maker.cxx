//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_trg_Maker class for Makers                                        //
// CVS history moved at the bottom                                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "Stiostream.h"
#include "St_trg_Maker.h"
#include "St_DataSetIter.h"
#include "StDAQMaker/StDAQReader.h"
#include "StDAQMaker/StTRGReader.h"
#include "StDAQMaker/StSSDReader.h"
#include "tables/St_dst_L0_Trigger_Table.h" // 24dec99
#include "tables/St_dst_L1_Trigger_Table.h" // 02feb00
#include "tables/St_dst_L2_Trigger_Table.h" // 02feb00
#include "tables/St_dst_TrgDet_Table.h"     // 24dec99
#include "tables/St_ctu_raw_Table.h"
#if 0
#include "tables/St_mwc_raw_Table.h"
#endif
#include "tables/St_dst_TrgDet_Table.h"
#define PREPOST 11 // CAUTION:  this number is also in dst_TrgDet.idl

#include "StDaqLib/TRG/trgStructures.h"
typedef struct {
  EvtDescData    EvtDesc;           /* L1 Event Descriptor Data */  
  TrgSumData     TrgSum;            /* summary data */
  RawTrgDet      RAW[PREPOST];      /* For simplicity, I assume that you don't 
				       want pre and post history. */
} MarilynMonroe;
MarilynMonroe *gs;


#include "StDaqLib/TRG/trgStructures2000.h"
typedef struct {
  EvtDescData2000    EvtDesc;       /* L1 Event Descriptor Data */  
  TrgSumData2000     TrgSum;        /* summary data */
  RawTrgDet2000      RAW[PREPOST];  /* For simplicity, I assume that you don't 
				       want pre and post history. */
} MarilynMonroe2000;
MarilynMonroe2000 *gs2000;



ClassImp(St_trg_Maker)
#define SANITYCheck(name,value) \
 if (name != value) {LOG_INFO << "Value of "#name" = |" << name << "| instead of expected "#value << endm; return  kStErr;}


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
#if 0
  InitMwcArrays();
#endif
  return StMaker::Init();
}

////////////////////////////////////////////////////////////////////////////////////////
// This section of code causes the functions in the file duplicated.code to appear twice
// in this code file.  The second time is for trigger data in the year 2000 format.
// The first time is for year 2001 format.

#define SWITCH(x) x
#define LATER_THAN_YEAR_2000
#include "St_trg_duplicated.code"
#undef SWITCH
#undef LATER_THAN_YEAR_2000

#define SWITCH(x) x ## 2000
#include "St_trg_duplicated.code"

////////////////////////////////////////////////////////////////////////////////////////
void St_trg_Maker::SecondDstSim(St_dst_L0_Trigger *dst2) 
{
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

/// Decode pBankTRGD and return the value of the year associated to the dataset
int St_trg_Maker::YearOfData(St_DataSet *herb) 
{
  StDAQReader *fromVictor = (StDAQReader*) (herb->GetObject()); assert(fromVictor);
  StTRGReader *trgReader  = fromVictor->getTRGReader();if(!trgReader) return 0;
  if(!trgReader->thereIsTriggerData()) return 0;
  int y = trgReader->getYear();
  assert(y);
  return y;
  
}

/*!

  Get the DataSet associated with the DAQReader. Based on the decoded
  year, returns  St_trg_Maker::Daq2000 St_trg_Maker::Daq (for year 2001)
  both in duplicated.code (beware of macro replacement SWITCH() ) or 
  St_trg_Maker::Daq2003 located in year2003.cxx .

*/
Int_t St_trg_Maker::Make(){
  static char initializationDone=0;
  St_dst_TrgDet     *dst1 = new St_dst_TrgDet("TrgDet",1);         if(!dst1) return kStWarn; dst1->SetNRows(1);
  St_dst_L0_Trigger *dst2 = new St_dst_L0_Trigger("L0_Trigger",1); if(!dst2) return kStWarn; dst2->SetNRows(1);
  St_dst_L1_Trigger *dst3 = new St_dst_L1_Trigger("L1_Trigger",1); if(!dst3) return kStWarn; dst3->SetNRows(1);
  St_dst_L2_Trigger *dst4 = new St_dst_L2_Trigger("L2_Trigger",1); if(!dst4) return kStWarn; dst4->SetNRows(1);
  AddData(dst1);
  AddData(dst2);
  AddData(dst3);
  AddData(dst4);

  /////////////////// EXAMPLE:  HOW TO GET THE EEMC DATA /////////////////////////
  // St_DataSet *daq = GetDataSet("StDAQReader");                 assert(daq);
  // StDAQReader *fromVictor = (StDAQReader*) (daq->GetObject()); assert(fromVictor);
  // StEEMCReader *steemcreader  = fromVictor->getEEMCReader();   assert(steemcreader);
  // for(int crate=3;crate<=5;crate++) { for(int chan=0;chan<128;chan++) {
  //    printf("EEMC crate=%3d chan=%3d ADC=%5d\n",crate,chan,steemcreader->getTowerAdc(crate,chan));
  // } }
  /////////////////// END OF EXAMPLE:  HOW TO GET THE EEMC DATA /////////////////////////

  ////////////////////////  EXAMPLE:  HOW TO GET SSD DATA  //////////////////////
  // {
  //   static int event=0;
  //   int data,pedestal,noise,channel,ladder; char eastWest;
  //   St_DataSet *daq = GetDataSet("StDAQReader");                 assert(daq);
  //   StDAQReader *fromVictor = (StDAQReader*) (daq->GetObject()); assert(fromVictor);
  //   StSSDReader *stssdreader  = fromVictor->getSSDReader();   assert(stssdreader);
  //   
  //   ladder=13; eastWest='W'; channel=68; // ladder numbers from 1, channel numbers from 0.
  //   if(stssdreader->getSsdData(ladder,eastWest,channel,data,pedestal,noise)==0) {
  //     printf("ssd physics data\n");
  //   } else {
  //     printf("ssd  pedestal data\n");
  //   }
  //   event++;
  //   printf("ssd event %2d, %02d%c, chan %05d, data  %03d\n",event,ladder,eastWest,channel,data);
  //   printf("ssd event %2d, %02d%c, chan %05d, ped   %03d\n",event,ladder,eastWest,channel,pedestal);
  //   printf("ssd event %2d, %02d%c, chan %05d, noise %03d\n",event,ladder,eastWest,channel,noise);
  // }
  //////////////////////  END OF SSD EXAMPLE  //////////////////////////////////////////

  // This just sort of prints out some representative SSS data for use as a baseline during development.
  // static int call=0;
  // int ped,noise,channel,data,debug1,debugt,ladder; char ew;
  // St_DataSet *daq = GetDataSet("StDAQReader");                 assert(daq);
  // StDAQReader *fromVictor = (StDAQReader*) (daq->GetObject()); assert(fromVictor);
  // StSSDReader *stssdreader  = fromVictor->getSSDReader();   assert(stssdreader);
  // call++;
  // for(debug1=0;debug1<20;debug1++) {
  //   debugt=0;
  //   switch(debug1) {
  //     case  0: ladder=13; ew='W'; break;
  //     case  1: ladder=12; ew='W'; break;
  //     case  2: ladder=10; ew='W'; break;
  //     case  3: ladder=11; ew='W'; break;
  //     case  4: ladder= 9; ew='W'; break;
  //     case  5: ladder=19; ew='W'; break;
  //     case  6: ladder=20; ew='W'; break;
  //     case  7: ladder= 2; ew='W'; break;
  //     case  8: ladder= 1; ew='W'; break;
  //     case  9: ladder= 3; ew='W'; break;
  //     case 10: ladder=13; ew='E'; break;
  //     case 11: ladder=12; ew='E'; break;
  //     case 12: ladder=10; ew='E'; break;
  //     case 13: ladder=11; ew='E'; break;
  //     case 14: ladder= 9; ew='E'; break;
  //     case 15: ladder=19; ew='E'; break;
  //     case 16: ladder=20; ew='E'; break;
  //     case 17: ladder= 2; ew='E'; break;
  //     case 18: ladder= 1; ew='E'; break;
  //     case 19: ladder= 3; ew='E'; break;
  //   }
  //   for(channel=0;channel<12288;channel++) {
  //     assert(stssdreader->getSsdData(ladder,ew,channel,data,ped,noise)==0);
  //     if(data&&debugt==0) { debugt=1; printf("BBB e_trg_=%02d lad=%02d%c channel=%05d data=%03d\n",call+1,ladder,ew,channel,data); }
  //   }
  // }

  

  St_DataSet *DAQdset = GetDataSet("StDAQReader");
  if(DAQdset) {
    //
    // Starting from 2006+, see StEvent and StEventMaker filling of
    // L0, l1, L2 from StTriggerData structure. 
    // StL0Trigger::set(const StTriggerData* t)
    //
    int yy=YearOfData(DAQdset);
    if (yy >= 2007 ){
      LOG_INFO << "trg is obsolete for years " << yy << ">= 2007" << endm;
      return kStOk;
    }

    switch(yy) {
      case 2000:
        LOG_INFO << "analyzing year 2000 trigger data." << endm;
        if(!initializationDone) { 
	  InitCtbArrays(); initializationDone=7; 
	}
        return Daq2000(DAQdset,dst1,dst2,dst3,dst4);
        break;

      case 2001:
        LOG_INFO << "analyzing year 2001 trigger data" << endm;
        if(!initializationDone) { 
	  InitCtbArrays2001(); 
	  initializationDone=7; 
	}
        return Daq(DAQdset,dst1,dst2,dst3,dst4);
        break;

      case 2003:
        LOG_INFO << "analyzing year 2003 trigger data"<< endm;
        if(!initializationDone) { 
	  InitCtbArrays2001(); /* use 2001 for 2003 */ 
	  initializationDone=7; 
	}
        return Daq2003(DAQdset,dst1,dst2,dst3,dst4);
        break;


      case 2004:
      case 2005:
        LOG_INFO <<  "analyzing year 2004/2005 trigger data (using 2003)" << endm;
        if(!initializationDone) { 
	  InitCtbArrays2001();                        /* use 2001 for 2004-2005  */ 
	  initializationDone=7; 
	}
        return Daq2003(DAQdset,dst1,dst2,dst3,dst4);  /* use same scheme as 2003 */
        break;

      case 0: 
	return kStWarn;

      default: 
	assert(0);
    }

  } else {
    if(!initializationDone) { InitCtbArrays2001(); initializationDone=7; }
    return Sim(dst1,dst2,dst3,dst4);
  }

  return kStOk; // To hush Insure++.

}


void St_trg_Maker::TakeCareOfL1andL2Sim(St_dst_L1_Trigger *dst3,St_dst_L2_Trigger *dst4) {
  // Like the rest of the sim stuff, this needs to be filled in.
  LOG_INFO << "we are in TakeCareOfL1andL2Sim" << endm ;
}


int St_trg_Maker::HandleCtu(St_ctu_raw *ctu_raw,St_dst_TrgDet *dst1) {
  if ( !dst1 ) {
     LOG_INFO << "dst1 not found" << endm ;
     return 0 ;
  }  
  dst_TrgDet_st *tt = 0 ;
  tt = dst1->GetTable();
  if ( !tt ) {
     LOG_INFO << "dst1 table not found" << endm ;
     return 0 ;
  }
  //
  // No pre and post crossing data in simulations
  // at least for the moment (PY 12/1/00).
  tt->npre = 0 ;
  tt->npost= 0;

  if ( !ctu_raw ) {
     LOG_INFO << "ctu_raw table not found" << endm ;
     return 0 ;
  }
  ctu_raw_st* ctbRaw = 0 ;
  ctbRaw = ctu_raw->GetTable();
  if ( !ctbRaw ) {
     LOG_INFO <<  "ctbRaw table not found" << endm ;
     return 0 ;
  }
  
  int tray = 0 ;
  int slat = 0 ;
  for ( int i = 0 ; i < ctu_raw->GetNRows() ; i++ ) {
//   printf ( "ieta iphi adc %d %d %d \n", ctbRaw[i].i_eta, ctbRaw[i].i_phi,
//                                         ctbRaw[i].adc ) ;

     if ( ctbRaw[i].i_eta < 3 ) {
        tray = getTrayCtb ( float(6. * (ctbRaw[i].i_phi-1)), -100. )  ;
	slat = 2 - ctbRaw[i].i_eta ;
     }
     else {
        tray = getTrayCtb ( float(6. * (ctbRaw[i].i_phi-1)),  100. )  ;
        slat = ctbRaw[i].i_eta - 2 ;
     }
     tt->nCtb[tray][slat][0]     = ctbRaw[i].adc ;
     tt->timeCtb[tray][slat][0] = 0;  
  }
  return 7;
}
//
//
int St_trg_Maker::getTrayCtb ( float phi, float z ) {
   float phiShift, phiShifted ;
   int iphi, iPhi ;

   if ( z > 0 ) {
      phiShift = 84  ;
      phiShifted = (phi - phiShift) ;
      if ( phiShifted < 0 ) phiShifted += 360 ;
      iphi = int(phiShifted / 6) ;
      iPhi = 59 - iphi ;
      if ( iPhi == 0 ) iPhi = 60 ;
      return iPhi ;
   }
   else {
      phiShift = 112  ;
      phiShifted = (phi - phiShift) ;
      if ( phiShifted < 0 ) phiShifted += 360 ;
      iphi = int(phiShifted / 6) ;
      iPhi = 62 + iphi ;
      if ( iPhi == 121 ) iPhi = 61 ;
      return iPhi ;
   }

}
void St_trg_Maker::Vladimir2Herbert(int input,int *sector,int *subsector) {
  int offset, flip;
  assert(input>=1&&input<=96);
  if(input>48) {
    input-=48; offset=0; flip = -1;
  } else if(45<=input&&input<=48) {
    *sector=24; *subsector=input-44; return;
  } else {
    offset=24; flip = 1;
  }
  *sector = (input-1)/4 + 1 ;
  *subsector = 4 + input - 4 * (*sector) ;
  *sector = flip*(offset - *sector);
}
#if 0
int St_trg_Maker::HandleMwc(St_mwc_raw *mwc_raw,St_dst_TrgDet *dst1) {
  int prePost,sector,subsector,index,irow;
  if(!mwc_raw) { LOG_INFO << "Did not find the mwc_raw table mwc."<< endm; return 7; }
  mwc_raw_st    *vladimir = mwc_raw->GetTable(); assert(vladimir);
  dst_TrgDet_st *herbert  = dst1->GetTable();    assert(herbert);
  herbert->npre=5; herbert->npost=5;
  assert(mwc_raw->GetNRows()==96*11); // We must have correct number of rows, else loops below will crash.
  for(irow=0;irow<96;irow++) {
    for(prePost=0;prePost<11;prePost++) {
      index=vladimir[96*prePost+irow].sector; // index = 1-48 for z<0,  49-98 for z>0.  Opposite STAR convention.
      Vladimir2Herbert(index,&sector,&subsector);
      assert(sector>=1&&sector<=24);
      assert(subsector>=1&&subsector<=4);
      herbert[0].nMwc[sector-1][subsector-1][prePost]=vladimir[96*prePost+irow].count;
    }
  }
  return 7;
}
#endif
int St_trg_Maker::Sim(St_dst_TrgDet *dst1,St_dst_L0_Trigger *dst2,St_dst_L1_Trigger *dst3,St_dst_L2_Trigger *dst4) {
  int rv=kStOK;

  St_DataSet *ctf = GetInputDS(".make/ctf/.data");
#if 0
  St_DataSet *mwc = GetInputDS(".make/mwc/.data");
  if (!ctf || !mwc) return kStWarn;
#else
  if (!ctf ) return kStWarn;
#endif
  St_ctu_raw   *ctu_raw  = (St_ctu_raw   *) ctf->Find("ctb_raw");
#if 0
  St_mwc_raw   *mwc_raw  = (St_mwc_raw   *) mwc->Find("raw");
#endif
//
  VpdSim(dst1); 
  ZdcSim(dst1);


#if 0
  if(!HandleMwc(mwc_raw,dst1)) rv=kStWarn;
#endif
  if(!HandleCtu(ctu_raw,dst1)) rv=kStWarn;
  SecondDstSim(dst2);
  TakeCareOfL1andL2Sim(dst3,dst4);

  return rv;
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
void St_trg_Maker::InitCtbArrays2001(void) { // from ctb_dsm_2001.map, see also ::InitCtbArrays().
static int call=0;
  call++;
  assert(call<=1); /* No sense doing this more than once. */
  auxctbmap[ 0]=104;
  auxctbmap[ 1]=8;
  auxctbmap[ 2]=40;
  auxctbmap[ 3]=72;
  auxctbmap[ 4]=120;
  auxctbmap[ 5]=24;
  auxctbmap[ 6]=56;
  auxctbmap[ 7]=88;
  auxctbmap[ 8]=136;
  auxctbmap[ 9]=168;
  auxctbmap[10]=200;
  auxctbmap[11]=232;
  auxctbmap[12]=152;
  auxctbmap[13]=184;
  auxctbmap[14]=216;
  auxctbmap[15]=248;
  ctbmap[1-1][1-1]=109;
  ctbmap[2-1][1-1]=108;
  ctbmap[3-1][1-1]=107;
  ctbmap[4-1][1-1]=106;
  ctbmap[5-1][1-1]=105;
  ctbmap[6-1][1-1]=7;
  ctbmap[7-1][1-1]=6;
  ctbmap[8-1][1-1]=5;
  ctbmap[9-1][1-1]=4;
  ctbmap[10-1][1-1]=3;
  ctbmap[11-1][1-1]=2;
  ctbmap[12-1][1-1]=1;
  ctbmap[13-1][1-1]=0;
  ctbmap[14-1][1-1]=15;
  ctbmap[15-1][1-1]=14;
  ctbmap[16-1][1-1]=13;
  ctbmap[17-1][1-1]=12;
  ctbmap[18-1][1-1]=11;
  ctbmap[19-1][1-1]=10;
  ctbmap[20-1][1-1]=9;
  ctbmap[21-1][1-1]=39;
  ctbmap[22-1][1-1]=38;
  ctbmap[23-1][1-1]=37;
  ctbmap[24-1][1-1]=36;
  ctbmap[25-1][1-1]=35;
  ctbmap[26-1][1-1]=34;
  ctbmap[27-1][1-1]=33;
  ctbmap[28-1][1-1]=32;
  ctbmap[29-1][1-1]=47;
  ctbmap[30-1][1-1]=46;
  ctbmap[31-1][1-1]=45;
  ctbmap[32-1][1-1]=44;
  ctbmap[33-1][1-1]=43;
  ctbmap[34-1][1-1]=42;
  ctbmap[35-1][1-1]=41;
  ctbmap[36-1][1-1]=71;
  ctbmap[37-1][1-1]=70;
  ctbmap[38-1][1-1]=69;
  ctbmap[39-1][1-1]=68;
  ctbmap[40-1][1-1]=67;
  ctbmap[41-1][1-1]=66;
  ctbmap[42-1][1-1]=65;
  ctbmap[43-1][1-1]=64;
  ctbmap[44-1][1-1]=79;
  ctbmap[45-1][1-1]=78;
  ctbmap[46-1][1-1]=77;
  ctbmap[47-1][1-1]=76;
  ctbmap[48-1][1-1]=75;
  ctbmap[49-1][1-1]=74;
  ctbmap[50-1][1-1]=73;
  ctbmap[51-1][1-1]=103;
  ctbmap[52-1][1-1]=102;
  ctbmap[53-1][1-1]=101;
  ctbmap[54-1][1-1]=100;
  ctbmap[55-1][1-1]=99;
  ctbmap[56-1][1-1]=98;
  ctbmap[57-1][1-1]=97;
  ctbmap[58-1][1-1]=96;
  ctbmap[59-1][1-1]=111;
  ctbmap[60-1][1-1]=110;
  ctbmap[1-1][2-1]=125;
  ctbmap[2-1][2-1]=124;
  ctbmap[3-1][2-1]=123;
  ctbmap[4-1][2-1]=122;
  ctbmap[5-1][2-1]=121;
  ctbmap[6-1][2-1]=23;
  ctbmap[7-1][2-1]=22;
  ctbmap[8-1][2-1]=21;
  ctbmap[9-1][2-1]=20;
  ctbmap[10-1][2-1]=19;
  ctbmap[11-1][2-1]=18;
  ctbmap[12-1][2-1]=17;
  ctbmap[13-1][2-1]=16;
  ctbmap[14-1][2-1]=31;
  ctbmap[15-1][2-1]=30;
  ctbmap[16-1][2-1]=29;
  ctbmap[17-1][2-1]=28;
  ctbmap[18-1][2-1]=27;
  ctbmap[19-1][2-1]=26;
  ctbmap[20-1][2-1]=25;
  ctbmap[21-1][2-1]=55;
  ctbmap[22-1][2-1]=54;
  ctbmap[23-1][2-1]=53;
  ctbmap[24-1][2-1]=52;
  ctbmap[25-1][2-1]=51;
  ctbmap[26-1][2-1]=50;
  ctbmap[27-1][2-1]=49;
  ctbmap[28-1][2-1]=48;
  ctbmap[29-1][2-1]=63;
  ctbmap[30-1][2-1]=62;
  ctbmap[31-1][2-1]=61;
  ctbmap[32-1][2-1]=60;
  ctbmap[33-1][2-1]=59;
  ctbmap[34-1][2-1]=58;
  ctbmap[35-1][2-1]=57;
  ctbmap[36-1][2-1]=87;
  ctbmap[37-1][2-1]=86;
  ctbmap[38-1][2-1]=85;
  ctbmap[39-1][2-1]=84;
  ctbmap[40-1][2-1]=83;
  ctbmap[41-1][2-1]=82;
  ctbmap[42-1][2-1]=81;
  ctbmap[43-1][2-1]=80;
  ctbmap[44-1][2-1]=95;
  ctbmap[45-1][2-1]=94;
  ctbmap[46-1][2-1]=93;
  ctbmap[47-1][2-1]=92;
  ctbmap[48-1][2-1]=91;
  ctbmap[49-1][2-1]=90;
  ctbmap[50-1][2-1]=89;
  ctbmap[51-1][2-1]=119;
  ctbmap[52-1][2-1]=118;
  ctbmap[53-1][2-1]=117;
  ctbmap[54-1][2-1]=116;
  ctbmap[55-1][2-1]=115;
  ctbmap[56-1][2-1]=114;
  ctbmap[57-1][2-1]=113;
  ctbmap[58-1][2-1]=112;
  ctbmap[59-1][2-1]=127;
  ctbmap[60-1][2-1]=126;
  ctbmap[61-1][1-1]=141;
  ctbmap[62-1][1-1]=140;
  ctbmap[63-1][1-1]=139;
  ctbmap[64-1][1-1]=138;
  ctbmap[65-1][1-1]=137;
  ctbmap[66-1][1-1]=167;
  ctbmap[67-1][1-1]=166;
  ctbmap[68-1][1-1]=165;
  ctbmap[69-1][1-1]=164;
  ctbmap[70-1][1-1]=163;
  ctbmap[71-1][1-1]=162;
  ctbmap[72-1][1-1]=161;
  ctbmap[73-1][1-1]=160;
  ctbmap[74-1][1-1]=175;
  ctbmap[75-1][1-1]=174;
  ctbmap[76-1][1-1]=173;
  ctbmap[77-1][1-1]=172;
  ctbmap[78-1][1-1]=171;
  ctbmap[79-1][1-1]=170;
  ctbmap[80-1][1-1]=169;
  ctbmap[81-1][1-1]=199;
  ctbmap[82-1][1-1]=198;
  ctbmap[83-1][1-1]=197;
  ctbmap[84-1][1-1]=196;
  ctbmap[85-1][1-1]=195;
  ctbmap[86-1][1-1]=194;
  ctbmap[87-1][1-1]=193;
  ctbmap[88-1][1-1]=192;
  ctbmap[89-1][1-1]=207;
  ctbmap[90-1][1-1]=206;
  ctbmap[91-1][1-1]=205;
  ctbmap[92-1][1-1]=204;
  ctbmap[93-1][1-1]=203;
  ctbmap[94-1][1-1]=202;
  ctbmap[95-1][1-1]=201;
  ctbmap[96-1][1-1]=231;
  ctbmap[97-1][1-1]=230;
  ctbmap[98-1][1-1]=229;
  ctbmap[99-1][1-1]=228;
  ctbmap[100-1][1-1]=227;
  ctbmap[101-1][1-1]=226;
  ctbmap[102-1][1-1]=225;
  ctbmap[103-1][1-1]=224;
  ctbmap[104-1][1-1]=239;
  ctbmap[105-1][1-1]=238;
  ctbmap[106-1][1-1]=237;
  ctbmap[107-1][1-1]=236;
  ctbmap[108-1][1-1]=235;
  ctbmap[109-1][1-1]=234;
  ctbmap[110-1][1-1]=233;
  ctbmap[111-1][1-1]=135;
  ctbmap[112-1][1-1]=134;
  ctbmap[113-1][1-1]=133;
  ctbmap[114-1][1-1]=132;
  ctbmap[115-1][1-1]=131;
  ctbmap[116-1][1-1]=130;
  ctbmap[117-1][1-1]=129;
  ctbmap[118-1][1-1]=128;
  ctbmap[119-1][1-1]=143;
  ctbmap[120-1][1-1]=142;
  ctbmap[61-1][2-1]=157;
  ctbmap[62-1][2-1]=156;
  ctbmap[63-1][2-1]=155;
  ctbmap[64-1][2-1]=154;
  ctbmap[65-1][2-1]=153;
  ctbmap[66-1][2-1]=183;
  ctbmap[67-1][2-1]=182;
  ctbmap[68-1][2-1]=181;
  ctbmap[69-1][2-1]=180;
  ctbmap[70-1][2-1]=179;
  ctbmap[71-1][2-1]=178;
  ctbmap[72-1][2-1]=177;
  ctbmap[73-1][2-1]=176;
  ctbmap[74-1][2-1]=191;
  ctbmap[75-1][2-1]=190;
  ctbmap[76-1][2-1]=189;
  ctbmap[77-1][2-1]=188;
  ctbmap[78-1][2-1]=187;
  ctbmap[79-1][2-1]=186;
  ctbmap[80-1][2-1]=185;
  ctbmap[81-1][2-1]=215;
  ctbmap[82-1][2-1]=214;
  ctbmap[83-1][2-1]=213;
  ctbmap[84-1][2-1]=212;
  ctbmap[85-1][2-1]=211;
  ctbmap[86-1][2-1]=210;
  ctbmap[87-1][2-1]=209;
  ctbmap[88-1][2-1]=208;
  ctbmap[89-1][2-1]=223;
  ctbmap[90-1][2-1]=222;
  ctbmap[91-1][2-1]=221;
  ctbmap[92-1][2-1]=220;
  ctbmap[93-1][2-1]=219;
  ctbmap[94-1][2-1]=218;
  ctbmap[95-1][2-1]=217;
  ctbmap[96-1][2-1]=247;
  ctbmap[97-1][2-1]=246;
  ctbmap[98-1][2-1]=245;
  ctbmap[99-1][2-1]=244;
  ctbmap[100-1][2-1]=243;
  ctbmap[101-1][2-1]=242;
  ctbmap[102-1][2-1]=241;
  ctbmap[103-1][2-1]=240;
  ctbmap[104-1][2-1]=255;
  ctbmap[105-1][2-1]=254;
  ctbmap[106-1][2-1]=253;
  ctbmap[107-1][2-1]=252;
  ctbmap[108-1][2-1]=251;
  ctbmap[109-1][2-1]=250;
  ctbmap[110-1][2-1]=249;
  ctbmap[111-1][2-1]=151;
  ctbmap[112-1][2-1]=150;
  ctbmap[113-1][2-1]=149;
  ctbmap[114-1][2-1]=148;
  ctbmap[115-1][2-1]=147;
  ctbmap[116-1][2-1]=146;
  ctbmap[117-1][2-1]=145;
  ctbmap[118-1][2-1]=144;
  ctbmap[119-1][2-1]=159;
  ctbmap[120-1][2-1]=158;
}
void St_trg_Maker::InitCtbArrays(void) { // from ctb_dsm.map, see also ::InitCtbArrays2001().
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
#if 0
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
#endif



// $Id: St_trg_Maker.cxx,v 1.58 2017/04/26 21:05:26 perev Exp $
// $Log: St_trg_Maker.cxx,v $
// Revision 1.58  2017/04/26 21:05:26  perev
// Hide m_DataSet
//
// Revision 1.57  2009/01/26 15:14:13  fisyak
// Comment out mwc
//
// Revision 1.56  2007/06/06 12:26:53  fine
// Switch to STAR Logger
//
// Revision 1.55  2007/04/28 17:57:26  perev
// Redundant StChain.h removed
//
// Revision 1.54  2007/03/23 17:28:56  jeromel
// One more small modif (print year)
//
// Revision 1.52  2005/01/07 15:26:54  jeromel
// Assume same decoding 2005=2004 .
//
// Revision 1.51  2004/08/07 03:02:20  perev
// corruption test added
//
// Revision 1.50  2004/03/09 18:38:43  ward
// Take care of Insure warning.
//
// Revision 1.49  2004/03/09 18:19:06  ward
// Added pedestal and noise capability to the SSD reader.
//
// Revision 1.48  2004/02/18 20:17:49  ward
// Access SSD data in makers.
//
// Revision 1.47  2004/01/03 03:32:26  jeromel
// Temptative 2003-like approach
//
// Revision 1.46  2003/10/06 04:07:20  perev
// bug in duplicated.code fixed and this file renamed
//
// Revision 1.45  2003/09/02 17:59:33  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.44  2003/07/16 19:58:35  perev
// Cleanup of StTriggerData2003 at all
//
// Revision 1.43  2003/03/24 18:12:16  ward
// Full support for EEMC from Herbert Ward.
//
// Revision 1.42  2003/01/21 04:41:29  jeromel
// float to int fix
//
// Revision 1.41  2003/01/21 01:31:57  jeromel
// Minor changes in messaging (small request from Janet) to bring some consistency.
// doxygenized the .h
//
// Revision 1.40  2003/01/16 13:32:32  ward
// Accomodation of the new trgStructures.h.
//
// Revision 1.37  2002/04/09 00:01:37  ward
// Bug fix in Vladimir2Herbert().
//
// Revision 1.36  2002/02/19 18:34:44  ward
// Changes from Jenn Klay: EMC unpacker rewritten, updated dsm-to-patch conversion to match offline software.
//
// Revision 1.35  2001/12/04 18:24:13  jeromel
// Small modif of return value to make Insure happy.
//
// Revision 1.34  2001/10/16 20:26:02  ward
// New code from Jennifer Klay for unpacking EMC data.
//
// Revision 1.33  2001/09/11 21:49:46  ward
// Changes to StDaqLib/TRG for running year 2000 data.
//
// Revision 1.32  2001/09/04 20:06:33  ward
// I had a typo in the previous correction.
//
// Revision 1.31  2001/09/04 20:02:49  ward
// Correction in ctb_dsm_2001.map copied into maker code.
//
// Revision 1.30  2001/09/03 19:09:39  ward
// Runtime selection of 2000 or 2001 trigger data format.
//
// Revision 1.29  2001/08/22 15:16:30  ward
// Changed the laser flag from x9001 to x9009.
//
// Revision 1.28  2001/08/15 17:12:18  ward
// m_Mode third bit means pass pulser events.
//
// Revision 1.27  2001/07/25 19:10:53  ward
// New function InitCtbArrays2001 for ctb_dsm_2001.map.
//
// Revision 1.26  2001/07/18 20:12:15  ward
// New trigger information for DST tables.
//
// Revision 1.25  2001/07/12 17:40:34  ward
// New version of trgStructures.h, and modifications to other code in support thereof.
//
// Revision 1.24  2001/01/18 16:55:05  ward
// Corrections in Vladimir2Herbert from Vladimir Morozov.
//
// Revision 1.23  2001/01/16 20:27:53  ward
// npre=5 npost=5 instead of 0 in St_trg_Maker::HandleMwc
//
// Revision 1.22  2001/01/10 18:12:12  ward
// MWC corrections from Vladimir Morozov.
//
// Revision 1.21  2001/01/02 18:10:44  ward
// Pablo Yepes' modifications in support of CTU simulations.
//
// Revision 1.20  2000/10/02 19:41:54  ward
// Added DSMInput and DetectorBusy to trigger DST output (dst_L0_Trigger).
//
// Revision 1.19  2000/08/19 19:51:55  ward
// Change physics mask from 0x2XXX to 0x4XXX.
//
// Revision 1.18  2000/08/16 01:28:40  ward
// Report TrgActionWd as diagnostic for oo.
//
// Revision 1.17  2000/08/15 19:41:32  ward
// Configuration via m_mode of which event types are skipped.
//
// Revision 1.16  2000/08/13 18:32:59  ward
// Returns kStErr for laser events.
//
// Revision 1.15  2000/07/27 18:06:18  ward
// Put TriggerWd into DST instead of TrgToken (for Jan Belewski).
//
// Revision 1.14  2000/07/19 22:03:43  ward
// Switch from non-attenuated to attenuated ZDC.
//
// Revision 1.13  2000/07/13 18:48:23  ward
// Fixed ZDC indices according to email from Javier.
//
// Revision 1.12  2000/06/25 23:51:03  fisyak
// Replace assert by return of kStErr
//
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
