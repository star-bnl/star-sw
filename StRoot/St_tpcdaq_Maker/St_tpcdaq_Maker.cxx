
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

//VP#include "StDaqLib/GENERIC/EventReader.hh"
#include "StTrsMaker/include/StTrsDetectorReader.hh"
#include "StTrsMaker/include/StTrsZeroSuppressedReader.hh"
#include "St_tpcdaq_Maker.h"
#include "TDataSetIter.h"
#include "TObjectSet.h"
#include "TH1.h"
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
#include "tables/St_daq100cl_Table.h"

ClassImp(St_tpcdaq_Maker)

#define PP printf
#define HISTOGRAMS

enum myNUMB { NSECT=24, NROW =45,DEBUG_ACTIVE_ROW =33,MAXPADROWSPERBANK=50};

#include "StDAQMaker/StDAQReader.h"
StDAQReader *victorPrelim=0;
StTPCReader *victor;
int gSector=-1;
enum myCORR {kGainCorr=1, kNoiseElim=2, kAsicTHold=4};
enum myDAQF {kPadRaw  =1, kDAQ100   =2              };

// #define DEVELOPMENT
//________________________________________________________________________________
int St_tpcdaq_Maker::GetCorrection(void) {
  return mCorrectionMask;
}
//________________________________________________________________________________
void St_tpcdaq_Maker::SetCorrection(int mask) {
  // bit 0  =   do GAIN_CORRECTION
  // bit 1  =   do NOISE_ELIM
  // bit 2  =   do ASIC_THRESHOLDS
  // Thus, the old mode (all three corrections) is mCorrection = 7 = bit 1 + bit 2 + bit 3.
  // To switch OFF all corrections use mask = 0.
  mCorrectionMask=mask;
}
//________________________________________________________________________________
St_tpcdaq_Maker::St_tpcdaq_Maker(const char *name,char *daqOrTrs):StMaker(name),gConfig(daqOrTrs)
{
  printf("This is St_tpcdaq_Maker, name = \"%s\".\n",name);
  alreadySet=0; // FALSE
  daq_flag = 1; // default value for DAQ Reading is to use pad_raw table filling
  mCorrectionMask = kGainCorr | kNoiseElim | kAsicTHold;
}
//________________________________________________________________________________
St_tpcdaq_Maker::~St_tpcdaq_Maker() {
}
//________________________________________________________________________________
Int_t St_tpcdaq_Maker::Init() {
  
  victorPrelim=0;

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
  return StMaker::Init();
}
//________________________________________________________________________________
Int_t St_tpcdaq_Maker::InitRun(Int_t RunNumber) {
  TDataSet *herb; 
  //int junk;
  if(mCorrectionMask&kNoiseElim) { SetNoiseEliminationStuff(); /*WriteStructToScreenAndExit();*/ }
  if(mCorrectionMask&kAsicTHold) {
    TDataSet *tpc_calib  = GetDataBase("Calibrations/tpc");
    assert(tpc_calib);
    St_asic_thresholds *asic = (St_asic_thresholds *) tpc_calib->Find("asic_thresholds");
    assert(asic);
    St_asic_thresholds &kasic = *asic;
    mThreshLo = kasic[0].thresh_lo;
    mThreshHi = kasic[0].thresh_hi;
    mNseqLo   = kasic[0].n_seq_lo;
    mNseqHi   = kasic[0].n_seq_hi;
  }

  if(m_Mode == 0 || m_Mode == 2) { // Update this for embedding.
    herb=GetDataSet("StDAQReader");
    assert(herb);
    victorPrelim=(StDAQReader*)(herb->GetObject()); assert(victorPrelim);
  } else if(m_Mode == 1) {// Trs
  } else {
    PP("-----------------------------------------------------------------\n");
    PP("The second argument of St_tpcdaq_Maker::St_tpcdaq_Maker() must be\n");
    PP("either \"daq\" or \"trs\".  Fatal error.  Please fix bfc.C.\n");
    assert(0); // Ctor called incorrectly. 
  }
  PP("end of St_tpcdaq_Maker::Init\n");
  return StMaker::InitRun(RunNumber);
}
//________________________________________________________________________________
char *St_tpcdaq_Maker::NameOfSector(int isect) {
  static char rv[16];
  sprintf(rv,"Sector_%i",isect);
  return rv;
}
//________________________________________________________________________________
void St_tpcdaq_Maker::MkTables(int isect,TDataSet *sector,
      St_raw_row **raw_row_in,St_raw_row **raw_row_out,
      St_raw_pad **raw_pad_in,St_raw_pad **raw_pad_out, 
      St_raw_seq **raw_seq_in,St_raw_seq **raw_seq_out,
      St_type_shortdata **pixel_data_in,St_type_shortdata **pixel_data_out,
      St_type_shortdata **pixel_indx_in,St_type_shortdata **pixel_indx_out) {

  TDataSetIter sect(sector);

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
  *pixel_indx_in=(St_type_shortdata*) sect("pixel_indx_in");
  if (!(*pixel_indx_in)) {
    *pixel_indx_in=new St_type_shortdata("pixel_indx_in",100);
    sect.Add(*pixel_indx_in);
  }

  *pixel_data_out=(St_type_shortdata*) sect("pixel_data_out");
  if (!(*pixel_data_out)) {
    *pixel_data_out=new St_type_shortdata("pixel_data_out",100);
    sect.Add(*pixel_data_out);
  }
  *pixel_indx_out=(St_type_shortdata*) sect("pixel_indx_out");
  if (!(*pixel_indx_out)) {
    *pixel_indx_out=new St_type_shortdata("pixel_indx_out",100);
    sect.Add(*pixel_indx_out);
  }
}
//________________________________________________________________________________
void St_tpcdaq_Maker::PadWrite(St_raw_pad *raw_pad_gen,int padR,int padOffset,
      int seqOffset,int nseq,int timeWhere,int pad)
{
  raw_pad_st singlerow;
  singlerow.PadOffset=padOffset;
  singlerow.SeqOffset=seqOffset;
  singlerow.nseq=nseq;
  singlerow.SeqModBreak=timeWhere;
  singlerow.PadId=pad;
  assert(raw_pad_gen->GetNRows()==padR);
  raw_pad_gen->AddAt(&singlerow);
}
//________________________________________________________________________________
inline void St_tpcdaq_Maker::PixelWrite(St_type_shortdata *pixel_data_gen,St_type_shortdata *pixel_indx_gen,
      int rownum,unsigned short datum, unsigned short id) 
{
  type_shortdata_st singlerow;
  singlerow.data=datum;
  type_shortdata_st singleid;
  singleid.data=id;
  assert(pixel_data_gen->GetNRows()==rownum);
  pixel_data_gen->AddAt(&singlerow);
  assert(pixel_indx_gen->GetNRows()==rownum);
  pixel_indx_gen->AddAt(&singleid);
}
//________________________________________________________________________________
void St_tpcdaq_Maker::SeqWrite(St_raw_seq *raw_seq_gen,int rownumber,
    int startTimeBin,int numberOfBinsInSequence) 
{
  raw_seq_st singlerow;
  if(startTimeBin>=0x100) mErr=1;
  singlerow.m=startTimeBin;
  singlerow.i=numberOfBinsInSequence-1;
  assert(raw_seq_gen->GetNRows()==rownumber);
  raw_seq_gen->AddAt(&singlerow);
}
//________________________________________________________________________________
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
  assert(raw_row_gen->GetNRows()==rownumber);
  raw_row_gen->AddAt(&singlerow);
}
//________________________________________________________________________________
int St_tpcdaq_Maker::getSector(Int_t isect) {
  int rv=0;
  if(m_Mode != 1) {    	// Use DAQ.
    gSector=isect;
  } else          {     // Use TRS.
    mZsr=mTdr->getZeroSuppressedReader(isect);
    if(!mZsr) rv=5; /* Either there are no hits for this sector, or there is an error. */
  }
  return rv; // 0 means there are hits and there is no error.
}
//________________________________________________________________________________
int St_tpcdaq_Maker::getPadList(int whichPadRow,unsigned char **padlist) {
  int rv=0; 
  if(m_Mode != 1) { // Use DAQ.
    // enforce recorded merge sequence flag
    victor->SetSequenceMerging(mMergeSequences);              
    rv=victor->getPadList(gSector,whichPadRow,*padlist);
    return rv;
  } else {           // Use TRS.
    assert(mZsr);
    rv=mZsr->getPadList(whichPadRow,padlist);
  }
  return rv;
}
//________________________________________________________________________________
void St_tpcdaq_Maker::AsicThresholds(float gain,int *nseqOld,StSequence **lst,UShort_t ***idt)
{
  static vector<StSequence, allocator<StSequence> > seqArr;
  static vector<UShort_t* , allocator<UShort_t* > > idtArr;
  if(mNseqLo<0) return; 	/* There is no asic.tpcdaq file. */
  seqArr.resize(0);idtArr.resize(0);

  for(int iseq=0;iseq<*nseqOld;iseq++) {
    StSequence seqNew; seqNew.length=0;
    StSequence *seqOld = (*lst)+iseq;
    int npix = seqOld->length;  
    int inSeq=0,numberAboveThresh=0;
    UShort_t *idtNew=0;
    for(int ipix=0;ipix<=npix;ipix++) {
      float conversion = (ipix<npix)? gain*seqOld->firstAdc[ipix]:0;
      int kase = inSeq;
      if(conversion> mThreshLo) kase |= 2;
      if(conversion> mThreshHi) kase |= 4;
      switch (kase) {
        case     0: break;

        case 0+2+0: ;
        case 4+2+0: inSeq=1;
        numberAboveThresh=0;
	seqNew.length=0; 
	seqNew.firstAdc=seqOld->firstAdc+ipix;
	seqNew.startTimeBin=seqOld->startTimeBin+ipix;
        if (*idt) idtNew = (*idt)[iseq]+ipix;
        case 0+2+1:;
        case 4+2+1:;
	seqNew.length++;
	if (kase&4) numberAboveThresh++;
        break;
	
        case 0+0+1: inSeq=0;
        if(seqNew.length<=mNseqLo) 	break;
        if(numberAboveThresh<=mNseqHi) 	break;
        seqArr.push_back(seqNew);
        if (*idt) idtArr.push_back(idtNew);
        break;
	
	default: assert(0);
      }//end case
    }//end pixels loop
  }//end seq loop
  *nseqOld=seqArr.size(); *lst=&seqArr[0];
  if (*idt) *idt = &idtArr[0];
}
//________________________________________________________________________________
int St_tpcdaq_Maker::getSequences(float gain,int row,int pad,int *nseq,StSequence **lst, UShort_t ***listOfIdt)
{
  if(listOfIdt)  *listOfIdt=0;
  int rv=0; TPCSequence *lstPrelim;
  if(m_Mode != 1) { // Use DAQ.
    rv=victor->getSequences(gSector,row,pad,*nseq,lstPrelim);
    *lst=(StSequence*)lstPrelim;
  } else {           // Use TRS.
    assert(sizeof(Sequence)==sizeof(StSequence));
    rv=mZsr->getSequences(row,pad,nseq,lst,listOfIdt);
  }
  if(mCorrectionMask&kAsicTHold) AsicThresholds(gain,nseq,lst,listOfIdt);
  return rv; // < 0 means serious error.
}

//________________________________________________________________________________
/// Method to set the DAQ Reading mode.
void St_tpcdaq_Maker::SetDAQFlag(Int_t mode)
{
  daq_flag = mode;
  cout << "St_tpcdaq_Maker::SetDAQFlag : Setting mandatory DAQ flag to ";
  if (daq_flag == 0){
    cout << " no table filling ";
  } else if (daq_flag & kPadRaw ){
    cout << " Using pad_raw ";
  } else if (daq_flag & kDAQ100 ){
    cout << " Using Clusters (DAQ100) ";
  } else {
    cout << " Unknown option" << daq_flag << endl;
    assert(0);
  }
  cout << endl;
}


#include "StDetectorDbMaker/StDetectorDbTpcRDOMasks.h"
#include "StDaqLib/TPC/fee_pin.h"
//________________________________________________________________________________
void St_tpcdaq_Maker::SetGainCorrectionStuff(int sector) {
  register int row,pad;

  TDataSet *tpc_calib  = GetDataBase("Calibrations/tpc"); assert(tpc_calib);

  St_tpcGain *gainObj = (St_tpcGain*) tpc_calib->Find("tpcGain"); assert(gainObj);

  assert(gainObj->GetNRows()==24);

  tpcGain_st *gains = gainObj->GetTable(); assert(gains);

  assert(sector>=1&&sector<=24);

  static StDetectorDbTpcRDOMasks* mask=0;
  static int tRDOFromRowAndPad[45][182];
  if(!mask) {
    mask = StDetectorDbTpcRDOMasks::instance();
    assert(mask);
    for(int tiFee=0;tiFee<182;tiFee++) {
      for(int tiPin=0;tiPin<32;tiPin++) {
        if(row_vs_fee[tiFee][tiPin]!=0 && pad_vs_fee[tiFee][tiPin]!=0) {
          tRDOFromRowAndPad[(row_vs_fee[tiFee][tiPin]-1)]
              [(pad_vs_fee[tiFee][tiPin]-1)]=
              rdo_vs_fee[tiFee][tiPin];
        }
      }
    }
  }

  for(row=0;row<45;row++) {
    for(pad=0;pad<182;pad++) {
      fGain[row][pad]=
          (mask->isOn(sector,tRDOFromRowAndPad[row][pad]))?
          gains[sector-1].Gain[row][pad] : -1.;
    }
  }
}
//________________________________________________________________________________
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

    if(!alreadySet) {
      noiseElim[sector].nbin=noise[sector].nbin;
      assert(noise[sector].nbin<=3); // Limit (3) is in both the St_tpcdaq_Maker.h and StDb/idl/noiseElim.idl.
      for(i=0;i<noise[sector].nbin;i++) {
        noiseElim[sector].low[i]=noise[sector].low[i];
        noiseElim[sector].up[i]=noise[sector].up[i];
      }
    }

  }
}
//________________________________________________________________________________
void St_tpcdaq_Maker::ExcludeTheseTimeBins(int lo1,int hi1,int lo2,int hi2,int lo3,int hi3) {
  int sector,nbin;
  assert(lo1<=hi1);
  assert(lo2<=hi2);
  assert(lo3<=hi3);
  alreadySet=7; // TRUE
  nbin=3;
  if(lo3<0||hi3<0) nbin=2;
  if(lo2<0||hi2<0) nbin=1;
  if(lo1<0||hi1<0) nbin=0;
  for(sector=0;sector<24;sector++) {
    noiseElim[sector].nbin=nbin;
    noiseElim[sector].low[0]=lo1;
    noiseElim[sector].up [0]=hi1;
    noiseElim[sector].low[1]=lo2;
    noiseElim[sector].up [1]=hi2;
    noiseElim[sector].low[2]=lo3;
    noiseElim[sector].up [2]=hi3;
  }
}
//________________________________________________________________________________
void St_tpcdaq_Maker::WriteStructToScreenAndExit() {
  int jj,ii;
  for(ii=0;ii<24;ii++) {
    PP("---------------------------------------------- sector %2d\n",ii+1);
    for(jj=0;jj<noiseElim[ii].nbin;jj++) {
      PP("Cut bins %3d to %3d.\n",noiseElim[ii].low[jj],noiseElim[ii].up[jj]);
    }
    for(jj=0;jj<noiseElim[ii].npad;jj++) {
      PP("Cut pad %3d of row %2d\n",noiseElim[ii].pad[jj],noiseElim[ii].row[jj]);
    }
  }
  assert(0); // This is in WriteStructToScreenAndExit().
}
///////////////////////////////////////
// comment 66f:  Here is the chain of function calls:
// TPCV2P0_ZS_SR::initialize rets 0 (error, corrupted event)
// TPCV2P0::getZeroSuppressedReader returns 0
// StTPCReader::setSector sets fZeroSuppressedReader to 0 (NULL)
// StTPCReader::getPadList returns -1
// St_tpcdaq_Maker::getPadList returns -1
// St_tpcdaq_Maker::Output returns 1
// St_tpcdaq_Maker::Maker returns kStErr
///////////////////////////////////////
//________________________________________________________________________________
int St_tpcdaq_Maker::Output() {
  char skip; int hj,lgg;
  int pixCnt=0;
  St_raw_row *raw_row_in,*raw_row_out,*raw_row_gen;
  St_raw_pad *raw_pad_in,*raw_pad_out,*raw_pad_gen;
  St_raw_seq *raw_seq_in,*raw_seq_out,*raw_seq_gen;
  St_type_shortdata *pixel_data_in,*pixel_data_out,*pixel_data_gen;
  St_type_shortdata *pixel_indx_in,*pixel_indx_out,*pixel_indx_gen;
  unsigned char *padlist;
  unsigned char *pointerToAdc;
  unsigned short conversion;
  char dataOuter[NSECT],dataInner[NSECT];
  TDataSet *sector;
  TDataSetIter raw_data_tpc(m_DataSet); //raw_data_tpc=iterator on St_tpcdaq_Maker's top level
  raw_sec_m_st singlerow;
  int pad=-1,sectorStatus,ipadrow,npad,ipad,seqStatus,iseq,nseq,startTimeBin,ibin;
  int numberOfUnskippedSeq,prevStartTimeBin,rowR,padR,seqR;  // row counters
  int iseqSave,pixTblWhere,seqLen,timeOff,numPadsWithSignal,pixOffset;
  int seqOffset,timeWhere;
  int nPixelThisPad,nSeqThisPadRow,offsetIntoPadTable;
  int nPixelPreviousPadRow;
  int isect,pixSave,pixR;
  unsigned long int nPixelThisPadRow;
  StSequence *listOfSequences;
  UShort_t       **listOfIdt = 0;
  St_raw_sec_m  *raw_sec_m;

  raw_sec_m = (St_raw_sec_m *) raw_data_tpc("raw_sec_m");
  if(!raw_sec_m) {
    raw_sec_m=new St_raw_sec_m("raw_sec_m",NSECT); raw_data_tpc.Add(raw_sec_m);
  }

  // See "DAQ to Offline", section "Better example - access by padrow,pad",
  // modifications thereto in Brian's email, SN325, and Iwona's SN325 expl.
  for(isect=1;isect<=NSECT;isect++) {
    if(mCorrectionMask&kGainCorr) SetGainCorrectionStuff(isect);
    dataOuter[isect-1]=0; dataInner[isect-1]=0;

    sector=raw_data_tpc(NameOfSector(isect));
    if(!sector) {
      raw_data_tpc.Mkdir(NameOfSector(isect));
      sector=raw_data_tpc(NameOfSector(isect));
    }
    MkTables(isect,sector,&raw_row_in,&raw_row_out,&raw_pad_in,&raw_pad_out, 
        &raw_seq_in,&raw_seq_out,&pixel_data_in,&pixel_data_out,&pixel_indx_in,&pixel_indx_out);
    sectorStatus=getSector(isect);
    if(sectorStatus) continue;
    raw_row_gen=raw_row_out; raw_pad_gen=raw_pad_out; rowR=0; padR=0;
    raw_seq_gen=raw_seq_out; pixel_data_gen=pixel_data_out; pixel_indx_gen=pixel_indx_out;seqR=0; pixR=0;
    nPixelPreviousPadRow=0;
    for(ipadrow=NROW-1;ipadrow>=0;ipadrow--) {
      if(ipadrow==12) { // switch to the inner part of this sector
        raw_row_gen=raw_row_in; raw_pad_gen=raw_pad_in; rowR=0; padR=0;
        raw_seq_gen=raw_seq_in; pixel_data_gen=pixel_data_in; pixel_indx_gen=pixel_indx_in; seqR=0; 
        pixR=0; nPixelPreviousPadRow=0;
      }
      pixSave=pixR; iseqSave=seqR; nPixelThisPadRow=0; nSeqThisPadRow=0;
      offsetIntoPadTable=padR; pixTblWhere=0; numPadsWithSignal=0;
      seqOffset=0; npad=getPadList(ipadrow+1,&padlist);

      if(victorPrelim&&m_Mode!=1) {
        if(!(victorPrelim->TPCPresent())) { assert(pixCnt==0); return 4321; /* TPC data missing in this event */ }
      }

      if(npad<0) return 1; // Corrupted event, see comment 66f.

      pixOffset=0;
      // printf("BBB isect=%d ,ipadrow=%d ,npad=%d \n",isect,ipadrow,npad);
      for( ipad=0 ; ipad<npad ; ipad++) {
        pad=padlist[ipad];
        if(mCorrectionMask&kNoiseElim) {
          skip=0;
          for(lgg=0;lgg<noiseElim[isect-1].npad;lgg++) {
            if(noiseElim[isect-1].row[lgg]==ipadrow+1&&noiseElim[isect-1].pad[lgg]==pad) { skip=7; break; }
          }
          if(skip) continue;
        }
        nPixelThisPad=0;
	Double_t gaincorrection = 1; // L3
	if(mCorrectionMask&kGainCorr) {
	  assert(pad>0&&pad<=182);
	  if (fGain[ipadrow][pad-1] < 0.125 || fGain[ipadrow][pad-1] > 8.0)  continue;
	  //          if (m_Mode == 2) gaincorrection = 1; // Trs
	  if (m_Mode == 0) gaincorrection = fGain[ipadrow][pad-1];
//yf (not ready yet to use uncorrected Trs)     if(mCorrectionMask&kGainCorr) gaincorrection = fGain[ipadrow][pad-1];
	}
        seqStatus=getSequences(gaincorrection,ipadrow+1,pad,&nseq,&listOfSequences,&listOfIdt);

        if(seqStatus<0) { Warning("Output","%d A",seqStatus); mErr=2; return 1; }
        if(!nseq) continue;

        numPadsWithSignal++; 
        if(ipadrow>=13) dataOuter[isect-1]=7; else dataInner[isect-1]=7;
        timeOff=0; timeWhere=0; prevStartTimeBin=-123;
#ifdef HISTOGRAMS
        m_pad_numSeq->Fill((Float_t)nseq);
#endif
        numberOfUnskippedSeq=0;
        for(iseq=0;iseq<nseq;iseq++) {
          startTimeBin=listOfSequences[iseq].startTimeBin;
          if(startTimeBin<  0) startTimeBin=  0;
          if(startTimeBin>511) startTimeBin=511;
          if(prevStartTimeBin>startTimeBin) { mErr=3; return 2; }
          prevStartTimeBin=startTimeBin; seqLen=listOfSequences[iseq].length;
          if(mCorrectionMask&kNoiseElim) {
            skip=0;
            for(lgg=0;lgg<noiseElim[isect-1].nbin;lgg++) {
              hj=startTimeBin;
              if(hj>=(noiseElim[isect-1].low[lgg])&&hj<=(noiseElim[isect-1].up[lgg])) { skip=7; break; }
              hj=startTimeBin+seqLen-1;
              if(hj>=(noiseElim[isect-1].low[lgg])&&hj<=(noiseElim[isect-1].up[lgg])) { skip=7; break; }
            }
            if(skip) continue; // Skip this sequence.
          }
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
	    UShort_t idt = (listOfIdt)? listOfIdt[iseq][ibin]:0; 
            if(mCorrectionMask&kGainCorr) {
              if(fGain[ipadrow][pad-1]>22.0) {
                printf("Fatal error in %s, line %d.\n",__FILE__,__LINE__);
                printf("ipadrow=%d, pad-1=%d, fgain=%g\n",ipadrow,pad-1,fGain[ipadrow][pad-1]);
                assert(0);
              }
              conversion=(short unsigned int)(0.5+gaincorrection*conversion);
            }
#ifdef HISTOGRAMS
            m_pix_AdcValue->Fill((double)(conversion));
#endif
            PixelWrite(pixel_data_gen,pixel_indx_gen,pixR++,conversion,idt);
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
//________________________________________________________________________________
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
  if(m_Mode != 1) return 0;
 TObjectSet *trsEvent=(TObjectSet*)GetDataSet("Event"); if(!trsEvent) return 1;
 mEvent=(StTpcRawDataEvent*)(trsEvent->GetObject());   if(!mEvent) return 3;
 mTdr = new StTrsDetectorReader(mEvent);
 return 0;
}
//________________________________________________________________________________
unsigned short int St_tpcdaq_Maker::Swap2(char doSwap,unsigned short int x) {
  char *hh,temp[2];
  if(!doSwap) return x;
  hh=(char*)(&x);
  temp[0]=hh[1]; temp[1]=hh[0];
  return *((unsigned short int*)temp);
}
//________________________________________________________________________________
unsigned int St_tpcdaq_Maker::Swap4(char doSwap,unsigned int x) {
  char *hh,temp[4];
  if(!doSwap) return x;
  hh=(char*)(&x);
  temp[0]=hh[3]; temp[1]=hh[2]; temp[2]=hh[1]; temp[3]=hh[0];
  return *((unsigned int*)temp);
}
//________________________________________________________________________________
char St_tpcdaq_Maker::WhetherToSwap(unsigned int x) {
  if(x==0x04030201) return 0;
  if(x==0x01020304) return 7;
  assert(0);
  return 0; // To silence Insure++.
}
//________________________________________________________________________________
void St_tpcdaq_Maker::DAQ100clTableOut(unsigned int receiverBoard,unsigned int mezzanine,
      unsigned int sectorCntsFrom1,char swap,
      const unsigned int *data) {
  unsigned int wordNumber[MAXPADROWSPERBANK],npadrow,ipadrow;
  unsigned int padrow,numClusters,icluster;
  unsigned int numberOfClustersInPreviousChunk,numberOfWordsInPreviousChunk;
  unsigned short int *pad,*time,*flag,*charge;
  static int totalRowCount=0;

  npadrow=Swap4(swap,*(data+0));
  if(npadrow==0) return;

  // Open the daq100cl root4star table for output in append mode.
  daq100cl_st singlerow;
  TDataSetIter rootIterator(m_DataSet);
  St_daq100cl *daq100cl = (St_daq100cl*) rootIterator("daq100cl");
  if(!daq100cl) {
    daq100cl=new St_daq100cl("daq100cl",1000);
    rootIterator.Add(daq100cl); totalRowCount=0;
  }

  // In preparation for intra-bank navigation, fill the array wordNumber[], which counts from 0.
  // This array tells the word number of the data at which the chunk for a given row starts.
  // See the discussion of TPCMZCLD in the DAQ format doc, at about page 17.
  wordNumber[0]=1;
  assert(npadrow<=MAXPADROWSPERBANK); // Protect against overfilling the array.
  for(ipadrow=1;ipadrow<npadrow;ipadrow++) {
    numberOfClustersInPreviousChunk = Swap4(swap,*(data+wordNumber[ipadrow-1]+1));
    numberOfWordsInPreviousChunk = 2 * numberOfClustersInPreviousChunk + 2;
    wordNumber[ipadrow] = wordNumber[ipadrow-1] + numberOfWordsInPreviousChunk;
  }

  // Now we have the array wordNumber[] and npadrow, which tells how many members the array has.
  for(ipadrow=1;ipadrow<npadrow;ipadrow++) {
    padrow=     Swap4(swap,*(data+wordNumber[ipadrow]  ));
    numClusters=Swap4(swap,*(data+wordNumber[ipadrow]+1));
    assert(numClusters<=6000); // Sanity check.  Number is from discusssion with Landgraf Oct 14.
    for(icluster=0;icluster<numClusters;icluster++) {
      pad=(unsigned short int*)(data+wordNumber[ipadrow]+2*icluster+2);
      time=pad+1; flag=pad+2; charge=pad+3;

      // At this point, we have all six of the columns ready for output to
      // the root4star table, so let's do it.
      singlerow.sector=sectorCntsFrom1; // Values of 1-24, not 0-23.
      singlerow.row=padrow;
      singlerow.pad=Swap2(swap,*pad);
      singlerow.timebucket=Swap2(swap,*time);
      singlerow.charge=Swap2(swap,*charge);
      singlerow.flag=Swap2(swap,*flag);
      singlerow.rb_mz=receiverBoard+100*mezzanine; // 0 < = rb_mz <= 211
      assert(daq100cl->GetNRows()==totalRowCount);
      daq100cl->AddAt(&singlerow);
      totalRowCount++;
    }
  }
}
//________________________________________________________________________________
void St_tpcdaq_Maker::PrepareSimulatedData(unsigned int sector,unsigned int *out) {
  static unsigned int cnt=0;
  unsigned short int *r;
  int ipadrow;

  out[0]=3; // 3 pad rows

  for(ipadrow=0;ipadrow<3;ipadrow++) {
    out[1+ipadrow*8]=sector+1; // row number
    out[2+ipadrow*8]=3;        // number of clusters this row
    r=(unsigned short*)&(out[3+ipadrow*8]); r[0]=(cnt++)%30; r[1]=sector+2; r[2]=123; r[3]=321;
    r=(unsigned short*)&(out[5+ipadrow*8]); r[0]=(cnt++)%30; r[1]=sector+2; r[2]=123; r[3]=321;
    r=(unsigned short*)&(out[7+ipadrow*8]); r[0]=(cnt++)%30; r[1]=sector+2; r[2]=123; r[3]=321;
  }
}
//________________________________________________________________________________
void St_tpcdaq_Maker::DAQ100clOutput(const unsigned int *pTPCP) {
  char swapTPCRBCLP,swapTPCSECLP,swapTPCSECP,swapTPCP,swapTPCMZCLD;
  const unsigned int *pTPCSECP,*pTPCSECLP,*pTPCRBCLP,*pTPCMZCLD;
  unsigned int sector,map,imz,irb,format,offset,length,isec,numberOfPresentSectors=0;
  assert(sizeof(unsigned int)==4); // Casting the pointer as uint* lets us avoid many *4's.
  assert(sizeof(unsigned short int)==2);
  swapTPCP=WhetherToSwap(*(pTPCP+5));

  // Loop over the sectors.  For some reason, in a standard DAQ file, 12 sectors are
  // missing, with each of the 12 present sectors representing two physical sectors.
  for(isec=0;isec<24;isec++) {

    // Navigate from the TPCP bank to the TPCSECP bank for this sector.
    offset=Swap4(swapTPCP,*(pTPCP+10+2*isec  ));
    length=Swap4(swapTPCP,*(pTPCP+10+2*isec+1));
    if(length==0) continue; // This sector is missing, as noted above.
    pTPCSECP=pTPCP+offset; assert(!strncmp((char*)pTPCSECP,"TPCSECP",7));
    swapTPCSECP=WhetherToSwap(*(pTPCSECP+5));

    // Navigate from TPCSECP to TPCSECLP, the first bank dedicated to online clusters.
#ifdef DEVELOPMENT
    unsigned int simulated[100];
    PrepareSimulatedData(isec+1,simulated);
    DAQ100clTableOut(isec+1,0,simulated); // Decodes data words of TPCMZCLD.
    continue;
#endif
    offset=Swap4(swapTPCSECP,*(pTPCSECP+8)); if(offset==0) continue;
    format=Swap4(swapTPCSECP,*(pTPCSECP+6)); if(format <2) continue;
    pTPCSECLP=pTPCSECP+offset; assert(!strncmp((char*)pTPCSECLP,"TPCSECLP",8));
    swapTPCSECLP=WhetherToSwap(*(pTPCSECLP+5));
    sector=Swap4(swapTPCSECLP,*(pTPCSECLP+3));

    // Loop over the 12 receiver boards, navigating from TPCSECLP to TPCRBCLP.
    for(irb=0;irb<12;irb++) {
      if(irb<6) map=0; else map=1;
      offset=Swap4(swapTPCSECLP,*(pTPCSECLP+10+2*irb  ));
      length=Swap4(swapTPCSECLP,*(pTPCSECLP+10+2*irb+1));
      if(length==0) continue;
      pTPCRBCLP=pTPCSECLP+offset; assert(!strncmp((char*)pTPCRBCLP,"TPCRBCLP",8));
      swapTPCRBCLP=WhetherToSwap(*(pTPCRBCLP+5));
      // Loop over the 3 mezzanine boards, navigating from TPCRBCLP to TPCMZCLD.
      for(imz=0;imz<3;imz++) {
        offset=Swap4(swapTPCRBCLP,*(pTPCRBCLP+10+2*imz  ));
        length=Swap4(swapTPCRBCLP,*(pTPCSECLP+10+2*imz+1));
        if(length==0) continue;
        pTPCMZCLD=pTPCRBCLP+offset; assert(!strncmp((char*)pTPCMZCLD,"TPCMZCLD",8));
        swapTPCMZCLD=WhetherToSwap(*(pTPCMZCLD+5));
        DAQ100clTableOut(irb,imz,
            sector+map,swapTPCMZCLD,pTPCMZCLD+10); // Decodes data words of TPCMZCLD.
      }
    }

    numberOfPresentSectors++;

  }
  // Not always 12 sectors assert(numberOfPresentSectors==12||numberOfPresentSectors==0); 
  // We expect 12 sectors
  // as described in the section on TPCSECLP on page 17 of the DAQ Format doc.
}
//________________________________________________________________________________
Int_t St_tpcdaq_Maker::Make() 
{
  int output,errorCode; const char *pTPCP;
  printf("St_tpcdaq_Maker::Make() method called in Mode=%d DAQ=%d\n",m_Mode,daq_flag); 
#ifdef DEVELOPMENT
  char junk[10];
  PP("\007St_tpcdaq_Maker::Make: Please input a value for daq_flag: ");
  gets(junk); daq_flag=atoi(junk); PP("daq_flag = %d.  Press return.\n",daq_flag);
#endif
  mErr=0;
  errorCode=GetEventAndDecoder();
  if(m_Mode != 1) {
    victor=victorPrelim->getTPCReader();
    if(!victor){
      PP("  getTPCReader() did not return any data. Continuing anyway\n");
      return kStOk; 
      // No TPC data,  Jerome says chain should continue.  
      // Herb Ward Apr 22 2003
    }
  }
  // printf("GetEventAndDecoder() = %d\n",errorCode);
  if(errorCode) {
    PP("Error: St_tpcdaq_Maker no event from TRS (%d).\n",errorCode);
    return kStErr;
  }
  assert(!m_DataSet->GetList());
  if(daq_flag & kPadRaw) { // Sometimes this causes us to skip the corruption check, but it's not im-
                        // portant because the data that would've gotten checked probably won't be used.
    output=Output();
    if(output==4321) printf("St_tpcdaq_Maker::Make() TPC data is missing this event, but do not skip event\n");
    else if( (output!=0) && (output!=4321) ) {
      PP("St_tpcdaq_Maker has detected .daq file corruption.  Skip this event.\n");
      return kStErr; // See comment 66f.
    }
  }
  if(daq_flag & kDAQ100) { // Herb Oct 2002 for DAQ100.
    if(m_Mode != 1) {
      pTPCP=(const char*)(victor->ptrTPCP); // StDaqLib's memory.  We have been set up as
         // a friend class to StDaqLib.  Let's be worthy of the trust and
         // use "const" to help us avoid modification of this borrowed memory.
      assert(strncmp(pTPCP,"TPCP",4)==0);
      DAQ100clOutput((const unsigned int*)pTPCP);
    }
  }
  if(mErr) {
    PP("St_tpcdaq_Maker failed with error code %d.\n",mErr);
    return kStFatal;
  } else {
    PP("Got through St_tpcdaq_Maker OK.\n");
  }
  return kStOK;
}


//________________________________________________________________________________
/*!
 *  Records the Sequence merging mode (and keep it once as this is a real
 *  callable maker unlike StDAQmaker() instantiated from StIOMaker() and
 *  not in the chain). Later use it for in DAQ mode (St_tpcdaq_Maker::getPadList)
 *  to enforce the same setting in StTPCReader via the similar SetSequenceMerging()
 *  method.
 */
//________________________________________________________________________________
char  St_tpcdaq_Maker::SetSequenceMerging(char mergeSequences)
{
  mMergeSequences=mergeSequences;
  (void) printf("St_tpcdaq_Maker::SetSequenceMerging: mMergeSequences is %s\n",(mMergeSequences?"ON":"OFF"));
  return mMergeSequences;
}






//  
// $Log: St_tpcdaq_Maker.cxx,v $
// Revision 1.93  2008/06/20 14:58:08  fisyak
// Change interal presentation for ADC from UChat_t to Short_t
//
// Revision 1.92  2008/04/24 20:54:00  fisyak
// Bug(1141) in logic of dead pads accounting
//
// Revision 1.91  2007/04/28 17:57:18  perev
// Redundant StChain.h removed
//
// Revision 1.90  2005/09/09 22:14:11  perev
// IdTruth added
//
// Revision 1.89  2005/07/19 22:26:23  perev
// Redundant assert removed
//
// Revision 1.88  2004/05/03 23:34:58  perev
// Possible non init WarnOff
//
// Revision 1.87  2004/03/31 19:34:14  jeromel
// Newline
//
// Revision 1.86  2004/03/15 23:54:46  jeromel
// Unused var removed (detail)
//
// Revision 1.85  2004/03/10 05:51:54  jeromel
// SetSequenceMerging() implementation (check comments) and note the
// victor->SetSequenceMerging() usage victor==StTPCReader though method
// is propagated there.
//
// Revision 1.84  2004/03/09 18:47:53  ward
// To silence Insure++.
//
// Revision 1.83  2004/01/27 23:38:14  jeromel
// Small change (more info in message)
//
// Revision 1.82  2004/01/05 16:51:29  ward
// Fix bug in ipadrow limits.
//
// Revision 1.81  2004/01/02 18:11:29  ward
// Change encoding of rb_mz.
//
// Revision 1.80  2004/01/02 17:53:18  ward
// Add receiver board and mezzanine to daq100cl output table.
//
// Revision 1.79  2004/01/02 17:05:46  ward
// Bug fixes from Jeff Landgraf after testing.
//
// Revision 1.78  2003/12/24 13:44:55  fisyak
// Add (GEANT) track Id information in Trs; propagate it via St_tpcdaq_Maker; account interface change in StTrsZeroSuppressedReaded in StMixerMaker
//
// Revision 1.77  2003/10/28 20:35:54  ward
// Chain control of NOISE_ELIM GAIN_CORRECTION ASIC_THRESHOLDS.
//
// Revision 1.76  2003/07/18 18:31:50  perev
// test for nonexistance of XXXReader added
//
// Revision 1.75  2003/07/10 19:41:00  ward
// Fix bug for running under TRS.
//
// Revision 1.74  2003/07/01 21:32:46  ward
// Do not skip event if TPC data is legitimately missing because of mixed triggers.
//
// Revision 1.73  2003/05/13 03:31:35  jeromel
// Minor message modif (by by Perry Mason)
//
// Revision 1.72  2003/04/22 20:12:44  ward
// So the chain can run when there is no TPC data.
//
// Revision 1.71  2002/10/18 20:08:18  didenko
// fixed flag
//
// Revision 1.70  2002/10/15 19:24:31  ward
// Stuff for pre-.daq file testing, and better handling of daq_flag.
//
// Revision 1.69  2002/10/13 20:43:38  ward
// Support for decoding DAQ100 data and writing it into a table.
//
// Revision 1.68  2002/10/11 18:10:53  jeromel
// Changes to accomodate for DAQ100 Cluster reading or raw hit reading
//
// Revision 1.67  2002/02/20 20:45:02  ward
// Implementation of RDO mask.  Mostly from Fabrice.
//
// Revision 1.66  2002/02/13 21:16:30  ward
// Move calibration access from Init() to InitRun().
//
// Revision 1.65  2001/05/17 20:16:02  ward
// Minor modification to allow running without gain corrections.
//
// Revision 1.64  2001/02/15 22:25:29  fisyak
// Add l3 option
//
// Revision 1.63  2001/02/13 18:36:24  fisyak
// Clean GAIN_LINE_SIZE
//
// Revision 1.62  2001/02/13 18:28:39  fisyak
// Step back with switching calibration
//
// Revision 1.60  2000/11/30 02:06:10  fisyak
// Remove scale factor 1.135, move it to calibration file
//
// Revision 1.59  2000/11/28 23:52:20  fisyak
// Add 1.135 factor to gain to make peack value = 1
//
// Revision 1.58  2000/11/25 23:15:03  fisyak
// Add cut for 0.125 < Gain < 8
//
// Revision 1.57  2000/11/07 16:30:29  ward
// New check for .daq corruption, with kStErr from St_tpcdaq_Maker.
//
// Revision 1.56  2000/06/26 18:25:20  ward
// mulitple veto zones for ExcludeTheseTimeBins
//
// Revision 1.55  2000/06/24 19:26:31  ward
// changed the name of the function to ExcludeTheseTimeBins
//
// Revision 1.54  2000/06/24 19:18:41  ward
// changed meaning of SetMinMaxTimeBucket args
//
// Revision 1.53  2000/06/24 19:13:27  ward
// added SetMinMaxTimeBucket(int lo,int hi) for Dave H.
//
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
