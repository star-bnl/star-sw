// $Id: StChargeStepMaker.cxx,v 1.1 2000/07/12 18:43:11 hardtke Exp $
// $Log: StChargeStepMaker.cxx,v $
// Revision 1.1  2000/07/12 18:43:11  hardtke
// initial version -- very slow, but seems to work
//

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StChargeStepMaker class for Makers                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include <iostream.h>
#include <stdlib.h>
#include <math.h>
#include "StChargeStepMaker.h"
#include "StChain.h"
#include "St_DataSet.h"
#include "St_DataSetIter.h"
#include "St_XDFFile.h"
#include "StMessMgr.h"
#include "tables/St_type_shortdata_Table.h"
#include "tables/St_tpg_detector_Table.h"
#include "tpc/St_xyz_newtab_Module.h"
#include "TH1.h"
#include "StTpcDb/StTpcDb.h"
#include "StTpcDb/StTpcDbMaker.h"

ClassImp(StChargeStepMaker)
  static const char *eRegions_names[] = {"innerWest",
					 "outerWest","innerEast","outerEast"}; 
  

//_____________________________________________________________________________
StChargeStepMaker::StChargeStepMaker(const char *name):
  StMaker(name),
  m_tclPixTransOn(kTRUE){
}

//_____________________________________________________________________________
StChargeStepMaker::~StChargeStepMaker() {
}

//_____________________________________________________________________________

Int_t StChargeStepMaker::Init() {

  // 		Create tables
  St_DataSet *tpc = GetDataBase("params/tpc");
  assert(tpc);
  // 		TSS parameters
  St_DataSet *tsspars = tpc->Find("tsspars");
  assert(tsspars);

  m_tsspar = NULL;
  m_tsspar = (St_tss_tsspar *) tsspars->Find("tsspar");
  assert(m_tsspar); 
  tss_tsspar_st *tsspar = m_tsspar->GetTable();
  tsspar->threshold = 1;


  // 		geometry parameters

  m_tpg_detector  = NULL;
  m_tpg_detector  = (St_tpg_detector  *) GetDataSet("tpcDB/.const/tpg_detector");
  assert ((m_tpg_detector)) ;

  m_tcl_sector_index = new St_tcl_sector_index("tcl_sector_index",1);
  m_tcl_sector_index->SetNRows(1); 
  AddConst(m_tcl_sector_index);
  

  //		Histograms     
  InitHistograms(); // book histograms
  
  return StMaker::Init();
}

//_____________________________________________________________________________

Int_t StChargeStepMaker::Make() {

  //  m_DataSet is global pointer from StMaker to data set

  if (Debug()) printf("Start of StChargeStepMaker");

  // initialize pointers to tables

  St_DataSet* sector;
  St_DataSet* raw_data_tpc = GetInputDS("tpc_raw");
  m_raw_data_tpc = kFALSE;
  Int_t sector_tot = 0;

  //  for (int i=0;i<4;i++){
  //    step[i]->Reset();
  //    derivative[i]->Reset();
  //  } 

  
  if (raw_data_tpc) {// Raw data exists -> make clustering
    m_raw_data_tpc = kTRUE;
    St_DataSetIter next(raw_data_tpc);
    St_raw_sec_m  *raw_sec_m = (St_raw_sec_m *) next("raw_sec_m");
    //Get the adcxyz table
    adcxyz = (St_tfc_adcxyz *) next("adcxyz");

    //counters for calculating size tables
    Int_t isumpix = 0;
    Int_t isumseq = 0;

    while ((sector=next())) {// loop over sectors
      const Char_t *name= 0;
      if ((name = strstr(sector->GetName(),"Sector"))) {
	// look for the sector number
	name  = strchr(name,'_')+1; Int_t indx = atoi(name);
	if (Debug()) printf(" Sector = %d \n", indx);
	St_DataSetIter sect(sector);
	St_type_shortdata  *pixel_data_in  = (St_type_shortdata *) sect("pixel_data_in");
	St_type_shortdata  *pixel_data_out = (St_type_shortdata *) sect("pixel_data_out");
	Int_t ipin = pixel_data_in->GetNRows();
	Int_t ipout = pixel_data_out->GetNRows();
	isumpix += ipin + ipout;
	if (Debug()) cout << "Total number of pixels, " << isumpix << endl;
	St_raw_seq  *raw_seq_in  = (St_raw_seq *) sect("raw_seq_in");
	St_raw_seq  *raw_seq_out = (St_raw_seq *) sect("raw_seq_out");
	Int_t nseqin = raw_seq_in->GetNRows();
	Int_t nseqout = raw_seq_out->GetNRows();
	isumseq += nseqin + nseqout;
	if (Debug()) cout << "Total number of sequences, " << isumseq << endl;
      }
    }
    
    //calculate or estimate the size before creating the tables

    if (!adcxyz && m_tclPixTransOn) {  
      // create flat adcxyz Table for pixel viewing
      if (Debug()) cout << "making adcxyz table with " << isumpix << " entries" << endl;
      adcxyz = new St_tfc_adcxyz("adcxyz",isumpix);  
      m_DataSet->Add(adcxyz);
      adcxyz->SetNRows(0);
    }
    // end creation tables

    next.Reset();

    while ((sector=next())) {  // loop over sectors
     const  Char_t *name = 0;
      if ((name = strstr(sector->GetName(),"Sector"))) {
	// look for the sector number
	name  = strchr(name,'_') + 1; 
	Int_t indx = atoi(name);
	printf(" Sector = %d \n", indx);
	tcl_sector_index_st *tcl_sector_index = m_tcl_sector_index->GetTable();
	m_tcl_sector_index->SetNRows(1);
	tcl_sector_index->CurrentSector = indx;
	St_DataSetIter sect(sector);
	St_raw_row         *raw_row_in     = (St_raw_row *) sect("raw_row_in");
	St_raw_row         *raw_row_out    = (St_raw_row *) sect("raw_row_out");
	St_raw_pad         *raw_pad_in     = (St_raw_pad *) sect("raw_pad_in");
	St_raw_pad         *raw_pad_out    = (St_raw_pad *) sect("raw_pad_out");
	St_raw_seq         *raw_seq_in     = (St_raw_seq *) sect("raw_seq_in");
	St_raw_seq         *raw_seq_out    = (St_raw_seq *) sect("raw_seq_out");
	St_type_shortdata  *pixel_data_in  = (St_type_shortdata *) sect("pixel_data_in");
	St_type_shortdata  *pixel_data_out = (St_type_shortdata *) sect("pixel_data_out");
	
	if (m_tclPixTransOn) {	  // call the pixel translation
	  if(Debug()) printf("Starting %20s for sector %2d.\n","xyz_newtab",indx);
	  // Need to guard against zero size output tables
          if(adcxyz->GetTableSize()){	  
	    Int_t res = xyz_newtab(m_tpg_detector,
				   m_tcl_sector_index,raw_sec_m,
				   raw_row_in,raw_pad_in,raw_seq_in,pixel_data_in,
				   raw_row_out,raw_pad_out,raw_seq_out,pixel_data_out,
				   adcxyz,m_tsspar);
	    
	    if (res != kSTAFCV_OK) Warning("Make","xyz_newtab == %d",res);
	  }
	}
	

      }
    }
  }
  
//		Histograms     
  MakeHistograms(); // clustering histograms
  //Calculate and print weighted means:
  for (int i=0;i<4;i++){
    float answer = GetWeightedMean(derivative[i]);
    result[i]->Fill(answer);
    cout << "weighted mean " << eRegions_names[i] << " = " << answer << endl;
  } 

  cout << "Got through StChargeStepMaker OK." << endl;

  return kStOK;
}

//-----------------------------------------------------------------------

void StChargeStepMaker::PrintInfo() {
  printf("**************************************************************\n");
  printf("* $Id: StChargeStepMaker.cxx,v 1.1 2000/07/12 18:43:11 hardtke Exp $\n");
  printf("**************************************************************\n");

  if (Debug()) StMaker::PrintInfo();
}

//-----------------------------------------------------------------------

void StChargeStepMaker::Clear(const char *opt) {

  cout << "resetting histos" << endl;
  for (int i=0;i<4;i++){
    step[i]->Reset();
    derivative[i]->Reset();
  } 


}
//-----------------------------------------------------------------------

Int_t StChargeStepMaker::Finish() {
  
  return StMaker::Finish();
}

//----------------------------------------------------------------------

void StChargeStepMaker::InitHistograms() {
theDb = ((StTpcDbMaker*)GetMaker("tpcDB"))->tpcDbInterface();
assert(theDb);
//Figure out approximately where the zero point is:
 cout << "TPC length = " << theDb->Dimensions()->outerEffectiveDriftDistance() << endl;
 cout << "Drift Velocity = " << theDb->DriftVelocity() << endl;
 cout << "sampling Frequency = " << theDb->Electronics()->samplingFrequency() << endl;
 cout << "trigger offset" << theDb->triggerTimeOffset() << endl;
theGuess = (int)((theDb->Dimensions()->outerEffectiveDriftDistance()/(theDb->DriftVelocity())+theDb->triggerTimeOffset())*theDb->Electronics()->samplingFrequency()*1e6);
 cout << "central membrane around tb = " << theGuess << endl;
 for(int i = 0;i<4;i++){
  char histname[50];
  sprintf(histname,"Step %s",eRegions_names[i]); 
  step[i] = new TH1S(histname,histname,100,theGuess-50,theGuess+50);
  AddHist(step[i]);
  sprintf(histname,"Diff %s",eRegions_names[i]); 
  derivative[i] = new TH1S(histname,histname,100,theGuess-50,theGuess+50);
  AddHist(derivative[i]);
  sprintf(histname,"Result %s",eRegions_names[i]); 
  result[i] = new TH1F(histname,histname,500,350,370);
  AddHist(result[i]);
 };

  // 		Create Histograms
}

//----------------------------------------------------------------------

void StChargeStepMaker::MakeHistograms() {
 tfc_adcxyz_st* xyztab = adcxyz->GetTable();
 for (int i = 0; i<adcxyz->GetNRows(); i++){
  int section = 0;
  if (xyztab[i].row>13) section++;
  if (xyztab[i].sector>12) section = section+2;
  step[section]->Fill(xyztab[i].bucket);
 }
 for (int ihist = 0;ihist<4;ihist++){
   for (int ientry = 0;ientry < step[ihist]->GetNbinsX()-1; ientry++){
    derivative[ihist]->SetBinContent(ientry,step[ihist]->GetBinContent(ientry) - step[ihist]->GetBinContent(ientry+1));
   }
 } 
}

//-----------------------------------------------------------------
float StChargeStepMaker::GetWeightedMean(TH1S* hist){
int peak = hist->GetMaximumBin();
float mom1=0.0;
float ysum=0.0;
float value;
 for (int i = peak-10;i<peak+10;i++){
  value = (float)hist->GetBinContent(i);
  ysum += value;
  mom1 += value*(float)(hist->GetBinCenter(i));
 }
 if (ysum!=0.0) mom1 = mom1/ysum;
 return mom1;
}
//_____________________________________________________________________________

