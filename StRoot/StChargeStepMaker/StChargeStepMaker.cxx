// $Id: StChargeStepMaker.cxx,v 1.8 2000/08/17 23:28:38 hardtke Exp $
// $Log: StChargeStepMaker.cxx,v $
// Revision 1.8  2000/08/17 23:28:38  hardtke
// read tpcChargeStepCalib parameters for database
//
// Revision 1.7  2000/08/15 00:25:41  hardtke
// require 10 good events before table is written
//
// Revision 1.6  2000/08/09 16:06:48  fisyak
// put tpcDriftVelocity into ./StarDb/Calibrations/tpc
//
// Revision 1.5  2000/08/04 21:03:40  perev
// Leaks + Clear() cleanup
//
// Revision 1.4  2000/07/28 18:31:54  hardtke
// print out tpcDriftVelocity table
//
// Revision 1.3  2000/07/14 14:42:43  hardtke
// Add return values so it compiles on Solaris
//
// Revision 1.2  2000/07/14 00:08:39  hardtke
// improve speed by factor of 1000
//
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
#include "tables/St_tpcDriftVelocity_Table.h"
#include "tables/St_tpcChargeStepCalib_Table.h"
#include "tpc/St_xyz_newtab_Module.h"
#include "TSystem.h"
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

  

  //		Histograms     
  InitHistograms(); // book histograms

  for (int isec = 0;isec<4;isec++){ 
    for (int ievt = 0; ievt<100;ievt++){
     pastresults[ievt][isec]=0.0;
    }
    nresults[isec]=0;
    lastresult[isec]=-1;
  }  // reset arrays for running average 

  date = 0;
  time = 0;
  return StMaker::Init();
}

//_____________________________________________________________________________

Int_t StChargeStepMaker::Make() {


  if (Debug()) printf("Start of StChargeStepMaker");

  if (date==0) {date = GetDate();cout << "date = " << date << endl;}
  if (time==0) {time = GetTime();cout << "time = " << time << endl;}

  // prepare tree  
  typedef struct 
  {
    Float_t x,y,z;
    Int_t time,pad,adc;
  } 
  Pixel_t;
  Pixel_t mypixel;

  //  mytree = new TTree("Pixels","Pixels");
  //  mytree->Branch("Pixels",&mypixel,"x:y:z:pad:adc");
  

  // pixel counter
  Int_t number_of_found = 0 ;

  // Fill display with data
  St_DataSet* raw_data_tpc = (St_DataSet*) GetInputDS("tpc_raw");

  if (!raw_data_tpc)
    {
      cout << "No raw data found." << endl ;
      return kStOK;
    }
  else
    {
      
      // iterator of raw_data_tpc to navigate through it
      St_DataSetIter rawdata(raw_data_tpc) ;
      for (int sec = 1;sec<=24;sec++){
      Char_t sectorname[10]; //= "Sector_1" ;
      sprintf(sectorname, "Sector_%d", sec);
      St_DataSet *sector = rawdata.FindObject(sectorname) ;
      //      cout << " Examining sector : " << sectorname << endl ;	
      
      if (!sector) return kStOK;
      
      St_DataSetIter sector_iterator(sector) ;
      //sector_iterator.Du() ;
      // get the raw tables of this secotor 
      St_raw_row *st_raw_row ;
      raw_row_st* row_st ;
      St_raw_pad *st_raw_pad ;
      raw_pad_st* pad_st ;
      St_raw_seq *st_raw_seq ;
      raw_seq_st* seq_st ;
      St_type_shortdata *st_shortdata ;
      type_shortdata_st* adcarray ;

      // rowindex
      for (int row = 45;row>0;row--){
      Int_t rowindex = 0;

      if (row<=13)
	{
	  // inner sector
	  // raw_row_in
	  st_raw_row = (St_raw_row*) sector_iterator("raw_row_in");
	  row_st = (raw_row_st*) st_raw_row->GetTable();
	  // raw_pad_in
	  st_raw_pad = (St_raw_pad*) sector_iterator("raw_pad_in");
	  pad_st = (raw_pad_st*) st_raw_pad->GetTable();
	  // raw_seq_in
	  st_raw_seq = (St_raw_seq*) sector_iterator("raw_seq_in");
	  seq_st = (raw_seq_st*) st_raw_seq->GetTable();
	  // raw_shortdata_in
	  st_shortdata = (St_type_shortdata *) sector_iterator("pixel_data_in");
	  adcarray = (type_shortdata_st*) st_shortdata->GetTable();
	  
	  // rowdindex
	  rowindex = 13 - row ;
	}
      else 
	{
	  //outer sector
	  // raw_row_in
	  st_raw_row = (St_raw_row*) sector_iterator("raw_row_out");
	  row_st = (raw_row_st*) st_raw_row->GetTable();
	  // raw_pad_in
	  st_raw_pad = (St_raw_pad*) sector_iterator("raw_pad_out");
	  pad_st = (raw_pad_st*) st_raw_pad->GetTable();
	  // raw_seq_in
	  st_raw_seq = (St_raw_seq*) sector_iterator("raw_seq_out");
	  seq_st = (raw_seq_st*) st_raw_seq->GetTable();
	  // raw_shortdata_in
	  st_shortdata = (St_type_shortdata *) sector_iterator("pixel_data_out");
	  adcarray = (type_shortdata_st*) st_shortdata->GetTable();
	  // row index
	  rowindex = 45 - row ;
	}	  

      ////////
      // Get data for this row
      ///////
      Int_t pixel_offset_row =(Int_t) (row_st[rowindex].ipixel) ;
      Int_t seq_offset_row =(Int_t) (row_st[rowindex].iseq) ;
      Int_t pad_offset_row =(Int_t) (row_st[rowindex].ipad) ;
      Int_t row_id = (Int_t) (row_st[rowindex].RowId) ;

      /////////
      // loop over pads
      /////////
      //cout << " Number of pads we loop over " <<  Int_t (row_st[rowindex].npad) << endl;
      for (Int_t padindex = 0 ; padindex < Int_t (row_st[rowindex].npad) ; padindex++)
	{
	  Int_t pixel_offset_pad = (Int_t) (pixel_offset_row + (pad_st[padindex+pad_offset_row].PadOffset));
	  Int_t seq_offset_pad = (Int_t) (seq_offset_row + (pad_st[padindex+pad_offset_row].SeqOffset));
	  Int_t num_seq_pad = (Int_t) (pad_st[padindex+pad_offset_row].nseq);
	    
	  Int_t pad_id = (Int_t) (pad_st[padindex+(row_st[rowindex].ipad)].PadId);
	  Int_t seq_mod_break = (Int_t) (pad_st[padindex+(row_st[rowindex].ipad)].SeqModBreak);
	  //if (num_seq_pad>1)
	  //{
	  //  cout << "alert padid " << pad_id ;
	  // cout << "    row  " << row_id ;
	  ////  cout << "    seq  " << num_seq_pad ;
	  //  }
	  ///////
	  // loop over sequenzes
	  ///////
	  //cout << " Number of sequenzes we loop over " << num_seq_pad << endl;
	  for ( Int_t sequenzindex = 0 ; sequenzindex < num_seq_pad ; sequenzindex++ )
	    {
	      Int_t timebucketoffset=0;
	      if ( sequenzindex < seq_mod_break )
		{
		  timebucketoffset = (Int_t) (seq_st[sequenzindex + seq_offset_pad].m);
		}
	      else if ( sequenzindex >= seq_mod_break )
		{
		  timebucketoffset = (Int_t) (seq_st[sequenzindex + seq_offset_pad].m + 256) ;
		} 
		

	      Int_t numberoftimebucketsinthissequenz =(Int_t) (seq_st[sequenzindex + seq_offset_pad].i);
	      //////
	      // loop over pixel in sequenz
	      //////
	      //cout << " Number of pixel we loop over " << numberoftimebucketsinthissequenz << endl;
	      for ( Int_t pixelindex = 0 ; pixelindex <= numberoftimebucketsinthissequenz ; pixelindex++ )
		{
		  // Get adc value
		  Short_t adc_value = (Short_t) adcarray[pixelindex+pixel_offset_pad].data;
		    
		  // Get according bucket
		  Int_t bucket_id = (Int_t) timebucketoffset + pixelindex;

		  if ( adc_value < 1024 && adc_value >= 0 && 
		       pad_id >= 1 && pad_id <= 182
		       && bucket_id >= 0 && bucket_id < 512 )
		    {
		      // Increase counter
		      number_of_found++;

		      // Fill Display
		      //		      myhist->Fill(pad_id,bucket_id,adc_value) ;

		      //		      PTRS.Setptrs((Double_t) pad_id, (Double_t) bucket_id,(Double_t) row_id, (Double_t) sec) ;
		      //		      transformer.raw_to_global(PTRS,XYZ) ;
		      //		      mypixel.x = (Float_t) XYZ.Getx() ;
		      //		      mypixel.y = (Float_t) XYZ.Gety() ;
		      //		      mypixel.z = (Float_t) XYZ.Getz() ;
		      mypixel.pad = (Int_t) pad_id ; 
		      mypixel.adc = (Int_t) adc_value ;
		      mypixel.time = (Int_t) bucket_id  ;
                      int section = 0;
                      if (row>13) section++;
                      if (sec>12) section = section+2;
		      //                      step[section]->Fill(mypixel.time,mypixel.adc);
                      step[section]->Fill(mypixel.time);
 

		      //		      mytree->Fill();

		     

		      if (number_of_found%100==0)
			{
			  //			  cout << " Filling  pad : " << pad_id << "\t" ;
//  			  cout << "time : " << bucket_id << "\t" ;
//  			  cout << "adc : " << adc_value << "\t" ;
//  			  cout << "x : " << mypixel.x  << "\t" ;
//  			  cout << "y : " << mypixel.y  << "\t" ;
//  			  cout << "z : " << mypixel.z << endl;
			}
		    }
		  else 
		    {
		      cout << "something wrong with adc value :" << adc_value <<endl;
		      cout << "in   pad : " << pad_id << "\t" ;
		      cout << "       time : " << bucket_id << "\t" ;
		      cout << "       adc : " << adc_value << endl;
		    }
		} // Loop over pixel in this sequenz 
	    } // Loop over sequnezes                           
	} // Loop over pads
      }   // Loop over rows
      }   // Loop over sectors
    }


  
//		Histograms     
    MakeHistograms(); // clustering histograms
  //Calculate and print weighted means:
  for (int i=0;i<4;i++){
    if (step[i]->GetEntries()>200000){
    float answer = GetWeightedMean(derivative[i]);
    if (answer<0.0) continue;
    result[i]->Fill(answer);
    AddToAverage(i,answer);
    float current_average = GetAverage(i);
    cout << "StChargeStepMaker: weighted mean "  << eRegions_names[i] << " = " << answer << endl;
    cout << "StChargeStepMaker: average "  << eRegions_names[i] << " = " << current_average << endl;
    }
  } 

  cout << "Got through StChargeStepMaker OK." << endl;

  return kStOK;
}

//-----------------------------------------------------------------------

void StChargeStepMaker::PrintInfo() {
  printf("**************************************************************\n");
  printf("* $Id: StChargeStepMaker.cxx,v 1.8 2000/08/17 23:28:38 hardtke Exp $\n");
  printf("**************************************************************\n");

  if (Debug()) StMaker::PrintInfo();
}

//-----------------------------------------------------------------------

void StChargeStepMaker::Clear(const char *opt) {

  for (int i=0;i<4;i++){
    step[i]->Reset();
    derivative[i]->Reset();
  } 
  StMaker::Clear();

}
//-----------------------------------------------------------------------

Int_t StChargeStepMaker::Finish() {
  if (nresults[1]>10&&nresults[3]>10){  // need some stats for decent result
    WriteTableToFile();
    cout << "StChargeStepMaker -- Create tpcDriftVelocityTable" << endl;
  }
  else {cout << "StChargeStepMaker -- Insufficient Statistics for Drift Velocity" << endl;
  }
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
theGuess = (int)((theDb->Dimensions()->outerEffectiveDriftDistance()/(theDb->DriftVelocity())-theDb->triggerTimeOffset())*theDb->Electronics()->samplingFrequency()*1e6);
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
  result[i] = new TH1F(histname,histname,200,theGuess-20,theGuess+20);
  AddHist(result[i]);
 };

  // 		Create Histograms
}

//----------------------------------------------------------------------

void StChargeStepMaker::MakeHistograms() {
 for (int ihist = 0;ihist<4;ihist++){
   for (int ientry = 0;ientry < step[ihist]->GetNbinsX()-1; ientry++){
    derivative[ihist]->SetBinContent(ientry,step[ihist]->GetBinContent(ientry) - step[ihist]->GetBinContent(ientry+1));
   }
 } 
}

//-----------------------------------------------------------------
float StChargeStepMaker::GetWeightedMean(TH1S* hist){
int peak = hist->GetMaximumBin();
 int location = hist->GetBinCenter(peak);
 if (location<theGuess-10||location>theGuess+10) {
   cout << "StChargeStepMaker:: False peak found at tb = " << location << endl;
   cout << "StChargeStepMaker:: Database may be way off" << endl;
   return -1.0;
 }
float mom1=0.0;
float ysum=0.0;
float value;
 float maxval = hist->GetBinContent(peak);
 for (int i = peak-5;i<peak+5;i++){
  value = (float)hist->GetBinContent(i);
  ysum += value;
  mom1 += value*(float)(hist->GetBinCenter(i));
 }
 if (ysum!=0.0) mom1 = mom1/ysum;
 return mom1;
}
//_____________________________________________________________________________
void StChargeStepMaker::AddToAverage(int section, float value){
  if (lastresult[section]!=99){
    lastresult[section] = lastresult[section]+1;
    int index = lastresult[section];
   pastresults[index][section] = value;
  }
  else {
   lastresult[section]=0;
   int index = lastresult[section];
   pastresults[index][section]=value;
  }
  if (nresults[section]<100) nresults[section] = nresults[section]+1;
}
//_____________________________________________________________________________
float StChargeStepMaker::GetAverage(int section){
 float sum=0.0;
 for (int i =0;i<nresults[section];i++){
  sum+=pastresults[i][section];
 }
  float value = sum/nresults[section];
  return value;
}

St_tpcDriftVelocity* StChargeStepMaker::driftTable(){
  St_tpcChargeStepCalib* cs = (St_tpcChargeStepCalib*)GetDataBase("Calibrations/tpc/tpcChargeStepCalib");
  assert(cs);
  tpcChargeStepCalib_st *mytable = (tpcChargeStepCalib_st*)cs->GetTable();
  float reference_velocity = mytable->laserVelocityEast;
  float reference_t0       = theDb->triggerTimeOffset()*1e6;
  float reference_clock    = theDb->Electronics()->samplingFrequency();
  float reference_step_west = mytable->chargeStepOuterWest;
  float reference_step_east = mytable->chargeStepOuterEast;;
  float Lwest = reference_velocity*((reference_step_west/reference_clock)+reference_t0);
  float Least = reference_velocity*((reference_step_east/reference_clock)+reference_t0);
  cout << "Least, trig offset = " << Least << " " << reference_t0 << endl; 
  float velocityEast = Least/((GetAverage(3)/theDb->Electronics()->samplingFrequency()) + theDb->triggerTimeOffset()*1e6);
  float velocityWest = Lwest/((GetAverage(1)/theDb->Electronics()->samplingFrequency()) + theDb->triggerTimeOffset()*1e6);
  St_tpcDriftVelocity* table = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st* row = table->GetTable();
  row->cathodeDriftVelocityEast = velocityEast;
  row->cathodeDriftVelocityWest = velocityWest;
  row->laserDriftVelocityEast = 0.0;
  row->laserDriftVelocityWest = 0.0;
  table->SetNRows(1);
  return table;
}

void StChargeStepMaker::WriteTableToFile(){
  char filename[80]; 
  sprintf(filename,"./StarDb/Calibrations/tpc/tpcDriftVelocity.%08d.%06d.C",date,time);
  TString dirname = gSystem->DirName(filename);
  if (!gSystem->OpenDirectory(dirname.Data())) { 
    if (gSystem->mkdir(dirname.Data())) {
      cout << "Directoty " << dirname << " creation failed" << endl;
    }
  }
  ofstream *out = new ofstream(filename);
  driftTable()->SavePrimitive(*out,"");
  return;
}




