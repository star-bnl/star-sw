// Author : Dominik Flierl 
// $Id: StClusterDisplayMaker.cxx,v 1.1 2000/07/07 02:31:31 flierl Exp $
//////////////////////////////////////////////////////////////////////////
//                                                                      //
//                                                                      //
// StClusterDisplay shows pixels and reconstructed clusters             //
//                                                                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "StClusterDisplayMaker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "StDAQMaker/StDAQReader.h"
#include "TH2.h"
#include "TCanvas.h"
#include "TPad.h"
#include "TTree.h"
#include "TGraphErrors.h"
#include "tables/St_raw_sec_m_Table.h"
#include "tables/St_raw_pad_Table.h"
#include "tables/St_raw_row_Table.h"
#include "tables/St_raw_seq_Table.h"
#include "tables/St_type_shortdata_Table.h"
#include "tables/St_tcl_tphit_Table.h"
#include "St_l3Clufi_Maker/St_l3Clufi_Maker.h"
#include "St_l3Clufi_Maker/St_l3_Coordinate_Transformer.h"
#include "St_l3Clufi_Maker/St_l3_Coordinates.h"
#include "St_TableSorter.h"

ClassImp(StClusterDisplayMaker) ;

//_____________________________________________________________________________
StClusterDisplayMaker::StClusterDisplayMaker(const char *name):StMaker(name){
  //  ClusterDisplay constructor
  //
  //  const char *name -  the name of this constructor
  //
  //  The first comment lines after the opening bracket
  //  ({) of a member function are considered as a member function description 
  //  see: begin_html <A HREF="http://root.cern.ch/root/Documentation.html"> ROOT HTML documentation </A> end_html   //
}
//_____________________________________________________________________________
StClusterDisplayMaker::~StClusterDisplayMaker(){
  // This ClusterDisplay destructor
  //
  //  The first comment lines after the opening bracket
  //  ({) of a member function are considered as a member function description 
  //
  //  The first comment lines after the opening bracket
  //  ({) of a member function are considered as a member function description 
  //  see: begin_html <A HREF="http://root.cern.ch/root/Documentation.html"> ROOT HTML documentation </A> end_html   //
}
//_____________________________________________________________________________
Int_t StClusterDisplayMaker::Init(){
  //  Init - is a first method the top level StChain calls to initialize all its makers
  //
  //  The first comment lines after the opening bracket
  //  ({) of a member function are considered as a member function description 
  //  see: begin_html <A HREF="http://root.cern.ch/root/Documentation.html"> ROOT HTML documentation </A> end_html   //

  // Create tables
  St_DataSetIter       local(GetDataBase("params"));
  // Create Histograms    
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StClusterDisplayMaker::Make(Int_t isec , Int_t irow ,Char_t* c, Int_t pad_min, Int_t pad_max, Int_t time_min, Int_t time_max)
{
  // init parameters
  sec = isec ;
  row = irow ;

  // get option
  TString option(c) ;
  
  // create histo
  if ( sec > 24 || sec < 1 || row < 1 || row > 45 )
    {
      cout << "Take default values for sec and row !\n" ;
      sec = 1;
      row = 1;
    }     
  Char_t sec_row[30]; 
  if ( sec < 10 && row <10 ) { sprintf ( sec_row, "Sec_0%1d_Row_0%1d", sec,row ) ; }
  else if ( sec < 10 && row >=10 ) { sprintf ( sec_row, "Sec_0%1d_Row_%2d", sec,row ) ; }
  else if ( sec >= 10 && row <10 ) { sprintf ( sec_row, "Sec_%2d_Row_0%1d", sec,row ) ; }
  else if ( sec >= 10 && row >=10 ) { sprintf ( sec_row, "Sec_%2d_Row_%2d", sec,row ) ; } ;

  
  if ( pad_max<pad_min || time_max<time_min || pad_min<0 || pad_max>182 || time_min<0 || time_max>512)
    {
      cout << "Take default values for histo!\n" ;
      pad_max = 182 ;
      pad_min = 1 ;
      time_max = 512 ;
      time_min = 0 ;
    }
  // !!!! add 0.5 because tb 338.0 in reality = 338.5 in histo !!!! 
  myhist = new TH2S(sec_row,sec_row,(pad_max-pad_min),(pad_min-0.5),(pad_max-0.5),(time_max-time_min),(time_min-0.5),(time_max-0.5)) ;
  myhist->SetXTitle("Pad");
  myhist->SetYTitle("Time Bucket");

  // fill histo with pixel
  //  if (row>13)
  //  {
  //Fill_Pixel_into_histo_outer() ;
  //  }
  //else
  // {
  Fill_Pixel_into_histo_inner() ;
  // }

  

  // create canvas and plot histo
  TCanvas* can = new TCanvas("ClusterCockpit","ClusterCockpit",600,500) ;
  can->ToggleEventStatus();
  myhist->SetStats(0) ;
  myhist->Draw("Colz") ;
  

  // graph with l3off points
  if (option.Contains("-l3off"))
      {
	  Int_t no_l3_off = Get_l3off_points() ;
	  graph_l3_off_points  = new TGraphErrors(no_l3_off,l3offpoints_pad,l3offpoints_time);
	  graph_l3_off_points->SetMarkerStyle(28);
	  graph_l3_off_points->SetMarkerColor(4);
	  graph_l3_off_points->SetMarkerSize(2);
	  graph_l3_off_points->Draw("P");
      }

  // graph with off points
  if (option.Contains("-off"))
      {
	  Int_t no_off = Get_off_points() ;
	  graph_off_points  = new TGraphErrors(no_off,offpoints_pad,offpoints_time,offpoints_pad_err,offpoints_time_err);
	  graph_off_points->SetMarkerStyle(24);
	  graph_off_points->SetMarkerColor(3);
	  graph_off_points->SetMarkerSize(2);
	  graph_off_points->Draw("P");
      }

  // graph with l3online points
  if (option.Contains("-l3on"))
      {
	  Int_t no_l3_on = Get_l3on_points() ;
	  graph_l3_on_points  = new TGraphErrors(no_l3_on,l3onpoints_pad,l3onpoints_time);
	  graph_l3_on_points->SetMarkerStyle(27);
	  graph_l3_on_points->SetMarkerColor(2);
	  graph_l3_on_points->SetMarkerSize(2);
	  graph_l3_on_points->Draw("P");
      }

  // graph with l3online points
  if (option.Contains("-mat"))
    {
      Int_t num_not_matched = 0 ;
      Int_t no_ma = Get_matched_points(num_not_matched) ;
      matched_points  = new TGraphErrors(no_ma,matched_points_pad,matched_points_time);
      matched_points->SetMarkerStyle(24);
      matched_points->SetMarkerColor(2);
      matched_points->SetMarkerSize(4);
      matched_points->Draw("P");

      if (option.Contains("-notmat") && num_not_matched>0)
	{
	   not_matched_points  = new TGraphErrors(num_not_matched,not_matched_points_pad,not_matched_points_time) ;
	   not_matched_points->SetMarkerStyle(24) ;
	   not_matched_points->SetMarkerColor(1) ;
	   not_matched_points->SetMarkerSize(4) ;
	   not_matched_points->Draw("P") ;
	} 
    }

  // go home
  return kStOK;
}

//__________________________________________________________________
void StClusterDisplayMaker::Fill_Pixel_into_histo_inner()
{

    // prepare cood-trans
    St_l3_Coordinate_Transformer transformer ;
    transformer.Use_transformation_provided_by_db();
    St_l3_xyz_Coordinate XYZ(0,0,0) ;
    St_l3_ptrs_Coordinate PTRS(0,0,0,0) ;
    

  // prepare tree  
  typedef struct 
  {
    Float_t x,y,z;
    Int_t time,pad,adc;
  } 
  Pixel_t;
  Pixel_t mypixel;

  mytree = new TTree("Pixels","Pixels");
  mytree->Branch("Pixels",&mypixel,"x:y:z:pad:adc");
  

  // pixel counter
  Int_t number_of_found = 0 ;

  // Fill display with data
  St_DataSet* raw_data_tpc = (St_DataSet*) GetInputDS("tpc_raw");
  if (!raw_data_tpc)
    {
      cout << "No raw data found." << endl ;
      return ;
    }
  else
    {
      
      // iterator of raw_data_tpc to navigate through it
      St_DataSetIter rawdata(raw_data_tpc) ;
      Char_t sectorname[10]; //= "Sector_1" ;
      sprintf(sectorname, "Sector_%d", sec);
      St_DataSet *sector = rawdata.FindObject(sectorname) ;
      cout << " Examining sector : " << sectorname << endl ;	
      
      if (!sector) return ;
      
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
      cout << "Examining rowindex  :"<< rowindex << endl ;
      Int_t pixel_offset_row =(Int_t) (row_st[rowindex].ipixel) ;
      Int_t seq_offset_row =(Int_t) (row_st[rowindex].iseq) ;
      Int_t pad_offset_row =(Int_t) (row_st[rowindex].ipad) ;
      Int_t row_id = (Int_t) (row_st[rowindex].RowId) ;
      cout << "Row_id  :"<< row_id << endl ;

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
		      myhist->Fill(pad_id,bucket_id,adc_value) ;

		      PTRS.Setptrs((Double_t) pad_id, (Double_t) bucket_id,(Double_t) row_id, (Double_t) sec) ;
		      transformer.raw_to_global(PTRS,XYZ) ;
		      mypixel.x = (Float_t) XYZ.Getx() ;
		      mypixel.y = (Float_t) XYZ.Gety() ;
		      mypixel.z = (Float_t) XYZ.Getz() ;
		      mypixel.pad = (Int_t) pad_id ; 
		      mypixel.adc = (Int_t) adc_value ;
		      mypixel.time = (Int_t) bucket_id  ;
		      mytree->Fill();

		     

		      if (number_of_found%100==0)
			{
			  cout << " Filling  pad : " << pad_id << "\t" ;
			  cout << "time : " << bucket_id << "\t" ;
			  cout << "adc : " << adc_value << "\t" ;
			  cout << "x : " << mypixel.x  << "\t" ;
			  cout << "y : " << mypixel.y  << "\t" ;
			  cout << "z : " << mypixel.z << endl;
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
    }
  cout << "Number of found clusters : " << number_of_found << "\t" ;
  cout << "sec : " << sec << "\t" ;
  cout << "row : " << row << endl ;
}
//________________________________
void StClusterDisplayMaker::Fill_Pixel_into_histo_outer()
{  
  Int_t number_of_found = 0 ;
	
  // Fill display with data

  St_DataSet* raw_data_tpc = (St_DataSet*) GetInputDS("tpc_raw");
  if (!raw_data_tpc)
    {
      cout << "No raw data found." << endl ;
      return ;
    }
  else
    {
      // iterator of raw_data_tpc to navigate through it
      St_DataSetIter rawdata(raw_data_tpc) ;
      Char_t* sectorname = "Sector_07" ;
      St_DataSet *sector = rawdata.FindObject(sectorname) ;
      cout << " Examining sector : " << sectorname << endl ;	

      St_DataSetIter sector_iterator(sector) ;
      //sector_iterator.Du() ;
      // get the raw tables of this secotor 
      // raw_row_out
      St_raw_row *raw_row_out = (St_raw_row*) sector_iterator("raw_row_out");
      raw_row_st* outerrow = (raw_row_st*) raw_row_out->GetTable();
      // raw_pad_out
      St_raw_pad *raw_pad_out = (St_raw_pad*) sector_iterator("raw_pad_out");
      raw_pad_st* padout = (raw_pad_st*) raw_pad_out->GetTable();
      // raw_seq_out
      St_raw_seq *raw_seq_out = (St_raw_seq*) sector_iterator("raw_seq_out");
      raw_seq_st* seqout = (raw_seq_st*) raw_seq_out->GetTable();
      // raw_shortdata_out
      St_type_shortdata *shortdataout = (St_type_shortdata *) sector_iterator("pixel_data_out");
      type_shortdata_st* adcarrayout = (type_shortdata_st*) shortdataout->GetTable();

      ////////
      // Get data for this row
      ///////
      Int_t rowindex = 45 - row ;
      cout << "Examining rowindex  :"<< rowindex << endl ;
      Int_t pixel_offset_row =(Int_t) (outerrow[rowindex].ipixel) ;
      Int_t seq_offset_row =(Int_t) (outerrow[rowindex].iseq) ;
      Int_t pad_offset_row =(Int_t) (outerrow[rowindex].ipad) ;
      Int_t row_id = (Int_t) (outerrow[rowindex].RowId) ;
      cout << "Row_id  :"<< row_id << endl ;

      /////////
      // loop over pads
      /////////
      //cout << " Number of pads we loop over " <<  Int_t (row_st[rowindex].npad) << endl;
      for (Int_t padindex = 0 ; padindex < Int_t (outerrow[rowindex].npad) ; padindex++)
	{
	  Int_t pixel_offset_pad = (Int_t) (pixel_offset_row + (padout[padindex+pad_offset_row].PadOffset));
	  Int_t seq_offset_pad = (Int_t) (seq_offset_row + (padout[padindex+pad_offset_row].SeqOffset));
	  Int_t num_seq_pad = (Int_t) (padout[padindex+pad_offset_row].nseq);
	    
	  Int_t pad_id = (Int_t) (padout[padindex+(outerrow[rowindex].ipad)].PadId);
	  Int_t seq_mod_break = (Int_t) (padout[padindex+(outerrow[rowindex].ipad)].SeqModBreak);
	  // if (num_seq_pad>1)
	  // 		{
	  // 		    cout << "alert padid " << pad_id ;
	  // 		    cout << "    row  " << row_id ;
	  // 		    cout << "    seq  " << num_seq_pad ;
	  // 		}
	  ///////
	  // loop over sequenzes
	  ///////
	  //cout << " Number of sequenzes we loop over " << num_seq_pad << endl;
	  for ( Int_t sequenzindex = 0 ; sequenzindex < num_seq_pad ; sequenzindex++ )
	    {
	      Int_t timebucketoffset=0;
	      if ( sequenzindex < seq_mod_break )
		{
		  timebucketoffset = (Int_t) (seqout[sequenzindex + seq_offset_pad].m);
		}
	      else if ( sequenzindex >= seq_mod_break )
		{
		  timebucketoffset = (Int_t) (seqout[sequenzindex + seq_offset_pad].m + 256) ;
		} 
		

	      Int_t numberoftimebucketsinthissequenz =(Int_t) (seqout[sequenzindex + seq_offset_pad].i);
	      //////
	      // loop over pixel in sequenz
	      //////
	      //cout << " Number of pixel we loop over " << numberoftimebucketsinthissequenz << endl;
	      for ( Int_t pixelindex = 0 ; pixelindex <= numberoftimebucketsinthissequenz ; pixelindex++ )
		{
		  // Get adc value
		  Short_t adc_value = (Short_t) adcarrayout[pixelindex+pixel_offset_pad].data;
		    
		  // Get according bucket
		  Int_t bucket_id = (Int_t) timebucketoffset + pixelindex;

		  if ( adc_value < 1024 && adc_value > 0 && 
		       pad_id > 1 && pad_id < 182
		       && bucket_id >= 0 && bucket_id < 512 )
		    {
		      // Fill Display
		      cout << " Filling  pad : " << pad_id << "\t" ;
		      cout << "       row : " << row_id << "\t" ;
		      cout << "       time : " << bucket_id << "\t" ;
		      cout << "       adc : " << adc_value << endl;
		      myhist->Fill(pad_id,bucket_id,adc_value) ;
		      number_of_found++;
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
      cout << "Number of found clusters : " << number_of_found << "/t" ;
      cout << "sec : " << sec << "/t" ;
      cout << "row : " << row_id << endl ;
    }
}//_____________________
Int_t StClusterDisplayMaker::Get_l3off_points()
{
    // prepare cood-trans
    cout << endl << endl;
    St_l3_Coordinate_Transformer transformer ;
    transformer.Use_transformation_provided_by_db();
    transformer.Print_parameters() ;
    St_l3_xyz_Coordinate XYZ(0,0,0) ;
    St_l3_ptrs_Coordinate PTRS(0,0,0,0) ;
       
    // prepare tree
    typedef struct 
    {
	Float_t x,y,z;
	Float_t charge,pad,time;
    } 
    POINT;
    POINT l3offpoints;
   
    TTree *l3offpointtree = new TTree("T","L3 offline points");
    l3offpointtree->Branch("POINT",&l3offpoints,"x:y:z:pad:time:charge");
    
    // prepare arrays
    Int_t max_number_of_points=1000;
    l3offpoints_pad = new Float_t[max_number_of_points];
    l3offpoints_time = new Float_t[max_number_of_points];

    // counter
    Int_t badcluster = 0 ;

    // loop over points
    St_tcl_tphit* sthit = (St_tcl_tphit*) GetDataSet("bfc/.make/l3/.make/l3Clufi/.data/L3hit");
    tcl_tphit_st* myl3offhit =  sthit->GetTable();
    if(!myl3offhit)
      {
	cout << "sorry no l3off data found" << endl ;
	return 0;
      }
    cout << "l3 clusters found in this sec and row: "<< endl;
    Int_t count = 0;
    for(Int_t i=0;i<sthit->GetNRows();i++)
	{
	    if( (myl3offhit[i].row-myl3offhit[i].row%100) / 100 == sec && myl3offhit[i].row%100 == row )
		{
		  // exclude unreasonable timebuckets
		  if (myl3offhit[i].z < 0 && sec<=12 ) { badcluster++ ; continue ; }
		  if (myl3offhit[i].z > 0 && sec>12 )  { badcluster++ ; continue ; }

		    l3offpoints.x = myl3offhit[i].x ;
		    l3offpoints.y = myl3offhit[i].y ;
		    l3offpoints.z = myl3offhit[i].z ;
		    l3offpoints.charge = myl3offhit[i].dedx ;
		    l3offpoints.pad = 1;
		    l3offpoints.time = 100;
		    l3offpointtree->Fill();
		    
		    // back transform
		    XYZ.Setx(myl3offhit[i].x) ;
		    XYZ.Sety(myl3offhit[i].y) ;
		    XYZ.Setz(myl3offhit[i].z) ;
		    transformer.global_to_raw(XYZ,PTRS) ;
		    
		    cout << i << "\t";
		    cout << "x :" << myl3offhit[i].x <<"\t";
		    cout << "y :" << myl3offhit[i].y <<"\t";
		    cout << "z :" << myl3offhit[i].z <<"\t";
		    cout << "q :" << myl3offhit[i].q <<"\t";
		    cout << "pad :" << PTRS.Getp()  <<"\t";
		    cout << "time :" << PTRS.Gett() << endl;
		    
		    if (count<max_number_of_points)
			{
			  // !!!! add 0.5 because tb 338.0 in reality = 338.5 in histo !!!! 
			  l3offpoints_pad[count] = (Float_t) (PTRS.Getp()) ;
			  l3offpoints_time[count] = (Float_t) (PTRS.Gett()) ;
			}
		    // increase counter
		    count++;
		}
	}
    cout << "Number of l3 clusters found in this row and sector: " << count << endl;
    cout << "Number of bad clusters found in this row and sector: " << badcluster << endl;
    cout << "Number of l3 clusters found in this event: " << sthit->GetNRows() << endl;
    return count;
}
//_____________________
Int_t StClusterDisplayMaker::Get_off_points()
{
  // prepare cood-trans
  cout << endl << endl;
  St_l3_Coordinate_Transformer transformer ;
  transformer.Use_transformation_provided_by_db();
  transformer.Print_parameters();
  St_l3_xyz_Coordinate XYZ(0,0,0) ;
  St_l3_ptrs_Coordinate PTRS(0,0,0,0) ;
  
  // prepare arrays
  Int_t max_number_of_points=1000;
  offpoints_pad = new Float_t[max_number_of_points];
  offpoints_time = new Float_t[max_number_of_points];
  offpoints_pad_err = new Float_t[max_number_of_points];
  offpoints_time_err = new Float_t[max_number_of_points];

  // loop over points
  St_tcl_tphit* sthit = (St_tcl_tphit*) GetDataSet("bfc/.make/tpc/.make/tpc_hits/.data/tphit");
  tcl_tphit_st* myoffhit =  sthit->GetTable();
  if (!myoffhit)
    {
      cout << "sorry no off points found" << endl ;
      return 0;
    }
  cout << "off clusters found in this sec and row"<< endl;

  // counter
  Int_t badcluster = 0 ;
  Int_t count = 0;
  for(Int_t i=0;i<sthit->GetNRows();i++)
    {
      if( (myoffhit[i].row-myoffhit[i].row%100) / 100 == sec && myoffhit[i].row%100 == row )
		{

		    // exclude unreasonable timebuckets
		    if (myoffhit[i].z < 0 && sec<=12 )  { badcluster++ ; continue ; }
		    if (myoffhit[i].z > 0 && sec>12 )  { badcluster++ ; continue ; }

		    // back transform
		    XYZ.Setx(myoffhit[i].x) ;
		    XYZ.Sety(myoffhit[i].y) ;
		    XYZ.Setz(myoffhit[i].z) ;
		    transformer.global_to_raw(XYZ,PTRS) ;
		  
		  
		    cout << i << "\t";
		    cout << "x :" << myoffhit[i].x <<"\t";
		    cout << "y :" << myoffhit[i].y <<"\t";
		    cout << "z :" << myoffhit[i].z <<"\t";
		    cout << "q :" << myoffhit[i].q <<"\t";
		    cout << "pads :" << myoffhit[i].npads <<"\t";
		    cout << "maxtime :" << myoffhit[i].maxtmbk <<"\t";
		    cout << "mintime :" << myoffhit[i].mintmbk <<"\t";
		    cout << "pad :" << PTRS.Getp()  <<"\t";
		    cout << "time :" << PTRS.Gett() << endl;
		  
		    if (count<max_number_of_points)
		      {
			// !!!! add 0.5 because tb 338.0 in reality = 338.5 in histo !!!! 
			offpoints_pad[count] = (Float_t) (PTRS.Getp()) ;
			offpoints_pad_err[count] = (Float_t) (myoffhit[i].npads/2) ;

			offpoints_time[count] = ((Float_t) PTRS.Gett()) ;
			Float_t n_tbs = (myoffhit[i].maxtmbk - myoffhit[i].mintmbk)/2  ;
			offpoints_time_err[count] = (Float_t) n_tbs ;
		      }
		    // increase counter
		    count++;
		}
    }
    cout << "Number of off clusters found in this row and sector: " << count << endl;
    cout << "Number of bad clusters found in this row and sector: " << badcluster << endl;
    cout << "Number of off clusters found in this event: " << sthit->GetNRows() << endl;
    return count;
}
//______________________
Int_t StClusterDisplayMaker::Get_l3on_points()
{
  return 1;
}
//______________________
Int_t StClusterDisplayMaker::Get_matched_points(Int_t& num_not_matched)
{
  // short anouncment
  cout << "Now do the matching " << endl ;

  // set verbose level
  Int_t output =2 ;

  // prepare cood-trans
  cout << endl << endl;
  St_l3_Coordinate_Transformer transformer ;
  transformer.Use_transformation_provided_by_db();
  transformer.Print_parameters();
  St_l3_xyz_Coordinate XYZ(0,0,0) ;
  St_l3_ptrs_Coordinate PTRS(0,0,0,0) ;

  // get offline points
  St_tcl_tphit* st_tcl_hits = (St_tcl_tphit*) GetDataSet("bfc/.make/tpc/.make/tpc_hits/.data/tphit");
  tcl_tphit_st* tcl_hits = (tcl_tphit_st*) st_tcl_hits->GetTable() ;
    if (!tcl_hits) 
	{
	    cout << "No offline points." << endl ;
	    return kStOK;
	}
    else
	{
	    cout << st_tcl_hits->GetNRows() << " offline hits found." << endl ;
	}

    // get l3 points
    St_tcl_tphit* st_l3_hits = (St_tcl_tphit*) GetDataSet("bfc/.make/l3/.make/l3Clufi/.data/L3hit");
    tcl_tphit_st* l3_hits = (tcl_tphit_st*) st_l3_hits->GetTable() ;
    if (!l3_hits) 
	{
	    cout << "No l3 points." << endl ;
	    return kStOK;
	}
    else
	{
	    cout << st_l3_hits->GetNRows() << " l3 hits found." << endl ;
	}
    
    // prepare arrays
    Int_t max_number_of_points=1000;
    matched_points_pad = new Float_t[max_number_of_points];
    matched_points_time = new Float_t[max_number_of_points];
    matched_points_pad_err = new Float_t[max_number_of_points];
    matched_points_time_err = new Float_t[max_number_of_points];
    not_matched_points_pad = new Float_t[max_number_of_points];
    not_matched_points_time = new Float_t[max_number_of_points];

    // sort l3points
    TString colName = "row";
    St_TableSorter sorter(st_l3_hits,colName,0,0); 

    // counters
    Int_t num_matched = 0 ;
    Int_t num_off = 0 ;
    Int_t num_l3 = 0 ;
    Int_t badcluster = 0 ;
        
    ////
    // loop over offline points
    ////
    for(Int_t offindex = 0 ; offindex < st_tcl_hits->GetNRows() ; offindex++ )
	{
	    // row!=0 do the matching just for the requested row
	    if( 
	       ( row==(tcl_hits[offindex ].row)%100 && 
		 sec==((tcl_hits[offindex ].row)-(tcl_hits[offindex ].row)%100)/100 ) )
		{

		  // check for bad coordinates
		  if (tcl_hits[offindex].z < 0 && sec<=12 )  { badcluster++ ; continue ; }
		  if (tcl_hits[offindex].z > 0 && sec>12 )  { badcluster++ ; continue ; }

		  // increase counter
		  num_off++;
		  
		  if (output>1)
		    {
		      cout << "off : " << offindex+1 ;
		      cout << "\t row : " <<  tcl_hits[offindex ].row ;
		      cout << "\t x : " <<  tcl_hits[offindex ].x ;
		      cout << "\t y : " <<  tcl_hits[offindex ].y ;
		      cout << "\t z : " <<  tcl_hits[offindex ].z ;
		      cout << "\t q : " <<  tcl_hits[offindex ].q << endl;
		    }
	    
		  ////
		  // loop over l3points in this row (on the same sector !)
		  ////
		  //Int_t first_index_in_sorted = sorter[tcl_hits[offindex ].row] ;
		  Int_t first_index_in_sorted = sorter.FindFirstKey(&(tcl_hits[offindex ].row)) ;
		  Int_t number_of_l3h_this_row = sorter.CountKey(&(tcl_hits[offindex ].row),0,1,0) ;
		  Double_t best_value = 100 ;
		  tcl_tphit_st* best_match = NULL ;
		  Double_t MAX_DIS_Z = 3 ;
		  Double_t MAX_DIS_SQUARE = 9 ;
		  Double_t best_dx = 10 ;
		  Double_t best_dy = 10 ;
		  Double_t best_dz = 10 ;
		  Double_t best_dis = 10 ;

		    for(Int_t l3index = 0 ; l3index < number_of_l3h_this_row ; l3index++ )
			{
			    Int_t original_index = sorter.GetIndex(first_index_in_sorted+l3index) ;
			    Double_t dz = tcl_hits[offindex ].z - l3_hits[original_index].z ;
			    if( fabs(dz) < MAX_DIS_Z )
				{ 
				    Double_t dy = tcl_hits[offindex ].y - l3_hits[original_index].y ;
				    Double_t dx = tcl_hits[offindex ].x - l3_hits[original_index].x ;
 				    Double_t dis = ( pow(dx,2) + pow(dy,2) + pow(dz,2) ) ;
				    if ( dis < MAX_DIS_SQUARE ) 
					{
					    if (dis < best_value)
						{
						    best_match = &l3_hits[original_index] ;
						    best_dx = dx ;
						    best_dy = dy ;
						    best_dz = dz ;
						    best_dis = dis ;
						}
					} // MAX_DIS_SQUARE 
				} // MAX_DIS_Z
			}
		    
		    if (output>1 && best_match)
			{
			  cout << "num of clu this padrow : " <<  number_of_l3h_this_row << endl;
			  cout << "l3     row : " << best_match->row ;
			  cout << "\t x : " <<  best_match->x ;
			  cout << "\t y : " <<  best_match->y ;
			  cout << "\t z : " <<  best_match->z ;
			  cout << "\t q : " <<  best_match->q << endl;
			}
	    
		    
		    if (best_match)
		      {
			// caclulate mean of off and l3 point
			float x = (best_match->x + tcl_hits[offindex ].x) / 2;
			float y = (best_match->y + tcl_hits[offindex ].y) / 2;
			float z = (best_match->z + tcl_hits[offindex ].z) / 2;
			
			// get pad time coordinate of this point
			XYZ.Setx(x) ;
			XYZ.Sety(y) ;
			XYZ.Setz(z) ;
			transformer.global_to_raw(XYZ,PTRS) ;
			  
			// fill into histo
			if (num_matched<max_number_of_points)
			  {
			    // !!!! add 0.5 because tb 338.0 in reality = 338.5 in histo !!!! 
			    matched_points_pad[num_matched] = (Float_t) (PTRS.Getp()) ;
			    matched_points_pad_err[num_matched] =1;
			    matched_points_time[num_matched] = (Float_t) (PTRS.Gett()) ;
			    matched_points_time_err[num_matched] =  1 ;
			  }
			num_matched++;
		      }
		    else
		      {
			// get pad time coordinate of this not matched point
			XYZ.Setx(tcl_hits[offindex ].x) ;
			XYZ.Sety(tcl_hits[offindex ].y) ;
			XYZ.Setz(tcl_hits[offindex ].z) ;
			transformer.global_to_raw(XYZ,PTRS) ;
			if (num_not_matched<max_number_of_points)
			  {
			    not_matched_points_pad[num_not_matched] = (Float_t) (PTRS.Getp()) ;
			    not_matched_points_time[num_not_matched] = (Float_t) (PTRS.Gett()) ;
			    num_not_matched++;
			  }
		      }
		}// row!=0 do the matching just for the requested row  
	} // loop over offline points
    
	    cout << "number of examined off clusters : "<<   num_off << endl;
    cout << "number of bad off clusters : "<<  badcluster  << endl;
    cout << "found matches : " <<  num_matched << " (" << (double) (num_matched) / (double)(num_off) << ")" <<endl;
    cout << "not matched clusters : " <<  num_not_matched << endl ;
    return num_matched;

}
//______________________
void StClusterDisplayMaker::Fill_Pixel_into_histo_artificial(TH2S* histo, Int_t row, Int_t sec)
{
  histo->Fill(10,10,5) ;
  histo->Fill(10,11,10) ;
  histo->Fill(10,12,8) ;
  histo->Fill(11,9,8) ;
  histo->Fill(11,10,15) ;
  histo->Fill(11,11,110) ;
  histo->Fill(11,12,18) ;
  histo->Fill(11,13,5) ;
  histo->Fill(12,10,25) ;
  histo->Fill(12,11,50) ;
  histo->Fill(12,12,28) ;
}
