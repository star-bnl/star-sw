// Author : Dominik Flierl 
// $Id: StClusterDisplayMaker.cxx,v 1.5 2000/09/15 21:20:20 fisyak Exp $
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
#include "TMarker.h"
#include "TGraphErrors.h"
#include "TPaveLabel.h"
#include "TArrow.h"
#include "tables/St_raw_sec_m_Table.h"
#include "tables/St_raw_pad_Table.h"
#include "tables/St_raw_row_Table.h"
#include "tables/St_raw_seq_Table.h"
#include "tables/St_type_shortdata_Table.h"
#include "tables/St_tcl_tphit_Table.h"
#include "St_l3Clufi_Maker/St_l3Clufi_Maker.h"
#include "St_l3_Coordinate_Transformer.h"
#include "St_l3_Coordinates.h"
#include "St_TableSorter.h"
#include "TLine.h"
#include "TString.h"
#include "l3/St_l3Clufi_Module.h"
#include "l3/St_l3totphit_Module.h"
#include "StDAQMaker/StDAQReader.h"
#include "StDaqLib/L3/L3_Reader.hh"

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
Int_t StClusterDisplayMaker::Make(Int_t isec , Int_t irow ,Char_t* c, Int_t ipad_min, Int_t ipad_max, Int_t itime_min, Int_t itime_max)
{
  // init parameters
  sec = isec ;
  row = irow ;
  pad_max = ipad_max ;
  pad_min = ipad_min ;
  time_max = itime_max ;
  time_min = itime_min ;
  

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

  // Fill pixel for this padrow into 2dhisto
  Fill_Pixel() ;
  
  // create canvas and plot histo
  Char_t cluscock[30];
  sprintf ( cluscock, "Sec_%1d_Row_%1d", sec,row );
  TCanvas* can = new TCanvas(cluscock,"ClusterCockpit",600,500) ;
  can->ToggleEventStatus();
  myhist->SetStats(0) ;
  myhist->Draw("Colz") ;
  
  // NO delete because I want to see this stuff later in my histo!

  /////
  // l3 points
  /////
  // graph with l3off points
  if (option.Contains("-l3highoff"))
    {
      Int_t t = PlotL3OffPoints("charge") ;
      if (option.Contains("-l3highoffDecon"))
	 {
	   t = PlotL3OffPoints("timedecon charge") ;
	   t = PlotL3OffPoints("paddecon charge") ;
	 }
    } 
  // graph with l3off points
  if (option.Contains("-l3off"))
    {  
      Int_t t = PlotL3OffPoints("") ;
      if (option.Contains("-l3offDecon"))
	{
	  t = PlotL3OffPoints("timedecon") ;
	  t = PlotL3OffPoints("paddecon") ;
	}
    } 
  // graph with l3online points
  if (option.Contains("-l3on"))
    {
      Int_t numPoints = PlotL3OnPoints("") ;
      if (option.Contains("-l3onDecon"))
	{
	  numPoints = PlotL3OnPoints("timedecon") ;
	  numPoints = PlotL3OnPoints("paddecon") ;
	}
    }

  
  /////////
  // offline points
  ////////
  // graph with ALL off points
   const Int_t maxNumberOfPoints = 1000 ;
  if (option.Contains("-off"))
    {
      // NO delete because I want to see this stuff later in my histo!
      Float_t offPointsPad[maxNumberOfPoints]  ;  
      Float_t offPointsTime[maxNumberOfPoints]  ; 
      Float_t offPointsPadErr[maxNumberOfPoints] ;
      Float_t offPointsTimeErr[maxNumberOfPoints] ;
	  
      Int_t noOffPoints = getOffPoints(offPointsPad,offPointsPadErr,
				       offPointsTime,offPointsTimeErr,
				       maxNumberOfPoints,"all") ;
	
      TGraphErrors* offPointsGraph  = new TGraphErrors(noOffPoints,
						       offPointsPad,offPointsTime,
						       offPointsPadErr,offPointsTimeErr) ;
      offPointsGraph->SetMarkerStyle(24) ;
      offPointsGraph->SetMarkerColor(3) ;
      offPointsGraph->SetMarkerSize(2) ;
      if (noOffPoints>0)
	{
	  offPointsGraph->Draw("P") ;
	}
	
      TLine* maxTimeBucketLine =  new TLine(0,Max_time_bucket_off,pad_max,Max_time_bucket_off) ;  
      maxTimeBucketLine->SetLineColor(3) ;
      maxTimeBucketLine->Draw() ;
    }

  // graph with off points not on a track (trackid==0)
  if (option.Contains("-notontrack"))
    {
      Float_t offPointsPad[maxNumberOfPoints] ;   
      Float_t offPointsTime[maxNumberOfPoints] ;  
      Float_t offPointsPadErr[maxNumberOfPoints] ; 
      Float_t offPointsTimeErr[maxNumberOfPoints] ; 
	
      Int_t noOffPoints = getOffPoints(offPointsPad,offPointsPadErr,
				       offPointsTime,offPointsTimeErr,
				       maxNumberOfPoints,"offtrack") ;
	
      TGraphErrors* offPointsGraph  = new TGraphErrors(noOffPoints,
						       offPointsPad,offPointsTime,
						       offPointsPadErr,offPointsTimeErr) ;
      offPointsGraph->SetMarkerStyle(24) ;
      offPointsGraph->SetMarkerColor(6) ;
      offPointsGraph->SetMarkerSize(2) ;
      if (noOffPoints>0)
	{
	  offPointsGraph->Draw("P") ;
	}
    }
  
  // graph with off points with hitflag == 1
  if (option.Contains("-hitflag"))
    {
      Float_t offDeconPointsPad[maxNumberOfPoints] ;
      Float_t offDeconPointsPadErr[maxNumberOfPoints] ;
      Float_t offDeconPointsTime[maxNumberOfPoints] ;
      Float_t offDeconPointsTimeErr[maxNumberOfPoints] ;
      Int_t noOffDeconPoints = getOffPoints(offDeconPointsPad,offDeconPointsPadErr,
					    offDeconPointsTime,offDeconPointsTimeErr,
					    maxNumberOfPoints,"hitflag") ;
      
      TGraphErrors* offDeconPointsGraph = new TGraphErrors(noOffDeconPoints,
							   offDeconPointsPad,offDeconPointsTime,
							   offDeconPointsPadErr,offDeconPointsTimeErr);
      offDeconPointsGraph->SetMarkerStyle(25);
      offDeconPointsGraph->SetMarkerColor(3);
      offDeconPointsGraph->SetMarkerSize(2);
      if (noOffDeconPoints>0)
	{
	  offDeconPointsGraph->Draw("P");
	}
    }
  
  ///////
  // matching
  ///////

  // graph with matching online with offline points
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

  ////
  // some explanationary stuff to be displayed
  ////
  // space distance 
  if (option.Contains("-scale"))
    {
	displayScale();
    }

  // legend
  if (option.Contains("-info"))
    {
	displayInfo() ;
    }
  
  // go home
  return kStOK;
}
 //__________________________________________________________________
void StClusterDisplayMaker::help()
{
    cout << "The currently supported options are :\n" ;
    cout << "-off        :  all points found by tcl\n" ;
    cout << "-notontrack :  color those points which have trackid==0\n" ;
    cout << "-hitflag    :  color those points which have flag==1\n" ;
    cout << endl ;
    cout << "-l3off  : display l3offline points\n" ;
    cout << "-mat    : display matching between l3offline and offline points\n" ;
    cout << "-notmat : display offline points which were not matched to a l3offpoint\n" ;
    cout << endl ;
    cout << "-scale : display scale in cm \n" ;
    cout << "-info  : display explantion of Markerstyle and Colors \n" ;
    cout << "example :  dis->Make(3,45,\"-off -l3off -hitflag -notontrack -scale\",0,144,0,400)\n " ;
    cout << " sector=3   row=45  pad=0-144  time0-400\n" ;
    cout << endl;

}
 //__________________________________________________________________
void StClusterDisplayMaker::displayInfo()
{
 TCanvas* can2 = new TCanvas("info","legend",300,600) ;
      can2->Draw() ;
      Double_t numb = 5 ; // number of items
      for (Int_t index=0 ; index<numb; index++)
	{
	  Double_t y1 = 0.1 + index * ((0.8-0.1)/(numb-1)) ; 
	  Double_t y2 = y1 + 1/(2*numb) ;
	  TString explain ; //text
	  Int_t symb ; // symbol id
	  Int_t col ; // color
	    	 
	  switch (index)
	    {
	    case 0 : explain = "all off points" ; symb = 24 ; col = 3; break;
	    case 1 :  explain = "off point with trackid == 0" ; symb = 24  ; col = 6; break;
	    case 2 :  explain = "off point with flag == 1" ; symb = 25 ; col = 3 ; break;
	    case 3 :  explain = "l3offline point" ; symb = 28 ; col = 4 ; break;
	    case 4 :  explain = "l3online point" ; symb = 27 ; col = 2 ; break;
	    default:  explain = "empty" ; symb = 28 ; col = 0 ; break;
	    }
	     
	  TPaveLabel*  leg1 = new TPaveLabel(0.2,y1,0.9,y2,explain) ;
	  leg1->Draw() ;
	  TMarker* mark1 = new TMarker(0.1,(y1+y2)/2,symb) ;
	  mark1->SetMarkerSize(2) ;
	  mark1->SetMarkerColor(col) ;
	  mark1->Draw() ;
	}
}
//__________________________________________________________________
void StClusterDisplayMaker::displayScale()
{
    // Setup transformation
    St_l3_Coordinate_Transformer transformer ;
    transformer.Use_transformation_provided_by_db();
    St_l3_xyz_Coordinate XYZ1(0,0,0) ;
    St_l3_xyz_Coordinate XYZ2(0,0,0) ;
    St_l3_ptrs_Coordinate PTRS(0,0,0,0) ;
	 
	 // Get points in ptrs calculate difference in xyz draw them draw values
	 // pad scale
      Double_t pad1 = pad_min + (pad_max-pad_min)*0.85 ;
      Double_t pad2 = pad_min + (pad_max-pad_min)*0.95 ;
      Double_t time1 = time_min + (time_max-time_min)*0.1 ;
      PTRS.Setptrs(pad1 , time1 , row ,sec ) ;
      transformer.raw_to_global(PTRS,XYZ1) ;
      PTRS.Setptrs(pad2 , time1 , row ,sec ) ;
      transformer.raw_to_global(PTRS,XYZ2) ;
	 
	 
      TArrow* arrow1 = new TArrow(pad1,time1,pad2,time1,0.02,"<>") ;
      arrow1->SetLineColor(1) ;
      arrow1->SetLineWidth(2) ;
      arrow1->Draw() ;
      double diff_pad = sqrt(
			     pow((XYZ1.Getx()-XYZ2.Getx()),2) + 
			     pow((XYZ1.Gety()-XYZ2.Gety()),2) + 
			     pow((XYZ1.Getz()-XYZ2.Getz()),2) );
      cout << "diff pad = " << diff_pad << endl;
	 
      Char_t padscale[30]; 
      sprintf ( padscale, "%.2f cm", diff_pad) ;
      Double_t time2 = time_min + (time_max-time_min)*0.01 ;
      Double_t time3 = time_min + (time_max-time_min)*0.08 ;

      TPaveLabel*  padscalelabel = new TPaveLabel(pad1,time2,pad2,time3,padscale);
      padscalelabel->SetFillColor(0) ;
      padscalelabel->SetTextSize(0.5) ;
      padscalelabel->SetBorderSize(0) ;
      padscalelabel->SetTextFont(13);
      padscalelabel->Draw() ;
	 
	 // timebucket scale
      St_l3_xyz_Coordinate XYZ3(0,0,0) ;
      St_l3_xyz_Coordinate XYZ4(0,0,0) ;
      Double_t time4 = time_min + (time_max-time_min)*0.15 ;
      Double_t time5 = time_min + (time_max-time_min)*0.25 ;

      PTRS.Setptrs(pad1 , time4 , row ,sec ) ;
      transformer.raw_to_global(PTRS,XYZ3) ;
      PTRS.Setptrs(pad1 , time5 , row ,sec ) ;
      transformer.raw_to_global(PTRS,XYZ4) ;
	 
      TArrow* arrow2 = new TArrow(pad1,time4,pad1,time5,0.02,"<>") ;
      arrow2->SetLineColor(1) ;
      arrow2->SetLineWidth(2) ;
      arrow2->SetAngle(90) ;
      arrow2->Draw();
      double diff_time = sqrt(
			      pow((XYZ4.Getx()-XYZ3.Getx()),2) + 
			      pow((XYZ4.Gety()-XYZ3.Gety()),2) + 
			      pow((XYZ4.Getz()-XYZ3.Getz()),2) );
      Char_t timescale[30]; 
      sprintf ( timescale, "%.2f cm", diff_time) ;
      Double_t pad3 = pad_min + (pad_max-pad_min)*0.77 ; 
      Double_t pad4 = pad_min + (pad_max-pad_min)*0.83 ;
      TPaveLabel*  timescalelabel = new TPaveLabel(pad3,time4,pad4,time5,timescale);
      timescalelabel->SetFillColor(0) ;
      timescalelabel->SetTextSize(0.4) ;
      timescalelabel->SetBorderSize(0) ;
      timescalelabel->SetTextAngle(90);
      timescalelabel->SetTextFont(13);
      timescalelabel->Draw() ;

      cout << "diff time = " << diff_time << endl;
}
//__________________________________________________________________
void StClusterDisplayMaker::Fill_Pixel()
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
      St_DataSet *sector = rawdata(sectorname) ;
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
Int_t StClusterDisplayMaker::Get_l3off_points()
{
  // prepare cood-trans
  cout << endl << endl;
  St_l3_Coordinate_Transformer transformer ;
  //transformer.Use_transformation_provided_by_db();
  transformer.Print_parameters() ;
  St_l3_xyz_Coordinate XYZ(0,0,0) ;
  St_l3_ptrs_Coordinate PTRS(0,0,0,0) ;
  if (row<=13)
    {
      Max_time_bucket_l3off = transformer.Get_max_timebucket_inner() ;
    }
  else
    {
      Max_time_bucket_l3off = transformer.Get_max_timebucket_outter() ;
    }
  cout << "max tb inner " << transformer.Get_max_timebucket_inner() << endl; 
  cout << "max tb outer " << transformer.Get_max_timebucket_outter() << endl;
	
       
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
  //St_tcl_tphit* sthit = (St_tcl_tphit*) GetDataSet("bfc/.make/l3Chain/.make/l3Clufi/.data/L3hit");
  St_tcl_tphit* sthit = (St_tcl_tphit*) GetDataSet("L3hit");
  tcl_tphit_st* myl3offhit = (tcl_tphit_st*)  sthit->GetTable();
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
Int_t StClusterDisplayMaker::getOffPoints(Float_t* padvec, Float_t* padvecerr, Float_t* timevec, Float_t* timevecerr, Int_t max_number_of_points, Char_t* off_on_track)
{
  // get option
  TString off_on_option(off_on_track) ;

  // prepare cood-trans
  cout << endl << endl;
  St_l3_Coordinate_Transformer transformer ;
  transformer.Use_transformation_provided_by_db();
  transformer.Print_parameters();
  St_l3_xyz_Coordinate XYZ(0,0,0) ;
  St_l3_ptrs_Coordinate PTRS(0,0,0,0) ;
  
  if(row<=13)
    {
      Max_time_bucket_off = transformer.Get_max_timebucket_inner() ;
    }
  else
    {
      Max_time_bucket_off = transformer.Get_max_timebucket_outter() ;
    }

  cout << "max tb inner " << transformer.Get_max_timebucket_inner() << endl; 
  cout << "max tb outer " << transformer.Get_max_timebucket_outter() << endl;
  
  // loop over points
  St_tcl_tphit* sthit = (St_tcl_tphit*) GetDataSet("tphit");
  if (!sthit)
    {
      cout << "sorry no off points found" << endl ;
      return 0;
    }
  tcl_tphit_st* myoffhit = (tcl_tphit_st*) sthit->GetTable();
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

	  // switch between on/off track
	  if (off_on_option.Contains("ontrack") && myoffhit[i].track == 0 ) { continue ; }
	  if (off_on_option.Contains("offtrack") && myoffhit[i].track != 0 ) { continue ; }
	  if (off_on_option.Contains("hitflag") && myoffhit[i].flag == 0 ) { continue ; }

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
	      padvec[count] = (Float_t) (PTRS.Getp()) ;
	      padvecerr[count] = (Float_t) (myoffhit[i].npads/2) ;

	      timevec[count] = ((Float_t) PTRS.Gett()) ;
	      Float_t n_tbs = (myoffhit[i].maxtmbk - myoffhit[i].mintmbk)/2  ;
	      timevecerr[count] = (Float_t) n_tbs ;
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
Int_t StClusterDisplayMaker::PlotL3OnPoints(TString option)
{
    // prepare arrays
    Int_t maxNumberOfPoints    = 1000 ;
    Float_t* l3onPointsPad     = new Float_t[maxNumberOfPoints]  ;  
    Float_t* l3onPointsTime    = new Float_t[maxNumberOfPoints]  ;
    Float_t* l3onPointsPadErr  = new Float_t[maxNumberOfPoints]  ;
    Float_t* l3onPointsTimeErr = new Float_t[maxNumberOfPoints]  ;

    TDataSet*    DAQReaderSet = GetDataSet("StDAQReader") ;
    StDAQReader* daqReader    = (StDAQReader*)(DAQReaderSet->GetObject()) ;
    StL3Reader*  ml3reader    = daqReader->getL3Reader() ;
    Int_t clusnumb = 0 ;
    Int_t sector ;
    if (sec%2 ==0){sector=sec-1;} else {sector=sec;} ;
    
    if (ml3reader->getI960ClusterReader(sector)->getClusterList())
	{
	    cout << "Found some i960 clusters in sector:" << sector <<endl ;
	    l3_cluster* myl3cluster = ml3reader->getI960ClusterReader(sector)->getClusterList();
	    Int_t numOfClusters = ml3reader->getI960ClusterReader(sector)->getNumberOfClusters() ;
	    for (Int_t clindex=0;  clindex<numOfClusters ; clindex++)
		{
		    // take only the right sector out of a supersector
		    Int_t RBMZ = (Int_t)(myl3cluster[clindex].RB_MZ) ;
		    Int_t rb   = RBMZ >> 4 ;
		    if ( ((sec%2 == 0) && (rb>6)) || ((sec%2 == 1) && (rb<=6)) )
			{
			    // take only the right row
			    Int_t mrow  = (Int_t)(myl3cluster[clindex].padrow) ;
			    if(mrow == row)
				{
				    // take only the right flag
				    if (option.Contains("timedecon") && ((myl3cluster[clindex].flags & 4) == 0)) { continue ; } 
				    if (option.Contains("paddecon") && ((myl3cluster[clindex].flags & 2) == 0)) { continue ; }
				    l3onPointsPad[clusnumb]  = ((Double_t)(myl3cluster[clindex].pad)) / 64 ; 
				    l3onPointsTime[clusnumb] = ((Double_t)(myl3cluster[clindex].time)) / 64 ;
				    l3onPointsPadErr[clusnumb] = 0 ;
				    l3onPointsTimeErr[clusnumb] = 0 ;
				    clusnumb++;
				}
			}
		}
	}


    // create graph
    TGraphErrors*  l3OnPointsGraph  = new TGraphErrors( clusnumb,
							l3onPointsPad,l3onPointsTime,
							l3onPointsPadErr,l3onPointsTimeErr );
    Int_t style = 27 ;
    if (option.Contains("timedecon") ){ style = 25; }
    if (option.Contains("paddecon") ) { style = 24; }
    l3OnPointsGraph->SetMarkerStyle(style);
    l3OnPointsGraph->SetMarkerColor(2);
    l3OnPointsGraph->SetMarkerSize(2);
    l3OnPointsGraph->Draw("P");
    
    return clusnumb ;
			    
}
//______________________
Int_t StClusterDisplayMaker::PlotL3OffPoints(TString option)
{
  // prepare cood-trans
  cout << endl << endl;
  St_l3_Coordinate_Transformer transformer ;
  //transformer.Use_transformation_provided_by_db();
  transformer.Print_parameters() ;
  St_l3_xyz_Coordinate XYZ(0,0,0) ;
  St_l3_ptrs_Coordinate PTRS(0,0,0,0) ;
  if (row<=13)
    {
      Max_time_bucket_l3off = transformer.Get_max_timebucket_inner() ;
    }
  else
    {
      Max_time_bucket_l3off = transformer.Get_max_timebucket_outter() ;
    }
         
  // prepare arrays
  Int_t max_number_of_points = 1000;
  Float_t* l3offpointsPad     = new Float_t[max_number_of_points] ;
  Float_t* l3offpointsTime    = new Float_t[max_number_of_points] ;
  Float_t* l3offpointsPadErr  = new Float_t[max_number_of_points] ;
  Float_t* l3offpointsTimeErr = new Float_t[max_number_of_points] ;

  // counter
  Int_t badcluster = 0 ;

  // loop over points
  St_tcl_tphit* sthit = (St_tcl_tphit*) GetDataSet("L3hit");
  tcl_tphit_st* myl3offhit = (tcl_tphit_st*)  sthit->GetTable();
  if(!myl3offhit)
    {
      cout << "sorry no l3off data found" << endl ;
      return 0;
    }
  cout << "l3 clusters found in this sec and row. "<< endl;
  Int_t count = 0;
  for(Int_t i=0;i<sthit->GetNRows();i++)
    {
      if( (myl3offhit[i].row-myl3offhit[i].row%100) / 100 == sec && myl3offhit[i].row%100 == row )
	{
	  // exclude unreasonable timebuckets
	  if (myl3offhit[i].z < 0 && sec<=12 ) { badcluster++ ; continue ; }
	  if (myl3offhit[i].z > 0 && sec>12 )  { badcluster++ ; continue ; }

	  // check flag
	  if (option.Contains("timedecon") && ((myl3offhit[i].flag & 4) == 0)) { continue ; } 
	  if (option.Contains("paddecon") && ((myl3offhit[i].flag & 2) == 0)) { continue ; }
	  if (option.Contains("charge") && (myl3offhit[i].q<80)) { continue ; }
	  		    
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
	  cout << "f :" << myl3offhit[i].flag <<"\t";
	  cout << "pad :" << PTRS.Getp()  <<"\t";
	  cout << "time :" << PTRS.Gett() << endl;
		    
	  if (count<max_number_of_points)
	    {
	      l3offpointsPad [count]    = (Float_t) (PTRS.Getp()) ;
	      l3offpointsTime[count]   = (Float_t) (PTRS.Gett()) ;
	      l3offpointsPadErr[count]  = 0 ;
	      l3offpointsTimeErr [count] = 0 ;
	    }
	  // increase counter
	  count++;
	}
    }
  // some output
  cout << "Number of l3 clusters found in this row and sector: " << count << endl;
  cout << "Number of bad clusters found in this row and sector: " << badcluster << endl;
  cout << "Number of l3 clusters found in this event: " << sthit->GetNRows() << endl;
   
  // plot graph
  TGraphErrors *graphL3Off  = new TGraphErrors(count,
					       l3offpointsPad,l3offpointsTime,
					       l3offpointsPadErr,l3offpointsTimeErr);
  Int_t style = 28 ;
  if (option.Contains("timedecon") ){ style = 25; }
  if (option.Contains("paddecon") ) { style = 24; }
  graphL3Off->SetMarkerStyle(style);
  graphL3Off->SetMarkerColor(4);
  graphL3Off->SetMarkerSize(2);
  graphL3Off->Draw("P");

  // plot max tb
  TLine* max1 = new TLine(pad_min,Max_time_bucket_l3off,pad_max ,Max_time_bucket_l3off) ;
  max1->SetLineColor(4);
  max1->Draw() ;

  return count;
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
  St_tcl_tphit* st_tcl_hits = (St_tcl_tphit*) GetDataSet("bfc/.make/tpcChain/.make/tpc_hits/.data/tphit");
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
  St_tcl_tphit* st_l3_hits = (St_tcl_tphit*) GetDataSet("bfc/.make/l3Chain/.make/l3Clufi/.data/L3hit");
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
