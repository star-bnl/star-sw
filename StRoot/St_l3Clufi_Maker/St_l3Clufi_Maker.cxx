//*-- Author : Victor Perevoztchikov
// 
// $Id: St_l3Clufi_Maker.cxx,v 1.15 2000/04/12 18:39:21 flierl Exp $
// $Log: St_l3Clufi_Maker.cxx,v $
// Revision 1.15  2000/04/12 18:39:21  flierl
// check whether enough memory is allocated for the clusters ( for the tables and the buffers )
//
// Revision 1.14  2000/03/24 19:45:16  flierl
// correct looping over pixels during filling of stpixel
//
// Revision 1.13  2000/03/21 20:33:09  flierl
// change output
//
// Revision 1.12  2000/03/03 22:42:52  flierl
// Improved coordinate transformation.
//
// Revision 1.11  2000/02/24 01:55:24  flierl
// i960 timing built in.
// output just with debug option.
// pixelarray has now space for 512 timebuckets.
//
// Revision 1.10  2000/02/10 20:43:47  flierl
// new files to provide classes which fill banks into tphits. this was done
// before by l3totphit in pams
//
// Revision 1.9  2000/01/28 20:41:35  flierl
// bug fixed :  St_hit_bank[supersectorindex-1];
//
// Revision 1.8  2000/01/28 18:51:26  flierl
// delete Stpixel .... created memory leak
//
// Revision 1.7  2000/01/26 22:31:25  flierl
// make cc5 happy. change char to cons char
//
// Revision 1.6  2000/01/20 14:36:23  flierl
// change return value from kStErr to kStWarn to avoid stalling of the whole chain
//
// Revision 1.5  1999/12/16 21:01:31  flierl
// feed tracker with banks instead of tcl_tphit structs
//
// Revision 1.4  1999/12/07 23:13:52  flierl
// histogramms created and filled
//
// Revision 1.3  1999/11/29 23:08:12  flierl
// minor changes in variable names
//
// Revision 1.2  1999/11/29 21:50:49  fisyak
// Fix compilation on Sun
//
// Revision 1.1.1.1  1999/11/19 18:31:48  flierl
// test
//
// Revision 1.12  1999/07/15 13:57:44  perev
// cleanup
//
// Revision 1.11  1999/07/10 22:59:16  fine
// Some comments have been introduced to show html docs
//
//////////////////////////////////////////////////////////////////////////
//  
//    here comes what l3 does ....
//                                                                    //
// St_TLA_Maker class for Makers                                        //
//                                                                      //
// This commented block at the top of the source file is considered as  //
// this class description to be present on the this class Web page.     //
//  see: begin_html <A HREF="http://root.cern.ch/root/Documentation.html"> ROOT HTML documentation </A> end_html                //
//                                                                      //
//  Submit any problem with this code via begin_html <A HREF="http://www.star.bnl.gov/STARAFS/comp/sofi/bugs/send-pr.html"><B><I>"STAR Problem Report Form"</I></B></A> end_html   //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "St_l3Clufi_Maker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "St_DataSet.h"
#include "tables/St_raw_sec_m_Table.h"
#include "tables/St_raw_pad_Table.h"
#include "tables/St_raw_row_Table.h"
#include "tables/St_raw_seq_Table.h"
#include "tables/St_tcl_tphit_Table.h"
#include "tables/St_type_shortdata_Table.h"
#include "l3/St_l3Clufi_Module.h"
#include "l3/St_l3totphit_Module.h"
#include "tables/St_pixelarray_Table.h"
#include "tables/St_hitarray_Table.h"
#include "TH1.h"
#include "St_l3banks_2_tphits.h"
#include "TStopwatch.h"
#include <string.h>
#include "St_l3_Coordinates.h"
#include "St_l3_Coordinate_Transformer.h"

extern TStopwatch i960[18];

ClassImp(St_l3Clufi_Maker)
  //_____________________________________________________________________________
  St_l3Clufi_Maker::St_l3Clufi_Maker(const char *name):StMaker(name){
  //  l3Clufi constructor
  //
  //  const char *name -  the name of this constructor
  //
  //  The first comment lines after the opening bracket
  //  ({) of a member function are considered as a member function description 
  //  see: begin_html <A HREF="http://root.cern.ch/root/Documentation.html"> ROOT HTML documentation </A> end_html   //
}
//_____________________________________________________________________________
St_l3Clufi_Maker::~St_l3Clufi_Maker(){
  // This l3Clufi destructor
  //
  //  The first comment lines after the opening bracket
  //  ({) of a member function are considered as a member function description 
  //
  //  The first comment lines after the opening bracket
  //  ({) of a member function are considered as a member function description 
  //  see: begin_html <A HREF="http://root.cern.ch/root/Documentation.html"> ROOT HTML documentation </A> end_html   //
}
//_____________________________________________________________________________
Int_t St_l3Clufi_Maker::Init(){
  //  Init - is a first method the top level StChain calls to initialize all its makers
  //
  //  The first comment lines after the opening bracket
  //  ({) of a member function are considered as a member function description 
  //  see: begin_html <A HREF="http://root.cern.ch/root/Documentation.html"> ROOT HTML documentation </A> end_html   //

  // Create tables
  St_DataSetIter       local(GetDataBase("params"));
  // Create Histograms 
  x_dis = new TH1F("L3ClufiTphitx","x coordinate of hits",400,-200,200);
  y_dis = new TH1F("L3ClufiTphity","y coordinate of hits",400,-200,200);
  z_dis = new TH1F("L3ClufiTphitz","z coordinate of hits",400,-400,400);
  charge_dis =  new TH1F("L3ClufiTphitcharge","charge of hits",40,-10,10);
  i960_time = new TH1D("times","times",18,1,19);
  for (Int_t id=0; id<18 ;id++) i960[id].Reset();
    
  // set max values for pixel array
  // these values must be equal to those in the module (in croat.h)
  Max_number_of_rows = 45;
  Max_number_of_pads = 184;
  Max_number_of_buckets = 512;

  // define buffer_size
  Buffer_size = 100000 ;

  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_l3Clufi_Maker::Make(){
  //
  //  this maker does :
  //  get tpc raw data
  //  prepare it to be handeld by the online clusterfinder (croat.c)
  //  call online clusterfinder
  //  produce output in online format 
  //  convert online format to tpt_points
  //  write tpt_points out
  //  
    

  // here we start
  cout << "Now we start l3Clufi Maker." << endl;
     
  // check time consumption of different parts of the maker
  TStopwatch timer1 ; 
  TStopwatch timer2 ; 
  TStopwatch timer3 ; 
  if (Debug()) { timer1.Reset() ; timer2.Reset() ; timer3.Reset() ; }
  
  // some counters
  Int_t totalpixelcount = 0 ;

     
  /////////
  // Get raw data 
  /////////
  raw_data_tpc = (St_DataSet*) GetInputDS("tpc_raw");
  if (Debug()) { timer1.Start() ; } 

  // do we have something ? 
  if (!raw_data_tpc)
    { 
      cerr << "No raw data found, l3 clusterfinding skipped ! " << endl;
      return kStWarn;
    } 
  else if (raw_data_tpc) 
    { 
      // what did we get ?
      //cout << endl << "We get some raw data." << endl << endl; 
      //raw_data_tpc->ls("*");
	
      // iterator of raw_data_tpc to navigate through it
      St_DataSetIter next(raw_data_tpc);

      // allocate memory for raw pixel data 
      Stpixel = (St_pixelarray*) new St_pixelarray("pixel",Max_number_of_rows*Max_number_of_pads*Max_number_of_buckets);
      pixelst = (pixelarray_st*) Stpixel->GetTable();

      // loop over sectors 
      Int_t sectorindex = 0; 
      while ( (sector = next()) && sectorindex <= 24 )
	{
	  // look if St_DataSet N is really a sector, (trial an error to get the sector id ...)
	  const Char_t *name;
	  if ((name = strstr(sector->GetName(),"Sector"))) 
	    {
	      // lets look which sector we got (sectorindex = xxx)
	      Char_t* sectorname = (Char_t*) sector->GetName();
	      if (sectorname[7] >= '1' || sectorname[7] <= '9')
		{
		  // sectorindex goes from 1 to 24
		  sectorindex = (Int_t) atoi(&(sectorname[7]));
		  //cout << "Processing sector : " << sectorindex << endl;
		}
	      else 
		{
		  cerr << " Bad raw data in sector " << sectorindex+1 << endl;
		  // stop this maker and go back to bfc
		  return kStWarn;
		}
			
	      ///////
	      // to handle supersectors two sectors must be treated as one !
	      // create out array for the supersector 
	      ///////
	      Int_t supersectorindex;
	      if ( sectorindex%2 == 1 )
		{
		  // in this case we have got the first sector of the two sectors
		  // allocate out array for the TWO secotors that means
		  // for the supersector ! 
		  // supersectorindex goes from 1 to 12
		  supersectorindex = (sectorindex+1)/2;
		  Char_t output_name[16] = "hits_in_sec_00";
		  if (supersectorindex>0 && supersectorindex<10)
		    {
		      Char_t sec_char = 48+supersectorindex;
		      output_name[13] = sec_char;
		    }
		  else if ( supersectorindex < 13 && supersectorindex > 9 )
		    {
		      output_name[12] = '1';
		      Char_t sec_char = 48+(supersectorindex-10);
		      output_name[13] = sec_char;
		    }
		  // allocate the table where the out bank will go for this supersector
		  // 100000 longs (=4byte=online "word") we need 2 words per cluster
		  // -> we have enough space for max. 50000 clusters per sector
		  St_hit_bank[supersectorindex-1] = new St_hitarray(output_name,Buffer_size);
		  St_hit_bank_this = St_hit_bank[supersectorindex-1];
		  hit_bank_this_st = (hitarray_st*) St_hit_bank_this->GetTable();
		  // fill TPCSECLP bankheader with 1,3,5 ... or 23
		  hit_bank_this_st[3].data = (supersectorindex*2-1);
				   
		  // add out table to the main dataset visible with browser after bfc
		  m_DataSet->Add(St_hit_bank[supersectorindex-1]);
		  //m_DataSet->ls("*");

		}
	      else if ( sectorindex%2 == 0 )
		{
		  // in this case we got the second sector of one supersector
		  // it gets the same out array but the start point will be different !
		  supersectorindex = sectorindex/2;
		  St_hit_bank_this = St_hit_bank[supersectorindex-1]; 
		}
			    
	      //cout << "  Supersecotr : " << supersectorindex << endl;
			
	      //////
	      // fill pixel array for this sector
	      //////	  
	      // set pixel array 0 clumsey and slow but working 
	      if (Debug()) { timer3.Start(0) ; }
	      for(Int_t pixindex=0;pixindex<Max_number_of_rows*Max_number_of_pads*Max_number_of_buckets;pixindex++)
		{
		  pixelst[pixindex].data=0;
		}
	      // fill pixel array inner rows
	      if ( Fill_pixel_of_inner_rows() != 1 ) 
		{
		  cerr << "Unable to fill pixels of inner rows";
		  cerr << " in sector " << sectorindex <<endl;
		  return kStWarn;
		}
	      // fill pixel array outer rows
	      if ( Fill_pixel_of_outer_rows() != 1 )
		{
		  cerr << "Unable to fill pixels of outer rows";
		  cerr << " in sector " << sectorindex <<endl;
		  return kStWarn;
		}
	      if (Debug()) { timer3.Stop() ; }	
	

	      // just checking
	      if (Debug())
		{
		  Int_t pixelcount = 0;
		  for(Int_t Pixindex=0;Pixindex<Max_number_of_rows*Max_number_of_pads*Max_number_of_buckets;Pixindex++)
		    {
		      if (pixelst[Pixindex].data!=0) { pixelcount++;} 
		    }
		  cout << pixelcount << " Pixel found in sector " << sectorindex ;
		  totalpixelcount += pixelcount ;
		  cout << "       total Pixel found : " << totalpixelcount << endl ;
		}
			    
	      /////
	      // now lets do the clusterfinding for this sector
	      ////
	      //cout << "Call the clusterfinding module for the "<< sectorindex <<"sector. " << endl;
			   			
	      // call the clusterfinder module 
	      if (Debug()) { timer2.Start(0) ; }
	      if ( l3Clufi(Stpixel,St_hit_bank_this) !=1 )
		{
		  // clean up, stop this maker and go back to bfc
		  cerr << "Problems in L3 clusterfinding module in sector: "<< sectorindex <<endl;
		  delete Stpixel;
		  return kStErr;
		}
	      if (Debug()) { timer2.Stop() ; }

	      // write out tables in online format just for the supersector
	      if ( sectorindex%2 == 0 )
		{
		  for(Int_t tt=0;tt<Buffer_size;tt++)
		      {
			  St_hit_bank_this->AddAt(&hit_bank_this_st[tt],tt);
		      }
		}

	      ////// ---> goto next sector
	      //cout << "Done with this sector. " << endl;
	    }// if ((name = strstr(sector->GetName(),"Sector"))) 
	}// while ( (sector = next()) && sectorindex<12 )
      // free memory
      delete Stpixel;
     
    } //  if (raw_data_tpc) 
   
  // timing
  if (Debug()) { timer1.Stop() ; }
  

  /////////
  /// now fill /l3/hits_in_sec_xx banks in /l3/hit/tcl_tphit tables
  /////////
  cout << "Start filling banks into tables..." << endl;
    
  //creat tcl_tphits table
  St_tcl_tphit* stl3hit = new St_tcl_tphit("L3hit",500000);
  tcl_tphit_st* l3hitst = (tcl_tphit_st*) stl3hit->GetTable();
  m_DataSet->Add(stl3hit);
    
  //loop over hits_in_sec_xx 
  for(Int_t sec_index=1;sec_index<=12; sec_index++)
    {
      // get l3 dataset
      St_DataSet* sec_bank_set = GetInputDS("l3Clufi");
      // create iterator
      St_DataSetIter sec_bank_iter(sec_bank_set);
      Char_t secname[15] = "hits_in_sec_00";
      if ( sec_index < 10 )
	{
	  Char_t sec_char = 48+sec_index;
	  secname[13] = sec_char;
	}
      else if ( sec_index == 10 || sec_index == 11 || sec_index == 12 )
	{
	  secname[12] = '1';
	  Char_t sec_char = 48+(sec_index-10);
	  secname[13] = (Char_t) sec_char;
	}
      // get hit array (=bank)
      St_hitarray* bank_entries = (St_hitarray*)sec_bank_iter(secname);
      hitarray_st* bank_entries_st = (hitarray_st*) bank_entries->GetTable();
	
      // fill banks into tphit structs
      St_l3banks_2_tphits* filler = new St_l3banks_2_tphits(stl3hit,bank_entries);
      if (filler->Filltclpoints() == 0) 
	  {
	    // clean up and abort this event !
	      delete filler;
	      cout << "Too many clusters." << endl ;
	      return kStErr;
	  }
	      
      delete filler;

      //  call module which translates banks into tables old style
      // l3totphit(bank_entries,stl3hit);
    }

  // fill histogramms
  cout << "Number of clusters found by l3 :  " << stl3hit->GetNRows() << endl << endl ;
  for(Int_t tt=0; tt<stl3hit->GetNRows() && tt<500000; tt++)
    {
      // fill histogramms
      x_dis->Fill(l3hitst[tt].x);
      y_dis->Fill(l3hitst[tt].y);
      z_dis->Fill(l3hitst[tt].z);
      charge_dis->Fill(l3hitst[tt].q);
    }

  // fill i960 timer
  for (Int_t index1=1;index1<19;index1++ )
    {
      i960_time->Fill(index1,(Double_t)i960[index1-1].CpuTime());
      //i960_time->Fill(index1,(Float_t)index1);
      if (Debug())
	{
	  cout << "Time per i960 :  " << index1 << "\t" ;
	  cout << (Double_t)(i960[index1-1].RealTime()) << "\t";
	  cout << (Double_t)(i960[index1-1].CpuTime()) << endl;
	}
    }
 
  // timing output
  if (Debug())
    {
      cout << endl ;
      cout << "Timer1 : " ; timer1.Print() ; cout << endl ;
      cout << "Timer2 : " ; timer2.Print() ; cout << endl ;
      cout << "Timer3 : " ; timer3.Print() ; cout << endl ;
      cout << endl ;
    }

  // done with the whole job  
  cout << "Done with l3 clusterfinding." << endl;
  return kStOK;    
}

//_____________________________________________________________________________
Int_t St_l3Clufi_Maker::Fill_pixel_of_inner_rows(){
 
  // to navigate through this sector create an iterator
  St_DataSetIter sect(sector);
    
  // get the raw tables of this secotor 
  // raw_row_in
  St_raw_row *raw_row_in = (St_raw_row*) sect("raw_row_in");
  raw_row_st* innerrow = (raw_row_st*) raw_row_in->GetTable();
  // raw_pad_in
  St_raw_pad *raw_pad_in = (St_raw_pad*) sect("raw_pad_in");
  raw_pad_st* padin = (raw_pad_st*) raw_pad_in->GetTable();
  // raw_seq_in
  St_raw_seq *raw_seq_in = (St_raw_seq*) sect("raw_seq_in");
  raw_seq_st* seqin = (raw_seq_st*) raw_seq_in->GetTable();
  // raw_shortdata_in
  St_type_shortdata *shortdatain = (St_type_shortdata *) sect("pixel_data_in");
  type_shortdata_st* adcarrayin = (type_shortdata_st*) shortdatain->GetTable();


  ///////
  // loop over inner rows
  ///////
  //cout << " Number of inner rows we will loop over  " << raw_row_in->GetNRows() << endl;
  for (Int_t rowindex = 0;rowindex < (raw_row_in->GetNRows());rowindex++)
    //for (Int_t rowindex = 0;rowindex < 13;rowindex++)
    {
      Int_t pixel_offset_row =(Int_t) (innerrow[rowindex].ipixel);
      Int_t seq_offset_row =(Int_t) (innerrow[rowindex].iseq);
      Int_t pad_offset_row =(Int_t) (innerrow[rowindex].ipad);
      Int_t row_id = (Int_t) (innerrow[rowindex].RowId);
      /////////
      // loop over pads
      /////////
      //cout << " Number of pads we loop over " <<  Int_t (innerrow[rowindex].npad) << endl;
      for (Int_t padindex = 0 ; padindex < Int_t (innerrow[rowindex].npad) ; padindex++)
	{
	  Int_t pixel_offset_pad = (Int_t) (pixel_offset_row + (padin[padindex+pad_offset_row].PadOffset));
	  Int_t seq_offset_pad = (Int_t) (seq_offset_row + (padin[padindex+pad_offset_row].SeqOffset));
	  Int_t num_seq_pad = (Int_t) (padin[padindex+pad_offset_row].nseq);
	  Int_t pad_id = (Int_t) (padin[padindex+(innerrow[rowindex].ipad)].PadId);
	  Int_t seq_mod_break = (Int_t) (padin[padindex+(innerrow[rowindex].ipad)].SeqModBreak);
	  ///////
	  // loop over sequenzes
	  ///////
	  //cout << " Number of sequenzes we loop over " << num_seq_pad << endl;
	  for ( Int_t sequenzindex = 0 ; sequenzindex < num_seq_pad ; sequenzindex++ )
	    {
	      Int_t timebucketoffset=0;
	      if ( sequenzindex < seq_mod_break )
		{
		  timebucketoffset = (Int_t) (seqin[sequenzindex + seq_offset_pad].m);
		}
	      else if ( sequenzindex >= seq_mod_break )
		{
		  timebucketoffset = (Int_t) (seqin[sequenzindex + seq_offset_pad].m + 256) ;
		} 


	      Int_t numberoftimebucketsinthissequenz =(Int_t) (seqin[sequenzindex + seq_offset_pad].i);
	      //////
	      // loop over pixel in sequenz
	      //////
	      //cout << " Number of pixel we loop over " << numberoftimebucketsinthissequenz << endl;
	      // attention : seqin.i =  numberoftimebucketsinthissequenz = number of buckets - 1 !
	      for ( Int_t pixelindex = 0 ; pixelindex <= numberoftimebucketsinthissequenz ; pixelindex++ )
		{
		  // get adc value
		  Short_t adc_value = (Short_t) adcarrayin[pixelindex+pixel_offset_pad].data;

		  // get bucket
		  Int_t bucket_id = (Int_t) timebucketoffset + pixelindex;

		  // get pixelposition in huge pixel array
		  Int_t pixelarrayindex = (row_id-1) * (Max_number_of_pads * Max_number_of_buckets) 
		    + (pad_id-1) * Max_number_of_buckets 
		    + (bucket_id);

		  if ( adc_value <= 1024 && adc_value >= 0 
		       && pixelarrayindex <= Max_number_of_rows*Max_number_of_pads*Max_number_of_buckets 
		       && pixelarrayindex >= 0 )
		    {
		      // Fill pixelarray
		      pixelst[pixelarrayindex].data = adc_value; 
		    }
		  else 
		    {
		      //cerr << "something wrong with adc values -> abort l3clufi." << endl;
		      //return 0;
		      cout << "Something wrong with adc value: " ;
		      cout << "  adc_value = " << adc_value ;
		      cout << "  pixelarrayindex = " <<  pixelarrayindex << endl;
		      pixelst[pixelarrayindex].data = 0;
		    }
				    
		  // some output
		  //if ((innerrow[rowindex].RowId) ==13)
		  //{
		  //Int_t according_timebucket = (Int_t) timebucketoffset + pixelindex;
		  /*cout << "Sector: " << sector->GetName() << "   Row: " << (Int_t) (innerrow[rowindex].RowId);
		    cout << "    Pad: " << Int_t (padin[padindex+(innerrow[rowindex].ipad)].PadId)  <<  "   ADC: ";
		    cout << adc_value << "   Time: " << according_timebucket << endl; */
		  //}
		} 
	    }                           
	}
    } 
  // go home
  //for(Int_t ii=0;ii<1000;ii++){ pixelst[ii].data = ii;}
  return 1;
}

//_____________________________________________________________________________
Int_t St_l3Clufi_Maker::Fill_pixel_of_outer_rows(){

  // to navigate through this sector
  St_DataSetIter sect(sector);
    
  // get the raw tables of this secotor 
  // raw_row_out
  St_raw_row *raw_row_out = (St_raw_row*) sect("raw_row_out");
  raw_row_st* outerrow = (raw_row_st*) raw_row_out->GetTable();
  // raw_pad_out
  St_raw_pad *raw_pad_out = (St_raw_pad*) sect("raw_pad_out");
  raw_pad_st* padout = (raw_pad_st*) raw_pad_out->GetTable();
  // raw_seq_out
  St_raw_seq *raw_seq_out = (St_raw_seq*) sect("raw_seq_out");
  raw_seq_st* seqout = (raw_seq_st*) raw_seq_out->GetTable();
  // raw_shortdata_out
  St_type_shortdata *shortdataout = (St_type_shortdata *) sect("pixel_data_out");
  type_shortdata_st* adcarrayout = (type_shortdata_st*) shortdataout->GetTable();
    

  ///////
  // loop over outer rows
  ///////
  //cout << " Number of outer rows we will loop over  " << raw_row_out->GetNRows() << endl;
  for (Int_t rowindex2 = 0;rowindex2 < (raw_row_out->GetNRows());rowindex2++)
    //for (Int_t rowindex2 = 0;rowindex2 < 0;rowindex2++)
    {
      Int_t pixel_offset_row =(Int_t) (outerrow[rowindex2].ipixel);
      Int_t seq_offset_row =(Int_t) (outerrow[rowindex2].iseq);
      Int_t pad_offset_row =(Int_t) (outerrow[rowindex2].ipad); 
      Int_t row_id = (Int_t) (outerrow[rowindex2].RowId);
      /////////
      // loop over pads
      /////////
      //cout << " Number of pads we loop over " <<  Int_t (outerrow[rowindex2].npad) << endl;
      for (Int_t padindex = 0 ; padindex < Int_t (outerrow[rowindex2].npad) ; padindex++)
	{
	  Int_t pixel_offset_pad = (Int_t) (pixel_offset_row + (padout[padindex+pad_offset_row].PadOffset));
	  Int_t seq_offset_pad = (Int_t) (seq_offset_row + (padout[padindex+pad_offset_row].SeqOffset));
	  Int_t num_seq_pad = (Int_t) (padout[padindex+pad_offset_row].nseq);
	  Int_t pad_id = (Int_t) (padout[padindex+(outerrow[rowindex2].ipad)].PadId);
	  Int_t seq_mod_break = (Int_t) (padout[padindex+(outerrow[rowindex2].ipad)].SeqModBreak);
	  ///////
	  // loop over sequenzes
	  ///////
	  //cout << " Number of sequenzes we loop over " << num_seq_pad << endl;
	  for ( Int_t sequenzindex = 0 ; sequenzindex < num_seq_pad ; sequenzindex++ )
	    {
	      Int_t timebucketoffset = 0 ;
	      if ( sequenzindex < seq_mod_break )
		{
		  timebucketoffset = (Int_t) (seqout[sequenzindex + seq_offset_pad].m);
		}
	      else if ( sequenzindex >= seq_mod_break )
		{
		  timebucketoffset = (Int_t) (seqout[sequenzindex + seq_offset_pad].m + 256 ) ;
		} 
	      Int_t numberoftimebucketsinthissequenz =(Int_t) (seqout[sequenzindex + seq_offset_pad].i);
			    
	      //////
	      // loop over pixel in sequenz
	      //////
	      //cout << " Number of pixel we loop over " << numberoftimebucketsinthissequenz << endl;
	      // attention : seqin.i =  numberoftimebucketsinthissequenz = number of buckets - 1 !
	      for ( Int_t pixelindex = 0 ; pixelindex <= numberoftimebucketsinthissequenz ; pixelindex++ )
		{
		  // get pixel 
		  Short_t adc_value = (Short_t) adcarrayout[pixelindex+pixel_offset_pad].data;

		  // get bucket
		  Int_t bucket_id = (Int_t) timebucketoffset + pixelindex;
				    
		  // get pixelposition in huge pixel array
		  Int_t pixelarrayindex = (row_id-1) * (Max_number_of_pads * Max_number_of_buckets) 
		    + (pad_id-1) * Max_number_of_buckets 
		    + (bucket_id) ;

		  if ( adc_value <= 1024 && adc_value >= 0 
		       && pixelarrayindex <= Max_number_of_rows*Max_number_of_pads*Max_number_of_buckets 
		       && pixelarrayindex >= 0 )
		    {
		      // Fill pixelarray
		      pixelst[pixelarrayindex].data = adc_value; 
		    }
		  else 
		    {
		      //cerr << "something wrong with adc values -> abort l3clufi." << endl;
		      //return 0;
		      cout << "Something wrong with adc value: " ;
		      cout << "  adc_value = " << adc_value ;
		      cout << "  pixelarrayindex = " <<  pixelarrayindex << endl;
		      pixelst[pixelarrayindex].data = 0; 
		    }
		  // some output
		  //Int_t according_timebucket = (Int_t) timebucketoffset + pixelindex;
				    
					
		  /*cout << "Sector: " << sector->GetName() << "   Row: " << (Int_t) (outerrow[rowindex2].RowId);
		    cout << "    Pad: " << Int_t (padout[padindex+(outerrow[rowindex2].ipad)].PadId)  <<  "   ADC: ";
		    cout << adc_value << "   Time: " << according_timebucket << endl; */
		}//pixel 
	    }//seq                           
	}//pad
    }//row
  // go home
  return 1;
}

