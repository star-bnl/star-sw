//*-- Author : Victor Perevoztchikov
// 
// $Id: St_l3Clufi_Maker.cxx,v 1.24 2001/06/27 16:15:24 flierl Exp $
// $Log: St_l3Clufi_Maker.cxx,v $
// Revision 1.24  2001/06/27 16:15:24  flierl
// ommit empty padrows from raw data files
//
// Revision 1.23  2001/06/07 10:30:42  flierl
// switch off writing into tables
//
// Revision 1.22  2001/05/14 19:39:18  flierl
// fix severe bug : wrong adc values were extracted from rawdata
//
// Revision 1.21  2001/04/26 18:28:09  flierl
// clean up & write L3_P struct out on demand
//
// Revision 1.20  2001/04/06 22:17:11  flierl
// add outcommented code to write clusters into files in order to run standalone tracking
//
// Revision 1.19  2000/07/22 21:19:29  flierl
// switch off writing clusters to table for embedding !
//
// Revision 1.18  2000/07/08 18:19:43  flierl
// rm ClassImp of transformation classes
//
// Revision 1.17  2000/06/26 22:14:18  fisyak
// remove params
//
// Revision 1.16  2000/05/12 20:32:46  fisyak
// Add ClassImp, new rootcint requires them
//
// Revision 1.15  2000/04/12 18:39:21  flierl
// check whether enough memory is allocated for the clusters ( for the tables and the buffers )
//
//Revision 1.14  2000/03/24 19:45:16  flierl
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
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include "daqFormats.h"
#include "L3Formats.h"

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
        
    // Create Histograms 
    x_dis = new TH1F("L3ClufiTphitx","x coordinate of hits",400,-200,200) ;
    y_dis = new TH1F("L3ClufiTphity","y coordinate of hits",400,-200,200) ;
    z_dis = new TH1F("L3ClufiTphitz","z coordinate of hits",400,-400,400) ;
    charge_dis =  new TH1F("L3ClufiTphitcharge","charge of hits",40,-10,10) ;
    i960_time = new TH1D("times","times",18,0.5,18.5) ;
    for (Int_t id=0; id<18 ;id++) i960[id].Reset() ;
    
    // set max values for pixel array :  these values must be equal to those in the module (in croat.h)
    Max_number_of_rows = 45 ;
    Max_number_of_pads = 184 ;
    Max_number_of_buckets = 512 ;

    // define buffer_size
    Buffer_size = 100000 ;

    // switch on/off debugging
    l3ClufiDebug = 0 ;

    // done
    return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_l3Clufi_Maker::Make(){
    //
    //  this maker does :
    //  get tpc raw data
    //  prepare it to be handeld by the online clusterfinder (croat.c)
    //  call online clusterfinder
    //  produce output in online format which is later used by l3 tracking 
    //  optional :
    //  write clusters into tables 
    //  write clusters into L3_P strcture ( l3 online format )
    //  
    
    // here we start
    cout << endl << "Now we start l3Clufi Maker." << endl;
    
    // check time consumption of different parts of this maker
    TStopwatch timer1 ; 
    TStopwatch timer2 ; 
    TStopwatch timer3 ; 
    if (l3ClufiDebug) { timer1.Reset() ; timer2.Reset() ; timer3.Reset() ; }
  
    // some counters
    Int_t totalpixelcount = 0 ;
    
    // get raw data 
    raw_data_tpc = (St_DataSet*) GetInputDS("tpc_raw");
    if (l3ClufiDebug) { timer1.Start() ; } 

    // do we have something ? 
    if (!raw_data_tpc)
	{ 
	    cerr << "No raw data found, l3 clusterfinding skipped ! " << endl;
	    return kStWarn;
	} 
    else if (raw_data_tpc) 
	{ 
	    // what did we get ?
	    // cout << endl << "We get some raw data." << endl << endl; 
	    // raw_data_tpc->ls("*");
	
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
			
			    ///
			    // to handle supersectors two sectors must be treated as one !
			    // create out array for the supersector 
			    ///
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
			
			    ///
			    // fill pixel array for this sector
			    ///	  
			    // set pixel array 0 clumsey and slow but working 
			    if (l3ClufiDebug) { timer3.Start(0) ; }
			    for(Int_t pixindex=0;pixindex<Max_number_of_rows*Max_number_of_pads*Max_number_of_buckets;pixindex++)
				{
				    pixelst[pixindex].data=0;
				}
			    // fill pixel into huge array
			    if ( FillPixel(sectorindex) != 1 ) 
				{
				    cerr << "Unable to fill pixels of inner rows";
				    cerr << " in sector " << sectorindex <<endl;
				    return kStWarn;
				}

			    if (l3ClufiDebug) { timer3.Stop() ; }	
			    // just checking
			    if (l3ClufiDebug)
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
			    
			    ///
			    // now lets do the clusterfinding for this sector
			    ///
			    //cout << "Call the clusterfinding module for the "<< sectorindex <<"sector. " << endl;
			   			
			    // call the clusterfinder module 
			    if (l3ClufiDebug) { timer2.Start(0) ; }
			    Int_t ret = l3Clufi(Stpixel,St_hit_bank_this);
			    if (!ret)
				{
				    // clean up, stop this maker and go back to bfc
				    cerr << "Problems in L3 clusterfinding module in sector: "<< sectorindex <<endl;
				    delete Stpixel;
				    return kStErr;
				}
			    if (l3ClufiDebug) { timer2.Stop() ; }
			    // store length of cluster bank
			    if ( sectorindex%2 == 1 )
				{
				    clbanklengths[supersectorindex-1] = ret ;
				}
			    else if ( sectorindex%2 == 0 )
				{
				    clbanklengths[supersectorindex-1] += ret ;
				}

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
    if (l3ClufiDebug) { timer1.Stop() ; }

    ///
    // fill clusters into one file : produce L3_P object which is usable by
    ///
    if (l3ClufiDebug)
	{
	    WriteClustersIntoFile() ;
	}

    /// 
    // fill clusters into tables : the same format offline clusterfinder tcl uses
    ///
    if (l3ClufiDebug)
	{
	    WriteClustersIntoTables() ;
	}

    ///
    // fill i960 timeing to display the time consumption for the 18 different mezzanine cards 
    ///
    if (l3ClufiDebug)
	{
	    for (Int_t index1=1;index1<=18;index1++ )
		{
		    // fill histos
		    i960_time->Fill(index1,(Double_t)i960[index1-1].CpuTime()) ;
		    // talk to me
		    cout << "Time per i960 :  " << index1 << "\t" ;
		    cout << (Double_t)(i960[index1-1].RealTime()) << "\t" ;
		    cout << (Double_t)(i960[index1-1].CpuTime()) << endl ;
		    // reset
		    i960[index1-1].Reset() ;
		}
	}
 
    ///
    // timing output for this Maker
    ///
    if (l3ClufiDebug)
	{
	    cout << endl ;
	    cout << "Timer1 : " ; timer1.Print() ; cout << endl ;
	    cout << "Timer2 : " ; timer2.Print() ; cout << endl ;
	    cout << "Timer3 : " ; timer3.Print() ; cout << endl ;
	    cout << endl ;
	}
  
    ///
    // done with the whole job  
    ///
    Int_t totalMemory = 0 ;
    for( Int_t ssindex = 0 ; ssindex <= 11 ; ssindex++ )
	{
	    totalMemory += clbanklengths[ssindex] ;
	}
    cout << "Done with l3 clusterfinding " << totalMemory/pow(2,16) << " Mb of clusters found." << endl << endl ;
    return kStOK;    
}
//_____________________________________________________________________________
Int_t St_l3Clufi_Maker::WriteClustersIntoTables() 
{
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

	    //  call module which translates banks into tables old stylex
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
    return(kTRUE) ;
}
//_____________________________________________________________________________
Int_t St_l3Clufi_Maker::WriteClustersIntoFile() 
{ 
    // allocate memory for raw data event
    UInt_t *event = new UInt_t [5000000] ;
  
    // open file for raw data dump
    int fd = open ("rawdata.dat", O_WRONLY|O_CREAT|O_TRUNC, 00777) ;
    if (!fd) 
	{
	    perror("l3Clufi") ; 
	    return -1 ;
	}
    cout << "l3Clufi:  Open file for raw data dump.\n" ;

    // pointer to top-layer bank L3_P
    L3_P *l3_p = (L3_P *)event ;
  
    // fill L3_P header
    strncpy (l3_p->bh.bank_type, CHAR_L3_P, 8) ;
    l3_p->bh.length     = 40 ; 
    // l3_p->bh.bank_id    = 77 ???? 
    l3_p->bh.format_ver = DAQ_RAW_FORMAT_VERSION ;
    l3_p->bh.byte_order = DAQ_RAW_FORMAT_ORDER ;
    l3_p->bh.token      = 1 ;
    l3_p->bh.w9         = DAQ_RAW_FORMAT_WORD9 ;
    l3_p->bh.crc        = 0 ;
    // no gl3 tracks in L3_P!
    l3_p->tracks.off = 0; 
    l3_p->tracks.len = 0;

    // set address of first L3_SECP bank behind l3_p 
    L3_SECP *l3_secp = (L3_SECP *) &event[sizeof(struct L3_P)/4] ;
        
    // loop over super sectors
    for(Int_t superSectorIndex = 0 ;  superSectorIndex<12 ; superSectorIndex++)
	{
	    // fill L3_SECP header
	    cout << "l3Clufi:   fill L3_SECP header, super sector :"<<  superSectorIndex << endl ;
	    strncpy (l3_secp->bh.bank_type, CHAR_L3_SECP, 8) ;
	    l3_secp->bh.length     = 18 ; 
	    l3_secp->bh.bank_id    = 2*superSectorIndex+1 ;
	    l3_secp->bh.format_ver = DAQ_RAW_FORMAT_VERSION ;
	    l3_secp->bh.byte_order = DAQ_RAW_FORMAT_ORDER ; 
	    l3_secp->bh.token      = 1 ;
	    l3_secp->bh.w9         = DAQ_RAW_FORMAT_WORD9 ;
	    l3_secp->bh.crc        = 0 ;
	    l3_secp->time = 99 ; // offline chain
	    l3_secp->seq  = 1 ;
	    l3_secp->trg_word = 0 ;
	    l3_secp->trg_in_word = 0 ;
  
	    // get offset
	    TPCSECLP* tpcseclp = (TPCSECLP *) (((UInt_t *)l3_secp)+sizeof(struct L3_SECP)/4) ;
	
	    // fill offsets and lengths
	    l3_secp->clusterp.off = (UInt_t *)tpcseclp - (UInt_t *)l3_secp ;
	    l3_secp->clusterp.len = 4*clbanklengths[superSectorIndex] ;
	    l3_secp->trackp.off = 0 ; // l3_secp->clusterp.off + l3_secp->clusterp.len;
	    l3_secp->trackp.len = 0 ;
	    l3_secp->sl3clusterp.off = 0 ; // l3_secp->trackp.off + l3_secp->trackp.len;
	    l3_secp->sl3clusterp.len = 0 ;
	  
	    // fill clusterdata	 
	    Int_t len =  4*clbanklengths[superSectorIndex] ;
	    hitarray_st* hh = (hitarray_st*) St_hit_bank[superSectorIndex]->GetTable() ;
	    memcpy ( tpcseclp, hh , len ) ;
	    
	
	    // now fill offsets in L3_P
	    l3_p->sector[2*superSectorIndex].off = (UInt_t *)l3_secp - (UInt_t *)l3_p ;
	    l3_p->sector[2*superSectorIndex].len = (UInt_t *)tpcseclp + l3_secp->clusterp.len - (UInt_t *)l3_secp ;
	    // as long as we have supersectors the odd sectors are set to 0    
	    l3_p->sector[2*superSectorIndex+1].off = 0 ; 
	    l3_p->sector[2*superSectorIndex+1].len = 0 ;
	    
	    // set pointer to next l3_secp
	    l3_secp = (L3_SECP *) ((UInt_t *)l3_p + l3_p->sector[2*superSectorIndex].off + l3_p->sector[2*superSectorIndex].len + 1) ;
	}
    
    // fill overall length in L3_P
    l3_p->len = (UInt_t *)l3_secp - (UInt_t *)l3_p;
    
    // dump data into file
    int wbytes = write (fd, l3_p, l3_p->len*4) ;
    if (!wbytes) perror ("l3Clufi") ;
    printf("l3Clufi: Close raw data file, %d bytes written to file.\n", wbytes) ;
    
    // clean up
    close (fd) ;
    delete event ;

    // go home
    return(kTRUE);
}
//_____________________________________________________________________________
Int_t St_l3Clufi_Maker::FillPixel(Int_t mSector)
{
    ///
    // to navigate through rawdata of this sector create an iterator
    ///
    St_DataSetIter rawdata(raw_data_tpc) ;
    Char_t sectorname[10] ; sprintf(sectorname, "Sector_%d", mSector) ;
    St_DataSet *tsector = rawdata(sectorname) ;  
    if (!tsector) return 0 ;
    St_DataSetIter mSectorIter(tsector) ;

    ///
    // define raw tables; for details see STAR NOTE SN325
    ///
    St_raw_row *st_raw_row ;
    raw_row_st* row_st ;
    St_raw_pad *st_raw_pad ;
    raw_pad_st* pad_st ;
    St_raw_seq *st_raw_seq ;
    raw_seq_st* seq_st ;
    St_type_shortdata *st_shortdata ;
    type_shortdata_st* adcarray ;

    ////
    // loop over padrows
    ////
    for ( Int_t mRow = 1 ; mRow <= 45 ; mRow++ )
	{
	    // rowindex
	    Int_t rowindex = 0 ;

	    // distinguish between inner and outer sector
	    if ( mRow <= 13 )
		{
		    // inner sector
		    // raw_row_in
		    st_raw_row = (St_raw_row*) mSectorIter("raw_row_in");
		    row_st = (raw_row_st*) st_raw_row->GetTable();
		    // if row is empty jump to the next one
		    if (st_raw_row->GetNRows() == 0 ) continue ;
		    // raw_pad_in
		    st_raw_pad = (St_raw_pad*) mSectorIter("raw_pad_in");
		    pad_st = (raw_pad_st*) st_raw_pad->GetTable();
		    // raw_seq_in
		    st_raw_seq = (St_raw_seq*) mSectorIter("raw_seq_in");
		    seq_st = (raw_seq_st*) st_raw_seq->GetTable();
		    // raw_shortdata_in
		    st_shortdata = (St_type_shortdata *) mSectorIter("pixel_data_in");
		    adcarray = (type_shortdata_st*) st_shortdata->GetTable();
	  
		    // rowdindex
		    rowindex = 13 - mRow ;
		}
	    else 
		{
		    //outer sector
		    // raw_row_in
		    st_raw_row = (St_raw_row*) mSectorIter("raw_row_out");
		    row_st = (raw_row_st*) st_raw_row->GetTable();
		    // if row is empty jump to the next one
		    if (st_raw_row->GetNRows() == 0 ) continue ;
		    // raw_pad_in
		    st_raw_pad = (St_raw_pad*) mSectorIter("raw_pad_out");
		    pad_st = (raw_pad_st*) st_raw_pad->GetTable();
		    // raw_seq_in
		    st_raw_seq = (St_raw_seq*) mSectorIter("raw_seq_out");
		    seq_st = (raw_seq_st*) st_raw_seq->GetTable();
		    // raw_shortdata_in
		    st_shortdata = (St_type_shortdata *) mSectorIter("pixel_data_out");
		    adcarray = (type_shortdata_st*) st_shortdata->GetTable();
		    // row index
		    rowindex = 45 - mRow ;
		}
      
	    ///
	    // get data for this row
	    ///
	    Int_t pixel_offset_row  =(Int_t) (row_st[rowindex].ipixel) ;
	    Int_t seq_offset_row    =(Int_t) (row_st[rowindex].iseq) ;
	    Int_t pad_offset_row    =(Int_t) (row_st[rowindex].ipad) ;
	    Int_t row_id            =(Int_t) (row_st[rowindex].RowId) ;
	    //cout << "Examining rowindex  :"<< rowindex << endl ;
	    //cout << "Row_id  :"<< row_id << endl ;

	    ///
	    // loop over pads
	    ///
	    //cout << " Number of pads we loop over " <<  Int_t (row_st[rowindex].npad) << endl;
	    for (Int_t padindex = 0 ; padindex < Int_t (row_st[rowindex].npad) ; padindex++)
		{
		    Int_t pixel_offset_pad = (Int_t) (pixel_offset_row + (pad_st[padindex+pad_offset_row].PadOffset));
		    Int_t seq_offset_pad   = (Int_t) (seq_offset_row + (pad_st[padindex+pad_offset_row].SeqOffset));
		    Int_t num_seq_pad      = (Int_t) (pad_st[padindex+pad_offset_row].nseq);
		    Int_t pad_id           = (Int_t) (pad_st[padindex+(row_st[rowindex].ipad)].PadId);
		    Int_t seq_mod_break    = (Int_t) (pad_st[padindex+(row_st[rowindex].ipad)].SeqModBreak);
	
		    ///
		    // loop over sequenzes
		    ///
		    //cout << " Number of sequenzes we loop over " << num_seq_pad << endl;
		    for ( Int_t sequenzindex = 0 ; sequenzindex < num_seq_pad ; sequenzindex++ )
			{
			    Int_t timebucketoffset = 0 ;
			    if ( sequenzindex < seq_mod_break )
				{
				    timebucketoffset = (Int_t) (seq_st[sequenzindex + seq_offset_pad].m) ;
				}
			    else if ( sequenzindex >= seq_mod_break )
				{
				    timebucketoffset = (Int_t) (seq_st[sequenzindex + seq_offset_pad].m + 256) ;
				} 
	      
			    Int_t numberoftimebucketsinthissequenz = (Int_t) (seq_st[sequenzindex + seq_offset_pad].i);
	      
			    ///
			    // loop over pixel in sequenz
			    ///
			    //cout << " Number of pixel we loop over " << numberoftimebucketsinthissequenz << endl;
			    for ( Int_t pixelindex = 0 ; pixelindex <= numberoftimebucketsinthissequenz ; pixelindex++ )
				{
				    // get adc value
				    Short_t adc_value = (Short_t) adcarray[pixel_offset_pad++].data;
		    
				    // get according bucket
				    Int_t bucket_id = (Int_t) timebucketoffset + pixelindex;
		  
				    // get pixelposition in huge pixel array
				    Int_t pixelarrayindex = (row_id-1) * (Max_number_of_pads * Max_number_of_buckets) 
					+ (pad_id-1) * Max_number_of_buckets 
					+ (bucket_id) ;

				    // check boundaries
				    if ( adc_value <= 2000 && adc_value >= 0 
					 && pixelarrayindex <= Max_number_of_rows*Max_number_of_pads*Max_number_of_buckets 
					 && pixelarrayindex >= 0 )
					{
					    // fill pixelarray
					    pixelst[pixelarrayindex].data = adc_value; 
					}
				    else 
					{
					    // something wrong if you end up here
					    cout << "Something wrong with adc value: " ;
					    cout << "  adc_value = " << adc_value ;
					    cout << "  pixelarrayindex = " <<  pixelarrayindex << endl;
					    pixelst[pixelarrayindex].data = 0; 
					}
				} // loop over pixel in this sequenz 
			} // loop over sequnezes                           
		} // loop over pads
	} // loop over rows
  
    // all done go home
    return 1 ;
}
