// $Id: StMinidaqMaker.cxx,v 1.18 2000/06/26 22:12:08 fisyak Exp $
// $Log: StMinidaqMaker.cxx,v $
// Revision 1.18  2000/06/26 22:12:08  fisyak
// remove params
//
// Revision 1.17  2000/02/03 20:47:40  fisyak
// CC5 fixes
//
// Revision 1.16  1999/07/15 13:57:14  perev
// cleanup
//
// Revision 1.15  1999/06/18 21:35:06  perev
// Fix BEGIN_RUN. Add Init in Make
//
// Revision 1.14  1999/05/13 20:58:24  liq
// set protections on table indexs
//
// Revision 1.13  1999/05/10 13:38:23  love
//   Book all reformat_new tables as 1 row, expand only when needed
//
// Revision 1.12  1999/05/05 14:07:12  love
// Used Debug on debug messages, changed initraw sign, fixes some print statements
//
// Revision 1.11  1999/04/28 00:32:31  sakrejda
// Dave's changes to handle missing tables checked in
//
// Revision 1.10  1999/04/08 19:02:43  liq
// set protection to check whether the IT,ST,or SD empty
//
// Revision 1.9  1999/04/08 16:29:51  sakrejda
// a call to tpg and seting parametrs in the maker removed
//
// Revision 1.8  1999/03/31 20:29:40  liq
// take off call module xyz_newtab
//
// Revision 1.7  1999/03/30 15:58:10  love
// Set Drift velocity and trigger time for a laser run
//
// Revision 1.6  1999/03/24 01:17:01  sakrejda
// tss_pars added to the xyz_newtab call
//
// Revision 1.5  1999/03/17 02:02:31  fisyak
// New scheme
//
// Revision 1.4  1999/03/15 00:36:45  perev
// For new Maker schema
//
// Revision 1.3  1999/02/23 16:36:45  love
// Set Clock to 9.4345 MC
//
// Revision 1.2  1999/02/20 02:25:42  liq
// call mudle reformat_new
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
// StMinidaqMaker class for Makers                                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include <iostream.h>
#include <strings.h>
#include <assert.h>
#include "StMinidaqMaker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "St_XDFFile.h"
#include "TROOT.h"
#include "TFile.h"
#include "TProfile.h"
#include "TNtuple.h"
#include "TH2.h"
#include "TH3.h"
#include "TRandom.h"
#include "tpc/St_tfc_load_native_gains_Module.h"
#include "tpc/St_init_raw_table_Module.h"
#include "tpc/St_reformat_new_Module.h"
   const Int_t StMinidaqMaker::no_of_sectors = 24;

ClassImp(StMinidaqMaker)

//_____________________________________________________________________________
StMinidaqMaker::StMinidaqMaker(const char *name):StMaker(name),

m_Params(0)

{
   m_init = 0;
   m_first_sector=1;
   m_last_sector=no_of_sectors;
}
//_____________________________________________________________________________
StMinidaqMaker::~StMinidaqMaker(){
}
//_____________________________________________________________________________
Int_t StMinidaqMaker::Init() {
  int i,l;

  if (m_init >=2) return 0;
  
  St_DataSet *begin_run = GetDataSet("BEGIN_RUN");
  if (!begin_run) {
     if (!m_init++) return 0;
     assert (begin_run);}
  m_init = 2;


  //Tell me I am here
  cout<<"Init miniDAQ maker"<<endl;
  // access to tsspar
  St_DataSet *tpc = GetDataBase("tpc");
  assert(tpc);
  St_DataSetIter       local(tpc);
   St_DataSet *tsspars = local("tsspars");
   if (tsspars){
       St_DataSetIter partable(tsspars);
       m_tsspar = (St_tss_tsspar *) partable("tsspar");
       if (!m_tsspar) {
         cout << "TPC tss parameter tables are incomplete."<< endl;  
         SafeDelete(tsspars);
       }
       else {
         tss_tsspar_st *tsspar = m_tsspar->GetTable();
         tsspar->min_sect = m_first_sector;
         tsspar->max_sect = m_last_sector;
         cout << " Min and Max Sector numbers for tpc slow simulation: " << 
           tsspar->min_sect <<" to "<< tsspar->max_sect << endl;
       }
   }

   if (!m_tfc_sector_index) {
         St_DataSetIter  loc(tpc);
         loc.Cd("tfcpars");
         m_tfc_sector_index = (St_tcl_sector_index *) loc("tfc_sector_index");
         if (!m_tfc_sector_index) {
           m_tfc_sector_index = new St_tcl_sector_index("tfc_sector_index",1);
           loc.Add(m_tfc_sector_index);
        }
    }

// Create tables in DataSet priv

   if (!m_Params) {m_Params = new St_DataSet("priv");}
   //yf   m_Fruits = m_Params;
   //
   if (m_Params) {
     St_DataSetIter       local(m_Params);
     St_DataSet *gaindir = local.Mkdir("tpc/gains");
  
     St_raw_sec_m *rsm = new St_raw_sec_m("gainq_sec_m",24);   
     St_tcl_sector_index *tsi = new St_tcl_sector_index("tfc_sector_index",1);
     tcl_sector_index_st *ttsi = (tcl_sector_index_st *)tsi->GetTable();

     gaindir->Add(rsm);
   


   //
     if(Debug()) cout<<"just before the loop"<<endl;

     St_DataSetIter gaintables(begin_run);
     

     for(i=0; i<24;i++) {
       St_DataSet *sector = new St_DataSet("Sector");
       sector->Add(new St_raw_row("gain_row_in",13));
       sector->Add(new St_raw_row("gain_row_out",32));
       sector->Add(new St_type_floatdata("gain_data_in",1750));
       sector->Add(new St_type_floatdata("gain_data_out",3942));
       gaindir->Add(sector);
     }

     char labeli[4];
     char labelg[4];
     char labels[4];
     char digit[2];
     for(i=1; i<=6;i++){
       strcpy(labeli,"IT");
       strcpy(labelg,"GN");
       strcpy(labels,"ST");
       sprintf(digit,"%d",i);
       digit[1]=0;
       strcat(labeli,digit);
       strcat(labelg,digit);
       strcat(labels,digit);
       St_type_index *m_it     = (St_type_index *) gaintables(labeli);
       St_type_gain_bad *m_gt  = (St_type_gain_bad *) gaintables(labelg);
       St_type_structtbl *m_st = (St_type_structtbl *) gaintables(labels);
       if(m_it){
         Int_t current_sector= -1;
         Int_t number_of_sectors = -1;
         Int_t list_of_sectors[24];
         type_index_st *it = m_it->GetTable();
         for(l=0; l < m_it->GetNRows();l++){
             if( it->sector != current_sector){
             current_sector= it->sector;
             number_of_sectors +=1;
             list_of_sectors[number_of_sectors]=current_sector;
             if(Debug())cout<<"current sector "<< current_sector<<endl;
             }
          }
         if(Debug())cout<<"number of sector s"<< number_of_sectors<<endl;
         for(l=0;l<=number_of_sectors; l++){
               ttsi->CurrentSector = list_of_sectors[l];
              St_DataSetIter nextSector(gaindir);
              if(Debug())cout<<" gaindir "<<gaindir<<endl;
              St_DataSet *sectorN = 0;
              Int_t sectorNumber =0;
              while ((sectorN = nextSector())) {
                   if(strcmp(sectorN->GetName(),"Sector") == 0){
                   sectorNumber++;
                  if(sectorNumber == list_of_sectors[l]){
		 if(Debug())cout<<"sector number "<<sectorNumber<<" "<<sectorN<<endl;
		 St_DataSetIter localIter(sectorN);
		 if(Debug())cout<<"created localIter"<<endl;
		 St_raw_row *rri = (St_raw_row *) localIter.Find("gain_row_in");
		 St_raw_row *rro = (St_raw_row *) localIter.Find("gain_row_out");
		 St_type_floatdata *fdi = (St_type_floatdata *) localIter.Find("gain_data_in");
		 St_type_floatdata *fdo = (St_type_floatdata *) localIter.Find("gain_data_out");
		 if(Debug()) cout<<"input pointers "<<rri<<" "<<rro<<" "<<fdi<<" "<<fdo<<endl;
		 Int_t result = tfc_load_native_gains(tsi,m_it,m_gt,m_st,rsm,rri,fdi, rro, fdo); 
                 if(result!=kSTAFCV_OK) Warning("Make","tfc_load_native_gains==%d",result);
                  }
                }
              }
           }
       }
     }
   }
   //tell where we are
  cout<<"finish init of StMinidaqMaker"<<endl;
  // create the histograms
   return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StMinidaqMaker::Make(){
//  PrintInfo();
  Init();
  cout<<"begin to make StMinidaqMaker"<<endl;
  TransferData(); //transfer data from TPC_DATA dataset to event/raw_data/tpc/
  cout<<"end of StMinidaqMaker"<<endl;
  return kStOK;
}
//_____________________________________________________________________________
void StMinidaqMaker::TransferData(){
   //tell where we are
   cout<<"begin to create tables in StMinidaqMaker"<<endl;
   St_DataSet   *sector = 0;
   // Check existance of DataSet and create it if any
   St_DataSetIter raw_data_tpc(m_DataSet);
   St_raw_sec_m  *raw_sec_m = (St_raw_sec_m *) raw_data_tpc("raw_sec_m");
   if (!raw_sec_m) {// This table defines the interpretation of subsequent tables.
     raw_sec_m = new St_raw_sec_m("raw_sec_m",no_of_sectors); 
     raw_data_tpc.Add(raw_sec_m); 
   }
// Check sectors output tables   
   for (Int_t i=m_first_sector;i<=m_last_sector;i++){
     Char_t name_of_sector[80];
     sprintf(name_of_sector,"Sector_%i",i);
     sector = raw_data_tpc(name_of_sector);
     if (!sector) {
       raw_data_tpc.Mkdir(name_of_sector);
       sector = raw_data_tpc(name_of_sector);
     }
     St_DataSetIter sect(sector);
     // pad row description, representing mapped data from one row within a subsector
     St_raw_row  *raw_row_in = (St_raw_row *) sect("raw_row_in");
     if (!raw_row_in) {raw_row_in = new St_raw_row("raw_row_in",13); sect.Add(raw_row_in);}
     St_raw_row  *raw_row_out = (St_raw_row *) sect("raw_row_out");
     if (!raw_row_out) {raw_row_out = new St_raw_row("raw_row_out",32); sect.Add(raw_row_out);}
     // pad description, representing mapped data from one pad 
     St_raw_pad  *raw_pad_in = (St_raw_pad *) sect("raw_pad_in");
     if (!raw_pad_in) {raw_pad_in = new St_raw_pad("raw_pad_in",1); sect.Add(raw_pad_in);}
     St_raw_pad  *raw_pad_out = (St_raw_pad *) sect("raw_pad_out");
     if (!raw_pad_out) {raw_pad_out = new St_raw_pad("raw_pad_out",1); sect.Add(raw_pad_out);}
     // This table defines the interpretation of subsequent tables.  One row per sector
     St_raw_seq  *raw_seq_in = (St_raw_seq *) sect("raw_seq_in");
     if (!raw_seq_in) {raw_seq_in = new St_raw_seq("raw_seq_in",1); sect.Add(raw_seq_in);}
     St_raw_seq  *raw_seq_out = (St_raw_seq *) sect("raw_seq_out");
     if (!raw_seq_out) {raw_seq_out = new St_raw_seq("raw_seq_out",1); sect.Add(raw_seq_out);}
     // Data Table (10-bit)
     St_type_shortdata  *pixel_data_in = (St_type_shortdata *) sect("pixel_data_in");
     if (!pixel_data_in) {pixel_data_in = new St_type_shortdata("pixel_data_in",1); 
     sect.Add(pixel_data_in);}
     St_type_shortdata  *pixel_data_out = (St_type_shortdata *) sect("pixel_data_out");
     if (!pixel_data_out) {pixel_data_out = new St_type_shortdata("pixel_data_out",1); 
     sect.Add(pixel_data_out);}
   } //end of loop of sector
   
   //begin to run module init_raw_table
    cout<<"begin to run module init_raw_table in StMinidaqMaker"<<endl;
   St_DataSetIter next(m_DataSet);

   while ((sector=next())){
       Char_t *name= 0;
       if ((name = strstr((char *)sector->GetName(),"Sector"))) {
       // look for the sector number
           name  = strchr(name,'_')+1;
           Int_t indx = atoi(name);
           if (Debug()) printf(" Sector = %d \n", indx);
           Int_t k;
           k = indx; if (k == m_first_sector) k = - indx;
           if(m_tsspar){
               tss_tsspar_st *tsspar = m_tsspar->GetTable();
               tsspar->min_sect = k;
               if(Debug())cout<<"run init_raw_table for sector= "<< k <<endl;
               St_DataSetIter sect(sector);
               St_raw_row         *raw_row_in     = (St_raw_row *) sect("raw_row_in");
               St_raw_row         *raw_row_out    = (St_raw_row *) sect("raw_row_out");
               Int_t initraw=0;
               if(raw_row_in&&raw_row_out){
                     initraw = init_raw_table(m_tsspar,  raw_sec_m,
                                              raw_row_in,  raw_row_out);
               }
              if(initraw!=kSTAFCV_OK) Warning("Make","init_raw_table== %d",initraw);
          }
       }
     }
       //begin to call module reformat_new
    char labelit[4];
    char labelst[4];
    char labelsd[4];
    char digits[2];
    //rewind DataSetIter
    next.Reset();
    // goto the TPC_DATA directory
    //   St_DataSetIter mdaqdata(GetDataSet("event/raw_data/tpc/TPC_DATA"));
     St_DataSet     *tpcmin =GetDataSet("TPC_DATA");
     assert (tpcmin);
     St_DataSetIter mdaqdata(tpcmin);

    //loop over all the sectors to run reformat_new module
     for (Int_t im=m_first_sector;im<=m_last_sector;im++){
       Char_t name_of_sector[80];
       sprintf(name_of_sector,"Sector_%i",im);
       sector = raw_data_tpc(name_of_sector);
       if (!sector) {
	 //        raw_data_tpc.Mkdir(name_of_sector);
	 //sector = raw_data_tpc(name_of_sector);
        printf(" there is no sector_%d in StMinidaqMaker::Make\n", im);
       }
 
      while ((sector=next())){
         Char_t *name= 0;
          if ((name = strstr((char *)sector->GetName(),"Sector"))) {
          // look for the sector number
              name  = strchr(name,'_')+1;
              Int_t indx = atoi(name);
               if (Debug()) printf(" Sector = %d \n", indx);
              Int_t k;
              k = indx; if (k == m_first_sector) k = - indx;
               //store sector no. into tfc_sector_index table
	      // St_DataSetIter sectpp(GetDataBase("tpc/tfcpars"));
              // m_tfc_sector_index= (St_tcl_sector_index *) sectpp("tfc_sector_index");
              tcl_sector_index_st *tfcss=m_tfc_sector_index->GetTable();
              tfcss->CurrentSector=k;
              m_tfc_sector_index->AddAt(tfcss,0);

              //look at the information from MiniDaq for this sector, from IT6 to IT1
              for(Int_t iit=6;iit>0;iit--){
                 strcpy(labelit,"IT");
                 strcpy(labelst,"ST");
                 strcpy(labelsd,"SD");
                 sprintf(digits,"%d",iit);
                 digits[1]=0;
                 strcat(labelit,digits);
                 strcat(labelst,digits);
                 strcat(labelsd,digits);
                 St_type_index *m_it     = (St_type_index *) mdaqdata(labelit);
                 if (m_it==0) continue;
                 St_type_structtbl *m_st  = (St_type_structtbl *) mdaqdata(labelst);
                 if (m_st==0) continue;
                 St_type_shortdata *m_sd = (St_type_shortdata *) mdaqdata(labelsd);
                 if (m_sd==0) continue;
                 // make sure these input tables filled,
		 // then judge whether the information of IT tables concerns to this Sector Number
                 if (Debug()) {
                    cout<<"it size "<<m_it->GetNRows()<<" st size "<<m_st->GetNRows();
                    cout<<" sd size "<<m_sd->GetNRows()<<endl;
		    }
		 type_index_st  *mmp= m_it->GetTable(); 
                 if (mmp==0) continue;
                 Int_t kj=mmp->sector;
                 if(m_it->GetNRows()&&m_st->GetNRows()&&m_sd->GetNRows()&&kj==k){
		   tss_tsspar_st *tsspar = m_tsspar->GetTable();
		   tsspar->min_sect = k;
		   St_DataSetIter sect(sector);

		   St_raw_row         *raw_row_in     = (St_raw_row *) sect("raw_row_in");
		   St_raw_row         *raw_row_out    = (St_raw_row *) sect("raw_row_out");
		   St_raw_pad         *raw_pad_in     = (St_raw_pad *) sect("raw_pad_in");
		   St_raw_pad         *raw_pad_out    = (St_raw_pad *) sect("raw_pad_out");
		   St_raw_seq         *raw_seq_in     = (St_raw_seq *) sect("raw_seq_in");
		   St_raw_seq         *raw_seq_out    = (St_raw_seq *) sect("raw_seq_out");
		   St_type_shortdata  *pixel_data_in  = (St_type_shortdata *) sect("pixel_data_in");
		   St_type_shortdata  *pixel_data_out = (St_type_shortdata *) sect("pixel_data_out");
		   
		   raw_pad_in->ReAllocate(4000);
		   raw_pad_out->ReAllocate(4000);
		   raw_seq_in->ReAllocate(50000);
		   raw_seq_out->ReAllocate(100000);
		   pixel_data_in->ReAllocate(900000);
		   pixel_data_out->ReAllocate(900000);
		   
		   Int_t res = 0;
		   if(m_tsspar&&m_tfc_sector_index&&m_it&&m_sd&&m_st&&
		      raw_row_in&&raw_row_out&&raw_pad_in&&raw_pad_out&&
		      raw_seq_in&&raw_seq_out&&pixel_data_in&&pixel_data_out) {
		     res=reformat_new(m_tsspar,m_tfc_sector_index,
				      m_it,m_sd,m_st,
				      raw_sec_m,
				      raw_row_in, raw_pad_in, raw_seq_in, pixel_data_in,
				      raw_row_out,raw_pad_out,raw_seq_out,pixel_data_out);
		   }
		   else {
		     Warning("TransferData","something wrong with tables pointers while calling reformat_new"); }
		   if(res!=kSTAFCV_OK) Warning("TransferData","reformat_new==%d",res);
                 }
	      }
          }
      }
     }
}

//_____________________________________________________________________________

//_____________________________________________________________________________
Int_t StMinidaqMaker::Finish()
{
  Clear();
  SafeDelete(m_Params);
  return kStOK;
}
