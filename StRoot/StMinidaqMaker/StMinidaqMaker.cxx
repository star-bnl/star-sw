// $Id: StMinidaqMaker.cxx,v 1.1 1999/02/11 23:40:19 sakrejda Exp $
// $Log: StMinidaqMaker.cxx,v $
// Revision 1.1  1999/02/11 23:40:19  sakrejda
// A maker for the test data created
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

//                                                                      //
// StMinidaqMaker class for Makers                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include <iostream.h>
#include <strings.h>
#include "StMinidaqMaker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "tpc/St_tfc_load_native_gains_Module.h"

ClassImp(StMinidaqMaker)

//_____________________________________________________________________________
StMinidaqMaker::StMinidaqMaker(const char *name, const char *title):
m_Params(0),
StMaker(name,title){
   drawinit=kFALSE;
}
//_____________________________________________________________________________
StMinidaqMaker::~StMinidaqMaker(){
}
//_____________________________________________________________________________
Int_t StMinidaqMaker::Init() {
  //Tell me I am here
  cout<<"Init miniDAQ maker"<<endl;
// Create tables

   if (!m_Params) {m_Params = new St_DataSet("priv");}
   m_Fruits = m_Params;
   //
   if (m_Params) {
     St_DataSetIter       local(m_Params);
     St_DataSet *gaindir = local.Mkdir("tpc/gains");
     St_raw_sec_m *rsm = new St_raw_sec_m("gainq_sec_m",24); 
     St_tcl_sector_index *tsi = new St_tcl_sector_index("tfc_sector_index",1);
     tcl_sector_index_st *ttsi = (tcl_sector_index_st *)tsi->GetTable();

     gaindir->Add(rsm);
 
   //

     cout<<"just before the loop"<<endl;

     St_DataSet     *gg = gStChain->DataSet("params");
     St_DataSetIter param(gg);
     St_DataSet     *begin_run = param("tpc/BEGIN_RUN");
     St_DataSetIter gaintables(begin_run);
     

     for(Int_t i=0; i<24;i++) {
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
         for(Int_t l=0; l < m_it->GetNRows();l++){
	   if( it->sector != current_sector){
	     current_sector= it->sector;
             number_of_sectors +=1;
             list_of_sectors[number_of_sectors]=current_sector;
	     cout<<"current sector"<< current_sector<<endl;
	   }
	 }
         cout<<"number of sectors"<< number_of_sectors<<endl;
	 for(l=0;l<=number_of_sectors; l++){
	   ttsi->CurrentSector = list_of_sectors[l];
           St_DataSetIter nextSector(gaindir);
	   cout<<" gaindir"<<gaindir<<endl;
           St_DataSet *sectorN = 0;
           Int_t sectorNumber =0;
	   while (sectorN = nextSector()) {
	     if(strcmp(sectorN->GetName(),"Sector") == 0){
	       sectorNumber++;
               if(sectorNumber == list_of_sectors[l]){
		 cout<<"sector number"<<sectorNumber<<" "<<sectorN<<endl;
		 St_DataSetIter localIter(sectorN);
		 cout<<"created localIter"<<endl;
		 St_raw_row *rri = (St_raw_row *) localIter.Find("gain_row_in");
		 St_raw_row *rro = (St_raw_row *) localIter.Find("gain_row_out");
		 St_type_floatdata *fdi = (St_type_floatdata *) localIter.Find("gain_data_in");
		 St_type_floatdata *fdo = (St_type_floatdata *) localIter.Find("gain_data_out");
		 cout<<"input pointers"<<rri<<" "<<rro<<" "<<fdi<<" "<<fdo<<endl;
		 Int_t result = tfc_load_native_gains(tsi,m_it,m_gt,m_st,rsm,rri,fdi, rro, fdo); 
	       }
	     }
	   }
	 }
       }
     }
   }
// Create Histograms    
   return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StMinidaqMaker::Make(){
//  PrintInfo();
  if (!m_DataSet->GetList())  {//if DataSet is empty fill it
}
  cout<<" I am in the minidaq maker NOW"<<endl;
 return kStOK;
}
//_____________________________________________________________________________
void StMinidaqMaker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: StMinidaqMaker.cxx,v 1.1 1999/02/11 23:40:19 sakrejda Exp $\n");
//  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
  if (gStChain->Debug()) StMaker::PrintInfo();
}

//_____________________________________________________________________________
Int_t StMinidaqMaker::Finish()
{
 Clear();
 SafeDelete(m_Params);
 return kStOK;
}
