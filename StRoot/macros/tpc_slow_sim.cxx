#ifndef __CINT__
#include "Rtypes.h"
#include "St_XDFFile.h"
#include "St_DataSet.h"
#include "St_Module.h"
#include "St_Table.h"
#endif
#include "iostream.h"
// Global top level directory
Int_t STAFCV_OK  = 1;//  alias/create STAFCV_OK  1
Int_t STAFCV_BAD = 0;//  alias/create STAFCV_BAD 0
Int_t staf_status;
St_DataSetIter dui;
St_DataSetIter *constant =0;
St_DataSetIter *geant=0;
St_DataSetIter *raw_data=0;
St_DataSetIter *data=0;
//__________________________________________________________________________
void wait(){
  Char_t input;
  cout << "Wait " << endl; cin >> input;
  cout << endl;
}

//__________________________________________________________________________
void setup_dir(){
//*Sets up STAR STAF directroy tree .

cout << " ********************************" << endl;
cout << " *** Setting up directory tree **" << endl;
cout << " ********************************" << endl;


dui.Mkdir("dui"); dui.Cd("dui");

// Create a "constant" sub-directory


// mkdir  /dui/constants/  
// mkdir  /dui/constants/ctf
// mkdir  /dui/constants/ctf/geometry/
// mkdir  /dui/constants/ctf/params/
// mkdir  /dui/constants/ctf/calib/  
// mkdir  /dui/constants/emc
// mkdir  /dui/constants/emc/geometry/
// mkdir  /dui/constants/emc/params/
// mkdir  /dui/constants/emc/calib/
// mkdir  /dui/constants/ftpc
// mkdir  /dui/constants/ftpc/geometry/
// mkdir  /dui/constants/ftpc/params/
// mkdir  /dui/constants/ftpc/calib/
// mkdir  /dui/constants/global
// mkdir  /dui/constants/global/geometry/
// mkdir  /dui/constants/global/params/
// mkdir  /dui/constants/global/calib/
// mkdir  /dui/constants/global/magnetic_field
// mkdir  /dui/constants/mwc
// mkdir  /dui/constants/mwc/geometry/
// mkdir  /dui/constants/mwc/params/
// mkdir  /dui/constants/mwc/calib/
// mkdir  /dui/constants/svt
// mkdir  /dui/constants/svt/geometry/
// mkdir  /dui/constants/svt/params/
// mkdir  /dui/constants/svt/calib/
// mkdir  /dui/constants/tpc
// mkdir  /dui/constants/tpc/geometry/
// mkdir  /dui/constants/tpc/params/
// mkdir  /dui/constants/tpc/calib/

constant = new St_DataSetIter(dui.Pwd());

constant->Mkdir("constants");
constant->Cd("constants");
constant->Mkdir("ctf");
constant->Mkdir("ctf/geometry");
constant->Mkdir("ctf/params");
constant->Mkdir("ctf/calib");
constant->Mkdir("emc");
constant->Mkdir("emc/geometry");
constant->Mkdir("emc/params");
constant->Mkdir("emc/calib");
constant->Mkdir("ftpc");
constant->Mkdir("ftpc/geometry");
constant->Mkdir("ftpc/params");
constant->Mkdir("ftpc/calib");
constant->Mkdir("global");
constant->Mkdir("global/geometry");
constant->Mkdir("global/params");
constant->Mkdir("global/calib");
constant->Mkdir("global/magnetic_field");
constant->Mkdir("mwc");
constant->Mkdir("mwc/geometry");
constant->Mkdir("mwc/params");
constant->Mkdir("mwc/calib");
constant->Mkdir("svt");
constant->Mkdir("svt/geometry");
constant->Mkdir("svt/params");
constant->Mkdir("svt/calib");
constant->Mkdir("tpc");
constant->Mkdir("tpc/geometry");
constant->Mkdir("tpc/params");
constant->Mkdir("tpc/calib");

// Create a "geant" sub-directory


// mkdir  /dui/geant/
// mkdir  /dui/geant/Event
// mkdir  /dui/geant/Run


geant = new St_DataSetIter(dui.Pwd());

geant->Mkdir("geant");
geant->Cd("geant");
//geant->Mkdir("Event");
//geant->Mkdir("Run");
// Create a "raw_data" sub-directory 
 
// mkdir  /dui/raw_data 
// mkdir  /dui/raw_data/emc/
// mkdir  /dui/raw_data/emc/pixels
// mkdir  /dui/raw_data/ftpc/
// mkdir  /dui/raw_data/ftpc/pixels
// mkdir  /dui/raw_data/svt/
// mkdir  /dui/raw_data/svt/pixels
// mkdir  /dui/raw_data/tpc/
// mkdir  /dui/raw_data/tpc/pixels
raw_data = new St_DataSetIter(dui.Pwd());

raw_data->Mkdir("raw_data");
raw_data->Cd("raw_data");
raw_data->Mkdir("emc");
raw_data->Mkdir("emc/pixels");
raw_data->Mkdir("ftpc");
raw_data->Mkdir("ftpc/pixels");
raw_data->Mkdir("svt");
raw_data->Mkdir("svt/pixels");
raw_data->Mkdir("tpc");
raw_data->Mkdir("tpc/pixels");
  
// Create a "data" sub-directory

  
// mkdir  /dui/data/ 
// mkdir  /dui/data/ctf/
// mkdir  /dui/data/emc/
// mkdir  /dui/data/emc/tracks/
// mkdir  /dui/data/emc/hits/
// mkdir  /dui/data/ftpc/
// mkdir  /dui/data/ftpc/tracks/
// mkdir  /dui/data/ftpc/hits/
// mkdir  /dui/data/global
// mkdir  /dui/data/global/tracks
// mkdir  /dui/data/global/vertices
// mkdir  /dui/data/mwc/
// mkdir  /dui/data/svt/
// mkdir  /dui/data/svt/tracks/
// mkdir  /dui/data/svt/hits/
// mkdir  /dui/data/tpc/
// mkdir  /dui/data/tpc/tracks/
// mkdir  /dui/data/tpc/hits/
// mkdir  /dui/data/trg       
data = new St_DataSetIter(dui.Pwd());

data->Mkdir("data");
data->Cd("data");
data->Mkdir("ctf");
data->Mkdir("emc");
data->Mkdir("emc/tracks");
data->Mkdir("emc/hits");
data->Mkdir("ftpc");
data->Mkdir("ftpc/tracks");
data->Mkdir("ftpc/hits");
data->Mkdir("global");
data->Mkdir("global/tracks");
data->Mkdir("global/vertices");
data->Mkdir("mwc");
data->Mkdir("svt");
data->Mkdir("svt/tracks");
data->Mkdir("svt/hits");
data->Mkdir("tpc");
data->Mkdir("tpc/tracks");
data->Mkdir("tpc/hits");
data->Mkdir("trg");
}



//__________________________________________________________________
St_XDFFile *g2t_file_init (Char_t *g2t_file){

//*** make place to keep the input 
//cd /dui

//*** open input streams 
  cout << " Opening input file:"<< g2t_file  << endl;
//newfilestream G2Tfile [g2t_file] r
  St_XDFFile *G2Tfile = new St_XDFFile(g2t_file);
//*** read first "non-information" event (from G2T) 
//stream/getevent G2Tfile /dui/geant 
  St_DataSet *run_old = geant->Next("Run");
  if (run_old) delete run_old;
  St_DataSet *run = G2Tfile->NextEvent();
  (geant->Pwd())->Add(run);
#ifdef __CINT__
  St_g2t_run *g2t_run = geant->GetTableObject("Run/g2t_run");
  
  g2t_run_st *g2t_run_t =  g2t_run->GetTable();
  cout << " g2t_run :" << g2t_run_t[0]->author;
  cout << " date :" << g2t_run_t[0]->date;
  cout << " time :" << g2t_run_t[0]->time << endl;
#endif
  return G2Tfile;
//cd /dui

} 
//______________________________________________***

void g2t_file_close(St_XDFFile *file){

//  cd /dui

//  stream/close G2Tfile
  cout << "stream/close G2Tfile" << endl;
  if (file) delete file;
}

 
//______________________________________________

void g2t_file_read(St_XDFFile *file){

// cd /dui


// stream/getevent G2Tfile /dui/geant
  // geant->Rmdir("Event"); 
 St_DataSet *p = geant->Pwd();
 St_DataSet *set = geant->Next("Event");
 p->Remove(set); delete set;
 St_DataSet *event = file->NextEvent();
 if (event) {cout << event << endl;
 geant->Pwd()->Add(event);
 event->ls();}
 else g2t_file_close (file);
// cd /dui 

}
//__________________________________________________________________________
void GetXdFile(Char_t *filename, const St_DataSet *dataset)
{
  cout << " Opening input file:"<< filename  << endl;
  if (!(dataset && filename && strlen(filename))) return;
  St_XDFFile tpg_xdf(filename);
  St_DataSet *event = tpg_xdf.NextEvent();
  if (event) dataset->Add(event);
}
//__________________________________________________________________________
//void GetXdFile(Char_t *filename, const St_DataSetIter &datasets)
//{
//  St_DataSet *set = datasets.Pwd();
//  if (set) GetXdFile(filename,set);
//}
//______________________________________________***
void tpg_init(){
  St_DataSetIter local(dui("/dui/constants/tpc/geometry")); //  cd /dui/constants/tpc/geometry
//  dio/newfilestream   geom /afs/rhic/star/packages/SL98c/params/tpc/tpg_pars.xdf r
//  dio/stream/getevent geom 
//  dio/stream/close    geom
  Char_t *tpg_pars = "/afs/rhic/star/packages/SL98c/params/tpc/tpg_pars.xdf";
  St_DataSet *set = local.Pwd();
  GetXdFile(tpg_pars,set);
//  GetXdFile(tpg_pars,local.Pwd());
//  cd tpgpar
    local.Cd("tpgpar");
//  tdm/newtable tpg_pad tpg_pad 1
  St_tpg_pad *tpg_pad = new St_tpg_pad("tpg_pad",1); local.AddTable(tpg_pad);
//  cd /dui
//  ami/module/call tpg_main /dui/constants/tpc/geometry/tpgpar/tpg_pad_plane _
//                           /dui/constants/tpc/geometry/tpgpar/tpg_detector _
//                         /dui/constants/tpc/geometry/tpgpar/tpg_pad
  St_DataSet *set1 = local("tpg_pad_plane");
  St_DataSet *set2 = local("tpg_detector");
  if (set1 && set2) {
  St_tpg_pad_plane *tpg_pad_plane = set1->GetStafTable();
  St_tpg_detector *tpg_detector = set2->GetStafTable();
  Int_t res = tpg_main(tpg_pad_plane,tpg_detector,tpg_pad);
  }
  else {cout << "tpg_init fails" << endl;}
//  cd /dui
}
//___________________________________________________
void create_tables_tpc_slow_sim_tss(){
  St_DataSetIter local(dui("/dui/raw_data/tpc/pixels")); //  cd /dui/raw_data/tpc/pixels
//  tdm/newtable tppad   tss_tppad    10000
//  tdm/newtable tppixel tss_tppixel  900000 
//  tdm/newtable tpmcpix tss_tpmcpix  900000
  St_tss_tppad   *tppad   = new St_tss_tppad  ("tppad",  900000); local.AddTable(tppad);  
  St_tss_tppixel *tppixel = new St_tss_tppixel("tppixel",900000); local.AddTable(tppixel);
  St_tss_tpmcpix *tpmcpix = new St_tss_tpmcpix("tpmcpix",900000); local.AddTable(tpmcpix);
     
  St_DataSetIter local2(constant->Next("tpc/calib")); //  cd /dui/constants/tpc/calib
//  tdm/newtable tpc_pedestal tpc_pedestal 1
//  tdm/newtable readout_map  readout_map  1
//  tdm/newtable bad_channels bad_channels 1
  St_tpc_pedestal *tpc_pedestal = new St_tpc_pedestal("tpc_pedestal",1); local2.AddTable(tpc_pedestal);
  St_readout_map  *readout_map  = new St_readout_map ("readout_map", 1); local2.AddTable(readout_map);
  St_bad_channels *bad_channels = new St_bad_channels("bad_channels",1); local2.AddTable(bad_channels);
//  cd /dui
}
//___________________________________________________
void create_tables_tpc_slow_sim_tfc(){
    St_DataSetIter local(dui("/dui/raw_data/tpc/pixels")); //  cd /dui/raw_data/tpc/pixels
//  tdm/newtable adcxyz  tfc_adcxyz   900000
    St_tfc_adcxyz *adcxyz   = new St_tfc_adcxyz  ("adcxyz", 900000); local.AddTable(adcxyz);
    cout << "create_tables_tpc_slow_sim_tfc adcxyz="<< adcxyz <<endl;
//  cd /dui
}
//___________________________________________________

void create_tables_tpc_slow_sim_tcl(){
  St_DataSetIter local(dui("/dui/data/tpc/hits")); //cd /dui/data/tpc/hits
//  tdm/newtable tphit     tcl_tphit     20000
//  tdm/newtable tphitau   tcl_tphit_aux 20000
  St_tcl_tphit   *tphit     = new St_tcl_tphit  ("tphit",    20000); local.AddTable(tphit);  
  St_tcl_tphit_aux *tphit_aux = new St_tcl_tphit_aux("tphit_aux",20000); local.AddTable(tphit_aux);
  //cd /dui/data/tpc/hits
//  tdm/newtable tpcluster tcl_tpcluster 20000
//  tdm/newtable tpseq     tcl_tpseq     100000
  St_tcl_tpcluster *tpcluster = new St_tcl_tpcluster("tpcluster", 20000); local.AddTable(tpcluster); 
  St_tcl_tpseq     *tpseq     = new St_tcl_tpseq     ("tpseq",   100000); local.AddTable(tpseq);
  //  cd /dui
}
//___________________________________________________
  void tss_init(){
    //  cd /dui/constants/tpc/params
    //  dio/newfilestream    tss /afs/rhic/star/packages/SL98c/params/tpc/tss_pars.xdf r
    //  dio/stream/getevent  tss
    //  dio/stream/close     tss
    //  cd /dui
    //    St_DataSetIter local(dui("/dui/constants/tpc/params"));
    //    GetXdFile("/afs/rhic/star/packages/SL98c/params/tpc/tss_pars.xdf",local.Pwd());
  Char_t *tss_pars = "/afs/rhic/star/packages/SL98c/params/tpc/tss_pars.xdf";
  St_DataSet *set = dui("/dui/constants/tpc/params");
  GetXdFile(tss_pars,set);
}
void initialize(Int_t adcxyzon){

  cout << " Initializing for tpc slow simulator " << endl;
  cout << " adc xyz on = " << adcxyzon << endl;
// *  create tables
  create_tables_tpc_slow_sim_tss(); //exec create_tables_tpc_slow_sim_tss
  //    if [adcxyzon].eq.1 then
  wait();
  if (adcxyzon == 1)  create_tables_tpc_slow_sim_tfc();//   exec create_tables_tpc_slow_sim_tfc
  //  endif
  //  wait();
  create_tables_tpc_slow_sim_tcl();// exec create_tables_tpc_slow_sim_tcl



// *   initialization for tpc slow simulator
  tss_init();// exec tss_init 
  tcl_init();// exec tcl_init
 }
//_______ tcl_init.kumac ____________________
void tcl_init(){
//cd /dui/constants/tpc/params
//  dio/newfilestream tclp /afs/rhic/star/packages/SL98c/params/tpc/tcl_pars.xdf r
//  dio/stream/getevent    tclp
//  dio/stream/close       tclp
//cd /dui
  //    St_DataSetIter local(dui("/dui/constants/tpc/params"));
  //    GetXdFile("/afs/rhic/star/packages/SL98c/params/tpc/tcl_pars.xdf",local.Pwd());
  Char_t *tcl_pars = "/afs/rhic/star/packages/SL98c/params/tpc/tcl_pars.xdf";
  St_DataSet *set = dui("/dui/constants/tpc/params");
  GetXdFile(tcl_pars,set);
}
// _____________________ tss_run.kumac  ____________________________________
void tss_run(){  
cout << "start tss_run" << endl;
//  cd /dui
//  tdm/table/rowcount /dui/raw_data/tpc/pixels/tppixel 0
//  tdm/table/rowcount /dui/raw_data/tpc/pixels/tpmcpix 0
//  tdm/table/rowcount /dui/raw_data/tpc/pixels/tppad   0
St_tss_tsspar    *tsspar       = constant->GetTableObject("tpc/params/tsspars/tsspar");
St_tpg_detector  *tpg_detector = constant->GetTableObject("tpc/geometry/tpgpar/tpg_detector");
St_tpg_pad_plane *tpg_pad_plane= constant->GetTableObject("tpc/geometry/tpgpar/tpg_pad_plane");
St_g2t_tpc_hit   *g2t_tpc_hit  = geant->GetTableObject("Event/g2t_tpc_hit");
St_g2t_track     *g2t_track    = geant->GetTableObject("Event/g2t_track");
St_tss_tppixel       *tppixel  = raw_data->GetTableObject("tpc/pixels/tppixel");
St_tss_tpmcpix       *tpmcpix  = raw_data->GetTableObject("tpc/pixels/tpmcpix");
St_tss_tppad         *tppad    = raw_data->GetTableObject("tpc/pixels/tppad");
St_bad_channels  *bad_channels = constant->GetTableObject("tpc/calib/bad_channels");
St_readout_map   *readout_map  = constant->GetTableObject("tpc/calib/readout_map");
St_tpc_pedestal  *tpc_pedestal = constant->GetTableObject("tpc/calib/tpc_pedestal");

//
//  ami/module/call tssam_
//        /dui/constants/tpc/params/tsspars/tsspar_
//        /dui/constants/tpc/geometry/tpgpar/tpg_detector_
//        /dui/constants/tpc/geometry/tpgpar/tpg_pad_plane_
//        /dui/geant/Event/g2t_tpc_hit_
//        /dui/geant/Event/g2t_track_
//        /dui/raw_data/tpc/pixels/tppixel_
//        /dui/raw_data/tpc/pixels/tpmcpix_
//        /dui/raw_data/tpc/pixels/tppad_
//        /dui/constants/tpc/calib/bad_channels_
//        /dui/constants/tpc/calib/readout_map_
//        /dui/constants/tpc/calib/tpc_pedestal
 cout << " tsspar " << tsspar << endl;
 cout << " tpg_detector " << tpg_detector << endl;
 cout << " tpg_pad_plane " << tpg_pad_plane << endl;
 cout << " g2t_tpc_hit " << g2t_tpc_hit << endl;
 cout << " g2t_track " << g2t_track << endl;
 cout << " tppixel " << tppixel << endl;
 cout << " tpmcpix " << tpmcpix << endl;
 cout << " tppad " << tppad << endl;
 cout << " bad_channels " << bad_channels << endl;
 cout << " readout_map " << readout_map << endl;
 cout << " tpc_pedestal " << tpc_pedestal << endl;
    staf_status = 
          tssam(tsspar,tpg_detector,tpg_pad_plane,g2t_tpc_hit,g2t_track,tppixel,
           tpmcpix,tppad,bad_channels,readout_map,tpc_pedestal);
//  if ( staf_status(1) .ne. STAFCV_OK) then
    //    if ( staf_status != STAFCV_OK) {printf("Problem running tss...\n");}
    //    else { printf("finish tss_run\n"); }
//  endif
    //    wait(); 
} 
//________________ tfc_run.kumac ___________________

void tfc_run(){ 

cout << "start tfc_run" << endl;

//  cd /dui
//  tdm/table/rowcount /dui/raw_data/tpc/pixels/adcxyz 0

//  alias/create STAFCV_OK 1
//  alias/create STAFCV_BAD 0

St_tpg_pad_plane *tpg_pad_plane = constant->GetTableObject("tpc/geometry/tpgpar/tpg_pad_plane");
St_tpg_detector  *tpg_detector  = constant->GetTableObject("tpc/geometry/tpgpar/tpg_detector");
St_tss_tppixel       *tppixel   = raw_data->GetTableObject("tpc/pixels/tppixel");
St_tss_tppad         *tppad     = raw_data->GetTableObject("tpc/pixels/tppad");
St_tfc_adcxyz        *adcxyz    = raw_data->GetTableObject("tpc/pixels/adcxyz");

//  ami/module/call xyz_
//       /dui/constants/tpc/geometry/tpgpar/tpg_pad_plane_
//       /dui/constants/tpc/geometry/tpgpar/tpg_detector_
//       /dui/raw_data/tpc/pixels/tppixel_
//       /dui/raw_data/tpc/pixels/tppad_
//       /dui/raw_data/tpc/pixels/adcxyz
 cout << "call xyz" << endl;
 cout << "*tpg_pad_plane" << tpg_pad_plane << endl;
 cout << "*tpg_detector" << tpg_detector << endl;
 cout << "*tss_tppixel" << tppixel << endl;
 cout << "*tppad" << tppad << endl;
 cout << "*adcxyz" << adcxyz << endl;
    staf_status = xyz(tpg_pad_plane,tpg_detector,tppixel,tppad,adcxyz);
    wait(); 
//  if ( staf_status(1) .ne. STAFCV_OK) then
  if (staf_status !=  STAFCV_OK)   cout << "Problem running tfc..." << endl;
  else   cout << "finish tfc_run" << endl;
}

//_______ tcl_run.kumac ___________________________

void run_tcl_make_clusters(){

cout << "start run_tcl_make_clusters" << endl;

//  cd /dui
//  tdm/table/rowcount /dui/data/tpc/hits/tpseq     0
//  tdm/table/rowcount /dui/data/tpc/hits/tpcluster 0

//  alias/create STAFCV_OK 1
//  alias/create STAFCV_BAD 0

St_tpg_pad_plane *tpg_pad_plane = constant->GetTableObject("tpc/geometry/tpgpar/tpg_pad_plane");
St_tss_tppad     *tppad         = raw_data->GetTableObject("tpc/pixels/tppad");
St_tss_tppixel   *tppixel       = raw_data->GetTableObject("tpc/pixels/tppixel");
St_tss_tpmcpix   *tpmcpix       = raw_data->GetTableObject("tpc/pixels/tpmcpix");
St_tcl_tpcluster *tpcluster     = data->GetTableObject("tpc/hits/tpcluster");
St_tcl_tpseq     *tpseq         = data->GetTableObject("tpc/hits/tpseq");

//  ami/module/call tcl_make_clusters_
//        /dui/constants/tpc/geometry/tpgpar/tpg_pad_plane_
//        /dui/raw_data/tpc/pixels/tppad_
//        /dui/raw_data/tpc/pixels/tppixel_
//        /dui/raw_data/tpc/pixels/tpmcpix_
//        /dui/data/tpc/hits/tpcluster_
//        /dui/data/tpc/hits/tpseq
  staf_status = tcl_make_clusters(tpg_pad_plane,tppad,tppixel,tpmcpix,tpcluster,tpseq);
    wait(); 
//  if ( staf_status(1) .ne. STAFCV_OK) then
  if (staf_status != STAFCV_OK) cout << "Problem running tcl_make_clusters..." << endl;
  else   cout << "finish run_tcl_make_clusters" << endl;
}

//_________________________________________

void run_tpham(){

cout << "start run_tpham" << endl;

//  cd /dui
//  tdm/table/rowcount /dui/data/tpc/hits/tphit 0
//  tdm/table/rowcount /dui/data/tpc/hits/tphitau 0

//  alias/create STAFCV_OK 1
//  alias/create STAFCV_BAD 0
St_DataSetIter tpcIterator(constant->Next("tpc")); 
//tpcIterator.Cd("tpc");
St_tcl_tclpar    *tclpar        = tpcIterator.GetTableObject("tpc/params/tclpars/tclpar");
St_tss_tsspar    *tsspar        = tpcIterator.GetTableObject("tpc/params/tsspars/tsspar");
St_tpg_detector  *tpg_detector  = tpcIterator.GetTableObject("tpc/geometry/tpgpar/tpg_detector");
St_tpg_pad_plane *tpg_pad_plane = tpcIterator.GetTableObject("tpc/geometry/tpgpar/tpg_pad_plane");

St_DataSetIter raw_tpc(raw_data->Next("tpc")); raw_tpc.Cd("tpc");
St_tss_tppixel *tppixel     = raw_tpc.GetTableObject("pixels/tppixel");
St_tss_tpmcpix *tpmcpix     = raw_tpc.GetTableObject("pixels/tpmcpix");

St_DataSetIter hits(data->Next("tpc/hits")); hits.Cd("hits");
St_tcl_tpseq     *tpseq     = hits.GetTableObject("tpseq");
St_tcl_tpcluster *tpcluster = hits.GetTableObject("tpcluster");
St_tcl_tphit     *tphit     = hits.GetTableObject("tphit");
St_tcl_tphit_aux *tphitau   = hits.GetTableObject("tphit_aux");

//  ami/module/call tpham_
//        /dui/constants/tpc/params/tclpars/tclpar_
//        /dui/constants/tpc/params/tsspars/tsspar_
//        /dui/constants/tpc/geometry/tpgpar/tpg_detector_
//        /dui/constants/tpc/geometry/tpgpar/tpg_pad_plane_
//        /dui/raw_data/tpc/pixels/tppixel_
//        /dui/raw_data/tpc/pixels/tpmcpix_
//        /dui/data/tpc/hits/tpseq_
//        /dui/data/tpc/hits/tpcluster_
//        /dui/data/tpc/hits/tphit_
//        /dui/data/tpc/hits/tphitau

cout << "dui/constants/tpc/params/tclpars" << tclpar << endl;
cout << "dui/constants/tpc/params/tsspars" << tsspar << endl;
cout << "dui/constants/tpc/geometry/tpgpar" << tpg_detector << endl;
cout << "dui/constants/tpc/geometry/tpgpar" << tpg_pad_plane << endl;
cout << "dui/raw_data/tpc/pixels" << tppixel << endl;
cout << "dui/raw_data/tpc/pixels" << tpmcpix << endl;
cout << "dui/data/tpc/hits" << tpseq << endl;
cout << "dui/data/tpc/hits" << tpcluster << endl;
cout << "dui/data/tpc/hits" << tphit << endl;
cout << "dui/data/tpc/hits" << tphitau << endl;

  staf_status = tpham(tclpar,tsspar,tpg_detector,tpg_pad_plane,tppixel,tpmcpix,tpseq,tpcluster,tphit,tphitau);
    wait(); 
  //  if ( staf_status(1) .ne. STAFCV_OK) then
  if ( staf_status != STAFCV_OK) cout << "Problem running tpham..." << endl;
  else   cout << "finish run_tpham" << endl;
}
//_______________________________________________________________________
void process_event(Int_t nev, Int_t adcxyzon){
  cout << " TPC slow sim: Processing Event # "<< nev << endl; 
  cout << "  adcxyzon = "<< adcxyzon << endl;
// call tssam - simulated tpc pixels
  //   tss_run(); //exec tss_run

// call xyz - reordered tpc pixels with more info
//  if [adcxyzon].eq.1 then
  if (adcxyzon == 1) tfc_run();//    exec tfc_run
//  endif

// call tcl_make_clusters - tpc cluster finder
  run_tcl_make_clusters(); // exec run_tcl_make_clusters

// call tpham - tpc hit finder
  run_tpham();//exec run_tpham 
}
//________________________________________________________________
void tpc_slow_sim(){
  gBenchmark->Start("tpc_slow_sim");
//Char_t *input_g2t_file = "/star/mds/data/SD98/auau200/g2t/central/hijing/set0001/regular/auau_ce_b0-2_0001_0010.xdf";
  Char_t *input_g2t_file = "/afs/rhic/star/data/samples/muons_100_ctb.dsl";
  Int_t  num_events=1;
  Int_t  tpc_sector_first=1;
  Int_t  tpc_sector_last=24;
  Int_t  adcxyzon=0;
//          INPUT_DATA_DIR=/star/mds/data/SD98/auau200/g2t/central/hijing/set0001/regular/_
//          input_g2t_file=auau_ce_b0-2_0001_0010.xdf_
//          num_events=1_
//          tpc_sector_first=1_
//          tpc_sector_last=24_
//          funit=21 _
//          nfile=tphit.ntup _
//          nid=111_
//          table=/dui/data/tpc/hits/tphit_
//          adcxyzon=1
//  
// --> to run
//    - change the inputs above as desired
//    - run staf++
//    -   make tpc   (you'll get pams needed: tpg,tfc,tss,tcl)
//    -   exec tpc_slow_sim
//         
// in general, the things in [small letters] are input values
//                           [CAPITAL LETTERS] are directories   
//
//
  cout << " *** Input G2T file -->         " << input_g2t_file << endl; 
  cout << " *** Num Events to process -->  " << num_events     << endl; 
  cout << " *** TPC Sector first,last -->  " << tpc_sector_first << tpc_sector_last << endl; 
  cout << " *** Fill adcxyz table for diag.? 0,1 = no,yes --> " << adcxyzon << endl; 
// generic inits
  setup_dir(); // exec setup_dir
// exec input_g2t_file_init [INPUT_DATA_DIR][input_g2t_file]
  St_XDFFile *g2t_file = g2t_file_init(input_g2t_file);
//  wait();
//  if STAF_STATUS(1).eq.0 goto ENDOFDATA
  if (g2t_file) { 
// * tpc geometry init
    tpg_init();//  exec tpg_init
//  wait();

// * tpc slow sim init
  initialize(adcxyzon);//  exec initialize [adcxyzon]
  wait();

// * now overwrite which sector you want to run (in case you don't want default).q
  // St_DataSet *set = dui("/dui/constants/tpc/params/tsspars");// cd /dui/constants/tpc/params/tsspars
   St_tss_tsspar *tsspar    = constant->GetTableObject("tpc/params/tsspars/tsspar");
   tss_tsspar_st *tsspar_st = tsspar->GetTable();
// tdm/table/cell/putvalue 'tsspar[0].min_sect' [tpc_sector_first]
   tsspar_st[0]->min_sect = tpc_sector_first;
//   tdm/table/cell/putvalue 'tsspar[0].max_sect' [tpc_sector_last]
   tsspar_st[0]->max_sect = tpc_sector_last;
   cout << "Minimum and Maximum Sector numbers for tpc slow simulation: "; 
//   tdm/table/cell/getvalue 'tsspar[0].min_sect'
//   tdm/table/cell/getvalue 'tsspar[0].max_sect'
//   tdm/table/cell/getvalue 'tsspar[0].bfield'
     cout << tsspar_st[0]->min_sect << "   " << tsspar_st[0]->max_sect << "  " << tsspar_st[0]->bfield << endl;
//wait
//   cd /dui
// * read event and then process thru tpc slow sim
//  do i=1,[num_events]
    Int_t i;
    for (i=1;i<=num_events;i++){
      cout << " processing event " << i << endl; 
      g2t_file_read(g2t_file); //exec input_g2t_file_read
//    if STAF_STATUS(1).eq.0 goto ENDOFDATA    
      if (!geant->Next("Event")) break;
      process_event(i,adcxyzon);//   exec process_event [i] [adcxyzon]
  wait();
// * write table from tpc fast sim to ntuple
		    //  enddo
					    }
    //  ENDOFDATA:
    //    if [i].lt.[num_events] then
    if (i < num_events)  cout << "End of input file" << endl;

  cout << "close input file" << endl;
    g2t_file_close();// exec input_g2t_file_close
  gBenchmark->Stop("tpc_slow_sim");
  gBenchmark->Print("tpc_slow_sim");
    }
}



