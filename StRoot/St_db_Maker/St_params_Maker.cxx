// $Id: St_params_Maker.cxx,v 1.1 1999/01/02 19:08:15 fisyak Exp $
// $Log: St_params_Maker.cxx,v $
// Revision 1.1  1999/01/02 19:08:15  fisyak
// Add ctf
//
// Revision 1.1  1998/10/31 00:28:32  fisyak
// Makers take care about branches
//
// Revision 1.6  1998/10/06 18:00:43  perev
// cleanup
//
// Revision 1.5  1998/09/18 23:03:33  fisyak
// put iosream into <>
//
// Revision 1.4  1998/09/15 20:55:24  fisyak
// Split St_DataSet -> St_DataSet + St_DataSetIter
//
// Revision 1.3  1998/08/14 15:25:40  fisyak
// Move out tpg from run
//
// Revision 1.2  1998/08/10 02:34:34  fisyak
// Add St_laser_Maker
//
// Revision 1.1  1998/08/07 19:34:53  fisyak
// Add St_params_Maker
//
// Revision 1.2  1998/07/20 15:08:15  fisyak
// Add tcl and tpt
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_params_Maker class for Makers                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include <iostream.h>
#include "TApplication.h"
#include "TInterpreter.h"
#include "St_params_Maker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "St_FileSet.h"
#include "tpc/St_tss_Module.h"
#include "tpc/St_xyz_newtab_Module.h"
#include "svt/St_svg_am_Module.h"
//_____________________________________________________________________________
EDataSetPass GetCparams(St_DataSet *set){
  if (strcmp(set->GetTitle(),"file") == 0){
    if (strstr(set->GetName(),".C")){
      // define the source dataset
      St_DataSet *parent = set->GetParent();

      // define the target dataset
      St_DataSet *target = gStChain->DataSet("params");
      if (target) {
        St_DataSetIter local(target);
        target = local.Cd(target->GetName());
 
        // Create the data-structure of the target dataset if any
        local.Reset(target);
        const TString parpath = parent->Path();
        St_DataSet *loc = local(parpath);
        if (!loc) loc = local.Mkdir(parpath);
	// Include the dataset from the XDF file into the target dataset
        if (loc) {
            TString  command = ".L /afs/rhic/star/packages/dev/StDb"; 
            command += set->Path();
	    //	    gROOT->LoadMacro(path);
	    gInterpreter->ProcessLine(command.Data());
	    St_DataSet *newset = (St_DataSet *) gInterpreter->Calc("CreateTable()");
            command.ReplaceAll(".L ",".U "); 
	    //	    gInterpreter->ProcessLine(command.Data());
  
	    //St_DataSet *newset = (St_DataSet *) gROOT->ProcessLine(path);
	    //            St_DataSet *newset = (St_DataSet *) G__exec_tempfile(path.Data());
            if (newset) loc->Add(newset);
        }
      }
    }
  }
  return kContinue;
}
ClassImp(St_params_Maker)

//_____________________________________________________________________________
  St_params_Maker::St_params_Maker(const char *name, const char *title):
StMaker(name,title){
   drawinit=kFALSE;
}
//_____________________________________________________________________________
St_params_Maker::~St_params_Maker(){
}
//_____________________________________________________________________________
void St_params_Maker::Clear(Option_t *option){
  // SafeDelete(m_DataSet);
}
//_____________________________________________________________________________
Int_t St_params_Maker::Init(){
// read parameters structure from STAR_PARAMS
   St_FileSet params("/afs/rhic/star/packages/dev/StDb","");
   if (! m_DataSet) m_DataSet = new St_DataSet("params");
   params.Pass(GetCparams);
// Complete parameters structure
   St_DataSetIter       local(m_DataSet);
// TPC Calibration tables
   St_DataSet *cal = gStChain->DataSet("calib");
   if (cal) {
     St_DataSetIter       calib(cal);
     if (!calib("tpc"))  calib.Mkdir("tpc"); 
     calib.Cd("tpc");
     St_tpc_pedestal *tpc_pedestal = (St_tpc_pedestal *) calib("tpc_pedestal");
     if (!tpc_pedestal) {
       tpc_pedestal = new St_tpc_pedestal("tpc_pedestal",1);
       calib.Add(tpc_pedestal);
     }
     St_readout_map *readout_map = (St_readout_map *) calib("readout_map");
     if (!readout_map) {
       readout_map = new St_readout_map("readout_map",1);
       calib.Add(readout_map);
     }
     St_bad_channels *bad_channels = (St_bad_channels *) calib("bad_channels");
     if (!bad_channels) {
       bad_channels = new St_bad_channels("bad_channels",1);
       calib.Add(bad_channels);
     }
   }
// TPC tss parameters
// TPC tcl parameters 
   St_DataSet *tclpars = local("tpc/tclpars");
   if (tclpars){
     St_DataSetIter partable(tclpars);
     St_tcl_sector_index *tcl_sector_index = new St_tcl_sector_index("tcl_sector_index",1); 
     partable.Add(tcl_sector_index);
   }
// TPC tpt parameters
   St_DataSet *tptpars = local("tpc/tptpars");
// TPC tid parameters
   St_DataSet *tidpars = local("tpc/tidpars");   
// SVT geometry parameters
   St_svg_shape  *shape  = (St_svg_shape *) local("svt/svgpars/shape");
   St_svg_config *config = (St_svg_config*) local("svt/svgpars/config");
   St_svg_geom   *geom   = (St_svg_geom  *) local("svt/svgpars/geom");
   if (!(shape && config)){
     cout << " St_params_Maker:tpg_pad_plane or tpg_detector do not exist" << endl;
   }
   else {
//    if (!geom) Int_t res = svg_am(config,shape,geom);
   }
//SVT srs params
//SVT stk params
// Create Histograms    
   return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_params_Maker::Make(){
  //  PrintInfo();
 return kStOK;
}
//_____________________________________________________________________________
void St_params_Maker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: St_params_Maker.cxx,v 1.1 1999/01/02 19:08:15 fisyak Exp $\n");
  printf("**************************************************************\n");
  if (gStChain->Debug()) StMaker::PrintInfo();
}

