// $Id: St_QA_Maker.cxx,v 1.39 1999/07/09 12:12:54 kathy Exp $
// $Log: St_QA_Maker.cxx,v $
// Revision 1.39  1999/07/09 12:12:54  kathy
// fixed all globtrk histograms as per 3pm thursday meeting
//
// Revision 1.38  1999/07/08 22:20:57  kathy
// updated limits on hist
//
// Revision 1.37  1999/07/07 21:23:16  kathy
// fixed log scales
//
// Revision 1.36  1999/07/07 16:58:32  kathy
// put log scales on some histograms
//
// Revision 1.35  1999/07/02 21:56:56  kathy
// update for tables which exist in 99f AND put in changes to event summary and globtrk histogram sets requested by offline analysis meeting
//
// Revision 1.34  1999/06/30 20:35:35  kathy
// now have 2D histograms being plotted with box plots instead of scatter plots
//
// Revision 1.33  1999/06/17 18:25:32  kathy
// fix so writes out blank canvas
//
// Revision 1.32  1999/06/15 14:44:52  kathy
// fix St_QA_Maker
//
// Revision 1.30  1999/06/11 20:05:51  kathy
// put in method FindHists to find the histogram directory, since it can be in different places depending on how/where you make the histograms
//
// Revision 1.29  1999/05/10 20:03:54  kathy
// add new member function ExamineLogYList and RemoveFromLogYList
//
// Revision 1.28  1999/05/10 17:16:16  kathy
// added new member function SetDefaultLogYList and implemented and tested
//
// Revision 1.27  1999/05/07 20:20:53  kathy
// now set logy on when hist name is in loglist
//
// Revision 1.26  1999/05/07 17:18:29  kathy
// new method AddToLogYList implemented and tested on solaris
//
// Revision 1.25  1999/05/06 12:48:44  fisyak
// Add search geant in search path for particle tables
//
// Revision 1.24  1999/05/05 19:35:52  kathy
// add new method ListHists and clean up
//
// Revision 1.23  1999/04/28 18:39:29  kathy
// removed check of two different directory for GetDataSet because the infrastructure code should take care of this and not the Makers
//
// Revision 1.22  1999/04/27 21:05:29  kathy
// clean up comments
//
// Revision 1.21  1999/04/23 14:04:07  kathy
// just cleaning up comments
//
// Revision 1.20  1999/04/21 20:19:18  kathy
// put in comments and cleaned up - works for mdc2 dst in dev now
//
// Revision 1.19  1999/04/20 01:16:59  fisyak
// Add check on. no of tracks in dE/dX
//
// Revision 1.18  1999/04/19 20:33:42  didenko
// uncommented MakeHistGen fuction
//
// Revision 1.17  1999/04/19 18:07:57  didenko
// QA_Maker for new scheme DST
//
// Revision 1.16  1999/03/11 23:14:49  fisyak
// Victor scheme
// 
// Revision 1.15  1999/03/11 21:13:13  kathy
// update to hist limits
//
// Revision 1.14  1999/03/09 16:30:23  fine
// Workqround of the St_io_Maker bug
//
// Revision 1.13  1999/03/07 19:26:15  fine
// QA->SetPostScriptFile(psFile) has been introduced
//
// Revision 1.12  1999/03/07 16:53:32  fine
// New method DrawHists
//
// Revision 1.11  1999/03/05 21:19:37  kathy
// added new histograms
//
// Revision 1.10  1999/03/03 23:34:29  kathy
// fixes to histograms
//
// Revision 1.9  1999/02/26 18:42:33  kathy
// added vertex histograms
//
// Revision 1.8  1999/02/26 17:24:42  kathy
// fix histograms
//
// Revision 1.7  1999/02/25 21:11:56  kathy
// fix histograms
//
// Revision 1.6  1999/02/25 19:25:39  kathy
// fix up histograms
//
// Revision 1.5  1999/02/24 21:15:02  kathy
// fixed histograms and added a few new ones
//
// Revision 1.4  1999/02/23 22:22:22  kathy
// changes to histograms: titles changed so they'll be in order and redundant ones removed
//
// Revision 1.3  1999/02/22 21:27:17  kathy
// moved hist from St_glb_Maker to St_QA_Maker and had to rename some etc
//
// Revision 1.2  1999/02/20 00:24:48  kathy
// fixed some of the histograms
//
// Revision 1.1  1999/02/08 19:28:23  didenko
// fixed directory level
//
// Revision 1.4  1999/01/22 22:53:14  didenko
// maker to fill QA histograms
//
// Revision 1.3  1999/01/22 22:19:57  didenko
// maker to fill QA histograms
//
// Revision 1.2  1998/12/21 19:43:17  fisyak
// Move ROOT includes to non system
//
// Revision 1.1  1998/11/01 16:42:25  fisyak
// dst analysis
//
///////////////////////////////////////////////////////////////////////////////
//                                                                           //
// St_QA_Maker class for Makers (evr + egr + ev0 + ev0_eval + event_summary  //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <stdlib.h>
#include <string.h>
#include "TStyle.h"
#include "TCanvas.h"
#include "TObjString.h"
#include "TPostScript.h"
#include "PhysicalConstants.h"
#include <math.h>
#include "TMath.h"
#include "St_QA_Maker.h"

// tables currently in 99e,99f
#include "St_dst_event_summary_Table.h"
#include "St_dst_event_header_Table.h"
#include "St_dst_track_Table.h"   // globtrk,globtrk2,primtrk
#include "St_dst_track_aux_Table.h" //primtrk_aux
#include "St_dst_vertex_Table.h"
#include "St_dst_point_Table.h"
#include "St_dst_v0_vertex_Table.h"
#include "St_dst_xi_vertex_Table.h"
#include "St_dst_dedx_Table.h"
#include "St_particle_Table.h"
#include "St_dst_TriggerDetectors_Table.h"
#include "St_g2t_rch_hit_Table.h"
#include "St_dst_monitor_soft_Table.h"

#include "StChain.h"
#include "St_DataSetIter.h"

const Int_t   St_QA_Maker::nxpT = 50;
const Int_t   St_QA_Maker::nyeta = 50;
const Float_t St_QA_Maker::xminpT = 0.0;
const Float_t St_QA_Maker::xmaxpT = 5.0;
const Float_t St_QA_Maker::ymineta = -2.0;
const Float_t St_QA_Maker::ymaxeta =  2.0;

const Int_t St_QA_Maker::nchisq = 50;
const Int_t St_QA_Maker::nmass  = 40;
const Int_t St_QA_Maker::ntau   = 40;
const Int_t St_QA_Maker::ndedx  = 50;
const Int_t St_QA_Maker::npnt   = 50;
const Int_t St_QA_Maker::nleng  = 50;
const Int_t St_QA_Maker::npsi   = 36;
const Int_t St_QA_Maker::knpsi  = 42;
const Int_t St_QA_Maker::ntrk   = 50;
const Int_t St_QA_Maker::nvrt   = 100;
const Int_t St_QA_Maker::nmnpt  = 50;
const Int_t St_QA_Maker::nmneta = 50;
const Int_t St_QA_Maker::nxyz   = 50;
const Int_t St_QA_Maker::knyeta = 60;
const Int_t St_QA_Maker::knid   = 10;
const Int_t St_QA_Maker::cnp   = 50;
const Int_t St_QA_Maker::cndedx = 50;    

const Float_t St_QA_Maker::kminnid  = 0.0;
const Float_t St_QA_Maker::kmaxnid  = 10.0;
const Float_t St_QA_Maker::minpsi   = 0.0;
const Float_t St_QA_Maker::kminpsi  = -60.0;
const Float_t St_QA_Maker::maxpsi   = 360.0;
const Float_t St_QA_Maker::minchisq = 0.;
const Float_t St_QA_Maker::maxchisq = 10.0;
const Float_t St_QA_Maker::minmass  = 0.0;
const Float_t St_QA_Maker::maxmass  = 2.0;
const Float_t St_QA_Maker::mindedx  = 0.0;
const Float_t St_QA_Maker::maxdedx  = 0.0005*1e6; // in keV/cm
const Float_t St_QA_Maker::minpnt   = 0.0;
const Float_t St_QA_Maker::maxpnt   = 50.0;
const Float_t St_QA_Maker::minleng  = 0.0;
const Float_t St_QA_Maker::maxleng  = 200.0;
const Float_t St_QA_Maker::mintau   = 0.0;
const Float_t St_QA_Maker::maxtau   = 20.0;
const Float_t St_QA_Maker::mintrk   = 0.0;
const Float_t St_QA_Maker::maxtrk   = 8000.0;
const Float_t St_QA_Maker::minvrt   = 2000.0;
const Float_t St_QA_Maker::maxvrt   = 4000.0;
const Float_t St_QA_Maker::minmpt   = 0.0;
const Float_t St_QA_Maker::maxmpt   = 2.0;
const Float_t St_QA_Maker::minmeta  = -0.2;
const Float_t St_QA_Maker::maxmeta  = 0.2;
const Float_t St_QA_Maker::kmineta  = -3.0;
const Float_t St_QA_Maker::kmaxeta  = 3.0;
const Float_t St_QA_Maker::minxyz   = 0.0;
const Float_t St_QA_Maker::maxxyz   = 50.0;
const Float_t St_QA_Maker::cminp = 0.0;
const Float_t St_QA_Maker::cmaxp = 2.0;
const Float_t St_QA_Maker::cmindedx = 0.0;
const Float_t St_QA_Maker::cmaxdedx =  0.1e-04*1e6; // change from GeV to keV per cm

ClassImp(St_QA_Maker)
  
//_____________________________________________________________________________
  St_QA_Maker::St_QA_Maker(const char *name, const char *title):StMaker(name,title)
{

// St_QA_Maker - constructor
//  - zero all pointers defined in the header file

  m_ListOfLog = 0;
  m_QACanvas = 0;

// for method MakeEvSum - from table event_summary
  m_trk_tot_gd = 0;       //! number of good global tracks divided by total
  m_glb_trk_tot=0;        //! # tracks total from globtrk
  m_glb_trk_plusminus=0;  //! # trks pos/neg. 
  m_glb_trk_prim=0;        //! # trks from primaries
  m_vert_total=0;    //! total number of vertices
  m_vert_V0=0;       //! number of V0 vertices
  m_mean_pt=0;       //! mean pt value
  m_mean_eta=0;      //! mean eta value 
  m_rms_eta=0;       //! rms eta value 
  m_T_average=0;     //! mean Temp
  m_prim_vrtx0=0;    //! primary vrtx x position
  m_prim_vrtx1=0;    //! primary vrtx y position
  m_prim_vrtx2=0;    //! primary vrtx z position
  m_vrtx_chisq=0;    //! primary vrtx chisq
  
// for method MakeGlob - from table globtrk

  m_globtrk_tot=0;
  m_globtrk_good=0;
  m_globtrk_iflag=0;
  m_det_id=0;
  m_point=0;
  m_max_point=0;
  m_fit_point=0;
  m_glb_charge=0;
  m_glb_x0=0;
  m_glb_xf=0;
  m_glb_y0=0;
  m_glb_yf=0;     
  m_glb_z0=0;     
  m_glb_zf=0;     
  m_psi=0;        
  m_tanl=0;       
  m_glb_theta=0;  
  m_eta=0;        
  m_pT=0;
  m_mom=0;        
  m_chisq0=0;     
  m_chisq1=0;     
  m_length=0;     
  m_glb_impact=0; 
  m_glb_ndf=0;    

  m_pT_eta_rec = 0;
  m_tanl_z0  = 0;
  m_mom_trklength = 0;
  m_eta_trklength = 0;
  m_npoint_length = 0;		  
  m_fpoint_length = 0;
  m_chisq0_mom = 0;
  m_chisq1_mom = 0;
  m_chisq0_eta = 0;
  m_chisq1_eta = 0;
  m_chisq0_dip = 0;
  m_chisq1_dip = 0;
  m_chisq0_z0 = 0;
  m_chisq1_z0 = 0;
  m_nfptonpt_mom = 0;
  m_nfptonpt_eta = 0;

  
// for method MakeDE - from table dst_dedx
  m_ndedx=0;         //! number of point to find dE/dx
  m_dedx0=0;         //! dE/dx [0]
  m_dedx1=0;         //! dE/dx [1] 
  
// for method MakeHistPrim - from table primtrk
  m_prim_pT=0;            //! pT  recostructed
  m_prim_eta=0;           //! eta recostructed
  m_prim_pT_eta_rec=0;    //! pT versus eta Spectra for reconstructed
  m_prim_tlength=0;       //! dst track length
  m_prim_chi2xd=0;        //! x chisq/degf
  m_prim_chi2yd=0;        //! y chisq/degf
  m_prim_point=0;         //! # points on track
  m_prim_fit_point=0;     //! # fitted points
  m_prim_psi=0;           //! psi angle_ 
  m_prim_det_id=0;        //! 
  m_prim_mom_trklength=0; //!
  m_prim_npoint_length=0; //!
  m_prim_fpoint_length=0; //!
  m_prim_chisq0_mom=0;    //!
  m_prim_chisq1_mom=0;    //!
  
  
  // for method MakeHistGen - from table particle
  m_H_pT_eta_gen=0; //! pT versus eta Spectra for generated
  m_H_pT_gen=0;     //! pT Spectra for generated
  m_H_eta_gen=0;    //! eta Spectra for generated
  m_H_vtxx=0;       //! production vertex (mm)
  m_H_vtxy=0;       //! production vertex (mm)
  m_H_vtxz=0;       //! production vertex (mm)
  m_H_npart=0;      //! total num particles generated
  m_H_ncpart=0;     //! number of charged e,mu,proton,kaon,pion
  
  // for MakeHistV0 - from table dst_v0_vertex
  m_ev0_lama_hist=0; //! Lambda mass
  m_ev0_k0ma_hist=0; //! K0 mass
  
  // for MakeHistPID - from tables primtrk & dst_dedx 
  m_p_dedx_rec=0;   //! dedx vs p
  
  
  // for method MakeHistVertex - from table dst_vertex
  m_v_detid=0; //! detector id where vertex was found 
  m_v_vtxid=0; //! vertex type
  m_v_x=0;     //! vertex coordinates in
  m_v_y=0;     //!  STAR reference 
  m_v_z=0;     //!   system
  m_v_pchi2=0; //! P(chi^2,ndf) of vertex fit
  
  m_pv_detid=0; //! row1-detector id where vertex was found 
  m_pv_vtxid=0; //! row1-vertex type
  m_pv_x=0;     //! row1-vertex coordinates in
  m_pv_y=0;     //!  STAR reference 
  m_pv_z=0;     //!   system
  m_pv_pchi2=0; //! row1-P(chi^2,ndf) of vertex fit
  

  // for method MakeHistXi

}
//_____________________________________________________________________________

St_QA_Maker::~St_QA_Maker(){
// St_QA_Maker - destructor
  SafeDelete(m_QACanvas);
  if (m_ListOfLog) {
    m_ListOfLog->Delete();
    SafeDelete(m_ListOfLog);
  }
}
//_____________________________________________________________________________

Int_t St_QA_Maker::DrawHists() 
{
// Method DrawHists -->
// Plot the selected  histograms and generate the postscript file as well 
  
  cout << " **** Now in St_QA_Maker::DrawHists  **** " << endl;


// set output ps file name
  TPostScript *psf = 0;
  const Char_t *psfileName = m_PsFileName.Data();
  if (!m_PsFileName.IsNull()) psf = new TPostScript((char *)psfileName);  
  
//set Style of Plots
  const Int_t numPads = m_PadColumns*m_PadRows;  
// SetPaperSize wants width & height in cm: A4 is 20,26 & US is 20,24
  gStyle->SetPaperSize(m_PaperWidth,m_PaperHeight); 
  gStyle->SetOptStat(111111);
//

//setup canvas
  SafeDelete(m_QACanvas);

// TCanvas wants width & height in pixels (712 x 950 corresponds to A4 paper)
  TCanvas *QACanvas = new TCanvas("CanvasName","Canvas Title",30*m_PaperWidth,30*m_PaperHeight);
//  QACanvas->SetFillColor(19);
  QACanvas->SetBorderSize(2);  
  QACanvas->Divide(m_PadColumns,m_PadRows);

  
  if (psf) psf->NewPage();
  const Char_t *firstHistName = m_FirstHistName.Data();
  const Char_t *lastHistName  = m_LastHistName.Data();


// Now find the histograms
// get the TList pointer to the histograms:
  TList  *dirList = 0;
  Char_t *dirHistName = "QAHist";
  dirList = FindHists(dirHistName);
 

  Int_t padCount = 0;
  
// Create an iterator called nextHist - use TIter constructor
  TIter nextHist(dirList);
  Int_t histCounter = 0;
  Int_t histReadCounter = 0;
  Bool_t started = kFALSE;

//NOTE!! the () used by nextHist below is an overloaded operator 
//     in TIter that returns a TObject* 

  TObject *obj = 0;
  Int_t chkdim=0;
  while (obj = nextHist()) {
//    cout << " **** Now in St_QA_Maker::DrawHists - in loop: " << endl;
//    cout << "               name = " << obj->GetName() << endl;

    if (obj->InheritsFrom("TH1")) { 
//    cout << " **** Now in St_QA_Maker::DrawHists - obj->InheritsFrom(TH1)  **** " << endl;
      histReadCounter++;
      printf(" %d. Reading ... %s::%s; Title=\"%s\"\n",histReadCounter,obj->ClassName(),obj->GetName(), obj->GetTitle());
      if (! started && (strcmp("*",firstHistName)==0 || strcmp(obj->GetName(),firstHistName)==0 ))  started = kTRUE;
      if (started) {
	if (strcmp(obj->GetName(),lastHistName)==0) started = kFALSE;
	histCounter++;
	printf("  -   %d. Drawing ... %s::%s; Title=\"%s\"\n",histCounter,obj->ClassName(),obj->GetName(), obj->GetTitle());
	if (padCount == numPads) {
	  if (psf) psf->NewPage();
	  padCount=0;
	}
	QACanvas->cd(++padCount);
          gPad->SetLogy(0);
	if (m_ListOfLog->FindObject(obj->GetName())){
	  gPad->SetLogy(1);
          cout << "St_QA_Maker::DrawHists -- Will draw in log scale: " << obj->GetName() <<endl;
        }
        chkdim = ((TH1 *)obj)->GetDimension();
	//	  cout << " name " << obj->GetName() << " dimension " << chkdim << endl;
        if (chkdim == 2) obj->Draw("box");
        else 
            obj->Draw();   
	if (gPad) gPad->Update();
      }
    }
  }

  if (psf) {
    psf->Close();
    delete psf;
  }
  return histCounter;
}

//_____________________________________________________________________________


TList* St_QA_Maker::FindHists(Char_t *histBranchName) 
{  
// Method FindHists -->
// Find pointer to histograms

  TList *dList=0;

  //cout << " Beg: FindHists, dList pointer = " << dList << endl;

//---- First look in your current Maker for histograms ==>
//---- If you've made the histograms on your own, they
//     should show up in your Maker's directory, so search for them there,
//     i.e. MakerName/.hist is where they'd be
// Note: Histograms is a method of StMaker
//---- If you have a chain, you'll always have the .hist directory, so
//     have to check if there's really anything there (so use First method)

  dList = Histograms();

  TObject *test=0;
  if (dList) test = dList->First();
  if (test){ 
      cout << " FindHists - found hist. in Maker-Branch " << endl;
     }

  //  cout << " Mid: FindHists, dList pointer = " << dList << endl;
  //cout << " Mid: FindHists, test pointer =  " << test << endl;

// If you have the pointer but the hist. really aren't here, set
//  the pointer back to zero
  if (!test) dList = 0;

  //cout << " Mid2: FindHists, dList pointer = " << dList << endl;
  //cout << " Mid2: FindHists, test pointer =  " << test << endl;


  if (!dList) {

//-------------- Now try and see if they're in histBranch from output of bfc

  St_DataSet *hist=0;
  hist = GetDataSet("hist");
  hist->ls(9);

  //find particular branch
  St_DataSet *QAH = hist->Find(histBranchName);

  // or can create iterator and look over all branches

  //now get the list of histograms
   dList = (TList *)QAH->GetObject();

  // now have we found them?
  if (dList){ 
      cout << " FindHists - found hist. in histBranch, with name:  " 
	   << histBranchName <<  endl;
     }

  }

  //cout << " End: FindHists, dList pointer = " << dList << endl;
  
 return dList;
}
//_____________________________________________________________________________


Int_t St_QA_Maker::ListHists() 
{  
// Method ListHists -->
// List of all histograms

  cout << " **** Now in St_QA_Maker::ListHists **** " << endl;

// get the TList pointer to the histograms:
  TList  *dirList = 0;
  Char_t *dirHistName = "QAHist";
  dirList = FindHists(dirHistName);


//Now want to loop over all histograms
// Create an iterator
  TIter nextObj(dirList);
  Int_t histReadCount = 0;
  TObject *obj = 0;

// use = here instead of ==, because we are setting obj equal to nextObj and then seeing if it's T or F
  while (obj = nextObj()) {

// now check if obj is a histogram
    if (obj->InheritsFrom("TH1")) {
 
      histReadCount++;
//  \n means newline, \" means print a quote
//      printf(" %d. Have histogram Type %s, Name %s with Title=\"%s\"\n",histReadCount,obj->ClassName(),obj->GetName(),obj->GetTitle());
      cout << " ListHists: Hist No. " << histReadCount << ", Type: " << obj->ClassName() 
           << ", Name: " << obj->GetName() << ", Title \"" << obj->GetTitle() << "\"  "<< endl; 
    }
  }

  cout << " ListHists: Total No. Histograms  = " << histReadCount <<endl;
  return histReadCount;
}

//_____________________________________________________________________________


Int_t St_QA_Maker::ExamineLogYList() 
{  
// Method ExamineLogYList
// List of all histograms that will be drawn with logy scale

  cout << " **** Now in St_QA_Maker::ExamineLogYList **** " << endl;

// m_ListOfLog -  is a list of log plots
// construct a TObject
  TObject *obj = 0;
// construct a TIter ==>  () is an overloaded operator in TIter
  TIter nextObj(m_ListOfLog);
  Int_t LogYCount = 0;

// use = here instead of ==, because we are setting obj equal to nextObj and then seeing if it's T or F
  while (obj = nextObj()) {

    cout << " St_QA_Maker::ExamineLogYList has hist " <<  obj->GetName() << endl;
    LogYCount++;

  }

  cout << " Now in St_QA_Maker::ExamineLogYList, No. Hist. in LogY scale = " << LogYCount <<endl;
  return LogYCount;
}

//_____________________________________________________________________________


Int_t St_QA_Maker::AddToLogYList(const Char_t *HistName){  
// Method AddToLogYList
//   making list of all histograms that we want drawn with LogY scale

//  cout << " **** Now in St_QA_Maker::AddToLogYList  **** " << endl;

// Since I'm creating a new list, must delete it in the destructor!!
//make a new TList on heap(persistant); have already defined m_ListOfLog in header file
   if (!m_ListOfLog) m_ListOfLog = new TList;

// the add method for TList requires a TObject input  (also can use TObjString)
// create TObjString on heap
   TObjString *HistNameObj = new TObjString(HistName);

// - check if it's already on the list - use FindObject method of TList
    TObject *lobj = 0;
    lobj = m_ListOfLog->FindObject(HistName);
// now can use Add method of TList
    if (!lobj) {
       m_ListOfLog->Add(HistNameObj);
       cout << " St_QA_Maker::AddToLogYList: " << HistName  <<endl;
    }
    else  cout << " St_QA_Maker::AddToLogYList: " << HistName << " already in list - not added" <<endl;
 
// return using a method of TList (inherits GetSize from TCollection)
 return m_ListOfLog->GetSize();
}


//_____________________________________________________________________________


Int_t St_QA_Maker::RemoveFromLogYList(const Char_t *HistName){  
// Method RemoveFromLogYList
//   remove hist from  list  that we want drawn with LogY scale

//  cout << " **** Now in St_QA_Maker::RemoveFromLogYList  **** " << endl;

// check if list exists:
  if (m_ListOfLog) {
    
// the remove method for TList requires a TObject input  
// - check if it's  on the list - use FindObject method of TList
    TObject *lobj = 0;
    lobj = m_ListOfLog->FindObject(HistName);
// now can use Remove method of TList
    if (lobj) {
      m_ListOfLog->Remove(lobj);
      cout << " RemoveLogYList: " << HistName << " has been removed from list" <<endl;
    }
    else  cout << " RemoveLogYList: " << HistName << " not on list - not removing" <<endl;

  } 
// return using a method of TList (inherits GetSize from TCollection)
 return m_ListOfLog->GetSize();
}


//_____________________________________________________________________________
// Method SetDefaultLogYList
//    - create default list of histograms we want plotted in LogY scale

void St_QA_Maker::SetDefaultLogYList(){  
// Method SetDefaultLogYList
//    - create default list of histograms we want plotted in LogY scale

  cout << " **** Now in St_QA_Maker::SetDefaultLogYList  **** " << endl;

  const Char_t *sdefList[] = {
 "QaGlobtrkDetId",
 "QaGlobtrkFlag",
 "QaGlobtrkNPnt",
 "QaGlobtrkNPntMax",
 "QaGlobtrkNPntFit",
 "QaGlobtrkPt",
 "QaGlobtrkP",
 "QaGlobtrkX0",
 "QaGlobtrkXf",
 "QaGlobtrkY0",
 "QaGlobtrkYf",
 "QaGlobtrkZ0",
 "QaGlobtrkZf",
 "QaGlobtrkTanl ",
 "QaGlobtrkTheta ",
 "QaGlobtrkEta",
 "QaGlobtrkChisq0P",
 "QaGlobtrkChisq1P",
 "QaGlobtrkLength",
 "QaGlobtrkImpact",
 "QaGlobtrkNdof",
 "QaDstDedxNdedx",
 "QaDstDedxDedx0", 
 "QaDstDedxDedx1",
 "QaPrimtrkPt",
 "QaParticlePt"};

  Int_t lengofList = 0;
  lengofList = sizeof(sdefList)/4;
  Int_t ilg = 0;
  Int_t numLog = 0;
  for (ilg=0;ilg<lengofList;ilg++) {
     numLog = AddToLogYList(sdefList[ilg]);
     cout <<  " !!! adding histogram " << sdefList[ilg] << " to LogY list "  << endl ;
  }

}

//_____________________________________________________________________________

Int_t St_QA_Maker::Finish() {
// St_QA_Maker - Finish, Draw histograms if SetDraw true
  if (drawinit)  DrawHists();

  return StMaker::Finish();
}
//_____________________________________________________________________________

Int_t St_QA_Maker::Init(){
// St_QA_Maker - Init; book histograms and set defaults for member functions
    
//book histograms --------------
  BookHistEvSum();
  BookHistGlob();
  BookHistDE();
  BookHistPrim();
  BookHistGen();
  BookHistV0();
  BookHistPID();
  BookHistVertex();
  BookHistXi();

//  Set default values for all methods:
  SetDraw(kFALSE);
  SetHistsNamesDraw();
  SetZones();
  SetPaperSize();
  SetPostScriptFile(); 
  SetDefaultLogYList();

  return StMaker::Init();
}
//_____________________________________________________________________________

Int_t St_QA_Maker::Make(){
// St_QA_Maker - Make; fill histograms
  
  // Call methods to fill histograms
  
  St_DataSet *dst = GetDataSet("dst");  
  
  // histograms from table event_summary
  MakeHistEvSum(dst);
  // histograms from table globtrk
  MakeHistGlob(dst);
  // histograms from table dst_dedx
  MakeHistDE(dst);
  // histograms from table primtrk
  MakeHistPrim(dst);
  // histograms from table particle
  MakeHistGen(dst);  
  // histograms from table dst_v0_vertex
  MakeHistV0(dst);
  // histograms from table primtrk & dst_dedx
  MakeHistPID(dst);
  // histograms from table dst_vertex
  MakeHistVertex(dst);
  MakeHistXi(dst);
  
  return kStOK;
}
//_____________________________________________________________________________

void St_QA_Maker::BookHistEvSum(){
  
 // for method MakeEvSum - from table event_summary
  m_trk_tot_gd    = new TH1F("QaEvsumTrkGoodDTotal","evsum: num good track over total",
                             50,0.9,1.1);
    m_trk_tot_gd->SetXTitle("number of good/total tracks");
  m_glb_trk_tot   = new TH1F("QaEvsumTrkTot","evsum: num tracks total ",
                             ntrk, mintrk, maxtrk);
  m_glb_trk_plusminus  = new TH1F("QaEvsumPlusMinusTrk", "evsum: num pos. over neg trks",
                             ntrk,0.8,1.4);
  m_glb_trk_prim    = new TH1F("QaEvsumTrkPrim","evsum: num good tracks from primaries ",
                             ntrk, mintrk, maxtrk);
	  
  m_vert_total = new TH1F("QaEvsumVertTot", "evsum: total num of vertices",100,0.,5000.);
  m_vert_V0    = new TH1F("QaEvsumVertV0", "evsum: num V0 vertices",100,0.,5000.); 
 
  m_mean_pt    = new TH1F("QaEvsumMeanPt",   "evsum: mean pt", nmnpt, 0., 2.0);
  m_mean_eta   = new TH1F("QaEvsumMeanEta",  "evsum: mean eta", nmneta, -0.25,0.25);
  m_rms_eta    = new TH1F("QaEvsumRmsEta",   "evsum: rms eta", nmneta, -2.5,2.5);
  m_T_average  = new TH1F("QaEvsumMeanTemp", "evsum: mean Temp", nmneta, 0., 0.5);
  m_prim_vrtx0 = new TH1F("QaEvsumPrimVertX","evsum: X of primary vertex", 40, -1.,1.);
  m_prim_vrtx1 = new TH1F("QaEvsumPrimVertY","evsum: Y of primary vertex", 40,-1.,1.);
  m_prim_vrtx2 = new TH1F("QaEvsumPrimVertZ","evsum: Z of primary vertex", nxyz,-50., 50.);
  m_vrtx_chisq = new TH1F("QaEvsumVrtxChisq","evsum: chisq of primary vertex",nchisq, 0., 10.); 
  
}

//_____________________________________________________________________________
void St_QA_Maker::BookHistGlob(){
  
// for method MakeGlob - from table globtrk

// 1D
  m_globtrk_tot   = new TH1F("QaGlobtrkTot",  "globtrk: tot # tracks",40,4000.,8000.);
  m_globtrk_iflag = new TH1F("QaGlobtrkFlag", "globtrk: iflag ",10,-1000.,1000.);

  m_globtrk_good  = new TH1F("QaGlobtrkGood", "globtrk: tot # good tracks",40,4000.,8000.);  
  m_det_id     = new TH1F("QaGlobtrkDetId",   "globtrk: Detector ID for tracks",11,-0.5,10.5);
  m_point      = new TH1F("QaGlobtrkNPnt",    "globtrk: N points on track", 50, 0.,50.);
  m_max_point  = new TH1F("QaGlobtrkNPntMax", "globtrk: N max points on track", 50, 0.,50.);
  m_fit_point  = new TH1F("QaGlobtrkNPntFit", "globtrk: N fit points on track", 50, 0.,50.);
  m_glb_charge = new TH1F("QaGlobtrkChrg",    "globtrk: charge ", 20,-2.,2.);
  m_glb_x0     = new TH1F("QaGlobtrkX0",      "globtrk: x coor. on helix at start ", 50,-200.,200.);
  m_glb_xf     = new TH1F("QaGlobtrkXf",      "globtrk: x of first tpc hit - on helix at start",50,-20.,20.);
  m_glb_y0     = new TH1F("QaGlobtrkY0",      "globtrk: y coor. on helix at start ", 50,-200.,200.);
  m_glb_yf     = new TH1F("QaGlobtrkYf",      "globtrk: y of first tpc hit - on helix at start",50,-20.,20.);
  m_glb_z0     = new TH1F("QaGlobtrkZ0",      "globtrk: z coor. on helix at start ", 50,-250.,250.);
  m_glb_zf     = new TH1F("QaGlobtrkZf",      "globtrk: z of first tpc hit - on helix at start",50,-20.,20.);
  m_psi        = new TH1F("QaGlobtrkPsi",     "globtrk: psi distribution", 36, 0.,360.);
  m_tanl       = new TH1F("QaGlobtrkTanl",    "globtrk: tanl distribution",32,-4.,4.);
  m_glb_theta  = new TH1F("QaGlobtrkTheta",   "globtrk: theta distribution",20,0.,4.);
  m_eta        = new TH1F("QaGlobtrkEta",     "globtrk: eta distribution",60,-3.0,3.0);
  m_pT         = new TH1F("QaGlobtrkPt",      "globtrk: pT distribution",50,0.,5.);
  m_mom        = new TH1F("QaGlobtrkP",       "globtrk: momentum distribution",50,0.,5.);
  m_chisq0     = new TH1F("QaGlobtrkChisq0C", "globtrk: chisq[0] per dof", 50, 0.,15.);
  m_chisq1     = new TH1F("QaGlobtrkChisq1C", "globtrk: chisq[1] per dof", 50, 0.,15.);
  m_length     = new TH1F("QaGlobtrkLength",  "globtrk: track length", 50,0.,300.);
  m_glb_impact = new TH1F("QaGlobtrkImpact",  "globtrk: impact param from prim vtx ", 50,0.,500.);
  m_glb_ndf    = new TH1F("QaGlobtrkNdof",    "globtrk: num deg of freedom", 100,-10.,90.);


// 2D
  m_pT_eta_rec = new TH2F("QaGlobtrkPtVsEta","globtrk: log pT versus eta", 20,-2.,2.,40,0.,10.);
    m_pT_eta_rec->SetXTitle("eta");
    m_pT_eta_rec->SetYTitle(" log pT (MeV)");


  m_tanl_z0 = new TH2F("QaGlobtrkTanlz0","globtrk: tanl(dip) versus z0",50,-250.,250.,60,-3.,3.);
    m_tanl_z0->SetXTitle("z0");
    m_tanl_z0->SetYTitle("tanl");


  m_mom_trklength = new TH2F("QaGlobtrkPVsTrkLength","globtrk: log mom vs trk length",
			     50,0.,250.,40,0.,10.);
    m_mom_trklength->SetXTitle("trk length");  
    m_mom_trklength->SetYTitle("log P (MeV)");

  m_eta_trklength = new TH2F("QaGlobtrkLengthVEta","globtrk: trk length vs eta",
			     20,-2.,2.,50,0.,250.);
    m_eta_trklength->SetXTitle("eta");
    m_eta_trklength->SetYTitle("length");


  m_npoint_length = new TH2F("QaGlobtrkNPntLength","globtrk: N points on trk vs trk length",
			     25,0.,250.,25,0.,50.);
    m_npoint_length->SetXTitle("length");
    m_npoint_length->SetYTitle("Npoints");

  m_fpoint_length = new TH2F("QaGlobtrkFitPntLength","globtrk: N fit points on trk vs trk length",
			     25,0.,250.,25,0.,50.);
    m_fpoint_length->SetXTitle("length");
    m_fpoint_length->SetYTitle("Npoints");

  m_chisq0_mom = new TH2F("QaGlobtrkChi0Mom","globtrk: Chisq0 vs log mom",40,0.,10.,50,0.,15.);
    m_chisq0_mom->SetXTitle("log P (MeV)");
    m_chisq0_mom->SetYTitle("chisq0") ;

  m_chisq1_mom = new TH2F("QaGlobtrkChi1Mom","globtrk: Chisq1 vs log mom",40,0.,10.,50,0.,15.);
    m_chisq1_mom->SetXTitle("log P (MeV)");
    m_chisq1_mom->SetYTitle("chisq1");


  m_chisq0_eta = new TH2F("QaGlobtrkChi0Eta","globtrk: Chisq0 vs eta",20,-2.,2.,20,0.,15.);
    m_chisq0_eta->SetXTitle("eta");
    m_chisq0_eta->SetYTitle("chisq0");

  m_chisq1_eta = new TH2F("QaGlobtrkChi1Eta","globtrk: Chisq1 vs eta",20,-2.,2.,20,0.,15.);
    m_chisq1_eta->SetXTitle("eta");
    m_chisq1_eta->SetYTitle("chisq1");


  m_chisq0_dip = new TH2F("QaGlobtrkChi0Tanl","globtrk: Chisq0 vs tanl(dip)",20,-5.,5.,20,0.,15.);
    m_chisq0_dip->SetXTitle("dip angle");
    m_chisq0_dip->SetYTitle("chisq0");

  m_chisq1_dip = new TH2F("QaGlobtrkChi1Tanl","globtrk: Chisq1 vs tanl(dip)",20,-5.,5.,20,0.,15.);
    m_chisq1_dip->SetXTitle("dip angle");
    m_chisq1_dip->SetYTitle("chisq1");

  m_chisq0_z0 = new TH2F("QaGlobtrkChi0z0","globtrk: Chisq0 vs z0",20,-250.,250.,20,0.,15.);
    m_chisq0_z0->SetXTitle("z0");
    m_chisq0_z0->SetYTitle("chisq0");

  m_chisq1_z0 = new TH2F("QaGlobtrkChi1z0","globtrk: Chisq1 vs z0",20,-250.,250.,20,0.,15.);
    m_chisq1_z0->SetXTitle("z0");
    m_chisq1_z0->SetYTitle("chisq1");

  m_nfptonpt_mom = new TH2F("QaGlobtrkRPntMom","globtrk: ratio Nfitpnt,Npnt vs log mom.",40,0.,10.,40,0.,2.0); 
     m_nfptonpt_mom->SetXTitle("log P (MeV)");
     m_nfptonpt_mom->SetYTitle("Ratio Nfitpnt/Npnt");

  m_nfptonpt_eta = new TH2F("QaGlobtrkRPntEta","globtrk: ratio Nfitpnt,Npnt vs Eta",40,-2.,2.,40,0.,2.0); 
     m_nfptonpt_eta->SetXTitle("eta");
     m_nfptonpt_eta->SetYTitle("Ratio Nfitpnt/Npnt");


}

//_____________________________________________________________________________
void St_QA_Maker::BookHistDE(){
  
  // for method MakeDE - from table dst_dedx
  m_ndedx   = new TH1F("QaDstDedxNdedx", "dedx: number of point to define dE/dx", 50,0., 50.);  
  m_dedx0   = new TH1F("QaDstDedxDedx0","dedx: dE/dx[0]", ndedx, mindedx, maxdedx/10.);
  m_dedx1   = new TH1F("QaDstDedxDedx1","dedx: dE/dx[1]", ndedx, mindedx, maxdedx);
  
}

//_____________________________________________________________________________
void St_QA_Maker::BookHistPrim(){
  // for MakeHistPrim - from table primtrk
  m_prim_pT         = new TH1F("QaPrimtrkPt","primtrk: pT distribution",nxpT,xminpT,xmaxpT);
  m_prim_eta        = new TH1F("QaPrimtrkEta","primtrk: eta distribution",knyeta,kmineta,kmaxeta);
  m_prim_pT_eta_rec = new TH2F("QaPrimtrkPtVsEta","primtrk: pT versus eta (reconstructed)",
			       20,kmineta,kmaxeta,20,xminpT,xmaxpT);
  m_prim_pT_eta_rec->SetXTitle("eta");
  m_prim_pT_eta_rec->SetYTitle("pT (GeV)");
  m_prim_mom_trklength = new TH2F("QaPrimtrkPVsTrkLength","primtrk: mom vs trk length",
				  50,0.,250.,20,0.,10.);
  m_prim_mom_trklength->SetXTitle("length");
  m_prim_mom_trklength->SetYTitle("mom");
  m_prim_point     = new TH1F("QaPrimtrkNPoint","primtrk: N points on track", npnt, minpnt, maxpnt);
  m_prim_fit_point = new TH1F("QaPrimtrkNPointFit","primtrk: N fit points on track", npnt,minpnt, maxpnt);
  m_prim_tlength    = new TH1F("QaPrimtrkLength","primtrk: track length",100,0.,400.);
  m_prim_npoint_length = new TH2F("QaPrimtrkNPntLength","primtrk: N points vs trk length",
				  25,minleng,250.,25,minpnt,maxpnt);
  m_prim_fpoint_length = new TH2F("QaPrimtrkFitPntLength","primtrk: N fit points vs trk length",
				  25,minleng,250.,25,minpnt,maxpnt);
  
  m_prim_chi2xd     = new TH1F("QaPrimtrkChiXY","primtrk: chisqxy/degf",100,0.,10.);
  m_prim_chi2yd     = new TH1F("QaPrimtrkChiSZ","primtrk: chisqsz/degf",100,0.,10.);
  
  m_prim_psi       = new TH1F("QaPrimtrkPsi","primtrk: psi distribution", 60, 0.,360.);
  m_prim_det_id    = new TH1F("QaPrimtrkDetId","primtrk: Detector ID for tracks",knid,kminnid,kmaxnid);
  
  m_prim_chisq0_mom = new TH2F("QaPrimtrkChi0Mom","primtrk: Chisq0 vs mom",20,0.,10.,20,0.,10.);
  m_prim_chisq0_mom->SetXTitle("mom");
  m_prim_chisq0_mom->SetYTitle("chisq0");
  m_prim_chisq1_mom = new TH2F("QaPrimtrkChi1Mom","primtrk: Chisq1 vs mom",20,0.,10.,20,0.,10.);
  m_prim_chisq1_mom->SetXTitle("mom");
  m_prim_chisq1_mom->SetYTitle("chisq1");
}


//_____________________________________________________________________________
void St_QA_Maker::BookHistGen(){
  // for MakeHistGen - from table particle
  m_H_npart   = new TH1F("QaParticleNumPart","total num particles (generated)",100,0.,30000.);
  m_H_ncpart  = new TH1F("QaParticleNumChgPart","num chg (e,mu,pi,K,p) part (generated)",100,0.,20000.);
  m_H_pT_gen  = new TH1F("QaParticlePt","charged: pt (generated)",nxpT,xminpT,xmaxpT);
  m_H_eta_gen = new TH1F("QaParticleEta","charged:eta (generated)",nyeta,-4.,4.);
  m_H_pT_eta_gen = new TH2F("QaParticlePtVsEta","charged:pT versus eta (generated)",
			    nyeta,kmineta,kmaxeta,nxpT,xminpT,xmaxpT);
  m_H_pT_eta_gen->SetXTitle("eta");
  m_H_pT_eta_gen->SetYTitle("pT (GeV)");
  m_H_vtxx    = new TH1F("QaParticleVtxX","Generator prod vertex x (mm)",50,-100.,100.);
  m_H_vtxy    = new TH1F("QaParticleVtxY","Generator prod vertex y (mm)",50,-100.,100.);
  m_H_vtxz    = new TH1F("QaParticleVtxZ","Generator prod vertex z (mm)",50,-500.,500.);
}

//_____________________________________________________________________________
void St_QA_Maker::BookHistV0(){
  
  // for MakeHistV0 - from table dst_v0_vertex
  m_ev0_lama_hist  = new TH1F("QaDstV0VertexLambdaMass","dst_v0_vertex: Lambda mass",50,1.05,1.15);
  m_ev0_k0ma_hist  = new TH1F("QaDstV0VertexK0Mass","dst_v0_vertex: k0 mass",50,.4,.6);
  
}

//_____________________________________________________________________________
void St_QA_Maker::BookHistPID(){
  
  // for MakeHistPID - from tables primtrk & dst_dedx 
  // Spectra/pid histograms. C.Ogilvie
  
  m_p_dedx_rec = new TH2F("QaPidPrimtrkDstdedxPVsDedx","primtrk-dst_dedx: p vs dedx (reconstructed)",
			  cnp,cminp,cmaxp,cndedx,cmindedx,cmaxdedx);
  m_p_dedx_rec->SetYTitle("dedx");
  m_p_dedx_rec->SetXTitle("p (GeV)");
  
}

//_____________________________________________________________________________
void St_QA_Maker::BookHistVertex(){
  // for MakeHistVertex - from table dst_vertex
  
  
  m_v_detid = new TH1F("QaVertexDetId"," vertex: Detector ID ",100,0.,100.);
  m_v_vtxid = new TH1F("QaVertexVtxId"," vertex: Vertex ID ",10,0.,10.);
  m_v_x     = new TH1F("QaVertexX"," vertex: x ",50,-25.,25.);
  m_v_y     = new TH1F("QaVertexY"," vertex: y ",50,-25.,25.);
  m_v_z     = new TH1F("QaVertexZ"," vertex: z ",50,-50.,50.);
  m_v_pchi2 = new TH1F("QaVertexChisq"," vertex: chisq/dof ",50,0.,5.);
  
  m_pv_detid = new TH1F("QaVertexPrDetId"," vertex,prim: Detector ID ",40,0.,40.);
  m_pv_vtxid = new TH1F("QaVertexPrVtxId"," vertex,prim: Vertex ID ",10,0.,10.);
  m_pv_x     = new TH1F("QaVertexPrX"," vertex,prim: x ",50,-1.,1.);
  m_pv_y     = new TH1F("QaVertexPrY"," vertex,prim: y ",50,-1.,1.);
  m_pv_z     = new TH1F("QaVertexPrZ"," vertex,prim: z ",50,-50.,50.);
  m_pv_pchi2 = new TH1F("QaVertexPrChisq"," vertex,prim: chisq/dof ",50,0.,5.);
  
}
//_____________________________________________________________________________
void St_QA_Maker::BookHistXi(){
  
}


//_____________________________________________________________________________
void St_QA_Maker::MakeHistEvSum(St_DataSet *dst){
  //  PrintInfo();
  // Fill histograms for event summary
  St_DataSetIter dstI(dst);         
  
  St_dst_event_summary *event_summary = (St_dst_event_summary *) dstI["event_summary"];
  if (event_summary) {
    dst_event_summary_st  *tt = event_summary->GetTable();

    for (Int_t j = 0; j < event_summary->GetNRows(); j++,tt++) {
      Float_t trk_tot =   tt->glb_trk_tot;
      Float_t trk_good =  tt->glb_trk_good;
      Float_t trk_plus =  tt->glb_trk_plus;
      Float_t trk_minus = tt->glb_trk_minus;

      m_trk_tot_gd->Fill(trk_good/trk_tot); 
      m_glb_trk_tot->Fill(tt->glb_trk_tot);
      m_glb_trk_plusminus->Fill(trk_plus/trk_minus);
      m_glb_trk_prim->Fill(tt->glb_trk_prim);
      
      m_vert_total->Fill(tt->n_vert_total);
      m_vert_V0->Fill(tt->n_vert_V0);


      m_mean_pt->Fill(tt->mean_pt);
      m_mean_eta->Fill(tt->mean_eta);
      m_rms_eta->Fill(tt->rms_eta);
      m_T_average->Fill(tt->T_average);

      if(!isnan((double)(tt->prim_vrtx[0])))  m_prim_vrtx0->Fill(tt->prim_vrtx[0]);
      if(!isnan((double)(tt->prim_vrtx[1])))  m_prim_vrtx1->Fill(tt->prim_vrtx[1]);
      if(!isnan((double)(tt->prim_vrtx[2])))  m_prim_vrtx2->Fill(tt->prim_vrtx[2]);

      m_vrtx_chisq->Fill(tt->prim_vrtx_chisq); 

    }
  }
} 

//-----------------------------------------------------------------

void St_QA_Maker::MakeHistGlob(St_DataSet *dst){

  St_DataSetIter dstI(dst);           

  St_dst_track *globtrk = (St_dst_track *) dstI["globtrk"];
  if (globtrk) {
    dst_track_st  *t   = globtrk->GetTable();

    Int_t cnttrk=0;
    Int_t cnttrkg=0;
    cnttrk = globtrk->GetNRows();
    m_globtrk_tot->Fill(cnttrk);

    for (Int_t i = 0; i < globtrk->GetNRows(); i++,t++){

      m_globtrk_iflag->Fill(t->iflag);

      if (t->iflag>0) {
        cnttrkg++;
	Float_t pT = -999.;
	pT = 1./TMath::Abs(t->invpt);
        Float_t lmevpt = log(pT*1000.0);
	Float_t theta = asin(1.) - atan(t->tanl);
	Float_t eta   =-log(tan(theta/2.));
	Float_t gmom  = pT/sin(theta);
        Float_t lmevmom = log(gmom*1000.0); 
	Float_t chisq0 = t->chisq[0];
	Float_t chisq1 = t->chisq[1]; 
	Float_t degoffree = t->n_fit_point;
	Float_t chisq0_p = chisq0/(degoffree-3);
	Float_t chisq1_p = chisq1/(degoffree-2);
        Float_t nfitntot = (float(t->n_fit_point))/(float(t->n_point));
        Float_t xdif = (t->x_first[0]) - (t->x0);
        Float_t ydif = (t->x_first[1]) - (t->y0);
        Float_t zdif = (t->x_first[2]) - (t->z0);


 	m_det_id->Fill(t->det_id);
	m_point->Fill(t->n_point);
	m_max_point->Fill(t->n_max_point);
	m_fit_point->Fill(t->n_fit_point);
        m_glb_charge->Fill(t->icharge);
        m_glb_x0->Fill(t->x0);
        m_glb_y0->Fill(t->y0);
        m_glb_z0->Fill(t->z0);
        m_glb_xf->Fill(xdif);
        m_glb_yf->Fill(ydif);
        m_glb_zf->Fill(zdif);
	m_psi->Fill(t->psi);
        m_tanl->Fill(t->tanl);
        m_glb_theta->Fill(theta);
	m_eta->Fill(eta);
	m_pT->Fill(pT);
        m_mom->Fill(gmom);
	m_length->Fill(t->length);
        m_glb_impact->Fill(t->impact);
        m_glb_ndf->Fill(t->ndegf);
	m_chisq0->Fill(chisq0_p);
	m_chisq1->Fill(chisq1_p);

	m_pT_eta_rec->Fill(eta,lmevpt);
        m_tanl_z0->Fill(t->z0,t->tanl);
	m_mom_trklength->Fill(t->length,lmevmom);
        m_eta_trklength->Fill(eta,t->length);
	m_npoint_length->Fill(t->length,float(t->n_point));
	m_fpoint_length->Fill(t->length,float(t->n_fit_point));
	m_chisq0_mom->Fill(lmevmom,chisq0_p);
	m_chisq1_mom->Fill(lmevmom,chisq1_p);
	m_chisq0_eta->Fill(eta,chisq0_p);
	m_chisq1_eta->Fill(eta,chisq1_p);
	m_chisq0_dip->Fill(t->tanl,chisq0_p);
	m_chisq1_dip->Fill(t->tanl,chisq1_p);
	m_chisq0_z0->Fill(t->z0,chisq0_p);
	m_chisq1_z0->Fill(t->z0,chisq1_p);
        m_nfptonpt_mom->Fill(lmevmom,nfitntot);
        m_nfptonpt_eta->Fill(eta,nfitntot);

      }
    }
    m_globtrk_good->Fill(cnttrkg);
  }       
}

//_____________________________________________________________________________

void St_QA_Maker::MakeHistDE(St_DataSet *dst) {
  // Fill histograms for dE/dx
  
  St_DataSetIter dstI(dst);
  
  
  St_dst_dedx *dst_dedx = (St_dst_dedx *) dstI["dst_dedx"];
  if(dst_dedx) {
    dst_dedx_st *d = dst_dedx->GetTable();
    for (Int_t i = 0; i < dst_dedx->GetNRows(); i++,d++) {
      m_ndedx->Fill(d->ndedx);
      m_dedx0->Fill(d->dedx[0]*1e6);
      m_dedx1->Fill(d->dedx[1]*1e6);
    }
  }
}

//_____________________________________________________________________________


void St_QA_Maker::MakeHistPrim(St_DataSet *dst){
  if (Debug()) cout << " *** in St_QA_Maker - filling primtrk histograms " << endl;
  St_DataSetIter dstI(dst);
  St_dst_track      *primtrk     = (St_dst_track *) dstI["primtrk"];
  
  if (primtrk) {
    dst_track_st  *t   = primtrk->GetTable();
    for (Int_t i = 0; i < primtrk->GetNRows(); i++,t++){
      if (t->iflag>0) {
	Float_t pT = 9999.;
	if (t->invpt) pT = 1./TMath::Abs(t->invpt);
	Float_t theta = TMath::Pi()/2.0 - TMath::ATan(t->tanl);
	Float_t eta   =-TMath::Log(TMath::Tan(theta/2.));
	Float_t primmom  = pT/sin(theta); 
	m_prim_pT->Fill(pT);
	m_prim_eta->Fill(eta);
	m_prim_pT_eta_rec->Fill(eta,pT);
	m_prim_point->Fill(t->n_point);
	m_prim_fit_point->Fill(t->n_fit_point);
	m_prim_psi->Fill(t->psi);
	m_prim_det_id->Fill(t->det_id);
	m_prim_tlength->Fill(t->length);
	// Al histograms
	if (t->ndegf>0) {
	  m_prim_chi2xd->Fill(t->chisq[0]/((t->ndegf+5.)/2.-3.));  
	  m_prim_chi2yd->Fill(t->chisq[1]/((t->ndegf+5.)/2.-2.));  
	}
	m_prim_mom_trklength->Fill(t->length,primmom);
	m_prim_npoint_length->Fill(t->length,float(t->n_point));
	m_prim_fpoint_length->Fill(t->length,float(t->n_fit_point));
	m_prim_chisq0_mom->Fill(primmom,float(t->chisq[0]/((t->ndegf+5.)/2.-3.)));
	m_prim_chisq1_mom->Fill(primmom,float(t->chisq[0]/((t->ndegf+5.)/2.-3.)));
      }
    }
  }
}

//_____________________________________________________________________________


void St_QA_Maker::MakeHistGen(St_DataSet *dst){
  if (Debug()) cout << " *** in St_QA_Maker - filling particle histograms " << endl;
  St_DataSetIter dstI(dst);
  
  St_particle   *part     = (St_particle  *) dstI["particle"];
  if (!part) part = (St_particle  *) DataSet("geant/particle");
  if (part){
    particle_st *p = part->GetTable();
    Int_t nchgpart=0;
    Int_t totpart=0;
    for (Int_t l=0; l < part->GetNRows(); l++, p++){
      //
      //  select only particles which can be detected
      //  in the STAR detector. Here we restrict us to/
      //  the most common species.
      //
      if(l!=0){                        // first row of table is header, so skip it!
	if (p->isthep == 1) {            // select good status only
	  totpart++;
	  if (abs(p->idhep) == 11   ||       // electrons
	      abs(p->idhep) == 13   ||       // muon
	      abs(p->idhep) == 211  ||       // pion
	      abs(p->idhep) == 321  ||       // kaon
	      abs(p->idhep) == 2212) {       // proton/
	    
	    nchgpart++;	    
	    Double_t px = p->phep[0];
	    Double_t py = p->phep[1];
	    Double_t pz = p->phep[2];
	    Double_t pT    =  TMath::Sqrt(px*px+py*py);
	    Double_t theta =  TMath::ATan2 ( pT, pz );
	    //        Double_t theta =  atan2 ( pT, pz );
	    Float_t  eta  = -TMath::Log(TMath::Tan(theta/2.));
	    m_H_pT_eta_gen->Fill(eta, (Float_t) pT);
	    m_H_pT_gen->Fill((Float_t) pT);
	    m_H_eta_gen->Fill(eta);
	    m_H_vtxx->Fill(p->vhep[0]);
	    m_H_vtxy->Fill(p->vhep[1]);
	    m_H_vtxz->Fill(p->vhep[2]);
	  }
	}
      }
    }
    m_H_npart->Fill(totpart);
    m_H_ncpart->Fill(nchgpart);
  }
}

//_____________________________________________________________________________


void St_QA_Maker::MakeHistV0(St_DataSet *dst){
  if (Debug()) cout << " *** in St_QA_Maker - filling dst_v0_vertex histograms " << endl;
  St_DataSetIter dstI(dst);         
  
  St_dst_v0_vertex  *dst_v0_vertex = (St_dst_v0_vertex *) dstI["dst_v0_vertex"];
  if (dst_v0_vertex) {
    dst_v0_vertex_st *v0 = dst_v0_vertex->GetTable();
    Float_t m_prmass2 = proton_mass_c2*proton_mass_c2;
    Float_t m_pimass2 = (0.139567*0.139567);
    for (Int_t k=0; k<dst_v0_vertex->GetNRows(); k++, v0++){
      Float_t e1a = v0->pos_px*v0->pos_px +  v0->pos_py*v0->pos_py
	+ v0->pos_pz*v0->pos_pz;
      Float_t e2 = v0->neg_px*v0->neg_px +  v0->neg_py*v0->neg_py
	+ v0->neg_pz*v0->neg_pz;
      Float_t e1 = e1a + m_prmass2;  
      e2 += m_pimass2;
      e1 = TMath::Sqrt(e1);
      e2 = TMath::Sqrt(e2);
      Float_t p = (v0->neg_px+v0->pos_px)*(v0->neg_px+v0->pos_px)
	+  (v0->neg_py+v0->pos_py)*(v0->neg_py+v0->pos_py)
	+ (v0->neg_pz+v0->pos_pz)*(v0->neg_pz+v0->pos_pz);
      Float_t inv_mass_la = TMath::Sqrt((e1+e2)*(e1+e2) - p);
      e1 = e1a + m_pimass2;
      e1 = TMath::Sqrt(e1);
      Float_t inv_mass_k0 = TMath::Sqrt((e1+e2)*(e1+e2) - p);
      m_ev0_lama_hist->Fill(inv_mass_la);
      m_ev0_k0ma_hist->Fill(inv_mass_k0);   
    }
  }
}

//_____________________________________________________________________________

void St_QA_Maker::MakeHistPID(St_DataSet *dst){
  if (Debug()) cout << " *** in St_QA_Maker - filling PID histograms " << endl;
  
  St_DataSetIter dstI(dst);        
  
  // spectra-PID diagnostic histograms
  St_dst_track      *primtrk     = (St_dst_track     *) dstI["primtrk"];
  St_dst_dedx       *dst_dedx    = (St_dst_dedx *) dstI["dst_dedx"];
  
  if (dst_dedx && primtrk) {
    dst_dedx_st  *d   = dst_dedx->GetTable();
    dst_track_st  *trk   = primtrk->GetTable();
    Int_t no_of_tracks  = primtrk->GetNRows();
    // loop over dedx entries
    for (Int_t l = 0; l < dst_dedx->GetNRows(); l++,d++){
      Float_t dedx_m = d->dedx[0];
      Int_t igl = d->id_track;
      Int_t igl_use = igl - 1;
      // this is bad style, since it assumes the global track has not been sorted
      // it works for now
      if (igl_use >= 0 && igl_use < no_of_tracks) {
	dst_track_st  *t = trk + igl_use ;
	if (t->iflag>0) {
	  Float_t invpt = t->invpt;
	  Float_t pT = 9999.;
	  if (invpt) pT = 1./TMath::Abs(invpt);
	  Float_t pz = pT*t->tanl;
	  Float_t  p = sqrt(pT*pT+pz*pz);
	  //     Float_t z0 = abs(t->x_first[2]);
	  Float_t x0 = t->x_first[0];
	  Float_t y0 = t->x_first[1];
	  //     Float_t r0 = sqrt(x0*x0+y0*y0);
	  
	  if (d->det_id==1 && d->ndedx >15 ) { 
	    m_p_dedx_rec->Fill(p,(float)(dedx_m*1e6)); // change from GeV/cm to keV/cm
	  }
	}
      }
    }
  }
}

//_____________________________________________________________________________


void St_QA_Maker::MakeHistVertex(St_DataSet *dst){
  if (Debug()) cout << " *** in St_QA_Maker - filling vertex histograms " << endl;
  St_DataSetIter dstI(dst);
  St_dst_vertex      *vertex     = (St_dst_vertex *) dstI["vertex"];
  
  if (vertex) {
    dst_vertex_st  *t   = vertex->GetTable();
    for (Int_t i = 0; i < vertex->GetNRows(); i++,t++){
      //         if (t->iflag>0) {  
      if (i==0){                           // plot of primary vertex only
	m_pv_detid->Fill(t->det_id); 
	m_pv_vtxid->Fill(t->vtx_id);
	if (!isnan(double(t->x))) m_pv_x->Fill(t->x);     
	if (!isnan(double(t->y))) m_pv_y->Fill(t->y);     
	if (!isnan(double(t->z))) m_pv_z->Fill(t->z);     
	m_pv_pchi2->Fill(t->pchi2);
      }
      m_v_detid->Fill(t->det_id); 
      m_v_vtxid->Fill(t->vtx_id);
      if (!isnan(double(t->x))) m_v_x->Fill(t->x);     
      if (!isnan(double(t->y))) m_v_y->Fill(t->y);     
      if (!isnan(double(t->z))) m_v_z->Fill(t->z);     
      m_v_pchi2->Fill(t->pchi2); 
      // }
    }
  }
}
//_____________________________________________________________________________
void St_QA_Maker::MakeHistXi(St_DataSet *dst){
  if (Debug()) cout << " *** in St_QA_Maker - filling dst_xi_vertex histograms " << endl;
}

//_____________________________________________________________________________


void St_QA_Maker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: St_QA_Maker.cxx,v 1.39 1999/07/09 12:12:54 kathy Exp $\n");
  //  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
  if (Debug()) StMaker::PrintInfo();
}
