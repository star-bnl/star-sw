/***************************************************************************
 *
 * $Id: StQACosmicMaker.cxx,v 1.2 1999/08/03 17:15:53 snelling Exp $
 *
 * Author: Raimond Snellings, LBNL, Jun 1999
 * Description:  Maker to QA the Cosmic data (hitfinding, tracking etc.)
 *
 * $Log: StQACosmicMaker.cxx,v $
 * Revision 1.2  1999/08/03 17:15:53  snelling
 * added id tags
 *
 *  
 **************************************************************************/
#include <iostream.h>
#include <stdlib.h>
#include <math.h>
#include "StQACosmicMaker.h"
#include "StChain.h"
#include "St_DataSet.h"
#include "St_DataSetIter.h"
#include "St_TableSorter.h"
#include "tpc/St_tcl_Module.h"
#include "tpc/St_tph_Module.h"
#include "tpc/St_tpt_residuals_Module.h"
#include "TH1.h"
#include "TH2.h"
#include "TNtuple.h"
#include "TProfile.h"

//-----------------------------------------------------------------------
ClassImp(StQACosmicMaker)

  StQACosmicMaker::StQACosmicMaker(const char *name):
  StMaker(name){
}

//-----------------------------------------------------------------------
StQACosmicMaker::~StQACosmicMaker() {
}

//-----------------------------------------------------------------------
Int_t StQACosmicMaker::Make() {

  // get pointers to tpc hit table
  St_DataSetIter Itpc_hits(GetDataSet("tpc_hits"));
  St_tcl_tphit *phtcl = 0;
  tcl_tphit_st *pttphit = 0;
  phtcl = (St_tcl_tphit *) Itpc_hits.Find("tphit");
  if (phtcl) {pttphit = phtcl->GetTable();}
  else { cout << "error: tphit table header does not exist " << endl; return kStWarn; }
  if (!pttphit) { cout << "error: tphit table does not exist " << endl; return kStWarn; }

  // get pointers to tpc residuals table and track table
  St_DataSetIter Itpc_trk(GetDataSet("tpc_tracks"));
  
  St_tpt_res *phres = 0;
  tpt_res_st *ptres = 0;
  phres = (St_tpt_res *) Itpc_trk.Find("restpt");
  if (phres) {ptres = phres->GetTable();}
  else { cout << "error: restpt table header does not exist " << endl; return kStWarn; }
  if (!ptres) { cout << "error: restpt table does not exist " << endl; return kStWarn; }

  St_tpt_track *phtrk = 0;
  tpt_track_st *pttrk = 0;
  phtrk = (St_tpt_track *) Itpc_trk.Find("tptrack");
  if (phtrk) {pttrk = phtrk->GetTable();}
  else { cout << "error: tptrack table header does not exist " << endl; return kStWarn; }
  if (!pttrk) { cout << "error: tptrack table does not exist " << endl; return kStWarn; }
  
  // create a sorter to get an index to a row
  Int_t nrows = phres->GetNRows();
  if (nrows == 0) {cout << "error: residual table contains zero rows " << endl; return kStWarn;}
  TString colName = "hit";
  St_TableSorter ressorter(*phres,colName,0,nrows-1);
  // create a sorter to get an index to a track
  nrows = phtrk->GetNRows();
  if (nrows == 0) {cout << "error: residual table contains zero rows " << endl; return kStWarn;}
  colName = "id";
  St_TableSorter trksorter(*phtrk,colName,0,nrows-1);

  for(Int_t i=0; i<phtcl->GetNRows();i++) {
    Int_t irow_res = ressorter[(Int_t)(pttphit[i].id)];
    // track in row table is 1000*id + position on track
    Int_t irow_trk = trksorter[(Int_t)(pttphit[i].track/1000.)];
    
    Float_t trkcalcp = sqrt((pttrk[irow_trk].tanl * pttrk[irow_trk].tanl + 1) /
			    (pttrk[irow_trk].invp * pttrk[irow_trk].invp));
    nttpc->Fill(
		(Float_t)(Int_t(pttphit[i].row/100.)),
		(Float_t)(pttphit[i].x),
		(Float_t)(pttphit[i].y),
		(Float_t)(pttphit[i].z),
		(Float_t)(pttphit[i].dx),
		(Float_t)(pttphit[i].dy),
		(Float_t)(pttphit[i].dz),
		(Float_t)(pttphit[i].alpha),
		(Float_t)(pttphit[i].lambda),
		(Float_t)(pttphit[i].dalpha),
		(Float_t)(pttphit[i].dlambda),
		(Float_t)(ptres[irow_res].resy),
		(Float_t)(ptres[irow_res].resz),
		(Float_t)(Float_t(pttrk[irow_trk].nfit)),
		(Float_t)(trkcalcp)
		);

    if (Debug()) {cout << "dip angle " <<pttphit[i].lambda << endl;}
  }
  
  return kStOK;
}

//-----------------------------------------------------------------------
void StQACosmicMaker::PrintInfo() {
  printf("**************************************************************\n");
  printf("* $Id: StQACosmicMaker.cxx,v 1.2 1999/08/03 17:15:53 snelling Exp $\n");
  printf("**************************************************************\n");
  if (Debug()) StMaker::PrintInfo();
}

//-----------------------------------------------------------------------
Int_t StQACosmicMaker::Finish() {
  MakeHistograms();
  return StMaker::Finish();
}
//-----------------------------------------------------------------------
Int_t StQACosmicMaker::Init() {
  
  Float_t xmin = -50.;
  Float_t xmax = 50.;
  Int_t nxbin = 25;
  Float_t ymin = -0.5;
  Float_t ymax = 0.5;
  Int_t nybin = 100;

  xyresvsalpha_prof = new TProfile("xyresvsalpha_prof",
				   "profile hist for xy residual vs crossing angle",
				   50,xmin,xmax,-1000.,1000.,"");
  
  nttpc = new TNtuple("nttpc",
		      "tpc ntuple",
		      "hrow:hx:hy:hz:hdx:hdy:hdz:halpha:hlamda:hdalpha:\
hdlamda:resy:resz:trknfit:trkcalcp");

  xyresvsalpha_inner = new TH2F("xyresvsalpha_inner",
				"xy residual vs crossing angle",
				nxbin,xmin,xmax,nybin,ymin,ymax);

  xyresvsalpha_inner_mean = new TH1D("xyresvsalpha_inner_mean",
				     "mean value gaussian fit xy residual vs crossing angle",
				     nxbin,xmin,xmax);

  xyresvsalpha_inner_sigma = new TH1D("xyresvsalpha_inner_sigma",
				      "sigma gaussian fit xy residual vs crossing angle",
				      nxbin,xmin,xmax);

  xyresvsalpha_inner_mag = new TH1D("xyresvsalpha_inner_mag",
				    "magnitude gaussian fit xy residual vs crossing angle",
				    nxbin,xmin,xmax);

  xyresvsalpha_inner_chi = new TH1D("xyresvsalpha_inner_chi",
				    "chi squared gaussian fit xy residual vs crossing angle",
				    nxbin,xmin,xmax);

  xyresvsalpha_inner_lowpt = new TH2F("xyresvsalpha_inner_lowpt",
				      "xy residual vs crossing angle",
				      nxbin,xmin,xmax,nybin,ymin,ymax);

  xyresvsalpha_inner_lowpt_mean = new TH1D("xyresvsalpha_inner_lowpt_mean",
					   "mean value gaussian fit xy residual vs crossing angle",
					   nxbin,xmin,xmax);

  xyresvsalpha_inner_lowpt_sigma = new TH1D("xyresvsalpha_inner_lowpt_sigma",
					    "sigma gaussian fit xy residual vs crossing angle",
					    nxbin,xmin,xmax);

  xyresvsalpha_inner_lowpt_mag = new TH1D("xyresvsalpha_inner_lowpt_mag",
					  "magnitude gaussian fit xy residual vs crossing angle",
					  nxbin,xmin,xmax);
  
  xyresvsalpha_inner_lowpt_chi = new TH1D("xyresvsalpha_inner_lowpt_chi",
					  "chi squared gaussian fit xy residual vs crossing angle",
					  nxbin,xmin,xmax);

  xyresvsalpha_outer = new TH2F("xyresvsalpha_outer",
				"xy residual vs crossing angle",
				nxbin,xmin,xmax,nybin,ymin,ymax);

  xyresvsalpha_outer_mean = new TH1D("xyresvsalpha_outer_mean",
				     "mean value gaussian fit xy residual vs crossing angle",
				     nxbin,xmin,xmax);

  xyresvsalpha_outer_sigma = new TH1D("xyresvsalpha_outer_sigma",
				      "sigma gaussian fit xy residual vs crossing angle",
				      nxbin,xmin,xmax);

  xyresvsalpha_outer_mag = new TH1D("xyresvsalpha_outer_mag",
				    "magnitude gaussian fit xy residual vs crossing angle",
				    nxbin,xmin,xmax);

  xyresvsalpha_outer_chi = new TH1D("xyresvsalpha_outer_chi",
				    "chi squared gaussian fit xy residual vs crossing angle",
				    nxbin,xmin,xmax);

  xyresvsalpha_outer_lowpt = new TH2F("xyresvsalpha_outer_lowpt",
				      "xy residual vs crossing angle",
				      nxbin,xmin,xmax,nybin,ymin,ymax);

  xyresvsalpha_outer_lowpt_mean = new TH1D("xyresvsalpha_outer_lowpt_mean",
					   "mean value gaussian fit xy residual vs crossing angle",
					   nxbin,xmin,xmax);

  xyresvsalpha_outer_lowpt_sigma = new TH1D("xyresvsalpha_outer_lowpt_sigma",
					    "sigma gaussian fit xy residual vs crossing angle",
					    nxbin,xmin,xmax);

  xyresvsalpha_outer_lowpt_mag = new TH1D("xyresvsalpha_outer_lowpt_mag",
					  "magnitude gaussian fit xy residual vs crossing angle",
					  nxbin,xmin,xmax);

  xyresvsalpha_outer_lowpt_chi = new TH1D("xyresvsalpha_outer_lowpt_chi",
					  "chi squared gaussian fit xy residual vs crossing angle",
					  nxbin,xmin,xmax);

  return StMaker::Init();
}

//-----------------------------------------------------------------------
void StQACosmicMaker::MakeHistograms() {

  Int_t i = 0;

  nttpc->Draw("resy:halpha>>xyresvsalpha_prof",
	      "resy != 0. && halpha != 0.",
	      "prof,goff");
  
  nttpc->Draw("resy:halpha>>xyresvsalpha_inner",
	      "resy != 0. && halpha != 0. && hrow <= 13. && trkcalcp >= 0.3",
	      "goff,same");
  xyresvsalpha_inner->FitSlicesY();
  ((TH1D *) gDirectory->Get("xyresvsalpha_inner_0"))->Copy(*xyresvsalpha_inner_mag);
  ((TH1D *) gDirectory->Get("xyresvsalpha_inner_1"))->Copy(*xyresvsalpha_inner_mean);
  ((TH1D *) gDirectory->Get("xyresvsalpha_inner_2"))->Copy(*xyresvsalpha_inner_sigma);
  ((TH1D *) gDirectory->Get("xyresvsalpha_inner_chi2"))->Copy(*xyresvsalpha_inner_chi);

  for (i=0;i<xyresvsalpha_inner_sigma->fN;i++) { 
    xyresvsalpha_inner_sigma->fArray[i]=fabs(xyresvsalpha_inner_sigma->fArray[i]);
  }
  
  xyresvsalpha_inner_sigma->SetXTitle("Crossing Angle");
  xyresvsalpha_inner_sigma->SetYTitle("sigma (cm)");
  xyresvsalpha_inner_sigma->SetTitle("sigma gaussian fit of xy residual vs crossing angle");
  xyresvsalpha_inner_sigma->SetName("xyresvsalpha_inner_sigma");
  xyresvsalpha_inner_sigma->GetXaxis()->SetLabelSize(0.04);
  xyresvsalpha_inner_sigma->GetYaxis()->SetLabelSize(0.04);
  xyresvsalpha_inner_sigma->SetMarkerColor(kRed);
  xyresvsalpha_inner_sigma->SetMarkerStyle(21);
  xyresvsalpha_inner_sigma->SetMaximum(0.5);
  
  xyresvsalpha_inner_mean->SetXTitle("Crossing Angle");
  xyresvsalpha_inner_mean->SetYTitle("mean (cm)");
  xyresvsalpha_inner_mean->SetTitle("mean gaussian fit of xy residual vs crossing angle");
  xyresvsalpha_inner_mean->SetName("xyresvsalpha_inner_mean");
  xyresvsalpha_inner_mean->GetXaxis()->SetLabelSize(0.04);
  xyresvsalpha_inner_mean->GetYaxis()->SetLabelSize(0.04);
  xyresvsalpha_inner_mean->SetMarkerColor(kBlue);
  xyresvsalpha_inner_mean->SetMarkerStyle(20);

  nttpc->Draw("resy:halpha>>xyresvsalpha_inner_lowpt",
	      "resy != 0. && halpha != 0. && hrow <= 13. && trkcalcp <= 0.3",
	      "goff,same");
  xyresvsalpha_inner_lowpt->FitSlicesY();
  ((TH1D *) gDirectory->Get("xyresvsalpha_inner_lowpt_0"))->Copy(*xyresvsalpha_inner_lowpt_mag);
  ((TH1D *) gDirectory->Get("xyresvsalpha_inner_lowpt_1"))->Copy(*xyresvsalpha_inner_lowpt_mean);
  ((TH1D *) gDirectory->Get("xyresvsalpha_inner_lowpt_2"))->Copy(*xyresvsalpha_inner_lowpt_sigma);
  ((TH1D *) gDirectory->Get("xyresvsalpha_inner_lowpt_chi2"))->Copy(*xyresvsalpha_inner_lowpt_chi);
  
  for (i=0;i<xyresvsalpha_inner_lowpt_sigma->fN;i++) { 
    xyresvsalpha_inner_lowpt_sigma->fArray[i]=fabs(xyresvsalpha_inner_lowpt_sigma->fArray[i]);
  }
  
  xyresvsalpha_inner_lowpt_sigma->SetXTitle("Crossing Angle");
  xyresvsalpha_inner_lowpt_sigma->SetYTitle("sigma (cm)");
  xyresvsalpha_inner_lowpt_sigma->SetTitle("sigma gaussian fit of xy residual vs crossing angle");
  xyresvsalpha_inner_lowpt_sigma->SetName("xyresvsalpha_inner_lowpt_sigma");
  xyresvsalpha_inner_lowpt_sigma->GetXaxis()->SetLabelSize(0.04);
  xyresvsalpha_inner_lowpt_sigma->GetYaxis()->SetLabelSize(0.04);
  xyresvsalpha_inner_lowpt_sigma->SetMarkerColor(kRed);
  xyresvsalpha_inner_lowpt_sigma->SetMarkerStyle(21);
  
  xyresvsalpha_inner_lowpt_mean->SetXTitle("Crossing Angle");
  xyresvsalpha_inner_lowpt_mean->SetYTitle("mean (cm)");
  xyresvsalpha_inner_lowpt_mean->SetTitle("mean gaussian fit of xy residual vs crossing angle");
  xyresvsalpha_inner_lowpt_mean->SetName("xyresvsalpha_inner_lowpt_mean");
  xyresvsalpha_inner_lowpt_mean->GetXaxis()->SetLabelSize(0.04);
  xyresvsalpha_inner_lowpt_mean->GetYaxis()->SetLabelSize(0.04);
  xyresvsalpha_inner_lowpt_mean->SetMarkerColor(kBlue);
  xyresvsalpha_inner_lowpt_mean->SetMarkerStyle(20);
  
  
  nttpc->Draw("resy:halpha>>xyresvsalpha_outer",
	      "resy != 0. && halpha != 0. && hrow > 13. && trkcalcp >= 0.3",
	      "goff,same");
  xyresvsalpha_outer->FitSlicesY();
  ((TH1D *) gDirectory->Get("xyresvsalpha_outer_0"))->Copy(*xyresvsalpha_outer_mag);
  ((TH1D *) gDirectory->Get("xyresvsalpha_outer_1"))->Copy(*xyresvsalpha_outer_mean);
  ((TH1D *) gDirectory->Get("xyresvsalpha_outer_2"))->Copy(*xyresvsalpha_outer_sigma);
  ((TH1D *) gDirectory->Get("xyresvsalpha_outer_chi2"))->Copy(*xyresvsalpha_outer_chi);
  
  for (i=0;i<xyresvsalpha_outer_sigma->fN;i++) { 
    xyresvsalpha_outer_sigma->fArray[i]=fabs(xyresvsalpha_outer_sigma->fArray[i]);
  }
  
  xyresvsalpha_outer_sigma->SetXTitle("Crossing Angle");
  xyresvsalpha_outer_sigma->SetYTitle("sigma (cm)");
  xyresvsalpha_outer_sigma->SetTitle("sigma gaussian fit of xy residual vs crossing angle");
  xyresvsalpha_outer_sigma->SetName("xyresvsalpha_outer_sigma");
  xyresvsalpha_outer_sigma->GetXaxis()->SetLabelSize(0.04);
  xyresvsalpha_outer_sigma->GetYaxis()->SetLabelSize(0.04);
  xyresvsalpha_outer_sigma->SetMarkerColor(kRed);
  xyresvsalpha_outer_sigma->SetMarkerStyle(21);
  xyresvsalpha_outer_sigma->SetMaximum(0.5);
  
  xyresvsalpha_outer_mean->SetXTitle("Crossing Angle");
  xyresvsalpha_outer_mean->SetYTitle("mean (cm)");
  xyresvsalpha_outer_mean->SetTitle("mean gaussian fit of xy residual vs crossing angle");
  xyresvsalpha_outer_mean->SetName("xyresvsalpha_outer_mean");
  xyresvsalpha_outer_mean->GetXaxis()->SetLabelSize(0.04);
  xyresvsalpha_outer_mean->GetYaxis()->SetLabelSize(0.04);
  xyresvsalpha_outer_mean->SetMarkerColor(kBlue);
  xyresvsalpha_outer_mean->SetMarkerStyle(20);
  
  nttpc->Draw("resy:halpha>>xyresvsalpha_outer_lowpt",
	      "resy != 0. && halpha != 0. && hrow > 13. && trkcalcp <= 0.3",
	      "goff,same");
  xyresvsalpha_outer_lowpt->FitSlicesY();
  // *xyresvsalpha_mean  = *((TH1D *) gDirectory->Get("xyresvsalpha_1"));
  ((TH1D *) gDirectory->Get("xyresvsalpha_outer_lowpt_0"))->Copy(*xyresvsalpha_outer_lowpt_mag);
  ((TH1D *) gDirectory->Get("xyresvsalpha_outer_lowpt_1"))->Copy(*xyresvsalpha_outer_lowpt_mean);
  ((TH1D *) gDirectory->Get("xyresvsalpha_outer_lowpt_2"))->Copy(*xyresvsalpha_outer_lowpt_sigma);
  ((TH1D *) gDirectory->Get("xyresvsalpha_outer_lowpt_chi2"))->Copy(*xyresvsalpha_outer_lowpt_chi);
  
  for (i=0;i<xyresvsalpha_outer_lowpt_sigma->fN;i++) { 
    xyresvsalpha_outer_lowpt_sigma->fArray[i]=fabs(xyresvsalpha_outer_lowpt_sigma->fArray[i]);
  }
  
  xyresvsalpha_outer_lowpt_sigma->SetXTitle("Crossing Angle");
  xyresvsalpha_outer_lowpt_sigma->SetYTitle("sigma (cm)");
  xyresvsalpha_outer_lowpt_sigma->SetTitle("sigma gaussian fit of xy residual vs crossing angle");
  xyresvsalpha_outer_lowpt_sigma->SetName("xyresvsalpha_outer_lowpt_sigma");
  xyresvsalpha_outer_lowpt_sigma->GetXaxis()->SetLabelSize(0.04);
  xyresvsalpha_outer_lowpt_sigma->GetYaxis()->SetLabelSize(0.04);
  xyresvsalpha_outer_lowpt_sigma->SetMarkerColor(kRed);
  xyresvsalpha_outer_lowpt_sigma->SetMarkerStyle(21);
  
  xyresvsalpha_outer_lowpt_mean->SetXTitle("Crossing Angle");
  xyresvsalpha_outer_lowpt_mean->SetYTitle("mean (cm)");
  xyresvsalpha_outer_lowpt_mean->SetTitle("mean gaussian fit of xy residual vs crossing angle");
  xyresvsalpha_outer_lowpt_mean->SetName("xyresvsalpha_outer_lowpt_mean");
  xyresvsalpha_outer_lowpt_mean->GetXaxis()->SetLabelSize(0.04);
  xyresvsalpha_outer_lowpt_mean->GetYaxis()->SetLabelSize(0.04);
  xyresvsalpha_outer_lowpt_mean->SetMarkerColor(kBlue);
  xyresvsalpha_outer_lowpt_mean->SetMarkerStyle(20);
  
}




