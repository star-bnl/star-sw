/***************************************************************************
 *
 * $Id: StQACosmicMaker.cxx,v 1.4 1999/08/17 18:55:54 snelling Exp $
 *
 * Author: Raimond Snellings, LBNL, Jun 1999
 * Description:  Maker to QA the Cosmic data (hitfinding, tracking etc.)
 *
 * $Log: StQACosmicMaker.cxx,v $
 * Revision 1.4  1999/08/17 18:55:54  snelling
 * Added two member funtions: setSector and setNrXbins
 *
 * Revision 1.3  1999/08/17 01:44:31  snelling
 * changed ntuple projection to normal histogram filling
 *
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
#include "tpc/St_xyz_newtab_Module.h"
#include "TH1.h"
#include "TH2.h"
#include "TNtuple.h"
#include "TProfile.h"

//-----------------------------------------------------------------------
ClassImp(StQACosmicMaker)

  StQACosmicMaker::StQACosmicMaker(const char *name):
  StMaker(name), 
  bSectorSelectionOn(kFALSE), 
  nXBins(200) {
}

//-----------------------------------------------------------------------
StQACosmicMaker::~StQACosmicMaker() {

}

//-----------------------------------------------------------------------
Int_t StQACosmicMaker::Make() {

  fillTNtuple();
  fillHistograms();

  return kStOK;
}

//-----------------------------------------------------------------------
void StQACosmicMaker::PrintInfo() {
  printf("**************************************************************\n");
  printf("* $Id: StQACosmicMaker.cxx,v 1.4 1999/08/17 18:55:54 snelling Exp $\n");
  printf("**************************************************************\n");
  if (Debug()) StMaker::PrintInfo();
}

//-----------------------------------------------------------------------
Int_t StQACosmicMaker::Finish() {

  calcHistograms();
  
  return StMaker::Finish();
}

//-----------------------------------------------------------------------
Int_t StQACosmicMaker::Init() {
  
  initHistograms(); 
  initTNtuple();

  return StMaker::Init();
}

//-----------------------------------------------------------------------
Int_t StQACosmicMaker::initTNtuple() {
  
  mTNtupleTPC = new TNtuple("nttpc",
			    "TPC TNtuple",
			    "hrow:hx:hy:hz:hdx:hdy:hdz:halpha:hlamda:hdalpha:\
hdlamda:resy:resz:trknfit:trkcalcp");
  
  return kStOK;
  
}

//-----------------------------------------------------------------------
Int_t StQACosmicMaker::fillTNtuple() {

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
    
    mTNtupleTPC->Fill(
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
    
    if (Debug()) {
      cout << "dip angle " <<pttphit[i].lambda << endl;
    }

  }

  return kStOK;
}

//-----------------------------------------------------------------------
Int_t StQACosmicMaker::initHistograms() {

  int i;

  Float_t xMin = -100.;
  Float_t xMax = 100.;
  Float_t yMin = -1.;
  Float_t yMax = 1.;
  Int_t nYBins = 200;
  
  for (i = 0; i < 4; i++) {
    TString *mHistTitle;
    TString *mHistName;
    char mCount[1];
    sprintf(mCount,"%d",i);

    mHistTitle = new TString("xy residual vs crossing angle");
    mHistName  = new TString("xyresvsalpha");
    mHistName->Append(*mCount);
    ResidualHists[i].mXYResVersusAlpha =
      new TH2F(mHistName->Data(), mHistTitle->Data(), nXBins, xMin, xMax, nYBins, yMin, yMax);
    ResidualHists[i].mXYResVersusAlpha->SetXTitle("crossing angle (radians)");
    ResidualHists[i].mXYResVersusAlpha->SetYTitle("xy residuals");
    delete mHistTitle;
    delete mHistName;

    mHistTitle = new TString("Mean xy residual vs crossing angle");
    mHistName  = new TString("meanxyresvsalpha");
    mHistName->Append(*mCount);
    ResidualHists[i].FitHists.mXYResVersusAlpha_mean =
      new TH1D(mHistName->Data(), mHistTitle->Data(), nXBins, xMin, xMax);
    delete mHistTitle;
    delete mHistName;

    mHistTitle = new TString("Sigma xy residual vs crossing angle");
    mHistName  = new TString("sigmaxyresvsalpha");
    mHistName->Append(*mCount);
    ResidualHists[i].FitHists.mXYResVersusAlpha_sigma =
      new TH1D(mHistName->Data(), mHistTitle->Data(), nXBins, xMin, xMax);
    delete mHistTitle;
    delete mHistName;

    mHistTitle = new TString("Magnitude xy residual vs crossing angle");
    mHistName  = new TString("magxyresvsalpha");
    mHistName->Append(*mCount);
    ResidualHists[i].FitHists.mXYResVersusAlpha_mag =
      new TH1D(mHistName->Data(), mHistTitle->Data(), nXBins, xMin, xMax);
    delete mHistTitle;
    delete mHistName;

    mHistTitle = new TString("ChiSquared xy residual vs crossing angle");
    mHistName  = new TString("chixyresvsalpha");
    mHistName->Append(*mCount);
    ResidualHists[i].FitHists.mXYResVersusAlpha_chi =
      new TH1D(mHistName->Data(), mHistTitle->Data(), nXBins, xMin, xMax);
    delete mHistTitle;
    delete mHistName;

  }

  return kStOK;

}

//-----------------------------------------------------------------------
Int_t StQACosmicMaker::fillHistograms() {
  
  int i;

  // get pointers to raw adc xyz table
  St_DataSetIter Itpc_raw(GetDataSet("tpc_raw"));
  St_tfc_adcxyz *phtfc = 0;
  tfc_adcxyz_st *ptadcxyz = 0;
  phtfc = (St_tfc_adcxyz *) Itpc_raw.Find("adcxyz");
  if (phtfc) {ptadcxyz = phtfc->GetTable();}
  else { cout << "Warning: adcxyz table header does not exist " << endl; return kStWarn; }
  if (!ptadcxyz) { cout << "Warning: adcxyz table does not exist " << endl; return kStWarn; }

  
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
  
  for(i=0; i<phtcl->GetNRows(); i++) {
    Int_t irow_res = ressorter[(Int_t)(pttphit[i].id)];
    // track in row table is 1000*id + position on track
    Int_t irow_trk = trksorter[(Int_t)(pttphit[i].track/1000.)];
    Int_t isector = (Int_t)(pttphit[i].row/100.);
    
    Float_t trkcalcp = sqrt((pttrk[irow_trk].tanl * pttrk[irow_trk].tanl + 1) /
			    (pttrk[irow_trk].invp * pttrk[irow_trk].invp));


    // global cuts and no specific sector selected
    if (ptres[irow_res].resy != 0. && pttphit[i].alpha != 0. && !bSectorSelectionOn) {    
      // inner sector
      if (isector <= 13) {
	if (trkcalcp >= 0.3) {
	  ResidualHists[0].mXYResVersusAlpha->
	    Fill((Float_t)(pttphit[i].alpha),(Float_t)(ptres[irow_res].resy) );
	}
	else {
	  ResidualHists[1].mXYResVersusAlpha->
	    Fill((Float_t)(pttphit[i].alpha),(Float_t)(ptres[irow_res].resy));
	}
      }
      // outer sector
      else {
	if (trkcalcp >= 0.3) {
	  ResidualHists[2].mXYResVersusAlpha->
	    Fill((Float_t)(pttphit[i].alpha),(Float_t)(ptres[irow_res].resy));
	}
	else {
	  ResidualHists[3].mXYResVersusAlpha->
	    Fill((Float_t)(pttphit[i].alpha),(Float_t)(ptres[irow_res].resy));
	}
      }
    }
    // fill histograms only for selected sector, low momentum in hist1 rest in hist0
    else {
      if (isector == SelectedSector) {
	if (trkcalcp >= 0.3) {
	  ResidualHists[0].mXYResVersusAlpha->
	    Fill((Float_t)(pttphit[i].alpha),(Float_t)(ptres[irow_res].resy) );
	}
	else {
	  ResidualHists[1].mXYResVersusAlpha->
	    Fill((Float_t)(pttphit[i].alpha),(Float_t)(ptres[irow_res].resy));
	}
      }
    }
  }

  return kStOK;

}
//-----------------------------------------------------------------------

Int_t StQACosmicMaker::calcHistograms() {

  int i;

  for (i = 0; i < 4; i++) {
    TString *mHistTitle;
    TString *mHistName;
    char mCount[1];
    sprintf(mCount,"%d",i);

    //    TH2F *test=(TH2F*)chain->Maker("QACosmics")->GetHistList()->FindObject("xyresvsalpha0");   
    //    (TH1D*) Maker(GetName())->GetHistList()->FindObject(mHistName->Data());   

    ResidualHists[i].mXYResVersusAlpha->FitSlicesY();
    if (Debug()) {  
      cout << "pointer to hist: " << ResidualHists[i].mXYResVersusAlpha << endl;
    }

    mHistTitle = new TString("Magnitude xy residual vs crossing angle");
    mHistName  = new TString("xyresvsalpha");
    mHistName->Append(*mCount);
    mHistTitle->Append(*mCount);
    mHistName->Append("_0");
    ((TH1D *) gDirectory->Get(mHistName->Data()))->Copy(*ResidualHists[i].FitHists.mXYResVersusAlpha_mag);
    delete ((TH1D *) gDirectory->Get(mHistName->Data()));
    ResidualHists[i].FitHists.mXYResVersusAlpha_mag->SetName(mHistTitle->Data());
    delete mHistName;
    delete mHistTitle;
    ResidualHists[i].FitHists.mXYResVersusAlpha_mag->SetXTitle("Crossing Angle (radians)");
    ResidualHists[i].FitHists.mXYResVersusAlpha_mag->SetYTitle("Magnitude");


    mHistTitle = new TString("Mean xy residual vs crossing angle");
    mHistName  = new TString("xyresvsalpha");
    mHistName->Append(*mCount);
    mHistTitle->Append(*mCount);
    mHistName->Append("_1");
    ((TH1D *) gDirectory->Get(mHistName->Data()))->Copy(*ResidualHists[i].FitHists.mXYResVersusAlpha_mean);
    delete ((TH1D *) gDirectory->Get(mHistName->Data()));
    ResidualHists[i].FitHists.mXYResVersusAlpha_mean->SetName(mHistTitle->Data());
    delete mHistName;
    delete mHistTitle;
    ResidualHists[i].FitHists.mXYResVersusAlpha_mean->SetMaximum(.4);
    ResidualHists[i].FitHists.mXYResVersusAlpha_mean->SetMinimum(-.4);
    ResidualHists[i].FitHists.mXYResVersusAlpha_mean->SetXTitle("Crossing Angle (radians)");
    ResidualHists[i].FitHists.mXYResVersusAlpha_mean->SetYTitle("Mean xy residuals");
    ResidualHists[i].FitHists.mXYResVersusAlpha_mean->GetXaxis()->SetLabelSize(0.04);
    ResidualHists[i].FitHists.mXYResVersusAlpha_mean->GetYaxis()->SetLabelSize(0.04);
    ResidualHists[i].FitHists.mXYResVersusAlpha_mean->SetMarkerColor(kBlue);
    ResidualHists[i].FitHists.mXYResVersusAlpha_mean->SetMarkerStyle(20);

    mHistTitle = new TString("Sigma xy residual vs crossing angle");
    mHistName  = new TString("xyresvsalpha");
    mHistName->Append(*mCount);
    mHistTitle->Append(*mCount);
    mHistName->Append("_2");
    ((TH1D *) gDirectory->Get(mHistName->Data()))->Copy(*ResidualHists[i].FitHists.mXYResVersusAlpha_sigma);
    delete ((TH1D *) gDirectory->Get(mHistName->Data()));
    ResidualHists[i].FitHists.mXYResVersusAlpha_sigma->SetName(mHistTitle->Data());
    delete mHistName;
    delete mHistTitle;
    ResidualHists[i].FitHists.mXYResVersusAlpha_sigma->SetXTitle("Crossing Angle (radians)");
    ResidualHists[i].FitHists.mXYResVersusAlpha_sigma->SetYTitle("Sigma");
    
    mHistTitle = new TString("Chi2 xy residual vs crossing angle");
    mHistName  = new TString("xyresvsalpha");
    mHistName->Append(*mCount);
    mHistTitle->Append(*mCount);
    mHistName->Append("_chi2");
    ((TH1D *) gDirectory->Get(mHistName->Data()))->Copy(*ResidualHists[i].FitHists.mXYResVersusAlpha_chi);
    delete ((TH1D *) gDirectory->Get(mHistName->Data()));
    ResidualHists[i].FitHists.mXYResVersusAlpha_chi->SetName(mHistTitle->Data());
    delete mHistName;
    delete mHistTitle;
    ResidualHists[i].FitHists.mXYResVersusAlpha_chi->SetXTitle("Crossing Angle (radians)");
    ResidualHists[i].FitHists.mXYResVersusAlpha_chi->SetYTitle("chi2");
    
    for (int j=0; j<ResidualHists[i].FitHists.mXYResVersusAlpha_sigma->fN; j++) { 
      ResidualHists[i].FitHists.mXYResVersusAlpha_sigma->fArray[j] = 
    	fabs(ResidualHists[i].FitHists.mXYResVersusAlpha_sigma->fArray[j]);
    }

  }

  return kStOK;

}

//-----------------------------------------------------------------------

void StQACosmicMaker::setSector(const Int_t sectorNumber) {

  SectorSelectionOn();

  if (sectorNumber > 0 && sectorNumber <=24) {
  SelectedSector = sectorNumber;
  cout << "Sector: " << SelectedSector << " selected" << endl;
  }
  else {
    cout << "error sector selected which does not exist" << endl;
  }

}

//-----------------------------------------------------------------------
