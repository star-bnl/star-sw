/***************************************************************************
 *
 * $Id: StQACosmicMaker.cxx,v 1.1.1.2 1999/09/08 17:34:21 snelling Exp $
 *
 * Author: Raimond Snellings, LBNL, Jun 1999
 * Description:  Maker to QA the Cosmic data (hitfinding, tracking etc.)
 *
 * $Log: StQACosmicMaker.cxx,v $
 * Revision 1.1.1.2  1999/09/08 17:34:21  snelling
 * added documentation
 *
 * Revision 1.8  1999/09/03 16:07:36  snelling
 * Added method to write out histogrmas and TNtuple
 *
 * Revision 1.7  1999/08/26 20:36:25  snelling
 * Fixed number of bins for row plot and changed fit function to landau
 *
 * Revision 1.6  1999/08/26 03:36:57  snelling
 * Added Q versus pad
 *
 * Revision 1.5  1999/08/19 00:29:57  snelling
 * Added Q distribution histograms and only used points on track
 *
 * Revision 1.4  1999/08/17 18:55:54  snelling
 * Added two member functions: setSector and setNrXbins
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
#include "TF1.h"
#include "TH1.h"
#include "TH2.h"
#include "TNtuple.h"
#include "TProfile.h"
#include "TFile.h"
#include "TString.h"


//-----------------------------------------------------------------------

ClassImp(StQACosmicMaker)

StQACosmicMaker::StQACosmicMaker(const char *name):
  StMaker(name),
  bSectorSelectionOn(kFALSE),
  bWriteTNtupleOn(kFALSE),
  bWritePostscriptOn(kFALSE),
  bWriteHistogramsOn(kFALSE),
  nXBins(50),
  MakerName(name)
{
  
  
}

//-----------------------------------------------------------------------

StQACosmicMaker::~StQACosmicMaker() {
}

//-----------------------------------------------------------------------

Int_t StQACosmicMaker::Init() {

  if (bWriteTNtupleOn) {initTNtuple();}
  initResHistograms();
  initChargeHistograms();

  return StMaker::Init();
}

//-----------------------------------------------------------------------

Int_t StQACosmicMaker::Make() {

  if (bWriteTNtupleOn) {fillTNtuple();}
  fillResHistograms();
  fillChargeHistograms();

  return kStOK;
}

//-----------------------------------------------------------------------

void StQACosmicMaker::PrintInfo() {
  printf("**************************************************************\n");
  printf("* $Id: StQACosmicMaker.cxx,v 1.1.1.2 1999/09/08 17:34:21 snelling Exp $\n");
  printf("**************************************************************\n");
  if (Debug()) StMaker::PrintInfo();
}

//-----------------------------------------------------------------------

Int_t StQACosmicMaker::Finish() {

  calcResHistograms();
  calcChargeHistograms();
  if (bWriteTNtupleOn) {writeOutTNtuple();}
  if (bWriteHistogramsOn) {writeOutHistograms();}
  
  return StMaker::Finish();
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

  /*
  if (this->GetOption(kEval)) {
    // get pointers to raw adc xyz table
    St_DataSetIter Itpc_raw(GetDataSet("tpc_raw"));
    St_tfc_adcxyz *phtfc = 0;
    tfc_adcxyz_st *ptadcxyz = 0;
    phtfc = (St_tfc_adcxyz *) Itpc_raw.Find("adcxyz");
    if (phtfc) {ptadcxyz = phtfc->GetTable();}
    else { cout << "Error: adcxyz table header does not exist " << endl; return kStWarn; }
    if (!ptadcxyz) { cout << "Error: adcxyz table does not exist " << endl; return kStWarn; }
  }
  */

  // get pointers to tpc hit table
  St_DataSetIter Itpc_hits(GetDataSet("tpc_hits"));
  St_tcl_tphit *phtcl = 0;
  tcl_tphit_st *pttphit = 0;
  phtcl = (St_tcl_tphit *) Itpc_hits.Find("tphit");
  if (phtcl) {pttphit = phtcl->GetTable();}
  else { cout << "Error: tphit table header does not exist " << endl; return kStWarn; }
  if (!pttphit) { cout << "Error: tphit table does not exist " << endl; return kStWarn; }

  // get pointers to tpc residuals table and track table
  St_DataSetIter Itpc_trk(GetDataSet("tpc_tracks"));
  St_tpt_res *phres = 0;
  tpt_res_st *ptres = 0;
  phres = (St_tpt_res *) Itpc_trk.Find("restpt");
  if (phres) {ptres = phres->GetTable();}
  else { cout << "Error: restpt table header does not exist " << endl; return kStWarn; }
  if (!ptres) { cout << "Error: restpt table does not exist " << endl; return kStWarn; }
  St_tpt_track *phtrk = 0;
  tpt_track_st *pttrk = 0;
  phtrk = (St_tpt_track *) Itpc_trk.Find("tptrack");
  if (phtrk) {pttrk = phtrk->GetTable();}
  else { cout << "Error: tptrack table header does not exist " << endl; return kStWarn; }
  if (!pttrk) { cout << "Error: tptrack table does not exist " << endl; return kStWarn; }

  // create a sorter to get an index to a row
  Int_t nrows = phres->GetNRows();
  if (nrows == 0) {cout << "Error: residual table contains zero rows " << endl; return kStWarn;}
  TString colName = "hit";
  St_TableSorter ressorter(*phres,colName,0,nrows-1);
  // create a sorter to get an index to a track
  nrows = phtrk->GetNRows();
  if (nrows == 0) {cout << "Error: residual table contains zero rows " << endl; return kStWarn;}
  colName = "id";
  St_TableSorter trksorter(*phtrk,colName,0,nrows-1);

  for(Int_t i=0; i<phtcl->GetNRows();i++) {
    Int_t irow_res = ressorter[(Int_t)(pttphit[i].id)];
    // track in row table is 1000*id + position on track
    Int_t irow_trk = trksorter[(Int_t)(pttphit[i].track/1000.)];

    if (irow_trk >= 0) {

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
      
      if (Debug()) {cout << "dip angle " <<pttphit[i].lambda << endl;}
    }
  }

  return kStOK;
}

//-----------------------------------------------------------------------

Int_t StQACosmicMaker::initResHistograms() {

  int i;
  TString *mHistTitle;
  TString *mHistName;
  char mCount[2];
  char *mIndexName[nChargeHist] = {" inner p > .3 GeV"," inner p < .3 GeV",
				    " outer p > .3 GeV"," outer p < .3 GeV"};

  char mSector[3];
  if (bSectorSelectionOn) {sprintf(mSector,"%2d",SelectedSector);}

  Float_t xMin = -100.;
  Float_t xMax = 100.;
  Float_t yMin = -1.;
  Float_t yMax = 1.;
  Int_t nYBins = 200;
  
  for (i = 0; i < nResHist; i++) {

    sprintf(mCount,"%1d",i);

    mHistTitle = new TString("xy residual vs crossing angle");
    mHistTitle->Append(mIndexName[i]);
    if (bSectorSelectionOn) {mHistTitle->Append(" Sector "); mHistTitle->Append(*mSector);}
    mHistName  = new TString("xyresvsalpha");
    mHistName->Append(*mCount);
    ResidualHists[i].mXYResVersusAlpha =
      new TH2F(mHistName->Data(), mHistTitle->Data(), nXBins, xMin, xMax, nYBins, yMin, yMax);
    ResidualHists[i].mXYResVersusAlpha->SetXTitle("crossing angle (radians)");
    ResidualHists[i].mXYResVersusAlpha->SetYTitle("xy residuals");
    delete mHistTitle;
    delete mHistName;

    mHistTitle = new TString("Mean xy residual vs crossing angle");
    mHistTitle->Append(mIndexName[i]);
    if (bSectorSelectionOn) {mHistTitle->Append(" Sector "); mHistTitle->Append(*mSector);}
    mHistName  = new TString("meanxyresvsalpha");
    mHistName->Append(*mCount);
    ResidualHists[i].FitHists.mXYResVersusAlpha_mean =
      new TH1D(mHistName->Data(), mHistTitle->Data(), nXBins, xMin, xMax);
    delete mHistTitle;
    delete mHistName;

    mHistTitle = new TString("Sigma xy residual vs crossing angle");
    mHistTitle->Append(mIndexName[i]);
    if (bSectorSelectionOn) {mHistTitle->Append(" Sector "); mHistTitle->Append(*mSector);}
    mHistName  = new TString("sigmaxyresvsalpha");
    mHistName->Append(*mCount);
    ResidualHists[i].FitHists.mXYResVersusAlpha_sigma =
      new TH1D(mHistName->Data(), mHistTitle->Data(), nXBins, xMin, xMax);
    delete mHistTitle;
    delete mHistName;

    mHistTitle = new TString("Magnitude xy residual vs crossing angle");
    mHistTitle->Append(mIndexName[i]);
    if (bSectorSelectionOn) {mHistTitle->Append(" Sector "); mHistTitle->Append(*mSector);}
    mHistName  = new TString("magxyresvsalpha");
    mHistName->Append(*mCount);
    ResidualHists[i].FitHists.mXYResVersusAlpha_mag =
      new TH1D(mHistName->Data(), mHistTitle->Data(), nXBins, xMin, xMax);
    delete mHistTitle;
    delete mHistName;

    mHistTitle = new TString("ChiSquared xy residual vs crossing angle");
    mHistTitle->Append(mIndexName[i]);
    if (bSectorSelectionOn) {mHistTitle->Append(" Sector "); mHistTitle->Append(*mSector);}
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

Int_t StQACosmicMaker::fillResHistograms() {
  
  int i;

  // get pointers to tpc hit table
  St_DataSetIter Itpc_hits(GetDataSet("tpc_hits"));
  St_tcl_tphit *phtcl = 0;
  tcl_tphit_st *pttphit = 0;
  phtcl = (St_tcl_tphit *) Itpc_hits.Find("tphit");
  if (phtcl) {pttphit = phtcl->GetTable();}
  else { cout << "Error: tphit table header does not exist " << endl; return kStWarn; }
  if (!pttphit) { cout << "Error: tphit table does not exist " << endl; return kStWarn; }
  
  // get pointers to tpc residuals table and track table
  St_DataSetIter Itpc_trk(GetDataSet("tpc_tracks"));
  St_tpt_res *phres = 0;
  tpt_res_st *ptres = 0;
  phres = (St_tpt_res *) Itpc_trk.Find("restpt");
  if (phres) {ptres = phres->GetTable();}
  else { cout << "Error: restpt table header does not exist " << endl; return kStWarn; }
  if (!ptres) { cout << "Error: restpt table does not exist " << endl; return kStWarn; }
  St_tpt_track *phtrk = 0;
  tpt_track_st *pttrk = 0;
  phtrk = (St_tpt_track *) Itpc_trk.Find("tptrack");
  if (phtrk) {pttrk = phtrk->GetTable();}
  else { cout << "Error: tptrack table header does not exist " << endl; return kStWarn; }
  if (!pttrk) { cout << "Error: tptrack table does not exist " << endl; return kStWarn; }

  // create a sorter to get an index to a row
  Int_t nrows = phres->GetNRows();
  if (nrows == 0) {cout << "Error: residual table contains zero rows " << endl; return kStWarn;}
  TString colName = "hit";
  St_TableSorter ressorter(*phres,colName,0,nrows-1);
  // create a sorter to get an index to a track
  nrows = phtrk->GetNRows();
  if (nrows == 0) {cout << "Error: residual table contains zero rows " << endl; return kStWarn;}
  colName = "id";
  St_TableSorter trksorter(*phtrk,colName,0,nrows-1);

  // fill histograms  
  for(i=0; i<phtcl->GetNRows(); i++) {
    Int_t irow_res = ressorter[(Int_t)(pttphit[i].id)];
    // track in tphit table is 1000*id + position on track
    Int_t irow_trk = trksorter[(Int_t)(pttphit[i].track/1000.)];
    // row in tphit table 100*sector + row
    Int_t isector = (Int_t)(pttphit[i].row/100.);
    Int_t irowsector = pttphit[i].row - 100 * isector;   

    // global cuts and only particles belonging to track
    if (ptres[irow_res].resy != 0. && pttphit[i].alpha != 0. && irow_trk >= 0) {
      // calculate total momentum of the track where the hit belongs to
      Float_t trkcalcp = sqrt((pttrk[irow_trk].tanl * pttrk[irow_trk].tanl + 1) /
			      (pttrk[irow_trk].invp * pttrk[irow_trk].invp));

      //  no specific sector selected 
      if (!bSectorSelectionOn) {
	// inner sector
	if (irowsector <= 13) {
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
	  // inner sector
	  if (irowsector <= 13) {
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
		Fill((Float_t)(pttphit[i].alpha),(Float_t)(ptres[irow_res].resy) );
	    }
	    else {
	      ResidualHists[3].mXYResVersusAlpha->
		Fill((Float_t)(pttphit[i].alpha),(Float_t)(ptres[irow_res].resy));
	    }
	  }
	}
      }
    }
  }

  return kStOK;
}

//-----------------------------------------------------------------------

Int_t StQACosmicMaker::calcResHistograms() {

  int i;
  TString *mHistTitle;
  TString *mHistName;
  char mCount[2];
  char *mIndexName[nChargeHist] = {" inner p > .3 GeV"," inner p < .3 GeV",
				   " outer p > .3 GeV"," outer p < .3 GeV"};
  char mSector[3];
  if (bSectorSelectionOn) {sprintf(mSector,"%2d",SelectedSector);}

  for (i = 0; i < nResHist; i++) {

    sprintf(mCount,"%1d",i);

    mHistTitle = new TString("xy residual vs crossing angle");
    mHistTitle->Append(mIndexName[i]);
    ResidualHists[i].mXYResVersusAlpha->FitSlicesY();
    ResidualHists[i].mXYResVersusAlpha->SetName(mHistTitle->Data());
    ResidualHists[i].mXYResVersusAlpha->SetTitle(mHistTitle->Data());
    delete mHistTitle;

    if (Debug()) {  
      cout << "pointer to hist: " << ResidualHists[i].mXYResVersusAlpha << endl;
    }

    mHistTitle = new TString("Magnitude xy residual vs crossing angle");
    mHistTitle->Append(mIndexName[i]);
    if (bSectorSelectionOn) {mHistTitle->Append(" Sector "); mHistTitle->Append(*mSector);}
    mHistName  = new TString("xyresvsalpha");
    mHistName->Append(*mCount);
    mHistName->Append("_0");
    ((TH1D *) gDirectory->Get(mHistName->Data()))->Copy(*ResidualHists[i].FitHists.mXYResVersusAlpha_mag);
    delete ((TH1D *) gDirectory->Get(mHistName->Data()));
    ResidualHists[i].FitHists.mXYResVersusAlpha_mag->SetName(mHistTitle->Data());
    ResidualHists[i].FitHists.mXYResVersusAlpha_mag->SetTitle(mHistTitle->Data());
    delete mHistName;
    delete mHistTitle;
    ResidualHists[i].FitHists.mXYResVersusAlpha_mag->SetXTitle("Crossing Angle (radians)");
    ResidualHists[i].FitHists.mXYResVersusAlpha_mag->SetYTitle("Magnitude");

    mHistTitle = new TString("Mean xy residual vs crossing angle");
    mHistTitle->Append(mIndexName[i]);
    if (bSectorSelectionOn) {mHistTitle->Append(" Sector "); mHistTitle->Append(*mSector);}
    mHistName  = new TString("xyresvsalpha");
    mHistName->Append(*mCount);
    mHistName->Append("_1");
    ((TH1D *) gDirectory->Get(mHistName->Data()))->Copy(*ResidualHists[i].FitHists.mXYResVersusAlpha_mean);
    delete ((TH1D *) gDirectory->Get(mHistName->Data()));
    ResidualHists[i].FitHists.mXYResVersusAlpha_mean->SetName(mHistTitle->Data());
    ResidualHists[i].FitHists.mXYResVersusAlpha_mean->SetTitle(mHistTitle->Data());
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
    mHistTitle->Append(mIndexName[i]);
    if (bSectorSelectionOn) {mHistTitle->Append(" Sector "); mHistTitle->Append(*mSector);}
    mHistName  = new TString("xyresvsalpha");
    mHistName->Append(*mCount);
    mHistName->Append("_2");
    ((TH1D *) gDirectory->Get(mHistName->Data()))->Copy(*ResidualHists[i].FitHists.mXYResVersusAlpha_sigma);
    delete ((TH1D *) gDirectory->Get(mHistName->Data()));
    ResidualHists[i].FitHists.mXYResVersusAlpha_sigma->SetName(mHistTitle->Data());
    ResidualHists[i].FitHists.mXYResVersusAlpha_sigma->SetTitle(mHistTitle->Data());
    delete mHistName;
    delete mHistTitle;
    ResidualHists[i].FitHists.mXYResVersusAlpha_sigma->SetXTitle("Crossing Angle (radians)");
    ResidualHists[i].FitHists.mXYResVersusAlpha_sigma->SetYTitle("Sigma");
    
    mHistTitle = new TString("Chi2 xy residual vs crossing angle");
    mHistTitle->Append(mIndexName[i]);
    if (bSectorSelectionOn) {mHistTitle->Append(" Sector "); mHistTitle->Append(*mSector);}
    mHistName  = new TString("xyresvsalpha");
    mHistName->Append(*mCount);
    mHistName->Append("_chi2");
    ((TH1D *) gDirectory->Get(mHistName->Data()))->Copy(*ResidualHists[i].FitHists.mXYResVersusAlpha_chi);
    delete ((TH1D *) gDirectory->Get(mHistName->Data()));
    ResidualHists[i].FitHists.mXYResVersusAlpha_chi->SetName(mHistTitle->Data());
    ResidualHists[i].FitHists.mXYResVersusAlpha_chi->SetTitle(mHistTitle->Data());
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

Int_t StQACosmicMaker::initChargeHistograms() {

  int i;
  char *mIndexName[nChargeHist]={"x","y","z","row"};
  TString *mHistTitle;
  TString *mHistName;

  char mSector[3];
  if (bSectorSelectionOn) {sprintf(mSector,"%2d",SelectedSector);}

  Float_t xMin = -250.;
  Float_t xMax = 250.;
  Float_t yMin = 0.;
  Float_t yMax = 0.00001;
  Int_t nYBins = 2000;


  // define the histograms for q versus x,y,z  
  for (i = 0; i < nChargeHist; i++) {

    mHistTitle = new TString("Q distribution versus ");
    mHistTitle->Append(mIndexName[i]);
    if (bSectorSelectionOn) {mHistTitle->Append(" Sector "); mHistTitle->Append(*mSector);}
    mHistName  = new TString("chargevs");
    mHistName->Append(mIndexName[i]);
    ChargeHists[i].mQdist =
      new TH2F(mHistName->Data(), mHistTitle->Data(), nXBins, xMin, xMax, nYBins, yMin, yMax);
    if (mIndexName[i] == "row") {ChargeHists[i].mQdist->SetBins(50, 0., 50.,2000, 0., 0.00001);}
    ChargeHists[i].mQdist->SetXTitle(mIndexName[i]);
    ChargeHists[i].mQdist->SetYTitle("Q");
    delete mHistTitle;
    delete mHistName;

    mHistTitle = new TString("Q profile versus ");
    mHistTitle->Append(mIndexName[i]);
    if (bSectorSelectionOn) {mHistTitle->Append(" Sector "); mHistTitle->Append(*mSector);}
    mHistName  = new TString("chargeprofvs");
    mHistName->Append(mIndexName[i]);
    ChargeHists[i].mQprof =
      new TProfile(mHistName->Data(), mHistTitle->Data(), nXBins, xMin, xMax, yMin, yMax);
    ChargeHists[i].mQprof->SetName(mHistTitle->Data());
    if (mIndexName[i] == "row") {ChargeHists[i].mQprof->SetBins(50, 0., 50.);}
    ChargeHists[i].mQprof->SetXTitle(mIndexName[i]);
    ChargeHists[i].mQprof->SetYTitle("Q profile");
    delete mHistTitle;
    delete mHistName;

    mHistTitle = new TString("Mean charge versus ");
    mHistTitle->Append(mIndexName[i]);
    if (bSectorSelectionOn) {mHistTitle->Append(" Sector "); mHistTitle->Append(*mSector);}
    mHistName  = new TString("meanchargevs");
    mHistName->Append(mIndexName[i]);
    ChargeHists[i].FitQHists.mQ_mean =
      new TH1D(mHistName->Data(), mHistTitle->Data(), nXBins, xMin, xMax);
    if (mIndexName[i] == "row") {ChargeHists[i].FitQHists.mQ_mean->SetBins(50, 0., 50.);}
    delete mHistTitle;
    delete mHistName;

    mHistTitle = new TString("Sigma charge versus ");
    mHistTitle->Append(mIndexName[i]);
    if (bSectorSelectionOn) {mHistTitle->Append(" Sector "); mHistTitle->Append(*mSector);}
    mHistName  = new TString("sigmachargevs");
    mHistName->Append(mIndexName[i]);
    ChargeHists[i].FitQHists.mQ_sigma =
      new TH1D(mHistName->Data(), mHistTitle->Data(), nXBins, xMin, xMax);
    if (mIndexName[i] == "row") {ChargeHists[i].FitQHists.mQ_sigma->SetBins(50, 0., 50.);}
    delete mHistTitle;
    delete mHistName;

    mHistTitle = new TString("Magnitude charge versus ");
    mHistTitle->Append(mIndexName[i]);
    if (bSectorSelectionOn) {mHistTitle->Append(" Sector "); mHistTitle->Append(*mSector);}
    mHistName  = new TString("magchargevs");
    mHistName->Append(mIndexName[i]);
    ChargeHists[i].FitQHists.mQ_mag =
      new TH1D(mHistName->Data(), mHistTitle->Data(), nXBins, xMin, xMax);
    if (mIndexName[i] == "row") {ChargeHists[i].FitQHists.mQ_mag->SetBins(50, 0., 50.);}
    delete mHistTitle;
    delete mHistName;

    mHistTitle = new TString("ChiSquared charge versus ");
    mHistTitle->Append(mIndexName[i]);
    if (bSectorSelectionOn) {mHistTitle->Append(" Sector "); mHistTitle->Append(*mSector);}
    mHistName  = new TString("chichargevs");
    mHistName->Append(mIndexName[i]);
    ChargeHists[i].FitQHists.mQ_chi =
      new TH1D(mHistName->Data(), mHistTitle->Data(), nXBins, xMin, xMax);
    if (mIndexName[i] == "row") {ChargeHists[i].FitQHists.mQ_chi->SetBins(50, 0., 50.);}
    delete mHistTitle;
    delete mHistName;
  }

  return kStOK;
}

//-----------------------------------------------------------------------

Int_t StQACosmicMaker::fillChargeHistograms() {
  
  int i;

  // get pointers to tpc hit table
  St_DataSetIter Itpc_hits(GetDataSet("tpc_hits"));
  St_tcl_tphit *phtcl = 0;
  tcl_tphit_st *pttphit = 0;
  phtcl = (St_tcl_tphit *) Itpc_hits.Find("tphit");
  if (phtcl) {pttphit = phtcl->GetTable();}
  else { cout << "Warning: tphit table header does not exist " << endl; return kStWarn; }
  if (!pttphit) { cout << "Warning: tphit table does not exist " << endl; return kStWarn; }
  
  // get pointers to tpc residuals table and track table
  St_DataSetIter Itpc_trk(GetDataSet("tpc_tracks"));
  St_tpt_track *phtrk = 0;
  tpt_track_st *pttrk = 0;
  phtrk = (St_tpt_track *) Itpc_trk.Find("tptrack");
  if (phtrk) {pttrk = phtrk->GetTable();}
  else { cout << "Warning: tptrack table header does not exist " << endl; return kStWarn; }
  if (!pttrk) { cout << "Warning: tptrack table does not exist " << endl; return kStWarn; }

  // create a sorter to get an index to a track
  int nrows = phtrk->GetNRows();
  if (nrows == 0) {cout << "Warning: residual table contains zero rows " << endl; return kStWarn;}
  TString colName = "id";
  St_TableSorter trksorter(*phtrk,colName,0,nrows-1);
  
  for(i=0; i<phtcl->GetNRows(); i++) {
    // track in row table is 1000*id + position on track
    Int_t irow_trk = trksorter[(Int_t)(pttphit[i].track/1000.)];
    Int_t isector = (Int_t)(pttphit[i].row/100.);
    Int_t irowsector = pttphit[i].row - 100 * isector;   
   
    // global cuts and only particles belonging to track
    if (pttphit[i].q != 0. && irow_trk >= 0) {
      // calculate total momentum of the track where the hit belongs to
      Float_t trkcalcp = sqrt((pttrk[irow_trk].tanl * pttrk[irow_trk].tanl + 1) /
			      (pttrk[irow_trk].invp * pttrk[irow_trk].invp));

      if (Debug()) {
	cout << "trk row nr in table: " << irow_trk << endl;
      }

      if (trkcalcp >= 0.3) {
	//  no specific sector selected 
	if (!bSectorSelectionOn) {
	  ChargeHists[0].mQdist->
	    Fill((Float_t)(pttphit[i].x),(Float_t)(pttphit[i].q) );
	  ChargeHists[1].mQdist->
	    Fill((Float_t)(pttphit[i].y),(Float_t)(pttphit[i].q) );
	  ChargeHists[2].mQdist->
	    Fill((Float_t)(pttphit[i].z),(Float_t)(pttphit[i].q) );
	  ChargeHists[3].mQdist->
	    Fill((Float_t)(irowsector),(Float_t)(pttphit[i].q) );
	  // profile histogram
	  ChargeHists[0].mQprof->
	    Fill((Float_t)(pttphit[i].x),(Float_t)(pttphit[i].q) );
	  ChargeHists[1].mQprof->
	    Fill((Float_t)(pttphit[i].y),(Float_t)(pttphit[i].q) );
	  ChargeHists[2].mQprof->
	    Fill((Float_t)(pttphit[i].z),(Float_t)(pttphit[i].q) );
	  ChargeHists[3].mQprof->
	    Fill((Float_t)(irowsector),(Float_t)(pttphit[i].q) );
	}
	// fill histograms only for selected sector
	else {
	  if (isector == SelectedSector) {
	    ChargeHists[0].mQdist->
	      Fill((Float_t)(pttphit[i].x),(Float_t)(pttphit[i].q) );
	    ChargeHists[1].mQdist->
	      Fill((Float_t)(pttphit[i].y),(Float_t)(pttphit[i].q) );
	    ChargeHists[2].mQdist->
	      Fill((Float_t)(pttphit[i].z),(Float_t)(pttphit[i].q) );
	    ChargeHists[3].mQdist->
	      Fill((Float_t)(irowsector),(Float_t)(pttphit[i].q) );
	    // profile histograms
	    ChargeHists[0].mQprof->
	      Fill((Float_t)(pttphit[i].x),(Float_t)(pttphit[i].q) );
	    ChargeHists[1].mQprof->
	      Fill((Float_t)(pttphit[i].y),(Float_t)(pttphit[i].q) );
	    ChargeHists[2].mQprof->
	      Fill((Float_t)(pttphit[i].z),(Float_t)(pttphit[i].q) );
	    ChargeHists[3].mQprof->
	      Fill((Float_t)(irowsector),(Float_t)(pttphit[i].q) );
	  }
	}
      }
    }
  }

  return kStOK;
}

//-----------------------------------------------------------------------

Int_t StQACosmicMaker::calcChargeHistograms() {

  int i;
  char *mIndexName[nChargeHist]={"x","y","z","row"};
  TString *mHistTitle;
  TString *mHistName;

  char mSector[3];
  if (bSectorSelectionOn) {sprintf(mSector, "%2d", SelectedSector);}

  for (i = 0; i < nChargeHist; i++) {

    mHistTitle = new TString("Q distribution versus ");
    mHistTitle->Append(mIndexName[i]);
    if (bSectorSelectionOn) {mHistTitle->Append(" Sector "); mHistTitle->Append(*mSector);}
    TF1 *mLandau = new TF1("Landau","landau",0.,1.);
    ChargeHists[i].mQdist->FitSlicesY(mLandau);
    ChargeHists[i].mQdist->SetXTitle(mIndexName[i]);
    ChargeHists[i].mQdist->SetName(mHistTitle->Data());
    ChargeHists[i].mQdist->SetTitle(mHistTitle->Data());
    delete mHistTitle;
    delete mLandau;
    
    mHistTitle = new TString("Magnitude Q distribution versus ");
    mHistTitle->Append(mIndexName[i]);
    if (bSectorSelectionOn) {mHistTitle->Append(" Sector "); mHistTitle->Append(*mSector);}
    mHistName  = new TString("chargevs");
    mHistName->Append(mIndexName[i]);
    mHistName->Append("_0");
    ((TH1D *) gDirectory->Get(mHistName->Data()))->Copy(*ChargeHists[i].FitQHists.mQ_mag);
    delete ((TH1D *) gDirectory->Get(mHistName->Data()));
    ChargeHists[i].FitQHists.mQ_mag->SetName(mHistTitle->Data());
    ChargeHists[i].FitQHists.mQ_mag->SetTitle(mHistTitle->Data());
    delete mHistName;
    delete mHistTitle;
    ChargeHists[i].FitQHists.mQ_mag->SetXTitle(mIndexName[i]);
    ChargeHists[i].FitQHists.mQ_mag->SetYTitle("Magnitude");

    mHistTitle = new TString("Mean Q distribution versus ");
    mHistTitle->Append(mIndexName[i]);
    if (bSectorSelectionOn) {mHistTitle->Append(" Sector "); mHistTitle->Append(*mSector);}
    mHistName  = new TString("chargevs");
    mHistName->Append(mIndexName[i]);
    mHistName->Append("_1");
    ((TH1D *) gDirectory->Get(mHistName->Data()))->Copy(*ChargeHists[i].FitQHists.mQ_mean);
    delete ((TH1D *) gDirectory->Get(mHistName->Data()));
    ChargeHists[i].FitQHists.mQ_mean->SetName(mHistTitle->Data());
    ChargeHists[i].FitQHists.mQ_mean->SetTitle(mHistTitle->Data());
    delete mHistName;
    delete mHistTitle;
    ChargeHists[i].FitQHists.mQ_mean->SetXTitle(mIndexName[i]);
    ChargeHists[i].FitQHists.mQ_mean->SetYTitle("Mean Q");
    ChargeHists[i].FitQHists.mQ_mean->GetXaxis()->SetLabelSize(0.04);
    ChargeHists[i].FitQHists.mQ_mean->GetYaxis()->SetLabelSize(0.04);
    ChargeHists[i].FitQHists.mQ_mean->SetMarkerColor(kBlue);
    ChargeHists[i].FitQHists.mQ_mean->SetMarkerStyle(20);

    mHistTitle = new TString("Sigma Q distribution versus ");
    mHistTitle->Append(mIndexName[i]);
    if (bSectorSelectionOn) {mHistTitle->Append(" Sector "); mHistTitle->Append(*mSector);}
    mHistName  = new TString("chargevs");
    mHistName->Append(mIndexName[i]);
    mHistName->Append("_2");
    ((TH1D *) gDirectory->Get(mHistName->Data()))->Copy(*ChargeHists[i].FitQHists.mQ_sigma);
    delete ((TH1D *) gDirectory->Get(mHistName->Data()));
    ChargeHists[i].FitQHists.mQ_sigma->SetName(mHistTitle->Data());
    ChargeHists[i].FitQHists.mQ_sigma->SetTitle(mHistTitle->Data());
    delete mHistName;
    delete mHistTitle;
    ChargeHists[i].FitQHists.mQ_sigma->SetXTitle(mIndexName[i]);
    ChargeHists[i].FitQHists.mQ_sigma->SetYTitle("Sigma Q");
    
    mHistTitle = new TString("Chi2 Q distribution versus ");
    mHistTitle->Append(mIndexName[i]);
    if (bSectorSelectionOn) {mHistTitle->Append(" Sector "); mHistTitle->Append(*mSector);}
    mHistName  = new TString("chargevs");
    mHistName->Append(mIndexName[i]);
    mHistName->Append("_chi2");
    ((TH1D *) gDirectory->Get(mHistName->Data()))->Copy(*ChargeHists[i].FitQHists.mQ_chi);
    delete ((TH1D *) gDirectory->Get(mHistName->Data()));
    ChargeHists[i].FitQHists.mQ_chi->SetName(mHistTitle->Data());
    ChargeHists[i].FitQHists.mQ_chi->SetTitle(mHistTitle->Data());
    delete mHistName;
    delete mHistTitle;
    ChargeHists[i].FitQHists.mQ_chi->SetXTitle(mIndexName[i]);
    ChargeHists[i].FitQHists.mQ_chi->SetYTitle("chi2 Q");
    
    for (int j=0; j<ChargeHists[i].FitQHists.mQ_sigma->fN; j++) { 
      ChargeHists[i].FitQHists.mQ_sigma->fArray[j] = 
    	fabs(ChargeHists[i].FitQHists.mQ_sigma->fArray[j]);
    }
  }

  return kStOK;
}

//-----------------------------------------------------------------------

Int_t StQACosmicMaker::writeOutHistograms() {

  TString *mHistFileName = new TString(MakerName.Data());
  mHistFileName->Append(".hists.root");

  TFile outHistFile(mHistFileName->Data(),"RECREATE"); 
  this->GetHistList()->Write();
  outHistFile.Close();

  delete mHistFileName;

  return kStOK;
}

//-----------------------------------------------------------------------

Int_t StQACosmicMaker::writeOutTNtuple() {


  TString *mTNtupleFileName = new TString(MakerName.Data());
  mTNtupleFileName->Append(".ntuple.root");

  TFile outTNtupleFile(mTNtupleFileName->Data(),"RECREATE"); 
  mTNtupleTPC->Write();
  outTNtupleFile.Close();

  delete mTNtupleFileName;
  
  return kStOK;
}

//-----------------------------------------------------------------------

Int_t StQACosmicMaker::writeOutPostscript() {
  
  return kStOK;
}

//-----------------------------------------------------------------------

