// $Id: StdEdxMaker.cxx,v 1.10 2001/03/23 20:00:28 fisyak Exp $
#include <iostream.h>
#include "StdEdxMaker.h"
// ROOT
#include "TMinuit.h"
#include "TSystem.h"
#include "TMath.h"
#include "TH2.h"
#include "TH3.h"
#include "TStyle.h"
#include "TF1.h"
#include "TProfile.h"
#include "TTree.h"
#include "TChain.h"
#include "TFile.h"
#include "TNtuple.h"
#include "TCanvas.h"
// StUtilities
#include "StMessMgr.h" 
#include "BetheBloch.h"
// St_base, StChain
#include "StBFChain.h"
#include "St_DataSetIter.h"
#include "TTableSorter.h"
// tables
#include "tables/St_tpt_track_Table.h"
#include "tables/St_stk_track_Table.h"
#include "tables/St_dst_dedx_Table.h"
#include "tables/St_dst_track_Table.h"
#include "tables/St_tpg_pad_plane_Table.h"
#include "tables/St_tpcGain_Table.h"
#include "tables/St_TpcSecRowCor_Table.h"
#include "tables/St_TpcTimeGain_Table.h"
#include "tables/St_TpcDriftDistCorr_Table.h"
#include "tables/St_fee_vs_pad_row_Table.h"
#include "tables/St_tpcBadPad_Table.h"
// global
#include "StDetectorId.h"
#include "StDedxMethod.h"
// StarClassLibrary
#include "StThreeVector.hh" 
#include "StHelixD.hh"
// Chairs
#include "StDstPointChair.h"
#include "StTrackChair.h"
// StDb
#include "StDbUtilities/StTpcCoordinateTransform.hh"
#include "StDbUtilities/StTpcLocalSectorCoordinate.hh"
#include "StDbUtilities/StTpcLocalCoordinate.hh"
#include "StDbUtilities/StTpcPadCoordinate.hh"
#include "StDbUtilities/StGlobalCoordinate.hh"
#include "StTpcDb/StTpcDb.h"
#include "StTpcDb/StTpcPadPlaneI.h"
const Int_t NRows =  45;
const Int_t NPads = 144;
BetheBloch BB;
const Int_t NHYPS = 5;
const Char_t *Names[] = {"p",
			 "K",
			 "pi",
			 "e",
			 "d"};
const Double_t Masses[] = {0.93827231,
			   0.493677,
			   0.13956995,
			   0.51099907e-3,
			   1.87561339};


struct dEdx_t {
  Int_t sector;
  Int_t row;
  Int_t pad;
  Int_t Fee;
  Double_t dE;
  Double_t dEU; // before correction
  Double_t dx;
  Double_t dEdx; 
  Double_t dEdxU; 
  Double_t dEdxP; // after pulser correction only
  Double_t dEdxL; // log of dEdx
  Double_t dEdxLU; // log of dEdx
  Double_t dEdxN; // normolized to BB
  Double_t dEdxNP; // normolized to BB
  Double_t xyz[3];
  Double_t Prob; 
  Double_t SigmaFee;
  Double_t xscale;
  Double_t zdev; 
};
enum EFitCase {kVal, kGrad};
enum ESector  {kTpcInner, kTpcOuter};
dEdx_t CdEdx[60]; // corrected
dEdx_t FdEdx[60]; // fit
Int_t NdEdx = 0, NPoints = 0;
Double_t dEdxS[60]; // dEdx sorted
Double_t dE, dEP, dx, dEdx, Sigma_dEdx;

TF1 *fFunc = 0;
TCanvas *fCanvas = 0;
TH1F    *h1 = 0;
TMinuit *gMinuit = new TMinuit(2);
// correction for signal width versus Half Fee no.
Double_t FeeSigma[2][3] = {
     {  3.57800e-01, -9.97264e-05,  4.85553e-07},
     {  1.11623e+00, -5.02868e-03,  9.04828e-06}
};
extern    void fcn(Int_t &npar, Double_t *gin, Double_t &f, Double_t *u, Int_t flag);
extern    void fGaus(Double_t x, Double_t *val, ESector l = kTpcInner);
extern    void MDFcorrOw(Double_t *x, Double_t *y);
// Histograms
TH3F *Z = 0, *ZC = 0, *ETA = 0, *SecRow = 0, *TPoints = 0, *Fee = 0, *FeeC = 0;// *sXY = 0, *SXY = 0;
TH2F *Time = 0, *Points70 = 0, *Points60 = 0, *Points70B = 0, *Points60B = 0, *SR = 0, *PointsFit = 0;
TH2F *FShapeI = 0, *FShapeO = 0, *FShapeIC = 0, *FShapeOC = 0;
TH2F *corrI = 0, *corrO = 0, *corrI2 = 0, *corrO2 = 0, *corrI5 = 0, *corrO5 = 0;
TH2D *hist70[NHYPS][2], *hist60[NHYPS][2], *histz[NHYPS][2];
TH2F *FitPull = 0;
TH2F *corrIw = 0, *corrOw = 0, *corrI2w = 0, *corrO2w = 0, *corrI5w = 0, *corrO5w = 0;
TH1F *corrI1w = 0, *corrO1w = 0;
TProfile *histB[NHYPS][2], *histBB[NHYPS][2]; 
ClassImp(StdEdxMaker)  

//_____________________________________________________________________________
StdEdxMaker::StdEdxMaker(const char *name):StMaker(name), m_tpcGain(0), 
m_TpcSecRow(0),m_fee_vs_pad_row(0), m_tpcTime(0), m_drift(0), m_badpads(0) {}
//_____________________________________________________________________________
StdEdxMaker::~StdEdxMaker(){}
//_____________________________________________________________________________
Int_t StdEdxMaker::Init(){
// 		TPG parameters
      // Normal to Sector
  Int_t NoSector = gStTpcDb->Dimensions()->numberOfSectors();
  Int_t NoRow    = 45;
  for (Int_t sector=1; sector<=NoSector; sector++) {
    Double_t beta = (sector > 12) ?
      (NoSector-sector)*2.*M_PI/(static_cast<double>(NoSector)/2.): 
      sector*2.*M_PI/(static_cast<double>(NoSector)/2.);
    mNormal[sector-1] = new StThreeVectorD(sin(beta), cos(beta), 0.);
  }
  TDataSet *tpc_calib  = GetDataBase("Calibrations/tpc"); assert(tpc_calib);
  m_tpcTime = (St_TpcTimeGain *) tpc_calib->Find("TpcTimeGain"); 
  TDataSet *tpc_daq  = GetDataBase("tpc/daq"); assert(tpc_daq);
  m_fee_vs_pad_row = (St_fee_vs_pad_row *) tpc_daq->Find("fee_vs_pad_row");
  if (!m_tpcTime) {
    cout << "TpcTimeGain is missing <=========== switch off time dependent calibration" << endl;
    assert(m_tpcTime); 
  }
  m_drift = (St_TpcDriftDistCorr *) tpc_calib->Find("TpcDriftDistCorr"); 
  if (!m_drift) {
    cout << "TpcDriftDistCorr is missing <=========== switch off dirft dependent calibration" << endl;
    assert(m_drift); 
  }
  m_badpads = (St_tpcBadPad *) tpc_calib->Find("BadPad");
  if (!m_badpads) cout << "=== List of bad pads is missing ===" << endl;
  m_TpcSecRow = (St_TpcSecRowCor *) tpc_calib->Find("TpcSecRow"); assert(m_TpcSecRow); 
  if (m_Mode > 0) {// calibration mode
    StMaker *tpcdaq = GetMaker("tpc_raw");
    if (!tpcdaq) {
      m_tpcGain = (St_tpcGain *) tpc_calib->Find("tpcGain"); assert(m_tpcGain); 
    }
    TFile *f = (TFile *) ((StBFChain *)GetChain())->GetTFile();
    if (f) {
      f->cd();
      Z   = new TH3F("Z","log(dEdx/Pion) versus sector(inner <= 24 and outter > 24)  and Z",
		     2*NoSector,1,2*NoSector+1,100,0.,200., 200,-5.,5.);
      ZC  = new TH3F("ZC","dEdxN versus sector(inner <= 24 and outter > 24)  and Z",
		     2*NoSector,1,2*NoSector+1,100,0.,200., 200,-5.,5.);
      ETA   = new TH3F("ETA","log(dEdx/Pion) versus sector(inner <= 24 and outter > 24)  and #{eta}",
		       2*NoSector,1,2*NoSector+1, 125,-.25,2.25, 200,-5.,5.);
      SR = new TH2F("SR","sector and row from points (input)",
		       NoSector,1,NoSector+1, NoRow,1.,NoRow+1);
      SecRow = new TH3F("SecRow","log(dEdx/Pion) versus sector  and row",
		       NoSector,1,NoSector+1, NoRow,1.,NoRow+1, 200,-5.,5.);
      Int_t noFee = 182;
      Fee    = new TH3F("Fee","dEdx versus sector and Fee after pulser correction only",
			NoSector,1,NoSector+1,  2*noFee,0,2*noFee, 200,-5.,5.);
      FeeC   = new TH3F("FeeC","dEdx versus sector and Fee",
			NoSector,1,NoSector+1,  2*noFee,0,2*noFee, 200,-5.,5.);
      corrI   = new TH2F("corrI","Correlation for Inner Sector for pair of nearest rows",
			100,-10.,10., 100,-10.,10.);
      corrO   = new TH2F("corrO","Correlation for Outer Sector for pair of nearest rows",
			100,-10.,10., 100,-10.,10.);
      corrI2   = new TH2F("corrI2","Correlation for Inner Sector for pair rows & row + 2",
			100,-10.,10., 100,-10.,10.);
      corrO2   = new TH2F("corrO2","Correlation for Outer Sector for pair rows & row + 2",
			100,-10.,10., 100,-10.,10.);
      corrI5   = new TH2F("corrI5","Correlation for Inner Sector for pair rows & row + 5",
			100,-10.,10., 100,-10.,10.);
      corrO5   = new TH2F("corrO5","Correlation for Outer Sector for pair rows & row + 5",
			100,-10.,10., 100,-10.,10.);
      corrIw   = new TH2F("corrIw","Weighted correlation for Inner Sector for pair of nearest rows",
			100,-10.,10., 100,-10.,10.);
      corrOw   = new TH2F("corrOw","Weighted correlation for Outer Sector for pair of nearest rows",
			100,-10.,10., 100,-10.,10.);
      corrI1w   = new TH1F("corrI1w","Weighted distribution for Inner Sector",100,-10.,10.);
      corrO1w   = new TH1F("corrO1w","Weighted distribution for Outer Sector",100,-10.,10.);
      corrI2w   = new TH2F("corrI2w","Weighted correlation for Inner Sector for pair rows & row + 2",
			100,-10.,10., 100,-10.,10.);
      corrO2w   = new TH2F("corrO2w","Weighted correlation for Outer Sector for pair rows & row + 2",
			100,-10.,10., 100,-10.,10.);
      corrI5w   = new TH2F("corrI5w","Weighted correlation for Inner Sector for pair rows & row + 5",
			100,-10.,10., 100,-10.,10.);
      corrO5w   = new TH2F("corrO5w","Weighted correlation for Outer Sector for pair rows & row + 5",
			100,-10.,10., 100,-10.,10.);
      Points70 = new TH2F("Points70","dEdx(I70) versus no. of measured points",50,0,50.,200,-1.,1.);
      Points60 = new TH2F("Points60","dEdx(I60) versus no. of measured points",50,0,50.,200,-1.,1.);
      Points70B = new TH2F("Points70B","dEdx(I70) versus no. of measured points BB",50,0,50.,200,-1.,1.);
      Points60B = new TH2F("Points60B","dEdx(I60) versus no. of measured points BB",50,0,50.,200,-1.,1.);
      PointsFit  = new TH2F("PointsFit","dEdx(fit) versus no. of measured points",
			    50,0,50., 200,-1.,1.);
      TPoints  = new TH3F("TPoints","dEdx(fit) versus no. of measured points and length", 
			  50,0,50., 150,10.,160., 200,-1.,1.);
      FShapeI  = new TH2F("FShapeI","(log(dEdx)-<z_{fit}>)*(dx)**0.36 versus log(dx) for Inner Sector", 
			  26,0.1, 1.4, 800,-15.,25.);
      FShapeO  = new TH2F("FShapeO","(log(dEdx)-<z_{fit}>)*(dx)**0.36 versus log(dx) for Outer Sector", 
			  36,0.6, 2.4, 800,-15.,25.);
      for (int hyp=0; hyp<NHYPS;hyp++) {
	for (int sCharge = 0; sCharge < 2; sCharge++) {
	  TString nameP = Names[hyp];
	  if (sCharge == 0) nameP += "P";
	  else        nameP += "N";
	  TString name = nameP;
	  name += "70";
	  TString title = "log(dE/dx70/I(";
	  title += Names[hyp];
	  title += ")) versus log10(p/m)";
	  hist70[hyp][sCharge] = new TH2D(name.Data(),title.Data(),100,-1.,4.,200,-1.,1.);
	  name = nameP;
	  name += "60";
	  title = "log(dE/dx60/I(";
	  title += nameP;
	  title += ")) versus log10(p/m)";
	  hist60[hyp][sCharge] = new TH2D(name.Data(),title.Data(),100,-1.,4.,200,-1.,1.);
	  name = nameP;
	  name += "z";
	  title = "zFit - log(I(";
	  title += nameP;
	  title += ")) versus log10(p/m)";
	  histz[hyp][sCharge] = new TH2D(name.Data(),title.Data(),100,-1.,4.,200,-1.,1.);
	  name = nameP;
	  name += "B";
	  title = "log(I_{Sirrf}(";
	  title += nameP;
	  title += ")) versus log10(p/m)";
	  histB[hyp][sCharge] = new TProfile(name.Data(),title.Data(),100,-1.,4.);
	  name += "B";
	  title = "log(I_{BB}(";
	  title += nameP;
	  title += ")) versus log10(p/m)";
	  histBB[hyp][sCharge] = new TProfile(name.Data(),title.Data(),100,-1.,4.);
	}
      }
      Time   = new TH2F("Time","zFit - log(I(pi)) versus Date& Time", 280,0.,70., 200,-1.,1.);
      FitPull= new TH2F("FitPull","(zFit - log(I(pi)))/dzFit  versus track length", 
			150,10.,160, 200,-5.,5.);
    }
  }
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StdEdxMaker::Make(){ 
  Double_t Scale2keV = 1.7428;
  Double_t Scale70   = 1.2315;
  Double_t Scale60   = 1.3146;
  St_DataSet *Dst = GetDataSet("dst"); assert(Dst);
  St_DataSet *dst = Dst->Find(".data/dst");
  if (!dst) dst = Dst;
  St_dst_dedx *dst_dedx     = (St_dst_dedx *) dst->Find("dst_dedx");
  if (m_tpcGain) {SafeDelete(dst_dedx);}
  if (!dst_dedx) {
    dst_dedx = new St_dst_dedx("dst_dedx",20000); dst->Add(dst_dedx);
  }
  else dst_dedx->ReAllocate(20000);
  St_dst_track     *globtrk     = (St_dst_track *)  dst->Find("globtrk");
  if (! globtrk) { 
    gMessMgr->Error() << "StdEdxMaker::Make(): global track table is not found " << endm; 
    return kStOK;
  }
  St_dst_track     *primtrk     = (St_dst_track *)  dst->Find("primtrk");
  St_stk_track     *stk_track   = (St_stk_track *) GetDataSet("stk_track");
  if (stk_track) {//Silicon if any
    Int_t NoSvtTracks = stk_track->GetNRows();
    stk_track_st *stktrack = stk_track->GetTable();
    dst_dedx_st dedx;
    for (Int_t i=0; i<NoSvtTracks; i++, stktrack) {
      dedx.id_track  =  stktrack->id_globtrk;
      dedx.det_id    =  kSvtId;    // SVT track
      dedx.method    =  0;
      dedx.ndedx     =  0;
      dedx.dedx[0]   =  stktrack->dedx[0];
      dedx.dedx[1]   =  stktrack->dedx[1];
      dst_dedx->AddAt(&dedx);
    }
  }
  St_dst_point     *point      = (St_dst_point *)  dst->Find("point");
  if (! point) {gMessMgr->Error() << "StdEdxMaker::Make(): dst points is not found " << endm; return kStOK;}
  TString          id_track("id_track");
  TString          id("id");
  TTableSorter     *pointS     = new TTableSorter(point,id_track);
  TTableSorter     *globtrkS   = new TTableSorter(globtrk,id);
  StDstPointChair  *pointC     = new StDstPointChair(point);
  StTrackChair     *globtrkC   = new StTrackChair(globtrk);
  
  TTableSorter     *primtrkS   = 0;
  StTrackChair     *primtrkC   = 0;
  if (primtrk){
		    primtrkS   = new TTableSorter(primtrk,id);
                    primtrkC   = new StTrackChair(primtrk);
  }
  Double_t TimeScale = 1;
  if (m_tpcTime) {TimeScale = (*m_tpcTime)[0].ScaleFactor;}
  StHelixD         *helix      = 0;
  StTpcCoordinateTransform transform(gStTpcDb);
  Int_t iprim, jprim, iglob, ipoint; 
  Int_t nPrimaryTracks = 0;
  Double_t pTinv = 0;
  Double_t p = 0;
  Double_t Eta = 0;
  Int_t NoFitPoints = 0;
  dst_track_st *primTrk = 0, *globTrk = 0, *primTRK = 0;
  if (primtrk) {
    primTrk = primtrk->GetTable();
    for (iprim = 0; iprim < primtrk->GetNRows(); iprim++, primTrk++) 
      if (primTrk->iflag > 0) nPrimaryTracks++;
  }
  Int_t kglob = 0, kprim = 0, kpoint = 0; 
  for (; kglob < globtrkS->GetNRows();kglob++) {
    iglob = globtrkS->GetIndex(kglob); assert (iglob >= 0);
    globTrk = globtrk->GetTable() + iglob;
    primTrk = 0;
    iprim = -1;
    if (globTrk->iflag < 0) continue;
    Int_t Id = globTrk->id;
    if (primtrkS && kprim < primtrkS->GetNRows()) {
      for (; kprim < primtrkS->GetNRows(); kprim++) {
	jprim = primtrkS->GetIndex(kprim); assert(jprim >=0);
	primTRK = primtrk->GetTable() + jprim;
	if (primTRK->iflag < 0) continue;
	if (primTRK->id < Id) continue;
	if (primTRK->id > Id) break;
	iprim = jprim;
	primTrk = primTRK;
	helix  = primtrkC->MakeHelix(iprim);
	NoFitPoints = primTrk->n_fit_point;
	kprim++;
	if (m_Mode > 0) {
	  Double_t tanl  = primTrk->tanl;
	  pTinv  = primtrkC->Invpt(iprim);
	  p = 1.e6;
	  if (pTinv > 1.e-6) p = 1./pTinv*TMath::Sqrt(1. + tanl*tanl);
	  Double_t Theta = TMath::ATan(tanl);
	  Eta = - TMath::Log(TMath::Tan(Theta/2.));
	}
	break;
      }
    }
    if (!helix) {
      helix = globtrkC->MakeHelix(iglob);
      NoFitPoints = globTrk->n_fit_point;
      if (m_Mode > 0) {
	Double_t tanl  = globTrk->tanl;
	pTinv  = globtrkC->Invpt(iglob);
	p = 1.e6;
	if (pTinv > 1.e-6) p = 1./pTinv*TMath::Sqrt(1. + tanl*tanl);
	Double_t Theta = TMath::ATan(tanl);
	Eta = - TMath::Log(TMath::Tan(Theta/2.));
      }
    }
    dst_dedx_st dedx;
    NPoints = 0;
    Int_t NFitPoints=0;
    NdEdx = 0;
    Double_t TrackLength = 0;
    Double_t avrgZ = 0;
    for (;kpoint < pointS->GetNRows();kpoint++) {
      ipoint = pointS->GetIndex(kpoint); assert (ipoint >= 0);
      if (pointC->TrackId(ipoint) < Id) continue;
      if (pointC->TrackId(ipoint) > Id) break;
      NPoints++;
      if (!pointC->GetFitFlag(ipoint)) continue;
      NFitPoints++;
      if (pointC->GetFlag(ipoint)) continue;
      if (pointC->DetectorId(ipoint) != kTpcId) continue;
      Int_t sector = pointC->Sector(ipoint);
      Int_t row = pointC->PadRow(ipoint);
      Double_t pointX = pointC->GetX(ipoint);
      Double_t pointY = pointC->GetY(ipoint);
      Double_t pointZ = pointC->GetZ(ipoint);
      
      StGlobalCoordinate global(pointX,pointY,pointZ);
      StTpcPadCoordinate Pad;      transform(global,Pad);
#ifdef DEBUG
      if (sector != Pad.sector() || 
	  row != Pad.row()) {
	gMessMgr->Warning() << "StdEdxMaker::" <<
	  " Sector = " << PadOnPlane.sector() << "/"  << "/" << Pad.sector() << "/" << sector <<
	  " Row = " << PadOnPlane.row() << "/" << Pad.row() << "/" << row << 
	  " Pad = " << PadOnPlane.pad() << "/" << Pad.pad() <<
	  " from Helix  is not matched with point/" << endm;;
	gMessMgr->Warning() << "StdEdxMaker:: Coordinates " << 
	  " x: " << xyzOnPlane.x() << "/" << pointX <<
	  " y: " << xyzOnPlane.y() << "/" << pointY <<
	  " z: " << xyzOnPlane.z() << "/" << pointZ << endm;
	continue;
      }
      sector = Pad.sector();
      row = Pad.row();
#endif
      //      if (row == 13) continue; // Skip row 13
      StThreeVectorD &normal = *mNormal[sector-1];
      //Get the position of the row center, transform to local sector coordinates
      StTpcPadCoordinate padCoord(sector, row, 1, 1);
      StTpcLocalSectorCoordinate lsMidCoord; transform(padCoord, lsMidCoord);
      StGlobalCoordinate          gMidCoord; transform(lsMidCoord, gMidCoord);
      const StThreeVectorD  gMidPos = StThreeVectorD(gMidCoord.position().x(),
						     gMidCoord.position().y(),
						     gMidCoord.position().z());
      // check that helix prediction is consistent with measurement
      Double_t s = helix->pathLength(gMidPos, normal);
      StThreeVectorD xyzOnPlane = helix->at(s);
      StGlobalCoordinate globalOnPlane(xyzOnPlane.x(),xyzOnPlane.y(),xyzOnPlane.z());
      StTpcPadCoordinate PadOnPlane;      transform(globalOnPlane,PadOnPlane);
      if (sector != PadOnPlane.sector() || 
	  row != PadOnPlane.row() ||
	  TMath::Abs(Pad.pad()-PadOnPlane.pad()) > 5) {
	if (iprim >= 0  && Debug() > 1) {
	  gMessMgr->Warning() << "StdEdxMaker::" <<
	    " Sector = " << PadOnPlane.sector() << "/"  << "/" << Pad.sector() << 
	    "/" << sector <<
	    " Row = " << PadOnPlane.row() << "/" << Pad.row() << "/" << row << 
	    " Pad = " << PadOnPlane.pad() << "/" << Pad.pad() <<
	    " from Helix  is not matched with point/" << endm;;
	  gMessMgr->Warning() << "StdEdxMaker:: Coordinates " << 
	    " x: " << xyzOnPlane.x() << "/" << pointX <<
	    " y: " << xyzOnPlane.y() << "/" << pointY <<
	    " z: " << xyzOnPlane.z() << "/" << pointZ << endm;
	}
	continue;
      }
      Double_t padlength;
      if (row<14) padlength = gStTpcDb->PadPlaneGeometry()->innerSectorPadLength();
      else 	     padlength = gStTpcDb->PadPlaneGeometry()->outerSectorPadLength();
      //Boost the local y value by +- padlength / 2.
      const StThreeVectorD  lsPos = StThreeVectorD(lsMidCoord.position().x(),
						   lsMidCoord.position().y(),
						   lsMidCoord.position().z());
      StTpcLocalSectorCoordinate lsTopCoord(lsPos.x(),
					    lsPos.y()+padlength/2.,
					    lsPos.z(),
					    sector);
      StTpcLocalSectorCoordinate lsBotCoord(lsPos.x(),
					    lsPos.y()-padlength/2.,
					    lsPos.z(),
					    sector);
      //Transform back to global coordinates
      StGlobalCoordinate gTopCoord; transform(lsTopCoord, gTopCoord);
      StGlobalCoordinate gBotCoord; transform(lsBotCoord, gBotCoord);
      const StThreeVectorD  gTopPos = StThreeVectorD(gTopCoord.position().x(),
						     gTopCoord.position().y(),
						     gTopCoord.position().z());
      const StThreeVectorD  gBotPos = StThreeVectorD(gBotCoord.position().x(),
						     gBotCoord.position().y(),
						     gBotCoord.position().z());
      double s_out = helix->pathLength(gTopPos, normal);
      double s_in  = helix->pathLength(gBotPos, normal);
      dx = TMath::Abs(s_out-s_in);
      if (dx < 0.5 || dx > 25.) continue;
      dE = pointC->GetCharge(ipoint)*Scale2keV;
      // Correctionsd
      dE *= TimeScale;
      Int_t pad = Pad.pad();
      dEP = dE;
      double gc = 1;
      Int_t fee = -1;
      CdEdx[NdEdx].SigmaFee = 1;
      if (m_TpcSecRow) {
	TpcSecRowCor_st *gain = m_TpcSecRow->GetTable() + sector - 1;
	gc =  gain->GainScale[row-1];
	CdEdx[NdEdx].SigmaFee = gain->GainRms[row-1];
      }
      if (gc < 0.0) continue;
      dE *= gc;
      Int_t SectN = sector; // drift distance
      if (row < 14) SectN += 24;
      Double_t z = pointZ;
      if (sector > 12) z = - z;
      if (m_drift) {
	TpcDriftDistCorr_st *cor = m_drift->GetTable();
	if (row <= 13) cor++;
	Double_t DriftCorr = 8.26477e-02 + 
	  cor->a[0]+z*(cor->a[1]+z*cor->a[2]);
	dE *= TMath::Exp(-DriftCorr);
      }
      TrackLength += dx;
      CdEdx[NdEdx].sector = sector;
      CdEdx[NdEdx].row = row;
      CdEdx[NdEdx].pad = pad;
      CdEdx[NdEdx].dx = dx;
      CdEdx[NdEdx].dEU= pointC->GetCharge(ipoint);;
      CdEdx[NdEdx].dE = dE; // corrected
      CdEdx[NdEdx].dEdxU = CdEdx[NdEdx].dEU/CdEdx[NdEdx].dx;
      CdEdx[NdEdx].dEdx  = CdEdx[NdEdx].dE/CdEdx[NdEdx].dx;
      CdEdx[NdEdx].dEdxP = dEP/CdEdx[NdEdx].dx;
      CdEdx[NdEdx].dEdxLU= TMath::Log(CdEdx[NdEdx].dEdxU);
      CdEdx[NdEdx].dEdxL = TMath::Log(CdEdx[NdEdx].dEdx);
      CdEdx[NdEdx].xyz[0] = pointX;
      CdEdx[NdEdx].xyz[1] = pointY;
      CdEdx[NdEdx].xyz[2] = pointZ;
      CdEdx[NdEdx].Fee    = -1;
      avrgZ += CdEdx[NdEdx].dEdxL;
      NdEdx++;
    }
    if (!NdEdx) continue;
    avrgZ /=NdEdx;
    SortdEdx(dEdxS,NdEdx);
    Double_t I70 = 0, I60 = 0, D70 = 0, D60 = 0;
    Int_t N70 = NdEdx - (int) (0.3*NdEdx + 0.5); 
    Int_t N60 = NdEdx - (int) (0.4*NdEdx + 0.5);
    Int_t k;
    for (k = 0; k < N70; k++) {
      I70 += dEdxS[k];
      D70 += dEdxS[k]*dEdxS[k];
      if (k < N60) {
	I60 += dEdxS[k];
	D60 += dEdxS[k]*dEdxS[k];
      }
    }
    if (N70 > 0) {
      I70 /= N70; D70 /= N70;
      D70  = sqrt(D70 - I70*I70);
      I70 *= Scale70;
      D70 *= Scale70;
      dedx.id_track  =  Id;
      dedx.det_id    =  kTpcId;    // TPC track 
      dedx.method    =  kTruncatedMeanIdentifier;
      dedx.ndedx     =  N70;
      dedx.dedx[0]   =  I70;
      dedx.dedx[1]   =  D70;
      dst_dedx->AddAt(&dedx);
    }
    if (N60 > 0) {
      I60 /= N60; D60 /= N60;
      D60  = sqrt(D60 - I60*I60);
      I60 *= Scale60;
      D60 *= Scale60;
      dedx.id_track  =  Id;
      dedx.det_id    =  kTpcId;    // TPC track 
      dedx.method    =  kOtherMethodIdentifier;
      dedx.ndedx     =  N60;
      dedx.dedx[0]   =  I60;
      dedx.dedx[1]   =  D60;
      dst_dedx->AddAt(&dedx);
    }
    Double_t chisq, fitZ, fitdZ;
    DoFitZ(chisq, fitZ, fitdZ);
    fitdZ *= 1.32920/(1.01405e+00 -8.12759e-04*TrackLength); // scale errors
    if (chisq > 0 && chisq < 10000.0) {
      dedx.id_track  =  Id;
      dedx.det_id    =  kTpcId;    // TPC track 
      dedx.method    =  kLikelihoodFitIdentifier;
      dedx.ndedx     =  NdEdx + 100*((int) TrackLength);
      dedx.dedx[0]   =  TMath::Exp(fitZ);
      if (fitdZ >= 1.) fitdZ = 0.999;
      dedx.dedx[1]   =  fitdZ; 
      dst_dedx->AddAt(&dedx);
    }
    if (primtrkC && iprim >= 0&& m_Mode > 0) {
      Double_t Pred[NHYPS], PredBB[NHYPS];
      Int_t sCharge = 0;
      if (primtrkC->Charge(iprim) < 0) sCharge = 1;
      for (int l = 0; l < NHYPS; l++) {
	Pred[l] = 1.e-6*BetheBloch::Sirrf(p/Masses[l],TrackLength,l==3); 
	PredBB[l] = BB(p/Masses[l]);
      }
      for (k = 0; k < NdEdx; k++) {
	FdEdx[k].dEdxN = TMath::Log(FdEdx[k].dEdx/Pred[2]);
	FdEdx[k].dEdxNP = TMath::Log(FdEdx[k].dEdxP/Pred[2]);
	if (SR) SR->Fill(FdEdx[k].sector,FdEdx[k].row);
	if (chisq > 0 && chisq < 10000.0) {
	  Double_t zk  = FdEdx[k].zdev;
	  if (FdEdx[k].Prob > 1.e-6) {
	    if (FdEdx[k].row > 13) corrO1w->Fill(zk,1./FdEdx[k].Prob);
	    else                   corrI1w->Fill(zk,1./FdEdx[k].Prob);
	  }
	  for (int l = 0; l < NdEdx; l++){
	    if (k == l) continue;
	    Double_t zl  = FdEdx[l].zdev;
	    if (FdEdx[l].row%2 == 1 && FdEdx[l].row - FdEdx[k].row  == 1) {
	      if (FdEdx[k].row > 13) {
		corrO->Fill(zk,zl); 
		if (FdEdx[k].Prob*FdEdx[l].Prob > 1.e-6) 
		  corrOw->Fill(zk,zl,1./(FdEdx[k].Prob*FdEdx[l].Prob));
	      }
	      else {
		corrI->Fill(zk,zl); 
		if (FdEdx[k].Prob*FdEdx[l].Prob > 1.e-6) 
		  corrIw->Fill(zk,zl,1./(FdEdx[k].Prob*FdEdx[l].Prob));
	      }
	    }
	    if (FdEdx[l].row%2 == 1 && FdEdx[l].row - FdEdx[k].row  == 2) {
	      if (FdEdx[k].row > 13) {
		corrO2->Fill(zk,zl); 
		if (FdEdx[k].Prob*FdEdx[l].Prob > 1.e-6) 
		  corrO2w->Fill(zk,zl,1./(FdEdx[k].Prob*FdEdx[l].Prob));
	      }
	      else {
		corrI2->Fill(zk,zl); 
		if (FdEdx[k].Prob*FdEdx[l].Prob > 1.e-6) 
		  corrI2w->Fill(zk,zl,1./(FdEdx[k].Prob*FdEdx[l].Prob));
	      }
	    }
	    if (FdEdx[l].row%2 == 1 && FdEdx[l].row - FdEdx[k].row  == 5) {
	      if (FdEdx[k].row > 13) {
		corrO5->Fill(zk,zl); 
		if (FdEdx[k].Prob*FdEdx[l].Prob > 1.e-6) 
		  corrO5w->Fill(zk,zl,1./(FdEdx[k].Prob*FdEdx[l].Prob));
	      }
	      else {
		corrI5->Fill(zk,zl); 
		if (FdEdx[k].Prob*FdEdx[l].Prob > 1.e-6) 
		  corrI5w->Fill(zk,zl,1./(FdEdx[k].Prob*FdEdx[l].Prob));
	      }
	    }
	  }
	}
      //      if (sXY) sXY->Fill(FdEdx[k].x[0],FdEdx[k].x[0],FdEdx[k].sector);
	if (Z && NoFitPoints >= 30) {
	  Double_t eta =  Eta;
	  Int_t SectN = FdEdx[k].sector; // drift distance
	  Double_t z = FdEdx[k].xyz[2];
	  if (SectN > 12) z = -z;
	  if (FdEdx[k].row < 14) SectN += 24;
	  Z->Fill(SectN,z,FdEdx[k].dEdxN);
	  ZC->Fill(SectN,z,FdEdx[k].zdev);
	  if (FdEdx[k].sector > 12) eta = -eta;
	  if (ETA) ETA->Fill(SectN,eta,FdEdx[k].dEdxN);
	  if (SecRow) SecRow->Fill(FdEdx[k].sector+0.5,FdEdx[k].row+0.5,FdEdx[k].dEdxNP);
	  if (Fee && FdEdx[k].Fee >= 0) {
	    Double_t fee1 =  FdEdx[k].Fee + 0.5;
	    Double_t Sect = FdEdx[k].sector + 0.5;
	    Double_t z = FdEdx[k].dEdxP;
	    Fee->Fill(Sect,fee1,z);
	    if (FeeC && FdEdx[k].SigmaFee > 0.) FeeC->Fill(Sect,fee1,FdEdx[k].dEdxN/FdEdx[k].SigmaFee);
	  }
	}
      }
      Points70->Fill(NdEdx,TMath::Log(I70/Pred[2]));
      Points60->Fill(NdEdx,TMath::Log(I60/Pred[2]));
      Points70B->Fill(NdEdx,TMath::Log(I70/PredBB[2]));
      Points60B->Fill(NdEdx,TMath::Log(I60/PredBB[2]));
      PointsFit->Fill(NdEdx,fitZ-TMath::Log(Pred[2]));
      TPoints->Fill(NdEdx,TrackLength,fitZ-TMath::Log(Pred[2]));
      if (NdEdx > 30) {
	for (k = 0; k < NdEdx; k++) {
	  if (FdEdx[k].SigmaFee > 0) {
	    Double_t xscale = 1;
	    if (FdEdx[k].row > 13) xscale = pow(FdEdx[k].dx,0.41);
	    else                   xscale = pow(FdEdx[k].dx,0.31);
	    xscale /= FdEdx[k].SigmaFee;
	    Double_t dEdxLog = FdEdx[k].dEdxL + 8.26477e-02;
	    Double_t dxLog   = TMath::Log(FdEdx[k].dx);
	    Double_t Shift   = (dEdxLog-fitZ)*xscale;
	    if (FdEdx[k].row <= 13) {
	      Shift -=  -1.84183e-01 + 1.62618e-01*dxLog;
	      FShapeI->Fill(dxLog,Shift);
	    }
	    else {
	      Shift -= -1.58130e-01 -4.87002e-02*dxLog;
	      FShapeO->Fill(dxLog,Shift);
	    }
	  }
	}
	Int_t best = -1; Double_t valBest = 9999.;
	Int_t good = -1; Double_t valGood = 9999.;
	for (int hyp=0; hyp<NHYPS;hyp++) {
	  hist70[hyp][sCharge]->Fill(TMath::Log10(p/Masses[hyp]),TMath::Log(I70/Pred[hyp]));
	  hist60[hyp][sCharge]->Fill(TMath::Log10(p/Masses[hyp]),TMath::Log(I60/Pred[hyp]));
	  Double_t dev = 9999.;
	  if (fitdZ > 0.) dev = TMath::Abs((fitZ - TMath::Log(Pred[hyp]))/fitdZ);
	  if (dev < valBest) {
	    good = best; valGood = valBest;
	    best = hyp;  valBest = dev;
	  }
	  if (hyp != best && dev < valGood) {
	    good = hyp; valGood = dev;
	  }
	  histz[hyp][sCharge]->Fill(TMath::Log10(p/Masses[hyp]),fitZ - TMath::Log(Pred[hyp]));
	  histB[hyp][sCharge]->Fill(TMath::Log10(p/Masses[hyp]),TMath::Log(Pred[hyp]));
	  histBB[hyp][sCharge]->Fill(TMath::Log10(p/Masses[hyp]),TMath::Log(PredBB[hyp]));
	}
	Double_t date = MyDate(GetDate(),GetTime());
	Time->Fill(date,fitZ - TMath::Log(Pred[2]));
	FitPull->Fill(TrackLength,(fitZ - TMath::Log(Pred[2]))/fitdZ);
      }
     SafeDelete(helix);
    }
  }
  SafeDelete(pointS);
  SafeDelete(primtrkS);
  SafeDelete(globtrkS);
  SafeDelete(pointC);
  SafeDelete(primtrkC);
  SafeDelete(globtrkC);
  return kStOK;
}
//________________________________________________________________________________
void StdEdxMaker::SortdEdx(Double_t *dEdxS, Int_t NdEdx) {
  int i;
  for (i = 0; i < NdEdx; i++) dEdxS[i] = CdEdx[i].dEdx;
  for (i = 0; i < NdEdx-1; i++) {
    for (int j = i+1; j < NdEdx; j++) {
      if (dEdxS[i] > dEdxS[j]) {
	double temp = dEdxS[i];
	dEdxS[i] = dEdxS[j];
	dEdxS[j] = temp;
      }
    }
  }
}
//__________________________________________________
Double_t StdEdxMaker::MyDate(Int_t date,Int_t time) {// combine date in time in one number startig 07/01/2000
  Int_t md = date-20000000; // printf("md %i\n",md);
  Int_t d  = md%100;        // printf("d %i\n",d);
  Int_t m  = md/100;        // printf("m %i\n",m);
  Double_t dd = d;
  if (m > 7) dd += 30; // July
  if (m > 8) dd += 31; // August
  if (m > 9) dd += 31; // September;
  // printf("dd = %f\n",dd);
  Double_t S = time%100;               // printf("S %i\n",S);
  Double_t M = (time/100)%100;         // printf("M %i\n",M);
  Double_t H = time/10000;             // printf("H %i\n",H);
  Double_t Date = dd + (H + (M + S/60)/60)/24;             // printf("Date %f\n",Date);
  return Date;
}
//________________________________________________________________________________
void fGaus(Double_t x, Double_t *val, ESector l){
  static const Int_t NpGaus = 12;
  static Double_t parGaus[2][NpGaus] = {
#if 0
  {0.023172, 1.437271, 2.460729, // inner
   0.103880, 0.242696, 1.577239,
   0.195327,-0.024190, 0.889409,
   0.007452,-1.433872, 0.582710},
  {0.014107, 2.636221, 2.435468, // outer
   0.073707, 0.963552, 1.515737,
   0.048300,-0.318190, 1.308623,
   0.200573,-0.038547,-0.945790}
  {0.025611, 1.418447,2.220133,  // inner 
   0.121489, 0.201385,1.415622,
   0.000408,-4.223811,0.757573,
   0.202918, 0.006651,0.836666},
  {0.013792, 2.854905, 2.672949, // outer
   0.068082, 1.024063, 1.654282,
   0.066709,-0.199578,-1.401709,
   0.156028,-0.045180,-1.000343}
  {0.027301, 1.259608,2.255916, // inner
   0.119186, 0.233070,1.401950,
   0.009470,-1.188956,0.585663,
   0.205339, 0.030715,0.802100},
  {0.013503, 2.878258,2.676623,
   0.070468, 0.971207,1.674857,
   0.064795,-0.199962,-1.387658,
   0.153726,-0.041741,-1.008292}
#endif
  {0.025665, 1.423958,2.213710,
   0.123577, 0.193294,1.408513,
   0.000420,-4.221713,0.769640,
   0.203597, 0.005066,0.823813},
  {0.013443, 2.874708,2.680563,
   0.064505, 1.049656,1.669515,
   0.070611,-0.159342,1.408693,
   0.154166,-0.044689,1.011159}
  };
  val[0] = val[1] = 0;
  for (Int_t j=0; j<NpGaus; j+=3){
    Double_t dev = (x - parGaus[l][j+1])/parGaus[l][j+2];
    val[1] += parGaus[l][j]*exp(-0.5*dev*dev)*(-dev/parGaus[l][j+2]);
    val[0] += parGaus[l][j]*exp(-0.5*dev*dev);
  }
}
//________________________________________________________________________________
void MDFcorrOw(Double_t *x, Double_t *Value) {
  // Static data variables
  static Int_t    gNVariables    = 2;
  static Int_t    gNCoefficients = 12;
  static Double_t gDMean         = -2.13744407349024695e-01;//2.11349e-05;
  static Double_t gXMin[] = {  -7.3, -7.3 };
  static Double_t gXMax[] = {  9.9, 9.9 };
  static Double_t gCoefficient[] = {  0.200633,  6.14428,  0.477188, -0.699637,
				     -0.705071, -2.41774, -1.00517,  -4.03022,
				      2.25038,  -2.53387,  3.29628,   0.214115};
// The powers are stored row-wise, that is p_ij = gPower[i * NVariables + j];
  static Int_t    gPower[] = {
    1,  1,  2,  2,  1,  2,  3,  1,  1,  3,  3,  2,
    2,  3,  4,  2,  3,  3,  2,  4,  5,  2,  2,  1};

  Value[0] = gDMean;
  Value[1] = Value[2] = 0;
  Int_t i,j,k;
  for (i = 0; i < gNCoefficients ; i++) {
    // Evaluate the ith term in the expansion
    Double_t term[3];
    term[0] = gCoefficient[i];
    term[1] = gCoefficient[i];
    term[2] = gCoefficient[i];
    for (j = 0; j < gNVariables; j++) {
      // Evaluate the polynomial in the jth variable.
      Int_t power = gPower[gNVariables * i + j]; 
      Double_t p1 = 1, p2 = 0, p3 = 0, r = 0, px = 0;
      Double_t v =  1 + 2. / (gXMax[j] - gXMin[j]) * (x[j] - gXMax[j]);
      Double_t vg =     2. / (gXMax[j] - gXMin[j]);
      // what is the power to use!
      switch(power) {
      case 1: r = 1; px = 0; break; 
      case 2: r = v; px = 1; break; 
      default: 
        p2 = v; 
	px = 1;
        for (k = 3; k <= power; k++) { 
	  px = p2;
          p3 = p2 * v;
          p1 = p2; p2 = p3; 
        }
        r = p3;
      }
      // multiply this term by the poly in the jth var
      term[0] *= r; 
      for (int l = 0; l < gNVariables; l++) {
	if (j == l) term[l+1] *= (power-1)*px*vg;
	else        term[l+1] *= r;
      } 
    }
    // Add this term to the final result
    for (j = 0; j < 3; j ++) Value[j] += term[j];
  }
}
//________________________________________________________________________________
void fcn(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag)
{
  f = 0.;
  gin[0] = 0.;
  gin[1] = 0.;
  Double_t Val[2];
  for (int i=0;i<NdEdx; i++) {
    Double_t dx =FdEdx[i].dx; 
    if (dx <  1.) dx = 1.;
    if (dx > 10.) dx = 10.;
    ESector l = kTpcInner;
    if (FdEdx[i].row > 13) l = kTpcOuter;
    FdEdx[i].xscale = 1;
    switch (l) {
    case kTpcInner: FdEdx[i].xscale = pow(FdEdx[i].dx,0.31);      break;
    default:   FdEdx[i].xscale = pow(FdEdx[i].dx,0.41);
    }
    FdEdx[i].xscale /= FdEdx[i].SigmaFee;
    Double_t dEdxLog = FdEdx[i].dEdxL + 8.26477e-02;
    Double_t dxLog   = TMath::Log(FdEdx[i].dx);
    FdEdx[i].zdev   = (dEdxLog-par[0])*FdEdx[i].xscale;
    switch (l) {
    case kTpcInner: FdEdx[i].zdev -= -1.84183e-01 + 1.62618e-01*dxLog; break;
    default:        FdEdx[i].zdev -= -1.58130e-01 - 4.87002e-02*dxLog;
    }
    fGaus(FdEdx[i].zdev,Val,l);
    FdEdx[i].Prob = Val[0];
    f -= TMath::Log( FdEdx[i].Prob );
    Double_t gg = Val[1];
    gin[0] +=  FdEdx[i].xscale*gg/FdEdx[i].Prob;
#if 0
    // correlations
    if (i > 0 && FdEdx[i].row > 14) {
      Int_t j = i - 1;
      if (FdEdx[i].sector == FdEdx[j].sector &&
	  FdEdx[i].row    == FdEdx[j].row + 1) {
	Double_t x[2];
	x[0] =  FdEdx[j].zdev;
	x[1] =  FdEdx[i].zdev;
	Double_t Corr[3];
	MDFcorrOw(x,Corr);
	f -= Corr[2];
	gin[0] -= FdEdx[j].xscale*Corr[1] + FdEdx[i].xscale*Corr[2];
      }
    }
#endif
  }
}
//________________________________________________________________________________
void StdEdxMaker::DoFitZ(Double_t &chisq, Double_t &fitZ, Double_t &fitdZ)
{
  Double_t avz = 0;
  for (int i=0;i<NdEdx;i++) {
    FdEdx[i] = CdEdx[i];
    FdEdx[i].dEdxL = FdEdx[i].dEdxL;
    avz += FdEdx[i].dEdxL;
    for (int j=0;j<i;j++) {// order by rowsto account correlations
      if (FdEdx[i].sector == FdEdx[j].sector &&
	  FdEdx[i].row    <  FdEdx[j].row) {
	dEdx_t temp = FdEdx[j];
	FdEdx[j] = FdEdx[i];
	FdEdx[i] = temp;
      }
    }
  }
  if (NdEdx>5) {
    avz /= NdEdx;
    Double_t arglist[10];
    Int_t ierflg = 0;
    gMinuit->SetFCN(fcn);
    //    gMinuit->SetPrintLevel(-1);
#if 1
    arglist[0] = -1;
    gMinuit->mnexcm("set print",arglist, 1, ierflg);
#endif
    gMinuit->mnexcm("set NOW",arglist, 0, ierflg);
    gMinuit->mnexcm("CLEAR",arglist, 0, ierflg);
    arglist[0] = 0.5;
    gMinuit->mnexcm("SET ERR", arglist ,1,ierflg);
    gMinuit->mnparm(0, "mean", avz, 0.01,0.,0.,ierflg); //First Guess
//     gMinuit->mnparm(1, "scale", 1.0, 0.01, 0.2, 20.0, ierflg);
//     arglist[0] = 2;
//     gMinuit->mnexcm("FIX",arglist,1,ierflg);
#if 1    
    arglist[0] = 1.;   // 1.
#else
    arglist[0] = 0.;   // Check gradient 
#endif
    gMinuit->mnexcm("SET GRAD",arglist,1,ierflg);
    //    gMinuit->mnexcm("SET STRAT",arglist,1,ierflg);
    arglist[0] = 500;
    arglist[1] = 1.;
    gMinuit->mnexcm("MIGRAD", arglist ,2,ierflg);
    gMinuit->mnexcm("HESSE  ",arglist,0,ierflg);
    //    gMinuit->mnexcm("end",arglist,0,ierflg);
// Print results
    Double_t edm,errdef;
    Int_t nvpar,nparx,icstat;
    gMinuit->mnstat(chisq,edm,errdef,nvpar,nparx,icstat);
    gMinuit->GetParameter(0, fitZ, fitdZ);
  }
  else {
    fitZ = fitdZ = chisq =  -999.;
  }
}
