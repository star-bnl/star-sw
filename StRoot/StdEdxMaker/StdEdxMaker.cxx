// $Id: StdEdxMaker.cxx,v 1.7 2000/12/29 14:36:29 fisyak Exp $
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
#include "tables/St_tpcFeeGainCor_Table.h"
#include "tables/St_TpcTimeGain_Table.h"
#include "tables/St_TpcDriftDistCorr_Table.h"
#include "tables/St_fee_vs_pad_row_Table.h"
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
  Double_t dE;
  Double_t dEU; // before correction
  Double_t dx;
  Double_t dEdx; 
  Double_t dEdxU; 
  Double_t dEdxL; // log of dEdx
  Double_t dEdxLU; // log of dEdx
  Double_t dEdxN; // normolized to BB
  Double_t xyz[3];
  Double_t Prob; 
};
dEdx_t CdEdx[60]; // corrected
dEdx_t FdEdx[60]; // fit
Int_t NdEdx = 0, NPoints = 0;
Double_t dEdxS[60]; // dEdx sorted
Double_t dE, dx, dEdx, Sigma_dEdx;

TF1 *fFunc = 0;
TCanvas *fCanvas = 0;
TH1F    *h1 = 0;
TMinuit *gMinuit = new TMinuit(2);
const Int_t NpGaus = 12;
// pow(x,0.36) parameters
Double_t parGaus[NpGaus] = { 0.626630,-0.196071,0.374226,
			     0.204128, 0.097587,0.674559,
			     0.023837, 1.018181,1.135371,
			    -0.001196,-1.539197,0.275564};
Double_t perGaus[NpGaus] = { 0.001127, 0.000277,0.000397,
			     0.001034, 0.002307,0.000891,
			     0.000131, 0.003892,0.001164,
			     0.000158, 0.034256,0.017512};
// Double_t sigmaI[2]       = { 9.57206e-01,-6.09601e-03};
// Double_t sigmaO[6]       = {-8.58965e-01, 2.95273e+00,-1.81994e+00, 5.45927e-01,-7.99525e-02, 4.60043e-03};
// Double_t muIO[6]  = {-2.79379e-01, 4.95047e-01,-3.22628e-01, 9.85297e-02,-1.42366e-02, 7.74382e-04};
// length correction in range [17,115]
Double_t CLength[6] = {-3.85006e-01, 3.12105e-02,-9.40609e-04, 1.33017e-05,-8.88061e-08, 2.26305e-10};
Double_t CPull[2]   = { 1.35893e+00, -1.27034e-03}; // pull vs length [50,140]
#if 0
// sqrt(x) parameters
Double_t parGaus[NpGaus] = {0.486236,-0.204430,0.460330,
			    0.016537,1.642435,1.085395,
			    0.198163,0.102950,0.790066,
			    0.000863,-2.143906,0.698302};
Double_t perGaus[NpGaus] = {0.001589,0.000449,0.000624,
			    0.000382,0.028459,0.007643,
			    0.001293,0.003442,0.002275,
			    0.000042,0.051532,0.020009};
#endif

extern    void fcn(Int_t &npar, Double_t *gin, Double_t &f, Double_t *u, Int_t flag);
extern    Double_t fGaus(Double_t x);
extern    Double_t gGaus(Double_t x);
// Histograms
TH3F *Z = 0, *ETA = 0, *SecRow = 0, *TPoints = 0, *Fee = 0;// *sXY = 0, *SXY = 0;
TH2F *Time = 0, *CTime = 0, *Points70 = 0, *Points60 = 0, *Points70B = 0, *Points60B = 0, *SR = 0, *PointsFit = 0;
TH2F *FShapeI = 0, *FShapeO = 0, *FShapeIC = 0, *FShapeOC = 0;
TH2F *corrI = 0, *corrO = 0, *corrI2 = 0, *corrO2 = 0, *corrI5 = 0, *corrO5 = 0, *corrIc = 0, *corrOc = 0;
TH2F *corrIU = 0, *corrOU = 0;
TH2D *hist70[NHYPS], *hist60[NHYPS], *histz[NHYPS], *histzC[NHYPS];
TH2F *FitPull = 0, *CFitPull = 0;
TH2F *corrIw = 0, *corrOw = 0, *corrI2w = 0, *corrO2w = 0, *corrI5w = 0, *corrO5w = 0;
TProfile *histB[NHYPS], *histBB[NHYPS]; 
ClassImp(StdEdxMaker)  

//_____________________________________________________________________________
StdEdxMaker::StdEdxMaker(const char *name):StMaker(name), m_tpcGain(0), m_tpcFeeGain(0), 
m_fee_vs_pad_row(0), m_tpcTime(0), m_drift(0) {}
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
    //    assert(m_tpcTime); 
  }
  m_drift = (St_TpcDriftDistCorr *) tpc_calib->Find("TpcDriftDistCorr"); 
  if (!m_drift) {
    cout << "TpcDriftDistCorr is missing <=========== switch off dirft dependent calibration" << endl;
    //    assert(m_drift); 
  }
  if (m_Mode > 0) {// calibration mode
    StMaker *tpcdaq = GetMaker("tpc_raw");
    if (!tpcdaq) {
      m_tpcGain = (St_tpcGain *) tpc_calib->Find("tpcGain"); assert(m_tpcGain); 
    }
    m_tpcFeeGain = (St_tpcFeeGainCor *) tpc_calib->Find("tpcFeeGain"); assert(m_tpcFeeGain); 
    TFile *f = (TFile *) ((StBFChain *)GetChain())->GetTFile();
    if (f) {
      f->cd();
      Z   = new TH3F("Z","log(dEdx/Pion) versus sector(inner <= 24 and outter > 24)  and Z",
		     2*NoSector,1,2*NoSector+1,125,-25.,225., 200,-5.,5.);
      ETA   = new TH3F("ETA","log(dEdx/Pion) versus sector(inner <= 24 and outter > 24)  and #{eta}",
		       2*NoSector,1,2*NoSector+1, 125,-.25,2.25, 200,-5.,5.);
#if 0
      sXY = new TH3F("sXY","XY for sector from points (input)",
		     50,-200,200,50,-200,200,
		     NoSector,1,NoSector+1);
      SXY = new TH3F("SXY","XY for sector from Pad transformation",
		     50,-200,200,50,-200,200,
		     NoSector,1,NoSector+1);
#endif
      SR = new TH2F("SR","sector and row from points (input)",
		       NoSector,1,NoSector+1, NoRow,1.,NoRow+1);
      SecRow = new TH3F("SecRow","log(dEdx/Pion) versus sector  and row",
		       NoSector,1,NoSector+1, NoRow,1.,NoRow+1, 200,-5.,5.);
      Int_t noFee = 182;
      Fee    = new TH3F("Fee","dEdx versus sector and Fee",
			NoSector,1,NoSector+1,  2*noFee,0,2*noFee, 200,-5.,5.);
#if 0
      CORR   = new TH3F("CORR","Correlation in <(z[row]-zfit)*(z[row+1]-zfit)> vs row nomber",
			44,1,45, 200,-5.,5., 200,-5.,5.);
#endif
      corrI   = new TH2F("corrI","Correlation for Inner Sector for pair of nearest rows",
			200,-5.,5., 200,-5.,5.);
      corrO   = new TH2F("corrO","Correlation for Outer Sector for pair of nearest rows",
			200,-5.,5., 200,-5.,5.);
      corrIU  = new TH2F("corrIU","Correlation for Inner Sector for pair of nearest rows before corrections",
			200,-5.,5., 200,-5.,5.);
      corrOU  = new TH2F("corrOU","Correlation for Outer Sector for pair of nearest rows before corrections",
			200,-5.,5., 200,-5.,5.);
      corrI2   = new TH2F("corrI2","Correlation for Inner Sector for pair rows & row + 2",
			200,-5.,5., 200,-5.,5.);
      corrO2   = new TH2F("corrO2","Correlation for Outer Sector for pair rows & row + 2",
			200,-5.,5., 200,-5.,5.);
      corrI5   = new TH2F("corrI5","Correlation for Inner Sector for pair rows & row + 5",
			200,-5.,5., 200,-5.,5.);
      corrO5   = new TH2F("corrO5","Correlation for Outer Sector for pair rows & row + 5",
			200,-5.,5., 200,-5.,5.);
      corrIw   = new TH2F("corrIw","Weighted correlation for Inner Sector for pair of nearest rows",
			200,-5.,5., 200,-5.,5.);
      corrOw   = new TH2F("corrOw","Weighted correlation for Outer Sector for pair of nearest rows",
			200,-5.,5., 200,-5.,5.);
      corrI2w   = new TH2F("corrI2w","Weighted correlation for Inner Sector for pair rows & row + 2",
			200,-5.,5., 200,-5.,5.);
      corrO2w   = new TH2F("corrO2w","Weighted correlation for Outer Sector for pair rows & row + 2",
			200,-5.,5., 200,-5.,5.);
      corrI5w   = new TH2F("corrI5w","Weighted correlation for Inner Sector for pair rows & row + 5",
			200,-5.,5., 200,-5.,5.);
      corrO5w   = new TH2F("corrO5w","Weighted correlation for Outer Sector for pair rows & row + 5",
			200,-5.,5., 200,-5.,5.);
      corrIc   = new TH2F("corrIc","Correlation for Inner Sector for pair of nearest row after correction",
			200,-5.,5., 200,-5.,5.);
      corrOc   = new TH2F("corrOc","Correlation for Outer Sector for pair of nearest row after correction",
			200,-5.,5., 200,-5.,5.);
      Points70 = new TH2F("Points70","dEdx(I70) versus no. of measured points",50,0,50.,200,-1.,1.);
      Points60 = new TH2F("Points60","dEdx(I60) versus no. of measured points",50,0,50.,200,-1.,1.);
      Points70B = new TH2F("Points70B","dEdx(I70) versus no. of measured points BB",50,0,50.,200,-1.,1.);
      Points60B = new TH2F("Points60B","dEdx(I60) versus no. of measured points BB",50,0,50.,200,-1.,1.);
      PointsFit  = new TH2F("PointsFit","dEdx(fit) versus no. of measured points",
			    50,0,50., 200,-1.,1.);
      TPoints  = new TH3F("TPoints","dEdx(fit) versus no. of measured points and length", 
			  50,0,50., 150,10.,160., 200,-1.,1.);
      FShapeI  = new TH2F("FShapeI","(log(dEdx)-<z_{fit}>)*(dx)**0.36 versus log(dx) for Inner Sector", 80,0, 4., 300,-7.5,7.5);
      FShapeO  = new TH2F("FShapeO","(log(dEdx)-<z_{fit}>)*(dx)**0.36 versus log(dx) for Outer Sector", 80,0, 4., 300,-7.5,7.5);
#if 0
      FShapeIC = new TH2F("FShapeIC","(log(dEdx)-<z_{fit}>)*(dx)**0.36 versus dx for Inner Sector corrected", 
			  40,1, 5., 300,-7.5,7.5);
      FShapeOC = new TH2F("FShapeOC","(log(dEdx)-<z_{fit}>)*(dx)**0.36 versus dx for Outer Sector corrected", 
			  80,2,10., 300,-7.5,7.5);
#endif
      for (int hyp=0; hyp<NHYPS;hyp++) {
	TString name = Names[hyp];
	name += "70";
	TString title = "log(dE/dx70/I(";
	title += Names[hyp];
	title += ")) versus log10(p/m)";
	hist70[hyp] = new TH2D(name.Data(),title.Data(),100,-1.,4.,200,-1.,1.);
	name = Names[hyp];
	name += "60";
	title = "log(dE/dx60/I(";
	title += Names[hyp];
	title += ")) versus log10(p/m)";
	hist60[hyp] = new TH2D(name.Data(),title.Data(),100,-1.,4.,200,-1.,1.);
	name = Names[hyp];
	name += "z";
	title = "zFit - log(I(";
	title += Names[hyp];
	title += ")) versus log10(p/m)";
	histz[hyp] = new TH2D(name.Data(),title.Data(),100,-1.,4.,200,-1.,1.);
	name = Names[hyp];
	name += "zClean";
	title = "zFit - log(I(";
	title += Names[hyp];
	title += ")) versus log10(p/m)";
	histzC[hyp] = new TH2D(name.Data(),title.Data(),100,-1.,4.,200,-1.,1.);
	name = Names[hyp];
	name += "B";
	title = "log(I_{Sirrf}(";
	title += Names[hyp];
	title += ")) versus log10(p/m)";
	histB[hyp] = new TProfile(name.Data(),title.Data(),100,-1.,4.);
	name += "B";
	title = "log(I_{BB}(";
	title += Names[hyp];
	title += ")) versus log10(p/m)";
	histBB[hyp] = new TProfile(name.Data(),title.Data(),100,-1.,4.);
      }
      Time   = new TH2F("Time","zFit - log(I(pi)) versus Date& Time", 280,0.,70., 200,-1.,1.);
      CTime  = new TH2F("CTime","zFit(Cleaned) - log(I(pi)) versus Date& Time", 280,0.,70., 200,-1.,1.);
      FitPull= new TH2F("FitPull","(zFit - log(I(pi)))/dzFit  versus track length", 
			150,10.,160, 200,-5.,5.);
      CFitPull= new TH2F("CFitPull","(zFit(Cleaned) - log(I(pi)))/dzFit  versus track length", 
			 150,10.,160, 200,-5.,5.);
    }
  }
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StdEdxMaker::Make(){ 
  Double_t Scale70 = TMath::Exp(2.15435500000000002e-01);
  Double_t Scale60 = TMath::Exp(2.79098499999999972e-01);
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
      dE = pointC->GetCharge(ipoint);
      Int_t sector = pointC->Sector(ipoint);
      Int_t row = pointC->PadRow(ipoint);
      Double_t pointX = pointC->GetX(ipoint);
      Double_t pointY = pointC->GetY(ipoint);
      Double_t pointZ = pointC->GetZ(ipoint);
      
      StGlobalCoordinate global(pointX,pointY,pointZ);
      StTpcPadCoordinate Pad;      transform(global,Pad);
#if 0
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
	    " Sector = " << PadOnPlane.sector() << "/"  << "/" << Pad.sector() << "/" << sector <<
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
      // Correctionsd
      Int_t pad = Pad.pad();
      if (m_tpcGain) {// && ! point->TestBit(kIsCalibrated)) {
	tpcGain_st *gain = m_tpcGain->GetTable() + sector - 1;
	dE *= gain->Gain[row-1][pad-1]; 
      }
      double gc = 1;
      Int_t fee = -1;
      if (m_tpcFeeGain && m_fee_vs_pad_row) {
	fee_vs_pad_row_st  *fee_vs_pad_row = m_fee_vs_pad_row->GetTable() + row - 1;
	fee = (Int_t) fee_vs_pad_row->fee[pad-1];
	tpcFeeGainCor_st *gain = m_tpcFeeGain->GetTable() + sector - 1;
	gc =  gain->Gain[fee][row%2];
      }
      if (gc < 0.0) continue;
      dE *= gc;
      dE *= TimeScale;
      Int_t SectN = sector; // drift distance
      if (row < 14) SectN += 24;
      Double_t z = pointZ;
      if (sector > 12) z = - z;
      if (m_drift) {
	TpcDriftDistCorr_st *cor = m_drift->GetTable() + SectN - 1;
	//	Double_t DriftCorr = TMath::Exp(-(cor->a0+z*(cor->a1 + z*cor->a2)));
	Double_t zz = z/190;
	Double_t DriftCorr = -7.40365e-02 + 
	  cor->a[0]+cor->a[1]*(2*zz-1)+cor->a[2]*TMath::Exp(cor->a[3]*(zz-1));
	dE *= TMath::Exp(-DriftCorr);
      }
#if 0 
      // dx correction
      if (dx < 1.8) dE *= TMath::Exp(-( 1.77818e-02+6.24927e-02*dx
				       -2.91655e-02-3.14485e-03*dx));
      else          dE *= TMath::Exp(-(-3.13286e-02+2.47879e-03*dx
				       -5.37064e-02+2.50372e-02*dx)); 
#endif
      TrackLength += dx;
      CdEdx[NdEdx].sector = sector;
      CdEdx[NdEdx].row = row;
      CdEdx[NdEdx].pad = pad;
      CdEdx[NdEdx].dx = dx;
      CdEdx[NdEdx].dEU= pointC->GetCharge(ipoint);;
      CdEdx[NdEdx].dE = dE; // corrected
      CdEdx[NdEdx].dEdxU= CdEdx[NdEdx].dEU/CdEdx[NdEdx].dx;
      CdEdx[NdEdx].dEdx = CdEdx[NdEdx].dE/CdEdx[NdEdx].dx;
      CdEdx[NdEdx].dEdxLU= TMath::Log(CdEdx[NdEdx].dEdxU);
      CdEdx[NdEdx].dEdxL = TMath::Log(CdEdx[NdEdx].dEdx);
      CdEdx[NdEdx].xyz[0] = pointX;
      CdEdx[NdEdx].xyz[1] = pointY;
      CdEdx[NdEdx].xyz[2] = pointZ;
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
    if (chisq > 0 && chisq < 10000.0) {
      Double_t TrkL = TrackLength;
#if 0
      if (TrkL <  17) TrkL =  17.;
      if (TrkL > 140) TrkL = 140.;
      Double_t CTrkL =   CLength[0]    + TrkL*(CLength[1]    + TrkL*(CLength[2]    + 
	           TrkL*(CLength[3]    + TrkL*(CLength[4]    + TrkL*(CLength[5])))));
      fitZ -= CTrkL;
#endif
#if 1
      if (TrkL <  50) TrkL =  50.;
      Double_t Cpull =   CPull[0]    + TrkL*(CPull[1]    + TrkL*(CPull[2]    + 
	           TrkL*(CPull[3]    + TrkL*(CPull[4]    + TrkL*(CPull[5])))));
      fitdZ *= Cpull;
#endif
      dedx.id_track  =  Id;
      dedx.det_id    =  kTpcId;    // TPC track 
      dedx.method    =  kLikelihoodFitIdentifier;
      dedx.ndedx     =  NdEdx + 100*((int) TrackLength);
      dedx.dedx[0]   =  TMath::Exp(fitZ);
      if (fitdZ >= 1.) fitdZ = 0.999;
      dedx.dedx[1]   =  fitdZ; //((int)(chisq*100)) + fitdZ;
      dst_dedx->AddAt(&dedx);
    }
    if (iprim >= 0 && m_Mode > 0) {
      Double_t Pred[NHYPS], PredBB[NHYPS];
      for (int l = 0; l < NHYPS; l++) {
	Pred[l] = 1.e-6*BetheBloch::Sirrf(p/Masses[l],TrackLength,0);
	PredBB[l] = BB(p/Masses[l]);
      }
      for (k = 0; k < NdEdx; k++) {
	FdEdx[k].dEdxN = TMath::Log(FdEdx[k].dEdx/Pred[2]);
	if (SR) SR->Fill(FdEdx[k].sector,FdEdx[k].row);
	if (chisq > 0 && chisq < 10000.0) {
	  Double_t zk  = (TMath::Log(FdEdx[k].dEdx ) - fitZ)*pow(FdEdx[k].dx,0.36);
	  Double_t zkU = (TMath::Log(FdEdx[k].dEdxU) - fitZ)*pow(FdEdx[k].dx,0.36);
	  for (int l = 0; l < NdEdx; l++){
	    if (k == l) continue;
	    Double_t zl  = (TMath::Log(FdEdx[l].dEdx ) - fitZ)*pow(FdEdx[l].dx,0.36);
	    Double_t zlU = (TMath::Log(FdEdx[l].dEdxU) - fitZ)*pow(FdEdx[l].dx,0.36);
	    if (FdEdx[l].row%2 == 1 && FdEdx[l].row - FdEdx[k].row  == 1) {
	      if (FdEdx[k].row > 13) {
		corrO->Fill(zk,zl); 
		corrOU->Fill(zkU,zlU); 
		if (FdEdx[k].Prob*FdEdx[l].Prob > 1.e-6) 
		  corrOw->Fill(zk,zl,1./(FdEdx[k].Prob*FdEdx[l].Prob));
		corrOc->Fill(zk,zl-0.272*zk); 
	      }
	      else {
		corrI->Fill(zk,zl); 
		corrIU->Fill(zkU,zlU); 
		if (FdEdx[k].Prob*FdEdx[l].Prob > 1.e-6) 
		  corrIw->Fill(zk,zl,1./(FdEdx[k].Prob*FdEdx[l].Prob));
		corrIc->Fill(zk,zl-0.046*zk); 
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
      //      if (sXY) sXY->Fill(CdEdx[k].x[0],CdEdx[k].x[0],CdEdx[k].sector);
	if (Z && NoFitPoints >= 30) {
	  Double_t eta =  Eta;
	  Int_t SectN = CdEdx[k].sector; // drift distance
	  if (CdEdx[k].row < 14) SectN += 24;
	  Z->Fill(SectN,CdEdx[k].xyz[2],CdEdx[k].dEdxN);
	  if (CdEdx[k].sector > 12) eta = -eta;
	  if (ETA) ETA->Fill(SectN,eta,CdEdx[k].dEdxN);
	  if (SecRow) SecRow->Fill(CdEdx[k].sector+0.5,CdEdx[k].row+0.5,CdEdx[k].dEdxN);
	  if (m_tpcFeeGain && m_fee_vs_pad_row) {
	    fee_vs_pad_row_st  *fee_vs_pad_row = 
	      m_fee_vs_pad_row->GetTable() + CdEdx[k].row - 1;
	    Int_t fee = (Int_t) fee_vs_pad_row->fee[CdEdx[k].pad-1];
	    if (Fee && fee >= 0) {
	      Double_t fee1   = 2*fee + CdEdx[k].row%2 + 0.5;
	      Double_t Sect = CdEdx[k].sector + 0.5;
	      Fee->Fill(Sect,fee1,CdEdx[k].dEdxN);
	    }
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
	  Double_t xscale = pow(CdEdx[k].dx,0.36);
	  Double_t dEdxLog = CdEdx[k].dEdxL;
	  Double_t dxLog   = TMath::Log(CdEdx[k].dx);
	  if (CdEdx[k].row <= 13) FShapeI->Fill(dxLog,(dEdxLog-fitZ)*xscale);
	  else                    FShapeO->Fill(dxLog,(dEdxLog-fitZ)*xscale);
	}
	Int_t best = -1; Double_t valBest = 9999.;
	Int_t good = -1; Double_t valGood = 9999.;
	for (int hyp=0; hyp<NHYPS;hyp++) {
	  hist70[hyp]->Fill(TMath::Log10(p/Masses[hyp]),TMath::Log(I70/Pred[hyp]));
	  hist60[hyp]->Fill(TMath::Log10(p/Masses[hyp]),TMath::Log(I60/Pred[hyp]));
	  Double_t dev = 9999.;
	  if (fitdZ > 0.) dev = TMath::Abs((fitZ - TMath::Log(Pred[hyp]))/fitdZ);
	  if (dev < valBest) {
	    good = best; valGood = valBest;
	    best = hyp;  valBest = dev;
	  }
	  if (hyp != best && dev < valGood) {
	    good = hyp; valGood = dev;
	  }
	  histz[hyp]->Fill(TMath::Log10(p/Masses[hyp]),fitZ - TMath::Log(Pred[hyp]));
	  histB[hyp]->Fill(TMath::Log10(p/Masses[hyp]),TMath::Log(Pred[hyp]));
	  histBB[hyp]->Fill(TMath::Log10(p/Masses[hyp]),TMath::Log(PredBB[hyp]));
	}
	Double_t date = MyDate(GetDate(),GetTime());
	Time->Fill(date,fitZ - TMath::Log(Pred[2]));
	FitPull->Fill(TrackLength,(fitZ - TMath::Log(Pred[2]))/fitdZ);
	if (valBest < 3 && valGood > 5.0) {
	  histzC[best]->Fill(TMath::Log10(p/Masses[best]),fitZ - TMath::Log(Pred[best]));
	  CTime->Fill(date,fitZ - TMath::Log(Pred[best]));
	  CFitPull->Fill(TrackLength,(fitZ - TMath::Log(Pred[best]))/fitdZ);
	}
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
Double_t fGaus(Double_t x) {
  Double_t val = 0;
  for (Int_t j=0; j<NpGaus; j+=3){
    Double_t dev = (x - parGaus[j+1])/parGaus[j+2];
    //      printf ("dev = %f\n",dev);
    val += parGaus[j]*exp(-0.5*dev*dev);
  }
  return val;
}
//________________________________________________________________________________
Double_t gGaus(Double_t x) {//derivatives
  Double_t val = 0;
  for (Int_t j=0; j<NpGaus; j+=3){
    Double_t dev = (x - parGaus[j+1])/parGaus[j+2];
    //      printf ("dev = %f\n",dev);
    val += parGaus[j]*exp(-0.5*dev*dev)*(-dev/parGaus[j+2]);
  }
  return val;
}
//________________________________________________________________________________
void fcn(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag)
{
#if 0
  static Double_t corrI = 0.046, corrO = 0.272;
#endif
  f = 0.;
  gin[0] = 0.;
  gin[1] = 0.;
  for (int i=0;i<NdEdx; i++) {
    Double_t mu = 0, sigma = 1;
    Double_t dx =FdEdx[i].dx; 
    if (dx <  1.) dx = 1.;
    if (dx > 10.) dx = 10.;
#if 0
    static Double_t muI[2]    = {-5.91052e-02, 4.16490e-02};
    static Double_t sigmaI[2] = {  9.62084e-01,-1.17671e-02};
    static Double_t muO[6]    = {-7.44921e-01, 1.24020e+00,-7.79951e-01, 2.32879e-01,-3.31352e-02,1.79072e-03};
    static Double_t sigmaO[6] = {-1.01100e+00, 3.22655e+00,-2.01124e+00, 6.11038e-01,-9.07746e-02,5.30432e-03};
    if (dx < 1.) dx = 1.;
    if (dx > 5.) dx = 5.;
    if (dx < 1.9) {
      mu    = muI[0]    + dx*muI[1];
      sigma = sigmaI[0] + dx*sigmaI[1];
    }
    else {
      mu    = muO[0]    + dx*(muO[1]    + dx*(muO[2]    + 
	  dx*(muO[3]    + dx*(muO[4]    + dx*(muO[5])))));
      sigma = sigmaO[0] + dx*(sigmaO[1] + dx*(sigmaO[2]    + 
	  dx*(sigmaO[3] + dx*(sigmaO[4] + dx*(sigmaO[5])))));
    }
#else
    Double_t dxL = TMath::Log(dx);
    if (FdEdx[i].row < 14) {
      mu    =  -1.40510e-02 + 8.20908e-02*dxL;
      sigma = 1. - 4.61981e-02 - 3.81961e-02*dxL;
    }
    else {
      mu    =  -5.02804e-01 + dxL*(2.14729e+00 +dxL*(-3.32605e+00 +dxL*(2.22391e+00 + dxL*-5.47997e-01)));
      sigma = 1. +  1.30637e-01 + dxL*(-2.45438e-01 +dxL*1.18801e-01);
    }
#endif
    Double_t xp = pow(dx,0.36);
    Double_t z  = FdEdx[i].dEdxL;
    Double_t corr = 0;
#if 0
    Double_t zI = 0;
    if (i > 0) {
      if (FdEdx[i].sector == FdEdx[i-1].sector &&
	  FdEdx[i].row - FdEdx[i-1].row == 1) {
	if (FdEdx[i-1].row > 13) {// Outer
	  zI = FdEdx[i-1].dEdxL;
	  corr = corrO;
	  continue;
	}
	if (FdEdx[i].row <= 13) {//Inner
	  zI = FdEdx[i-1].dEdxL;
	  corr = corrI;
	}

      }
    }
    Double_t zz = (z - par[0]) - corr*(zI - par[0]);
#else
    Double_t zz = z - par[0];
#endif
    Double_t xx = (zz*xp - mu)*sigma;
    Double_t sc = (1. - corr)*xp*sigma;
    FdEdx[i].Prob = fGaus(xx);
    f -= TMath::Log( FdEdx[i].Prob );
    Double_t gg = gGaus(xx);
    gin[0] +=  gg/FdEdx[i].Prob*sc;

   }
}
//________________________________________________________________________________
void StdEdxMaker::DoFitZ(Double_t &chisq, Double_t &fitZ, Double_t &fitdZ)
{
  if (NdEdx>10) {
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
    avz /= NdEdx;
    Double_t arglist[10];
    Int_t ierflg = 0;
    gMinuit->SetFCN(fcn);
    //    gMinuit->SetPrintLevel(-1);
    arglist[0] = -1;
    gMinuit->mnexcm("set print",arglist, 1, ierflg);
    gMinuit->mnexcm("set NOW",arglist, 0, ierflg);
    gMinuit->mnexcm("CLEAR",arglist, 0, ierflg);
    arglist[0] = 0.5;
    gMinuit->mnexcm("SET ERR", arglist ,1,ierflg);
    gMinuit->mnparm(0, "mean", avz, 0.01,0.,0.,ierflg); //First Guess
//     gMinuit->mnparm(1, "scale", 1.0, 0.01, 0.2, 20.0, ierflg);
//     arglist[0] = 2;
//     gMinuit->mnexcm("FIX",arglist,1,ierflg);
    
    arglist[0] = 1.;   // 1.
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
    
    //Print Results to screen
    gMinuit->GetParameter(0, fitZ, fitdZ);
    //    gMinuit->GetParameter(1, fitS, fitdS);
//     gMinuit->mnprin(3,chisq);
//     cout <<"mean:\t\t"<<fitZ<<"\t +- "<<fitdZ<<endl;
    //    cout <<"scale:\t\t"<<fitS<<"\t +- "<<fitdS<<endl;
  }
  else {
    fitZ = fitdZ = chisq =  -999.;
  }
  //  cout <<"\narithmeticMean:\t"<< avz <<endl;
}









