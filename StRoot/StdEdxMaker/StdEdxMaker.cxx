// $Id: StdEdxMaker.cxx,v 1.2 2000/11/25 23:19:53 fisyak Exp $
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
#include "tables/St_TpcTimeGain_Table.h"
#include "tables/St_TpcDriftDistCorr_Table.h"
// global
#include "StDetectorId.h"
#include "StDedxMethod.h"
// StarClassLibrary
#include "StThreeVector.hh" 
#include "StHelixD.hh"
#include "BetheBloch.h"
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
// Eefit
#include "EeFit.h"
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


Int_t NdEdx = 0, NPoints = 0;
Double_t dEdxS[60]; // dEdx sorted
Double_t dEdxN[60],dxN[60],dEdxL[60];
Double_t dE, dx, dEdx, Sigma_dEdx;

TF1 *fFunc = 0;
TCanvas *fCanvas = 0;
TH1F    *h1 = 0;
TMinuit *gMinuit = new TMinuit(2);
const Int_t NpGaus = 12;
Double_t parGaus[NpGaus] = {0.486236,-0.204430,0.460330,
			    0.016537,1.642435,1.085395,
			    0.198163,0.102950,0.790066,
			    0.000863,-2.143906,0.698302};
Double_t perGaus[NpGaus] = {0.001589,0.000449,0.000624,
			    0.000382,0.028459,0.007643,
			    0.001293,0.003442,0.002275,
			    0.000042,0.051532,0.020009};


extern    void fcn(Int_t &npar, Double_t *gin, Double_t &f, Double_t *u, Int_t flag);
extern    Double_t fGaus(Double_t x);
extern    Double_t gGaus(Double_t x);
// Histograms
TH3F *Z = 0, *ETA = 0, *SecRow = 0;
TNtuple *eefit = 0;
TH2F *Time = 0, *Points70 = 0, *Points60 = 0; TH2F *PointsFit = 0; TH3F *TPoints = 0;
TH2F *Shape = 0, *ZShape = 0, *FShape = 0;
TH2D *hist70[NHYPS], *hist60[NHYPS], *histz[NHYPS];
TProfile *histB[NHYPS], *histBB[NHYPS]; 
ClassImp(StdEdxMaker)  

//_____________________________________________________________________________
StdEdxMaker::StdEdxMaker(const char *name):StMaker(name), m_tpcGain(0), m_tpcTime(0), m_drift(0) 
{
}
//_____________________________________________________________________________
StdEdxMaker::~StdEdxMaker(){
}
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
  m_tpcTime = (St_TpcTimeGain *) tpc_calib->Find("TpcTimeGain"); assert(m_tpcTime); 
  m_drift = (St_TpcDriftDistCorr *) tpc_calib->Find("TpcDriftDistCorr"); assert(m_drift); 
  if (m_Mode > 0) {// calibration mode
    StMaker *tpcdaq = GetMaker("tpc_raw");
    if (!tpcdaq) {
      m_tpcGain = (St_tpcGain *) tpc_calib->Find("tpcGain"); assert(m_tpcGain); 
    }
    TFile *f = (TFile *) ((StBFChain *)GetChain())->GetTFile();
    if (f) {
      f->cd();
      if (m_Mode > 1) {// ntuples for EeFit
	eefit = new TNtuple("eefit","Input to EeFit program",
			    "NoPrimaryTracks:NoMeauredPoint:TrackLengths:Pmag:Eta:zFit");
      }
      Z   = new TH3F("Z","log(dEdx/Pion) versus sector(inner <= 24 and outter > 24)  and Z",
		     2*NoSector,1,2*NoSector+1,125,-25.,225., 200,-5.,5.);
      ETA   = new TH3F("ETA","log(dEdx/Pion) versus sector(inner <= 24 and outter > 24)  and #{eta}",
		       2*NoSector,1,2*NoSector+1, 125,-.25,2.25, 200,-5.,5.);
      SecRow = new TH3F("SecRow","log(dEdx/Pion) versus sector  and row",
		       NoSector,1,NoSector+1, NoRow,1.,NoRow+1, 200,-5.,5.);
      Time   = new TH2F("Time","dEdx versus Date& Time", 280,0.,70., 200,-5.,5.);
      Points70 = new TH2F("Points70","dEdx(I70) versus no. of measured points",50,0,50.,200,-1.,1.);
      Points60 = new TH2F("Points60","dEdx(I60) versus no. of measured points",50,0,50.,200,-1.,1.);
      PointsFit  = new TH2F("PointsFit","dEdx(fit) versus no. of measured points",50,0,50., 200,-1.,1.);
      TPoints  = new TH3F("TPoints","dEdx(fit) versus no. of measured points and length", 
			  50,0,50., 50,0,250., 200,-1.,1.);
      Shape  = new TH2F("Shape","log(dEdx/Pion)*sqrt(dx) versus dx",50,0,10., 300,-7.5,7.5);
      ZShape  = new TH2F("ZShape","(log(dEdx)-<z>)*sqrt(dx) versus dx", 50,0,10., 300,-7.5,7.5);
      FShape  = new TH2F("FShape","(log(dEdx)-<z_{fit}>)*sqrt(dx) versus dx", 50,0,10., 300,-7.5,7.5);
      for (int hyp=0; hyp<NHYPS;hyp++) {
	TString name = Names[hyp];
	name += "70";
	TString title = "log(dE/dx70/I(";
	title += Names[hyp];
	title += ")) versus log10(p/m)";
	hist70[hyp] = new TH2D(name.Data(),title.Data(),50,-1.,4.,200,-1.,1.);
	name = Names[hyp];
	name += "60";
	title = "log(dE/dx60/I(";
	title += Names[hyp];
	title += ")) versus log10(p/m)";
	hist60[hyp] = new TH2D(name.Data(),title.Data(),50,-1.,4.,200,-1.,1.);
	name = Names[hyp];
	name += "z";
	title = "zFit - log(I(";
	title += Names[hyp];
	title += ")) versus log10(p/m)";
	histz[hyp] = new TH2D(name.Data(),title.Data(),50,-1.,4.,200,-1.,1.);
	name = Names[hyp];
	name += "B";
	title = "log(I_{Sirrf}(";
	title += Names[hyp];
	title += ")) versus log10(p/m)";
	histB[hyp] = new TProfile(name.Data(),title.Data(),50,-1.,4.);
	name += "B";
	title = "log(I_{BB}(";
	title += Names[hyp];
	title += ")) versus log10(p/m)";
	histBB[hyp] = new TProfile(name.Data(),title.Data(),50,-1.,4.);
      } 
    }
  }
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StdEdxMaker::Make(){ 
  St_DataSet *dst = GetDataSet("dst"); assert(dst);
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
  StHelixD         *helix      = 0;
  StTpcCoordinateTransform transform(gStTpcDb);
  EeFit_t EeFit;
  Int_t iprim, iglob, ipoint; 
  Int_t nPrimaryTracks = 0;
  Double_t pTinv;
  Double_t p;
  Double_t Eta;
  Double_t Pred[NHYPS], PredBB[NHYPS];
  if (primtrk) {
    dst_track_st *primTrk = primtrk->GetTable();
    for (iprim = 0; iprim < primtrk->GetNRows(); iprim++, primTrk++) 
      if (primTrk->iflag > 0) nPrimaryTracks++;
  }
  Int_t kglob = 0, kprim = 0, kpoint = 0; 
  for (; kglob < globtrkS->GetNRows();kglob++) {
    iglob = globtrkS->GetIndex(kglob); assert (iglob >= 0);
    iprim = 0;
    if ((*globtrk)[iglob].iflag < 0) continue;
    Int_t Id = (*globtrk)[iglob].id;
    if (primtrkS && kprim < primtrkS->GetNRows()) {
      for (; kprim < primtrkS->GetNRows(); kprim++) {
	iprim = primtrkS->GetIndex(kprim); assert(iprim >=0);
	if ((*primtrk)[iprim].iflag < 0) continue;
	if ((*primtrk)[iprim].id < Id) continue;
	if ((*primtrk)[iprim].id > Id) break;
	helix  = primtrkC->MakeHelix(iprim);
	kprim++;
	if (m_Mode > 0) {
	  Double_t tanl  = (*primtrk)[iprim].tanl;
	  pTinv  = primtrkC->Invpt(iprim);
	  p = 1.e6;
	  if (pTinv > 1.e-6) p = 1./pTinv*TMath::Sqrt(1. + tanl*tanl);
	  Double_t Theta = TMath::ATan(tanl);
	  Eta = - TMath::Log(TMath::Tan(Theta/2.));
	  for (int l = 0; l < NHYPS; l++) {
	    Pred[l] = 1.e-6*Sirrf(p/Masses[l]);
	    PredBB[l] = BB(p/Masses[l]);
	  }
	}
      }
    }
    if (!helix) helix = globtrkC->MakeHelix(iglob);
    dst_dedx_st dedx;
    NPoints = 0;
    NdEdx = 0;
    Double_t TrackLength = 0;
    Double_t avrgZ = 0;
    for (;kpoint < pointS->GetNRows();kpoint++) {
      ipoint = pointS->GetIndex(kpoint); assert (ipoint >= 0);
      if (pointC->TrackId(ipoint) < Id) continue;
      if (pointC->TrackId(ipoint) > Id) break;
      NPoints++;
      if (pointC->GetFlag(ipoint)) continue;
      //      if (!pointC->GetFitFlag(ipoint)) continue;
      if (pointC->DetectorId(ipoint) != kTpcId) continue;
      dE = pointC->GetCharge(ipoint);
      Int_t sector = pointC->Sector(ipoint);
      Int_t padrow = pointC->PadRow(ipoint);
      if (padrow == 13) continue; // Skip row 13
      StThreeVectorD &normal = *mNormal[sector-1];
      //Get the position of the padrow center, transform to local sector coordinates
      StTpcPadCoordinate padCoord(sector, padrow, 1, 1);
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
      StGlobalCoordinate global(pointC->GetX(ipoint),pointC->GetY(ipoint),pointC->GetZ(ipoint));
      StTpcPadCoordinate Pad;      transform(global,Pad);
      if (sector != PadOnPlane.sector() || 
	  padrow != PadOnPlane.row() ||
	  TMath::Abs(Pad.pad()-PadOnPlane.pad()) > 5) {
	if (iprim  && Debug() > 1) {
	  gMessMgr->Warning() << "StdEdxMaker::" <<
	    " Sector = " << PadOnPlane.sector() << "/"  << "/" << Pad.sector() << "/" << sector <<
	    " PadRow = " << PadOnPlane.row() << "/" << Pad.row() << "/" << padrow << 
	    " Pad = " << PadOnPlane.pad() << "/" << Pad.pad() <<
	    " from Helix  is not matched with point/" << endm;;
	  gMessMgr->Warning() << "StdEdxMaker:: Coordinates " << 
	    " x: " << xyzOnPlane.x() << "/" << pointC->GetX(ipoint) <<
	    " y: " << xyzOnPlane.y() << "/" << pointC->GetY(ipoint) <<
	    " z: " << xyzOnPlane.z() << "/" << pointC->GetZ(ipoint) << endm;
	}
	continue;
      }
      Double_t padlength;
      if (padrow<14) padlength = gStTpcDb->PadPlaneGeometry()->innerSectorPadLength();
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
      // Corrections
      if (m_tpcGain) {
	Int_t pad = Pad.pad();
	tpcGain_st *gain = m_tpcGain->GetTable() + sector - 1;
	dE *= gain->Gain[padrow-1][pad-1]; 
      }
      Int_t SectN = sector; // drift distance
      if (padrow < 14) SectN += 24;
      TpcDriftDistCorr_st *cor = m_drift->GetTable() + SectN - 1;
      Double_t z = pointC->GetZ(ipoint);
      if (sector > 12) z = - z;
      Double_t DriftCorr = TMath::Exp(-(cor->a0+z*(cor->a1 + z*cor->a2)));
      dE *= DriftCorr;
      dE *= (*m_tpcTime)[0].ScaleFactor; // time
      TrackLength += dx;
      dEdxN[NdEdx] = dE/dx;
      dxN[NdEdx]   = dx;
      dEdxN[NdEdx] = dE/dx;
      dEdxS[NdEdx] = dEdxN[NdEdx];
      avrgZ += TMath::Log(dEdxN[NdEdx]);
      if (Z) {
	Double_t dEdxNorm = TMath::Log(dEdxN[NdEdx]/Pred[2]);
	Double_t eta =  Eta;
	Z->Fill(SectN,z,dEdxNorm);
	if (sector > 12) eta = -eta;
	if (ETA) ETA->Fill(SectN,eta,dEdxNorm);
	Double_t date = MyDate(GetDate(),GetTime());
	if (Time) Time->Fill(date,dEdxNorm);
	if (SecRow) SecRow->Fill(sector+0.5,padrow+0.5,dEdxNorm);
      }
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
      dedx.id_track  =  Id;
      dedx.det_id    =  kTpcId;    // TPC track 
      dedx.method    =  kLikelihoodFitIdentifier;
      dedx.ndedx     =  NdEdx;
      dedx.dedx[0]   =  TMath::Exp(fitZ);
      if (fitdZ >= 1.) fitdZ = 0.999;
      dedx.dedx[1]   =  ((int)(chisq*100)) + fitdZ;
      dst_dedx->AddAt(&dedx);
    }
    if (iprim && m_Mode > 0) {
      Points70->Fill(NdEdx,TMath::Log(I70/Pred[2]));
      Points60->Fill(NdEdx,TMath::Log(I60/Pred[2]));
      PointsFit->Fill(NdEdx,fitZ-TMath::Log(Pred[2]));
      TPoints->Fill(NdEdx,TrackLength,fitZ-TMath::Log(Pred[2]));
      if (NdEdx > 30) {
	for (k = 0; k < NdEdx; k++) {
	  Double_t dEdxNorm = TMath::Log(dEdxN[k]/Pred[2]);
	  Shape->Fill(dxN[k],dEdxNorm*TMath::Sqrt(dxN[k]));
	  ZShape->Fill(dxN[k],(TMath::Log(dEdxN[k])-avrgZ)*TMath::Sqrt(dxN[k]));
	  FShape->Fill(dxN[k],(TMath::Log(dEdxN[k])-fitZ)*TMath::Sqrt(dxN[k]));
	}
	for (int hyp=0; hyp<NHYPS;hyp++) {
	  hist70[hyp]->Fill(TMath::Log10(p/Masses[hyp]),TMath::Log(I70/Pred[hyp]));
	  hist60[hyp]->Fill(TMath::Log10(p/Masses[hyp]),TMath::Log(I60/Pred[hyp]));
	  histz[hyp]->Fill(TMath::Log10(p/Masses[hyp]),fitZ - TMath::Log(Pred[hyp]));
	  histB[hyp]->Fill(TMath::Log10(p/Masses[hyp]),TMath::Log(Pred[hyp]));
	  histBB[hyp]->Fill(TMath::Log10(p/Masses[hyp]),TMath::Log(PredBB[hyp]));
	}
      }
      EeFit.NoPrimaryTracks = nPrimaryTracks;
      EeFit.NoMeauredPoints = NdEdx;
      EeFit.TrackLength     = TrackLength;
      EeFit.Pmag            = primtrkC->Charge(iprim)*p;
      EeFit.Eta             = Eta;
      EeFit.zFit            = fitZ;
      if (eefit)   eefit->Fill(&EeFit.NoPrimaryTracks);
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
void StdEdxMaker::SortdEdx(Double_t *dEdxS, Int_t NPoints) {
  for (int i = 0; i< NPoints-1; i++) {
    for (int j=i+1; j< NPoints; j++) {
      if (dEdxS[i] > dEdxS[j]) {
        Double_t temp = dEdxS[i];
	dEdxS[i] = dEdxS[j];
	dEdxS[j] = temp;
      }
    }
  }
}
//________________________________________________________________________________
static Double_t Sirrf(Double_t poverm) {
  //        returns value of relative ionisation normalised to value
  //        at p/m=4 poverm    p/m (=beta gamma)             (input)
  Double_t dsri[40] = { 
    1.640, 1.410, 1.200, 1.070, 1.025,
    1.000, 1.000, 1.010, 1.030, 1.055,
    1.085, 1.115, 1.145, 1.175, 1.210,
    1.245, 1.275, 1.310, 1.340, 1.370,
    1.395, 1.420, 1.440, 1.460, 1.480,
    1.495, 1.510, 1.520, 1.530, 1.540,
    1.545, 1.550, 1.555, 1.560, 1.560,
    1.560, 1.560, 1.560, 1.560, 1.560};
  Double_t par[3] = {3.71529e-01, -1.72618e-01, -3.32974e-01};
  Double_t sirrf;
  if (poverm < 1.) {//  p/m less than 1 assume 1/beta**2 behaviour below table
    sirrf = dsri[0]*(1.+1./(poverm*poverm))/2.;
  }
  else {// interpolate table to get ri
    Double_t pt = 10.*TMath::Log10 (poverm);
    if (pt > 38.) pt = 38.; // if p/m above 10000 treat as saturated
    Int_t ipt = (int) pt;
    Double_t dpt = pt - ipt;
    sirrf = dsri[ipt] + (dsri[ipt+1] - dsri[ipt])*dpt;
    //    printf("Sirrf: pt = %f ipt = %i dpt = %f:%f %f \n",pt,ipt,dpt,dsri[ipt],dsri[ipt+1]);
  }
  //  printf("Sirrf: poverm = %f sirrf = %f\n",poverm,sirrf);
    Double_t S = par[0] + TMath::Log(sirrf);
    if (poverm < 1.) {
      Double_t x = TMath::Log10(poverm);
      S *= (1. + (par[1] + par[2]*x)*x);
    }
    Double_t si = TMath::Exp(S);
  return si;
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
void plotResults()
{
  if (fCanvas) {
    Double_t m, dm;//, s, ds;
    gMinuit->GetParameter(0, m, dm);
    //    gMinuit->GetParameter(1, s, ds);
    Int_t nBin = 20;
    Double_t minBin = -5, maxBin = 5;
    if (h1) delete h1;
    h1 = new TH1F("h1","de/dx of hit dist", nBin, minBin, maxBin);
    for (int i=0; i<NPoints; i++) {h1->Fill(((dEdxN[i]-m)*sqrt(dxN[i])));}
    h1->SetNormFactor(2.);
    
    fCanvas->cd();
    h1->Draw();
//     if (fLine) delete fLine;
//     fLine = new TLine(mean, 0., mean, NPoints/2.);
//     fLine->SetLineColor(2);
//     fLine->SetLineStyle(2);
//     fLine->Draw("same");
    if (!fFunc) {
      //      for (int k=0;k<NpGaus;k++) printf ("params[%i] = %f\n",k,parGaus[k]);
      fFunc = new TF1("g3us","gaus(0)+gaus(3)+gaus(6)+gaus(9)",minBin, maxBin);
      fFunc->SetParameters(parGaus);
    }
    fFunc->Draw("same");
    fCanvas->Update();
    
//     cout <<"Enter any integer to continue"<<endl;
//     int temp;
//     cin>>temp;
  }    
  return;
}
//________________________________________________________________________________
void fcn(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag)
{
  f = 0.;
  gin[0] = 0.;
  gin[1] = 0.;
  for (int i=0;i<NPoints; i++) {
    //    Double_t xx = (dEdxN[i]-par[0])/par[1]*sqrt(dxN[i]);
    Double_t xx = (dEdxL[i]-par[0])*sqrt(dxN[i]*par[1]);
    Double_t ff = fGaus(xx);
    Double_t sc = sqrt(par[1]*dxN[i]);
    Double_t val = ff*sc;
    f -= TMath::Log( val );
    Double_t gg = gGaus(xx);
    gin[0] += gg/ff*sc;
    gin[1] -= 0.5/par[1]*(xx*gg/ff+1.);
    //    printf ("pars %f %f xx = %f f=%f\n",par[0],par[1],xx,f);
  }
  //   printf ("par = %f %f iflag = %i  f = %f gin = %f\n",par[0],par[1],iflag,f,gin[0]);
  if (iflag == 3) { // terminating entry 
//     plotResults();
//     Int_t i;
//     cout << "Type integer " << endl;
//     cin >> i;
  }
}
//________________________________________________________________________________
void StdEdxMaker::DoFitZ(Double_t &chisq, Double_t &fitZ, Double_t &fitdZ)
{
  if (NPoints>10) {
    Double_t avz = 0;
    for (int i=0;i<NPoints;i++) {dEdxL[i] = TMath::Log(dEdxN[i]); avz += dEdxL[i];}
    avz /= NPoints;
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
    gMinuit->mnparm(1, "scale", 1.0, 0.01, 0.2, 20.0, ierflg);
    arglist[0] = 2;
    gMinuit->mnexcm("FIX",arglist,1,ierflg);
    
    arglist[0] = 1.;  
    gMinuit->mnexcm("SET GRAD",arglist,1,ierflg);
    //    gMinuit->mnexcm("SET STRAT",arglist,1,ierflg);
    arglist[0] = 500;
    arglist[1] = 1.;
    gMinuit->mnexcm("MIGRAD", arglist ,2,ierflg);
    //    gMinuit->mnexcm("HESSE  ",arglist,0,ierflg);
//     gMinuit->mnexcm("end",arglist,0,ierflg);
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









