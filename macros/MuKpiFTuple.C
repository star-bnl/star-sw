//#define GlobalTracks_type
#define __TCFIT__
#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include <stdio.h>
#include "TSystem.h"
#include "TMath.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TProfile.h"
#include "TStyle.h"
#include "TF1.h"
#include "TTree.h"
#include "TChain.h"
#include "TFile.h"
#include "TNtuple.h"
#include "TCanvas.h"
#include "TMinuit.h"
#include "TSpectrum.h"
#include "TString.h"
#include "TLine.h"
#include "TText.h"
#include "TROOT.h"
#include "TList.h"
#include "TPolyMarker.h"
#include "TBenchmark.h"
#include "StBichsel/Bichsel.h"
#include "BetheBloch.h"
#include "TDirIter.h"
#include "TTreeIter.h"
#include "TLorentzVector.h"
#include "TVector3.h"
#include "StMemStat.h"
#include "TStopwatch.h"
//#include "StThreeVectorF.hh"
#include "THelixTrack.h"
#include "TRVector.h"
#include "TRMatrix.h"
#include "TRSymMatrix.h"
#include "StDcaGeometry.h"
#include "array.h"
#include "constant.h"
#include "cuts.h"
#include "histoBook.h"
#include "StCloseFileOnTerminate.h"
#ifdef __TCFIT__
#include "StarRoot/TCFit.h"
#include "StarRoot/THelixTrack.h"
#endif
#include "KFParticle.h"
#include "MVertex.h"
#include "MTrack.h"
#include<exception>
#else
class TSystem;
class TMath;
class TH1;
class TH2;
class TH3;
class TProfile;
class TStyle;
class TF1;
class TTree;
class TChain;
class TFile;
class TNtuple;
class TCanvas;
class TMinuit;
class TSpectrum;
class TString;
class TLine;
class TText;
class TROOT;
class TList;
class TPolyMarker;
class Bichsel;
class BetheBloch;
class TDirIter;
class TTreeIter;
#endif
class Bichsel;
Bichsel *m_Bichsel = 0;
#define PrP(A)   if (_debug) {cout << "\t" << (#A) << " = \t" << ( A ) << endl;}
//________________________________________________________________________________
Bool_t isTrigger(UInt_t id, const UInt_t *ids) {
  for (UInt_t i = 0; i < NTriggerIDs; i++) {
    if (ids[i] && id == ids[i]) return kTRUE;
  }
  return kFALSE;
}
//________________________________________________________________________________
Int_t SelectGoodRuns(Int_t currRun)
{
  //cout <<" check bad run : " << currRun << endl;
  Int_t res =0;
  for(Int_t ii=0;ii<(sizeof(badrun)/sizeof(badrun[0]));++ii)
    {
      if(badrun[ii] == currRun)
	{
	  //cout <<" bad run found : badrun[ii] : "<< badrun[ii] << endl;
	  res=1;
	}
    }
  return res;
}
//__________________________________________________
void MuKpiFTuple(const Char_t *files="",
		 const Char_t* Out = "KFPARTICLE",
		 Char_t *decayMode = "D0_REAL_PASS3"
		 ) {
  if (!m_Bichsel) {
    gSystem->Load("StBichsel"); 
    gSystem->Load("StarClassLibrary");  
    m_Bichsel = Bichsel::Instance();
  }
  StMemStat memstat; 
  // get the files
  //Char_t * files = new Char_t[100];
  //sprintf(files,"%s%d%s","/star/data02/pwg/bouchet/FILES/mix_",dir,"_*.MuDst.root");
  //cout <<" dir is =" << files<< endl;
  
  TTreeIter iter("MuDst");
  Char_t *file1 = 0;
  Int_t NFiles = 0;
  TDirIter Dir(files);
  Char_t *file = 0;
  while ((file = (Char_t *) Dir.NextFile())) {iter.AddFile(file); NFiles++; file1 = file;}
  cout << files << "\twith " << NFiles << " files" << endl; 
  if (! file1 ) return;
  TString output(Out);

  cout << " Fitter  = "<< Out << endl;
  cout << " Input  = "<< decayMode << endl;
  output.ReplaceAll(".root","");
  //output += Form("%s.root",decayMode);
  if(refit==0)
    {
      cout <<" refit is off " << endl;
      output += Form("%s",".norefit_");
    }
  else
    {
      cout <<" refit is on " << endl;
      output += Form("%s",".refit_");
    }
  output += Form("%s.root",decayMode);
  //#endif
  cout << "Output for " << output << endl;
  const Int_t*&      RunId                  = iter("MuEvent.mRunInfo.mRunId");
  const Int_t*&      EventId                = iter("MuEvent.mEventInfo.mId"); 
  const Int_t*&      uTime                  = iter("MuEvent.mEventInfo.mTime"); 
  const Float_t*&    MuEvent_mRunInfo_mCenterOfMassEnergy     = iter("MuEvent.mRunInfo.mCenterOfMassEnergy");
#ifdef __TCFIT__
  const Double_t*&mMagneticFieldZ            = iter("MuEvent.mRunInfo.mMagneticFieldZ");
#endif
  const UInt_t*& mNTriggerId_mId             = iter(Form("MuEvent.mTriggerIdCollection.mNTriggerId.mId[%i]",NTriggerIDs));
  const Int_t    &NoPrimVertex               = iter("PrimaryVertices");
  const Float_t *&PrimVertexX                = iter("PrimaryVertices.mPosition.mX1");
  const Float_t *&PrimVertexY                = iter("PrimaryVertices.mPosition.mX2");
  const Float_t *&PrimVertexZ                = iter("PrimaryVertices.mPosition.mX3");
  const Float_t *&PrimVerSigX                = iter("PrimaryVertices.mPosError.mX1");
  const Float_t *&PrimVerSigY                = iter("PrimaryVertices.mPosError.mX2");
  const Float_t *&PrimVerSigZ                = iter("PrimaryVertices.mPosError.mX3");
  const Float_t *&PrimRanking                = iter("PrimaryVertices.mRanking");
  
  const Int_t    &NoTracks                   = iter("PrimaryTracks");
  const Int_t   *&PrimaryTracks_mVertexIndex = iter("PrimaryTracks.mVertexIndex");
  //   const UChar_t *&mNHitsFitInner             = iter("PrimaryTracks.mNHitsFitInner");
  //   const UChar_t *&mNHitsPossInner            = iter("PrimaryTracks.mNHitsPossInner");
  //   const UChar_t *&mNHitsFit                  = iter("PrimaryTracks.mNHitsFit");
  
  //   const Float_t *&pT                         = iter("PrimaryTracks.mPt");
  const Float_t *&pX                         = iter("PrimaryTracks.mP.mX1");
  const Float_t *&pY                         = iter("PrimaryTracks.mP.mX2");
  const Float_t *&pZ                         = iter("PrimaryTracks.mP.mX3");
  //   const Float_t *&Eta                        = iter("PrimaryTracks.mEta");
  //   const Float_t *&Phi                        = iter("PrimaryTracks.mPhi");
  const Int_t   *&mIndex2Global              = iter("PrimaryTracks.mIndex2Global");
  //   const Float_t*&    mDCA_mX1                   = iter("PrimaryTracks.mDCA.mX1");
  //   const Float_t*&    mDCA_mX2                   = iter("PrimaryTracks.mDCA.mX2");
  //   const Float_t*&    mDCA_mX3                   = iter("PrimaryTracks.mDCA.mX3");
  //   const Float_t*&    mDCAGlobal_mX1             = iter("PrimaryTracks.mDCAGlobal.mX1");
  //   const Float_t*&    mDCAGlobal_mX2             = iter("PrimaryTracks.mDCAGlobal.mX2");
  //   const Float_t*&    mDCAGlobal_mX3             = iter("PrimaryTracks.mDCAGlobal.mX3");
  
  // Global
  const Int_t    &NoTracksGl                   = iter("GlobalTracks");
  const UChar_t *&mNHitsFitInnerGl             = iter("GlobalTracks.mNHitsFitInner");
  const UChar_t *&mNHitsPossInnerGl            = iter("GlobalTracks.mNHitsPossInner");
  const UChar_t *&mNHitsFitGl                  = iter("GlobalTracks.mNHitsFitTpc");
  const UChar_t *&mNHitsPosGl                  = iter("GlobalTracks.mNHitsPossTpc");
  const Float_t *&pTGl                         = iter("GlobalTracks.mPt");
  const Float_t *&pXGl                         = iter("GlobalTracks.mP.mX1");
  const Float_t *&pYGl                         = iter("GlobalTracks.mP.mX2");
  const Float_t *&pZGl                         = iter("GlobalTracks.mP.mX3");
  const Float_t *&EtaGl                        = iter("GlobalTracks.mEta");
  const Float_t *&PhiGl                        = iter("GlobalTracks.mPhi");
  const Int_t*&      GlobalTracks_mIndex2Cov                  = iter("GlobalTracks.mIndex2Cov");
  const Float_t*&    CovGlobTrack_mImp                        = iter("CovGlobTrack.mImp");
  const Float_t*&    CovGlobTrack_mZ                          = iter("CovGlobTrack.mZ");
  const Float_t*&    CovGlobTrack_mPsi                        = iter("CovGlobTrack.mPsi");
  const Float_t*&    CovGlobTrack_mPti                        = iter("CovGlobTrack.mPti");
  const Float_t*&    CovGlobTrack_mTan                        = iter("CovGlobTrack.mTan");
  const Float_t*&    CovGlobTrack_mCurv                       = iter("CovGlobTrack.mCurv");
  const Float_t*&    CovGlobTrack_mImpImp                     = iter("CovGlobTrack.mImpImp");
  const Float_t*&    CovGlobTrack_mZImp                       = iter("CovGlobTrack.mZImp");
  const Float_t*&    CovGlobTrack_mZZ                         = iter("CovGlobTrack.mZZ");
  const Float_t*&    CovGlobTrack_mPsiImp                     = iter("CovGlobTrack.mPsiImp");
  const Float_t*&    CovGlobTrack_mPsiZ                       = iter("CovGlobTrack.mPsiZ");
  const Float_t*&    CovGlobTrack_mPsiPsi                     = iter("CovGlobTrack.mPsiPsi");
  const Float_t*&    CovGlobTrack_mPtiImp                     = iter("CovGlobTrack.mPtiImp");
  const Float_t*&    CovGlobTrack_mPtiZ                       = iter("CovGlobTrack.mPtiZ");
  const Float_t*&    CovGlobTrack_mPtiPsi                     = iter("CovGlobTrack.mPtiPsi");
  const Float_t*&    CovGlobTrack_mPtiPti                     = iter("CovGlobTrack.mPtiPti");
  const Float_t*&    CovGlobTrack_mTanImp                     = iter("CovGlobTrack.mTanImp");
  const Float_t*&    CovGlobTrack_mTanZ                       = iter("CovGlobTrack.mTanZ");
  const Float_t*&    CovGlobTrack_mTanPsi                     = iter("CovGlobTrack.mTanPsi");
  const Float_t*&    CovGlobTrack_mTanPti                     = iter("CovGlobTrack.mTanPti");
  const Float_t*&    CovGlobTrack_mTanTan                     = iter("CovGlobTrack.mTanTan");
  const Float_t*&    mDCAGlobal_mX1Gl              = iter("GlobalTracks.mDCAGlobal.mX1");
  const Float_t*&    mDCAGlobal_mX2Gl              = iter("GlobalTracks.mDCAGlobal.mX2");
  const Float_t*&    mDCAGlobal_mX3Gl              = iter("GlobalTracks.mDCAGlobal.mX3");
 const Float_t*&    GlobalTracks_mFirstPoint_mX1  = iter("GlobalTracks.mFirstPoint.mX1");
  const Float_t*&    GlobalTracks_mFirstPoint_mX2  = iter("GlobalTracks.mFirstPoint.mX2");
  const Float_t*&    GlobalTracks_mFirstPoint_mX3  = iter("GlobalTracks.mFirstPoint.mX3");
  const Short_t*&    QGl                           = iter("GlobalTracks.mHelix.mQ");
  const Int_t   *&NSigmaElectronGl             = iter("GlobalTracks.mNSigmaElectron");
  const Int_t   *&NSigmaPionGl                 = iter("GlobalTracks.mNSigmaPion");
  const Int_t   *&NSigmaKaonGl                 = iter("GlobalTracks.mNSigmaKaon");
  const Int_t   *&NSigmaProtonGl               = iter("GlobalTracks.mNSigmaProton");
  const Float_t*&    dEdxTrackLength           = iter("GlobalTracks.mProbPidTraits.mdEdxTrackLength");
  const Float_t*&    GdEdx                     = iter("GlobalTracks.mdEdx");
  
  TFile *fOut = new TFile(output,"recreate");
  TString Name;
  TString Title;
  // two particle plots
  struct Plot_t {
    const Char_t *Name;
    const Char_t *Title;
  };

//--------------------
//  init TrkTree  
//--------------------
  TTree *aTree = new TTree("DTree","DPLUS Reconstruction Tree");

  //aTree->SetAutoSave(1000000);
  aTree->Branch("event",&DTree.event,"event/I");
  aTree->Branch("PVX",&DTree.PVX,"PVX/F");
  aTree->Branch("PVY",&DTree.PVY,"PVY/F");
  aTree->Branch("PVZ",&DTree.PVZ,"PVZ/F");
  aTree->Branch("ResVX",&DTree.ResVX,"ResVZ/F");
  aTree->Branch("ResVY",&DTree.ResVY,"ResVY/F");
  aTree->Branch("ResVZ",&DTree.ResVZ,"ResVZ/F");
  aTree->Branch("index",&DTree.index,"index/I");
  aTree->Branch("NTrk",&DTree.NTrk,"NTrk/I");
  aTree->Branch("gRefMult",&DTree.gRefMult,"gRefMult/I");
  aTree->Branch("CpuTime",&DTree.CpuTime,"CpuTime/D");
  aTree->Branch("RealTime",&DTree.RealTime,"RealTime/D");
  aTree->Branch("mem",&DTree.mem,"mem/D");
  aTree->Branch("Candidates",&DTree.Candidates,"Candidates/I");
  aTree->Branch("TpcK",&DTree.TpcK,"TpcK[Candidates]/I");
  aTree->Branch("TpcPi1",&DTree.TpcPi1,"TpcPi1[Candidates]/I");
  aTree->Branch("SsdK",&DTree.SsdK,"SsdK[Candidates]/I");
  aTree->Branch("SsdPi1",&DTree.SsdPi1,"SsdPi1[Candidates]/I");
  aTree->Branch("SvtK",&DTree.SvtK,"SvtK[Candidates]/I");
  aTree->Branch("SvtPi1",&DTree.SvtPi1,"SvtPi1[Candidates]/I");
  aTree->Branch("DcaXYK",&DTree.DcaXYK,"DcaXYK[Candidates]/F");
  aTree->Branch("DcaXYPi1",&DTree.DcaXYPi1,"DcaXYPi1[Candidates]/F");
  aTree->Branch("DcaZK",&DTree.DcaZK,"DcaZK[Candidates]/F");
  aTree->Branch("DcaZPi1",&DTree.DcaZPi1,"DcaZPi1[Candidates]/F");
  aTree->Branch("Dca3dK",&DTree.Dca3dK,"Dca3dK[Candidates]/F");
  aTree->Branch("Dca3dPi1",&DTree.Dca3dPi1,"Dca3dPi1[Candidates]/F");
  aTree->Branch("SigmaDcaXYK",&DTree.SigmaDcaXYK,"SigmaDcaXYK[Candidates]/F");
  aTree->Branch("SigmaDcaXYPi1",&DTree.SigmaDcaXYPi1,"SigmaDcaXYPi1[Candidates]/F");
  aTree->Branch("SigmaDcaZK",&DTree.SigmaDcaZK,"SigmaDcaZK[Candidates]/F");
  aTree->Branch("SigmaDcaZPi1",&DTree.SigmaDcaZPi1,"SigmaDcaZPi1[Candidates]/F");
  aTree->Branch("EtaK",&DTree.EtaK,"EtaK[Candidates]/F");
  aTree->Branch("EtaPi1",&DTree.EtaPi1,"EtaPi1[Candidates]/F");
  aTree->Branch("RapK",&DTree.RapK,"RapK[Candidates]/F");
  aTree->Branch("RapPi1",&DTree.RapPi1,"RapPi1[Candidates]/F");
  aTree->Branch("dEdxK",&DTree.dEdxK,"dEdxK[Candidates]/F");
  aTree->Branch("dEdxPi1",&DTree.dEdxPi1,"dEdxPi1[Candidates]/F");
  aTree->Branch("SigmadEdxBK",&DTree.SigmadEdxBK,"SigmadEdxBK[Candidates]/F");
  aTree->Branch("SigmadEdxBPi1",&DTree.SigmadEdxBPi1,"SigmadEdxBPi1[Candidates]/F");
  aTree->Branch("MomK",&DTree.MomK,"MomK[Candidates]/F");
  aTree->Branch("MomPi1",&DTree.MomPi1,"MomPi1[Candidates]/F");
  aTree->Branch("PtK",&DTree.PtK,"PtK[Candidates]/F");
  aTree->Branch("PtPi1",&DTree.PtPi1,"PtPi1[Candidates]/F");
  aTree->Branch("PhiK",&DTree.PhiK,"PhiK[Candidates]/F");
  aTree->Branch("PhiPi1",&DTree.PhiPi1,"PhiPi1[Candidates]/F");
  aTree->Branch("KFX",&DTree.KFX,"KFX[Candidates]/F");
  aTree->Branch("KFY",&DTree.KFY,"KFY[Candidates]/F");
  aTree->Branch("KFZ",&DTree.KFZ,"KFZ[Candidates]/F");
  aTree->Branch("KFpX",&DTree.KFpX,"KFpX[Candidates]/F");
  aTree->Branch("KFpY",&DTree.KFpY,"KFpY[Candidates]/F");
  aTree->Branch("KFpZ",&DTree.KFpZ,"KFpZ[Candidates]/F");
  aTree->Branch("KFMass",&DTree.KFMass,"KFMass[Candidates]/F");
  aTree->Branch("KFErrMass",&DTree.KFErrMass,"KFErrMass[Candidates]/F");
  aTree->Branch("KFS",&DTree.KFS,"KFS[Candidates]/F");
  aTree->Branch("KFdS",&DTree.KFdS,"KFdS[Candidates]/F");
  aTree->Branch("KFProb",&DTree.KFProb,"KFProb[Candidates]/F");
  aTree->Branch("KFChi2",&DTree.KFChi2,"KFChi2[Candidates]/F");
  aTree->Branch("KFNDF",&DTree.KFNDF,"KFNDF[Candidates]/F");
  aTree->Branch("KFDecayLength",&DTree.KFDecayLength,"KFDecayLength[Candidates]/F");
  aTree->Branch("KFErrDecayLength",&DTree.KFErrDecayLength,"KFErrDecayLength[Candidates]/F");
  aTree->Branch("KFDecayLengthXY",&DTree.KFDecayLengthXY,"KFDecayLengthXY[Candidates]/F");
  aTree->Branch("KFErrDecayLengthXY",&DTree.KFErrDecayLengthXY,"KFErrDecayLengthXY[Candidates]/F");
  aTree->Branch("ChargeK",&DTree.ChargeK,"ChargeK[Candidates]/I");
  aTree->Branch("ChargePi1",&DTree.ChargePi1,"ChargePi1[Candidates]/I");
  aTree->Branch("KPID",&DTree.KPID,"KPID[Candidates]/I");
  aTree->Branch("KFP",&DTree.KFP,"KFP[Candidates]/F");
  aTree->Branch("KFpT",&DTree.KFpT,"KFpT[Candidates]/F");
  aTree->Branch("Angle",&DTree.Angle,"Angle[Candidates]/D");
  aTree->Branch("KFChi2Vertex",&DTree.KFChi2Vertex,"KFChi2Vertex[Candidates]/F");
  aTree->Branch("dcaxy1V",&DTree.dcaxy1V,"dcaxy1V[Candidates]/F");
  aTree->Branch("dcaxy2V",&DTree.dcaxy2V,"dcaxy2V[Candidates]/F");
  aTree->Branch("dca1V",&DTree.dca1V,"dca1V[Candidates]/F");
  aTree->Branch("dca2V",&DTree.dca2V,"dca2V[Candidates]/F");
  aTree->Branch("dca1Verr",&DTree.dca1Verr,"dca1Verr[Candidates]/F");
  aTree->Branch("dca2Verr",&DTree.dca2Verr,"dca2Verr[Candidates]/F");
  aTree->Branch("slength",&DTree.slength,"slength[Candidates]/F");
  aTree->Branch("dslength",&DTree.dslength,"dslength[Candidates]/F");
   // Fitter 
  Int_t NevProc   = 0;
  Int_t checkLoop = 0;
  Int_t totCand   = 0;
  /*
    #ifdef __TCFIT__
    TCFitV0 dat;
    TCFit tc("Fit decay length",&dat); 
    tc.SetDebug(0);
    #endif
  */
  MTrack track[3];
  KFParticle particle[5];// vertex + track1 + track2 + mother
  StCloseFileOnTerminate::Instantiate();
  TStopwatch timer1;
  timer1.Start(kTRUE);
  while (iter.Next()) { // Loop over events
    NevProc++;
    Double_t dEdxScale = 1;
    if (RunId[0] < 10000) {
      dEdxScale = TMath::Exp(7.81779499999999961e-01);
    }
    Int_t run = RunId[0];
    Int_t eveId = EventId[0];
    Int_t isGood  = SelectGoodRuns(RunId[0]);
    if(isGood==1)continue;    
    if (! (isTrigger(200001,mNTriggerId_mId) || isTrigger(200003,mNTriggerId_mId) || isTrigger(200013,mNTriggerId_mId))) continue;    
    Double_t HZ = 0.000299792458 * mMagneticFieldZ[0];
    KFParticle::SetField(mMagneticFieldZ[0]);
    for (Int_t l = 0; l < 1; l++) {
      if (l != 0) continue; // the best vertex is the first one
      if(PrimRanking[l]<0) continue;
      priVtxZ1->Fill(PrimVertexZ[l],l);
      priVtxZ2->Fill(PrimVertexZ[l],NoTracks);
      priVtxZ3->Fill(l,NoTracks);
      //cout << " l = " << l << " # of tracks = " << NoTracks <<" zvertex = " << PrimVertexZ[l] << endl;
      int mult = 0;
      //cout << " Number of prim Tracks = " << NoTracks << endl;
      if(!(TMath::Abs(PrimVertexX[l])<1.e-5 && TMath::Abs(PrimVertexY[l])<1.e-5 && TMath::Abs(PrimVertexZ[l])<1.e-5))
	{
	  for (Int_t kk = 0; kk<NoTracks; kk++) 
	    {
	      if (PrimaryTracks_mVertexIndex[kk] != l) continue;
	      Int_t kkk = mIndex2Global[kk];
	      if (kkk < 0 || kkk > NoTracksGl) continue;
	      Float_t dcaGlobal = TMath::Sqrt(mDCAGlobal_mX1Gl[kkk]*mDCAGlobal_mX1Gl[kkk] + mDCAGlobal_mX2Gl[kkk]*mDCAGlobal_mX2Gl[kkk] + mDCAGlobal_mX3Gl[kkk]*mDCAGlobal_mX3Gl[kkk]);
	      //printf(" TPC= %d Eta =%f dcaglobal=%f\n",mNHitsFitGl[kkk],EtaGl[kkk],dcaGlobal);
	      if (mNHitsFitGl[kkk] >= 10 && TMath::Abs(EtaGl[kkk])<.5 && dcaGlobal<3)
		mult++;
	    }
	}
      if (TMath::Abs(PrimVertexZ[l]) > zcut ) continue;
      DTree.event    = NevProc;
      DTree.PVX      = PrimVertexX[l];
      DTree.PVY      = PrimVertexY[l];
      DTree.PVZ      = PrimVertexZ[l];
      DTree.ResVX    = PrimVerSigX[l];
      DTree.ResVY    = PrimVerSigY[l];
      DTree.ResVZ    = PrimVerSigZ[l];
      DTree.index    = l;
      DTree.NTrk     = NoTracks;
      DTree.gRefMult = mult;

      Double_t eta_max = - TMath::Log(TMath::Tan(0.5*TMath::ATan2(R, L-PrimVertexZ[l])));
      Double_t eta_min = - TMath::Log(TMath::Tan(0.5*TMath::ATan2(R,-L-PrimVertexZ[l])));
      Double_t vtx[3] = {PrimVertexX[l], PrimVertexY[l], PrimVertexZ[l]};
      MVertex vert;
      vert.SetXYZ(PrimVertexX[l],PrimVertexY[l],PrimVertexZ[l]);
      //vert.SetXYZ(0.,0.,PrimVertexZ[l]); // track will be moved to (x,y)_Vertex
      Double_t CovVert[6] = {
	PrimVerSigX[l]*PrimVerSigX[l], 
	0, PrimVerSigY[l]*PrimVerSigY[l],
	0, 0, PrimVerSigZ[l]*PrimVerSigZ[l]};
      if (CovVert[0] < 1e-8) CovVert[0] = 1e-8;
      if (CovVert[2] < 1e-8) CovVert[2] = 1e-8;
      if (CovVert[5] < 1e-8) CovVert[5] = 1e-8;
      vert.SetCovarianceMatrix(CovVert);
      vert.SetNContributors(2);// changed to 3 contributors (=3 tracks)
      vert.SetChi2(1.01);
      
      particle[0] = KFParticle(vert);
      if (_debug > 2) {
	cout << "1. Construction from Vertex" << endl<< endl;
	cout << "	Vertex	Particle:";
	for(int i=0; i<3; i++)
	  cout <<"\tfP["<<i<<"]	"<< vert.GetParameter(i) << "	" << particle[0].Parameter(i);
	cout << endl;
	cout <<"fNDF	"<< 2*vert.GetNContributors()-3 << "	" << particle[0].GetNDF();
	cout <<"fChi2	"<< vert.GetChi2() << "	" << particle[0].GetChi2() << endl;
      }
      // pairs
    
      Int_t NoFSsdHits[2]={0,0};
      Int_t NoFSvtHits[2]={0,0};
      
      Int_t NoPSvtHits[2]={0,0};
      Int_t NoPSsdHits[2]={0,0};

      Double_t dcaXY[2]={0,0};
      Double_t dcaZ[2]={0,0};
      Double_t sigmaXY[2]={0,0};
      Double_t sigmaZ[2]={0,0};
      TVector3 p[4];
      TLorentzVector p4[4][2]; // K,pi; e,e;
      Int_t charge[2]={0,0};
      TVector3 dir[2];
      TVector3 dcaG[2];
      Double_t sigmadEdx[2]={0,0};
      Double_t dEdx[2]={0,0};
      TLorentzVector PP[4];
      Double_t dL=0;
      Double_t eL=0;
      Double_t slength=0;
      Double_t chisq=0;
      Double_t prob=0, probKF=0;
      Double_t dslength=0;
      int cand =0;
      Int_t NoTracksU = 0;
      for (Int_t k = 0; k<NoTracks; k++) { // <==== 1st particle K
	if (PrimaryTracks_mVertexIndex[k] != l) continue;
	Int_t kg = mIndex2Global[k];  // <==== index of global associated with the above primary track
       	if (kg < 0 || kg > NoTracksGl) continue;
	Float_t ratio= ((float)mNHitsFitGl[kg]/(float)mNHitsPosGl[kg]);
	if(pTGl[kg]<pTCut)continue;
	if(mNHitsFitGl[kg]<TpcCutMin)continue;
	if(mNHitsFitGl[kg]>TpcCutMax)continue;
	if(ratio<TpcRatioCutMin)continue;
	if(ratio>TpcRatioCutMax)continue;
	if(TMath::Abs(EtaGl[kg])>EtaCut) continue;
	if (dEdxTrackLength[kg] < TrackLengthCut) continue;
	NoFSvtHits[0] = (mNHitsFitInnerGl[kg] & 0x7);
	NoFSsdHits[0] = ((mNHitsFitInnerGl[kg]& 0x18) >> 3);
	
	if(NoFSvtHits[0] < SiCut ) continue;
	if(NoFSsdHits[0] > SsdCut) continue;
	if(NoFSvtHits[0] > SvtCut) continue;
	
	//Float_t radiusK  = TMath::Sqrt(GlobalTracks_mFirstPoint_mX1[kg]*GlobalTracks_mFirstPoint_mX1[kg]+GlobalTracks_mFirstPoint_mX2[kg]*GlobalTracks_mFirstPoint_mX2[kg]);
	//Bool_t conditionK1 = ((NoFSsdHits[0]+NoFSvtHits[0])==2 && radiusK<RadiusCutSvt1); 
	//Bool_t conditionK2 = ((NoFSsdHits[0]+NoFSvtHits[0])>2  && radiusK<RadiusCutSvt2); 
	//Bool_t conditionK3 = conditionK1 || conditionK2;

	//if(!conditionK3)continue;

	//if(QGl[kg]>0) continue;
	//cout << " KAON : ssd =" << NoFSsdHits[0] <<  " KAON : ist =" << NoFIstHits[0] <<  " KAON : pix =" << NoFPxlHits[0] << endl;
	p[0] = TVector3(pXGl[kg],pYGl[kg],pZGl[kg]);
	dir[0] = p[0].Unit();
	dEdx[0] = dEdxScale*GdEdx[kg];
	p4[0][0].SetVectMag(p[0],amK);
	p[2] = TVector3(pX[k],pY[k],pZ[k]);
	p4[0][1].SetVectMag(p[2],amK);
	sigmadEdx[0] = Bichsel::GetdEdxResolution(uTime[0], dEdxTrackLength[kg]);
	charge[0] = 0;
	if  (QGl[kg] < 0) charge[0] = 1;
	
	dcaG[0] = TVector3(mDCAGlobal_mX1Gl[kg],mDCAGlobal_mX2Gl[kg],mDCAGlobal_mX3Gl[kg]);
	
	Int_t kgc = GlobalTracks_mIndex2Cov[kg];
        static StDcaGeometry dcaGeometry;
	Double_t parsK[6] = {
	  CovGlobTrack_mImp[kgc],CovGlobTrack_mZ[kgc],CovGlobTrack_mPsi[kgc],
	  CovGlobTrack_mPti[kgc],CovGlobTrack_mTan[kgc],CovGlobTrack_mCurv[kgc]};
	Double_t errsK[15] = {
	  CovGlobTrack_mImpImp[kgc],
	  CovGlobTrack_mZImp[kgc],  CovGlobTrack_mZZ[kgc],
	  CovGlobTrack_mPsiImp[kgc],CovGlobTrack_mPsiZ[kgc],CovGlobTrack_mPsiPsi[kgc],
	  CovGlobTrack_mPtiImp[kgc],CovGlobTrack_mPtiZ[kgc],CovGlobTrack_mPtiPsi[kgc],CovGlobTrack_mPtiPti[kgc],
	  CovGlobTrack_mTanImp[kgc],CovGlobTrack_mTanZ[kgc],CovGlobTrack_mTanPsi[kgc],CovGlobTrack_mTanPti[kgc],CovGlobTrack_mTanTan[kgc]};
	
	static StDcaGeometry dcaGeometryC;
	Float_t parsKC[6] = {
	  CovGlobTrack_mImp[kgc],CovGlobTrack_mZ[kgc],CovGlobTrack_mPsi[kgc],
	  CovGlobTrack_mPti[kgc],CovGlobTrack_mTan[kgc],CovGlobTrack_mCurv[kgc]};
	Float_t errsKC[15] = {
	  CovGlobTrack_mImpImp[kgc],
	  CovGlobTrack_mZImp[kgc],  CovGlobTrack_mZZ[kgc],
	  CovGlobTrack_mPsiImp[kgc],CovGlobTrack_mPsiZ[kgc],CovGlobTrack_mPsiPsi[kgc],
	  CovGlobTrack_mPtiImp[kgc],CovGlobTrack_mPtiZ[kgc],CovGlobTrack_mPtiPsi[kgc],CovGlobTrack_mPtiPti[kgc],
	  CovGlobTrack_mTanImp[kgc],CovGlobTrack_mTanZ[kgc],CovGlobTrack_mTanPsi[kgc],CovGlobTrack_mTanPti[kgc],CovGlobTrack_mTanTan[kgc]};
	
	dcaGeometry.set(parsK, errsK);
        THelixTrack thelixK = dcaGeometry.thelix();
	
	dcaGeometryC.set(parsKC, errsKC);
	THelixTrack thelixKC = dcaGeometryC.thelix();
	
	thelixKC.Move(thelixKC.Path(vtx));
	dcaXY[0] = dcaGeometryC.impact();
	dcaZ[0]  = dcaGeometryC.z();
	
	Double_t ermx[3];
	thelixKC.Dca(vtx,dcaXY[0],dcaZ[0],ermx,2);
	sigmaXY[0]   = 0.0;
	sigmaZ[0]    = 0.0;
        if (ermx[0] > 0) sigmaXY[0] = TMath::Sqrt(ermx[0]);
        if (ermx[2] > 0) sigmaZ[0]  = TMath::Sqrt(ermx[2]);
	sigmaXY[0] = TMath::Sqrt(sigmaXY[0]*sigmaXY[0] + 
				 PrimVerSigX[l]*PrimVerSigX[l] +  
				 PrimVerSigY[l]*PrimVerSigY[l]);
	sigmaZ[0] = TMath::Sqrt(sigmaZ[0]*sigmaZ[0] + PrimVerSigZ[l]*PrimVerSigZ[l]);
	NoTracksU++;
	Float_t stkaon = dcaG[0].Mag()/sqrt(sigmaXY[0]*sigmaXY[0]+sigmaZ[0]*sigmaZ[0]);
	//if(stkaon<SigmaTCut) continue;
	//if(TMath::Abs(dcaXY[0]/sigmaXY[0])<SigmaTCut) continue;
	if(TMath::Abs(dcaXY[0])>DcaCutMax) continue;
	TRVector p1(6);
	TRSymMatrix C1(21);
	dcaGeometry.GetXYZ(p1.GetArray(),C1.GetArray());
	track[0].SetParameters(p1.GetArray());
	track[0].SetCovarianceMatrix(C1.GetArray());
	track[0].SetNDF(1);
	track[0].SetChi2(1.5);
	track[0].SetID(15);
	Int_t q   = 1;
	Int_t pdg = 321;
	if (charge[0]) {
	  q = -1;
	  pdg = -321;
	}
	track[0].SetCharge(q);
	particle[1] = KFParticle(track[0], pdg);
	if (_debug > 2) {
	  cout << "2. Construction from Track" << endl<< endl;
	  cout << "	Track	Particle";
	  for(int i=0; i<6; i++)
	    cout <<"\tfP["<<i<<"]	"<< track[0].GetParameter(i) << "	" << particle[1].Parameter(i);
	  cout << endl;
	}
	for (Int_t i = 0; i < NoTracks; i++) {// <==== 2nd particle pi
	  if (i ==  k) continue;
	  if (PrimaryTracks_mVertexIndex[i] != l) continue;
	  Int_t ig = mIndex2Global[i];
	  if (ig < 0 || ig > NoTracksGl) continue;
	  if (pTGl[ig] < pTCut) continue;
	  Float_t ratio= ((float)mNHitsFitGl[ig]/(float)mNHitsPosGl[ig]);
	  if(mNHitsFitGl[ig]<TpcCutMin)continue;
	  if(mNHitsFitGl[ig]>TpcCutMax)continue;
	  if(ratio<TpcRatioCutMin)continue;
	  if(ratio>TpcRatioCutMax)continue;
	  if(TMath::Abs(EtaGl[ig])>EtaCut) continue;
	  if (dEdxTrackLength[ig] < TrackLengthCut) continue;
	  NoFSvtHits[1] = (mNHitsFitInnerGl[ig] & 0x7);
	  NoFSsdHits[1] = ((mNHitsFitInnerGl[ig]& 0x18) >> 3);
	  if(NoFSvtHits[1] <  SiCut ) continue;
	  if(NoFSsdHits[1] >  SsdCut) continue;
	  if(NoFSvtHits[1] >  SvtCut) continue;
	  //Float_t radiusPi  = TMath::Sqrt(GlobalTracks_mFirstPoint_mX1[ig]*GlobalTracks_mFirstPoint_mX1[ig]+GlobalTracks_mFirstPoint_mX2[ig]*GlobalTracks_mFirstPoint_mX2[ig]);
	  //Bool_t conditionPi1 = ((NoFSsdHits[1]+NoFSvtHits[1])==2 && radiusPi<RadiusCutSvt1); 
	  //Bool_t conditionPi2 = ((NoFSsdHits[1]+NoFSvtHits[1])>2  && radiusPi<RadiusCutSvt2); 
	  //Bool_t conditionPi3 = conditionPi1 || conditionPi2;
	  
	  //if(!conditionPi3)continue;

	  p[1] = TVector3(pXGl[ig],pYGl[ig],pZGl[ig]);
	  //if((p[0].Mag()+p[1].Mag())< sumPCut) continue;
	  dir[1] = p[1].Unit();
	  dEdx[1] = dEdxScale*GdEdx[ig];
	  p4[1][0].SetVectMag(p[1],amPi);
	  p[3] = TVector3(pX[i],pY[i],pZ[i]);
	  p4[1][1].SetVectMag(p[3],amPi);
	  sigmadEdx[1] = Bichsel::GetdEdxResolution(uTime[0], dEdxTrackLength[ig]);
	  charge[1] = 0;
	  if  (QGl[ig] < 0) charge[1] = 1;
	  //if(QGl[kg]==QGl[ig]) continue;
	  dcaG[1] = TVector3(mDCAGlobal_mX1Gl[ig],mDCAGlobal_mX2Gl[ig],mDCAGlobal_mX3Gl[ig]);
	  Int_t igc = GlobalTracks_mIndex2Cov[ig];
	  static StDcaGeometry dcaGeometryPi;
	  Double_t parsPi[6] = {
	    CovGlobTrack_mImp[igc],CovGlobTrack_mZ[igc],CovGlobTrack_mPsi[igc],
	    CovGlobTrack_mPti[igc],CovGlobTrack_mTan[igc],CovGlobTrack_mCurv[igc]};
	  Double_t errsPi[15] = {
	    CovGlobTrack_mImpImp[igc],
	    CovGlobTrack_mZImp[igc],  CovGlobTrack_mZZ[igc],
	    CovGlobTrack_mPsiImp[igc],CovGlobTrack_mPsiZ[igc],CovGlobTrack_mPsiPsi[igc],
	    CovGlobTrack_mPtiImp[igc],CovGlobTrack_mPtiZ[igc],CovGlobTrack_mPtiPsi[igc],CovGlobTrack_mPtiPti[igc],
	    CovGlobTrack_mTanImp[igc],CovGlobTrack_mTanZ[igc],CovGlobTrack_mTanPsi[igc],CovGlobTrack_mTanPti[igc],CovGlobTrack_mTanTan[igc]};

	  static StDcaGeometry dcaGeometryPiC;
	  Float_t parsPiC[6] = {
	    CovGlobTrack_mImp[igc],CovGlobTrack_mZ[igc],CovGlobTrack_mPsi[igc],
	    CovGlobTrack_mPti[igc],CovGlobTrack_mTan[igc],CovGlobTrack_mCurv[igc]};
	  Float_t errsPiC[15] = {
	    CovGlobTrack_mImpImp[igc],
	    CovGlobTrack_mZImp[igc],  CovGlobTrack_mZZ[igc],
	    CovGlobTrack_mPsiImp[igc],CovGlobTrack_mPsiZ[igc],CovGlobTrack_mPsiPsi[igc],
	    CovGlobTrack_mPtiImp[igc],CovGlobTrack_mPtiZ[igc],CovGlobTrack_mPtiPsi[igc],CovGlobTrack_mPtiPti[igc],
	    CovGlobTrack_mTanImp[igc],CovGlobTrack_mTanZ[igc],CovGlobTrack_mTanPsi[igc],CovGlobTrack_mTanPti[igc],CovGlobTrack_mTanTan[igc]};

	  dcaGeometryPi.set(parsPi, errsPi);
	  THelixTrack thelixPi =  dcaGeometryPi.thelix();

	  dcaGeometryPiC.set(parsPiC, errsPiC);
	  THelixTrack thelixPiC = dcaGeometryPiC.thelix();
	  thelixPiC.Move(thelixPiC.Path(vtx));
	  dcaXY[1] = dcaGeometryPiC.impact();
	  dcaZ[1]  = dcaGeometryPiC.z();

	  Double_t ermxpi[3];
	  thelixPiC.Dca(vtx,dcaXY[1],dcaZ[1],ermxpi,2);
	  sigmaXY[1]   = 0;
	  sigmaZ[1]    = 0;
	  if (ermxpi[0] > 0) sigmaXY[1] = TMath::Sqrt(ermxpi[0]);
	  if (ermxpi[2] > 0) sigmaZ[1]  = TMath::Sqrt(ermxpi[2]);
	  sigmaXY[1] = TMath::Sqrt(sigmaXY[1]*sigmaXY[1] + 
				   PrimVerSigX[l]*PrimVerSigX[l] +  
				   PrimVerSigY[l]*PrimVerSigY[l]);
	  sigmaZ[1] = TMath::Sqrt(sigmaZ[1]*sigmaZ[1] + PrimVerSigZ[l]*PrimVerSigZ[l]);
	  Float_t stpion = dcaG[1].Mag()/sqrt(sigmaXY[1]*sigmaXY[1]+sigmaZ[1]*sigmaZ[1]);
	  //if(stpion<SigmaTCut) continue;
	  //if(TMath::Abs(dcaXY[1]/sigmaXY[1])<SigmaTCut) continue;
	  if(TMath::Abs(dcaXY[1])>DcaCutMax) continue;
	  TRVector p2(6);
	  TRSymMatrix C2(21);
	  dcaGeometryPi.GetXYZ(p2.GetArray(),C2.GetArray());
	  track[1].SetParameters(p2.GetArray());
	  track[1].SetCovarianceMatrix(C2.GetArray());
	  track[1].SetNDF(1);
	  track[1].SetChi2(1.5);
	  track[1].SetID(15);
	  q   = 1;
	  pdg = 211;
	  if (charge[1]) {
	    q = -1;
	    pdg = -211;
	  } 
	  track[1].SetCharge(q);
	  particle[2] = KFParticle(track[1], pdg);
	  if (_debug > 2) {
	    cout << "2. Construction from an other Track" << endl<< endl;
	    cout << "	Track	Particle" << endl;
	    for(int i=0; i<6; i++)
	      cout <<"\tfP["<<i<<"]	"<< track[1].GetParameter(i) << "	" << particle[2].Parameter(i) << endl;
	    cout << endl;
	    
	    for(int i=0; i<21; i++)
	      cout <<"fC["<<i<<"]	"<< track[1].GetCovariance(i) << "	" << particle[2].Covariance(i) << endl;
	    cout << endl;
	    cout <<"fQ	"<< track[1].Charge() << "	" << particle[2].GetQ() << endl;
	    cout <<"PDG	"<< "11" << endl;
	    double M, dM;
	    particle[2].GetMass(M,dM);
	    cout <<"Mass		" << M << endl;
	    cout << endl;
	  }
	  
 Double_t dEdx[2]   = {dEdxScale*GdEdx[kg],dEdxScale*GdEdx[ig]};
	  // nsigma from bichsel
	  
	  Double_t am[2] = {amK, amPi};
	  Double_t pIdB[2][2];
	  for(int mm=0;mm<2;mm++){
	    for(int nn=0;nn<2;nn++){
	      pIdB[mm][nn]=0.0;
	    }
	  }
	  
	  for (Int_t t = 0; t < 2; t++) {
	    for (Int_t h = 0; h < 2; h++) {
	      //double norm = TMath::Sqrt(p[t].x()*p[t].x() + p[t].y()*p[t].y()+p[t].z()*p[t].z());
	      Double_t bg = p[t].Mag()/am[h];
	      //Double_t bg = (double)norm/(double)am[h];
	      Double_t bg10 = TMath::Log10(bg);
	      Double_t dEdxP = 1.e-6*m_Bichsel->GetI70M(bg10,1,0);
	      pIdB[t][h] = TMath::Log(dEdx[t]/dEdxP)/sigmadEdx[t];
	    }
	  }
	  
	  Bool_t KaonPiD  = TMath::Abs(pIdB[0][0]) < SigmaKaonCut;
	  Bool_t PionPiD  = TMath::Abs(pIdB[1][1]) < SigmaPionCut;
	  Bool_t KPiPiD   = (KaonPiD && PionPiD);
	  if(!KPiPiD) continue;

	  const KFParticle pVertex = particle[0];
	  int NDaughters = 2;
	  const KFParticle *vDaughters[2] = {&particle[1],&particle[2]};
	  
	  KFParticle DP;
	  gBenchmark->Start("KFParticle");
	  DP.Construct(vDaughters,NDaughters,&pVertex,-1,0);
	  //https://www.gsi.de/documents/DOC-2010-Jun-126-1.pdf
	  //http://www.physi.uni-heidelberg.de/~minjung/alicegirl/analysis/GorbunovKFParticle.08.11.25.pdf
	  /*
	    1. Construct D0 
	    2. subtract daughters from primary vertex
	    3. Add mother particle to primary vertex to improve the primary vertex 
	    4. SetProductionVertex of mother particle to primary vertex --> mother particle is fully fitted
	    5. SetProductionVertex of daughter particles to primary vertex --> daughters are fully fitted
	  */
	  
	  if(refit==1)
	    {
	      //subtract daughters
	      
	      //particle[1].SubtractFromVertex(particle[0]);
	      //particle[2].SubtractFromVertex(particle[0]);
	      
	      /*
		particle[0]-=particle[1];
		particle[0]-=particle[2];
		particle[0]+=DP;
	      */
	      //add mother to PV
	      //cout <<" before refit : pos X = " << particle[0].GetX() << " pos Y = " << particle[0].GetY() <<" posZ = " << particle[0].GetZ() << endl;
	      //particle[0].AddDaughter(DP);// does not work
	      //cout <<" after refit : pos X = " << particle[0].GetX() << " pos Y = " << particle[0].GetY() <<" posZ = " << particle[0].GetZ() << endl;
	      //set productionVertex of mother
	      DP.SetProductionVertex(pVertex);
	      //set productionVertex of daughters
	      particle[1].SetProductionVertex(DP);
	      particle[2].SetProductionVertex(DP);
	    }
	  else
	    {
	      //particle[0].AddDaughter(DP);
	      //DP.SetProductionVertex(pVertex);
	    }
	  
	  gBenchmark->Stop("KFParticle");
	  probKF = TMath::Prob(DP.GetChi2(),DP.GetNDF());
	  if(probKF<ProbKFCut) continue;
	  if((DP.GetDecayLength()/DP.GetErrDecayLength())<KFDecayCut) continue;
	  if(DP.GetPt()< KFpTCut) continue;
	  //if(DP.GetChi2()> KFChiCut) continue;
	  if(DP.GetMass()< mKpipiMin || DP.GetMass() > mKpipiMax) continue;
	  if (_debug> 2) {
	    cout << "4.2 Construction without constrained Mass, with vertex hipotesis " << endl;
	    cout << "DP x,y,z =" << DP.GetX() << ","<< DP.GetY() << ","<< DP.GetZ() << ","
		 << "\tpx,y,z =" << DP.GetPx() << ","<< DP.GetPy() << ","<< DP.GetPz() << ","
		 << "\tM = "     << DP.GetMass() << " +/- " << DP.GetErrMass() 
		 << "\tS = " << DP.GetS() << " +/- " << DP.GetErrS() << endl;
	    cout << "chi2 = " << DP.GetChi2() << "\tNDF = " << DP.GetNDF() << "\tprob = " << probKF << endl;
	  }
	  //--------------
	  /*
	    TLorentzVector D0TCFIT;
	    tc.Reset(); dat.Reset();
	    for (Int_t ip = 0; ip < 2; ip++) 
	    {
	    dat.mTkBas[ip].Reset();
	    Double_t ptin = QGl[kg]/p[ip].Pt();
	    dat.mTkBas[ip].SetHz(1.); 	    
	    Double_t wk[9];
	    p[ip].GetXYZ(wk+3);
	    dcaG[ip].GetXYZ(wk+0);
	    THelixTrack th(wk+0,wk+3,ptin*HZ);
	    th.Backward();
	    double s =th.Path(0.,0.);
	    th.Move(s);th.Backward();
	    th.Eval(0.,wk,wk+3);
	    TVector3 pos(wk);
	    dir[ip] = TVector3(wk+3);
	    dat.mTkBas[ip].Reset();
	    dat.mTkBas[ip].Set(pos,dir[ip],ptin);
	    dat.mTkBas[ip].mass= (!ip)? amK:amPi;
	    dat.mTkBas[ip].Update();
	    Float_t *errs = &errsKC[0];
	    if (ip)  errs = &errsPiC[0];
	    for (Int_t n = 0; n < 5; n++) {
	    for (Int_t m = 0; m <= n; m++) {
	    Int_t nm = (n*(n+1))/2 + m;
	    dat.mTEBas[ip].Set(n,m,errs[nm]);
	    }
	    }
	    dat.mTkBas[0].SetHz(mMagneticFieldZ[0]/4.98478);   
	    dat.mTkBas[1].SetHz(mMagneticFieldZ[0]/4.98478);   
	    }
	    dat.Ready();
	    dat.FixPar(TCFitV0::kCNRJ);
	    tc.SetMaxIter(10);
	    if (tc.Fit()) {
	    #ifdef DEBUG
	    cout << "tc.Fit fails" << endl;
	    #endif
	    continue;
	    }
	    chisq = dat.GetFcn();  
	    prob = TMath::Prob(chisq,dat.GetNDF());
	    D0TCFIT = dat.mTkFit[0].P4()+dat.mTkFit[1].P4();
	    
	    slength = dat.GetPar(TCFitV0::kLEN_2); 
	    dslength= sqrt(dat.ErMx(TCFitV0::kLEN_2,TCFitV0::kLEN_2));
	  */
	  // THelixTrack 
	  //initialization for safety
	  double d1=0,d2=0;
	  double x1[3]={0,0,0},x2[3]={0,0,0};
	  d1  = thelixK.PathX(thelixPi,&d2);
	  thelixK.Eval(d1,x1);
	  thelixPi.Eval(d2,x2);
	  Float_t deltaX12  = x1[0] - x2[0];
	  Float_t deltaY12  = x1[1] - x2[1];
	  Float_t deltaZ12  = x1[2] - x2[2];
	  
	  //if(!KPiPiD) continue;
	  
	  double rapK   = 0.5 * TMath::Log((p4[0][0].E() + pZGl[kg])/(p4[0][0].E() - pZGl[kg]));
	  double rapPi1 = 0.5 * TMath::Log((p4[1][0].E() + pZGl[ig])/(p4[1][0].E() - pZGl[ig]));
	  
	  //here we calculate the angle between the Dplus flight line and its momentum
	  StThreeVectorD PV(PrimVertexX[l],PrimVertexY[l],PrimVertexZ[l]);
	  StThreeVectorD SEC(DP.GetX(),DP.GetY(),DP.GetZ());
	  StThreeVectorD DPMOM(DP.GetPx(),DP.GetPy(),DP.GetPz());
	  StThreeVectorD decay = SEC - PV;
	  TVector3 dec(0,0,0);
	  dec.SetX(decay.x());
	  dec.SetY(decay.y());
	  dec.SetZ(decay.z());
	  TVector3 ptot(0,0,0);
	  ptot.SetX(DPMOM.x());
	  ptot.SetY(DPMOM.y());
	  ptot.SetZ(DPMOM.z());
	  Double_t svtx[3] = {DP.GetX(),DP.GetY(),DP.Z()};
	  Double_t dcaXYV[2];
	  Double_t dcaZV[2];
	  Double_t ersx_trk1[3];
	  Double_t ersx_trk2[3];
	  
	  THelixTrack tK    = dcaGeometry.thelix();
	  THelixTrack tpi   = dcaGeometryPi.thelix();

	  tK.Move(tK.Path(svtx));
	  tpi.Move(tpi.Path(svtx));
	  
	  tK.Dca(svtx,dcaXYV[0],dcaZV[0],ersx_trk1,2);
	  tpi.Dca(svtx,dcaXYV[1],dcaZV[1],ersx_trk2,2);
	  Float_t dcav[2];
	  Float_t errdcav[2][2]; // track {{0,1},{xy,z}}
	  dcav[0] = sqrt(dcaXYV[0]*dcaXYV[0]+dcaZV[0]*dcaZV[0]);
	  dcav[1] = sqrt(dcaXYV[1]*dcaXYV[1]+dcaZV[1]*dcaZV[1]);
	  if (ersx_trk1[0] >0) errdcav[0][0] = sqrt(ersx_trk1[0]);
	  if (ersx_trk1[2] >0) errdcav[0][1] = sqrt(ersx_trk1[2]);
	  if (ersx_trk2[0] >0) errdcav[1][0] = sqrt(ersx_trk2[0]);
	  if (ersx_trk2[2] >0) errdcav[1][1] = sqrt(ersx_trk2[2]);

	  //float dca1v = sqrt(dcaXYV[0]*dcaXYV[0]+dcaZV[0]*dcaZV[0]);
	  //float dca2v = sqrt(dcaXYV[1]*dcaXYV[1]+dcaZV[1]*dcaZV[1]);
	  Float_t KFERR[3] = {DP.GetErrX(),DP.GetErrY(),DP.GetErrZ()};
	  errdcav[0][0] = sqrt(errdcav[0][0]*errdcav[0][0] + KFERR[0]*KFERR[0]+KFERR[1]*KFERR[1]);//trk1, xy
	  errdcav[0][1] = sqrt(errdcav[0][1]*errdcav[0][1] + KFERR[2]*KFERR[2]); // trk1 , Z
	  errdcav[1][0] = sqrt(errdcav[1][0]*errdcav[1][0] + KFERR[0]*KFERR[0]+KFERR[1]*KFERR[1]);// trk2, xy
	  errdcav[1][1] = sqrt(errdcav[1][1]*errdcav[1][1] + KFERR[2]*KFERR[2]); // trk2, Z
	  Float_t errdca1v  = sqrt(errdcav[0][0]*errdcav[0][0]+errdcav[0][1]*errdcav[0][1]); 
	  Float_t errdca2v  = sqrt(errdcav[1][0]*errdcav[1][0]+errdcav[1][1]*errdcav[1][1]); 
	  Bool_t  dca1vgood = dcav[0]<DcaVCut;
	  Bool_t  dca2vgood = dcav[1]<DcaVCut;
	  
	  Bool_t alldcav_good = (dca1vgood && dca2vgood);
	  if(alldcav_good == false) continue;
	  Double_t cosDP = dec.Angle(ptot);
	  
	  if(DEBUG ==1)
	    {
	      cout << "4.2 Construction without constrained Mass, with vertex hipotesis " << endl;
	      cout << "DP x,y,z =" << DP.GetX() << ","<< DP.GetY() << ","<< DP.GetZ() << ","
		   << "\tpx,y,z =" << DP.GetPx() << ","<< DP.GetPy() << ","<< DP.GetPz() << ","
		   << "\tM = "     << DP.GetMass() << " +/- " << DP.GetErrMass() 
		   << "\tS = " << DP.GetS() << " +/- " << DP.GetErrS() << endl;
	      cout << "chi2 = " << DP.GetChi2() << "\tNDF = " << DP.GetNDF() << "\tprob = " << probKF << endl;
	    }
	  //if (p[0].Mag() < 0.1 || p[1].Mag() < 0.1) continue;
	  Double_t LengthOverSigma = -99;
	  Double_t slengthKF = DP.GetS();
	  Int_t PID =0;
	  (KPiPiD)?PID=1:PID=0;	    
	  if(KPiPiD==0) continue;
	  DTree.TpcK[cand]      = (Int_t)mNHitsFitGl[kg];
	  DTree.TpcPi1[cand]    = (Int_t)mNHitsFitGl[ig];
	  DTree.SsdK[cand]      = (Int_t)NoFSsdHits[0];
	  DTree.SsdPi1[cand]    = (Int_t)NoFSsdHits[1];
	  DTree.SvtK[cand]      = (Int_t)NoFSvtHits[0];
	  DTree.SvtPi1[cand]    = (Int_t)NoFSvtHits[1];
	  DTree.DcaXYK[cand]    = dcaXY[0];
	  DTree.DcaXYPi1[cand]  = dcaXY[1];
	  DTree.DcaZK[cand]     = dcaZ[0];
	  DTree.DcaZPi1[cand]   = dcaZ[1];
	  DTree.Dca3dK[cand]    = dcaG[0].Mag();//sqrt(dcaXY[0]*dcaXY[0]+dcaZ[0]*dcaZ[0]);
	  DTree.Dca3dPi1[cand]  = dcaG[1].Mag();//sqrt(dcaXY[1]*dcaXY[1]+dcaZ[1]*dcaZ[1]);
	  DTree.SigmaDcaXYK[cand]   = sigmaXY[0];
	  DTree.SigmaDcaXYPi1[cand] = sigmaXY[1];
	  DTree.SigmaDcaZK[cand]   = sigmaZ[0];
	  DTree.SigmaDcaZPi1[cand] = sigmaZ[1];
	  DTree.EtaK[cand]     = EtaGl[kg];
	  DTree.EtaPi1[cand]   = EtaGl[ig];
	  DTree.RapK[cand]     = rapK;
	  DTree.RapPi1[cand]   = rapPi1;
	  DTree.dEdxK[cand]    = (TMath::Log10(dEdx[0])+6.);
	  DTree.dEdxPi1[cand]  = (TMath::Log10(dEdx[1])+6.);
	  DTree.SigmadEdxBK[cand]   = pIdB[0][0];
	  DTree.SigmadEdxBPi1[cand] = pIdB[1][1];
	  DTree.MomK[cand]      = p[0].Mag();
	  DTree.MomPi1[cand]    = p[1].Mag();
	  DTree.PtK[cand]       = pTGl[kg];
	  DTree.PtPi1[cand]     = pTGl[ig];
	  DTree.PhiK[cand]      = PhiGl[kg];
	  DTree.PhiPi1[cand]    = PhiGl[ig];
	  DTree.KFX[cand]       = DP.GetX();
	  DTree.KFY[cand]       = DP.GetY();
	  DTree.KFZ[cand]       = DP.GetZ();
	  DTree.KFpX[cand]      = DP.GetPx();
	  DTree.KFpY[cand]      = DP.GetPy();
	  DTree.KFpZ[cand]      = DP.GetPz();
	  DTree.KFMass[cand]    = DP.GetMass();
	  DTree.KFErrMass[cand] = DP.GetErrMass();
	  DTree.KFS[cand]       = DP.GetS();
	  DTree.KFdS[cand]      = DP.GetErrS();
	  DTree.KFProb[cand]    = probKF;
	  DTree.KFChi2[cand]    = DP.GetChi2();
	  DTree.KFNDF[cand]     = DP.GetNDF();
	  DTree.KFDecayLength[cand]      = DP.GetDecayLength();
	  DTree.KFErrDecayLength[cand]   = DP.GetErrDecayLength();
	  DTree.KFDecayLengthXY[cand]    = DP.GetDecayLengthXY();
	  DTree.KFErrDecayLengthXY[cand] = DP.GetErrDecayLengthXY();
	  DTree.ChargeK[cand]   = QGl[kg];
	  DTree.ChargePi1[cand] = QGl[ig];
	  DTree.KPID[cand]      = (Int_t)PID;
	  DTree.KFP[cand]       = DP.GetP();	      
	  DTree.KFpT[cand]      = DP.GetPt();
	  DTree.dcaXY12[cand]   = TMath::Sqrt(deltaX12*deltaX12 + deltaY12*deltaY12);
	  DTree.dcaXYZ12[cand]  = TMath::Sqrt(deltaX12*deltaX12 + deltaY12*deltaY12+ deltaZ12*deltaZ12);
	  //DTree.KFpTK[cand]     = particle[1].GetPt(); 
	  //DTree.KFpTPi1[cand]   = particle[2].GetPt(); 
	  //DTree.KFpTPi2[cand]   = particle[3].GetPt(); 
	  DTree.Angle[cand]     = cosDP;
	  DTree.KFChi2Vertex[cand] = DP.GetDeviationFromVertex(pVertex);
	  DTree.dcaxy1V[cand]    = dcaXYV[0];
	  DTree.dcaxy2V[cand]   = dcaXYV[1];
	  DTree.dca1V[cand]     = dcav[0];
	  DTree.dca2V[cand]     = dcav[1];
	  DTree.dca1Verr[cand]     = errdca1v;
	  DTree.dca2Verr[cand]     = errdca2v;
	  DTree.slength[cand]   = slength;
	  DTree.dslength[cand]  = dslength;
	  cand++;
	  totCand++;
	} //i tracks
      } //k tracks
      if(cand>0)
	{
	  timer1.Stop();
	  //cout << "--- End of event loop: " <<  sw.Print() << endl;
	  //cout <<" Tree filled " << endl;
	  DTree.RealTime   = TStopwatch::GetRealTime();
	  DTree.CpuTime    = TStopwatch::GetCPUTime();
	  DTree.Candidates = cand;
	  DTree.mem        = memstat.Used();
	  timer1.Continue();
	  aTree->Fill();
	  //aTree->Reset(); 
	}
    }// l vertex
    if (NevProc%50 == 0) {
      cout << NevProc << "\tevents processed so far" <<endl;
      cout<<"after reading b: memory used: "<<memstat.Used()<<endl;
      //cout << " check Loop = " << checkLoop << endl;
      //      gBenchmark->Show("TCFit");
      //      gBenchmark->Show("KFParticle");
      }
  }
  if(writeHisto)
    {
      priVtxZ1->Write();
      priVtxZ2->Write();
      priVtxZ3->Write();
    }
  fOut->Write();
  delete fOut;
}
