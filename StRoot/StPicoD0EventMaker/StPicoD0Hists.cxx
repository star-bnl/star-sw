#include <cmath>
#include "TH1F.h"
#include "TH2F.h"
#include "TFile.h"
#include "TString.h"
#include "../StPicoDstMaker/StPicoEvent.h"
#include "../StPicoPrescales/StPicoPrescales.h"
#include "StPicoD0Event.h"
#include "StKaonPion.h"
#include "StCuts.h"

#include "StPicoD0Hists.h"

//-----------------------------------------------------------------------
StPicoD0Hists::StPicoD0Hists(TString fileBaseName) : mPrescales(NULL), mOutFile(NULL),
  mh1TotalEventsInRun(NULL), mh1TotalHftTracksInRun(NULL), mh1TotalGRefMultInRun(NULL),
  mh1TotalD0CandidatesInRun(NULL), mh2KaonDcaVsPt(NULL), mh2PionDcaVsPt(NULL), 
  mh2CosThetaVsPt(NULL), mh2DcaDaughtersVsPt(NULL),
  mh2InvariantMassVsPtUnlike(NULL), mh2InvariantMassVsPtLike(NULL)
{
  mPrescales = new StPicoPrescales(cuts::prescalesFilesDirectoryName);

  mOutFile = new TFile(Form("%s.picoD0.hists.root",fileBaseName.Data()),"RECREATE");

  int nRuns = mPrescales->numberOfRuns();
  TH1::SetDefaultSumw2();
  mh1TotalEventsInRun = new TH1F("mh1TotalEventsInRun","totalEventsInRun;runIndex;totalEventsInRun",nRuns+1,0,nRuns+1);
  mh1TotalHftTracksInRun = new TH1F("mh1TotalHftTracksInRun","totalHftTracksInRun;runIndex;totalHftTracksInRun",nRuns+1,0,nRuns+1);
  mh1TotalGRefMultInRun = new TH1F("mh1TotalGRefMultInRun","totalGRefMultInRun;runIndex;totalGRefMultInRun",nRuns+1,0,nRuns+1);
  mh1TotalKaonsInRun = new TH1F("mh1TotalKaonsInRun","totalKaonsInRun;runIndex;totalKaonsInRun",nRuns+1,0,nRuns+1);
  mh1TotalPionsInRun = new TH1F("mh1TotalPionsInRun","totalPionsInRun;runIndex;totalPionsInRun",nRuns+1,0,nRuns+1);
  mh1TotalD0CandidatesInRun = new TH1F("mh1TotalD0CandidatesInRun","totalD0CandidatesInRun;runIndex;totalD0CandidatesInRun",nRuns+1,0,nRuns+1);
  mh2NKaonsVsNPions = new TH2F("mh2NKaonsVsNPions","nKaonsVsNPions;nPions;nKaons",1000,0,1000,300,0,300);
  mh2KaonDcaVsPt = new TH2F("mh2KaonDcaVsPt","kaonDcaVsPt;p_{T}(K#pi)(GeV/c);K DCA(cm)",120,0,12,50,0,0.05);
  mh2PionDcaVsPt = new TH2F("mh2PionDcaVsPt","pionDcaVsPt;p_{T}(K#pi)(GeV/c);#pi DCA(cm)",120,0,12,50,0,0.05);
  mh2CosThetaVsPt = new TH2F("mh2CosThetaVsPt","cosThetaVsPt;p_{T}(K#pi)(GeV/c);cos(#theta)",120,0,12,500,0,1.0);
  mh2DcaDaughtersVsPt = new TH2F("mh2DcaDaughtersVsPt","dcaDaughtersVsPt;p_{T}(K#pi)(GeV/c);dcaDaughters(cm)",120,0,12,200,0,0.02);
  mh2InvariantMassVsPtUnlike = new TH2F("mh2InvariantMassVsPtUnlike","invariantMassVsPtUnlike;p_{T}(K#pi)(GeV/c);m_{K#pi}(GeV/c^{2})",120,0,12,220,0,2.2);
  mh2InvariantMassVsPtLike = new TH2F("mh2InvariantMassVsPtLike","invariantMassVsPtLike;p_{T}(K#pi)(GeV/c);m_{K#pi}(GeV/c^{2})",120,0,12,220,0,2.2);
}
StPicoD0Hists::~StPicoD0Hists()
{
  delete mPrescales;
  // note that histograms are owned by mOutFile. They will be destructed 
  // when the file is closed.
}
//-----------------------------------------------------------------------
void StPicoD0Hists::addEvent(StPicoEvent const& picoEvent,StPicoD0Event const & picoD0Event,unsigned int const nHftTracks)
{
  int runIndex = mPrescales->runIndex(picoD0Event.runId());
  mh1TotalEventsInRun->Fill(runIndex);
  mh1TotalHftTracksInRun->Fill(runIndex,nHftTracks);
  mh1TotalGRefMultInRun->Fill(runIndex,picoEvent.grefMult());
  mh1TotalKaonsInRun->Fill(runIndex,picoD0Event.nKaons());
  mh1TotalPionsInRun->Fill(runIndex,picoD0Event.nPions());
  mh1TotalD0CandidatesInRun->Fill(runIndex,picoD0Event.nKaonPion());
  mh2NKaonsVsNPions->Fill(picoD0Event.nPions(),picoD0Event.nKaons());
}
//---------------------------------------------------------------------
void StPicoD0Hists::addKaonPion(StKaonPion const* const kp, bool const fillMass, bool const unlike)
{
  if(unlike)
  {
    mh2KaonDcaVsPt->Fill(kp->pt(),kp->kaonDca());
    mh2PionDcaVsPt->Fill(kp->pt(),kp->pionDca());
    mh2CosThetaVsPt->Fill(kp->pt(),cos(kp->pointingAngle()));
    mh2DcaDaughtersVsPt->Fill(kp->pt(),kp->dcaDaughters());
  }

  if(fillMass)
  {
    if(unlike) mh2InvariantMassVsPtUnlike->Fill(kp->pt(),kp->m());
    else       mh2InvariantMassVsPtLike->Fill(kp->pt(),kp->m());
  }
}
//---------------------------------------------------------------------
void StPicoD0Hists::closeFile()
{
  mOutFile->cd();
  mh1TotalEventsInRun->Write();
  mh1TotalHftTracksInRun->Write();
  mh1TotalGRefMultInRun->Write();
  mh1TotalKaonsInRun->Write();
  mh1TotalPionsInRun->Write();
  mh1TotalD0CandidatesInRun->Write();
  mh2NKaonsVsNPions->Write();
  mh2KaonDcaVsPt->Write();
  mh2PionDcaVsPt->Write();
  mh2CosThetaVsPt->Write();
  mh2DcaDaughtersVsPt->Write();
  mh2InvariantMassVsPtUnlike->Write();
  mh2InvariantMassVsPtLike->Write();
  mOutFile->Close();
}
