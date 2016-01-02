#include <cmath>
#include "TH1F.h"
#include "TH2F.h"
#include "TFile.h"
#include "TString.h"
#include "../StPicoDstMaker/StPicoTrack.h"
#include "../StPicoDstMaker/StPicoEvent.h"
#include "../StPicoPrescales/StPicoPrescales.h"
#include "StPicoNpeEvent.h"
#include "StElectronPair.h"
#include "StCuts.h"

#include "StPicoNpeHists.h"

ClassImp(StPicoNpeHists)

//-----------------------------------------------------------------------
StPicoNpeHists::StPicoNpeHists(TString fileBaseName) : mPrescales(NULL), mOutFile(NULL),
mh1TotalEventsInRun(NULL), mh1TotalHftTracksInRun(NULL), mh1TotalGRefMultInRun(NULL),
mh2InvariantMassVsPt(NULL)
{
    mPrescales = new StPicoPrescales(cuts::prescalesFilesDirectoryName);
    
    mOutFile = new TFile(Form("%s.picoNpe.hists.root",fileBaseName.Data()),"RECREATE");
    
    int nRuns = mPrescales->numberOfRuns();
    TH1::SetDefaultSumw2();
    mh1TotalEventsInRun = new TH1F("mh1TotalEventsInRun","totalEventsInRun;runIndex;totalEventsInRun",nRuns+1,0,nRuns+1);
    mh1TotalHftTracksInRun = new TH1F("mh1TotalHftTracksInRun","totalHftTracksInRun;runIndex;totalHftTracksInRun",nRuns+1,0,nRuns+1);
    mh1TotalGRefMultInRun = new TH1F("mh1TotalGRefMultInRun","totalGRefMultInRun;runIndex;totalGRefMultInRun",nRuns+1,0,nRuns+1);
    
    mh1TotalElectronsInRun = new TH1F("mh1TotalElectronsInRun","totalElectronsInRun;runIndex;totalElectronsInRun",nRuns+1,0,nRuns+1);
    mh1TotalPartnersInRun = new TH1F("mh1TotalPartnersInRun","totalPartnersInRun;runIndex;totalPartnersInRun",nRuns+1,0,nRuns+1);
    mh1TotalPhECandidatesInRun = new TH1F("mh1TotalPhECandidatesInRun","totalPhECandidatesInRun;runIndex;totalPhECandidatesInRun",nRuns+1,0,nRuns+1);
    mh2NElectronsVsNPartners = new TH2F("mh2NElectronsVsNPartners","nElectronsVsNPartners;nPartners;nElectrons",500,0,500,200,0,200);
  
    mh2PairDcaVsPt = new TH2F("mh2PairDcaVsPt","PairDcaVsPt;p_{T}(Tagged e)(GeV/c);Pair DCA(cm))",120,0,12,100,0,1);
    mh2InvariantMassVsPt = new TH2F("mh2InvariantMassVsPt","InvariantMassVsPt;p_{T}(Tagged e)(GeV/c);Invarient Mass(GeV/c^2)",120,0,12,201,0,0.2);
    mh2ConversionPosition = new TH2F("mh2ConversionPosition","mh2ConversionPosition; x(cm); y(cm)",100,-10,10,100,-10,10);
    
    mh2PairDcaVsPtQaCut = new TH2F("mh2PairDcaVsPtQaCut","PairDcaVsPtQaCut;p_{T}(Tagged e)(GeV/c);Pair DCA(cm))",120,0,12,100,0,1);
    mh2InvariantMassVsPtQaCut = new TH2F("mh2InvariantMassVsPtQaCut","InvariantMassVsPtQaCut;p_{T}(Tagged e)(GeV/c);Invarient Mass(GeV/c^2)",120,0,12,201,0,0.2);
    mh2ConversionPositionQaCut = new TH2F("mh2ConversionPositionQaCut","mh2ConversionPositionQaCut; x(cm); y(cm)",100,-10,10,100,-10,10);
    
    
}
StPicoNpeHists::~StPicoNpeHists()
{
    delete mPrescales;
    // note that histograms are owned by mOutFile. They will be destructed
    // when the file is closed.
}
//-----------------------------------------------------------------------
void StPicoNpeHists::addEvent(StPicoEvent const& picoEvent,StPicoNpeEvent const & picoNpeEvent,unsigned int const nHftTracks)
{
    int runIndex = mPrescales->runIndex(picoNpeEvent.runId());
    mh1TotalEventsInRun->Fill(runIndex);
    mh1TotalHftTracksInRun->Fill(runIndex,nHftTracks);
    mh1TotalGRefMultInRun->Fill(runIndex,picoEvent.grefMult());
    mh1TotalElectronsInRun->Fill(runIndex,picoNpeEvent.nElectrons());
    mh1TotalPartnersInRun->Fill(runIndex,picoNpeEvent.nPartners());
    mh1TotalPhECandidatesInRun->Fill(runIndex,picoNpeEvent.nElectronPair());
    mh2NElectronsVsNPartners->Fill(picoNpeEvent.nPartners(),picoNpeEvent.nElectrons());
    
}
//---------------------------------------------------------------------
void StPicoNpeHists::addElectronPair(StElectronPair const* const epair, float electronPt, bool const fillMass)
{
    mh2PairDcaVsPt->Fill(electronPt,epair->pairDca());
    mh2InvariantMassVsPt->Fill(electronPt,epair->pairMass());
    mh2ConversionPosition->Fill(epair->positionX(),epair->positionY());
   if(fillMass) {
       mh2PairDcaVsPtQaCut->Fill(electronPt,epair->pairDca());
        mh2InvariantMassVsPtQaCut->Fill(electronPt,epair->pairMass());
        mh2ConversionPositionQaCut->Fill(epair->positionX(),epair->positionY());
    }
}//---------------------------------------------------------------------
void StPicoNpeHists::closeFile()
{
    mOutFile->cd();
    mh1TotalEventsInRun->Write();
    mh1TotalHftTracksInRun->Write();
    mh1TotalGRefMultInRun->Write();
    mh1TotalElectronsInRun->Write();
    mh1TotalPartnersInRun->Write();
    mh1TotalPhECandidatesInRun->Write();
    mh2NElectronsVsNPartners->Write();
    
    mh2PairDcaVsPt->Write();
    mh2InvariantMassVsPt->Write();
    mh2ConversionPosition->Write();

    mh2PairDcaVsPtQaCut->Write();
    mh2InvariantMassVsPtQaCut->Write();
    mh2ConversionPositionQaCut->Write();
    
    mOutFile->Close();
}
