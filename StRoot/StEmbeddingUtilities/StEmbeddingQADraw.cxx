/****************************************************************************************************
 * $Id: StEmbeddingQADraw.cxx,v 1.36 2019/07/10 05:45:34 zhux Exp $
 * $Log: StEmbeddingQADraw.cxx,v $
 * Revision 1.36  2019/07/10 05:45:34  zhux
 * added option for btof pid for primary real tracks
 *
 * Revision 1.35  2015/04/19 01:46:59  zhux
 * Wrong particle name (under sl64) is fixed in ::getParticleName().
 *
 * Revision 1.34  2013/03/20 01:08:39  huck
 * use mu- to compare to pi-
 *
 * Revision 1.33  2011/04/26 20:27:45  hmasui
 * Add gamma geantid check
 *
 * Revision 1.32  2011/04/12 03:01:04  hmasui
 * Fix isMatchedPairOk() to properly process particles with decay daughters
 *
 * Revision 1.31  2011/04/07 02:29:57  hmasui
 * Print (pseudo-)rapidity up to 2nd digit string
 *
 * Revision 1.30  2011/04/06 20:01:29  hmasui
 * Put back Ncommon vs NHitFit histogram for all pt, and fix a bug for name of projected histograms for Ncommon vs NHitFit
 *
 * Revision 1.29  2011/04/05 04:26:42  hmasui
 * Bug fix for printing trigger id's, and follow default ROOT color scheme
 *
 * Revision 1.28  2011/04/01 05:07:07  hmasui
 * Added track selections in the 2nd page, Ncommon vs NhitFit (pt dependent), and 1/pt(RC)-1/pt(MC) vs pt plot
 *
 * Revision 1.27  2011/02/25 18:34:40  hmasui
 * Add phi comparison between real and reconstructed primary (matched pairs) in embedding
 *
 * Revision 1.26  2011/02/11 03:44:53  hmasui
 * Draw error messages in pdf if histogram is missing. Add error check for Ncommon histogram
 *
 * Revision 1.25  2011/01/31 21:33:53  hmasui
 * Add setParentGeantId() function to allow the multiple decays
 *
 * Revision 1.24  2010/08/04 21:16:06  hmasui
 * Replace geantid to nsigma cut for real tracks in legend
 *
 * Revision 1.23  2010/08/03 23:39:35  hmasui
 * Fix a bug in SetMaximum() function for MC rapidity distribution
 *
 * Revision 1.22  2010/07/12 21:31:49  hmasui
 * Use StEmbeddingQAUtilities::getParticleDefinition() instead of StParticleTable
 *
 * Revision 1.21  2010/06/28 21:29:41  hmasui
 * Fixed the multiplicity x-axis range for all branches
 *
 * Revision 1.20  2010/06/22 16:31:21  hmasui
 * Separate 2D and 1D QA for MC tracks. Add pol0 fit for MC eta, y and phi distributions.
 *
 * Revision 1.19  2010/06/10 14:51:25  hmasui
 * Added legend for each page
 *
 * Revision 1.18  2010/05/27 16:29:00  hmasui
 * Remove / character from particle name
 *
 * Revision 1.17  2010/05/14 19:51:45  hmasui
 * Modify the text size for Nevts, MC particle name etc to fit the window
 *
 * Revision 1.16  2010/04/24 19:50:56  hmasui
 * Optimize to draw run number list, and fix bugs for maximum of y-axis in several histograms
 *
 * Revision 1.15  2010/04/07 19:45:12  hmasui
 * Use box option for dE/dx vs p to reduce the pdf file size
 *
 * Revision 1.14  2010/03/15 21:05:24  hmasui
 * Separate MC vertices QA into 2 pages. Added constraint on z-vertex cut for vx(vy) vs vz histograms.
 *
 * Revision 1.13  2010/02/24 18:13:23  hmasui
 * Modify color code explanation more explicitly. Comparison of phi distributions between reconstructed embedding with MC tracks
 *
 * Revision 1.12  2010/02/23 16:56:47  hmasui
 * Add phi distributions QA (MC vs reconstructed)
 *
 * Revision 1.11  2010/02/19 18:07:45  hmasui
 * Divide runlist into two different pads
 *
 * Revision 1.10  2010/02/16 02:14:03  hmasui
 * Print PDF file only for all QA plots
 *
 * Revision 1.9  2010/01/28 21:51:24  hmasui
 * Add Vx vs Vz and Vy vs Vz histograms in the event-wise QA
 *
 * Revision 1.8  2010/01/26 17:50:54  hmasui
 * Fix geantidFound to match the correct geant id. Use number of accepted events, not number of all events without vertex cuts. Add QA for eventid, runid and # of particles per event
 *
 * Revision 1.7  2009/12/22 21:40:09  hmasui
 * Add comments for functions and members
 *
 ****************************************************************************************************/

#include <assert.h>
#include <string>

#include "TCanvas.h"
#include "TError.h"
#include "TF1.h"
#include "TFile.h"
#include "TGraphErrors.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TLatex.h"
#include "TLegend.h"
#include "TMath.h"
#include "TObject.h"
#include "TPaveText.h"
#include "TPDF.h"
#include "TProfile.h"
#include "TROOT.h"
#include "TStyle.h"
#include "TSystem.h"

#include "StEmbeddingQADraw.h"
#include "StEmbeddingQAUtilities.h"
#include "StMessMgr.h"
#include "StParticleDefinition.hh"

using namespace std ;

ClassImp(StEmbeddingQADraw)

  /// Initialize static data members
  UInt_t StEmbeddingQADraw::mCanvasId = 0;

//____________________________________________________________________________________________________
StEmbeddingQADraw::StEmbeddingQADraw(const TString embeddingFile, const TString realDataFile, const Int_t geantid,
    const Bool_t isEmbeddingOnly)
  : mIsEmbeddingOnly(isEmbeddingOnly), mIsGIFOn(kFALSE), mIsJPGOn(kFALSE), mIsEPSOn(kFALSE), mIsPSOn(kFALSE), mGeantId(geantid)
{
  /// Constructor for StEmbeddingQADraw
  mCanvas         = 0 ;
  mMainPad        = 0 ;
  mPDF            = 0 ;

  mInputEmbedding = 0 ;
  mInputRealData  = 0 ;

  /// Open input files, see Bool_t open(const TString embeddingFile, const TString realDataFile)
  mIsOpen = open(embeddingFile, realDataFile);

  /// Get year and production from input file
  if(mIsOpen){
    TString fileName(embeddingFile);
    fileName.Remove(fileName.Index(".root"), fileName.Length()); // remove .root
 
    for(Int_t i=0;i<3;i++){
      const Int_t start = 0 ;
      const Int_t stop  = fileName.First("_") ;
      const TString subString(fileName(start, stop));
 
      fileName.Remove(start, stop+1);
 
      if( i == 2 ){
        mYear = subString.Atoi(); // Convert character to integer
        mProduction = fileName ;
      }
    }
  }
  else{
    /// Do not define the year and production if input files have not been opened properly
    mYear = -10 ;
    mProduction = "";
  }

  mInputParentGeantId = 0 ;
}

//____________________________________________________________________________________________________
StEmbeddingQADraw::StEmbeddingQADraw(const TString embeddingFile, const TString realDataFile,
    const Int_t year, const TString production, const Int_t geantid, const Bool_t isEmbeddingOnly)
  : mIsEmbeddingOnly(isEmbeddingOnly), mIsGIFOn(kFALSE), mIsJPGOn(kFALSE), mIsEPSOn(kFALSE), mIsPSOn(kFALSE), mGeantId(geantid)
{
  /// Constructor for StEmbeddingQADraw
  /// Define year and production from the input arguments

  /// Open input files, see Bool_t open(const TString embeddingFile, const TString realDataFile)
  mIsOpen = open(embeddingFile, realDataFile);

  /// Year, production and particle name by hand
  mYear         = year ;
  mProduction   = production ;

  mInputParentGeantId = 0 ;
}

//____________________________________________________________________________________________________
StEmbeddingQADraw::~StEmbeddingQADraw()
{
  /// Destructor

  /// Clear MC and daughter geantid array
  mMcGeantId.clear();
  mDaughterGeantId.clear();
  mParentGeantId.clear();
  mParentParentGeantId.clear();

  /// Close input files
  if( mInputEmbedding || mInputEmbedding->IsOpen() ) mInputEmbedding->Close();
  if( mInputRealData || mInputRealData->IsOpen() ) mInputRealData->Close();
}

//____________________________________________________________________________________________________
void StEmbeddingQADraw::setParentGeantId(const Int_t parentgeantid)
{
  mInputParentGeantId = parentgeantid ;
  LOG_INFO << "StEmbeddingQADraw::setParentGeantId  Set parent geantid = "
    << mInputParentGeantId
    << endm;
}

//____________________________________________________________________________________________________
void StEmbeddingQADraw::setPtMax(const Double_t ptmax)
{ 
  mPtMax = ptmax ;
  LOG_INFO << "StEmbeddingQADraw::setPtMax()  Set maximum pt = " << mPtMax
    << " GeV/c to be drawn" << endm;
}


//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::open(const TString embeddingFile, const TString realDataFile)
{
  /// Open input embedding and real data files

  // OPEN embedding file
  mInputEmbedding = TFile::Open(embeddingFile);

  if(!mInputEmbedding || !mInputEmbedding->IsOpen() || mInputEmbedding->IsZombie() ){
    Error("StEmbeddingQADraw", "can't find %s", embeddingFile.Data());
    return kFALSE;
  }
  LOG_INFO << "OPEN input (embedding) : " << mInputEmbedding->GetName() << endm;
  LOG_INFO << endm;

  /// Do not open the real data file if 'mIsEmbeddingOnly' flag is true
  if( mIsEmbeddingOnly ) return kTRUE ;

  // OPEN real data file
  mInputRealData = TFile::Open(realDataFile);

  if(!mInputRealData || !mInputRealData->IsOpen() || mInputRealData->IsZombie() ){
    Error("StEmbeddingQADraw", "can't find %s", realDataFile.Data());
    return kFALSE;
  }
  LOG_INFO << "OPEN input (real data) : " << mInputRealData->GetName() << endm;
  LOG_INFO << endm;

  return kTRUE ;
}


//____________________________________________________________________________________________________
void StEmbeddingQADraw::init()
{
  /// Initialization of data members

  /// Set maximum pt. Default is 5 GeV/c --> Draw histograms up to 5 GeV/c
  mPtMax = 5.0;

  /// Clear MC and daughter geantid array
  mMcGeantId.clear();
  mDaughterGeantId.clear();
  mParentGeantId.clear();
  mParentParentGeantId.clear();

  LOG_INFO << "#------------------------------------------------------------" << endm;
  LOG_INFO << Form("  Year       =  %10d", mYear) << endm;
  LOG_INFO << Form("  Production =  %10s", mProduction.Data()) << endm;
  LOG_INFO << Form("  Particle   =  %10s", getParticleName()) << endm;
  LOG_INFO << "#------------------------------------------------------------" << endm;

  /// set global ROOT styles
  StEmbeddingQAUtilities::instance()->setStyle();

  /// Make sure that we have histograms for the input geant id
  checkInputGeantId() ;

  /// Find daughter geant id from Contaminated pair histograms 
  setDaughterGeantId();

  /// Initialize canvas
  LOG_INFO << "Initialize canvas" << endm ;
  mCanvas = new TCanvas(Form("c%d", mCanvasId), Form("c%d", mCanvasId++), 700, 800);
  mCanvas->SetFillColor(1);
  mCanvas->cd();

  /// Initialize main pad
  mMainPad = new TPad("mainPad", "", 0.00, 0.00, 1.00, 0.90);
  mMainPad->SetFillColor(10);
  mMainPad->Draw();

  /// Initialize PDF file
  mPDF = new TPDF(Form("%sqa_%s.pdf", mOutputFigureDirectory.Data(), getBaseName()));
  LOG_INFO << "Initialize PDF file : " << mPDF->GetName() << endm ;

  /// Draw # of events, input MC particle name, library and year
  mCanvas->cd();
  TPaveText* info = new TPaveText(0.00, 0.95, 0.63, 1.00);
  info->SetBorderSize(1);
  info->SetFillColor(kBlack);
  info->SetTextFont(42);
  info->SetTextColor(10);
  info->SetTextSize(0.030);

  const Int_t nevents = getEntries() ;
  TString neventsName(Form("%d", getEntries())) ;
  if( nevents > 1000000 )    neventsName = Form("%1.2f M", (Double_t)nevents/1.0e+06);
  else if ( nevents > 1000 ) neventsName = Form("%1.2f K", (Double_t)nevents/1.0e+03);

  info->AddText(Form("N_{events}=%s, MC=%s, %s, (%d)", neventsName.Data(), getParticleName(), mProduction.Data(), mYear));
  info->Draw();

  /// First page
  mMainPad->cd();

  TLatex* title = new TLatex(0.1, 0.6, "Embedding QA");
  title->SetTextSize(0.10);
  title->Draw();

  TString qaName("embedding + real");
  if ( mIsEmbeddingOnly ){
    qaName = "embedding only";
  }

  TLatex* qaTitle = new TLatex(0.12, 0.5, qaName);
  qaTitle->SetTextSize(0.07);
  qaTitle->Draw();

  /// Red (embedding)
  /// Blue and black (real)
  TLine* lineEmbedding = new TLine(0, 0, 1, 1);
  TLine* lineRealBlue  = new TLine(0, 0, 1, 1);
  TLine* lineRealBlack = new TLine(0, 0, 1, 1);
  lineEmbedding->SetLineColor(kRed);    lineEmbedding->SetLineWidth(2);
  lineRealBlue ->SetLineColor(kBlue);   lineRealBlue ->SetLineWidth(2);
  lineRealBlack->SetLineColor(kBlack);  lineRealBlack->SetLineWidth(2);

  TLegend* colorCode = new TLegend(0.05, 0.15, 0.8, 0.45);
  colorCode->SetTextSize(0.035);
  colorCode->SetFillColor(10);
  colorCode->SetHeader("Color code for embedding and real data");
  colorCode->AddEntry(lineRealBlack, "MC (black)", "L");
  colorCode->AddEntry(lineEmbedding, "Reconstructed embedding tracks* (red)", "L");
  colorCode->AddEntry(lineRealBlue,  "Real** (blue)", "L");
  colorCode->Draw();

  TLatex* note0 = new TLatex(0.1, 0.10, "* matched pairs or contaminated pairs");
  TLatex* note1 = new TLatex(0.1, 0.05, "** black is also used, see legend for each pad");
  note0->SetTextSize(0.03);
  note1->SetTextSize(0.03);
  note0->Draw();
  note1->Draw();

  mCanvas->cd();
  mCanvas->Update();

  mPDF->NewPage() ;

  // Print event and track selections
  const StEmbeddingQAUtilities* utility = StEmbeddingQAUtilities::instance() ;
  TPaveText* header = initCanvas("Event & track selections");

  // Event selections
  TPaveText* eventSelections = new TPaveText(0.05, 0.60, 0.95, 0.92);
  eventSelections->SetTextAlign(12);
  eventSelections->SetBorderSize(1);
  eventSelections->SetFillColor(10);
  eventSelections->SetTextColor(kBlack);
  eventSelections->SetTextSize(0.030);
  eventSelections->SetTextFont(42);

  eventSelections->AddText("*** Event selections");
  // z-vertex cut
  eventSelections->AddText(Form("  z-vertex cut   : |v_{z}| < %1.1f cm", utility->getZVertexCut()));
  // trigger id's
  const vector<UInt_t> triggerId(utility->getTriggerIdCut());
  if ( !triggerId.empty() ) {
    TString triggers("");
    for(UInt_t i=0; i<triggerId.size(); i++) {
      triggers += Form("%d", triggerId[i]);
      if( i != triggerId.size() - 1 ) triggers += ", ";
    }
    eventSelections->AddText(Form("  trigger id cut : id = %s", triggers.Data()));
  }
  eventSelections->AddText("NOTE: Trigger id cut for real data has to be made manually in doEmbeddingQAMaker.C");
  eventSelections->SetAllWith("***", "color", kRed);
  eventSelections->SetAllWith("***", "font", 72);
  eventSelections->SetAllWith("***", "size", 0.033);
  eventSelections->SetAllWith("NOTE", "color", kBlue);
  eventSelections->SetAllWith("NOTE", "font", 72);
  eventSelections->SetAllWith("NOTE", "size", 0.020);
  eventSelections->Draw();

  // Track selections
  TPaveText* trackSelections = new TPaveText(0.05, 0.05, 0.95, 0.59);
  trackSelections->SetTextAlign(12);
  trackSelections->SetBorderSize(1);
  trackSelections->SetFillColor(10);
  trackSelections->SetTextColor(kBlack);
  trackSelections->SetTextSize(0.030);
  trackSelections->SetTextFont(42);
  trackSelections->AddText("*** Track selections");
  trackSelections->AddText(Form("  %1.1f < p_{T} < %1.1f GeV/c", utility->getPtMinCut(), utility->getPtMaxCut())); // pt cut
  trackSelections->AddText(Form("  |#eta| < %1.2f", utility->getEtaCut()));
  trackSelections->AddText(Form("  |y| < %1.2f", utility->getRapidityCut()));
  trackSelections->AddText(Form("  nHitsFit > %3d", utility->getNHitCut()));
  trackSelections->AddText(Form("  nHitsFit/nHitsPoss > %1.2f", utility->getNHitToNPossCut()));
  trackSelections->AddText(Form("  global Dca < %1.1f cm", utility->getDcaCut()));
  trackSelections->AddText(Form("  |n#sigma| < %1.1f, using %s", utility->getNSigmaCut(),utility->getBTofPid()?"BTOF":"TPC dE/dx"));
  trackSelections->AddText("NOTE1: Rapidity cut for real data has to be made manually in doEmbeddingQAMaker.C");
  trackSelections->AddText("NOTE2: Cut on its own variable is currently disabled, e.x. no dca cut for dca histogram");
  trackSelections->SetAllWith("***", "color", kRed);
  trackSelections->SetAllWith("***", "font", 72);
  trackSelections->SetAllWith("***", "size", 0.033);
  trackSelections->SetAllWith("NOTE", "color", kBlue);
  trackSelections->SetAllWith("NOTE", "font", 72);
  trackSelections->SetAllWith("NOTE", "size", 0.020);
  trackSelections->Draw();

  mCanvas->cd();
  mCanvas->Update();
  mPDF->NewPage() ;

  delete header ;
}

//____________________________________________________________________________________________________
void StEmbeddingQADraw::setOutputDirectory(const TString name)
{
  /// Set output directory

  mOutputFigureDirectory = name ;

  /// Make sure that the directory exists or not
  if( gSystem->AccessPathName(name) == kTRUE ){ // 0 is true, i.e. directory exists
    Error("setOutputDirectory", "Directory %s does not exist. Set current directory as the output location");
    mOutputFigureDirectory = "./";
  }

  /// Make sure you put '/' at the end of directory name. If not, add '/'
  if ( mOutputFigureDirectory.Last('/') + 1 != mOutputFigureDirectory.Length() ){
    LOG_INFO << endm;
    LOG_INFO << "Put / at the end of output directory name" << endm;
    mOutputFigureDirectory.Append("/");
  }

  LOG_INFO << "Set output directory for figures : " << mOutputFigureDirectory << endm;
}

//____________________________________________________________________________________________________
void StEmbeddingQADraw::print(const TCanvas& canvas, const TString name) const
{
  /// Print output figure (default is false, i.e. do not print any figures)

  if( mIsPNGOn ) canvas.Print(Form("%s%s_%s.png", mOutputFigureDirectory.Data(), name.Data(), getBaseName()));
  if( mIsGIFOn ) canvas.Print(Form("%s%s_%s.gif", mOutputFigureDirectory.Data(), name.Data(), getBaseName()));
  if( mIsJPGOn ) canvas.Print(Form("%s%s_%s.jpg", mOutputFigureDirectory.Data(), name.Data(), getBaseName()));
  if( mIsEPSOn ) canvas.Print(Form("%s%s_%s.eps", mOutputFigureDirectory.Data(), name.Data(), getBaseName()));
  if( mIsPSOn )  canvas.Print(Form("%s%s_%s.ps", mOutputFigureDirectory.Data(), name.Data(), getBaseName()));
}

//____________________________________________________________________________________________________
void StEmbeddingQADraw::checkInputGeantId()
{
  /// Check input geant id
  ///  if we don't have histograms for the input geantid put the geantid from the MC histogram

  TH1* hGeantId = (TH1D*) getHistogram("hGeantId_0");

  /// mGeantId = -1 if geantid histogram doesn't exist
  if(!hGeantId){
    mGeantId = -1 ;
    return ;
  }

  /// Scan geantid histogram and store the id's in the geantid array
  for(Int_t id=0; id<hGeantId->GetNbinsX(); id++){
    const Bool_t isGeantIdOk = hGeantId->GetBinContent(id+1)>0.0;
    if(!isGeantIdOk) continue ;

    const Int_t geantid = TMath::Nint(hGeantId->GetBinLowEdge(id+1));
    mMcGeantId.push_back(geantid);
  }

  const StEmbeddingQAUtilities* utility = StEmbeddingQAUtilities::instance() ;

  if( mMcGeantId.size() == 1 ){
    /// For single particle embedding
    ///   Check if the input geantid = geantid found in the histogram
    ///     - found --> do nothing
    ///     - not found --> ask users to proceed the QA or not
    if ( mGeantId == mMcGeantId[0] ) return ; /// do nothing if input geantid is found
    else{
      LOG_INFO << "#----------------------------------------------------------------------------------------------------" << endm;
      LOG_INFO << "  Input geant id doesn't correspond to the geant id in the MC histogram" << endm ;
      LOG_INFO << "  Geantid in the MC histogram, geantid = " << mMcGeantId[0]
               << ",  particle name = " << utility->getParticleDefinition(mMcGeantId[0])->name().c_str() << endm;
      LOG_INFO << "  Do you want to proceed to the QA for geantid = " << mMcGeantId[0] << " ?" << endm;
      LOG_INFO << "  [yes=1/no=0]:" << endm;
      UInt_t proceedQA = 0 ;
      cin >> proceedQA ;
      LOG_INFO << "#----------------------------------------------------------------------------------------------------" << endm;

      /// Stop the QA if you answer NO
      assert(proceedQA);

      /// Put the geantid and proceed the QA
      mGeantId = mMcGeantId[0] ; 
    }
  }
  else{
    /// For multiple particle embedding (like pythia events)
    ///   Check if the input geantid can be found in the MC histogram
    ///     - found --> do nothing
    ///     - not found --> ask users to proceed the QA or not
    for(vector<Int_t>::iterator iter = mMcGeantId.begin(); iter != mMcGeantId.end(); iter++){
      const Int_t geantidFound = (*iter) ;
      if ( mGeantId == geantidFound ) return ; /// do nothing if input geantid is found
    }

    /// List all available MC particles and ask users which particles do you want to do the QA
    LOG_INFO << "#----------------------------------------------------------------------------------------------------" << endm;
    LOG_INFO << "  Cannot find input geantid in the MC histogram" << endm;
    LOG_INFO << "  Available geantid's are listed below;" << endm;
    LOG_INFO << "    geantid          particle name" << endm;
    for(vector<Int_t>::iterator iter = mMcGeantId.begin(); iter != mMcGeantId.end(); iter++){
      const Int_t geantidFound = (*iter) ;
      LOG_INFO << Form("    %5d            %10s", geantidFound, 
          utility->getParticleDefinition(geantidFound)->name().c_str()) << endm;
    }
    LOG_INFO << "  Which geantid you want to do the QA ?" << endm;
    LOG_INFO << "  [Put the one geantid listed above. If you want to stop the program, please put 0]:" << endm;
    cin >> mGeantId ;

    /// Stop the QA if you don't want to proceed
    assert(mGeantId);

    LOG_INFO << "  QA for geantid = " << mGeantId
             << ",  particle name = " << utility->getParticleDefinition(mGeantId)->name().c_str() << endm;
    LOG_INFO << "#----------------------------------------------------------------------------------------------------" << endm;
  }

}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::isOpen() const
{
  /// Check if
  ///  - input files (both embedding and real data) are found
  ///  - input geantid is correct
  ///
  ///  Stop program by assert if one of them is false

  if(!mIsOpen) LOG_INFO << "No input files found" << endm;
  assert(mIsOpen);

  const Bool_t isGeantIdOk = mGeantId>0 ;
  if(!isGeantIdOk) LOG_INFO << "Cannot find input geantid in the histogram or no histogram in the input" << endm;
  assert(isGeantIdOk);

  return kTRUE ;
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::isMatchedPairOk() const
{
  /// Check # of matched pairs > 0
  // NOTE: If there are no matched pairs in the minimc due to the error
  //       this function won't work.
  //       The condition "!isDecay()" would give us the right answer
//  TH1* hGeantId = (TH1D*) getHistogram("hGeantId_1");
//  if(!hGeantId) return kFALSE ;
//  return (hGeantId->GetEntries()>0) ;

  /// Use StParticleDefinition::stable()
  // NOTE: Except for electrons, pions, kaons and protons
  // some of those are assigned as "unstable"
  if ( StEmbeddingQAUtilities::instance()->isEPiKP(mGeantId) ) {
    // stable
    return kTRUE ;
  }
  else if ( StEmbeddingQAUtilities::instance()->isGamma(mGeantId) ) {
    // unstable for gamma
    return kFALSE ;
  }
  else{
    return StEmbeddingQAUtilities::instance()->getParticleDefinition(mGeantId)->stable() ;
  }
}


//____________________________________________________________________________________________________
void StEmbeddingQADraw::setDaughterGeantId()
{
  /// Find daughter geantid from histogram

  /// Check # of matched pairs > 0
  if(isMatchedPairOk()){
    /// Use matched pairs, not contaminated pairs. Set input geantid (mGeantid) into daughter geantid array
    mDaughterGeantId.push_back(mGeantId);
    // Set 0 for parent and parent-parent geantid
    mParentGeantId.push_back(0);
    mParentParentGeantId.push_back(0);
    return;
  }

  /// Try to find out the daughter geantid from the histogram
  for(UInt_t id=0; id<1000; id++){
    const StEmbeddingQAUtilities* utility = StEmbeddingQAUtilities::instance() ;
    const Int_t contamCategoryId = utility->getCategoryId("CONTAM") ;

    Int_t n = 0 ;
    if( mInputParentGeantId == 0 ) n = 1 ;
    else                           n = 2 ;

    for(Int_t i=0; i<n; i++) {
      Int_t daughterId     = id ;
      Int_t parentId       = mGeantId ;
      Int_t parentParentId = 0 ;
      if( i == 1 ) {
        daughterId     = id ;
        parentId       = mInputParentGeantId ;
        parentParentId = mGeantId ;
      }
//        hDca = (TH3D*) mInputEmbedding->Get(Form("hDca_%d_%d_%d_%d", contamCategoryId, 0, mGeantId, id));
//        hDca = (TH3D*) mInputEmbedding->Get(Form("hDca_%d_%d_%d_%d", contamCategoryId, mGeantId, mInputParentGeantId, id));
      TH3* hDca = (TH3D*) mInputEmbedding->Get(Form("hDca_%d_%d_%d_%d", contamCategoryId, parentParentId, parentId, daughterId)) ;

      if ( hDca ){
        if( i == 0 ){
          const Char_t* daughterName = utility->getParticleDefinition(daughterId)->name().c_str() ;
          const Char_t* parentName   = utility->getParticleDefinition(parentId)->name().c_str() ; 
 
          LOG_INFO << Form("Find daughter %10s from parent %10s", daughterName, parentName) << endm;
        }
        else{
          const Char_t* daughterName     = utility->getParticleDefinition(daughterId)->name().c_str() ;
          const Char_t* parentName       = utility->getParticleDefinition(parentId)->name().c_str() ; 
          const Char_t* parentparentName = utility->getParticleDefinition(parentParentId)->name().c_str() ; 
 
          LOG_INFO << Form("Find daughter %10s from parent %10s (from %10s)", daughterName, parentName, parentparentName) << endm;
        }

        mDaughterGeantId.push_back(id);
        mParentGeantId.push_back(parentId);
        mParentParentGeantId.push_back(parentParentId);
      }
    }
  }

  /// If no daughter particles, set mGeantId
  if ( mDaughterGeantId.empty() ) mDaughterGeantId.push_back(mGeantId);
}

//____________________________________________________________________________________________________
Int_t StEmbeddingQADraw::getEntries() const
{
  /// Get number of events from the z-vertex histogram

  TH1* hVz = (TH1D*) getHistogram("hVzAccepted");
  if(!hVz) return 0;

  return (Int_t)( hVz->GetEntries() );
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::isDecay() const
{
  /// Check whether we have decay daughters in our QA histograms

  /// Check matched pairs
  /// Matched pairs > 0, i.e. don't need to look at the contaminated pairs
  if(isMatchedPairOk()) return kFALSE ;

  /// Number of decay daughters should be >= 2
  return ( mDaughterGeantId.size() > 1 ) ;
}

//____________________________________________________________________________________________________
Int_t StEmbeddingQADraw::getGeantIdReal(const Int_t daughter) const
{
  /// Get daughter particle id for the real data
  ///   if daughters are not e/pi/K/p, return pi+ (geantid=8)

  if(mDaughterGeantId[daughter]==6) return 9; // if mu-, use pi-
  return ( StEmbeddingQAUtilities::instance()->isEPiKP(mDaughterGeantId[daughter]) ) ? mDaughterGeantId[daughter] : 8 ;
}

//____________________________________________________________________________________________________
Int_t StEmbeddingQADraw::getCategoryId(const Bool_t isEmbedding) const
{
  /// For embedding QA
  ///   isDecay = kTRUE  --> Contaminated pairs
  ///   isDecay = kFALSE --> Matched pairs
  /// For real data QA
  ///   Use primary track
  const StEmbeddingQAUtilities* utility = StEmbeddingQAUtilities::instance() ;

  if ( isEmbedding ){
    return (isDecay()) ? utility->getCategoryId("CONTAM") : utility->getCategoryId("MATCHED") ; 
  }
  else{
    return utility->getCategoryId("PRIMARY") ;
  }
}

//____________________________________________________________________________________________________
TObject* StEmbeddingQADraw::getHistogram(const TString name, const Bool_t isEmbedding) const
{
  /// Get histogram from either embedding or real data ROOT file

  if(!isOpen()) return 0;

  /// Check if 'mIsEmbeddingOnly' is true (Real data will not be drawn)
  /// return 0 for real data
  if( mIsEmbeddingOnly && !isEmbedding) return 0 ;

  TObject* obj = (isEmbedding) ? mInputEmbedding->Get(name) : mInputRealData->Get(name) ;
  if ( !obj ){
    Error("StEmbeddingQADraw::getHistogram", "Histogram %s doesn't exist", name.Data());
    return 0;
  }

  LOG_DEBUG << "StEmbeddingQADraw::getHistogram()  get histogram = " << name << endm;

  return obj ;
}


//____________________________________________________________________________________________________
TObject* StEmbeddingQADraw::getHistogram(const TString name, const UInt_t daughter, const Bool_t isEmbedding,
    const UInt_t parentparentid) const
{
  /// Get histogram from either embedding or real data ROOT file
  /// Define histogram name from daughter number and category id
  /// 
  /// default parent-parent id is 0

  if ( daughter >= mDaughterGeantId.size() ){
    Error("StEmbeddingQADraw::getHistogram", "Unknown daughter index, index=%3d", daughter);
    return 0;
  }

  return getHistogram(getHistogramName(name, daughter, isEmbedding, parentparentid), isEmbedding) ;
}

//____________________________________________________________________________________________________
const Char_t* StEmbeddingQADraw::getHistogramName(const TString name, const UInt_t daughter, const Bool_t isEmbedding,
    const UInt_t parentparentid) const
{
  const Int_t category = getCategoryId(isEmbedding) ;

  if( isEmbedding ){
    /// Histogram name is 
    ///   - {histogram name}_{category id}_{particle id} for stable particles
    ///   - {histogram name}_{category id}_{parent-parent particle id}_{parent particle id}_{daughter particle id} for unstable particles

    if( isDecay() ) {
//      if ( mParentGeantId == 0 ) return Form("%s_%d_%d_%d_%d", name.Data(), category, parentparentid, mGeantId, mDaughterGeantId[daughter]) ;
//      else                       return Form("%s_%d_%d_%d_%d", name.Data(), category, mGeantId, mParentGeantId, mDaughterGeantId[daughter]) ;
      return Form("%s_%d_%d_%d_%d", name.Data(), category, mParentParentGeantId[daughter], mParentGeantId[daughter], mDaughterGeantId[daughter]) ;
    }
    else {
      return Form("%s_%d_%d", name.Data(), category, mGeantId) ;
    }
  }
  else{
    /// Only primary particles in the real data
    ///   - If mDaughterGeantId is not e/pi/K/p, use pi+
    ///   NOTE: mDaughterGeantId[0] = mGeantId for stable particles
    const Int_t geantidReal = getGeantIdReal(daughter) ;

    return Form("%s_%d_%d", name.Data(), category, geantidReal) ;
  }
}

//____________________________________________________________________________________________________
Double_t StEmbeddingQADraw::getNormalization(const TH1& h) const
{
  /// Get normalization of histogram
  /// Normalization = 1/(Nevts * binWidth)

  const Double_t ntrack   = h.Integral() ;
  if( ntrack == 0 ) return 1.0 ;

  const Double_t binWidth = h.GetBinWidth(1) ;
  if( binWidth == 0.0 ) return 1.0 ;

  return 1.0/(ntrack*binWidth);
}

//____________________________________________________________________________________________________
const Char_t* StEmbeddingQADraw::getBaseName() const
{
  /// Get {year}_{production}_{particle name}

  return Form("%d_%s_%s", mYear, mProduction.Data(), getParticleName());
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::drawStatistics(const Double_t x1, const Double_t y1, const Double_t x2, const Double_t y2,
    const Double_t textSize) const
{
  /// Print
  ///  - number of events
  ///  - Year
  ///  - Production
  ///  - Particle name

  TPaveText* statistics = new TPaveText(x1, y1, x2, y2);
  statistics->SetTextFont(42);
  statistics->SetTextSize(textSize);
  statistics->SetBorderSize(1);
  statistics->SetFillColor(10);
  statistics->AddText(Form("N_{evts} = %d", getEntries()));
  statistics->AddText(Form("Year: %d", mYear));
  statistics->AddText(Form("Production: %s", mProduction.Data()));
  statistics->AddText(Form("Particle: %s", getParticleName()));
  statistics->Draw();

  return kTRUE ;
}

//____________________________________________________________________________________________________
TPaveText* StEmbeddingQADraw::drawHeader(const TString description,
    const Double_t x1, const Double_t y1, const Double_t x2, const Double_t y2, const Double_t textSize) const
{
  /// Draw header for each QA, title for QA plots, daughter particle name etc
  TPaveText* header = new TPaveText(x1, y1, x2, y2);
  header->SetBorderSize(1);
  header->SetFillColor(kBlack);
  header->SetTextColor(10);
  header->SetTextSize(textSize);
  header->SetTextFont(42);
  header->AddText(description);
  header->Draw();
 
  return header ; // need to be deleted after printing PDF
}

//____________________________________________________________________________________________________
void StEmbeddingQADraw::drawLegend(const UInt_t id, TH1* hembed, TH1* hreal, 
    const Option_t* option, const Bool_t doSplit) const
{
  TLegend* leg = new TLegend(0, 0.2, 1, 0.8);
  leg->SetFillColor(10);

  // Use fixed font size
  leg->SetTextFont(43);
  leg->SetTextSize(15);

  leg->AddEntry(hembed, getEmbeddingParticleName(id, doSplit), option) ;
  if(hreal) leg->AddEntry(hreal, getRealParticleName(id, doSplit), option) ;
  leg->Draw();
}

//____________________________________________________________________________________________________
void StEmbeddingQADraw::drawErrorMessages(const TString histogramName) const
{
  const StEmbeddingQAUtilities* utility = StEmbeddingQAUtilities::instance() ;
  const Int_t categoryId = getCategoryId() ;
  const TString title(utility->getCategoryTitle(categoryId));

  TPaveText* error0 = new TPaveText(0.05, 0.60, 0.95, 0.92, "NDC");
  error0->SetTextAlign(12);
  error0->SetBorderSize(1);
  error0->SetFillColor(10);
  error0->SetTextColor(kBlack);
  error0->SetTextSize(0.030);
  error0->SetTextFont(42);
  error0->AddText(Form("*** No histogram \"%s\" found", histogramName.Data()));
  error0->AddText(Form("Please make sure that you have %s in the minimc.root.", title.Data()));
  error0->AddText("Open minimc.root file and then check number of tracks.");
  error0->AddText(Form("There are maybe no %s in the minimc.", title.Data()));
  error0->AddText("Or you used older base QA codes.");
  error0->AddText("In case you have finite number of tracks,");
  error0->AddText("please also have a look at geantid.");
  error0->SetAllWith("***", "color", kRed);
  error0->SetAllWith("***", "font", 72);
  error0->SetAllWith("***", "size", 0.033);


  TPaveText* error1 = new TPaveText(0.05, 0.05, 0.95, 0.55, "NDC");
  error1->SetTextAlign(12);
  error1->SetBorderSize(1);
  error1->SetFillColor(10);
  error1->SetTextColor(kBlack);
  error1->SetTextSize(0.025);
  error1->SetTextFont(42);
  error1->AddText("See example below how to check number of tracks and geantid");

  if ( isDecay() ) {
    error1->AddText("[ROOT]> StMiniMcTree->Draw(\"mNContamPair\")");
    error1->AddText("or");
    error1->AddText("[ROOT]> StMiniMcTree->Scan(\"mNContamPair\")");
    error1->AddText(Form("For geantid in %s", title.Data()));
    error1->AddText("[ROOT]> StMiniMcTree->Draw(\"mContamPairs.mGeantId\")");
    error1->AddText("or use \"Scan()\" function");
  }
  else {
    error1->AddText("[ROOT]> StMiniMcTree->Draw(\"mNMatchedPair\")");
    error1->AddText("or");
    error1->AddText("[ROOT]> StMiniMcTree->Scan(\"mNMatchedPair\")");
    error1->AddText(Form("For geantid in %s", title.Data()));
    error1->AddText("[ROOT]> StMiniMcTree->Draw(\"mMatchedPairs.mGeantId\")");
    error1->AddText("or use \"Scan()\" function");
  }
  error1->SetAllWith("ROOT", "color", kBlue);
  error1->SetAllWith("ROOT", "font",  52);
  error1->SetAllWith("ROOT", "size",  0.028);

  error0->Draw();
  error1->Draw();
}

//____________________________________________________________________________________________________
const Char_t* StEmbeddingQADraw::getParticleName(const Int_t geantid) const
{
  /// Get particle name

  /// Default input geantid = -1 --> return particle name for mGeantid
  const Int_t id = (geantid<0) ? mGeantId : geantid ;
  const StParticleDefinition* particle = StEmbeddingQAUtilities::instance()->getParticleDefinition(id) ;

  if(!particle){
    Error("StEmbeddingQADraw::getParticleName", "Can't find particle geantid=%d in StParticleDefinition", id);
    assert(0);
  }

  /// Remove "/" from particle name (ex. J/Psi --> JPsi)
  /// Since particle name will be used for pdf filename, and "/" will be 
  /// recognized as directory so that output will be disappeared if "/"
  /// is included in the filename
  /// Remove "()" and replace "*" to "start"
  static TString name;
  name = particle->name().c_str();
  while( name.Contains("/") ) name.Remove(name.Index("/"), 1); // Remove "/" from particle name
  while( name.Contains("(") ) name.Remove(name.Index("("), 1); // Remove "(" from particle name
  while( name.Contains(")") ) name.Remove(name.Index(")"), 1); // Remove "/" from particle name
  while( name.Contains("*") ) name.Replace(name.Index("*"), 1, "star"); // Replace "*" to "star"

  return name.Data() ;
}

//____________________________________________________________________________________________________
const Char_t* StEmbeddingQADraw::getMcParticleName() const
{
  const StEmbeddingQAUtilities* utility = StEmbeddingQAUtilities::instance() ;
  const Int_t categoryId = getCategoryId() ;
  const TString name(utility->getCategoryName(categoryId));
  const TString parent( (utility->isContaminated(name)) ? "Parent " : "" ); 

  return Form("%s%s (MC, geantid=%d)", parent.Data(), getParticleName(mGeantId), mGeantId);
}

//____________________________________________________________________________________________________
const Char_t* StEmbeddingQADraw::getEmbeddingParticleName(const UInt_t id, const Bool_t doSplit) const
{
  if(id>=mDaughterGeantId.size()){
    Error("StEmbeddingQADraw::getEmbeddingParticleName", "Unknown daughter particle index, id=%3d", id);
    return "";
  }

  const StEmbeddingQAUtilities* utility = StEmbeddingQAUtilities::instance() ;
  const Int_t categoryId = getCategoryId() ;
  const TString name(utility->getCategoryName(categoryId));

//  const TString parent( (mParentGeantId==0) ? "" : Form(" (from %s)", getParticleName(mParentGeantId)) );
//  const TString daughter( (utility->isContaminated(name)) ? "Daughter " : "" ); 
//  const Int_t daughterId = mDaughterGeantId[id] ;
  const Int_t parentParentId = mParentParentGeantId[id] ;
  const Int_t parentId       = mParentGeantId[id] ;
  const Int_t daughterId     = mDaughterGeantId[id] ;

  const TString daughter( (utility->isContaminated(name)) ? "Daughter " : "" ); 
  TString parent("");
  // For unstable particles
  if ( parentId != 0 ) {
    const TString parentName(getParticleName(parentId));

    if ( parentParentId == 0 ) {
      parent = " (from " +  parentName + ")";
    }
    else{
      const TString parentParentName(getParticleName(parentParentId));
      parent = " (from " + parentName + " #leftarrow " + parentParentName + ")";
    }
  }

  return (doSplit) ? Form("#splitline{%s%s%s}{(%s, geantid=%d)}", daughter.Data(), getParticleName(daughterId),
      parent.Data(), utility->getCategoryName(categoryId).Data(), daughterId)
    : Form("%s%s%s (%s, geantid=%d)", daughter.Data(), getParticleName(daughterId),
        parent.Data(), utility->getCategoryName(categoryId).Data(), daughterId);
}

//____________________________________________________________________________________________________
const Char_t* StEmbeddingQADraw::getRealParticleName(const UInt_t id, const Bool_t doSplit) const
{
  // nsigma < 2 is hard-coded (2 sigma cut is implemented in StEmbeddingQATrack.cxx).
  const StEmbeddingQAUtilities* utility = StEmbeddingQAUtilities::instance() ;

  if(id>=mDaughterGeantId.size()){
    Error("StEmbeddingQADraw::getRealParticleName", "Unknown daughter particle index, id=%3d", id);
    return "";
  }

  const Int_t geantid = getGeantIdReal(id) ;
  return (doSplit) ?  Form("#splitline{%s}{(%s, |n #sigma_{%s}|<2 %s)}", getParticleName(geantid), 
      StEmbeddingQAUtilities::instance()->getCategoryName(getCategoryId(kFALSE)).Data(), getParticleName(geantid), utility->getBTofPid()?"BTOF":"TPC")
    : Form("%s (%s, |n #sigma_{%s}|<2 %s)", getParticleName(geantid),
      StEmbeddingQAUtilities::instance()->getCategoryName(getCategoryId(kFALSE)).Data(), getParticleName(geantid), utility->getBTofPid()?"BTOF":"TPC");
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::drawEvent() const
{
  /// Event-wise QA

  LOG_INFO << endm;
  LOG_INFO << "----------------------------------------------------------------------------------------------------" << endm ;
  LOG_INFO << "QA for Event-wise informations ..." << endm;

  /// Make sure (1) input ROOT files and (2) input geantid
  if(!isOpen()) return kFALSE ;

  //----------------------------------------------------------------------------------------------------
  // Event-wise informations
  //----------------------------------------------------------------------------------------------------

  //----------------------------------------------------------------------------------------------------
  // Vertices
  drawVertices() ;

  //----------------------------------------------------------------------------------------------------
  /// Run id, event id QA
  drawRunEventId() ;

  //----------------------------------------------------------------------------------------------------
  /// Number of particles
  drawNumberOfParticles() ;

  LOG_INFO << "----------------------------------------------------------------------------------------------------" << endm ;
  LOG_INFO << endm;

  return kTRUE ;
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::drawVertices() const
{
  /// Draw event vertices
  LOG_INFO << "QA for event-vertices ..." << endm;

  /// Get minimum/maximum z-vertex cut
  const Double_t vzMin = getVzAcceptedMinimum() ;
  const Double_t vzMax = getVzAcceptedMaximum() ;
  TPaveText* header = initCanvas(Form("Event vertices, offline cuts: %1.1f < v_{z} < %1.1f cm", vzMin, vzMax), 2, 2);

  /// z-vertex
  mMainPad->cd(1);
  TH1* hVz = (TH1D*) getHistogram("hVz");
  if(!hVz) return kFALSE ;

  TH1* hVzAccepted = (TH1D*) getHistogram("hVzAccepted");
  if(!hVzAccepted) return kFALSE ;

  hVzAccepted->SetLineColor(kCyan);
  hVzAccepted->SetFillColor(kCyan);
  hVz->Draw();
  hVzAccepted->Draw("same");
  hVz->Draw("same");

  /// y vs x vertices
  mMainPad->cd(2);
  TH2* hVyVx = (TH2D*) getHistogram("hVyVx");
  if(!hVyVx) return kFALSE ;
  hVyVx->Draw("colz");

  /// x vs z vertices, constrained within z-vertex cuts
  mMainPad->cd(3);
  TH2* hVxVz = (TH2D*) getHistogram("hVxVz");
  if(!hVxVz) return kFALSE ;
  hVxVz->SetAxisRange(vzMin, vzMax, "X");
  hVxVz->Draw("colz");

  /// y vs z vertices, constrained within z-vertex cuts
  mMainPad->cd(4);
  TH2* hVyVz = (TH2D*) getHistogram("hVyVz");
  if(!hVyVz) return kFALSE ;
  hVyVz->SetAxisRange(vzMin, vzMax, "X");
  hVyVz->Draw("colz");

  mCanvas->cd();
  mCanvas->Update();
  mPDF->NewPage() ;

  /// V(Data) - V(MC)
  delete header ;
  header = initCanvas("Event vertices, #Deltav = v(Data) - v(MC)", 2, 2);

  /// dVx, dVy, dVz
  for(Int_t i=0; i<3; i++){
    mMainPad->cd(i+1);
    TH1* hdV = 0 ;
    if( i == 0 ) hdV = (TH1D*) getHistogram("hdVx");
    if( i == 1 ) hdV = (TH1D*) getHistogram("hdVy");
    if( i == 2 ) hdV = (TH1D*) getHistogram("hdVz");
    if(!hdV) return kFALSE ;

    hdV->Draw();
  }

  mCanvas->cd();
  mCanvas->Update();
  mPDF->NewPage() ;

  //----------------------------------------------------------------------------------------------------

  delete header ;

  return kTRUE ;
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::drawRunEventId() const
{
  /// Draw run and event id. List all runid in the bottom left pad
  LOG_INFO << "QA for run and event id ..." << endm;

  TPaveText* header = initCanvas("Run and event id", 2, 2);

  mMainPad->cd(1);
  TH1* hRunNumber = (TH1D*) mInputEmbedding->Get("hRunNumber");
  if(!hRunNumber) return kFALSE ;
  hRunNumber->Draw();

  mMainPad->cd(2);
  TH1* hEventId = (TH1D*) mInputEmbedding->Get("hEventId");
  if(!hEventId) return kFALSE ;
  hEventId->Draw();

  /// List of run id
  TPaveText* runlist[2];
  for(Int_t i=0;i<2;i++){
    runlist[i] = new TPaveText(0.1, 0.05, 0.9, 0.95);
    runlist[i]->SetTextFont(42);
    runlist[i]->SetTextSize(0.04);
    runlist[i]->SetBorderSize(1);
    runlist[i]->SetFillColor(10);
    runlist[i]->AddText("Run id         statistics");
  }

  // Count total number of runs
  Int_t runTotal = 0 ;
  for(Int_t irun=0; irun<hRunNumber->GetNbinsX(); irun++){
    if(hRunNumber->GetBinContent(irun+1)>0.0) runTotal++;
  }

  // If number of runs >= 10, divided into 2 pads
  Int_t runAccepted = 0 ;
  for(Int_t irun=0; irun<hRunNumber->GetNbinsX(); irun++){
    const Double_t count = hRunNumber->GetBinContent(irun+1);
    if(count==0.0) continue ;

    Int_t pad = 0;
    if( runTotal < 10 ) pad = 0 ; // always drawing left pad
    else{
      // Divide into 2 pads
      if( runAccepted < runTotal/2 ) pad = 0;
      else                           pad = 1 ;
    }

    const Int_t runid = StEmbeddingQAUtilities::instance()->getRunId((Int_t)hRunNumber->GetBinLowEdge(irun+1), mYear);
    runlist[pad]->AddText(Form("%10d  %10d events", runid, (Int_t)count));

    runAccepted++ ;
  }

  mMainPad->cd(3) ;
  runlist[0]->Draw();

  mMainPad->cd(4) ;
  runlist[1]->Draw();

  mCanvas->cd();
  mCanvas->Update();

  mPDF->NewPage() ;
  //----------------------------------------------------------------------------------------------------

  delete header ;

  return kTRUE ;
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::drawNumberOfParticles() const
{
  /// Draw number of particles for all embedding branches
  LOG_INFO << "QA for multiplicity distributions (number of particles per event) ..." << endm;

  TPaveText* header = initCanvas("Multiplicity distribution", 2, 3);

  for(UInt_t ic=0; ic<StEmbeddingQAConst::mNEmbedding; ic++){
    mMainPad->cd(ic+1);
    mMainPad->GetPad(ic+1)->SetLogy();

    TH1* hNParticles = (TH1D*) mInputEmbedding->Get(Form("hNParticles_%d", ic));
    if(!hNParticles) return kFALSE ;

    TString title(hNParticles->GetTitle());
    title.Remove(0, title.Index(',')+2);
    hNParticles->SetTitle(title);

    // Set x-axis range
    if( hNParticles->GetMean() == 0 && hNParticles->GetRMS() == 0 ){
      // If no particles in the branch
      hNParticles->SetAxisRange(0, 10, "X");

      LOG_DEBUG << "StEmbeddingQADraw::drawNumberOfParticles  no particles for " << title << endm ;
    }
    else if( hNParticles->GetRMS() == 0 ){
      // Fixed multiplicity should have RMS = 0
      const Double_t mean = hNParticles->GetMean() ;
      const Double_t xmin = (mean-10.0<0.0) ? 0.0 : mean-10.0 ;
      const Double_t xmax = mean + 10.0 ;
      hNParticles->SetAxisRange(xmin, xmax, "X");

      LOG_DEBUG << "StEmbeddingQADraw::drawNumberOfParticles  constant number of particles = " << mean
               << " for " << title
               << endm;
    }
    else{
      // Varied multiplicity (like 5% of refmult)

      // Try to find the maximum multiplicity
      Double_t xmax = 0.0 ;
      for(Int_t i=hNParticles->GetNbinsX()-1; i!=0; i--){
        if( hNParticles->GetBinContent(i+1) != 0.0 ){
          // Find first bin, contains finite bin content
          xmax = hNParticles->GetBinCenter(i+1) ;
          break ;
        }
      }
      hNParticles->SetAxisRange(0, xmax+10, "X");

      LOG_DEBUG << "StEmbeddingQADraw::drawNumberOfParticles  varied multiplicity, max range = " << xmax
               << " for " << title
               << endm;
    }

    hNParticles->Draw();
  }

  mCanvas->cd();
  mCanvas->Update();

  mPDF->NewPage() ;

  delete header ;

  return kTRUE ;
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::drawMcTrack() const
{
  /// MC track QA

  LOG_INFO << "QA for MC tracks (pt, eta, y, phi) ..." << endm;

  /// Make sure (1) input ROOT files and (2) input geantid
  if(!isOpen()) return kFALSE ;

  //----------------------------------------------------------------------------------------------------
  // Track-wise informations
  TPaveText* header = initCanvas("MC track QA (2D)", 2, 2);

  /// pT vs eta
  mMainPad->cd(1);
  TH2* hPtVsEta = (TH2D*) getHistogram(Form("hPtVsEta_0_%d", mGeantId));
  if(!hPtVsEta) return kFALSE;

  hPtVsEta->SetTitle("");
  hPtVsEta->Draw("colz");
  hPtVsEta->SetAxisRange(0, mPtMax, "Y");

  /// pT vs y
  mMainPad->cd(2);
  TH2* hPtVsY = (TH2D*) getHistogram(Form("hPtVsY_0_%d", mGeantId));
  if(!hPtVsY) return kFALSE ;

  hPtVsY->SetTitle("");
  hPtVsY->Draw("colz");
  hPtVsY->SetAxisRange(0, mPtMax, "Y");

  /// pT vs phi
  mMainPad->cd(3);
  TH2* hPtVsPhi = (TH2D*) getHistogram(Form("hPtVsPhi_0_%d", mGeantId));
  if(!hPtVsPhi) return kFALSE ;

  hPtVsPhi->SetTitle("");
  hPtVsPhi->Draw("colz");
  hPtVsPhi->SetAxisRange(0, mPtMax, "Y");

  mCanvas->cd();
  mCanvas->Update();

  mPDF->NewPage() ;

  delete header ;

  // 1D projections
  header = initCanvas("MC track QA (1D)", 2, 2);

  /// Projections (pt, eta, y, phi)
  TH1* hPt  = (TH1D*) hPtVsPhi->ProjectionY("hPtMc");
  TH1* hEta = (TH1D*) hPtVsEta->ProjectionX("hEtaMc");
  TH1* hY   = (TH1D*) hPtVsY->ProjectionX("hYMc");
  TH1* hPhi = (TH1D*) hPtVsPhi->ProjectionX("hPhiMc");
  hPt->SetTitleOffset(1.0, "X");
  hPt->SetXTitle("p_{T} (GeV/c)");
  hEta->SetXTitle("#eta");
  hY->SetXTitle("rapidity");

  hPt ->SetTitle("");
  hEta->SetTitle("");
  hY  ->SetTitle("");
  hPhi->SetTitle("");

  hPt ->Sumw2() ;
  hEta->Sumw2() ;
  hY  ->Sumw2() ;
  hPhi->Sumw2() ;

  hPt->SetMinimum(0.0);
  hPt->SetMaximum(hPt->GetMaximum()*1.2);
  hPt->SetAxisRange(0, mPtMax, "X");
  hEta->SetMinimum(0.0);
  hEta->SetMaximum(hEta->GetMaximum()*1.2);
  hY->SetMinimum(0.0);
  hY->SetMaximum(hY->GetMaximum()*1.2);
  hPhi->SetMinimum(0.0);
  hPhi->SetMaximum(hPhi->GetMaximum()*1.2);

  // 1. pT
  // 3. y
  // 3. phi
  // 4. eta
  gStyle->SetOptFit(1);
  for(Int_t ipad=0; ipad<4; ipad++){
    mMainPad->cd(ipad+1);
    TH1* hdraw = 0 ;
    if( ipad == 0 ) hdraw = hPt ;
    if( ipad == 1 ) hdraw = hEta ;
    if( ipad == 2 ) hdraw = hY ;
    if( ipad == 3 ) hdraw = hPhi ;

    hdraw->Draw();

    if( ipad != 0 ){
      // Fitting by pol0
      Double_t min = -1.0 ;
      Double_t max =  1.0 ;
      if( ipad == 3 ){ min = -TMath::Pi() ; max = TMath::Pi() ; } // phi
 
      TF1* pol0Fit = new TF1(Form("pol0Fit_%d", ipad), "pol0", min, max);
      pol0Fit->SetLineColor(kRed);
      pol0Fit->SetLineWidth(1);
      pol0Fit->SetLineStyle(2);
      pol0Fit->SetParameter(0, hdraw->GetMean(2));
      hdraw->Fit(pol0Fit, "rq0");
      pol0Fit->Draw("same");
    }
  }

  mCanvas->cd();
  mCanvas->Update();

  mPDF->NewPage() ;

  delete header ;

  gStyle->SetOptFit(0);

  return kTRUE ;
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::drawTrack() const
{
  /// Track-wise QA
  LOG_INFO << endm ;
  LOG_INFO << "#----------------------------------------------------------------------------------------------------" << endm ;
  LOG_INFO << "QA for reconstructed tracks ..." << endm ;

  /// Make sure (1) input ROOT files and (2) input geantid
  if(!isOpen()) return kFALSE ;

  //----------------------------------------------------------------------------------------------------
  // Track-wise informations (Comparison between MC and reconstructed particles)
  ///  1: Geant id
  const Bool_t isGeantIdOk = drawGeantId();

  ///  2: Azimuthal angle distributions
  const Bool_t isPhiOk      = drawPhi();
 
  ///  3: (pseudo-)rapidity distributions
  const Bool_t isRapidityOk = drawRapidity();
 
  ///  4: Momentum and pt
  const Bool_t isMomentumOk = drawMomentum();
  const Bool_t isPtOk       = drawPt();
 
  ///  5: dE/dx
  const Bool_t isdEdxOk     = drawdEdx();
 
  ///  6: Global dca
  const Bool_t isDcaOk      = drawDca();
 
  ///  7: NHit
  const Bool_t isNHitOk     = drawNHit();

  LOG_INFO << "#----------------------------------------------------------------------------------------------------" << endm ;
  LOG_INFO << endm ;
  
  return isGeantIdOk && isPhiOk && isRapidityOk && isMomentumOk && isPtOk && isdEdxOk && isDcaOk && isNHitOk ;
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::drawGeantId() const
{
  /// QA for geantid

  LOG_INFO << "QA for geant id ..." << endm;

  /// Make sure (1) input ROOT files and (2) input geantid
  if(!isOpen()) return kFALSE ;

  /// pad0: MC tracks
  /// pad1: Reconstructed tracks
  /// pad2: Statistics
  TPaveText* header = initCanvas("Geant id", 1, 2);

  /// Draw MC tracks
  TPad* topPad = (TPad*) mMainPad->cd(1);
  topPad->Divide(2, 1);
  topPad->cd(1);
  TH1* hGeantIdMc = (TH1D*) getHistogram("hGeantId_0");
  if(!hGeantIdMc) return kFALSE ;

  /// Draw MC id +/- 10
  const Int_t mcIdBin    = hGeantIdMc->GetMaximumBin() ;
  const Double_t mcId    = hGeantIdMc->GetBinCenter(mcIdBin);
  const Double_t mcIdMin = (mcId-10<0) ? 0 : mcId-10 ;
  const Double_t mcIdMax = mcId + 10 ;

  hGeantIdMc->SetTitle(Form("MC Geant id for %s", getParticleName()));
  hGeantIdMc->SetMaximum(hGeantIdMc->GetMaximum()*1.2);
  hGeantIdMc->SetAxisRange(mcIdMin, mcIdMax, "X");
  hGeantIdMc->Draw();

  /// Draw line at expected geantid id for MC tracks
  TLine* hGeantIdMcExpected = new TLine(mGeantId+0.5, 0, mGeantId+0.5, hGeantIdMc->GetMaximum()) ; 
  hGeantIdMcExpected->SetLineColor(kBlue);
  hGeantIdMcExpected->Draw();

  /// Draw reconstructed geant id
  topPad->cd(2);
  TH1* hGeantIdReco = (TH1D*) getHistogram(Form("hGeantId_%d", getCategoryId()));
  if(!hGeantIdReco) return kFALSE ;

  hGeantIdReco->SetTitle("Reconstructed geant id");
  hGeantIdReco->SetMaximum(hGeantIdReco->GetMaximum()*1.2);
  hGeantIdReco->SetAxisRange(0, 50, "X"); // Max geant id is 50
  hGeantIdReco->Draw();

  /// Draw expected geant id lines for decay daughters
  TLine** hGeantIdRecoExpected = new TLine*[mDaughterGeantId.size()];

  for(UInt_t id=0; id<mDaughterGeantId.size(); id++){
    const Int_t geantid = mDaughterGeantId[id] ;
    hGeantIdRecoExpected[id] = new TLine(geantid+0.5, 0, geantid+0.5, hGeantIdReco->GetMaximum()) ; 
    hGeantIdRecoExpected[id]->SetLineColor(kRed);
    hGeantIdRecoExpected[id]->SetLineStyle(id+1);
    hGeantIdRecoExpected[id]->Draw();
  }

  mMainPad->cd(2);
  TLegend* leg = new TLegend(0.1, 0.2, 0.9, 0.8);
  leg->SetTextSize(0.05);
  leg->SetFillColor(10);
  leg->SetHeader("Particle informations");

  leg->AddEntry(hGeantIdMcExpected, getMcParticleName(), "L");

  for(UInt_t id=0; id<mDaughterGeantId.size(); id++){
    leg->AddEntry( hGeantIdRecoExpected[id], getEmbeddingParticleName(id), "L");
  }
  leg->Draw();

  mCanvas->cd();
  mCanvas->Update();

  mPDF->NewPage() ;

  delete header ;

  return kTRUE ;
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::drawPhi() const
{
  /// QA for azimuthal angle (phi) distributions

  /// Make sure (1) input ROOT files and (2) input geantid
  if(!isOpen()) return kFALSE ;

  /// Comparison of embedding with MC
  Bool_t isPhiEmbeddingVsMcOk   = drawProjection2D("phi", kTRUE);

  /// Added comparison with real data
  Bool_t isPhiEmbeddingVsRealOk = drawProjection2D("phi", kFALSE);

  return isPhiEmbeddingVsMcOk && isPhiEmbeddingVsRealOk ;
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::drawRapidity() const
{
  /// QA for (pseudo-)rapidity

  /// Make sure (1) input ROOT files and (2) input geantid
  if(!isOpen()) return kFALSE ;

  /// Pseudo-rapidity
  const Bool_t isEtaOk = drawProjection2D("eta");

  /// Rapidity
  const Bool_t isYOk   = drawProjection2D("y");

  return isEtaOk && isYOk ;
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::drawMomentum() const
{
  /// QA for momentum

  /// Make sure (1) input ROOT files and (2) input geantid
  if(!isOpen()) return kFALSE ;

  /// Plot reconstructed momentum vs MC momentum (2D) (Embedding only)
  for(UInt_t id=0; id<mDaughterGeantId.size(); id++){
    TPaveText* header = 0 ;

    TH2* hRecoPVsMcP = (TH2D*) getHistogram("hRecoPVsMcP", id, kTRUE);
    if(!hRecoPVsMcP){
      // Draw error messages
      header = initCanvas("Reconstructed momentum vs MC momentum");
      drawErrorMessages(getHistogramName("hRecoPVsMcP", id, kTRUE)) ;

      mCanvas->cd();
      mCanvas->Update();
      mPDF->NewPage();

      delete header ;

      return kFALSE ;
    }

    header = initCanvas("Reconstructed momentum vs MC momentum");

    hRecoPVsMcP->SetAxisRange(0, mPtMax, "X");
    hRecoPVsMcP->SetAxisRange(0, mPtMax, "Y");

    hRecoPVsMcP->SetTitle(getParticleName(mDaughterGeantId[id]));

    hRecoPVsMcP->Draw("colz");

    mCanvas->cd();
    mCanvas->Update();

    mPDF->NewPage() ;

    delete header ;
  }

  return drawProjection2D("momentum");
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::drawPt() const
{
  /// QA for pt

  /// Make sure (1) input ROOT files and (2) input geantid
  if(!isOpen()) return kFALSE ;

  // 1/pT(Gl) - 1/pT(MC) vs pT(MC) (Embedding only)
  for(UInt_t id=0; id<mDaughterGeantId.size(); id++){
    TPaveText* header = 0 ;

    TH2* hdInvPtVsPt = (TH2D*) getHistogram("hdInvPtVsPt", id, kTRUE);
    if(!hdInvPtVsPt){
      // Draw error messages
      header = initCanvas(Form("1/p_{T} (Gl) - 1/p_{T} (MC) vs p_{T} (MC) (%s)", getParticleName(mDaughterGeantId[id])));
      drawErrorMessages(getHistogramName("hdInvPtVsPt", id, kTRUE)) ;

      mCanvas->cd();
      mCanvas->Update();
      mPDF->NewPage();

      delete header ;

      return kFALSE ;
    }

    gStyle->SetPadRightMargin(0.15);

    header = initCanvas(Form("1/p_{T} (Gl) - 1/p_{T} (MC) vs p_{T} (MC) (%s)", getParticleName(mDaughterGeantId[id])), 1, 2);
    // 2D
    mMainPad->cd(1);
    hdInvPtVsPt->SetAxisRange(0, mPtMax, "X");
    hdInvPtVsPt->SetTitle("");
    hdInvPtVsPt->Draw("colz");

    // Profile
    mMainPad->cd(2);
    TProfile* pdInvPtVsPt = (TProfile*) hdInvPtVsPt->ProfileX(Form("p%s", hdInvPtVsPt->GetName()));
    pdInvPtVsPt->SetAxisRange(0, mPtMax, "X");
    pdInvPtVsPt->SetTitle("");
    pdInvPtVsPt->SetYTitle(hdInvPtVsPt->GetYaxis()->GetTitle());
    pdInvPtVsPt->SetLineWidth(2);
    pdInvPtVsPt->SetMinimum(hdInvPtVsPt->GetYaxis()->GetXmin());
    pdInvPtVsPt->SetMaximum(hdInvPtVsPt->GetYaxis()->GetXmax());
    pdInvPtVsPt->Draw();

    mCanvas->cd();
    mCanvas->Update();

    mPDF->NewPage() ;

    delete header ;
  }

  gStyle->SetPadRightMargin(0.05);

  return drawProjection2D("pt");
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::drawProjection2D(const TString name, const Bool_t isMC) const
{
  /// Utility function to draw 1D projection histgrams from 2D

  /// Input 2D histogram is pt or momentum vs eta or y
  /// or pt vs phi
  //
  /// Projection into eta or y for pt = 0.2 - 5 GeV/c (0.5 GeV/c step)
  /// or
  /// Projection into pt or momentum for |Delta eta| = 0.5 in |eta| < 2
  /// or 
  /// Projection into pt for phi distributions
  ///
  /// if isMC = kTRUE (default is kFALSE), compare the reconstructed embedding tracks with the MC tracks
  /// NOTE: only applied for stable MC particles. If they have decay daughters,
  ///       force to plot real data

  TString nameLower(name);
  nameLower.ToLower();

  LOG_INFO << "QA for " << name << " ..." << endm;

  /// Define y-axis title, histogram name and projection axis from the input argument
  TString histoName("");
  Bool_t isProjectionX = kFALSE ;
  TString yTitle("");
  TString xTitle("");

  if( nameLower.Contains("pt") ){
    xTitle = "#eta";
    yTitle = "p_{T}";
    histoName = "hPtVsEta";
    isProjectionX = kFALSE ;
  }
  else if( nameLower.Contains("momentum") ){
    xTitle = "#eta";
    yTitle = "p";
    histoName = "hMomVsEta";
    isProjectionX = kFALSE ;
  }
  else if( nameLower.Contains("eta") || name.Contains("pseudorapidity") ){
    xTitle = "p_{T}";
    yTitle = "#eta";
    histoName = "hPtVsEta";
    isProjectionX = kTRUE ;
  }
  else if( nameLower.Contains("y") || name.Contains("rapidity") ){
    xTitle = "p_{T}";
    yTitle = "y";
    histoName = "hPtVsY";
    isProjectionX = kTRUE ;
  }
  else if( nameLower.Contains("phi")){
    xTitle = "p_{T}";
    yTitle = "#phi";
    histoName = "hPtVsPhi";
    isProjectionX = kTRUE ;
  }
  else{
    Error("DrawProjection2D", "Unknown variable, %s", name.Data());
    LOG_INFO << "  Current implemented variables are" << endm;
    LOG_INFO << "-----------------------------------------------------------------" << endm;
    LOG_INFO << "   Input                       variable" << endm;
    LOG_INFO << "-----------------------------------------------------------------" << endm;
    LOG_INFO << "   pt                           p_{T}" << endm;
    LOG_INFO << "   momentum                     p" << endm;
    LOG_INFO << "   eta or pseudorapidity        eta" << endm;
    LOG_INFO << "   y or rapidity                y" << endm;
    LOG_INFO << "   phi                          phi" << endm;
    LOG_INFO << "-----------------------------------------------------------------" << endm;
    LOG_INFO << endm;
    LOG_INFO << "NOTE : Input is case insensitive" << endm;
    LOG_INFO << endm;

    return kFALSE ;
  }

  gStyle->SetPadRightMargin(0.05);

  // MC tracks
  TH2* h2DMc = (TH2D*) getHistogram(Form("%s_0_%d", histoName.Data(), mGeantId));

  for(UInt_t id=0; id<mDaughterGeantId.size(); id++){
    const TString headerTitle(Form("Projection of %s for each %s bin", yTitle.Data(), xTitle.Data()));
    TPaveText* header = 0 ;

    TH2* h2DEmbed = (TH2D*) getHistogram(histoName, id, kTRUE);
    if(!h2DEmbed){
      // Draw error message
      header = initCanvas(headerTitle);
      drawErrorMessages(getHistogramName(histoName, id, kTRUE)) ;

      mCanvas->cd();
      mCanvas->Update();
      mPDF->NewPage();

      delete header ;

      return kFALSE ;
    }

    TH2* h2DReal  = (TH2D*) getHistogram(histoName, id, kFALSE);

    /// Define canvas Ndivisions
    header = initCanvas(headerTitle);

    Int_t npad = 0 ;
    Int_t npadMax = 0 ;
    if( isProjectionX ){
      mMainPad->Divide(2, 3);
      npad = 5 ;
      npadMax = 6 ;
    }
    else{
      mMainPad->Divide(2, 4);
      npad = 6 ;
      npadMax = 8 ;
    }

    /// Get bins
    /// eta bins = 6 : |eta| < 1.5
    /// pt bins  = 10 : pt = 0.2 - 5 GeV/c
    const Int_t nbins = (isProjectionX) ? 10 : 6 ;
    const Double_t binStep = 0.5 ;
    const Double_t binMin  = (isProjectionX) ? 0.0 : -1.5 ;

    Int_t ipad = 1 ;
    for(Int_t ibin=0; ibin<nbins; ibin++){
      if( ipad%(npad+1) == 0 ){
        mCanvas->cd();
        mCanvas->Update();
        mPDF->NewPage();

        if(header) delete header ;
        header = initCanvas(headerTitle);
 
        if( isProjectionX ) mMainPad->Divide(2, 3);
        else                mMainPad->Divide(2, 4);
        ipad = 1 ;
      }

      const Double_t xMin = (isProjectionX && ibin==0) ? 0.2 : binMin + binStep * ibin ;
      const Double_t xMax = (isProjectionX && ibin==0) ? 0.5 : binMin + binStep * (ibin+1.0) ;
      const Int_t xMinBin = (isProjectionX) ? h2DEmbed->GetYaxis()->FindBin(xMin)     : h2DEmbed->GetXaxis()->FindBin(xMin) ;
      const Int_t xMaxBin = (isProjectionX) ? h2DEmbed->GetYaxis()->FindBin(xMax) - 1 : h2DEmbed->GetXaxis()->FindBin(xMax) - 1;

      TH1* hEmbed = 0;
      TH1* hReal  = 0;
      TH1* hMc    = 0;
      if( isProjectionX ){
        if( h2DEmbed ) hEmbed = (TH1D*) h2DEmbed->ProjectionX(Form("h%sEmbed_%d_%d", name.Data(), id, ibin), xMinBin, xMaxBin);
        if( h2DReal )  hReal  = (TH1D*) h2DReal->ProjectionX(Form("h%sReal_%d_%d", name.Data(), id, ibin), xMinBin, xMaxBin);
        if( h2DMc )    hMc    = (TH1D*) h2DMc->ProjectionX(Form("h%sMc_%d_%d", name.Data(), id, ibin), xMinBin, xMaxBin);
      }
      else{
        if( h2DEmbed ) hEmbed = (TH1D*) h2DEmbed->ProjectionY(Form("h%sEmbed_%d_%d", name.Data(), id, ibin), xMinBin, xMaxBin);
        if( h2DReal )  hReal  = (TH1D*) h2DReal->ProjectionY(Form("h%sReal_%d_%d", name.Data(), id, ibin), xMinBin, xMaxBin);
        if( h2DMc )    hMc    = (TH1D*) h2DMc->ProjectionY(Form("h%sMc_%d_%d", name.Data(), id, ibin), xMinBin, xMaxBin);
      }

      hEmbed->SetLineColor(kRed);
      if(hEmbed->GetSumw2N()==0) hEmbed->Sumw2();
      hEmbed->Scale( getNormalization(*hEmbed) );

      if( hReal ){
        hReal->SetLineColor(kBlue);
        if(hReal->GetSumw2N()==0) hReal->Sumw2();
        hReal->Scale( getNormalization(*hReal) );
      }

      if( hMc ){
        hMc->SetLineColor(kBlack);
        if(hMc->GetSumw2N()==0) hMc->Sumw2();
        hMc->Scale( getNormalization(*hMc) );
      }

      hEmbed->SetMinimum(0.0);

      if( isProjectionX ){
        hEmbed->SetTitle(Form("%1.1f < p_{T} < %1.1f (GeV/c)", xMin, xMax));
      }
      else{
        hEmbed->SetTitle(Form("%1.1f < #eta < %1.1f", xMin, xMax));
        hEmbed->SetAxisRange(0, mPtMax, "X");
      }
      hEmbed->SetYTitle(Form("(1/N_{trk})dN/d%s", yTitle.Data()));

      mMainPad->cd(ipad) ;
      hEmbed->Draw();

      /// Check if MC particle have decay daughters
      if(isMC && !isDecay()){
        /// Set maximum of y-axis from MC and embedding
        hEmbed->SetMaximum( TMath::Max(hMc->GetMaximum(), hEmbed->GetMaximum()) * 1.2 );

        hMc->Draw("same");
      }
      else{
        /// Set maximum of y-axis
        /// Use max(real, embed) if real exists
        /// Otherwise use embed
        const Double_t yMax = (hReal) ? TMath::Max(hReal->GetMaximum(), hEmbed->GetMaximum()) : hEmbed->GetMaximum() ;
        hEmbed->SetMaximum( yMax * 1.2 );

        if(hReal) hReal->Draw("hsame");
      }
      hEmbed->Draw("same");

      // Draw legend in the last pad in each Canvas
      if ( ipad == 1 ){
        mMainPad->cd(npadMax);
        TLegend* leg = new TLegend(0.0, 0.2, 1.0, 0.8);
        leg->SetFillColor(10);
        leg->SetTextFont(43);
        leg->SetTextSize(15);

        leg->AddEntry(hEmbed, getEmbeddingParticleName(id, kTRUE), "L");
        if(isMC && !isDecay()){
          leg->AddEntry(hMc, getMcParticleName(), "L");
        }
        else{
          if(hReal) leg->AddEntry(hReal, getRealParticleName(id, kTRUE), "L");
        }
        leg->Draw();
      }

      ipad++;
    }

    mCanvas->cd();
    mCanvas->Update();

    mPDF->NewPage();

    if(header) delete header ;
  }

  gStyle->SetPadRightMargin(0.15);

  return kTRUE ;
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::drawdEdx() const
{
  /// QA for dE/dx

  /// Make sure (1) input ROOT files and (2) input geantid
  if(!isOpen()) return kFALSE ;

  /// vs MC momentum
  const Bool_t isMcOk   = drawdEdxVsMomentum(kTRUE) ;

  /// vs Reconstructed momentum
  const Bool_t isRecoOk = drawdEdxVsMomentum(kFALSE) ;

  return isMcOk && isRecoOk ;
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::drawdEdxVsMomentum(const Bool_t isMcMomentum) const
{
  /// Draw dE/dx vs momentum variables (either momentum or pt)
  /// Draw projection of dE/dx distribution for each momentum slice
  /// Draw mean/sigma as a function of momentum variables
  //
  /// Compare (1) embedding, (2) real data without PID, (3) real data with PID

  /// Select MC or reconstructed momentum for the embedding data
  /// Use reconstructed momentum for the real data
  const TString momName  = (isMcMomentum) ? "Mc" : "Reco" ;

  LOG_INFO << "QA for dE/dx (" << momName << " momentum ...)" << endm;

  gStyle->SetOptFit(0);
  gStyle->SetPadRightMargin(0.05);

  for(UInt_t id=0; id<mDaughterGeantId.size(); id++){
    //----------------------------------------------------------------------------------------------------
    // 2D
    //----------------------------------------------------------------------------------------------------
    TString headerTitle("");
    if( mIsEmbeddingOnly ){
      headerTitle = Form("dE/dx vs momentum (Embedding:%s)", getParticleName(mDaughterGeantId[id]) );
    }
    else{
      headerTitle = Form("dE/dx vs momentum (Embedding:%s, Real:%s)", 
            getParticleName(mDaughterGeantId[id]), getParticleName(getGeantIdReal(id)) );
    }
    TPaveText* header = 0 ;

    // Embedding
    TH2* hdEdxVsMomEmbed = (TH2D*) getHistogram(Form("hdEdxVsMom%s", momName.Data()), id, kTRUE);
    if(!hdEdxVsMomEmbed){
      // Draw error messages
      header = initCanvas(headerTitle);
      drawErrorMessages(getHistogramName(Form("hdEdxVsMom%s", momName.Data()), id, kTRUE)) ;

      mCanvas->cd();
      mCanvas->Update();
      mPDF->NewPage();

      delete header ;

      return kFALSE ;
    }

    header = initCanvas(headerTitle, 1, 2);

    hdEdxVsMomEmbed->SetLineColor(kRed);
    hdEdxVsMomEmbed->SetMarkerColor(kRed);

    // Real data
    TH2* hdEdxVsMomReal = (TH2D*) getHistogram("hdEdxVsMomReco", id, kFALSE);
//    if(!hdEdxVsMomReal) return kFALSE ;

    if( hdEdxVsMomReal ){
      hdEdxVsMomReal->SetLineColor(kBlack);
      hdEdxVsMomReal->SetMarkerColor(kBlack);
    }

    // Real data (with PID)
    TH2* hdEdxVsMomPidReal = (TH2D*) getHistogram("hdEdxVsMomRecoPidCut", id, kFALSE);
//    if(!hdEdxVsMomPidReal) return kFALSE ;

    if( hdEdxVsMomPidReal ){
      hdEdxVsMomPidReal->SetLineColor(kBlue);
      hdEdxVsMomPidReal->SetMarkerColor(kBlue);
    }

    // dE/dx vs momentum
    mMainPad->cd(1);
    hdEdxVsMomEmbed->SetAxisRange(0, mPtMax, "X");
    hdEdxVsMomEmbed->SetXTitle(Form("%s momentum (GeV/c)", momName.Data()));
    hdEdxVsMomEmbed->SetTitle("");
    hdEdxVsMomEmbed->SetMinimum(0);

    hdEdxVsMomEmbed->Draw("box");
    if( hdEdxVsMomReal )    hdEdxVsMomReal->Draw("box same");
    if( hdEdxVsMomPidReal ) hdEdxVsMomPidReal->Draw("box same");
    hdEdxVsMomEmbed->Draw("box same");

    mMainPad->cd(2);

    const StEmbeddingQAUtilities* utility = StEmbeddingQAUtilities::instance() ;
    TLegend* leg = new TLegend(0.1, 0.25, 0.9, 0.9);
    leg->SetFillColor(10);
    leg->SetTextSize(0.05);
    leg->AddEntry( hdEdxVsMomEmbed, getEmbeddingParticleName(id), "L");
    if( hdEdxVsMomReal )    leg->AddEntry( hdEdxVsMomReal, "Real data", "L");
    if( hdEdxVsMomPidReal ) leg->AddEntry( hdEdxVsMomPidReal, Form("Real data with PID cut (#sigma<2) %s",utility->getBTofPid()?"BTOF":"TPC"), "L");
    leg->Draw();

    mCanvas->cd();
    mCanvas->Update();

    mPDF->NewPage();

    delete header ;

    //----------------------------------------------------------------------------------------------------
    /// 1D projections for each momentum bin
    ///  From 0.2 GeV/c to 5.0 GeV/c (5*5)
    //----------------------------------------------------------------------------------------------------
    headerTitle = "Projection of dE/dx for each p bin";
//    header = initCanvas(headerTitle, 3, 3);
    header = initCanvas(headerTitle, 2, 4);
    Int_t ipad = 1 ;
//    const Int_t npad = 8 ;
//    const Int_t npadMax = 9;
    const Int_t npad = 6 ;
    const Int_t npadMax = 8;

    TGraphErrors* gMeanVsMom[2];  // 0:embedding, 1:real data
    TGraphErrors* gSigmaVsMom[2]; // 0:embedding, 1:real data

    for(Int_t i=0; i<2; i++){
      gMeanVsMom[i]  = new TGraphErrors();
      gSigmaVsMom[i] = new TGraphErrors();
      gMeanVsMom[i] ->SetMarkerStyle(24 - i*4);
      gSigmaVsMom[i]->SetMarkerStyle(24 - i*4);
    }
    gMeanVsMom[0] ->SetMarkerColor(kRed);    gMeanVsMom[0] ->SetLineColor(kRed);
    gSigmaVsMom[0]->SetMarkerColor(kRed);    gSigmaVsMom[0]->SetLineColor(kRed);
    gMeanVsMom[1] ->SetMarkerColor(kBlue);   gMeanVsMom[1] ->SetLineColor(kBlue);
    gSigmaVsMom[1]->SetMarkerColor(kBlue);   gSigmaVsMom[1]->SetLineColor(kBlue);

    const Int_t npt      = 24 ;
    const Double_t ptBin = 0.2 ;
    for(Int_t ipt=0; ipt<npt; ipt++){
      if( ipad % (npad+1) == 0 ){
        mCanvas->cd();
        mCanvas->Update();
 
        mPDF->NewPage();
 
        if(header) delete header ;
        header = initCanvas(headerTitle, 2, 4);
        ipad = 1 ;
      }

      const Double_t ptMin = 0.2 + ipt*ptBin ;
      const Double_t ptMax = ptMin + ptBin ;
      const Int_t ptMinBin = hdEdxVsMomEmbed->GetXaxis()->FindBin(ptMin);
      const Int_t ptMaxBin = hdEdxVsMomEmbed->GetXaxis()->FindBin(ptMax-0.001);
      if( ptMinBin == ptMaxBin ){
        LOG_INFO << Form("%1.1f - %1.1f GeV/c : bin = (%4d, %4d)", ptMin, ptMax, ptMinBin, ptMaxBin) << endm;
      }

      // Projections
      TH1* hdEdxEmbed = (TH1D*) hdEdxVsMomEmbed->ProjectionY(Form("hdEdxEmbed%s_%d_%d", momName.Data(), id, ipt), ptMinBin, ptMaxBin);
      TH1* hdEdxReal  = 0;
      if( hdEdxVsMomPidReal )
        hdEdxReal = (TH1D*) hdEdxVsMomPidReal->ProjectionY(Form("hdEdxReal%s_%d_%d", momName.Data(), id, ipt), ptMinBin, ptMaxBin);

      hdEdxEmbed->Sumw2() ;
      hdEdxEmbed->Scale( getNormalization(*hdEdxEmbed) ) ;

      if( hdEdxReal ){
        hdEdxReal->Sumw2() ;
        hdEdxReal->Scale( getNormalization(*hdEdxReal) ) ;
      }

      hdEdxEmbed->SetMinimum(0.0);

      // Use max(real, embed) if real exists
      // Use embed if not
      if( hdEdxReal ){
        hdEdxEmbed->SetMaximum( TMath::Max(hdEdxReal->GetMaximum(), hdEdxEmbed->GetMaximum())*1.2 );
      }
      else{
        hdEdxEmbed->SetMaximum( hdEdxEmbed->GetMaximum()*1.2 );
      }

      hdEdxEmbed->SetTitle(Form("%1.1f < %s p < %1.1f GeV/c", ptMin, momName.Data(), ptMax));
      hdEdxEmbed->SetYTitle("(1/N_{trk})dN/d(dE/dx)");

      mMainPad->cd(ipad);
      hdEdxEmbed->Draw("h");
      if( hdEdxReal ) hdEdxReal->Draw("hsame");
      hdEdxEmbed->Draw("hsame");

      //----------------------------------------------------------------------------------------------------
      // Extract mean and sigma (Oct/22/2009)
      //----------------------------------------------------------------------------------------------------
      TF1 fEmbed(Form("fEmbed%s_%d_%d", momName.Data(), id, ipt), "gaus", 0, 10);
      TF1 fReal(Form("fReal%s_%d_%d", momName.Data(), id, ipt), "gaus", 0, 10);
      fEmbed.SetLineColor(kRed);
      fReal.SetLineColor(kBlue);
      fEmbed.SetLineWidth(1);
      fReal.SetLineWidth(1);

      if( hdEdxReal ){
        hdEdxReal->Fit(fReal.GetName(), "rq0");
        fReal.Draw("same");
      }
      hdEdxEmbed->Fit(fEmbed.GetName(), "rq0");
      fEmbed.Draw("same");

      const Double_t pt = (ptMin+ptMax)/2.0 ;
      gMeanVsMom[0]->SetPoint(ipt, pt, fEmbed.GetParameter(1));
      gMeanVsMom[0]->SetPointError(ipt, 0.0, fEmbed.GetParError(1));
      gSigmaVsMom[0]->SetPoint(ipt, pt, fEmbed.GetParameter(2));
      gSigmaVsMom[0]->SetPointError(ipt, 0.0, fEmbed.GetParError(2));

      if( hdEdxReal ){
        gMeanVsMom[1]->SetPoint(ipt, pt, fReal.GetParameter(1));
        gMeanVsMom[1]->SetPointError(ipt, 0.0, fReal.GetParError(1));
        gSigmaVsMom[1]->SetPoint(ipt, pt, fReal.GetParameter(2));
        gSigmaVsMom[1]->SetPointError(ipt, 0.0, fReal.GetParError(2));
      }
      else{
        gMeanVsMom[1]->SetPoint(ipt, pt, -9999.);
        gMeanVsMom[1]->SetPointError(ipt, 0.0, 0.0);
        gSigmaVsMom[1]->SetPoint(ipt, pt, -9999.);
        gSigmaVsMom[1]->SetPointError(ipt, 0.0, 0.0);
      }

      // Draw legend in the last pad in each Canvas
      if( ipad == 1 ){
        mMainPad->cd(npadMax);
        drawLegend(id, hdEdxEmbed, hdEdxReal, "L", kTRUE) ;
//        TLegend* leg = new TLegend(0, 0.2, 1, 0.8);
//        leg->SetFillColor(10);
//        leg->SetTextFont(43);
//        leg->SetTextSize(15);
//        leg->AddEntry( hdEdxEmbed, getEmbeddingParticleName(id, kTRUE), "L");
//        if( hdEdxReal ) leg->AddEntry( hdEdxReal, getRealParticleName(id, kTRUE), "L");
//        leg->Draw();
      }

      ipad++;
    }// pt loop

    mCanvas->cd();
    mCanvas->Update();

    mPDF->NewPage();

    delete header ;

    //----------------------------------------------------------------------------------------------------
    /// Mean/Sigma vs momentum (real vs embed)
    //----------------------------------------------------------------------------------------------------
    headerTitle = "Mean/#sigma of dE/dx vs momentum";
    header = initCanvas(headerTitle, 1, 2);

    for(Int_t i=0; i<2; i++){
      mMainPad->cd(i+1);

      const Double_t ymax = (i==0) ? 7.2 : 1.4 ;
      TH1* frame = mMainPad->GetPad(i+1)->DrawFrame(0, 0.0, 5.0, ymax);
      frame->SetXTitle(Form("%s momentum (GeV/c)", momName.Data()));
      if( i == 0 ) frame->SetYTitle("Mean (keV/cm)");
      if( i == 1 ) frame->SetYTitle("#sigma (keV/cm)");

      TLegend* leg = new TLegend(0.23, 0.86, 0.92, 0.99);
      leg->SetBorderSize(1);
      leg->SetTextFont(43);
      leg->SetTextSize(15);
      leg->SetFillColor(10);

      if( i == 0 ){
        gMeanVsMom[0]->Draw("P");
        gMeanVsMom[1]->Draw("P");

        leg->AddEntry( gMeanVsMom[0], getEmbeddingParticleName(id), "P");
        leg->AddEntry( gMeanVsMom[1], getRealParticleName(id), "P");
      }
      if( i == 1 ){
        gSigmaVsMom[0]->Draw("P");
        gSigmaVsMom[1]->Draw("P");

        leg->AddEntry( gSigmaVsMom[0], getEmbeddingParticleName(id), "P");
        leg->AddEntry( gSigmaVsMom[1], getRealParticleName(id), "P");
      }
      leg->Draw();
    }

    mCanvas->cd();
    mCanvas->Update();

    mPDF->NewPage() ;

    delete header ;
  }

  return kTRUE ;
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::drawDca() const
{
  /// QA for global DCA distributions for each (pt, eta) slice

  LOG_INFO << "QA for dca distributions ..." << endm;

  /// Make sure (1) input ROOT files and (2) input geantid
  if(!isOpen()) return kFALSE ;

  return drawProjection3D("Dca");
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::drawNHit() const
{
  /// QA for Nfit distributions for each (pt, eta) slice

  LOG_INFO << "QA for NHit distributions ..." << endm;

  /// Make sure (1) input ROOT files and (2) input geantid
  if(!isOpen()) return kFALSE ;

  gStyle->SetPadRightMargin(0.17);

  /// QA for NCommon hit vs NHit
  for(UInt_t id=0; id<mDaughterGeantId.size(); id++){
    TH3* hNCommonHitVsNHit = (TH3D*) getHistogram("hNCommonHitVsNHit", id, kTRUE);
    if ( !hNCommonHitVsNHit ) continue ;

    TString headerTitle("");
    if( mIsEmbeddingOnly ){
      headerTitle = Form("N_{common} vs N_{hit} (Embedding:%s)", getParticleName(mDaughterGeantId[id]) );
    }
    else{
      headerTitle = Form("N_{common} vs N_{hit} (Embedding:%s, Real:%s)", 
            getParticleName(mDaughterGeantId[id]), getParticleName(getGeantIdReal(id)) );
    }

    // pt integrated
    TPaveText* header = initCanvas(headerTitle);

    // Maximum pt = 5 GeV/c for ncommon hit histogram
    hNCommonHitVsNHit->SetAxisRange(0.2, 5.0);
    TH2* hNCommonHitVsNHit2D = (TH2D*) hNCommonHitVsNHit->Project3D("zy");
    hNCommonHitVsNHit2D->SetName(Form("hNCommonHitVsNHit2D_%d", id));
    hNCommonHitVsNHit2D->SetTitle(Form("%1.1f < p_{T} < %1.1f GeV/c", 0.2, 5.0));
    hNCommonHitVsNHit2D->Draw("colz");

    mCanvas->cd();
    mCanvas->Update();
    mPDF->NewPage();
    delete header ;

    // Slices for each pt bin (0.5 GeV/c step)
    if( mIsEmbeddingOnly ){
      headerTitle = Form("N_{common} vs N_{hit}, p_{T} dependence (Embedding:%s)", getParticleName(mDaughterGeantId[id]) );
    }
    else{
      headerTitle = Form("N_{common} vs N_{hit}, p_{T} dependence (Embedding:%s, Real:%s)", 
            getParticleName(mDaughterGeantId[id]), getParticleName(getGeantIdReal(id)) );
    }
    for(Int_t jpt=0; jpt<2; jpt++) {
      TPaveText* header = initCanvas(headerTitle, 2, 3);

      const Int_t npt = hNCommonHitVsNHit->GetNbinsX()/2 ;
      for(Int_t ipt=0; ipt<npt; ipt++) {
        mMainPad->cd(ipt+1);
        const Int_t ptId     = jpt*npt + ipt + 1;
        Double_t ptmin = hNCommonHitVsNHit->GetXaxis()->GetBinLowEdge(ptId) ; 
        Double_t ptmax = hNCommonHitVsNHit->GetXaxis()->GetBinLowEdge(ptId+1) ; 
        if( ptmin == 0.0 ) ptmin = 0.2 ;

        hNCommonHitVsNHit->SetAxisRange(ptmin, ptmax);
        hNCommonHitVsNHit2D = (TH2D*) hNCommonHitVsNHit->Project3D("zy");
        hNCommonHitVsNHit2D->SetName(Form("hNCommonHitVsNHit2D_%d_%d_%d", id, ipt, jpt));
        hNCommonHitVsNHit2D->SetTitle(Form("%1.1f < p_{T} < %1.1f GeV/c", ptmin, ptmax));
        hNCommonHitVsNHit2D->Draw("colz");
      }
      mCanvas->cd();
      mCanvas->Update();
      mPDF->NewPage();
      delete header ;
    }
  }

  gStyle->SetPadRightMargin(0.05);

  return drawProjection3D("NHit") ;
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::drawProjection3D(const TString name) const
{
  /// Utility function to get 1D projection from 3D histogram

  /// Plot histograms in each (pt, eta) space
  /// Suppose the input histogram is TH3 (x:pt, y:eta, z:variable you want to plot)

  /// Force to set batch mode to reduce the processing time
  const Bool_t isBatch = gROOT->IsBatch() ;
  if( !isBatch ){
    LOG_INFO << "Enter batch mode ..." << endm;
    gROOT->SetBatch(kTRUE);
  }

  TString nameLower(name);
  nameLower.ToLower();

  TString headerTitle(Form("%s distribution for (p_{T}, #eta) slices", name.Data()));
  for(UInt_t id=0; id<mDaughterGeantId.size(); id++){
    TPaveText* header = 0 ;

    /// Get 3D histograms
    TH3* h3DEmbed = (TH3D*) getHistogram(Form("h%s", name.Data()), id, kTRUE);
    if(!h3DEmbed){
      // Draw error messages
      header = initCanvas(headerTitle);
      drawErrorMessages(getHistogramName(Form("h%s", name.Data()), id, kTRUE));

      mCanvas->cd();
      mCanvas->Update();
      mPDF->NewPage();

      delete header ;

      return kFALSE ;
    }

    TH3* h3DReal  = (TH3D*) getHistogram(Form("h%s", name.Data()), id, kFALSE);

    const Int_t nPt       = h3DEmbed->GetNbinsX() ;
    const Int_t nEta      = h3DEmbed->GetNbinsY() ;
    const Double_t ptMin  = h3DEmbed->GetXaxis()->GetXmin() ;
    const Double_t ptMax  = h3DEmbed->GetXaxis()->GetXmax() ;
    const Double_t etaMin = h3DEmbed->GetYaxis()->GetXmin() ;
    const Double_t etaMax = h3DEmbed->GetYaxis()->GetXmax() ;
    const Double_t etaBin = (etaMax-etaMin)/(Double_t)nEta ;
    const Double_t ptBin  = (ptMax-ptMin)/(Double_t)nPt ;

    for(Int_t ipt=0; ipt<nPt; ipt++){
      TString pt(Form("%1.1f < p_{T} < %1.1f (GeV/c)", ptMin+ipt*ptBin, ptMin+(ipt+1)*ptBin));
      if( ipt == 0 ) pt = Form("%1.1f < p_{T} < %1.1f (GeV/c)", 0.1, ptMin+(ipt+1)*ptBin);

      TPaveText* header = initCanvas(headerTitle, 2, 3);

      const Int_t npad = 5 ;
      const Int_t npadMax = 6 ;
      Int_t ipad = 1 ;
      for(Int_t ieta=0; ieta<nEta; ieta++){
        if( ipad % (npad+1) == 0 ){
          mCanvas->cd();
          mCanvas->Update();
          mPDF->NewPage();

          if(header) delete header ;
          header = initCanvas(headerTitle, 2, 3);

          ipad = 1 ;
        }

        TString eta(Form("%1.1f < #eta < %1.1f", etaMin+ieta*etaBin, etaMin+(ieta+1)*etaBin));

        TH1* hEmbed = (TH1D*) h3DEmbed->ProjectionZ(Form("h%sEmbed_%d_%d_%d", name.Data(), id, ipt, ieta), ipt+1, ipt+1, ieta+1, ieta+1);
        TH1* hReal  = 0 ;
        if( h3DReal ){
          hReal = (TH1D*) h3DReal->ProjectionZ(Form("h%sReal_%d_%d_%d", name.Data(), id, ipt, ieta), ipt+1, ipt+1, ieta+1, ieta+1);
          hReal ->Sumw2();
          hReal ->Scale( getNormalization(*hReal) );
          hReal ->SetLineColor(kBlue);
        }
        hEmbed->Sumw2();
        hEmbed->Scale( getNormalization(*hEmbed) );
        hEmbed->SetLineColor(kRed);
        hEmbed->SetMinimum(0.0);

        // Set maximum
        // If real data exists, use max(real, embed)
        // If not, use embedding histogram
        if( hReal ){
          hEmbed->SetMaximum( TMath::Max(hReal->GetMaximum(), hEmbed->GetMaximum()) * 1.2 );
        }
        else{
          hEmbed->SetMaximum( hEmbed->GetMaximum() * 1.2 );
        }

        hEmbed->SetTitle(eta + ", " + pt);
        hEmbed->SetYTitle(Form("(1/N_{trk})dN/d%s", name.Data())) ;

        mMainPad->cd(ipad);
        hEmbed->Draw();
        if(hReal) hReal->Draw("hsame");
        hEmbed->Draw("same");

        // Draw legend in the last pad for each Canvas
        if( ipad == 1 ){
          mMainPad->cd(npadMax);
          drawLegend(id, hEmbed, hReal, "L", kTRUE);
        }

        ipad++;
      }// eta loop
      mCanvas->cd();
      mCanvas->Update();
      mPDF->NewPage();

      delete header ;
    }// pt loop
  }// daughter particle loop

//  mCanvas->cd();
//  mCanvas->Update();
// 
//  mPDF->NewPage() ;

  // Back to the original mode
  if( isBatch ) gROOT->SetBatch(kTRUE);  // Stay batch mode if you've started the macro by batch mode
  else          gROOT->SetBatch(kFALSE); // Exit batch mode if you've not

  return kTRUE ;
}

//____________________________________________________________________________________________________
TPaveText* StEmbeddingQADraw::initCanvas(const TString headerTitle, const Int_t nx, const Int_t ny) const
{
  mMainPad->Clear();
  mCanvas->cd();

  /// Drawing header in the current canvas
  TPaveText* header = drawHeader(headerTitle);

  mMainPad->cd();
  if( nx != 0 && ny != 0 ) mMainPad->Divide(nx, ny);

  return header ;
}

//____________________________________________________________________________________________________
Double_t StEmbeddingQADraw::getVzAcceptedMinimum() const
{
  /// Get minimum vz value from histogram
//  TH1* hVzAccepted = (TH1D*) getHistogram("hVzAccepted");
// 
//  Double_t vzMin = -200.0 ;
//  for(Int_t ix=0; ix<hVzAccepted->GetNbinsX(); ix++){
//    const Double_t count = hVzAccepted->GetBinContent(ix+1);
//    if(count!=0){
//      vzMin = hVzAccepted->GetBinLowEdge(ix+1);
//      LOG_INFO << "Find minimum vz cut, v_z(min) = " << vzMin << " (cm) " << endm ;
//      break ;
//    }
//  }
// 
//  return vzMin ;

  // Use actual cut value 
  return -StEmbeddingQAUtilities::instance()->getZVertexCut() ;
}

//____________________________________________________________________________________________________
Double_t StEmbeddingQADraw::getVzAcceptedMaximum() const
{
  /// Get maximum vz value from histogram
//  TH1* hVzAccepted = (TH1D*) getHistogram("hVzAccepted");
// 
//  Double_t vzMax = 200.0 ;
//  for(Int_t ix=hVzAccepted->GetNbinsX()-1; ix!=-1; ix--){
//    const Double_t count = hVzAccepted->GetBinContent(ix+1);
//    if(count!=0){
//      vzMax = hVzAccepted->GetBinLowEdge(ix+1) + hVzAccepted->GetBinWidth(ix+1);
//      LOG_INFO << "Find maximum vz cut, v_z(max) = " << vzMax << " (cm) " << endm ;
//      break ;
//    }
//  }
// 
//  return vzMax ;

  // Use actual cut value 
  return StEmbeddingQAUtilities::instance()->getZVertexCut() ;
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::draw() const
{
  /// QA for event-wise and track-wise (both MC and reconstructed) histograms

  //====================================================================================================
  //
  // Start drawing
  //
  //====================================================================================================

  //----------------------------------------------------------------------------------------------------
  /// (1) Event-wise informations
  const Bool_t isEventOk = drawEvent();
  //----------------------------------------------------------------------------------------------------

  //----------------------------------------------------------------------------------------------------
  /// (2-1) Track-wise informations (MC)
  const Bool_t isMcTrackOk = drawMcTrack();
  //----------------------------------------------------------------------------------------------------

  //----------------------------------------------------------------------------------------------------
  /// (2-2) Track-wise informations (Real vs Embedding)
  const Bool_t isTrackOk = drawTrack();
  //----------------------------------------------------------------------------------------------------

  return isEventOk && isMcTrackOk && isTrackOk ;
}

//____________________________________________________________________________________________________
Bool_t StEmbeddingQADraw::finish()
{
  // Close PDF file
  LOG_INFO << "StEmbeddingQADraw::finish()  Close PDF file : " << mPDF->GetName() << endm ;

  mMainPad->cd();
  mMainPad->Clear();

  TLatex* title = new TLatex(0.1, 0.6, "End of QA");
  title->SetTextSize(0.1);
  title->Draw();

  mCanvas->cd();
  mCanvas->Update();

  mPDF->Close() ;

  return kTRUE ;
}

void StEmbeddingQADraw::setPNGOn() { LOG_INFO << endm << "Print figures to PNG file" << endm << endm; mIsPNGOn = kTRUE ; }
void StEmbeddingQADraw::setGIFOn() { LOG_INFO << endm << "Print figures to GIF file" << endm << endm; mIsGIFOn = kTRUE ; }
void StEmbeddingQADraw::setJPGOn() { LOG_INFO << endm << "Print figures to JPG file" << endm << endm; mIsJPGOn = kTRUE ; }
void StEmbeddingQADraw::setEPSOn() { LOG_INFO << endm << "Print figures to EPS file" << endm << endm; mIsEPSOn = kTRUE ; }
void StEmbeddingQADraw::setPSOn()  { LOG_INFO << endm << "Print figures to PS file"  << endm << endm; mIsPSOn = kTRUE ; }



