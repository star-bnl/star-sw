// $Id: StHistUtil.cxx,v 2.108 2019/05/22 21:24:30 genevb Exp $
// $Log: StHistUtil.cxx,v $
// Revision 2.108  2019/05/22 21:24:30  genevb
// Add sDCA vs. time-in-run
//
// Revision 2.107  2019/03/26 15:29:35  genevb
// Introduce ETOF
//
// Revision 2.106  2019/03/14 02:31:52  genevb
// Introduce iTPC plots
//
// Revision 2.105  2019/03/04 20:55:59  genevb
// Improve RDO layout for iTPC
//
// Revision 2.104  2019/02/25 19:20:18  genevb
// Sector numbering fix
//
// Revision 2.103  2018/07/06 22:13:04  smirnovd
// [Cosmetic] Changes in white space
//
// Revision 2.102  2018/07/06 22:10:26  smirnovd
// [Cosmetic] Inverse test conditions to skip loop iterations
//
// Revision 2.101  2018/05/05 04:00:54  genevb
// iTPC RDO outlines
//
// Revision 2.100  2018/05/02 21:06:32  genevb
// Initial accomodation for iTPC
//
// Revision 2.99  2016/06/13 20:31:10  genevb
// Resolve Coverity BUFFER_SIZE_WARNING with careful copy function
//
// Revision 2.98  2016/06/10 02:55:54  genevb
// Coverity: memory leaks, possible null pointer dereferences, over-write character buffers
//
// Revision 2.97  2016/03/16 20:39:21  genevb
// remove accidental extraneous line
//
// Revision 2.96  2016/03/16 20:34:43  genevb
// Histogram list by subsystem, single TPC sector reference choice, and a couple histogram minima set
//
// Revision 2.95  2015/01/21 17:30:33  genevb
// Provide histogram normalization
//
// Revision 2.94  2014/04/10 17:58:40  genevb
// Fix for dE/dx slope plot
//
// Revision 2.93  2014/02/20 20:16:19  genevb
// Adjust dE/dx slope hist range, handle ROOT change for 2D polar plots
//
// Revision 2.92  2014/01/30 19:44:06  genevb
// Additional TPC histogram for monitoring gas contamination
//
// Revision 2.91  2013/03/12 03:42:55  genevb
// typo correction
//
// Revision 2.90  2013/03/12 03:41:18  genevb
// handle fms/fpd naming for now
//
// Revision 2.89  2013/03/12 03:05:43  genevb
// Add FMS/FPD histograms for Run 13+
//
// Revision 2.88  2012/05/01 18:37:19  genevb
// ZCol for Vtx XY distribution
//
// Revision 2.87  2012/03/07 02:04:10  genevb
// Differentiate event counts for Reference
//
// Revision 2.86  2012/03/03 01:29:06  genevb
// Output found/total vertices
//
// Revision 2.85  2012/01/31 22:14:53  genevb
// QA Shift Mode, optimized for AutoQA Browser
//
// Revision 2.84  2011/05/24 20:50:43  genevb
// Allow limited graphics file printing
//
// Revision 2.83  2011/05/13 21:12:58  genevb
// Hide original prefixes in title when plotting reference hists
//
// Revision 2.82  2011/03/15 21:05:25  genevb
// TPC hit phi sector labels
//
// Revision 2.81  2011/02/23 20:56:56  genevb
// Default to general histograms for references in absence of trig typed
//
// Revision 2.80  2011/02/23 18:46:29  genevb
// Fixed a bug introduced in version 2.71 when adding hists
//
// Revision 2.79  2011/02/19 02:43:39  genevb
// Fix those missing consts
//
// Revision 2.78  2011/02/19 02:22:18  genevb
// Allow for specification of histogram usage by the required detector sets
//
// Revision 2.77  2011/02/07 20:25:26  genevb
// Allow for limiting detectors
//
// Revision 2.76  2011/01/28 18:47:55  genevb
// Better handling of dirName
//
// Revision 2.75  2011/01/24 18:36:28  genevb
// Save hist list to results file even if not comparing
//
// Revision 2.74  2011/01/19 02:05:22  genevb
// Allow plain ROOT files with hists, and individual plot generation from 1 file
//
// Revision 2.73  2010/12/13 16:23:26  genevb
// Remove previous change, add Kolmogorov maximum distance as default
//
// Revision 2.72  2010/12/11 23:28:00  genevb
// Allow negative modes for 1-result
//
// Revision 2.71  2010/04/19 20:43:49  genevb
// Fixed bug with AddHists introduced in last fix
//
// Revision 2.70  2010/04/19 19:11:13  genevb
// Fixed bug with AddHists when some files are missing hists
//
// Revision 2.69  2010/04/09 21:13:28  genevb
// Use hobj pointer to ensure proper handling with reference hists
//
// Revision 2.68  2010/03/17 02:53:06  genevb
// Add hists even if not in first file
//
// Revision 2.67  2010/03/12 17:28:15  genevb
// Same ROOT quirk fix as done in rev. 2.64, but for reference hists
//
// Revision 2.66  2010/03/12 07:29:05  genevb
// Additional capability for saving images of each pad
//
// Revision 2.65  2010/03/08 18:04:33  genevb
// Include analysis score/result on plots
//
// Revision 2.64  2010/01/14 19:29:53  genevb
// Fix ROOT quirk with 1 page print, fix string/char conversions, protect LOG calls
//
// Revision 2.63  2009/05/11 17:52:14  genevb
// Fix fit-not-attached-to-hist-for-empty-hists, needed for ROOT 5.22
//
// Revision 2.62  2009/05/06 17:19:09  genevb
// Avoid PDF bug with text at angles
//
// Revision 2.61  2009/05/05 23:16:12  genevb
// Draw TPC sector boundaries and labels
//
// Revision 2.60  2009/05/05 00:31:07  genevb
// Add Anode guide lines in TPC Sector plots
//
// Revision 2.59  2009/05/04 23:37:40  genevb
// Add RDO boundary lines in TPC Sector plots
//
// Revision 2.58  2009/04/18 02:55:06  genevb
// Larger arrays for more trigger type hists
//
// Revision 2.57  2009/04/05 14:37:59  genevb
// Catch missing files
//
// Revision 2.56  2009/03/27 21:18:36  genevb
// Add Jet Patch trigger histograms
//
// Revision 2.55  2009/03/25 02:42:30  genevb
// Box --> Col for 2D plots
//
// Revision 2.54  2009/03/23 23:50:16  genevb
// Add color and logarithmic scales to TPC hit location plots
//
// Revision 2.53  2009/03/19 17:43:42  genevb
// Catch XYTpc hists which were polar
//
// Revision 2.52  2009/03/19 01:08:08  genevb
// Show both xy and rphi TPC hit hists
//
// Revision 2.51  2009/03/13 21:45:40  genevb
// Remove unhelpful stats
//
// Revision 2.50  2009/03/13 19:27:24  genevb
// Now draw TPC xy hits in polar coords
//
// Revision 2.49  2009/01/17 01:48:54  genevb
// Fixed broken multi-page output introduced in previous commit
//
// Revision 2.48  2009/01/08 23:40:13  genevb
// Introduce analyses with reference histograms
//
// Revision 2.47  2008/07/10 21:25:08  genevb
// Add canvas-to-code output with .CC suffix
//
// Revision 2.46  2008/07/09 20:52:16  genevb
// allow saving histograms to plain ROOT file
//
// Revision 2.45  2008/05/30 05:48:03  genevb
// Add ability to write out histogram data to .C files
//
// Revision 2.44  2008/05/28 05:16:06  genevb
// Allow summing over (ignoring) histogram prefixes
//
// Revision 2.43  2008/05/23 17:54:54  genevb
// Allow subset histogram list when copying/extracting
//
// Revision 2.42  2008/05/23 17:09:22  genevb
// Allow the use of any file for PrintList specification
//
// Revision 2.41  2008/05/15 19:37:17  genevb
// Changes to FTPC radial step plots/fits
//
// Revision 2.40  2008/02/06 23:31:32  genevb
// missing reset of logZ
//
// Revision 2.39  2007/12/14 02:18:29  genevb
// Add text of counts on NullPrimVtx plots
//
// Revision 2.38  2007/12/13 23:17:45  genevb
// Force 0 minimum on FTPC radial hists
//
// Revision 2.37  2007/04/28 20:36:15  perev
// Redundant StChain.h removed
//
// Revision 2.36  2007/04/24 00:33:06  genevb
// SSD hists; Logz for Dedx
//
// Revision 2.35  2007/04/20 03:42:35  genevb
// cout -> LOG_INFO
//
// Revision 2.34  2007/04/20 01:11:11  genevb
// ZCol on Dedx; printf -> LOG_INFO
//
// Revision 2.33  2007/04/07 04:39:04  genevb
// Use ZCol for PointXYTpc
//
// Revision 2.32  2007/03/13 18:42:27  genevb
// Add Svt list, simplified hlist include files, handle StMultiH2F, store dirName
//
// Revision 2.31  2007/02/26 20:45:00  genevb
// SVT drift hist
//
// Revision 2.30  2007/01/03 19:03:41  genevb
// Patch for hist titles removed after migration to Root vers. 5
//
// Revision 2.29  2006/05/18 16:38:03  genevb
// Introduce StHistUtil::GetRunYear()
//
// Revision 2.28  2006/04/24 21:23:13  genevb
// Fix problem with overlayed hists, and Patch for missing titles
//
// Revision 2.27  2006/03/28 21:35:31  genevb
// Single page output capability for eps,jpg,png,gif,tiff,etc. [see TPad::Print()]
//
// Revision 2.26  2006/03/28 01:58:38  genevb
// Allow PDF (and other) output formats (was only PostScript)
//
// Revision 2.25  2005/04/19 15:14:17  genevb
// Slight reordering of some FTPC code on user ranges in radial hists
//
// Revision 2.24  2005/02/08 17:12:37  genevb
// Limiting range on some PMD histos
//
// Revision 2.23  2004/12/13 15:52:35  genevb
// Numerous updates: PMD, primtrk, FPD, QAShift lists
//
// Revision 2.22  2004/10/04 16:40:41  genevb
// FTPC radial histos
//
// Revision 2.21  2004/06/09 22:01:31  genevb
// Modify line parameter for FTPC hist
//
// Revision 2.20  2004/05/24 15:13:10  genevb
// Range limit on FTPC radial step hists
//
// Revision 2.19  2004/02/12 16:54:22  genevb
// Separate MinBias histos
//
// Revision 2.18  2004/02/12 05:02:59  genevb
// Year 4 AuAu changes. New SVT histos.
//
// Revision 2.17  2004/02/10 16:31:15  genevb
// A few extra histo lines, features
//
// Revision 2.16  2003/09/02 17:55:26  perev
// gcc 3.2 updates + WarnOff
//
// Revision 2.15  2003/02/20 20:08:36  genevb
// Add new prefixes, other small modifications
//
// Revision 2.14  2003/02/15 22:00:14  genevb
// Add tpcSectors
//
// Revision 2.13  2003/01/22 21:32:46  genevb
// Fix Solaris compilation errors
//
// Revision 2.12  2002/09/06 02:51:34  genevb
// Remove limit on maximum number of histograms that can be copied
//
// Revision 2.11  2002/04/23 01:59:54  genevb
// Addition of BBC/FPD histos
//
// Revision 2.10  2002/02/12 18:41:57  genevb
// Additional FTPC histograms
//
// Revision 2.9  2002/01/26 03:04:05  genevb
// Fixed some problems with fcl histos
//
// Revision 2.8  2002/01/21 22:09:23  genevb
// Include some ftpc histograms from StFtpcClusterMaker
//
// Revision 2.7  2001/08/27 21:16:03  genevb
// Better filename hanlding
//
// Revision 2.6  2001/05/24 01:47:42  lansdell
// minor bug fixes and updates
//
// Revision 2.5  2001/05/23 00:14:27  lansdell
// added some logx code
//
// Revision 2.4  2001/04/25 14:17:16  genevb
// Reduced line width for newer Root
//
// Revision 2.3  2000/09/15 21:19:10  fisyak
// HPUX does not like delete [] newHist
//
// Revision 2.2  2000/08/28 19:21:40  genevb
// Plot MultiH1F hists like 1d
//
// Revision 2.1  2000/08/25 22:06:50  genevb
// Added histo descriptor in top right
//
// Revision 2.0  2000/08/25 15:47:38  genevb
// New revision: cleaned up, multiple PS files
//
//
///////////////////////////////////////////////////////////////////////////////
// Histogram Utility methods for use with star makers and bfc output
///////////////////////////////////////////////////////////////////////////////
                                                                          

#include <Stiostream.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <Stsstream.h>
#include "TFile.h"
#include "TROOT.h"
#include "PhysicalConstants.h"
#include "TStyle.h"
#include "TCanvas.h"
#include "TObjString.h"
#include "TMath.h"
#include "TString.h"
#include "TPaveLabel.h"
#include "TPaveText.h"
#include "TLegend.h"
#include "TDatime.h"
#include "TLine.h"
#include "TLatex.h"
#include "StMessMgr.h"

#include "St_DataSetIter.h"
#include "StMaker.h"
#include "TF1.h"
#include "TH3.h"
#include "TProfile.h"

#include "StHistUtil.h"

typedef TH1* TH1ptr;
typedef const char* charptr;

const char* possibleQAPrefixes[10] = {"","LM","MM","HM","HP","XX","MB","CL","HT","JP"};
const char* possibleQASuffixes[10] = {
  "General",
  "Low Mult",
  "Mid Mult",
  "High Mult",
  "High Pt",
  "Other Physics",
  "MinBias",
  "Central",
  "High Tower",
  "Jet Patch"
};

enum QAprintModes {QAprintSet,
                   QAprintSetRef,
                   QAprintIndiv,
                   QAprintIndivRef};
UInt_t QAU1 = 1;

int sizeOfCharPtr = sizeof(Char_t*);
int sizeOfTH1Ptr = sizeof(TH1*);

//_____________________________________________________________________________
// copied from StdEdxY2Maker vers. 1.83,
//   avoids loading that library just for these
Double_t StdEdxY2Maker_gaus2(Double_t *x, Double_t *p) {
  Double_t NormL = p[0];
  Double_t mu    = p[1];
  Double_t muP   = mu + p[4];
  Double_t sigma = p[2];
  Double_t sigmaP = TMath::Sqrt(sigma*sigma + 0.101741*0.101741);
  Double_t phi   = p[3];
  Double_t frac = TMath::Sin(phi);
  frac *= frac;
  return TMath::Exp(NormL)*((1 - frac)*TMath::Gaus(x[0],mu ,sigma ,kTRUE) + 
			    frac      *TMath::Gaus(x[0],muP,sigmaP,kTRUE)); 
}
TF1 *StdEdxY2Maker_Gaus2() {
  TF1 *f = new TF1("Gaus2",StdEdxY2Maker_gaus2,-3,3,5);
  f->SetParName(0,"NormL"); f->SetParLimits(0,-10.,10.);
  f->SetParName(1,"mu");    f->SetParLimits(1,-0.5,0.5);
  f->SetParName(2,"sigma"); f->SetParLimits(2, 0.2,0.5);
  f->SetParName(3,"phiP");  f->SetParLimits(3, 0.0,TMath::Pi()/4);
  f->SetParName(4,"muP");
  f->SetParameters(10,0,0.3,0.1,1.315);
  //  f->FixParameter(4,1.425);
  return f;
}
//_____________________________________________________________________________

ClassImp(StHistUtil)
  
//_____________________________________________________________________________

// Constructor

StHistUtil::StHistUtil(){

  numOfPosPrefixes = 10;
  possiblePrefixes = possibleQAPrefixes;
  possibleSuffixes = possibleQASuffixes;

  m_ListOfLogY = 0;
  m_ListOfLogX = 0;
  m_ListOfPrint = 0;
  m_HistCanvas = 0;
  m_HistCanvasR = 0;
  debug = kFALSE;
  m_CurPrefix = -1;
  m_CurPage = 0;
  m_CurFileName = "";
  m_OutFileName = "";
  m_OutType = "ps"; // postscript output by default
  m_PrintMode = 0;
  m_OutMultiPage = kTRUE;
  m_OutIndividuals = "";
  m_QAShiftMode = kFALSE;
  m_RunYear = 0;

  Ltitle = 0;
  Ldesc = 0;

  maxHistCopy = 4096;
  newHist = new TH1ptr[maxHistCopy];
  memset(newHist,0,maxHistCopy*sizeOfTH1Ptr);
  m_dirName[0] = 0;

  ignorePrefixes = kFALSE;

  m_analMode = kFALSE;
  m_refResultsFile[0] = 0;
  m_refOutFile[0] = 0;
  m_refCuts = 0;
  m_refInFile = 0;
  m_PntrToMaker = 0;
  m_PntrToPlainFile = 0;

  m_PadColumns = 0;
  m_PadRows = 0;
  m_PaperWidth = 0;
  m_PaperHeight = 0;

  m_Detectors = "";
}
//_____________________________________________________________________________

// Destructor

StHistUtil::~StHistUtil(){
  SafeDelete(m_HistCanvas);
  SafeDelete(m_HistCanvasR);
  if (m_ListOfLogY) {
    m_ListOfLogY->Delete();
    SafeDelete(m_ListOfLogY);
  }
  if (m_ListOfLogX) {
    m_ListOfLogX->Delete();
    SafeDelete(m_ListOfLogX);
  }
  if (m_ListOfPrint) {
    m_ListOfPrint->Delete();
    SafeDelete(m_ListOfPrint);
  }
  if (newHist) {
    for (int ijk=0; ijk<maxHistCopy; ijk++) delete newHist[ijk];
    delete [] newHist;
  }
}
//_____________________________________________________________________________
void StHistUtil::SetOutFile(const Char_t *fileName, const Char_t* type) {
  m_OutFileName = fileName;
  if (m_OutFileName.EndsWith("+")) {
    m_OutIndividuals = ".eps"; // only option working in ROOT 5.22
    m_PrintMode |= QAU1<<QAprintIndiv;
    if (m_OutFileName.EndsWith("++")) m_PrintMode |= QAU1<<QAprintIndivRef;
    while (m_OutFileName.EndsWith("+")) m_OutFileName.Chop();
  }
  if (type) {
    m_OutType = type;
  } else {
    if (m_OutFileName.EndsWith(".ps")) m_OutType="ps";
    else if (m_OutFileName.EndsWith(".eps")) m_OutType="eps";
    else if (m_OutFileName.EndsWith(".epsf")) m_OutType="Preview";
    else if (m_OutFileName.EndsWith(".pdf")) m_OutType="pdf";
    else if (m_OutFileName.EndsWith(".jpg")) m_OutType="jpg";
    else if (m_OutFileName.EndsWith(".jpeg")) m_OutType="jpg";
    else if (m_OutFileName.EndsWith(".gif")) m_OutType="gif";
    else if (m_OutFileName.EndsWith(".tif")) m_OutType="tiff";
    else if (m_OutFileName.EndsWith(".tiff")) m_OutType="tiff";
    else if (m_OutFileName.EndsWith(".svg")) m_OutType="svg";
    else if (m_OutFileName.EndsWith(".xpm")) m_OutType="xpm";
    else if (m_OutFileName.EndsWith(".png")) m_OutType="png";
    else if (m_OutFileName.EndsWith(".CC")) m_OutType="CC"; // Save canvases as code
    else if (m_OutFileName.EndsWith(".C")) m_OutType="C"; // Save histograms as code
    else if (m_OutFileName.EndsWith(".root")) m_OutType="root";
    else if (m_OutFileName.EndsWith("none")) m_OutType="none"; // No output set
    else if (m_OutFileName.EndsWith(".qas")) m_OutType="qas"; // QA Shift mode
    else {
      LOG_INFO << "SetHistUtil::SetOutFile(): unknown type, assuming ps" << endm;
      m_OutType = "ps";
      m_OutFileName.Append(".ps");
    }
  }
  if (!m_OutType.CompareTo("qas")) { // QA Shift mode options
    m_QAShiftMode = kTRUE;
    m_OutIndividuals = ".svg";
    m_PrintMode |= QAU1<<QAprintIndiv;
    m_PrintMode |= QAU1<<QAprintIndivRef;
    m_OutType = "none";
  }
  if (m_OutType.CompareTo("none")) {
    m_PrintMode |= QAU1<<QAprintSet;
    m_PrintMode |= QAU1<<QAprintSetRef;
  }

  // Multipage output for ps,pdf
  m_OutMultiPage = !(m_OutType.CompareTo("ps")
                  && m_OutType.CompareTo("pdf") );
  if (m_OutMultiPage) {
    LOG_INFO << "StHistUtil::SetOutFile(): Multipage output" << endm;
  } else {
    LOG_INFO << "StHistUtil::SetOutFile(): Single page output" << endm;
  }
}
//_____________________________________________________________________________
void StHistUtil::CloseOutFile() {
  m_HistCanvas->Modified();
  m_HistCanvas->Update();
  if (!m_CurFileName.IsNull()) {
    if (m_OutMultiPage) m_CurFileName.Append(")");
    if (m_OutType.CompareTo("CC")) {
      // single page seems to have a bug with "()" notation as of Root 5.22.00
      if (m_CurPage==1) m_CurFileName.Chop().Chop();
      if (m_PrintMode & QAU1<<QAprintSet)
        m_HistCanvas->Print(m_CurFileName.Data(),m_OutType.Data());
    } else
      m_HistCanvas->SaveSource(m_CurFileName.Data());
    if (m_refInFile) {
      m_HistCanvasR->Modified();
      m_HistCanvasR->Update();
      m_CurFileNameR.Append(")");
      // single page seems to have a bug with "()" notation as of Root 5.22.00
      if (m_CurPage==1) m_CurFileNameR.Chop().Chop();
      if (m_PrintMode & QAU1<<QAprintSetRef)
        m_HistCanvasR->Print(m_CurFileNameR.Data(),m_OutType.Data());
      // anal mode doesn't support single page output
    }
  } else {
    LOG_INFO << "StHistUtil::CloseOutFile(): No output file" << endm;
  }
}
//_____________________________________________________________________________
TString StHistUtil::StripPrefixes(const Char_t* histName, Int_t& prenum, Int_t mode) {
  // mode < 0 : strip maker name
  // mode > 0 : strip trigger prefix
  // mode = 0 : strip both
  // Figure out and strip appropriate prefix index
  TString hName(histName);
  Char_t makerBuffer[4];
  memset(makerBuffer,0,4);
  if ((hName.BeginsWith("Tab")) || (hName.BeginsWith("StE"))) {
    memcpy(makerBuffer,histName,3);
    hName.Remove(0,3);
  }
  prenum = 0; // Possible prefix=0 means no prefix
  if (mode >= 0) {
    for (Int_t i=1; i<numOfPosPrefixes; i++) {
      if (hName.BeginsWith(possiblePrefixes[i])) {
        prenum = i;
        hName.Remove(0,strlen(possiblePrefixes[i]));
        break;
      }
    }
    if (mode>0) hName.Prepend(makerBuffer);
  }
  return hName;
}
//_____________________________________________________________________________
Bool_t StHistUtil::CheckOutFile(const Char_t *histName) {
// Method to determine appropriate PostScript file for output

  // Figure out appropriate prefix index
  Int_t newPrefix = -1;
  StripPrefixes(histName,newPrefix);

  if (newPrefix == m_CurPrefix) return kFALSE;

  CloseOutFile();
  m_CurPrefix = newPrefix;
  if (m_OutType.CompareTo("C") && m_OutType.CompareTo("root")) {
    m_CurFileName = m_OutFileName;
    Ssiz_t insertPos = m_CurFileName.Last('.');
    if (insertPos<0) insertPos = m_CurFileName.Length();
    if (m_OutMultiPage) m_CurFileName.Append("(");
    else m_CurFileName.Insert(insertPos,"_");
    m_CurFileName.Insert(insertPos,possiblePrefixes[m_CurPrefix]);
  }
  (m_CurFileNameR = "Ref_") += m_CurFileName;

  if (Ldesc) {
    Ldesc->Clear();
    Ldesc->AddText(possibleSuffixes[m_CurPrefix]);
    Ldesc->AddText("Hists");
  }
  return kTRUE;
}
//_____________________________________________________________________________
Int_t StHistUtil::DrawHists(const Char_t *dirName) {
// Method DrawHists -->
// Plot the selected  histograms and generate the postscript file as well 
  
  LOG_INFO << " **** Now in StHistUtil::DrawHists  **** " << endm;

  Int_t canvasWidth,canvasHeight;

  if (m_QAShiftMode) {
    LOG_INFO << "In QA Shift Mode - overriding other inputs" << endm;
    m_PadColumns=1;
    m_PadRows=1;
    canvasWidth = 250;
    canvasHeight = 250;
  } else {
    // SetPaperSize wants width & height in cm: A4 is 20,26 & US is 20,24
    gStyle->SetPaperSize(m_PaperWidth,m_PaperHeight); 
    // TCanvas wants width & height in pixels (712 x 950 corresponds to A4 paper)
    //                                        (600 x 780                US      )
    //  TCanvas *m_HistCanvas = new TCanvas("CanvasName","Canvas Title",30*m_PaperWidth,30*m_PaperHeight);
    canvasWidth = 600;
    canvasHeight = 780;
  }

  //set Style of Plots
  const Int_t numPads = m_PadColumns*m_PadRows;  
  gStyle->SetOptStat(111111);
  gStyle->SetStatStyle(0);
  gStyle->SetOptDate(0);
  gStyle->SetPalette(1);

  
  //setup canvas
  SafeDelete(m_HistCanvas);
  SafeDelete(m_HistCanvasR);

  if (m_refInFile) {
    m_HistCanvasR = new TCanvas("CanvasNameR"," STAR Reference Histogram Canvas",20,20,canvasWidth,canvasHeight);
  }
  m_HistCanvas = new TCanvas("CanvasName"," STAR Maker Histogram Canvas",0,0,canvasWidth,canvasHeight);


  TPad *graphPad = m_HistCanvas;
  TPad *graphPadR = m_HistCanvasR;
  TPaveLabel* LtitleR = 0;
  TPaveLabel *Ldatetime = 0;
  TPaveLabel *Lpage = 0;
  m_CurPage=1;

  if (!m_QAShiftMode) {
  // Do not draw page numbers, titles, etc. in QA Shift mode

    // write title at top of canvas - first page
    Ltitle = new TPaveLabel(0.08,0.96,0.88,1.0,m_GlobalTitle.Data(),"br");
    Ltitle->SetFillColor(18);
    Ltitle->SetTextFont(32);
    Ltitle->SetTextSize(0.5);
    Ltitle->Draw();
    if (m_refInFile) {
      m_HistCanvasR->cd();
      LtitleR = new TPaveLabel(0.08,0.96,0.88,1.0,m_refInFile->GetName(),"br");
      LtitleR->SetFillColor(18);
      LtitleR->SetTextFont(32);
      LtitleR->SetTextSize(0.5);
      LtitleR->Draw();
      m_HistCanvas->cd();
    }

    // write descriptor at top of canvas - first page
    Ldesc = new TPaveText(0.90,0.96,0.99,1.0,"br");
    Ldesc->SetFillColor(18);
    Ldesc->SetTextFont(32);
    Ldesc->Draw();
    if (m_refInFile) {
      m_HistCanvasR->cd();
      Ldesc->Draw();
      m_HistCanvas->cd();
    }

    // now put in date & time at bottom right of canvas - first page
    TDatime HistTime;
    const Char_t *myTime = HistTime.AsString();
    Ldatetime = new TPaveLabel(0.7,0.01,0.95,0.03,myTime,"br");
    Ldatetime->SetTextSize(0.6);
    Ldatetime->Draw();
    if (m_refInFile) {
      m_HistCanvasR->cd();
      Ldatetime->Draw();
      m_HistCanvas->cd();
    }


    // now put in page # at bottom left of canvas - first page
    Lpage = new TPaveLabel(0.1,0.01,0.16,0.03,Form("%d",m_CurPage),"br");
    Lpage->SetTextSize(0.6);
    Lpage->Draw();
    if (m_refInFile) {
      m_HistCanvasR->cd();
      Lpage->Draw();
      m_HistCanvas->cd();
    }

    // Make 1 big pad on the canvas - make it a little bit inside the  canvas 
    //    - must cd to get to this pad! 
    // order is x1 y1 x2 y2 
    graphPad = new TPad("PadName","Pad Title",0.0,0.05,1.00,0.95);
    graphPad->Draw();
    graphPad->cd();
    graphPad->Divide(m_PadColumns,m_PadRows);
    if (m_refInFile) {
      m_HistCanvasR->cd();
      graphPadR = new TPad("PadNameR","Pad TitleR",0.0,0.05,1.00,0.95);
      graphPadR->Draw();
      graphPadR->cd();
      graphPadR->Divide(m_PadColumns,m_PadRows);
    graphPad->cd();
    }

  } // !m_QAShiftMode

  Int_t padCount = 0;
  Bool_t padAdvance = kTRUE;


  // Now find the histograms
  // get the TList pointer to the histograms:
  PathCopy(m_dirName,dirName);
  TList* dirList = (m_PntrToMaker ? FindHists(m_dirName) : FindHists(m_PntrToPlainFile));
  if (!dirList) { LOG_INFO << " DrawHists - histograms not available! " << endm; }

  TIter nextHist(dirList);
  Int_t histCounter = 0;
  Int_t histReadCounter = 0;
  Bool_t started = kFALSE;


  TObject *obj = 0;
  TLine ruler;
  TLatex latex;

  ofstream* C_ostr = 0;
  TFile* root_ofile = 0;
  if (!m_OutType.CompareTo("C")) {
    C_ostr = new ofstream(m_OutFileName);
    (*C_ostr) << "   gSystem->Load(\"St_base\");" << endl;
    (*C_ostr) << "   gSystem->Load(\"StUtilities\");" << endl;
    // not supporting ref output
  } else if (!m_OutType.CompareTo("root"))
     root_ofile = new TFile(m_OutFileName,"RECREATE");

  // Reference analyses:
  TList* dirListR = 0;
  ofstream* R_ostr = 0;
  if (m_analMode) {
    dirListR = FindHists(m_refInFile);
    // By default, save histograms to a ROOT file as future reference
    if (!root_ofile) root_ofile = new TFile(m_refOutFile,"RECREATE");
  }

  // function to fit FTPC radial step
  static TF1* fitFRS = 0;
  if (!fitFRS) fitFRS = new TF1("fitFRS","[0]*(x<[1]-[2])+([0]+([3]-[0])*(x-([1]-[2]))/(2*[2]))*(x>[1]-[2])*(x<[1]+[2])+[3]*(x>[1]+[2])",6.5, 9.0);

  while ((obj = nextHist())) {

    if (!obj->InheritsFrom("TH1")) continue;

      TH1* hobj = (TH1*) obj;
      const char* oname = hobj->GetName();
      const char* otitle = hobj->GetTitle();
      TString oName = oname;
      oName.ReplaceAll(' ','_');
      histReadCounter++;
      LOG_INFO << Form(" %d. Reading ... %s::%s; Title=\"%s\"\n",
        histReadCounter,hobj->ClassName(),oname, otitle) << endm;
      if (!started && (m_FirstHistName.CompareTo("*")==0 ||
                       m_FirstHistName.CompareTo(oName)==0))
        started = kTRUE;

// Here is the actual loop over histograms !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      if (started) {
	if (oName.CompareTo(m_LastHistName)==0) started = kFALSE;
	histCounter++;

//...........................................................................


        // If there's no print list, then print all histograms in directory
        // If there is a print list, then only print if hist name is on list
	if (!m_ListOfPrint || (m_ListOfPrint->FindObject(oname))) {

          // Some ROOT output options
          if (root_ofile) {
            root_ofile->cd();
            hobj->Write();
          }
          if (C_ostr) {
            hobj->SavePrimitive(*C_ostr);
            continue;
          }

          // this histogram will actually be printed/drawn!!
          LOG_INFO << Form("  -   %d. Drawing ... %s::%s; Title=\"%s\"\n",
	    histCounter,hobj->ClassName(),oname, otitle) << endm;

          // Switch to a new page...............................
	  if (CheckOutFile(oname)) {
	    padCount = numPads;
	    m_CurPage = 0;
	  }
          if (m_QAShiftMode) {
            graphPad->Clear();
            if (m_refInFile) graphPadR->Clear();
            padCount = 0;
            m_CurPage++;
          } else if (padCount == numPads) {
            // must redraw the histcanvas for each new page!
            m_HistCanvas->Modified();
            m_HistCanvas->Update();
	    if (m_PrintMode & QAU1<<QAprintSet &&
                m_CurPage>0 && !m_CurFileName.IsNull()) {
              if (m_OutType.CompareTo("CC")) {
	        m_HistCanvas->Print(m_CurFileName.Data(),m_OutType.Data());
              } else
                m_HistCanvas->SaveSource(m_CurFileName.Data());
	      m_CurFileName.ReplaceAll("(",0); // doesn't hurt to do > once
            } else {
	      m_HistCanvas->Draw();
	    }

	    while (padCount > 0) graphPad->GetPad(padCount--)->Clear();

            if (m_refInFile) {
              m_HistCanvasR->Modified();
              m_HistCanvasR->Update();
	      if (m_PrintMode & QAU1<<QAprintSetRef &&
	          m_CurPage>0 && !m_CurFileName.IsNull()) {
	        m_HistCanvasR->Print(m_CurFileNameR.Data(),m_OutType.Data());
	        m_CurFileNameR.ReplaceAll("(",0); // doesn't hurt to do > once
	      } else m_HistCanvasR->Draw();
              padCount = numPads;
	      while (padCount > 0) graphPadR->GetPad(padCount--)->Clear();
            }

            // update the page number
            m_CurPage++;
            if (Lpage) Lpage->SetLabel(Form("%d",m_CurPage));

	    if (!m_OutMultiPage && !m_CurFileName.IsNull()) {
              Ssiz_t last_us = m_CurFileName.Last('_') + 1;
              Ssiz_t lastdot = m_CurFileName.Last('.') - last_us;
              m_CurFileName.Replace(last_us,lastdot,Form("%d",m_CurPage));
	    }
          }

          // check dimension of histogram
          Int_t chkdim = hobj->GetDimension();

          // go to next pad 
          int curPad = (++padCount);

          // Will need to display both histograms if doing reference analysis...
          TH1* hobjO = hobj;
          TH1* hobjR = 0;
          if (dirListR) {
            Int_t tempint = -1;
            // try: full name
            TString onamebase = oname;
#define SingleTpcSectorReference false
            if (SingleTpcSectorReference && onamebase.Contains("iTpcSector")) {
              // last parameter is the single sector to use for reference:
              // e.g. TpcSector14 => TpcSector20 if the number is "20"
              onamebase.Replace(onamebase.Index("iTpcSector")+10,2,"17");
            } else if (SingleTpcSectorReference && onamebase.Contains("TpcSector")) {
              // last parameter is the single sector to use for reference:
              // e.g. TpcSector20 => TpcSector14 if the number is "14"
              onamebase.Replace(onamebase.Index("TpcSector")+9,2,"14");
            }
            hobjR = (TH1*) (dirListR->FindObject(onamebase.Data()));
            if (!hobjR) {
              // try: strip just maker from name
              onamebase = StripPrefixes(oname,tempint,-1);
              hobjR = (TH1*) (dirListR->FindObject(onamebase.Data()));
              if (!hobjR) {
                // try: strip just trigger type from name
                onamebase = StripPrefixes(oname,tempint,1);
                hobjR = (TH1*) (dirListR->FindObject(onamebase.Data()));
                if (!hobjR) {
                // try: strip maker and trigger type from name
                  onamebase = StripPrefixes(oname,tempint,0);
                  hobjR = (TH1*) (dirListR->FindObject(onamebase.Data()));
                }
              }
            }
            if (hobjR) {
              TString htitle = StripPrefixes(hobjR->GetTitle(),tempint,0);
              if (!htitle.BeginsWith("Ref:")) htitle.Prepend("Ref:");
              hobjR->SetTitle(htitle.Data());
            }
          }
          TVirtualPad* objPad = 0;
          for (int analRepeat = 0;analRepeat < (hobjR ? 2 : 1); analRepeat++) {

	  padAdvance = kTRUE;
          if (analRepeat) {
            objPad=gPad;
            graphPadR->cd(m_QAShiftMode ? 0 : curPad);
            hobj=hobjR;
          } else graphPad->cd(m_QAShiftMode ? 0 : curPad);

          // set x & y grid off by default
	  gPad->SetGridy(0);
	  gPad->SetGridx(0);

          if (oName.Contains("GtrkPadfT")) hobj->SetMinimum(0.8);
	  
          // set logX,Y,Z scale on/off

// Normalized histograms: rely on negative bin content 0 as a flag (seems OK for now, but not guaranteed)
          Float_t BinCont0 = hobj->GetBinContent(0);
          if (BinCont0 < 0) {
            LOG_INFO << "       -- Will normalize by 1/" << -BinCont0 << ": " << oname << endm;
            hobj->Scale(-1.0/BinCont0);
          }


// Set logY scale on if: there is a loglist, if the hist name is on the list, if it has entries
//    and if the max entries in all bins is > 0
          if (m_ListOfLogY && m_ListOfLogY->FindObject(oname) &&
	     hobj->GetEntries() && hobj->GetMaximum() ) {
	    gPad->SetLogy(1);
            if (!analRepeat) {LOG_INFO << "       -- Will draw in logY scale: " << oname <<endm;}
	  } else gPad->SetLogy(0);


// Set logX scale on if: there is a loglist, if the hist name is on the list, if it has entries
//    and if the max entries in all bins is > 0
	  if (m_ListOfLogX && m_ListOfLogX->FindObject(oname) &&
	     hobj->GetEntries() && hobj->GetMaximum() ) {
	    gPad->SetLogx(1);
            if (!analRepeat) {LOG_INFO << "       -- Will draw in logX scale: " << oname <<endm;}
	  } else gPad->SetLogx(0);


// Set logZ scale
          if (oName.EndsWith("PVsDedx") ||
              oName.Contains("fms_qt_") ||
              oName.Contains("fpd_channel_") ||
              oName.Contains("TofPID") ||
              oName.Contains("RP_cluster_xy") ||
              oName.Contains("TpcSector") ||
              oName.Contains("PointRPTpc") ||
              oName.Contains("PointXYTpc")) {
            gPad->SetLogz(1);
            if (!analRepeat) {LOG_INFO << "       -- Will draw in logZ scale: " << oname <<endm;}
          } else gPad->SetLogz(0);

// Limit x range for some histograms
          if (oName.EndsWith("QaPointTpc") ||
              oName.EndsWith("QaPointSvt") ||
              oName.Contains("QaPointSvtLaser") ||
              oName.EndsWith("QaPointSsd") ||
              oName.EndsWith("QaPointFtpc") ||
              oName.EndsWith("QaRichTot") ||
              oName.EndsWith("QaV0Vtx") ||
              oName.EndsWith("QaXiVtxTot") ||
              oName.Contains("QaPmdTotal") ||
              oName.Contains("QaCpvTotal") ||
              oName.EndsWith("SImpactTime") ||
              oName.EndsWith("trkGoodTTS")) {
            Float_t mean = hobj->GetMean(1);
            Float_t window = hobj->GetRMS(1);
            Float_t bwid = hobj->GetXaxis()->GetBinWidth(1);
            if (window < bwid) window = bwid;
            hobj->SetAxisRange(mean-5*window,mean+5*window,"X");
          }

// Limit both x & y ranges together for some histograms
          if (oName.EndsWith("trkGoodF")) {
            Float_t mean1 = hobj->GetMean(1);
            Float_t mean2 = hobj->GetMean(2);
            Float_t window1 = hobj->GetRMS(1);
            Float_t window2 = hobj->GetRMS(2);
            Float_t bwid = hobj->GetXaxis()->GetBinWidth(1);
            if (window1 < bwid) window1  = bwid;
            if (window2 < bwid) window2  = bwid;
            Float_t lo = TMath::Min(mean1-5*window1,mean2-5*window2);
            Float_t hi = TMath::Max(mean1+5*window1,mean2+5*window2);
            hobj->SetAxisRange(lo,hi,"X");
            hobj->SetAxisRange(lo,hi,"Y");
          }

// Limit both x & y ranges for QaVtxFtpcE/WTpcXY histograms for non-year 7 runs
          if (oName.Contains("VtxFtpc")&&oName.Contains("TpcXY")&&m_RunYear!=7){
            hobj->GetXaxis()->SetRangeUser(-2.0,2.0);
            hobj->GetYaxis()->SetRangeUser(-2.0,2.0);
          }

          // actually draw,print
          if ((chkdim == 3) && (hobj->InheritsFrom("StMultiH2F"))) {
            hobj->Draw("Col");
          } else if ((chkdim == 3) && (oName.Contains("Z3A"))) {
            latex.SetTextAngle(0);
            latex.SetTextAlign(12);
            TH3F* Z3A = (TH3F*) hobj;
            Bool_t noneYet = kTRUE;
            // copied from StdEdxY2Maker::FinishRun() vers. 1.83,
            //   with minor modifications for plotting the results
            Double_t slope = 1.7502e-6;// slope from Blair   1/( O2 in ppm., cm )
            const Char_t *IO[2] = {"Inner", "Outer"};
            Double_t    xmin[2] = { 40, 40};
            Double_t    xmax[2] = {200,180};
            TF1 *gg = StdEdxY2Maker_Gaus2();
            float histmiddle = 0;
            for (Int_t io = 1; io <= 2; io++) {
              Z3A->GetXaxis()->SetRange(io,io);
              TH2 *I = (TH2 *) Z3A->Project3D(Form("zy%i",io));
              if (I) {
                I->FitSlicesY(gg);
                TH1D *proj = (TH1D*) gDirectory->Get(Form("%s_1",I->GetName()));
                if (proj) {
                  proj->Fit("pol1","erq",(noneYet ? "" : "same"),xmin[io-1],xmax[io-1]);
                  proj->SetLineColor(8-io);
                  proj->SetMarkerColor(8-io);
                  proj->SetMarkerStyle(24-io);
                  proj->SetStats(0);
                  proj->SetTitle(otitle);
                  TF1 *f = (TF1 *) proj->GetListOfFunctions()->FindObject("pol1");
                  if (f) {
                    gMessMgr->Info() << "StHistUtil: Estimated content of O2 (ppm) "
                                     << "from slope in drift distance for "
                                     << Form("%s = %10.2f +/- %10.2f", IO[io-1],
                                     -f->GetParameter(1)/slope, f->GetParError(1)/slope)
                                     << endm;
                    if (noneYet) histmiddle = f->Eval(100.0);
                    f->SetLineColor(6-2*io);
                    latex.SetTextColor(6-2*io);
                    latex.DrawLatex(20,histmiddle+0.9-0.6*io,Form("%s : %10.2f +/- %10.2f\n",
                      IO[io-1],-f->GetParameter(1)/slope,f->GetParError(1)/slope));
                  }
                  if (noneYet) {
                    proj->SetMinimum(histmiddle-0.4);
                    proj->SetMaximum(histmiddle+0.4);
                    noneYet = kFALSE;
                  }
                }
              }
            }
            latex.SetTextColor(1);
          } else if ((chkdim==2) && (oName.Contains("PointRPTpc") ||
                      (oName.Contains("PointXYTpc") &&  // Unfortunately was polar for a short time
                       TMath::Abs((hobj->GetYaxis()->GetXmax()/TMath::Pi())-2)<1e-5))) {
            TH2F* htmp = new TH2F(Form("%s.",hobj->GetName()),hobj->GetTitle(),1,-200,200,1,-200,200);
            float hmin = (oName.Contains("PointRPTpcQ") ? 1e-4 : 1.0);
            htmp->Fill(0.,0.,hmin); htmp->SetMinimum(hmin);
            htmp->SetStats(kFALSE);
            htmp->Draw();
            hobj->SetMinimum(0.9*hmin);
            if (gROOT->GetVersionInt() < 52800) {
              hobj->Draw("Pol ZCol Same");
            } else {
              // lego plots always needed phi,r from x,y
              // (z)col plots, however, needed r,phi from x,y
              // Now, (z)col plots also need phi,r from x,y
              // https://sft.its.cern.ch/jira/browse/ROOT-2845
              FlipAxes(hobj)->Draw("Pol ZCol Same");
            }
          } else if ((chkdim == 2) &&
                     (oName.EndsWith("SImpactTime"))) {
            hobj->SetMarkerStyle(7);
            ((TH2F*) hobj)->ProfileX()->Draw();
          } else if ((chkdim == 2) &&
                     (oName.EndsWith("SvtLoc") ||
                      oName.EndsWith("PVsDedx") ||
                      oName.EndsWith("VtxPrXY") ||
                      oName.EndsWith("SSD") ||
                      oName.EndsWith("PointXYSvt") ||
                      oName.Contains("TpcSector") ||
                      oName.Contains("PointXYTpc") ||
                      oName.Contains("SectorvsSensor") ||
                      oName.Contains("LaddervsSensor"))) {
            hobj->Draw("ZCol");
          } else if ((chkdim == 2) && (!hobj->InheritsFrom("StMultiH1F"))) {
            hobj->Draw("Col");
	    if ((oName.EndsWith("trkGoodF"))||(oName.EndsWith("VtxSvtvsTpc"))) {
              ruler.SetLineColor(46);
              ruler.SetLineWidth(2);
              ruler.DrawLineNDC(0.1,0.1,0.9,0.9);
	    }
          } else {
            if (oName.Contains("QaBbc") ||
		(oName.Contains("QaPmd") && !oName.Contains("Total")) ||
                (oName.Contains("QaFpd") && !oName.Contains("Sums"))) {
              hobj->SetBarOffset();
            }
	    hobj->SetLineWidth(2);
            if (oName.EndsWith("Mass")) hobj->Draw("e");
	    else hobj->Draw();
	    if (oName.BeginsWith("fcl_radial") && (hobj->GetEntries() > 0)) {
              //Fits to radial steps FTPCE+W/////05/14/08///nav+gvb
	      hobj->GetXaxis()->SetRangeUser(6.5,9.0);
	      hobj->Fit("pol0","","", 6.5, 7.2);
	      double n1= hobj->GetFunction("pol0")->GetParameter(0);
	      hobj->Fit("pol0","","",8.3,9.0);
	      double n2= hobj->GetFunction("pol0")->GetParameter(0);
	      fitFRS->SetParameters(n1, 7.85, 0.35, n2);
	      hobj->Fit(fitFRS, "R");

              double rstep = fitFRS->GetParameter(1);
              double erstep = fitFRS->GetParError(1);
              float hmin = hobj->GetMinimum();
              float hmax = hobj->GetMaximum();
	      ruler.SetLineColor(kBlack);
	      ruler.SetLineWidth(2);
	      ruler.DrawLine(7.8,hmin,7.8,hmax);
	      ruler.SetLineColor(kGreen);
	      ruler.SetLineWidth(3);
	      ruler.DrawLine(rstep,hmin,rstep,hmax);
	      ruler.SetLineWidth(1);
	      ruler.DrawLine(rstep-erstep,hmin,rstep-erstep,hmax);
	      ruler.DrawLine(rstep+erstep,hmin,rstep+erstep,hmax);
	    }


	  }

          if (oName.Contains("NullPrimVtx")) {
            int msdVtx = (int) (hobj->GetBinContent(hobj->FindBin(-1.)));
            int qstVtx = (int) (hobj->GetBinContent(hobj->FindBin(0.)));
            int goodVtx = (int) (hobj->GetBinContent(hobj->FindBin(1.)));
            int fndVtx = qstVtx + goodVtx;
            int totVtx = fndVtx + msdVtx;
            Float_t txtSiz = latex.GetTextSize();
            latex.SetTextSize(txtSiz*1.5);
            latex.SetTextAngle(90);
            latex.SetTextAlign(3);
            latex.SetTextColor(4);
            latex.DrawLatex(-0.8,0,Form("  missed:  %d",msdVtx));
            latex.DrawLatex(0.2,0,Form("  questionable:  %d",qstVtx));
            latex.DrawLatex(1.2,0,Form("  good:  %d",goodVtx));
            latex.SetTextSize(txtSiz*2);
            latex.SetTextColor(2);
            latex.DrawLatex(-1.8,0,Form("   total:  %d",totVtx));
            latex.SetTextAlign(1);
            latex.SetTextColor(kGreen+3);
            latex.DrawLatex(-1.1,0,Form("   found:  %d",fndVtx));
            // restore
            latex.SetTextColor(1);
            latex.SetTextSize(txtSiz);
            LOG_INFO << (m_CurPrefix ? possiblePrefixes[m_CurPrefix] : "GE")
                     << (analRepeat ? " Ref" : "")
                     << " QA Events (found vtx/total) "
                     << fndVtx << " / " << totVtx << endm;
          }

          if (oName.Contains("iTpcSector")) {
            // Draw RDO boundaries
            ruler.SetLineColor(1);
            ruler.SetLineWidth(1);
            // between RDOs 4-8, draw +/- (npads_row1+npads_row2)/2 * (pitch/2)
            float pitch = 0.67/2.0; // 6.7mm pitch
            ruler.DrawLine(-137*pitch,64.5,137*pitch,64.5);
            ruler.DrawLine(-123*pitch,56.5,123*pitch,56.5);
            ruler.DrawLine(-111*pitch,48.5,111*pitch,48.5);
            ruler.DrawLine( -97*pitch,40.5, 97*pitch,40.5);
            // between RDOs 1-2, outer X pads [X/2 at each end] are in RDO 1
            pitch = 0.50/2.0; // 5.0mm pitch
            int row_width = 70; int in_step1 = 52; int in_step2 = 54;
            float row1=10.5; float row2 = row1+1.0; float row3 = row1+2.0;
            ruler.DrawLine(-(row_width-in_step2)*pitch,row3,(row_width-in_step2)*pitch,row3);
            ruler.DrawLine(-(row_width-in_step2)*pitch,row2,-(row_width-in_step1)*pitch,row2);
            ruler.DrawLine((row_width-in_step2)*pitch,row2,(row_width-in_step1)*pitch,row2);
            ruler.DrawLine(-row_width*pitch,row1,-(row_width-in_step1)*pitch,row1);
            ruler.DrawLine((row_width-in_step1)*pitch,row1,row_width*pitch,row1);
            ruler.DrawLine(-(row_width-in_step1)*pitch,row1,-(row_width-in_step1)*pitch,row2);
            ruler.DrawLine((row_width-in_step1)*pitch,row1,(row_width-in_step1)*pitch,row2);
            ruler.DrawLine(-(row_width-in_step2)*pitch,row3,-(row_width-in_step2)*pitch,row2);
            ruler.DrawLine((row_width-in_step2)*pitch,row3,(row_width-in_step2)*pitch,row2);
            // between RDOs 2-3&4
            bool east = (atoi(&(oName.Data()[oName.Last('r')+1])) > 12);
            pitch = (east ? 0.50 : -0.50); // 5.0mm pitch, switch orientation for east vs. west
            float p01 = 46*pitch; float row01 = 22.5;
            float p02 = 36*pitch; float row02 = 22.5;
            float p03 = 36*pitch; float row03 = 21.5;
            float p04 = 39*pitch; float row04 = 21.5;
            float p05 = 39*pitch; float row05 = 20.5;
            float p06 = 19*pitch; float row06 = 20.5;
            float p07 = 19*pitch; float row07 = 22.5;
            float p08 =  9*pitch; float row08 = 22.5;
            float p09 =  9*pitch; float row09 = 23.5;
            float p10 =  2*pitch; float row10 = 23.5;
            float p11 =  2*pitch; float row11 = 24.5;
            ruler.DrawLine( p01,row01, p02,row02);
            ruler.DrawLine( p02,row02, p03,row03);
            ruler.DrawLine( p03,row03, p04,row04);
            ruler.DrawLine( p04,row04, p05,row05);
            ruler.DrawLine( p05,row05, p06,row06);
            ruler.DrawLine( p06,row06, p07,row07);
            ruler.DrawLine( p07,row07, p08,row08);
            ruler.DrawLine( p08,row08, p09,row09);
            ruler.DrawLine( p09,row09, p10,row10);
            ruler.DrawLine( p10,row10, p11,row11);
            ruler.DrawLine(-p01,row01,-p02,row02);
            ruler.DrawLine(-p02,row02,-p03,row03);
            ruler.DrawLine(-p03,row03,-p04,row04);
            ruler.DrawLine(-p04,row04,-p05,row05);
            ruler.DrawLine(-p05,row05,-p06,row06);
            ruler.DrawLine(-p06,row06,-p07,row07);
            ruler.DrawLine(-p07,row07,-p08,row08);
            ruler.DrawLine(-p08,row08,-p09,row09);
            ruler.DrawLine(-p09,row09,-p10,row10);
            ruler.DrawLine(-p10,row10,-p11,row11);
            ruler.DrawLine(-p11,row11, p11,row11);
            // between RDOs 3-4
            p01 =   0*pitch; row01 = 24.5;
            p02 =   0*pitch; row02 = 26.5;
            p03 =  -7*pitch; row03 = 26.5;
            p04 =  -7*pitch; row04 = 28.5;
            p05 =  -8*pitch; row05 = 28.5;
            p06 =  -8*pitch; row06 = 30.5;
            p07 =  10*pitch; row07 = 30.5;
            p08 =  10*pitch; row08 = 32.5;
            p09 =  11*pitch; row09 = 32.5;
            p10 =  11*pitch; row10 = 35.5;
            p11 =  10*pitch; row11 = 35.5;
            float p12 =  10*pitch; float row12 = 36.5;
            float p13 =   0*pitch; float row13 = 36.5;
            float p14 =   0*pitch; float row14 = 40.5;
            ruler.DrawLine(p01,row01,p02,row02);
            ruler.DrawLine(p02,row02,p03,row03);
            ruler.DrawLine(p03,row03,p04,row04);
            ruler.DrawLine(p04,row04,p05,row05);
            ruler.DrawLine(p05,row05,p06,row06);
            ruler.DrawLine(p06,row06,p07,row07);
            ruler.DrawLine(p07,row07,p08,row08);
            ruler.DrawLine(p08,row08,p09,row09);
            ruler.DrawLine(p09,row09,p10,row10);
            ruler.DrawLine(p10,row10,p11,row11);
            ruler.DrawLine(p11,row11,p12,row12);
            ruler.DrawLine(p12,row12,p13,row13);
            ruler.DrawLine(p13,row13,p14,row14);
            latex.SetTextAngle(0);
            latex.SetTextAlign(32);
            latex.DrawLatex(50,5,"RDO 1");
            latex.DrawLatex(50,18,"2");
            latex.DrawLatex(50,33,(east ? "4 , 3" : "3 , 4"));
            latex.DrawLatex(50,44,"5");
            latex.DrawLatex(50,52,"6");
            latex.DrawLatex(50,60,"7");
            latex.DrawLatex(50,68,"8");

            // Draw Anode guides
            ruler.SetLineColor(2);
            ruler.DrawLine(-52,10.5,-47,10.5);
            ruler.DrawLine(-52,20.5,-47,20.5);
            ruler.DrawLine(-52,30.5,-47,30.5);
            ruler.DrawLine(-52,40.5,-47,40.5);
            ruler.DrawLine(-52,48.1,-47,48.1);
            ruler.DrawLine(-52,56.1,-47,56.1);
            ruler.DrawLine(-52,64.1,-47,64.1);
            latex.SetTextAlign(12);
            latex.SetTextColor(2);
            latex.DrawLatex(-50, 5.1,"1 Anode");
            latex.DrawLatex(-50,15.1,"2");
            latex.DrawLatex(-50,25.1,"3");
            latex.DrawLatex(-50,35.1,"4");
            latex.DrawLatex(-50,44.1,"5");
            latex.DrawLatex(-50,52.1,"6");
            latex.DrawLatex(-50,60.1,"7");
            latex.DrawLatex(-50,68.1,"8");
            latex.SetTextColor(1);
          } else if (oName.Contains("TpcSector")) {
            // Draw RDO boundaries
            ruler.SetLineColor(1);
            ruler.SetLineWidth(1);
            // between RDOs 2-6, draw +/- (npads_row1+npads_row2)/2 * (pitch/2)
            float pitch = 0.67/2.0; // 6.7mm pitch
            ruler.DrawLine(-137*pitch,37.5,137*pitch,37.5);
            ruler.DrawLine(-123*pitch,29.5,123*pitch,29.5);
            ruler.DrawLine(-111*pitch,21.5,111*pitch,21.5);
            ruler.DrawLine( -97*pitch,13.5, 97*pitch,13.5);
            // between RDOs 1-2, outer 24(68) pads [12(34) at each end] are in RDO 1
            pitch = 0.335/2.0; // 3.35mm pitch
            int row_width = 134; int in_step = 68; float row1=6.5;
            if (m_RunYear < 17) { row_width = 142; in_step = 24; row1=7.5; }
            float row2 = row1+1.0;
            ruler.DrawLine(-(row_width-in_step)*pitch,row2,(row_width-in_step)*pitch,row2);
            ruler.DrawLine(-row_width*pitch,row1,-(row_width-in_step)*pitch,row1);
            ruler.DrawLine((row_width-in_step)*pitch,row1,row_width*pitch,row1);
            ruler.DrawLine(-(row_width-in_step)*pitch,row1,-(row_width-in_step)*pitch,row2);
            ruler.DrawLine((row_width-in_step)*pitch,row1,(row_width-in_step)*pitch,row2);
            latex.SetTextAngle(0);
            latex.SetTextAlign(32);
            latex.DrawLatex(50,4,"RDO 1");
            latex.DrawLatex(50,10,"2");
            latex.DrawLatex(50,17,"3");
            latex.DrawLatex(50,25,"4");
            latex.DrawLatex(50,33,"5");
            latex.DrawLatex(50,41,"6");

            // Draw Anode guides
            ruler.SetLineColor(2);
            ruler.DrawLine(-52,2.9,-47,2.9);
            ruler.DrawLine(-52,6.2,-47,6.2);
            ruler.DrawLine(-52,9.4,-47,9.4);
            ruler.DrawLine(-52,13.5,-47,13.5);
            ruler.DrawLine(-52,21.1,-47,21.1);
            ruler.DrawLine(-52,29.1,-47,29.1);
            ruler.DrawLine(-52,37.1,-47,37.1);
            latex.SetTextAlign(12);
            latex.SetTextColor(2);
            latex.DrawLatex(-50,1.5,"1 Anode");
            latex.DrawLatex(-50,4.6,"2");
            latex.DrawLatex(-50,7.8,"3");
            latex.DrawLatex(-50,11.4,"4");
            latex.DrawLatex(-50,17.1,"5");
            latex.DrawLatex(-50,25.1,"6");
            latex.DrawLatex(-50,33.1,"7");
            latex.DrawLatex(-50,41.1,"8");
            latex.SetTextColor(1);
          }

          if (oName.Contains("PointRPTpc") ||
              oName.Contains("PointXYTpc") ) {
            // Draw sector boundaries and label
            int eastsec = (oName.Contains("TpcE") || oName.Contains("TpcQE") ? 12 : 0);
            ruler.SetLineColor(0);
            ruler.SetLineStyle(2);
            float sz = latex.GetTextSize();
            latex.SetTextAlign(32);
            latex.SetTextSize(0.032);
            int secn = (eastsec ? 21 : 13);
            float phistep = TMath::Pi()/12;
            for (float phi=phistep; phi<6.2; phi+=phistep) {
              float xsec = TMath::Cos(phi);
              float ysec = TMath::Sin(phi);
              ruler.DrawLine(50*xsec,50*ysec,199*xsec,199*ysec);
              phi+=phistep;
              xsec = TMath::Cos(phi);
              ysec = TMath::Sin(phi);
              latex.SetTextAngle(phi*180/TMath::Pi()+1); // +1 degree necessary to avoid PDF bug
              latex.DrawLatex(52*xsec,52*ysec,Form("%d",secn%12 + eastsec + 1));
              secn += (eastsec ? 1 : -1);
            }
            latex.SetTextSize(sz);
            ruler.SetLineStyle(1);
          }

          if (oName.EndsWith("QaPointPhiT")) {
            float hmin = hobj->GetMinimum() ;
            float hmax = hobj->GetMaximum() ;
            float hmid = hmin+0.12*(hmax-hmin);
            float sz = latex.GetTextSize();
            latex.SetTextSize(0.045);
            latex.SetTextColor(2);
            latex.SetTextAngle(90);
            latex.SetTextAlign(22);
            for (int secn = 1; secn < 13; secn++) {
              float phisec = 87 - secn*30;
              while (phisec < 1) phisec += 360;
              latex.DrawLatex(phisec,hmid,Form("%d  /  %d",secn,(23-secn)%12 + 13));
            }
            latex.SetTextSize(sz);
            latex.SetTextColor(1);
            latex.SetTextAngle(0);
          }

          if ((chkdim == 2) && (oName.Contains("TPC_charge"))) {
            TH2F* hobj_TPC_charge = (TH2F*) hobj;

            TProfile *prof_hobjTPC = (TProfile*) hobj_TPC_charge->ProfileX();
            prof_hobjTPC->SetMarkerStyle(20);
            prof_hobjTPC->SetMarkerSize(0.6);
            prof_hobjTPC->SetMarkerColor(6);
            prof_hobjTPC->SetLineColor(6);
            
            //landau fit
            static TF1 *flandau = 0;
            if (!flandau) flandau = new TF1("flandau", "[0]*TMath::Landau(x,[1],[2])",0,200);
            flandau->SetParameters(2e-3*(hobj->GetEntries()), 125, 30);
            hobj_TPC_charge->FitSlicesY(flandau);
            hobj->Draw("colz");
            TH1D *mean_landau = (TH1D*) (gDirectory->Get(Form("%s_1",hobj_TPC_charge->GetName())));
            if (mean_landau) {
              mean_landau->SetMarkerStyle(20);
              mean_landau->SetMarkerSize(0.6);
              mean_landau->SetMarkerColor(1);
              mean_landau->Draw("same p");
            } else {
              LOG_WARN << "TPC_charge Landau FitSlicesY failed for " << hobj->GetName() << endm;
            }
            
            //rms landau
            //TH1D *mean_landau2 = (TH1D*) (gDirectory->Get(Form("%s_2",hobj_TPC_charge_lan->GetName())));
            //if (mean_landau2) {
            //  mean_landau2->SetMarkerStyle(20);
            //  mean_landau2->SetMarkerSize(0.6);
            //  mean_landau2->SetMarkerColor(2);
            //  mean_landau2->Draw("same p");
            //}
            
            prof_hobjTPC->Draw("same p");
            
            //draw a legend
            TLegend *ilegend = new TLegend(0.15,0.80,0.38,0.90);
            ilegend->SetFillColor(0);
            ilegend->SetHeader("Legend");
            ilegend->SetMargin(0.25);
            ilegend->AddEntry(prof_hobjTPC,"Profile mean","pz");
            ilegend->AddEntry(mean_landau,"Landau MPV","pz");
            ilegend->Draw();
          }

          if (padAdvance) {if (gPad) gPad->Update();}
          else {if (!analRepeat) padCount--;}

          } // analRepeat for loop

          // Run reference analysis...(on first loop, before we forgot hobj)
          if (hobjR) {
            StHistUtilRef* huR = 0;
            if (m_refCuts) {
              // try: full name
              huR = (StHistUtilRef*) (m_refCuts->FindObject(oname));
              if (!huR) {
                // try: strip just maker from name
                Int_t tempint = -1;
                TString onamebase = StripPrefixes(oname,tempint,-1);
                huR = (StHistUtilRef*) (m_refCuts->FindObject(onamebase.Data()));
                if (!huR) {
                  // try: strip just trigger type from name
                  onamebase = StripPrefixes(oname,tempint,1);
                  huR = (StHistUtilRef*) (m_refCuts->FindObject(onamebase.Data()));
                  if (!huR) {
                    // try: strip maker and trigger type from name
                    onamebase = StripPrefixes(oname,tempint,0);
                    huR = (StHistUtilRef*) (m_refCuts->FindObject(onamebase.Data()));
                  }
                }
              }
            }
            double result = 0;

            // default to Kolm. max distance, no cut
            int mode = ( huR ? huR->Mode : 2 );
            double cut = ( huR ? huR->Cut : 0);

            // modes:
            switch (mode) {
              case (0) : // Chi2
                result = hobjO->Chi2Test(hobjR,(huR ? huR->Options() : "WW"));
                break;
              case (1) : // Kolmogorov probability
                // Note that Sumw2() seems to affect the way histograms are drawn,
                // but Kolmogorov test complains in current ROOT version (won't in
                // future versions). Don't know what to do for now.
                //if (hobjO->GetSumw2N() == 0) hobjO->Sumw2();
                //if (hobjR->GetSumw2N() == 0) hobjR->Sumw2();
                result = hobjO->KolmogorovTest(hobjR,(huR ? huR->Options() : ""));
                break;
              case (2) : // Kolmogorov maximum distance
                // Seems to work for both 1D and 2D
                TString analOpts = (huR ? huR->Options() : "");
                analOpts.ToUpper();
                if (! analOpts.Contains('M')) analOpts += 'M';
                result = 1.0 - hobjO->KolmogorovTest(hobjR,analOpts.Data());
                break;
            }
            bool analPass = (result >= cut);

            TString score = Form("Score: %4.2f (%s vs. %4.2f)",result,
                        (analPass ? "PASS" : "FAIL"),cut);

            if (m_PrintMode & QAU1<<QAprintIndivRef)
              gPad->Print(Form("Ref_%s%s",oName.Data(),m_OutIndividuals.Data()));
            if (m_QAShiftMode)
              gPad->Print(Form("Ref_%s.png",oName.Data()));

            if (objPad) {
              objPad->cd();
              float sz = latex.GetTextSize();
              latex.SetTextSize(0.038);
              latex.SetTextAngle(90);
              latex.SetTextAlign(11);
              latex.SetTextColor(analPass ? kGreen+4 : kRed+2);
              latex.DrawTextNDC(0.999,0.001,score.Data());
              objPad->Update();
              latex.SetTextColor(1);
              latex.SetTextSize(sz);
              if (m_PrintMode & QAU1<<QAprintIndiv)
                gPad->Print(Form("%s%s",oName.Data(),m_OutIndividuals.Data()));
              if (m_QAShiftMode)
                gPad->Print(Form("%s.png",oName.Data()));
            }


            LOG_INFO << Form("  -   %d. Reference Test (mode=%d) %s",
                        histReadCounter,mode,score.Data()) << endm;
            if (strlen(m_refResultsFile)) {
              if (!R_ostr) R_ostr = new ofstream(m_refResultsFile);
              (*R_ostr) << m_CurPage << " " << curPad << " " << oName << " " << result << endl;
            }
          } else {
            if (m_PrintMode & QAU1<<QAprintIndiv)
              gPad->Print(Form("%s%s",oName.Data(),m_OutIndividuals.Data()));
            if (m_QAShiftMode)
              gPad->Print(Form("%s.png",oName.Data()));
            if (strlen(m_refResultsFile)) {
              if (!R_ostr) R_ostr = new ofstream(m_refResultsFile);
              (*R_ostr) << m_CurPage << " " << curPad << " " << oName << " 1.0" << endl;
            }
          }

        }
      }

//NOTE! (13jan00,kt) 
//--> obj->Draw just appends the histogram to the list
//    --> you must update the current pad (gPad) or the whole big pad (graphPad)
//        to actually see the stupid thing

// just ended  actual loop over histograms !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  }

  if (C_ostr) delete C_ostr;
  if (R_ostr) delete R_ostr;
  if (root_ofile) delete root_ofile;

  CloseOutFile();
  return histCounter;
}
 
//_____________________________________________________________________________


TList* StHistUtil::FindHists(const Char_t *dirName, const Char_t *withPrefix) 
{  

// NOTE - must have already used method SetPntrToMaker to get the
//       pointer m_PntrToMaker  to an StMaker class!
//

//  Method 1 ------------------------------------------------
// Method FindHists -->
// Find pointer to histograms under a Maker

  TList *dList=0;

  LOG_INFO << " Beg: FindHists, dList pointer = " << dList << endm;

//---- First look under Maker for histograms ==>
//They  should show up in your Maker's directory, so search for them there,
//     i.e. MakerName/.hist is where they'd be
// Note: Histograms is a method of StMaker
//---- If you have a chain, you'll always have the .hist directory, so
//     have to check if there's really anything there (so use First method)

//
  PathCopy(m_dirName,dirName);
  StMaker *temp = m_PntrToMaker->GetMaker(m_dirName);

  if (temp) {
    LOG_INFO << "FindHists - found pointer to maker" << endm;
    dList = temp->Histograms();
  }

// Now check to see if any histograms exist here (look for something in
//  the list (test)
  TObject *test=0;
  if (dList) test = dList->First();
  if (test){ 
      LOG_INFO << " FindHists - found hist. in Maker-Branch " << endm;
     }

    LOG_INFO << " Mid: FindHists, dList pointer = " << dList << endm;
    LOG_INFO << " Mid: FindHists, test pointer =  " << test << endm;

// If you have the pointer but the hist. really aren't here, set
//  the pointer back to zero
  if (!test) dList = 0;

  LOG_INFO << " Mid2: FindHists, dList pointer = " << dList << endm;
  LOG_INFO << " Mid2: FindHists, test pointer =  " << test << endm;


  if (!dList) {

// Method 2 -----------------------------------------------------

//-------------- Now try and see if they're in histBranch from output of bfc


  St_DataSet *hist=0;
  hist = m_PntrToMaker->GetDataSet("hist");
  if (hist) {
//    hist->ls(9);

// must look in dirNameHist 
// use TString to append "Hist" to the dirName
// += is overloaded operator of TString

    TString hBN(m_dirName);
    hBN += "Hist";
    
//find particular branch
    St_DataSet *QAH = 0;
    QAH = hist->Find(hBN.Data());
// or can create iterator and look over all branches

//now get the list of histograms
    if (QAH)  {
      dList = (TList *)QAH->GetObject();
    }

  }

// now have we found them?
  if (dList){ 
      LOG_INFO << " FindHists - found hist. in histBranch, with name:  " 
	   << m_dirName <<  endm;
     }
  else { 
         LOG_INFO << " FindHists - histogram branch has not been found for branch --> "
	   << m_dirName <<  endm;
     }

  }

  if (dList && (withPrefix || m_ListOfPrint)) dList = TrimListByPrefix(dList,withPrefix);

  LOG_INFO << " FindHists, dList pointer = " << dList << endm;
  
 
 return dList;
}
//_____________________________________________________________________________
 
TList* StHistUtil::FindHists(TFile* histFile, const Char_t* withPrefix) {
  if (!histFile) return 0;
  TList* dList = histFile->GetList();
  if (dList->GetSize() == 0) {
    histFile->ReadAll();
    dList = histFile->GetList();
  }

  TObject* test = (dList ? dList->First() : 0);
  LOG_INFO << " Mid5: FindHists, dList pointer = " << dList << endm;
  LOG_INFO << " Mid5: FindHists, test pointer =  " << test << endm;
  if (!test) dList = 0;

  if (dList && (withPrefix || m_ListOfPrint)) dList = TrimListByPrefix(dList,withPrefix);
  return dList;
}
//_____________________________________________________________________________
 
TList* StHistUtil::TrimListByPrefix(TList* dList, const Char_t* withPrefix) {
  TList* dList2 = new TList;

  //Now want to loop over all histograms
  // Create an iterator
  TIter nextObj(dList);
  TObject *obj = 0;
  int withPrefixNumber = -1;
  int prefixNumber = -1;
  if (withPrefix) StripPrefixes(withPrefix,withPrefixNumber);
  while ((obj = nextObj())) {
    Bool_t addIt = kTRUE;
    if (withPrefix) {
      StripPrefixes(obj->GetName(),prefixNumber);
      if (prefixNumber != withPrefixNumber) addIt = kFALSE;
    }
    if (addIt && ((!m_ListOfPrint) ||
                  (obj->InheritsFrom("TH1") &&
                   m_ListOfPrint->FindObject(obj->GetName())))) dList2->AddLast(obj);
  }
  return dList2;
}
//_____________________________________________________________________________


Int_t StHistUtil::ListHists(const Char_t *dirName) 
{  
// Method ListHists -->
// List of all histograms

  if (Debug()) {
    LOG_INFO << " **** Now in StHistUtil::ListHists **** " << endm;
  }

// get the TList pointer to the histograms:
  TList* dirList = (m_PntrToMaker ? FindHists(dirName) : FindHists(m_PntrToPlainFile));

  if (!dirList) { LOG_INFO << " ListHists - histograms not available! " << endm; }

//Now want to loop over all histograms
// Create an iterator
  TIter nextObj(dirList);
  Int_t histReadCount = 0;
  TObject *obj = 0;

// use = here instead of ==, because we are setting obj equal to nextObj and then seeing if it's T or F
  while ((obj = nextObj())) {

// now check if obj is a histogram
    if (obj->InheritsFrom("TH1")) {
 
      histReadCount++;
//  \n means newline, \" means print a quote
//      LOG_INFO << Form(" %d. Have histogram Type %s, Name %s with Title=\"%s\"\n",histReadCount,obj->ClassName(),obj->GetName(),obj->GetTitle()) << endm;
            LOG_INFO << " ListHists: Hist No. " << histReadCount << ", Type: " << obj->ClassName() 
           << ", Name: " << obj->GetName() << ", Title \"" << obj->GetTitle() << "\"  "<< endm; 
    }
  }

  LOG_INFO << " ListHists: Total No. Histograms Booked  = " << histReadCount <<endm;
  return histReadCount;
}


//_____________________________________________________________________________

Int_t StHistUtil::PrintInfoHists(TList *dirList,  const Char_t *fname )
{  

  LOG_INFO << " **** Now in StHistUtil::PrintInfoHists **** " << endm;
  LOG_INFO << " output file = " << fname << endm;

  ofstream fout(fname);

  if (!dirList) { LOG_INFO << " PrintInfoHists - histograms not available! " << endm; }

  Int_t histInfoCount = 0;

  if (dirList){

//Now want to loop over all histograms
// Create an iterator
    TIter nextObj(dirList);
    TObject *obj = 0;

    LOG_INFO << " Hist #, Name, #Entries, Mean, RMS " << endm;
    fout << " Hist #, Name, #Entries, Mean, RMS " << endl;

// use = instead of ==, because we are setting obj equal to nextObj and then seeing if it's T or F

    while ((obj = nextObj())) {

// now check if obj is a histogram
      if (obj->InheritsFrom("TH1")) {
 
        histInfoCount++;

        LOG_INFO << 
              histInfoCount << " " <<
              obj->GetName() << " " <<
              ((TH1 *)obj)->GetEntries() << " " <<
              ((TH1 *)obj)->GetMean() << " " <<
              ((TH1 *)obj)->GetRMS() << " " <<
              endm;

        fout << 
              histInfoCount << " " <<
              obj->GetName() << " " <<
              ((TH1 *)obj)->GetEntries() << " " <<
              ((TH1 *)obj)->GetMean() << " " <<
              ((TH1 *)obj)->GetRMS() << " " <<
              endl;

      }
    }
  } // if dirList

  LOG_INFO << " PrintInfoHists: # hist read  = " << histInfoCount <<endm;

  return histInfoCount;
}


//_____________________________________________________________________________


Int_t StHistUtil::CopyHists(TList *dirList)
{  
  if (Debug()) {
    LOG_INFO << " **** Now in StHistUtil::CopyHists **** " << endm;
  }

  if (!dirList) { LOG_INFO << " StHistUtil::CopyHists - histogram Pointer not set! " << endm; }

// create array of pointers to the new histograms I will create

  Int_t tempint,ijk=0;
  Int_t histCopyCount = 0;

  if (dirList){
   TIter nextObj(dirList);
   TObject *obj = 0;
    while ((obj = nextObj())) {    
     if (obj->InheritsFrom("TH1") &&
         (!m_ListOfPrint || (m_ListOfPrint->FindObject(obj->GetName())))) {
       histCopyCount++;         
       if (ijk >= maxHistCopy - 1){
         Int_t newMaxHistCopy = maxHistCopy * 4;
         TH1** temp1 = new TH1ptr[newMaxHistCopy];
         memset(temp1,0,newMaxHistCopy*sizeOfTH1Ptr);
         memcpy(temp1,newHist,maxHistCopy*sizeOfTH1Ptr);
         delete [] newHist;
         newHist = temp1;
         maxHistCopy = newMaxHistCopy;
       } // if ijk
       newHist[ijk] = ((TH1 *)obj->Clone());
       if (ignorePrefixes) {
         newHist[ijk]->SetName (StripPrefixes(newHist[ijk]->GetName (),tempint).Data());
         newHist[ijk]->SetTitle(StripPrefixes(newHist[ijk]->GetTitle(),tempint).Data());
       }
       ijk++;
     }   // if obj
    }    // while obj
  }      // if dirList

  LOG_INFO << " ListHists: Total No. Histograms Copied  = " << 
        histCopyCount <<endm;

// Now see if we can find these copies:
 // Int_t imk = 0;
 //for (imk=0;imk<histCopyCount;imk++) {
 //  if (newHist[imk]->InheritsFrom("TH1")) {       
 //        LOG_INFO << " !!! NEW Type: " << newHist[imk]->ClassName() << 
 //             ", Name: "    << newHist[imk]->GetName() << 
 //             ", Title: "   << newHist[imk]->GetTitle() << 
 //	    ", Max: " << ((TH1 *)newHist[imk])->GetMaximum() << endm; 
 //  }
 //} 

  return histCopyCount;
}

//_____________________________________________________________________________

// **** IMPORTANT! ***** 
//  THIS METHOD ASSUMES YOU HAVE ALREADY USED CopyHists TO PUT
//  HISTOGRAMS FROM 1 FILE INTO newHist array
//   -- this method is used in subsequent files!

Int_t StHistUtil::AddHists(TList *dirList,Int_t numHistCopy)
{  
  if (Debug()) {
    LOG_INFO << " **** Now in StHistUtil::AddHists **** " << endm;
  }
  //  LOG_INFO << " num hists to copy = " << numHistCopy << endm;

  if (!dirList) { LOG_INFO << 
        " StHistUtil::AddHists - histogram Pointer not set! " << endm; }

  Int_t histAddCount = 0;
  Int_t tempInt=0;

  if (dirList){
    if (numHistCopy < 0) numHistCopy = dirList->GetSize();
    TIter nextObj(dirList);
    TObject *obj = 0;

    while ((obj = nextObj())) {
      if (obj->InheritsFrom("TH1")) {
        TString oName = obj->GetName();
        if (ignorePrefixes) oName = StripPrefixes(oName.Data(),tempInt);
// now want to add these histograms to the copied ones:
	Int_t tempint,imk = 0;
        Bool_t notfound = true;
	for (imk=0;imk<maxHistCopy;imk++) {
          if (newHist[imk]) {		
             TString nName = newHist[imk]->GetName();
             if (ignorePrefixes) nName = StripPrefixes(nName.Data(),tempInt);
             if (! (nName.CompareTo(oName))) {
	       //LOG_INFO << "  ---- hist num to add --- " << imk << endm;
	       newHist[imk]->Add((TH1 *)obj);
	       histAddCount++;
               notfound = false;
	       //LOG_INFO << " !!! Added histograms with Name: " << newHist[imk]->GetName() <<  endm;
	     } // strcmp
          } else break; // ran out of existing hists
	}  // loop over imk

        if (notfound) {
          // Hist doesn't exist yet, so let's add it
          if (imk >= maxHistCopy-1){
            Int_t newMaxHistCopy = maxHistCopy * 4;
            TH1** temp1 = new TH1ptr[newMaxHistCopy];
            memset(temp1,0,newMaxHistCopy*sizeOfTH1Ptr);
            memcpy(temp1,newHist,maxHistCopy*sizeOfTH1Ptr);
            delete [] newHist;
            newHist = temp1;
            maxHistCopy = newMaxHistCopy;
          } // if imk
          newHist[imk] = ((TH1 *)obj->Clone());
          if (ignorePrefixes) {
            newHist[imk]->SetName (StripPrefixes(newHist[imk]->GetName (),tempint).Data());
            newHist[imk]->SetTitle(StripPrefixes(newHist[imk]->GetTitle(),tempint).Data());
          }
          numHistCopy++;
	  histAddCount++;

        }   // notfound
      }   // if obj inherits from th1
    }   //while
  } //dirlist

  LOG_INFO << " StHistUtil::AddHists: Total No. Histograms Added  = " << 
        histAddCount <<endm;


  return histAddCount;
}

//_____________________________________________________________________________


Int_t StHistUtil::ExamineLogYList() 
{  
// Method ExamineLogYList
// List of all histograms that will be drawn with logy scale

  if (Debug()) {
    LOG_INFO << " **** Now in StHistUtil::ExamineLogYList **** " << endm;
  }

// m_ListOfLogY -  is a list of log plots
// construct a TObject
  TObject *obj = 0;
// construct a TIter ==>  () is an overloaded operator in TIter
  TIter nextObj(m_ListOfLogY);
  Int_t LogYCount = 0;

// use = here instead of ==, because we are setting obj equal to nextObj and then seeing if it's T or F
  while ((obj = nextObj())) {

    if (Debug()) { LOG_INFO << " StHistUtil::ExamineLogYList has hist " <<  obj->GetName() << endm; }
    LogYCount++;

  }

  LOG_INFO << " Now in StHistUtil::ExamineLogYList, No. Hist. in LogY scale = " << LogYCount <<endm;
  return LogYCount;
}

//_____________________________________________________________________________


Int_t StHistUtil::ExamineLogXList() 
{  
// Method ExamineLogXList
// List of all histograms that will be drawn with logX scale

  if (Debug()) {
    LOG_INFO << " **** Now in StHistUtil::ExamineLogXList **** " << endm;
  }

// m_ListOfLogX -  is a list of log plots
// construct a TObject
  TObject *obj = 0;
// construct a TIter ==>  () is an overloaded operator in TIter
  TIter nextObj(m_ListOfLogX);
  Int_t LogXCount = 0;

// use = here instead of ==, because we are setting obj equal to nextObj and then seeing if it's T or F
  while ((obj = nextObj())) {

    if (Debug()) {
      LOG_INFO << " StHistUtil::ExamineLogXList has hist " <<  obj->GetName() << endm;
    }
    LogXCount++;

  }

  LOG_INFO << " Now in StHistUtil::ExamineLogXList, No. Hist. in LogX scale = " << LogXCount <<endm;
  return LogXCount;
}

//_____________________________________________________________________________


Int_t StHistUtil::ExaminePrintList() 
{  
// Method ExaminePrintList
// List of all histograms that will be drawn,printed

  if (Debug()) {
    LOG_INFO << " **** Now in StHistUtil::ExaminePrintList **** " << endm;
  }

// m_ListOfPrint -  is a list of hist to print,draw

// check if there is a list
  if (!m_ListOfPrint){
    LOG_INFO << "      no subset print list was setup - all hist in directory will be printed " << endm;
    //    return PrintCount;
    return 0;
  }

// construct a TObject
  TObject *obj = 0;

// construct a TIter ==>  () is an overloaded operator in TIter
  TIter nextObj(m_ListOfPrint);
  Int_t PrintCount = 0;

// use = here instead of ==, because we are setting obj equal to nextObj and then seeing if it's T or F
  while ((obj = nextObj())) {

    if (Debug()) {
      LOG_INFO << " StHistUtil::ExaminePrintList has hist " <<  obj->GetName() << endm;
    }
    PrintCount++;

  }

  LOG_INFO << " Now in StHistUtil::ExaminePrintList, No. Hist. to Print,Draw = " << PrintCount <<endm;
  return m_ListOfPrint->GetSize();
}

//_____________________________________________________________________________


Int_t StHistUtil::AddToLogYList(const Char_t *HistName){  
// Method AddToLogYList
//   making list of all histograms that we want drawn with LogY scale

   if (Debug()) {
     LOG_INFO << " **** Now in StHistUtil::AddToLogYList  **** " << endm;
   }

// Since I'm creating a new list, must delete it in the destructor!!
//make a new TList on heap(persistant); have already defined m_ListOfLogY in header file
   if (!m_ListOfLogY) m_ListOfLogY = new TList;

// the add method for TList requires a TObject input  (also can use TObjString)
// create TObjString on heap
   TObjString *HistNameObj = new TObjString(HistName);

// - check if it's already on the list - use FindObject method of TList
    TObject *lobj = 0;
    lobj = m_ListOfLogY->FindObject(HistName);
// now can use Add method of TList
    if (!lobj) {
       m_ListOfLogY->Add(HistNameObj);
       if (Debug()) {
         LOG_INFO << " StHistUtil::AddToLogYList: " << HistName  <<endm;
       }
    } else {
      LOG_INFO << " StHistUtil::AddToLogYList: " << HistName << " already in list - not added" <<endm;
    }
 
// return using a method of TList (inherits GetSize from TCollection)
  return m_ListOfLogY->GetSize();
}


//_____________________________________________________________________________


Int_t StHistUtil::AddToLogXList(const Char_t *HistName){  
// Method AddToLogXList
//   making list of all histograms that we want drawn with LogX scale

   if (Debug()) {
     LOG_INFO << " **** Now in StHistUtil::AddToLogXList  **** " << endm;
   }

// Since I'm creating a new list, must delete it in the destructor!!
//make a new TList on heap(persistant); have already defined m_ListOfLogX in header file
   if (!m_ListOfLogX) m_ListOfLogX = new TList;

// the add method for TList requires a TObject input  (also can use TObjString)
// create TObjString on heap
   TObjString *HistNameObj = new TObjString(HistName);

// - check if it's already on the list - use FindObject method of TList
    TObject *lobj = 0;
    lobj = m_ListOfLogX->FindObject(HistName);
// now can use Add method of TList
    if (!lobj) {
       m_ListOfLogX->Add(HistNameObj);
       if (Debug()) {
         LOG_INFO << " StHistUtil::AddToLogXList: " << HistName  <<endm;
       }
    } else {
      LOG_INFO << " StHistUtil::AddToLogXList: " << HistName << " already in list - not added" <<endm;
    }
 
// return using a method of TList (inherits GetSize from TCollection)
 return m_ListOfLogX->GetSize();
}


//_____________________________________________________________________________


Int_t StHistUtil::AddToPrintList(const Char_t *HistName){  

// Method AddToPrintList
//   making list of all histograms that we want drawn,printed

  if (Debug()) {
    LOG_INFO << " **** Now in StHistUtil::AddToPrintList  **** " << endm;
  }

// Since I'm creating a new list, must delete it in the destructor!!
//make a new TList on heap(persistant); have already defined m_ListOfPrint in header file
   if (!m_ListOfPrint) m_ListOfPrint = new TList;

// the add method for TList requires a TObject input  (also can use TObjString)
// create TObjString on heap
   TObjString *HistNameObj = new TObjString(HistName);

// now can use Add method of TList
    if (!m_ListOfPrint->Contains(HistName)) {
       m_ListOfPrint->Add(HistNameObj);
       if (Debug()) {
         LOG_INFO << " StHistUtil::AddToPrintList: " << HistName  <<endm;
       }
    } else {
      LOG_INFO << " StHistUtil::AddToPrintList: " << HistName << " already in list - not added" <<endm;
    }
 
// return using a method of TList (inherits GetSize from TCollection)
 return m_ListOfPrint->GetSize();

}

//_____________________________________________________________________________

Int_t StHistUtil::RemoveFromLogYList(const Char_t *HistName){  
// Method RemoveFromLogYList
//   remove hist from  list  that we want drawn with LogY scale

  if (Debug()) {
    LOG_INFO << " **** Now in StHistUtil::RemoveFromLogYList  **** " << endm;
  }

// check if list exists:
  if (m_ListOfLogY) {
    
// the remove method for TList requires a TObject input  
// - check if it's  on the list - use FindObject method of TList
    TObject *lobj = 0;
    lobj = m_ListOfLogY->FindObject(HistName);
// now can use Remove method of TList
    if (lobj) {
      m_ListOfLogY->Remove(lobj);
      if (Debug()) {
        LOG_INFO << " RemoveLogYList: " << HistName << " has been removed from list" <<endm;
      }
    } else {
      LOG_INFO << " RemoveLogYList: " << HistName << " not on list - not removing" <<endm;
    }

    // return using a method of TList (inherits GetSize from TCollection)
    return m_ListOfLogY->GetSize();
  }

  LOG_INFO << " RemoveLogYList: " << HistName << " not on list - not removing" <<endm;
  return 0;
}


//_____________________________________________________________________________

Int_t StHistUtil::RemoveFromLogXList(const Char_t *HistName){  
// Method RemoveFromLogXList
//   remove hist from  list  that we want drawn with LogX scale

  if (Debug()) {
    LOG_INFO << " **** Now in StHistUtil::RemoveFromLogXList  **** " << endm;
  }

// check if list exists:
  if (m_ListOfLogX) {
    
// the remove method for TList requires a TObject input  
// - check if it's  on the list - use FindObject method of TList
    TObject *lobj = 0;
    lobj = m_ListOfLogX->FindObject(HistName);
// now can use Remove method of TList
    if (lobj) {
      m_ListOfLogX->Remove(lobj);
      if (Debug()) {
        LOG_INFO << " RemoveLogXList: " << HistName << " has been removed from list" <<endm;
      }
    } else {
      LOG_INFO << " RemoveLogXList: " << HistName << " not on list - not removing" <<endm;
    }

    // return using a method of TList (inherits GetSize from TCollection)
    return m_ListOfLogX->GetSize();
  } 

  LOG_INFO << " RemoveLogXList: " << HistName << " not on list - not removing" <<endm;
  return 0;
}


//_____________________________________________________________________________

Int_t StHistUtil::RemoveFromPrintList(const Char_t *HistName){  
// Method RemoveFromPrintList
//   remove hist from  list  that we want drawn,printed

  if (Debug()) {
    LOG_INFO << " **** Now in StHistUtil::RemoveFromPrintList  **** " << endm;
  }

// check if list exists:
  if (m_ListOfPrint) {
    
// the remove method for TList requires a TObject input  
// - check if it's  on the list - use FindObject method of TList
    TObject *lobj = 0;
    lobj = m_ListOfPrint->FindObject(HistName);
// now can use Remove method of TList
    if (lobj) {
      m_ListOfPrint->Remove(lobj);
      if (Debug()) {
        LOG_INFO << " RemovePrintList: " << HistName << " has been removed from list" <<endm;
      }
    } else {
      LOG_INFO << " RemovePrintList: " << HistName << " not on list - not removing" <<endm;
    }

    // return using a method of TList (inherits GetSize from TCollection)
    return m_ListOfPrint->GetSize();
  } 

  LOG_INFO << " RemovePrintList: " << HistName << " not on list - not removing" <<endm;
  return 0;
}


//_____________________________________________________________________________
// Method SetDefaultLogYList
//    - create default list of histograms we want plotted in LogY scale

void StHistUtil::SetDefaultLogYList(const Char_t *dirName)
{  
// Method SetDefaultLogYList
//    - create default list of histograms we want plotted in LogY scale

  if (Debug()) {
    LOG_INFO << " **** Now in StHistUtil::SetDefaultLogYList  **** " << endm;
  }


  PathCopy(m_dirName,dirName);
  TString type;
  if (!strcmp(m_dirName,"QA"))
    type = "Tab";
  else if (!strcmp(m_dirName,"EventQA"))
    type = "StE";
  else // default for now
    type = "StE";

  const Char_t* sdefList[] = {
    #include "St_QA_Maker/QAhlist_logy.h"
  };

  Int_t lengofList = sizeof(sdefList)/sizeOfCharPtr;
  Int_t numLog = 0;
  Int_t ilg = 0;
  for (ilg=0;ilg<lengofList;ilg++) {
    TString listString = sdefList[ilg];
    if (!(listString.BeginsWith("fcl") ||
          listString.BeginsWith("fms_qt_") ||
          listString.BeginsWith("fpd_channel_"))) {
      for (Int_t k=0; k<numOfPosPrefixes; k++) {
        ((listString = type) += possiblePrefixes[k]) += sdefList[ilg];
        numLog = AddToLogYList(listString.Data());
      }
    } else numLog = AddToLogYList(listString.Data());
  }

  LOG_INFO <<  " !!!  StHistUtil::SetDefaultLogYList, # histogram put in list " << numLog << endm;

}

//_____________________________________________________________________________
// Method SetDefaultLogXList
//    - create default list of histograms we want plotted in LogX scale

void StHistUtil::SetDefaultLogXList(const Char_t *dirName)
{  
// Method SetDefaultLogXList
//    - create default list of histograms we want plotted in LogX scale

  if (Debug()) {
    LOG_INFO << " **** Now in StHistUtil::SetDefaultLogXList  **** " << endm;
  }

  PathCopy(m_dirName,dirName);
  TString type;
  if (!strcmp(m_dirName,"QA"))
    type = "Tab";
  else if (!strcmp(m_dirName,"EventQA"))
    type = "StE";
  else // default for now
    type = "StE";

  const Char_t* sdefList[] = {
    #include "St_QA_Maker/QAhlist_logx.h"
  };

  Int_t lengofList = sizeof(sdefList)/sizeOfCharPtr;
  Int_t numLog = 0;
  Int_t ilg = 0;
  for (ilg=0;ilg<lengofList;ilg++) {
    TString listString = sdefList[ilg];
    if (!(listString.BeginsWith("fcl") ||
          listString.BeginsWith("fms_qt_") ||
          listString.BeginsWith("fpd_channel_"))) {
      for (Int_t k=0; k<numOfPosPrefixes; k++) {
        ((listString = type) += possiblePrefixes[k]) += sdefList[ilg];
        numLog = AddToLogXList(listString.Data());
      }
    } else numLog = AddToLogXList(listString.Data());
  }

  LOG_INFO <<  " !!!  StHistUtil::SetDefaultLogXList, # histogram put in list " << numLog << endm;

}


//_____________________________________________________________________________
// Method SetDefaultPrintList
//    - create default list of histograms we want drawn,printed
//    - analType can be one of a number of predefined histogram lists,
//      or a file which has histogram names listed one per line
//      (files like $STAR/StRoot/St_QA_Maker/QAhlist*.h are properly
//      parsed as well, but histogram names are sufficient)

void StHistUtil::SetDefaultPrintList(const Char_t *dirName, const Char_t *analType)
{  

  LOG_INFO << " **** Now in StHistUtil::SetDefaultPrintList  **** " << endm;

  const Char_t **sdefList=0;
  Int_t lengofList = 0;
  bool mustDeleteList = false;

  PathCopy(m_dirName,dirName);
  TString type;
  if (!strcmp(m_dirName,"QA"))
    type = "Tab";
  else if (!strcmp(m_dirName,"EventQA"))
    type = "StE";
  else // default for now
    type = "StE";


// If not analysis Type is set, then don't setup a list
  if ((!strcmp(analType,"")) || (!strcmp(analType,"All")) ) {
    LOG_INFO << " All histograms in directory will be printed/drawn, no list set" << endm;
    return;
  }

// Cosmic Data Table QA list .................................................
  const Char_t* sdefList1[] = {
    #include "St_QA_Maker/QAhlist_QA_Cosmic.h"
  };
  if ((!strcmp(m_dirName,"QA")) && (!strcmp(analType,"Cosmic"))) {
    sdefList = sdefList1; lengofList = sizeof(sdefList1)/sizeOfCharPtr;
  }

// Test Table QA list.........................................................
  const Char_t* sdefList2[] = {
    #include "St_QA_Maker/QAhlist_QA_TestQATable.h"
  };
  if ((!strcmp(m_dirName,"QA")) && (!strcmp(analType,"TestQATable"))) {
    sdefList = sdefList2; lengofList = sizeof(sdefList2)/sizeOfCharPtr;
  }

// FTPC Table QA list.........................................................
  const Char_t* sdefList3[] = {
    #include "St_QA_Maker/QAhlist_QA_Ftpc.h"
  };
  if ((!strcmp(m_dirName,"QA")) && (!strcmp(analType,"Ftpc"))) {
    sdefList = sdefList3; lengofList = sizeof(sdefList3)/sizeOfCharPtr;
  }

// FTPC Table QA list.........................................................
  const Char_t* sdefList4[] = {
    #include "St_QA_Maker/QAhlist_QA_MDC3.h"
  };
  if ((!strcmp(m_dirName,"FlowTag")) && (!strcmp(analType,"MDC3"))) {
    sdefList = sdefList4; lengofList = sizeof(sdefList4)/sizeOfCharPtr;
  }

// St_QA_Maker histograms without svt and ftpc histograms.....................
  const Char_t* sdefList5[] = {
    #include "St_QA_Maker/QAhlist_QA_year1.h"
  };
  if ((!strcmp(m_dirName,"QA")) && (!strcmp(analType,"year1"))) {
    sdefList = sdefList5; lengofList = sizeof(sdefList5)/sizeOfCharPtr;
  }

// St_QA_Maker histograms without the svt and ftpc histograms.................
  const Char_t* sdefList6[] = {
    #include "St_QA_Maker/QAhlist_EventQA_year1.h"
  };
  if ((!strcmp(m_dirName,"EventQA")) && (!strcmp(analType,"year1"))) {
    sdefList = sdefList6; lengofList = sizeof(sdefList6)/sizeOfCharPtr;
  }

// St_QA_Maker histograms for QA shift........................................
  const Char_t* sdefList7[] = {
    #include "St_QA_Maker/QAhlist_QA_qa_shift.h"
  };
  if ((!strcmp(m_dirName,"QA")) && (!strcmp(analType,"qa_shift"))) {
    sdefList = sdefList7; lengofList = sizeof(sdefList7)/sizeOfCharPtr;
  }

// St_QA_Maker histograms for QA shift........................................
  const Char_t* sdefList8[] = {
    #include "St_QA_Maker/QAhlist_EventQA_qa_shift.h"
  };
  if ((!strcmp(m_dirName,"EventQA")) && (!strcmp(analType,"qa_shift"))) {
    sdefList = sdefList8; lengofList = sizeof(sdefList8)/sizeOfCharPtr;
  }

// St_QA_Maker histograms for tpcSectors......................................
  const Char_t* sdefList9[] = {
    #include "St_QA_Maker/QAhlist_tpcSectors.h"
  };
  if ((!strcmp(m_dirName,"EventQA")) && (!strcmp(analType,"tpcSectors"))) {
    sdefList = sdefList9; lengofList = sizeof(sdefList9)/sizeOfCharPtr;
  }

// St_QA_Maker histograms for Svt.............................................
  const Char_t* sdefList10[] = {
    #include "St_QA_Maker/QAhlist_Svt.h"
  };
  if (!strcmp(analType,"Svt")) {
    sdefList = sdefList10; lengofList = sizeof(sdefList10)/sizeOfCharPtr;
  }

// St_QA_Maker histograms for subsystems......................................
  const Char_t* sdefList11[] = {
    #include "St_QA_Maker/QAhlist_subsystems.h"
  };
  if (!strcmp(analType,"subsys")) {
    sdefList = sdefList11; lengofList = sizeof(sdefList11)/sizeOfCharPtr;
  }

  if (!sdefList) {
    // Try reading in a file as specified by analType
    ifstream analFile(analType);
    if (analFile.good()) {
      LOG_INFO << "Reading print list from: " << analType << endm;
      sdefList = new charptr[4096];
      mustDeleteList = true;
      char analBuffer[256];
      TString analString;
      Bool_t commenting = kFALSE;
      Int_t commentIdx = -1;
      while (analFile.getline(analBuffer,255)) {
        analString = analBuffer;
        if (commenting) {
          commentIdx = analString.Index("*/");
          if (commentIdx>=0) {
             analString.Remove(0,commentIdx+2);
             commenting = kFALSE;
          } else continue;
        }
        if (!commenting) {
          commentIdx = analString.Index("/*");
          if (commentIdx>=0) {
            analString.Remove(commentIdx);
            commenting = kTRUE;
          }
        }
        commentIdx = analString.Index("//");
        if (commentIdx>=0) analString.Remove(commentIdx);
        analString.Remove(TString::kBoth,' ').Remove(TString::kBoth,',').Remove(TString::kBoth,' ').Remove(TString::kBoth,'"'); 
        Int_t alen = analString.Length();
        if (alen<1) continue;
        Char_t* tempc = new Char_t[alen+1];
        strcpy(tempc,analString.Data());
        sdefList[lengofList] = tempc;
        lengofList++;
      }
    } else {
      LOG_WARN << "Could not find list of histograms specified by: " << analType << endm;
    }
  }

  Int_t numPrt = 0;
  Int_t ilg = 0;
  for (ilg=0;ilg<lengofList;ilg++) {
    TString ilgString = sdefList[ilg];
    if (mustDeleteList) delete [] sdefList[ilg];
    Bool_t addIt = kTRUE;
    if (ilgString.BeginsWith(":")) {
      Ssiz_t endDetSpec = ilgString.Index(":",1) + 1;
      TString detSpec = ilgString(0,endDetSpec);
      addIt = DetectorIn(detSpec.Data());
      if (addIt) ilgString.Remove(0,endDetSpec);
    }
    if (addIt) {
      if (!(ilgString.BeginsWith("fcl") ||
            ilgString.BeginsWith("fms_qt_") ||
            ilgString.BeginsWith("Z3A") ||
            ilgString.Contains("etof",TString::ECaseCompare::kIgnoreCase) ||
            ilgString.BeginsWith("G_matchCand_") ||
            ilgString.BeginsWith("fpd_channel_"))) {
        for (Int_t k=0; k<numOfPosPrefixes; k++) {
          TString listString = type;
          (listString += possiblePrefixes[k]) += ilgString;
          numPrt = AddToPrintList(listString.Data());
        }
      } else numPrt = AddToPrintList(ilgString.Data());
      if (Debug()) {
        LOG_INFO <<  " !!! adding histogram " << ilgString << " to print list "  << endm ;
      }
    }
  }
  if (mustDeleteList) delete [] sdefList;
  
  LOG_INFO <<  " !!!  StHistUtil::SetDefaultPrintList, # histogram put in list " << numPrt << endm;

}

//_____________________________________________________________________________

// Method Overlay1D
//    - takes two TH1F histograms and overlays them

Int_t StHistUtil::Overlay1D(Char_t *dirName,Char_t *inHist1,
			    Char_t *inHist2) {

  LOG_INFO << " **** Now in StHistUtil::Overlay1D **** " << endm;

  Int_t n1dHists = 0;
  PathCopy(m_dirName,dirName);

// get the TList pointer to the histograms
  TList* dirList = (m_PntrToMaker ? FindHists(m_dirName) : FindHists(m_PntrToPlainFile));

// check that directory exists
  if (!dirList)
    return kStErr;

  LOG_INFO << "Histogram directory exists -> Find and overlay histograms" << endm;
// Now want to loop over all histograms
// Create an iterator
  TIter nextObj(dirList);
  TObject *obj = 0;

// temporary holder histograms
  TH1F *hist1f1 = new TH1F;
  TH1F *hist1f2 = new TH1F;

// use = here instead of ==, because we are setting obj equal to nextObj
// and then seeing if it's T or F
  while ((obj = nextObj())) {

// now check if obj is a histogram and see if it matches input name
    if (obj->InheritsFrom("TH1")) {
      if (obj->GetName() == (TString)inHist1 ||
	  obj->GetName() == (TString)inHist2) {
	LOG_INFO << " Found Histogram: Type '" << obj->ClassName() << "', Name '"
	     << obj->GetName() << "', Title '" << obj->GetTitle() << "'"
	     << endm;

// check on type of histogram and make copies
	if (obj->ClassName() == (TString)"TH1F") {
	  if (obj->GetName() == (TString)inHist1) {
	    *hist1f1 = *(TH1F *)obj;
	    n1dHists++;
	  }
	  if (obj->GetName() == (TString)inHist2) {
	    *hist1f2 = *(TH1F *)obj;
	    n1dHists++;
	  }
	} else {
	  LOG_INFO << " ERROR: histogram not of type TH1F !!!" << endm;
        }
      }
    }
  }

// if the two histograms exist, overlay them
  if (n1dHists == 2) {
    hist1f1->SetLineColor(4);
    hist1f1->SetLineStyle(1);
    hist1f2->SetLineColor(2);
    hist1f2->SetLineStyle(2);

    hist1f1->SetStats(kFALSE);
    hist1f2->SetStats(kFALSE);

    hist1f1->SetTitle(hist1f1->GetTitle()+(TString)" and "+hist1f2->GetTitle());
    hist1f2->SetTitle(hist1f1->GetTitle());
// create a new canvas
    TCanvas *newCanvas = new TCanvas("c1d","Combined 1D Histogram",600,780);
    newCanvas->Draw();

// write title at top of canvas
    Ltitle = new TPaveLabel(0.1,0.96,0.9,1.0,m_GlobalTitle.Data(),"br");
    Ltitle->SetFillColor(18);
    Ltitle->SetTextFont(32);
    Ltitle->SetTextSize(0.5);
    Ltitle->Draw();

// now put in date & time at bottom right of canvas
    TDatime HistTime;
    const Char_t *myTime = HistTime.AsString();
    TPaveLabel *Ldatetime = new TPaveLabel(0.7,0.01,0.95,0.03,myTime,"br");
    Ldatetime->SetTextSize(0.6);
    Ldatetime->Draw();

// create a pad
    TPad *newPad = new TPad("p1d","Combined 1D Histogram",0.02,0.04,0.98,0.93);
    newPad->Draw();
    newPad->cd();

// draw the histograms
    if (hist1f1->GetMaximum() >= hist1f2->GetMaximum()) {
      hist1f1->Draw();
      hist1f2->Draw("Same");
    }
    else {
      hist1f2->Draw();
      hist1f1->Draw("Same");
    }

// make a legend
    TLegend *legend = new TLegend(0.75,0.85,0.98,0.95);
    legend->SetFillColor(0);
    legend->SetHeader("Legend");
    legend->SetMargin(0.25);
    legend->AddEntry(hist1f1,inHist1,"l");
    legend->AddEntry(hist1f2,inHist2,"l");
    legend->Draw();

    newCanvas->Update();

    return kStOk;
  }

  return kStErr;
}

//_____________________________________________________________________________

// Method Overlay2D
//    - takes two TH2F histograms and overlays them

Int_t StHistUtil::Overlay2D(Char_t *dirName,Char_t *inHist1,
			    Char_t *inHist2) {

  LOG_INFO << " **** Now in StHistUtil::Overlay2D **** " << endm;

  Int_t n2dHists = 0;
  PathCopy(m_dirName,dirName);

// get the TList pointer to the histograms
  TList* dirList = (m_PntrToMaker ? FindHists(m_dirName) : FindHists(m_PntrToPlainFile));

// check that directory exists
  if (!dirList)
    return kStErr;

  LOG_INFO << "Histogram directory exists -> Find and overlay histograms" << endm;
// Now want to loop over all histograms
// Create an iterator
  TIter nextObj(dirList);
  TObject *obj = 0;

// temporary holder histograms
  TH2F *hist2f1 = new TH2F;
  TH2F *hist2f2 = new TH2F;

// use = here instead of ==, because we are setting obj equal to nextObj
// and then seeing if it's T or F
  while ((obj = nextObj())) {

// now check if obj is a histogram and see if it matches input name
    if (obj->InheritsFrom("TH1")) {
      if (obj->GetName() == (TString)inHist1 ||
	  obj->GetName() == (TString)inHist2) {
	LOG_INFO << " Found Histogram: Type '" << obj->ClassName() << "', Name '"
	     << obj->GetName() << "', Title '" << obj->GetTitle() << "'"
	     << endm;

// check on type of histogram and make copies
	if (obj->ClassName() == (TString)"TH2F") {
	  if (obj->GetName() == (TString)inHist1) {
	    *hist2f1 = *(TH2F *)obj;
	    n2dHists++;
	  }
	  if (obj->GetName() == (TString)inHist2) {
	    *hist2f2 = *(TH2F *)obj;
	    n2dHists++;
	  }
	} else {
	  LOG_INFO << " ERROR: histogram is not of type TH2F !!!" << endm;
        }
      }
    }
  }

// if the two histograms exist, overlay them
  if (n2dHists == 2) {
    hist2f1->SetLineColor(4);
    hist2f2->SetLineColor(2);

    hist2f1->SetStats(kFALSE);
    hist2f2->SetStats(kFALSE);

    hist2f1->SetTitle(hist2f1->GetTitle()+(TString)" and "+hist2f2->GetTitle());
    hist2f2->SetTitle(hist2f1->GetTitle());

// create a new canvas and pad to write to
    TCanvas *newCanvas = new TCanvas("c2d","Combined 2D Histogram",600,780);
    newCanvas->Draw();

// write title at top of canvas
    Ltitle = new TPaveLabel(0.1,0.96,0.9,1.0,m_GlobalTitle.Data(),"br");
    Ltitle->SetFillColor(18);
    Ltitle->SetTextFont(32);
    Ltitle->SetTextSize(0.5);
    Ltitle->Draw();

// now put in date & time at bottom right of canvas
    TDatime HistTime;
    const Char_t *myTime = HistTime.AsString();
    TPaveLabel *Ldatetime = new TPaveLabel(0.7,0.01,0.95,0.03,myTime,"br");
    Ldatetime->SetTextSize(0.6);
    Ldatetime->Draw();

// create a pad
    TPad *newPad = new TPad("p2d","Combined 2D Histogram",0.02,0.04,0.98,0.93);
    newPad->Draw();
    newPad->cd();

// draw the histograms
    if (hist2f1->GetMaximum() >= hist2f2->GetMaximum()) {
      hist2f1->Draw("Box");
      hist2f2->Draw("BoxSame");
    }
    else {
      hist2f2->Draw("Box");
      hist2f1->Draw("BoxSame");
    }

// make a legend
    TLegend *legend = new TLegend(0.75,0.85,0.98,0.95);
    legend->SetFillColor(0);
    legend->SetHeader("Legend");
    legend->SetMargin(0.25);
    legend->AddEntry(hist2f1,inHist1,"f");
    legend->AddEntry(hist2f2,inHist2,"f");
    legend->Draw();

    newCanvas->Update();

    return kStOk;
  }

  return kStErr;
}

//_____________________________________________________________________________

// Method GetRunYear
//    - determines the run year from the filename
//    - assumes runnumber is first all digit token after 2 "_" delimiters
//    - assumes runyear is all but the last 6 digits of runnumber

Int_t StHistUtil::GetRunYear(const Char_t *filename) {

  m_RunYear = 0;
  TString FileName = filename;
  TString delim = "_";
  TObjArray* tokens = FileName.Tokenize(delim);
  for (int tk=2; tk<tokens->GetEntries(); tk++) {
    TString& tok = ((TObjString*) (tokens->At(tk)))->String(); 
    if (tok.IsDigit()) {
      Ssiz_t loc = tok.Length()-6;
      if (loc>0) m_RunYear = atoi(tok.Remove(loc).Data());
      break;
    }
  }
  return m_RunYear;

}

//_____________________________________________________________________________

// Method SetRefAnalysis
//    - Properly set input and output files for reference histogram analysis

void StHistUtil::SetRefAnalysis(const Char_t* refOutFile, const Char_t* refResultsFile,
              const Char_t* refCutsFile, const Char_t* refInFile) {

  LOG_INFO << "StHistUtil: Will run in reference histogram analysis mode." << endm;
  m_analMode = kTRUE;

  if (refInFile && strlen(refInFile)) {
    LOG_INFO << "StHistUtil: Using reference histogram file " << refInFile << endm;
    m_refInFile = new TFile(refInFile);
    if (!m_refInFile) {LOG_ERROR << "file not found: " << refInFile << endm;}
  }
  if (refCutsFile && strlen(refCutsFile)) {
    LOG_INFO << "StHistUtil: Using reference cuts file " << refCutsFile << endm;
    m_refCuts = new TList;
    ifstream refCuts(refCutsFile);
    if (!refCuts.is_open()) {
      LOG_ERROR << "StHistUtil: Unable to open cuts file! Proceeding with no cuts..." << endm;
      return;
    }
    char buf_name[256];
    char buf_opts[64];
    while (refCuts.good()) {
      refCuts >> buf_name;
      if (!refCuts.good()) break;
      int mode;
      double cut;
      refCuts >> mode >> cut >> buf_opts;
      if (!refCuts.good()) break;
      if (buf_opts[0] == '!') buf_opts[0] = 0; // no options
      if (Debug()) {
        LOG_INFO << "StHistUtil: Loading cut : " << buf_name << " : " << mode
                 << " : " << cut << " : " << buf_opts << " :" << endm;
      }
      m_refCuts->AddLast(new StHistUtilRef(buf_name,buf_opts,mode,cut));
    }
  }

  // refOutFile will not be used if no reference histograms are found
  PathCopy(m_refResultsFile,refResultsFile);
  // refOutFile will not be used if already writing hists to a ROOT file
  PathCopy(m_refOutFile,refOutFile);
}

//_____________________________________________________________________________

void StHistUtil::SetDetectors(const Char_t *detectors) {
  // entering a null pointer uses all detector hists
  // entering a zero length string uses no detector hists
  if (detectors) {
    m_Detectors = detectors;
    // use colons as delimiters
    m_Detectors.ReplaceAll(" ",":");
    m_Detectors.ReplaceAll(",",":");
    m_Detectors.Prepend(":");
    m_Detectors.Append(":");
    while (m_Detectors.Index("::") >= 0) m_Detectors.ReplaceAll("::",":");
    LOG_INFO << "StHistUtil::SetDetectors(): using detectors " << m_Detectors << endm;
  } else {
    m_Detectors = "";
    LOG_INFO << "StHistUtil::SetDetectors(): using all detectors" << endm;
  }
}

//_____________________________________________________________________________

Bool_t StHistUtil::DetectorIn(const Char_t *detector) {
  // detector should be passed with delimeters, e.g. ":tpc:"
  // semicolons should separate multiple possible detectors ("or")
  // commas should separate multiple required detectors ("and")
  // Example ":tpc,svt;tpx,svt:" is either tpc+svt or tpx+svt
  Bool_t isIn = kFALSE;
  if (m_Detectors.Length()) {
    Ssiz_t idx =  0;
    TString detOpts = detector;
    TString curDetSet = "";
    TString curDet = "";
    while (idx < detOpts.Length() && ! isIn) {
      Ssiz_t D1 = detOpts.Index(";",idx);
      Ssiz_t setLength = (D1 < idx ? detOpts.Length() : D1+1) - idx;
      curDetSet = detOpts(idx,setLength);
      Ssiz_t idx2 = 0;
      isIn = kTRUE;
      while (idx2 < curDetSet.Length() && isIn) {
        Ssiz_t D2 = curDetSet.Index(",",idx2);
        Ssiz_t detLength = (D2 < idx2 ? curDetSet.Length() : D2+1) - idx2;
        curDet = curDetSet(idx2,detLength);
        curDet.ReplaceAll(";",":");
        curDet.ReplaceAll(",",":");
        curDet.Prepend(":");
        curDet.Append(":");
        while (curDet.Index("::") >= 0) curDet.ReplaceAll("::",":");
        if (m_Detectors.Index(curDet.Data()) < 0) isIn = kFALSE;
        else idx2 += detLength;
      }
      idx += setLength;
    }
  } else {
    // set to use all detectors
    isIn = kTRUE;
  }
  return isIn;
}

//_____________________________________________________________________________

TH1* StHistUtil::FlipAxes(TH1* hist) {
  if (!(hist->InheritsFrom("TH2"))) return hist;
  // For now, just converts whatever into TH2D
  // ...and there is no automatic garbage collection
  Int_t xbins = hist->GetNbinsX();
  Int_t ybins = hist->GetNbinsY();
  TH2D* newhist = new TH2D(Form("%s_flip",hist->GetName()),hist->GetTitle(),
    ybins,hist->GetYaxis()->GetXmin(),hist->GetYaxis()->GetXmax(),
    xbins,hist->GetXaxis()->GetXmin(),hist->GetXaxis()->GetXmax());
  for (int xbin=1; xbin<=xbins; xbin++) {
    for (int ybin=1; ybin<=ybins; ybin++) {
      newhist->SetBinContent(ybin,xbin,hist->GetBinContent(xbin,ybin));
      newhist->SetBinError(ybin,xbin,hist->GetBinError(xbin,ybin));
    }
  }
  newhist->SetMinimum(hist->GetMinimumStored());
  newhist->SetMaximum(hist->GetMaximumStored());
  return newhist;
}

//_____________________________________________________________________________

void StHistUtil::PathCopy(char* destination, const char* source) {
  // carefully copy path strings to avoid buffer over-writes or over-reads
  if (source && strcmp(destination,source)) {
    if (strlen(source) < maxPathLen) {
      strncpy(destination,source,maxPathLen-1);
      destination[maxPathLen-1] = 0;
    } else {
      LOG_ERROR << " source path too long: " << source << endm;
    }
  }
}

//_____________________________________________________________________________

//_____________________________________________________________________________


ClassImp(StHistUtilRef)
StHistUtilRef::StHistUtilRef(const char* name, const char* opts,
                             const int mode, const double cut):
  TNamed(name,opts),Mode(mode),Cut(cut) {}
