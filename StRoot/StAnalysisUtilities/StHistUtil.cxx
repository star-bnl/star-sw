// $Id: StHistUtil.cxx,v 2.54 2009/03/23 23:50:16 genevb Exp $
// $Log: StHistUtil.cxx,v $
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

#include "StHistUtil.h"

typedef TH1* TH1ptr;
typedef char* charptr;

char* possibleQAPrefixes[9] = {"","LM","MM","HM","HP","XX","MB","CL","HT"};
char* possibleQASuffixes[9] = {
  "General",
  "Low Mult",
  "Mid Mult",
  "High Mult",
  "High Pt",
  "Other Physics",
  "MinBias",
  "Central",
  "High Tower"
};

int sizeOfCharPtr = sizeof(Char_t*);
int sizeOfTH1Ptr = sizeof(TH1*);

ClassImp(StHistUtil)
  
//_____________________________________________________________________________

// Constructor

StHistUtil::StHistUtil(){

  numOfPosPrefixes = 9;
  possiblePrefixes = possibleQAPrefixes;
  possibleSuffixes = possibleQASuffixes;

  m_ListOfLogY = 0;
  m_ListOfLogX = 0;
  m_ListOfPrint = 0;
  m_HistCanvas = 0;
  m_HistCanvasR = 0;
  debug = kFALSE;
  m_CurPrefix = -1;
  m_OutType = "ps"; // postscript output by default
  m_OutMultiPage = kTRUE;
  m_RunYear = 0;

  maxHistCopy = 512;
  newHist = new TH1ptr[maxHistCopy];
  memset(newHist,0,maxHistCopy*sizeOfTH1Ptr);
  m_dirName[0] = 0;

  ignorePrefixes = kFALSE;

  m_analMode = kFALSE;
  m_refResultsFile[0] = 0;
  m_refOutFile[0] = 0;
  m_refCuts = 0;
  m_refInFile = 0;
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
//   if (newHist){
//     delete [] newHist;
//   }
}
//_____________________________________________________________________________
void StHistUtil::SetOutFile(const Char_t *fileName, const Char_t* type) {
  m_OutFileName = fileName;
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
    else {
      LOG_INFO << "SetHistUtil::SetOutFile(): unknown type, assuming ps" << endm;
      m_OutType = "ps";
      m_OutFileName.Append(".ps");
    }
  }

  // Multipage output for ps,pdf
  m_OutMultiPage = !(m_OutType.CompareTo("ps")
                  && m_OutType.CompareTo("pdf") );
  if (m_OutMultiPage)
    LOG_INFO << "StHistUtil::SetOutFile(): Multipage output" << endm;
  else
    LOG_INFO << "StHistUtil::SetOutFile(): Single page output" << endm;
}
//_____________________________________________________________________________
void StHistUtil::CloseOutFile() {
  m_HistCanvas->Modified();
  m_HistCanvas->Update();
  if (!m_CurFileName.IsNull()) {
    if (m_OutMultiPage) m_CurFileName.Append(")");
    if (m_OutType.CompareTo("CC"))
      m_HistCanvas->Print(m_CurFileName.Data(),m_OutType.Data());
    else
      m_HistCanvas->SaveSource(m_CurFileName.Data());
    if (m_refInFile) {
      m_HistCanvasR->Modified();
      m_HistCanvasR->Update();
      m_CurFileNameR.Append(")");
      m_HistCanvasR->Print(m_CurFileNameR.Data(),m_OutType.Data());
      // anal mode doesn't support single page output
    }
  } else {
    LOG_INFO << "StHistUtil::CloseOutFile(): No output file" << endm;
  }
}
//_____________________________________________________________________________
TString StHistUtil::StripPrefixes(const Char_t* histName, Int_t& prenum) {
  // Figure out and strip appropriate prefix index
  TString hName(histName);
  Char_t makerBuffer[4];
  memset(makerBuffer,0,4);
  if ((hName.BeginsWith("Tab")) || (hName.BeginsWith("StE"))) {
    memcpy(makerBuffer,histName,3);
    hName.Remove(0,3);
  }
  prenum = 0; // Possible prefix=0 means no prefix
  for (Int_t i=1; i<numOfPosPrefixes; i++) {
    if (hName.BeginsWith(possiblePrefixes[i])) {
      prenum = i;
      hName.Remove(0,strlen(possiblePrefixes[i]));
      break;
    }
  }
  hName.Prepend(makerBuffer);
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

  Ldesc->Clear();
  Ldesc->AddText(possibleSuffixes[m_CurPrefix]);
  Ldesc->AddText("Hists");
  return kTRUE;
}
//_____________________________________________________________________________
Int_t StHistUtil::DrawHists(Char_t *dirName) {
// Method DrawHists -->
// Plot the selected  histograms and generate the postscript file as well 
  
  LOG_INFO << " **** Now in StHistUtil::DrawHists  **** " << endm;


  //set Style of Plots
  const Int_t numPads = m_PadColumns*m_PadRows;  
  // SetPaperSize wants width & height in cm: A4 is 20,26 & US is 20,24
  gStyle->SetPaperSize(m_PaperWidth,m_PaperHeight); 
  gStyle->SetOptStat(111111);
  gStyle->SetStatStyle(0);
  gStyle->SetOptDate(0);
  gStyle->SetPalette(1);

  
  //setup canvas
  SafeDelete(m_HistCanvas);
  SafeDelete(m_HistCanvasR);

  // TCanvas wants width & height in pixels (712 x 950 corresponds to A4 paper)
  //                                        (600 x 780                US      )
  //  TCanvas *m_HistCanvas = new TCanvas("CanvasName","Canvas Title",30*m_PaperWidth,30*m_PaperHeight);
  if (m_refInFile) {
    m_HistCanvasR = new TCanvas("CanvasNameR"," STAR Reference Histogram Canvas",20,20,600,780);
  }
  m_HistCanvas = new TCanvas("CanvasName"," STAR Maker Histogram Canvas",0,0,600,780);

  // write title at top of canvas - first page
  Ltitle = new TPaveLabel(0.08,0.96,0.88,1.0,m_GlobalTitle.Data(),"br");
  Ltitle->SetFillColor(18);
  Ltitle->SetTextFont(32);
  Ltitle->SetTextSize(0.5);
  Ltitle->Draw();
  if (m_refInFile) {
    m_HistCanvasR->cd();
    TPaveLabel* LtitleR = new TPaveLabel(0.08,0.96,0.88,1.0,m_refInFile->GetName(),"br");
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
  TPaveLabel *Ldatetime = new TPaveLabel(0.7,0.01,0.95,0.03,myTime,"br");
  Ldatetime->SetTextSize(0.6);
  Ldatetime->Draw();
  if (m_refInFile) {
    m_HistCanvasR->cd();
    Ldatetime->Draw();
    m_HistCanvas->cd();
  }


  // now put in page # at bottom left of canvas - first page
  Int_t Ipagenum=1;
  TPaveLabel *Lpage = new TPaveLabel(0.1,0.01,0.16,0.03,Form("%d",Ipagenum),"br");
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
  TPad *graphPad = new TPad("PadName","Pad Title",0.0,0.05,1.00,0.95);
  graphPad->Draw();
  graphPad->cd();
  graphPad->Divide(m_PadColumns,m_PadRows);
  TPad *graphPadR = 0;
  if (m_refInFile) {
    m_HistCanvasR->cd();
    graphPadR = new TPad("PadNameR","Pad TitleR",0.0,0.05,1.00,0.95);
    graphPadR->Draw();
    graphPadR->cd();
    graphPadR->Divide(m_PadColumns,m_PadRows);
    graphPad->cd();
  }

  Int_t padCount = 0;
  Bool_t padAdvance = kTRUE;


  // Now find the histograms
  // get the TList pointer to the histograms:
  if (dirName) strcpy(m_dirName,dirName);
  TList* dirList = FindHists(m_dirName);
  if (!dirList) LOG_INFO << " DrawHists - histograms not available! " << endm;

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

    if (obj->InheritsFrom("TH1")) { 
      TH1* hobj = (TH1*) obj;
      const char* oname = obj->GetName();
      TString oName = oname;
      histReadCounter++;
      LOG_INFO << Form(" %d. Reading ... %s::%s; Title=\"%s\"\n",
        histReadCounter,obj->ClassName(),oname, obj->GetTitle()) << endm;
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
	    histCounter,obj->ClassName(),oname, obj->GetTitle()) << endm;

          // Switch to a new page...............................
	  if (CheckOutFile(oname)) {
	    padCount = numPads;
	    Ipagenum = 0;
	  }
          if (padCount == numPads) {
            // must redraw the histcanvas for each new page!
            m_HistCanvas->Modified();
            m_HistCanvas->Update();
	    if (Ipagenum>0 && !m_CurFileName.IsNull()) {
              if (m_OutType.CompareTo("CC"))
	        m_HistCanvas->Print(m_CurFileName.Data(),m_OutType.Data());
              else
                m_HistCanvas->SaveSource(m_CurFileName.Data());
	      m_CurFileName.ReplaceAll("(",0); // doesn't hurt to do > once
            } else {
	      m_HistCanvas->Draw();
	    }

	    while (padCount > 0) graphPad->GetPad(padCount--)->Clear();

            if (m_refInFile) {
              m_HistCanvasR->Modified();
              m_HistCanvasR->Update();
	      if (Ipagenum>0 && !m_CurFileName.IsNull()) {
	        m_HistCanvasR->Print(m_CurFileNameR.Data(),m_OutType.Data());
	        m_CurFileNameR.ReplaceAll("(",0); // doesn't hurt to do > once
	      } else m_HistCanvasR->Draw();
              padCount = numPads;
	      while (padCount > 0) graphPadR->GetPad(padCount--)->Clear();
            }

            // update the page number
            Ipagenum++;
            Lpage->SetLabel(Form("%d",Ipagenum));

	    if (!m_OutMultiPage && !m_CurFileName.IsNull()) {
              Ssiz_t last_us = m_CurFileName.Last('_') + 1;
              Ssiz_t lastdot = m_CurFileName.Last('.') - last_us;
              m_CurFileName.Replace(last_us,lastdot,Form("%d",Ipagenum));
	    }
          }

          // check dimension of histogram
          Int_t chkdim = hobj->GetDimension();

          // go to next pad 
          int curPad = (++padCount);

          // Will need to display both histograms if doing reference analysis...
          TH1* hobjR = (TH1*) (dirListR ? dirListR->FindObject(oname) : 0);
          TH1* hobjO = hobj;
          for (int analRepeat = 0;analRepeat < (hobjR ? 2 : 1); analRepeat++) {

	  padAdvance = kTRUE;
          if (analRepeat) {graphPadR->cd(curPad); hobj=hobjR;}
	  else graphPad->cd(curPad);

          // set x & y grid off by default
	  gPad->SetGridy(0);
	  gPad->SetGridx(0);
	  
          // set logX,Y,Z scale on/off

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
            LOG_INFO << "       -- Will draw in logX scale: " << oname <<endm;
	  } else gPad->SetLogx(0);


// Set logZ scale
          if (oName.EndsWith("PVsDedx") ||
              oName.Contains("TpcSector") ||
              oName.Contains("PointRPTpc") ||
              oName.Contains("PointXYTpc") ) {
            gPad->SetLogz(1);
            LOG_INFO << "       -- Will draw in logZ scale: " << oname <<endm;
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
              oName.EndsWith("trkGoodTTS")) {
            Float_t mean = hobj->GetMean(1);
            Float_t window = hobj->GetRMS(1);
            Float_t bwid = hobj->GetBinWidth(1);
            if (window < bwid) window = bwid;
            hobj->SetAxisRange(mean-5*window,mean+5*window,"X");
          }

// Limit both x & y ranges together for some histograms
          if (oName.EndsWith("trkGoodF")) {
            Float_t mean1 = hobj->GetMean(1);
            Float_t mean2 = hobj->GetMean(2);
            Float_t window1 = hobj->GetRMS(1);
            Float_t window2 = hobj->GetRMS(2);
            Float_t bwid = hobj->GetBinWidth(1);
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
          if ((chkdim == 3) && (obj->InheritsFrom("StMultiH2F"))) {
            obj->Draw("Box");
          } else  if ((chkdim==2) && (oName.Contains("PointRPTpc") ||
                      (oName.Contains("PointXYTpc") &&  // Unfortunately was polar for a short time
                       TMath::Abs((hobj->GetYaxis()->GetXmax()/TMath::Pi())-2)<1e-5))) {
            TH2F* htmp = new TH2F(Form("%s.",obj->GetName()),obj->GetTitle(),1,-200,200,1,-200,200);
            htmp->Fill(0.,0.,.1);htmp->SetMinimum(1);
            htmp->SetStats(kFALSE);
            htmp->Draw();
            obj->Draw("pol zcol same");
          } else if ((chkdim == 2) &&
                     (oName.EndsWith("SvtLoc") ||
                      oName.EndsWith("PVsDedx") ||
                      oName.EndsWith("SSD") ||
                      oName.EndsWith("PointXYSvt") ||
                      oName.Contains("TpcSector") ||
                      oName.Contains("PointXYTpc"))) {
            obj->Draw("ZCol");
          } else if ((chkdim == 2) && (!obj->InheritsFrom("StMultiH1F"))) {
            obj->Draw("Box");
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
	    if (oName.BeginsWith("fcl_radial")) {
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
            latex.SetTextAngle(90);
            latex.SetTextAlign(3);
            latex.DrawLatex(0.5,0,Form("   found:  %d",
              (int) (hobj->GetBinContent(hobj->FindBin(1.)))));
            latex.SetTextAlign(1);
            latex.DrawLatex(-0.5,0,Form("   missed:  %d",
              (int) (hobj->GetBinContent(hobj->FindBin(-1.)))));
          }

          if (padAdvance) {if (gPad) gPad->Update();}
          else {if (!analRepeat) padCount--;}

          } // analRepeat for loop

          // Run reference analysis...(on first loop, before we forgot hobj)
          if (hobjR) {
            StHistUtilRef* huR =
             (StHistUtilRef*) (m_refCuts ? m_refCuts->FindObject(oname) : 0);
            double result = 0;

            // default to Kolm. for 1-d hists, Chi2 for others
            int mode = ( huR ? huR->Mode : (chkdim > 1 ? 0 : 1) );
            double cut = ( huR ? huR->Cut : 0);

            // modes:
            switch (mode) {
              case (0) : // Chi2
                result = hobjO->Chi2Test(hobjR,(huR ? huR->Options() : "WW"));
                break;
              case (1) : // Kolmogorov
                // Note that Sumw2() seems to affect the way histograms are drawn,
                // but Kolmogorov test complains in current ROOT version (won't in
                // future versions). Don't know what to do for now.
                //if (hobjO->GetSumw2N() == 0) hobjO->Sumw2();
                //if (hobjR->GetSumw2N() == 0) hobjR->Sumw2();
                result = hobjO->KolmogorovTest(hobjR,(huR ? huR->Options() : ""));
                break;
            }

            LOG_INFO << Form("  -   %d. Reference Test (mode=%d) Score: %f (vs. cut = %f => %s)",
                        histReadCounter,mode,result,cut,
                        (result < cut ? "FAIL" : "PASS")) << endm;
            if (!R_ostr) R_ostr = new ofstream(m_refResultsFile);
            (*R_ostr) << Ipagenum << " " << curPad << " " << oname << " " << result << endl;
          }

        }
      }

//NOTE! (13jan00,kt) 
//--> obj->Draw just appends the histogram to the list
//    --> you must update the current pad (gPad) or the whole big pad (graphPad)
//        to actually see the stupid thing

// just ended  actual loop over histograms !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    }
  }

  if (C_ostr) delete C_ostr;
  if (R_ostr) delete R_ostr;
  if (root_ofile) delete root_ofile;

  CloseOutFile();
  return histCounter;
}
 
//_____________________________________________________________________________


TList* StHistUtil::FindHists(Char_t *dirName, Char_t *withPrefix) 
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
  if (dirName) strcpy(m_dirName,dirName);
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
	   << dirName <<  endm;
     }
  else { 
         LOG_INFO << " FindHists - histogram branch has not been found for branch --> "
	   << dirName <<  endm;
     }

  }

  if (dList && (withPrefix || m_ListOfPrint)) dList = TrimListByPrefix(dList,withPrefix);

  LOG_INFO << " FindHists, dList pointer = " << dList << endm;
  
 
 return dList;
}
//_____________________________________________________________________________
 
TList* StHistUtil::FindHists(TFile* histFile, Char_t* withPrefix) {
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
 
TList* StHistUtil::TrimListByPrefix(TList* dList, Char_t* withPrefix) {
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


Int_t StHistUtil::ListHists(Char_t *dirName) 
{  
// Method ListHists -->
// List of all histograms

  if (Debug())
    LOG_INFO << " **** Now in StHistUtil::ListHists **** " << endm;

// get the TList pointer to the histograms:
  TList  *dirList = 0;
  if (dirName) strcpy(m_dirName,dirName);
  dirList = FindHists(m_dirName);

  if (!dirList) LOG_INFO << " ListHists - histograms not available! " << endm;

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

  if (!dirList) LOG_INFO << " PrintInfoHists - histograms not available! " << endm;

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
  if (Debug())
    LOG_INFO << " **** Now in StHistUtil::CopyHists **** " << endm;

  if (!dirList) LOG_INFO << " StHistUtil::CopyHists - histogram Pointer not set! " << endm;

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
       if (ijk>=maxHistCopy){
         Int_t newMaxHistCopy = maxHistCopy * 4;
         TH1** temp1 = new TH1ptr[newMaxHistCopy];
         memset(temp1,0,newMaxHistCopy*sizeOfTH1Ptr);
         memcpy(temp1,newHist,maxHistCopy*sizeOfTH1Ptr);
         delete newHist;
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
  if (Debug())
    LOG_INFO << " **** Now in StHistUtil::AddHists **** " << endm;
  //  LOG_INFO << " num hists to copy = " << numHistCopy << endm;

  if (!dirList) LOG_INFO << 
        " StHistUtil::AddHists - histogram Pointer not set! " << endm;

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
	Int_t imk = 0;
	for (imk=0;imk<numHistCopy;imk++) {
          if (newHist[imk]) {		
             TString nName = newHist[imk]->GetName();
             if (ignorePrefixes) nName = StripPrefixes(nName.Data(),tempInt);
             if (! (nName.CompareTo(oName))) {
	       //LOG_INFO << "  ---- hist num to add --- " << imk << endm;
	       newHist[imk]->Add((TH1 *)obj);
	       histAddCount++;
	       //LOG_INFO << " !!! Added histograms with Name: " << newHist[imk]->GetName() <<  endm;
	     } // strcmp
	  }  // if newHist[imk] exists   
	}  // loop over imk
      }   // if obj inherits from th1
    }    //while
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

  if (Debug())
    LOG_INFO << " **** Now in StHistUtil::ExamineLogYList **** " << endm;

// m_ListOfLogY -  is a list of log plots
// construct a TObject
  TObject *obj = 0;
// construct a TIter ==>  () is an overloaded operator in TIter
  TIter nextObj(m_ListOfLogY);
  Int_t LogYCount = 0;

// use = here instead of ==, because we are setting obj equal to nextObj and then seeing if it's T or F
  while ((obj = nextObj())) {

    if (Debug()) LOG_INFO << " StHistUtil::ExamineLogYList has hist " <<  obj->GetName() << endm;
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

  if (Debug())
    LOG_INFO << " **** Now in StHistUtil::ExamineLogXList **** " << endm;

// m_ListOfLogX -  is a list of log plots
// construct a TObject
  TObject *obj = 0;
// construct a TIter ==>  () is an overloaded operator in TIter
  TIter nextObj(m_ListOfLogX);
  Int_t LogXCount = 0;

// use = here instead of ==, because we are setting obj equal to nextObj and then seeing if it's T or F
  while ((obj = nextObj())) {

    if (Debug())
      LOG_INFO << " StHistUtil::ExamineLogXList has hist " <<  obj->GetName() << endm;
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

  if (Debug())
    LOG_INFO << " **** Now in StHistUtil::ExaminePrintList **** " << endm;

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

    if (Debug())
      LOG_INFO << " StHistUtil::ExaminePrintList has hist " <<  obj->GetName() << endm;
    PrintCount++;

  }

  LOG_INFO << " Now in StHistUtil::ExaminePrintList, No. Hist. to Print,Draw = " << PrintCount <<endm;
  return m_ListOfPrint->GetSize();
}

//_____________________________________________________________________________


Int_t StHistUtil::AddToLogYList(const Char_t *HistName){  
// Method AddToLogYList
//   making list of all histograms that we want drawn with LogY scale

   if (Debug())
     LOG_INFO << " **** Now in StHistUtil::AddToLogYList  **** " << endm;

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
       if (Debug())
         LOG_INFO << " StHistUtil::AddToLogYList: " << HistName  <<endm;
    }
    else  LOG_INFO << " StHistUtil::AddToLogYList: " << HistName << " already in list - not added" <<endm;
 
// return using a method of TList (inherits GetSize from TCollection)
  return m_ListOfLogY->GetSize();
}


//_____________________________________________________________________________


Int_t StHistUtil::AddToLogXList(const Char_t *HistName){  
// Method AddToLogXList
//   making list of all histograms that we want drawn with LogX scale

   if (Debug())
     LOG_INFO << " **** Now in StHistUtil::AddToLogXList  **** " << endm;

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
       if (Debug())
         LOG_INFO << " StHistUtil::AddToLogXList: " << HistName  <<endm;
    }
    else  LOG_INFO << " StHistUtil::AddToLogXList: " << HistName << " already in list - not added" <<endm;
 
// return using a method of TList (inherits GetSize from TCollection)
 return m_ListOfLogX->GetSize();
}


//_____________________________________________________________________________


Int_t StHistUtil::AddToPrintList(const Char_t *HistName){  

// Method AddToPrintList
//   making list of all histograms that we want drawn,printed

  if (Debug())
    LOG_INFO << " **** Now in StHistUtil::AddToPrintList  **** " << endm;

// Since I'm creating a new list, must delete it in the destructor!!
//make a new TList on heap(persistant); have already defined m_ListOfPrint in header file
   if (!m_ListOfPrint) m_ListOfPrint = new TList;

// the add method for TList requires a TObject input  (also can use TObjString)
// create TObjString on heap
   TObjString *HistNameObj = new TObjString(HistName);

// now can use Add method of TList
    if (!m_ListOfPrint->Contains(HistName)) {
       m_ListOfPrint->Add(HistNameObj);
       if (Debug())
         LOG_INFO << " StHistUtil::AddToPrintList: " << HistName  <<endm;
    }
    else  LOG_INFO << " StHistUtil::AddToPrintList: " << HistName << " already in list - not added" <<endm;
 
// return using a method of TList (inherits GetSize from TCollection)
 return m_ListOfPrint->GetSize();

}

//_____________________________________________________________________________

Int_t StHistUtil::RemoveFromLogYList(const Char_t *HistName){  
// Method RemoveFromLogYList
//   remove hist from  list  that we want drawn with LogY scale

  if (Debug())
    LOG_INFO << " **** Now in StHistUtil::RemoveFromLogYList  **** " << endm;

// check if list exists:
  if (m_ListOfLogY) {
    
// the remove method for TList requires a TObject input  
// - check if it's  on the list - use FindObject method of TList
    TObject *lobj = 0;
    lobj = m_ListOfLogY->FindObject(HistName);
// now can use Remove method of TList
    if (lobj) {
      m_ListOfLogY->Remove(lobj);
      if (Debug())
        LOG_INFO << " RemoveLogYList: " << HistName << " has been removed from list" <<endm;
    }
    else  LOG_INFO << " RemoveLogYList: " << HistName << " not on list - not removing" <<endm;

  } 
// return using a method of TList (inherits GetSize from TCollection)
 return m_ListOfLogY->GetSize();
}


//_____________________________________________________________________________

Int_t StHistUtil::RemoveFromLogXList(const Char_t *HistName){  
// Method RemoveFromLogXList
//   remove hist from  list  that we want drawn with LogX scale

  if (Debug())
    LOG_INFO << " **** Now in StHistUtil::RemoveFromLogXList  **** " << endm;

// check if list exists:
  if (m_ListOfLogX) {
    
// the remove method for TList requires a TObject input  
// - check if it's  on the list - use FindObject method of TList
    TObject *lobj = 0;
    lobj = m_ListOfLogX->FindObject(HistName);
// now can use Remove method of TList
    if (lobj) {
      m_ListOfLogX->Remove(lobj);
      if (Debug())
        LOG_INFO << " RemoveLogXList: " << HistName << " has been removed from list" <<endm;
    }
    else  LOG_INFO << " RemoveLogXList: " << HistName << " not on list - not removing" <<endm;

  } 
// return using a method of TList (inherits GetSize from TCollection)
 return m_ListOfLogX->GetSize();
}


//_____________________________________________________________________________

Int_t StHistUtil::RemoveFromPrintList(const Char_t *HistName){  
// Method RemoveFromPrintList
//   remove hist from  list  that we want drawn,printed

  if (Debug())
    LOG_INFO << " **** Now in StHistUtil::RemoveFromPrintList  **** " << endm;

// check if list exists:
  if (m_ListOfPrint) {
    
// the remove method for TList requires a TObject input  
// - check if it's  on the list - use FindObject method of TList
    TObject *lobj = 0;
    lobj = m_ListOfPrint->FindObject(HistName);
// now can use Remove method of TList
    if (lobj) {
      m_ListOfPrint->Remove(lobj);
      if (Debug())
        LOG_INFO << " RemovePrintList: " << HistName << " has been removed from list" <<endm;
    }
    else  LOG_INFO << " RemovePrintList: " << HistName << " not on list - not removing" <<endm;

  } 
// return using a method of TList (inherits GetSize from TCollection)
 return m_ListOfPrint->GetSize();
}


//_____________________________________________________________________________
// Method SetDefaultLogYList
//    - create default list of histograms we want plotted in LogY scale

void StHistUtil::SetDefaultLogYList(Char_t *dirName)
{  
// Method SetDefaultLogYList
//    - create default list of histograms we want plotted in LogY scale

  if (Debug())
    LOG_INFO << " **** Now in StHistUtil::SetDefaultLogYList  **** " << endm;


  if (dirName) strcpy(m_dirName,dirName);
  TString type;
  if (!strcmp(m_dirName,"QA"))
    type = "Tab";
  else if (!strcmp(m_dirName,"EventQA"))
    type = "StE";
  else // default for now
    type = "StE";

  Char_t* sdefList[] = {
    #include "St_QA_Maker/QAhlist_logy.h"
  };

  Int_t lengofList = sizeof(sdefList)/sizeOfCharPtr;
  Int_t numLog = 0;
  Int_t ilg = 0;
  for (ilg=0;ilg<lengofList;ilg++) {
    TString listString = sdefList[ilg];
    if (!listString.BeginsWith("fcl")) {
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

void StHistUtil::SetDefaultLogXList(Char_t *dirName)
{  
// Method SetDefaultLogXList
//    - create default list of histograms we want plotted in LogX scale

  if (Debug())
    LOG_INFO << " **** Now in StHistUtil::SetDefaultLogXList  **** " << endm;

  if (dirName) strcpy(m_dirName,dirName);
  TString type;
  if (!strcmp(m_dirName,"QA"))
    type = "Tab";
  else if (!strcmp(m_dirName,"EventQA"))
    type = "StE";
  else // default for now
    type = "StE";

  Char_t* sdefList[] = {
    #include "St_QA_Maker/QAhlist_logx.h"
  };

  Int_t lengofList = sizeof(sdefList)/sizeOfCharPtr;
  Int_t numLog = 0;
  Int_t ilg = 0;
  for (ilg=0;ilg<lengofList;ilg++) {
    TString listString = sdefList[ilg];
    if (!listString.BeginsWith("fcl")) {
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

void StHistUtil::SetDefaultPrintList(Char_t *dirName, Char_t *analType)
{  

  LOG_INFO << " **** Now in StHistUtil::SetDefaultPrintList  **** " << endm;

  Char_t **sdefList=0;
  Int_t lengofList = 0;

  if (dirName) strcpy(m_dirName,dirName);
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
  Char_t* sdefList1[] = {
    #include "St_QA_Maker/QAhlist_QA_Cosmic.h"
  };
  if ((!strcmp(m_dirName,"QA")) && (!strcmp(analType,"Cosmic"))) {
    sdefList = sdefList1; lengofList = sizeof(sdefList1)/sizeOfCharPtr;
  }

// Test Table QA list.........................................................
  Char_t* sdefList2[] = {
    #include "St_QA_Maker/QAhlist_QA_TestQATable.h"
  };
  if ((!strcmp(m_dirName,"QA")) && (!strcmp(analType,"TestQATable"))) {
    sdefList = sdefList2; lengofList = sizeof(sdefList2)/sizeOfCharPtr;
  }

// FTPC Table QA list.........................................................
  Char_t* sdefList3[] = {
    #include "St_QA_Maker/QAhlist_QA_Ftpc.h"
  };
  if ((!strcmp(m_dirName,"QA")) && (!strcmp(analType,"Ftpc"))) {
    sdefList = sdefList3; lengofList = sizeof(sdefList3)/sizeOfCharPtr;
  }

// FTPC Table QA list.........................................................
  Char_t* sdefList4[] = {
    #include "St_QA_Maker/QAhlist_QA_MDC3.h"
  };
  if ((!strcmp(m_dirName,"FlowTag")) && (!strcmp(analType,"MDC3"))) {
    sdefList = sdefList4; lengofList = sizeof(sdefList4)/sizeOfCharPtr;
  }

// St_QA_Maker histograms without svt and ftpc histograms.....................
  Char_t* sdefList5[] = {
    #include "St_QA_Maker/QAhlist_QA_year1.h"
  };
  if ((!strcmp(m_dirName,"QA")) && (!strcmp(analType,"year1"))) {
    sdefList = sdefList5; lengofList = sizeof(sdefList5)/sizeOfCharPtr;
  }

// St_QA_Maker histograms without the svt and ftpc histograms.................
  Char_t* sdefList6[] = {
    #include "St_QA_Maker/QAhlist_EventQA_year1.h"
  };
  if ((!strcmp(m_dirName,"EventQA")) && (!strcmp(analType,"year1"))) {
    sdefList = sdefList6; lengofList = sizeof(sdefList6)/sizeOfCharPtr;
  }

// St_QA_Maker histograms for QA shift........................................
  Char_t* sdefList7[] = {
    #include "St_QA_Maker/QAhlist_QA_qa_shift.h"
  };
  if ((!strcmp(m_dirName,"QA")) && (!strcmp(analType,"qa_shift"))) {
    sdefList = sdefList7; lengofList = sizeof(sdefList7)/sizeOfCharPtr;
  }

// St_QA_Maker histograms for QA shift........................................
  Char_t* sdefList8[] = {
    #include "St_QA_Maker/QAhlist_EventQA_qa_shift.h"
  };
  if ((!strcmp(m_dirName,"EventQA")) && (!strcmp(analType,"qa_shift"))) {
    sdefList = sdefList8; lengofList = sizeof(sdefList8)/sizeOfCharPtr;
  }

// St_QA_Maker histograms for tpcSectors......................................
  Char_t* sdefList9[] = {
    #include "St_QA_Maker/QAhlist_tpcSectors.h"
  };
  if ((!strcmp(m_dirName,"EventQA")) && (!strcmp(analType,"tpcSectors"))) {
    sdefList = sdefList9; lengofList = sizeof(sdefList9)/sizeOfCharPtr;
  }

// St_QA_Maker histograms for Svt.............................................
  Char_t* sdefList10[] = {
    #include "St_QA_Maker/QAhlist_Svt.h"
  };
  if (!strcmp(analType,"Svt")) {
    sdefList = sdefList10; lengofList = sizeof(sdefList10)/sizeOfCharPtr;
  }

  if (!sdefList) {
    // Try reading in a file as specified by analType
    ifstream analFile(analType);
    if (analFile.good()) {
      LOG_INFO << "Reading print list from: " << analType << endm;
      sdefList = new charptr[4096];
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
    TString listString = sdefList[ilg];
    if (!listString.BeginsWith("fcl")) {
      for (Int_t k=0; k<numOfPosPrefixes; k++) {
        ((listString = type) += possiblePrefixes[k]) += sdefList[ilg];
        numPrt = AddToPrintList(listString.Data());
      }
    } else numPrt = AddToPrintList(listString.Data());
    if (Debug())
      LOG_INFO <<  " !!! adding histogram " << sdefList[ilg] << " to print list "  << endm ;
  }
  
  LOG_INFO <<  " !!!  StHistUtil::SetDefaultPrintList, # histogram put in list " << numPrt << endm;

}

//_____________________________________________________________________________

// Method Overlay1D
//    - takes two TH1F histograms and overlays them

Int_t StHistUtil::Overlay1D(Char_t *dirName,Char_t *inHist1,
			    Char_t *inHist2) {

  LOG_INFO << " **** Now in StHistUtil::Overlay1D **** " << endm;

  Int_t n1dHists = 0;
  if (dirName) strcpy(m_dirName,dirName);

// get the TList pointer to the histograms
  TList  *dirList = 0;
  dirList = FindHists(m_dirName);

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
	}
	else
	  LOG_INFO << " ERROR: histogram not of type TH1F !!!" << endm;
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
  if (dirName) strcpy(m_dirName,dirName);

// get the TList pointer to the histograms
  TList  *dirList = 0;
  dirList = FindHists(m_dirName);

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
	}
	else
	  LOG_INFO << " ERROR: histogram is not of type TH2F !!!" << endm;
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
    m_refInFile = ( refInFile ? new TFile(refInFile) : 0 );
    if (!m_refInFile) {LOG_ERROR << "file not found: " << refInFile << endm;}
  }
  if (refCutsFile && strlen(refCutsFile)) {
    LOG_INFO << "StHistUtil: Using reference cuts file " << refCutsFile << endm;
    m_refCuts = new TList;
    ifstream refCuts(refCutsFile);
    char buf_name[256];
    char buf_opts[64];
    while (!refCuts.eof()) {
      refCuts >> buf_name;
      if (refCuts.eof()) break;
      int mode;
      double cut;
      refCuts >> mode >> cut >> buf_opts;
      if (buf_opts[0] == '!') buf_opts[0] = 0; // no options
      m_refCuts->AddLast(new StHistUtilRef(buf_name,buf_opts,mode,cut));
    }
  }

  // refOutFile will not be used if no reference histograms are found
  strcpy(m_refResultsFile,refResultsFile);
  // refOutFile will not be used if already writing hists to a ROOT file
  strcpy(m_refOutFile,refOutFile);
}

//_____________________________________________________________________________

//_____________________________________________________________________________


ClassImp(StHistUtilRef)
StHistUtilRef::StHistUtilRef(const char* name, const char* opts,
                             const int mode, const double cut):
  TNamed(name,opts),Mode(mode),Cut(cut) {}
