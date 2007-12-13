// $Id: StHistUtil.cxx,v 2.38 2007/12/13 23:17:45 genevb Exp $
// $Log: StHistUtil.cxx,v $
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
#include "StMessMgr.h"

#include "St_DataSetIter.h"
#include "StMaker.h"

#include "StHistUtil.h"

typedef TH1* TH1ptr;

Int_t numOfPosPrefixes = 9;
char* possiblePrefixes[9] = {"","LM","MM","HM","HP","XX","MB","CL","HT"};
char* possibleSuffixes[9] = {
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

  m_ListOfLogY = 0;
  m_ListOfLogX = 0;
  m_ListOfPrint = 0;
  m_HistCanvas = 0;
  debug = kFALSE;
  m_CurPrefix = -1;
  m_OutType = "ps"; // postscript output by default
  m_OutMultiPage = kTRUE;
  m_RunYear = 0;

  maxHistCopy = 512;
  newHist = new TH1ptr[maxHistCopy];
  memset(newHist,0,maxHistCopy*sizeOfTH1Ptr);
  m_dirName[0] = 0;

}
//_____________________________________________________________________________

// Destructor

StHistUtil::~StHistUtil(){
  SafeDelete(m_HistCanvas);
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
    m_HistCanvas->Print(m_CurFileName.Data(),m_OutType.Data());
  } else {
    LOG_INFO << "StHistUtil::CloseOutFile(): No output file" << endm;
  }
}
//_____________________________________________________________________________
Bool_t StHistUtil::CheckOutFile(const Char_t *histName) {
// Method to determine appropriate PostScript file for output

  // Figure out appropriate prefix index
  Int_t newPrefix = 0;
  TString hName = histName;
  if ((hName.BeginsWith("Tab")) || (hName.BeginsWith("StE"))) {
    hName.Remove(0,3);
  }
  for (Int_t i=1; i<numOfPosPrefixes; i++) {
    if (hName.BeginsWith(possiblePrefixes[i])) newPrefix = i;
  }

  if (newPrefix != m_CurPrefix) {
    CloseOutFile();
    m_CurPrefix = newPrefix;
    m_CurFileName = m_OutFileName;
    Ssiz_t insertPos = m_CurFileName.Last('.');
    if (insertPos<0) insertPos = m_CurFileName.Length();
    if (m_OutMultiPage) m_CurFileName.Append("(");
    else m_CurFileName.Insert(insertPos,"_");
    m_CurFileName.Insert(insertPos,possiblePrefixes[m_CurPrefix]);

    Ldesc->Clear();
    Ldesc->AddText(possibleSuffixes[m_CurPrefix]);
    Ldesc->AddText("Hists");
    return kTRUE;
  }
  return kFALSE;
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

  // TCanvas wants width & height in pixels (712 x 950 corresponds to A4 paper)
  //                                        (600 x 780                US      )
  //  TCanvas *m_HistCanvas = new TCanvas("CanvasName","Canvas Title",30*m_PaperWidth,30*m_PaperHeight);
  m_HistCanvas = new TCanvas("CanvasName"," STAR Maker Histogram Canvas",600,780);

  // write title at top of canvas - first page
  Ltitle = new TPaveLabel(0.08,0.96,0.88,1.0,m_GlobalTitle.Data(),"br");
  Ltitle->SetFillColor(18);
  Ltitle->SetTextFont(32);
  Ltitle->SetTextSize(0.5);
  Ltitle->Draw();

  // write descriptor at top of canvas - first page
  Ldesc = new TPaveText(0.90,0.96,0.99,1.0,"br");
  Ldesc->SetFillColor(18);
  Ldesc->SetTextFont(32);
  Ldesc->Draw();

  // now put in date & time at bottom right of canvas - first page
  TDatime HistTime;
  const Char_t *myTime = HistTime.AsString();
  TPaveLabel *Ldatetime = new TPaveLabel(0.7,0.01,0.95,0.03,myTime,"br");
  Ldatetime->SetTextSize(0.6);
  Ldatetime->Draw();


  // now put in page # at bottom left of canvas - first page
  Int_t Ipagenum=1;
  char Ctmp[100];
  //convert to character
  sprintf(Ctmp,"%d",Ipagenum);
  TPaveLabel *Lpage = new TPaveLabel(0.1,0.01,0.16,0.03,Ctmp,"br");
  Lpage->SetTextSize(0.6);
  Lpage->Draw();

  // Make 1 big pad on the canvas - make it a little bit inside the  canvas 
  //    - must cd to get to this pad! 
  // order is x1 y1 x2 y2 
  TPad *graphPad = new TPad("PadName","Pad Title",0.0,0.05,1.00,0.95);
  graphPad->Draw();
  graphPad->cd();
  graphPad->Divide(m_PadColumns,m_PadRows);

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


  TH1* hobj1 = NULL;
  TH1* hobj2 = NULL;
  TH1* hobjradialW = NULL;
  TH1* hobjradialE = NULL;

  TObject *obj = 0;
  Int_t chkdim=0;
  TLine ruler;
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
	      m_HistCanvas->Print(m_CurFileName.Data(),
	        m_OutType.Data());
	      m_CurFileName.ReplaceAll("(",0); // doesn't hurt to do > once
            } else {
	      m_HistCanvas->Draw();
	    }

	    while (padCount > 0) graphPad->GetPad(padCount--)->Clear();

            // update the page number
            Ipagenum++;
            sprintf(Ctmp,"%d",Ipagenum);
            Lpage->SetLabel(Ctmp);

	    if (!m_OutMultiPage && !m_CurFileName.IsNull()) {
              Ssiz_t last_us = m_CurFileName.Last('_') + 1;
              Ssiz_t lastdot = m_CurFileName.Last('.') - last_us;
              m_CurFileName.Replace(last_us,lastdot,Form("%d",Ipagenum));
	    }
          }

          // go to next pad 
	  graphPad->cd(++padCount);
	  padAdvance = kTRUE;
//NOTE! (13jan00,kt) -->  this cd is really acting on gPad!!!
//   --> gPad is a global variable & one uses it to set attributes of current pad
//  --> you can see the full list of global variables by starting ROOT and entering .g
//  --> to find the full list of commands, type ? in ROOT 

          // set x & y grid off by default
	  gPad->SetGridy(0);
	  gPad->SetGridx(0);
	  
          // set logY & logX scale off
	  gPad->SetLogy(0);
	  gPad->SetLogx(0);

// Set logY scale on if: there is a loglist, if the hist name is on the list, if it has entries
//    and if the max entries in all bins is > 0
          if (m_ListOfLogY && m_ListOfLogY->FindObject(oname) &&
	     hobj->GetEntries() && hobj->GetMaximum() ) {
	    gPad->SetLogy(1);
            LOG_INFO << "       -- Will draw in logY scale: " << oname <<endm;
	  }


// Set logX scale on if: there is a loglist, if the hist name is on the list, if it has entries
//    and if the max entries in all bins is > 0
	  if (m_ListOfLogX && m_ListOfLogX->FindObject(oname) &&
	     hobj->GetEntries() && hobj->GetMaximum() ) {
	    gPad->SetLogx(1);
            LOG_INFO << "       -- Will draw in logX scale: " << oname <<endm;
	  }


// Set logZ scale
          if (oName.EndsWith("PVsDedx")) {
            gPad->SetLogz(1);
            LOG_INFO << "       -- Will draw in logZ scale: " << oname <<endm;
          }

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


          // check dimension of histogram
          chkdim = hobj->GetDimension();

          // actually draw,print
          if ((chkdim == 3) && (obj->InheritsFrom("StMultiH2F"))) {
            obj->Draw("Box");
          } else if ((chkdim == 2) &&
                     (oName.EndsWith("SvtLoc") ||
                      oName.EndsWith("PVsDedx") ||
                      oName.EndsWith("SSD") ||
                      oName.EndsWith("PointXYSvt") ||
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
	    else if (oName.BeginsWith("fcl_radial")) {
	      if (oName.EndsWith("W")) hobjradialW = hobj;
	      else hobjradialE = hobj;
	      hobj->SetStats(kFALSE);     
              hobj->GetXaxis()->SetRangeUser(7.0,9.0);
              hobj->SetTitle("FTPCW+E cluster radial position");
	      if ( hobjradialW && hobjradialE) {
		if ( hobjradialW->GetMaximum() >= hobjradialE->GetMaximum()) {     
                  hobj1 = hobjradialW; hobj2 = hobjradialE;
                } else {
                  hobj1 = hobjradialE; hobj2 = hobjradialW;
                }
                hobj1->SetMinimum(0);
		hobj1->Draw();
		gPad->Modified();
		ruler.SetLineColor(kBlack);
                ruler.SetLineWidth(2);
                ruler.DrawLine(7.8,0.,7.8,hobj1->GetMaximum());
	        hobj2->Draw("Same");

                // make a legend
                TLegend *legend = new TLegend(0.75,0.85,0.98,0.95);
                legend->SetFillColor(0);
                legend->SetHeader("Legend");
                legend->SetMargin(0.25);
                legend->AddEntry(hobjradialE,"FtpcEast","l");
                legend->AddEntry(hobjradialW,"FtpcWest","l");
                legend->Draw();
	      } else {
	        padAdvance = kFALSE; // wait for both histograms before drawing
	      }
            }
	    else hobj->Draw();
	  }

	  if (!padAdvance) padCount--;
	  else if (gPad) gPad->Update();
        }
      }

//NOTE! (13jan00,kt) 
//--> obj->Draw just appends the histogram to the list
//    --> you must update the current pad (gPad) or the whole big pad (graphPad)
//        to actually see the stupid thing

// just ended  actual loop over histograms !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    }
  }

  CloseOutFile();
  return histCounter;
}
 
//_____________________________________________________________________________


TList* StHistUtil::FindHists(Char_t *dirName) 
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

  LOG_INFO << " FindHists, dList pointer = " << dList << endm;
  
 
 return dList;
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

  Int_t ijk=0;
  Int_t histCopyCount = 0;

  if (dirList){
   TIter nextObj(dirList);
   TObject *obj = 0;
    while ((obj = nextObj())) {    
     if (obj->InheritsFrom("TH1")) {
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

  if (dirList){
    if (numHistCopy < 0) numHistCopy = dirList->GetSize();
    TIter nextObj(dirList);
    TObject *obj = 0;

    while ((obj = nextObj())) {
      if (obj->InheritsFrom("TH1")) {
// now want to add these histograms to the copied ones:
	Int_t imk = 0;
	for (imk=0;imk<numHistCopy;imk++) {
          if (newHist[imk]) {		
	     if (strcmp( (newHist[imk]->GetName()), (obj->GetName()) )==0) {
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

//_____________________________________________________________________________

