// $Id: StHistUtil.cxx,v 2.11 2002/04/23 01:59:54 genevb Exp $
// $Log: StHistUtil.cxx,v $
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
                                                                          

#include <iostream.h>
#include <fstream.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <strstream.h>

#include "PhysicalConstants.h"
#include "TStyle.h"
#include "TCanvas.h"
#include "TObjString.h"
#include "TPostScript.h"
#include "TMath.h"
#include "TString.h"
#include "TPaveLabel.h"
#include "TPaveText.h"
#include "TLegend.h"
#include "TDatime.h"
#include "TLine.h"

#include "StChain.h"
#include "St_DataSetIter.h"
#include "StMaker.h"

#include "StHistUtil.h"


Int_t numOfPosPrefixes = 4;
char* possiblePrefixes[4] = {"","LM","MM","HM"};
char* possibleSuffixes[4] = {
  "General",
  "Low Mult",
  "Mid Mult",
  "High Mult"
};


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
Bool_t StHistUtil::CheckPSFile(const Char_t *histName) {
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
    m_CurPrefix = newPrefix;
    TString m_CurFileName = m_PsFileName;
    Ssiz_t insertPos = m_CurFileName.Index(".ps");
    if (insertPos < 0) {          // No .ps suffix in file name
      m_CurFileName.Append(possiblePrefixes[m_CurPrefix]);
      m_CurFileName.Append(".ps");
    } else {
      m_CurFileName.Insert(insertPos,possiblePrefixes[m_CurPrefix]);
    }
    if (!m_PsFileName.IsNull()) {
      if (psf) psf->Close();
      else psf = new TPostScript();
      psf->Open(m_CurFileName.Data());
    }
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
  
  cout << " **** Now in StHistUtil::DrawHists  **** " << endl;


  // set output ps file name
  psf = 0;


  //set Style of Plots
  const Int_t numPads = m_PadColumns*m_PadRows;  
  // SetPaperSize wants width & height in cm: A4 is 20,26 & US is 20,24
  gStyle->SetPaperSize(m_PaperWidth,m_PaperHeight); 
  gStyle->SetOptStat(111111);
  gStyle->SetStatStyle(0);


  //setup canvas
  SafeDelete(m_HistCanvas);

  // TCanvas wants width & height in pixels (712 x 950 corresponds to A4 paper)
  //                                        (600 x 780                US      )
  //  TCanvas *HistCanvas = new TCanvas("CanvasName","Canvas Title",30*m_PaperWidth,30*m_PaperHeight);
  TCanvas *HistCanvas = new TCanvas("CanvasName"," STAR Maker Histogram Canvas",600,780);

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
  //convert to character
  Char_t Ctmp[10];
  ostrstream Cpagenum(Ctmp,10);
  Cpagenum << Ipagenum << ends;
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


  // Now find the histograms
  // get the TList pointer to the histograms:
  TList* dirList = FindHists(dirName);
  if (!dirList) cout << " DrawHists - histograms not available! " << endl;

  TIter nextHist(dirList);
  Int_t histCounter = 0;
  Int_t histReadCounter = 0;
  Bool_t started = kFALSE;


  TObject *obj = 0;
  Int_t chkdim=0;
  TLine ruler;
  while ((obj = nextHist())) {

    if (obj->InheritsFrom("TH1")) { 
      TH1* hobj = (TH1*) obj;
      const char* oname = obj->GetName();
      TString oName = oname;
      histReadCounter++;
      printf(" %d. Reading ... %s::%s; Title=\"%s\"\n",
        histReadCounter,obj->ClassName(),oname, obj->GetTitle());
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
          printf("  -   %d. Drawing ... %s::%s; Title=\"%s\"\n",
	    histCounter,obj->ClassName(),oname, obj->GetTitle());

          // Switch to a new page...............................
	  if (CheckPSFile(oname)) {
	    padCount = numPads;
	    Ipagenum = 0;
	  }
          if (padCount == numPads) {
            if (psf) psf->NewPage();

            // update the page number
            Ipagenum++;
            Cpagenum.seekp(0);
            Cpagenum << Ipagenum << ends;
            Lpage->SetLabel(Ctmp);

            // must redraw the histcanvas for each new page!
            HistCanvas->Modified();
            HistCanvas->Update();
            padCount=0;
          }
          // go to next pad 
          graphPad->cd(++padCount);
//NOTE! (13jan00,kt) -->  this cd is really acting on gPad!!!
//   --> gPad is a global variable & one uses it to set attributes of current pad
//  --> you can see the full list of global variables by starting ROOT and entering .g
//  --> to find the full list of commands, type ? in ROOT 

          // set logY & logX scale off
	  gPad->SetLogy(0);
	  gPad->SetLogx(0);	  

// Set logY scale on if: there is a loglist, if the hist name is on the list, if it has entries
//    and if the max entries in all bins is > 0
          if (m_ListOfLogY && m_ListOfLogY->FindObject(oname) &&
	     hobj->GetEntries() && hobj->GetMaximum() ) {
	    gPad->SetLogy(1);
            cout << "       -- Will draw in logY scale: " << oname <<endl;
	  }


// Set logX scale on if: there is a loglist, if the hist name is on the list, if it has entries
//    and if the max entries in all bins is > 0
	  if (m_ListOfLogX && m_ListOfLogX->FindObject(oname) &&
	     hobj->GetEntries() && hobj->GetMaximum() ) {
	    gPad->SetLogx(1);
            cout << "       -- Will draw in logX scale: " << oname <<endl;
	  }

// Limit x range for some histograms
          if (oName.EndsWith("QaPointTpc") ||
              oName.EndsWith("QaPointSvt") ||
              oName.EndsWith("QaPointSsd") ||
              oName.EndsWith("QaPointFtpc") ||
              oName.EndsWith("QaRichTot") ||
              oName.EndsWith("QaV0Vtx") ||
              oName.EndsWith("QaXiVtxTot") ||
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

          // check dimension of histogram
          chkdim = hobj->GetDimension();

          // actually draw,print
          if ((chkdim == 2) && (!obj->InheritsFrom("StMultiH1F"))) {
            obj->Draw("box");
	    if (oName.EndsWith("trkGoodF")) {
              ruler.SetLineColor(46);
              ruler.SetLineWidth(2);
              ruler.DrawLineNDC(0.1,0.1,0.9,0.9);
	    }
          } else {
            if (oName.Contains("QaBbc") ||
                (oName.Contains("QaFpd") && !oName.Contains("Sums"))) {
              hobj->SetBarOffset();
            }
	    hobj->SetLineWidth(2);
	    hobj->Draw();
            if (!oName.CompareTo("fcl_radialW") ||
                !oName.CompareTo("fcl_radialE")) {
              ruler.SetLineColor(46);
              ruler.SetLineWidth(2);
              ruler.DrawLine(7.73,0.,7.73,hobj->GetMaximum());
            }
	  }
	  if (gPad) gPad->Update();
        }
      }

//NOTE! (13jan00,kt) 
//--> obj->Draw just appends the histogram to the list
//    --> you must update the current pad (gPad) or the whole big pad (graphPad)
//        to actually see the stupid thing

// just ended  actual loop over histograms !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    }
  }

  if (psf) {
    psf->Close();
    delete psf;
  } else cout << "StHistUtil::Draw(): No PostScript output" << endl;
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

  cout << " Beg: FindHists, dList pointer = " << dList << endl;

//---- First look under Maker for histograms ==>
//They  should show up in your Maker's directory, so search for them there,
//     i.e. MakerName/.hist is where they'd be
// Note: Histograms is a method of StMaker
//---- If you have a chain, you'll always have the .hist directory, so
//     have to check if there's really anything there (so use First method)

//
  StMaker *temp = m_PntrToMaker->GetMaker(dirName);
    if (temp) {
      cout << "FindHists - found pointer to maker" << endl;
      dList = temp->Histograms();
    }

// Now check to see if any histograms exist here (look for something in
//  the list (test)
  TObject *test=0;
  if (dList) test = dList->First();
  if (test){ 
      cout << " FindHists - found hist. in Maker-Branch " << endl;
     }

    cout << " Mid: FindHists, dList pointer = " << dList << endl;
    cout << " Mid: FindHists, test pointer =  " << test << endl;

// If you have the pointer but the hist. really aren't here, set
//  the pointer back to zero
  if (!test) dList = 0;

  cout << " Mid2: FindHists, dList pointer = " << dList << endl;
  cout << " Mid2: FindHists, test pointer =  " << test << endl;


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

    TString hBN(dirName);
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
      cout << " FindHists - found hist. in histBranch, with name:  " 
	   << dirName <<  endl;
     }
  else { 
         cout << " FindHists - histogram branch has not been found for branch --> "
	   << dirName <<  endl;
     }

  }

  cout << " FindHists, dList pointer = " << dList << endl;
  
 
 return dList;
}
//_____________________________________________________________________________


Int_t StHistUtil::ListHists(Char_t *dirName) 
{  
// Method ListHists -->
// List of all histograms

  if (Debug())
    cout << " **** Now in StHistUtil::ListHists **** " << endl;

// get the TList pointer to the histograms:
  TList  *dirList = 0;
  dirList = FindHists(dirName);

  if (!dirList) cout << " ListHists - histograms not available! " << endl;

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
//      printf(" %d. Have histogram Type %s, Name %s with Title=\"%s\"\n",histReadCount,obj->ClassName(),obj->GetName(),obj->GetTitle());
            cout << " ListHists: Hist No. " << histReadCount << ", Type: " << obj->ClassName() 
           << ", Name: " << obj->GetName() << ", Title \"" << obj->GetTitle() << "\"  "<< endl; 
    }
  }

  cout << " ListHists: Total No. Histograms Booked  = " << histReadCount <<endl;
  return histReadCount;
}


//_____________________________________________________________________________

Int_t StHistUtil::PrintInfoHists(TList *dirList,  const Char_t *fname )
{  

  cout << " **** Now in StHistUtil::PrintInfoHists **** " << endl;
  cout << " output file = " << fname << endl;

  ofstream fout(fname);

  if (!dirList) cout << " PrintInfoHists - histograms not available! " << endl;

  Int_t histInfoCount = 0;

  if (dirList){

//Now want to loop over all histograms
// Create an iterator
    TIter nextObj(dirList);
    TObject *obj = 0;

    cout << " Hist #, Name, #Entries, Mean, RMS " << endl;
    fout << " Hist #, Name, #Entries, Mean, RMS " << endl;

// use = instead of ==, because we are setting obj equal to nextObj and then seeing if it's T or F

    while ((obj = nextObj())) {

// now check if obj is a histogram
      if (obj->InheritsFrom("TH1")) {
 
        histInfoCount++;

        cout << 
              histInfoCount << " " <<
              obj->GetName() << " " <<
              ((TH1 *)obj)->GetEntries() << " " <<
              ((TH1 *)obj)->GetMean() << " " <<
              ((TH1 *)obj)->GetRMS() << " " <<
              endl;

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

  cout << " PrintInfoHists: # hist read  = " << histInfoCount <<endl;

  return histInfoCount;
}


//_____________________________________________________________________________


Int_t StHistUtil::CopyHists(TList *dirList)
{  
  if (Debug())
    cout << " **** Now in StHistUtil::CopyHists **** " << endl;

  if (!dirList) cout << " StHistUtil::CopyHists - histogram Pointer not set! " << endl;

// create array of pointers to the new histograms I will create
 Int_t ilg = 0;
 cout << " StHistUtil::CopyHists - max # hist to copy (hardwired) = " << 
   maxHistCopy << endl;
 for (ilg=0;ilg<maxHistCopy;ilg++) {
      newHist[ilg]=0;
 }

  Int_t ijk=0;
  Int_t histCopyCount = 0;

  if (dirList){
   TIter nextObj(dirList);
   TObject *obj = 0;
    while ((obj = nextObj())) {    
     if (obj->InheritsFrom("TH1")) {
       histCopyCount++;         
       if (ijk<maxHistCopy){
         newHist[ijk] = ((TH1 *)obj->Clone());
       } // if ijk
       ijk++;
     }   // if obj
    }    // while obj
  }      // if dirList

  cout << " ListHists: Total No. Histograms Copied  = " << 
        histCopyCount <<endl;

// Now see if we can find these copies:
 // Int_t imk = 0;
 //for (imk=0;imk<histCopyCount;imk++) {
 //  if (newHist[imk]->InheritsFrom("TH1")) {       
 //        cout << " !!! NEW Type: " << newHist[imk]->ClassName() << 
 //             ", Name: "    << newHist[imk]->GetName() << 
 //             ", Title: "   << newHist[imk]->GetTitle() << 
 //	    ", Max: " << ((TH1 *)newHist[imk])->GetMaximum() << endl; 
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
    cout << " **** Now in StHistUtil::AddHists **** " << endl;
  //  cout << " num hists to copy = " << numHistCopy << endl;

  if (!dirList) cout << 
        " StHistUtil::AddHists - histogram Pointer not set! " << endl;

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
	  if (strcmp( (newHist[imk]->GetName()), (obj->GetName()) )==0) {
	    //cout << "  ---- hist num to add --- " << imk << endl;
	    newHist[imk]->Add((TH1 *)obj);
	    histAddCount++;
	    //cout << " !!! Added histograms with Name: " << newHist[imk]->GetName() <<  endl;

	  } // strcmp
	}  // loop over imk
      }   // if obj inherits from th1
    }    //while
  } //dirlist

  cout << " StHistUtil::AddHists: Total No. Histograms Added  = " << 
        histAddCount <<endl;


  return histAddCount;
}

//_____________________________________________________________________________


Int_t StHistUtil::ExamineLogYList() 
{  
// Method ExamineLogYList
// List of all histograms that will be drawn with logy scale

  if (Debug())
    cout << " **** Now in StHistUtil::ExamineLogYList **** " << endl;

// m_ListOfLogY -  is a list of log plots
// construct a TObject
  TObject *obj = 0;
// construct a TIter ==>  () is an overloaded operator in TIter
  TIter nextObj(m_ListOfLogY);
  Int_t LogYCount = 0;

// use = here instead of ==, because we are setting obj equal to nextObj and then seeing if it's T or F
  while ((obj = nextObj())) {

    if (Debug()) cout << " StHistUtil::ExamineLogYList has hist " <<  obj->GetName() << endl;
    LogYCount++;

  }

  cout << " Now in StHistUtil::ExamineLogYList, No. Hist. in LogY scale = " << LogYCount <<endl;
  return LogYCount;
}

//_____________________________________________________________________________


Int_t StHistUtil::ExamineLogXList() 
{  
// Method ExamineLogXList
// List of all histograms that will be drawn with logX scale

  if (Debug())
    cout << " **** Now in StHistUtil::ExamineLogXList **** " << endl;

// m_ListOfLogX -  is a list of log plots
// construct a TObject
  TObject *obj = 0;
// construct a TIter ==>  () is an overloaded operator in TIter
  TIter nextObj(m_ListOfLogX);
  Int_t LogXCount = 0;

// use = here instead of ==, because we are setting obj equal to nextObj and then seeing if it's T or F
  while ((obj = nextObj())) {

    if (Debug())
      cout << " StHistUtil::ExamineLogXList has hist " <<  obj->GetName() << endl;
    LogXCount++;

  }

  cout << " Now in StHistUtil::ExamineLogXList, No. Hist. in LogX scale = " << LogXCount <<endl;
  return LogXCount;
}

//_____________________________________________________________________________


Int_t StHistUtil::ExaminePrintList() 
{  
// Method ExaminePrintList
// List of all histograms that will be drawn,printed

  if (Debug())
    cout << " **** Now in StHistUtil::ExaminePrintList **** " << endl;

// m_ListOfPrint -  is a list of hist to print,draw

// check if there is a list
  if (!m_ListOfPrint){
    cout << "      no subset print list was setup - all hist in directory will be printed " << endl;
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
      cout << " StHistUtil::ExaminePrintList has hist " <<  obj->GetName() << endl;
    PrintCount++;

  }

  cout << " Now in StHistUtil::ExaminePrintList, No. Hist. to Print,Draw = " << PrintCount <<endl;
  return m_ListOfPrint->GetSize();
}

//_____________________________________________________________________________


Int_t StHistUtil::AddToLogYList(const Char_t *HistName){  
// Method AddToLogYList
//   making list of all histograms that we want drawn with LogY scale

   if (Debug())
     cout << " **** Now in StHistUtil::AddToLogYList  **** " << endl;

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
         cout << " StHistUtil::AddToLogYList: " << HistName  <<endl;
    }
    else  cout << " StHistUtil::AddToLogYList: " << HistName << " already in list - not added" <<endl;
 
// return using a method of TList (inherits GetSize from TCollection)
  return m_ListOfLogY->GetSize();
}


//_____________________________________________________________________________


Int_t StHistUtil::AddToLogXList(const Char_t *HistName){  
// Method AddToLogXList
//   making list of all histograms that we want drawn with LogX scale

   if (Debug())
     cout << " **** Now in StHistUtil::AddToLogXList  **** " << endl;

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
         cout << " StHistUtil::AddToLogXList: " << HistName  <<endl;
    }
    else  cout << " StHistUtil::AddToLogXList: " << HistName << " already in list - not added" <<endl;
 
// return using a method of TList (inherits GetSize from TCollection)
 return m_ListOfLogX->GetSize();
}


//_____________________________________________________________________________


Int_t StHistUtil::AddToPrintList(const Char_t *HistName){  

// Method AddToPrintList
//   making list of all histograms that we want drawn,printed

  if (Debug())
    cout << " **** Now in StHistUtil::AddToPrintList  **** " << endl;

// Since I'm creating a new list, must delete it in the destructor!!
//make a new TList on heap(persistant); have already defined m_ListOfPrint in header file
   if (!m_ListOfPrint) m_ListOfPrint = new TList;

// the add method for TList requires a TObject input  (also can use TObjString)
// create TObjString on heap
   TObjString *HistNameObj = new TObjString(HistName);

// - check if it's already on the list - use FindObject method of TList
    TObject *lobj = 0;
    lobj = m_ListOfPrint->FindObject(HistName);

// now can use Add method of TList
    if (!lobj) {
       m_ListOfPrint->Add(HistNameObj);
       if (Debug())
         cout << " StHistUtil::AddToPrintList: " << HistName  <<endl;
    }
    else  cout << " StHistUtil::AddToPrintList: " << HistName << " already in list - not added" <<endl;
 
// return using a method of TList (inherits GetSize from TCollection)
 return m_ListOfPrint->GetSize();

}

//_____________________________________________________________________________

Int_t StHistUtil::RemoveFromLogYList(const Char_t *HistName){  
// Method RemoveFromLogYList
//   remove hist from  list  that we want drawn with LogY scale

  if (Debug())
    cout << " **** Now in StHistUtil::RemoveFromLogYList  **** " << endl;

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
        cout << " RemoveLogYList: " << HistName << " has been removed from list" <<endl;
    }
    else  cout << " RemoveLogYList: " << HistName << " not on list - not removing" <<endl;

  } 
// return using a method of TList (inherits GetSize from TCollection)
 return m_ListOfLogY->GetSize();
}


//_____________________________________________________________________________

Int_t StHistUtil::RemoveFromLogXList(const Char_t *HistName){  
// Method RemoveFromLogXList
//   remove hist from  list  that we want drawn with LogX scale

  if (Debug())
    cout << " **** Now in StHistUtil::RemoveFromLogXList  **** " << endl;

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
        cout << " RemoveLogXList: " << HistName << " has been removed from list" <<endl;
    }
    else  cout << " RemoveLogXList: " << HistName << " not on list - not removing" <<endl;

  } 
// return using a method of TList (inherits GetSize from TCollection)
 return m_ListOfLogX->GetSize();
}


//_____________________________________________________________________________

Int_t StHistUtil::RemoveFromPrintList(const Char_t *HistName){  
// Method RemoveFromPrintList
//   remove hist from  list  that we want drawn,printed

  if (Debug())
    cout << " **** Now in StHistUtil::RemoveFromPrintList  **** " << endl;

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
        cout << " RemovePrintList: " << HistName << " has been removed from list" <<endl;
    }
    else  cout << " RemovePrintList: " << HistName << " not on list - not removing" <<endl;

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
    cout << " **** Now in StHistUtil::SetDefaultLogYList  **** " << endl;


  TString type;

  if (!strcmp(dirName,"QA"))
    type = "Tab";
  if (!strcmp(dirName,"EventQA"))
    type = "StE";

  Char_t* sdefList[] = {
    #include "St_QA_Maker/QAhlist_logy.h"
  };

  Int_t lengofList = sizeof(sdefList)/4;
  Int_t numLog = 0;
  Int_t ilg = 0;
  for (ilg=0;ilg<lengofList;ilg++) {
    TString listString;
    if (strcmp(sdefList[ilg],"QaInnerSectorDeDx") &&
	strcmp(sdefList[ilg],"QaOuterSectorDeDx") &&
	strcmp(sdefList[ilg],"QaDedxAllSectors") &&
	strcmp(sdefList[ilg],"fcl_chargestepW") &&
	strcmp(sdefList[ilg],"fcl_chargestepE") &&
        strcmp(sdefList[ilg],"fcl_radialW") &&
	strcmp(sdefList[ilg],"fcl_radialE")) {
      for (Int_t k=0; k<numOfPosPrefixes; k++) {
        ((listString = type) += possiblePrefixes[k]) += sdefList[ilg];
        numLog = AddToLogYList(listString.Data());
      }
    } else {
      listString = sdefList[ilg];
      numLog = AddToLogYList(listString.Data());
    }
  }

  cout <<  " !!!  StHistUtil::SetDefaultLogYList, # histogram put in list " << numLog << endl;

}

//_____________________________________________________________________________
// Method SetDefaultLogXList
//    - create default list of histograms we want plotted in LogX scale

void StHistUtil::SetDefaultLogXList(Char_t *dirName)
{  
// Method SetDefaultLogXList
//    - create default list of histograms we want plotted in LogX scale

  if (Debug())
    cout << " **** Now in StHistUtil::SetDefaultLogXList  **** " << endl;

  TString type;

  if (!strcmp(dirName,"QA"))
    type = "Tab";
  if (!strcmp(dirName,"EventQA"))
    type = "StE";

  Char_t* sdefList[] = {
    #include "St_QA_Maker/QAhlist_logx.h"
  };

  Int_t lengofList = sizeof(sdefList)/4;
  Int_t numLog = 0;
  Int_t ilg = 0;
  for (ilg=0;ilg<lengofList;ilg++) {
    TString listString;
    if (strcmp(sdefList[ilg],"QaInnerSectorDeDx") &&
	strcmp(sdefList[ilg],"QaOuterSectorDeDx") &&
	strcmp(sdefList[ilg],"QaDedxAllSectors") &&
	strcmp(sdefList[ilg],"fcl_chargestepW") &&
	strcmp(sdefList[ilg],"fcl_chargestepE") &&
        strcmp(sdefList[ilg],"fcl_radialW") &&
	strcmp(sdefList[ilg],"fcl_radialE")) {
      for (Int_t k=0; k<numOfPosPrefixes; k++) {
        ((listString = type) += possiblePrefixes[k]) += sdefList[ilg];
        numLog = AddToLogXList(listString.Data());
      }
    } else {
      listString = sdefList[ilg];
      numLog = AddToLogXList(listString.Data());
    }
  }

  cout <<  " !!!  StHistUtil::SetDefaultLogXList, # histogram put in list " << numLog << endl;

}

//_____________________________________________________________________________
// Method SetDefaultPrintList
//    - create default list of histograms we want drawn,printed

void StHistUtil::SetDefaultPrintList(Char_t *dirName, Char_t *analType)
{  

  cout << " **** Now in StHistUtil::SetDefaultPrintList  **** " << endl;

  Char_t **sdefList=0;
  Int_t lengofList = 0;


// If not analysis Type is set, then don't setup a list
  if ((!strcmp(analType,"")) || (!strcmp(analType,"All")) ) {
    cout << " All histograms in directory will be printed/drawn, no list set" << endl;
    return;
  }

// Cosmic Data Table QA list .................................................
  if ((!strcmp(dirName,"QA")) && (!strcmp(analType,"Cosmic"))) {
    Char_t* sdefList1[] = {
      #include "St_QA_Maker/QAhlist_QA_Cosmic.h"
    };
    sdefList = sdefList1;
    lengofList = sizeof(sdefList1)/4;
  }

// Test Table QA list.........................................................
  if ((!strcmp(dirName,"QA")) && (!strcmp(analType,"TestQATable"))) {
    Char_t* sdefList2[] = {
      #include "St_QA_Maker/QAhlist_QA_TestQATable.h"
    };
    sdefList = sdefList2;
    lengofList = sizeof(sdefList2)/4;
  }

// FTPC Table QA list.........................................................
  if ((!strcmp(dirName,"QA")) && (!strcmp(analType,"Ftpc"))) {
    Char_t* sdefList3[] = {
      #include "St_QA_Maker/QAhlist_QA_Ftpc.h"
    };
    sdefList = sdefList3;
    lengofList = sizeof(sdefList3)/4;
  }

// FTPC Table QA list.........................................................
  if ((!strcmp(dirName,"FlowTag")) && (!strcmp(analType,"MDC3"))) {
    Char_t* sdefList4[] = {
      #include "St_QA_Maker/QAhlist_QA_MDC3.h"
    };
    sdefList = sdefList4;
    lengofList = sizeof(sdefList4)/4;
  }

// St_QA_Maker histograms without svt and ftpc histograms.....................
  if ((!strcmp(dirName,"QA")) && (!strcmp(analType,"year1"))) {
    Char_t* sdefList5[] = {
      #include "St_QA_Maker/QAhlist_QA_year1.h"
    };
    sdefList = sdefList5;
    lengofList = sizeof(sdefList5)/4;
  }

// St_QA_Maker histograms without the svt and ftpc histograms.................
  if ((!strcmp(dirName,"EventQA")) && (!strcmp(analType,"year1"))) {
    Char_t* sdefList6[] = {
      #include "St_QA_Maker/QAhlist_EventQA_year1.h"
    };
    sdefList = sdefList6;
    lengofList = sizeof(sdefList6)/4;
  }

// St_QA_Maker histograms for QA shift........................................
  if ((!strcmp(dirName,"QA")) && (!strcmp(analType,"qa_shift"))) {
    Char_t* sdefList7[] = {
      #include "St_QA_Maker/QAhlist_QA_qa_shift.h"
    };
    sdefList = sdefList7;
    lengofList = sizeof(sdefList7)/4;
  }

// St_QA_Maker histograms for QA shift........................................
  if ((!strcmp(dirName,"EventQA")) && (!strcmp(analType,"qa_shift"))) {
    Char_t* sdefList8[] = {
      #include "St_QA_Maker/QAhlist_EventQA_qa_shift.h"
    };
    sdefList = sdefList8;
    lengofList = sizeof(sdefList8)/4;
  }

  Int_t numPrt = 0;
  Int_t ilg = 0;
  for (ilg=0;ilg<lengofList;ilg++) {
    numPrt = AddToPrintList(sdefList[ilg]);
    if (Debug())
      cout <<  " !!! adding histogram " << sdefList[ilg] << " to print list "  << endl ;
  }
  
  cout <<  " !!!  StHistUtil::SetDefaultPrintList, # histogram put in list " << numPrt << endl;

}

//_____________________________________________________________________________

// Method Overlay1D
//    - takes two TH1F histograms and overlays them

Int_t StHistUtil::Overlay1D(Char_t *dirName,Char_t *inHist1,
			    Char_t *inHist2) {

  cout << " **** Now in StHistUtil::Overlay1D **** " << endl;

  Int_t n1dHists = 0;

// get the TList pointer to the histograms
  TList  *dirList = 0;
  dirList = FindHists(dirName);

// check that directory exists
  if (!dirList)
    return kStErr;

  cout << "Histogram directory exists -> Find and overlay histograms" << endl;
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
	cout << " Found Histogram: Type '" << obj->ClassName() << "', Name '"
	     << obj->GetName() << "', Title '" << obj->GetTitle() << "'"
	     << endl;

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
	  cout << " ERROR: histogram not of type TH1F !!!" << endl;
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

  cout << " **** Now in StHistUtil::Overlay2D **** " << endl;

  Int_t n2dHists = 0;

// get the TList pointer to the histograms
  TList  *dirList = 0;
  dirList = FindHists(dirName);

// check that directory exists
  if (!dirList)
    return kStErr;

  cout << "Histogram directory exists -> Find and overlay histograms" << endl;
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
	cout << " Found Histogram: Type '" << obj->ClassName() << "', Name '"
	     << obj->GetName() << "', Title '" << obj->GetTitle() << "'"
	     << endl;

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
	  cout << " ERROR: histogram is not of type TH2F !!!" << endl;
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
