// $Id: StHistUtil.cxx,v 1.3 2000/01/27 16:18:50 kathy Exp $
// $Log: StHistUtil.cxx,v $
// Revision 1.3  2000/01/27 16:18:50  kathy
// adding more hist names in setdefaultlogx list to make tom trainor happier
//
// Revision 1.2  2000/01/26 19:29:26  kathy
// add methods SetDefaultLogXList,AddToLogXList,ExamineLogXList,RemoveFromLogXList - requested by T.Trainor - impact param hists are now draw with LogX scale
//
// Revision 1.1  2000/01/18 16:42:40  kathy
// move StHistUtil class from St_QA_Maker directory and put into StAnalysisUtilities
//
// Revision 1.27  2000/01/13 16:22:32  kathy
// changed DrawHist method so that it now correctly prints from a list (if printlist is available)
//
// Revision 1.26  2000/01/12 23:17:49  kathy
// changes so it can print from a list
//
// Revision 1.24  2000/01/12 16:49:04  kathy
// add new methods so that one can set a list which will be used to print,draw a subset of the histograms corresponding to a given maker; new methods are SetDefaultPrintList,AddToPrintList,RemoveFromPrintList,ExaminePrintList; can't test it yet because seems can't find directory of histograms in DEV anymore and there are conflicts in NEW; updates to DrawHist method to use this new list are not done yet
//
// Revision 1.23  1999/12/29 17:52:30  kathy
// changes to hist limits and list of logY scales
//
// Revision 1.22  1999/12/22 17:16:43  kathy
// fix so that it doesn't try to print/draw in logY scale if the max entries in any bins is 0
//
// Revision 1.21  1999/12/22 16:32:28  kathy
// check if histogram has entries before setting logY scale
//
// Revision 1.20  1999/12/21 15:50:32  kathy
// got page numbers to print out properlycvs -n update - thanks Jeff & Valery!
//
// Revision 1.19  1999/12/20 17:31:25  kathy
// updates to write page numbers on output histogram ps file
//
// Revision 1.18  1999/12/17 22:11:32  kathy
// add psi vs phi hist, change limits
//
// Revision 1.17  1999/12/16 23:12:03  kathy
// fixed list of default histograms, had wrong title and added a few for tables and stevent; rescaled some histograms and fixed titles in booking
//
// Revision 1.16  1999/12/16 03:56:20  lansdell
// mirrored Kathy's changes in St_QA_Maker.cxx: separated tpc and tpc+svt histograms for global tracks using StEvent; added r0,phi0,z0,curvature histograms for global tracks in the tpc
//
// Revision 1.15  1999/12/15 20:32:17  kathy
// separated the tpc and tpc+svt histograms for globtrk table; had to book and fill new histograms, add histograms to default logy list AND had to change what values of iflag I cut on for filling each different type of track in makehistglob method
//
// Revision 1.14  1999/12/15 18:31:05  kathy
// added 4 new histogram to globtrk for tpc - r0,phi0,z0,curvature; also put 3 of these in default logY list; also changed scale on iflag hist. for globtrk & primtrk
//
// Revision 1.13  1999/12/13 20:08:37  lansdell
// added pt vs eta in ftpc histogram to match table QA changes; updated logy scale histograms
//
// Revision 1.12  1999/12/10 17:38:24  kathy
// now reprint canvas on each page of postscript output file; also changed some histogram limits
//
// Revision 1.11  1999/12/08 22:58:16  kathy
// changed histogram limits and made names smaller
//
// Revision 1.10  1999/12/07 23:14:18  kathy
// fix primary vtx histograms for dst tables; split apart the ftpc and tpc in the dedx histograms
//
// Revision 1.9  1999/12/07 21:54:15  kathy
// added date and time to DrawHist method in StHistUtil class so that this is printed at bottom right of histogram output
//
// Revision 1.7  1999/12/06 22:25:05  kathy
// split apart the tpc and ftpc (east & west) histograms for the globtrk table; had to add characters to end of each histogram pointer to differentiate the different ones; updated the default list of hist to be plotted with logy scale
//
// Revision 1.6  1999/11/24 14:55:32  lansdell
// log scale fixed for QA histograms (gene)
//
// Revision 1.5  1999/11/22 22:46:41  lansdell
// update to identify histogram method used (StEvent or DST tables) by Gene; StEventQAMaker code partially completed (run bfcread_dst_EventQAhist.C)
//
// Revision 1.4  1999/11/18 18:18:17  kathy
// adding default logY hist list for EventQA histogram set
//
// Revision 1.3  1999/11/05 22:26:01  kathy
// now allow setting of global title from a method
//
// Revision 1.2  1999/11/05 21:51:58  kathy
// write title at top of each page of histograms in DrawHists method
//
// Revision 1.1  1999/09/20 20:12:15  kathy
// moved the histogram utility methods out of St_QA_Maker and into StHistUtil because they can really be used by any Maker and associated histograms
//

///////////////////////////////////////////////////////////////////////////////
// Histogram Utility methods for use with star makers and bfc output
///////////////////////////////////////////////////////////////////////////////
                                                                          

#include <iostream.h>
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
#include "TDatime.h"

#include "StChain.h"
#include "St_DataSetIter.h"
#include "StMaker.h"

#include "StHistUtil.h"

ClassImp(StHistUtil)
  
//_____________________________________________________________________________

// Constructor

StHistUtil::StHistUtil(){

  m_ListOfLogY = 0;
  m_ListOfLogX = 0;
  m_ListOfPrint = 0;
  m_HistCanvas = 0;


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
}
//_____________________________________________________________________________

Int_t StHistUtil::DrawHists(Char_t *dirName) 
{
// Method DrawHists -->
// Plot the selected  histograms and generate the postscript file as well 
  
  cout << " **** Now in StHistUtil::DrawHists  **** " << endl;


// set output ps file name
  TPostScript *psf = 0;
  const Char_t *psfileName = m_PsFileName.Data();
  if (!m_PsFileName.IsNull()) psf = new TPostScript((char *)psfileName);  
  
// set global title which goes at top of each page of histograms
  const Char_t *gtitle = m_GlobalTitle.Data();

//set Style of Plots
  const Int_t numPads = m_PadColumns*m_PadRows;  
// SetPaperSize wants width & height in cm: A4 is 20,26 & US is 20,24
  gStyle->SetPaperSize(m_PaperWidth,m_PaperHeight); 

  gStyle->SetOptStat(111111);
//

//setup canvas
  SafeDelete(m_HistCanvas);

// TCanvas wants width & height in pixels (712 x 950 corresponds to A4 paper)
//                                        (600 x 780                US      )
  //  TCanvas *HistCanvas = new TCanvas("CanvasName","Canvas Title",30*m_PaperWidth,30*m_PaperHeight);
    TCanvas *HistCanvas = new TCanvas("CanvasName"," STAR Maker Histogram Canvas",600,780);

  //  HistCanvas->SetFillColor(19);
  // but now we have paper size in cm
  //  HistCanvas->Range(0,0,20,24);
  //  Can set range to something which makes it equivalent to canvas but is 0,1 by default
  //HistCanvas->SetBorderSize(2);  

// Range for all float numbers used by ROOT methods is now 0,1 by default!

// order of PaveLabel is x1,y1,x2,y2 - fraction of pad (which is the canvas now)
// option = br means the shadow of box starts at bottom right
//    myself, I'd like to get rid of the stupid shadow that somebody decided I need!

// write title at top of canvas - first page
   TPaveLabel *Ltitle = new TPaveLabel(0.1,0.96,0.9,1.0,(char *)gtitle,"br");
   Ltitle->SetFillColor(18);
   Ltitle->SetTextFont(32);
   Ltitle->SetTextSize(0.5);
   // Ltitle->SetTextColor(49);
   Ltitle->Draw();

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
//    cout << " Ipage " << Ipagenum << " Cpage " << Ctmp << endl;
  TPaveLabel *Lpage = new TPaveLabel(0.1,0.01,0.2,0.03,Ctmp,"br");
  Lpage->SetTextSize(0.6);
  Lpage->Draw();

// Make 1 big pad on the canvas - make it a little bit inside the  canvas 
//    - must cd to get to this pad! 
// order is x1 y1 x2 y2 
  TPad *graphPad = new TPad("PadName","Pad Title",0.0,0.05,1.00,0.95);
  graphPad->Draw();
  graphPad->cd();

// Now divide the canvas (should work on the last pad created) 
  graphPad->Divide(m_PadColumns,m_PadRows);

  if (psf) psf->NewPage();
  const Char_t *firstHistName = m_FirstHistName.Data();
  const Char_t *lastHistName  = m_LastHistName.Data();

  //  cout << " **** Now finding hist **** " << endl;

// Now find the histograms
// get the TList pointer to the histograms:
  TList  *dirList = 0;
  dirList = FindHists(dirName);
 
  Int_t padCount = 0;
  
// Create an iterator called nextHist - use TIter constructor
  TIter nextHist(dirList);
  Int_t histCounter = 0;
  Int_t histReadCounter = 0;
  Bool_t started = kFALSE;

//NOTE!! the () used by nextHist below is an overloaded operator 
//     in TIter that returns a TObject* 

  TObject *obj = 0;
  Int_t chkdim=0;
  while ((obj = nextHist())) {
//   cout << " **** Now in StHistUtil::DrawHists - in loop: " << endl;
//   cout << "               name = " << obj->GetName() << endl;


    if (obj->InheritsFrom("TH1")) { 
//    cout << " **** Now in StHistUtil::DrawHists - obj->InheritsFrom(TH1)  **** " << endl;
      histReadCounter++;
      printf(" %d. Reading ... %s::%s; Title=\"%s\"\n",histReadCounter,obj->ClassName(),obj->GetName(), obj->GetTitle());
      if (! started && (strcmp("*",firstHistName)==0 || strcmp(obj->GetName(),firstHistName)==0 ))  started = kTRUE;

// Here is the actual loop over histograms !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      if (started) {
	if (strcmp(obj->GetName(),lastHistName)==0) started = kFALSE;
	histCounter++;

// Switch to a new page.....................................................
	if (padCount == numPads) {
	  if (psf) psf->NewPage();

// update the page number
          Ipagenum++;
          ostrstream Cpagenumt(Ctmp,10);
          Cpagenumt << Ipagenum << ends;
//          cout << " Ipage " << Ipagenum << " Cpage " << Ctmp << endl;
          Lpage->SetLabel(Ctmp);

// must redraw the histcanvas for each new page of postscript file!
	  //    HistCanvas->cd();
        HistCanvas->Modified();
        HistCanvas->Update();
	  padCount=0;
	}
//...........................................................................


// If there's no print list, then print all histograms in directory
// If there is a print list, then only print if hist name is on list
	//cout << " print list pointer = " << m_ListOfPrint << endl;
        //cout << "     size of list   = " << m_ListOfPrint->GetSize() << endl;
 
	if (!m_ListOfPrint || (m_ListOfPrint && m_ListOfPrint->FindObject(obj->GetName()) )){

// this histogram will actually be printed/drawn!!
        printf("  -   %d. Drawing ... %s::%s; Title=\"%s\"\n",
	  histCounter,obj->ClassName(),obj->GetName(), obj->GetTitle());

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
	if (m_ListOfLogY && m_ListOfLogY->FindObject(obj->GetName()) && ((TH1 *)obj)->GetEntries()
            && ((TH1 *)obj)->GetMaximum() ){
	  gPad->SetLogy(1);
          cout << "             -- Will draw in logY scale: " << obj->GetName() <<endl;
	 }


// Set logX scale on if: there is a loglist, if the hist name is on the list, if it has entries
//    and if the max entries in all bins is > 0
	if (m_ListOfLogX && m_ListOfLogX->FindObject(obj->GetName()) && ((TH1 *)obj)->GetEntries()
            && ((TH1 *)obj)->GetMaximum() ){
	  gPad->SetLogx(1);
          cout << "             -- Will draw in logX scale: " << obj->GetName() <<endl;
	 }


// check dimension of histogram
          chkdim = ((TH1 *)obj)->GetDimension();
	  //	  cout << " name " << obj->GetName() << " dimension " << chkdim << endl;

// actually draw,print
          if (chkdim == 2) obj->Draw("box");
          else 
              obj->Draw();   
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
  }
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

    hist->ls(9);

// must look in dirNameHist 
// use TString to append "Hist" to the dirName
// += is overloaded operator of TString

    TString hBN(dirName);
    hBN += "Hist";
    
//find particular branch
    St_DataSet *QAH = hist->Find(hBN.Data());

// or can create iterator and look over all branches

//now get the list of histograms
    dList = (TList *)QAH->GetObject();

  }

// now have we found them?
  if (dList){ 
      cout << " FindHists - found hist. in histBranch, with name:  " 
	   << dirName <<  endl;
     }

  }

  cout << " End: FindHists, dList pointer = " << dList << endl;
  
 return dList;
}
//_____________________________________________________________________________


Int_t StHistUtil::ListHists(Char_t *dirName) 
{  
// Method ListHists -->
// List of all histograms

  cout << " **** Now in StHistUtil::ListHists **** " << endl;

// get the TList pointer to the histograms:
  TList  *dirList = 0;
  dirList = FindHists(dirName);


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


Int_t StHistUtil::ExamineLogYList() 
{  
// Method ExamineLogYList
// List of all histograms that will be drawn with logy scale

  cout << " **** Now in StHistUtil::ExamineLogYList **** " << endl;

// m_ListOfLogY -  is a list of log plots
// construct a TObject
  TObject *obj = 0;
// construct a TIter ==>  () is an overloaded operator in TIter
  TIter nextObj(m_ListOfLogY);
  Int_t LogYCount = 0;

// use = here instead of ==, because we are setting obj equal to nextObj and then seeing if it's T or F
  while ((obj = nextObj())) {

    cout << " StHistUtil::ExamineLogYList has hist " <<  obj->GetName() << endl;
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

  cout << " **** Now in StHistUtil::ExamineLogXList **** " << endl;

// m_ListOfLogX -  is a list of log plots
// construct a TObject
  TObject *obj = 0;
// construct a TIter ==>  () is an overloaded operator in TIter
  TIter nextObj(m_ListOfLogX);
  Int_t LogXCount = 0;

// use = here instead of ==, because we are setting obj equal to nextObj and then seeing if it's T or F
  while ((obj = nextObj())) {

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

//  cout << " **** Now in StHistUtil::AddToLogYList  **** " << endl;

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
//       cout << " StHistUtil::AddToLogYList: " << HistName  <<endl;
    }
    else  cout << " StHistUtil::AddToLogYList: " << HistName << " already in list - not added" <<endl;
 
// return using a method of TList (inherits GetSize from TCollection)
 return m_ListOfLogY->GetSize();
}


//_____________________________________________________________________________


Int_t StHistUtil::AddToLogXList(const Char_t *HistName){  
// Method AddToLogXList
//   making list of all histograms that we want drawn with LogX scale

//  cout << " **** Now in StHistUtil::AddToLogXList  **** " << endl;

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
//       cout << " StHistUtil::AddToLogXList: " << HistName  <<endl;
    }
    else  cout << " StHistUtil::AddToLogXList: " << HistName << " already in list - not added" <<endl;
 
// return using a method of TList (inherits GetSize from TCollection)
 return m_ListOfLogX->GetSize();
}


//_____________________________________________________________________________


Int_t StHistUtil::AddToPrintList(const Char_t *HistName){  

// Method AddToPrintList
//   making list of all histograms that we want drawn,printed

//  cout << " **** Now in StHistUtil::AddToPrintList  **** " << endl;

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
//       cout << " StHistUtil::AddToPrintList: " << HistName  <<endl;
    }
    else  cout << " StHistUtil::AddToPrintList: " << HistName << " already in list - not added" <<endl;
 
// return using a method of TList (inherits GetSize from TCollection)
 return m_ListOfPrint->GetSize();

}

//_____________________________________________________________________________

Int_t StHistUtil::RemoveFromLogYList(const Char_t *HistName){  
// Method RemoveFromLogYList
//   remove hist from  list  that we want drawn with LogY scale

//  cout << " **** Now in StHistUtil::RemoveFromLogYList  **** " << endl;

// check if list exists:
  if (m_ListOfLogY) {
    
// the remove method for TList requires a TObject input  
// - check if it's  on the list - use FindObject method of TList
    TObject *lobj = 0;
    lobj = m_ListOfLogY->FindObject(HistName);
// now can use Remove method of TList
    if (lobj) {
      m_ListOfLogY->Remove(lobj);
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

//  cout << " **** Now in StHistUtil::RemoveFromLogXList  **** " << endl;

// check if list exists:
  if (m_ListOfLogX) {
    
// the remove method for TList requires a TObject input  
// - check if it's  on the list - use FindObject method of TList
    TObject *lobj = 0;
    lobj = m_ListOfLogX->FindObject(HistName);
// now can use Remove method of TList
    if (lobj) {
      m_ListOfLogX->Remove(lobj);
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

//  cout << " **** Now in StHistUtil::RemoveFromPrintList  **** " << endl;

// check if list exists:
  if (m_ListOfPrint) {
    
// the remove method for TList requires a TObject input  
// - check if it's  on the list - use FindObject method of TList
    TObject *lobj = 0;
    lobj = m_ListOfPrint->FindObject(HistName);
// now can use Remove method of TList
    if (lobj) {
      m_ListOfPrint->Remove(lobj);
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

  cout << " **** Now in StHistUtil::SetDefaultLogYList  **** " << endl;


  Char_t **sdefList=0;
  Int_t lengofList = 0;

  if (strcmp(dirName,"QA")==0) {
   Char_t* sdefList1[] = {
 "TabQaGtrkDetId",
 "TabQaGtrkFlag",
 "TabQaGtrkXf0",
 "TabQaGtrkYf0",
 "TabQaGtrkZf0",
 "TabQaGtrkImpactT",
 "TabQaGtrkNPntT",
 "TabQaGtrkNPntMaxT",
 "TabQaGtrkNPntFitT",
 "TabQaGtrkPtT",
 "TabQaGtrkPT",
 "TabQaGtrkR0T",
 "TabQaGtrkZ0T",
 "TabQaGtrkCurvT",
 "TabQaGtrkXfT",
 "TabQaGtrkYfT",
 "TabQaGtrkZfT",
 "TabQaGtrkRT",
 "TabQaGtrkRnfT",
 "TabQaGtrkRnmT",
 "TabQaGtrkTanlT",
 "TabQaGtrkThetaT",
 "TabQaGtrkEtaT",
 "TabQaGtrkLengthT",
 "TabQaGtrkXf0TS",
 "TabQaGtrkYf0TS",
 "TabQaGtrkZf0TS",
 "TabQaGtrkImpactTS",
 "TabQaGtrkNPntTS",
 "TabQaGtrkNPntMaxTS",
 "TabQaGtrkNPntFitTS",
 "TabQaGtrkPtTS",
 "TabQaGtrkPTS",
 "TabQaGtrkR0TS",
 "TabQaGtrkZ0TS",
 "TabQaGtrkCurvTS",
 "TabQaGtrkXfTS",
 "TabQaGtrkYfTS",
 "TabQaGtrkZfTS",
 "TabQaGtrkRTS",
 "TabQaGtrkRnfTS",
 "TabQaGtrkRnmTS",
 "TabQaGtrkTanlTS",
 "TabQaGtrkThetaTS",
 "TabQaGtrkEtaTS",
 "TabQaGtrkLengthTS",
 "TabQaGtrkNPntFE",
 "TabQaGtrkNPntMaxFE",
 "TabQaGtrkNPntFitFE",
 "TabQaGtrkPtFE",
 "TabQaGtrkPFE",
 "TabQaGtrkXfFE",
 "TabQaGtrkYfFE",
 "TabQaGtrkZfFE",
 "TabQaGtrkRFE",
 "TabQaGtrkRnfFE",
 "TabQaGtrkRnmFE",
 "TabQaGtrkTanlFE",
 "TabQaGtrkThetaFE",
 "TabQaGtrkEtaFE",
 "TabQaGtrkLengthFE",
 "TabQaGtrkNPntFW",
 "TabQaGtrkNPntMaxFW",
 "TabQaGtrkNPntFitFW",
 "TabQaGtrkPtFW",
 "TabQaGtrkPFW",
 "TabQaGtrkXfFW",
 "TabQaGtrkYfFW",
 "TabQaGtrkZfFW",
 "TabQaGtrkRFW",
 "TabQaGtrkRnfFW",
 "TabQaGtrkRnmFW",
 "TabQaGtrkTanlFW",
 "TabQaGtrkThetaFW",
 "TabQaGtrkEtaFW",
 "TabQaGtrkLengthFW",
 "TabQaPtrkDetId",
 "TabQaPtrkFlag",
 "TabQaPtrkNPnt",
 "TabQaPtrkNPntMax",
 "TabQaPtrkNPntFit",
 "TabQaPtrkPt",
 "TabQaPtrkP",
 "TabQaPtrkXf0",
 "TabQaPtrkXf",
 "TabQaPtrkYf0",
 "TabQaPtrkYf",
 "TabQaPtrkZf0",
 "TabQaPtrkZf",
 "TabQaPtrkR",
 "TabQaPtrkRnf",
 "TabQaPtrkTanl ",
 "TabQaPtrkTheta ",
 "TabQaPtrkEta",
 "TabQaPtrkLength",
 "TabQaPtrkImpact",
 "TabQaPtrkNdof"
 "TabQaDedxNum",
 "TabQaDedxDedx0T", 
 "TabQaDedxDedx1T",
 "TabQaDedxDedx0FE", 
 "TabQaDedxDedx1FE",
 "TabQaDedxDedx0FW", 
 "TabQaDedxDedx1FW",
 "TabQaEvgenPt",
 "TabQaEvgenVtxX",
 "TabQaEvgenVtxY",
 "TabQaEvgenVtxZ",
 "TabQaVtxX",
 "TabQaVtxY",
 "TabQaVtxZ",
 "TabQaVtxChisq"
  };
  sdefList = sdefList1;
  lengofList = sizeof(sdefList1)/4;  
  }

  if (strcmp(dirName,"EventQA")==0) {
   Char_t* sdefList2[] = {
 "StEQaGtrkDetId",
 "StEQaGtrkFlag",
 "StEQaGtrkXf0",
 "StEQaGtrkYf0",
 "StEQaGtrkZf0",
 "StEQaGtrkImpactT",
 "StEQaGtrkNPntT",
 "StEQaGtrkNPntMaxT",
 "StEQaGtrkNPntFitT",
 "StEQaGtrkPtT",
 "StEQaGtrkPT",
 "StEQaGtrkR0T",
 "StEQaGtrkZ0T",
 "StEQaGtrkCurvT",
 "StEQaGtrkXfT",
 "StEQaGtrkYfT",
 "StEQaGtrkZfT",
 "StEQaGtrkRT",
 "StEQaGtrkRnfT",
 "StEQaGtrkRnmT",
 "StEQaGtrkTanlT",
 "StEQaGtrkThetaT",
 "StEQaGtrkEtaT",
 "StEQaGtrkLengthT",
 "StEQaGtrkXf0TS",
 "StEQaGtrkYf0TS",
 "StEQaGtrkZf0TS",
 "StEQaGtrkImpactTS",
 "StEQaGtrkNPntTS",
 "StEQaGtrkNPntMaxTS",
 "StEQaGtrkNPntFitTS",
 "StEQaGtrkPtTS",
 "StEQaGtrkPTS",
 "StEQaGtrkR0TS",
 "StEQaGtrkZ0TS",
 "StEQaGtrkCurvTS",
 "StEQaGtrkXfTS",
 "StEQaGtrkYfTS",
 "StEQaGtrkZfTS",
 "StEQaGtrkRTS",
 "StEQaGtrkRnfTS",
 "StEQaGtrkRnmTS",
 "StEQaGtrkTanlTS",
 "StEQaGtrkThetaTS",
 "StEQaGtrkEtaTS",
 "StEQaGtrkLengthTS",
 "StEQaGtrkNPntFE",
 "StEQaGtrkNPntMaxFE",
 "StEQaGtrkNPntFitFE",
 "StEQaGtrkPtFE",
 "StEQaGtrkPFE",
 "StEQaGtrkXfFE",
 "StEQaGtrkYfFE",
 "StEQaGtrkZfFE",
 "StEQaGtrkRFE",
 "StEQaGtrkRnfFE",
 "StEQaGtrkRnmFE",
 "StEQaGtrkTanlFE",
 "StEQaGtrkThetaFE",
 "StEQaGtrkEtaFE",
 "StEQaGtrkLengthFE",
 "StEQaGtrkNPntFW",
 "StEQaGtrkNPntMaxFW",
 "StEQaGtrkNPntFitFW",
 "StEQaGtrkPtFW",
 "StEQaGtrkPFW",
 "StEQaGtrkXfFW",
 "StEQaGtrkYfFW",
 "StEQaGtrkZfFW",
 "StEQaGtrkRFW",
 "StEQaGtrkRnfFW",
 "StEQaGtrkRnmFW",
 "StEQaGtrkTanlFW",
 "StEQaGtrkThetaFW",
 "StEQaGtrkEtaFW",
 "StEQaGtrkLengthFW",
 "StEQaPtrkDetId",
 "StEQaPtrkFlag",
 "StEQaPtrkNPnt",
 "StEQaPtrkNPntMax",
 "StEQaPtrkNPntFit",
 "StEQaPtrkPt",
 "StEQaPtrkP",
 "StEQaPtrkXf0",
 "StEQaPtrkXf",
 "StEQaPtrkYf0",
 "StEQaPtrkYf",
 "StEQaPtrkZf0",
 "StEQaPtrkZf",
 "StEQaPtrkR",
 "StEQaPtrkRnf",
 "StEQaPtrkTanl ",
 "StEQaPtrkTheta ",
 "StEQaPtrkEta",
 "StEQaPtrkLength",
 "StEQaPtrkImpact",
 "StEQaPtrkNdof",
 "StEQaDedxNum",
 "StEQaDedxDedx0T", 
 "StEQaDedxDedx1T",
 "StEQaDedxDedx0FE", 
 "StEQaDedxDedx1FE",
 "StEQaDedxDedx0FW", 
 "StEQaDedxDedx1FW",
 "StEQaEvgenPt",
 "StEQaEvgenVtxX",
 "StEQaEvgenVtxY",
 "StEQaEvgenVtxZ",
 "StEQaVtxX",
 "StEQaVtxY",
 "StEQaVtxZ",
 "StEQaVtxChisq"
   };
  sdefList = sdefList2;
  lengofList = sizeof(sdefList2)/4;  
  }

  //  else 
  //  { cout << " StHistUtil::SetDefaultLogYList - no hist set in def logy list " << endl; } 
  //   cout <<  " !! HERE I AM1 " << lengofList << endl ;

  Int_t numLog = 0;

  if (lengofList) {
    Int_t ilg = 0;
    for (ilg=0;ilg<lengofList;ilg++) {
     numLog = AddToLogYList(sdefList[ilg]);
     //     cout <<  " !!! adding histogram " << sdefList[ilg] << " to LogY list "  << endl ;
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

  cout << " **** Now in StHistUtil::SetDefaultLogXList  **** " << endl;


  Char_t **sdefList=0;
  Int_t lengofList = 0;

  if (strcmp(dirName,"QA")==0) {
   Char_t* sdefList1[] = {
    "TabQaGtrkImpactT",
    "TabQaGtrkImpactTS",
    "TabQaGtrkCurvT",
    "TabQaGtrkCurvTS",
    "TabQaPtrkImpact"
   };
  sdefList = sdefList1;
  lengofList = sizeof(sdefList1)/4;  
  }

  if (strcmp(dirName,"EventQA")==0) {
   Char_t* sdefList2[] = {
    "StEQaGtrkImpactT",
    "StEQaGtrkImpactTS",
    "TabQaGtrkCurvT",
    "TabQaGtrkCurvTS",
    "StEQaPtrkImpact"
   };
  sdefList = sdefList2;
  lengofList = sizeof(sdefList2)/4;  
  }

  //  else 
  //  { cout << " StHistUtil::SetDefaultLogXList - no hist set in def logX list " << endl; } 
  //   cout <<  " !! HERE I AM1 " << lengofList << endl ;

  Int_t numLog = 0;

  if (lengofList) {
    Int_t ilg = 0;
    for (ilg=0;ilg<lengofList;ilg++) {
     numLog = AddToLogXList(sdefList[ilg]);
     //     cout <<  " !!! adding histogram " << sdefList[ilg] << " to LogX list "  << endl ;
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
  if ( strcmp(analType,"")==0 || strcmp(analType,"All")==0 ) {
      cout << " All histograms in directory will be printed/drawn, no list set" << endl;
      return;
  }

// Cosmic Data Table QA list .............................................
  if (strcmp(dirName,"QA")==0 && strcmp(analType,"Cosmic")==0) {
   Char_t* sdefList1[] = {
 "TabQaEvsumTrkTot",
 "TabQaEvsumPlusMinusTrk",
 "TabQaEvsumMeanPt",
 "TabQaGtrkGood",
 "TabQaGtrkNPntT",
 "TabQaGtrkNPntMaxT",
 "TabQaGtrkNPntFitT",
 "TabQaGtrkRnmT",
 "TabQaGtrkChrgT",
 "TabQaGtrkR0T",
 "TabQaGtrkPhi0T",
 "TabQaGtrkZ0T",
 "TabQaGtrkCurvT",
 "TabQaGtrkXfT",
 "TabQaGtrkXf0",
 "TabQaGtrkYfT",
 "TabQaGtrkYf0",
 "TabQaGtrkZfT",
 "TabQaGtrkZf0",
 "TabQaGtrkRT",
 "TabQaGtrkLengthT",
 "TabQaGtrkPsiT",
 "TabQaGtrkTanlT",
 "TabQaGtrkThetaT",
 "TabQaGtrkPtT",
 "TabQaGtrkPT",
 "TabQaGtrkChisq0T",
 "TabQaGtrkChisq1T",
 "TabQaGtrkXfYfT",
 "TabQaGtrkTanlzf",
 "TabQaGtrkPVsTrkLength",
 "TabQaGtrkNPntLengthT",
 "TabQaGtrkChi0MomT",
 "TabQaGtrkChi1MomT",
 "TabQaGtrkChi0TanlT",
 "TabQaGtrkChi1TanlT",
 "TabQaGtrkChi0zfT",
 "TabQaGtrkChi1zfT",
 "TabQaGtrkPsiPhiT"
   };
  sdefList = sdefList1;
  lengofList = sizeof(sdefList1)/4;  
  }

// Test Table QA list.........................................................
  if (strcmp(dirName,"QA")==0 && strcmp(analType,"TestQATable")==0) {
   Char_t* sdefList2[] = {
     "TabQaVtxX",
     "TabQaVtxY"
   };
  sdefList = sdefList2;
  lengofList = sizeof(sdefList2)/4;  
  }

// FTPC Table QA list.........................................................
  if (strcmp(dirName,"QA")==0 && strcmp(analType,"Ftpc")==0) {
   Char_t* sdefList3[] = {
    "TabQaGtrkNPntFE",
    "TabQaGtrkNPntFW"
   };
  sdefList = sdefList3;
  lengofList = sizeof(sdefList3)/4;  
  }


  Int_t numPrt = 0;

  if (lengofList) {
    Int_t ilg = 0;
    for (ilg=0;ilg<lengofList;ilg++) {
     numPrt = AddToPrintList(sdefList[ilg]);
     //     cout <<  " !!! adding histogram " << sdefList[ilg] << " to print list "  << endl ;
    }
  }
  
  cout <<  " !!!  StHistUtil::SetDefaultPrintList, # histogram put in list " << numPrt << endl;

}

//_____________________________________________________________________________











