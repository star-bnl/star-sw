// $Id: StHistUtil.cxx,v 1.3 1999/11/05 22:26:01 kathy Exp $
// $Log: StHistUtil.cxx,v $
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

#include "PhysicalConstants.h"
#include "TStyle.h"
#include "TCanvas.h"
#include "TObjString.h"
#include "TPostScript.h"
#include "TMath.h"
#include "TString.h"
#include "TPaveLabel.h"

#include "StChain.h"
#include "St_DataSetIter.h"
#include "StMaker.h"

#include "StHistUtil.h"

ClassImp(StHistUtil)
  
//_____________________________________________________________________________

// Constructor

StHistUtil::StHistUtil(){

  m_ListOfLog = 0;
  m_HistCanvas = 0;


}
//_____________________________________________________________________________

// Destructor

StHistUtil::~StHistUtil(){
  SafeDelete(m_HistCanvas);
  if (m_ListOfLog) {
    m_ListOfLog->Delete();
    SafeDelete(m_ListOfLog);
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
//                                        (600 x 720                US      )
  TCanvas *HistCanvas = new TCanvas("CanvasName","Canvas Title",30*m_PaperWidth,30*m_PaperHeight);

//  HistCanvas->SetFillColor(19);
// but now we have paper size in cm
  //  HistCanvas->Range(0,0,20,24);
  //  Can set range to something which makes it equivalent to canvas but is 0,1 by default
  //HistCanvas->SetBorderSize(2);  

// Range for all float numbers used by ROOT methods is now 0,1 by default!

// write title at top of canvas
// order of PaveLabel is x1,y1,x2,y2 - fraction of pad (which is the canvas now)
   TPaveLabel *title = new TPaveLabel(0.1,0.96,0.9,0.99,(char *)gtitle,"br");
   title->SetFillColor(18);
   title->SetTextFont(32);
   title->SetTextSize(0.6);
   // title->SetTextColor(49);
   title->Draw();

// Make 1 big pad on the canvas - make it a little shorter than the canvas 
//    - must cd to get to this pad! 
  TPad *graphPad = new TPad("PadName","Pad Title",0.01,0.05,0.95,0.95);
  graphPad->Draw();
  graphPad->cd();

// Now divide the canvas (should work on the last pad created) 
  graphPad->Divide(m_PadColumns,m_PadRows);

  if (psf) psf->NewPage();
  const Char_t *firstHistName = m_FirstHistName.Data();
  const Char_t *lastHistName  = m_LastHistName.Data();

  cout << " **** Now finding hist **** " << endl;

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
  while (obj = nextHist()) {
   cout << " **** Now in StHistUtil::DrawHists - in loop: " << endl;
   cout << "               name = " << obj->GetName() << endl;


    if (obj->InheritsFrom("TH1")) { 
//    cout << " **** Now in StHistUtil::DrawHists - obj->InheritsFrom(TH1)  **** " << endl;
      histReadCounter++;
      printf(" %d. Reading ... %s::%s; Title=\"%s\"\n",histReadCounter,obj->ClassName(),obj->GetName(), obj->GetTitle());
      if (! started && (strcmp("*",firstHistName)==0 || strcmp(obj->GetName(),firstHistName)==0 ))  started = kTRUE;
      if (started) {
	if (strcmp(obj->GetName(),lastHistName)==0) started = kFALSE;
	histCounter++;
	printf("  -   %d. Drawing ... %s::%s; Title=\"%s\"\n",histCounter,obj->ClassName(),obj->GetName(), obj->GetTitle());
	if (padCount == numPads) {
	  if (psf) psf->NewPage();
	  padCount=0;
	}
	graphPad->cd(++padCount);
          gPad->SetLogy(0);
	if (m_ListOfLog && m_ListOfLog->FindObject(obj->GetName())){
	  gPad->SetLogy(1);
          cout << "StHistUtil::DrawHists -- Will draw in log scale: " << obj->GetName() <<endl;
        }
        chkdim = ((TH1 *)obj)->GetDimension();
	//	  cout << " name " << obj->GetName() << " dimension " << chkdim << endl;
        if (chkdim == 2) obj->Draw("box");
        else 
            obj->Draw();   
	if (gPad) gPad->Update();
      }
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
  while (obj = nextObj()) {

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

// m_ListOfLog -  is a list of log plots
// construct a TObject
  TObject *obj = 0;
// construct a TIter ==>  () is an overloaded operator in TIter
  TIter nextObj(m_ListOfLog);
  Int_t LogYCount = 0;

// use = here instead of ==, because we are setting obj equal to nextObj and then seeing if it's T or F
  while (obj = nextObj()) {

    cout << " StHistUtil::ExamineLogYList has hist " <<  obj->GetName() << endl;
    LogYCount++;

  }

  cout << " Now in StHistUtil::ExamineLogYList, No. Hist. in LogY scale = " << LogYCount <<endl;
  return LogYCount;
}

//_____________________________________________________________________________


Int_t StHistUtil::AddToLogYList(const Char_t *HistName){  
// Method AddToLogYList
//   making list of all histograms that we want drawn with LogY scale

//  cout << " **** Now in StHistUtil::AddToLogYList  **** " << endl;

// Since I'm creating a new list, must delete it in the destructor!!
//make a new TList on heap(persistant); have already defined m_ListOfLog in header file
   if (!m_ListOfLog) m_ListOfLog = new TList;

// the add method for TList requires a TObject input  (also can use TObjString)
// create TObjString on heap
   TObjString *HistNameObj = new TObjString(HistName);

// - check if it's already on the list - use FindObject method of TList
    TObject *lobj = 0;
    lobj = m_ListOfLog->FindObject(HistName);
// now can use Add method of TList
    if (!lobj) {
       m_ListOfLog->Add(HistNameObj);
       cout << " StHistUtil::AddToLogYList: " << HistName  <<endl;
    }
    else  cout << " StHistUtil::AddToLogYList: " << HistName << " already in list - not added" <<endl;
 
// return using a method of TList (inherits GetSize from TCollection)
 return m_ListOfLog->GetSize();
}


//_____________________________________________________________________________


Int_t StHistUtil::RemoveFromLogYList(const Char_t *HistName){  
// Method RemoveFromLogYList
//   remove hist from  list  that we want drawn with LogY scale

//  cout << " **** Now in StHistUtil::RemoveFromLogYList  **** " << endl;

// check if list exists:
  if (m_ListOfLog) {
    
// the remove method for TList requires a TObject input  
// - check if it's  on the list - use FindObject method of TList
    TObject *lobj = 0;
    lobj = m_ListOfLog->FindObject(HistName);
// now can use Remove method of TList
    if (lobj) {
      m_ListOfLog->Remove(lobj);
      cout << " RemoveLogYList: " << HistName << " has been removed from list" <<endl;
    }
    else  cout << " RemoveLogYList: " << HistName << " not on list - not removing" <<endl;

  } 
// return using a method of TList (inherits GetSize from TCollection)
 return m_ListOfLog->GetSize();
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
 "QaGlobtrkDetId",
 "QaGlobtrkFlag",
 "QaGlobtrkNPnt",
 "QaGlobtrkNPntMax",
 "QaGlobtrkNPntFit",
 "QaGlobtrkPt",
 "QaGlobtrkP",
 "QaGlobtrkXf0",
 "QaGlobtrkXf",
 "QaGlobtrkYf0",
 "QaGlobtrkYf",
 "QaGlobtrkZf0",
 "QaGlobtrkZf",
 "QaGlobtrkR",
 "QaGlobtrkRnf",
 "QaGlobtrkTanl ",
 "QaGlobtrkTheta ",
 "QaGlobtrkEta",
 "QaGlobtrkLength",
 "QaGlobtrkImpact",
 "QaGlobtrkNdof",
 "QaPrimtrkDetId",
 "QaPrimtrkFlag",
 "QaPrimtrkNPnt",
 "QaPrimtrkNPntMax",
 "QaPrimtrkNPntFit",
 "QaPrimtrkPt",
 "QaPrimtrkP",
 "QaPrimtrkXf0",
 "QaPrimtrkXf",
 "QaPrimtrkYf0",
 "QaPrimtrkYf",
 "QaPrimtrkZf0",
 "QaPrimtrkZf",
 "QaPrimtrkR",
 "QaPrimtrkRnf",
 "QaPrimtrkTanl ",
 "QaPrimtrkTheta ",
 "QaPrimtrkEta",
 "QaPrimtrkLength",
 "QaPrimtrkImpact",
 "QaPrimtrkNdof",
 "QaDstDedxNdedx",
 "QaDstDedxDedx0", 
 "QaDstDedxDedx1",
 "QaParticlePt",
 "QaParticleVtxX",
 "QaParticleVtxY",
 "QaParticleVtxZ"
   };
  sdefList = sdefList1;
  lengofList = sizeof(sdefList1)/4;
  }

  //  else 
  //  { cout << " StHistUtil::SetDefaultLogYList - no hist set in def logy list " << endl; } 
  //   cout <<  " !! HERE I AM1 " << lengofList << endl ;

  if (lengofList) {
    Int_t ilg = 0;
    Int_t numLog = 0;
    for (ilg=0;ilg<lengofList;ilg++) {
     numLog = AddToLogYList(sdefList[ilg]);
     cout <<  " !!! adding histogram " << sdefList[ilg] << " to LogY list "  << endl ;
    }
  }

}

//_____________________________________________________________________________











