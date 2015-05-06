/*
 * StTagFilterMaker
 *
 * Base class to skip production of events using
 * criteria stored in a .pretags.root Tag TTree
 * 
 * Derived classes should implement SkipEvent()
 * to determine whether events should be skipped or not.
 * 
 * SetVarList() can be used to provide a colon-separated
 * list of variables (formulas) which will automatically
 * be evaluated for use in SkipEvent().
 * Individual variable values will be available as GetVal(0),GetVal(1) ...
 * Individual variable names  will be available as GetVar(0),GetVar(1) ...
 *
 * Otherwise, derived classes can access the Tag TTree
 * as they wish, without needing selection criteria, as
 * a TEventList is applied to select the run and event.
 *
 */


#include "StTagFilterMaker.h"
#include "StMessMgr.h"
#include "StEvtHddr.h"
#include "TFile.h"
#include "TTree.h"
#include "TEntryList.h"
#include "TTreeFormula.h"


ClassImp(StTagFilterMaker)

//____________________________________________________________________________________________________
StTagFilterMaker::StTagFilterMaker(const Char_t *name) : StMaker(name) 
{
  mTagFile = "";
  mVarList = "";
  mSkippedEventCounter = 0;
  mFile = 0;
  mTree = 0;
  mEntryList = new TEntryList(Form("%s_EList",name),Form("Entry list for %s",name));
}
//____________________________________________________________________________________________________
StTagFilterMaker::~StTagFilterMaker()
{ 
  SafeDelete(mFile);
  delete mEntryList;
}
//____________________________________________________________________________________________________
Int_t StTagFilterMaker::Init()
{
  SetAttr(".Privilege", 1);

  return StMaker::Init();
}
//____________________________________________________________________________________________________
Int_t StTagFilterMaker::InitRun(const int runnum)
{

  /// Open the input .pretags.root file and get the Tag tree.
  /// If an input .preptags.root file has not yet been defined
  /// then default to using an input .pretags.root file which
  /// has the same specification as a tags.root file for the
  /// current chain's input data file, but with the ".tags.root"
  /// suffix appropriately replaced by ".pretags.root"

  if (mTagFile.IsWhitespace()){
    mTagFile = GetTFile()->GetName();
    mTagFile.ReplaceAll(".tags.root",".pretags.root");
  }

  // Open the .pretags.root file
  mFile = TFile::Open(mTagFile);
  if (! mFile ) {
    LOG_ERROR << "Input TagFile : " << mTagFile << " cannot be opened" << endm;
    return kStErr;
  }
  LOG_INFO << "Input TagFile : " << mTagFile << " opened" << endm;
  mEntryList->SetDirectory(mFile); // mEntryList's directory must not be an output file


  // Get the Tag tree
  mTree = static_cast<TTree*>(mFile->Get("Tag"));
  if (! mTree ) {
    LOG_ERROR << "In TagFile : " << mTagFile << " cannot find TTree \"Tag\"" << endm;
    return kStErr;
  }

  return kStOK;
}
//____________________________________________________________________________________________________
Int_t StTagFilterMaker::Make() 
{
  /// Select the Tag TTree entry for this run and event, as found in the "EvtHddr" dataset
  StEvtHddr* EvtHddr = static_cast<StEvtHddr*>(GetDataSet("EvtHddr"));
  if (! EvtHddr) {
    LOG_ERROR << "EvtHddr has not been found" << endm;
    return kStErr;
  }
  /// Use TTree::Draw() to fill a TEntryList
  mEntryList->GetDirectory()->cd(); // must be in mEntryList's directory before TTree::Draw()
  int nFound = mTree->Draw(Form(">>%s",mEntryList->GetName()),
                           Form("mRunNumber==%i&&mEventNumber==%i",
                                EvtHddr->GetRunNumber(),EvtHddr->GetEventNumber()),
                           "entrylist");
  /// Apply the TEntryList to all subsequent queries on the Tag TTree
  mTree->SetEntryList(mEntryList);
  if (nFound != 1) {
    LOG_ERROR << "Run/Event = " << EvtHddr->GetRunNumber() << "/" << EvtHddr->GetEventNumber() 
	      << " has been found in tag file " << nFound << " times" <<  endm;
    return kStErr;
  }

  /// Evaluate any preset variable list of interest from the Tag TTree
  if (mVarList.Length()) EvalVarList();

  /// Issue the request to skip an event if directed by SkipEvent()
  if (SkipEvent()) {
    mSkippedEventCounter++;
    return kStSKIP;
  }

  return StMaker::Make();
}
//____________________________________________________________________________________________________
void StTagFilterMaker::EvalVarList(const char* varList)
{
  /// Evaluate a supplied colon-separated variable list, or use the standard list 
  mTree->Draw((varList ? varList : mVarList.Data()),0,"goff");
}
//____________________________________________________________________________________________________
Double_t StTagFilterMaker::GetVal(int i, int idx)
{
  /// Return the value of the i-th variable (formula) evaluated
  return (mTree->GetVal(i))[idx];
}
//____________________________________________________________________________________________________
const char* StTagFilterMaker::GetVar(int i)
{
  /// Return the name of the i-th variable (formula) evaluated
  return mTree->GetVar(i)->GetTitle();
}
//____________________________________________________________________________________________________
void StTagFilterMaker::Clear(const Option_t*) 
{
  if (mTree) mTree->SetEntryList(0);
  StMaker::Clear();
}

/* -------------------------------------------------------------------------
 * $Id: StTagFilterMaker.cxx,v 1.3 2015/05/06 18:10:22 genevb Exp $
 * $Log: StTagFilterMaker.cxx,v $
 * Revision 1.3  2015/05/06 18:10:22  genevb
 * Avoid letting TEntryList and histogram from TTree::Draw() getting into output files
 *
 * Revision 1.2  2015/05/05 20:23:42  genevb
 * pre.tags.root => pretags.root
 *
 * Revision 1.1  2015/05/01 21:25:50  jeromel
 * First version of the DataFiler + one imp: MTD. Code from GVB reviewed & closed (VP+JL))
 *
 *
 * -------------------------------------------------------------------------
 */

