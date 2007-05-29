//*-- Author : Valeri Fine (fine@bnl.gov)
// 
// $Id: StHistCollectorMaker.cxx,v 2.7 2007/05/29 20:46:19 fine Exp $
//
////////////////////////////////////////////////////////////////////////////
//                                                                        //
// StHistCollectorMaker is to collect the histBranch staff from the several //
//                    files                                               //
//                                                                        //
// use $STAR/StRoot/macro/analysis/doHists.C macro to see hot it works    //
//                                                                        //
// This maker collects non-empty histograms from histBranch               //
//             calling GetDataSet("hist")                                 //
// and accumulates it under ".const/Merged" sub-dataset.                  //
// The "Merged" dataset preserves the dataset structure of the original   //
// histBrach. Namely, it contains the one level of heirarchy with         //
// the maker names and TList object contaoins the list of histogram       //
// producing by the maker with the several reconstruction sessions        //
//                                                                        //
//  See: $STAR/StRoot/macros/analysis/doHists.C macro as example.         //
//       doHist.C macro is derived from doEvents.C. It preserves doEvents.C
//       user interface.
//                                                                        //
//  Submit any problem with this code via begin_html <A HREF="http://www.star.bnl.gov/STARAFS/comp/sofi/bugs/send-pr.html"><B><I>"STAR Problem Report Form"</I></B></A> end_html   //
//                                                                        //
////////////////////////////////////////////////////////////////////////////

#include "StHistCollectorMaker.h"
#include "TDataSetIter.h"
#include "TH1.h"

ClassImp(StHistCollectorMaker)

//_____________________________________________________________________________
StHistCollectorMaker::StHistCollectorMaker(const char *name):StMaker(name){
 //  StHistCollectorMaker constructor
 //  const char *name -  the name of the maker
  fMergedSet = 0;
}
//_____________________________________________________________________________
StHistCollectorMaker::~StHistCollectorMaker(){ }
//_____________________________________________________________________________
Int_t StHistCollectorMaker::Init(){
// Create the dataset to merger the coming histograms
   if (fMergedSet) delete fMergedSet;
   fMergedSet = 0;
   return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StHistCollectorMaker::Make(){
 //  Make - this methoid is called in loop for each event
  static const char *datasetName = "Merged";
  if (!fMergedSet) {
    // Qurey one's "Mergeg" dataset (assuming I/O maker)
    // to take over
    fMergedSet = GetDataSet(datasetName);
    if (!fMergedSet) fMergedSet = new TDataSet(datasetName);
    fMergedSet->Shunt(0);
    AddConst(fMergedSet);
  }
  AddHists();
  return kStOK;
}

//_____________________________________________________________________________
TDataSet *StHistCollectorMaker::AddHists()
{
 //  Update dataset fMergedSet with "histBranch"  staff
  TDataSet *rec = fMergedSet;
  TDataSet *histBranch = GetDataSet("hist");
  if (!histBranch) { 
     LOG_INFO << "No hist" << endm;
     return 0;} 
  TDataSetIter nextDonor(histBranch);
  TDataSet *donor = 0;
  while((donor = nextDonor())) {
    Bool_t found = kFALSE;
    TDataSetIter nextOld(rec);
    const Char_t *newname = donor->GetName();
    TDataSet *oldset = 0;
    while ( (oldset = nextOld()) && !found) {
      // if the "new" set does contain the dataset
      // with the same name as ours update it too
      if (oldset->IsThisDir(newname)) {
        UpdateHists((TObjectSet *)oldset,(TObjectSet *)donor);
        found = kTRUE;
      }
    }
    // If the new "set" contains some new dataset with brand-new name
    // move it into the our dataset and remove it from its old location
    if (!found) { 
      // Take in account the non-empty list only 
      TList *newList   = (TList *)donor->GetObject();
      if (newList && newList->GetSize() > 0) {
        // remove the histogram from its directories
        TIter  nextDonor(newList);
        TH1 *donorHist = 0;
        Int_t count = 0;
        while ( (donorHist = (TH1 *)nextDonor()) ) {
           if (donorHist->GetEntries() > 0) {
             donorHist->SetDirectory(0);
             count++; // count non-empty histograms
           } else { 
             newList->Remove(donorHist); 
           }
        }
        if (count) donor->Shunt(rec); // accept this if it contains one non-empty hist at least
      }
    }
  }
  return 0;
}
//_____________________________________________________________________________
void  StHistCollectorMaker::UpdateHists(TObjectSet *oldSet,TObjectSet *newSet)
{
  // Merge two sets of the histograms
  if (oldSet && newSet) {
    // Get the list of the new histograms
    TList *newList   = (TList *)newSet->GetObject();
    TList *oldList = (TList *)oldSet->GetObject();
    TIter  nextDonor(newList);
    TIter  nextOld(oldList);

    TH1 *donor = 0;
    while ( (donor = (TH1 *)nextDonor()) ) {
      Bool_t found = kFALSE;
      if (donor->GetEntries() > 0 ) {
        const Char_t *newname = donor->GetName();
        TH1 *oldHist = 0;
        while ( (oldHist = (TH1 *)nextOld()) && !found) {
          // if the "new" set does contain the dataset
          // with the same name as ours update it too
          if (!strcmp(oldHist->GetName(),newname)) {
             found = kTRUE;
             oldHist->Add(donor); // merge histograms
          }
        }; nextOld.Reset(); // old histograms has been looked up

        // If the new "set" contains some new dataset with brand-new name
        // move it into the our dataset and remove it from its old location
        if (!found) { // move histogram to this 
          oldList->Add(donor);
          newList->Remove(donor);
          donor->SetDirectory(0);
        }      
      }
    }
  }
}

// $Log: StHistCollectorMaker.cxx,v $
// Revision 2.7  2007/05/29 20:46:19  fine
// Introduce logger-based output
//
// Revision 2.6  2007/04/28 20:36:14  perev
// Redundant StChain.h removed
//
// Revision 2.5  2000/12/02 01:03:10  fine
// Recursive collection of histogram introduced
//
// Revision 2.4  2000/12/01 02:16:42  fine
// the performance issue resolved
//
// Revision 2.3  2000/11/30 20:13:27  fine
// Get rid of the empty histograms (thanks Fisyak)
//
// Revision 2.2  2000/11/30 19:37:26  fine
// Reference to doHists.C macro has been added
//
// Revision 2.1  2000/11/30 19:35:14  fine
// New analysis utility to collect all histogram from all histBranh production branches
//

