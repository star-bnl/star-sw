//*-- Author : Valeri Fine (fine@bnl.gov)
// 
// $Id: StHistCollectorMaker.cxx,v 2.2 2000/11/30 19:37:26 fine Exp $
//
////////////////////////////////////////////////////////////////////////////
//                                                                        //
// StHistCollectorMaker is to collect the histBranch staff from the several //
//                    files                                               //
//                                                                        //
// use $STAR/StRoot/macro/analysis/doHists.C macro to see hot it works    //
//                                                                        //
// This maker collects histograms from histBranch                         //
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
#include "StChain.h"
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
   fMergedSet = new TDataSet("Merged");
   AddConst(fMergedSet);
   return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StHistCollectorMaker::Make(){
 //  Make - this methoid is called in loop for each event
  AddHists();
  return kStOK;
}

//_____________________________________________________________________________
TDataSet *StHistCollectorMaker::AddHists()
{
 //  Update dataset fMergedSet with "histBranch"  staff
  TDataSet *rec = fMergedSet;
  TDataSet *histBranch = GetDataSet("hist");
  if (!histBranch) { printf("No hist\n");  return 0;} 
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
        while ( (donorHist = (TH1 *)nextDonor()) ) donorHist->SetDirectory(0);      
        donor->Shunt(rec);
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
      const Char_t *newname = donor->GetName();
      TH1 *oldHist = 0;
      while ( (oldHist = (TH1 *)nextOld()) && !found) {
        // if the "new" set does contain the dataset
        // with the same name as ours update it too
        if (!strcmp(oldHist->GetName(),newname)) {
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

// $Log: StHistCollectorMaker.cxx,v $
// Revision 2.2  2000/11/30 19:37:26  fine
// Reference to doHists.C macro has been added
//
// Revision 2.1  2000/11/30 19:35:14  fine
// New analysis utility to collect all histogram from all histBranh production branches
//

