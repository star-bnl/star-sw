//*CMZ :          12/07/98  18.27.27  by  Valery Fine(fine@mail.cern.ch)
//*-- Author :    Valery Fine(fine@mail.cern.ch)   03/07/98

// Copyright (C) Valery Fine (Valeri Faine) 1998. All right reserved
 
//*KEEP,TDataset,T=C++.
#include "St_DataSetIter.h"
#include "St_DataSet.h"
//*KEEP,TBrowser.
#include <TBrowser.h>
//*KEND.
#include <TSystem.h>
 
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_DataSet                                                           //
//                                                                      //
// St_DataSet class is a base class to implement the directory-like     //
// data structures and maintain it via St_DataSetIter class iterator    //
//                                                                      //
// St_DataSet can be iterated using an iterator object (see St_DataSetIter) //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
 
ClassImp(St_DataSet)
 
//______________________________________________________________________________
St_DataSet::St_DataSet(const Char_t *name, St_DataSet *parent) : TNamed(), fList(0), fMother(0)
{
   if (strchr(name,'/')) {
      Error("St_DataSet::St_DataSet","dataset name cannot contain a slash", name);
      return;
   }
 
   if (parent) SetParent(parent);
 
   SetName(name);
   SetTitle("St_DataSet");
}
//______________________________________________________________________________
St_DataSet::~St_DataSet()
{
   Delete();
} 
//______________________________________________________________________________
void St_DataSet::Add(St_DataSet *dataset)
{
  if (!dataset) return;
 
  if (!fList) {
       fList = new TList;
   //    if (fMother) fList->SetParent(fMother);
  }
 
  // Check whether this new child has got any partent yet
  if (!dataset->GetParent()) dataset->SetParent(this);
  fList->Add(dataset);
}
 
//______________________________________________________________________________
void St_DataSet::Browse(TBrowser *b)
{
  // Browse this dataset (called by TBrowser).
   St_DataSetIter next(this);
   St_DataSet *obj;
   if (b)
       while (obj = next())
                 b->Add(obj,obj->GetName());
}
//______________________________________________________________________________
void St_DataSet::Delete(Option_t *opt){
  // First we should break our relationship with the parent if any
  if (fMother && !TestBit(kCanDelete) ) 
  {
    St_DataSet *parent = GetParent();
    if (parent) parent->Remove(this);
  }

  // Delete list of the St_DataSet
  if (fList) {
    TIter next(fList);   
    St_DataSet *son = 0;
 // Delete the sons of this St_DataSet only
    while (son = (St_DataSet *)next()) {
       if (this == son->GetParent()) {
       // mark the object is deleted from the St_DataSet dtor or Delete method
          son->SetBit(kCanDelete);
          delete son;        
       }
    }
    delete fList;
    fList = 0;
  }
}
 
//______________________________________________________________________________
void  St_DataSet::ls(Option_t *option)
{
 /////////////////////////////////////////////////////////////////////
 //                                                                 //
 //  ls(Option_t *option)                                           //
 //                                                                 //
 //    option       - defines the path to be listed                 //
 //           = "*" -  means print all levels                       //
 //                                                                 //
 /////////////////////////////////////////////////////////////////////
 
  if (option && !strcmp(option,"*")) ls(Int_t(0));
  else {
    St_DataSet *set = 0;
    if (option && strlen(option) > 0) {
      St_DataSetIter local(this);
      St_DataSet *set = local(option);
    }
    else
      set = this;
    if (set) set->ls(Int_t(1));
    else 
      if (option) Warning("ls","Dataset <%s> not found",option);
  }     
}

//______________________________________________________________________________
void St_DataSet::ls(Int_t depth)
{
 /////////////////////////////////////////////////////////////////////
 //                                                                 //
 //  ls(Int_t depth)                                                //
 //                                                                 //
 //  Prints the list of the this St_DataSet.                        //
 //                                                                 //
 //  Parameter:                                                     //
 //  =========                                                      //
 //    Int_t depth >0 the number of levels to be printed            //
 //               =0 all levels will be printed                     //
 //            No par - ls() prints only level out                  //
 //                                                                 //
 /////////////////////////////////////////////////////////////////////
 
  TNamed::ls();
 
  if (fList && depth != 1 ) {
    TIter next(fList);
    St_DataSet *d=0;
    while (d = (St_DataSet *)next()) {
        IncreaseDirLevel();
        d->ls(depth == 0 ? 0 : --depth);
        DecreaseDirLevel();
    }
  }
} 
#if 0
//______________________________________________________________________________
Bool_t    St_DataSet::IsEmpty() const 
{ 
 return (HasData() == 0 && GetListSize() == 0);
}
#endif
//______________________________________________________________________________
Bool_t St_DataSet::IsThisDir(const Char_t *dirname) const 
{
  return !strcmp(GetName(),dirname); 
}
#if 0
//______________________________________________________________________________
TTree *MakeTree(St_DataSet *dataset)
{
  // Creare a TTree object for the current Dataset
 
  strcat(path,GetName());
 
  if (fList && depth != 1 ) {
    TIter next(fList);
    St_DataSet *d=0;
    while (d = (St_DataSet *)next()) {
        IncreaseDirLevel();
        d->ls(depth == 0 ? 0 : --depth);
        DecreaseDirLevel();
    }
  }
}
//______________________________________________________________________________
void FillTree()
{
}
#endif
//______________________________________________________________________________
TString St_DataSet::Path() const
{
 // return the full path of this data set 
   TString str;
   St_DataSet *parent = GetParent();
   if (parent) { 
       str = parent->Path();
       str += "/";
   }
   str +=  GetName();
   return str;
}
//______________________________________________________________________________
void St_DataSet::Remove(St_DataSet *set)
{
  if (fList && set) fList->Remove(set);
}
 
//______________________________________________________________________________
EDataSetPass St_DataSet::Pass(EDataSetPass ( *callback)(St_DataSet *),Int_t depth)
{
 /////////////////////////////////////////////////////////////////////
 //                                                                 //
 // Pass (callback,depth)                                           //
 //                                                                 //
 // Calls callback(this) for all datasets those recursively         //
 //                                                                 //
 //  Parameter:                                                     //
 //  =========                                                      //
 //    Int_t depth >0 the number of levels to be passed             //
 //                =0 all levels will be passed                     //
 //                                                                 //
 //  Return:                                                        //
 //  ======                                                         //
 //  kContinue - continue passing                                   //
 //  kPrune    - stop passing the current branch, go to the next one//
 //  kStop     - stop passing, leave all braches                    //
 //                                                                 //
 /////////////////////////////////////////////////////////////////////
 
  if (!callback) return kStop;

  EDataSetPass condition = callback(this);

  if (condition == kContinue){
    if (fList && depth != 1 ) {
      TIter next(fList);
      St_DataSet *d=0;
      while (d = (St_DataSet *)next()) {
         condition = d->Pass(callback, depth == 0 ? 0 : --depth);
         if (condition == kStop) break;
      }
    }
  }
  return condition;
}
//______________________________________________________________________________
Int_t St_DataSet::Purge(Option_t *opt)
{
 //
 // Purge  - deletes all "dummy" datasets those are not ended up with some
 //          dataset with data inside (those return HasData() = 0)
 //
 // Purge does affect only the structural links and doesn't touch refs.
 //
 if (fList) {
   TIter next(fList);   
   St_DataSet *son = 0;
// Purge the sons of this St_DataSet only
   TList garbage;
   while (son = (St_DataSet *)next()) {
     if (this == son->GetParent()) {
     // mark the object is deleted from the St_DataSet dtor
        son->Purge();
//        if (son->IsEmpty()) //  Bool_t IsEmpty() const { return (HasData() == 0 && GetListSize() == 0) 
        if (son->HasData() == 0 && son->GetListSize() == 0) 
                           garbage.Add(son);
     }
   }
   garbage.Delete();
 }
 return 0;
}
//______________________________________________________________________________
void  St_DataSet::SetParent(St_DataSet *parent){
//
//  Break the "parent" relationship with the current object parent if present
//  Set the new parent if any
//
   St_DataSet *oldparent = GetParent();
    // Each St_DataSet object must have only parent, therefore ...
   if (oldparent)
       oldparent->Remove(this);       // Break relations with the current parents
//   if (fList) 
//      fList->SetParent(parent);       // Establish a new relationships
   SetMother(parent);                 // Adjust St_DataSet::fMother pointer as well
}
//______________________________________________________________________________
void St_DataSet::SetWrite()
{
 //
 // To Write object first we should temporary break the 
 // the backward fMother pointer (otherwise ROOT follows this links
 // and will pull fMother out too.
 //
  TObject *mothersav = fMother; // GetParent();
  fMother = 0;
//  SetParent();
  Write();
  // Restore the fMother pointer
  // SetParent(mothersav);
  fMother = mothersav;
}
//______________________________________________________________________________
void St_DataSet::Shunt(St_DataSet *dataset)
{
  // Remove object from the original dataset and insert into this one
  if (!dataset) return;

  if (!fList) {
       fList = new TList;
//       if (fMother) fList->SetParent(fMother);
  }
 
  // Check whether this new child has got any parent yet
  dataset->SetParent(this);
  fList->Add(dataset);
}
//______________________________________________________________________________
void St_DataSet::Update(St_DataSet* set,UInt_t opt)
{
//
// Update this St_DataSet with "set"
//
// ATTENTION !!!
// ---------
// This method changes the parent relationships of the input "set"
//

  if(!set) return;

  St_DataSetIter nextnew(set);
  St_DataSet *newset = 0;
  while(newset = nextnew()) {
    Bool_t found = kFALSE;
    // Check whether this has the list of the sons
    if (fList) {
      TIter nextold(fList);
      const Char_t *newname = newset->GetName(); 
      St_DataSet *oldset = 0;
      while ( ((oldset = (St_DataSet *)nextold())!=0) && !found) {
        // if the "new" set does contain the dataset 
        // with the same name as ours update it too
        if (oldset->IsThisDir(newname)) {
           oldset->Update(newset);
           found = kTRUE;
        }
      }
    }
    // If the new "set" contains some new dataset with brand-new name
    // move it into the our dataset and remove it from its old location
    if (!found) Shunt(newset);
  }
}
//______________________________________________________________________________
void St_DataSet::Update()
{
 //
 //  Update()
 //
 //  Recursively updates all tables for all nested datasets
 //  in inverse order
 //
 
  St_DataSetIter next(this);
  St_DataSet *set = 0;
  while( set = next())
            set->Update();
}

