//*CMZ :          12/07/98  18.27.27  by  Valery Fine(fine@mail.cern.ch)
//*-- Author :    Valery Fine(fine@mail.cern.ch)   03/07/98

// Copyright (C) Valery Fine (Valeri Faine) 1998. All right reserved
 
//*KEEP,TDataset,T=C++.
#include <iostream.h>
#include "St_DataSetIter.h"
#include "St_DataSet.h"
//*KEEP,TBrowser.
#include "TBrowser.h"
//*KEND.
#include "TSystem.h"
 
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
St_DataSet::St_DataSet(const Char_t *name, St_DataSet *parent) : TNamed(name,"St_DataSet")
{
  //  cout << "ctor for " << GetName() << " - " << GetTitle() << endl;
   if (strchr(name,'/')) {
      Error("St_DataSet::St_DataSet","dataset name cannot contain a slash", name);
      return;
   }
   fList =0; fParent=0;
   if (parent) parent->Add(this);
}
//______________________________________________________________________________
St_DataSet::St_DataSet(const St_DataSet &pattern,EDataSetPass iopt)
{
  //
  // Creates St_DataSet (clone) with a topology similar with St_DataSet *pattern
  //
  //  Parameters:
  //  -----------
  //  pattern        - the pattern dataset
  //  iopt = kStruct - clone only my structural links
  //         kAll    - clone all links
  //         kRefs   - clone only refs
  //         kMarked - clone marked (not implemented yet) only
  //
  //   All new-created sets become the structural ones anyway.
  //
  //  cout << "ctor for " << GetName() << " - " << GetTitle() << endl;

  SetName(pattern.GetName());
  SetTitle(pattern.GetTitle());

  St_DataSet *set = 0;
  St_DataSetIter next((St_DataSet *)&pattern);
  Bool_t optsel = (iopt == kStruct);
  Bool_t optall = (iopt == kAll);
  while ((set = next())) {
// 		define the parent of the next set
     St_DataSet *parent = set->GetParent(); 
     if ( optall || (optsel && parent == this) )
       Add((St_DataSet *)(set->Clone()));
  }
}
//______________________________________________________________________________
St_DataSet::St_DataSet(TNode &src){

}
//______________________________________________________________________________
St_DataSet::~St_DataSet()
{
//  		cout << "Default destructor for " << GetName() << " - " << GetTitle() << endl;
   Shunt(0); Delete();
} 
//______________________________________________________________________________
void St_DataSet::AddLast(St_DataSet *dataset)
{
// 	 Add St_DataSet object at the end of the dataset list of this dataset
  if (!dataset) return;
 
  if (!fList) fList = new TList;
 
  // Check whether this new child has got any partent yet
  if (!dataset->GetParent()) dataset->SetParent(this);
  fList->AddLast(dataset);
}
 
//______________________________________________________________________________
void St_DataSet::AddFirst(St_DataSet *dataset)
{
  //  Add St_DataSet object at the beginning of the dataset list of this dataset
  if (!dataset) return;
 
  if (!fList) 
       fList = new TList;
 
  // Check whether this new child has got any partent yet
  if (!dataset->GetParent()) dataset->SetParent(this);
  fList->AddFirst(dataset);
}

//______________________________________________________________________________
void St_DataSet::Browse(TBrowser *b)
{
  // Browse this dataset (called by TBrowser).
   St_DataSetIter next(this);
   St_DataSet *obj;
   if (b)
       while ((obj = next())) b->Add(obj,obj->GetName());
}
//______________________________________________________________________________
TObject *St_DataSet::Clone() {
   return new St_DataSet(*this);
}
//______________________________________________________________________________
void St_DataSet::Delete(Option_t *opt){
// 	First we should break our relationship with the parent if any
  if(opt){/*unused*/}

// 	Delete list of the St_DataSet
  if (!fList) return;
  TIter next(fList);   
  St_DataSet *son = 0;
// 	Delete the sons of this St_DataSet only
  while ((son = (St_DataSet *)next())) {
    if (this != son->GetParent()) continue;
// 		mark the object is deleted from the St_DataSet dtor or Delete method
    son->SetBit(kCanDelete); delete son;
  }
// 	Cleare list
  fList->Clear("nodelete");
  delete fList;
  fList = 0;
}
//______________________________________________________________________________
St_DataSet *St_DataSet::Find(const Char_t *path) const
{
  //
  // Full description see: St_DataSetIter::Find
  //
  // Note. This is method is quite expansive. 
  // ----- It is done to simplify the user's code when one wants to find ONLY object.
  //       If you need to find more then 1 object in this dataset,
  //       regard using St_DataSetIter class yourself.
  //
  St_DataSetIter next((St_DataSet*)this);
  return next.Find(path);
}
//______________________________________________________________________________
St_DataSet *St_DataSet::FindObject(const Char_t *name,const Char_t *path,Option_t *opt) const
{
  //
  // Full description see: St_DataSetIter::FindObject
  //
  // Note. This is method is quite expansive. 
  // ----- It is done to simplify the user's code when one wants to find ONLY object.
  //       If you need to find more then 1 object in this dataset,
  //       regard using St_DataSetIter class yourself.
  //

  St_DataSetIter next((St_DataSet*)this);
  return next.FindObject(name,path,opt);
}
//______________________________________________________________________________
St_DataSet *St_DataSet::First() const
{
 //  Return the first object in the list. Returns 0 when list is empty.
 if (fList) return (St_DataSet *)(fList->First());
 return 0;
}

//______________________________________________________________________________
St_DataSet *St_DataSet::Last() const
{
 // Return the last object in the list. Returns 0 when list is empty.
 if (fList) return (St_DataSet *)(fList->Last());
 return 0;
}
 
//______________________________________________________________________________
void  St_DataSet::ls(Option_t *option) const
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
      St_DataSetIter local((St_DataSet*)this);
      set = local(option);
    }
    else
      set = (St_DataSet*)this;
    if (set) set->ls(Int_t(1));
    else 
      if (option) Warning("ls","Dataset <%s> not found",option);
  }     
}

//______________________________________________________________________________
void St_DataSet::ls(Int_t depth) const
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
 
  printf("%3d - %s\t%s\n",GetDirLevel(),(const char*)Path(),(char*)GetTitle());
  if (!fList || depth == 1 ) return;
  if (!depth) depth = 99999;
 
  TIter next(fList);
  St_DataSet *d=0;
  while ((d = (St_DataSet *)next())) {
    IncreaseDirLevel();
    d->ls(depth-1);
    DecreaseDirLevel();
  }
} 
//______________________________________________________________________________
Bool_t St_DataSet::IsLocked() const {return 0;}
//______________________________________________________________________________
Bool_t St_DataSet::IsThisDir(const Char_t *dirname,int len,int ignorecase) const 
{
  if (!ignorecase) {
    if (len<0) {return !strcmp (GetName(),dirname); 
    } else     {return !strncmp(GetName(),dirname,len);}
  } else {
    const char *name = GetName();
    if (len==-1) len = strlen(dirname);
    for (int i=0;i<len;i++) { if ( tolower(name[i])!=tolower(dirname[i])) return 0;}
    return 1;
  }
}
//______________________________________________________________________________
void St_DataSet::Mark()
{ Mark(kMark,kSet); }
//______________________________________________________________________________
void St_DataSet::Mark(UInt_t flag,EBitOpt reset)
{  SetBit(flag,reset); }

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
 //  Return (this value mast be returned by the user's callback):   //
 //  ======                                                         //
 //  kContinue - continue passing                                   //
 //  kPrune    - stop passing the current branch, go to the next one//
 //  kUp       - stop passing, leave the current branch,            //
 //              return to previous level and continue              //
 //  kStop     - stop passing, leave all braches                    //
 //                                                                 //
 /////////////////////////////////////////////////////////////////////
 
  if (!callback) return kStop;

  EDataSetPass condition = callback(this);

  if (condition == kContinue){
    if (fList && depth != 1 ) {
      TIter next(fList);
      St_DataSet *d=0;
      while ( (d = (St_DataSet *)next()) ) {
         condition = d->Pass(callback, depth == 0 ? 0 : --depth);
         if (condition == kStop || condition == kUp) break;
      }
    }
  }
  return condition==kUp ? kContinue:condition;
}
//______________________________________________________________________________
EDataSetPass St_DataSet::Pass(EDataSetPass ( *callback)(St_DataSet *,void*),void *user,Int_t depth)
{
 /////////////////////////////////////////////////////////////////////
 //                                                                 //
 // Pass (callback,user,depth)                                      //
 //                                                                 //
 // Calls callback(this,user) for all datasets those recursively    //
 //                                                                 //
 //  Parameter:                                                     //
 //  =========                                                      //
 //    Int_t depth >0 the number of levels to be passed             //
 //                =0 all levels will be passed                     //
 //                                                                 //
 //  Return (this value mast be returned by the user's callback):   //
 //  ======                                                         //
 //  kContinue - continue passing                                   //
 //  kPrune    - stop passing the current branch, go to the next one//
 //  kUp       - stop passing, leave the current branch,            //
 //              return to previous level and continue              //
 //  kStop     - stop passing, leave all braches                    //
 //                                                                 //
 /////////////////////////////////////////////////////////////////////
 
  if (!callback) return kStop;

  EDataSetPass condition = callback(this,user);

  if (condition == kContinue){
    if (fList && depth != 1 ) {
      TIter next(fList);
      St_DataSet *d=0;
      while ((d = (St_DataSet *)next())) {
        condition = d->Pass(callback, user, depth == 0 ? 0 : --depth);
        if (condition == kStop) break;
        if (condition == kUp  ) break;
      }
    }
  }
  return (condition==kUp) ? kContinue:condition;
}
//______________________________________________________________________________
Int_t St_DataSet::Purge(Option_t *)
{
//
// Purge  - deletes all "dummy" datasets those are not ended up with some
//          dataset with data inside (those return HasData() = 0)
//
// Purge does affect only the structural links and doesn't touch refs.
//

 if (!fList) return 0;
 TIter next(fList);   
 St_DataSet *son = 0;
// 		Purge the sons of this St_DataSet only
   TList garbage;
   while ((son = (St_DataSet *)next())) {
     if (this == son->GetParent()) continue;
// 		mark the object is deleted from the St_DataSet dtor
     son->Purge();
     if (son->HasData() || son->GetListSize()) continue;
     delete son;
 }
 return 0;
}
//______________________________________________________________________________
void  St_DataSet::SetLock(int lock){}
//______________________________________________________________________________
void  St_DataSet::SetParent(St_DataSet *parent){
//
//  Break the "parent" relationship with the current object parent if present
//  Set the new parent if any
//
   fParent = parent;                 
}
//______________________________________________________________________________
void St_DataSet::SetWrite()
{
 //
 // To Write object first we should temporary break the 
 // the backward fParent pointer (otherwise ROOT follows this links
 // and will pull fParent out too.
 //
  St_DataSet *saveParent = fParent; // GetParent();
  fParent = 0;
  Write();
  fParent = saveParent;
}
//______________________________________________________________________________
void St_DataSet::Shunt(St_DataSet *dataset)
{
// 	Remove the object from the original and add it to dataset 
  if (fParent) fParent->Remove(this);
  SetParent(0);
  if (dataset) dataset->Add(this);
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
  if(opt){/*unused*/}
  if(!set) return;

  St_DataSetIter nextnew(set);
  St_DataSet *newset = 0;
  while((newset = nextnew())) {
    Bool_t found = kFALSE;
// 		Check whether this has the list of the sons
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
   if (!found) newset->Shunt(this);
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
  while(( set = next())) set->Update();
}
//______________________________________________________________________________
void St_DataSet::Sort()
{
St_DataSetIter next(this,0);
St_DataSet *ds;
TList *list;
  while ((ds=next())) {
    list = ds->GetList();
    if (!list) continue;
    list->Sort(); ds->Sort();
  }
}
