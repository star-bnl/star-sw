//*CMZ :          12/07/98  18.27.27  by  Valery Fine(fine@mail.cern.ch)
//*-- Author :    Valery Fine(fine@mail.cern.ch)   03/07/98

// Copyright (C) Valery Fine (Valeri Faine) 1998. All right reserved
// $Id: St_DataSet.cxx,v 1.55 1999/10/28 16:24:29 fine Exp $
// $Log: St_DataSet.cxx,v $
// Revision 1.55  1999/10/28 16:24:29  fine
// St_DataSet major correction: it may be built with TList (default) or with TObjArray
//
// Revision 1.54  1999/09/24 23:31:37  perev
// Add title update in St_DataSet::Update
//
// Revision 1.53  1999/09/04 00:28:01  fine
// St_Table::NaN from VP and gloabl dataset have been introduced
//
// Revision 1.52  1999/07/23 13:26:06  fine
// Several new methods to mark the datasets have been introduced
//
// Revision 1.51  1999/06/26 01:40:55  fisyak
// Add Valery's abstract buffer
//
// Revision 1.50  1999/06/09 22:08:53  fine
// Comment clean up
//

#include <iostream.h>
#include "St_DataSetIter.h"
#include "St_DataSet.h"
#include "StBufferAbc.h"

#include "TBrowser.h"

#include "TSystem.h"
 
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_DataSet                                                           //
//                                                                      //
// St_DataSet class is to create a special compound object-container:   //
//                                                                      //
// ==================================================================== //
//    St_DataSet object ::= the "named" list of St_DataSet objects      //
// ==================================================================== //
// where the "list" (the pointer to TList object) may contain no object //
//                                                                      //
//  St_DataSet object has a back pointer to its "parent" St_DataSet     //
//  object, the "character" *name* and "character" *title*              //
//                                                                      //
//  The service this class does provide is to help the user to build    //
//  and manage the hierarchy of his/her data but the data itself.       //
//                                                                      //
//  So it is not "Container" itself rather the basement (base class)    //
//  to built the containers.                                            //
//                                                                      //
//  One may derive the custom container classes from St_DataSet.        //
//  See for example St_ObjectSet, St_Table, St_Node, St_FileSet         //
//  These classes  derived from St_DataSet:                             //
//                                                                      //
//   Class Name                                                         //
//   ----------                                                         //
//  St_ObjectSet::public St_DataSet - is a container for TObject        //
//  St_Table::    public St_DataSet - is a container for the array      // 
//                                    of any "plain" C-structure        //
//  St_Node::     public St_DataSet - is a container for 3D objects     //
//  StMaker::     public St_DataSet - is a container for STAR "control" //
//                                    objects                           //
//   etc etc                                                            //
//                                                                      //
//  St_DataSet class is a base class to implement the directory-like    //
//  data structures and maintain it via St_DataSetIter class iterator   //
//                                                                      //
// St_DataSet can be iterated using an iterator object (see St_DataSetIter) //
//            or by St_DataSet::Pass method (see below)                 //
//                                                                      //
//  Terms:    Dataset       - any object from the list above            //
//  =====     Member          is called "DataSet Member"                //
//                                                                      //
//          Structural      - the "Dataset Member" is its               //
//            member          "Structural member" if its "back pointer" //         
//                            points to this object                     //
//                                                                      //
//           Dataset        - we will say this St_DataSet object "OWNs" //
//            Owner           (or is an OWNER / PARENT of ) another     //
//          (parent)          St_DataSet object if the last one is its  //
//                            "Structural Member"                       //
//                                                                      //
//          Associated      - If some object is not "Structural member" //
//            member          of this object we will say it is an       //
//                            "Associated Member" of this dataset       //
//                                                                      //
//           Orphan         - If some dataset is a member of NO other   //
//           dataset          St_DataSet object it is called an "orphan"//
//                            dataset object                            //
//                                                                      //
// - Any St_DataSet object may be "Owned" by one and only one another   //
//   St_DataSet object if any.                                          //
//                                                                      //
// - Any St_DataSet object can be the "Structural Member" of one and    //
//   only one another St_DataSet                                        //
//                                                                      //
// - Any St_DataSet object may be an "Associated Member" for any number //
//   of other St_DataSet objects if any                                 //
//                                                                      //
// - NAME issue:                                                        //
//   Each "dataset member" is in possession of some "alpha-numerical"   //
//   NAME as defined by TNamed class.                                   //
//   The NAME may contain any "printable" symbols but "SLASH" - "/"     //
//   The symbol "RIGHT SLASH" - "/" can not be used as any part of the  //
//   "DataSet Member" NAME                                              //
//    Any DataSet  can be found by its NAME with St_DataSetIter object  //
//                                                                      //
// - TITLE issue:                                                       //
//   Each "dataset member" is in possession of the "alpha-numerical"    //
//   TITLE as defined by TNamed class. The meaning of the TITLE is      //
//   reserved for the derived classes to hold there some indetification //
//   that is special for that derived class.                            //
//                                                                      //
//   This means the user must be careful about  the "St_DataSet         //
//   NAME and TITLE since this may cause some "side effects" of the     //
//   particular class functions                                         //
//                                                                      //
// - It is NOT required those all "DataSet Members" are in possession   //
//   of the unique names, i.e. any number of "DataSet Members"          // 
//   may bear one and the same name                                     //
//                                                                      //
//   Actions:                                                           //
//   ========                                                           //
//   Create  DataSet is born either as "Orphan" or                      //
//                                  as "Structural Member"              //
//           of another St_DataSet object                               //
//                                                                      //
//   Add     One dataset can be included into another dataset.          //
//           Upon adding:                                               //
//           -  the "Orphan dataset" becomes "Structural Member"        //
//           - "Structural Members" of another dataset becomes the      //
//             "Associated Member" of this datatset                     //
//                                                                      //
//   Delete  - Upon deleting the "Structural Member":                   //              
//             - "REMOVES" itself  from the "Parent DataSet".           //
//             - Its "Associated memberships" is not changed though     //
//                                                                      //
//              The last means the DataSet with the "Associated Members"//
//              may contain a DIED pointers to unexisting "Associated"  //
//              objects !!!                                             //
//                                                                      //
//  Further information is provided my the particular method            //
//  descriptions.                                                       //
//                                                                      //
//  The St_DataSet class has several methods to control object('s)      // 
//  memberships                                                         //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
 
St_DataSet mainSet("DSMAIN");
St_DataSet *St_DataSet::fgMainSet = &mainSet;

ClassImp(St_DataSet)
 
//______________________________________________________________________________
St_DataSet::St_DataSet(const Char_t *name, St_DataSet *parent, Bool_t arrayFlag)
           : TNamed(name,"St_DataSet")
{
  //  cout << "ctor for " << GetName() << " - " << GetTitle() << endl;
   if (strchr(name,'/')) {
      Error("St_DataSet::St_DataSet","dataset name cannot contain a slash", name);
      return;
   }
   fList =0; fParent=0;
   if (arrayFlag) SetBit(kArray);
   if (parent) parent->Add(this);
//   else AddMain(this);
}

//______________________________________________________________________________
St_DataSet *St_DataSet::GetRealParent(){
  St_DataSet *p = GetParent();
  if (fgMainSet && p == fgMainSet) p = 0;
  return p;
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
  assert(0);
}
//______________________________________________________________________________
St_DataSet::~St_DataSet()
{
//  		cout << "Default destructor for " << GetName() << " - " << GetTitle() << endl;
   Shunt(0); Delete();
} 
//______________________________________________________________________________
void  St_DataSet::MakeCollection()
{
  if (!fList) 
    fList = TestBit(kArray) ? new TObjArray : new TList; 
}
//______________________________________________________________________________
void St_DataSet::AddAt(St_DataSet *dataset,Int_t idx)
{
//
// Add St_DataSet object at the "idx" position in ds 
// or at the end of the dataset 
// The final result is defined by either TList::AddAt or TObjArray::AddAt
// methods
//
  if (!dataset) return;
 
  MakeCollection();
 
  // Check whether this new child has got any parent yet
  if (!dataset->GetRealParent()) dataset->SetParent(this);
  fList->AddAt(dataset,idx);
}
//______________________________________________________________________________
void St_DataSet::AddAtAndExpand(St_DataSet *dataset, Int_t idx=0)
{
//   !!!! Under construction !!!!!
// Add St_DataSet object at the "idx" position in ds 
// or at the end of the dataset 
// The final result is defined by either TList::AddAt or TObjArray::AddAt
// methods
//
  if (!dataset) return;
 
  MakeCollection();
 
  // Check whether this new child has got any parent yet
  if (!dataset->GetRealParent()) dataset->SetParent(this);
  fList->AddAt(dataset,idx);
}
//______________________________________________________________________________
void St_DataSet::AddLast(St_DataSet *dataset)
{
// Add St_DataSet object at the end of the dataset list of this dataset
  if (!dataset) return;
 
  MakeCollection();
 
  // Check whether this new child has got any parent yet
  if (!dataset->GetRealParent()) dataset->SetParent(this);
  fList->AddLast(dataset);
}
 
//______________________________________________________________________________
void St_DataSet::AddFirst(St_DataSet *dataset)
{
 // Add St_DataSet object at the beginning of the dataset list of this dataset
  if (!dataset) return;
 
  MakeCollection();
 
  // Check whether this new child has got any partent yet
  if (!dataset->GetRealParent()) dataset->SetParent(this);
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
void St_DataSet::Delete(Option_t *opt)
{
//
// Delete - deletes the list of the St_DataSet objects and all "Structural Members"
//          as well
//          This method doesn't affect the "Associated Members"
//
  if(opt){/*unused*/}

// 	Delete list of the St_DataSet
  if (!fList) return;
  TIter next(fList);   
  St_DataSet *son = 0; 
  //  Delete the "Structural Members" of this St_DataSet only
  while ((son = (St_DataSet *)next())) {
    if (this != son->GetParent()) continue;
    // mark the object is deleted from the St_DataSet dtor or Delete method
    son->SetBit(kCanDelete); delete son;
  }
  //  Cleare list
  fList->Clear("nodelete");
  delete fList;
  fList = 0;
}
//______________________________________________________________________________
St_DataSet  *St_DataSet::FindByPath(const Char_t *path) const 
{  
  // Aliase for St_DataSet::Find(const Char_t *path) method
   return Find(path);
}
//______________________________________________________________________________
St_DataSet *St_DataSet::Find(const Char_t *path) const
{
  //
  // Full description see: St_DataSetIter::Find
  //
  // Note. This method is quite expansive. 
  // ----- It is done to simplify the user's code when one wants to find ONLY object.
  //       If you need to find more then 1 object in this dataset,
  //       regard using St_DataSetIter class yourself.
  //
  St_DataSetIter next((St_DataSet*)this);
  return next.Find(path);
} 
//______________________________________________________________________________
St_DataSet  *St_DataSet::FindByName(const Char_t *name,const Char_t *path,Option_t *opt) const
{ 
  // Aliase for St_DataSet::FindObject(const Char_t *name,const Char_t *path,Option_t *opt) method
  return FindObject(name,path,opt);
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
void St_DataSet::MarkAll()
{
  // Mark all members of this dataset
  Mark();
  St_DataSetIter nextMark(this,0);
  St_DataSet *set = 0;
  while ( (set = nextMark()) ) set->Mark();
}
//______________________________________________________________________________
void St_DataSet::UnMarkAll()
{
  // UnMark all members of this dataset
  Mark(kMark,kReset);
  St_DataSetIter nextMark(this,0);
  St_DataSet *set = 0;
  while ( (set = nextMark()) ) set->Mark(kMark,kReset);
}
//______________________________________________________________________________
void St_DataSet::InvertAllMarks()
{
 // Invert mark bit for all members of this dataset
  if (IsMarked()) Mark(kMark,kReset);
  else Mark();
  St_DataSetIter nextMark(this,0);
  St_DataSet *set = 0;
  while (( set = nextMark()) ) {
   if (set->IsMarked()) set->Mark(kMark,kReset);
   else set->Mark();
 }
}

//______________________________________________________________________________
Bool_t St_DataSet::IsEmpty() const 
{ 
   // return kTRUE if the "internal" collection has no member
   return First() ? kFALSE : kTRUE ;
}
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
St_DataSet  *St_DataSet::RemoveAt(Int_t idx)
{
  //
  // Remove object from the "idx" cell of this set and return
  // the pointer to the removed object if any
  //
  St_DataSet *set = 0;
  if (fList) {
      set = (St_DataSet *)fList->At(idx);
      fList->RemoveAt(idx);
  }
  return set;
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
// Purge  - deletes all "dummy" "Structural Members" those are not ended
//          up with some dataset with data inside (those return HasData() = 0)
//
// Purge does affect only the "Structural Members" and doesn't "Associated" ones
//

 if (!fList) return 0;
 TIter next(fList);   
 St_DataSet *son = 0;
   // Purge "Structural Members" only
   TList garbage;
   while ((son = (St_DataSet *)next())) {
     if (this == son->GetParent()) continue;
     //   mark the object is deleted from the St_DataSet dtor
     son->Purge();
     if (son->HasData() || son->GetListSize()) continue;
     delete son;
 }
 return 0;
}
//______________________________________________________________________________
void  St_DataSet::SetLock(int ){}
//______________________________________________________________________________
void  St_DataSet::SetParent(St_DataSet *parent){
//
//  Break the "parent" relationship with the current object parent if present
//  parent != 0   Makes this object the "Structural Member" 
//                of the "parent" dataset
//          = 0   Makes this object the "pure Associator", i.e it makes this 
//                object the "Structural Member" of NO other St_DataSet
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
void St_DataSet::Shunt(St_DataSet *newParent)
{
  //
  //  Remove the object from the original and add it to dataset 
  //  St_DataSet dataset   != 0  -  Make this object the "Structural Member"
  //                                of "dataset"
  //                        = 0  -  Make this object "Orphan"
  //
  if (fParent) fParent->Remove(this);
  SetParent(0);
  if (newParent) newParent->Add(this);
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

  SetTitle(set->GetTitle());
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
//______________________________________________________________________________
Int_t St_DataSet::Streamer(StBufferAbc &R__b){return 0;}

