// @(#)root/table:$Id$
// Author: Valery Fine(fine@mail.cern.ch)   03/07/98

/*************************************************************************
 * Copyright (C) 1995-2000, Rene Brun and Fons Rademakers.               *
 * All rights reserved.                                                  *
 *                                                                       *
 * For the licensing terms see $ROOTSYS/LICENSE.                         *
 * For the list of contributors see $ROOTSYS/README/CREDITS.             *
 *************************************************************************/

#include "Riostream.h"
#include "TSystem.h"
#include "TDataSetIter.h"
#include "TDataSet.h"

#include "TROOT.h"
#include "TBrowser.h"

#include "TSystem.h"
#include <assert.h>

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TDataSet                                                             //
//                                                                      //
// TDataSet class is to create a special compound object-container:     //
//                                                                      //
// ==================================================================== //
//    TDataSet object ::= the "named" list of TDataSet objects          //
// ==================================================================== //
// where the "list" (the pointer to TList object) may contain no object //
//                                                                      //
//  TDataSet object has a back pointer to its "parent" TDataSet         //
//  object, the "character" *name* and "character" *title*              //
//                                                                      //
//  The service this class does provide is to help the user to build    //
//  and manage the hierarchy of their data but the data itself.         //
//                                                                      //
//  So it is not "Container" itself rather the basement (base class)    //
//  to built the containers.                                            //
//                                                                      //
//  One may derive the custom container classes from TDataSet.          //
//  See for example TObjectSet, TTable, TVolume, TFileSet               //
//  These classes  derived from TDataSet:                               //
//                                                                      //
//   Class Name                                                         //
//   ----------                                                         //
//  TObjectSet::public TDataSet - is a container for TObject            //
//  TTable::    public TDataSet - is a container for the array          //
//                                    of any "plain" C-structure        //
//  TNode::     public TDataSet - is a container for 3D objects         //
//  TMaker::     public TDataSet - is a container for STAR "control"    //
//                                    objects                           //
//   etc etc                                                            //
//                                                                      //
//  TDataSet class is a base class to implement the directory-like      //
//  data structures and maintain it via TDataSetIter class iterator     //
//                                                                      //
// TDataSet can be iterated using an iterator object (see TDataSetIter) //
//            or by TDataSet::Pass method (see below)                   //
//                                                                      //
//  Terms:    Dataset       - any object from the list above            //
//  =====     Member          is called "DataSet Member"                //
//                                                                      //
//          Structural      - the "Dataset Member" is its               //
//            member          "Structural member" if its "back pointer" //
//                            points to this object                     //
//                                                                      //
//           Dataset        - we will say this TDataSet object "OWNs"   //
//            Owner           (or is an OWNER / PARENT of ) another     //
//          (parent)          TDataSet object if the last one is its    //
//                            "Structural Member"                       //
//                                                                      //
//          Associated      - If some object is not "Structural member" //
//            member          of this object we will say it is an       //
//                            "Associated Member" of this dataset       //
//                                                                      //
//           Orphan         - If some dataset is a member of NO other   //
//           dataset          TDataSet object it is called an "orphan"  //
//                            dataset object                            //
//                                                                      //
// - Any TDataSet object may be "Owned" by one and only one another     //
//   TDataSet object if any.                                            //
//                                                                      //
// - Any TDataSet object can be the "Structural Member" of one and      //
//   only one another TDataSet                                          //
//                                                                      //
// - Any TDataSet object may be an "Associated Member" for any number   //
//   of other TDataSet objects if any                                   //
//                                                                      //
// - NAME issue:                                                        //
//   Each "dataset member" is in possession of some "alpha-numerical"   //
//   NAME as defined by TNamed class.                                   //
//   The NAME may contain any "printable" symbols but "SLASH" - "/"     //
//   The symbol "RIGHT SLASH" - "/" can not be used as any part of the  //
//   "DataSet Member" NAME                                              //
//    Any DataSet  can be found by its NAME with TDataSetIter object    //
//                                                                      //
// - TITLE issue:                                                       //
//   Each "dataset member" is in possession of the "alpha-numerical"    //
//   TITLE as defined by TNamed class. The meaning of the TITLE is      //
//   reserved for the derived classes to hold there some indetification //
//   that is special for that derived class.                            //
//                                                                      //
//   This means the user must be careful about  the "TDataSet           //
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
//           of another TDataSet object                                 //
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
//  The TDataSet class has several methods to control object('s)        //
//  memberships                                                         //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

TDataSet mainSet("DSMAIN");
TDataSet *TDataSet::fgMainSet = &mainSet;

ClassImp(TDataSet);

////////////////////////////////////////////////////////////////////////////////
///  std::cout << "ctor for " << GetName() << " - " << GetTitle() << std::endl;

TDataSet::TDataSet(const Char_t *name, TDataSet *parent, Bool_t arrayFlag)
           : TNamed(name,"TDataSet"),fParent(0),fList(0)
{
   if (name && strchr(name,'/')) {
      Error("TDataSet::TDataSet","dataset name (%s) cannot contain a slash", name);
      return;
   }
   fList =0; fParent=0;
   if (arrayFlag) SetBit(kArray);
   if (parent) parent->Add(this);
//   else AddMain(this);
}

////////////////////////////////////////////////////////////////////////////////
///return real parent

TDataSet *TDataSet::GetRealParent()
{
   TDataSet *p = GetParent();
   if (fgMainSet && p == fgMainSet) p = 0;
   return p;
}
////////////////////////////////////////////////////////////////////////////////
///
/// Creates TDataSet (clone) with a topology similar with TDataSet *pattern
///
///  Parameters:
///  -----------
///  pattern        - the pattern dataset
///  iopt = kStruct - clone only my structural links
///         kAll    - clone all links
///         kRefs   - clone only refs
///         kMarked - clone marked (not implemented yet) only
///
///   All new-created sets become the structural ones anyway.
///
///  std::cout << "ctor for " << GetName() << " - " << GetTitle() << std::endl;

TDataSet::TDataSet(const TDataSet &pattern,EDataSetPass iopt):TNamed(pattern.GetName(),pattern.GetTitle()),
fParent(0),fList(0)
{
   TDataSet *set = 0;
   TDataSetIter next((TDataSet *)&pattern);
   Bool_t optsel = (iopt == kStruct);
   Bool_t optall = (iopt == kAll);
   while ((set = next())) {
//              define the parent of the next set
      TDataSet *parent = set->GetParent();
      if ( optall || (optsel && parent == this) )
         Add((TDataSet *)(set->Clone()));
   }
}

////////////////////////////////////////////////////////////////////////////////
/// This copy ctor has been depricated (left for thwe sake of the backweard compatibility)

TDataSet::TDataSet(TNode &)
{
   assert(0);
}
////////////////////////////////////////////////////////////////////////////////
///              std::cout << "Default destructor for " << GetName() << " - " << GetTitle() << std::endl;

TDataSet::~TDataSet()
{
   Shunt(0); Delete();
}

////////////////////////////////////////////////////////////////////////////////
/// Create the internal container at once if any

void  TDataSet::MakeCollection()
{
   if (!fList)
      fList = TestBit(kArray) ? (TSeqCollection *)new TObjArray : (TSeqCollection *) new TList;
}

////////////////////////////////////////////////////////////////////////////////
///
/// Add TDataSet object at the "idx" position in ds
/// or at the end of the dataset
/// The final result is defined by either TList::AddAt or TObjArray::AddAt
/// methods
///

void TDataSet::AddAt(TDataSet *dataset,Int_t idx)
{
   if (!dataset) return;

   MakeCollection();

   // Check whether this new child has got any parent yet
   if (!dataset->GetRealParent()) dataset->SetParent(this);
   fList->AddAt(dataset,idx);
}

////////////////////////////////////////////////////////////////////////////////
///   !!!! Under construction !!!!!
/// Add TDataSet object at the "idx" position in ds
/// or at the end of the dataset
/// The final result is defined by either TList::AddAt or TObjArray::AddAt
/// methods
///

void TDataSet::AddAtAndExpand(TDataSet *dataset, Int_t idx)
{
   if (!dataset) return;

   MakeCollection();

   // Check whether this new child has got any parent yet
   if (!dataset->GetRealParent()) dataset->SetParent(this);
   if (TestBit(kArray)) ((TObjArray *) fList)->AddAtAndExpand(dataset,idx);
   else                  fList->AddAt(dataset,idx);
}

////////////////////////////////////////////////////////////////////////////////
/// Add TDataSet object at the end of the dataset list of this dataset

void TDataSet::AddLast(TDataSet *dataset)
{
   if (!dataset) return;

   MakeCollection();

   // Check whether this new child has got any parent yet
   if (!dataset->GetRealParent()) dataset->SetParent(this);
   fList->AddLast(dataset);
}

////////////////////////////////////////////////////////////////////////////////
/// Add TDataSet object at the beginning of the dataset list of this dataset

void TDataSet::AddFirst(TDataSet *dataset)
{
   if (!dataset) return;

   MakeCollection();

   // Check whether this new child has got any partent yet
   if (!dataset->GetRealParent()) dataset->SetParent(this);
   fList->AddFirst(dataset);
}

////////////////////////////////////////////////////////////////////////////////
/// Browse this dataset (called by TBrowser).

void TDataSet::Browse(TBrowser *b)
{
   TDataSetIter next(this);
   TDataSet *obj;
   if (b)
      while ((obj = next())) b->Add(obj,obj->GetName());
}

////////////////////////////////////////////////////////////////////////////////
/// the custom implementation fo the TObject::Clone

TObject *TDataSet::Clone(const char*) const
{
   return new TDataSet(*this);
}

////////////////////////////////////////////////////////////////////////////////
///
/// Delete - deletes the list of the TDataSet objects and all "Structural Members"
///          as well
///          This method doesn't affect the "Associated Members"
///

void TDataSet::Delete(Option_t *opt)
{
   if(opt){/*unused*/}

//      Delete list of the TDataSet
   TSeqCollection     *thisList = GetCollection();
   if (!thisList) return;
   fList = 0;
   TIter next(thisList);
   TDataSet *son = 0;
   //  Delete the "Structural Members" of this TDataSet only
   while ((son = (TDataSet *)next())) {
      if ( (!son->TObject::IsOnHeap()) || (this != son->TDataSet::GetParent()) ) continue;
      // mark the object is deleted from the TDataSet dtor or Delete method
      son->TDataSet::SetParent(0);
      if (son->TDataSet::Last()) { son->TDataSet::Delete(); }
      son->TObject::SetBit(kCanDelete);
      delete son;
   }
   //  Cleare list
   thisList->Clear("nodelete");
   delete thisList;
}

////////////////////////////////////////////////////////////////////////////////
/// Aliase for TDataSet::Find(const Char_t *path) method

TDataSet  *TDataSet::FindByPath(const Char_t *path) const
{
   return Find(path);
}

////////////////////////////////////////////////////////////////////////////////
///
/// Full description see: TDataSetIter::Find
///
/// Note. This method is quite expansive.
/// ----- It is done to simplify the user's code when one wants to find ONLY object.
///       If you need to find more then 1 object in this dataset,
///       regard using TDataSetIter class yourself.
///

TDataSet *TDataSet::Find(const Char_t *path) const
{
   TDataSetIter next((TDataSet*)this);
   return next.Find(path);
}

////////////////////////////////////////////////////////////////////////////////
///
/// Full description see: TDataSetIter::Find
///
/// Note. This is method is quite expansive.
/// ----- It is done to simplify the user's code when one wants to find ONLY object.
///       If you need to find more then 1 object in this dataset,
///       regard using TDataSetIter class yourself.
///

TDataSet *TDataSet::FindByName(const Char_t *name,const Char_t *path,Option_t *opt) const
{
   TDataSetIter next((TDataSet*)this);
   return next.FindByName(name,path,opt);
}

 ///////////////////////////////////////////////////////////////////////////////
 ///
 /// Full description see: TDataSetIter::Find
 ///
 /// Note. This method is quite expansive.
 /// ----- It is done to simplify the user's code when one wants to find ONLY object.
 ///       If you need to find more then 1 object in this dataset,
 ///       regard using TDataSetIter class yourself.
 ///

TDataSet *TDataSet::FindByTitle(const Char_t *title,const Char_t *path,Option_t *opt) const
{
   TDataSetIter next((TDataSet*)this);
   return next.FindByTitle(title,path,opt);
}

////////////////////////////////////////////////////////////////////////////////
///  Return the first object in the list. Returns 0 when list is empty.

TDataSet *TDataSet::First() const
{
   if (fList) return (TDataSet *)(fList->First());
   return 0;
}

////////////////////////////////////////////////////////////////////////////////
///add data set to main data set

void TDataSet::AddMain(TDataSet *set)
{
   if (fgMainSet && set) fgMainSet->AddFirst(set);
}

////////////////////////////////////////////////////////////////////////////////
///return pointer to the main dataset

TDataSet *TDataSet::GetMainSet()
{
   return fgMainSet;
}

////////////////////////////////////////////////////////////////////////////////
/// The depricated method (left here for the sake of the backward compatibility)

TObject *TDataSet::GetObject() const
{
   Print("***DUMMY GetObject***\n");
   return 0;
}

////////////////////////////////////////////////////////////////////////////////
/// Return the last object in the list. Returns 0 when list is empty.

TDataSet *TDataSet::Last() const
{
   if (fList) return (TDataSet *)(fList->Last());
   return 0;
}

////////////////////////////////////////////////////////////////////////////////
/// Return the object next to this one in the parent structure
/// This convinient but time-consuming. Don't use it in the inner loops

TDataSet *TDataSet::Next() const
{
   TDataSet *set = 0;
   TDataSet *parent = GetParent();
   if (parent) {
      TIter next(parent->GetCollection());
      // Find this object
      while ( (set = (TDataSet *)next()) && (set != this) ){}
      if (set) set = (TDataSet *)next();
   }
   return set;
}

////////////////////////////////////////////////////////////////////////////////
/// Return the object that is previous to this one in the parent structure
/// This convinient but time-consuming. Don't use it in the inner loops

TDataSet *TDataSet::Prev() const
{
   TDataSet *prev = 0;
   TDataSet *set  = 0;
   TDataSet *parent = GetParent();
   if (parent) {
      TIter next(parent->GetCollection());
      // Find this object
      while ( (set = (TDataSet *)next()) && (set != this) ){prev = set;}
      if (!set) prev = 0;
   }
   return prev;
}
////////////////////////////////////////////////////////////////////////////////
/// The depricated method (left here for the sake of the backward compatibility)

void TDataSet::SetObject(TObject * /*obj*/)
{
   Print("***DUMMY PutObject***\n");
}

////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////
///                                                                 //
///  ls(Option_t *option)                                           //
///                                                                 //
///    option       - defines the path to be listed                 //
///           = "*" -  means print all levels                       //
///                                                                 //
//////////////////////////////////////////////////////////////////////

void  TDataSet::ls(Option_t *option) const
{
   if (option && !strcmp(option,"*")) ls(Int_t(0));
   else {
      TDataSet *set = 0;
      if (option && strlen(option) > 0) {
         TDataSetIter local((TDataSet*)this);
         set = local(option);
      } else
         set = (TDataSet*)this;
      if (set) set->ls(Int_t(1));
      else
         if (option) Warning("ls","Dataset <%s> not found",option);
   }
}

////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////
///                                                                 //
///  ls(Int_t depth)                                                //
///                                                                 //
///  Prints the list of the this TDataSet.                          //
///                                                                 //
///  Parameter:                                                     //
///  =========                                                      //
///    Int_t depth >0 the number of levels to be printed            //
///               =0 all levels will be printed                     //
///            No par - ls() prints only level out                  //
///                                                                 //
//////////////////////////////////////////////////////////////////////

void TDataSet::ls(Int_t depth) const
{
   PrintContents();

   if (!fList || depth == 1 ) return;
   if (!depth) depth = 99999;

   TIter next(fList);
   TDataSet *d=0;
   while ((d = (TDataSet *)next())) {
      TROOT::IncreaseDirLevel();
      d->ls(depth-1);
      TROOT::DecreaseDirLevel();
   }
}
////////////////////////////////////////////////////////////////////////////////
/// apply the class default ctor to instantiate a new object of the same kind.
/// This is a base method to be overriden by the classes
/// derived from TDataSet (to support TDataSetIter::Mkdir for example)

TDataSet *TDataSet::Instance() const
{
   return instance();
}

////////////////////////////////////////////////////////////////////////////////
/// Compare the name of the TDataSet with "dirname"
/// ignorercase flags indicates whether the comparision is case sensitive

Bool_t TDataSet::IsThisDir(const Char_t *dirname,int len,int ignorecase) const
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

////////////////////////////////////////////////////////////////////////////////
/// Mark all members of this dataset

void TDataSet::MarkAll()
{
   Mark();
   TDataSetIter nextMark(this,0);
   TDataSet *set = 0;
   while ( (set = nextMark()) ) set->Mark();
}

////////////////////////////////////////////////////////////////////////////////
/// UnMark all members of this dataset

void TDataSet::UnMarkAll()
{
   Mark(kMark,kReset);
   TDataSetIter nextMark(this,0);
   TDataSet *set = 0;
   while ( (set = nextMark()) ) set->Mark(kMark,kReset);
}

////////////////////////////////////////////////////////////////////////////////
/// Invert mark bit for all members of this dataset

void TDataSet::InvertAllMarks()
{
   if (IsMarked()) Mark(kMark,kReset);
   else Mark();
   TDataSetIter nextMark(this,0);
   TDataSet *set = 0;
   while (( set = nextMark()) ) {
      if (set->IsMarked()) set->Mark(kMark,kReset);
      else set->Mark();
   }
}

////////////////////////////////////////////////////////////////////////////////
/// return kTRUE if the "internal" collection has no member

Bool_t TDataSet::IsEmpty() const
{
   return First() ? kFALSE : kTRUE ;
}

////////////////////////////////////////////////////////////////////////////////
/// Callback method to complete ls() method recursive loop
/// This is to allow to sepoarate navigation and the custom invormation
/// in the derived classes (see; TTable::PrintContents for example

void TDataSet::PrintContents(Option_t *opt) const {
   if (opt) { /* no used */ }
   Printf("%3d - %s\t%s\n",TROOT::GetDirLevel(),(const char*)Path(),(char*)GetTitle());
}

////////////////////////////////////////////////////////////////////////////////
/// return the full path of this data set

TString TDataSet::Path() const
{
   TString str;
   TDataSet *parent = GetParent();
   if (parent) {
      str = parent->Path();
      str += "/";
   }
   str +=  GetName();
   return str;
}

////////////////////////////////////////////////////////////////////////////////
/// Remiove the "set" from this TDataSet

void TDataSet::Remove(TDataSet *set)
{
   if (fList && set) {
      if (set->GetParent() == this) set->SetParent(0);
      fList->Remove(set);
   }

}

////////////////////////////////////////////////////////////////////////////////
///
/// Remove object from the "idx" cell of this set and return
/// the pointer to the removed object if any
///

TDataSet  *TDataSet::RemoveAt(Int_t idx)
{
   TDataSet *set = 0;
   if (fList) {
      set = (TDataSet *)fList->At(idx);
      fList->RemoveAt(idx);
      if (set && (set->GetParent() == this) ) set->SetParent(0);
   }
   return set;
}

////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////
///                                                                 //
/// Pass (callback,depth)                                           //
///                                                                 //
/// Calls callback(this) for all datasets those recursively         //
///                                                                 //
///  Parameter:                                                     //
///  =========                                                      //
///    Int_t depth >0 the number of levels to be passed             //
///                =0 all levels will be passed                     //
///                                                                 //
///  Return (this value mast be returned by the user's callback):   //
///  ======                                                         //
///  kContinue - continue passing                                   //
///  kPrune    - stop passing the current branch, go to the next one//
///  kUp       - stop passing, leave the current branch,            //
///              return to previous level and continue              //
///  kStop     - stop passing, leave all braches                    //
///                                                                 //
//////////////////////////////////////////////////////////////////////

TDataSet::EDataSetPass TDataSet::Pass(EDataSetPass ( *callback)(TDataSet *),Int_t depth)
{
   if (!callback) return kStop;

   EDataSetPass condition = callback(this);

   if (condition == kContinue){
      if (fList && depth != 1 ) {
         TIter next(fList);
         TDataSet *d=0;
         while ( (d = (TDataSet *)next()) ) {
            condition = d->Pass(callback, depth == 0 ? 0 : --depth);
            if (condition == kStop || condition == kUp) break;
         }
      }
   }
   return condition==kUp ? kContinue:condition;
}

////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////
///                                                                 //
/// Pass (callback,user,depth)                                      //
///                                                                 //
/// Calls callback(this,user) for all datasets those recursively    //
///                                                                 //
///  Parameter:                                                     //
///  =========                                                      //
///    Int_t depth >0 the number of levels to be passed             //
///                =0 all levels will be passed                     //
///                                                                 //
///  Return (this value mast be returned by the user's callback):   //
///  ======                                                         //
///  kContinue - continue passing                                   //
///  kPrune    - stop passing the current branch, go to the next one//
///  kUp       - stop passing, leave the current branch,            //
///              return to previous level and continue              //
///  kStop     - stop passing, leave all braches                    //
///                                                                 //
//////////////////////////////////////////////////////////////////////

TDataSet::EDataSetPass TDataSet::Pass(EDataSetPass ( *callback)(TDataSet *,void*),void *user,Int_t depth)
{
   if (!callback) return kStop;

   EDataSetPass condition = callback(this,user);

   if (condition == kContinue){
      if (fList && depth != 1 ) {
         TIter next(fList);
         TDataSet *d=0;
         while ((d = (TDataSet *)next())) {
            condition = d->Pass(callback, user, depth == 0 ? 0 : --depth);
            if (condition == kStop) break;
            if (condition == kUp  ) break;
         }
      }
   }
   return (condition==kUp) ? kContinue:condition;
}

////////////////////////////////////////////////////////////////////////////////
///
/// Purge  - deletes all "dummy" "Structural Members" those are not ended
///          up with some dataset with data inside (those return HasData() = 0)
///
/// Purge does affect only the "Structural Members" and doesn't "Associated" ones
///

Int_t TDataSet::Purge(Option_t *)
{
   if (!fList) return 0;
   TIter next(fList);
   TDataSet *son = 0;
   // Purge "Structural Members" only
   TList garbage;
   while ((son = (TDataSet *)next())) {
      if (this == son->GetParent()) continue;
      //   mark the object is deleted from the TDataSet dtor
      son->Purge();
      if (son->HasData() || son->GetListSize()) continue;
      delete son;
   }
   return 0;
}

////////////////////////////////////////////////////////////////////////////////
///
///  Break the "parent" relationship with the current object parent if present
///  parent != 0   Makes this object the "Structural Member"
///                of the "parent" dataset
///          = 0   Makes this object the "pure Associator", i.e it makes this
///                object the "Structural Member" of NO other TDataSet
///

void  TDataSet::SetParent(TDataSet *parent)
{
   fParent = parent;
}

////////////////////////////////////////////////////////////////////////////////
/// One should not use this method but TDataSet::Write instead
/// This method os left here for the sake of the backward compatibility
/// To Write object first we should temporary break the
/// the backward fParent pointer (otherwise ROOT follows this links
/// and will pull fParent out too.
///

void TDataSet::SetWrite()
{
   Write();
}

////////////////////////////////////////////////////////////////////////////////
///
///  Remove the object from the original and add it to dataset
///  TDataSet dataset   != 0  -  Make this object the "Structural Member"
///                                of "dataset"
///                        = 0  -  Make this object "Orphan"
///

void TDataSet::Shunt(TDataSet *newParent)
{
   if (fParent)   fParent->Remove(this);
   if (newParent) newParent->Add(this);
}

////////////////////////////////////////////////////////////////////////////////
///
/// Update this TDataSet with "set"
///
/// ATTENTION !!!
/// ---------
/// This method changes the parent relationships of the input "set"
///

void TDataSet::Update(TDataSet* set,UInt_t opt)
{
   if(opt){/*unused*/}
   if(!set) return;

   SetTitle(set->GetTitle());
   TDataSetIter nextnew(set);
   TDataSet *newset = 0;
   while((newset = nextnew())) {
      Bool_t found = kFALSE;
      //              Check whether this has the list of the sons
      if (fList) {
         TIter nextold(fList);
         const Char_t *newname = newset->GetName();
         TDataSet *oldset = 0;
         while ( ((oldset = (TDataSet *)nextold())!=0) && !found) {
            // if the "new" set does contain the dataset
            // with the same name as ours update it too
            // (We do not update itself (oldset == newset)
            if ( (oldset != newset) && oldset->IsThisDir(newname) ) {
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

////////////////////////////////////////////////////////////////////////////////
///
///  Update()
///
///  Recursively updates all tables for all nested datasets
///  in inverse order
///

void TDataSet::Update()
{
   TDataSetIter next(this);
   TDataSet *set = 0;
   while(( set = next())) set->Update();
}

////////////////////////////////////////////////////////////////////////////////
/// Sort recursively all members of the TDataSet with TList::Sort method

void TDataSet::Sort()
{
   TDataSetIter next(this,0);
   TDataSet *ds;
   TList *list;
   while ((ds=next())) {
      list = ds->GetList();
      if (!list) continue;
      list->Sort(); ds->Sort();
   }
}

////////////////////////////////////////////////////////////////////////////////
///
/// To Write object first we should temporary break the
/// the backward fParent pointer (otherwise ROOT follows this links
/// and will pull fParent out too.
///

Int_t TDataSet::Write(const char *name, Int_t option, Int_t bufsize)
{
   TDataSet *saveParent = fParent; // GetParent();
   fParent = 0;
   Int_t nbytes = TObject::Write(name,option, bufsize);
   fParent = saveParent;
   return nbytes;
}

////////////////////////////////////////////////////////////////////////////////
///
/// To Write object first we should temporary break the
/// the backward fParent pointer (otherwise ROOT follows this links
/// and will pull fParent out too.
///

Int_t TDataSet::Write(const char *name, Int_t option, Int_t bufsize) const
{
   TDataSet *saveParent = fParent; // GetParent();
   const_cast<TDataSet*>(this)->fParent = 0;
   Int_t nbytes = TObject::Write(name,option, bufsize);
   const_cast<TDataSet*>(this)->fParent = saveParent;
   return nbytes;
}
