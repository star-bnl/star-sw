//*CMZ :          12/07/98  18.27.27  by  Valery Fine(fine@mail.cern.ch)
//*-- Author :    Valery Fine(fine@mail.cern.ch)   03/07/98
 
//*KEEP,TDataset,T=C++.
#include "St_DataSet.h"
//*KEEP,TBrowser.
#include "TBrowser.h"
//*KEND.
 
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_DataSet                                                           //
//                                                                      //
// St_DataSet class is a base class to implement the directory-like     //
// data structures and maintain it via St_DataSetIter class iterator    //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
 
ClassImp(St_DataSetIter)
 
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_DataSetIter                                                       //
//                                                                      //
// St_DataSetIter is a class iterator to navigate St_DataSet objects    //
// via 3 internal pointers :                                            //
//                                                                      //
//  1. fRootDataSet    - "root" dataset                                 //
//  2. fWorkingDataSet - Working dataset                                //
//  3. fNext           - TIter for the the list of the "root" dataset   //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
 
//______________________________________________________________________________
 St_DataSetIter::St_DataSetIter(St_DataSet *l, Bool_t dir)
 : fRootDataSet(l), fWorkingDataSet(l),
   fNext(l ? new TIter(l->fList,dir):0)
{ }
 
//______________________________________________________________________________
St_DataSet *St_DataSetIter::Add(St_DataSet *set, St_DataSet *dataset)
{
 ///////////////////////////////////////////////////////////////////////////////
 //                                                                           //
 // Add - adds the set to the dataset defined with the second parameters      //
 //                                                                           //
 // St_DataSet dataset != 0 - Add the set to the St_DataSet *dataset          //
 //                                                                           //
 //                   = 0 - (by default) to the current St_DataSet defined    //
 //                          with fWorkingDataSet data member                 //
 //                                                                           //
 //  returns  the pointer to set is success or ZERO poiner                    //
 //  =======                                                                  //
 //                                                                           //
 //  Note: If this St_DataSetIter is empty (i.e. Pwd() returns 0), the "set"  //
 //        becomes the "root" dataset of this iterator                        //                                                                         //
 ///////////////////////////////////////////////////////////////////////////////
 
  if (!set) return 0;
  St_DataSet *s =  dataset;
  if (!s) s = Pwd();
  if (s) {
     s->Add(set);
     s = set;
  }
  else {
  //  make the coming dataset the current one for the iterator
     s = set;
     fRootDataSet    = s;
     fWorkingDataSet = s;
     if (fNext) {
       Error("Add","St_DataSetIter has been corrupted ;-!"); 
       delete fNext; 
       fNext = 0;
     }
     fNext = new TIter(s->fList);
  }
  return s;
}
 
//______________________________________________________________________________
St_DataSet *St_DataSetIter::Add(St_DataSet *dataset, const Char_t *path)
{
 ///////////////////////////////////////////////////////////////////////////////
 //                                                                           //
 // Add                                                                       //
 //                                                                           //
 // Char_t path != 0 - Add a St_DataSet dataset to the St_DataSet dataset     //
 //                    defined with "path"                                    //
 //              = 0 - (by default) to the current St_DataSet defined         //
 //                     with fWorkingDataSet data member                      //
 //                                                                           //
 //  returns the dataset is success or ZERO pointer                           //
 //  =======                                                                  //
 //                                                                           //
 ///////////////////////////////////////////////////////////////////////////////
 if (!dataset) return 0;
 St_DataSet *set = 0;
 if (path && strlen(path)) set = Next(path);
 return Add(dataset,set);
}
 
//______________________________________________________________________________
St_DataSet *St_DataSetIter::Cd(const Char_t *dirname){
/////////////////////////////////////////////////////////////////////
//                                                                 //
// St_DataSet *St_DataSetIter::Cd(const Char_t *dirname)           //
//                                                                 //
// Change the current working directory to dirname                 //
//                                                                 //
// Returns the pointer to the new "working" St_DataSet             //
// =======   0,  if the new directory doesn't exist.               //
//                                                                 //
/////////////////////////////////////////////////////////////////////
  St_DataSet *set =  Next(dirname);
  if (set) fWorkingDataSet = set;
  return set;
}
 
//______________________________________________________________________________
St_DataSet *St_DataSetIter::Dir(Char_t *dirname)
{
//
// Print the names of the St_DataSet objects for the datatset named with "dirname"
// apart of TDatSet::Ls()  this method prints one level only
//
  St_DataSet *set = fWorkingDataSet;
  if (dirname) set = Next(dirname);
  if (set) set->ls();
  return set;
}
//______________________________________________________________________________
St_DataSet *St_DataSetIter::Ls(const Char_t *dirname,Option_t *opt) {
//
//   Ls(const Char_t *dirname,Option_t)
//
//   Prints the list of the St_DataSet defined with dirname
//
//   dirname     = 0   - prints the current dataset
//   dirname[0]  = '/' - print St_DataSet defined with dirname
//   dirname[0] != '/' - prints DataSet with respect of the current class
//
 
  St_DataSet *set= Next(dirname);
  if (set) set->ls(opt);
  return set;
}
 
//______________________________________________________________________________
St_DataSet *St_DataSetIter::Ls(const Char_t *dirname,Int_t deep) {
//
//   Ls(const Char_t *dirname,Option_t)
//
//   Prints the list of the St_DataSet defined with dirname
//
//   dirname     = 0   - prints the current dataset
//   dirname[0]  = '/' - print St_DataSet defined with dirname
//   dirname[0] != '/' - prints DataSet with respect of the current class
//
 
  St_DataSet *set= Next(dirname);
  if (set) set->ls(deep);
  return set;
}
 
//______________________________________________________________________________
St_DataSet *St_DataSetIter::Mkdir(const Char_t *dirname){
 St_DataSet *set = 0;
 set = Next(dirname,0,kTRUE);
 if (!fNext)  Reset();  // Create a new iterator
 return set;
}
 
//______________________________________________________________________________
Int_t St_DataSetIter::Rmdir(St_DataSet *dataset,Option_t *option){
  St_DataSet *set = dataset;
  if (set) {
    delete set;
    if (set == fRootDataSet) fRootDataSet = 0;
    fWorkingDataSet = fRootDataSet;
  }
  return (Int_t)dataset;
}
 
//______________________________________________________________________________
St_DataSet *St_DataSetIter::Next(const Char_t *path, St_DataSet *rootset,
                                 Bool_t mkdirflag)
{
 ////////////////////////////////////////////////////////////////////////////////
 //                                                                            //
 //           "path" ::= <relative path> | <absolute path> | <empty>           //
 //                                                                            //
 //  "relative path" ::= <dataset name> | <dataset name>/<dataset name>        //
 //                                                                            //
 //  "absolute path" ::= /<relative path>                                      //
 //  "empty"         ::= zero pointer | pointer to zero length string          //
 //                                                                            //
 // "relative path": the search is done against of fWorkingDataSet data mem    //
 // "absolute path": the search is done against of fRootDataSet    data mem    //
 // "empty path"   : no search is done just next St_DataSet is returned if any //
 //                                                                            //
 ////////////////////////////////////////////////////////////////////////////////
 
   if (!path || !strlen(path)) return rootset;
 
   St_DataSet *dataset = rootset;
   const Char_t pathseparator='/';
   const Char_t *startpos = path;
   const Char_t *seppos = startpos ? strchr(startpos,pathseparator) : 0;
 
 // delete all "blanks"
 
 //*-*
 //*-* define the path type
 //
   if ( startpos && seppos==startpos )
   {
      //*-* "absolute path":
 
      startpos = seppos+1;
      seppos = strchr(startpos,pathseparator);
      if (!dataset) dataset = fRootDataSet;
   }
   else
      if (!dataset)
           dataset = fWorkingDataSet;  //*-* "relative path"
 
   ULong_t ldirname = 0;
 
   if (seppos)
      ldirname=ULong_t(seppos-startpos);
   else
      ldirname = strlen(startpos);
 
   if (ldirname) {
      Char_t *dirname = new Char_t[ldirname+1];
      strncpy(dirname,startpos,ldirname);
      dirname[ldirname]=0;
 
      St_DataSet *thisset = dataset;
      Bool_t found = kFALSE;
      if (mkdirflag && !fRootDataSet) {
 
      // There is no "root" St_DataSet object
      //     Let's create it
        St_DataSet *set = new St_DataSet(dirname,dataset);
        if (dataset)
              dataset->Add(set);
        else
              dataset = set;
 
        thisset   = dataset;
        if (!fRootDataSet) {
          fRootDataSet       = dataset;
          fWorkingDataSet    = dataset;
        }
        found = kTRUE;
      }
      else {
        TList *list = dataset->GetListOfDataset();
        if (list) {
          TIter next(list);
          St_DataSet *obj = 0;
          while (!(found = dataset->IsThisDir(dirname)) && (obj = (St_DataSet *)next()) )
                dataset = obj;
        } else
          found = dataset->IsThisDir(dirname);
      }
      if (!found) dataset = 0;
      if (!found && mkdirflag) {
          found = kTRUE;
          dataset = new St_DataSet(dirname,thisset);
          if (thisset)
             thisset->Add(dataset);
      }
      // Go to the next recursive level
      if (found)
        dataset = Next(seppos,dataset,mkdirflag);
 
      delete [] dirname;
   }
   return dataset;
}
 
//______________________________________________________________________________
Bool_t St_DataSet::IsThisDir(const Char_t *dirname) const 
{
  return !strcmp(GetName(),dirname); 
}
 
//______________________________________________________________________________
void St_DataSetIter::Reset(St_DataSet *l)
{
  if (l) {
    fRootDataSet    = l;
    fWorkingDataSet = l;
    if (fNext) delete fNext; fNext = 0;
    if (fRootDataSet->fList)
             fNext = new TIter(fRootDataSet->fList);
  }
  else {
    if (fNext)
        fNext->Reset();
    else if (fRootDataSet->fList)
        fNext = new TIter(fRootDataSet->fList);
  }
}
//______________________________________________________________________________
//______________________________________________________________________________
 
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
St_DataSet::~St_DataSet(){
 // Delete list of the St_DataSet
  if (fMother) {
   // First we should break our relationship with the parent if any
    St_DataSet *parent = GetParent();
    if (parent) parent->Remove(this);
    if (fList) {
      fList->Delete();
      delete fList;
      fList = 0;
    }
   }
}
//______________________________________________________________________________
void St_DataSet::Add(St_DataSet *dataset)
{
  if (!dataset) return;
 
  if (!fList) {
       fList = new TList;
       if (fMother) fList->SetParent(fMother);
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
void  St_DataSet::ls(Option_t *option)
{
 /////////////////////////////////////////////////////////////////////
 //                                                                 //
 //  ls(Option_t *option)                                           //
 //                                                                 //
 //    option= "*" means print all levels                           //
 //                                                                 //
 /////////////////////////////////////////////////////////////////////
 
  if (option && !strcmp(option,"*")) ls(Int_t(0));
  else                               ls(Int_t(1));
}
 
//______________________________________________________________________________
void  St_DataSet::ls(Int_t deep)
{
 /////////////////////////////////////////////////////////////////////
 //                                                                 //
 //  ls(Int_t deep)                                                 //
 //                                                                 //
 //  Prints the list of the this St_DataSet.                        //
 //                                                                 //
 //  Parameter:                                                     //
 //  =========                                                      //
 //    Int_t deep >0 the number of levels to be printed             //
 //               =0 all levels will be printed                     //
 //            No par - ls() prints only level out                  //
 //                                                                 //
 /////////////////////////////////////////////////////////////////////
 
  TNamed::ls();
 
  if (fList && deep != 1 ) {
    TIter next(fList);
    St_DataSet *d=0;
    while (d = (St_DataSet *)next()) {
        IncreaseDirLevel();
        d->ls(deep == 0 ? 0 : --deep);
        DecreaseDirLevel();
    }
  }
}
#if 0
//______________________________________________________________________________
TTree *MakeTree(St_DataSet *dataset)
{
  // Creare a TTree object for the current Dataset
 
  strcat(path,GetName());
 
  if (fList && deep != 1 ) {
    TIter next(fList);
    St_DataSet *d=0;
    while (d = (St_DataSet *)next()) {
        IncreaseDirLevel();
        d->ls(deep == 0 ? 0 : --deep);
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
void St_DataSet::Remove(St_DataSet *set)
{
  if (fList && set) fList->Remove(set);
}
 
//______________________________________________________________________________
void  St_DataSet::SetParent(St_DataSet *parent){
//
//  Break the "parent" relationship with the current object parent if present
//  Set the new parent if any
//
  if (fList) {
    St_DataSet *oldparent = GetParent();
    // Each St_DataSet object must have only parent, therefore ...
    if (oldparent)
       oldparent->Remove(this);       // Break relations with the current parents
    fList->SetParent(parent);         // Establish a new relationships
  }
  SetMother(parent);                  // Adjust St_DataSet::fMother poiner as well
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
 
