//*CMZ :          12/07/98  18.27.27  by  Valery Fine(fine@mail.cern.ch)
//*-- Author :    Valery Fine(fine@mail.cern.ch)   03/07/98
// Copyright (C) Valery Fine (Valeri Faine) 1998. All right reserved 
//*KEEP,TDataset,T=C++.
#include "St_DataSetIter.h"
#include "St_DataSet.h"
//*KEEP,TBrowser.
#include "TBrowser.h"
//*KEND.
#include "TSystem.h"

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
 : fRootDataSet(l), fWorkingDataSet(l),fDepth(1),fMaxDepth(1)
  ,fDataSet(0)
  ,fNext(l ? new TIter(l->fList,dir):0)
{ }
 
//______________________________________________________________________________
 St_DataSetIter::St_DataSetIter(St_DataSet *l, Int_t depth, Bool_t dir)
 : fRootDataSet(l), fWorkingDataSet(l), fMaxDepth(depth),fDepth(1)
  ,fDataSet(0)
  ,fNext(l ? new TIter(l->fList,dir):0)
{ 
  // Create a DataSet iterator to pass all nodes of the 
  //     "depth"  levels
  //  of  St_DataSet *l  

  if (fMaxDepth != 1) {
     fNextSet[fDepth-1]= fNext;
     if (fMaxDepth > 100) fMaxDepth = 100;
  }
}

//______________________________________________________________________________
St_DataSetIter::~St_DataSetIter()
{
  if (fMaxDepth != 1) {
   for (int i = fDepth-1;i>=0;i--) {
     TIter *s = fNextSet[i];
     if (s) delete s;
   }
  }
  fDepth = 1;

  SafeDelete(fNext);
}
//______________________________________________________________________________
St_DataSet *St_DataSetIter::Add(St_DataSet *set, St_DataSet *dataset)
{
 ///////////////////////////////////////////////////////////////////////////////
 //                                                                           //
 // Add - adds the set to the dataset defined with the second parameters      //
 //                                                                           //
 // St_DataSet dataset != 0 - Add the set to the St_DataSet *dataset          //
 //                                                                           //
 //                     = 0 - (by default) to the current St_DataSet defined  //
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
// Remark:  The name = ".." has a special meaning.                 //
// ------   St_DataSetIter::Cd("..") returns the parent set        //
//          But one still can not use ".." as a legal part         // 
//          of the full path                                       //
/////////////////////////////////////////////////////////////////////
  St_DataSet *set = 0;
  if (strcmp(dirname,"..")) 
    set =  Next(dirname);
  else
    set = fWorkingDataSet->GetParent();
  if (set) fWorkingDataSet = set;
  return set;
}
 
//______________________________________________________________________________
St_DataSet *St_DataSetIter::Dir(Char_t *dirname)
{
//
// Print the names of the St_DataSet objects for the datatset named with "dirname"
// apart of St_DataSet::Ls()  this method prints one level only
//
  St_DataSet *set = (St_DataSet *)fWorkingDataSet;
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
St_DataSet *St_DataSetIter::Ls(const Char_t *dirname,Int_t depth) {
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
  if (set) set->ls(depth);
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
//
//  Remove the St_DataSet *dataset from the current dataset
//
  St_DataSet *set = dataset;
  if (set) {
    delete set;
    if (set == fRootDataSet) fRootDataSet = 0;
    fWorkingDataSet = fRootDataSet;
  }
  return (Int_t)dataset;
}
 
//______________________________________________________________________________
St_DataSet *St_DataSetIter::Next()
{
 //
 // returns the pointer the "next" St_DataSet object
 //         = 0 if all objects have been returned.
 //

  if (fMaxDepth==1) fDataSet = fNext ? (St_DataSet *)fNext->Next():0;
  else {

    // Check the whether the next level does exist 
    if (fDataSet && (fDepth < fMaxDepth || fMaxDepth ==0) ) 
    {
      // create the next level iterator, go deeper
      TList *list = fDataSet->GetListOfDataset();
      // Look for the next level
      if (list && list->GetSize() ) {
         fDepth++;
         if (fDepth >= 100) 
            Error("Next()"
                  ," to many nested levels of your St_DataSet has been detected");
         fNextSet[fDepth-1] = new TIter(list);
      }
    }

    // Pick the next object of the current level
    TIter *next = fNextSet[fDepth-1];
    if (next) {
      fDataSet = (St_DataSet *)next->Next();

      // Go upstair if the current one has been escaped
      if (!fDataSet) {
        // go backwards direction
        while (!fDataSet && fDepth) {
          fDepth--;
          delete next;
          next = fNextSet[fDepth-1];
          if (next) 
             fDataSet = (St_DataSet *)next->Next();
        }
      }
    }
  }
  return (St_DataSet *)fDataSet;
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
 //  Remark: This version can not treat any "special name" like "..", ".", etc //
 //  ------                                                                    //
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
void St_DataSetIter::Reset(St_DataSet *l, int depth)
{
  //
  // St_DataSet *l != 0 means the new start pointer
  //    depth      != 0 means the new value for the depth 
  //                    otherwise the privious one is used;
  //
  fDataSet = 0;
  if (fMaxDepth != 1) {
  // clean all interators
    for (int i = fDepth-1;i>=0;i--) {
      TIter *s = fNextSet[i];
      if (s) delete s;
    }
    fNext = 0; // this iterator has been deleted in the loop above
  }
  fDepth = 1;

  if (l) {
    fRootDataSet    = l;
    fWorkingDataSet = l;
    SafeDelete(fNext);
    if (fRootDataSet->fList)
             fNext = new TIter(fRootDataSet->fList);
  }
  else {
    if (fNext)
        fNext->Reset();
    else if (fRootDataSet->fList)
        fNext = new TIter(fRootDataSet->fList);
  }
  // set the new value of the maximum depth to bypass
  if (depth) fMaxDepth = depth;
}
//______________________________________________________________________________
St_DataSet *St_DataSetIter::Shunt(St_DataSet *set, St_DataSet *dataset)
{
 ///////////////////////////////////////////////////////////////////////////////
 //                                                                           //
 // Shunt - moves the set to the dataset defined with the second parameters   //
 //                                                                           //
 // St_DataSet dataset != 0 - Add the set to the St_DataSet *dataset          //
 //                                                                           //
 //                     = 0 - (by default) to the current St_DataSet defined  //
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
     s->Shunt(set);
     s = set;
  }
  else {
  //  make the coming dataset the current one for the iterator
     s = set;
     fRootDataSet    = s;
     fWorkingDataSet = s;
     if (fNext) {
       Error("Shunt","St_DataSetIter has been corrupted ;-!"); 
       delete fNext; 
       fNext = 0;
     }
     fNext = new TIter(s->fList);
  }
  return s;
}
 
//______________________________________________________________________________
St_DataSet *St_DataSetIter::Shunt(St_DataSet *dataset, const Char_t *path)
{
 ///////////////////////////////////////////////////////////////////////////////
 //                                                                           //
 // Shunt                                                                     //
 //                                                                           //
 // Char_t path != 0 - Move a St_DataSet dataset from its parent to           //
 //                    the St_DataSet dataset                                 //
 //                    defined with "path"                                    //
 //              = 0 - (by default) to the current St_DataSet defined         //
 //                    with fWorkingDataSet data member                       //
 //                                                                           //
 //  returns the dataset is success or ZERO pointer                           //
 //  =======                                                                  //
 //                                                                           //
 ///////////////////////////////////////////////////////////////////////////////
 if (!dataset) return 0;
 St_DataSet *set = 0;
 if (path && strlen(path)) set = Next(path);
 return Shunt(dataset,set);
}


