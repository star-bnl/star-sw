//*CMZ :          13/08/98  18.27.27  by  Valery Fine(fine@bnl.gov)
//*-- Author :    Valery Fine(fine@mail.cern.ch)   13/08/98 

#ifndef ROOT_St_DataSetIter
#define ROOT_St_DataSetIter
 

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_DataSetIter                                                       //
//                                                                      //
// Iterator of St_DataSet lists.                                        //
//                                                                      //
// Provides "standard" features of the TIter class for St_DataSet object//
//                             and                                      //
// allows navigating St_DataSet structure using the custom "directory"  //
//    notation (see St_DataSet::Find(const Char *path) method)          //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
 
//*KEEP,TList.
#include "TList.h"
//*KEEP,TNamed.
#include "TNamed.h"
//*KEND.

#include "St_DataSet.h"

class St_DataSetIter : public TObject{
protected:
   TIter           *fNext;            // "standard" ROOT iterator for containers
   TIter           *fNextSet[100];    // the list of the TList iterators to bypass the whole dataset
   Int_t            fDepth;           // the current depth of the passing
   Int_t            fMaxDepth;        // the max depth of the passing (=1 by default)
   
   const St_DataSet *fDataSet;        // Pointer to the last selected St_DataSet
   St_DataSet       *fRootDataSet;    // Pointer to the root St_DataSet
   St_DataSet       *fWorkingDataSet; // Pointer to the working St_DataSet
   St_DataSet       *NextDataSet(TIter &next);
   St_DataSet       *NextDataSet(Int_t nDataSet);
public:
  St_DataSetIter(St_DataSet *l=0, Int_t depth=1, Bool_t dir=kIterForward);
  St_DataSetIter(St_DataSet *l, Bool_t dir);
  virtual         ~St_DataSetIter();
 
  virtual St_DataSet    *Add(St_DataSet *set){return Add(set,(St_DataSet *)0);}
  virtual St_DataSet    *Add(St_DataSet *set, const Char_t *path);
  virtual St_DataSet    *Add(St_DataSet *set, St_DataSet *dataset);
 
  virtual St_DataSet    *Cd(const Char_t *dirname);
  virtual St_DataSet    *operator()( EDataSetPass mode=kContinue ) {return  Next(mode);}
  virtual St_DataSet    *operator()(const Char_t *path) { return Find(path); }
  virtual St_DataSet    *operator[](const Char_t *path);
  virtual Int_t          GetDepth() const {return fDepth;}
  virtual St_DataSet    *Cwd() const {return fWorkingDataSet;}
  virtual St_DataSet    *Dir(Char_t *dirname);
  virtual Int_t          Du() const;            // summarize dataset usage
  virtual Int_t          Df() const {return 0;} // report number of free "table" blocks. 

  virtual St_DataSet    *Find(const Char_t *path, St_DataSet *rootset=0,Bool_t mkdir=kFALSE);
  virtual St_DataSet    *FindByPath(const Char_t *path, St_DataSet *rootset=0,Bool_t mkdir=kFALSE);
  virtual St_DataSet    *FindObject(const Char_t *name,const Char_t *path="",Option_t *opt="");
  virtual St_DataSet    *FindByName(const Char_t *name,const Char_t *path="",Option_t *opt="");
  virtual St_DataSet    *FindObject(St_DataSet *set,const Char_t *path,Option_t *opt="");
  virtual Int_t          Flag(UInt_t flag=kMark,EBitOpt reset=kSet){return Flag((St_DataSet *)0,flag,reset);}
  virtual Int_t          Flag(const Char_t *path,UInt_t flag=kMark,EBitOpt reset=kSet);
  virtual Int_t          Flag(St_DataSet *dataset,UInt_t flag=kMark,EBitOpt reset=kSet);

  virtual St_DataSet    *Ls(const Char_t *dirname="",Option_t *opt="");
  virtual St_DataSet    *Ls(const Char_t *dirname,Int_t depth);
  virtual St_DataSet    *ls(const Char_t *dirname="",Option_t *opt="")   {return Ls(dirname,opt);}
  virtual St_DataSet    *ls(const Char_t *dirname,Int_t depth){return Ls(dirname,depth);}
  virtual St_DataSet    *Mkdir(const Char_t *dirname);
  virtual St_DataSet    *Md(const Char_t *dirname)                       {return Mkdir(dirname);}
  virtual TString        Path(const Char_t *path)                        {St_DataSet *set = Find(path); return set ? TString (""):set->Path();}
  virtual TString        Path() {return fWorkingDataSet ? TString ("") : fWorkingDataSet->Path();}
  virtual St_DataSet    *Pwd(Option_t *opt="") const                     {if (Cwd()) Cwd()->ls(opt); return Cwd();}
  virtual Int_t          Rmdir(St_DataSet *dataset,Option_t *option="");
  virtual Int_t          Rmdir(const Char_t *dirname,Option_t *option=""){return Rmdir(Find(dirname),option);}
  virtual Int_t          Rd(const Char_t *dirname,Option_t *option="")   {return Rmdir(Find(dirname),option);}
 
  virtual St_DataSet    *Shunt(St_DataSet *set){return Shunt(set,(St_DataSet *)0);}
  virtual St_DataSet    *Shunt(St_DataSet *set, const Char_t *path);
  virtual St_DataSet    *Shunt(St_DataSet *set, St_DataSet *dataset);
 
  virtual St_DataSet    *Next( EDataSetPass mode=kContinue);
  virtual St_DataSet    *Next(const Char_t *path, St_DataSet *rootset=0,Bool_t mkdir=kFALSE){return Find(path,rootset,mkdir);}
  virtual void           Notify(St_DataSet *dataset);
  const Option_t *GetOption() const                                      { return fNext ? fNext->GetOption():0; }
  virtual void           Reset(St_DataSet *l=0,Int_t depth=0);
  ClassDef(St_DataSetIter,0)
};

#endif

