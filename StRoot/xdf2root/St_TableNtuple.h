// $Id: St_TableNtuple.h,v 1.2 1999/02/17 22:54:15 genevb Exp $
// $Log: St_TableNtuple.h,v $
// Revision 1.2  1999/02/17 22:54:15  genevb
// Fixed errors when no tables/datasets found in St_TableNtuple
//
//
// Revision 1.1 1999/01/27 10:28:29 genevb
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_TableNtuple                                                       //
//                                                                      //
// St_TableNtuple is a class to convert STAR Tables into ntuples        //
// The class inherits from TTree, so it can be used just as a           //
// TTree would (Draw(), etc.). Columns are not made for table           //
// entries which are not basic numerical entities. Table entries        //
// which are arrays are spread into multiple columns with names         //
// given by the entry and index (e.g. chisq[3] => chisq0, chisq1,       //
// and chisq2). The class constructor defines the TTree and its         //
// TBranches. You must use the Fill() member function to fill the       //
// TTree from a table.                                                  //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef STAR_St_TableNtuple
#define STAR_St_TableNtuple

#ifdef __CINT__

#pragma link off all globals;
#pragma link off all classes;
#pragma link off all functions;

#pragma link C++ class St_TableNtuple;
#endif

#include "TTree.h"

class St_Table;
class TFile;
class St_XDFFile;
class TBrowser;

class St_TableNtuple : public TTree {
 private:
  TString   mTableClass; //  Name of table class
    Int_t   mNvar;       //  Number of columns used
     void **mArgs;       //! Array of pointers to variables
   TClass  *mClassPtr;   //! StAF table class used in the table
     enum   NumType {
    kNAN, kFloat, kInt, kLong, kShort, kDouble, kUInt, kULong, kUShort
  }        *mType;       //! Number type of table columns
    Int_t  *mOffset;     //! Offset values for pointers to table columns

 protected:
   virtual    void LearnTable(const St_Table &table, Bool_t buildTree=kFALSE, Int_t bufsize=25000);

 public:
                   St_TableNtuple();
                   St_TableNtuple(const St_Table &table, Int_t bufsize=25000);
                   ~St_TableNtuple();
   virtual   Int_t AddTFile(const Char_t *file, Char_t *dataset, Char_t *tname, Int_t firstEvent=1, Int_t nEvents=-1);
   virtual   Int_t AddTFile(TFile &f, Char_t *dataset, Char_t *tname, Int_t firstEvent=1, Int_t nEvents=-1);
   virtual   Int_t AddXDFFile(const Char_t *file, Char_t *dataset, Char_t *tname, Int_t firstEvent=1, Int_t nEvents=-1);
   virtual   Int_t AddXDFFile(St_XDFFile &f, Char_t *dataset, Char_t *tname, Int_t firstEvent=1, Int_t nEvents=-1);
   virtual    void Browse(TBrowser *b);
   virtual   Int_t Fill(const St_Table &table, Int_t firstRow=0, Int_t nRows=-1);
   virtual   Int_t GetNvar() const { return mNvar; }
   virtual Char_t* GetStTableClassName() const { return mTableClass.Data(); }
   virtual TClass* GetTableClass() const { return mClassPtr; }
   virtual    void PrintInfo();
   ClassDef(St_TableNtuple,1)
};

#endif
