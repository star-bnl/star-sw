//*CMZ :          13/08/98  18.27.27  by  Valery Fine(fine@bnl.gov)
//*-- Author :    Valery Fine(fine@mail.cern.ch)   13/08/98 
//***********************************************************************
//     C++ class library to create and manipulate hierarchy datasets
// * Copyright(c) 1997~1999  [BNL] Brookhaven National Laboratory, STAR, All rights reserved
// * Author                  Valerie Fine  (fine@bnl.gov)
// * Copyright(c) 1997~1999  Valerie Fine  (fine@bnl.gov)
// *
// * This program is distributed in the hope that it will be useful,
// * but WITHOUT ANY WARRANTY; without even the implied warranty of
// * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
// *
// * Permission to use, copy, modify and distribute this software and its
// * documentation for any purpose is hereby granted without fee,
// * provided that the above copyright notice appear in all copies and
// * that both that copyright notice and this permission notice appear
// * in supporting documentation.  Brookhaven National Laboratory makes no
// * representations about the suitability of this software for any
// * purpose.  It is provided "as is" without express or implied warranty.
// ************************************************************************

// $Id: St_DataSet.h,v 1.38 1999/11/04 18:01:19 fine Exp $
// $Log: St_DataSet.h,v $
// Revision 1.38  1999/11/04 18:01:19  fine
// Copyright text introduced
//
// Revision 1.37  1999/10/28 16:24:29  fine
// St_DataSet major correction: it may be built with TList (default) or with TObjArray
//
// Revision 1.36  1999/09/04 00:28:01  fine
// St_Table::NaN from VP and gloabl dataset have been introduced
//
// Revision 1.35  1999/08/06 15:24:35  fine
// UnMark method has been introduced
//
// Revision 1.34  1999/07/23 13:26:06  fine
// Several new methods to mark the datasets have been introduced
//
// Revision 1.33  1999/06/26 01:40:55  fisyak
// Add Valery's abstract buffer
//
// Revision 1.32  1999/06/09 22:08:53  fine
// Comment clean up
//
#ifndef ROOT_St_DataSet
#define ROOT_St_DataSet
 
 
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_DataSet                                                           //
//                                                                      //
// St_DataSet class is a base class to implement the directory-like     //
// data structures and maintain it via St_DataSetIter class iterator    //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
 

#include "TList.h"
#include "TObjArray.h"

#include "TNamed.h"
#include "TNode.h"

 
class St_DataSetIter;
class TBrowser;
class StBufferAbc;

//----- dataset flags
enum ESetBits {
     kMark        = BIT(22)   // if object is marked
    ,kArray       = BIT(20)   // if object has StObjArray inside
};

enum EBitOpt { 
               kSet   = kTRUE,
               kReset = kFALSE
             };

// The control codes to navigate the St_DataSet structure via St_DataSet::Pass method

typedef enum {
      kContinue,  // continue passing 
      kPrune,     // stop passing of the current branch but continue with the next one if any
      kStop,      // break passing
      kUp,        // break passing, return to the previous level, then continue
      kStruct,    // work with structural links only
      kAll,       // work with all links 
      kRefs,      // work with refs links only
      kMarked,    // work with marked links only
      kLock = BIT(21) // DataSet is locked
     } EDataSetPass;

class St_DataSet : public TNamed
{
 friend class St_DataSetIter;
 friend class St_DataSetTree;
 private:
    void operator=(const St_DataSet &){}
 protected: 
    static St_DataSet  *fgMainSet; // pointer the main dataset;    
    St_DataSet         *fParent;   // pointer to mother of the directory
    TSeqCollection     *fList;     // List of the the the objects included into this dataset
    virtual void SetMother(TObject *mother) {SetParent((St_DataSet*)mother);}
    St_DataSet(const Char_t *name,const Char_t *title):
    TNamed(name,title),fParent(0),fList(0){} // to support TDictionary
    void AddMain(St_DataSet *set);
    static EDataSetPass SortIt(St_DataSet *ds);
    static EDataSetPass SortIt(St_DataSet *ds,void *user);
    St_DataSet *GetRealParent();
    void MakeCollection();

     
 public:
 
    St_DataSet(const Char_t *name="", St_DataSet *parent=0,  Bool_t arrayFlag = kFALSE);
    St_DataSet(const St_DataSet &src,EDataSetPass iopt=kAll);
    St_DataSet(TNode &src); 
    virtual ~St_DataSet();
    virtual void         Add(St_DataSet *dataset);
    virtual void         AddAt(St_DataSet *dataset,Int_t idx=0);
    virtual void         AddAtAndExpand(St_DataSet *dataset, Int_t idx=0);
    virtual void         AddFirst(St_DataSet *dataset);
    virtual void         AddLast(St_DataSet *dataset);
            St_DataSet  *At(Int_t idx) const;
    virtual Int_t        Audit(Option_t *opt=0){if(opt){/*touch*/};return 0;};
    virtual void         Browse(TBrowser *b);
    virtual TObject     *Clone();
    virtual void         Delete(Option_t *opt="");   
    virtual St_DataSet  *Find(const Char_t *path) const;
    virtual St_DataSet  *FindObject(const Char_t *name,const Char_t *path="",Option_t *opt="") const;
    virtual St_DataSet  *FindByPath(const Char_t *path) const;
    virtual St_DataSet  *FindByName(const Char_t *name,const Char_t *path="",Option_t *opt="") const;
    virtual St_DataSet  *First() const;
            TObjArray   *GetObjArray() const { return (TObjArray *)fList; }
            TSeqCollection *GetCollection() const { return (TSeqCollection *)fList; }
            TList       *GetList()   const { return (TList *)fList; }
    virtual Int_t        GetListSize() const;
    static  St_DataSet  *GetMainSet(){ return fgMainSet;}
            TObject     *GetMother() const { return (TObject*)GetParent();}
    virtual TObject     *GetObject() const {printf("***DUMMY GetObject***\n");return 0;}
    virtual St_DataSet  *GetParent() const { return fParent;}
    virtual Long_t       HasData() const {return 0;} 	// Check whether this dataset has extra "data-members"
    virtual TString      Path() const;                  // return the "full" path of this dataset
    virtual EDataSetPass Pass(EDataSetPass ( *callback)(St_DataSet *),Int_t depth=0);
    virtual EDataSetPass Pass(EDataSetPass ( *callback)(St_DataSet *,void*),void *user,Int_t depth=0);
    virtual Int_t        Purge(Option_t *opt="");   
    virtual void         Remove(St_DataSet *set);
    virtual St_DataSet  *RemoveAt(Int_t idx);
    virtual void         SetLock(Int_t lock);
    virtual void         SetMother(St_DataSet *parent=0){SetParent(parent);};
    virtual void         SetObject(TObject *obj){printf("***DUMMY PutObject***%p\n",obj);}
    virtual void         SetParent(St_DataSet *parent=0);
    virtual void         SetWrite();
    virtual void         Shunt(St_DataSet *newParent=0);
    virtual void         Sort();			//Sort objects in lexical order
    virtual Bool_t       IsEmpty() const;
    virtual Bool_t       IsFolder() const {return kTRUE;}
    virtual Bool_t       IsLocked() const ;
    virtual Bool_t       IsMarked() const ;
    virtual Bool_t       IsThisDir(const Char_t *dirname,int len=-1,int ignorecase=0) const ;
    virtual St_DataSet  *Last() const;
    virtual void         ls(Option_t *option="")  const;      // Option "*" means print all levels
    virtual void         ls(Int_t depth)  const;              // Print the "depth" levels of this datatset
            void         Mark();                              // *MENU*
            void         UnMark();                            // *MENU*
            void         MarkAll();                           // *MENU*
            void         UnMarkAll();                         // *MENU*
            void         InvertAllMarks();                    // *MENU*
            void         Mark(UInt_t flag,EBitOpt reset=kSet);
    virtual Int_t        Streamer(StBufferAbc &R__b);
    virtual void         Update();                            // Update dataset
    virtual void         Update(St_DataSet *set,UInt_t opt=0);// Update this dataset with the new one
    ClassDef(St_DataSet,1)
};

inline void        St_DataSet::Add(St_DataSet *dataset){ AddLast(dataset); }
inline void        St_DataSet::AddMain(St_DataSet *set){ if (fgMainSet && set) fgMainSet->AddFirst(set);}
inline St_DataSet *St_DataSet::At(Int_t idx) const {return fList ? (St_DataSet *)fList->At(idx) : 0;  }
inline Int_t       St_DataSet::GetListSize() const {return (fList) ? fList->GetSize():0;}
inline Bool_t      St_DataSet::IsMarked() const { return TestBit(kMark); }
inline void        St_DataSet::Mark()     { Mark(kMark,kSet); }
inline void        St_DataSet::UnMark()   { Mark(kMark,kReset); }
inline void        St_DataSet::Mark(UInt_t flag,EBitOpt reset){ SetBit(flag,reset); }

#endif
