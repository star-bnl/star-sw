#ifdef __CINT__

#pragma link off all globals;
#pragma link off all classes;
#pragma link off all functions;
#endif

//*CMZ :          13/08/98  18.27.27  by  Valery Fine(fine@bnl.gov)
//*-- Author :    Valery Fine(fine@mail.cern.ch)   13/08/98 

#ifndef _StRegistry_
#define _StRegistry_
 
 
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_DataSet                                                           //
//                                                                      //
// St_DataSet class is a base class to implement the directory-like     //
// data structures and maintain it via St_DataSetIter class iterator    //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
 
//*KEEP,TList.
#include "TList.h"
#include "TObjArray.h"
//*KEEP,TNamed.
#include "TNamed.h"
#include "TNode.h"
//*KEND.
 

class StRegistry 
{
 protected: 
    static TObjArray *fReg;				// pointer to container of containers
 public:
 StRegistry(){};
 static Int_t SetColl (TSeqCollection *coll); 		// Register new container
 static Int_t GetColl (const char *name); 		// get index of container
 static const char    *GetCollName (Int_t idx );	// get name of cont by index
 static TSeqCollection   *GetColl (Int_t idx );		// get name of cont by index
 static void  List();					// print list of registered conts    
 static ULong_t Ident(ULong_t colidx,ULong_t objidx){return colidx<<24 |objidx;};
 static void    Ident(ULong_t ident,ULong_t &colidx,ULong_t &objidx)
  {colidx = ident<<24;  objidx = ident & 0x00ffffff;};
};
 

class StStrArray : public TObjArray {
protected:
 TString fName;
 UInt_t fIdx;
 void Book(TObject* obj,int idx)
 { obj->SetUniqueID(StRegistry::Ident(fIdx,idx));};
public:
 StStrArray(const char *name=0)
 { if (!name) return; fName=name; fIdx=StRegistry::SetColl(this);};

 const Char_t *GetName() const {return fName;};

 virtual void AddFirst(TObject* obj)
 { TObjArray::AddFirst(obj); Book(obj,LowerBound());}

 virtual void AddLast(TObject* obj)
 { TObjArray::AddLast(obj); Book(obj,GetLast());}

 virtual void Add(TObject* obj){AddLast(obj);}

};

class StArray {
public:
int fUnit;	// Size of element in bytes
int fNEnt;       // Size of allocated array in elements
int fLast;      // index of Last filled element
char *fArray;   // array

public:

StArray(int iSize=4, int nEnt=10)
{ fUnit = iSize; fArray=0; fNEnt=0; fLast=0; Expand(nEnt);};
virtual void 		Expand(int n);
virtual void 		AddAt(void *w,Int_t idx, int size);
virtual void 		AddAt(Int_t w,   Int_t idx){AddAt(&w,idx,sizeof(Int_t   ));};
virtual void 		AddAt(ULong_t w, Int_t idx){AddAt(&w,idx,sizeof(ULong_t ));};
virtual void 		AddAt(Float_t w, Int_t idx){AddAt(&w,idx,sizeof(Float_t ));};
virtual void 		AddAt(Double_t w,Int_t idx){AddAt(&w,idx,sizeof(Double_t));};
virtual void   	       *AtV(int idx) const {return (idx>=fNEnt || idx <0)? 0: fArray+idx*fUnit;};
virtual Int_t   	AtI(int idx) const {void *v; return (v = AtV(idx))? *(Int_t*   )v:0;};
virtual Float_t 	AtF(int idx) const {void *v; return (v = AtV(idx))? *(Float_t* )v:0;};
virtual Double_t 	AtD(int idx) const {void *v; return (v = AtV(idx))? *(Double_t*)v:0;};
virtual Int_t		GetLast(){return fLast;};
};

class StRefArray : public StArray,public TObjArray
{
protected:
TSeqCollection *fCurrColl;
Int_t fCurrIdent;
public:
StRefArray():StArray(sizeof(void*),5)
{fCurrColl=0;fCurrIdent=0; TObjArray::Expand(0);}
virtual void AddAt(TObject *obj,Int_t idx);
virtual void Add(TObject *obj){AddLast(obj);};
virtual void AddLast(TObject *obj){AddAt(obj,StArray::fLast+1);};
virtual void AddFirst(TObject *obj){AddAt(obj,0);};

virtual TObject *At(Int_t idx) const;
virtual TObject *Last() const {return At(StArray::fLast);};
virtual TObject *First() const {return At(0);};
virtual void Clear(Option_t* option){StArray::Expand(0);};
};
#endif

