#ifdef __CINT__

#pragma link off all globals;
#pragma link off all classes;
#pragma link off all functions;
#pragma link C++ class StObjArrayIter-;
#pragma link C++ class StObjArray-;
#pragma link C++ class StRefArray-;
#pragma link C++ class StStrArray-;
#pragma link C++ function testqwe();
#endif

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
 static void Clear(){if (fReg) fReg->Clear();};
 static Int_t SetColl (TObjArray *coll); 		// Register new container
 static Int_t GetColl (const char *name); 	 	// get index of container
 static const char    *GetCollName (Int_t idx );	// get name of cont by index
 static TObjArray     *GetColl (Int_t idx );		// get name of cont by index
 static void  List() ;					// print list of registered conts    
 static ULong_t Ident(ULong_t colidx,ULong_t objidx){return colidx<<24 |objidx;};
 static void    Ident(ULong_t ident,ULong_t &colidx,ULong_t &objidx)
  {colidx = ident<<24;  objidx = ident & 0x00ffffff;};
 static Int_t GetNColl(){return (fReg) ? fReg->GetLast()+1:0;};				// Number of collections
};
class StObjArrayIter;
 
class StObjArray : public TObjArray
{
public:
StObjArray(Int_t s = TCollection::kInitCapacity):TObjArray(s,0){};
virtual        ~StObjArray(){printf("~StObjArray %p\n",this);};
virtual void 	push_back(TObject *obj) {AddLast(obj);};
virtual void 	pop_back() {RemoveAt(GetLast());};
virtual Int_t   size() const {return GetLast()+1;};
virtual void    Resize(Int_t num);
virtual void    resize(Int_t num){Resize(num);};
virtual Int_t 	capacity() const {return Capacity();}
virtual TObject* back() const {return Last();};
virtual TObject* front() const {return First();};
virtual void 	clear(){Clear();};
virtual Bool_t 	empty() const {return GetLast()<LowerBound();};
virtual TObject* at(Int_t idx){return At(idx);};
virtual void    SetLast(Int_t last);
virtual const StObjArrayIter begin() const;
virtual const StObjArrayIter Begin() const;
virtual const StObjArrayIter end() const;
virtual const StObjArrayIter End() const;


ClassDef(StObjArray,1)
};

class StObjArrayIter : public TObjArrayIter
{
public:
StObjArrayIter(const TObjArray* col=0, Bool_t dir = kIterForward) :  TObjArrayIter(col, dir)
{ printf("StObjArrayIter=%p\n",this);};
StObjArrayIter(const StObjArrayIter &init) :  TObjArrayIter(0,kIterForward)
{ *this=init; printf("StObjArrayIter2=%p %p\n",this,&init);};

virtual       ~StObjArrayIter(){printf("~StObjArrayIter %p\n",this);}; 
virtual void   SetCursor(Int_t kursor);
virtual Int_t  GetCursor() const;
virtual void   SetDirection(Bool_t dir = kIterForward);
virtual Bool_t GetDirection() const;
virtual void   SetCollection(TObjArray *coll);
virtual TObject* operator*();
virtual StObjArrayIter &operator++();
virtual StObjArrayIter &operator--();
virtual void operator=(const StObjArrayIter &iter);
virtual Bool_t operator==(const StObjArrayIter &iter);


ClassDef(StObjArrayIter,1)
};

class StStrArray : public StObjArray {
protected:
 TString fName;
 ULong_t fIdx;

 void Book(TObject* obj,int idx)
 { obj->SetUniqueID(StRegistry::Ident(fIdx,idx));};
public:
 StStrArray(const char *name=0);

 virtual const Char_t *GetName() const {return fName;};
 virtual void          SetName(const char* name);

 virtual void AddFirst(TObject* obj)
 { TObjArray::AddFirst(obj); Book(obj,0);}

 virtual void AddLast(TObject* obj)
 { TObjArray::AddLast(obj); Book(obj,GetLast());}

 virtual void AddAt(TObject* obj, Int_t idx)
 { TObjArray::AddAt(obj,idx); Book(obj,idx);};
 
 virtual void AddAtAndExpand(TObject* obj, Int_t idx)
 { TObjArray::AddAtAndExpand(obj,idx); Book(obj,idx);};
 
ClassDef(StStrArray,1)
};


class StRefArray : public StObjArray
{
public:
StRefArray(Int_t s = TCollection::kInitCapacity)
:StObjArray(s){};

ClassDef(StRefArray,1)
};

void testqwe();

#endif

