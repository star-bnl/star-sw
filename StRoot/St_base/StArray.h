#if 0
#ifdef __CINT__

#pragma link off all globals;
#pragma link off all classes;
#pragma link off all functions;
#pragma link C++ class StRegistry-;
#pragma link C++ class StObjArrayIter-;
#pragma link C++ class StObjArray-;
#pragma link C++ class StRefArray-;
#pragma link C++ class StStrArray-;
#pragma link C++ class QWERTY;
#pragma link C++ class QWERTYRef-;
#pragma link C++ class QWERTYStr-;
#pragma link C++ class QWERTYIter-;
#pragma link C++ function testqwe();
#endif //__CINT__
#endif // 0
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
 
#include <assert.h>
#include "TList.h"
#include "TObjArray.h"
#include "TNamed.h"
#include "StObject.h"

class StObject;
class StObjArray;
class StStrArray;
class StRefArray;
class StRegistry : public TObject
{
 protected: 
    static TObjArray *fReg;				// pointer to container of containers
    static TList     *fNon;				// pointer to container of non init containers
 public:
 StRegistry(){};
 static void Clear();
 static Int_t SetColl (StStrArray *coll); 		// Register new container
 static void  RemColl (StStrArray *coll); 		// UnRegister container
 static Int_t GetColl (const char *name); 	 	// get index of container
 static const char    *GetCollName (Int_t idx );	// get name of cont by index
 static StStrArray    *GetColl (Int_t idx );		// get name of cont by index
 static void  List() ;					// print list of registered conts    
 static ULong_t Ident(ULong_t colidx,ULong_t objidx);
 static void    Ident(ULong_t ident,ULong_t &colidx,ULong_t &objidx);
 static Int_t GetNColl();				// Number of collections
 static void  AddNon(StRefArray *coll);
 static void  Init();
ClassDef(StRegistry,0)
};
class StObjArrayIter;
//	Intermediate class differs from TObjArray only by one supressed method [] 
class StTObjArray : public TSeqCollection {

    protected:
    TObjArray *fArr;
    public:

                         StTObjArray(Int_t s = TCollection::kInitCapacity, Int_t lowerBound = 0)
                         { fArr = new TObjArray(s,lowerBound); }
                         StTObjArray(const StTObjArray& a){ fArr = new TObjArray(*a.fArr);}
            virtual  ~StTObjArray(){delete fArr;}
            virtual void operator =(const StTObjArray &a){ delete fArr;fArr = new TObjArray(*a.fArr);} 
            virtual void Add(TObject* obj){fArr->Add(obj);}
            virtual void AddAfter(TObject* after, TObject* obj){fArr->AddAfter(after,obj);}
            virtual void AddAt(TObject* obj, Int_t idx){fArr->AddAt(obj,idx);}
            virtual void AddAtAndExpand(TObject* obj, Int_t idx){fArr->AddAtAndExpand(obj,idx);}
            virtual Int_t AddAtFree(TObject* obj){return fArr->AddAtFree(obj);}
            virtual void AddBefore(TObject* before, TObject* obj){fArr->AddBefore(before,obj);}
            virtual void AddFirst(TObject* obj){fArr->AddFirst(obj);}
            virtual void AddLast(TObject* obj){fArr->AddLast(obj);}
        virtual TObject* After(TObject* obj) const {return fArr->After(obj);}
        virtual TObject* At(Int_t idx) const {return fArr->At(idx);}
        virtual TObject* Before(TObject* obj) const {return fArr->Before(obj) ;}
           virtual Int_t Capacity() const { return fArr->Capacity();}
            virtual Int_t BinarySearch(TObject* obj, Int_t upto = kMaxInt){return fArr->BinarySearch(obj,upto);}
            virtual void Clear(Option_t* option=""){fArr->Clear(option);}
            virtual void Compress(){fArr->Compress();}
            virtual void Delete(Option_t* option=""){fArr->Delete(option);}
            virtual void Expand(Int_t newSize){fArr->Expand(newSize);}
        virtual TObject* First() const {return fArr->First() ;}
                   Int_t GetEntries() const {return fArr->GetEntries();}
                   Int_t GetEntriesFast() const {return fArr->GetEntriesFast();}
                   Int_t GetLast() const {return fArr->GetLast();}
                   Int_t GetSize() const {return fArr->GetSize();}
           virtual Int_t IndexOf(TObject* obj) const {return fArr->IndexOf(obj);}
        virtual TObject* Last() const {return fArr->Last();}
                   Int_t LowerBound() const {return fArr->LowerBound();}
      virtual TIterator* MakeIterator(Bool_t dir = kIterForward) const {return fArr->MakeIterator(dir);}
//VP supressed       virtual TObject*& operator[](Int_t i)
        virtual TObject* Remove(TObject* obj){return fArr->Remove(obj);}
            virtual void RemoveAt(Int_t idx){fArr->RemoveAt(idx);}
            virtual void SetLast(Int_t last){fArr->SetLast (last);}
            virtual void Sort(Int_t upto = kMaxInt){fArr->Sort(upto);}
                TObject* UncheckedAt(Int_t i) const {return fArr->UncheckedAt(i);}
            virtual TObjArray* GetTObjArray() const {return fArr;}

ClassDef(StTObjArray,1)
};

class StObjArray : public StTObjArray
{
public:
            StObjArray(Int_t s = TCollection::kInitCapacity):StTObjArray(s,0){};
            StObjArray(const StObjArray& from):StTObjArray(from){};
                
            
virtual        ~StObjArray(){};

virtual void 	Browse(TBrowser *b);
virtual void 	Clean(StObject *obj=0);
virtual void 	Clean(StObjArrayIter *iter);
virtual void 	Erase(StObject *obj);
virtual void 	Erase(StObjArrayIter *iter);

virtual void 	push_back(const TObject *obj);
virtual void 	pop_back();
virtual UInt_t  size() const;
virtual void    Resize(Int_t num);
virtual void    resize(Int_t num);
virtual Int_t 	capacity() const ;
virtual TObject* Back() const ;
virtual TObject* Front() const;
virtual void 	clear();
virtual Bool_t 	empty() const ;
virtual void    SetLast(Int_t last);
virtual const TIterator *Begin() const;
virtual const TIterator *End() const;
virtual TIterator* MakeIterator(Bool_t dir = kIterForward) const;
virtual TObject** GetCell(Int_t idx) const;

ClassDef(StObjArray,1)
};

class StObjArrayIter : public TObjArrayIter
{
public:
StObjArrayIter(const StObjArray* col=0, Bool_t dir = kIterForward) :  TObjArrayIter(0, dir)
{ 
  fColl = col; if (col) { SetCollection(fColl); Reset();}
};

StObjArrayIter(const StObjArrayIter &init) :  TObjArrayIter(0,kIterForward)
{ *this=init;};

virtual       ~StObjArrayIter(){}; 
virtual void   SetCursor(Int_t kursor);
virtual Int_t  GetCursor() const;
virtual void   SetDirection(Bool_t dir = kIterForward);
virtual Bool_t GetDirection() const;
virtual void   SetCollection(const TCollection *coll);
virtual TObject* GetObject() const;
virtual StObjArrayIter &operator++();
virtual StObjArrayIter &operator--();
virtual StObjArrayIter &operator++(int);
virtual StObjArrayIter &operator--(int);
virtual void operator=(const StObjArrayIter &iter);
virtual Bool_t operator==(const StObjArrayIter &iter) const;
virtual Bool_t operator!=(const StObjArrayIter &iter) const;

protected:
const StObjArray* fColl;
ClassDef(StObjArrayIter,1)
};

class StStrArray : public StObjArray {
protected:
 TString fName;
 TString fIDName;
 ULong_t fIdx;

 void Book(TObject* obj,int idx);

public:
 StStrArray(const Char_t *name=0, Int_t s=0);
 StStrArray(const StStrArray &from);
 virtual ~StStrArray();
 virtual void operator =(const StStrArray &a); 

 virtual const Char_t *GetName() const ;
 virtual void          SetName(const char* name) ; 
 virtual const Char_t *GetIDName() const ;
 virtual void          SetIDName(const char* name);

 virtual void AddFirst(TObject* obj);

 virtual void AddLast(TObject* obj);

 virtual void AddAt(TObject* obj, Int_t idx);
 
 virtual void AddAtAndExpand(TObject* obj, Int_t idx);
 
ClassDef(StStrArray,1)
};


class StRefArray : public StObjArray
{
public:
StRefArray(Int_t s = TCollection::kInitCapacity):StObjArray(s){};
StRefArray(const StRefArray &from):StObjArray(from){};
virtual void Decode();
virtual ~StRefArray(){};
ClassDef(StRefArray,1)
};


#ifndef __CINT__
#define StCollectionDef(QWERTY) \
class St ## QWERTY;\
class St ## QWERTY ## Iterator;\
class StPtrVec ## QWERTY : public StRefArray \
{ \
public: \
StPtrVec ## QWERTY(Int_t s = TCollection::kInitCapacity);\
StPtrVec ## QWERTY(const StPtrVec ## QWERTY &from);\
virtual        ~StPtrVec ## QWERTY ##(){};\
virtual void push_back(const St ## QWERTY &Obj);\
virtual void push_back(const St ## QWERTY *Obj);\
virtual St ## QWERTY ## * back() const ;\
virtual St ## QWERTY ## * front() const;\
virtual const St ## QWERTY ## Iterator begin() const ;\
virtual const St ## QWERTY ## Iterator end()   const ;\
virtual TIterator* MakeIterator(Bool_t dir = kIterForward) const;\
virtual void clean(St ## QWERTY *obj=0);\
virtual void erase(St ## QWERTY *obj=0);\
virtual void clean(St ## QWERTY ## Iterator *iter);\
virtual void erase(St ## QWERTY ## Iterator *iter);\
virtual void clean(St ## QWERTY &obj);\
virtual void erase(St ## QWERTY &obj);\
virtual void clean(St ## QWERTY ## Iterator &iter);\
virtual void erase(St ## QWERTY ## Iterator &iter);\
virtual St ## QWERTY*& operator[](Int_t i) const;\
ClassDef(StPtrVec ## QWERTY ##,1) \
};\
class StSPtrVec ## QWERTY : public StStrArray \
{ \
public: \
StSPtrVec ## QWERTY(const Char_t *name=0,Int_t s = TCollection::kInitCapacity);\
StSPtrVec ## QWERTY(const StSPtrVec ## QWERTY &from);\
virtual void push_back(const St ## QWERTY &Obj);\
virtual void push_back(const St ## QWERTY *Obj);\
virtual St ## QWERTY ## * back() const;\
virtual St ## QWERTY ## * front() const;\
virtual const St ## QWERTY ## Iterator begin() const ;\
virtual const St ## QWERTY ## Iterator end()   const ;\
virtual TIterator* MakeIterator(Bool_t dir = kIterForward) const;\
virtual void clean(St ## QWERTY *obj=0);\
virtual void erase(St ## QWERTY *obj=0);\
virtual void clean(St ## QWERTY ## Iterator *iter);\
virtual void erase(St ## QWERTY ## Iterator *iter);\
virtual void clean(St ## QWERTY &obj);\
virtual void erase(St ## QWERTY &obj);\
virtual void clean(St ## QWERTY ## Iterator &iter);\
virtual void erase(St ## QWERTY ## Iterator &iter);\
virtual St ## QWERTY*& operator[](Int_t i) const;\
ClassDef(StSPtrVec ## QWERTY,1) \
};\
class St ## QWERTY ## Iterator : public StObjArrayIter \
{ \
public: \
 \
St ## QWERTY ## Iterator(const StObjArray* col=0, Bool_t dir = kIterForward);\
 \
St ## QWERTY ## Iterator(const St ## QWERTY ## Iterator &init);\
virtual       ~St ## QWERTY ## Iterator(){};  \
virtual St ## QWERTY *operator*() const;\
ClassDef(St ## QWERTY ## Iterator,1) \
};\
typedef St ## QWERTY ## Iterator     StPtrVec  ## QWERTY ## Iterator;\
typedef St ## QWERTY ## Iterator     StPtrVec  ## QWERTY ## ConstIterator;\
typedef St ## QWERTY ## Iterator     StSPtrVec ## QWERTY ## Iterator;\
typedef St ## QWERTY ## Iterator     StSPtrVec ## QWERTY ## ConstIterator;\

//______________________________________________________________
#define StCollectionImp(QWERTY) \
\
ClassImp(St ## QWERTY ## Iterator) \
void 		St       ## QWERTY ## Iterator	::Streamer	(TBuffer& ){} \
void 		StPtrVec ## QWERTY  		::Streamer	(TBuffer& b){StRefArray::Streamer(b);} \
TIterator* 	StPtrVec ## QWERTY 		::MakeIterator	(Bool_t dir) const \
  {return (TIterator*)new St ## QWERTY ## Iterator(this,dir);} \
\
ClassImp(StPtrVec ## QWERTY ##) \
		StPtrVec ## QWERTY::StPtrVec ## QWERTY(Int_t s):StRefArray(s){};\
		StPtrVec ## QWERTY::StPtrVec ## QWERTY(const StPtrVec ## QWERTY &from):StRefArray(from){};\
void 		StPtrVec ## QWERTY::push_back(const St ## QWERTY &Obj){StObjArray::push_back((TObject*)&Obj);}  \
void 		StPtrVec ## QWERTY::push_back(const St ## QWERTY *Obj){StObjArray::push_back((TObject*)Obj);} \
St ## QWERTY*	StPtrVec ## QWERTY::back() const \
  {return (St ## QWERTY ## *)Back();};\
St ## QWERTY* 	StPtrVec ## QWERTY::front() const \
  {return (St ## QWERTY ## *)Front();};\
\
const St ## QWERTY ## Iterator 	StPtrVec ## QWERTY::begin() const { return *((St ## QWERTY ## Iterator*)Begin());}\
const St ## QWERTY ## Iterator 	StPtrVec ## QWERTY::end()   const { return *((St ## QWERTY ## Iterator*)End());}  \
void 				StPtrVec ## QWERTY::clean(St ## QWERTY* 		obj ){Clean(obj);  }	  \
void 				StPtrVec ## QWERTY::erase(St ## QWERTY*			obj ){Clean(obj);  }	  \
void 				StPtrVec ## QWERTY::clean(St ## QWERTY ## Iterator* 	iter){Clean(iter); }	  \
void 				StPtrVec ## QWERTY::erase(St ## QWERTY ## Iterator* 	iter){Clean(iter); }	  \
void 				StPtrVec ## QWERTY::clean(St ## QWERTY&			obj ){Clean(&obj); }	  \
void 				StPtrVec ## QWERTY::erase(St ## QWERTY&			obj ){Clean(&obj); }	  \
void 				StPtrVec ## QWERTY::clean(St ## QWERTY ## Iterator&	iter){Clean(&iter);}	  \
void 				StPtrVec ## QWERTY::erase(St ## QWERTY ## Iterator&	iter){Clean(&iter);}	  \
St ## QWERTY*& StPtrVec ## QWERTY::operator[](Int_t i) const{return *((St ## QWERTY**)GetCell(i));};\
\
ClassImp(StSPtrVec ## QWERTY) \
	StSPtrVec ## QWERTY::StSPtrVec ## QWERTY(const Char_t *name,Int_t s ):StStrArray(name,s){};\
	StSPtrVec ## QWERTY::StSPtrVec ## QWERTY(const StSPtrVec ## QWERTY &from):StStrArray(from){};\
void 		StSPtrVec ## QWERTY::push_back(const St ## QWERTY &Obj){StObjArray::push_back((TObject*)&Obj);} \
void 		StSPtrVec ## QWERTY::push_back(const St ## QWERTY *Obj){StObjArray::push_back((TObject*)Obj);} \
St ## QWERTY* 	StSPtrVec ## QWERTY::back() const \
  {return (St ## QWERTY ## *)Back();};\
St ## QWERTY* 	StSPtrVec ## QWERTY::front() const \
  {return (St ## QWERTY ## *)Front();};\
\
void 		StSPtrVec ## QWERTY::Streamer    (TBuffer& b){StStrArray::Streamer(b);} \
TIterator* 	StSPtrVec ## QWERTY::MakeIterator(Bool_t dir) const \
  {return (TIterator*)new St ## QWERTY ## Iterator(this,dir);} \
\
const St ## QWERTY ## Iterator 	StSPtrVec ## QWERTY::begin() const { return *((St ## QWERTY ## Iterator*)Begin());};\
const St ## QWERTY ## Iterator 	StSPtrVec ## QWERTY::end()   const { return *((St ## QWERTY ## Iterator*)End());};\
				St ## QWERTY ## Iterator::St ## QWERTY ## Iterator(const StObjArray* col, Bool_t dir ) :  StObjArrayIter(col, dir) \
  {};\
 \
				St ## QWERTY ## Iterator::St ## QWERTY ## Iterator(const St ## QWERTY ## Iterator &init) :  StObjArrayIter(0,kIterForward) \
  { *this=init;};\
 \
St ## QWERTY *St ## QWERTY ## Iterator::operator*() const {return (St ## QWERTY ## *)GetObject();} \
void StSPtrVec ## QWERTY::clean(St ## QWERTY *obj){Clean(obj);}\
void StSPtrVec ## QWERTY::erase(St ## QWERTY *obj){Erase(obj);}\
void StSPtrVec ## QWERTY::clean(St ## QWERTY ## Iterator *iter){Clean(iter);}\
void StSPtrVec ## QWERTY::erase(St ## QWERTY ## Iterator *iter){Erase(iter);}\
void StSPtrVec ## QWERTY::clean(St ## QWERTY &obj){Clean(&obj);}\
void StSPtrVec ## QWERTY::erase(St ## QWERTY &obj){Erase(&obj);}\
void StSPtrVec ## QWERTY::clean(St ## QWERTY ## Iterator &iter){Clean(&iter);}\
void StSPtrVec ## QWERTY::erase(St ## QWERTY ## Iterator &iter){Erase(&iter);}\
St ## QWERTY*& StSPtrVec ## QWERTY::operator[](Int_t i) const{return *((St ## QWERTY**)GetCell(i));};\

#endif /*End not __CINT__*/
#endif

