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
 
#include "TList.h"
#include "TObjArray.h"
#include "TNamed.h"
#include "TArrayI.h"

class StVectorInt : public TArrayI
{
public:
   StVectorInt(int n=0):TArrayI(n){fLast = -1;}; 
virtual ~StVectorInt(){};
virtual void Set(int n);
virtual void resize(int n){Set(n);};
virtual void Add(const int &c);
virtual void push_back(const int &c){Add(c);};
virtual void pop_back(){if (fLast>-1) fArray[fLast--]=0;};
virtual int  At(int idx){return (idx>=0 && idx<fN) ? fArray[idx]:0;};
virtual int& at(int idx){return fArray[idx];};
virtual int& front(){return at(0);};
virtual int& back(){return at(fLast);};
protected:
int fLast;
ClassDef(StVectorInt,0)
};


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
 static void Clear(){if (fReg) fReg->Clear();};
 static Int_t SetColl (StStrArray *coll); 		// Register new container
 static void  RemColl (StStrArray *coll); 		// UnRegister container
 static Int_t GetColl (const char *name); 	 	// get index of container
 static const char    *GetCollName (Int_t idx );	// get name of cont by index
 static StStrArray    *GetColl (Int_t idx );		// get name of cont by index
 static void  List() ;					// print list of registered conts    
 static ULong_t Ident(ULong_t colidx,ULong_t objidx){return colidx<<24 |objidx;};
 static void    Ident(ULong_t ident,ULong_t &colidx,ULong_t &objidx)
  {colidx = ident>>24;  objidx = ident & 0x00ffffff;};
 static Int_t GetNColl(){return (fReg) ? fReg->GetLast()+1:0;};				// Number of collections
 static void  AddNon(StRefArray *coll);
 static void  Init();
ClassDef(StRegistry,0)
};
class StObjArrayIter;
 
class StObjArray : public TCollection
{
public:
                StObjArray(Int_t s = TCollection::kInitCapacity){fObjArr = new TObjArray(s,0);};
                StObjArray(StObjArray& a) {assert(0);}
virtual        ~StObjArray(){delete fObjArr;};

            virtual void AddAfter(TObject* after, TObject* obj){fObjArr->AddAfter(after,obj);};
            virtual void Add(TObject* obj){fObjArr->Add(obj);};
            virtual void AddAt(TObject* obj, Int_t idx){fObjArr->AddAt(obj,idx);};
            virtual void AddAtAndExpand(TObject* obj, Int_t idx){fObjArr->AddAtAndExpand(obj,idx);};
           virtual Int_t AddAtFree(TObject* obj){return fObjArr->AddAtFree(obj);};
            virtual void AddBefore(TObject* before, TObject* obj){fObjArr->AddBefore(before,obj);};
            virtual void AddFirst(TObject* obj){fObjArr->AddFirst(obj);};
            virtual void AddLast(TObject* obj){fObjArr->AddLast(obj);};
        virtual TObject* After(TObject* obj) const {return fObjArr->After(obj);};
        virtual TObject* At(Int_t idx) const {return fObjArr->At(idx);};
        virtual TObject* Before(TObject* obj) const {return fObjArr->Before(obj);};
           virtual Int_t BinarySearch(TObject* obj, Int_t upto = kMaxInt) const {return fObjArr->BinarySearch(obj,upto);};
            virtual void Browse(TBrowser *b);
           virtual void Clear(Option_t* option=""){fObjArr->Clear(option);};
            virtual void Compress(){fObjArr->Compress();};
            virtual void Delete(Option_t* option=""){if(fObjArr)fObjArr->Delete(option);};
            virtual void Expand(Int_t newSize){fObjArr->Expand(newSize);};
        virtual TObject* First() const {return fObjArr->First();};
                   Int_t GetEntries() const {return fObjArr->GetEntries();};
                   Int_t GetEntriesFast() const {return fObjArr->GetEntriesFast();};
                   Int_t GetLast() const {return fObjArr->GetLast();};
           virtual Int_t IndexOf(TObject* obj) const {return fObjArr->IndexOf(obj);};
        virtual TObject* Last() const {return fObjArr->Last();};
                   Int_t LowerBound() const {return fObjArr->LowerBound();};
        virtual TObject* Remove(TObject* obj){return fObjArr->Remove(obj);};
            virtual void RemoveAt(Int_t idx){fObjArr->RemoveAt(idx);};
            virtual void Sort(Int_t upto = kMaxInt){fObjArr->Sort(upto);};
                TObject* UncheckedAt(Int_t i) const {return fObjArr->UncheckedAt(i);};
           virtual Int_t GetSize() const { return fObjArr->GetSize(); }          

virtual void 	push_back(const TObject *obj) {AddLast((TObject*)obj);};
virtual void 	pop_back() {RemoveAt(GetLast());};
virtual UInt_t  size() const {return GetLast()+1;};
virtual void    Resize(Int_t num);
virtual void    resize(Int_t num){Resize(num);};
virtual Int_t 	capacity() const {return fObjArr->Capacity();}
virtual TObject* Back() const {return Last();};
virtual TObject* Front() const {return First();};
virtual void 	clear(){Clear();};
virtual Bool_t 	empty() const {return fObjArr->IsEmpty();};
virtual void    SetLast(Int_t last);
virtual const TIterator *Begin() const;
virtual const TIterator *End() const;
virtual TIterator* MakeIterator(Bool_t dir = kIterForward) const;
virtual TObject** GetCell(Int_t idx) const{return &fObjArr->TObjArray::operator[](idx);};

// Data Members
TObjArray *fObjArr;

ClassDef(StObjArray,1)
};

class StObjArrayIter : public TObjArrayIter
{
public:
StObjArrayIter(const StObjArray* col=0, Bool_t dir = kIterForward) :  TObjArrayIter(0, dir)
{ 
  fColl = col; if (col) { SetCollection(fColl->fObjArr); Reset();}
};

StObjArrayIter(const StObjArrayIter &init) :  TObjArrayIter(0,kIterForward)
{ *this=init;};

virtual       ~StObjArrayIter(){}; 
virtual void   SetCursor(Int_t kursor);
virtual Int_t  GetCursor() const;
virtual void   SetDirection(Bool_t dir = kIterForward);
virtual Bool_t GetDirection() const;
virtual void   SetCollection(TObjArray *coll);
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

 void Book(TObject* obj,int idx)
 { obj->SetUniqueID(StRegistry::Ident(fIdx,idx));};

public:
 StStrArray(const Char_t *name=0, Int_t s=0);
 virtual ~StStrArray();

 virtual const Char_t *GetName() const {return fName;};
 virtual void          SetName(const char* name) {fName = name;}; 
 virtual const Char_t *GetIDName() const ;
 virtual void          SetIDName(const char* name);

 virtual void AddFirst(TObject* obj)
 { StObjArray::AddFirst(obj); Book(obj,0);}

 virtual void AddLast(TObject* obj)
 { StObjArray::AddLast(obj); Book(obj,GetLast());}

 virtual void AddAt(TObject* obj, Int_t idx)
 { StObjArray::AddAt(obj,idx); Book(obj,idx);};
 
 virtual void AddAtAndExpand(TObject* obj, Int_t idx)
 { StObjArray::AddAtAndExpand(obj,idx); Book(obj,idx);};
 
ClassDef(StStrArray,1)
};


class StRefArray : public StObjArray
{
public:
StRefArray(Int_t s = TCollection::kInitCapacity)
:StObjArray(s){};
virtual void Decode();
virtual ~StRefArray(){};
ClassDef(StRefArray,1)
};


#if 0
class QWERTY : public TObject
{
public:
QWERTY(){};
virtual ~QWERTY(){};
ClassDef(QWERTY,1)
};

class QWERTYIter;

class QWERTYRef : public StRefArray
{
public:
QWERTYRef(Int_t s = TCollection::kInitCapacity):StRefArray(s){};
virtual        ~QWERTYRef(){printf("~StObjArray %p\n",this);};
virtual QWERTY* back() const {return (QWERTY*)Back();};
virtual QWERTY* front() const {return (QWERTY*)Front();};
virtual const QWERTYIter begin() const ;
virtual const QWERTYIter end()   const ;
virtual TIterator* MakeIterator(Bool_t dir = kIterForward) const;
virtual QWERTY *&operator[](int idx) const {return *((QWERTY**)GetCell(idx));};
ClassDef(QWERTYRef,1)
};

class QWERTYStr : public StStrArray
{
public:
QWERTYStr(const Chat_t *name=0,Int_t s = TCollection::kInitCapacity):StStrArray(name,s){};
virtual        ~QWERTYStr(){printf("~QWERTYStr %p\n",this);};
virtual QWERTY* back() const {return (QWERTY*)Back();};
virtual QWERTY* front() const {return (QWERTY*)Front();};
virtual const QWERTYIter begin() const ;
virtual const QWERTYIter end()   const ;
virtual TIterator* MakeIterator(Bool_t dir = kIterForward) const;
virtual QWERTY *&operator[](int idx) const {return *((QWERTY**)GetCell(idx));};
ClassDef(QWERTYStr,1)
};


class QWERTYIter : public StObjArrayIter
{
public:

QWERTYIter(const StObjArray* col=0, Bool_t dir = kIterForward) :  StObjArrayIter(col, dir)
{ printf("QWERTYIter=%p\n",this);};

QWERTYIter(const QWERTYIter &init) :  StObjArrayIter(0,kIterForward)
{ *this=init; printf("QWERTYIter2=%p %p\n",this,&init);};

virtual       ~QWERTYIter(){printf("~QWERTYIter %p\n",this);}; 
virtual QWERTY *operator*() const {return (QWERTY*)GetObject();}
ClassDef(QWERTYIter,1)
};



void testqwe();
#endif // 0




#define StCollectionDef(QWERTY) \
class St ## QWERTY ## Iterator; \
class StVecPtr ## QWERTY : public StRefArray \
{ \
public: \
StVecPtr ## QWERTY ## (Int_t s = TCollection::kInitCapacity):StRefArray(s){}; \
virtual        ~StVecPtr ## QWERTY ##(){}; \
virtual void push_back(const St ## QWERTY &Obj){StObjArray::push_back((TObject*)&Obj);}  \
virtual void push_back(const St ## QWERTY *Obj){StObjArray::push_back((TObject*)Obj);} \
virtual St ## QWERTY ## * back() const \
{return (St ## QWERTY ## *)Back();}; \
virtual St ## QWERTY ## * front() const \
{return (St ## QWERTY ## *)Front();}; \
virtual const St ## QWERTY ## Iterator begin() const ; \
virtual const St ## QWERTY ## Iterator end()   const ; \
virtual TIterator* MakeIterator(Bool_t dir = kIterForward) const; \
virtual St ## QWERTY *&operator[](int idx) const\
{return *((St ## QWERTY ## **)GetCell(idx));}; \
ClassDef(StVecPtr ## QWERTY ##,1) \
}; \
class St ## QWERTY ## Collection : public StStrArray \
{ \
public: \
St ## QWERTY ## Collection(const Char_t *name=0,Int_t s = TCollection::kInitCapacity):StStrArray(name,s){}; \
virtual        ~St ## QWERTY ## Collection()\
{}; \
virtual void push_back(const St ## QWERTY &Obj){StObjArray::push_back((TObject*)&Obj);} \
virtual void push_back(const St ## QWERTY *Obj){StObjArray::push_back((TObject*)Obj);} \
virtual St ## QWERTY ## * back() const \
{return (St ## QWERTY ## *)Back();}; \
virtual St ## QWERTY ## * front() const \
{return (St ## QWERTY ## *)Front();}; \
virtual const St ## QWERTY ## Iterator begin() const ; \
virtual const St ## QWERTY ## Iterator end()   const ; \
virtual TIterator* MakeIterator(Bool_t dir = kIterForward) const; \
virtual St ## QWERTY *&operator[](int idx) const\
{return *((St ## QWERTY ## **)GetCell(idx));}; \
ClassDef(St ## QWERTY ## Collection,1) \
}; \
class St ## QWERTY ## Iterator : public StObjArrayIter \
{ \
public: \
 \
St ## QWERTY ## Iterator(const StObjArray* col=0, Bool_t dir = kIterForward) :  StObjArrayIter(col, dir) \
{}; \
 \
St ## QWERTY ## Iterator(const St ## QWERTY ## Iterator &init) :  StObjArrayIter(0,kIterForward) \
{ *this=init;}; \
 \
virtual       ~St ## QWERTY ## Iterator(){};  \
virtual St ## QWERTY *operator*() const {return (St ## QWERTY ## *)GetObject();} \
ClassDef(St ## QWERTY ## Iterator,1) \
}; \
typedef St ## QWERTY ## Iterator     St ## QWERTY ## ConstIterator; \
typedef St ## QWERTY ## Collection      St ## QWERTY ## Collection; \
//______________________________________________________________
#define StCollectionImp(QWERTY) \
ClassImp(St ## QWERTY ## Iterator) \
void St ## QWERTY ## Iterator::Streamer(TBuffer& ){} \
void StVecPtr ## QWERTY ##::Streamer(TBuffer& b){StRefArray::Streamer(b);} \
TIterator* StVecPtr ## QWERTY ##::MakeIterator(Bool_t dir) const \
{return (TIterator*)new St ## QWERTY ## Iterator(this,dir);} \
ClassImp(StVecPtr ## QWERTY ##) \
const St ## QWERTY ## Iterator StVecPtr ## QWERTY ##::begin() const { return *((St ## QWERTY ## Iterator*)Begin());}; \
const St ## QWERTY ## Iterator StVecPtr ## QWERTY ##::end()   const { return *((St ## QWERTY ## Iterator*)End());}; \
ClassImp(St ## QWERTY ## Collection) \
void St ## QWERTY ## Collection::Streamer(TBuffer& b){StStrArray::Streamer(b);} \
TIterator* St ## QWERTY ## Collection::MakeIterator(Bool_t dir) const \
{return (TIterator*)new St ## QWERTY ## Iterator(this,dir);} \
const St ## QWERTY ## Iterator St ## QWERTY ## Collection::begin() const { return *((St ## QWERTY ## Iterator*)Begin());}; \
const St ## QWERTY ## Iterator St ## QWERTY ## Collection::end()   const { return *((St ## QWERTY ## Iterator*)End());}; 


#endif

