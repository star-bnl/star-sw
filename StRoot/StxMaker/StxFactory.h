#ifndef StxFactory_H
#define StxFactory_H 
#include <string.h>
#include <assert.h>
#include <typeinfo>
#include "TNamed.h"
/*!
  Abstract base class defining a  factory mechanism
  <p>
  This class defines the concept of factory, an agent responsible for the
  creation or instantiation of a given type of class. The class is templated.
  The template represents the base class to be intanstiated and served by the
  factory. Implementation (derived class) may serve objects from class derived
  based on the "Factorized" template class.
*/
//______________________________________________________________________________
class BFactory : public TNamed {
 public:
  virtual void Clear(const Option_t* opt="")=0; //!Clear/delete all objects owned by this factory
  virtual void Reset()=0; //!Reset this factory
  virtual void free(void *obj)=0;//!Free an object for reuse 
  static void Free(void *obj) {//!Free an object for reuse 
    long *v = ((long*)obj) - 1; if (!*v) v--;    assert((*v)&1L);
    BFactory *f = (BFactory*)((*v)-1);    f->free(obj);
  }
  void SetFastDelete()	{fFastDel=1;}
  void SetMaxIncrementCount(Int_t maxCount)	{fMaxCount=maxCount;}
  Int_t MaxIncrementCount() const		{return fMaxCount;  }
  Int_t CurrentSize()  const 		{return fCurCount;  }
  Int_t CurrentCount() const 		{return fCurCount;  }
 protected:
  BFactory(const Char_t * name) : TNamed(name,""), fMaxCount(1000000), fCurCount(0), fUseCount(0), 
    fFastDel(0), fInstCount(0), fFreeCount(0) {}
  virtual ~BFactory() {}
  Int_t fMaxCount;
  Int_t fCurCount;
  Int_t fUseCount;
  Int_t fFastDel;
  Int_t fInstCount;
  Int_t fFreeCount;
  static Double_t fgTotal;  
};

//______________________________________________________________________________
template <class Abstract>
class Factory : public BFactory {
 public:
  Factory(const Char_t * name): BFactory(name) {}
  virtual ~Factory() {}
  virtual void free(Abstract *obj)=0; //!Free an object for reuse 
  virtual void free(void *obj)=0;
  virtual Abstract *getInstance()=0; //!Get a pointer to instance of objects served by this factory.
};
//______________________________________________________________________________
template<class Object>
class StxHolder  {
 public:
  StxHolder() {memset(fChar,0,((Char_t*)&fObj)-fChar);}
  union {
    StxHolder *fNext;
    long       fLong;
    Char_t       fChar[1];
  };
  Object fObj;
};
//______________________________________________________________________________
template<class Object>
class StxBlock {
 public:
  enum {kSize=100};
  StxBlock(StxBlock **bTop,StxHolder<Object> **hTop,Char_t *buf) {fBuff=buf; Reset(bTop,hTop);}
  void Reset(StxBlock **bTop,StxHolder<Object> **hTop) {
    fNext=*bTop; *bTop=this; for (Int_t i=0;i<kSize;i++) {fArr[i].fNext = *hTop;*hTop = fArr+i;}
  }
  Int_t getSize() const {return kSize;}
  
  StxBlock *fNext;
  Char_t     *fBuff;
  StxHolder<Object> fArr[kSize];
};


//______________________________________________________________________________
template <class Concrete, class Abstract>
class StxFactory : public Factory<Abstract>
{
 public:
  void   free(Abstract *obj) {
    static const Int_t shift = (Char_t*)(&(((StxHolder<Concrete>*)1)->fObj))-(Char_t*)1;
    obj->Unset();
    StxHolder<Concrete>* h = (StxHolder<Concrete>*)((Char_t*)obj-shift);
    assert((h->fLong-1)== (long)this);
    h->fNext = fHTop; fHTop=h; this->fUseCount--; this->fFreeCount++;
  }

  void   free(void *obj) { free((Abstract*)obj);}
  //!Clear/delete all objects owned by this factory
  void Clear(const Option_t* opt="") {
    Double_t sz=0;
    StxBlock<Concrete>* b = fBTop;
    while (b) {
      StxBlock<Concrete>* d = b;
      b=b->fNext;
      if (this->fFastDel) {delete [] d->fBuff;} else { delete d;}
      sz += sizeof(StxBlock<Concrete>);
      this->fgTotal -= sizeof(StxBlock<Concrete>)*1e-6;
    }
    fBTop=0; fHTop=0; this->fCurCount=0; this->fUseCount=0;
    printf("*** %s::Clear() %g MegaBytes Total %g Inst/Free=%d %d\n"
	   ,this->GetName(),sz*1e-6
	   ,this->fgTotal,this->fInstCount,this->fFreeCount);
    this->fInstCount=0; this->fFreeCount=0;
  }

    
  //!Reset this factory
  void Reset() {
    if (!this->fUseCount) return;
    typedef StxBlock<Concrete> B_t;
    B_t* b = fBTop;
    fBTop=0;fHTop=0;
    while (b) {B_t* n = b->fNext; b->Reset(&fBTop,&fHTop); b=n;}
    this->fUseCount=0;
  }


  //!Get a pointer to instance of objects served by this factory.
  Abstract* getInstance() {
    enum {FENCE = sizeof(double)+2*sizeof(long)+1};
    if (!fHTop)  {
      assert(this->fCurCount < this->fMaxCount);  
      if (this->fFastDel)    {
	Int_t   nBuf = sizeof(StxBlock<Concrete>) + FENCE;
	Char_t *cBuf = new Char_t[nBuf];
	cBuf[nBuf-1]=46;
	new((StxBlock<Concrete>*)cBuf) StxBlock<Concrete>(&fBTop,&fHTop,cBuf);
	assert(cBuf[nBuf-1]==46);
      } else {
	new StxBlock<Concrete>(&fBTop,&fHTop,   0);
      }
      this->fCurCount += fBTop->getSize();
      this->fgTotal   += sizeof(StxBlock<Concrete>)*1e-6;
    }
    this->fInstCount++;
    StxHolder<Concrete> *h = fHTop;
    fHTop = h->fNext;
    h->fNext=0;
    h->fObj.Reset();
    this->fUseCount++;
    h->fLong= ((long)this)+1;		//set factory addres+1
    return &h->fObj;  
  }  

  static StxFactory*  myInstance() {static StxFactory* my=0; if (!my) my = new StxFactory; return my;}


 private:
  StxFactory() : Factory<Abstract>("") {
    fHTop=0;fBTop=0;this->SetName(typeid(*this).name());
    printf("*** Factory created *** %s\n",this->GetName());
  }
  ~StxFactory(){Clear();}
  StxBlock<Concrete>  *fBTop;
  StxHolder<Concrete> *fHTop;
  
};
#endif
