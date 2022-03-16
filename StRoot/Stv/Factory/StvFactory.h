#ifndef StvFactory_H
#define StvFactory_H 
#include <string.h>
#include <assert.h>
#include <typeinfo>
#include "FactoryT.h"
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
template<class Object>
class StvHolder  {
public:
	StvHolder();
public:
  StvHolder *fNext;
  FactoryB  *fFact;
  Object fObj;
};

//______________________________________________________________________________
template<class Object>
class StvBlock {
public:
enum {kSize=100};
     StvBlock(StvBlock **bTop,StvHolder<Object> **hTop,char *buf);
void reset(StvBlock **bTop,StvHolder<Object> **hTop);
uint getSize() const {return kSize;}

StvBlock *fNext;
char     *fBuff;
StvHolder<Object> fArr[kSize];
};


//______________________________________________________________________________
template <class Concrete, class Abstract>
class StvFactory : public FactoryT<Abstract>
{
public:
void   free(Abstract *obj);  
void   free(void *obj) { free((Abstract*)obj);}
  ///Clear/delete all objects owned by this factory
void clear();  
void print();  

  ///Reset this factory
void reset();

  ///Get a pointer to instance of objects served by this factory.
Abstract* getInstance();
static StvFactory*  myInstance();

protected:
   StvFactory();
  ~StvFactory(){this->clear();}

StvBlock<Concrete>  *fBTop;
StvHolder<Concrete> *fHTop;

};
//______________________________________________________________________________
//______________________________________________________________________________
//______________________________________________________________________________
template <class Object>
StvBlock<Object>::StvBlock(StvBlock **bTop,StvHolder<Object> **hTop,char *buf)
{
  fBuff=buf;
  this->reset(bTop,hTop);
}
template <class Object>
void StvBlock<Object>::reset(StvBlock **bTop,StvHolder<Object> **hTop)
{
  fNext=*bTop; *bTop=this;
  for (int i=0;i<kSize;i++) {
    fArr[i].fNext = *hTop;
   *hTop = fArr+i;
  }
}
//______________________________________________________________________________
//______________________________________________________________________________
template <class Object>
StvHolder<Object>::StvHolder()
{
  char *b = (char*)&fNext;
  memset(b,0,((char*)&fObj)-b);
}
//______________________________________________________________________________
//______________________________________________________________________________
template <class Concrete, class Abstract>
StvFactory<Concrete,Abstract>::StvFactory():FactoryT<Abstract>("")
{
  fHTop=0;fBTop=0;
  this->SetName(typeid(*this).name());
  printf("*** FactoryT created *** %s\n",this->GetName());
}
template <class Concrete, class Abstract>
StvFactory<Concrete,Abstract>* StvFactory<Concrete,Abstract>::myInstance() 
{
   static StvFactory* my=0;
   if (!my) my = new StvFactory;
   return my;
}
//______________________________________________________________________________
template <class Concrete, class Abstract>
Abstract *StvFactory<Concrete,Abstract>::getInstance() 
{
  enum {FENCE = sizeof(double)+2*sizeof(long)+1};
  if (!fHTop)  {
    assert ("StvFactory::getInstance() - Too many instances" &&
            this->fCurCount < this->fMaxCount);

    if (this->fFastDel)    {
       uint   nBuf = sizeof(StvBlock<Concrete>) + FENCE;
       char *cBuf = new char[nBuf];
       cBuf[nBuf-1]=46;
       new((StvBlock<Concrete>*)cBuf) StvBlock<Concrete>(&fBTop,&fHTop,cBuf);
       assert(cBuf[nBuf-1]==46);
    } else {
       new StvBlock<Concrete>(&fBTop,&fHTop,   0);
    }
    this->fCurCount += fBTop->getSize();
    this->fgTotal   += sizeof(StvBlock<Concrete>)*1e-6;
  }
  this->fInstCount++;
  StvHolder<Concrete> *h = fHTop;
  fHTop = h->fNext;
  h->fNext=0;
  h->fObj.reset();
  this->fUseCount++;
  assert(!h->fFact || (unsigned long)h->fFact == 0xFF);
  h->fFact = this;		//set factory addres
  return &h->fObj;  
}  
//______________________________________________________________________________
template <class Concrete, class Abstract>
void StvFactory<Concrete,Abstract>::free(Abstract *obj)
{
  static const uint shift = (char*)(&(((StvHolder<Concrete>*)1)->fObj))-(char*)1;
  obj->unset();
  StvHolder<Concrete>* h = (StvHolder<Concrete>*)((char*)obj-shift);
  assert(h->fFact == this); 
  h->fNext = fHTop; h->fFact = (FactoryB*)0xFF;
  fHTop=h; 
  this->fUseCount--; this->fFreeCount++;
}

//______________________________________________________________________________
template <class Concrete, class Abstract>
void StvFactory<Concrete,Abstract>::clear()
{
  double sz=0;
  StvBlock<Concrete>* b = fBTop;
  while (b) {
    StvBlock<Concrete>* d = b;
    b=b->fNext;
    if (this->fFastDel) {delete [] d->fBuff;} else { delete d;}
    sz += sizeof(StvBlock<Concrete>);
    this->fgTotal -= sizeof(StvBlock<Concrete>)*1e-6;
  }
  fBTop=0; fHTop=0; this->fCurCount=0; this->fUseCount=0;
  printf("*** %s::clear() %g MegaBytes Total %g Inst/Free=%d %d\n"
        ,this->GetName(),sz*1e-6
	,this->fgTotal*1e-6,this->fInstCount,this->fFreeCount);
  this->fInstCount=0; this->fFreeCount=0;
}
//______________________________________________________________________________
template <class Concrete, class Abstract>
void StvFactory<Concrete,Abstract>::print()
{
  double sz=0;
  StvBlock<Concrete>* b = fBTop;
  while (b) {
    b=b->fNext;
    sz += sizeof(StvBlock<Concrete>);
    this->fgTotal -= sizeof(StvBlock<Concrete>)*1e-6;
  }
  this->fCurCount=0; this->fUseCount=0;
  printf("*** %s::print() %g MegaBytes Total %g Inst/Free=%d %d \n"
        ,this->GetName(),sz*1e-6
	,this->fgTotal  ,this->fInstCount,this->fFreeCount);
}
//______________________________________________________________________________
template <class Concrete, class Abstract>
void StvFactory<Concrete,Abstract>::reset()
{
  if (!this->fUseCount) return;
  
  typedef StvBlock<Concrete> B_t;
  B_t* b = fBTop;
  fBTop=0;fHTop=0;
  while (b) {
    B_t* n = b->fNext;
    b->reset(&fBTop,&fHTop);
    b=n;
  }
  this->fUseCount=0;
}
#endif
