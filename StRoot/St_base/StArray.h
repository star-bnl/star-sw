#ifndef _StRegistry_
#define _StRegistry_
 
 
//////////////////////////////////////////////////////////////////////////
//                                                                      //
//                                                                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
 
#include <assert.h>
#include "TString.h"
#include "TObject.h"
#include "StObject.h"
//#ifndef __CINT__
class TObject;
#include <vector>
#include <algorithm>
    using std::vector;
    using std::random_shuffle;
  typedef vector<TObject*> VecTObj;
#ifndef __CINT__
  typedef vector<TObject*>::iterator       VecTObjIter;
  typedef vector<TObject*>::const_iterator const_VecTObjIter;
  typedef VecTObjIter StObjArrayIter;
  typedef VecTObjIter StStrArrayIter;
  typedef VecTObjIter StRefArrayIter;
  typedef const_VecTObjIter const_StObjArrayIter;
  typedef const_VecTObjIter const_StStrArrayIter;
  typedef const_VecTObjIter const_StRefArrayIter;
#endif /*!__CINT__*/

#ifdef __CINT__NEVER
	class VecTObj;
	class VecTObjIter;
	class const_VecTObjIter;
	class StObjArrayIter;
	class StStrArrayIter;
	class StRefArrayIter;
	class const_StObjArrayIter;
	class const_StStrArrayIter;
	class const_StRefArrayIter;
#endif /*__CINT__*/

class StObjLink : public TObject
{
public:
   	StObjLink(const StObject *p=0)	{fLink=(StObject*)p;}
   	StObjLink(const StObjLink &from):TObject() {fLink=(StObject*)from.fLink;}
       ~StObjLink()             	{fLink=0;}

   	StObjLink &operator=(StObjLink &from){fLink=from.fLink;return *this;}
   	StObjLink &operator=(StObject *obj)  {fLink=obj; return *this;}
   	StObject  &operator*() 				{return *fLink;}
   	StObject  *operator->() 			{return  fLink;}
//   	          operator StObject *&() 		{return  fLink;}
//                operator const StObject *() const 	{return  fLink;}
   	StObject*  get() 				{return  fLink;}
virtual Bool_t     IsFolder() const {return 1;}
virtual void Browse(TBrowser* b); 			
//   void Streamer(TBuffer &buf);

   StObject *fLink;
   ClassDef(StObjLink,1)
};

template<class T>
    class StLink : public StObjLink {
public:
    typedef T element_type;
    StLink(T *p = 0):StObjLink(p){};
   StLink<T> &operator=(T *obj)  {fLink=obj; return *this;}
   StLink<T> &operator=(const StLink<T> &fr) {fLink=(StObject*)fr.fLink; return *this;}
   T         &operator*() 		{return *((T*)fLink);}
   T         *operator->() 		{return   (T*)fLink; }
   const T   *operator->() const	{return   (const T*)fLink; }
              operator const T *() const{return   (const T*)fLink; }
   	      operator T *&() 		{return   (     T*&)fLink; }
};


class StObjArray;
class StStrArray;
class StRefArray;


class StObjArray : public StObject {
public:
             StObjArray(Int_t sz=0){if(sz) resize(sz);}
   virtual  ~StObjArray(){}
virtual void Browse(TBrowser *b);
        void random_shuffle(int start=0,int end=0x7fffffff);
        size_t capacity() const {return fV.capacity();}
        size_t max_size() const {return fV.max_size();}
        void   clear()          {fV.clear()          ;}
        void   pop_back()       {fV.pop_back()       ;}
        int    empty() const    {return fV.empty()   ;}
        size_t size () const    {return fV.size()    ;}
        void reserve(size_t sz) {fV.reserve(sz)      ;}
        void resize(size_t sz,TObject *v=0) {fV.resize(sz,v);}
virtual Bool_t IsFolder() const;
        TObject *       &at(int i)       {return fV[i];}
        void     put_at(TObject *obj,int i){fV[i]=obj;}

        TObject * const &front() const { return fV.front();}
        TObject *       &front()       { return fV.front();}

        TObject * const &back()  const { return fV.back();}
        TObject *       &back()        { return fV.back();}
        TObject *        find(const char *name) const;

#ifndef __CINT__
	const_VecTObjIter begin() const {return fV.begin();}
        VecTObjIter       begin()       {return fV.begin();}
const_VecTObjIter end()   const {return fV.end();}
        VecTObjIter end()         {return fV.end();}
        TObject**   Erase(TObject**    it                 ,int del); 
        TObject**   Erase(TObject**    fst,TObject**   lst,int del);
#endif /*!__CINT__*/

        TObject *&       operator[](Int_t i)       {return fV[i];}
        TObject * const &operator[](Int_t i) const {return fV[i];}
void    push_back(const TObject * const to){fV.push_back((TObject*)to);}
        Int_t getEntries() const;
void    ls(const char *tit="") const;
void    Print(const char *tit="") const { ls(tit); }
protected:
//#ifndef __CINT__
VecTObj fV;
//#endif //_CINT__
ClassDef(StObjArray,4)
};


class StStrArray : public StObjArray {
protected:

//NONMONO void Book(TObject* obj,int idx);

public:
 StStrArray(Int_t sz=0);
 StStrArray(const StStrArray &from);
 virtual ~StStrArray();
       void operator=(const StStrArray &a); 

 void push_back(const TObject *to);
 void put_at(TObject *obj,int i);
 void clear();
 virtual void makeZombie(int flg);			
private:
ClassDef(StStrArray,3)
};


class StRefArray : public StObjArray
{
public:
  StRefArray(Int_t sz=0);
  StRefArray(const StRefArray &from);
   ~StRefArray(){};
ClassDef(StRefArray,3)
};

//	Utilities

//	Macros

#ifndef __CINT__
#define StCollectionDef(QWERTY) \
class St ## QWERTY;\
typedef St ## QWERTY**            St ## QWERTY ## Iterator;\
typedef St ## QWERTY * const * const_St ## QWERTY ## Iterator;\
\
class StPtrVec ## QWERTY : public StRefArray \
{ \
public: \
StPtrVec ## QWERTY(Int_t sz=0):StRefArray(sz){};\
StPtrVec ## QWERTY(const StPtrVec ## QWERTY &from):StRefArray(from){};\
virtual        ~StPtrVec ## QWERTY(){};\
\
 St ## QWERTY * const &at(Int_t idx) const {return (St ## QWERTY  * const &)fV[idx];}\
 St ## QWERTY *       &at(Int_t idx)       {return (St ## QWERTY  *       &)fV[idx];}\
\
 St ## QWERTY * const &front()       const {return (St ## QWERTY  * const &)fV.front();}\
 St ## QWERTY *       &front()             {return (St ## QWERTY  *       &)fV.front();}\
\
 St ## QWERTY * const &back()        const {return (St ## QWERTY  * const &)fV.back();}\
 St ## QWERTY *       &back()              {return (St ## QWERTY  *       &)fV.back();}\
\
const_St ## QWERTY ## Iterator begin() const {return (const_St ## QWERTY ## Iterator)&(*(fV.begin()));}\
      St ## QWERTY ## Iterator begin()       {return (      St ## QWERTY ## Iterator)&(*(fV.begin()));}\
const_St ## QWERTY ## Iterator end()   const {return (const_St ## QWERTY ## Iterator)&(*(fV.end()));}\
      St ## QWERTY ## Iterator end()         {return (      St ## QWERTY ## Iterator)&(*(fV.end()));}\
      St ## QWERTY ## Iterator erase(St ## QWERTY ## Iterator  it)\
      {return (St ## QWERTY ## Iterator)Erase((TObject**)it,0);}\
      St ## QWERTY ## Iterator erase(St ## QWERTY ## Iterator fst,St ## QWERTY ## Iterator lst)\
      {return (St ## QWERTY ## Iterator)Erase((TObject**)fst,(TObject**)lst,0);}\
      St ## QWERTY *       &operator[](Int_t i)       {return at(i);}\
      St ## QWERTY * const &operator[](Int_t i) const {return at(i);}\
void  push_back(const St ## QWERTY * const to){fV.push_back((TObject*const)to);}\
\
ClassDef(StPtrVec ## QWERTY ,1) \
};\
\
\
class StSPtrVec ## QWERTY : public StStrArray \
{ \
public: \
StSPtrVec ## QWERTY(Int_t sz=0):StStrArray(sz){};\
StSPtrVec ## QWERTY(const StSPtrVec ## QWERTY &from):StStrArray(from){};\
\
 St ## QWERTY * const &at(Int_t idx) const {return (St ## QWERTY  * const &)fV[idx];}\
 St ## QWERTY *       &at(Int_t idx)       {return (St ## QWERTY  *       &)fV[idx];}\
\
 St ## QWERTY * const &front()       const {return (St ## QWERTY  * const &)fV.front();}\
 St ## QWERTY *       &front()             {return (St ## QWERTY  *       &)fV.front();}\
\
 St ## QWERTY * const &back()        const {return (St ## QWERTY  * const &)fV.back();}\
 St ## QWERTY *       &back()              {return (St ## QWERTY  *       &)fV.back();}\
\
const_St ## QWERTY ## Iterator begin() const {return (const_St ## QWERTY ## Iterator)&(*(fV.begin()));}\
      St ## QWERTY ## Iterator begin()       {return (      St ## QWERTY ## Iterator)&(*(fV.begin()));}\
const_St ## QWERTY ## Iterator end()   const {return (const_St ## QWERTY ## Iterator)&(*(fV.end()));}\
      St ## QWERTY ## Iterator end()         {return (      St ## QWERTY ## Iterator)&(*(fV.end()));}\
      St ## QWERTY ## Iterator erase(St ## QWERTY ## Iterator  it)\
      {return (St ## QWERTY ## Iterator)Erase((TObject**)it,1);}\
      St ## QWERTY ## Iterator erase(St ## QWERTY ## Iterator fst,St ## QWERTY ## Iterator lst)\
      {return (St ## QWERTY ## Iterator)Erase((TObject**)fst,(TObject**)lst,1);}\
      St ## QWERTY *       &operator[](Int_t i)       {return at(i);}\
      St ## QWERTY * const &operator[](Int_t i) const {return at(i);}\
void  push_back(const St ## QWERTY *to){StStrArray::push_back((TObject*)to);}\
\
ClassDef(StSPtrVec ## QWERTY,1) \
};\
typedef       St ## QWERTY ## Iterator  StPtrVec ## QWERTY ## Iterator;\
typedef const_St ## QWERTY ## Iterator  StPtrVec ## QWERTY ## ConstIterator;\
typedef       St ## QWERTY ## Iterator  StSPtrVec ## QWERTY ## Iterator;\
typedef const_St ## QWERTY ## Iterator  StSPtrVec ## QWERTY ## ConstIterator;\

//______________________________________________________________
#define StCollectionImp(QWERTY) \
\
ClassImpUnique(StPtrVec ## QWERTY ,1 ) \
void 		StPtrVec  ## QWERTY::Streamer(TBuffer& b){StRefArray::Streamer(b);} \
\
ClassImpUnique(StSPtrVec ## QWERTY , 2 ) \
\
void 		StSPtrVec ## QWERTY::Streamer(TBuffer& b){StStrArray::Streamer(b);} \

#endif /*End not __CINT__*/
#endif

