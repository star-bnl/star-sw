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
#include "TString.h"
#include "StObject.h"
#ifndef __CINT__
#include <vector>
#include <algorithm>
using std::vector;
using std::random_shuffle;
typedef vector<TObject*> VecTObj;
typedef vector<TObject*>::iterator       VecTObjIter;
typedef vector<TObject*const>::iterator const_VecTObjIter;
typedef VecTObjIter StObjArrayIter;
typedef VecTObjIter StStrArrayIter;
typedef VecTObjIter StRefArrayIter;
typedef const_VecTObjIter const_StObjArrayIter;
typedef const_VecTObjIter const_StStrArrayIter;
typedef const_VecTObjIter const_StRefArrayIter;
#endif /*!__CINT__*/

#ifdef __CINT__
	class VecTObj;
	class VecTObjIter;
	class  const_VecTObjIter;
	class StObjArrayIter;
	class StStrArrayIter;
	class StRefArrayIter;
	class const_StObjArrayIter;
	class const_StStrArrayIter;
	class const_StRefArrayIter;
#endif /*__CINT__*/

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
virtual Bool_t IsFolder();
        TObject *&at(int i){return fV[i];}
        void     put_at(TObject *obj,int i){fV[i]=obj;}

        TObject * const &front() const { return fV.front();}
        TObject *       &front()       { return fV.front();}

        TObject * const &back()  const { return fV.back();}
        TObject *       &back()        { return fV.back();}

const_VecTObjIter begin() const {return fV.begin();}
        VecTObjIter begin()       {return fV.begin();}
const_VecTObjIter end()   const {return fV.end();}
        VecTObjIter end()         {return fV.end();}
        VecTObjIter erase(VecTObjIter it                 ) {return fV.erase(it);}
        VecTObjIter erase(VecTObjIter fst,VecTObjIter lst) {return fV.erase(fst,lst);}
        TObject *&       operator[](Int_t i)       {return fV[i];}
        TObject * const &operator[](Int_t i) const {return fV[i];}
void    push_back(TObject * const to){fV.push_back(to);}
protected:
#ifndef __CINT__
VecTObj fV;
#endif //_CINT__
ClassDef(StObjArray,2)
};


class StStrArray : public StObjArray {
protected:

//NONMONO void Book(TObject* obj,int idx);

public:
 StStrArray(const Char_t *name="",Int_t sz=0);
 StStrArray(Int_t sz);
 StStrArray(const StStrArray &from);
 virtual ~StStrArray();
 virtual void operator =(const StStrArray &a); 

 const char *GetIDName() const ;
 void                SetIDName(const char* name);
 virtual void        SetName(const char *name);
 virtual const char *GetName() const;

 void push_back(TObject *to){fV.push_back(to);}
 VecTObjIter erase(VecTObjIter fst,VecTObjIter lst=0);
 void clear();
private:
TString fIDName;
TString fName;
 
ClassDef(StStrArray,2)
};


class StRefArray : public StObjArray
{
public:
  StRefArray(Int_t sz=0);
  StRefArray(const StRefArray &from);
   ~StRefArray(){};
ClassDef(StRefArray,2)
};

//	Utilities

//	Macros

#ifndef __CINT__
#define StCollectionDef(QWERTY) \
class St ## QWERTY;\
typedef St ## QWERTY**            St ## QWERTY ## Iterator;\
typedef St ## QWERTY*const* const_St ## QWERTY ## Iterator;\
\
class StPtrVec ## QWERTY : public StRefArray \
{ \
public: \
StPtrVec ## QWERTY(Int_t sz=0):StRefArray(sz){};\
StPtrVec ## QWERTY(const StPtrVec ## QWERTY &from):StRefArray(from){};\
virtual        ~StPtrVec ## QWERTY ##(){};\
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
const_St ## QWERTY ## Iterator begin() const {return (const_St ## QWERTY ## Iterator)fV.begin();}\
      St ## QWERTY ## Iterator begin()       {return (      St ## QWERTY ## Iterator)fV.begin();}\
const_St ## QWERTY ## Iterator end()   const {return (const_St ## QWERTY ## Iterator)fV.end();}\
      St ## QWERTY ## Iterator end()         {return (      St ## QWERTY ## Iterator)fV.end();}\
      St ## QWERTY ## Iterator erase(St ## QWERTY ## Iterator  it)\
      {return (St ## QWERTY ## Iterator)fV.erase((VecTObjIter)it);}\
      St ## QWERTY ## Iterator erase(St ## QWERTY ## Iterator fst,St ## QWERTY ## Iterator lst)\
      {return (St ## QWERTY ## Iterator)fV.erase((VecTObjIter)fst,(VecTObjIter)lst);}\
      St ## QWERTY *       &operator[](Int_t i)       {return at(i);}\
      St ## QWERTY * const &operator[](Int_t i) const {return at(i);}\
void  push_back(St ## QWERTY * const to){fV.push_back((TObject*const)to);}\
\
ClassDef(StPtrVec ## QWERTY ##,1) \
};\
\
\
class StSPtrVec ## QWERTY : public StStrArray \
{ \
public: \
StSPtrVec ## QWERTY(const Char_t *name=0,Int_t sz=0):StStrArray(name,sz){};\
StSPtrVec ## QWERTY(Int_t sz):StStrArray("",sz){};\
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
const_St ## QWERTY ## Iterator begin() const {return (const_St ## QWERTY ## Iterator)fV.begin();}\
      St ## QWERTY ## Iterator begin()       {return (      St ## QWERTY ## Iterator)fV.begin();}\
const_St ## QWERTY ## Iterator end()   const {return (const_St ## QWERTY ## Iterator)fV.end();}\
      St ## QWERTY ## Iterator end()         {return (      St ## QWERTY ## Iterator)fV.end();}\
      St ## QWERTY ## Iterator erase(St ## QWERTY ## Iterator  it)\
      {return (St ## QWERTY ## Iterator)fV.erase((VecTObjIter)it);}\
      St ## QWERTY ## Iterator erase(St ## QWERTY ## Iterator fst,St ## QWERTY ## Iterator lst)\
      {return (St ## QWERTY ## Iterator)fV.erase((VecTObjIter)fst,(VecTObjIter)lst);}\
      St ## QWERTY *       &operator[](Int_t i)       {return at(i);}\
      St ## QWERTY * const &operator[](Int_t i) const {return at(i);}\
void  push_back(const St ## QWERTY *to){StStrArray::push_back((TObject*)to);}\
\
ClassDef(StSPtrVec ## QWERTY,1) \
};\
typedef       St ## QWERTY ## Iterator  StPtrVec  ## QWERTY ## Iterator;\
typedef const_St ## QWERTY ## Iterator  StPtrVec  ## QWERTY ## ConstIterator;\
typedef       St ## QWERTY ## Iterator  StSPtrVec ## QWERTY ## Iterator;\
typedef const_St ## QWERTY ## Iterator  StSPtrVec ## QWERTY ## ConstIterator;\

//______________________________________________________________
#define StCollectionImp(QWERTY) \
\
ClassImp(StPtrVec ## QWERTY ##) \
void 		StPtrVec  ## QWERTY::Streamer(TBuffer& b){StRefArray::Streamer(b);} \
\
ClassImp(StSPtrVec ## QWERTY) \
\
void 		StSPtrVec ## QWERTY::Streamer(TBuffer& b){StStrArray::Streamer(b);} \

#endif /*End not __CINT__*/
#endif

