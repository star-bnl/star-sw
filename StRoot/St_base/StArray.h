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
#include <vector>

class StObjArray;
class StStrArray;
class StRefArray;
typedef vector<TObject*> VecTObj;
typedef vector<TObject*>::iterator VecTObjIter;
typedef VecTObjIter StObjArrayIter;
typedef VecTObjIter StStrArrayIter;
typedef VecTObjIter StRefArrayIter;

class StObjArray : public VecTObj , public StObject {
public:
             StObjArray(){}
   virtual  ~StObjArray(){}
virtual void Browse(TBrowser *b);
virtual void random_shuffle(int start,int end);
virtual Bool_t IsFolder();

private:
TString fName;

ClassDef(StObjArray,2)
};


class StStrArray : public StObjArray {
protected:

//NONMONO void Book(TObject* obj,int idx);

public:
 StStrArray(const Char_t *name="");
 StStrArray(const StStrArray &from);
 virtual ~StStrArray();
 virtual void operator =(const StStrArray &a); 

 const char *GetIDName() const ;
 void                SetIDName(const char* name);
 virtual void        SetName(const char *name);
 virtual const char *GetName() const;

 void push_back(TObject *to){VecTObj::push_back(to);}
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
  StRefArray(){};
  StRefArray(const StRefArray &from);
   ~StRefArray(){};
ClassDef(StRefArray,2)
};

//	Utilities
//void random_shuffle(StObjArrayIter &start,StObjArrayIter &end);

//	Macros

#ifndef __CINT__
#define StCollectionDef(QWERTY) \
class St ## QWERTY;\
typedef St ## QWERTY**  St ## QWERTY ## Iterator;\
\
class StPtrVec ## QWERTY : public StRefArray \
{ \
public: \
StPtrVec ## QWERTY();\
StPtrVec ## QWERTY(const StPtrVec ## QWERTY &from);\
virtual        ~StPtrVec ## QWERTY ##(){};\
\
const St ## QWERTY  *&at(Int_t idx) const {return (const St ## QWERTY  *&)(*this)[idx];}\
      St ## QWERTY  *&at(Int_t idx)       {return (      St ## QWERTY  *&)(*this)[idx];}\
\
const St ## QWERTY  *& front() const { return (const St ## QWERTY  *&)VecTObj::front();}\
      St ## QWERTY  *& front()       { return (      St ## QWERTY  *&)VecTObj::front();}\
\
const St ## QWERTY  *& back()  const { return (const St ## QWERTY  *&)VecTObj::back();}\
      St ## QWERTY  *& back()        { return (      St ## QWERTY  *&)VecTObj::back();}\
\
const St ## QWERTY ## Iterator begin() const {return (const St ## QWERTY ## Iterator)VecTObj::begin();}\
      St ## QWERTY ## Iterator begin()       {return (      St ## QWERTY ## Iterator)VecTObj::begin();}\
const St ## QWERTY ## Iterator end()   const {return (const St ## QWERTY ## Iterator)VecTObj::end();}\
      St ## QWERTY ## Iterator end()         {return (      St ## QWERTY ## Iterator)VecTObj::end();}\
      St ## QWERTY ## Iterator erase(St ## QWERTY ## Iterator  it)\
      {return (St ## QWERTY ## Iterator)VecTObj::erase((VecTObjIter)it);}\
      St ## QWERTY ## Iterator erase(St ## QWERTY ## Iterator fst,St ## QWERTY ## Iterator lst)\
      {return (St ## QWERTY ## Iterator)VecTObj::erase((VecTObjIter)fst,(VecTObjIter)lst);}\
      St ## QWERTY *& operator[](Int_t i)       {return at(i);}\
const St ## QWERTY *& operator[](Int_t i) const {return at(i);}\
void  push_back(const St ## QWERTY *to){VecTObj::push_back((TObject*)to);}\
\
ClassDef(StPtrVec ## QWERTY ##,1) \
};\
\
\
class StSPtrVec ## QWERTY : public StStrArray \
{ \
public: \
StSPtrVec ## QWERTY(const Char_t *name=0);\
StSPtrVec ## QWERTY(const StSPtrVec ## QWERTY &from);\
\
\
const St ## QWERTY  *&at(Int_t idx) const {return (const St ## QWERTY  *&)(*this)[idx];}\
      St ## QWERTY  *&at(Int_t idx)       {return (      St ## QWERTY  *&)(*this)[idx];}\
\
const St ## QWERTY  *& front() const { return (const St ## QWERTY  *&)VecTObj::front();}\
      St ## QWERTY  *& front()       { return (      St ## QWERTY  *&)VecTObj::front();}\
\
const St ## QWERTY  *& back()  const { return (const St ## QWERTY  *&)VecTObj::back();}\
      St ## QWERTY  *& back()        { return (      St ## QWERTY  *&)VecTObj::back();}\
\
const St ## QWERTY ## Iterator begin() const {return (const St ## QWERTY ## Iterator)VecTObj::begin();}\
      St ## QWERTY ## Iterator begin()       {return (      St ## QWERTY ## Iterator)VecTObj::begin();}\
const St ## QWERTY ## Iterator end()   const {return (const St ## QWERTY ## Iterator)VecTObj::end();}\
      St ## QWERTY ## Iterator end()         {return (      St ## QWERTY ## Iterator)VecTObj::end();}\
      St ## QWERTY ## Iterator erase(St ## QWERTY ## Iterator fst,St ## QWERTY ## Iterator lst=0)\
         {return (St ## QWERTY ## Iterator)StStrArray::erase((VecTObjIter)fst,(VecTObjIter)lst);}\
      St ## QWERTY *& operator[](Int_t i)       {return at(i);}\
const St ## QWERTY *& operator[](Int_t i) const {return at(i);}\
void  push_back(const St ## QWERTY *to){StStrArray::push_back((TObject*)to);}\
\
ClassDef(StSPtrVec ## QWERTY,1) \
};\
typedef St ## QWERTY ## Iterator     StPtrVec  ## QWERTY ## Iterator;\
typedef St ## QWERTY ## Iterator     StPtrVec  ## QWERTY ## ConstIterator;\
typedef St ## QWERTY ## Iterator     StSPtrVec ## QWERTY ## Iterator;\
typedef St ## QWERTY ## Iterator     StSPtrVec ## QWERTY ## ConstIterator;\

//______________________________________________________________
#define StCollectionImp(QWERTY) \
\
ClassImp(StPtrVec ## QWERTY ##) \
		StPtrVec ## QWERTY::StPtrVec ## QWERTY():StRefArray(){};\
		StPtrVec ## QWERTY::StPtrVec ## QWERTY(const StPtrVec ## QWERTY &from):StRefArray(from){}\
void 		StPtrVec ## QWERTY::Streamer    (TBuffer& b){StRefArray::Streamer(b);} \
\
ClassImp(StSPtrVec ## QWERTY) \
	StSPtrVec ## QWERTY::StSPtrVec ## QWERTY(const Char_t *name):StStrArray(name){}\
	StSPtrVec ## QWERTY::StSPtrVec ## QWERTY(const StSPtrVec ## QWERTY &from):StStrArray(from){}\
\
void 		StSPtrVec ## QWERTY::Streamer    (TBuffer& b){StStrArray::Streamer(b);} \

#endif /*End not __CINT__*/
#endif

