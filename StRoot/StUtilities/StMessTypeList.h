// $Id: StMessTypeList.h,v 1.1 1999/06/23 15:17:45 genevb Exp $
// $Log: StMessTypeList.h,v $
// Revision 1.1  1999/06/23 15:17:45  genevb
// Introduction of StMessageManager
//
//
// Revision 1.1 1999/01/27 10:28:29 genevb
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StMessTypeList                                                       //
//                                                                      //
// This class manages the message types in STAR. It is a singleton.     //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef ClassStMessTypeList
#define ClassStMessTypeList

#include <vector>
#ifdef ST_NO_TEMPLATE_DEF_ARGS
// Syntax currently required by Solaris compiler
#define StVector(T) vector<T, allocator<T> >
#else
#define StVector(T) vector<T>
#endif

class StMessTypePair {
private:
 const Char_t* type;
 const Char_t* text;
public:
 StMessTypePair(const Char_t* ty, const Char_t* te);
 virtual ~StMessTypePair();
 const Char_t* Type() const {return type;}
 const Char_t* Text() const {return text;}
 ClassDef(StMessTypePair,0)
};

typedef StVector(StMessTypePair*) StMessTypeVec;
typedef StVector(StMessTypePair*)::iterator StMessTypeVecIter;


class StMessTypeList {
 private:
   static StMessTypeList* mInstance;

 protected:
   StMessTypeVec messList;
   StMessTypeList();
   StMessTypeList(const StMessTypeList&);

 public:
   virtual ~StMessTypeList();
   static StMessTypeList* Instance();
                    Int_t AddType(const Char_t* type, const Char_t* text);
                    Int_t FindTypeNum(Char_t* type);
          StMessTypePair* FindType(Char_t* type);
            const Char_t* Text(Char_t* type);
                    Int_t ListTypes();
   ClassDef(StMessTypeList,0)
};

#endif
