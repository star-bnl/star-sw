// $Id: StMessTypeList.h,v 1.3 1999/06/29 17:37:30 genevb Exp $
// $Log: StMessTypeList.h,v $
// Revision 1.3  1999/06/29 17:37:30  genevb
// Lots of fixes...
//
// Revision 1.2  1999/06/26 00:24:52  genevb
// Fixed const type mismatches
//
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
 const char* type;
 const char* text;
public:
 StMessTypePair(const char* ty, const char* te);
 virtual ~StMessTypePair();
 const char* Type() const {return type;}
 const char* Text() const {return text;}
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
                    int AddType(const char* type, const char* text);
                    int FindTypeNum(const char* type);
          StMessTypePair* FindType(const char* type);
            const char* Text(const char* type);
                    int ListTypes();
};

#endif
