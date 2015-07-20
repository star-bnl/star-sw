/*!
  \class StMessTypeList
  \author G. Van Buren, BNL

  This class manages the message types in STAR. It is a singleton.
  
*/

#ifndef ClassStMessTypeList
#define ClassStMessTypeList

#include <stdlib.h>
#include <vector>
#if !defined(ST_NO_NAMESPACES)
using std::vector;
#endif

#ifdef ST_NO_TEMPLATE_DEF_ARGS
// Syntax currently required by Solaris compiler
#define StVector(T) vector<T, allocator<T> >
typedef vector<int, allocator<int> > intVector;
#else
#define StVector(T) vector<T>
typedef vector<int> intVector;
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
            const char* FindNumType(size_t typeNum);
            const char* FindNumText(size_t typeNum);
            const char* Text(const char* type) {
                          StMessTypePair* temp = FindType(type);
                          return ( (temp) ? temp->Text() : 0 );
                        }
                    int ListTypes();
};

#endif

// $Id: StMessTypeList.h,v 1.10 2015/07/20 18:34:31 genevb Exp $
// $Log: StMessTypeList.h,v $
// Revision 1.10  2015/07/20 18:34:31  genevb
// Include stdlib to get size_t
//
// Revision 1.9  2003/09/25 21:19:22  genevb
// Some new cout-like functions and friend functions, some doxygen-ization
//
// Revision 1.8  2000/01/26 19:04:48  genevb
// Adjust use of namespaces
//
// Revision 1.7  1999/12/28 21:29:55  porter
// added 'using std::vector' and a (char*) cast to allow compilation
// using solaris CC5.  Many warnings of const char* vs char* still
// exist but will await Gene's return to fix these
//
// Revision 1.6  1999/09/14 15:42:03  genevb
// Some bug fixes, workaround for nulls in strings
//
// Revision 1.5  1999/09/10 21:05:55  genevb
// Some workarounds for RedHat6.0
//
// Revision 1.4  1999/06/30 17:24:49  genevb
// Better limit management, remove Bool_t
//
// Revision 1.3  1999/06/29 17:37:30  genevb
// Lots of fixes...
//
// Revision 1.2  1999/06/26 00:24:52  genevb
// Fixed const type mismatches
//
// Revision 1.1  1999/06/23 15:17:45  genevb
// Introduction of StMessageManager
//
// Revision 1.0  1999/01/27 10:28:29  genevb
//
