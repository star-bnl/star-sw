#ifndef ValErrPair_h
#define ValErrPair_h


#include <stdio.h>
#include <iostream>
#include <map>
#include <set>
#include <utility> // for std::pair definition

#include "TObject.h"


class ValErrPair;

typedef std::set<ValErrPair>               ValErrSet;
typedef ValErrSet::iterator                ValErrSetIter;
typedef std::map<std::string, ValErrPair>  ValErrMap;


class ValErrPair : public TObject
{
public:
   std::pair<Double_t, Double_t> ve;

#ifndef __CINT__
public:
   Double_t &v;      //!
   Double_t &e;      //!
   Double_t &first;  //!
   Double_t &second; //!
#endif

public:
   ValErrPair();
   ValErrPair(Double_t vv, Double_t ee);
   ValErrPair(const ValErrPair& vep);

   ValErrPair& operator=(const ValErrPair& vep);

   friend std::ostream& operator<<(std::ostream& os, const ValErrPair& vep);

   ClassDef(ValErrPair, 1)
};


bool operator==(const ValErrPair& lhs, const ValErrPair& rhs);
inline bool operator!=(const ValErrPair& lhs, const ValErrPair& rhs){return !operator==(lhs,rhs);} 
bool operator< (const ValErrPair& lhs, const ValErrPair& rhs);
inline bool operator> (const ValErrPair& lhs, const ValErrPair& rhs){return  operator< (rhs,lhs);} 
inline bool operator<=(const ValErrPair& lhs, const ValErrPair& rhs){return !operator> (lhs,rhs);} 
inline bool operator>=(const ValErrPair& lhs, const ValErrPair& rhs){return !operator< (lhs,rhs);}


#endif
