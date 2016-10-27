
#include "ValErrPair.h"

ClassImp(ValErrPair)

using namespace std;


ValErrPair::ValErrPair() : TObject(), ve(0, -1), v(ve.first), e(ve.second), first(ve.first), second(ve.second)
{
   //v = ve.first;
   //e = ve.second;
}


ValErrPair::ValErrPair(Double_t vv, Double_t ee) : TObject(), ve(vv, ee), v(ve.first), e(ve.second), first(ve.first), second(ve.second)
{
   //v = ve.first;
   //e = ve.second;
}


ValErrPair::ValErrPair(const ValErrPair& vep) : TObject(), ve(vep.ve), v(ve.first), e(ve.second), first(ve.first), second(ve.second)
{
}


/** */
ValErrPair& ValErrPair::operator=(const ValErrPair &vep)
{ //{{{
   if (this == &vep)
      return *this;

   ve = vep.ve;

   return *this;
} //}}}


/** */
ostream& operator<<(ostream& os, const ValErrPair &vep)
{ //{{{
   os << "array( " << vep.ve.first << ", " << vep.ve.second << " )";
   //os << "array( " << vep.first << ", " << vep.second << " )";
   //os << "array( " << vep.v << ", " << vep.e << " )";
   //os << "first: " << vep.first << ", second: " << vep.second << endl;
   //os << "v: " << vep.v << ", e: " << vep.e << endl;
   return os;
} //}}}


/** */
bool operator<(const ValErrPair &lhs, const ValErrPair &rhs)
{ //{{{
   if (lhs.ve.first <  rhs.ve.first) return true;
   if (lhs.ve.first == rhs.ve.first && lhs.ve.second < rhs.ve.second) return true;

   return false;
} //}}}


/** */
bool operator==(const ValErrPair &lhs, const ValErrPair &rhs)
{ //{{{
   if (lhs.ve.first == rhs.ve.first && lhs.ve.second == rhs.ve.second) return true;
   return false;
} //}}}
