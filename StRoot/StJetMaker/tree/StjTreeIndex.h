// -*- mode: c++;-*-
// $Id: StjTreeIndex.h,v 1.6 2015/08/14 16:37:32 rfatemi Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTREEINDEX_H
#define STJTREEINDEX_H

#include <Rtypes.h>
#include <TObject.h>
#include <ostream>


class StjTreeIndex : public TObject {

public:
  StjTreeIndex(Int_t big = 0, Int_t small = 0) : Big(big), Small(small) { }
  virtual ~StjTreeIndex() { }


  Int_t big() const { return Big; }
  Int_t small() const { return Small; }

private:

  Int_t Big;
  Int_t Small;

  ClassDef(StjTreeIndex, 1)

};

inline bool operator<(const StjTreeIndex& v1, const StjTreeIndex& v2) {
  if(v1.big() != v2.big()) return v1.big() < v2.big();
  return v1.small() < v2.small();
}

inline bool operator>(const StjTreeIndex& v1, const StjTreeIndex& v2) {
    return v2 < v1;
}

inline bool operator==(const StjTreeIndex& v1, const StjTreeIndex& v2){
  return !( v1 > v2 ) && !( v2 > v1);
}

inline bool operator!=(const StjTreeIndex& v1, const StjTreeIndex& v2)
{
  return(!(v1 == v2));
}

inline std::ostream& operator<<(std::ostream& out, const StjTreeIndex& v)
{
  out << "StjTreeIndex: " << v.big() << " " << v.small();
  return out;
}

#endif // STJTREEINDEX_H
