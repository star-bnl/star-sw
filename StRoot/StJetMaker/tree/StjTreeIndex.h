// -*- mode: c++;-*-
// $Id: StjTreeIndex.h,v 1.5 2008/10/22 19:50:06 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTREEINDEX_H
#define STJTREEINDEX_H

#include <Rtypes.h>
#include <TObject.h>
#include <ostream>


#ifdef major
#undef major
#endif

#ifdef minor
#undef minor
#endif

class StjTreeIndex : public TObject {

public:
  StjTreeIndex(Int_t major = 0, Int_t minor = 0) : _major(major), _minor(minor) { }
  virtual ~StjTreeIndex() { }

  Int_t major() const { return _major; }
  Int_t minor() const { return _minor; }

private:

  Int_t _major;
  Int_t _minor;

  ClassDef(StjTreeIndex, 1)

};

inline bool operator<(const StjTreeIndex& v1, const StjTreeIndex& v2) {
  if(v1.major() != v2.major()) return v1.major() < v2.major();
  return v1.minor() < v2.minor();
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
  out << "StjTreeIndex: " << v.major() << " " << v.minor();
  return out;
}

#endif // STJTREEINDEX_H
