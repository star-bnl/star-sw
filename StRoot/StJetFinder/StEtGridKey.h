// -*- mode: c++;-*-
// $Id: StEtGridKey.h,v 1.1 2008/04/29 00:11:09 tai Exp $
#ifndef STETGRIDKEY_H
#define STETGRIDKEY_H

#include <ostream>

class StEtGridKey {

public:
  StEtGridKey() : iEta(0), iPhi(0) {};
  StEtGridKey(int ie, int ip) : iEta(ie), iPhi(ip) {};

  friend std::ostream& operator<<(std::ostream& os, const StEtGridKey& key)
  {
    return os << "iEta:\t" << key.iEta << "\tiPhi:\t" << key.iPhi;
  }

  friend bool operator<(const StEtGridKey& lhs, const StEtGridKey& rhs){
    if (lhs.iEta < rhs.iEta) return true;
    else if (lhs.iEta > rhs.iEta) return false;
    else return lhs.iPhi < rhs.iPhi;
  }

  friend bool operator==(const StEtGridKey& lhs, const StEtGridKey& rhs){
    return !( lhs < rhs ) && !( rhs < lhs);
  }

  friend struct StEtGridKeyLessThan;

  int eta() const { return iEta; }
  int phi() const { return iPhi; }

private:
  int iEta;
  int iPhi;
};


#endif  // STETGRIDKEY_H
