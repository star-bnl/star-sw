// Hey Emacs this is -*-c++-*-
#ifndef STAR_EEmcTower_h
#define STAR_EEmcTower_h
// $Id: EEmcTower.h,v 1.5 2005/02/05 00:53:28 perev Exp $


#include <Stiostream.h>

#include "TObject.h"
#include "TVector3.h"

#if !defined(ST_NO_NAMESPACES)
using std::ostream;
#endif


class EEmcTower : public TObject  {
public:
  /// the default constructor
  EEmcTower()  { mSec=mSub=mEta=-1; mADC=mEdep=0.0; };
  
  /// the explicit constructor
  EEmcTower(int s, int ss, int e, float adc=0.0, float ene=0.0) { 
    mSec  = (char)s;
    mSub  = (char)ss;
    mEta  = (char)e;
    mADC  = adc;
    mEdep = ene;
  };

  /// an explicit constructor that uses labels rather intigers
  EEmcTower(const char *label, float adc=0.0, float ene=0.0);
  
  /// a copy constructor
  EEmcTower(const EEmcTower& t) {
    mSec  = (char) t.Sec();
    mSub  = (char) t.SubSec();
    mEta  = (char) t.Eta();
    mADC  = t.ADC();
    mEdep = t.dE();
  }

  /// the destructor
  ~EEmcTower() { };

  /// gets adc value associated with the tower (pedestal adjusted)
  float ADC() const    { return mADC; }
  /// sets adc value associated with the tower (pedestal adjusted)
  float ADC(float d)   { mADC=d; return mADC; }

  /// gets calibrated energy loss value associated with the tower
  float dE() const    { return mEdep; }
  /// sets calibrated energy loss value associated with the tower
  float dE(float e)   { mEdep=e; return mEdep; }
  
  /// gets tower sector index, computer offset [0,....)
  int  Sec   () const { return mSec; }
  /// sets tower sector index, computer offset [0,....)
  int  Sec   (int s)  { mSec=(char)s; return s; }

  /// gets tower subsector index, computer offset [0,....) 
  int  SubSec() const { return mSub; }
  /// sets tower subsector index, computer offset [0,....) 
  int  SubSec(int s)  { mSub=(char)s; return s; }

  /// gets tower eta index, computer offset [0,....) 
  int  Eta   () const { return mEta; }
  /// sets tower eta index, computer offset [0,....) 
  int  Eta   (int e)  { mEta=(char)e; return e; }
  

  /// gets tower sector label, human offset [1..12]
  int  SecLabel   () const { return mSec+1; }
  /// sets tower sector label, human offset [1..12]
  int  SecLabel   (int s)  { mSec=(char)(s-1  ); return s; }

  /// gets tower subsector label, human offset [A..E]
  int  SubSecLabel() const { return mSub+'A'; }
  /// sets tower subsector label, human offset [A..E]
  int  SubSecLabel(int s)  { mSub=(char)(s-'A'); return s; }

  /// gets tower eta label, human offset [1..12]
  int  EtaLabel   () const { return mEta+1; }
  /// sets tower eta label, human offset [1..12]
  int  EtaLabel   (int e)  { mEta=(char)(e-1  ); return e; }

  /// returns tower label, e.g. "05TB09"
  const char *TowerLabel() const; 

  /// print tower hit info in xml-like style
  ostream& Out ( ostream &out ) const ;  

  /// compare two towers
  bool operator==(const EEmcTower &t) {
    if(mSec!=t.Sec())    return false;
    if(mSub!=t.SubSec()) return false;
    if(mEta!=t.Eta())    return false;
    return true;
  }
  
  /// compare two towers
  bool operator!=(const EEmcTower &t) {
    if(mSec==t.Sec())    return false;
    if(mSub==t.SubSec()) return false;
    if(mEta==t.Eta())    return false;
    return true;
  }
  
  /// copy one tower to another
  ///EEmcTower& operator=(const EEmcTower &t);

private:
  bool  ParseLabel(const char* label);
  //
  char  mSec;   //
  char  mSub;   //
  char  mEta;   //
  char  mDummy; // byte align
  float mADC;   //
  float mEdep;  //
public:
  ClassDef(EEmcTower, 3)   // 
};

/// print EEmcTower to a ostream 
/// \param out a reference to an ostream 
/// \param t   a reference to a EEmcTower
ostream&  operator<<(ostream &out, const EEmcTower    &t  );

#endif
