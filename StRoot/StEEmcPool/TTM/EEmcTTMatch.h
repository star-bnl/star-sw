// Hey Emacs this is -*-c++-*-
#ifndef STAR_EEmcTTMatch
#define STAR_EEmcTTMatch
// $Id: EEmcTTMatch.h,v 1.1 2004/05/04 18:28:57 zolnie Exp $

/*!
 *                                                                     
 * \class  EEmcTTMatch
 * \author Piotr A. Zolnierczuk
 * \date   2004/04/30
 *
 * \brief  FIXME
 *
 * FIXME
 * 
 */                                                                      
#include <ostream>
#include <map>


#include "TObject.h"
#include "TVector3.h"

#if !defined(ST_NO_NAMESPACES)
using std::map;
using std::ostream;
#endif

class StMuTrack;

class EEmcTower : public TObject  {
public:
  EEmcTower()  { mSec=mSub=mEta=-1; mEdep=0.0; mLabel=NULL; };
  EEmcTower(int s, int ss, int e, float ene=0.0) { 
    mSec  = (unsigned char)s;
    mSub  = (unsigned char)ss;
    mEta  = (unsigned char)e;
    mEdep = ene;
    mLabel= NULL;
    WriteLabel();
  };
  EEmcTower(const char *label, float ene=0.0);

  ~EEmcTower() { if(mLabel) delete mLabel; }

  float dE() const    { return mEdep; }
  float dE(float e)   { mEdep=e; return mEdep; }
  
  // get/set tower computer indices 0-offset (0,....)
  int  Sec   () const { return mSec; }
  int  Sec   (int s)  { mSec=(char)s; return s; }
  //
  int  SubSec() const { return mSub; }
  int  SubSec(int s)  { mSub=(char)s; return s; }
  //
  int  Eta   () const { return mEta; }
  int  Eta   (int e)  { mEta=(char)e; return e; }
  //
  // get/set tower mortal human indices 1-offset (1-12,'A'-'E')
  int  SecLabel   () const { return mSec+1; }
  int  SecLabel   (int s)  { mSec=(char)(s-1  ); return s; }
  //
  int  SubSecLabel() const { return mSub+'A'; }
  int  SubSecLabel(int s)  { mSub=(char)(s-'A'); return s; }
  //
  int  EtaLabel   () const { return mEta+1; }
  int  EtaLabel   (int e)  { mEta=(char)(e-1  ); return e; }

  //maybe later will implement 
  const char *TowerLabel() { return mLabel; }
  
  //
  // print tower hit info in xml-like style
  ostream& Out ( ostream &out ) const ;  
  
private:
  //
  void  WriteLabel();
  void  ParseLabel(const char* label);
  //
  char  mSec;
  char  mSub;
  char  mEta;
  float mEdep;
  char *mLabel; //! do not store this variable
public:
  ClassDef(EEmcTower, 1)   // 
};



class EEmcTTMatch: public EEmcTower {
public:
  EEmcTTMatch();
  ~EEmcTTMatch(); 

  void    Clear(Option_t *opt);
  void    Add(EEmcTower *t) { mTower = t; }
  void    Add(StMuTrack *t);
  EEmcTower *Tower()   { return mTower;   }
  TList     *Tracks()  { return mTracks;  }
  Int_t      Matches() { return mNTracks; }

  ostream& Out(ostream &out ) const; 


  static  Bool_t  ExtrapolateToZ    ( const StMuTrack *track , const double  z, TVector3 &r); 

private:
   EEmcTower *mTower; 
   TList     *mTracks;
   Int_t      mNTracks; //! 

public:

  ClassDef(EEmcTTMatch, 1)   // 

};


ostream&  operator<<(ostream &out, const EEmcTower    &t  );
ostream&  operator<<(ostream &out, const StMuTrack    &t  );  
ostream&  operator<<(ostream &out, const EEmcTTMatch  &m  );

#endif
