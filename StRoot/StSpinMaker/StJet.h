//////////////////////////////////////////////////////////////////////
//
// $Id: StJet.h,v 1.1 2002/02/11 20:30:48 akio Exp $
// $Log: StJet.h,v $
// Revision 1.1  2002/02/11 20:30:48  akio
// Many updates, including very first version of jet finder.
//
//
// Revision 1.0  2002/01/25 Akio Ogawa
// First Version of StJet 
//
//////////////////////////////////////////////////////////////////////
//
// StJet
//
// Event class for a Jet
//
//////////////////////////////////////////////////////////////////////
#ifndef StJet_h
#define StJet_h

#include "TObject.h"
class StEtCell;

class StJet : public TObject {

public:  
  StJet();
  StJet(StJet*);
  virtual ~StJet();

  void  print();
  void  add(StEtCell *);

  Float_t      et;
  Float_t      phi;
  Float_t      ex;
  Float_t      ey;
  Float_t      eta;  
  Int_t        nCell;

  ClassDef(StJet,2)
};

#endif
