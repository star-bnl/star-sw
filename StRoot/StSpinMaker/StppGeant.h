//////////////////////////////////////////////////////////////////////
//
// $Id: StppGeant.h,v 1.1 2002/01/16 20:22:53 akio Exp $
// $Log: StppGeant.h,v $
// Revision 1.1  2002/01/16 20:22:53  akio
// First version
//
//
// Revision 1.0  2001/06/14 Akio Ogawa
// First Version of StppGeant
//
//////////////////////////////////////////////////////////////////////
//
// StppGeant
//
// geant info class for Spin pp uDst
//
//////////////////////////////////////////////////////////////////////
#ifndef StppGeant_h
#define StppGeant_h
#include "TObject.h"
#include "TDataSet.h"
#include "TClonesArray.h"
class StppParticle;

class StppGeant : public TObject {
 public:
          StppGeant();
  virtual ~StppGeant();

  void    clear();   
#ifndef __CINT__
  Int_t   fill(TDataSet* geant); 
#endif /* __CINT__ */

  Float_t        gXVertex;
  Float_t        gYVertex;
  Float_t        gZVertex;
  Int_t          gNParticle;
  StppParticle  *gLCP;
  TClonesArray  *gParticles;

  void setInfoLevel(Int_t level){infoLevel = level;};

 private:
  Int_t infoLevel;

  ClassDef(StppGeant,1)

};

#endif
