#ifndef __AgUDecay_h__
#define __AgUDecay_h__

#include "TObject.h"
#include "TLorentzVector.h"
#include "StarDecayManager.h"
class TClonesArray;

class TVirtualMCDecayer;

/**!
   \class AgUDecay
   \brief Interface between starsim and virtual decayer (VMC implementation)

 */

class AgUDecay : public TObject
{
public:

  static AgUDecay &instance(){ return sInstance; }
  void   SetDecayer( StarDecayManager *dcay = 0 ){ mDecayer = dcay; }

  static StarDecayManager *Manager(){ 
    StarDecayManager *d = new StarDecayManager();
    instance().SetDecayer( d );
    return d;
  };

  Int_t operator()();

private:
protected:

  AgUDecay();

  static AgUDecay sInstance;

  StarDecayManager *mDecayer;
  TClonesArray *mArray;
  TLorentzVector mP;

  ClassDef(AgUDecay,1);
};

#endif
