#ifndef __StarParticleReader_h__
#define __StarParticleReader_h__

#include "TObject.h"
class StarParticleStack;
class StarParticleData;

/**
   \class StarParticleReader
   \author Jason C. Webb
   \brief Pushes particles out from the StarParticleStack to geant3.
   
*/

class StarParticleReader : public TObject
{
 public:

  ~StarParticleReader(){ /* nada */ };

  /// Set the particle stack from which we will read events
  /// @param stack The particle stack
  void SetStack( StarParticleStack *stack ){ mStack = stack; }

  /// Read events from the particle stack and push them out to starsim
  void ReadEvent();

  /// Return the single instance of this class
  static StarParticleReader &Instance();

  void SetVert( Float_t *vertex, Int_t ntbeam, Int_t nttarg, Float_t *ubuf, Int_t nu, Int_t &nv );
  void SetKine( Float_t *plab,   Int_t idpart, Int_t nv,     Float_t *ubuf, Int_t nb, Int_t &nt );

 private:
 protected:

  // The particle stack
  StarParticleStack *mStack;

  static StarParticleReader *mInstance;
  StarParticleData    *mParticleData;

  StarParticleReader();


  ClassDef(StarParticleReader,1);

};

#endif
