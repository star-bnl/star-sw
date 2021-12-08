#ifndef __KFParticleXPropagator__
#define __KFParticleXPropagator__
#include "KFParticle.h"
class KFParticleXPropagator : public TObject {
 public: 
  KFParticleXPropagator() {fgKFParticleXPropagator = this;}
  virtual ~KFParticleXPropagator() {fgKFParticleXPropagator = 0;}
  static KFParticleXPropagator *instance() {return fgKFParticleXPropagator;}
  static KFParticle &GetFParticle() {return *&fgParticle;}
  static KFParticle &GetTParticle() {return *&fgmParticle;}
 private:
  static KFParticleXPropagator *fgKFParticleXPropagator;
 protected:
  static KFParticle fgParticle; // Initial particle
  static KFParticle fgmParticle; // After calculation of DS;
  ClassDef(KFParticleXPropagator,1)
};
#endif /* __KFParticleXPropagator__ */
