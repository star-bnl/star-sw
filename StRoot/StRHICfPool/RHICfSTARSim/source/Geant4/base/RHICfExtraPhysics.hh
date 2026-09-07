#ifndef RHICFExtraPhysics_h
#define RHICFExtraPhysics_h 1

#include "globals.hh"

#include "G4VPhysicsConstructor.hh"

class RHICfExtraPhysics : public G4VPhysicsConstructor
{
  public:

    RHICfExtraPhysics();
    virtual ~RHICfExtraPhysics();

    virtual void ConstructParticle();
    virtual void ConstructProcess();

};

#endif
