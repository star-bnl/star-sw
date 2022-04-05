#include "StMcHitI.h"
// MS event hit interface

StThreeVectorF StMcHitI::position() const
{ return StThreeVectorF(x(),y(),z()); }
StThreeVectorF  StMcHitI::localMomentum() const
{ return   StThreeVectorF(px(),py(),pz()); }    
