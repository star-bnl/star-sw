//StiHitTranslator.h
//M.L. Miller (Yale Software)
//04/01

#ifndef StiHitTranslator_HH
#define StiHitTranslator_HH

#include "StiTranslator.h"

class StTpcHit;
class StSvtHit;
class StiHit;

class StiHitTranslator : public StiTranslator
{
public:
    StiHitTranslator();
    virtual ~StiHitTranslator();
    
    void operator() (const StTpcHit*, StiHit*);
    void operator() (const StSvtHit*, StiHit*);
    
protected:
    
};

#endif
