//StiHit.cxx
//M.L. Miller (Yale Software)
//04/01

#include <iostream>
#include "StiHit.h"

ostream& operator<<(ostream& os, const StiHit& hit)
{
    return os <<hit.sector()<<"\t"<<hit.padrow()<<"\t"<<hit.x()<<"\t"<<hit.y()<<"\t"<<hit.z();
}
