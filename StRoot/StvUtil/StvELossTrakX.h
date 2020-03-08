// $Id: StvELossTrakX.h,v 1.1.2.2 2020/03/08 19:37:53 perev Exp $
//
//
// Class StvELossTrakX
// ------------------


#ifndef STIELOSSTRAKX_H
#define STIELOSSTRAKX_H
#include "StvUtil/StvELossTrak.h"
 class StvELossTrakX : public StvELossTrak
{
public:
         StvELossTrakX(){Reset();}
        ~StvELossTrakX(){;}
virtual void Add(double len);
protected:
ClassDef(StvELossTrakX,0) 
};
#endif //STVELOSSTRAKX_H   
