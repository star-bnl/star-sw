// $Id: StvELossTrakX.h,v 1.1.2.1 2020/01/24 20:41:08 perev Exp $
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
ClassDef(StvELossTrak,0) 
};
#endif //STVELOSSTRAKX_H   
