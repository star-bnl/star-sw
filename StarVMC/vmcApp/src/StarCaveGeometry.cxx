/* $Id: StarCaveGeometry.cxx,v 1.1 2004/07/12 20:36:38 potekhin Exp $ */

#include "StarCaveGeometry.h"

ClassImp(StarCaveGeometry)

StarCaveGeometry* StarCaveGeometry::_geo = 0;

//_______________________________________________________________________
  StarCaveGeometry::StarCaveGeometry() {
    Double_t world[3];
//     world[0] = halfWorldLength;
//     world[1] = halfWorldLength;
//     world[2] = halfWorldLength;
//     gMC->Gsvolu("WRLD", "BOX", fImedAir, world, 3);
    _geo=this;
}
//_______________________________________________________________________
StarCaveGeometry::StarCaveGeometry(const StarCaveGeometry& g_) {
  _geo=this;
}

//_______________________________________________________________________
StarCaveGeometry::StarCaveGeometry(const char* name_, const char *title_):StarGeometry(name_,title_) {
  _geo=this;
}

//_______________________________________________________________________
StarCaveGeometry::~StarCaveGeometry()
{
}
//_______________________________________________________________________

StarGeometry*  StarCaveGeometry::GetInstance(){   // Returns the pointer of the unique instance
  return _geo; 
}
