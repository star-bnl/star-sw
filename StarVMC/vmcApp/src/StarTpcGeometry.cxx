/* $Id: StarTpcGeometry.cxx,v 1.1 2004/07/12 20:36:39 potekhin Exp $ */

#include "StarTpcGeometry.h"

ClassImp(StarTpcGeometry)

StarTpcGeometry* StarTpcGeometry::_geo = 0;

//_______________________________________________________________________
  StarTpcGeometry::StarTpcGeometry()
{
  _geo=this;
}
//_______________________________________________________________________
StarTpcGeometry::StarTpcGeometry(const StarTpcGeometry& g_)
{
  _geo=this;
}

//_______________________________________________________________________
StarTpcGeometry::StarTpcGeometry(const char* name_, const char *title_):
  StarGeometry(name_,title_)
{
  _geo=this;
}

//_______________________________________________________________________
StarTpcGeometry::~StarTpcGeometry()
{
}
//_______________________________________________________________________

StarGeometry*  StarTpcGeometry::GetInstance(){   // Returns the pointer of the unique instance
  return _geo; 
}
