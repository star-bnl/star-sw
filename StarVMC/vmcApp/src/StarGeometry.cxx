/* $Id: StarGeometry.cxx,v 1.1 2004/07/12 20:36:38 potekhin Exp $ */

#include "StarGeometry.h"

ClassImp(StarGeometry)

;


//_______________________________________________________________________
StarGeometry::StarGeometry() {}
StarGeometry::StarGeometry(const StarGeometry& g_) {}
StarGeometry::StarGeometry(const char* name_, const char *title_): TNamed(name_,title_) {}
StarGeometry::~StarGeometry() {}
