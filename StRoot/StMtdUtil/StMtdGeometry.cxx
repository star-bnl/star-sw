#include "StMtdGeometry.h"
#include "StMessMgr.h" 

StMtdGeometry *gMtdGeometry = 0;


// ___________________________________________________________________________
StMtdGeometry::StMtdGeometry(const char* name, const char* title)
  : TNamed(name,title)
{
  LOG_INFO << "Noop" << endm;
  gMtdGeometry = this;
}


StMtdGeometry::~StMtdGeometry()
{
  LOG_INFO << "StMtdGeometry at pointer =" << (void*)gMtdGeometry
	   << " will be deleted" << endm;
  gMtdGeometry = 0;
}
