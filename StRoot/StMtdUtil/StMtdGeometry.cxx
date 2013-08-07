/********************************************************************
 * $Id: StMtdGeometry.cxx,v 1.3 2013/08/07 18:25:30 geurts Exp $
 ********************************************************************
 *
 * $Log: StMtdGeometry.cxx,v $
 * Revision 1.3  2013/08/07 18:25:30  geurts
 * include CVS Id and Log tags
 *
 *
 *******************************************************************/
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
