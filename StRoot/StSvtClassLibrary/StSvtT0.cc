/***************************************************************************
 *
 * $Id: StSvtT0.cc,v 1.1 2003/04/14 15:48:19 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT t0
 *
 ***************************************************************************
 *
 * $Log: StSvtT0.cc,v $
 * Revision 1.1  2003/04/14 15:48:19  munhoz
 * adding t0 object
 *
 *
 **************************************************************************/
////////////////////////////////////////////////////////////////////////////
//                                                                        //
//             This class represents the SVT t0 object.                   //
//                                                                        //
////////////////////////////////////////////////////////////////////////////

#include "StSvtT0.hh"
#include "StMessMgr.h"

ClassImp(StSvtT0)

StSvtT0::StSvtT0() : StObject()
{}

StSvtT0::~StSvtT0()
{}

StSvtT0::StSvtT0(const StSvtT0& geom)
{}

StSvtT0& StSvtT0::operator = (const StSvtT0& geom)
{
  return *this;
}
