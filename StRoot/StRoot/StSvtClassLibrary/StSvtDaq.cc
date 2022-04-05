/***************************************************************************
 *
 * $Id: StSvtDaq.cc,v 1.1 2004/01/30 00:13:03 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Daq Parameters
 *
 ***************************************************************************
 *
 * $Log: StSvtDaq.cc,v $
 * Revision 1.1  2004/01/30 00:13:03  munhoz
 * daq parameters object
 *
 *
 **************************************************************************/
////////////////////////////////////////////////////////////////////////////
//                                                                        //
// This class represents the SVT Daq PArameters object.                   //
//                                                                        //
////////////////////////////////////////////////////////////////////////////


#include "StSvtDaq.hh"
#include "StMessMgr.h"

ClassImp(StSvtDaq)

StSvtDaq::StSvtDaq()
{}

StSvtDaq::~StSvtDaq()
{}

StSvtDaq::StSvtDaq(const StSvtDaq& daqParam)
{}

StSvtDaq& StSvtDaq::operator = (const StSvtDaq& daqParam)
{
  return *this;
}
