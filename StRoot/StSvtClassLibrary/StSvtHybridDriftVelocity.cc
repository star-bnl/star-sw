/***************************************************************************
 *
 * $Id: StSvtHybridDriftVelocity.cc,v 1.1 2000/11/30 20:38:30 caines Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Hybrid BASE class
 *
 ***************************************************************************
 *
 * $Log: StSvtHybridDriftVelocity.cc,v $
 * Revision 1.1  2000/11/30 20:38:30  caines
 * Drift Velocity files
 *
 **************************************************************************/
////////////////////////////////////////////////////////////////////////////
//                                                                        //
// This class represents the SVT drift velocity object.                   //
//                                                                        //
////////////////////////////////////////////////////////////////////////////


#include "StSvtHybridDriftVelocity.hh"

ClassImp(StSvtHybridDriftVelocity)

StSvtHybridDriftVelocity::StSvtHybridDriftVelocity():StSvtHybridObject()
{
  //Default constructor.
  numberOfAnodes = 240;
}

StSvtHybridDriftVelocity::StSvtHybridDriftVelocity(int barrel, int ladder, int wafer, int hybrid):StSvtHybridObject(barrel, ladder, wafer, hybrid)
{
  numberOfAnodes = 240;
}

StSvtHybridDriftVelocity::~StSvtHybridDriftVelocity()
{}

StSvtHybridDriftVelocity::StSvtHybridDriftVelocity(const StSvtHybridDriftVelocity& hybrid)
{}

StSvtHybridDriftVelocity& StSvtHybridDriftVelocity::operator = (const StSvtHybridDriftVelocity& hybrid)
{
  return *this;
}

float StSvtHybridDriftVelocity::operator [] (int anode)
{
  return 0;
}

