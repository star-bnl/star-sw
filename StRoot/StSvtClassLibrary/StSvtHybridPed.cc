/***************************************************************************
 *
 * $Id: StSvtHybridPed.cc,v 1.1 2000/06/15 15:45:54 caines Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Hybrid Pedestal class
 *
 ***************************************************************************
 *
 * $Log: StSvtHybridPed.cc,v $
 * Revision 1.1  2000/06/15 15:45:54  caines
 * Add Pedestal Class for SVT
 *
 **************************************************************************/
////////////////////////////////////////////////////////////////////////////
//                                                                        //
// This is the class containing the pedestals of each hybrid.                 //
//                                                                        //
////////////////////////////////////////////////////////////////////////////

#include "StSvtHybridPed.hh"

ClassImp(StSvtHybridPed)

StSvtHybridPed::StSvtHybridPed() : 
  StSvtHybridPixels()
{}

StSvtHybridPed::StSvtHybridPed(int barrel, int ladder, int wafer, int hybrid, pedestalType type) : 
  StSvtHybridPixels(barrel, ladder, wafer, hybrid)
{
  mType = type;
}

