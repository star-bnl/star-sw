/***************************************************************************
 *
 * $Id: StSvtHybridDriftCurve.cc,v 1.2 2004/07/27 19:59:32 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT drift curve 
 *
 ***************************************************************************
 *
 * $Log: StSvtHybridDriftCurve.cc,v $
 * Revision 1.2  2004/07/27 19:59:32  munhoz
 * fix initialization
 *
 * Revision 1.1  2004/07/26 00:04:37  munhoz
 * adding drift curve class
 *
 *
 **************************************************************************/
////////////////////////////////////////////////////////////////////////////
//                                                                        //
// This class represents the SVT drift curve object.                   //
//                                                                        //
////////////////////////////////////////////////////////////////////////////


#include "StSvtHybridDriftCurve.hh"

ClassImp(StSvtHybridDriftCurve)

StSvtHybridDriftCurve::StSvtHybridDriftCurve():StSvtHybridObject()
{
  //Default constructor.
  for (int i=0; i<3; i++)
    for (int j=0; j<10; j++)
      driftCurve[i][j] = 0;

}

StSvtHybridDriftCurve::StSvtHybridDriftCurve(int barrel, int ladder, int wafer, int hybrid):StSvtHybridObject(barrel, ladder, wafer, hybrid)
{
  for (int i=0; i<3; i++)
    for (int j=0; j<10; j++)
      driftCurve[i][j] = 0;
    
}

StSvtHybridDriftCurve::~StSvtHybridDriftCurve()
{}

StSvtHybridDriftCurve::StSvtHybridDriftCurve(const StSvtHybridDriftCurve& hybrid)
{}

StSvtHybridDriftCurve& StSvtHybridDriftCurve::operator = (const StSvtHybridDriftCurve& hybrid)
{
  return *this;
}

