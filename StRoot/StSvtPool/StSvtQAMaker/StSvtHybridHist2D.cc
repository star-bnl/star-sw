/***************************************************************************
 *
 * $Id: StSvtHybridHist2D.cc,v 1.1 2004/02/06 02:30:35 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Hybrid Histogram 2D BASE class
 *
 ***************************************************************************
 *
 * $Log: StSvtHybridHist2D.cc,v $
 * Revision 1.1  2004/02/06 02:30:35  munhoz
 * inserting SVT online monitor
 *
 **************************************************************************/

#include "StSvtHybridHist2D.hh"
#include "TH2.h"

ClassImp(StSvtHybridHist2D)

StSvtHybridHist2D::StSvtHybridHist2D() : StSvtHybridObject()
{}

StSvtHybridHist2D::StSvtHybridHist2D(int barrel, int ladder, int wafer, int hybrid) : 
  StSvtHybridObject(barrel, ladder, wafer, hybrid)
{}

StSvtHybridHist2D::StSvtHybridHist2D(Text_t* name, Text_t* title, Int_t nbinsx, Axis_t xlow, Axis_t xup, Int_t nbinsy, Axis_t ylow, Axis_t yup) : 
  StSvtHybridObject()
{
hist = new  TH2F(name, title, nbinsx, xlow, xup, nbinsy, ylow, yup);
}

StSvtHybridHist2D::StSvtHybridHist2D(int barrel, int ladder, int wafer, int hybrid, 
				 Text_t* name, Text_t* title, Int_t nbinsx, Axis_t xlow, Axis_t xup, Int_t nbinsy, Axis_t ylow, Axis_t yup) : 
  StSvtHybridObject(barrel, ladder, wafer, hybrid)
{
hist = new  TH2F(name,  title, nbinsx, xlow, xup, nbinsy, ylow, yup);
}

StSvtHybridHist2D::~StSvtHybridHist2D()
{
delete hist;
}
