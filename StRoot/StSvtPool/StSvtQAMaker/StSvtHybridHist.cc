/***************************************************************************
 *
 * $Id: StSvtHybridHist.cc,v 1.1 2004/02/06 02:30:35 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Hybrid Histogram BASE class
 *
 ***************************************************************************
 *
 * $Log: StSvtHybridHist.cc,v $
 * Revision 1.1  2004/02/06 02:30:35  munhoz
 * inserting SVT online monitor
 *
 **************************************************************************/

#include "StSvtHybridHist.hh"

ClassImp(StSvtHybridHist)

StSvtHybridHist::StSvtHybridHist() : StSvtHybridObject()
{}

StSvtHybridHist::StSvtHybridHist(int barrel, int ladder, int wafer, int hybrid) : 
  StSvtHybridObject(barrel, ladder, wafer, hybrid)
{}

StSvtHybridHist::StSvtHybridHist(Text_t* name, Text_t* title, Int_t nbinsx, Axis_t xlow, Axis_t xup) : 
  StSvtHybridObject()
{
  hist = new TH1F(name, title, nbinsx, xlow, xup);
}

StSvtHybridHist::StSvtHybridHist(int barrel, int ladder, int wafer, int hybrid, 
				 Text_t* name, Text_t* title, Int_t nbinsx, Axis_t xlow, Axis_t xup) : 
  StSvtHybridObject(barrel, ladder, wafer, hybrid)
{
  hist = new TH1F(name,  title, nbinsx, xlow, xup);
}

StSvtHybridHist::~StSvtHybridHist()
{
  delete hist;
}
