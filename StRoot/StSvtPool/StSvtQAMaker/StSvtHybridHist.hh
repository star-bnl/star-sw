/***************************************************************************
 *
 * $Id: StSvtHybridHist.hh,v 1.1 2004/02/06 02:30:35 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Hybrid Histogram BASE class
 *
 ***************************************************************************
 *
 * $Log: StSvtHybridHist.hh,v $
 * Revision 1.1  2004/02/06 02:30:35  munhoz
 * inserting SVT online monitor
 *
 **************************************************************************/

#ifndef STSVTHYBRIDHIST_HH
#define STSVTHYBRIDHIST_HH

#include "StSvtClassLibrary/StSvtHybridObject.hh"
#include "TH1.h"
//class TH1F;

class StSvtHybridHist: public StSvtHybridObject
{
public:
  StSvtHybridHist();
  StSvtHybridHist(int barrel, int ladder, int wafer, int hybrid);
  StSvtHybridHist(Text_t* name, Text_t* title, Int_t nbinsx, Axis_t xlow, Axis_t xup);
  StSvtHybridHist(int barrel, int ladder, int wafer, int hybrid, 
		  Text_t* name, Text_t* title, Int_t nbinsx, Axis_t xlow, Axis_t xup);
  StSvtHybridHist(const StSvtHybridHist&);
  ~StSvtHybridHist();
  StSvtHybridHist& operator = (const StSvtHybridHist&);

  TH1F* getHist() {return hist;}

protected:

  TH1F* hist; //

  ClassDef(StSvtHybridHist,1)
};

#endif
