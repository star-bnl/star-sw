/***************************************************************************
 *
 * $Id: StSvtHybridHist2D.hh,v 1.1 2004/02/06 02:30:35 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Hybrid 2D Histogram BASE class
 *
 ***************************************************************************
 *
 * $Log: StSvtHybridHist2D.hh,v $
 * Revision 1.1  2004/02/06 02:30:35  munhoz
 * inserting SVT online monitor
 *
 **************************************************************************/

#ifndef STSVTHYBRIDHIST2D_HH
#define STSVTHYBRIDHIST2D_HH

#include "StSvtClassLibrary/StSvtHybridObject.hh"
#include "TH1.h"
class TH2F;

class StSvtHybridHist2D: public StSvtHybridObject
{
public:
  StSvtHybridHist2D();
  StSvtHybridHist2D(int barrel, int ladder, int wafer, int hybrid);
  StSvtHybridHist2D(Text_t* name, Text_t* title, Int_t nbinsx, Axis_t xlow, Axis_t xup, Int_t nbinsy, Axis_t ylow, Axis_t yup);
  StSvtHybridHist2D(int barrel, int ladder, int wafer, int hybrid, 
		  Text_t* name, Text_t* title, Int_t nbinsx, Axis_t xlow, Axis_t xup, Int_t nbinsy, Axis_t ylow, Axis_t yup);
  StSvtHybridHist2D(const StSvtHybridHist2D&);
  ~StSvtHybridHist2D();
  StSvtHybridHist2D& operator = (const StSvtHybridHist2D&);

  TH2F* getHist() {return hist;}

protected:

  TH2F* hist; //!

  ClassDef(StSvtHybridHist2D,1)
};

#endif
