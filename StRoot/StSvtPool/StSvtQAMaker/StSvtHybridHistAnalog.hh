/***************************************************************************
 *
 * $Id: StSvtHybridHistAnalog.hh,v 1.2 2004/05/12 17:47:57 perev Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Hybrid Histogram BASE class
 *
 ***************************************************************************
 *
 * $Log: StSvtHybridHistAnalog.hh,v $
 * Revision 1.2  2004/05/12 17:47:57  perev
 * WarnOff
 *
 * Revision 1.1  2004/02/06 02:30:35  munhoz
 * inserting SVT online monitor
 *
 **************************************************************************/

#ifndef STSVTHYBRIDHISTANALOG_HH
#define STSVTHYBRIDHISTANALOG_HH

#include "StSvtClassLibrary/StSvtHybridObject.hh"
#include "TH1.h"
class TH1F;

class StSvtHybridHistAnalog: public StSvtHybridObject
{
public:
  StSvtHybridHistAnalog();
  StSvtHybridHistAnalog(int barrel, int ladder, int wafer, int hybrid);
  StSvtHybridHistAnalog(Text_t* name, Text_t* title, Int_t nbinsx, Axis_t xlow, Axis_t xup);
  StSvtHybridHistAnalog(int barrel, int ladder, int wafer, int hybrid, 
			Text_t* name, Text_t* title, Int_t nbinsx, Axis_t xlow, Axis_t xup);
  StSvtHybridHistAnalog(const StSvtHybridHistAnalog&);
  ~StSvtHybridHistAnalog();
  StSvtHybridHistAnalog& operator = (const StSvtHybridHistAnalog&);

  TH1F* getHist(int n=1);

  void Fill(float x, int anode);
  void Draw(const char* option=0);
  void Reset(int n=0);

protected:

  TH1F* histA; //!
  TH1F* histB; //!
  TH1F* histC; //!

  ClassDef(StSvtHybridHistAnalog,1)
};

#endif
