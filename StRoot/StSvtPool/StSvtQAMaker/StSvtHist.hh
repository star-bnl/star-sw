/***************************************************************************
 *
 * $Id: StSvtHist.hh,v 1.1 2004/02/06 02:30:34 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Histograms Collection class
 *
 ***************************************************************************
 *
 * $Log: StSvtHist.hh,v $
 * Revision 1.1  2004/02/06 02:30:34  munhoz
 * inserting SVT online monitor
 *
 **************************************************************************/

#ifndef STSVTHIST_HH
#define STSVTHIST_HH

#include "StSvtClassLibrary/StSvtHybridCollection.hh"

class StSvtHybridHist;

class StSvtHist: public StSvtHybridCollection
{
protected:
  StSvtHybridHist* hist;             //!
  Text_t* mTitle;                    //!

public:
  StSvtHist(char* config = 0, Text_t* name = 0, Text_t* title = 0, 
	    Int_t nbinsx = 0, Float_t xlow = 0, Float_t xup = 0);
  StSvtHist(const StSvtHist&);
  virtual ~StSvtHist();
  StSvtHist& operator = (const StSvtHist&);

  void setHist(Text_t* name, Text_t* title, Int_t nbinsx, Axis_t xlow, Axis_t xup);
  void setTitle(Text_t* title);
  void setThisTitle(Text_t* title);
  Text_t* getTitle(){return mTitle;}
  void setXTitle(Text_t* title);
  void setYTitle(Text_t* title);
  void setBins(Int_t nbinsx, Axis_t xlow, Axis_t xup);
  void setMaximum(Float_t max);
  void setMinimum(Float_t min);
  void setMaximum();
  void setMinimum();
  Float_t getMaximum(int barrelID=0, int ladderID=0);
  Float_t getMinimum(int barrelID=0, int ladderID=0);
  void reset();

  ClassDef(StSvtHist,1)
};

#endif
