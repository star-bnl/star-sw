/***************************************************************************
 *
 * $Id: StSvtHist2D.hh,v 1.1 2004/02/06 02:30:34 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Histograms Collection class
 *
 ***************************************************************************
 *
 * $Log: StSvtHist2D.hh,v $
 * Revision 1.1  2004/02/06 02:30:34  munhoz
 * inserting SVT online monitor
 *
 **************************************************************************/

#ifndef STSVTHIST2D_HH
#define STSVTHIST2D_HH

#include "StSvtClassLibrary/StSvtHybridCollection.hh"

class StSvtHybridHist2D;

class StSvtHist2D: public StSvtHybridCollection
{
protected:
  StSvtHybridHist2D* hist;             //!
  Text_t* mTitle;

public:
  StSvtHist2D(char* config = 0, Text_t* name = 0, Text_t* title = 0, 
	      Int_t nbinsx = 0, Axis_t xlow = 0, Axis_t xup = 0,
	      Int_t nbinsy = 0, Axis_t ylow = 0, Axis_t yup = 0);
  StSvtHist2D(const StSvtHist2D&);
  ~StSvtHist2D();
  StSvtHist2D& operator = (const StSvtHist2D&);

  void setHist(Text_t* name, Text_t* title, Int_t nbinsx, Axis_t xlow, Axis_t xup, Int_t nbinsy, Axis_t ylow, Axis_t yup);
  void setTitle(Text_t* title);
  void setThisTitle(Text_t* title);
  Text_t* getTitle(){return mTitle;}
  void setXTitle(Text_t* title);
  void setYTitle(Text_t* title);
  void setMarkerStyle(Int_t style);
  void setMarkerSize(Float_t size);
  void setLineWidth(Int_t width);
  void setBins(Int_t nbinsx, Axis_t xlow, Axis_t xup, Int_t nbinsy, Axis_t ylow, Axis_t yup);
  void setMaximum(Float_t max);
  void setMinimum(Float_t min);
  void setMaximum();
  void setMinimum();
  Float_t getMaximum(int barrelID=0, int ladderID=0);
  Float_t getMinimum(int barrelID=0, int ladderID=0);
  void reset();

  ClassDef(StSvtHist2D,1)
};

#endif
