/***************************************************************************
 *
 * $Id: StSvtHistAnalog.hh,v 1.1 2004/02/06 02:30:35 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Histograms Collection class
 *
 ***************************************************************************
 *
 * $Log: StSvtHistAnalog.hh,v $
 * Revision 1.1  2004/02/06 02:30:35  munhoz
 * inserting SVT online monitor
 *
 **************************************************************************/

#ifndef STSVTHISTANALOG_HH
#define STSVTHISTANALOG_HH

#include "StSvtClassLibrary/StSvtHybridCollection.hh"

class StSvtHybridHistAnalog;

class StSvtHistAnalog: public StSvtHybridCollection
{
protected:
  StSvtHybridHistAnalog* hist;             //!
  Text_t* mTitle;                    //!

public:
  StSvtHistAnalog(char* config=0, Text_t* name=0, Text_t* title=0, Int_t nbinsx=0, Axis_t xlow=0, Axis_t xup=0);
  StSvtHistAnalog(const StSvtHistAnalog&);
  virtual ~StSvtHistAnalog();
  StSvtHistAnalog& operator = (const StSvtHistAnalog&);

  void setHist(Text_t* name, Text_t* title, Int_t nbinsx, Axis_t xlow, Axis_t xup);
  void setTitle(Text_t* title);
  void setThisTitle(Text_t* title);
  Text_t* getTitle(){return mTitle;}
  void setXTitle(Text_t* title);
  void reset();

  Float_t getMaximum(int barrelID=0, int ladderID=0);
  Float_t getMinimum(int barrelID=0, int ladderID=0);

  ClassDef(StSvtHistAnalog,1)
};

#endif
