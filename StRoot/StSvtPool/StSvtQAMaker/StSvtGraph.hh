/***************************************************************************
 *
 * $Id: StSvtGraph.hh,v 1.1 2004/02/06 02:30:34 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Graph Collection class
 *
 ***************************************************************************
 *
 * $Log: StSvtGraph.hh,v $
 * Revision 1.1  2004/02/06 02:30:34  munhoz
 * inserting SVT online monitor
 *
 **************************************************************************/

#ifndef STSVTGRAPH_HH
#define STSVTGRAPH_HH

#include "StSvtClassLibrary/StSvtHybridCollection.hh"

class StSvtHybridGraph;

class StSvtGraph: public StSvtHybridCollection
{
protected:
  StSvtHybridGraph* graph;             //!
  Text_t* mTitle;                    //!

public:
  StSvtGraph(char* config = 0);
  StSvtGraph(const StSvtGraph&);
  virtual ~StSvtGraph();
  StSvtGraph& operator = (const StSvtGraph&);

  void setGraph();

  void setTitle(Text_t* title);
  void setThisTitle(Text_t* title);
  void setXTitle(Text_t* title);
  void setYTitle(Text_t* title);

  void setDefaultMaximum(float max);
  void setDefaultMinimum(float min);
  void setMaximum(Float_t max);
  void setMinimum(Float_t min);
  void setMaximum();
  void setMinimum();
  Float_t getMaximum(int barrelID=0, int ladderID=0);
  Float_t getMinimum(int barrelID=0, int ladderID=0);

  void reset();

  Bool_t isGlobalMax(){return kTRUE;}
  Bool_t isGlobalMin(){return kTRUE;}

  ClassDef(StSvtGraph,1)
};

#endif
