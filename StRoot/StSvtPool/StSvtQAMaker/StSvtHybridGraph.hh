/***************************************************************************
 *
 * $Id: StSvtHybridGraph.hh,v 1.1 2004/02/06 02:30:35 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Hybrid Histogram BASE class
 *
 ***************************************************************************
 *
 * $Log: StSvtHybridGraph.hh,v $
 * Revision 1.1  2004/02/06 02:30:35  munhoz
 * inserting SVT online monitor
 *
 **************************************************************************/

#ifndef STSVTHYBRIDGRAPH_HH
#define STSVTHYBRIDGRAPH_HH

#include "StSvtClassLibrary/StSvtHybridObject.hh"
#include "TString.h"

class StSvtHybridCollection;

class TGraph;
class TH1F;
class TArrayF;

class StSvtHybridGraph: public StSvtHybridObject
{
public:
  StSvtHybridGraph();
  StSvtHybridGraph(int barrel, int ladder, int wafer, int hybrid);
  StSvtHybridGraph(const StSvtHybridGraph&);
  ~StSvtHybridGraph();
  StSvtHybridGraph& operator = (const StSvtHybridGraph&);

  TGraph* getGraph() {return graph;}
  void setGraph(TGraph* gr) {graph = gr;}
  void Fill(int nEvents, StSvtHybridCollection* collEvent, StSvtHybridCollection* collData);
  void Fill(int nEvents, TArrayF* collEvent, TArrayF* collData);
  void Draw(char* option=0);

  void setTitle(Text_t* title);
  void setThisTitle(Text_t* title);
  void setXTitle(Text_t* title){mXTitle = title;}
  void setYTitle(Text_t* title){mYTitle = title;}

  void setMaximum(float max){mMaximum = max;}
  void setMinimum(float min){mMinimum = min;}

  void setMaximum(){mMaximum = mDefaultMaximum;}
  void setMinimum(){mMinimum = mDefaultMinimum;}

  void setDefaultMaximum(float max){mDefaultMaximum = max;}
  void setDefaultMinimum(float min){mDefaultMinimum = min;}

  TH1F* getHist();

protected:

  TGraph* graph;   //!

  TString mTitle;
  Text_t* mXTitle; //!
  Text_t* mYTitle; //!

  float mMinimum;
  float mMaximum;

  float mDefaultMinimum;
  float mDefaultMaximum;

  ClassDef(StSvtHybridGraph,1)
};

#endif
