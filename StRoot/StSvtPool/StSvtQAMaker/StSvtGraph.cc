/***************************************************************************
 *
 * $Id: StSvtGraph.cc,v 1.1 2004/02/06 02:30:34 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Graph Collection class
 *
 ***************************************************************************
 *
 * $Log: StSvtGraph.cc,v $
 * Revision 1.1  2004/02/06 02:30:34  munhoz
 * inserting SVT online monitor
 *
 **************************************************************************/

#include "StSvtGraph.hh"
#include "StSvtHybridGraph.hh"

#include "TH1.h"
#include "TGraph.h"

ClassImp(StSvtGraph)

StSvtGraph::StSvtGraph(char* config) : 
  StSvtHybridCollection(config)
{
  setGraph();
}

StSvtGraph::~StSvtGraph()
{
  delete[] graph;
}

void StSvtGraph::setGraph()
{
  //loop over all hybrids
  for (int barrel = 1;barrel <= getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= getNumberOfHybrids();hybrid++) {

	  if (getHybridIndex(barrel, ladder, wafer, hybrid) < 0) continue;

	  graph = new StSvtHybridGraph(barrel,ladder,wafer,hybrid);
	  this->at(getHybridIndex(barrel,ladder,wafer,hybrid)) = graph;
	}
      }
    }
  }
}

void StSvtGraph::setTitle(Text_t* title)
{
  mTitle = title;

  int indexHybrid;
  //loop over all hybrids
  for (int barrel = 1;barrel <= getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= getNumberOfHybrids();hybrid++) {

	  indexHybrid = getHybridIndex(barrel, ladder, wafer, hybrid);
	  if (indexHybrid < 0) continue;
       
	  ((StSvtHybridGraph*)at(indexHybrid))->setTitle(title);
	}
      }
    }
  }
}

void StSvtGraph::setThisTitle(Text_t* title)
{
  mTitle = title;

  int indexHybrid;
  //loop over all hybrids
  for (int barrel = 1;barrel <= getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= getNumberOfHybrids();hybrid++) {

	  indexHybrid = getHybridIndex(barrel, ladder, wafer, hybrid);
	  if (indexHybrid < 0) continue;
       
	  ((StSvtHybridGraph*)at(indexHybrid))->setThisTitle(title);
	}
      }
    }
  }
}

void StSvtGraph::setXTitle(Text_t* title)
{
  int indexHybrid;
  //loop over all hybrids
  for (int barrel = 1;barrel <= getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= getNumberOfHybrids();hybrid++) {

	  indexHybrid = getHybridIndex(barrel, ladder, wafer, hybrid);
	  if (indexHybrid < 0) continue;
       
	  ((StSvtHybridGraph*)at(indexHybrid))->setXTitle(title);
	}
      }
    }
  }
}

void StSvtGraph::setYTitle(Text_t* title)
{
  int indexHybrid;
  //loop over all hybrids
  for (int barrel = 1;barrel <= getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= getNumberOfHybrids();hybrid++) {

	  indexHybrid = getHybridIndex(barrel, ladder, wafer, hybrid);
	  if (indexHybrid < 0) continue;
       
	  ((StSvtHybridGraph*)at(indexHybrid))->setYTitle(title);
	}
      }
    }
  }
}

void StSvtGraph::setMaximum(Float_t max)
{
  int indexHybrid;
  //loop over all hybrids
  for (int barrel = 1;barrel <= getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= getNumberOfHybrids();hybrid++) {

	  indexHybrid = getHybridIndex(barrel, ladder, wafer, hybrid);
	  if (indexHybrid < 0) continue;
       
	  ((StSvtHybridGraph*)at(indexHybrid))->setMaximum(max);
	}
      }
    }
  }
}

void StSvtGraph::setMinimum(Float_t min)
{
  int indexHybrid;
  //loop over all hybrids
  for (int barrel = 1;barrel <= getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= getNumberOfHybrids();hybrid++) {

	  indexHybrid = getHybridIndex(barrel, ladder, wafer, hybrid);
	  if (indexHybrid < 0) continue;

	  ((StSvtHybridGraph*)at(indexHybrid))->setMinimum(min);
	}
      }
    }
  }
}

void StSvtGraph::setMaximum()
{
  int indexHybrid;
  //loop over all hybrids
  for (int barrel = 1;barrel <= getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= getNumberOfHybrids();hybrid++) {

	  indexHybrid = getHybridIndex(barrel, ladder, wafer, hybrid);
	  if (indexHybrid < 0) continue;

	  ((StSvtHybridGraph*)at(indexHybrid))->setMaximum();
	}
      }
    }
  }
}

void StSvtGraph::setMinimum()
{
  int indexHybrid;
  //loop over all hybrids
  for (int barrel = 1;barrel <= getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= getNumberOfHybrids();hybrid++) {

	  indexHybrid = getHybridIndex(barrel, ladder, wafer, hybrid);
	  if (indexHybrid < 0) continue;

	  ((StSvtHybridGraph*)at(indexHybrid))->setMinimum();
	}
      }
    }
  }
}

void StSvtGraph::setDefaultMaximum(Float_t max)
{
  int indexHybrid;
  //loop over all hybrids
  for (int barrel = 1;barrel <= getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= getNumberOfHybrids();hybrid++) {

	  indexHybrid = getHybridIndex(barrel, ladder, wafer, hybrid);
	  if (indexHybrid < 0) continue;
       
	  ((StSvtHybridGraph*)at(indexHybrid))->setDefaultMaximum(max);
	}
      }
    }
  }
}

void StSvtGraph::setDefaultMinimum(Float_t min)
{
  int indexHybrid;
  //loop over all hybrids
  for (int barrel = 1;barrel <= getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= getNumberOfHybrids();hybrid++) {

	  indexHybrid = getHybridIndex(barrel, ladder, wafer, hybrid);
	  if (indexHybrid < 0) continue;

	  ((StSvtHybridGraph*)at(indexHybrid))->setDefaultMinimum(min);
	}
      }
    }
  }
}

Float_t StSvtGraph::getMaximum(int barrelID, int ladderID)
{
  float max=0, temp;
  int indexHybrid;
  TGraph* graph;

  int firstBarrel, lastBarrel, firstLadder, lastLadder;

  if (barrelID) {
    firstBarrel = barrelID;
    lastBarrel = barrelID;
  }
  else {
    firstBarrel = 1;
    lastBarrel = getNumberOfBarrels();
  }

  //loop over all hybrids
  for (int barrel = firstBarrel;barrel <= lastBarrel;barrel++) {

    if (ladderID) {
      firstLadder = ladderID;
      lastLadder = ladderID;
    }
    else {
      firstLadder = 1;
      lastLadder = getNumberOfLadders(barrel);
    }

    for (int ladder = firstLadder;ladder <= lastLadder;ladder++) {
      for (int wafer = 1;wafer <= getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= getNumberOfHybrids();hybrid++) {

	  indexHybrid = getHybridIndex(barrel, ladder, wafer, hybrid);
	  if (indexHybrid < 0) continue;

	  graph = ((StSvtHybridGraph*)at(indexHybrid))->getGraph();
	  if (graph->GetHistogram()) {
	    temp = graph->GetHistogram()->GetMaximum();
	  
	    if (temp > max)
	      max = temp;
	  }
	}
      }
    }
  }

  return max;
}

Float_t StSvtGraph::getMinimum(int barrelID, int ladderID)
{
  float min=999999, temp;
  int indexHybrid;
  TGraph* graph;

  int firstBarrel, lastBarrel, firstLadder, lastLadder;

  if (barrelID) {
    firstBarrel = barrelID;
    lastBarrel = barrelID;
  }
  else {
    firstBarrel = 1;
    lastBarrel = getNumberOfBarrels();
  }

  //loop over all hybrids
  for (int barrel = firstBarrel;barrel <= lastBarrel;barrel++) {

    if (ladderID) {
      firstLadder = ladderID;
      lastLadder = ladderID;
    }
    else {
      firstLadder = 1;
      lastLadder = getNumberOfLadders(barrel);
    }

    for (int ladder = firstLadder;ladder <= lastLadder;ladder++) {
      for (int wafer = 1;wafer <= getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= getNumberOfHybrids();hybrid++) {

	  indexHybrid = getHybridIndex(barrel, ladder, wafer, hybrid);
	  if (indexHybrid < 0) continue;

	  graph = ((StSvtHybridGraph*)at(indexHybrid))->getGraph();
	  if (graph->GetHistogram()) {
	    temp = graph->GetHistogram()->GetMinimum();
	  
	    if (temp < min)
	      min = temp;
	  }
	}
      }
    }
  }

  return min;
}

void StSvtGraph::reset()
{
  int indexHybrid;
  TGraph* graph;
  //loop over all hybrids
  for (int barrel = 1;barrel <= getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= getNumberOfHybrids();hybrid++) {

	  indexHybrid = getHybridIndex(barrel, ladder, wafer, hybrid);
	  if (indexHybrid < 0) continue;

	  graph = ((StSvtHybridGraph*)at(indexHybrid))->getGraph();
	  if (graph->GetHistogram())
	    graph->GetHistogram()->Reset();
	}
      }
    }
  }
}

