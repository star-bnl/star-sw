/***************************************************************************
 *
 * $Id: StSvtHistAnalog.cc,v 1.1 2004/02/06 02:30:34 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Histograms Collection class
 *
 ***************************************************************************
 *
 * $Log: StSvtHistAnalog.cc,v $
 * Revision 1.1  2004/02/06 02:30:34  munhoz
 * inserting SVT online monitor
 *
 **************************************************************************/

#include "StSvtHistAnalog.hh"
#include "StSvtHybridHistAnalog.hh"

#include "TH1.h"

ClassImp(StSvtHistAnalog)

StSvtHistAnalog::StSvtHistAnalog(char* config, Text_t* name, Text_t* title, Int_t nbinsx, Axis_t xlow, Axis_t xup) : 
  StSvtHybridCollection(config)
{
  if (name)
    setHist(name, title, nbinsx, xlow, xup);
}

StSvtHistAnalog::~StSvtHistAnalog()
{
  delete[] hist;
}

void StSvtHistAnalog::setHist(Text_t* name, Text_t* title, Int_t nbinsx, Axis_t xlow, Axis_t xup)
{
  char append[100], append2[100], temp[100], temp2[200];

  mTitle = title;

  //loop over all hybrids
  for (int barrel = 1;barrel <= getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= getNumberOfHybrids();hybrid++) {

	  if (getHybridIndex(barrel, ladder, wafer, hybrid) < 0) continue;

	  strcpy(temp,name);
	  sprintf(append,"_%d_%d_%d_%d",barrel,ladder,wafer,hybrid);
	  strcat(temp,append);
      
	  strcpy(temp2,title);      
	  sprintf(append2," - Barrel #%d, Ladder #%d, Wafer #%d, Hybrid #%d",barrel,ladder,wafer,hybrid);
	  strcat(temp2,append2);

	  hist = new StSvtHybridHistAnalog(temp, temp2, nbinsx, xlow, xup);
	  this->at(getHybridIndex(barrel,ladder,wafer,hybrid)) = hist;
	}
      }
    }
  }
}

void StSvtHistAnalog::setTitle(Text_t* title)
{
  char append[100], temp[100];

  mTitle = title;

  //loop over all hybrids
  for (int barrel = 1;barrel <= getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= getNumberOfHybrids();hybrid++) {

	  if (getHybridIndex(barrel, ladder, wafer, hybrid) < 0) continue;

	  strcpy(temp,title);      
	  sprintf(append," - Barrel #%d, Ladder #%d, Wafer #%d, Hybrid #%d",barrel,ladder,wafer,hybrid);
	  strcat(temp,append);

	  ((StSvtHybridHistAnalog*)at(getHybridIndex(barrel,ladder,wafer,hybrid)))->getHist(1)->SetTitle(temp);
	}
      }
    }
  }
}

void StSvtHistAnalog::setThisTitle(Text_t* title)
{
  //loop over all hybrids
  for (int barrel = 1;barrel <= getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= getNumberOfHybrids();hybrid++) {

	  if (getHybridIndex(barrel, ladder, wafer, hybrid) < 0) continue;

	  ((StSvtHybridHistAnalog*)at(getHybridIndex(barrel,ladder,wafer,hybrid)))->getHist(1)->SetTitle(title);
	}
      }
    }
  }
}

void StSvtHistAnalog::setXTitle(Text_t* title)
{
  //loop over all hybrids
  for (int barrel = 1;barrel <= getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= getNumberOfHybrids();hybrid++) {

	  if (getHybridIndex(barrel, ladder, wafer, hybrid) < 0) continue;

	  ((StSvtHybridHistAnalog*)at(getHybridIndex(barrel,ladder,wafer,hybrid)))->getHist(1)->SetXTitle(title);
	  ((StSvtHybridHistAnalog*)at(getHybridIndex(barrel,ladder,wafer,hybrid)))->getHist(2)->SetXTitle(title);
	  ((StSvtHybridHistAnalog*)at(getHybridIndex(barrel,ladder,wafer,hybrid)))->getHist(3)->SetXTitle(title);
	}
      }
    }
  }
}

Float_t StSvtHistAnalog::getMaximum(int barrelID, int ladderID)
{
  float max=0, temp;

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

	  if (getHybridIndex(barrel, ladder, wafer, hybrid) < 0) continue;
	 
	  temp = ((StSvtHybridHistAnalog*)at(getHybridIndex(barrel,ladder,wafer,hybrid)))->getHist(1)->GetMaximum();
	  
	  if (temp > max)
	    max = temp;
	}
      }
    }
  }

  return max;
}

Float_t StSvtHistAnalog::getMinimum(int barrelID, int ladderID)
{
  float min=999999, temp;

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

	  if (getHybridIndex(barrel, ladder, wafer, hybrid) < 0) continue;
	 
	  temp = ((StSvtHybridHistAnalog*)at(getHybridIndex(barrel,ladder,wafer,hybrid)))->getHist(1)->GetMinimum();
	  
	  if (temp < min)
	    min = temp;
	}
      }
    }
  }

  return min;
}

void StSvtHistAnalog::reset()
{
  //loop over all hybrids
  for (int barrel = 1;barrel <= getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= getNumberOfHybrids();hybrid++) {

	  if (getHybridIndex(barrel, ladder, wafer, hybrid) < 0) continue;

	  ((StSvtHybridHistAnalog*)at(getHybridIndex(barrel,ladder,wafer,hybrid)))->getHist(1)->Reset();
	  ((StSvtHybridHistAnalog*)at(getHybridIndex(barrel,ladder,wafer,hybrid)))->getHist(2)->Reset();
	  ((StSvtHybridHistAnalog*)at(getHybridIndex(barrel,ladder,wafer,hybrid)))->getHist(3)->Reset();
	}
      }
    }
  }
}

