/***************************************************************************
 *
 * $Id: StSvtHist.cc,v 1.1 2004/02/06 02:30:34 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Histograms Collection class
 *
 ***************************************************************************
 *
 * $Log: StSvtHist.cc,v $
 * Revision 1.1  2004/02/06 02:30:34  munhoz
 * inserting SVT online monitor
 *
 **************************************************************************/

#include "StSvtHist.hh"
#include "StSvtHybridHist.hh"

#include "TH1.h"

ClassImp(StSvtHist)

StSvtHist::StSvtHist(char* config, Text_t* name, Text_t* title, Int_t nbinsx, Float_t xlow, Float_t xup) : 
  StSvtHybridCollection(config)
{
  if (name)
    setHist(name, title, nbinsx, xlow, xup);
}

StSvtHist::~StSvtHist()
{
  delete[] hist;
}

void StSvtHist::setHist(Text_t* name, Text_t* title, Int_t nbinsx, Axis_t xlow, Axis_t xup)
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

	  hist = new StSvtHybridHist(temp, temp2, nbinsx, xlow, xup);
	  this->at(getHybridIndex(barrel,ladder,wafer,hybrid))=hist;
	}
      }
    }
  }
}

void StSvtHist::setTitle(Text_t* title)
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

	  ((StSvtHybridHist*)at(getHybridIndex(barrel,ladder,wafer,hybrid)))->getHist()->SetTitle(temp);
	}
      }
    }
  }
}

void StSvtHist::setThisTitle(Text_t* title)
{
  //loop over all hybrids
  for (int barrel = 1;barrel <= getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= getNumberOfHybrids();hybrid++) {

	  if (getHybridIndex(barrel, ladder, wafer, hybrid) < 0) continue;

	  ((StSvtHybridHist*)at(getHybridIndex(barrel,ladder,wafer,hybrid)))->getHist()->SetTitle(title);
	}
      }
    }
  }
}

void StSvtHist::setXTitle(Text_t* title)
{
  //loop over all hybrids
  for (int barrel = 1;barrel <= getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= getNumberOfHybrids();hybrid++) {

	  if (getHybridIndex(barrel, ladder, wafer, hybrid) < 0) continue;

	  ((StSvtHybridHist*)at(getHybridIndex(barrel,ladder,wafer,hybrid)))->getHist()->SetXTitle(title);
	}
      }
    }
  }
}

void StSvtHist::setYTitle(Text_t* title)
{
  //loop over all hybrids
  for (int barrel = 1;barrel <= getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= getNumberOfHybrids();hybrid++) {

	  if (getHybridIndex(barrel, ladder, wafer, hybrid) < 0) continue;

	  ((StSvtHybridHist*)at(getHybridIndex(barrel,ladder,wafer,hybrid)))->getHist()->SetYTitle(title);
	}
      }
    }
  }
}

void StSvtHist::setBins(Int_t nbinsx, Axis_t xlow, Axis_t xup)
{
  //loop over all hybrids
  for (int barrel = 1;barrel <= getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= getNumberOfHybrids();hybrid++) {

	  if (getHybridIndex(barrel, ladder, wafer, hybrid) < 0) continue;

	  ((StSvtHybridHist*)at(getHybridIndex(barrel,ladder,wafer,hybrid)))->getHist()->SetBins(nbinsx, xlow, xup);
	}
      }
    }
  }
}

void StSvtHist::setMaximum(Float_t max)
{
  //loop over all hybrids
  for (int barrel = 1;barrel <= getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= getNumberOfHybrids();hybrid++) {

	  if (getHybridIndex(barrel, ladder, wafer, hybrid) < 0) continue;

	  ((StSvtHybridHist*)at(getHybridIndex(barrel,ladder,wafer,hybrid)))->getHist()->SetMaximum(max);
	}
      }
    }
  }
}

void StSvtHist::setMinimum(Float_t min)
{
  //loop over all hybrids
  for (int barrel = 1;barrel <= getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= getNumberOfHybrids();hybrid++) {

	  if (getHybridIndex(barrel, ladder, wafer, hybrid) < 0) continue;

	  ((StSvtHybridHist*)at(getHybridIndex(barrel,ladder,wafer,hybrid)))->getHist()->SetMinimum(min);
	}
      }
    }
  }
}

void StSvtHist::setMaximum()
{
  //loop over all hybrids
  for (int barrel = 1;barrel <= getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= getNumberOfHybrids();hybrid++) {

	  if (getHybridIndex(barrel, ladder, wafer, hybrid) < 0) continue;

	  ((StSvtHybridHist*)at(getHybridIndex(barrel,ladder,wafer,hybrid)))->getHist()->SetMaximum();
	}
      }
    }
  }
}

void StSvtHist::setMinimum()
{
  //loop over all hybrids
  for (int barrel = 1;barrel <= getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= getNumberOfHybrids();hybrid++) {

	  if (getHybridIndex(barrel, ladder, wafer, hybrid) < 0) continue;

	  ((StSvtHybridHist*)at(getHybridIndex(barrel,ladder,wafer,hybrid)))->getHist()->SetMinimum();
	}
      }
    }
  }
}

Float_t StSvtHist::getMaximum(int barrelID, int ladderID)
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
	 
	  temp = ((StSvtHybridHist*)at(getHybridIndex(barrel,ladder,wafer,hybrid)))->getHist()->GetMaximum();

	  if (temp > max)
	    max = temp;
	}
      }
    }
  }

  return max;
}

Float_t StSvtHist::getMinimum(int barrelID, int ladderID)
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
	 
	  temp = ((StSvtHybridHist*)at(getHybridIndex(barrel,ladder,wafer,hybrid)))->getHist()->GetMinimum();
	  
	  if (temp < min)
	    min = temp;
	}
      }
    }
  }

  return min;
}

void StSvtHist::reset()
{
  //loop over all hybrids
  for (int barrel = 1;barrel <= getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= getNumberOfHybrids();hybrid++) {

	  if (getHybridIndex(barrel, ladder, wafer, hybrid) < 0) continue;

	  ((StSvtHybridHist*)at(getHybridIndex(barrel,ladder,wafer,hybrid)))->getHist()->Reset();
	  //((StSvtHybridHist*)at(getHybridIndex(barrel,ladder,wafer,hybrid)))->getHist()->SetMaximum();
	  //((StSvtHybridHist*)at(getHybridIndex(barrel,ladder,wafer,hybrid)))->getHist()->SetMinimum();
	}
      }
    }
  }
}

