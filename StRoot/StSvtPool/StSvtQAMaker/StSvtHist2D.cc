/***************************************************************************
 *
 * $Id: StSvtHist2D.cc,v 1.1 2004/02/06 02:30:34 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Histograms Collection class
 *
 ***************************************************************************
 *
 * $Log: StSvtHist2D.cc,v $
 * Revision 1.1  2004/02/06 02:30:34  munhoz
 * inserting SVT online monitor
 *
 **************************************************************************/

#include "StSvtHist2D.hh"
#include "StSvtHybridHist2D.hh"

#include "TH2.h"

ClassImp(StSvtHist2D)

StSvtHist2D::StSvtHist2D(char* config, Text_t* name, Text_t* title, Int_t nbinsx, Axis_t xlow, Axis_t xup, Int_t nbinsy, Axis_t ylow, Axis_t yup) : 
  StSvtHybridCollection(config)
{
  if (name)
    setHist(name, title, nbinsx, xlow, xup, nbinsy, ylow, yup); 
}

StSvtHist2D::~StSvtHist2D()
{}

void StSvtHist2D::setHist(Text_t* name, Text_t* title, Int_t nbinsx, Axis_t xlow, Axis_t xup, Int_t nbinsy, Axis_t ylow, Axis_t yup)
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

	  hist = new StSvtHybridHist2D(temp, temp2, nbinsx, xlow, xup, nbinsy, ylow, yup);
	  this->at(getHybridIndex(barrel,ladder,wafer,hybrid)) = hist;
	}
      }
    }
  }
}

void StSvtHist2D::setTitle(Text_t* title)
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

	  ((StSvtHybridHist2D*)at(getHybridIndex(barrel,ladder,wafer,hybrid)))->getHist()->SetTitle(temp);
	}
      }
    }
  }
}

void StSvtHist2D::setThisTitle(Text_t* title)
{
  //loop over all hybrids
  for (int barrel = 1;barrel <= getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= getNumberOfHybrids();hybrid++) {

	  if (getHybridIndex(barrel, ladder, wafer, hybrid) < 0) continue;

	  ((StSvtHybridHist2D*)at(getHybridIndex(barrel,ladder,wafer,hybrid)))->getHist()->SetTitle(title);
	}
      }
    }
  }
}

void StSvtHist2D::setXTitle(Text_t* title)
{
  //loop over all hybrids
  for (int barrel = 1;barrel <= getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= getNumberOfHybrids();hybrid++) {

	  if (getHybridIndex(barrel, ladder, wafer, hybrid) < 0) continue;

	  ((StSvtHybridHist2D*)at(getHybridIndex(barrel,ladder,wafer,hybrid)))->getHist()->SetXTitle(title);
	}
      }
    }
  }
}

void StSvtHist2D::setYTitle(Text_t* title)
{
  //loop over all hybrids
  for (int barrel = 1;barrel <= getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= getNumberOfHybrids();hybrid++) {

	  if (getHybridIndex(barrel, ladder, wafer, hybrid) < 0) continue;

	  ((StSvtHybridHist2D*)at(getHybridIndex(barrel,ladder,wafer,hybrid)))->getHist()->SetYTitle(title);
	}
      }
    }
  }
}

void StSvtHist2D::setMarkerSize(Float_t size)
{
  //loop over all hybrids
  for (int barrel = 1;barrel <= getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= getNumberOfHybrids();hybrid++) {

	  if (getHybridIndex(barrel, ladder, wafer, hybrid) < 0) continue;

	  ((StSvtHybridHist2D*)at(getHybridIndex(barrel,ladder,wafer,hybrid)))->getHist()->SetMarkerSize(size);
	}
      }
    }
  }
}

void StSvtHist2D::setMarkerStyle(Int_t style)
{
  //loop over all hybrids
  for (int barrel = 1;barrel <= getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= getNumberOfHybrids();hybrid++) {

	  if (getHybridIndex(barrel, ladder, wafer, hybrid) < 0) continue;

	  ((StSvtHybridHist2D*)at(getHybridIndex(barrel,ladder,wafer,hybrid)))->getHist()->SetMarkerStyle(style);
	}
      }
    }
  }
}

void StSvtHist2D::setLineWidth(Int_t width)
{
  //loop over all hybrids
  for (int barrel = 1;barrel <= getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= getNumberOfHybrids();hybrid++) {

	  if (getHybridIndex(barrel, ladder, wafer, hybrid) < 0) continue;

	  ((StSvtHybridHist2D*)at(getHybridIndex(barrel,ladder,wafer,hybrid)))->getHist()->SetLineWidth(width);
	}
      }
    }
  }
}

void StSvtHist2D::setBins(Int_t nbinsx, Axis_t xlow, Axis_t xup, Int_t nbinsy, Axis_t ylow, Axis_t yup)
{
  //loop over all hybrids
  for (int barrel = 1;barrel <= getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= getNumberOfHybrids();hybrid++) {

	  if (getHybridIndex(barrel, ladder, wafer, hybrid) < 0) continue;

	  ((StSvtHybridHist2D*)at(getHybridIndex(barrel,ladder,wafer,hybrid)))->getHist()->
	    SetBins(nbinsx, xlow, xup, nbinsy, ylow, yup);
	}
      }
    }
  }
}

void StSvtHist2D::setMaximum(Float_t max)
{
  //loop over all hybrids
  for (int barrel = 1;barrel <= getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= getNumberOfHybrids();hybrid++) {

	  if (getHybridIndex(barrel, ladder, wafer, hybrid) < 0) continue;

	  ((StSvtHybridHist2D*)at(getHybridIndex(barrel,ladder,wafer,hybrid)))->getHist()->SetMaximum(max);
	}
      }
    }
  }
}

void StSvtHist2D::setMinimum(Float_t min)
{
  //loop over all hybrids
  for (int barrel = 1;barrel <= getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= getNumberOfHybrids();hybrid++) {

	  if (getHybridIndex(barrel, ladder, wafer, hybrid) < 0) continue;

	  ((StSvtHybridHist2D*)at(getHybridIndex(barrel,ladder,wafer,hybrid)))->getHist()->SetMinimum(min);
	}
      }
    }
  }
}

void StSvtHist2D::setMaximum()
{
  //loop over all hybrids
  for (int barrel = 1;barrel <= getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= getNumberOfHybrids();hybrid++) {

	  if (getHybridIndex(barrel, ladder, wafer, hybrid) < 0) continue;

	  ((StSvtHybridHist2D*)at(getHybridIndex(barrel,ladder,wafer,hybrid)))->getHist()->SetMaximum();
	}
      }
    }
  }
}

void StSvtHist2D::setMinimum()
{
  //loop over all hybrids
  for (int barrel = 1;barrel <= getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= getNumberOfHybrids();hybrid++) {

	  if (getHybridIndex(barrel, ladder, wafer, hybrid) < 0) continue;

	  ((StSvtHybridHist2D*)at(getHybridIndex(barrel,ladder,wafer,hybrid)))->getHist()->SetMinimum();
	}
      }
    }
  }
}

Float_t StSvtHist2D::getMaximum(int barrelID, int ladderID)
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
	 
	  temp = ((StSvtHybridHist2D*)at(getHybridIndex(barrel,ladder,wafer,hybrid)))->getHist()->GetMaximum();
	  
	  if (temp > max)
	    max = temp;
	}
      }
    }
  }

  return max;
}

Float_t StSvtHist2D::getMinimum(int barrelID, int ladderID)
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
	 
	  temp = ((StSvtHybridHist2D*)at(getHybridIndex(barrel,ladder,wafer,hybrid)))->getHist()->GetMinimum();
	  
	  if (temp < min)
	    min = temp;
	}
      }
    }
  }

  return min;
}

void StSvtHist2D::reset()
{
  //loop over all hybrids
  for (int barrel = 1;barrel <= getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= getNumberOfHybrids();hybrid++) {

	  if (getHybridIndex(barrel, ladder, wafer, hybrid) < 0) continue;

	  ((StSvtHybridHist2D*)at(getHybridIndex(barrel,ladder,wafer,hybrid)))->getHist()->Reset();
	}
      }
    }
  }
}

