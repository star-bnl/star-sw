//////////////////////////////////////////////////////////////////////
//
// $Id: StJet.cxx,v 1.1 2002/02/11 20:30:48 akio Exp $
// $Log: StJet.cxx,v $
// Revision 1.1  2002/02/11 20:30:48  akio
// Many updates, including very first version of jet finder.
//
//
// Revision 1.0  2001/06/14 Akio Ogawa
//
//////////////////////////////////////////////////////////////////////
#include <iostream.h>
#include <stdio.h>
#include "StEventTypes.h"
#include "StEvent.h"
#include "StJet.h"
#include "StEtCell.h"

ClassImp(StJet);

StJet::StJet(){
  et = 0.0;
  ex = 0.0;
  ey = 0.0;
  eta =0.0;
  phi =0.0;
  nCell =0;
}

StJet::StJet(StJet* j){
  et = j->et;
  ex = j->ex;
  ey = j->ey;
  eta = j->eta;
  phi = j->phi;
  nCell = j->nCell;
}

StJet::~StJet() {
} 

void StJet::print(){
  printf("Jet: Et= %6.3f   Eta= %6.3f   Phi= %6.3f    nCell= %4d\n",et,eta,phi,nCell);
}

void StJet::add(StEtCell* cell){
  float oldet = et;
  float etCell = cell->et;
  float etaCell = cell->eta();
  float phiCell = cell->phi();
  et  = etCell;
  ex  = (ex*oldet + cos(phiCell)*etCell) / et;
  ey  = (ey*oldet + sin(phiCell)*etCell) / et;
  eta = (eta*oldet + etaCell*etCell) /et;
  phi = (float)atan2((double)ey,(double)ex);
  nCell++;
  cell->flipSign();
}
