/***************************************************************************
 * $Id: StGenericVertexFinder.cxx,v 1.1 2003/05/09 22:22:46 lbarnby Exp $
 *
 * Author: Lee Barnby, April 2003
 *
 ***************************************************************************
 * Description: Base class for vertex finders
 *
 ***************************************************************************/
#include "StGenericVertexFinder.h"
#include "StMessMgr.h"

void StGenericVertexFinder::FillStEvent(StEvent* event) const{
  //Adds the vertex to StEvent (currently as a primary)
  // Here we invent our own flag and other data to put in
  // In real life we have to get it from somewhere (as done for position)
  UInt_t minuitFlag=1000; //LSB example only, value to be agreed on
  Float_t ex,ey,ez; // Position errors 
  ex = this->error().x();ey = this->error().y();ez = this->error().z();
  Float_t cov[6] = {ex*ex,0.0,ey*ey,0.0,0.0,ez*ez};

  Float_t xSq = 5.43;
  Float_t probXSq = 0.2468;

  StPrimaryVertex* primV = new StPrimaryVertex();
  primV->setPosition(this->result());    //requires StThreeVectorF
  primV->setFlag(minuitFlag+this->status());       //requires unsigned int
  primV->setCovariantMatrix(cov);      //requires float[6]
  primV->setChiSquared(xSq);           //requires float
  primV->setProbChiSquared(probXSq);       //requires float
  //primV->setParent();  //requires StTrack* but we won't use this, also
  //addDaughter(StTrack*) and removeDaughter(StTrack*) not used here
  //addDaughter would be used when filling primary tracks in later maker

  event->addPrimaryVertex(primV);
  gMessMgr->Debug()
    << "StGenericVertexFinder::FillStEvent: Added new primary vertex" << endm;
}

// $Log: StGenericVertexFinder.cxx,v $
// Revision 1.1  2003/05/09 22:22:46  lbarnby
// Initial revision: a base class for STAR (StEvent-based) vertex finders
//
