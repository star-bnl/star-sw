/**********************************************
 *
 * $Id: StMcHitComparisons.cc,v 2.1 2000/03/07 15:09:54 calderon Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez
 ***********************************************
 * Description: Define the comparisons to be used
 *              in the multimaps
 *              & sorting of containers
 *
 ***********************************************
 * $Log: StMcHitComparisons.cc,v $
 * Revision 2.1  2000/03/07 15:09:54  calderon
 * Initial Revision.
 * Comparisons used for sorting the hit containers, and
 * for ordering the hits in the multimaps.
 * 
 * 
 **********************************************/
#include "StMcHitComparisons.hh"
#include "StEventTypes.h"

#include "StMcTpcHit.hh"
#include "StMcSvtHit.hh"
#include "StMcFtpcHit.hh"
#include "StMcRichHit.hh"

bool compTpcHit::operator()(const StTpcHit* h1, const StTpcHit* h2) const {
    if        (h1->position().z() != h2->position().z()) {
	return h1->position().z() <  h2->position().z();
    }
    else if   (h1->position().y() != h2->position().y()) {
	return h1->position().y() <  h2->position().y();
    }
    else return h1->position().x() < h2->position().x();
    
}
bool compMcTpcHit::operator()(const StMcTpcHit* h1, const StMcTpcHit* h2) const {
    if        (h1->position().z() != h2->position().z()) {
	return h1->position().z() <  h2->position().z();
    }
    else if   (h1->position().y() != h2->position().y()) {
	return h1->position().y() <  h2->position().y();
    }
    else return h1->position().x() < h2->position().x();
    
}
bool compSvtHit::operator()(const StSvtHit* h1, const StSvtHit* h2) const {
    if        (h1->position().z() != h2->position().z()) {
	return h1->position().z() <  h2->position().z();
    }
    else if   (h1->position().y() != h2->position().y()) {
	return h1->position().y() <  h2->position().y();
    }
    else return h1->position().x() < h2->position().x();
    
}
bool compMcSvtHit::operator()(const StMcSvtHit* h1, const StMcSvtHit* h2) const {
    if        (h1->position().z() != h2->position().z()) {
	return h1->position().z() <  h2->position().z();
    }
    else if   (h1->position().y() != h2->position().y()) {
	return h1->position().y() <  h2->position().y();
    }
    else return h1->position().x() < h2->position().x();
    
}
bool compFtpcHit::operator()(const StFtpcHit* h1, const StFtpcHit* h2) const {
    // comparison is btw hits in the same plane, so
    // z coordinate is irrelevant.  Use r and phi
    float phi1, phi2;
    if ((phi1 = h1->position().phi()) != (phi2 = h2->position().phi())) 
	return  phi1 < phi2;
    else return h1->position().perp() < h2->position().perp();
    
}
bool compMcFtpcHit::operator()(const StMcFtpcHit* h1, const StMcFtpcHit* h2) const {
    // comparison is btw hits in the same plane, so
    // z coordinate is irrelevant
    float phi1, phi2;
    if ((phi1 = h1->position().phi()) != (phi2 = h2->position().phi())) 
	return  phi1 < phi2;
    else return h1->position().perp() < h2->position().perp();
    
}
