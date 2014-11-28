/**********************************************
 *
 * $Id: StMcHitComparisons.cc,v 2.4 2005/11/22 21:44:52 fisyak Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez
 ***********************************************
 * Description: Define the comparisons to be used
 *              in the multimaps
 *              & sorting of containers
 *
 ***********************************************
 * $Log: StMcHitComparisons.cc,v $
 * Revision 2.4  2005/11/22 21:44:52  fisyak
 * Add compress Print for McEvent, add Ssd collections
 *
 * Revision 2.3  2000/06/09 19:52:07  calderon
 * No longer use 2 different functions for SVT and TPC that do the same thing,
 * just use one function for the base class
 *
 * Revision 2.2  2000/06/06 02:58:41  calderon
 * Introduction of Calorimeter classes.  Modified several classes
 * accordingly.
 *
 * Revision 2.1  2000/03/07 15:09:54  calderon
 * Initial Revision.
 * Comparisons used for sorting the hit containers, and
 * for ordering the hits in the multimaps.
 * 
 * 
 **********************************************/
#include "StTpcHit.h"
#include "StSvtHit.h"
#include "StSsdHit.h"
#include "StFtpcHit.h"
#include "StRichHit.h"

#include "StMcTpcHit.hh"
#include "StMcSvtHit.hh"
#include "StMcSsdHit.hh"
#include "StMcFtpcHit.hh"
#include "StMcRichHit.hh"
#include "StMcHitComparisons.hh"
bool compHit::operator()(const StHit* h1, const StHit* h2) const {
    if        (h1->position().z() != h2->position().z()) {
	return h1->position().z() <  h2->position().z();
    }
    else if   (h1->position().y() != h2->position().y()) {
	return h1->position().y() <  h2->position().y();
    }
    else return h1->position().x() < h2->position().x();    
}
bool compMcHit::operator()(const StMcHit* h1, const StMcHit* h2) const {
    if        (h1->position().z() != h2->position().z()) {
	return h1->position().z() <  h2->position().z();
    }
    else if   (h1->position().y() != h2->position().y()) {
	return h1->position().y() <  h2->position().y();
    }
    else return h1->position().x() < h2->position().x();    
}
bool compRPhi::operator()(const StThreeVectorF& p1, const StThreeVectorF& p2) const{
    // for Ftpc comparison is btw hits in the same plane, so
    // z coordinate is irrelevant.  Use r and phi
    float phi1, phi2;
    if ((phi1 = p1.phi()) != (phi2 = p2.phi())) 
	return  phi1 < phi2;
    else return p1.perp() < p2.perp();
}
bool compFtpcHit::operator()(const StFtpcHit* h1, const StFtpcHit* h2) const {
    compRPhi crp;
    return crp(h1->position(), h2->position());    
}
bool compMcFtpcHit::operator()(const StMcFtpcHit* h1, const StMcFtpcHit* h2) const {
    compRPhi crp;
    return crp(h1->position(), h2->position());    
}
