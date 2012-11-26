/*
 * Created by S. Gliske, May 2012
 *
 * Description: McParticle_t, used in the McEEmcTree.
 *
 * Note: this class does not explicitly depend on the STAR framework,
 * and so the trees can be read outside of the STAR framework.  Note:
 * data members are all public, to allow a lighter weight implementation.
 * 
 *
 */

#include <Rtypes.h>
#include <TVector3.h>

#include "McParticle.h"

// default constructor
McParticle_t::McParticle_t() : parentIdx(-1), startVertexIdx(-1), stopVertexIdx(-1), gId(0), pId(0), E(0),
                               sector(-1), uPos(0), vPos(0), uE(0), vE(0),
                               eTow(0), ePre1(0), ePre2(0), ePost(0) { /* */ };


// deconstructor
McParticle_t::~McParticle_t() { /* */ };

ClassImp( McParticle_t );

/*
 * $Id: McParticle.cxx,v 1.1 2012/11/26 19:04:30 sgliske Exp $
 * $Log: McParticle.cxx,v $
 * Revision 1.1  2012/11/26 19:04:30  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/EEmcTreeContainers to StRoot/StEEmcPool/EEmcTreeContainers
 *
 *
 */
