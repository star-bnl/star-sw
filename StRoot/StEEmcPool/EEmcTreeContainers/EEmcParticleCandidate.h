/*
 * Created by S. Gliske, May 2012
 *
 * Description: Container used in the EEmcAnalysisTree.  Represents a candidate particle.
 *
 * Note: this does not explicitly depend on the STAR framework, and so
 * the trees can be read outside of the STAR framework.  Note: data
 * members are all public, to allow a lighter weight implementation. from
 * the TTree.
 *
 * While other makers may redefine the interpretation of PID, this
 * class defaults to a value of PID = 1000.
 * 
 */

#ifndef EEmcParticleCandidate_H__
#define EEmcParticleCandidate_H__

#include <Rtypes.h>
#include <TObject.h>
#include <TVector3.h>

class EEmcHit_t;

class EEmcParticleCandidate_t : public TObject {

 public:
   // constructor
   EEmcParticleCandidate_t();
   EEmcParticleCandidate_t( Int_t hitIdx_, const EEmcHit_t& hit, TVector3& vertex );

   // default equals operator and copy constructor OK

   Int_t PID, hitIdx1;
   Float_t E, M, PT;
   TVector3 position;  // where it hits the endcap
   TVector3 momentum;  // in the direction from primary vertex to the position

 private:
   ClassDef( EEmcParticleCandidate_t, 1);
};

inline EEmcParticleCandidate_t::EEmcParticleCandidate_t() : PID(-9999), hitIdx1(-1), E(0), M(0), PT(0) { /* */ };

#endif

/*
 * $Id: EEmcParticleCandidate.h,v 1.1 2012/11/26 19:04:30 sgliske Exp $
 * $Log: EEmcParticleCandidate.h,v $
 * Revision 1.1  2012/11/26 19:04:30  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/EEmcTreeContainers to StRoot/StEEmcPool/EEmcTreeContainers
 *
 *
 */
