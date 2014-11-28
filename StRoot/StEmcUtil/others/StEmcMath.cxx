#include <cmath>
#include <TArrayD.h>
#include "StEmcMath.h"
#include "StEvent/StMeasuredPoint.h"
#include "StarClassLibrary/StThreeVectorF.hh"
#include "StEmcUtil/others/emcInternalDef.h"
#include "StEmcUtil/geometry/StEmcGeom.h"

ClassImp(StEmcMath)

Bool_t 
StEmcMath::etaPhi(StMeasuredPoint* point,StMeasuredPoint* vertex, Double_t &eta, Double_t &phi)
{ 
//
// point must be defined; vertex is zero on default 
//
	const StThreeVectorF *p1, *p2;
	p1 = NULL;
	StThreeVectorF vt;

	if(point)  {
		if(vertex) p1 = &vertex->position();
		p2 = &point->position();

		if(p1) vt = (*p2) - (*p1); // shift
		else   vt = (*p2);

		eta = vt.pseudoRapidity();
		phi = vt.phi();
		return kTRUE;
	}
	return kFALSE; // point indefined
}

Double_t 
StEmcMath::pseudoRapidity(StMeasuredPoint* point,StMeasuredPoint* vertex)
{ 
  Double_t eta, phi;
  if(etaPhi(point,vertex, eta,phi)) return eta;
  else return -9999.;
}

Double_t 
StEmcMath::phi(StMeasuredPoint* point,StMeasuredPoint* vertex)
{ 
  Double_t eta, phi;
  if(etaPhi(point,vertex, eta,phi)) return phi;
  else return -9999.;
}

UInt_t 
StEmcMath::detectorId(const StDetectorId stId)
{
  // Transition from STAR numeration to internal EMC numeration
  Int_t id = stId - kBarrelEmcTowerIdentifier + 1;
  if(id<BEMC || id> ESMDP) return 0; // Wrong value of stId
  else                     return UInt_t(id);
}

StDetectorId
StEmcMath::detectorId(const UInt_t id)
{
  // Transition from internal EMC numeration numeration to STAR
  StDetectorId stId = StDetectorId(id + kBarrelEmcTowerIdentifier - 1);
  if(stId<kBarrelEmcTowerIdentifier || stId >kEndcapSmdVStripIdentifier) 
  return kUnknownId;
  else return stId; 
}

Double_t 
StEmcMath::getPhiPlusMinusPi(const Double_t phi)
{
  // convert phi to the range from -pi<= phi< pi (as in StEmcGeom) 
  Double_t phiw = phi;
  while(phiw >=  M_PI) phiw -= 2.*M_PI;
  while(phiw <  -M_PI) phiw += 2.*M_PI;
  return phiw;
}  

TArrayD  *StEmcMath::binForSmde(Bool_t kprint)
{
// Calculate correct eta binnig for smd eta
	StEmcGeom* geom = StEmcGeom::getEmcGeom(3);   // For SMDE
	const Int_t neta = geom->NEta();
	Int_t iw1, iw2;
	const Float_t *eb = geom->Eta();
	TArrayD *xb = new TArrayD(2*neta+1); 
	(*xb)[neta]   = 0.0;
	for(Int_t ik=0; ik<neta; ik++){
		iw1 = neta + 1 + ik;
		iw2 = neta-ik-1;
		Float_t x1 = eb[ik], x2, xw;
		if(ik<neta-1) {
			x2 = eb[ik+1];
			xw = (x1+x2)*0.5;
		}
		else xw = 0.99;
		(*xb)[iw1] = +xw;
		(*xb)[iw2] = -xw;
		if(kprint)
			printf(" iw1 %i %f => iw2 %i %f => eta %f\n", iw1,(*xb)[iw1], iw2,(*xb)[iw2], eb[ik]);
	}
	return xb;
}
