/***************************************************************************
 *
 * $Id: StStrangeTagsMaker.cxx,v 1.20 2007/04/28 17:57:02 perev Exp $
 *
 * Author: Gene Van Buren, Feb 1999
 ***************************************************************************
 *
 * Description:  Maker to fill the Strangeness Tags
 *
 ***************************************************************************
 *
 * $Log: StStrangeTagsMaker.cxx,v $
 * Revision 1.20  2007/04/28 17:57:02  perev
 * Redundant StChain.h removed
 *
 * Revision 1.19  2004/07/27 12:02:00  lbarnby
 * Filling of NKink and MaxPtKink tags added.
 *
 * Revision 1.18  2004/07/26 16:02:32  lbarnby
 * Added Xibar, Omega(bar). Introduce max pt tags.
 *
 * Revision 1.17  2003/10/07 03:10:27  perev
 * Leak of struct on mTagTable fixed
 *
 * Revision 1.16  2003/09/02 17:59:05  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.15  2003/01/22 23:37:18  genevb
 * Change function for abs()
 *
 * Revision 1.14  2000/01/27 19:29:50  fisyak
 * Put StrangeTag to .data
 *
 * Revision 1.13  1999/12/07 23:23:56  genevb
 * Fixed linux warnings
 *
 * Revision 1.12  1999/11/16 19:20:44  genevb
 * Modified for StEvent 2.0
 *
 * Revision 1.11  1999/09/24 01:23:26  fisyak
 * Reduced Include Path
 *
 * Revision 1.10  1999/07/15 13:57:28  perev
 * cleanup
 *
 * Revision 1.9  1999/06/27 22:45:31  fisyak
 * Merge StRootEvent and StEvent
 *
 * Revision 1.8  1999/04/14 22:05:11  genevb
 * Comply with momentumOfV0 call
 *
 * Revision 1.7  1999/03/20 22:07:43  perev
 * new maker schema
 *
 * Revision 1.6  1999/03/15 01:18:36  genevb
 * Fixed Xi vertex typo
 *
 * Revision 1.5  1999/02/24 15:38:49  genevb
 * Fixed a typo
 *
 * Revision 1.4  1999/02/24 15:34:00  genevb
 * Fixed a typo
 *
 * Revision 1.3  1999/02/24 02:03:38  genevb
 * Add Xi vertices
 *
 * Revision 1.2  1999/02/22 16:44:16  genevb
 * Switched to PhysicalConstants.h
 *
 * Revision 1.1  1999/02/21 23:35:11  genevb
 * Strangeness Tags Maker
 *
 **************************************************************************/
#include "StStrangeTagsMaker.h"
#include "StEvent.h"
#include "StV0Vertex.h"
#include "StXiVertex.h"
#include "StEventTypes.h"
#include "StTrack.h"
#include "TMath.h"
#include "PhysicalConstants.h"
#include "phys_constants.h"

ClassImp(StStrangeTagsMaker)

StStrangeTagsMaker::StStrangeTagsMaker(const char *name, const char *title)
    : StMaker(name, title)
{
  mEvent    = 0;
  mTagTable = 0;       // init pointer to tag table
}

StStrangeTagsMaker::~StStrangeTagsMaker()
{
  mTagTable = 0;    // clean up
}

Int_t StStrangeTagsMaker::Init()
{
  mRange = 0.025;
  m2Range = 2*mRange;
  mMasspi2 = pion_plus_mass_c2 * pion_plus_mass_c2;
  mMasspr2 = proton_mass_c2 * proton_mass_c2;
  mMassla2 = lambda_mass_c2 * lambda_mass_c2;
  mMasska2 = M_KAON_MINUS * M_KAON_MINUS; // defined in phys_constants.h
  
  return StMaker::Init();
}

Int_t StStrangeTagsMaker::Make()
{
  mTagTable = new StrangeTag_st;
  mEvent = (StEvent *) GetInputDS("StEvent");
  if (!mEvent) return kStOK; // If no event, we're done
  
  fillTag();
  St_StrangeTag *StrangeTag = new St_StrangeTag("StrangeTag",1);
  AddData(StrangeTag);
  StrangeTag->AddAt(mTagTable,0);
  delete mTagTable; mTagTable=0;
  return kStOK;
}


StrangeTag_st* StStrangeTagsMaker::tag()
{
  return mTagTable;
}

void StStrangeTagsMaker::fillTag()
{
  StSPtrVecV0Vertex& v0Vertices = mEvent->v0Vertices();
  size_t v0tot = v0Vertices.size();
  StSPtrVecXiVertex& xiVertices = mEvent->xiVertices();
  size_t castot = xiVertices.size();

  Int_t nbelowK0 = 0;
  Int_t nK0 = 0;
  Int_t naboveK0 = 0;
  Int_t nbelowLa = 0;
  Int_t nLa = 0;
  Int_t naboveLa = 0;
  Int_t nbelowLb = 0;
  Int_t nLb = 0;
  Int_t naboveLb = 0;
  Int_t nbelowXi = 0;
  Int_t nXi = 0;
  Int_t naboveXi = 0;
  Int_t nbelowXibar = 0;
  Int_t nXibar = 0;
  Int_t naboveXibar = 0;
  Int_t nbelowOm = 0;
  Int_t nOm = 0;
  Int_t naboveOm = 0;
  Int_t nbelowOmbar = 0;
  Int_t nOmbar = 0;
  Int_t naboveOmbar = 0;
  Int_t nKinkPos = 0;
  Int_t nKinkNeg = 0;

  Float_t perc;
  
  size_t i;

  //max Pt temporary variables
  Float_t maxPtK0=0.0;
  Float_t maxPtLa=0.0;
  Float_t maxPtLb=0.0;
  Float_t maxPtXi=0.0;
  Float_t maxPtXibar=0.0;
  Float_t maxPtOm=0.0;
  Float_t maxPtOmbar=0.0;
  Float_t maxPtKinkPos=0.0;
  Float_t maxPtKinkNeg=0.0;

  // v0s
    
  for (i=0; i<v0tot; i++) {
    StV0Vertex *vertex = v0Vertices[i];
    const StThreeVectorF& nMom = vertex->momentumOfDaughter(negative);
    const StThreeVectorF& pMom = vertex->momentumOfDaughter(positive);
    StThreeVectorF vMom = nMom + pMom;
    Float_t pN2 = nMom.mag2();
    Float_t pP2 = pMom.mag2();
    Float_t pV2 = vMom.mag2();
    Float_t eNpi = ::sqrt(pN2 + mMasspi2);
    Float_t eNpr = ::sqrt(pN2 + mMasspr2);
    Float_t ePpi = ::sqrt(pP2 + mMasspi2);
    Float_t ePpr = ::sqrt(pP2 + mMasspr2);
    Float_t eK0 = eNpi + ePpi;
    Float_t eLa = eNpi + ePpr;
    Float_t eLb = eNpr + ePpi;
    Float_t maK0 = ::sqrt(eK0*eK0 - pV2);
    Float_t maLa = ::sqrt(eLa*eLa - pV2);
    Float_t maLb = ::sqrt(eLb*eLb - pV2);
    
    
    Float_t ptV0 = TMath::Sqrt((vMom.x()*vMom.x())+(vMom.y()*vMom.y()));
    
    
    perc = (maK0/kaon_0_short_mass_c2) - 1.;
    if (TMath::Abs(perc) < (2.0*mRange)) // mRange too narrow for highPt K0s
      {
	nK0++;
	if(ptV0>maxPtK0) maxPtK0=ptV0;
      }
    else if (TMath::Abs(perc + (2.0*m2Range)) < (2.0*mRange)) nbelowK0++;
    else if (TMath::Abs(perc - (2.0*m2Range)) < (2.0*mRange)) naboveK0++;
    
    perc = (maLa/lambda_mass_c2) - 1.;
    if (TMath::Abs(perc) < (0.5*mRange)) // mRange too wide for La, Lb.
      {
	nLa++;
	if(ptV0>maxPtLa) maxPtLa=ptV0;
      }
    else if (TMath::Abs(perc + (0.5*m2Range)) < (0.5*mRange)) nbelowLa++;
    else if (TMath::Abs(perc - (0.5*m2Range)) < (0.5*mRange)) naboveLa++;
    
    perc = (maLb/antilambda_mass_c2) - 1.;
    if (TMath::Abs(perc) < (0.5*mRange))
      {
	nLb++;
	if(ptV0>maxPtLb) maxPtLb=ptV0;
      }
    else if (TMath::Abs(perc + (0.5*m2Range)) < (0.5*mRange)) nbelowLb++;
    else if (TMath::Abs(perc - (0.5*m2Range)) < (0.5*mRange)) naboveLb++;
    
  } //End V0 loop
      
  // Cascades
      
  for (i=0; i<castot; i++) {
    StXiVertex *vertex = xiVertices[i];
    const StThreeVectorF& bMom = vertex->momentumOfBachelor();
    double pcharge = vertex->chargeOfBachelor();
    StThreeVectorF lMom = vertex->momentumOfV0();
    StThreeVectorF cMom = lMom + bMom;
    Float_t pB2 = bMom.mag2();
    Float_t pL2 = lMom.mag2();
    Float_t pC2 = cMom.mag2();
    Float_t epi = ::sqrt(pB2 + mMasspi2);
    Float_t eka = ::sqrt(pB2 + mMasska2);
    Float_t ela = ::sqrt(pL2 + mMassla2); 
    Float_t eXi = ela + epi;
    Float_t maXi = ::sqrt(eXi*eXi - pC2);
    Float_t eOm = ela + eka;
    Float_t maOm = ::sqrt(eOm*eOm - pC2);
    
    Float_t ptCas = TMath::Sqrt((cMom.x()*cMom.x())+(cMom.y()*cMom.y()));
    perc = (maXi/xi_minus_mass_c2) - 1.;
    // Xi minus, Xi-bar plus
    if(pcharge<0)
      {
	if (TMath::Abs(perc) < mRange)
	  {
	    nXi++;
	    if(ptCas>maxPtXi) maxPtXi=ptCas;
	  }
	else if (TMath::Abs(perc + m2Range) < mRange) nbelowXi++;
	else if (TMath::Abs(perc - m2Range) < mRange) naboveXi++;
      }
    else
      {
	if (TMath::Abs(perc) < mRange)
	  {
	    nXibar++;
	    if(ptCas>maxPtXibar) maxPtXibar=ptCas;
	  }
	else if (TMath::Abs(perc + m2Range) < mRange) nbelowXibar++;
	else if (TMath::Abs(perc - m2Range) < mRange) naboveXibar++;
      }
    
    // Omega minus, Omega-bar plus
    perc = (maOm/M_OMEGA_MINUS) - 1.;
    if(pcharge<0)
      {
	if (TMath::Abs(perc) < mRange)
	  {
	    nOm++;
	    if(ptCas>maxPtOm) maxPtOm=ptCas;
	  }
	else if (TMath::Abs(perc + m2Range) < mRange) nbelowOm++;
	else if (TMath::Abs(perc - m2Range) < mRange) naboveOm++;
      }
    else
      {
	if (TMath::Abs(perc) < mRange)
	  {
	    nOmbar++;
	    if(ptCas>maxPtOmbar) maxPtOmbar=ptCas;
	  }
	else if (TMath::Abs(perc + m2Range) < mRange) nbelowOmbar++;
	else if (TMath::Abs(perc - m2Range) < mRange) naboveOmbar++;
      }
    
    
  } // End Cascade loop

  // Kinks
  StSPtrVecKinkVertex& kinkVertices = mEvent->kinkVertices();
  for(StSPtrVecKinkVertexIterator i=kinkVertices.begin();i != kinkVertices.end();i++){
    StTrack* parentTrack=(*i)->parent();
    Int_t charge = parentTrack->geometry()->charge();
    Float_t ptKink = parentTrack->geometry()->momentum().perp();
    if(charge > 0){
      nKinkPos++;
      if(ptKink > maxPtKinkPos) maxPtKinkPos = ptKink;
    }
    else if (charge < 0){
      nKinkNeg++;
      if(ptKink > maxPtKinkNeg) maxPtKinkNeg = ptKink;
    }
  }

  mTagTable->NV0 = v0tot;
  mTagTable->NbelowK0 = nbelowK0;
  mTagTable->NK0 = nK0;
  mTagTable->NaboveK0 = naboveK0;
  mTagTable->NbelowLa = nbelowLa;
  mTagTable->NLa = nLa;
  mTagTable->NaboveLa = naboveLa;
  mTagTable->NbelowLb = nbelowLb;
  mTagTable->NLb = nLb;
  mTagTable->NaboveLb = naboveLb;
  mTagTable->NbelowXi = nbelowXi;
  mTagTable->NXi = nXi;
  mTagTable->NaboveXi = naboveXi;
  mTagTable->NbelowXibar = nbelowXibar;
  mTagTable->NXibar = nXibar;
  mTagTable->NaboveXibar = naboveXibar;
  mTagTable->NbelowOm = nbelowOm;
  mTagTable->NOm = nOm;
  mTagTable->NaboveOm = naboveOm;
  mTagTable->NbelowOmbar = nbelowOmbar;
  mTagTable->NOmbar = nOmbar;
  mTagTable->NaboveOmbar = naboveOmbar;
  
  mTagTable->MaxPtK0 = maxPtK0;
  mTagTable->MaxPtLa = maxPtLa;
  mTagTable->MaxPtLb = maxPtLb;
  mTagTable->MaxPtXi = maxPtXi;
  mTagTable->MaxPtXibar = maxPtXibar;
  mTagTable->MaxPtOm = maxPtOm;
  mTagTable->MaxPtOmbar = maxPtOmbar;
      
  mTagTable->range = mRange;

  mTagTable->NKinkPos = nKinkPos;
  mTagTable->NKinkNeg = nKinkNeg;
  mTagTable->MaxPtKinkPos = maxPtKinkPos;
  mTagTable->MaxPtKinkNeg = maxPtKinkNeg;

}

void StStrangeTagsMaker::printTag(ostream& os) 
{
  os << "--- Strangeness Tag Table ---" << endl; 
  if (!mTagTable) 
    os << "(empty)" << endl;
  else {
    os << "Range used:  +/-" << mTagTable->range << " * M" << endl;
    os << "No. V0's:       " << mTagTable->NV0 << endl;
    os << "No. K0's:       " << mTagTable->NK0 << endl;
    os << "No. La's:       " << mTagTable->NLa << endl;
    os << "No. Lb's:       " << mTagTable->NLb << endl;
    os << "No. Xi's:       " << mTagTable->NXi << endl;
    os << "No. Xibar's:       " << mTagTable->NXi << endl;
    os << "No. Om's:       " << mTagTable->NOm << endl;
    os << "No. Ombar's:       " << mTagTable->NOmbar << endl;
    os << "Backgrounds: (same bin size)" << endl;
    os << "No. below K0's: " << mTagTable->NbelowK0 << endl;
    os << "No. above K0's: " << mTagTable->NaboveK0 << endl;
    os << "No. below La's: " << mTagTable->NbelowLa << endl;
    os << "No. above La's: " << mTagTable->NaboveLa << endl;
    os << "No. below Lb's: " << mTagTable->NbelowLb << endl;
    os << "No. above Lb's: " << mTagTable->NaboveLb << endl;
    os << "No. below Xi's: " << mTagTable->NbelowXi << endl;
    os << "No. above Xi's: " << mTagTable->NaboveXi << endl;
    os << "No. below Xibar's: " << mTagTable->NbelowXibar << endl;
    os << "No. above Xibar's: " << mTagTable->NaboveXibar << endl;
    os << "No. below Om's: " << mTagTable->NbelowOm << endl;
    os << "No. above Om's: " << mTagTable->NaboveOm << endl;
    os << "No. below Ombar's: " << mTagTable->NbelowOmbar << endl;
    os << "No. above Ombar's: " << mTagTable->NaboveOmbar << endl;
    os << "Rare probe tags: (same ranges)" << endl;
    os << "max Pt K0:      " << mTagTable->MaxPtK0  << endl;
    os << "max Pt La:      " << mTagTable->MaxPtLa  << endl;
    os << "max Pt Lb:      " << mTagTable->MaxPtLb  << endl;
    os << "max Pt Xi:      " << mTagTable->MaxPtXi  << endl;
    os << "max Pt Xibar:      " << mTagTable->MaxPtXibar  << endl;
    os << "max Pt Om:      " << mTagTable->MaxPtOm  << endl;
    os << "max Pt Ombar:      " << mTagTable->MaxPtOmbar  << endl;
  }   
}
