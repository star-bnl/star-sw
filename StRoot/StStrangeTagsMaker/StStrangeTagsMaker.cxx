/***************************************************************************
 *
 * $Id: StStrangeTagsMaker.cxx,v 1.12 1999/11/16 19:20:44 genevb Exp $
 *
 * Author: Gene Van Buren, Feb 1999
 ***************************************************************************
 *
 * Description:  Maker to fill the Strangeness Tags
 *
 ***************************************************************************
 *
 * $Log: StStrangeTagsMaker.cxx,v $
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
#include "StChain.h"
#include <math.h>
#include "PhysicalConstants.h"

ClassImp(StStrangeTagsMaker)

StStrangeTagsMaker::StStrangeTagsMaker(const char *name, const char *title)
    : StMaker(name, title)
{
    mEvent    = 0;
    mTagTable = 0;       // init pointer to tag table
}

StStrangeTagsMaker::~StStrangeTagsMaker()
{
    delete mTagTable;    // clean up
}

Int_t StStrangeTagsMaker::Init()
{
    mRange = 0.025;
    m2Range = 2*mRange;
    mMasspi2 = pion_plus_mass_c2 * pion_plus_mass_c2;
    mMasspr2 = proton_mass_c2 * proton_mass_c2;
    mMassla2 = lambda_mass_c2 * lambda_mass_c2;

    return StMaker::Init();
}

Int_t StStrangeTagsMaker::Make()
{
    delete mTagTable;
    mTagTable = new StrangeTag_st;
    mEvent = (StEvent *) GetInputDS("StEvent");
    if (!mEvent) return kStOK; // If no event, we're done
      
    fillTag();
            
    return kStOK;
}


StrangeTag_st* StStrangeTagsMaker::tag()
{
    return mTagTable;
}

void StStrangeTagsMaker::fillTag()
{
    StSPtrVecV0Vertex& v0Vertices = mEvent->v0Vertices();
    Int_t v0tot = v0Vertices.size();
    StSPtrVecXiVertex& xiVertices = mEvent->xiVertices();
    Int_t castot = xiVertices.size();
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

    unsigned int i;

    for (i=0; i<v0tot; i++) {
        StV0Vertex *vertex = v0Vertices[i];
        const StThreeVectorF& nMom = vertex->momentumOfDaughter(negative);
        const StThreeVectorF& pMom = vertex->momentumOfDaughter(positive);
        StThreeVectorF vMom = nMom + pMom;
        Float_t pN2 = nMom.mag2();
        Float_t pP2 = pMom.mag2();
        Float_t pV2 = vMom.mag2();
        Float_t eNpi = sqrt(pN2 + mMasspi2);
        Float_t eNpr = sqrt(pN2 + mMasspr2);
        Float_t ePpi = sqrt(pP2 + mMasspi2);
        Float_t ePpr = sqrt(pP2 + mMasspr2);
        Float_t eK0 = eNpi + ePpi;
        Float_t eLa = eNpi + ePpr;
        Float_t eLb = eNpr + ePpi;
        Float_t maK0 = sqrt(eK0*eK0 - pV2);
        Float_t maLa = sqrt(eLa*eLa - pV2);
        Float_t maLb = sqrt(eLb*eLb - pV2);

        Float_t perc = (maK0/kaon_0_short_mass_c2) - 1.;
        if (abs(perc) < mRange) nK0++;
        else if (abs(perc + m2Range) < mRange) nbelowK0++;
        else if (abs(perc - m2Range) < mRange) naboveK0++;

        perc = (maLa/lambda_mass_c2) - 1.;
        if (abs(perc) < mRange) nLa++;
        else if (abs(perc + m2Range) < mRange) nbelowLa++;
        else if (abs(perc - m2Range) < mRange) naboveLa++;

        perc = (maLb/antilambda_mass_c2) - 1.;
        if (abs(perc) < mRange) nLb++;
        else if (abs(perc + m2Range) < mRange) nbelowLb++;
        else if (abs(perc - m2Range) < mRange) naboveLb++;

    }
    for (i=0; i<castot; i++) {

        StXiVertex *vertex = xiVertices[i];
        const StThreeVectorF& pMom = vertex->momentumOfBachelor();
        StThreeVectorF lMom = vertex->momentumOfV0();
        StThreeVectorF xMom = lMom + pMom;
        Float_t pP2 = pMom.mag2();
        Float_t pL2 = lMom.mag2();
        Float_t pX2 = xMom.mag2();
        Float_t epi = sqrt(pP2 + mMasspi2);
        Float_t ela = sqrt(pL2 + mMassla2);
        Float_t eXi = ela + epi;
        Float_t maXi = sqrt(eXi*eXi - pX2);

        Float_t perc = (maXi/xi_minus_mass_c2) - 1.;
        if (abs(perc) < mRange) nXi++;
        else if (abs(perc + m2Range) < mRange) nbelowXi++;
        else if (abs(perc - m2Range) < mRange) naboveXi++;

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
    mTagTable->range = mRange;
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
        os << "Backgrounds: (same bin size)" << endl;
        os << "No. below K0's: " << mTagTable->NbelowK0 << endl;
        os << "No. above K0's: " << mTagTable->NaboveK0 << endl;
        os << "No. below La's: " << mTagTable->NbelowLa << endl;
        os << "No. above La's: " << mTagTable->NaboveLa << endl;
        os << "No. below Lb's: " << mTagTable->NbelowLb << endl;
        os << "No. above Lb's: " << mTagTable->NaboveLb << endl;
        os << "No. below Xi's: " << mTagTable->NbelowXi << endl;
        os << "No. above Xi's: " << mTagTable->NaboveXi << endl;
    }   
}
