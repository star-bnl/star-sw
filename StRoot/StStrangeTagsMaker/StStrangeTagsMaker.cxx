/***************************************************************************
 *
 * $Id: StStrangeTagsMaker.cxx,v 1.1 1999/02/21 23:35:11 genevb Exp $
 *
 * Author: Gene Van Buren, Feb 1999
 ***************************************************************************
 *
 * Description:  Maker to fill the Strangeness Tags
 *
 ***************************************************************************
 *
 * $Log: StStrangeTagsMaker.cxx,v $
 * Revision 1.1  1999/02/21 23:35:11  genevb
 * Strangeness Tags Maker
 *
 **************************************************************************/
#include "StStrangeTagsMaker.h"
#include "StRoot/StEventReaderMaker/StEventReaderMaker.h"
#include "StEvent/StEvent.hh"
#include "StEvent/StV0Vertex.hh"
#include "StChain/StChain.h"
#include "TMath.h"
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
    delete mTagTable;    // clean up
}

StStrangeTagsMaker::Init()
{
    mRange = 0.02;
    m2Range = 2*mRange;
    mMasspi2 = M_PION_PLUS*M_PION_PLUS;
    mMasspr2 = M_PROTON*M_PROTON;

    return StMaker::Init();
}

Int_t StStrangeTagsMaker::Make()
{
    delete mTagTable;
    mTagTable = new StrangeTag_st;
    StEventReaderMaker* evMaker = (StEventReaderMaker*) gStChain->Maker("events");
    if (! evMaker->event()) return kStOK;
    mEvent = evMaker->event();                      	
      
    fillTag();
            
    return kStOK;
}

void StStrangeTagsMaker::PrintInfo()
{
    cout << "$Id: StStrangeTagsMaker.cxx,v 1.1 1999/02/21 23:35:11 genevb Exp $" << endl;
    if (gStChain->Debug()) StMaker::PrintInfo();
}

StrangeTag_st* StStrangeTagsMaker::tag()
{
    return mTagTable;
}

void StStrangeTagsMaker::fillTag()
{
    Int_t v0tot = 0;
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

    for (StVertexIterator vertices = mEvent->vertexCollection()->begin();
                    vertices != mEvent->vertexCollection()->end(); vertices++) {
      if ( (*vertices)->type() != V0 ) continue;
      v0tot++;
      StV0Vertex *vertex = (StV0Vertex *) *vertices;
      StThreeVector<float> nMom = vertex->momentumOfDaughter(negativeTrack);
      StThreeVector<float> pMom = vertex->momentumOfDaughter(positiveTrack);
      StThreeVector<float> vMom = nMom + pMom;
      Float_t pN2 = nMom.mag2();
      Float_t pP2 = pMom.mag2();
      Float_t pV2 = vMom.mag2();
      Float_t eNpi = TMath::Sqrt(pN2 + mMasspi2);
      Float_t eNpr = TMath::Sqrt(pN2 + mMasspr2);
      Float_t ePpi = TMath::Sqrt(pP2 + mMasspi2);
      Float_t ePpr = TMath::Sqrt(pP2 + mMasspr2);
      Float_t eK0 = eNpi + ePpi;
      Float_t eLa = eNpi + ePpr;
      Float_t eLb = eNpr + ePpi;
      Float_t maK0 = TMath::Sqrt(eK0*eK0 - pV2);
      Float_t maLa = TMath::Sqrt(eLa*eLa - pV2);
      Float_t maLb = TMath::Sqrt(eLb*eLb - pV2);

      Float_t perc = (maK0/M_KAON_0_SHORT) - 1.;
      if (TMath::Abs(perc) < mRange) nK0++;
      else if (TMath::Abs(perc + m2Range) < mRange) nbelowK0++;
      else if (TMath::Abs(perc - m2Range) < mRange) naboveK0++;

      perc = (maLa/M_LAMBDA) - 1.;
      if (TMath::Abs(perc) < mRange) nLa++;
      else if (TMath::Abs(perc + m2Range) < mRange) nbelowLa++;
      else if (TMath::Abs(perc - m2Range) < mRange) naboveLa++;

      perc = (maLb/M_ANTILAMBDA) - 1.;
      if (TMath::Abs(perc) < mRange) nLb++;
      else if (TMath::Abs(perc + m2Range) < mRange) nbelowLb++;
      else if (TMath::Abs(perc - m2Range) < mRange) naboveLb++;

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
        os << "No. V0's:       " << mTagTable->NV0 << endl;
        os << "No. K0's:       " << mTagTable->NK0 << endl;
        os << "No. La's:       " << mTagTable->NLa << endl;
        os << "No. Lb's:       " << mTagTable->NLb << endl;
//        os << "No. Xi's:       " << mTagTable->NXi << endl;
        os << "Range used:     " << mTagTable->range << endl;
        os << "No. below K0's: " << mTagTable->NbelowK0 << endl;
        os << "No. above K0's: " << mTagTable->NaboveK0 << endl;
        os << "No. below La's: " << mTagTable->NbelowLa << endl;
        os << "No. above La's: " << mTagTable->NaboveLa << endl;
        os << "No. below Lb's: " << mTagTable->NbelowLb << endl;
        os << "No. above Lb's: " << mTagTable->NaboveLb << endl;
//        os << "No. below Xi's: " << mTagTable->NbelowXi << endl;
//        os << "No. above Xi's: " << mTagTable->NaboveXi << endl;
    }   
}
