/***************************************************************************
 *
 * $Id: StProbPidTraits.cxx,v 2.5 2004/07/21 14:09:57 fisyak Exp $
 *
 * Author: Yuri Fisyak, Oct 2002
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StProbPidTraits.cxx,v $
 * Revision 2.5  2004/07/21 14:09:57  fisyak
 * Add GetChi2Prob method
 *
 * Revision 2.4  2004/07/15 16:36:24  ullrich
 * Removed all clone() declerations and definitions. Use StObject::clone() only.
 *
 * Revision 2.3  2003/09/07 03:49:02  perev
 * gcc 3.2 + WarnOff
 *
 * Revision 2.2  2003/09/02 17:58:05  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 2.1  2002/10/31 22:47:11  fisyak
 * Add new class to keep H.Bichsel probabilities for each mass hypothesis
 *
 **************************************************************************/
#include <Stiostream.h>
#include "StProbPidTraits.h"
#include "StParticleTypes.hh"
#include "TMath.h"
ClassImp(StProbPidTraits)

static const char rcsid[] = "$Id: StProbPidTraits.cxx,v 2.5 2004/07/21 14:09:57 fisyak Exp $";
StParticleDefinition *StProbPidTraits::mPidParticleDefinitions[KPidParticles] = {
    StElectron::instance(),
    StProton::instance(),     
    StKaonMinus::instance(),  
    StPionMinus::instance(),  
    StMuonMinus::instance(),  
    StDeuteron::instance(),   
    StTriton::instance(),     
    StHe3::instance(),        
    StAlpha::instance()
};

//________________________________________________________________________________
StProbPidTraits::StProbPidTraits(const Int_t NDF, const StDetectorId Id, const StPidParticle N, 
				 const Float_t *PidArray, Double_t *Fractions) : 
    StTrackPidTraits(Id), mNDF(NDF), mSum(0), mFractions(Fractions) {
	if (PidArray) mPidArray = new TArrayF(N,PidArray);
	else          mPidArray = new TArrayF(N);
    }

//________________________________________________________________________________
StProbPidTraits::~StProbPidTraits() { delete mPidArray;} 

//________________________________________________________________________________
Double_t StProbPidTraits::GetProbability(Int_t PartId) {
    if (mSum > 0) return mProbability[PartId];
    Int_t N = mPidArray->GetSize();
    mSum = 0;
    memset(&mProbability[0],0,KPidParticles*sizeof(Double_t));
    const Float_t *Array = mPidArray->GetArray();
    Int_t i;
    for (i=kPidElectron; i< N; i++) {
	if (mFractions) 
	    mProbability[i] = TMath::Exp(-Array[i]/2.)*mFractions[i];
	else
      mProbability[i] = TMath::Exp(-Array[i]/2.);
	mSum += mProbability[i];
    }
    if (mSum > 0) for (i=kPidElectron; i< N; i++) mProbability[i] /= mSum;
    return mSum > 0 ? mProbability[PartId] : 0;
}
//________________________________________________________________________________
Double_t StProbPidTraits::GetChi2Prob(Int_t k) const {
  if (mNDF < 1 || ! mPidArray ) return -1;
  Int_t N = mPidArray->GetSize();
  if (k >= N) return -1;
  const Float_t *Array = mPidArray->GetArray();
  return TMath::Prob(Array[k],mNDF);
}
//________________________________________________________________________________
void StProbPidTraits::Print(Option_t *opt) const {
  StProbPidTraits *This = (StProbPidTraits *) this;
  Int_t N = mPidArray->GetSize();
  const Float_t *Array = mPidArray->GetArray();
  Int_t i;
  cout << "\tNDF = \t" << mNDF << endl;
  for (i = kPidElectron; i < N; i++) {
    cout << "Particle : \t" << mPidParticleDefinitions[i]->name();
    if (mFractions) cout << "\tFraction : \t" << mFractions[i];
    cout << "\tProbability :\t" << This->GetProbability(i)
	 << "\tChisq :\t" << Array[i];
    if (mNDF > 0) cout << "\t Chisq Prob:\t" << GetChi2Prob(i);
    cout << endl;
  }
}
