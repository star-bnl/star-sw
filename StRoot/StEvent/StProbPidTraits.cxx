/***************************************************************************
 *
 * $Id: StProbPidTraits.cxx,v 2.2 2003/09/02 17:58:05 perev Exp $
 *
 * Author: Yuri Fisyak, Oct 2002
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StProbPidTraits.cxx,v $
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

static const char rcsid[] = "$Id: StProbPidTraits.cxx,v 2.2 2003/09/02 17:58:05 perev Exp $";
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
void StProbPidTraits::Print(Option_t *opt) {
  Int_t N = mPidArray->GetSize();
  const Float_t *Array = mPidArray->GetArray();
  Int_t i;
  cout << "\tNDF = \t" << mNDF << endl;
  for (i = 0; i < N; i++) {
    cout << "Particle : \t" << mPidParticleDefinitions[i]->name();
    if (mFractions) cout << "\tFraction : \t" << mFractions[i];
    cout << "\tProbability :\t" << GetProbability(i) 
	 << "\tChisq :\t" << Array[i];
      if (mNDF > 0) cout << "\t Chisq Prob:\t" << TMath::Prob(Array[i],mNDF);
    cout << endl;
  }
}
//________________________________________________________________________________
StObject* StProbPidTraits::clone() const { return new StProbPidTraits(*this); }
