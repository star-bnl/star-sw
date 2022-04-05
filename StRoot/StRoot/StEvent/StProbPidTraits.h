/*!
 * \class StProbPidTraits 
 * \author Yuri Fisyak, Oct 2002
 */
/***************************************************************************
 *
 * $Id: StProbPidTraits.h,v 2.6 2013/12/17 15:07:34 fisyak Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description: Yuri Fisyak, Oct 2002
 *    keep track Likelihood for each mass hypothesis
 *
 **************************************************************************/
#ifndef StProbPidTraits_hh
#define StProbPidTraits_hh
#include "StDetectorId.h"
#include "StTrackPidTraits.h"
#include "TArrayF.h"
#include "StParticleDefinition.hh"
#include "StPidParticleDefinition.h"

class StProbPidTraits : public StTrackPidTraits {
public:
    StProbPidTraits(const Int_t NDF=0, const StDetectorId Id=kUnknownId, const StPidParticle N=KPidParticles, 
		    const Float_t *PidArray = 0, Double_t *Fractions = 0);
    virtual ~StProbPidTraits();
    Int_t    GetNDF()      {return mNDF;}
    TArrayF *GetPidArray() {return mPidArray;}
    Double_t GetProbability(Int_t PartId) ;
    Double_t GetChi2Prob(Int_t PartId) const; // 
    Double_t GetSum() { return mSum;}
    void     SetFractions(Double_t *Fractions) {mFractions = Fractions; mSum = 0;}
    void     SetNDF(Int_t ndf)                 {mNDF = ndf;}
    void     Print(Option_t *opt = "") const;
    static   StParticleDefinition  *mPidParticleDefinitions[KPidParticles]; //!

 protected:
    Int_t     mNDF;
    TArrayF  *mPidArray;
    Double_t  mSum;                         //!
    Double_t *mFractions;                   //!
    Double_t  mProbability[KPidParticles];  //!
    
    ClassDef(StProbPidTraits,1)
};
#endif
