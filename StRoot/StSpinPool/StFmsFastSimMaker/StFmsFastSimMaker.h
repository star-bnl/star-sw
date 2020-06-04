// \class StFmsFastSimMaker
// \author Akio Ogawa
//
//   This is FMS fast simulator maker, without using GEANT, but taking tracks
//   from PYTHIA and parametrized calorimeter resposes
//
//  $Id: StFmsFastSimMaker.h,v 1.1 2016/06/09 12:17:43 akio Exp $
//  $Log: StFmsFastSimMaker.h,v $
//  Revision 1.1  2016/06/09 12:17:43  akio
//  First version
//

#ifndef STAR_StFmsFastSimMaker_HH
#define STAR_StFmsFastSimMaker_HH

#include "StMaker.h"
#include "StLorentzVectorF.hh"

class StFmsDbMaker;
class StFmsCollection;

class StFmsFastSimMaker : public StMaker{
public: 
    StFmsFastSimMaker(const Char_t* name="FmsFastSimMaker");
    ~StFmsFastSimMaker();
    Int_t Init();
    Int_t Make();
    void  Clear(Option_t *option="");

    Int_t nPi0() {return mRealPi0.size();}
    StLorentzVectorF *pi0(Int_t v) {return mRealPi0[v];}

    void setPrint(int v){mPrint=v;}

private:
    int mPrint;
    Float_t hadronResponse(float e, float& f);

    vector<StLorentzVectorF *> mRealPi0; //

    virtual const char *GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StFmsFastSimMaker.h,v 1.1 2016/06/09 12:17:43 akio Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
    
    ClassDef(StFmsFastSimMaker,0);
};

#endif
