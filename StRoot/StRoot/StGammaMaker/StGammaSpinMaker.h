////////////////////////////////////////////////////////////
//                                                        //
//    StGammaSpinMaker                                    //
//                                                        //
//    Retrieve event spin information                     //
//                                                        //
//    Original concept and implementation by              //
//    Jason Webb (Valpo)                                  //
//                                                        //
////////////////////////////////////////////////////////////

#ifndef STAR_StGammaSpinMaker_h
#define STAR_StGammaSpinMaker_h

#include "StMaker.h"
#include "TString.h"

class TFile;
class StGammaEvent;
class StSpinDbMaker;

class StGammaSpinMaker: public StMaker
{

    public:
    
        StGammaSpinMaker(const char *name="mGammaSpinMaker");
        ~StGammaSpinMaker();
        
        virtual const char* GetCVS() const
        {static const char cvs[] = "Tag $Name:  $ $Id: StGammaSpinMaker.h,v 1.4 2014/08/06 11:43:18 jeromel Exp $ built " __DATE__ " " __TIME__; return cvs; }
        
        // Required Maker Methods
        Int_t Init() { return StMaker::Init(); }
        Int_t Make();
        void  Clear(Option_t *opts="") { return StMaker::Clear(opts); }
        Int_t Finish() { return kStOK; }

    private:
    protected:

        StSpinDbMaker *mSpinDb;

  ClassDef(StGammaSpinMaker, 1);

};

#endif
