////////////////////////////////////////////////////////////
//                                                        //
//    StGammaEventMaker                                   //
//                                                        //
//    First StGamma maker in the chain, responsible for   //
//    creating the StGammaEvent and filling with basic    //
//    event information.  Other StGamma makers will       //
//    access the StGammaEvent to add further information. //
//                                                        //
//    Original concept and implementation by Jason        //
//    Webb (Valpo) and Pibero Djawatho (IUCF)             //
//                                                        //
////////////////////////////////////////////////////////////

#ifndef STAR_StGammaEventMaker
#define STAR_StGammaEventMaker

#include <vector>

#include <StMaker.h>
#include "StEEmcUtil/EEmcGeom/EEmcGeomDefs.h"

class StGammaEvent;

class StGammaTower;
class StGammaStrip;

class StGammaPythiaEvent;
class StGammaPythiaEventMaker;

class StMuDstMaker;

class StGammaEventMaker: public StMaker
{

    public:
    
        StGammaEventMaker(const char *name = "mGammaMaker");
        ~StGammaEventMaker();
        
        virtual const char* GetCVS() const
        {static const char cvs[] = "Tag $Name:  $ $Id: StGammaEventMaker.h,v 1.8 2014/08/06 11:43:18 jeromel Exp $ built " __DATE__ " " __TIME__; return cvs; }
        
        // Required Maker Methods
        Int_t Init();
        void  Clear( Option_t *opts= "");
        Int_t Make();
        Int_t Finish() { return StMaker::Finish(); }
        
        StGammaEvent *event() { return mGammaEvent; }
        void addSimuTrigger(unsigned int triggerId) { mRequestedTriggers.push_back(triggerId); }        

    private:
        
        StGammaEvent *mGammaEvent;
        StGammaPythiaEvent* mPythia;
        StGammaPythiaEventMaker* mPythiaMaker;
        StMuDstMaker *muDstMaker;
        vector<unsigned int> mRequestedTriggers;
        
        ClassDef(StGammaEventMaker, 1);
  
};

#endif
