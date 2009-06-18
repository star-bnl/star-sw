////////////////////////////////////////////////////////////
//                                                        //
//    StGammaPythiaEventMaker                             //
//                                                        //
//    Michael Betancourt                                  //
//    Massachusetts Institute of Technology               //
//                                                        //
//    Retrieve event information from the PYTHIA          //
//    and GEANT records                                   //
//                                                        //
//    Original implementation by Pibero Djawatho (IUCF)   //
//                                                        //
////////////////////////////////////////////////////////////

#ifndef STAR_StGammaPythiaEventMaker
#define STAR_StGammaPythiaEventMaker

class StMcVertex;
class StGammaPythiaEvent;

#include "StMaker.h"

class StGammaPythiaEventMaker : public StMaker 
{

    public:
        StGammaPythiaEventMaker(const char* name = "GammaPythia") : StMaker(name), mPythia(0) {}
        ~StGammaPythiaEventMaker() {}
        
        virtual const char* GetCVS() const
        {static const char cvs[] = "Tag $Name:  $ $Id: StGammaPythiaEventMaker.h,v 1.3 2009/06/18 05:51:21 betan Exp $ built "__DATE__" "__TIME__; return cvs; }
        
        void SetPythia(StGammaPythiaEvent* pythia) { mPythia = pythia; }
        
        Int_t Init() { return StMaker::Init(); }
        void Clear(Option_t *opts = "") { return StMaker::Clear(opts); }
        Int_t Make();
        Int_t Finish() { return kStOK; }
    
    private:
        
        StGammaPythiaEvent* mPythia;
        
        ClassDef(StGammaPythiaEventMaker, 2);
  
};

#endif
