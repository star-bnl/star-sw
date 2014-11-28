////////////////////////////////////////////////////////////
//                                                        //
//    StGammaScheduleMaker                                  //
//                                                        //
//    Michael Betancourt                                  //
//    Massachusetts Institute of Technology               //
//                                                        //
//    Allows for the utilization of multiple timestamps   //
//    within a given simulation file                      //
//                                                        //
////////////////////////////////////////////////////////////

#ifndef STAR_StGammaScheduleMaker
#define STAR_StGammaScheduleMaker

#include "StMaker.h"
#include <vector>

using namespace std;

struct stamp
{
    int date;
    int time;
    double weight;
    double event;
};

class StGammaScheduleMaker : public StMaker 
{

    public:
        StGammaScheduleMaker(const char* name = "GammaSchedule");
        ~StGammaScheduleMaker() {}
        
        virtual const char* GetCVS() const
        {static const char cvs[] = "Tag $Name:  $ $Id: StGammaScheduleMaker.h,v 1.3 2014/08/06 11:43:18 jeromel Exp $ built " __DATE__ " " __TIME__; return cvs; }
        
        Int_t Init();
        void Clear(Option_t *opts = "") { return StMaker::Clear(opts); }
        Int_t Make();
        Int_t Finish() { return kStOK; }
        
        void addTimestamp(int date, int time, double weight);
        void rearrange();
        
        int index() { return mStampIndex; }
        int nStamps() { return mStamps.size(); }
    
    private:
        
        double mTotalEvents;
        int mCurrentEvent;
        int mStampIndex;
        vector<stamp> mStamps;
        
        ClassDef(StGammaScheduleMaker, 1);
  
};

#endif
