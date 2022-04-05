////////////////////////////////////////////////////////////
//                                                        //
//    StGammaMaker                                        //
//                                                        //
//    Michael Betancourt                                  //
//    Massachusetts Institute of Technology               //
//                                                        //
//    Wrapper for all things Gamma                        //
//                                                        //
//    StGammaPythiaEventMaker                             //
//        Gathers information from the PYTHIA and         //
//        GEANT records for later analysis.               //
//                                                        //
//    StGammaScheduleMaker                                //
//        Sets the STAR database timestamp dynamically    //
//        while avoiding unnecessary database overhead    //
//                                                        //
//    StGammaEventMaker                                   //
//        Initializes a StGammaEvent and fills in         //
//        basic event information.  Other StGamma         //
//        makers access this maker to contribute          //
//        further information.                            //
//                                                        //
//    StGammaRawMaker                                     //
//        Accesses, processes, and stores raw             //
//        detector (BEMC, EEMC, and TPC) responses.       //
//                                                        //
//    StGammaCandidateMaker                               //
//        Converts raw clusters into a unified object     //
//        (the StGammaCandidate).  Associates with each   //
//        candidate all detector objects within the       //
//        cluster itself and within a surrounding cone    //
//                                                        //
//    StGammaTreeMaker                                    //
//        Creates and attends to a tree where each        //
//        StGammaEvent is stored                          //
//                                                        //
//                                                        //
//    Original StGammaMaker concept and implementation    //
//    by Jason Webb (Valpo) and Pibero Djawatho (IUCF)    //
//                                                        //
////////////////////////////////////////////////////////////

#ifndef STAR_StGammaMaker
#define STAR_StGammaMaker

#include "StMaker.h"

#include "StGammaEventMaker.h"
#include "StGammaRawMaker.h"
#include "StGammaCandidateMaker.h"
#include "StGammaTreeMaker.h"
#include "StGammaPythiaEventMaker.h"
#include "StGammaScheduleMaker.h"

#include "StBarrelEmcClusterMaker.h"
//#include "StEEmcPool/StEEmcClusterMaker/StEEmcGenericClusterMaker.h"
#include "StEEmcPool/StEEmcClusterMaker/StMyClusterMaker.h"
#include "StEEmcPool/StEEmcA2EMaker/StEEmcA2EMaker.h"

#include "StSpinPool/StMCAsymMaker/StMCAsymMaker.h"

#include "StMuDSTMaker/COMMON/StMuDstMaker.h"

using namespace std;

class StGammaMaker: public StMaker
{

    public:
    
        enum detectorSwitch { kBemc, kEemc, kBoth };
        enum analysisType { kData, kSimu };
    
        StGammaMaker(const char *name = "gammaMaker", detectorSwitch detector = kBoth, analysisType analysis = kData);
        ~StGammaMaker() {};
        
        virtual const char* GetCVS() const
        {static const char cvs[] = "Tag $Name:  $ $Id: StGammaMaker.h,v 1.6 2014/08/06 11:43:18 jeromel Exp $ built " __DATE__ " " __TIME__; return cvs; }
        
        // Required Maker Methods
        Int_t Init();
        void  Clear(const Option_t* option = "") { StMaker::Clear(option); }
        Int_t Make() { return kStOK; }     
        Int_t Finish() { return kStOK; }
        
        /////////////////////////////////////////////
        //                Mutators                 //
        /////////////////////////////////////////////
    
        // Set StGammaTreeMaker parameters
        void setOutputFile(const char *output) { mTreeMaker->SetFilename(output); }
        void storeEmptyEvents() { mTreeMaker->storeEmptyEvents(); } // Be default empty events are skipped
        
        // Set StGammaCandidateMaker parameters
        // MinimumEt defaults to 0.0
        // Radius    defaults to 0.7
        // BSMD Range defaults to 0.05
        // ESMD Range defaults to 20 cm
        void setClusterEtThreshold(double threshold) { mCandidateMaker->SetMinimumEt(threshold); }
        void setConeRadius(double radius) { mCandidateMaker->SetRadius(radius); }
        void setBsmdRange(double range) { mCandidateMaker->SetBsmdRange(range); }
        void setEsmdRange(double range) { mCandidateMaker->SetEsmdRange(range); }
        
        void useStrictBemcStatus() { mCandidateMaker->useStrictBemcStatus(); }
        
        // Set clustering thresholds
        // In the BEMC
        //     Seed Energy Threshold    defaults to 0.0
        //     Cluster Energy Threshold defaults to 0.0
        // In the EEMC
        //     Seed Energy Threshold defaults to 0.8
        //     There is no cluster energy threshold
        void setSeedEnergyThreshold(double threshold);
        void setClusterEnergyThreshold(double threshold);
        
        // Set thresholds for adding tracks and towers to the StGammaEvent
        // TrackCutoff defaults to 0.0
        // TowerCutoff defaults to 0.0
        void setTrackEtRawThreshold(double threshold) { mRawMaker->SetTrackCutoff(threshold); }
        void setTowerEtRawThreshold(double threshold) { mRawMaker->SetTowerCutoff(threshold); }

        // Exclude any BEMC towers from the raw maker
        void excludeBemcTower(int softId) { mRawMaker->excludeBemcTower(softId); }

        // Shift BEMC tower gain for systematic studies
        void shiftBemcGains(double shift) { mRawMaker->shiftBemcGains(shift); }
        
        // Set threshold for including towers in the EEMC clustering
        // MinimumEnergy defaults to 0.1
        void setEemcTowerClusterThreshold(double threshold) { if(mUseEemc) mEemcClusterMaker->setMinimumEnergy(threshold); }
        
        // Add timestamps when running over simulation
        void addTimestamp(int date, int time, double weight);

        // Add trigger Ids to be simulated if trigger emulation is in the chain
        void addSimuTrigger(unsigned int triggerId) { mEventMaker->addSimuTrigger(triggerId); }
        
    private:
    
        bool mUseBemc;
        bool mUseEemc;
        
        bool mSimu;
        
        StMuDstMaker            *mMuDstMaker;
        StEEmcA2EMaker          *mEemcAnalysis;
        
        StGammaPythiaEventMaker *mPythiaMaker;
        StMCAsymMaker           *mAsymMaker;
        StGammaScheduleMaker    *mScheduleMaker;
        
        StGammaEventMaker       *mEventMaker;
        StGammaRawMaker         *mRawMaker;
        
        StBarrelEmcClusterMaker *mBemcClusterMaker;
        StMyClusterMaker        *mEemcClusterMaker;
        
        StGammaCandidateMaker   *mCandidateMaker;
        StGammaTreeMaker        *mTreeMaker;

    ClassDef(StGammaMaker, 1);
        
};


#endif
