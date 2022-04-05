// -*- mode: C++ -*-

//
// Pibero Djawotho <pibero@indiana.edu>
// Indiana University
// 19 July 2007
//

#ifndef ST_GAMMA_PYTHIA_EVENT_H
#define ST_GAMMA_PYTHIA_EVENT_H

#include <vector>
using std::vector;

#include "TLorentzVector.h"
#include "StSpinPool/StMCAsymMaker/StPythiaEvent.h"

class StGammaPythiaEvent : public StPythiaEvent 
{

    public:
    
        StGammaPythiaEvent() {}
        StGammaPythiaEvent(const StPythiaEvent* pythia) : StPythiaEvent(*pythia) {}
        ~StGammaPythiaEvent() {}
        
        virtual const char* GetCVS() const
        {static const char cvs[] = "Tag $Name:  $ $Id: StGammaPythiaEvent.h,v 1.6 2014/08/06 11:43:18 jeromel Exp $ built " __DATE__ " " __TIME__; return cvs; }
        
        void Clear(Option_t* option = "");
        
        int numberOfPrompt() const;
        int numberOfConversion() const;
        int numberOfHadron() const;
        int numberOfNeutralPion() const;
        
        TLorentzVector& prompt(int i);
        TLorentzVector& conversion(int i);
        TLorentzVector& hadron(int i);
        TLorentzVector& neutralPion(int i);
        
        vector<TLorentzVector>& prompt();
        vector<TLorentzVector>& conversion();
        vector<TLorentzVector>& hadron();
        vector<TLorentzVector>& neutralPion();

    private:
        vector<TLorentzVector> mPrompt;
        vector<TLorentzVector> mConversion;
        vector<TLorentzVector> mHadron;
        vector<TLorentzVector> mNeutralPion;
        
        ClassDef(StGammaPythiaEvent, 2);
        
};

inline void StGammaPythiaEvent::Clear(Option_t* option)
{
    mPrompt.clear();
    mConversion.clear();
    mHadron.clear();
    mNeutralPion.clear();
    StPythiaEvent::Clear(option);
}

inline int StGammaPythiaEvent::numberOfPrompt() const { return mPrompt.size(); }
inline int StGammaPythiaEvent::numberOfConversion() const { return mConversion.size(); }
inline int StGammaPythiaEvent::numberOfHadron() const { return mHadron.size(); }
inline int StGammaPythiaEvent::numberOfNeutralPion() const { return mNeutralPion.size(); }

inline TLorentzVector& StGammaPythiaEvent::prompt(int i) { return mPrompt[i]; }
inline TLorentzVector& StGammaPythiaEvent::conversion(int i) { return mConversion[i]; }
inline TLorentzVector& StGammaPythiaEvent::hadron(int i) { return mHadron[i]; }
inline TLorentzVector& StGammaPythiaEvent::neutralPion(int i) { return mNeutralPion[i]; }

inline vector<TLorentzVector>& StGammaPythiaEvent::prompt() { return mPrompt; }
inline vector<TLorentzVector>& StGammaPythiaEvent::conversion() { return mConversion; }
inline vector<TLorentzVector>& StGammaPythiaEvent::hadron() { return mHadron; }
inline vector<TLorentzVector>& StGammaPythiaEvent::neutralPion() { return mNeutralPion; }

#endif
