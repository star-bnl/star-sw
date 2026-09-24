#ifndef _CRMCRHICfEventOutput_h_
#define _CRMCRHICfEventOutput_h_

#include "CRMCRHICfFilter.h"
#include "CRMCRHICfOption.h"

#include <HepMC3/GenEvent.h>
#include <HepMC3/GenParticle.h>
#include <HepMC3/GenVertex.h>
#include "CRMChepevt.h"
#include "CRMCstat.h"

#include <CRMCinterface.h>
#include <CRMCconfig.h>
#include <iomanip>
#include <iostream>

#include "TRandom3.h"
#include "TString.h"
#include "TFile.h"
#include "TTree.h"
#include "TClonesArray.h"
#include "TParticle.h"

class CRMCRHICfOption;

using namespace std;

class CRMCRHICfEventOutput 
{
    public:
        CRMCRHICfEventOutput();
        ~CRMCRHICfEventOutput();

        void InitOutput(const CRMCRHICfOption& cfg);
        void FillRHICfEvent(const CRMCRHICfOption& cfg, const int nEvent, int& passEventNum);
        void CloseOutput();

    private:
        void PrintEvent();
        void InitOutputFile(const CRMCRHICfOption& cfg);
        void InitVertexFluctuation();

        CRMCRHICfFilter* fRHICfFilter;

        CRMChepevt<HepMC3::GenParticlePtr,
            HepMC3::GenVertexPtr,
            HepMC3::FourVector,
            HepMC3::GenEvent> _hepevt;
        HepMC3::GenEvent _event;

        TFile* fFile;
        TTree* fEventTree;
        TTree* fRunTree;
        TClonesArray* fParticleArray;
        TParticle* fParticle;

        Int_t fRHICfRunType;
        Int_t fModelIdx;

        Int_t fGenEventNum;
        Int_t fPassedEventNum;
        Int_t fProcessID;

        // ====== vertex fluctuation parameters =======
        TRandom3* fRandom;
        double fVertexMean[3]; // mm [x, y, z]
        double fVertexSigma[3]; // mm [x, y, z]

};


#endif
