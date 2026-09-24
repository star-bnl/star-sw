#include "CRMCRHICfEventOutput.h"

CRMCRHICfEventOutput::CRMCRHICfEventOutput()
: fRHICfRunType(CRMCRHICfOption::RunType::NON), fModelIdx(-1)
{
}

CRMCRHICfEventOutput::~CRMCRHICfEventOutput()
{
    delete fRHICfFilter;
    delete fEventTree;
    delete fRunTree;
    delete fFile;
    delete fParticleArray;
    delete fParticle;

    fRHICfFilter = nullptr;
    fEventTree = nullptr;
    fRunTree = nullptr;
    fFile = nullptr;
    fParticleArray = nullptr;
    fParticle = nullptr;
}

void CRMCRHICfEventOutput::InitOutput(const CRMCRHICfOption& cfg)
{
    InitOutputFile(cfg);

    fRandom = new TRandom3(cfg.GetSeed());
    InitVertexFluctuation();
    
    fRHICfFilter = new CRMCRHICfFilter(fRHICfRunType);
    fRHICfFilter -> Init();
}

//--------------------------------------------------------------------
void CRMCRHICfEventOutput::FillRHICfEvent(const CRMCRHICfOption& cfg, const int nEvent, int& passEventNum)
{
    if (!_hepevt.convert(_event)){throw std::runtime_error("!!!Could not read next event");}
    fParticleArray -> Clear("C");

    // random vertex for STAR
    double collisionVtxX = fRandom -> Gaus(fVertexMean[0], fVertexSigma[0]); // [mm]
    double collisionVtxY = fRandom -> Gaus(fVertexMean[1], fVertexSigma[1]); // [mm]
    double collisionVtxZ = fRandom -> Gaus(fVertexMean[2], fVertexSigma[2]); // [mm]

    fGenEventNum = nEvent+1;
    fPassedEventNum = passEventNum+1;
    fProcessID = gCRMC_data.typevt;

    int RHICfHitTrkNum = 0;
    int particleNum = _event.particles_size();
    for(int par=0; par<particleNum; par++) {
        auto p = (_event.particles())[par];

        int stat = p -> status();
        int id = p -> id();
        int pid = p -> pdg_id();
        double eta = p -> momentum().pseudoRapidity();
        double vx = p -> production_vertex()->position().x() + collisionVtxX; // [mm]
        double vy = p -> production_vertex()->position().y() + collisionVtxY; // [mm]
        double vz = p -> production_vertex()->position().z() + collisionVtxZ; // [mm]
        double t = p -> production_vertex()->position().t(); // [mm/c]
        double px = p -> momentum().px();
        double py = p -> momentum().py();
        double pz = p -> momentum().pz();
        double e = p -> momentum().e();
        double mass = p -> generated_mass();

        int parentSize = p -> parents().size();
        int daughterSize = p -> children().size();

        int parentIdx1 = -1;
        int parentIdx2 = -1;
        int daughterIdx1 = -1;
        int daughterIdx2 = -1;

        if(parentSize == 1){
            parentIdx1 = p -> parents()[0] -> id();
            parentIdx2 = 0;
        }
        if(stat == 2 && daughterSize != 0){
            daughterIdx1 = p -> children()[0] -> id();
            if(daughterSize == 2){daughterIdx2 = p -> children()[1] -> id();}
        }

        // ========================================================================================================================
        // Note: Generator-level parent and daughter index would be shifted to +1 in this RHICfSimGenerator.root
        //       This shifted index will be fixed in STAR simulation generator class, this code is correct way!
        //       If you want to look the Generator-level information in RHICfSimGenerator.root, you should subtract the index to 1.
        // ========================================================================================================================

        fParticle = (TParticle*)fParticleArray -> ConstructedAt(par);
        fParticle -> SetPdgCode(pid);
        fParticle -> SetStatusCode(stat);
        fParticle -> SetProductionVertex(vx, vy, vz, t); // [mm, mm, mm, mm/c]
        fParticle -> SetMomentum(px, py, pz, e); // [GeV/c]
        fParticle -> SetCalcMass(mass); // [GeV/c^2]
        fParticle -> SetFirstMother(parentIdx1);
        fParticle -> SetLastMother(parentIdx2);
        fParticle -> SetFirstDaughter(daughterIdx1);
        fParticle -> SetLastDaughter(daughterIdx2);

        if(fRHICfRunType == CRMCRHICfOption::RunType::ALL){continue;}
        if(stat != 1){continue;} // only final state

        pid = abs(pid);
        if(11 < pid && pid < 19 ){continue;} // cut the lepton (except electron)
        if(e < 1.){continue;} // energy cut 1 GeV
        if(pz <= 0.){continue;} // opposite direction cut

        bool isInterest = fRHICfFilter->IsInterestedParticle(pid);
        
        // cut the final state charged particle generated Z-position before end of DX magnet
        if(!isInterest && vz < 15000.){continue;}
        int hit = fRHICfFilter->GetRHICfGeoHit(vx, vy, vz, px, py, pz, e);
        if(hit < 0){continue;}

        RHICfHitTrkNum++;
    }

    if(fRHICfRunType != CRMCRHICfOption::RunType::ALL){
        if(RHICfHitTrkNum != 0){
            fEventTree -> Fill();
            PrintEvent();
            passEventNum++;
        }
    }
    else{
        fEventTree -> Fill();
        passEventNum++;
    }
}

//--------------------------------------------------------------------
void CRMCRHICfEventOutput::CloseOutput()
{
    fFile -> cd();
    fRunTree -> Write();
    fEventTree -> Write();
    fFile -> Close();
    cout << "CRMCRHICfEventOutput::CloseOutput() --- Output file has been written" << endl;
}

void CRMCRHICfEventOutput::PrintEvent()
{
    cout << "--- CRMCRHICfEventOutput::PrintEvent() --- " << endl;
    cout << " Generated Event Number: " << fGenEventNum << endl;
    cout << " Passed Event Number   : " << fEventTree -> GetEntries() << endl;
    cout << " Event Process Id      : " << fProcessID  << endl;
    cout << " Total Particle Number : " << fParticleArray -> GetEntries() << endl;
}

void CRMCRHICfEventOutput::InitOutputFile(const CRMCRHICfOption& cfg)
{
    fRHICfRunType = cfg.GetRHICfRunType();
    fModelIdx = cfg.GetModelIdx();

    TString outputName = cfg.GetOutputFileName() + ".root";
    fFile = new TFile(outputName, "recreate");
    fRunTree = new TTree("Run", "Run");
    fEventTree = new TTree("Event", "Event");

    fRunTree -> Branch("RHICfRunType", &fRHICfRunType, "RHICfRunType/I");
    fRunTree -> Branch("ModelType", &fModelIdx, "ModelType/I");
    fRunTree -> Fill();

    fParticleArray = new TClonesArray("TParticle");
    fEventTree -> Branch("GenEventNum", &fGenEventNum, "GenEventNum/I");
    fEventTree -> Branch("PassedEventNum", &fPassedEventNum, "PassedEventNum/I");
    fEventTree -> Branch("ProcessID", &fProcessID, "ProcessID/I");
    fEventTree -> Branch("Particles", &fParticleArray);

    fGenEventNum = 0;
    fPassedEventNum = 0;
    fProcessID = -1;
}

void CRMCRHICfEventOutput::InitVertexFluctuation()
{
    fVertexMean[0] = 0.; // x
    fVertexMean[1] = 0.; // y
    fVertexMean[2] = 0.; // z
    if(fRHICfRunType == CRMCRHICfOption::RunType::TL){
        fVertexMean[0] = 0.044 * 10.; // [mm]
        fVertexMean[1] = 0.186 * 10.; // [mm]
    }
    else if(fRHICfRunType == CRMCRHICfOption::RunType::TS){
        fVertexMean[0] = 0.022 * 10.; // [mm]
        fVertexMean[1] = 0.19 * 10.; // [mm]
    }
    else if(fRHICfRunType == CRMCRHICfOption::RunType::TOP){
        fVertexMean[0] = 0.022 * 10.; // [mm]
        fVertexMean[1] = -0.053 * 10.; // [mm]
    }
    fVertexSigma[0] = 0.2; // [mm]
    fVertexSigma[1] = 0.2; // [mm]
    fVertexSigma[2] = 300.; // [mm]
}
