#include "RHICfPrimaryGeneratorAction.hh"


RHICfPrimaryGeneratorAction::RHICfPrimaryGeneratorAction()
{
    fParticleGun = new G4ParticleGun();
    fParTable = G4ParticleTable::GetParticleTable();

    fSimUtil = RHICfSimUtil::GetRHICfSimUtil();
    fSimOpt = fSimUtil -> GetOptions();
}

RHICfPrimaryGeneratorAction::~RHICfPrimaryGeneratorAction()
{
}

void RHICfPrimaryGeneratorAction::GeneratePrimaries(G4Event* anEvent)
{
    if(!fParticleGun){return;}

    if(fSimUtil->IsStarSimMode()){
        ReadSimDstParticle(anEvent);
    }
    else if(fSimUtil->IsSingleGenMode()){
        CreateSingleGen(anEvent);
    }
    else{
        
    }
}

void RHICfPrimaryGeneratorAction::ReadSimDstParticle(G4Event* anEvent)
{
    int eventId = anEvent->GetEventID();
    fInputTree -> GetEntry(eventId);
    
    // for matching the incident particle information
    fGenSimTrkID.clear();

    // start the propagating of particles
    int simTrackNum = fSimDst -> GetSimTrackNum();
    for(int i=0; i<simTrackNum; i++){
        fSimTrack = fSimDst -> GetSimTrack(i);
        bool isSimPropagate = fSimTrack -> IsSimPropagate();
        if(!isSimPropagate){continue;}

        int pid = fSimTrack -> GetPid();

        double startVx = fSimTrack -> GetVxStart(); // [cm]
        double startVy = fSimTrack -> GetVyStart(); // [cm]
        double startVz = fSimTrack -> GetVzStart(); // [cm]

        double px = fSimTrack -> GetPx();
        double py = fSimTrack -> GetPy();
        double pz = fSimTrack -> GetPz();
        double p = sqrt(px*px + py*py + pz*pz);
        double e = fSimTrack -> GetE();
        
        double unitX = px/p;
        double unitY = py/p;
        double unitZ = pz/p;

        fParticle = fParTable -> FindParticle(pid);
        fParticleGun -> SetParticleDefinition(fParticle);
        fParticleGun -> SetParticleMomentumDirection(G4ThreeVector(unitX, unitY, unitZ));
        fParticleGun -> SetParticleEnergy(e *GeV);
        fParticleGun -> SetParticlePosition(G4ThreeVector(startVx *cm, startVy *cm, startVz *cm));
        fParticleGun -> GeneratePrimaryVertex(anEvent);
        fGenSimTrkID.push_back(fSimTrack->GetId());
    }
}

void RHICfPrimaryGeneratorAction::CreateSingleGen(G4Event* anEvent)
{
    fGenSimTrkID.clear();

    int pid = fSimOpt -> GetOptInt("PDG");
    double vx, vy, vz;
    double px, py, pz;
    double e;

    if(fSimOpt->CheckOpt("VertexX")){
        vx = fSimOpt -> GetOptDouble("VertexX");
        vy = fSimOpt -> GetOptDouble("VertexY");
        vz = fSimOpt -> GetOptDouble("VertexZ");
    }
    else if(fSimOpt->CheckOpt("VertexRandomDist")){
        if("GAUS" == fSimOpt->GetOptString("VertexRandomDist")){
            double meanXY = fSimOpt->GetOptDouble("VertexRandomXY1");
            double sigmaXY = fSimOpt->GetOptDouble("VertexRandomXY2");
            double meanZ = fSimOpt->GetOptDouble("VertexRandomZ1");
            double sigmaZ = fSimOpt->GetOptDouble("VertexRandomZ2");

            vx = fSimUtil -> GetRandomGaus(meanXY, sigmaXY);
            vy = fSimUtil -> GetRandomGaus(meanXY, sigmaXY);
            vz = fSimUtil -> GetRandomGaus(meanZ, sigmaZ);
        }
        else if("UNIFORM" == fSimOpt->GetOptString("VertexRandomDist")){
            double boundXY1 = fSimOpt->GetOptDouble("VertexRandomXY1");
            double boundXY2 = fSimOpt->GetOptDouble("VertexRandomXY2");
            double boundZ1 = fSimOpt->GetOptDouble("VertexRandomZ1");
            double boundZ2 = fSimOpt->GetOptDouble("VertexRandomZ2");

            vx = fSimUtil -> GetRandomUniform(boundXY1, boundXY2);
            vy = fSimUtil -> GetRandomUniform(boundXY1, boundXY2);
            vz = fSimUtil -> GetRandomUniform(boundZ1, boundZ2);
        }
    }
    else{
        vx = 0.;
        vy = 0.;
        vz = 0.;
    }

    if(fSimOpt->CheckOpt("Energy")){e = fSimOpt->GetOptDouble("Energy");}

    if(fSimOpt->CheckOpt("Direction")){
        TString dir = fSimOpt->GetOptString("Direction");
        dir.ToUpper();
        if(dir == "X"){
            px = 1.;
            py = 0.;
            pz = 0.;
        }
        else if(dir == "Y"){
            px = 0.;
            py = 1.;
            pz = 0.;
        }
        else{
            px = 0.;
            py = 0.;
            pz = 1.;
        }
    }
    else{
        px = 0.;
        py = 0.;
        pz = 1.;
    }

    fParticle = fParTable -> FindParticle(pid);
    double mass = fParticle -> GetPDGMass()*0.001; // [GeV/c^2]
    double p = sqrt(e*e -mass*mass);
    double p_x = px/p;
    double p_y = py/p;
    double p_z = pz/p;

    fSimDst -> Clear();
    fSimTrack = fSimDst -> GetSimTrack(fSimDst->GetSimTrackNum());
    fSimTrack -> SetIsPrimary();
    fSimTrack -> SetIsSimPropagate();
    fSimTrack -> SetIsFinal();
    fSimTrack -> SetId(0);
    fSimTrack -> SetParentId(0);
    fSimTrack -> SetPid(pid);
    fSimTrack -> SetEnergy(e);
    fSimTrack -> SetMomentum(p_x, p_y, p_z);
    fSimTrack -> SetVertexStart(vx, vy, vz);

    fParticleGun -> SetParticleDefinition(fParticle);
    fParticleGun -> SetParticleMomentumDirection(G4ThreeVector(px, py, pz));
    fParticleGun -> SetParticleEnergy(e *GeV);
    fParticleGun -> SetParticlePosition(G4ThreeVector(vx *cm, vy *cm, vz *cm));
    fParticleGun -> GeneratePrimaryVertex(anEvent);

    fGenSimTrkID.push_back(0);

    cout << "Generating particle [PDG:" <<pid << ", vertex(" <<  vx << " " << vy << " " << vz << "), direction(" << px << " " <<  py << " " << pz << "), E: " << e << " GeV]" << endl;
}

int RHICfPrimaryGeneratorAction::GetGenerateTrkNum()
{
    return fGenSimTrkID.size();
}
int RHICfPrimaryGeneratorAction::GetGenerateSimTrkID(int trackID)
{
    if(trackID > GetGenerateTrkNum() || trackID < 1){return -1;}
    return fGenSimTrkID[trackID-1];
}