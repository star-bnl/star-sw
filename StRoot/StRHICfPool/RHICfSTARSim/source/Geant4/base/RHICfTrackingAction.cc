#include "RHICfTrackingAction.hh"

#include "RHICfTrackInfo.hh"
#include "G4ParticleDefinition.hh"

RHICfTrackingAction::RHICfTrackingAction()
{
}

RHICfTrackingAction::~RHICfTrackingAction()
{
}

void RHICfTrackingAction::PreUserTrackingAction(const G4Track* track)
{
    // Set the primary particle's ID into Track information
    if (track->GetParentID()==0){
        RHICfTrackInfo* info = new RHICfTrackInfo(track->GetTrackID());
        const_cast<G4Track*>(track)->SetUserInformation(info);
    }
}

void RHICfTrackingAction::PostUserTrackingAction(const G4Track* track)
{
    int parentID = track -> GetParentID();
    int trackID = track -> GetTrackID();

    // Set the end vertex for generated tracks
    if(parentID == 0){
        fRunManager = G4RunManager::GetRunManager();
        fGenAction = (RHICfPrimaryGeneratorAction*)fRunManager->GetUserPrimaryGeneratorAction();
        int genTrkNum = fGenAction -> GetGenerateTrkNum();
        if(genTrkNum == 0){return;}

        int simTrkId = fGenAction->GetGenerateSimTrkID(trackID);
        fSimTrack = fSimDst->GetSimTrack(simTrkId);

        double vxEnd = track -> GetPosition().x()*0.1; // [cm]
        double vyEnd = track -> GetPosition().y()*0.1; // [cm]
        double vzEnd = track -> GetPosition().z()*0.1; // [cm]
        fSimTrack -> SetVertexEnd(vxEnd, vyEnd, vzEnd);
    }

    fSDManager = G4SDManager::GetSDMpointer();
    fRHICfTruthCounterSD = (RHICfTruthCounterSD*)fSDManager->FindSensitiveDetector("TruthCounter");
    fZDCTruthCounterSD = (RHICfZDCTruthCounterSD*)fSDManager->FindSensitiveDetector("ZDCTruthCounter");

    int RHICfIncidentTrackID = fRHICfTruthCounterSD -> GetIncidentTrackID();
    int ZDCIncidentTrackID = fZDCTruthCounterSD -> GetIncidentTrackID();

    if(RHICfIncidentTrackID > 0 || ZDCIncidentTrackID > 0){
        fRunManager = G4RunManager::GetRunManager();
        fGenAction = (RHICfPrimaryGeneratorAction*)fRunManager->GetUserPrimaryGeneratorAction();

        if(parentID == 0){
            int simTrkId = fGenAction->GetGenerateSimTrkID(trackID);
            if(RHICfIncidentTrackID > 0){
                int towerIdx = fRHICfTruthCounterSD -> GetIncidentTowerIdx();
                double incidentPosX = fRHICfTruthCounterSD -> GetIncidentPosX();
                double incidentPosY = fRHICfTruthCounterSD -> GetIncidentPosY();
                double incidentEnergy = fRHICfTruthCounterSD -> GetIncidentEnergy();
                fSimRHICfHit = fSimDst -> GetSimRHICfHit();
                fSimRHICfHit -> SetSimTrkId(towerIdx, simTrkId, incidentPosX, incidentPosY, incidentEnergy);
            }
            if(ZDCIncidentTrackID > 0){
                double incidentPosX = fZDCTruthCounterSD -> GetIncidentPosX();
                double incidentPosY = fZDCTruthCounterSD -> GetIncidentPosY();
                double incidentEnergy = fZDCTruthCounterSD -> GetIncidentEnergy();
                fSimZDC = fSimDst -> GetSimZDC();
                fSimZDC -> SetSimTrkId(simTrkId, incidentPosX, incidentPosY, incidentEnergy);
            }
        }
        else{
            const G4ParticleDefinition* particle = track -> GetParticleDefinition();
            int pid = particle -> GetPDGEncoding();

            G4ThreeVector vertex = track -> GetVertexPosition();
            double vxStart = vertex.x()*0.1; // [cm]
            double vyStart = vertex.y()*0.1; // [cm]
            double vzStart = vertex.z()*0.1; // [cm]
            double vxEnd = track -> GetPosition().x()*0.1; // [cm]
            double vyEnd = track -> GetPosition().y()*0.1; // [cm]
            double vzEnd = track -> GetPosition().z()*0.1; // [cm]    
            
            double energy = track -> GetVertexKineticEnergy()*0.001; // [GeV]
            double mass = particle -> GetPDGMass()*0.001; // [GeV/c^2]
            G4ThreeVector direction = track -> GetVertexMomentumDirection();
            
            double p = sqrt(energy*energy - mass*mass);
            double px = p*direction.x();
            double py = p*direction.y();
            double pz = p*direction.z();

            int simTrkId = fGenAction->GetGenerateSimTrkID(trackID);
            if(simTrkId == -1){
                int primaryTrackID = dynamic_cast<RHICfTrackInfo*>(track->GetUserInformation())->GetPrimaryID();
                int primarySimTrkID = fGenAction->GetGenerateSimTrkID(primaryTrackID);
                int simTrackNum = fSimDst->GetSimTrackNum();
                fSimTrack = fSimDst->GetSimTrack(simTrackNum);
                fSimTrack -> Clear();
                fSimTrack -> SetId(simTrackNum);
                fSimTrack -> SetParentId(primarySimTrkID);
                fSimTrack -> SetPid(pid);
                fSimTrack -> SetEnergy(energy);
                fSimTrack -> SetMomentum(px, py, pz);
                fSimTrack -> SetVertexStart(vxStart, vyStart, vzStart);
                fSimTrack -> SetVertexEnd(vxEnd, vyEnd, vzEnd);
                fSimTrack -> SetIsFinal();

                if(RHICfIncidentTrackID > 0){
                    fSimTrack -> SetIsRHICfHit();
                    int towerIdx = fRHICfTruthCounterSD -> GetIncidentTowerIdx();
                    double incidentPosX = fRHICfTruthCounterSD -> GetIncidentPosX();
                    double incidentPosY = fRHICfTruthCounterSD -> GetIncidentPosY();
                    double incidentEnergy = fRHICfTruthCounterSD -> GetIncidentEnergy();
                    fSimRHICfHit = fSimDst -> GetSimRHICfHit();
                    fSimRHICfHit -> SetSimTrkId(towerIdx, simTrackNum, incidentPosX, incidentPosY, incidentEnergy);
                }
                if(ZDCIncidentTrackID > 0){
                    double incidentPosX = fZDCTruthCounterSD -> GetIncidentPosX();
                    double incidentPosY = fZDCTruthCounterSD -> GetIncidentPosY();
                    double incidentEnergy = fZDCTruthCounterSD -> GetIncidentEnergy();
                    fSimZDC = fSimDst -> GetSimZDC();
                    fSimZDC -> SetSimTrkId(simTrackNum, incidentPosX, incidentPosY, incidentEnergy);
                }
            }
        }
    }
}
