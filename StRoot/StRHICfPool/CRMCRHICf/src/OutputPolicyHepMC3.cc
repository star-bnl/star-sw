#include <OutputPolicyHepMC3.h>

#include <CRMCoptions.h>
#include <CRMCinterface.h>
#include <CRMCconfig.h>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <stdexcept>

//--------------------------------------------------------------------
void OutputPolicyHepMC3::InitOutput(const CRMCoptions& cfg)
{
    // ================= RHICf+STAR simulation generator Initialization ===================
    _hepmc3.init(cfg);

    TString outputPath = getenv("PWD");

    TString rhicfRunTypeName = cfg.GetRHICfRunType();
    fRHICfRunType = -1;
    rhicfRunTypeName.ToUpper();
    if(rhicfRunTypeName.Index("TL") != -1){fRHICfRunType = RunType::TL;}
    else if(rhicfRunTypeName.Index("TS") != -1){fRHICfRunType = RunType::TS;}
    else if(rhicfRunTypeName.Index("TOP") != -1){fRHICfRunType = RunType::TOP;}
    else if(rhicfRunTypeName.Index("ALL") != -1){fRHICfRunType = RunType::ALL;} // No RHICf acceptance cut mode
    else{throw std::runtime_error("!!! wrong RHICf Run Type");}

    int modelType = cfg.GetHEModel();
    fModelIdx = -1;
    TString modelName = "";
    switch (modelType){
        case 13:
            modelName = "QGSJETIII01"; 
            fModelIdx = 1;
            break;

        case 6:
            modelName = "SIBYLL"; 
            fModelIdx = 2;
            break;

        case 0:
            modelName = "EPOSLHCR"; 
            fModelIdx = 3;
            break;

        case 1:
            modelName = "EPOSLHCR_FAST"; 
            fModelIdx = 4;
            break;

        case 7:
            modelName = "QGSJETII04"; 
            fModelIdx = 5;
            break;

        default:
            cerr << " No support model for RHICf simulation, terminate.." << endl;
            exit(1);
            break;
    }

    TString jobTime = "";
    time_t timer;
    timer = time(NULL); 
    struct tm* t = localtime(&timer); 
    int date = (t->tm_year -100)*10000 + (t->tm_mon+1)*100 + t->tm_mday;
    int time = t->tm_hour*10000 + t->tm_min*100 + t->tm_sec;
    jobTime = Form("%i%i", date, time);
    
    TString jobIndex = cfg.GetJobIndex();
    if(jobIndex != ""){jobIndex = "_" + jobIndex;}

    TString outputName = outputPath +"/crmc_"+ modelName +"_"+ rhicfRunTypeName +"_"+ jobTime + jobIndex +".RHICfSimGenerator.root";
    
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

    fRandom = new TRandom3(cfg.GetSeed());
    InitVertexFluctuation();
    if(fRHICfRunType != ALL){InitRHICfGeometry();}

    cout << "--- RHICfSimGenerator Initialization ---" << endl;
    cout << "Model          : " << modelName << endl;
    cout << "RHICf Run Type : " << rhicfRunTypeName << endl;
    cout << "Output File    : " << outputName << endl;
    cout << "Initialization --- done..." << endl;
}

//--------------------------------------------------------------------
void OutputPolicyHepMC3::FillRHICfEvent(const CRMCoptions& cfg, const int nEvent, int& passEventNum)
{
    if (!_hepevt.convert(_event)){throw std::runtime_error("!!!Could not read next event");}
    if (!cfg.IsTest()){_hepmc3.fillInEvent(cfg, nEvent, _event);}
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

        if(fRHICfRunType == RunType::ALL){continue;}
        if(stat != 1){continue;} // only final state

        pid = abs(pid);
        if(11 < pid && pid < 19 ){continue;} // cut the lepton (except electron)
        if(e < 1.){continue;} // energy cut 1 GeV
        if(pz <= 0.){continue;} // opposite direction cut

        bool isInterest = IsInterestedParticle(pid);
        
        // cut the final state charged particle generated Z-position before end of DX magnet
        if(!isInterest && vz < 15000.){continue;}
        int hit = GetRHICfGeoHit(vx, vy, vz, px, py, pz, e);
        if(hit < 0){continue;}

        RHICfHitTrkNum++;
    }

    if(fRHICfRunType != RunType::ALL){
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
void OutputPolicyHepMC3::CloseOutput(const CRMCoptions&)
{
    fFile -> cd();
    fRunTree -> Write();
    fEventTree -> Write();
    fFile -> Close();
    cout << "OutputPolicyHepMC3::CloseOutput() --- Output file has been written" << endl;
}

void OutputPolicyHepMC3::PrintEvent()
{
    cout << "--- CRMC RHICfSimGenerator::PrintEvent() --- " << endl;
    cout << " Generated Event Number: " << fGenEventNum << endl;
    cout << " Passed Event Number   : " << fEventTree -> GetEntries() << endl;
    cout << " Event Process Id      : " << fProcessID  << endl;
    cout << " Total Particle Number : " << fParticleArray -> GetEntries() << endl;
}

void OutputPolicyHepMC3::InitVertexFluctuation()
{
    fVertexMean[0] = 0.; // x
    fVertexMean[1] = 0.; // y
    fVertexMean[2] = 0.; // z
    if(fRHICfRunType == RunType::TL){
        fVertexMean[0] = 0.044 * 10.; // [mm]
        fVertexMean[1] = 0.186 * 10.; // [mm]
    }
    else if(fRHICfRunType == RunType::TS){
        fVertexMean[0] = 0.022 * 10.; // [mm]
        fVertexMean[1] = 0.19 * 10.; // [mm]
    }
    else if(fRHICfRunType == RunType::TOP){
        fVertexMean[0] = 0.022 * 10.; // [mm]
        fVertexMean[1] = -0.053 * 10.; // [mm]
    }
    fVertexSigma[0] = 0.2; // [mm]
    fVertexSigma[1] = 0.2; // [mm]
    fVertexSigma[2] = 300.; // [mm]
}

void OutputPolicyHepMC3::InitRHICfGeometry()
{
    double distTowersCenter = 47.4; // [mm], Distance between the center of small and large tower
    double towerPosYByRun = 0.; // [mm], RHICf detector y-axis shift position by run types with respect to global coordinate
    if(fRHICfRunType == RunType::TL){towerPosYByRun = -47.4;}
    if(fRHICfRunType == RunType::TS){towerPosYByRun = 0.;}
    if(fRHICfRunType == RunType::TOP){towerPosYByRun = 21.6;}

    fRHICfPoly = new TH2Poly();
    fRHICfPoly -> SetName("RHICfPoly");
    fRHICfPoly -> SetStats(0);

    double x[4], y[4];
    for(int tower=0; tower<2; tower++){
        double towerGlobalPosY = towerPosYByRun;
        if(tower == RHICfTower::LargeTower){towerGlobalPosY += distTowersCenter;}

        for(int boundary=0; boundary<4; boundary++){
            x[boundary] = GetRHICfDetectorBoundary(tower, 0, boundary);
            y[boundary] = towerGlobalPosY + GetRHICfDetectorBoundary(tower, 1, boundary);
        }
        fRHICfPoly -> AddBin(4, x, y);
    }
    if(!fRHICfPoly){throw std::runtime_error("RHICf geometry is not initialized");}
}

double OutputPolicyHepMC3::GetRHICfDetectorBoundary(int towerIdx, int xyIdx, int boundaryIdx)
{
  const double SQRT2 = sqrt(2);
  double detectorSize = 0.;
  if(towerIdx == RHICfTower::SmallTower){detectorSize = 20.;} // [mm]
  else if(towerIdx == RHICfTower::LargeTower){detectorSize = 40.;} // [mm]
  else{return -999.;}

  if(boundaryIdx < 0 || boundaryIdx > 3){return -999.;}
  bool isEvenNumber = (boundaryIdx%2 == 0)? true : false;

  double valueSign = (boundaryIdx < 2)? 1. : -1.;
  double detectorBoundaryPoint = valueSign * SQRT2 * detectorSize/2.;

  if(xyIdx == 0){ // x-axis
    if(isEvenNumber){ return detectorBoundaryPoint; }// even number
    else{ return 0.; }// odd number
  }
  else if(xyIdx == 1){ // y-axis
    if(isEvenNumber){ return 0.; } // even number
    else{ return detectorBoundaryPoint; }// odd number
  }

  return -999.;
}

bool OutputPolicyHepMC3::IsInterestedParticle(int pid)
{
    // only listed for final state particles
    int pdg = abs(pid);
    switch(pdg)
    {
        case 2112: return true; // n
        case 130 : return true; // K0_L
        case 22  : return true; // gamma
        
        default  : return false; // other charged and uninterested particles
    }
    return false;
}

int OutputPolicyHepMC3::GetRHICfGeoHit(double posX, double posY, double posZ, double px, double py, double pz, double e)
{
    double momMag = sqrt(px*px + py*py + pz*pz);
    double unitVecX = px/momMag;
    double unitVecY = py/momMag;
    double unitVecZ = pz/momMag;

    double detPosZ = 17800.; // [mm]
    double z = detPosZ - posZ;
    if(z < 0.){return -1;} // create z-position cut

    double x = z * (unitVecX/unitVecZ) + posX;
    double y = z * (unitVecY/unitVecZ) + posY;

    int type = fRHICfPoly -> FindBin(x, y);
    if(type < 1 || type > 2){return -1;} // RHICf geometrical hit cut

    return type;
} 
