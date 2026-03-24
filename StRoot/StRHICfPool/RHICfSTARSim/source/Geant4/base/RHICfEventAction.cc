#include "RHICfEventAction.hh"

RHICfEventAction::RHICfEventAction()
{
    fSimUtil = RHICfSimUtil::GetRHICfSimUtil();
    fSimOpt = fSimUtil -> GetOptions();
}

RHICfEventAction::~RHICfEventAction()
{
}

void RHICfEventAction::BeginOfEventAction(const G4Event* evt)
{
    G4cout << "RHICfEventAction::BeginOfEventAction() -- Event: " << evt->GetEventID() << " progressing..." << G4endl;
}

void RHICfEventAction::EndOfEventAction(const G4Event* evt)
{
    fHitCollThisEvent = evt->GetHCofThisEvent();
    if(!fHitCollThisEvent) return;

    fRunManager = G4RunManager::GetRunManager();
    fSDManager = G4SDManager::GetSDMpointer();

    fGenAction = (RHICfPrimaryGeneratorAction*)fRunManager->GetUserPrimaryGeneratorAction();

    // Assign the StRHICfSimEvent
    fSimEvent = fSimDst -> GetSimEvent();

    if(fSimUtil -> IsSingleGenMode()){
        fSimEvent -> SetEventNumber(evt->GetEventID()+1);
        fSimEvent -> SetGenFinalParNum(1);
        fSimEvent -> SetPrimaryTrkNum(1);

        TString runTypeName = fSimOpt->GetOptString("RUNTYPE");
        int runType = -1;
        if(runTypeName =="TL"){runType = rTLtype;}
        if(runTypeName =="TS"){runType = rTStype;}
        if(runTypeName =="TOP"){runType = rTOPtype;}
        fSimEvent -> SetRHICfRunType(runType);
    }

    // ================ RHICf Sensitive detectors ================
    fSimRHICfHit = fSimDst -> GetSimRHICfHit();

    // RHICf Forward Counter 
    G4int idRHICfFC = fSDManager->GetCollectionID("FC");
    fRHICfFCHitColl = (FCHitsCollection*)fHitCollThisEvent->GetHC(idRHICfFC);
    for(unsigned int i=0; i<fRHICfFCHitColl->GetSize(); i++){
        int towerIdx = (*fRHICfFCHitColl)[i]->GetTower();
        double edep = (*fRHICfFCHitColl)[i]->GetEdep()/1000.; // [GeV]

        fSimRHICfHit -> SetFCdE(towerIdx, edep);
    }

    // RHICf GSO Plate
    G4int idRHICfGSOPlate = fSDManager->GetCollectionID("GSOplate");
    fRHICfGSOPlateHitColl = (GSOplateHitsCollection*)fHitCollThisEvent->GetHC(idRHICfGSOPlate);
    for(unsigned int i=0; i<fRHICfGSOPlateHitColl->GetSize(); i++){
        int towerIdx = (*fRHICfGSOPlateHitColl)[i]->GetTower();
        int plateIdx = (*fRHICfGSOPlateHitColl)[i]->GetPlate();
        double edep = (*fRHICfGSOPlateHitColl)[i]->GetEdep()/1000.; // [GeV]

        fSimRHICfHit -> SetPlatedE(towerIdx, plateIdx, edep);
    }

    // RHICf GSO Bar
    G4int idRHICfGSOBar=fSDManager->GetCollectionID("GSObar");
    fRHICfGSOBarHitColl = (GSObarHitsCollection*)fHitCollThisEvent->GetHC(idRHICfGSOBar);
    for(unsigned int i=0; i<fRHICfGSOBarHitColl->GetSize(); i++){
        int towerIdx = (*fRHICfGSOBarHitColl)[i]->GetTower();
        int layerIdx = (*fRHICfGSOBarHitColl)[i]->GetBelt();
        int xyIdx = (*fRHICfGSOBarHitColl)[i]->GetXY();
        int barIdx = (*fRHICfGSOBarHitColl)[i]->GetBar();
        double edep = (*fRHICfGSOBarHitColl)[i]->GetEdep()/1000.; // [GeV]

        fSimRHICfHit -> SetGSOBardE(towerIdx, layerIdx, xyIdx, barIdx, edep);
    }

    // ================ ZDC Sensitive detectors ================
    fSimZDC = fSimDst -> GetSimZDC();

    // ZDC Modules (PMT)
    G4int idZDCModudle=fSDManager->GetCollectionID("ZDC");
    fZDCPMTHitColl = (ZDCHitsCollection*)fHitCollThisEvent->GetHC(idZDCModudle);
    for(unsigned int i=0; i<fZDCPMTHitColl->GetSize(); i++) {
        int moduleIdx = (*fZDCPMTHitColl)[i]->GetModule();
        int photonNum = (*fZDCPMTHitColl)[i]->GetNphoton();
        double edep = (*fZDCPMTHitColl)[i]->GetEdep()/1000.; // [GeV]

        fSimZDC -> SetPmtPhotonNum(moduleIdx, photonNum);
        fSimZDC -> SetPmtdE(moduleIdx, edep);
    }

    // ZDC SMD
    G4int idZDCSMD=fSDManager->GetCollectionID("SMD");
    fZDCSMDHitColl = (SMDHitsCollection*)fHitCollThisEvent->GetHC(idZDCSMD);
    for(unsigned int i=0; i<fZDCSMDHitColl->GetSize(); i++) {
        int xyIdx = (*fZDCSMDHitColl)[i]->GetXY();
        int smdIdx = (*fZDCSMDHitColl)[i]->GetSMD();
        double edep = (*fZDCSMDHitColl)[i]->GetEdep()/1000.; // [GeV]

        fSimZDC -> SetSMDdE(xyIdx, smdIdx, edep);
    }

    // Make the RHICf trigger 
    bool isShowerTrigger = IsShowerTrigger();
    bool isType1Pi0Trigger = IsType1Pi0Trigger();
    bool isHighEMTrigger = IsHighEMTrigger();

    if(!isShowerTrigger && !isType1Pi0Trigger && !isHighEMTrigger){return;} // Not triggered
    if(isShowerTrigger){fSimEvent -> SetIsShowerTrigger();}
    if(isType1Pi0Trigger){fSimEvent -> SetIsType1Pi0Trigger();}
    if(isHighEMTrigger){fSimEvent -> SetIsHighEMTrigger();}

    // Fill the Output SimDst Tree
    fOutputTree -> Fill();
    // Print the event information
    EventPrint();
}

bool RHICfEventAction::IsShowerTrigger()
{
    const double dEThreshold = 0.045; // [45 MeV]

    for(int it=0; it<rTowerNum; it++){
        int hitNum = 0;
        for(int ip=0; ip<rPlateNum; ip++){
            double de = fSimRHICfHit -> GetPlatedE(it, ip); // [GeV]
            if(dEThreshold < de){hitNum++;}
            else{hitNum--;}
            if(hitNum >= 3){
                return true;
            }
        }
    }
    return false;
}

bool RHICfEventAction::IsType1Pi0Trigger()
{
    const double dEThreshold = 0.045; // [45 MeV]

    int fireTower = 0;
    for(int it=0; it<rTowerNum; it++){
        int hitNum = 0;
        for(int ip=0; ip<7; ip++){ // up to 6th layer 
            double de = fSimRHICfHit -> GetPlatedE(it, ip); // [GeV]
            if(dEThreshold < de){
                hitNum++;
            }
            else{
                hitNum--;
            }
            if(hitNum >= 3){
                fireTower++;
                break;
            }
        }
    }
    if(fireTower == 2){return true;}
    return false;
}

bool RHICfEventAction::IsHighEMTrigger()
{
    const double highEMThreshold = 0.5; // [500 MeV]

    for(int it=0; it<rTowerNum; it++){
        double de = fSimRHICfHit -> GetPlatedE(it, 4); // [GeV]
        if(de > highEMThreshold){return true;}
    }
    return false;
}

void RHICfEventAction::EventPrint()
{
    cout << "=======================================================================================" << endl;
    cout << "RHICfEventAction::EventPrint() -- Event: " << fSimEvent -> GetEventNumber() -1 << endl;
    TString procName = fSimUtil -> GetProcessName(fSimEvent -> GetProcessId());
    cout << "    StRHICfSimEvent -- Process: " << procName << endl;

    cout << "    StRHICfSimTrack -- Primary Trk Num: " << fSimEvent -> GetPrimaryTrkNum() << ", ";
    cout << "Propagated Trk Num: " << fGenAction -> GetGenerateTrkNum() << endl;

    cout << "    StRHICfSimEvent -- RHICfTrigger: ";
    if(fSimEvent -> IsShowerTrigger()){cout << " Shower, ";}
    if(fSimEvent -> IsType1Pi0Trigger()){cout << " Type1 Pi0, ";}
    if(fSimEvent -> IsHighEMTrigger()){cout << " High-EM, ";}
    cout << endl;

    double eSumTS = 0.;
    for(int i=0; i<fSimRHICfHit -> GetSimTrkNum(0); i++){
        eSumTS += fSimRHICfHit->GetSimTrkIncidentEnergy(0,i);
    }
    double eSumTL = 0.;
    for(int i=0; i<fSimRHICfHit -> GetSimTrkNum(1); i++){
        eSumTL += fSimRHICfHit->GetSimTrkIncidentEnergy(1,i);
    }

    cout << "    RHICf incident track num (Sum Energy) -- in TS: " << fSimRHICfHit -> GetSimTrkNum(0) << " (" << eSumTS << " GeV), ";
    cout << "TL: " << fSimRHICfHit -> GetSimTrkNum(1) << " (" << eSumTL << " GeV)" << endl;

    double eSumZDC = 0.;
    for(int i=0; i<fSimZDC -> GetSimTrkNum(); i++){
        eSumZDC += fSimZDC->GetSimTrkIncidentEnergy(i);
    }
    cout << "    ZDC incident track num (Sum Energy): " << fSimZDC -> GetSimTrkNum() << " (" << eSumZDC << " GeV)" << endl;

    cout << "=======================================================================================" << endl;
}
