#include "G4RunManager.hh"
#include "G4UImanager.hh"
#include "Randomize.hh"

#include "RHICfDetectorConstruction.hh"
#include "RHICfPhysicsList.hh"
#include "RHICfPrimaryGeneratorAction.hh"
#include "RHICfRunAction.hh"
#include "RHICfEventAction.hh"
#include "RHICfTrackingAction.hh"
#include "RHICfSteppingAction.hh"

#include "RHICfSimPar.hh"
#include "RHICfSimUtil.hh"

#ifdef G4VIS_USE
#include "G4VisExecutive.hh"
#endif

#define G4UI_USE_TCSH

#if defined(G4UI_USE_TCSH)
#include "G4UIterminal.hh"
#include "G4UItcsh.hh"
#else
#include "G4UIterminal.hh"
#endif

int main(int argc,char** argv)
{
    if(argc==1) {
        G4cout << "No input file!" << G4endl;
        return 1;
    }

    cout << "====================== RHICf+STAR simulation ======================" << endl;

    // ==================================== RHICfSimUtil  ===============================================
    RHICfSimUtil* simUtil = RHICfSimUtil::GetRHICfSimUtil(argc, argv);
    RHICfSimOptions* simOpt = simUtil -> GetOptions();

    /// Set seed for Geant4
    Long64_t geantSeed = 0;
    if(!simOpt->CheckOpt("SEED1")){geantSeed = simUtil -> GenSeed();}
    else{geantSeed = simOpt->GetOptInt("SEED1");}
    G4Random::setTheSeed(geantSeed);

    // Construct the default run manager
    G4RunManager* runManager = new G4RunManager;

    RHICfDetectorConstruction* detector = new RHICfDetectorConstruction();
    G4VUserPhysicsList* physics = new RHICfPhysicsList("QGSP_BERT");

    runManager->SetUserInitialization(detector);
    runManager->SetUserInitialization(physics);

    G4UserRunAction* runAction = new RHICfRunAction();
    RHICfPrimaryGeneratorAction* genAction = new RHICfPrimaryGeneratorAction();
    G4UserEventAction* eventAction = new RHICfEventAction();
    G4UserTrackingAction* trackAction = new RHICfTrackingAction();
    G4UserSteppingAction* stepAction = new RHICfSteppingAction();

    runManager->SetUserAction(runAction);
    runManager->SetUserAction(genAction);
    runManager->SetUserAction(eventAction);
    runManager->SetUserAction(trackAction);
    runManager->SetUserAction(stepAction);

    // Initialize G4 kernel
    runManager->Initialize();

    #ifdef G4VIS_USE
    // Initialize visualization
    G4VisManager* visManager=new G4VisExecutive;
    visManager->Initialize();
    #endif

    // Get the pointer to the User Interface manager
    G4UImanager* UI=G4UImanager::GetUIpointer();

    if(0) {
        // interactive mode : define visualization UI terminal
        G4UIsession* session = 0;
    #if defined(G4UI_USE_TCSH)
        session = new G4UIterminal(new G4UItcsh);
    #else
        session = new G4UIterminal();
    #endif
        session->SessionStart();
        delete session;
    }else{
        int eventNum = simOpt -> GetOptInt("EventNum");
        UI->ApplyCommand(Form("/run/beamOn %i", eventNum));
    }
    return 0;
}