#include "G4Run.hh"
#include "G4RunManager.hh"

#include <iostream>
#include <iomanip>
#include <cstdlib>
#include <unistd.h>

#include "RHICfRunAction.hh"
#include "RHICfPrimaryGeneratorAction.hh"
#include "RHICfEventAction.hh"
#include "RHICfTrackingAction.hh"

RHICfRunAction::RHICfRunAction():G4UserRunAction()
{
}

RHICfRunAction::~RHICfRunAction()
{
}

void RHICfRunAction::BeginOfRunAction(const G4Run* aRun)
{  
	fSimUtil = RHICfSimUtil::GetRHICfSimUtil();
	fSimOpt = fSimUtil -> GetOptions();

	G4RunManager* runManager = G4RunManager::GetRunManager();
	RHICfPrimaryGeneratorAction* genAction = (RHICfPrimaryGeneratorAction*)runManager->GetUserPrimaryGeneratorAction();
	RHICfEventAction* eventAction = (RHICfEventAction*)runManager->GetUserEventAction();
	RHICfTrackingAction* trackingAction = (RHICfTrackingAction*)runManager->GetUserTrackingAction();

	TString inputFile = "";
	if(fSimOpt->CheckOpt("AUTOINPUT")){inputFile = fSimOpt -> GetOptString("AUTOINPUT");}
	else{inputFile = fSimOpt -> GetOptString("INPUT");}
	
	if(fSimUtil->IsStarSimMode()){
		fInSimDstFile = new TFile(inputFile, "READ");
		if(!fInSimDstFile){
			cout << "RHICfRunAction::BeginOfRunAction() -- Error: " << inputFile << " can not opend! "<< endl;
			return;
		}
		cout << "RHICfRunAction::BeginOfRunAction() -- STARSim Mode has been detected.." << endl;

		fInSimDstTree = (TTree*)fInSimDstFile -> Get("StRHICfSimDst");
		fSimDst = new StRHICfSimDst();
		fSimDst -> ReadDstArray(fInSimDstTree);

		TString outputFileName = fSimOpt -> GetOptString("OUTPUT");
		fOutSimDstFile = new TFile(outputFileName, "RECREATE");
        fOutSimDstTree = fInSimDstTree -> CloneTree(0);

		genAction -> SetInputSimDstTree(fInSimDstTree);
		genAction -> SetSimDst(fSimDst);

		eventAction -> SetOutputSimDstTree(fOutSimDstTree);
		eventAction -> SetSimDst(fSimDst);

		trackingAction -> SetSimDst(fSimDst);
	}
	else if(fSimUtil->IsSingleGenMode()){
		TString outputFileName = "";
		if(fSimOpt -> CheckOpt("OUTPUTNAME")){outputFileName = fSimOpt -> GetOptString("OUTPUTNAME");}
		TString outputFilePath = fSimOpt -> GetOptString("OUTPATH");
		if(outputFilePath[outputFilePath.Sizeof()-1] != '/'){outputFilePath = outputFilePath + "/";}
		TString runTypeName = fSimOpt -> GetOptString("RUNTYPE");

		TString outputName = outputFilePath + "SingleGen_"+ runTypeName + "_"  + outputFileName + ".rhicfsim.RHICfSimDst.root";

		fOutSimDstFile = new TFile(outputName, "RECREATE");
		fOutSimDstTree = new TTree("StRHICfSimDst", "StRHICfSimDst");

		fSimDst = new StRHICfSimDst();
		fSimDst -> CreateDstArray(fOutSimDstTree);
		
		genAction -> SetSimDst(fSimDst);
		eventAction -> SetOutputSimDstTree(fOutSimDstTree);
		eventAction -> SetSimDst(fSimDst);
		trackingAction -> SetSimDst(fSimDst);
	}
	else{
		// To be updated for HepMC mode...
	}

	G4cout << "RHICfRunAction::BeginOfRunAction() -- Estimated number of event: " << fSimOpt -> GetOptInt("eventNum") << " start." << G4endl;
}

void RHICfRunAction::EndOfRunAction(const G4Run*)
{
	fOutSimDstFile -> cd();
	fOutSimDstTree -> Write();

    cout << "RHICfRunAction::EndOfRunAction() -- Number of event: " << fOutSimDstTree->GetEntries() << " done." << endl;
}