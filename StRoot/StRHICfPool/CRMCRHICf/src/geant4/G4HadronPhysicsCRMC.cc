#include "G4HadronPhysicsCRMC.hh"

#include "G4ParticleDefinition.hh"
#include "G4ProcessManager.hh"
#include "G4ParticleTypes.hh"

#include "G4HadronInelasticProcess.hh"
#include "G4HadronElasticProcess.hh"
//#include "G4BuilderType.hh"
#include "G4HadronicProcessType.hh"
#include "G4HadronicProcess.hh"
#include "G4HadronElastic.hh"


#include "G4HadronicInelasticModelCRMC.hh"
//#include "G4TrivialHadronicModel.hh"
//#include "G4ComponentGGHadronNucleusXsc.hh"

#include "G4SystemOfUnits.hh"

#include "G4ComponentGGNuclNuclXsc.hh"
#include "G4CrossSectionInelastic.hh"


G4ThreadLocal G4VComponentCrossSection*  G4HadronPhysicsCRMC::theGGNuclNuclXS = 0;
G4ThreadLocal G4VCrossSectionDataSet*    G4HadronPhysicsCRMC::theNuclNuclData = 0;

static constexpr double THRESHOLD_ENERGY_GEV_DEFAULT=300.*GeV;
static constexpr double ELASTIC_MIN_ENERGY_GEV = 100.*TeV;

static constexpr double HIGH_ENERGY_MODEL_CRITERION = 100.*GeV;

const std::array<std::string, 13> G4HadronPhysicsCRMC::fModelNames = { "EPOS-LHC", "EPOS-1.99", "QGSJET-01", "", "", "",
    "SIBYLL-2.3D", "QGSJETII-04", "", "", "", "QGSJETII-03", "DPMJET-3.06", "QGSJETIII" };

G4HadronPhysicsCRMC::G4HadronPhysicsCRMC(int model):G4VHadronPhysics("HadronIonInelasticCrmc"),
	fEthr(THRESHOLD_ENERGY_GEV_DEFAULT),
	fEthr_2(THRESHOLD_ENERGY_GEV_DEFAULT),
	fModel(model),
	fPrintDebug(false),
	//fTrivial(nullptr),
	fCRMC(nullptr)
{}

G4HadronPhysicsCRMC::~G4HadronPhysicsCRMC()
{
	delete theGGNuclNuclXS; theGGNuclNuclXS =0;
}


void G4HadronPhysicsCRMC::ConstructProcess()
{
	fCRMC = new G4HadronicInelasticModelCRMC(fModel, fModelNames[fModel]);

	std::cout<<"fEthr = "<< fEthr/GeV <<" GeV" << std::endl;
	std::cout<<"fEthr_2 = "<< fEthr_2/GeV <<" GeV" << std::endl;

	fCRMC->SetMinEnergy(fEthr);
	fCRMC->SetPrintDebug(fPrintDebug);

	// * Elastic interactions above 100 TeV
	//fTrivial = new G4HadronElastic();
	//fTrivial->SetMinEnergy(ELASTIC_MIN_ENERGY_GEV);
	//fTrivial->SetMaxEnergy(fCRMC->GetMaxEnergy()); // Use the same Max energy as in the inelastic model


	theNuclNuclData =
		new G4CrossSectionInelastic( theGGNuclNuclXS = new G4ComponentGGNuclNuclXsc() );

	AddAllHadrons();
	std::cout<<
		"CRMC GEANT4 Interface physics initializeed"
		<<std::endl;
}

void G4HadronPhysicsCRMC::AddAllHadrons(){
	// Mesons
	AddProcess("gPionPlus",G4PionPlus::PionPlus());
	AddProcess("gPionMinus",G4PionMinus::PionMinus());
	AddProcess("gPionZero",G4PionZero::PionZero());
	AddProcess("gEta",G4Eta::Eta());
	AddProcess("gEtaPrime",G4EtaPrime::EtaPrime());

	AddProcess("gKaonPlus",G4KaonPlus::KaonPlus());
	AddProcess("gKaonMinus",G4KaonMinus::KaonMinus());
	AddProcess("gKaonZero",G4KaonZero::KaonZero());
	AddProcess("gAntiKaonZero",G4AntiKaonZero::AntiKaonZero());
	AddProcess("gKaonZeroLong",G4KaonZeroLong::KaonZeroLong());
	AddProcess("gKaonZeroShort",G4KaonZeroShort::KaonZeroShort());

	AddProcess("gDMesonPlus",G4DMesonPlus::DMesonPlus());
	AddProcess("gDMesonMinus",G4DMesonMinus::DMesonMinus());
	AddProcess("gDMesonZero",G4DMesonZero::DMesonZero());
	AddProcess("gAntiDMesonZero",G4AntiDMesonZero::AntiDMesonZero());
	AddProcess("gDsMesonPlus",G4DsMesonPlus::DsMesonPlus());
	AddProcess("gDsMesonMinus",G4DsMesonMinus::DsMesonMinus());
	AddProcess("gJPsi",G4JPsi::JPsi());
	AddProcess("gEtac",G4Etac::Etac());

	AddProcess("gBMesonPlus",G4BMesonPlus::BMesonPlus());
	AddProcess("gBMesonMinus",G4BMesonMinus::BMesonMinus());
	AddProcess("gBMesonZero",G4BMesonZero::BMesonZero());
	AddProcess("gAntiBMesonZero",G4AntiBMesonZero::AntiBMesonZero());
	AddProcess("gBsMesonZero",G4BsMesonZero::BsMesonZero());
	AddProcess("gAntiBsMesonZero",G4AntiBsMesonZero::AntiBsMesonZero());
	AddProcess("gUpsilon",G4Upsilon::Upsilon());


	// Barions
	AddProcess("gProton",G4Proton::Proton());
	AddProcess("gAntiProton",G4AntiProton::AntiProton());
	AddProcess("gNeutron",G4Neutron::Neutron()); //,false);
	AddProcess("gAntiNeutron",G4AntiNeutron::AntiNeutron());

	AddProcess("gLambda",G4Lambda::Lambda());
	AddProcess("gSigmaPlus",G4SigmaPlus::SigmaPlus());
	AddProcess("gSigmaZero",G4SigmaZero::SigmaZero());
	AddProcess("gSigmaMinus",G4SigmaMinus::SigmaMinus());
	AddProcess("gXiMinus",G4XiMinus::XiMinus());
	AddProcess("gXiZero",G4XiZero::XiZero());
	AddProcess("gOmegaMinus",G4OmegaMinus::OmegaMinus());

	AddProcess("gAntiLambda",G4AntiLambda::AntiLambda());
	AddProcess("gAntiSigmaPlus",G4AntiSigmaPlus::AntiSigmaPlus());
	AddProcess("gAntiSigmaZero",G4AntiSigmaZero::AntiSigmaZero());
	AddProcess("gAntiSigmaMinus",G4AntiSigmaMinus::AntiSigmaMinus());
	AddProcess("gAntiXiMinus",G4AntiXiMinus::AntiXiMinus());
	AddProcess("gAntiXiZero",G4AntiXiZero::AntiXiZero());
	AddProcess("gAntiOmegaMinus",G4AntiOmegaMinus::AntiOmegaMinus());

	AddProcess("gLambdacPlus",G4LambdacPlus::LambdacPlus());
	AddProcess("gSigmacPlusPlus",G4SigmacPlusPlus::SigmacPlusPlus());
	AddProcess("gSigmacPlus",G4SigmacPlus::SigmacPlus());
	AddProcess("gSigmacZero",G4SigmacZero::SigmacZero());
	AddProcess("gXicPlus",G4XicPlus::XicPlus());
	AddProcess("gXicZero",G4XicZero::XicZero());
	AddProcess("gOmegacZero",G4OmegacZero::OmegacZero());

	AddProcess("gAntiLambdacPlus",G4AntiLambdacPlus::AntiLambdacPlus());
	AddProcess("gAntiSigmacPlusPlus",G4AntiSigmacPlusPlus::AntiSigmacPlusPlus());
	AddProcess("gAntiSigmacPlus",G4AntiSigmacPlus::AntiSigmacPlus());
	AddProcess("gAntiSigmacZero",G4AntiSigmacZero::AntiSigmacZero());
	AddProcess("gAntiXicPlus",G4AntiXicPlus::AntiXicPlus());
	AddProcess("gAntiXicZero",G4AntiXicZero::AntiXicZero());
	AddProcess("gAntiOmegacZero",G4AntiOmegacZero::AntiOmegacZero());

	AddProcess("gLambdab",G4Lambdab::Lambdab());
	AddProcess("gSigmabPlus",G4SigmabPlus::SigmabPlus());
	AddProcess("gSigmabZero",G4SigmabZero::SigmabZero());
	AddProcess("gSigmabMinus",G4SigmabMinus::SigmabMinus());
	AddProcess("gXibZero",G4XibZero::XibZero());
	AddProcess("gXibMinus",G4XibMinus::XibMinus());
	AddProcess("gOmegabMinus",G4OmegabMinus::OmegabMinus());

	AddProcess("gAntiLambdab",G4AntiLambdab::AntiLambdab());
	AddProcess("gAntiSigmabPlus",G4AntiSigmabPlus::AntiSigmabPlus());
	AddProcess("gAntiSigmabZero",G4AntiSigmabZero::AntiSigmabZero());
	AddProcess("gAntiSigmabMinus",G4AntiSigmabMinus::AntiSigmabMinus());
	AddProcess("gAntiXibZero",G4AntiXibZero::AntiXibZero());
	AddProcess("gAntiXibMinus",G4AntiXibMinus::AntiXibMinus());
	AddProcess("gAntiOmegabMinus",G4AntiOmegabMinus::AntiOmegabMinus());

	// Nuclei
	AddProcess("gAlpha",G4Alpha::Alpha());
	AddProcess("gDeuteron",G4Deuteron::Deuteron());
	AddProcess("gHe3",G4He3::He3());
	AddProcess("gTriton",G4Triton::Triton());

	AddProcess("gAntiAlpha",G4AntiAlpha::AntiAlpha());
	AddProcess("gAntiDeuteron",G4AntiDeuteron::AntiDeuteron());
	AddProcess("gAntiHe3",G4AntiHe3::AntiHe3());
	AddProcess("gAntiTriton",G4AntiTriton::AntiTriton());

	//ions
	AddProcess("gGenericIon",G4GenericIon::GenericIon());
}

void G4HadronPhysicsCRMC::AddProcess(const G4String& name, G4ParticleDefinition* part, G4bool doElastic)
{
	G4ProcessManager* pManager = part->GetProcessManager();


	// Inelastic
	int inelastic_process_i = -1;
	G4ProcessVector*  inelastic_pvec = part->GetProcessManager()->GetProcessList();
	for(size_t i=0; i<inelastic_pvec->size(); ++i) {
		if(G4HadronicProcessType::fHadronInelastic == ((*inelastic_pvec)[i])->GetProcessSubType())
			inelastic_process_i = i;
	}
	if(inelastic_process_i >= 0 ){
		G4HadronicProcess* hadi = static_cast<G4HadronicProcess*>((*inelastic_pvec)[inelastic_process_i]);

		std::vector<G4HadronicInteraction*>& process_list = hadi->GetHadronicInteractionList(); // GetManagerPointer()->GetHadronicInteractionList();

		for(std::vector<G4HadronicInteraction*>::const_iterator it=process_list.begin(); it!=process_list.end(); it++)
		{
			if( (*it)->GetMaxEnergy() > HIGH_ENERGY_MODEL_CRITERION)
				(*it)->SetMaxEnergy(fEthr_2);
			else if((*it)->GetMinEnergy()<fEthr)
			{
				if((*it)->GetMaxEnergy()>fEthr)
					(*it)->SetMaxEnergy(fEthr);
			}
		}
		//hadi->AddDataSet(theNuclNuclData);
		hadi->RegisterMe(fCRMC);
	}
	else{
		std::cout<<"[G4HadronPhysicsCRMC] INFO No existing hadronic inelastic process found for the particle: " << name <<  " skippin it." << std::endl;
	}


	// Elastic
	if(doElastic){
		int elastic_process_i = -1;
		G4ProcessVector*  pvec = part->GetProcessManager()->GetProcessList();
		for(size_t i=0; i<pvec->size(); ++i) {
			if(G4HadronicProcessType::fHadronElastic == 	((*pvec)[i])->GetProcessSubType())
				elastic_process_i = i;
		}
		if(elastic_process_i >= 0 ){
			G4HadronicProcess* hade = static_cast<G4HadronicProcess*>((*pvec)[elastic_process_i]);
			//hade->RegisterMe(fTrivial);  
			// Instead of registring a "trivial" elastic model, find the most energetic exising 
			// elastic process and extend its range to the same range as the inelastic model
			std::vector<G4HadronicInteraction*>& process_list = hade->GetHadronicInteractionList(); 
			for(std::vector<G4HadronicInteraction*>::const_iterator it=process_list.begin(); it!=process_list.end(); it++){
				if((*it)->GetMaxEnergy() >= ELASTIC_MIN_ENERGY_GEV * 0.99) (*it)->SetMaxEnergy( fCRMC->GetMaxEnergy() );
			}
		}
		else{
			std::cout<<"[G4HadronPhysicsCRMC] INFO No existing hadronic elastic process found for the particle: " << name <<  " skippin it." << std::endl;
		}
	}
}

//! Energy threshold (in internal Geant4 units) to switch between
//! Geant4 and CRMC physics
void G4HadronPhysicsCRMC::SetEnergyThreshold(double thr)
{
	fEthr = thr;
	fEthr_2 = thr;
}

//! Energy limits of Geant4-CRMC transition region
void G4HadronPhysicsCRMC::SetEnergyThreshold(double E1, double E2)
{
	if(E2>E1)
	{
		fEthr = E1;
		fEthr_2 = E2;
	}
	else
	{
		fEthr = E2;
		fEthr_2 = E1;
	}
}
