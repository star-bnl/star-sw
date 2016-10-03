// Program to create ROOT files for EvtGen validation plots,
// generalising storage of daughter information for each decay chain step.
// There are three integers to keep track of the decay tree history:
//   dVtx = present decay vertex integer number
//   pVtx = parent decay vertex integer number
//   daug = daughter order number for the given particle
//
// Consider the following decay chain history, where Vj is vertex number j:
//   V1
// A -> B      +             C                      <---- First chain level
//      |                    |
//      +--> B1 +  B2        +--> C1 C2 C3          <---- Second chain level
//     V2     |             V5    |
//            +--> B1a  B1b       +--> C1a C1b C1c  <---- Third chain level
//           V3          |       V6
//                       +--> d1 d2                 <---- Fourth chain level
//                      V4
//
// Since the decay tree is stored recursively, the information order follows
// the immediate, sequential, sub-decay vertex order V1, V2, V3, V4, V5, V6:
// A, B, B1, B1a, B1b, d1, d2, B2, C, C1, C1a, C1b, C1c, C2, C3
//
// The unique set of integer "attributes" for each particle is as follows:
//
// Particle   dVtx    pVtx     daug    
// A          0       0        0       The mother particle
// B          1       0        1       
// B1         2       1        1       
// B1a        3       2        1       
// B1b        3       2        2
// d1         4       3        1
// d2         4       3        2
// B2         2       1        2
// C          1       0        2
// C1         5       1        1
// C1a        6       5        1
// C1b        6       5        2
// C1c        6       5        3
// C2         5       1        2
// C3         5       1        3
//
// The decay tree will be defined by the decay file. Additional photons from PHOTOS will
// only appear as appended, extra daughters on the RHS of a given vertex, e.g. if a FSR
// photon is added to the RHS of V4, there will be a "d3" particle with attributes (4,3,3)

#include "genRootDecayChain.hh"

#include "EvtGen/EvtGen.hh"
#include "EvtGenBase/EvtRandomEngine.hh"
#include "EvtGenBase/EvtAbsRadCorr.hh"
#include "EvtGenBase/EvtDecayBase.hh"
#include "EvtGenBase/EvtParticle.hh"
#include "EvtGenBase/EvtParticleFactory.hh"
#include "EvtGenBase/EvtPatches.hh"
#include "EvtGenBase/EvtPDL.hh"
#include "EvtGenBase/EvtSimpleRandomEngine.hh"
#include "EvtGenBase/EvtMTRandomEngine.hh"

#ifdef EVTGEN_EXTERNAL
#include "EvtGenExternal/EvtExternalGenList.hh"
#endif

#include "TStyle.h"
#include "TROOT.h"

#include <iostream>
#include <list>
#include <string>

using std::cout;
using std::endl;
using std::string;

genRootDecayChain::genRootDecayChain(const string& decayFileName,
				     const string& rootFileName,
				     const string& parentName,
				     int nEvents, bool storeMtmXYZ) : 
    _decayFileName(decayFileName),
    _rootFileName(rootFileName),
    _parentName(parentName),
    _nEvents(nEvents),
    _storeMtmXYZ(storeMtmXYZ),
    _theFile(0),
    _theTree(0),
    _theCanvas(0)
{

    _theFile = new TFile(rootFileName.c_str(), "recreate");
    _theTree = new TTree("Data", "Data");
    _theTree->SetDirectory(_theFile);

    gROOT->SetStyle("Plain");
    gStyle->SetOptStat(0);
    
    _theCanvas = new TCanvas("theCanvas", "", 900, 700);
    _theCanvas->Clear();
    _theCanvas->UseCurrentStyle();

}

genRootDecayChain::~genRootDecayChain() {

    _theFile->Close();
    delete _theCanvas;

}

void genRootDecayChain::run() {

    this->initTree();
    this->generateEvents();
    this->writeTree();
}

void genRootDecayChain::initTree() {

    // Initialise internal variables
    _eventId = 0;
    _PDGId = 0;
    _dVtx = 0, _pVtx = 0, _daug = 0;
    _px = 0.0, _py = 0.0, _pz = 0.0;
    _p = 0.0, _E = 0.0, _m = 0.0, _t = 0.0;

    // Set-up the TTree
    _theTree->Branch("eventId", &_eventId, "eventId/I");
    _theTree->Branch("PDGId", &_PDGId, "PDGId/I");
    _theTree->Branch("dVtx", &_dVtx, "dVtx/I");
    _theTree->Branch("pVtx", &_pVtx, "pVtx/I");
    _theTree->Branch("daug", &_daug, "daug/I");
    _theTree->Branch("p", &_p, "p/D");
    _theTree->Branch("E", &_E, "E/D");
    _theTree->Branch("pL", &_pL, "pL/D");
    _theTree->Branch("EL", &_EL, "EL/D");
    _theTree->Branch("m", &_m, "m/D");

    if (_storeMtmXYZ) {

	// Store momentum components as well
	_theTree->Branch("px", &_px, "px/D");
	_theTree->Branch("py", &_py, "py/D");
	_theTree->Branch("pz", &_pz, "pz/D");
	_theTree->Branch("pxL", &_pxL, "pxL/D");
	_theTree->Branch("pyL", &_pyL, "pyL/D");
	_theTree->Branch("pzL", &_pzL, "pzL/D");

    }    

    // Lifetime
    _theTree->Branch("t", &_t, "t/D");

}

void genRootDecayChain::writeTree() {

    _theFile->cd();
    _theTree->Write();

}

void genRootDecayChain::generateEvents() {

    EvtRandomEngine* randomEngine = 0;
    EvtAbsRadCorr* radCorrEngine = 0;
    std::list<EvtDecayBase*> extraModels;
  
    // Define the random number generator
#ifdef EVTGEN_CPP11
    // Use the Mersenne-Twister generator (C++11 only)
    randomEngine = new EvtMTRandomEngine();
#else
    randomEngine = new EvtSimpleRandomEngine();
#endif
  
    // Initialize the generator - read in the decay table and particle properties.
    // For our validation purposes, we just want to read in one decay file

#ifdef EVTGEN_EXTERNAL
    bool useEvtGenRandom(false);
    EvtExternalGenList genList(true, "", "gamma", useEvtGenRandom);
    radCorrEngine = genList.getPhotosModel();
    extraModels = genList.getListOfModels();
#endif

    int mixingType(1);
    bool useXml(false);

    EvtGen evtGen(_decayFileName.c_str(), "../evt.pdl", randomEngine,
		  radCorrEngine, &extraModels, mixingType, useXml);

    EvtParticle* theParent(0);

    EvtId theId = EvtPDL::getId(_parentName);
    if (theId.getId() == -1 && theId.getAlias() == -1) {
	cout<<"Error. Could not find valid EvtId for "<<_parentName<<endl;
	return;
    }

    // Loop to create nEvents
    int i;
    for (i = 0; i < _nEvents; i++) {

        // Parent particle 4-momentum
        EvtVector4R pInit(EvtPDL::getMass(theId), 0.0, 0.0, 0.0);

	_eventId = i;

	if (i%1000 == 0) {cout<<"Event number = "<<i+1<<" out of "<<_nEvents<<std::endl;}
    
	theParent = EvtParticleFactory::particleFactory(theId, pInit);
	if (theParent->getSpinStates() == 3) {theParent->setVectorSpinDensity();}

	// Generate the event
	evtGen.generateDecay(theParent);

	// Decay vertex index
	theParent->setAttribute("dVtx", 0);
	// Parent vertex index
	theParent->setAttribute("pVtx", 0);
	// Daughter index
	theParent->setAttribute("daug", 0);

	int nDaug = theParent->getNDaug();
	int iDaug(0);

	storeTreeInfo(theParent);

	//cout<<"Event number = "<<i<<endl;
	//theParent->printTree();

	// Initialise the vertex number
	_vertexNo = 1;

	// Loop over the daughter tracks
	for (iDaug = 0; iDaug < nDaug; iDaug++) {

	    EvtParticle* daug = theParent->getDaug(iDaug);
	    // Decay vertex index
	    daug->setAttribute("dVtx", 1);
	    // Parent decay vertex
	    daug->setAttribute("pVtx", 0);
	    // Daughter index: 1,2,..,nDaug
	    daug->setAttribute("daug", iDaug+1);
	
	    // Recursively store the daughter information
	    this->storeDaughterInfo(daug);

	} // daughter loop

	theParent->deleteTree();

    } // event loop

    delete randomEngine;

}

void genRootDecayChain::storeDaughterInfo(EvtParticle* theParticle) {

    if (!theParticle) {return;}

    // Store the particle information in the TTree
    storeTreeInfo(theParticle);
		
    // Loop over the particle's own daughter tracks and call this function 
    // recursively, keeping track of the decay chain order number
    int nDaug = theParticle->getNDaug();
    int iDaug(0);

    // Increment the decay vertex integer if this particle has daughters
    if (nDaug > 0) {_vertexNo++;}

    // The parent vertex index for the daughters is equal to the 
    // current particle decay vertex index
    int parentVtx = theParticle->getAttribute("dVtx");

    // First, we need to loop over the given particle's daughters and set the 
    // attributes so we keep track of the decay tree history at this chain level
    for (iDaug = 0; iDaug < nDaug; iDaug++) {

	EvtParticle* daug = theParticle->getDaug(iDaug);

	// Set the attributes for the daughters
	daug->setAttribute("dVtx", _vertexNo);
	daug->setAttribute("pVtx", parentVtx);
	daug->setAttribute("daug", iDaug+1);

    }

    // Now loop over the daughters again and recursively call this function to
    // follow the rest of the decay tree history
    for (iDaug = 0; iDaug < nDaug; iDaug++) {

	EvtParticle* daug = theParticle->getDaug(iDaug);
	storeDaughterInfo(daug);
	
    }

}

void genRootDecayChain::storeTreeInfo(EvtParticle* theParticle) {

    if (!theParticle) {return;}

    // Store the particle information in the TTree by first setting the internal
    // variables and then calling Fill()
    _dVtx = theParticle->getAttribute("dVtx");
    _pVtx = theParticle->getAttribute("pVtx");
    _daug = theParticle->getAttribute("daug");

    //cout<<"Particle "<<theParticle->getName()<<", dVtx = "<<_dVtx
    //<<", pVtx = "<<_pVtx<<", daug = "<<_daug<<endl;

    EvtVector4R p4Lab = theParticle->getP4Lab(); // lab frame = mother frame
    EvtVector4R p4 = theParticle->getP4(); // frame = parents frame

    // PDG id
    _PDGId = EvtPDL::getStdHep(theParticle->getId());
	
    // 4-momenta in parent frame
    _E = p4.get(0);
    _px = p4.get(1);
    _py = p4.get(2);
    _pz = p4.get(3);
    _p = sqrt(_px*_px + _py*_py + _pz*_pz);

    // 4-momenta in lab frame
    _EL = p4Lab.get(0);
    _pxL = p4Lab.get(1);
    _pyL = p4Lab.get(2);
    _pzL = p4Lab.get(3);
    _pL = sqrt(_pxL*_pxL + _pyL*_pyL + _pzL*_pzL);
    
    // Rest mass and lifetime
    _m = theParticle->mass();
    _t = theParticle->getLifetime();

    // Store the information in the TTree
    _theTree->Fill();

}

int main(int argc, char** argv) {

    // Use the B0 -> K'*0 gamma, K'*0 -> K+ pi- decays as an example
    string decayFileName("BKstarGamma.dec");
    //string decayFileName("BuDst0rhop.dec");
    if (argc > 1) {decayFileName = argv[1];}
    cout<<"Decay file name is "<<decayFileName<<endl;

    string rootFileName("BKstarGamma.root");
    //string rootFileName("BuDst0rhop.root");
    if (argc > 2) {rootFileName = argv[2];}
    cout<<"Root file name is "<<rootFileName<<endl;

    string parentName("B0");
    //string parentName("B-");
    if (argc > 3) {parentName = argv[3];}
    cout<<"Parent name is "<<parentName<<endl;

    int nEvents(100000);
    if (argc > 4) {nEvents = atoi(argv[4]);}

    bool storeMtmXYZ = true;

    genRootDecayChain myGen(decayFileName, rootFileName, parentName, nEvents, storeMtmXYZ);
    myGen.run();    

    return 0;

}
