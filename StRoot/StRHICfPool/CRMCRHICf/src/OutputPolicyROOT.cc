#include <OutputPolicyROOT.h>

#include <CRMCoptions.h>
#include <CRMCinterface.h>

#include <TTree.h>
#include <TFile.h>

#include <iostream>

using namespace std;


OutputPolicyROOT::OutputPolicyROOT()
  :  fSigmaPairTot(0),
     fSigmaPairInel(0),
     fSigmaPairEl(0),
     fSigmaTot(0),
     fSigmaInel(0),
     fSigmaEl(0),
     fFile(0),
     fHead(0),
     iSigId(0),
     fParticle(0)
{
}


void 
OutputPolicyROOT::InitOutput(const CRMCoptions& cfg) 
{

  fFile = TFile::Open(cfg.GetOutputFileName().c_str(), "RECREATE");

  // file header
  
  fHead = new TTree("Header", "run header");

  fHead->Branch("Seed", const_cast<int*>(&cfg.fSeed), "Seed/I");     // random seedj
  fHead->Branch("ProjectileId", const_cast<int*>(&cfg.fProjectileId), "Projectile/I");   // beam/projectile Id
  fHead->Branch("ProjectileMomentum", const_cast<double*>(&cfg.fProjectileMomentum), "ProjectileMomentum/D");             // beam momentum
  fHead->Branch("TargetMomentum", const_cast<double*>(&cfg.fTargetMomentum),	"TargetMomentum/D");             // target momentum
  fHead->Branch("TargetId", const_cast<int*>(&cfg.fTargetId), "TargetId/I");                 // target Id
  fHead->Branch("HEModel", const_cast<int*>(&cfg.fHEModel), "HEModel/I");   // HE model flag
  fHead->Branch("sigmaPairTot", const_cast<double*>(&fSigmaPairTot), "sigmaPairTot/D");   // projectile-Nucleon tot sigma
  fHead->Branch("sigmaPairInel", const_cast<double*>(&fSigmaPairInel), "sigmaPairInel/D");   // projectile-Nucleon inel sigma
  fHead->Branch("sigmaPairEl", const_cast<double*>(&fSigmaPairEl), "sigmaPairEl/D");   // projectile-Nucleon el sigma
  fHead->Branch("sigmaTot", const_cast<double*>(&fSigmaTot), "sigmaTot/D");   // overal tot sigma
  fHead->Branch("sigmaInel", const_cast<double*>(&fSigmaInel), "sigmaInel/D");   // overal inel sigma
  fHead->Branch("sigmaEl", const_cast<double*>(&fSigmaEl), "sigmaEl/D");   // overal el sigma
  
    
  // particle list
  fParticle = new TTree("Particle","particles produced");
  
  fParticle->Branch("nPart", &gCRMC_data.fNParticles, "nPart/I");
  fParticle->Branch("ImpactParameter", &gCRMC_data.fImpactParameter, "ImpactParameter/D");
  fParticle->Branch("ProcessID", &iSigId, "ProcessId/I");
  fParticle->Branch("pdgid", gCRMC_data.fPartId, "pdgid[nPart]/I");
  fParticle->Branch("status", gCRMC_data.fPartStatus, "status[nPart]/I");
  fParticle->Branch("px", gCRMC_data.fPartPx, "px[nPart]/D");
  fParticle->Branch("py", gCRMC_data.fPartPy, "py[nPart]/D");
  fParticle->Branch("pz", gCRMC_data.fPartPz, "pz[nPart]/D");
  fParticle->Branch("E", gCRMC_data.fPartEnergy, "E[nPart]/D");
  fParticle->Branch("m", gCRMC_data.fPartMass, "m[nPart]/D");
}


void
OutputPolicyROOT::FillEvent(const CRMCoptions& cfg,const int nEvent)
{
  if (fSigmaPairTot==0) { 
    fSigmaPairTot = gCRMC_data.sigtot;
    fSigmaPairInel = gCRMC_data.sigine;
    fSigmaPairEl = gCRMC_data.sigela;
    fSigmaTot = gCRMC_data.sigtotaa;
    fSigmaInel = gCRMC_data.sigineaa;    
    fSigmaEl = gCRMC_data.sigelaaa;
    fHead->Fill(); // do only once
  }


  std::cout << " gCRMC_data.typevt " << gCRMC_data.typevt << std::endl;
   
  //Process Id
   //an integer ID uniquely specifying the signal process (i.e. MSUB in Pythia)
   iSigId = -1;
   switch (gCRMC_data.typevt) // if negative typevt mini plasma was created by event (except -4)
     {
     case   0: iSigId = 91; break; //elastic
     case   1: iSigId = 95; break; //ND
     case  -1: iSigId = 96; break; //ND with core
     case   2: iSigId = 94; break; //DD
     case  -2: iSigId = 94; break; //DD with core
     case   3: iSigId = 97; break; //CD
     case  -3: iSigId = 97; break; //CD with core
     case   4: iSigId = 92; break; //SD (proj excit.)
     case  -4: iSigId = 93; break; //SD (targ excit.)
     case  10: iSigId = 98; break; //pion exchange elastic
     case  11: iSigId = 98; break; //pion exchange ND
     case -11: iSigId = 98; break; //pion exchange ND with core
     case  12: iSigId = 98; break; //pion exchange DD
     case -12: iSigId = 98; break; //pion exchange DD with core
     case  13: iSigId = 98; break; //pion exchange CD
     case -13: iSigId = 98; break; //pion exchange CD with core
     case  14: iSigId = 98; break; //pion exchange SD (proj excit.)
     case -14: iSigId = 98; break; //pion exchange SD (targ excit.)
     default: cerr << "Signal ID not recognised for setting HEPEVT" << endl;
     }


  fParticle->Fill();
}


void
OutputPolicyROOT::CloseOutput(const CRMCoptions& cfg)
{      
  fFile->cd();
  fFile->Write();
  fFile->Close();
}
