// Program to create ROOT files for EvtGen validation plots.
// This looks at the 1st generation daughters and stores 4-momenta
// info into a ROOT file for further analysis.
// Useful for Pythia, Photos and Tauola decay tests.

#include "EvtGen/EvtGen.hh"

#include "EvtGenBase/EvtParticle.hh"
#include "EvtGenBase/EvtParticleFactory.hh"
#include "EvtGenBase/EvtPatches.hh"
#include "EvtGenBase/EvtPDL.hh"
#include "EvtGenBase/EvtRandom.hh"
#include "EvtGenBase/EvtSimpleRandomEngine.hh"
#include "EvtGenBase/EvtMTRandomEngine.hh"
#include "EvtGenBase/EvtHepMCEvent.hh"
#include "EvtGenBase/EvtSpinDensity.hh"
#include "EvtGenBase/EvtSpinType.hh"
#include "EvtGenBase/EvtComplex.hh"
#include "EvtGenBase/EvtCPUtil.hh"

#include "EvtGenBase/EvtAbsRadCorr.hh"
#include "EvtGenBase/EvtDecayBase.hh"

#include "HepMC/GenEvent.h"
#include "HepMC/GenVertex.h"
#include "HepMC/GenParticle.h"
#include "HepMC/SimpleVector.h"

#ifdef EVTGEN_EXTERNAL
#include "EvtGenExternal/EvtExternalGenList.hh"
#endif

#include "TFile.h"
#include "TTree.h"
#include "TH1.h"
#include "TF1.h"
#include "TStyle.h"
#include "TCanvas.h"
#include "TLine.h"
#include "TROOT.h"

#include <iostream>
#include <string>
#include <vector>
#include <list>

using std::cout;
using std::endl;
using std::string;

// Flight-time histograms for B0, B0bar
TH1F* H_total = new TH1F("Total", "", 300, 0.0, 12.0);
TH1F* H_B0 = new TH1F("B0", "", 300, 0.0, 12.0);
TH1F* H_B0bar = new TH1F("B0bar", "", 300, 0.0, 12.0);

int B0Id(511), B0barId(-511);

void storeBFlightTimes(HepMC::GenEvent* theEvent);
bool checkSignal(std::vector<int>& daugIdVect);
double calcFlightTime(HepMC::FourVector& BDecayVtx, HepMC::FourVector& B4mtm);
double sineFitFun(double* x, double* p);
double timeFitFun(double* x, double* p);

int main(int argc, char** argv) {

  string decayFileName("CPVDecayFiles/Bd_JpsiKSeeCPV.dec");
  if (argc > 1) {decayFileName = argv[1];}
  cout<<"Decay file name is "<<decayFileName<<endl;

  string rootFileName("rootFiles/CPVDecayTest.root");
  if (argc > 2) {rootFileName = argv[2];}
  cout<<"Root file name is "<<rootFileName<<endl;

  string parentName("Upsilon(4S)");
  if (argc > 3) {parentName = argv[3];}
  cout<<"Parent name is "<<parentName<<endl;

  int nEvents(10000);
  if (argc > 4) {nEvents = atoi(argv[4]);}

  double sin2Beta = sin(0.775);
  if (argc > 5) {sin2Beta = atof(argv[5]);}

  cout<<"Number of events is "<<nEvents<<endl;
  cout<<"sin2Beta = "<<sin2Beta<<" (used to draw oscillation maxima lines)"<<endl;

  // Define the random number generator

  EvtRandomEngine* myRandomEngine = 0;

#ifdef EVTGEN_CPP11
  // Use the Mersenne-Twister generator (C++11 only)
  myRandomEngine = new EvtMTRandomEngine();
#else
  myRandomEngine = new EvtSimpleRandomEngine();
#endif

  EvtAbsRadCorr* radCorrEngine = 0;
  std::list<EvtDecayBase*> extraModels;

#ifdef EVTGEN_EXTERNAL
  EvtExternalGenList genList;
  radCorrEngine = genList.getPhotosModel();
  extraModels = genList.getListOfModels();
#endif

  int mixingType = EvtCPUtil::Incoherent;

  // Initialize the generator - read in the decay table and particle properties.
  EvtGen myGenerator("../DECAY_2010.DEC", "../evt.pdl", myRandomEngine, 
		     radCorrEngine, &extraModels, mixingType);

  myGenerator.readUDecay(decayFileName.c_str());

  EvtId theId = EvtPDL::getId(parentName);
  if (theId.getId() == -1 && theId.getAlias() == -1) {
    cout<<"Error. Could not find valid EvtId for "<<parentName<<endl;
    return -1;
  }

  // Start all initial (parent) decays at the origin
  EvtVector4R origin(0.0, 0.0, 0.0, 0.0);
  EvtSpinDensity* spinDensity = 0;

  EvtSpinType::spintype baseSpin = EvtPDL::getSpinType(theId);

  if (baseSpin == EvtSpinType::VECTOR) {
    cout<<"Setting spin density for vector particle "<<parentName<<endl;
    spinDensity = new EvtSpinDensity();
    spinDensity->setDiag(EvtSpinType::getSpinStates(EvtSpinType::VECTOR));
    spinDensity->set(1, 1, EvtComplex(0.0, 0.0));
  }

  // Loop to create nEvents

  int i;
  for (i = 0; i < nEvents; i++) {

    if (i%1000 == 0) {cout<<"Event number = "<<i+1<<" out of "<<nEvents<<std::endl;}

    // Set up the parent particle
    int PDGId = EvtPDL::getStdHep(theId);
    EvtVector4R pInit(EvtPDL::getMass(theId), 0.0, 0.0, 0.0);
    
    // Generate a new HepMC event with the decays. We own this pointer, so we must 
    // delete it after using the HepMC event information.
    EvtHepMCEvent* theEvent = myGenerator.generateDecay(PDGId, pInit, origin, spinDensity);

    // Retrieve the HepMC event information
    HepMC::GenEvent* hepMCEvent = theEvent->getEvent();
    //hepMCEvent->print();

    // Fill the B0/B0bar flight time histograms
    storeBFlightTimes(hepMCEvent);
    
    // Cleanup the event to avoid memory leaks
    delete theEvent;

  }

  H_total->Sumw2();
  H_B0->Sumw2();
  H_B0bar->Sumw2();

  TH1F* H_Diff = dynamic_cast<TH1F*>(H_B0->Clone("H_Diff"));
  H_Diff->Add(H_B0bar, -1.0);

  TH1F* H_DiffSum = dynamic_cast<TH1F*>(H_Diff->Clone("H_DiffSum"));
  H_DiffSum->Divide(H_total);

  TF1* sineFit = new TF1("sineFit", sineFitFun, 0.0, 12.0, 3);

  sineFit->SetParName(0, "N");
  sineFit->SetParName(1, "a");
  sineFit->SetParName(2, "#phi");
  sineFit->SetParameter(0, 0.5);
  sineFit->SetParameter(1, 0.7);
  sineFit->SetParameter(2, -0.7);
  H_DiffSum->Fit(sineFit );


  TF1* timeFit = new TF1("timeFit", timeFitFun, 0.0, 12.0, 2);
  timeFit->SetParName(0, "N");
  timeFit->SetParName(1, "#Gamma");
  timeFit->SetParameter(0, 500);
  timeFit->SetParameter(1, 0.6);
  timeFit->SetParLimits(1, 0.0, 1.0);
  H_total->Fit(timeFit);

  gROOT->SetStyle("Plain");
  gStyle->SetOptFit(1111);
  TCanvas* theCanvas = new TCanvas("theCanvas", "", 900, 700);
  theCanvas->UseCurrentStyle();

  H_DiffSum->SetXTitle("t (ps)");
  H_DiffSum->Draw();

  // Plot +- sin(2beta) lines
  TLine line1(0.0, sin2Beta, 12.0, sin2Beta); line1.Draw();
  TLine line2(0.0, -sin2Beta, 12.0, -sin2Beta); line2.Draw();
  theCanvas->Print("BCPVSinFit.gif");

  H_total->SetXTitle("t (ps)");
  H_total->Draw();
  theCanvas->Print("BTimeFit.gif");

  TFile* theFile = new TFile(rootFileName.c_str(), "recreate");
  theFile->cd();
  H_B0->Write();
  H_B0bar->Write();
  H_total->Write();
  H_Diff->Write();
  H_DiffSum->Write();
  theFile->Close();

  // Cleanup
  delete theCanvas;
  delete spinDensity;
  delete myRandomEngine;

  cout<<"Done."<<endl;

  return 0;

}

void storeBFlightTimes(HepMC::GenEvent* theEvent) {

  std::list<HepMC::GenVertex*> allVertices;
  HepMC::GenEvent::vertex_iterator vertexIter;

  // Loop over vertices in the event
  for (vertexIter = theEvent->vertices_begin(); 
       vertexIter != theEvent->vertices_end(); ++vertexIter) {

    // Get the vertex
    HepMC::GenVertex* theVertex = *vertexIter;

    if (theVertex == 0) {continue;}

    // Check to see if the incoming particle is a B candidate.
    // If so, also look at the outgoing particles to see if we have a signal decay.
    // For these, get the B decay vertex position and the B 4-momentum to calculate
    // the B lifetime.
    
    bool gotB0(false), gotB0bar(false);
    HepMC::FourVector B4mtm;
    
    HepMC::GenVertex::particles_in_const_iterator inIter;
    for (inIter = theVertex->particles_in_const_begin();
	 inIter != theVertex->particles_in_const_end(); ++inIter) {
      
      HepMC::GenParticle* inParticle = *inIter;
      
      if (inParticle == 0) {continue;}
      
      int inPDGId = inParticle->pdg_id();
      if (inPDGId == B0Id) {
	gotB0 = true;
      } else if (inPDGId == B0barId) {
	gotB0bar = true;
      }
      
      if (gotB0 == true || gotB0bar == true) {
	B4mtm = inParticle->momentum();
      }
      
    } // Loop over ingoing vertex particles
    
    if (gotB0 == true || gotB0bar == true) {
      
      // Check outgoing particles
      std::vector<int> daugIdVect;
      HepMC::GenVertex::particles_out_const_iterator outIter;
      for (outIter = theVertex->particles_out_const_begin();
	   outIter != theVertex->particles_out_const_end(); ++outIter) {
	
	HepMC::GenParticle* outParticle = *outIter;
	
	if (outParticle != 0) {
	  int outPDGId = outParticle->pdg_id();
	  daugIdVect.push_back(outPDGId);
	}
	
      } // Loop over outgoing vertex particles
      
	// Check if we have the signal decay
      bool gotSignal = checkSignal(daugIdVect);
      
      // Fill the flight time histograms for signal B decays
      if (gotSignal == true) {
	
	HepMC::FourVector BDecayVtx = theVertex->position();
	double flightTime = calcFlightTime(BDecayVtx, B4mtm);
	
	if (gotB0 == true) {
	  
	  H_B0->Fill(flightTime);
	  H_total->Fill(flightTime);
	  
	} else {
	  
	  H_B0bar->Fill(flightTime);
	  H_total->Fill(flightTime);
	  
	}
	
      } // Got signal B decay (for flight-time histograms)
      
    } // Got a B candidate
    
  } // Loop over event vertices
  
}

double calcFlightTime(HepMC::FourVector& BDecayVtx, HepMC::FourVector& B4mtm) {

  double flightTime(0.0);

  double distance = BDecayVtx.rho()*1e-3; // metres
  double momentum = B4mtm.rho(); // GeV/c
  double BMass = 5.2795; // GeV/c^2
  double c0 = 299792458.0; // m/s

  if (momentum > 0.0) {

    flightTime = 1.0e12*distance*BMass/(momentum*c0); // picoseconds

  }

  return flightTime;

}

bool checkSignal(std::vector<int>& daugIdVect) {

  bool gotSignal(false);

  int nDaug = daugIdVect.size();

  // Check for J/psi Ks decays
  if (nDaug == 2) {

    int daug1Id = daugIdVect[0];
    int daug2Id = daugIdVect[1];
    if ((daug1Id == 443 && daug2Id == 310) || (daug1Id == 310 && daug2Id == 443)) {
      gotSignal = true;
    }

  }

  return gotSignal;

}

double sineFitFun(double* x, double* p) {

  double t = x[0];
  double N = p[0];
  double a = p[1];
  double phi = p[2];

  double funcVal = N*sin(a*t + phi);

  return funcVal;

}

double timeFitFun(double* x, double* p) {

  double t = x[0];
  double N0 = p[0];
  double gamma = p[1];
  double funcVal = N0*(exp(-gamma*t));

  return funcVal;

}
