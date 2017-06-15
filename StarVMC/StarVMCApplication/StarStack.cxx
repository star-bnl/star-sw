/**************************************************************************
 * Copyright(c) 1998-1999, ALICE Experiment at CERN, All rights reserved. *
 *                                                                        *
 * Author: The ALICE Off-line Project.                                    *
 * Contributors are mentioned in the code where appropriate.              *
 *                                                                        *
 * Permission to use, copy, modify and distribute this software and its   *
 * documentation strictly for non-commercial purposes is hereby granted   *
 * without fee, provided that the above copyright notice appears in all   *
 * copies and that both the copyright notice and this permission notice   *
 * appear in the supporting documentation. The authors make no claims     *
 * about the suitability of this software for any purpose. It is          *
 * provided "as is" without express or implied warranty.                  *
 **************************************************************************/

/* $Id$ */

///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//  Particles stack class                                                    //
//  Implements the TMCVirtualStack of the Virtual Monte Carlo                //
//  Holds the particles transported during simulation                        //
//  Is used to compare results of reconstruction with simulation             //
//  Author A.Morsch                                                          //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////

#include "Riostream.h"
#include "TClonesArray.h"
#include "TObjArray.h"
#include "TPDGCode.h"
#include "TMCProcess.h"
#include "TParticle.h"
#include "TParticlePDG.h"
#include "TDatabasePDG.h"
#include "TTree.h"
#include "TDirectory.h"
#include "TError.h"
#include "StarStack.h"
Int_t StarStack::fgDebug = 0;
ClassImp(StarStack);
using namespace std;  
//_______________________________________________________________________
  StarStack::StarStack():
    fParticles("TParticle", 1000),
    fCurrentTrack(0),
    fNtrack(0),
    fNprimary(0),
    fTrackNo(0),
    fCurrentID(-1)
{
  //
  // Default constructor
  //
}

//_______________________________________________________________________
StarStack::StarStack(Int_t size, const char* /*evfoldname*/):
  fParticles("TParticle",1000),
  fCurrentTrack(0),
  fNtrack(0),
  fNprimary(0),
  fTrackNo(0),
  fCurrentID(-1)
{
  //
  //  Constructor
  //
}

//_______________________________________________________________________
StarStack::StarStack(const StarStack& st):
  TVirtualMCStack(st),
  fParticles("TParticle",1000),
  fCurrentTrack(0),
  fNtrack(st.GetNtrack()),
  fNprimary(st.GetNprimary()),
  fTrackNo(0),
  fCurrentID(-1)
{
  // Copy constructor
}


//_______________________________________________________________________
void StarStack::Copy(TObject&) const
{
  Fatal("StarStack::Copy","Not implemented!");
}

//_______________________________________________________________________
StarStack::~StarStack()
{
  //
  // Destructor
  //
  
  fParticles.Clear();
}
//
// public methods
//
//________________________________________________________________________________
void StarStack::Print(Option_t *option) const {
  cout << "StarStack Info  ";
  cout << "\tTotal number of particles:   " <<  GetNtrack();
  cout << "\tNumber of primary particles: " <<  GetNprimary();
  cout << "\tStack size: " << fStack.size() << endl;
  for (Int_t i=0; i<GetNtrack(); i++) {
    TParticle * part = (TParticle *) fParticles[i];
    cout << Form("%4i:",i);
    cout << "Done:" << part->TestBit(kDoneBit);
    cout <<";Dau:" << part->TestBit(kDaughtersBit);
    cout <<";Tra:" << part->TestBit(kTransportBit) << ";";
    part->Print();
  }  
  
}

//_____________________________________________________________________________
void StarStack::PushTrack(Int_t done, Int_t parent, Int_t pdg, const Float_t *pmom,
			  const Float_t *vpos, const Float_t *polar, Float_t tof,
			  TMCProcess mech, Int_t &ntr, Float_t weight, Int_t is)
{ 
  //
  // Load a track on the stack
  //
  // done     1 if the track has to be transported
  //          0 if not
  // parent   identifier of the parent track. -1 for a primary
  // pdg    particle code
  // pmom     momentum GeV/c
  // vpos     position 
  // polar    polarisation 
  // tof      time of flight in seconds
  // mecha    production mechanism
  // ntr      on output the number of the track stored
  //
  
  //  const Float_t tlife=0;
  
  //
  // Here we get the static mass
  // For MC is ok, but a more sophisticated method could be necessary
  // if the calculated mass is required
  // also, this method is potentially dangerous if the mass
  // used in the MC is not the same of the PDG database
  //
  TParticlePDG* pmc =  TDatabasePDG::Instance()->GetParticle(pdg);
  if (pmc) {
    Float_t mass = TDatabasePDG::Instance()->GetParticle(pdg)->Mass();
    Float_t e=TMath::Sqrt(mass*mass+pmom[0]*pmom[0]+
			  pmom[1]*pmom[1]+pmom[2]*pmom[2]);
    
    //    printf("Loading  mass %f ene %f No %d ip %d parent %d done %d pos %f %f %f mom %f %f %f kS %d m \n",
    //	   mass,e,fNtrack,pdg,parent,done,vpos[0],vpos[1],vpos[2],pmom[0],pmom[1],pmom[2],kS);
    
    
    PushTrack(done, parent, pdg, pmom[0], pmom[1], pmom[2], e,
	      vpos[0], vpos[1], vpos[2], tof, polar[0], polar[1], polar[2],
	      mech, ntr, weight, is);
  } else {
    Warning("StarStack::PushTrack",Form("Particle type %d not defined in PDG Database !", pdg));
    Warning("StarStack::PushTrack","Particle skipped !");
  }
}

//_____________________________________________________________________________
void StarStack::PushTrack(Int_t done, Int_t parent, Int_t pdg,
			  Double_t px, Double_t py, Double_t pz, Double_t e,
			  Double_t vx, Double_t vy, Double_t vz, Double_t tof,
			  Double_t polx, Double_t poly, Double_t polz,
			  TMCProcess mech, Int_t &ntr, Double_t weight, Int_t is)
{ 
  //
  // Load a track on the stack
  //
  // done        1 if the track has to be transported
  //             0 if not
  // parent      identifier of the parent track. -1 for a primary
  // pdg         particle code
  // kS          generation status code
  // px, py, pz  momentum GeV/c
  // vx, vy, vz  position 
  // polar       polarisation 
  // tof         time of flight in seconds
  // mech        production mechanism
  // ntr         on output the number of the track stored
  // is          = 0 to put in stack otherwise put to TCloneArray (a la KINE)
  //    
  // New method interface: 
  // arguments were changed to be in correspondence with TParticle
  // constructor.
  // Note: the energy is not calculated from the static mass but
  // it is passed by argument e.
  
  const Int_t kFirstDaughter=-1;
  const Int_t kLastDaughter=-1;
  fNtrack = GetNtrack();
  Int_t index = fNtrack;
  TParticle* particle
    = new(fParticles[index]) 
    TParticle(pdg, is, parent, -1, kFirstDaughter, kLastDaughter,
	      px, py, pz, e, vx, vy, vz, tof);
  
  particle->SetPolarisation(polx, poly, polz);
  particle->SetWeight(weight);
  particle->SetUniqueID(mech);
  //  Declare that the daughter information is valid
  particle->SetBit(kDaughtersBit);
  //  Add the particle to the stack
  if(parent>=0) {
    TParticle *parentTrack = Particle(parent);
    if (parentTrack) {
      parentTrack->SetLastDaughter(index);
      if (parentTrack->GetFirstDaughter()<0) parentTrack->SetFirstDaughter(index);
    }
    else {
      Error("StarStack::PushTrack",Form("Parent %d does not exist",parent));
    }
  }
  if(!done) {
    particle->SetBit(kDoneBit);
  } else {
    particle->SetBit(kTransportBit);
    if (mech != kPPrimary) { // primaries always go to particle list
      Double_t R = particle->R();
      Double_t Z = particle->Vz();
      Double_t Ekin = particle->Ek(); 
      if (R > 180 || TMath::Abs(Z) > 180 || Ekin < 0.010) { // 10 MeV Cut
	fStack.push(*particle);
	fParticles.RemoveAt(index);
	index--;
	particle = &fStack.top();
	if (Debug() > 1) {
	  cout << "Push to stack:   ";
	}
      }
    } else {
      fNtrack++;
      if (Debug() > 1) {
	cout << "Push to Paticles:";
      }
    }
    if (Debug() > 1) {
      cout << Form("%4i:%4i",index,fStack.size());
      cout << Form("%30s:",TMCProcessName[mech]);
      particle->Print();
    }
  }
  ntr = fNtrack + fStack.size();
}
//_____________________________________________________________________________
TParticle*  StarStack::PopNextTrack(Int_t& itrack) {
  //
  // Returns next track from stack of particles
  //
  TParticle* track = GetNextParticle();
  if (track) {
    itrack = ++fTrackNo;
    track->SetBit(kDoneBit);
  }
  else
    itrack = -1;
  fCurrentTrack = track;
  if (track && Debug() > 1) {
    cout << Form("Pop track %4i/%4i:",itrack,fCurrentID); 
    fCurrentTrack->Print();
  }
  return track;
}
//_____________________________________________________________________________
TParticle*  StarStack::PopPrimaryForTracking(Int_t i)
{
  //
  // Returns i-th primary particle if it is flagged to be tracked,
  // 0 otherwise
  //
  
  TParticle* particle = Particle(i);
  if (! particle) return particle;
  if (!particle->TestBit(kDoneBit)) {
    fCurrentTrack = particle;
    fCurrentID = i;
    return particle;
  }
  else
    return 0;
}      
//_____________________________________________________________________________
void  StarStack::Clean(Int_t size) 
{
  //
  // Reset stack data except for fTreeK
  //
  
  fNtrack=0;
  fNprimary=0;
  fCurrentID = -1;
  fTrackNo = 0;
  fParticles.Clear();
}

//_____________________________________________________________________________
void  StarStack::Reset(Int_t size) 
{
  //
  // Reset stack data including fTreeK
  //
  
  Clean(size);
}

//_____________________________________________________________________________
TParticle* StarStack::GetNextParticle()
{
  //
  // Return next particle from stack of particles
  //
  static TParticle particleFromStack;
  TParticle* particle = 0;
  if (fStack.empty()) {
    Int_t fNtrack = GetNtrack();
    for (Int_t i = 0; i < fNtrack; i++) {
      particle = PopPrimaryForTracking(i);
      if (particle && ! particle->TestBit(kDoneBit)) break;
    }
  } else {
    particleFromStack = fStack.top();
    fStack.pop();
    particle = &particleFromStack;
  }
  return particle;  
}
//_____________________________________________________________________________

Bool_t StarStack::IsStable(Int_t pdg) const
{
  //
  // Decide whether particle (pdg) is stable
  //
  
  
  // All ions/nucleons are considered as stable
  // Nuclear code is 10LZZZAAAI
  if(pdg>1000000000)return kTRUE;
  
  const Int_t kNstable = 18;
  Int_t i;
  
  Int_t pdgStable[kNstable] = {
    kGamma,             // Photon
    kElectron,          // Electron
    kMuonPlus,          // Muon 
    kPiPlus,            // Pion
    kKPlus,             // Kaon
    kK0Short,           // K0s
    kK0Long,            // K0l
    kProton,            // Proton 
    kNeutron,           // Neutron
    kLambda0,           // Lambda_0
    kSigmaMinus,        // Sigma Minus
    kSigmaPlus,         // Sigma Plus
    3312,               // Xsi Minus 
    3322,               // Xsi 
    3334,               // Omega
    kNuE,               // Electron Neutrino 
    kNuMu,              // Muon Neutrino
    kNuTau              // Tau Neutrino
  };
  
  Bool_t isStable = kFALSE;
  for (i = 0; i < kNstable; i++) {
    if (pdg == TMath::Abs(pdgStable[i])) {
      isStable = kTRUE;
      break;
    }
  }
  
  return isStable;
}

//_____________________________________________________________________________
Bool_t StarStack::IsPhysicalPrimary(Int_t index)
{
  //
  // Test if a particle is a physical primary according to the following definition:
  // Particles produced in the collision including products of strong and
  // electromagnetic decay and excluding feed-down from weak decays of strange
  // particles.
  //
  TParticle* p = Particle(index);
  Int_t ist = p->GetStatusCode();
  
  //
  // Initial state particle
  if (ist > 2) return kFALSE;
  if (ist == 2) return kTRUE;
  Int_t pdg = TMath::Abs(p->GetPdgCode());
  
  if (!IsStable(pdg)) return kFALSE;
  
  if (index < GetNprimary()) {
    //
    // Particle produced by generator
    return kTRUE;
  } else {
    //
    // Particle produced during transport
    //
    Int_t imo =  p->GetFirstMother();
    if (imo >= 0) {
      TParticle* pm  = Particle(imo);
      Int_t mpdg = TMath::Abs(pm->GetPdgCode());
      // Check for Sigma0 
      if ((mpdg == 3212) &&  (imo <  GetNprimary())) return kTRUE;
      // 
      // Check if it comes from a pi0 decay
      //
      if ((mpdg == kPi0) && (imo < GetNprimary()))   return kTRUE; 
      
      // Check if this is a heavy flavor decay product
      Int_t mfl  = Int_t (mpdg / TMath::Power(10, Int_t(TMath::Log10(mpdg))));
      //
      // Light hadron
      if (mfl < 4) return kFALSE;
      
      //
      // Heavy flavor hadron produced by generator
      if (imo <  GetNprimary()) {
	return kTRUE;
      }
      
      // To be sure that heavy flavor has not been produced in a secondary interaction
      // Loop back to the generated mother
      pm = 0;
      while (imo >=  GetNprimary()) {
	imo = pm->GetFirstMother();
	if (imo >= 0) pm  =  Particle(imo);
      }
      if (pm) {
	mpdg = TMath::Abs(pm->GetPdgCode());
	mfl  = Int_t (mpdg / TMath::Power(10, Int_t(TMath::Log10(mpdg))));
	
	if (mfl < 4) {
	  return kFALSE;
	} else {
	  return kTRUE;
	} 
      }
    }
    return kFALSE;
  } // produced by generator ?
} 

Bool_t StarStack::IsSecondaryFromWeakDecay(Int_t index) {
  
  // If a particle is not a physical primary, check if it comes from weak decay
  
  TParticle* particle = Particle(index);
  Int_t uniqueID = particle->GetUniqueID();
  
  if(IsPhysicalPrimary(index)) return kFALSE;
  
  Int_t indexMoth = particle->GetFirstMother();
  if(indexMoth < 0) return kFALSE; // if index mother < 0 and not a physical primary, is a non-stable product or one of the beams
  TParticle* moth = Particle(indexMoth);
  Float_t codemoth = (Float_t)TMath::Abs(moth->GetPdgCode());
  // mass of the flavour
  Int_t mfl = 0;
  // Protect the "rootino" case when codemoth is 0
  if (TMath::Abs(codemoth)>0) mfl = Int_t (codemoth / TMath::Power(10, Int_t(TMath::Log10(codemoth))));
  
  if(mfl == 3 && uniqueID == kPDecay) return kTRUE;// The first mother is strange and it's a decay
  if(codemoth == 211 && uniqueID == kPDecay) return kTRUE;// pion+- decay products
  if(codemoth == 13 && uniqueID == kPDecay) return kTRUE;// muon decay products
  
  /// Hypernuclei case
    if (TMath::Abs(moth->GetPdgCode()) > 1000000000 && uniqueID == kPDecay) {
      if ((moth->GetPdgCode() / 10000000) % 10 != 0) return kTRUE; /// Number of lambdas in the hypernucleus != 0
	}
    
    return kFALSE;
    
}
Bool_t StarStack::IsSecondaryFromMaterial(Int_t index) {
  
  // If a particle is not a physical primary, check if it comes from material
  
  if(IsPhysicalPrimary(index)) return kFALSE;
  if(IsSecondaryFromWeakDecay(index)) return kFALSE;
  TParticle* particle = Particle(index);
  Int_t indexMoth = particle->GetFirstMother();
  if(indexMoth < 0) return kFALSE; // if index mother < 0 and not a physical primary, is a non-stable product or one of the beams
  return kTRUE;
  
}
