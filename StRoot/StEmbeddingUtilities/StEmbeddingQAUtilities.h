
#ifndef __StEmbeddingQAUtilities_h__
#define __StEmbeddingQAUtilities_h__

#include "TString.h"
#include "StMiniMcEvent/StMiniMcEvent.h"

//____________________________________________________________________________________________________
namespace StEmbeddingQAUtilities {

  //----------------------------------------------------------------------------------------------------
  //  id       node           description
  //----------------------------------------------------------------------------------------------------
  //  0         MC             MC tracks (embedding)
  //  1         MATCHED        Matched pairs (embedding)
  //  2         GHOST          Ghost pairs (embedding)
  //  3         CONTAM         Contaminated pairs (embedding)
  //  4         MATGLOB        Matched global pairs (embedding)
  //----------------------------------------------------------------------------------------------------
  //  5         PRIMARY        Primary tracks (real)
  //  6         GLOBAL         Global tracks (real)
  //----------------------------------------------------------------------------------------------------
  enum {
    kNEmbedding = 5,
    kNReal      = 2,
    kNCategory  = kNEmbedding + kNReal
  };

  // Particle id
  Int_t GetParticleId(const TString name) ;         // Geant particle id from particle name
  Int_t GetDaughterParticleId(const TString name,   // Daughter particle id from parent particle name
      const Int_t daughter=0);                      // daughter (=0,1,2,...)
  Int_t GetCategoryId(const TString name) ;         // Category id (see StEmbeddingQAUtilities.cxx)
  Category GetCategory(const UInt_t id) ;           // Category from category id
  TString GetCategoryName(const UInt_t id) ;        // Category name from category id
  TString GetCategoryTitle(const UInt_t id) ;       // Category title from category id

  // Mass utilities
  Double_t GetMass(const Int_t particleId) ;        // mass
  Double_t GetMass2(const Int_t particleId) ;       // mass^2
  Double_t GetMass2Daughter(const Int_t particleId, // Daughter mass^2 from parent particle id
      const Int_t daughter=0) ;

  // Name
  TString GetParticleName(const Int_t particleId, const Bool_t isTex=kTRUE);

  // Daghters
  Int_t GetNDaughter(const Int_t particleId) ;      // Number of daughters

  Int_t GetCharge(const Int_t particleId) ;  // charge

  void Print(const TString option = "PID") ;

};

#endif


