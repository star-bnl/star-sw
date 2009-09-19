
#include <iostream>
#include <map>

#include "TError.h"

#include "StEmbeddingQAUtilities.h"

using namespace std ;

namespace StEmbeddingQAUtilities {

  // Category
  static const Category kCategory[] = {MC, MATCHED, GHOST, CONTAM, MATGLOB};
  static const TString kCategoryName[] = {"MC", "MATCHED", "GHOST", "CONTAM", "MATGLOB", "PRIMARY", "GLOBAL"};
  static const TString kCategoryTitle[] = { 
    "MC tracks", "Matched pairs", "Ghost pairs", "Contaminated pairs", "Matched global pairs",
    "Primary tracks (real)", "Global tracks (real)" 
  };

  static Bool_t isParticleRegisterd = kFALSE ;
  map<TString, Int_t> kParticleId;

  //____________________________________________________________________________________________________
  void RegisterParticleId()
  {
    if( isParticleRegisterd ) return ;

    // Register paticle id's
    kParticleId.insert(pair<TString, Int_t>("photon", 1));      kParticleId.insert(pair<TString, Int_t>("gamma", 1)); // photon
    kParticleId.insert(pair<TString, Int_t>("eplus", 2));       kParticleId.insert(pair<TString, Int_t>("e+", 2));    // positron
    kParticleId.insert(pair<TString, Int_t>("eminus", 3));      kParticleId.insert(pair<TString, Int_t>("e-", 3));    // electron
    kParticleId.insert(pair<TString, Int_t>("pi0", 7));                                                               // pi0
    kParticleId.insert(pair<TString, Int_t>("piplus", 8));      kParticleId.insert(pair<TString, Int_t>("pi+", 8));   // pi+
    kParticleId.insert(pair<TString, Int_t>("piminus", 9));     kParticleId.insert(pair<TString, Int_t>("pi-", 9));   // pi-
    kParticleId.insert(pair<TString, Int_t>("kplus", 11));      kParticleId.insert(pair<TString, Int_t>("k+", 11));   // k+
    kParticleId.insert(pair<TString, Int_t>("kminus", 12));     kParticleId.insert(pair<TString, Int_t>("k-", 12));   // k-
    kParticleId.insert(pair<TString, Int_t>("proton", 14));     kParticleId.insert(pair<TString, Int_t>("p", 14));    // proton
    kParticleId.insert(pair<TString, Int_t>("antiproton", 15)); kParticleId.insert(pair<TString, Int_t>("pbar", 15)); // anti-proton
    kParticleId.insert(pair<TString, Int_t>("d0", 37));                                                               // D0
    kParticleId.insert(pair<TString, Int_t>("d0bar", 38));                                                            // D0bar
    kParticleId.insert(pair<TString, Int_t>("phi", 50));                                                              // phi
    kParticleId.insert(pair<TString, Int_t>("jpsi", 160));                                                            // J/Psi
    kParticleId.insert(pair<TString, Int_t>("lambda1520", 995));                                                      // Lambda(1520)

//    cout << "#--------------------------------------------------" << endl;
//    cout << "List of available particles (alphabetical order)" << endl;
//    for(map<TString, Int_t>::iterator iter = kParticleId.begin();
//        iter != kParticleId.end(); iter++){
//      cout << Form("Particle: %10s,   particleid: %10d", iter->first.Data(), iter->second) << endl;
//    }
//    cout << "#--------------------------------------------------" << endl;

    isParticleRegisterd = kTRUE;
  }

  //__________________________________________________________________________________________
  Int_t GetParticleId(const TString name)
  {
    // Get particle id from particle name
    //  - Input particle name is case insensitive
    //     For example, eplus, Eplus, ePlus etc should work
 
    RegisterParticleId();

    TString _name(name);
    _name.ToLower() ;

    map<TString, Int_t>::iterator particleId(kParticleId.find(_name));

    if( particleId != kParticleId.end() ){
      const TString pname(particleId->first);
      const Int_t pid(particleId->second);
      cout << pname << " " << pid << endl;
      return pid ;
    }
 
    /*
    if ( _name.CompareTo("photon") || _name.CompareTo("gamma") )         return 1 ;
    else if ( _name.CompareTo("eplus") || _name.CompareTo("e+") )        return 2 ;
    else if ( _name.CompareTo("eminus") || _name.CompareTo("e-") )       return 3 ;
    else if ( _name.CompareTo("pi0") )                                   return 7 ;
    else if ( _name.CompareTo("piplus") || _name.CompareTo("pi+") )      return 8 ;
    else if ( _name.CompareTo("piminus") || _name.CompareTo("pi-") )     return 9 ;
    else if ( _name.CompareTo("kplus") || _name.CompareTo("k+") )        return 11 ;
    else if ( _name.CompareTo("kminus") || _name.CompareTo("k-") )       return 12 ;
    else if ( _name.CompareTo("proton") || _name.CompareTo("p") )        return 14 ;
    else if ( _name.CompareTo("antiproton") || _name.CompareTo("pbar") ) return 15 ;
    else if ( _name.CompareTo("d0") )                                    return 37 ;
    else if ( _name.CompareTo("d0bar") )                                 return 38 ;
    else if ( _name.CompareTo("phi") )                                   return 50 ;
    else if ( _name.CompareTo("jpsi") )                                  return 160 ;
    else if ( _name.CompareTo("lambda1520") )                            return 995 ;
    else{
      Error("GetParticleId", "Unknown particle name, name = %15s", name.Data());
      Print("pid");
    }
    */
 
    return -1 ;
  }

  //__________________________________________________________________________________________
  Int_t GetDaughterParticleId(const TString name, const Int_t daughter)
  {
    // Get daughter particle id from parent particle name
    //   Currently, only 
    //   - 2 body decay
    //   - decay into opposite charged particles
    //   are implemented

    TString _name(name);
    _name.ToLower();

    switch ( daughter ){
      // Positive (or neutral)
      case 0:
        if ( _name.Contains("pi0") )             return  1 ;
        else if ( _name.Contains("d0") )         return  8 ;
        else if ( _name.Contains("d0bar") )      return 11 ;
        else if ( _name.Contains("phi") )        return 11 ;
        else if ( _name.Contains("jpsi") )       return  2 ;
        else if ( _name.Contains("lambda1520") ) return 14 ;
        else{
          Error("GetDaughterParticleId", "Unknown particle name, name = %15s", name.Data());
          Print("pid");
          return -1 ;
        }

      // Negative (or neutral)
      case 1:
        if ( _name.Contains("d0") )              return 12 ;
        else if ( _name.Contains("d0bar") )      return  9 ;
        else if ( _name.Contains("phi") )        return 12 ;
        else if ( _name.Contains("jpsi") )       return  3 ;
        else if ( _name.Contains("lambda1520") ) return 12 ;
        else{
          Error("GetDaughterParticleId", "Unknown particle name, name = %15s", name.Data());
          Print("pid");
          return -1 ;
        }

      default:
        Error("GetDaughterParticleId", "Currently, >= 3 body decays are not implemented yet.");
        return -1 ;
    }

    return -1 ;
  }

  //__________________________________________________________________________________________
  Int_t GetCategoryId(const TString name)
  {
    // Get category id from node name in the minimc tree
    //  - Input node name is case insensitive
    //     For example, mc, mC, Mc and MC should work
 
    TString _name(name);
    _name.ToLower() ;
 
    if ( _name.Contains("mc") )           return 0;
    else if ( _name.Contains("matched") ) return 1;
    else if ( _name.Contains("ghost") )   return 2;
    else if ( _name.Contains("contam") )  return 3;
    else if ( _name.Contains("matglob") ) return 4;
    else if ( _name.Contains("primary") ) return 5;
    else if ( _name.Contains("global") )  return 6;
    else{
      Error("GetCategoryId", "Unknown node name, name = %15s", name.Data());
      cout << endl ;
      cout << "#====================================================================================================" << endl;
      cout << "  Current implemented nodes" << endl;
      cout << "----------------------------------------------------------------------------------------------------" << endl;
      cout << "Input          node                            node id                                              " << endl;
      cout << "----------------------------------------------------------------------------------------------------" << endl;
      cout << "MC           Monte Carlo tracks                  0     " << endl;
      cout << "MATCHED      Matched pairs                       1     " << endl;
      cout << "GHOST        Ghost pairs                         2     " << endl;
      cout << "CONTAM       Contaminated pairs                  3     " << endl;
      cout << "MATGLOB      Matched global pairs                4     " << endl;
      cout << "PRIMARY      Primary tracks (real data)          5     " << endl;
      cout << "GLOBAL       Global tracks (real data)           6     " << endl;
      cout << "#====================================================================================================" << endl;
      cout << "  NOTE : Input particle name is case insensitive" << endl;
      cout << "  NOTE : You can put mc, mC, Mc and MC for MC tracks, for example" << endl;
      cout << endl;
    }

    return -1 ;
  }

  //__________________________________________________________________________________________
  Category GetCategory(const UInt_t id)
  {
    // Get category from category id
    if( id >= kNEmbedding ){
      Error("GetCategory", "Unknown category id, id=%3d. Return MC.", id);
      return kCategory[0] ;
    }

    return kCategory[id] ;
  }

  //__________________________________________________________________________________________
  TString GetCategoryName(const UInt_t id)
  {
    // Get category name from category id
    if( id >= kNCategory ){
      Error("GetCategoryName", "Unknown category id, id=%3d", id);
      return "";
    }

    return kCategoryName[id] ;
  }

  //__________________________________________________________________________________________
  TString GetCategoryTitle(const UInt_t id)
  {
    // Get category title from category id
    if( id >= kNCategory ){
      Error("GetCategoryTitle", "Unknown category id, id=%3d", id);
      return "";
    }

    return kCategoryTitle[id] ;
  }

  //__________________________________________________________________________________________
  Double_t GetMass(const Int_t particleId)
  {
    // Get mass from particle id (GeV/c^2)
 
    const Double_t mev2gev = 1.0e-03 ;
 
    switch( particleId ){
      // GeV
      case 1   : return    0.0      * mev2gev; // photon
      case 2   : return    0.510998 * mev2gev; // positron
      case 3   : return    0.510998 * mev2gev; // electron
      case 7   : return  134.9766   * mev2gev; // pi0
      case 8   : return  139.57018  * mev2gev; // pion+
      case 9   : return  139.57018  * mev2gev; // pion-
      case 11  : return  493.677    * mev2gev; // kaon+
      case 12  : return  493.677    * mev2gev; // kaon-
      case 14  : return  938.27203  * mev2gev; // proton
      case 15  : return  938.27203  * mev2gev; // anti-proton
      case 37  : return 1864.84     * mev2gev; // D0
      case 38  : return 1864.84     * mev2gev; // D0bar
      case 50  : return 1019.455    * mev2gev; // phi
      case 160 : return 3096.916    * mev2gev; // jpsi
      case 995 : return 1519.5      * mev2gev; // lambda(1520)
      default:
        Error("GetMass", "Unknown particle id, id=%3d. set mass = 0", particleId);
        return 0.0 ;
    }
 
    return -1 ;
  }

  //__________________________________________________________________________________________
  Double_t GetMass2(const Int_t particleId)
  {
    // Get mass square from particle id (GeV^2/c^4)

    const Double_t mass = GetMass(particleId) ;
    return mass * mass ;
  }

  //__________________________________________________________________________________________
  Double_t GetMass2Daughter(const Int_t particleId, const Int_t daughter)
  {
    // Get mass square from particle id for daughter particles (GeV^2/c^4)

    const TString parentName(GetParticleName(particleId, kFALSE)); // Parent particle name
    const Int_t daughterId = GetDaughterParticleId(parentName, daughter); // Daughter particle id

    // return 0.0 if daughterId = -1 (i.e. no decay daughters)
    if ( daughterId == -1 ) return 0.0 ;

    return GetMass2(daughterId);
  }

  //__________________________________________________________________________________________
  TString GetParticleName(const Int_t particleId, const Bool_t isTex)
  {
    // Get particle name from particle id (latex format)
    if( isTex ){
      switch( particleId ){
        case 1   : return "#gamma" ;       // photon
        case 2   : return "e^{+}" ;        // positron
        case 3   : return "e^{-}" ;        // electron
        case 7   : return "#pi^{0}" ;      // pi0
        case 8   : return "#pi^{+}" ;      // pion+
        case 9   : return "#pi^{-}" ;      // pion-
        case 11  : return "K^{+}" ;        // kaon+
        case 12  : return "K^{-}" ;        // kaon-
        case 14  : return "p" ;            // proton
        case 15  : return "#bar{p}" ;      // anti-proton
        case 37  : return "D_{0}" ;        // D0
        case 38  : return "#bar{D_{0}}" ;  // D0bar
        case 50  : return "#phi" ;         // phi
        case 160 : return "J/#Psi" ;       // jpsi
        case 995 : return "#Lambda(1520)"; // lambda(1520)
        default:
          Error("GetParticleName", "Unknown particle id, id=%3d. return nothing", particleId);
          return "";
      }
    }
    else{ // text format
      switch( particleId ){
        case 1   : return "Photon" ;     // photon
        case 2   : return "EPlus" ;      // positron
        case 3   : return "EMinus" ;     // electron
        case 7   : return "Pi0" ;        // pi0
        case 8   : return "PiPlus" ;     // pion+
        case 9   : return "PiMinus" ;    // pion-
        case 11  : return "KPlus" ;      // kaon+
        case 12  : return "KMinus" ;     // kaon-
        case 14  : return "Proton" ;     // proton
        case 15  : return "PBar" ;       // anti-proton
        case 37  : return "D0" ;         // D0
        case 38  : return "D0Bar" ;      // D0bar
        case 50  : return "Phi" ;        // phi
        case 160 : return "JPsi" ;       // jpsi
        case 995 : return "Lambda1520" ; // lambda(1520)
        default:
          Error("GetParticleName", "Unknown particle id, id=%3d. return nothing", particleId);
          return "";
      }
    }
 
    return "" ;
  }

  //__________________________________________________________________________________________
  Int_t GetNDaughter(const Int_t particleId)
  {
    // Get number of daughters from particle id

    switch( particleId ){
      case 1   : return 0 ; // photon
      case 2   : return 0 ; // positron
      case 3   : return 0 ; // electron
      case 7   : return 0 ; // pi0
      case 8   : return 0 ; // pion+
      case 9   : return 0 ; // pion-
      case 11  : return 0 ; // kaon+
      case 12  : return 0 ; // kaon-
      case 14  : return 0 ; // proton
      case 15  : return 0 ; // anti-proton
      case 37  : return 2 ; // D0
      case 38  : return 2 ; // D0bar
      case 50  : return 2 ; // phi
      case 160 : return 2 ; // jpsi
      case 995 : return 2 ; // lambda(1520)
      default:
        Error("GetNDaghter", "Unknown particle id, id=%3d. return 0", particleId);
        return 0;
    }

    return 0;
  }

  //__________________________________________________________________________________________
  Int_t GetCharge(const Int_t particleId)
  {
    // Get charge

    switch( particleId ){
      case 1   : return  0 ; // photon
      case 2   : return  1 ; // positron
      case 3   : return -1 ; // electron
      case 7   : return  0 ; // pi0
      case 8   : return  1 ; // pion+
      case 9   : return -1 ; // pion-
      case 11  : return  1 ; // kaon+
      case 12  : return -1 ; // kaon-
      case 14  : return  1 ; // proton
      case 15  : return -1 ; // anti-proton
      case 37  : return  0 ; // D0
      case 38  : return  0 ; // D0bar
      case 50  : return  0 ; // phi
      case 160 : return  0 ; // jpsi
      case 995 : return  0 ; // lambda(1520)
      default:
        Error("GetCharge", "Unknown particle id, id=%3d. return 1", particleId);
        return 1 ;
    }

    return 1 ;
  }

  //__________________________________________________________________________________________
  void Print(const TString option)
  {
    // Print
 
    TString _option(option);
    _option.ToLower() ;
 
    if( _option.Contains("pid") ){
      cout << endl ;
      cout << "#====================================================================================================" << endl;
      cout << "  Current implemented particles" << endl;
      cout << "----------------------------------------------------------------------------------------------------" << endl;
      cout << "Input                    particle        particle id            decay daughters" << endl;
      cout << "----------------------------------------------------------------------------------------------------" << endl;
      cout << "photon (or gamma)        Photon               1                        -       " << endl;
      cout << "eplus (or e+)            Positron             2                        -       " << endl;
      cout << "eminus (or e-)           Electron             3                        -       " << endl;
      cout << "pi0                      Neutral Pion         7                        -       " << endl;
      cout << "piplus (or pi+)          Pion +               8                        -       " << endl;
      cout << "piminus (or pi-)         Pion -               9                        -       " << endl;
      cout << "kplus (or k+)            Kaon +              11                        -       " << endl;
      cout << "kminus (or k-)           Kaon -              12                        -       " << endl;
      cout << "proton (or p)            Proton              14                        -       " << endl;
      cout << "antiproton (or pbar)     Anti-Proton         15                        -       " << endl;
      cout << "D0                       D0                  37                     pi+ / K-   " << endl;                   
      cout << "D0bar                    D0 bar              38                     K+ / pi-   " << endl;
      cout << "phi                      Phi                 50                     K+ / K-    " << endl;
      cout << "jpsi                     J/Psi              160                     e+ / e-    " << endl;
      cout << "lambda1520               Lambda(1520)       995                     p / K-     " << endl;
      cout << "#====================================================================================================" << endl;
      cout << "  NOTE : Input particle name is case insensitive" << endl;
      cout << "  NOTE : You can put eplus, Eplus, ePlus or E+ etc for electrons" << endl; 
      cout << endl;
    }
  }

}


