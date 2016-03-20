//----------------------------------------------------------------------------
// Implementation of the KFParticle class
// .
// @author  I.Kisel, I.Kulakov, M.Zyzak
// @version 1.0
// @since   20.08.13
// 
// 
//  -= Copyright &copy ALICE HLT and CBM L1 Groups =-
//____________________________________________________________________________


#ifndef KFParticleDatabase_H
#define KFParticleDatabase_H

#include "Vc/Vc"

class KFParticleDatabase
{
 public:
  KFParticleDatabase();

  ~KFParticleDatabase() {};

  float GetMass(const int pdg) const
  {
    int pdgIndex = 2;
    switch ( abs(pdg) )
    {
      case         11: pdgIndex = 0; break;
      case         13: pdgIndex = 1; break;
      case         19: pdgIndex = 1; break;
      case        211: pdgIndex = 2; break;
      case        321: pdgIndex = 3; break;
      case       2212: pdgIndex = 4; break;
      case 1000010020: pdgIndex = 5; break;
      case 1000010030: pdgIndex = 6; break;
      case 1000020030: pdgIndex = 7; break;
      case 1000020040: pdgIndex = 8; break;
      case       3112: pdgIndex = 9; break;
      case       3222: pdgIndex = 10; break;
      case       3312: pdgIndex = 11; break;
      case       3334: pdgIndex = 12; break;
      default:   pdgIndex = 2; break;
    }
    
    return fMass[pdgIndex];
  }

  Vc::float_v GetMass(const Vc::int_v& pdg) const
  {
    Vc::int_v pdgIndex(2);
    pdgIndex(Vc::abs(pdg) ==         11) = 0;    
    pdgIndex(Vc::abs(pdg) ==         13) = 1;
    pdgIndex(Vc::abs(pdg) ==         19) = 1;
    pdgIndex(Vc::abs(pdg) ==        211) = 2;
    pdgIndex(Vc::abs(pdg) ==        321) = 3;
    pdgIndex(Vc::abs(pdg) ==       2212) = 4;
    pdgIndex(Vc::abs(pdg) == 1000010020) = 5;
    pdgIndex(Vc::abs(pdg) == 1000010030) = 6;
    pdgIndex(Vc::abs(pdg) == 1000020030) = 7;
    pdgIndex(Vc::abs(pdg) == 1000020040) = 8;
    pdgIndex(Vc::abs(pdg) ==       3112) = 9;
    pdgIndex(Vc::abs(pdg) ==       3222) = 10;
    pdgIndex(Vc::abs(pdg) ==       3312) = 11;
    pdgIndex(Vc::abs(pdg) ==       3334) = 12;
    Vc::float_v mass(fMass, pdgIndex);
    return mass;
  }

  void GetMotherMass(const Vc::int_v& pdg, Vc::float_v& massMotherPDG, Vc::float_v& massMotherPDGSigma) const
  {
    Vc::int_v pdgIndex(0);
    pdgIndex(pdg ==  310) = 0;    
    pdgIndex(Vc::abs(pdg) == 3122) = 1;
    pdgIndex(Vc::abs(pdg) == 3312) = 2;
    pdgIndex(pdg == 22) = 3;
    pdgIndex(Vc::abs(pdg) == 3334) = 4;
    pdgIndex(Vc::abs(pdg) == 3004) = 5;
    pdgIndex(Vc::abs(pdg) == 3006) = 6;
    pdgIndex(Vc::abs(pdg) == 3007) = 7;
        
    massMotherPDG.gather(fMassSecPDG, pdgIndex);
    massMotherPDGSigma.gather(fMassSecPDGSigma, pdgIndex);
  }
  
  void GetMotherMass(const int pdg, float& massMotherPDG, float& massMotherPDGSigma) const
  {
    int pdgIndex = 2;
    switch ( abs(pdg) )
    {
      case  310: pdgIndex = 0; break;
      case 3122: pdgIndex = 1; break;
      case 3312: pdgIndex = 2; break;
      case   22: pdgIndex = 3; break;
      case 3334: pdgIndex = 4; break;
      case 3004: pdgIndex = 5; break;
      case 3006: pdgIndex = 6; break;
      case 3007: pdgIndex = 7; break;
      default:   pdgIndex = 0; break;
    }
   
    massMotherPDG = fMassSecPDG[pdgIndex];
    massMotherPDGSigma = fMassSecPDGSigma[pdgIndex];
  }
  
  const float& GetPi0Mass() const { return fMassPi0PDG; }
  const float& GetPi0MassSigma() const { return fMassPi0PDGSigma; }
  const float& GetD0Mass() const { return fMassD0PDG; }
  const float& GetD0MassSigma() const { return fMassD0PDGSigma; }
  const float& GetDPlusMass() const { return fMassDPlusPDG; }
  const float& GetDPlusMassSigma() const { return fMassDPlusPDGSigma; }
  
  static const KFParticleDatabase* Instance() { return fDatabase; }

 private:
  float fMass[13];

  float fMassSecPDG[8];
  float fMassSecPDGSigma[8];
  
  float fMassPi0PDG;
  float fMassPi0PDGSigma;

  float fMassD0PDG;
  float fMassD0PDGSigma;
  float fMassDPlusPDG;
  float fMassDPlusPDGSigma;
  
  static KFParticleDatabase* fDatabase;
};

#endif