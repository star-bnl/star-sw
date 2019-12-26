/*
 * This file is part of KFParticle package
 * Copyright (C) 2007-2019 FIAS Frankfurt Institute for Advanced Studies
 *               2007-2019 Goethe University of Frankfurt
 *               2007-2019 Ivan Kisel <I.Kisel@compeng.uni-frankfurt.de>
 *               2007-2019 Maksym Zyzak
 *
 * KFParticle is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * KFParticle is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */


#ifndef KFParticleDatabase_H
#define KFParticleDatabase_H

#include "Vc/Vc"

/** @class KFParticleDatabase
 ** @brief The class stores information about particle masses and expected width of the peacks.
 ** @author  M.Zyzak, I.Kisel
 ** @date 05.02.2019
 ** @version 1.0
 **
 ** The class contains information required for the reconstruction of decay trees: \n
 ** - masses of the stable particles; \n
 ** - masses of the intermediate daughter particles (like Lambda, K0, Xi, D0, etc.);\n
 ** - expected widths of the peaks.
 **/

class KFParticleDatabase
{
 public:
  KFParticleDatabase();

  virtual ~KFParticleDatabase() {};

  float GetMass(const int pdg) const
  {
    /** Returns scalar float variable with the mass of the stable particle with the given PDG code.
     ** If the given PDG code is not the list of the current database mass of the pion is returned.
     ** \param[in] pdg - the input PDG code
     **/
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
    /** Returns vector float variable with the mass of the stable particles with the given PDG codes.
     ** If the given PDG code is not in the list of the current database mass of the pion is returned.
     ** \param[in] pdg - the input PDG codes of a set of particles in the SIMD-vector format
     **/
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
    /** Returns vector float variable with the mass of the short-lived particles with the given PDG codes
     ** and the expected widths of the corresponding peaks.
     ** If the given PDG code is not in the list of the current database mass of K0s is returned.
     ** \param[in] pdg - the input PDG code
     ** \param[out] massMotherPDG - the output table mass for the given PDG code
     ** \param[out] massMotherPDGSigma - expected width of the corresponding peak
     **/
    
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
    /** Returns scalar float variables with the mass of the short-lived particle  with the given PDG code
     ** and the expected width of the corresponding peak.
     ** If the given PDG code is not in the list of the current database mass of K0s is returned.
     ** \param[in] pdg - the input PDG code
     ** \param[out] massMotherPDG - the output table mass for the given PDG code
     ** \param[out] massMotherPDGSigma - expected width of the corresponding peak
     **/
        
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
  
  const float& GetPi0Mass() const { return fMassPi0PDG; }                ///< Returns the table PDG pi0 mass.
  const float& GetPi0MassSigma() const { return fMassPi0PDGSigma; }      ///< Returns expected width of the pi0 peak.
  const float& GetD0Mass() const { return fMassD0PDG; }                  ///< Returns the table PDG D0 mass.
  const float& GetD0MassSigma() const { return fMassD0PDGSigma; }        ///< Returns expected width of the D0 peak.
  const float& GetDPlusMass() const { return fMassDPlusPDG; }            ///< Returns the table PDG D+ mass.
  const float& GetDPlusMassSigma() const { return fMassDPlusPDGSigma; }  ///< Returns expected width of the D+ peak.
  
  static const KFParticleDatabase* Instance() { return fDatabase; }     ///< Returns a pointer to the singleton object.

 private:
  /** Table PDG masses of particles, which can be registered by the tracking detector directly: \n
   ** [ 0] - electron; \n [ 1] - muon; \n [ 2] - pion; \n [ 3] - kaon; \n [ 4] - proton; \n [ 5] - deutron; \n [ 6] - triton; \n [ 7] - He3; \n
   ** [ 8] - He4; \n [ 9] - Sigma-; \n [10] - Sigma+; \n [11] - Xi; \n [12] - Omega. */
  float fMass[13];

  /** Table PDG masses of short-lived particles, which are used for reconstruction of the decay trees: \n
   ** [ 0] - K0s; \n [ 1] - Lambda; \n [ 2] - Xi; \n [ 3] - gamma; \n [ 4] - Omega; \n [ 5] - H3Lambda; \n [ 6] - He4Lambda; \n [ 7] - He5Lambda. */
  float fMassSecPDG[8];
  /** Expected widths of peaks of short-lived particles, which are used for reconstruction of the decay trees: \n
   ** [ 0] - K0s; \n [ 1] - Lambda; \n [ 2] - Xi; \n [ 3] - gamma; \n [ 4] - Omega; \n [ 5] - H3Lambda; \n [ 6] - He4Lambda; \n [ 7] - He5Lambda. */
  float fMassSecPDGSigma[8];
  
  float fMassPi0PDG;          ///< Table mass of pi0
  float fMassPi0PDGSigma;     ///< Expected width of the pi0 peak

  float fMassD0PDG;           ///< Table mass of D0 
  float fMassD0PDGSigma;      ///< Expected width of the D0 peak
  float fMassDPlusPDG;        ///< Table mass of D+ 
  float fMassDPlusPDGSigma;   ///< Expected width of the D+ peak
  
  static KFParticleDatabase* fDatabase; ///< A singleton object.
};

#endif
