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

#include "KFParticleDatabase.h"

KFParticleDatabase* KFParticleDatabase::fDatabase = 0;

KFParticleDatabase::KFParticleDatabase():
  fMassPi0PDG(0.13498),
#ifdef PANDA_STT
  fMassPi0PDGSigma(0.009),
#elif defined ALICE_ITS
  fMassPi0PDGSigma(0.006),
#elif defined STAR_HFT
  fMassPi0PDGSigma(0.006),
#elif defined CBM
  fMassPi0PDGSigma(0.006),
#else 
  fMassPi0PDGSigma(0.006),
#endif
  fMassD0PDG(1.86484),
#ifdef CBM
  fMassD0PDGSigma(0.0145),
#else
  fMassD0PDGSigma(0.0154),
#endif
  fMassDPlusPDG(1.86962),
#ifdef CBM
  fMassDPlusPDGSigma(0.0145)
#else
  fMassDPlusPDGSigma(0.0115)
#endif
{
  /** The default constructor. Initialises masses and widths of the peaks. 
   ** Be aware, that widths of the peaks are experiment specific.
   **/
  fMass[ 0] = 0.5109989461E-03; // e
  fMass[ 1] = 0.1056583745;     // mu
  fMass[ 2] = 0.13957039;       // pi
  fMass[ 3] = 0.493677;         // K
  fMass[ 4] = 0.9382720813;     // p
  fMass[ 5] = 1.87561294257;    // d
  fMass[ 6] = 2.80892113298;    // t
  fMass[ 7] = 2.80839160743;    // He3
  fMass[ 8] = 3.7273794066;     // He4
  fMass[ 9] = 5.6055375;        // He6
  fMass[10] = 5.6015181;        // Li6
  fMass[11] = 6.5338336;        // Li7
  fMass[12] = 6.5341844;        // Be7
  fMass[13] = 1.197449;         // Sigma-
  fMass[14] = 1.18937;          // Sigma+
  fMass[15] = 1.32171;          // Xi
  fMass[16] = 1.67245;          // Omega

  fMassSecPDG[0] = 0.497614; //K0
  fMassSecPDG[1] = 1.115683; //Lambda
  fMassSecPDG[2] = 1.32171; //Xi  
  fMassSecPDG[3] = 0; //gamma
  fMassSecPDG[4] = 1.67245; //Omega
  fMassSecPDG[5] = 2.9924; //H3L
  fMassSecPDG[6] = 3.9216; //He4L
  fMassSecPDG[7] = 4.8392; //He5L
  
#ifdef PANDA_STT
  fMassSecPDGSigma[0]=12.0e-3; //K0 TODO tune
  fMassSecPDGSigma[1]=2.7e-3; //Lambda
  fMassSecPDGSigma[2]=2.8e-3; //Xi TODO tune
#elif defined ALICE_ITS
  fMassSecPDGSigma[0]=17.7e-3;
  fMassSecPDGSigma[1]=5.9e-3;
  fMassSecPDGSigma[2]=7.3e-3;
#elif defined STAR_HFT
  fMassSecPDGSigma[0]=17.7e-3;
  fMassSecPDGSigma[1]=5.9e-3;
  fMassSecPDGSigma[2]=7.3e-3;
#elif defined CBM
  fMassSecPDGSigma[0]=3.7e-3; //2.2e-3;
  fMassSecPDGSigma[1]=1.5e-3; //1.2e-3;
  fMassSecPDGSigma[2]=2.0e-3;  
#else //STAR
  fMassSecPDGSigma[0]=4.9e-3;
  fMassSecPDGSigma[1]=2.1e-3;
  fMassSecPDGSigma[2]=2.1e-3;
#endif
  fMassSecPDGSigma[3]=6.0e-3; //TODO tune //Gamma
  fMassSecPDGSigma[4]=2.1e-3; //Omega
  fMassSecPDGSigma[5]=3.0e-3; //H3L
  fMassSecPDGSigma[6]=3.0e-3; //He4L
  fMassSecPDGSigma[7]=3.0e-3; //He5L
  
  fDatabase = this;
}

KFParticleDatabase kfPartDatabase; //create instance
