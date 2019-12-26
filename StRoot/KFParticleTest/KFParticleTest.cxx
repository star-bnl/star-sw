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

#include "KFParticle.h"
#include "KFPTrack.h"
#include "KFPVertex.h"
#include "KFParticleSIMD.h"
#include "KFParticleTest.h"

#include <iostream>
#include <iomanip>
#include <cmath>

#ifndef KFParticleStandalone
ClassImp(KFParticleTest)
#endif

std::ostream&  operator<<(std::ostream& os, const KFParticleBase& particle) {
  static const char *vn[14] = {"x","y","z","px","py","pz","E","S","M","t","p","Q","Chi2","NDF"};

  for (Int_t i = 0; i < 8; i++) {
    if (i == 6) continue;                                    // E
    if (i == 7 && particle.GetParameter(i) <= 0.0) continue; // S
    if (particle.GetParameter(i) == 0. && particle.GetCovariance(i,i) == 0) continue;
    if (particle.GetCovariance(i,i) > 0) 
      os << " " << vn[i]<<": "<< std::setw(8) << particle.GetParameter(i)<< " +/- " << std::setw(6) << sqrt(particle.GetCovariance(i,i));
    else 
      os << " " << vn[i] << ": " << std::setw(8) << particle.GetParameter(i);
  }
  float Mtp[3], MtpErr[3];
  particle.GetMass(Mtp[0], MtpErr[0]);     if (MtpErr[0] < 1e-7 || MtpErr[0] > 1e10) MtpErr[0] = -13;
  particle.GetLifeTime(Mtp[1], MtpErr[1]); if (MtpErr[1] <=   0 || MtpErr[1] > 1e10) MtpErr[1] = -13;
  particle.GetMomentum(Mtp[2], MtpErr[2]); if (MtpErr[2] <=   0 || MtpErr[2] > 1e10) MtpErr[2] = -13;
  for (Int_t i = 8; i < 11; i++) {
    if (i == 9 && Mtp[i-8] <= 0.0) continue; // t
    if (MtpErr[i-8] > 0 && MtpErr[i-8] < 1e10) os << " " << vn[i] << ": " << std::setw(8) << Mtp[i-8] << " +/-" << std::setw(7) << MtpErr[i-8];
    else                                       os << " " << vn[i] << ": " << std::setw(8) << Mtp[i-8];
  }
  os << " pdg:" << std::setw(5) << particle.GetPDG() << " Q: "<< std::setw(2) << int(particle.GetQ()) << " chi2/NDF: " << std::setw(8) << particle.GetChi2() << "/" << std::setw(2) << particle.GetNDF();
  return os;
}

KFParticleTest::KFParticleTest():fMotherSingle(0),fMotherSIMD(0)
{
  fMotherSingle = new KFParticle();
  fMotherSIMD = new KFParticleSIMD();
}

KFParticleTest::~KFParticleTest()
{
  if(fMotherSingle) delete fMotherSingle;
  if(fMotherSIMD) delete fMotherSIMD;
}

void KFParticleTest::RunTest() 
{
  RunTestSingle();
  RunTestSIMD();
}

void KFParticleTest::RunTestSingle()
{
  std::cout.setf(std::ios::fixed);
  std::cout.setf(std::ios::showpoint);
  std::cout.precision(3);
    
  std::cout << "Try different constructors" << std::endl<< std::endl;
  std::cout << "1. Construction from Vertex" << std::endl<< std::endl;
  KFPVertex vert;
  vert.SetXYZ(0.0, 0.0, 10.0);
  vert.SetCovarianceMatrix( 0.01,
                            0.00,  0.01,
                            0.00,  0.00, 0.01 );
  vert.SetNContributors(2);
  vert.SetChi2(1.01);
  
  KFParticle p1(vert);
  std::cout << "Vertex Particle p1" << std::endl << "  " << p1 << std::endl;

  ///  *****************************************************************************************
  
#ifdef HomogeneousField  
  KFParticle::SetField(4.9797992706298828);
#endif
  float point[3]={0.f};
  float b[3] = {0.f};
  p1.GetFieldValue(point,b);
  std::cout << "Set Field " <<  std::setw(6) << b[2] << std::endl;
  
  std::cout << std::endl << "2. Construction from Track" << std::endl<< std::endl;
  
  KFPTrack track;
  track.SetParameters(-0.061996019110347252, -1.3579236865955473,   27.147283554077148, 
    		       0.62539337626870062,  -0.028552340672283318, -0.18467358509984011);
  float C[21]= {3.3055800809774214e-05, 
		   0.00098316976438185002,    0.04740889543423539, 
		  -8.5596097466772512e-05, -0.0037516094381694971,    0.032156504690647125, 
		  -2.2812597903705375e-05, -0.0012121012247057524,  3.0646383360925928e-05, 6.1388628418184652e-05, 
		  -4.4071909055788304e-06,-0.00048870318030618627,  3.8062554692505919e-05, 1.2177141510445709e-05, 7.6900178535210476e-06, 
		   6.6224441962932268e-06, 0.00034363110217286891, -0.00031520420397528146,-1.6277704753223909e-05,-3.4322154557097545e-06, 1.027411488502718e-05};
  track.SetCovarianceMatrix(C);
  track.SetNDF(1);
  track.SetChi2(1.5);
  track.SetCharge(-1);
  
  KFParticle p2(track, -211);
  
  std::cout << "Track Particle p2" << std::endl <<"  "<< p2 << std::endl;
  
  ///  *****************************************************************************************
  
  std::cout << std::endl << "3. Now we will create one more particle from track and call the construction from these 2 particles" << std::endl<< std::endl;
  
  KFPTrack track2;
  track2.SetParameters(-0.20371287092090862,   3.0678058943547839,  -19.93988037109375, 
		        0.37533048135363339, 0.024923235867488316, 0.19031024520542122);
  float C2[21]=
    { 0.00022312908970259721, 
     -0.00064291160449645151,   0.089331037457232143, 
      0.00047880877483649206,  -0.045478494677353445,     0.11199165135622025, 
      4.6362085390124077e-07, 0.00070978326424729935, -0.00014164977426380486, 1.7553871209443515e-05, 
     -2.2044831998838091e-05,-0.00059994741249631909,  0.00030148707952079015,-4.6574515272730461e-06, 7.2618497455845866e-06, 
     -1.2427988441207971e-06, 0.00030830063771211896, -0.00061853865528922161, 5.4390968700069889e-06,-1.9914477627292868e-06, 8.9837108094398403e-06};

  track2.SetCovarianceMatrix(C2);
  track2.SetNDF(2);
  track2.SetChi2(2.5);
  track2.SetCharge(+1);
  KFParticle p3(track2, 211); // PDG = 11
  std::cout << "Track Particle p3 " << std::endl <<"  "<< p3 << std::endl;
  
  KFParticle p4(p2, p3);
  std::cout << "Particle p4(p2,p3)" << std::endl << "  " << p4 << std::endl;

  ///  *****************************************************************************************
  std::cout << std::endl << "4. Construction with constrained Mass or (and) vertex position values" << std::endl<< std::endl;

/// This is the example of the KFParticleBase::Construct function usage.
/// parameter 1 - array of the daughter particles
/// parameter 2 - number of the daughter particles
/// parameter 3 - vertex (it should be the object of the KFParticle class)
/// parameter 4 - the value we force the particle mass to be equial to.

  const KFParticle pVertex = p1;
  int NDaughters = 2;
  const KFParticle *vDaughters[2] = {&p2, &p3};
  
  Double_t Mass = 0.497614;
  
  std::cout << "4.1 Construction with constrained Mass, without vertex hypothesis " << std::endl<< std::endl;
/// we assume Mass to be the mass of the constructed particle
  KFParticle K0;
  K0.Construct(vDaughters,NDaughters,0,Mass);
  std::cout << "Dauthers" << std::endl
	    << "  " << *vDaughters[0] << std::endl
	    << "  " << *vDaughters[1] << std::endl
	    << "Mass    " << Mass << std::endl;
  std::cout << "Particle K0" << std::endl << K0 << std::endl;
  K0.SetProductionVertex(pVertex);
  std::cout << " Add parent Vertex" << std::endl;
  std::cout << " K0 with vertex  " << K0 << std::endl;
  std::cout << std::endl << "4.2 Construction without constrained Mass, with vertex hypothesis " << std::endl<< std::endl;
/// we assume p1 to be the vertex of the constructed particle
  KFParticle K0_1;
  K0_1.Construct(vDaughters,NDaughters,&pVertex,-1);

  std::cout << "Dauthers" << std::endl
	    << "  " << *vDaughters[0] << std::endl
	    << "  " << *vDaughters[1] << std::endl
	    << "PV " << std::endl << "  " << pVertex        << std::endl;
  std::cout << "K0_1" << std::endl <<"  " << K0_1 << std::endl;
  std::cout << std::endl << "4.3 Construction with constrained Mass, with vertex hypothesis " << std::endl<< std::endl;
///we assume p1 to be the vertex of the constructed particle, Mass to be the mass of the constructed particle
  KFParticle K0_2;
  K0_2.Construct(vDaughters,NDaughters,&pVertex,Mass);

  std::cout << "Dauthers" << std::endl
	    << "  " << *vDaughters[0] << std::endl
	    << "  " << *vDaughters[1] << std::endl
	    << "PV " << std::endl << "  " << pVertex        << std::endl
	    << "Mass    " << Mass << std::endl;
  std::cout << "K0_2" << std::endl <<  "  " << K0_2 << std::endl;

  std::cout << std::endl << "4.4 Construction K0_3(p2,p3) without constrained Mass, without vertex hypothesis " << std::endl<< std::endl;
///we assume p1 to be the vertex of the constructed particle, Mass to be the mass of the constructed particle
  KFParticle K0_3;
  K0_3.Construct(vDaughters,NDaughters,0,-1);

  std::cout << "Dauthers" << std::endl
       << "  " << *vDaughters[0] << std::endl
       << "  " << *vDaughters[1] << std::endl;
  std::cout << "K0_3" << std::endl << "  " <<  K0_3 << std::endl;
}

void KFParticleTest::RunTestSIMD()
{
}

void KFParticleTest::CompareSingleAndSIMDResults()
{
}

void KFParticleTest::PrintTutorial()
{
}
