#include "StRoot/StarRoot/KFParticle.h"
#include "StRoot/StarRoot/MTrack.h"
#include "StRoot/StarRoot/MVertex.h"
#include "TDatabasePDG.h"
#include "TParticlePDG.h"

#include <fstream>
#include "Riostream.h"
#include <iomanip>
#include <cstdio>
#include <cstring>
using namespace std;
//________________________________________________________________________________
void KFParticleK0() {
  cout << "Try different constructors" << endl<< endl;
  cout << "1. Construction from Vertex" << endl<< endl;
  MVertex vert;
  vert.SetXYZ(0.0, 0.0, 10.0);
  vert.SetCovarianceMatrix( 0.01,
                            0.00,  0.01,
                            0.00,  0.00, 0.01 );
  vert.SetNContributors(2);
  vert.SetChi2(1.01);
  
  KFParticle p1(vert);
  p1.SetID(1);
  cout << "	Vertex	Particle p1" << endl << p1 << endl;
  //  p1.Print("All");
  ///  *****************************************************************************************
    KFParticle::SetField(+4.9797992706298828); cout << "Set Field " <<  KFParticle::GetFieldAlice() << endl;
  cout << "2. Construction from Track" << endl<< endl;
  
  MTrack track;
  track.SetParameters(-0.061996019110347252, -1.3579236865955473,   27.147283554077148, 
    		       0.62539337626870062,  -0.028552340672283318, -0.18467358509984011);
  Double_t C[21]= {3.3055800809774214e-05, 
		   0.00098316976438185002,    0.04740889543423539, 
		  -8.5596097466772512e-05, -0.0037516094381694971,    0.032156504690647125, 
		  -2.2812597903705375e-05, -0.0012121012247057524,  3.0646383360925928e-05, 6.1388628418184652e-05, 
		  -4.4071909055788304e-06,-0.00048870318030618627,  3.8062554692505919e-05, 1.2177141510445709e-05, 7.6900178535210476e-06, 
		   6.6224441962932268e-06, 0.00034363110217286891, -0.00031520420397528146,-1.6277704753223909e-05,-3.4322154557097545e-06, 1.027411488502718e-05};
  track.SetCovarianceMatrix(C);
#if 1
  track.SetNDF(1);
  track.SetChi2(1.5);
#endif
  track.SetCharge(-1);
  
  KFParticle p2(track, -211);
  p2.SetID(15);
  
  cout << "	Track	Particle p2" << endl << p2 << endl;
  //  p2.Print("All");
  
  ///  *****************************************************************************************
  
  cout << "3. Now we will create one more particle from track and call the construction from these 2 particles" << endl<< endl;
  
  MTrack track2;
  track2.SetParameters(-0.20371287092090862,   3.0678058943547839,  -19.93988037109375, 
		        0.37533048135363339, 0.024923235867488316, 0.19031024520542122);
  Double_t C2[21]=
    { 0.00022312908970259721, 
     -0.00064291160449645151,   0.089331037457232143, 
      0.00047880877483649206,  -0.045478494677353445,     0.11199165135622025, 
      4.6362085390124077e-07, 0.00070978326424729935, -0.00014164977426380486, 1.7553871209443515e-05, 
     -2.2044831998838091e-05,-0.00059994741249631909,  0.00030148707952079015,-4.6574515272730461e-06, 7.2618497455845866e-06, 
     -1.2427988441207971e-06, 0.00030830063771211896, -0.00061853865528922161, 5.4390968700069889e-06,-1.9914477627292868e-06, 8.9837108094398403e-06};

  track2.SetCovarianceMatrix(C2);
#if 1
  track2.SetNDF(2);
  track2.SetChi2(2.5);
#endif
  track2.SetCharge(+1);
  KFParticle p3(track2, 211); // PDG = 11
  p3.SetID(25);
  cout << "	Particle p3" << endl << p3 << endl;
  //  p3.Print("All");
  
  KFParticle p4(p2, p3);
  p4.SetID(200);
  cout << "	Particle p4(p2,p3)" << endl << p4 << endl;
  //  p4.Print("All");
  ///  *****************************************************************************************
  cout << "4. Construction with constrained Mass or (and) vertex position values" << endl<< endl;

/// This is the example of the KFParticleBase::Construct function usage.
/// parameter 1 - array of the daughter particles
/// parameter 2 - number of the daughter particles
/// parameter 3 - vertex (it should be the object of the KFParticle class)
/// parameter 4 - the value we force the particle mass to be equial to.

  const KFParticle pVertex = p1;
  int NDaughters = 2;
  const KFParticle *vDaughters[2] = {&p2, &p3};
  Double_t Mass = TDatabasePDG::Instance()->GetParticle(310)->Mass();
  cout << "4.1 Construction with constrained Mass, without vertex hypothesis " << endl<< endl;
/// we assume Mass to be the mass of the constructed particle
  KFParticle K0;
  K0.SetID(201);
  K0.Construct(vDaughters,NDaughters,0,Mass,0);
  p2.SetParentID(K0.GetID()); p3.SetParentID(K0.GetID());
  cout << "Dauthers" << endl
	    << *vDaughters[0] << endl
	    << *vDaughters[1] << endl
	    << "Mass    " << Mass << endl;
  cout << "	Particle K0" << endl << K0 << endl;
  //  vDaughters[0]->Print("All");
  //  vDaughters[1]->Print("All");
  //  K0.Print("All");
  K0.SetProductionVertex(pVertex);
  cout << " Add parent Vertex" << endl;
  cout << " K0 with vertex" << K0 << endl;
  cout << "4.2 Construction without constrained Mass, with vertex hypothesis " << endl<< endl;
/// we assume p1 to be the vertex of the constructed particle
  KFParticle K0_1;
  K0_1.SetID(201);
  K0_1.Construct(vDaughters,NDaughters,&pVertex,-1,0);
  p2.SetParentID(K0_1.GetID()); p3.SetParentID(K0_1.GetID());
  cout << "Dauthers" << endl
	    << *vDaughters[0] << endl
	    << *vDaughters[1] << endl
	    << pVertex        << endl;
  cout << "	Particle K0_1" << endl << K0_1 << endl;
  //  vDaughters[0]->Print("All");
  //  vDaughters[1]->Print("All");
  //  K0_1.Print("All");
  cout << "4.3 Construction with constrained Mass, with vertex hypothesis " << endl<< endl;
///we assume p1 to be the vertex of the constructed particle, Mass to be the mass of the constructed particle
  KFParticle K0_2;
  K0_2.SetID(202);
  K0_2.Construct(vDaughters,NDaughters,&pVertex,Mass,0);
  p2.SetParentID(K0_2.GetID()); p3.SetParentID(K0_2.GetID());
  cout << "Dauthers" << endl
	    << *vDaughters[0] << endl
	    << *vDaughters[1] << endl
	    << pVertex        << endl
	    << "Mass    " << Mass << endl;
  cout << "K0_2" << endl <<  K0_2 << endl;
#if 1
  //  vDaughters[0]->Print("All");
  //  vDaughters[1]->Print("All");
  //  K0_2.Print("All");
  cout << "4.4 Construction K0_3(p2,p3) without constrained Mass, without vertex hypothesis " << endl<< endl;
///we assume p1 to be the vertex of the constructed particle, Mass to be the mass of the constructed particle
  KFParticle K0_3;
  K0_3.SetID(203);
  K0_3.Construct(vDaughters,NDaughters,0,-1,0);
  p2.SetParentID(K0_3.GetID()); p3.SetParentID(K0_3.GetID());
  cout << "Dauthers" << endl
       << *vDaughters[0] << endl
       << *vDaughters[1] << endl;
  cout << "K0_3" << endl <<  K0_3 << endl;
  //  vDaughters[0]->Print("All");
  //  vDaughters[1]->Print("All");
  //  K0_3.Print("All");
#endif
}
