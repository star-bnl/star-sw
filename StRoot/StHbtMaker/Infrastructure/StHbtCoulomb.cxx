// This is a Coulomb correction class which
// 1. Reads in the dat from a file
// 2. Performs a linear interpolation in R and creates any array of interpolations
// 3. Interpolates in eta and returns the Coulomb correction to user
//
// - Randy Wells, OSU
//////////////////////////////////////////////////////////////////////////////////

#include "StHbtMaker/Infrastructure/StHbtCoulomb.h"
#include <fstream.h>
#include <cstdlib>
#include <stdio.h>
#include <cassert>
#include "PhysicalConstants.h"

ClassImp(StHbtCoulomb)

StHbtCoulomb::StHbtCoulomb() {
  mFile = "/afs/rhic/star/hbt/coul/StHbtCorrectionFiles/correctionpp.dat";
  if (!mFile) {
    cout << " No file, dummy!" << endl;
    assert(0);
  }
  mRadius = -1.0;
  cout << "You have 1 default Coulomb correction!" << endl;
}

StHbtCoulomb::StHbtCoulomb(const char *readFile,
			   const double& radius) {
  mFile = readFile;
  mRadius = radius;
  CreateLookupTable(mRadius);
  cout << "You have 1 Coulomb correction!" << endl;
}

StHbtCoulomb::~StHbtCoulomb() {

}

void StHbtCoulomb::SetRadius(const double& radius) {
  cout << " StHbtCoulomb::setRadius() " << endl;
  mRadius = radius;
  CreateLookupTable(mRadius);
}

double StHbtCoulomb::GetRadius() {
  return (mRadius);
}

void StHbtCoulomb::SetFile(const char *readFile) {
  cout << " StHbtCoulomb::SetFile() " << endl;
  if (*mFile!=*readFile) {
    mFile = readFile;
    // Create new lookup table since file has changed
    if (mRadius>0.0) {
      CreateLookupTable(mRadius);
    }
  }
}

void StHbtCoulomb::CreateLookupTable(const double& radius) {
  cout << " StHbtCoulomb::CreateLookupTable() " << endl;
  // Read radii from mFile
  // Create array(pair) of linear interpolation between radii

  if (radius<0.0) {
    cout << " StHbtCoulomb::CreateLookupTable -> NEGATIVE RADIUS " << endl;
    cout << "  call StHbtCoulomb::SetRadius(r) with positive r " << endl;
    cerr << " StHbtCoulomb::CreateLookupTable -> NEGATIVE RADIUS " << endl;
    cerr << "  call StHbtCoulomb::SetRadius(r) with positive r " << endl;
    assert(0);
  }
  ifstream mystream(mFile);
  if (!mystream) {
    cout << "Could not open file" << endl;
    assert(0);
  }
  else {
    cout << "Input correction file opened" << endl;
  }

  char tempstring[2001];
  float radii[2000];
  int NRadii = 0;
  if (!mystream.getline(tempstring,2000)) {
    cout << "Could not read radii from file" << endl;
    assert(0);
  }
  for (int ii=0; ii<strlen(tempstring); ii++) {
    while (tempstring[ii]==' ') ii++;
    sscanf(&tempstring[ii++],"%f",&radii[++NRadii]);
    while ( tempstring[ii]!=' ' && (ii)<strlen(tempstring) )ii++;
  }
  cout << " Read " << NRadii << " radii from file" << endl;

  double LowRadius = -1.0;
  double HighRadius = -1.0;
  int LowIndex = 0;
  for(int iii=1; iii<=NRadii-1; iii++) { // Loop to one less than #radii
    if ( radius > radii[iii] && radius < radii[iii+1] ) {
      LowRadius = radii[iii];
      HighRadius = radii[iii+1];
      LowIndex = iii;
    }
  }
  if ( (LowRadius < 0.0) || (HighRadius < 0.0) ) {
    cout << "StHbtCoulomb::CreateLookupTable --> Problem interpolating radius" << endl;
    cout << "  Check range of radii in lookup file...." << endl;
    cerr << "StHbtCoulomb::CreateLookupTable --> Problem interpolating radius" << endl;
    cerr << "  Check range of radii in lookup file...." << endl;
    assert(0);
  }

  double corr[100];           // array of corrections ... must be > NRadii
  mNLines = 0;
  double tempEta = 0;
  while (mystream >> tempEta) {
    for (int i=1; i<=NRadii; i++) {
      mystream >> corr[i];
    }
      double LowCoulomb = corr[LowIndex];
      double HighCoulomb = corr[LowIndex+1];
      double nCorr = ( (radius-LowRadius)*HighCoulomb+(HighRadius-radius)*LowCoulomb )/(HighRadius-LowRadius);
      mEta[mNLines] = tempEta;     // Eta
      mCoulomb[mNLines] = nCorr;   // Interpolated Coulomb correction for radius
      mNLines++;
  }
  mystream.close();
  cout << "Lookup Table is created with " << mNLines << " points" << endl;
}

double StHbtCoulomb::CoulombCorrect(const double& eta) {
  // Interpolates in eta
  if (mRadius < 0.0) {
    cout << "StHbtCoulomb::CoulombCorrect(eta) --> Trying to correct for negative radius!" << endl;
    cerr << "StHbtCoulomb::CoulombCorrect(eta) --> Trying to correct for negative radius!" << endl;
    assert(0);
  }
  int middle=int(mNLines/2);
  if (eta*mEta[middle]<0.0) {
    cout << "StHbtCoulomb::CoulombCorrect(eta) --> eta: " << eta << " has wrong sign for data file! " << endl;
    cerr << "StHbtCoulomb::CoulombCorrect(eta) --> eta: " << eta << " has wrong sign for data file! " << endl;
    assert(0);
  }

  double Corr = -1.0;
  
  if ( (eta>mEta[0]) && (mEta[0]>0.0) ) Corr = mCoulomb[0];
  if ( (eta<mEta[mNLines]) && (mEta[mNLines]<0.0) ) Corr = mCoulomb[mNLines];
  int ii = 0;
  while ( (ii<(mNLines-1)) && (Corr<0.0) ) { // Loop until one from the last in file
    if ( (eta < mEta[ii]) && (eta > mEta[ii+1]) ) { // Eta is decreasing in file
      double LowEta = mEta[ii];
      double HighEta = mEta[ii+1];    
      double LowCoulomb = mCoulomb[ii];
      double HighCoulomb = mCoulomb[ii+1];
      //      cout << LowEta << " *** Eta *** " << HighEta << endl;
      //      cout << LowCoulomb << " *** Coulomb *** " << HighCoulomb << endl;
      Corr = ( (eta-LowEta)*HighCoulomb+(HighEta-eta)*LowCoulomb )/(HighEta-LowEta);
    }
    ii++;
  }
  if (Corr<0.0) {
    cout << "StHbtCoulomb::CoulombCorrect(eta) --> No correction" << endl;
    cout << "  Check range of eta in file: Input eta  " << eta << endl;
    cerr << "StHbtCoulomb::CoulombCorrect(eta) --> No correction" << endl;
    cerr << "  Check range of eta in file: Input eta  " << eta << endl;
    assert(0);
  } 
  return (Corr);

}

double StHbtCoulomb::CoulombCorrect(const double& eta,
				    const double& radius) {
  // Checks radii ... input radius and mRadius
  // Calls createLookupTable if neccessary
  // Interpolate(linear) between etas in the created lookup table

  if (radius < 0.0) {
    if (mRadius < 0.0) {
      // Both radii are negative
      cout << "StHbtCoulomb::CoulombCorrect(eta,r) --> input and member radii are negative!" << endl;
      cerr << "StHbtCoulomb::CoulombCorrect(eta,r) --> input and member radii are negative!" << endl;
      assert(0);
    }
  }
  else {
    // radius > 0.0
    if (radius == mRadius) {
      // Both radii are positive and equal
      //      cout << "Radii are the same!!!" << endl;
    }
    else {
      // Both radii are positive but not equal
      mRadius = radius;
      CreateLookupTable(mRadius);
    }
  }

  // Interpolate in eta
  double correction = CoulombCorrect(eta);
  return (correction);
}

double StHbtCoulomb::CoulombCorrect(const double& Z1Z2, const double& mass1,
				    const double& mass2, const double& Qinv) {
  double eta2 = Eta(Z1Z2,mass1,mass2,Qinv);
  double correction = CoulombCorrect(eta2);
  return (correction);
}

double StHbtCoulomb::CoulombCorrect(const double& Z1Z2, const double& mass1,
				    const double& mass2, const double& Qinv,
				    const double& radius) {
  double eta2 = Eta(Z1Z2,mass1,mass2,Qinv);
  double correction = CoulombCorrect(eta2,radius);
  return (correction);
}


double StHbtCoulomb::CoulombCorrect(const StHbtPair* pair, const double& charge) {
  double mass1 = pair->track1()->FourMomentum().m();
  double mass2 = pair->track2()->FourMomentum().m();
  double Qinv = fabs(pair->qInv());
  double eta2 = Eta(charge,mass1,mass2,Qinv);
  double correction = CoulombCorrect(eta2);
  return (correction);
}

double StHbtCoulomb::CoulombCorrect(const StHbtPair* pair, const double& charge,
				    const double& radius) {
  double mass1 = pair->track1()->FourMomentum().m();
  double mass2 = pair->track2()->FourMomentum().m();
  double Qinv = fabs(pair->qInv());
  double eta2 = Eta(charge,mass1,mass2,Qinv);
  double correction = CoulombCorrect(eta2,radius);
  return (correction);
}

double StHbtCoulomb::Eta(const double& Z1Z2, const double& mass1,
			 const double& mass2, const double& Qinv) {
  double reducedMass = mass1*mass2/(mass1+mass2);
  double temp = Z1Z2*reducedMass*fine_structure_const/(Qinv/2.0);
  return (temp);
}
