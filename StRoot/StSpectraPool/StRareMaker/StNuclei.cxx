#include "StNuclei.h"
#include "StRareTrack.h"
#include "StEventTypes.h"
#include <stdlib.h>
ClassImp(StNuclei)

StNuclei::StNuclei(float plow,float phigh, int charge, int mass){
  pcut[0]    = plow;
  pcut[1]    = phigh;
  chargeToAccept = charge;
  minMass = mass;  
}

int StNuclei::Accept(StPrimaryTrack* trk){
  int iret = 0;
  int chargeOK = 0;
  int dedxOK = 0;
  int pOK=0;
  StRareTrack track(trk);
  if (chargeToAccept==0) {chargeOK = 1;}
  else if (chargeToAccept<0 && track.chargesign()<0) {chargeOK=1;}
  else if (chargeToAccept>0 && track.chargesign()>0) {chargeOK=1;}
  float dedxmin;
  int theCharge = 1;
  if (abs(chargeToAccept)>1) theCharge=2;
  float dedxExp =track.dedxExpected(minMass*0.939,theCharge); 
  dedxmin = 0.6*dedxExp; // within 40%
  float dedx = track.dedx();
  //now I want to elimate tracks that are too close to more common particles.
  float dedxbad;
  if (minMass>1) dedxbad = 1.1*track.dedxExpected((minMass-1)*0.939,theCharge);
  if (minMass==1) dedxbad = 1.1*track.dedxExpected(0.494,1); //not kaon
  if (dedx>dedxmin&&dedx>dedxbad) dedxOK=1;
  float random_number = (float)rand()/(float)RAND_MAX;
  //  cout << "random number = " << random_number << endl;
  if (random_number<0.005){dedxOK=1;}  //randomly select particles
  if (track.p()>pcut[0]&&track.p()<pcut[1]&&track.npntfit()>20&&track.iflag()>=0) pOK = 1;
  //  cout << "chargeOK = " << chargeOK << endl;
  //  cout << "dedxOK = " << dedxOK << endl;
  //  cout << "pOK = " << pOK << endl;
  iret = chargeOK*dedxOK*pOK;
  return iret;
}
void StNuclei::Report(){
  cout << "StNuclei: charge = " << chargeToAccept << endl; 
  cout << "StNuclei: mass >= " << minMass << endl; 
  cout << "StNuclei: momentum > " << pcut << endl;
}
