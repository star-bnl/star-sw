
#include "StHbtMaker/Cut/franksPairCut.h"
#include <string>
#include <cstdio>

#ifdef __ROOT__
ClassImp(franksPairCut)
#endif

//__________________
franksPairCut::franksPairCut(){
  mNPairsPassed = mNPairsFailed = 0;
}
//__________________
franksPairCut::franksPairCut(const franksPairCut& c) : StHbtPairCut(c) {
  mNPairsPassed = mNPairsFailed = 0;
  mPrimaryVertex = c.mPrimaryVertex;
}
//__________________
//franksPairCut::~franksPairCut(){
//  /* no-op */
//}
//__________________
bool franksPairCut::Pass(const StHbtPair* pair){
  
  double p1 = pair->track1()->Helix().pathLengths(pair->track2()->Helix()).first;
  double p2 = pair->track1()->Helix().pathLengths(pair->track2()->Helix()).second;

  StHbtThreeVector mid  = (pair->track1()->Helix().at(p1) + pair->track2()->Helix().at(p2) )/2.;
  StHbtThreeVector vertexVec  = mid - mPrimaryVertex;

  double dcaHelicees = abs(pair->track1()->Helix().at(p1) - pair->track2()->Helix().at(p2));
  double angle = vertexVec.angle(pair->fourMomentumSum().vect());
  double dcaPhi = sin(angle)*abs(vertexVec);

#ifdef STHBTDEBUG
  cout << " angle: " << angle*180./3.1415927 << " dcaHelicees : " << dcaHelicees;
  cout << " dcaPhi: " << dcaPhi << " mid: " << mid << endl;
#endif  
  
  bool temp;
  (dcaPhi<5.) ? temp=true : temp=false;

  temp ? mNPairsPassed++ : mNPairsFailed++;
  return temp;
}
//__________________
StHbtString franksPairCut::Report(){
  string Stemp = "Franks Pair Cut have to finish cut and report; this is just a test - \n";
  char Ctemp[100];
  sprintf(Ctemp,"Number of pairs which passed:\t%ld  Number which failed:\t%ld\n",mNPairsPassed,mNPairsFailed);
  Stemp += Ctemp;
  StHbtString returnThis = Stemp;
  return returnThis;
}
//__________________
