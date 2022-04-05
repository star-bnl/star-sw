#include "StFractionalChargeTracks.h"
#include "StRareTrack.h"
#include "StEventTypes.h"
ClassImp(StFractionalChargeTracks)

StFractionalChargeTracks::StFractionalChargeTracks(float dedxin, float pin){
  dedxcut = dedxin;
  pcut    = pin;
}

int StFractionalChargeTracks::Accept(StPrimaryTrack* trk){
  int iret = 0;
  StRareTrack track = StRareTrack(trk);
  if (track.p()>pcut&&track.dedx()<dedxcut*track.dedxPi()
      &&track.npntfit()>=20&&track.iflag()>=0) iret = 1;
  return iret;
}
void StFractionalChargeTracks::Report(){
  cout << "StFractionalChargeTracks: dE/dx < " << dedxcut 
       << " * dE/dx(pi) "<< endl;
  cout << "StFractionalChargeTracks: momentum > " << pcut << endl;
}
