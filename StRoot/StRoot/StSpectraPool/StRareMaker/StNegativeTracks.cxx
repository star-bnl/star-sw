#include "StNegativeTracks.h"
#include "StRareTrack.h"
#include "StEventTypes.h"
#include <stdlib.h>
ClassImp(StNegativeTracks)

StNegativeTracks::StNegativeTracks(float plow,float phigh){
  pcut[0]    = plow;
  pcut[1]    = phigh;
}

int StNegativeTracks::Accept(StPrimaryTrack* trk){
  int iret = 0;
  int chargeOK = 0;
  int pOK=0;
  StRareTrack track(trk);
  if (track.chargesign()<0) {chargeOK=1;}
  if (track.p()>pcut[0]&&track.p()<pcut[1]&&track.npntfit()>20&&track.iflag()>=0) pOK = 1;
  iret = chargeOK*pOK;
  return iret;
}
void StNegativeTracks::Report(){
  cout << "StNegativeTracks: momentum > " << pcut << endl;
}
