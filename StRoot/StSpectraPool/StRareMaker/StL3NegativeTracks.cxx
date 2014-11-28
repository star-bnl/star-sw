#include "StL3NegativeTracks.h"
#include "StL3RareTrack.h"
#include <stdlib.h>

ClassImp(StL3NegativeTracks)

StL3NegativeTracks::StL3NegativeTracks(float plow, float phigh)
{
  pcut[0] = plow;
  pcut[1] = phigh;
}

int StL3NegativeTracks::Accept(StGlobalTrack* trk) {
  int iret = 0;
  int chargeOK = 0;
  int pOK=0;
  StL3RareTrack track(trk);
  if (track.chargesign()<0)
        chargeOK = 1;
  if (track.p()>pcut[0] && track.p()<pcut[1]
      && track.npntfit()>20)
        pOK = 1;
  iret = chargeOK*pOK;
  return iret;
}

void StL3NegativeTracks::Report(){
  cout << "StL3NegativeTracks: momentum > " << pcut << endl;
}
