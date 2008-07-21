/*!
  \class StPythiaMuTrackFourVec
  \author M.L. Miller (MIT Software)

  A simple class to wrap a StPythiaParticle to be passed into the jet finder.
*/

#ifndef StPythiaMuTrackFourVec_HH
#define StPythiaMuTrackFourVec_HH

#include <emulator/StMuTrackFourVec.h>

class StPythiaParticle;

class StPythiaMuTrackFourVec : public StMuTrackFourVec
{
public:
    ///Must pass a valid StPythiaParticle, and it's location in the
    StPythiaMuTrackFourVec();

    ///Default destructor
    virtual ~StPythiaMuTrackFourVec();

protected:
    
};



#endif
