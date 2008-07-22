

//std
#include <iostream>
using namespace std;

//ROOT
#include "TClonesArray.h"
#include "TObject.h"

//SCL
#include "StarClassLibrary/StThreeVectorF.hh"

//StMuDstMaker
#include "StMuDSTMaker/COMMON/StMuTrack.h"

//local
#include "StJetMaker/StPythia/StPythiaMuTrackFourVec.h"

StPythiaMuTrackFourVec::StPythiaMuTrackFourVec()
{
    /*
      StLorentzVectorF mom4( p->Px(), p->Py(), p->Pz(), p->E() );
      StThreeVectorF mom3(p->Px(), p->Py(), p->Pz() );
      StMuTrackFourVec::Init(0, mom4, index);
    */
}

StPythiaMuTrackFourVec::~StPythiaMuTrackFourVec()
{
}

