#if defined(__CINT__)

#pragma link off all globals;
#pragma link off all classes;
#pragma link off all functions;

#if ROOT_VERSION_CODE >= 393216
#pragma link C++ class StPicoThreeVector<float>+;
#pragma link C++ class StPicoThreeVector<double>+;
#else
#pragma link C++ class StPicoThreeVector<float>-;
#pragma link C++ class StPicoThreeVector<double>-;
#endif

#pragma link C++ class StPicoArrays+;   //Maybe + is not needed
#pragma link C++ class StPicoTrack+;
#pragma link C++ class StPicoMtdTrigger+;
#pragma link C++ class StPicoMtdPidTraits+;
#pragma link C++ class StPicoMtdHit+;
#pragma link C++ class StPicoFmsHit+;
#pragma link C++ class StPicoEvent+;
#pragma link C++ class StPicoEpdHit+;
#pragma link C++ class StPicoEmcTrigger+;
#pragma link C++ class StPicoDst+;      //Maybe + is not needed
#pragma link C++ class StPicoBTowHit+;
#pragma link C++ class StPicoBTofPidTraits+;
#pragma link C++ class StPicoBTofHit+;
#pragma link C++ class StPicoBEmcPidTraits+;
#pragma link C++ class StPicoBbcHit+;
#pragma link C++ class StPicoHelix+;
#pragma link C++ class StPicoPhysicalHelix+;
#pragma link C++ class StPicoTrackCovMatrix+;
#pragma link C++ class StPicoDstReader;

//#pragma link C++ typedef StPicoThreeVectorF;
//#pragma link C++ typedef StPicoThreeVectorD;

#endif
