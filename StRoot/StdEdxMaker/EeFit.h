#ifndef St_EeFit_h
#define St_EeFit_h
struct EeFit_t {
  Float_t  NoPrimaryTracks;
  Float_t  NoMeauredPoints;
  Float_t  TrackLength;
  Float_t  Pmag; // signed momentum
  Float_t  Eta; 
  Float_t  zFit; // Fitted ionization
};
#endif
