#ifndef STRAREEVENT_HH
#define STRAREEVENT_HH
#include "TObject.h"
#include "TClonesArray.h"

class StEvent;
class StPrimaryTrack;

class StRareEvent : public TObject {

 public:
  StRareEvent();
  //  StRareEvent(StEvent* event);
  void FillRareEvent(StEvent* event);
  void Clear(Option_t *option="");
  ~StRareEvent();
  int   EventNumber() const;
  int   RunNumber() const;
  float vertexZ() const;
  int   numberOfGoodPrimaryTracks() const;
  float magneticField() const;
  void  AddTrack(StPrimaryTrack* track);
  TClonesArray* getTracks() const;
  TClonesArray* fRareTracks;

 private:
  static TClonesArray* fgRareTracks;
  int   fNRareTrack;
  int   fEventNumber;
  int   fRunNumber;
  float fVertexZ;
  int   fnumberOfGoodPrimaryTracks;
  float fmagneticField;

  ClassDef(StRareEvent,1)
};

 
inline int StRareEvent::EventNumber() const {return fEventNumber;}
inline int StRareEvent::numberOfGoodPrimaryTracks() const {return fnumberOfGoodPrimaryTracks;}
inline float StRareEvent::vertexZ() const {return fVertexZ;}
inline float StRareEvent::magneticField() const {return fmagneticField;} 

#endif
