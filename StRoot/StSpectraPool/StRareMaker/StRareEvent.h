#ifndef STRAREEVENT_HH
#define STRAREEVENT_HH
#include "TObject.h"
#include "TClonesArray.h"

class StEvent;
class StPrimaryTrack;
class StGlobalTrack;
class StL3Trigger;

class StRareEvent : public TObject {

 public:
  StRareEvent();
  //  StRareEvent(StEvent* event);
  ~StRareEvent();

  void  fillRareEvent(StEvent* event);
  void  fillL3Info(StL3Trigger *l3Event);
  void  clear(Option_t *option="");

  int   eventNumber() const;
  int   runNumber() const;
  int   triggerWord() const;
  float vertexZ() const;
  int   numberOfTracks() const;
  int   numberOfGoodPrimaryTracks() const;
  float magneticField() const;
  void  addTrack(StPrimaryTrack* track);
  void  addL3Track(StGlobalTrack* l3track);
  float l3vertexZ() const;
  int   numberOfL3Tracks() const;
  short l3Unbiased() const;
  int   numberOfProccesedEvents() const;
  int   numberOfAcceptedEvents() const;
  int   numberOfBuildEvents() const;
  float triggerData(int i) const; 
  int   l3Flag() const;
  TClonesArray* getTracks() const;
  TClonesArray* getL3Tracks() const;

 private:
  TClonesArray* fRareTracks;
  static TClonesArray* fgRareTracks;
  int   fNRareTrack;
  int   fEventNumber;
  int   fRunNumber;
  int   fTriggerWord;
  float fVertexZ;
  int   fnumberOfGoodPrimaryTracks;
  float fmagneticField;

  // L3 stuff
  TClonesArray* fL3RareTracks;
  static TClonesArray* fgL3RareTracks;
  int   fNL3RareTrack;
  float fL3VertexZ;
  short fL3Unbiased;
  int   fNProcessed;
  int   fNAccept;
  int   fNBuild;
  float fTriggerData[10];
  int   fL3Flag;

  ClassDef(StRareEvent,3)
};

 
inline int StRareEvent::eventNumber() const {return fEventNumber;}
inline int StRareEvent::runNumber() const {return fRunNumber;}
inline int StRareEvent::triggerWord() const {return fTriggerWord;}
inline int StRareEvent::numberOfGoodPrimaryTracks() const {return fnumberOfGoodPrimaryTracks;}
inline int StRareEvent::numberOfTracks() const {return fNRareTrack;}
inline float StRareEvent::vertexZ() const {return fVertexZ;}
inline float StRareEvent::magneticField() const {return fmagneticField;}
inline TClonesArray* StRareEvent::getTracks() const {return fRareTracks;}


// L3 stuff
inline int StRareEvent::numberOfL3Tracks() const {return fNL3RareTrack;}
inline short StRareEvent::l3Unbiased() const {return fL3Unbiased;}
inline int StRareEvent::numberOfProccesedEvents() const {return fNProcessed;}
inline int StRareEvent::numberOfAcceptedEvents() const {return fNAccept;}
inline int StRareEvent::numberOfBuildEvents() const {return fNBuild;}
inline float StRareEvent::l3vertexZ() const {return fL3VertexZ;}
inline TClonesArray* StRareEvent::getL3Tracks() const {return fL3RareTracks;}
inline float StRareEvent::triggerData(int i) const {
      if (i>=0 && i<=9) return fTriggerData[i];
      else return -9999;
}
inline int StRareEvent::l3Flag() const {return fL3Flag;}

#endif
