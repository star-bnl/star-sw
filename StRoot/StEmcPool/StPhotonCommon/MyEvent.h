#ifndef MYEVENT_H
#define MYEVENT_H

#include <TObject.h>
#include <TClonesArray.h>
#include <TVector3.h>

class MyPoint;
class MyMcTrack;

class MyEvent : public TObject{
 protected:
  int fRunId;
  int fEventId;
  int fDate;
  int fTime;
  int fTrigger;
  int fPrescale[4];
  int fHiTowAdc;
  int fHiTowId;
  int fHiTowStat;
  float fHiTowEn;
  TVector3 fVertex;
  int fGoodPrimaryTracks;  
  int fGoodPrimaryTracksBarrelW;//0<eta<1
  int fGoodGlobalTracks;
  int fFtpcRefMultTracks;
  int fCratesCorrupted;
  int fModulesNoHits;
  int fModulesBad;
  float fEnBarrel;
  float fTpcPt;
  float fTpcPtBarrelW;//0<eta<1
  float fZdcSumW;
  float fZdcSumE;
  float fZdcVertexZ;
  float fBbcSumE;
  float fBbcSumW;
  float fCtbSum;//mips
  //new->
  float fBbcVertexZ;
  //<-

  float fWeight;
  float fPartonPt;

  int fNumberOfPoints;
  int fNumberOfMcPhotons;
  int fNumberOfMcPions;

  TClonesArray *fPointArray;
  TClonesArray *fMcPhotonArray;
  TClonesArray *fMcPionArray;
  MyMcTrack *fMcTrack;

 public:
  MyEvent(int run=0,int ev=0,int date=0,int time=0);
  MyEvent(const MyEvent&);
  ~MyEvent();
  
  int runId() {return fRunId;}
  int eventId() {return fEventId;}
  int date() {return fDate;}
  int time() {return fTime;}
  int trigger() {return fTrigger;}
  int prescale(unsigned int i) {return fPrescale[i];}
  int highTowerAdc() {return fHiTowAdc;}
  int highTowerId() {return fHiTowId;}
  int highTowerStatus() {return fHiTowStat;}
  float highTowerEnergy() {return fHiTowEn;}
  TVector3 vertex() {return fVertex;}
  int goodPrimaries() {return fGoodPrimaryTracks;}
  int goodPrimBarrel() {return fGoodPrimaryTracksBarrelW;}
  int goodGlobals() {return fGoodGlobalTracks;}
  int refMult() {return fFtpcRefMultTracks;}
  int corruptedCrates() {return fCratesCorrupted;}
  int modulesNoHits() {return fModulesNoHits;}
  int modulesBad() {return fModulesBad;}
  int numberOfPoints() {return fNumberOfPoints;}
  int numberOfMcPhotons() {return fNumberOfMcPhotons;}
  int numberOfMcPions() {return fNumberOfMcPions;}

  float energyInBarrel() {return fEnBarrel;}
  float momentumInTpc() {return fTpcPt;}
  float momentumInTpcWest() {return fTpcPtBarrelW;}
  float zdcSumWest() {return fZdcSumW;}
  float zdcSumEast() {return fZdcSumE;}
  float zdcVertexZ() {return fZdcVertexZ;}
  float bbcSumEast() {return fBbcSumE;}
  float bbcSumWest() {return fBbcSumW;}
  float ctbSum() {return fCtbSum;}
  float bbcVertexZ() {return fBbcVertexZ;}

  float weight() {return fWeight;}
  float partonPt() {return fPartonPt;}
  
  TClonesArray *getPointArray() {return fPointArray;}//array with barrel points
  TClonesArray *getMcPhotonArray() {return fMcPhotonArray;}//array with mc photons
  TClonesArray *getMcPionArray() {return fMcPionArray;}
  MyMcTrack *getMcTrack() {return fMcTrack;}//the generated meson/gamma

  void setRunId(int val) {fRunId=val;}
  void setEventId(int val) {fEventId=val;}
  void setDate(int val) {fDate=val;}
  void setTime(int val) {fTime=val;}
  void setTrigger(int val) {fTrigger=val;}
  void setPrescale(unsigned int i,int val) {fPrescale[i]=val;}
  void setHighTowerAdc(int val) {fHiTowAdc=val;}
  void setHighTowerId(int val) {fHiTowId=val;}
  void setHighTowerStatus(int val) {fHiTowStat=val;}
  void setHighTowerEnergy(float val) {fHiTowEn=val;}
  void setVertex(float val1,float val2,float val3){
    fVertex.SetX(val1);
    fVertex.SetY(val2);
    fVertex.SetZ(val3);
  }
  void setGoodPrimaries(int val) {fGoodPrimaryTracks=val;}
  void setGoodPrimBarrel(int val) {fGoodPrimaryTracksBarrelW=val;}
  void setGoodGlobals(int val) {fGoodGlobalTracks=val;}
  void setRefMult(int val) {fFtpcRefMultTracks=val;}
  void setCorruptedCrates(int val) {fCratesCorrupted=val;}
  void setModulesNoHits(int val) {fModulesNoHits=val;}
  void setModulesBad(int val) {fModulesBad=val;}
  
  void setEnergyInBarrel(float val) {fEnBarrel=val;}
  void setMomentumInTpc(float val) {fTpcPt=val;}
  void setMomentumInTpcWest(float val) {fTpcPtBarrelW=val;}
  void setZdcSumWest(float val) {fZdcSumW=val;}
  void setZdcSumEast(float val) {fZdcSumE=val;}
  void setZdcVertexZ(float val) {fZdcVertexZ=val;}
  void setBbcSumEast(float val) {fBbcSumE=val;}
  void setBbcSumWest(float val) {fBbcSumW=val;}
  void setCtbSum(float val) {fCtbSum=val;}
  void setBbcVertexZ(float val) {fBbcVertexZ=val;}

  void setWeight(float val) {fWeight=val;}
  void setPartonPt(float val) {fPartonPt=val;}

  void addPoint(MyPoint*);
  void addMcPhoton(MyMcTrack*);
  void addMcPion(MyMcTrack*);
  void setMcTrack(MyMcTrack*);

  ClassDef(MyEvent,6)
};
#endif

