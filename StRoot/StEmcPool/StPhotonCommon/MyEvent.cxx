#include "MyPoint.h" 
#include "MyMcTrack.h" 

#include "MyEvent.h"

ClassImp(MyEvent)

MyEvent::MyEvent(Int_t run,Int_t ev,Int_t date,Int_t time)
    : TObject()
{
  fRunId=run;
  fEventId=ev;
  fDate=date;
  fTime=time;
  fTrigger=0;
  for(UInt_t i=0;i<4;i++) fPrescale[i]=0;
  fHiTowAdc=0;
  fHiTowId=0;
  fHiTowStat=0;
  fHiTowEn=0.;
  fVertex=TVector3();
  fGoodPrimaryTracks=0;
  fGoodPrimaryTracksBarrelW=0;
  fGoodGlobalTracks=0;
  fFtpcRefMultTracks=0;
  fCratesCorrupted=0;
  fModulesNoHits=0;
  fModulesBad=0;
  fNumberOfPoints=0;
  fNumberOfMcPhotons=0;
  fNumberOfMcPions=0;

  fEnBarrel=0.;
  fTpcPt=0.;
  fTpcPtBarrelW=0.;
  fZdcSumW=0.;
  fZdcSumE=0.;
  fZdcVertexZ=0.;
  fBbcSumE=0.;
  fBbcSumW=0.;
  fCtbSum=0.;
  fBbcVertexZ=0.;

  fWeight=1.;
  fPartonPt=0.;

  fPointArray=new TClonesArray("MyPoint",10);
  fMcPhotonArray=new TClonesArray("MyMcTrack",5);
  fMcPionArray=new TClonesArray("MyMcTrack",5);
  fMcTrack=new MyMcTrack();
}
MyEvent::MyEvent(const MyEvent& ev)
    : TObject(ev)
{
  fRunId=ev.fRunId;
  fEventId=ev.fEventId;
  fDate=ev.fDate;
  fTime=ev.fTime;
  fTrigger=ev.fTrigger;
  for(UInt_t i=0;i<4;i++) fPrescale[i]=ev.fPrescale[i];
  fHiTowAdc=ev.fHiTowAdc;
  fHiTowId=ev.fHiTowId;
  fHiTowStat=ev.fHiTowStat;
  fHiTowEn=ev.fHiTowEn;
  fVertex=ev.fVertex;
  fGoodPrimaryTracks=ev.fGoodPrimaryTracks;
  fGoodPrimaryTracksBarrelW=ev.fGoodPrimaryTracksBarrelW;
  fGoodGlobalTracks=ev.fGoodGlobalTracks;
  fFtpcRefMultTracks=ev.fFtpcRefMultTracks;
  fCratesCorrupted=ev.fCratesCorrupted;
  fModulesNoHits=ev.fModulesNoHits;
  fModulesBad=ev.fModulesBad;
  fNumberOfPoints=ev.fNumberOfPoints;
  fNumberOfMcPhotons=ev.fNumberOfMcPhotons;
  fNumberOfMcPions=ev.fNumberOfMcPions;

  fEnBarrel=ev.fEnBarrel;
  fTpcPt=ev.fTpcPt;
  fTpcPtBarrelW=ev.fTpcPtBarrelW;
  fZdcSumW=ev.fZdcSumW;
  fZdcSumE=ev.fZdcSumE;
  fZdcVertexZ=ev.fZdcVertexZ;
  fBbcSumE=ev.fBbcSumE;
  fBbcSumW=ev.fBbcSumW;
  fCtbSum=ev.fCtbSum;
  fBbcVertexZ=ev.fBbcVertexZ;

  fWeight=ev.fWeight;
  fPartonPt=ev.fPartonPt;

  fPointArray=new TClonesArray(*ev.fPointArray);
  fMcPhotonArray=new TClonesArray(*ev.fMcPhotonArray);
  fMcPionArray=new TClonesArray(*ev.fMcPionArray);
  fMcTrack=new MyMcTrack(*ev.fMcTrack);
}
MyEvent::~MyEvent()
{
  delete fPointArray;
  delete fMcPhotonArray;
  delete fMcPionArray;
  delete fMcTrack;
}

void MyEvent::addPoint(MyPoint *point)
{
  if(point){
    TClonesArray &pointArray=*fPointArray;
    new(pointArray[fNumberOfPoints++]) MyPoint(point);
  }
  return;
}
void MyEvent::addMcPhoton(MyMcTrack *tr)
{
  if(tr){
    TClonesArray &gammaArray=*fMcPhotonArray;
    new(gammaArray[fNumberOfMcPhotons++]) MyMcTrack(tr);
  }
  return;
}
void MyEvent::addMcPion(MyMcTrack *tr)
{
  if(tr){
    TClonesArray &pionArray=*fMcPionArray;
    new(pionArray[fNumberOfMcPions++]) MyMcTrack(tr);
  }
  return;
}
void MyEvent::setMcTrack(MyMcTrack *tr)
{
  if(tr){
    fMcTrack=new MyMcTrack(tr);
  }
  return;
}

