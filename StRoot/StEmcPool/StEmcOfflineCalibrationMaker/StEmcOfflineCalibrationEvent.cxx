//StEmcOfflineCalibrationEvent.cxx

#include "StEmcOfflineCalibrationEvent.h"

ClassImp(StEmcOfflineCalibrationTrack)

StEmcOfflineCalibrationTrack::StEmcOfflineCalibrationTrack() { }

StEmcOfflineCalibrationTrack::~StEmcOfflineCalibrationTrack() { }

void StEmcOfflineCalibrationTrack::Clear(Option_t* option) { }


ClassImp(StEmcOfflineCalibrationEvent)

StEmcOfflineCalibrationEvent::StEmcOfflineCalibrationEvent()
{
	tracks = new TClonesArray("StEmcOfflineCalibrationTrack",10);
	nTracks = 0;
	
	//initialize some arrays to zero
	for(int i=0; i<10; i++)	vx[i]=vy[i]=vz[i]=ranking[i]=0.;
	
	//trust all basic variables are initialized to zero;
}

StEmcOfflineCalibrationEvent::~StEmcOfflineCalibrationEvent()
{
	tracks->Clear();
	l2Result.Reset();
}

void StEmcOfflineCalibrationEvent::Clear(Option_t* option)
{
	tracks->Clear();
	nTracks = 0;
	l2Result.Reset();
	for(int i=0; i<10; i++)	vx[i]=vy[i]=vz[i]=ranking[i]=0.;
	
	//shouldn't be caught with stale data in other vars -- either they're filled each event or not at all
}

void StEmcOfflineCalibrationEvent::addTrack(StEmcOfflineCalibrationTrack* track)
{
	new ( (*tracks)[tracks->GetLast()+1] ) StEmcOfflineCalibrationTrack(*track);
	nTracks++;
}
