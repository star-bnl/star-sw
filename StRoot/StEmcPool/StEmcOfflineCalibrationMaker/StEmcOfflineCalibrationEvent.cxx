//StEmcOfflineCalibrationEvent.cxx

#include "StEmcOfflineCalibrationEvent.h"

ClassImp(StEmcOfflineCalibrationCluster)

StEmcOfflineCalibrationCluster::StEmcOfflineCalibrationCluster()
{
  nTracks = 0;
  tracks = new TClonesArray("StEmcOfflineCalibrationTrack",10);
}

StEmcOfflineCalibrationCluster::~StEmcOfflineCalibrationCluster()
{
  tracks->Clear();
}

void StEmcOfflineCalibrationCluster::Clear(Option_t* option)
{
  nTracks = 0;
  if(tracks)
    tracks->Clear();
}

void StEmcOfflineCalibrationCluster::addTrack(StEmcOfflineCalibrationTrack* track)
{
  new ( (*tracks)[tracks->GetLast()+1] ) StEmcOfflineCalibrationTrack(*track);
  nTracks++;
}

//----------------------------

ClassImp(StEmcOfflineCalibrationTrack)

StEmcOfflineCalibrationTrack::StEmcOfflineCalibrationTrack()
{
}

StEmcOfflineCalibrationTrack::~StEmcOfflineCalibrationTrack()
{
}

void StEmcOfflineCalibrationTrack::Clear(Option_t* option)
{
}

/*
void StEmcOfflineCalibrationTrack::takeTowerTrack(StEmcTowerCalibrationTrack* track)
{
  tower_id_exit = track->tower_id_exit;
  highest_neighbor = track->highest_neighbor;
  vertexIndex = track->vertexIndex;
  p = track->p;
  deta = track->deta;
  dphi = track->dphi;
  nHits = track->nHits;
  nFitPoints = track->nFitPoints;
  nDedxPoints = track->nDedxPoints;
  dEdx = track->dEdx;
  nSigmaElectron = track->nSigmaElectron;
  for(int i = 0; i < 9; i++)
    {
      tower_id[i] = track->tower_id[i];
      tower_adc[i] = track->tower_adc[i];
      tower_pedestal[i] = track->tower_pedestal[i];
      tower_pedestal_rms[i] = track->tower_pedestal_rms[i];
      tower_status[i] = track->tower_status[i];
      preshower_adc[i] = track->preshower_adc[i];
      preshower_pedestal[i] = track->preshower_pedestal[i];
      preshower_pedestal_rms[i] = track->preshower_pedestal_rms[i];
      preshower_status[i] = track->preshower_status[i];
      preshower_cap[i] = track->preshower_cap[i];
    }
}
*/
//--------------------------------------------

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
  new((*tracks)[tracks->GetLast()+1]) StEmcOfflineCalibrationTrack(*track);
  nTracks++;
}
