#ifndef STAR_StEmcOfflineCalibrationEvent
#define STAR_StEmcOfflineCalibrationEvent

#include "TObject.h"
#include "TClonesArray.h"
#include "TVector3.h"

class StEmcOfflineCalibrationTrack : public TObject
{
public:
	unsigned short	tower_id[9];
	unsigned short	tower_id_exit;
	
	unsigned short	tower_adc[9];
	float			tower_pedestal[9];
	float			tower_pedestal_rms[9];
	int				tower_status[9];
	float			highest_neighbor;
	
	unsigned short	preshower_adc[9];
	float			preshower_pedestal[9];
	float			preshower_pedestal_rms[9];
	int				preshower_status[9];
	unsigned char	preshower_cap[9];
	
	unsigned short	vertexIndex;
	
	double			p;
	float			deta;
	float			dphi;
	unsigned short	nHits;
	unsigned short	nFitPoints;
	unsigned short	nDedxPoints;
	unsigned short	nHitsPossible;
	double			dEdx;
	double			nSigmaElectron;	
	
	StEmcOfflineCalibrationTrack();
	~StEmcOfflineCalibrationTrack();
	
	void Clear(Option_t* option="");
	
	ClassDef(StEmcOfflineCalibrationTrack, 2)
};

class StEmcOfflineCalibrationEvent : public TObject
{
public:
	//event info
	unsigned short	fill;
	unsigned int	run;
	unsigned int	event;
	int				date;
	int				time;
	
	unsigned int	fileid1;
	unsigned int	fileid2;
	
	//track arrays
	unsigned short	nTracks;
	TClonesArray*	tracks;
	
	//multiple vertex support
	unsigned short	nVertices;
	float			vx[10];	//[nVertices]
	float			vy[10];	//[nVertices]
	float			vz[10];	//[nVertices]
	float			ranking[10]; //[nVertices]
	
	//triggers (1 == trigger fired for this event)
	unsigned short	mbTrigger;
	unsigned short	htTrigger; //exclude http for now
//	unsigned int	rawTriggerBlock;
	
	int htTrigMaker[3]; //Yes/No ID DSM
	
	StEmcOfflineCalibrationEvent();
	~StEmcOfflineCalibrationEvent();
	
	void Clear(Option_t* option="");
	
	void addTrack(StEmcOfflineCalibrationTrack* track);
	
	ClassDef(StEmcOfflineCalibrationEvent,3)
};	
#endif
