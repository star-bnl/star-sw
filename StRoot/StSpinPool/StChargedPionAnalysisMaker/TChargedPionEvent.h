#ifndef STAR_TChargedPionEvent
#define STAR_TChargedPionEvent

#include "TObject.h"
#include "TClonesArray.h"
#include "TVector3.h"

class TChargedPion : public TObject
{
public:
	double			pt;
	double			eta;
	double			phi;
	unsigned short	nHits;
	unsigned short	nFitPoints;
	unsigned short	nDedxPoints;
	unsigned short	nHitsPossible;
	short			charge;
	double			chi2;
	short			key;
	short			flag;
	double			globalPt;
	double			globalEta;
	double			globalPhi;
	unsigned short	vertexIndex;
	TVector3		dca;
	TVector3		firstPoint;
	TVector3		lastPoint;
	
	//dedx stuff
	double			dEdx;
	double			nSigmaPion;
	double			nSigmaProton;
	double			nSigmaKaon;
	double			nSigmaElectron;
		
	TChargedPion();
	~TChargedPion();
	
	void Clear(Option_t* option="");
	
	ClassDef(TChargedPion, 4)
};


class TChargedPionEvent : public TObject
{
public:
	//event info
	unsigned short	fill;
	unsigned int	run;
	unsigned int	event;
	unsigned int	fileid1;
	unsigned int	fileid2;
	unsigned short	x7;
	unsigned short	x48;
	unsigned short	bbcTimeBin;
	
	//track arrays
	unsigned short	nPionCandidates;
	TClonesArray*	pionCandidates;
	unsigned short	nLowPtTracks;
	TClonesArray*	lowPtTracks; //1<pt<2
	
	//multiple vertex support, hopefully it doesn't slow us down
	unsigned short	nVertices;
	float			vx[10];	//[nVertices]
	float			vy[10];	//[nVertices]
	float			vz[10];	//[nVertices]
	float			ranking[10]; //[nVertices]
	
	//triggers (1 == trigger fired for this event)
	unsigned short	mbTrigger;
	unsigned short	ht1Trigger;
	unsigned short	ht2Trigger;
	unsigned short	jp1Trigger;
	unsigned short	jp2Trigger;

	//prescales
	float			mbPrescale;
	float			ht1Prescale;
	float			ht2Prescale;
	float			jp1Prescale;
	float			jp2Prescale;
	
	//spin database
	unsigned short	trigspin; //this one comes from L0 trigger
	unsigned short	dbspin;
	unsigned short	spinQA[5];

	//simulation triggers
	unsigned short	bbcTrigger;
	int				ht1TrigMaker[3]; //[YesNo][towID][ADC]
	int				ht2TrigMaker[3];
	int				jp1TrigMaker[3];
	int				jp2TrigMaker[3];
	
	//pythia stuff, particle ids are given by flavor[2], flavor[3]
	unsigned short	subProcessID; 
	unsigned int	event2;	//should be identical to event, 0 means geant!=mcEvent
	unsigned int	flavor[4];
	float			parton1[10];
	float			parton2[10];
	float			s;
	float			t;
	float			u;
	float			partonic_pt;
	float			cos_theta;
	float			x1;
	float			x2;
	double			Q2; //partonic_pt*partonic_pt
	double			partonic_all;
	double			df1[5]; //[LO][STD][0][MAX][MIN]
	double			df2[5];
	double			f1[2]; //[LO][NLO]
	double			f2[2];
	
	
	TChargedPionEvent();
	~TChargedPionEvent();

	void Clear(Option_t* option="");

	void addTrack(TChargedPion* track);

	ClassDef(TChargedPionEvent,12)
};	
#endif
