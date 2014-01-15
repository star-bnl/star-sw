#ifndef STAR_StEmcOfflineCalibrationEvent
#define STAR_StEmcOfflineCalibrationEvent

#include "TObject.h"
#include "TClonesArray.h"
#include "TVector3.h"
#include "TArrayI.h"
#include "StThreeVectorF.hh"
//#include "StEmcTowerCalibrationEvent.h"

#include <vector>
#include <map>
using namespace std;

//------------------------------------------------
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

	int charge;
	
	unsigned short	preshower_adc[9];
	float			preshower_pedestal[9];
	float			preshower_pedestal_rms[9];
	int				preshower_status[9];
	unsigned char	preshower_cap[9];

	unsigned short smde_id[11];
	unsigned short smde_adc[11];
	float smde_pedestal[11];
	float smde_pedestal_rms[11];
	int smde_status[11];
	unsigned char smde_cap[11];

	unsigned short smdp_id[11];
	unsigned short smdp_adc[11];
	float smdp_pedestal[11];
	float smdp_pedestal_rms[11];
	int smdp_status[11];
	unsigned char smdp_cap[11];
	
	unsigned short	vertexIndex;
	
	double eta;
	short flag;
	int bad;

	double			p;
	StThreeVectorF track;
	float			deta;
	float			dphi;
	unsigned short	nHits;
	unsigned short	nFitPoints;
	unsigned short	nDedxPoints;
	unsigned short	nHitsPossible;
	double			dEdx;
	double			nSigmaElectron;	
	

	// information from TOF
	float vpd_vz;
	float dca_global;
	int tofmatchedflag;
	float toftime;
	float tofbeta;
	float tofpathlength;
	float tofsigmaelectron;
	float tofprobelectron;

	unsigned short htTrig;//was there an ht trigger?
	unsigned short nonhtTrig;//was there a non ht trigger?

	unsigned short tofTrig; // was there a TOF trigger?

	//void takeTowerTrack(StEmcTowerCalibrationTrack* track);
	StEmcOfflineCalibrationTrack();
	~StEmcOfflineCalibrationTrack();
	
	void Clear(Option_t* option="");
	
	ClassDef(StEmcOfflineCalibrationTrack, 8)
};

//----------------------------------------------------------------------
class StEmcOfflineCalibrationCluster : public TObject
{

 public:
  StEmcOfflineCalibrationCluster();
  ~StEmcOfflineCalibrationCluster();
  unsigned int nTracks;
  TClonesArray* tracks;
  StEmcOfflineCalibrationTrack centralTrack;

  void addTrack(StEmcOfflineCalibrationTrack* track);
  void Clear(Option_t* option="");

  ClassDef(StEmcOfflineCalibrationCluster,1);

};

//------------------------------------------------------------

class StEmcOfflineCalibrationVertex : public TObject
{
public:
	float mPosition[3];
	float mPosError[3];
	
	int mVertexFinderId;
	Float_t mRanking;
	UShort_t mNTracksUsed;
	UShort_t mNCTBMatch;
	UShort_t mNBEMCMatch;
	UShort_t mNEEMCMatch;
	UShort_t mNCrossCentralMembrane;
	Float_t mSumTrackPt;
	Float_t mMeanDip;
	Float_t mChiSquared;
	
	// RefMult fields
	UShort_t mRefMultNeg;
	UShort_t mRefMultPos;
	UShort_t mRefMultFtpcWest;
	UShort_t mRefMultFtpcEast;
	
	ClassDef(StEmcOfflineCalibrationVertex, 1)
};

//----------------------------------------------------------------------

class StEmcOfflineCalibrationTrigger : public TObject
{
	int trigId;
	float prescale;
	int isSatisfied;
	
	ClassDef(StEmcOfflineCalibrationTrigger, 1)
};

//----------------------------------------------------------------------

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
	map<unsigned int, unsigned int> triggerResult;
	vector<unsigned int>	triggerIds;
	TArrayI			l2Result;
	map<unsigned int, vector< pair<int,int> > > towersAboveThreshold;//or patches

	
	int htTrigMaker[3]; //Yes/No ID DSM
	
	StEmcOfflineCalibrationEvent();
	~StEmcOfflineCalibrationEvent();
	
	void Clear(Option_t* option="");
	
	void addTrack(StEmcOfflineCalibrationTrack* track);
	
	ClassDef(StEmcOfflineCalibrationEvent,5)
};	
#endif
