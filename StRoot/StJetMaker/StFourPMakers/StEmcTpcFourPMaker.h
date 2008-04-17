/***************************************************************************
 *
 * $Id: StEmcTpcFourPMaker.h,v 1.5 2008/04/17 20:12:17 tai Exp $
 * $Log: StEmcTpcFourPMaker.h,v $
 * Revision 1.5  2008/04/17 20:12:17  tai
 * cleaned up the code.
 *
 * Revision 1.4  2007/01/10 19:18:45  mmiller
 * Fixed a compiler warning in depricated code (StEmcTpcFourpMaker, which is no longer used).
 *
 * Revision 1.3  2004/11/30 19:01:42  mmiller
 * Back compatibility for pre P04k bemc corrupt events
 *
 * Revision 1.2  2004/10/13 15:32:34  mmiller
 * Big clean of StEmcTpcFourPMaker, no longer crashes simulation pass!
 *
 * Revision 1.1  2004/10/12 18:18:03  mmiller
 * Add StFourPMakers subdirectory
 *
 * Revision 1.2  2004/08/10 15:41:38  mmiller
 * Added StCdfChargedConeJetFinder to the scheme.  Updated 4pmaker with Thomas' fix for adckludge.  Updated run macros to reflect changes
 *
 * Revision 1.1  2004/07/08 15:41:03  mmiller
 * First release of StJetMaker.  Mike's commit of full clean of StJetFinder, StJetMaker, and StSpinMaker.  builds and runs in dev.
 *
 * Revision 1.19  2004/04/27 22:27:09  thenry
 * Added getValuesFromHitId function which is a nice wrapper function for
 * the routines which find eta phi and energy from hitId.
 *
 * Revision 1.18  2004/03/22 21:41:05  thenry
 * Added UseSimpleADCCal() switch for proper analysis of Pythia Data
 *
 * Revision 1.17  2004/02/26 22:23:34  thenry
 * Fixed eta-Shift zVertex bug.
 *
 * Revision 1.16  2003/10/01 16:39:29  thenry
 * Added new getter for StProjectedTracks to StEmcTpcFourPMaker, and used
 * in StJetOutputMaker so that StTrackStruct now contains track px, py, pz
 *
 * Revision 1.15  2003/09/24 20:54:07  thenry
 * Fixed ANSI compatibility problems.
 *
 * Revision 1.14  2003/09/11 05:49:20  perev
 * ansi corrs
 *
 * Revision 1.13  2003/09/10 19:47:20  perev
 * ansi corrs
 *
 * Revision 1.12  2003/09/02 17:58:39  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.11  2003/08/29 17:30:04  thenry
 * Added some comments
 *
 * Revision 1.10  2003/08/27 18:08:39  thenry
 * Added Cuts on number of points above threshold and total EMC energy.
 * Added set point threshold.
 * Fixed bug in Eta and Theta math.
 *
 * Revision 1.9  2003/07/30 20:33:36  thenry
 * Added sanity check for total EMC energy, and some variables are now stored
 * for possible access later (per event).
 *
 * Revision 1.8  2003/07/24 22:11:17  thenry
 * Now can use any of Points, Hits, Clusters.  Now uses probability weighted
 * mass calculation.  Now Subtracts track emc deposited energy from nearest
 * first EMC energies until track emc deposited energy is used up or too
 * away.
 *
 * Revision 1.7  2003/06/26 22:37:32  thenry
 * Fixed a bug in the indexing of the points
 *
 * Revision 1.6  2003/06/25 23:03:31  thenry
 * Fixed indexes
 *
 * Revision 1.5  2003/05/29 21:16:05  thenry
 * Added initProbabilities to StProjectedTrack class
 *
 * Revision 1.4  2003/05/15 17:54:19  thenry
 * Constructor modified to accept the StEmcADCToEMaker* (default is NULL), so
 * that if StEmcADCToEMaker* is not NULL, the StEmcCollection from the
 * StEmcADCToEMaker will be used, instead of the default StEmcMuCollection
 * obtained directly from the muDst.
 *
 * Revision 1.3  2003/04/25 22:52:29  thenry
 * Better speed-- before took 5 seconds PER EVENT, now takes 1/7 the time
 * that reading from the MuDst does.  Fast enough.  Promiscuity with respect
 * to all towers (hot or not) still needs to be fixed.
 *
 * Revision 1.2  2003/04/24 14:15:15  thenry
 * These changes are really the first working version of the StFourPMakers
 * and teh StJetMakers.  This is all c++ stl implementation, and by virtue of
 * that fact, StEmcTpcFourPMaker bady needs to be speed optimized.
 *
 * Revision 1.1  2003/04/04 21:36:42  thenry
 * Creates lists of Four Vectors by combining data from both the TPC and EMC
 * for use with the StJetMaker
 *
 * Revision 1.0  2003/02/27 21:38:10  thenry
 * Created by Thomas Henry
 *
 * Author: Thomas Henry February 2003
 ***************************************************************************
 *
 * Description:  Maker which creates a list of Four Momentums from the TPC
 * and EMC corresponding to charged particles and photons, but subtracting 
 * some of the energy deposited in the EMC by the charged particles.
 *
 ***************************************************************************/
#ifndef StEmcTpcFourPMaker_h
#define StEmcTpcFourPMaker_h
using namespace std;
//std
#include <iostream>
#include <sstream>
#include <iterator>
#include <map>
#include <algorithm>
#include <string>
#include <math.h>

#include "StLorentzVectorD.hh"
#include "StMuDSTMaker/COMMON/StMuEmcPoint.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StJetMaker/StFourPMakers/StFourPMaker.h"
#include "StEmcADCtoEMaker/StEmcADCtoEMaker.h"

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

class BadPathLengthException : public string
{
public:
    BadPathLengthException() : string("Failed to solve PathLength.") {};
};

extern double SMDR;
extern double mSMDR;
extern double mHSMDR;
extern double mtwoPi;
extern double mme;	
extern double mmpr;
extern double mmpi;
extern double mmk;

class StCorrectedEmcPoint
{
public:
    StCorrectedEmcPoint() : correctedE(0), mPoint(0), thetaShift(0), index(0) {};
    StCorrectedEmcPoint(StMuEmcPoint* p, int _index) : correctedE(0), 
						       mPoint(p), thetaShift(0), index(_index) {};
    StCorrectedEmcPoint(StMuEmcPoint* p, int _index, StThreeVectorD vertex) : 
	mPoint(p), index(_index)
    {
	thetaShift = (vertex.z()/100.0)/SMDR;
	correctedE = p->getEnergy();
    };
    StCorrectedEmcPoint(StMuEmcPoint* p, int _index, double zv) : 
	mPoint(p), index(_index) 
    {
	thetaShift = (zv/100.0)/SMDR;
	correctedE = p->getEnergy();
    };
    StCorrectedEmcPoint(const StCorrectedEmcPoint& p)
    {
	mPoint = p.getPoint();
	correctedE = p.E();
	thetaShift = p.getThetaShift();
	index = p.getIndex();
    };

    virtual ~StCorrectedEmcPoint() {};

    inline bool init(StMuEmcPoint* p, StThreeVectorD vertex)
    {
	return init(p, vertex.z());
    };
    bool init(StMuEmcPoint* p, double zv)
    {
	mPoint = p;
        if(p)
	    correctedE = p->getEnergy();
	else return false;
        thetaShift = (zv/100.0)/SMDR;
	return true;
    };
    inline double pEta(void) { return Eta(); };
    inline double Eta(void) { 
	if(!mPoint) return 0.0; 
	return EtabyTheta(atan2(tan(ThetabyEta(mPoint->getEta())), 1.0-tan(ThetabyEta(mPoint->getEta()))*thetaShift)); 
    };
    inline double E(void) const{ return correctedE; };
    inline double Phi(void) { 
	if(!mPoint) return 0.0;
	double phi = mPoint->getPhi(); 
	while(phi < 0) phi+=mtwoPi;
	while(phi > mtwoPi) phi -= mtwoPi;
	return phi;
    };
    inline double pTheta(void) { return 2.0*atan(exp(-Eta())); };
    inline double Theta(void) { return 2.0*atan(exp(-Eta())); };
    inline double ThetabyEta(double veta) { return 2.0*atan(exp(-veta)); };
    inline double EtabyTheta(double vtheta) { return -log(tan(vtheta/2.0)); };
    inline double pPhi(void) { return Phi(); };
    inline void SetE(double newE) { correctedE = newE; };
    inline void SubE(double subE) { correctedE -= subE; };
    inline bool PhotonRemaining(void) { return correctedE > 0.0; };
    inline StLorentzVectorD P(void)
    {
	return StLorentzVectorD(correctedE*cos(Phi())*sin(Theta()), 
				correctedE*sin(Phi())*sin(Theta()), 
				correctedE*cos(Theta()), 
				correctedE);
    };
    inline StMuEmcPoint* getPoint(void) const{ return mPoint; };
    inline double getThetaShift(void) const{ return thetaShift; };
    inline int getIndex(void) const{ return index; };

protected:
    double correctedE;
    StMuEmcPoint* mPoint;
    double thetaShift;
    int index;
};

class StProjectedTrack
{
public:
    StProjectedTrack() : mTrack(0), index(0) { };
    StProjectedTrack(StMuTrack* t, int _index) : mTrack(t), index(_index) 
    {
	initProbabilities(t);
	fourP = StLorentzVectorD(eBar(), mom());
	StPhysicalHelixD helix = mTrack->outerHelix();
	pairD s = helix.pathLength(mSMDR);
	if(isnan(s.first) || isnan(s.second)) throw BadPathLengthException();
	double path = ((s.first < 0) || (s.second < 0)) ? std::max(s.first, s.second) : std::min(s.first, s.second);
	projection = helix.at(path);
    };
    StProjectedTrack(StMuTrack* t, int _index, StThreeVectorD vertex) : 
	mTrack(t), index(_index) 
    {
	initProbabilities(t);
	fourP = StLorentzVectorD(eBar(), mom());
	StPhysicalHelixD helix = mTrack->outerHelix();
	pairD s = helix.pathLength(mSMDR);
	if(isnan(s.first) || isnan(s.second)) throw BadPathLengthException();
	double path = ((s.first < 0) || (s.second < 0)) ? std::max<double>(s.first, s.second) : std::min<double>(s.first, s.second);
	projection = helix.at(path) - vertex;
    };
    StProjectedTrack(const StProjectedTrack &t) { 
	mTrack = t.getTrack();
	probPion = t.getProbPion();
	probKaon = t.getProbKaon();
	probProton = t.getProbProton();
	probElectron = t.getProbElectron();
	fourP = t.P();
	projection = t.proj();
	index = t.getIndex();
    };
    virtual ~StProjectedTrack() {};

    void initProbabilities(StMuTrack *t)
    {
	if(!t) { 
	    probPion = 1.0; 
	    probKaon = probProton = probElectron = 0; 
	    return; }
	probPion = t->pidProbPion();
	if((probPion < 0) || (probPion > 1)) probPion = 0;
    probKaon = t->pidProbKaon();
    if((probKaon < 0) || (probKaon > 1)) probKaon = 0;
probProton = t->pidProbProton();
if((probProton < 0) || (probProton > 1)) probProton = 0;
probElectron = t->pidProbElectron();
if((probElectron < 0) || (probElectron > 1)) probElectron = 0;
if((probPion == 0) && (probKaon == 0) && 
   (probProton == 0) && (probElectron == 0))
    {
	probPion = 1.0;
	return;
    }
double sum = probPion + probKaon + probProton + probElectron;
if(fabs(sum-1.0) < .01)
    {
	probPion /= sum;
	probKaon /= sum;
	probProton /= sum;
	probElectron /= sum;
    }
};
bool init(StMuTrack *t, int _index, StThreeVectorD vertex)
{
    mTrack = t;
    if(t == NULL) return false;
    index = _index;
    initProbabilities(t);
    fourP = StLorentzVectorD(eBar(), mom());
    StPhysicalHelixD helix = mTrack->outerHelix();
    pairD s = helix.pathLength(mSMDR);
    if(isnan(s.first) || isnan(s.second)) throw BadPathLengthException();
    double path = ((s.first < 0) || (s.second < 0)) 
	? std::max(s.first, s.second) : std::min(s.first, s.second);
    projection = helix.at(path) - vertex;
    return true;
};
inline double Eta(void) { return fourP.pseudoRapidity(); };
inline double Phi(void) { return fourP.phi(); };
double depE(void);
inline double E(void) { return fourP.e(); };
inline double pEta(void) { return projection.pseudoRapidity(); };
inline double pPhi(void) { return projection.phi(); };
inline StMuTrack* getTrack(void) const { return mTrack; };
inline double pTheta(void) { return 2.0*atan(exp(-pEta())); };
inline double getProbPion(void) const { return probPion; };
inline double getProbKaon(void) const { return probKaon; };
inline double getProbProton(void) const { return probProton; };
inline double getProbElectron(void) const { return probElectron; };
inline double masssqr(void) { double ms = mass(); return ms*ms; };
inline double mass(void) { return fourP.m(); };
inline double eBar(void) { return probElectron*eElectron() + 
	probPion*ePion() + probProton*eProton() + probKaon*eKaon(); };
inline double eElectron(void) { return ePart(mme); };
inline double eKaon(void) { return ePart(mmk); };
inline double eProton(void) { return ePart(mmpr); };
inline double ePion(void) { return ePart(mmpi); };
inline double ePart(double parMass) 
{ return ::sqrt(parMass*parMass + mom().mag2()); };
inline StThreeVectorD mom(void) { return mTrack->momentum(); };
inline const StLorentzVectorD &P(void) const 
{ const StLorentzVectorD &ret = fourP; return ret; };
inline const StThreeVectorD &proj(void) const 
{ const StThreeVectorD &ret = projection; return ret; };
void probEIsZero(void)
{
    double renormalize = 1.0/(1.0 - probElectron);
    probPion *= renormalize;
    probKaon *= renormalize;
    probProton *= renormalize;
    probElectron = 0;
};
inline void probEIsOne(void)
{
    probPion = probKaon = probProton = 0;
    probElectron = 1.0;
};
inline int getIndex(void) const{ return index; };

protected:
StMuTrack* mTrack;
double probPion;
double probProton;
double probElectron;
double probKaon;

StLorentzVectorD fourP;
StThreeVectorD projection;
int index;
};

typedef map<StMuTrack*, StProjectedTrack, less<StMuTrack*> > trackMap;
typedef map<StMuEmcPoint*, StCorrectedEmcPoint, less<StMuEmcPoint*> > pointMap;

typedef pair<StMuTrack*, StMuEmcPoint*> pointerPair;

// This pointerPairLessThan function is designed to work (though it should
// work generally) when either pointerPair.first == NULL or .second == NULL
// thus sorting the pointerPairs into a track list and a point list, all of
// a consistent type.
typedef less<pointerPair> pointerPairLessThan;

//Doing this keeps the track->point and point->track info all in one map 
typedef multimap<pointerPair, pointerPair, pointerPairLessThan> pointerMap;

class binCircleList : public multimap<long, long, less<long> >
{
public:
    //Searching via init_circle takes too long, so we switch to just
    //Theta 
    void init(long phiR, long thetaR)
    {
	init_theta(phiR, thetaR);
    };
    void init_theta(long phiR, long thetaR)
    {
	double thetaRsqr = (double) thetaR*thetaR;
	for(long thetaBin = -thetaR; thetaBin <= thetaR; thetaBin++)
	    {
		double thetaBinsqr = (double) thetaBin*thetaBin;
		double radiussqr = thetaBinsqr/thetaRsqr;
		if(radiussqr <= 1.0)
		    insert(pair<long, long>(thetaBin, 0));
	    } 
    };
    void init_circle(long phiR, long thetaR)
    {
	double phiRsqr = (double) phiR*phiR;
	double thetaRsqr = (double) thetaR*thetaR;
	for(long phiBin = -phiR; phiBin <= phiR; phiBin++)
	    for(long thetaBin = -thetaR; thetaBin <= thetaR; thetaBin++)
		{
		    double phiBinsqr = (double) phiBin*phiBin;
		    double thetaBinsqr = (double) thetaBin*thetaBin;
		    double radiussqr = phiBinsqr/phiRsqr + thetaBinsqr/thetaRsqr;
		    if(radiussqr <= 1.0)
			insert(pair<long, long>(thetaBin, phiBin));
		} 
    };
};

static const pointerPair pointsBegin(((StMuTrack*)NULL), ((StMuEmcPoint*)1)); 

typedef multimap<StMuEmcPoint*, StMuTrack*, less<StMuEmcPoint*> > pointToTracks;
typedef multimap<StMuTrack*, StMuEmcPoint*, less<StMuTrack*> > trackToPoints;

typedef map<long, double, less<long> > timesMap;

typedef vector<StMuEmcPoint*> pointVector;

typedef map<long, pointVector, less<long> > _binmap;

class StEmcTpcBinMap : public _binmap
{
public:
    const long phiBins;
    const long thetaBins;
    const double phiRadius;
    const double thetaRadius;

    // Except for these two objects, the whole structure is nothing but pointers.
    trackMap moddTracks;
    pointMap moddPoints;
    pointToTracks p2t;
    trackToPoints t2p;

protected:
    StThreeVectorD mVertex;
    binCircleList binchecklist;
    StEmcTpcBinMap() : phiBins(0), thetaBins(0), phiRadius(0), thetaRadius(0) {};

public:
    StEmcTpcBinMap(long pBins, long tBins, double pR, double tR)
	: phiBins(pBins), thetaBins(tBins), phiRadius(pR), thetaRadius(tR) 
    {
	binchecklist.init(phiBin(phiRadius) - phiBin(0), thetaBin(thetaRadius) - thetaBin(0));
    };

    _binmap::iterator insert(StMuEmcPoint* point)
    {
	long b = bin(point);
	if(empty(b))
	    _binmap::insert(_binmap::value_type(b, pointVector()));
	_binmap::iterator it = _binmap::find(b);
	pointVector &pv = (*it).second;
	pv.push_back(point);        
	return it;
    };

    inline void clearall(void) {
	//for(_binmap::iterator binit = begin(); binit != end(); ++binit)
	//(*binit).second.clear();
	clear();
	p2t.clear();
	t2p.clear();
	moddTracks.clear(); 
	moddPoints.clear(); 
    };
    inline void setVertex(StThreeVectorD &vertex) { mVertex = vertex; };
    inline void setVertex(StThreeVectorF &vertex) 
    { 
	mVertex = StThreeVectorD(vertex); 
    };

    inline void insertTrack(StMuTrack* track, int index)
    {
	try{
	    moddTracks[track] = StProjectedTrack(track, index, mVertex);
	} catch (BadPathLengthException &b) { }; 
	return;
    };
    inline _binmap::iterator insertPoint(StMuEmcPoint* point, int index)
    {
	moddPoints[point] = StCorrectedEmcPoint(point, index, mVertex);
	return insert(point);
    };
    inline long bin(StMuEmcPoint* point) { return bin(moddPoints[point]); };
    inline long bin(StMuTrack* track) { return bin(moddTracks[track]); };
    inline long bin(StProjectedTrack &p) { return bin(p.pPhi(), p.pTheta()); };
    inline long bin(StCorrectedEmcPoint &p) { return bin(p.pPhi(), p.pTheta());};
    inline long bin(double pPhi, double pTheta) 
    { return bin(thetaBin(pTheta), phiBin(pPhi)); };
    inline long thetaBin(double pTheta) 
    { return (long)((pTheta/M_PI_2)*(((double)thetaBins)/2.0)); }; 
    inline long phiBin(double pPhi) 
    { return (long)((pPhi/(2.0*M_PI))*((double)phiBins)); };
    inline long bin(long thetaB, long phiB)
    {
	while(phiB < 0) { phiB += phiBins; };
	while(phiB >= phiBins) { phiB -= phiBins; }; 
	//The full two dimensions is too slow, so collapse into 1D:theta
	//return phiB + phiBins*thetaB;    
	return thetaB;
    };
    inline long bin(StMuTrack* track, binCircleList::value_type &relative)
    {
	return bin(moddTracks[track], relative);
    };
    inline long bin(StProjectedTrack &track, binCircleList::value_type &relative)
    {
	return bin(
		   thetaBin(track.pTheta())+relative.first,
		   phiBin(track.pPhi())+relative.second);
    };
    pointVector &getPoints(long b)
    {  return (*(_binmap::find(b))).second; };
    inline bool empty(long bin)
    {  return _binmap::find(bin) == end(); };
    inline bool exists(long bin)
    {  return empty(bin) == false; };
    inline bool empty(StMuTrack *track, binCircleList::value_type &relative)
    {  return empty(bin(track, relative)); };
    inline bool exists(StMuTrack *track, binCircleList::value_type &relative)
    {  return empty(track, relative) == false; };

    double trackPointRadiusSqr(StProjectedTrack& track, StCorrectedEmcPoint& point)
    {
	double dphi = track.pPhi() - point.pPhi();
	double deta = track.pEta() - point.pEta();
	return dphi*dphi + deta*deta;
    }

    // Loop through the tracks, and if the track bins into a bin with points
    // in it, then loop over the points and check to see if the point-track
    // distance is small enough to call this a pair.  If so, add the pair
    // to the pair lists.
    void correlate(double realradiussqr)
    {
	for(trackMap::iterator track = moddTracks.begin(); 
	    track != moddTracks.end(); ++track)
	    {
		//bool found = false;
		for(binCircleList::iterator relative = binchecklist.begin();
		    relative != binchecklist.end(); ++relative)
		    {
			long binval = bin((*track).second, *relative);
			if(exists(binval))
			    {
				pointVector points = getPoints(binval);
				for(pointVector::iterator point = points.begin(); 
				    point != points.end(); ++point)
				    {
					StCorrectedEmcPoint &cPoint = moddPoints[*point];
					StProjectedTrack &cTrack = (*track).second;
					if(trackPointRadiusSqr(cTrack, cPoint) > realradiussqr) 
					    continue;
					p2t.insert(pointToTracks::value_type
						   (*point, (*track).first));
					t2p.insert(trackToPoints::value_type
						   ((*track).first, *point));
					//found = true;
					//break;
				    }
			    }
			//if(found) break;
		    }
	    }
    };
};

typedef vector<StMuEmcPoint> createdPointVector;

class StEmcTpcFourPMaker : public StFourPMaker {
public: 
    enum EMCHitType {Hits=0, Clusters=1, Points=2};
  
    const double radiussqr;
    double seconds;
    timesMap timeLengths;

public:
    void SetDepRatios(double PIDR, double KDR, 
		      double PRDR, double EDR, double CAD);
    void SetDepRatios(void);
  
    StEmcTpcFourPMaker(const char* name, StMuDstMaker *pevent, 
		       long pBins, long thBins, double pRad, 
		       double thRad, double rsqr, 
		       StEmcADCtoEMaker* adcToEMaker = NULL);

    virtual Int_t Make();
    virtual Int_t Finish();
    
    StMuEmcCollection* getMuEmcCollection(void) { return muEmc; };
    void setUseType(EMCHitType uType) { useType = uType; };
    EMCHitType getUseType(void) { return useType; };
    void setMaxPoints(long mPoints) { maxPoints = mPoints; };
    long getMaxPoints(void) { return maxPoints; };
    StProjectedTrack &getTrack(StMuTrack *trk)     { return binmap.moddTracks[trk]; };


    StEmcTpcBinMap binmap;   
protected:
    StEmcADCtoEMaker* adc2E;
    StMuEmcCollection* muEmc;
    StEmcCollection* emc;
    int maxHits;
    int numCoincidences;
    double sumPtTracks;
    double sumEMC;
    double EMCSanityThreshold;
    double sumSubtracted;
    double sumTheorySubtracted;
    double minPointThreshold;
    long maxPoints;
    long numberPoints;
    bool aborted;
    bool noAbortions;
    
public:
    createdPointVector fakePoints;
    double mPIDR, mKDR, mPRDR, mEDR, mCAD;
    EMCHitType useType;

    // Stop abortions, useful for simulated data
    void SetNoAbortions(void) { noAbortions = true; };
    // Number of tracks which land near EMC Points
    int getNumberCoincidences(void) { return numCoincidences; };
    // The sum of Pt of all the primary tracks in the event
    double getSumPtTracks(void) { return sumPtTracks; }; 
    // The sum of all the EMC Points or the sum of all the EMC hits including negatives in the event
    double getSumEMC(void) { return sumEMC; };
    // The sum of the energy subtracted from the points before they are sent
    // to the JetFinder
    double getSumSubtracted(void) { return sumSubtracted; };
    // The maximum amount of energy which could possibly be subtracted from
    // the points before they are sent to the JetFinder
    double getSumTheorySubtracted(void) { return sumTheorySubtracted; };
    // Set the maximum total Point energy or the maximum sum of all EMC towers
    // which will be run through the JetFinder (StppMikeConeJetFinder is limited)
    void setEMCSanityThreshold(double EST) { EMCSanityThreshold = EST; };
    // Set the minimum energy required before the Point will be sent to the
    // JetFinder
    void setMinPointThreshold(double threshold) 
    { minPointThreshold = threshold; };
    // The number of Points which fit the above criterion
    long getNumPoints(void) { return numberPoints; };
    // If this object decided not to send the tracks+points to the JetFinder
    // because of exceeding above thresholds then isAborted() will return true
    bool isAborted(void) { return aborted; };
 
    ClassDef(StEmcTpcFourPMaker,0)
	};

typedef multimap<double, StMuEmcPoint*, less<double> > DistanceToPointMap;

#endif


