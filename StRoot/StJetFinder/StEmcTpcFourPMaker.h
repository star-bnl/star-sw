/***************************************************************************
 *
 * $Id: StEmcTpcFourPMaker.h,v 1.7 2003/06/26 22:37:32 thenry Exp $
 * $Log: StEmcTpcFourPMaker.h,v $
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
#include <iostream>
#include <sstream>
#include <iterator>
#include <map>
#include <algorithm>
#include <string>
#include <math.h>

#include "StFourPMaker.h"
#include "StLorentzVectorD.hh"
#include "StMuDSTMaker/COMMON/StMuEmcPoint.h"
#include "StEmcADCtoEMaker/StEmcADCtoEMaker.h"

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

class BadPathLengthException : public string
{
 public:
  BadPathLengthException() : string("Failed to solve PathLength.") {};
};

class StCorrectedEmcPoint
{
public:
    static const double SMDR = 2.2625;
    static const double HSMDR = 1.13125;
    static const double twoPi = M_PI*2.0;

    StCorrectedEmcPoint() : correctedE(0), mPoint(0), etaShift(0), index(0) {};
    StCorrectedEmcPoint(StMuEmcPoint* p, int _index) : correctedE(0), 
      mPoint(p), etaShift(0), index(_index) {};
    StCorrectedEmcPoint(StMuEmcPoint* p, int _index, StThreeVectorD vertex) : 
      mPoint(p), index(_index)
    {
      etaShift = atan2(vertex.z()/100.0, SMDR);
      correctedE = p->getEnergy();
    };
    StCorrectedEmcPoint(StMuEmcPoint* p, int _index, double zv) : 
      mPoint(p), index(_index) 
    {
      etaShift = atan2(zv/100.0, SMDR);
      correctedE = p->getEnergy();
    };
    StCorrectedEmcPoint(const StCorrectedEmcPoint& p)
    {
      mPoint = p.getPoint();
      correctedE = p.E();
      etaShift = p.getEtaShift();
      index = p.getIndex();
    };

    virtual ~StCorrectedEmcPoint() {};

    inline bool init(StMuEmcPoint* p, StThreeVectorD vertex)
      {
	init(p, vertex.z());
      };
    bool init(StMuEmcPoint* p, double zv)
      {
	mPoint = p;
        if(p)
	  correctedE = p->getEnergy();
        etaShift = atan2(zv/100.0, SMDR);
      };
    inline double pEta(void) { return Eta(); };
    inline double Eta(void) { 
      if(!mPoint) return 0.0; 
      return mPoint->getEta() - etaShift; 
    };
    inline double E(void) const{ return correctedE; };
    inline double Phi(void) { 
      if(!mPoint) return 0.0;
      double phi = mPoint->getPhi(); 
      while(phi < 0) phi+=twoPi;
      while(phi > twoPi) phi -= twoPi;
      return phi;
    };
    inline double pTheta(void) { return asinh(Eta()); };
    inline double Theta(void) { return asinh(Eta()); };
    inline double pPhi(void) { return Phi(); };
    inline void SetE(double newE) { correctedE = newE; };
    inline void SubE(double subE) { correctedE -= subE; };
    inline bool PhotonRemaining(void) { return correctedE > 0.0; };
    inline StLorentzVectorD P(void)
    {
      return StLorentzVectorD(correctedE, correctedE*cos(Phi())*cos(Theta()), 
			      correctedE*sin(Phi())*cos(Theta()), 
			      correctedE*sin(Theta()));
    };
    inline StMuEmcPoint* getPoint(void) const{ return mPoint; };
    inline double getEtaShift(void) const{ return etaShift; };
    inline int getIndex(void) const{ return index; };

protected:
    double correctedE;
    StMuEmcPoint* mPoint;
    double etaShift;
    int index;
};

class StProjectedTrack
{
 public:
    static const double SMDR = 226.25;
    static const double HSMDR = 113.125;
    static const double twoPi = M_PI*2.0;
    static const double PionAveDepRatio = 0.2;
    static const double KaonAveDepRatio = 0.2;
    static const double ProtonAveDepRatio = 0.2;
    static const double ElectronAveDepRatio = 1.0;;
    static const double me = .000511;	
    static const double mpr = .9383;
    static const double mpi = .1396;
    static const double mk = .4937;

    StProjectedTrack() : mTrack(0), index(0) { };
    StProjectedTrack(StMuTrack* t, int _index) : mTrack(t), index(_index) 
    {
      fourP = StLorentzVectorD(sqrt(masssqr() + mom().mag2()), mom());
      StPhysicalHelixD helix = mTrack->outerHelix();
      pairD s = helix.pathLength(SMDR);
      if(isnan(s.first) || isnan(s.second)) throw BadPathLengthException();
      double path = ((s.first < 0) || (s.second < 0)) ? max(s.first, s.second) : min(s.first, s.second);
      projection = helix.at(path);
      initProbabilities(t);
    };
    StProjectedTrack(StMuTrack* t, int _index, StThreeVectorD vertex) : 
      mTrack(t), index(_index) 
    {
      fourP = StLorentzVectorD(sqrt(masssqr() + mom().mag2()), mom());
      StPhysicalHelixD helix = mTrack->outerHelix();
      pairD s = helix.pathLength(SMDR);
      if(isnan(s.first) || isnan(s.second)) throw BadPathLengthException();
      double path = ((s.first < 0) || (s.second < 0)) ? max(s.first, s.second) : min(s.first, s.second);
      projection = helix.at(path) - vertex;
      initProbabilities(t);
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
	probKaon = t->pidProbKaon();
	probProton = t->pidProbProton();
	probElectron = t->pidProbElectron();
      };
    bool init(StMuTrack *t, StThreeVectorD vertex)
      {
	mTrack = t;
	fourP = StLorentzVectorD(sqrt(masssqr() + mom().mag2()), mom());
	StPhysicalHelixD helix = mTrack->outerHelix();
	pairD s = helix.pathLength(SMDR);
        if(isnan(s.first) || isnan(s.second)) throw BadPathLengthException();
	double path = ((s.first < 0) || (s.second < 0)) 
	  ? max(s.first, s.second) : min(s.first, s.second);
	projection = helix.at(path) - vertex;
      };
    inline double Eta(void) { return fourP.pseudoRapidity(); };
    inline double Phi(void) { return fourP.phi(); };
    inline double depE(void) { return (probElectron*ElectronAveDepRatio +
      probPion*PionAveDepRatio + probProton*ProtonAveDepRatio +
      probKaon*ElectronAveDepRatio)*E(); };
    inline double E(void) { return fourP.e(); };
    inline double pEta(void) { return projection.pseudoRapidity(); };
    inline double pPhi(void) { return projection.phi(); };
    inline StMuTrack* getTrack(void) const { return mTrack; };
    inline double pTheta(void) { return asinh(pEta()); };
    inline double getProbPion(void) const { return probPion; };
    inline double getProbKaon(void) const { return probKaon; };
    inline double getProbProton(void) const { return probProton; };
    inline double getProbElectron(void) const { return probElectron; };
    inline double masssqr(void) { double ms = mass(); return ms*ms; };
    inline double mass(void) { return probElectron*me +
      probPion*mpi + probProton*mpr + probKaon*mk; };
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
  StEmcTpcBinMap() {};

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
    (*it).second.push_back(point);        
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
  inline long bin(StCorrectedEmcPoint &p) { return bin(p.pPhi(), p.pTheta()); };
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

  // Loop through the tracks, and if the track bins into a bin with points
  // in it, then loop over the points and check to see if the point-track
  // distance is small enough to call this a pair.  If so, add the pair
  // to the pair lists.
  void correlate(double realradiussqr)
  {
    for(trackMap::iterator track = moddTracks.begin(); 
	track != moddTracks.end(); ++track)
      {
	bool found = false;
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
		    double dphi = (*track).second.pPhi() - cPoint.pPhi();
		    double deta = (*track).second.pEta() - cPoint.pEta();
		    if(dphi*dphi + deta*deta > realradiussqr) continue;
		    p2t.insert(pointToTracks::value_type
			       (*point, (*track).first));
		    t2p.insert(trackToPoints::value_type
			       ((*track).first, *point));
		    found = true;
		    break;
		  }
	      }
	    if(found) break;
	  }
      }
  };
};

class StEmcTpcFourPMaker : public StFourPMaker {
public: 
    const double radiussqr;
    double seconds;
    timesMap timeLengths;
    
public:
    StEmcTpcFourPMaker(const char* name, StMuDstMaker *pevent, 
      long pBins, long thBins, double pRad, double thRad, double rsqr, 
      StEmcADCtoEMaker* adcToEMaker = NULL);
    virtual Int_t Make();
    
    StMuEmcCollection* getMuEmcCollection(void) { return muEmc; };

    StEmcTpcBinMap binmap;   
protected:
    StEmcADCtoEMaker* adc2E;
    StMuEmcCollection* muEmc;

    ClassDef(StEmcTpcFourPMaker,1)
};
#endif

