/***************************************************************************
 *
 * $Id: StEmcTpcFourPMaker.h,v 1.2 2003/04/24 14:15:15 thenry Exp $
 * $Log: StEmcTpcFourPMaker.h,v $
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

#define M_PI 3.14159265358979323846

class StCorrectedEmcPoint
{
public:
    static const double SMDR = 2.2625;
    static const double HSMDR = 1.13125;
    static const double twoPi = M_PI*2.0;

    StCorrectedEmcPoint() : correctedE(0), mPoint(0), etaShift(0) {};
    StCorrectedEmcPoint(StMuEmcPoint* p) : correctedE(0), mPoint(p), etaShift(0) {};
    StCorrectedEmcPoint(StMuEmcPoint* p, StThreeVectorD vertex) : mPoint(p)
    {
      etaShift = atan2(vertex.z()/100.0, SMDR);
      correctedE = p->getEnergy();
    };
    StCorrectedEmcPoint(StMuEmcPoint* p, double zv) : mPoint(p) 
    {
      etaShift = atan2(zv/100.0, SMDR);
      correctedE = p->getEnergy();
    };
    StCorrectedEmcPoint(const StCorrectedEmcPoint& p)
    {
      mPoint = p.getPoint();
      correctedE = p.E();
      etaShift = p.getEtaShift();
    };

    virtual ~StCorrectedEmcPoint() {};

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
    inline double pPhi(void) { return Phi(); };
    inline void SetE(double newE) { correctedE = newE; };
    inline void SubE(double subE) { correctedE -= subE; };
    inline bool PhotonRemaining(void) { return correctedE > 0.0; };
    inline StLorentzVectorD P(void)
    {
      double pt = correctedE/sqrt(1.0+sinh(Eta())*sinh(Eta()));
      return StLorentzVectorD(correctedE, pt*cos(Phi()), pt*sin(Phi()), pt*sinh(Eta()));
    };
    inline StMuEmcPoint* getPoint(void) const{ return mPoint; };
    inline double getEtaShift(void) const{ return etaShift; };

protected:
    double correctedE;
    StMuEmcPoint* mPoint;
    double etaShift;
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

    StProjectedTrack() : mTrack(0) {};
    StProjectedTrack(StMuTrack* t) : mTrack(t) 
    {
      fourP = StLorentzVectorD(sqrt(masssqr() + mom().mag2()), mom());
      StPhysicalHelixD helix = mTrack->outerHelix();
      pairD s = helix.pathLength(SMDR);
      double path = ((s.first < 0) || (s.second < 0)) ? max(s.first, s.second) : min(s.first, s.second);
      projection = helix.at(path);
    };
    StProjectedTrack(StMuTrack* t, StThreeVectorD vertex) : mTrack(t) 
    {
      fourP = StLorentzVectorD(sqrt(masssqr() + mom().mag2()), mom());
      StPhysicalHelixD helix = mTrack->outerHelix();
      pairD s = helix.pathLength(SMDR);
      double path = ((s.first < 0) || (s.second < 0)) ? max(s.first, s.second) : min(s.first, s.second);
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
    };
    virtual ~StProjectedTrack() {};

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

protected:
    StMuTrack* mTrack;
    double probPion;
    double probProton;
    double probElectron;
    double probKaon;

    StLorentzVectorD fourP;
    StThreeVectorD projection;
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
  void init(long phiR, long thetaR)
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

typedef map<long, pointerMap, less<long> > _binmap;
//typedef pair<pointerMap::iterator, pointerMap::iterator> pMapIteratorPair;

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

protected:
  StThreeVectorD mVertex;
  binCircleList binchecklist;
  StEmcTpcBinMap() {};

public:
  StEmcTpcBinMap(long pBins, long tBins, double pR, double tR)
   : phiBins(pBins), thetaBins(tBins), phiRadius(pR), thetaRadius(tR) 
  {
    binchecklist.init(phiBin(phiRadius), thetaBin(thetaRadius)); 
  };

  _binmap::iterator insert(pointerPair& pPair)
  {
    long b = bin(pPair);
    if(empty(b))
      _binmap::insert(pair<long, pointerMap>(b, pointerMap()));
    _binmap::iterator it = _binmap::find(b);
    _binmap::value_type &val = *it;
    val.second.insert(pair<pointerPair,pointerPair>(pPair,pPair));        
    return it;
  };

  inline void clearall(void) { clear(); moddTracks.clear(); moddPoints.clear(); };
  inline void setVertex(StThreeVectorD &vertex) { mVertex = vertex; };

  inline _binmap::iterator insertTrack(StMuTrack* track)
  {
    moddTracks[track] = StProjectedTrack(track, mVertex);
    pointerPair pp = pointerPair(track, NULL);
    return insert(pp);
  };
  inline _binmap::iterator insertPoint(StMuEmcPoint* point)
  {
    moddPoints[point] = StCorrectedEmcPoint(point, mVertex);
    pointerPair pp = pointerPair(NULL, point);
    return insert(pp);
  };
  inline long bin(pointerPair& pPair) 
  { 
    if(pPair.first)
      return bin(moddTracks[pPair.first]);
    if(pPair.second)
      return bin(moddPoints[pPair.second]);
    return 0;
  };
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
    return phiB + phiBins*thetaB;    
  };
  inline long bin(pointerPair &track, binCircleList::value_type &relative)
  { return bin(track.first, relative); };
  inline long bin(StMuTrack* track, binCircleList::value_type &relative)
  {
    return bin(
      thetaBin(moddTracks[track].pTheta())+relative.first,
      phiBin(moddTracks[track].pPhi())+relative.second);
  };
  long bin(pointerMap::value_type &track, binCircleList::value_type &relative)
  {  return bin((pointerPair &) track.first, relative); };
  pointerMap::iterator beginTrack(value_type &bin) 
  {  return bin.second.begin(); };
  pointerMap::iterator endTrack(value_type &bin) 
  {  return bin.second.lower_bound(pointsBegin); };
  inline pointerMap::iterator beginPoint(value_type &bin) 
  {  return bin.second.lower_bound(pointsBegin); };
  inline pointerMap::iterator endPoint(value_type &bin) 
  {  return bin.second.end(); };
  pointerMap::iterator beginTrack(
    pointerMap::value_type &track, binCircleList::value_type &relative) 
  {  return getPointerMap(track, relative).begin(); };
  pointerMap::iterator endTrack(
    pointerMap::value_type &track, binCircleList::value_type &relative) 
  {  return getPointerMap(track, relative).lower_bound(pointsBegin); };
  pointerMap::iterator beginPoint(
    pointerMap::value_type &track, binCircleList::value_type &relative) 
  {  return getPointerMap(track, relative).lower_bound(pointsBegin); };
  pointerMap::iterator endPoint(
    pointerMap::value_type &track, binCircleList::value_type &relative) 
  {  return getPointerMap(track, relative).end(); };
  pointerMap &getPointerMap(
    pointerMap::value_type &track, binCircleList::value_type &relative)
  {  return getPointerMap(bin(track, relative)); };
  pointerMap &getPointerMap(long b)
  {  _binmap::value_type &val = *(_binmap::find(b)); return val.second; };
  inline bool empty(long bin)
  {  return _binmap::find(bin) == end(); };
  inline bool empty(pointerMap::value_type &track, binCircleList::value_type &relative)
  {  return empty(bin(track, relative)); };
  inline bool exists(pointerMap::value_type &track, binCircleList::value_type &relative)
  {  return empty(track, relative) == false; };

  void correlate(double realradiussqr)
  // That is, look in each bin, find all tracks without corresponding points,
  // look at all the bins which are "close", and if we find a point without a 
  // corresponding track, we found a pair, so add the pair in both the local
  // and the foreign bin.
  {
    for(iterator binit = begin(); binit != end(); ++binit)
      for(pointerMap::iterator track = beginTrack(*binit); 
          track != endTrack(*binit); ++track)
      {
	pointerMap::value_type &track_val = *track;
	_binmap::value_type &bin_val = *binit;
        if(track_val.second.second == 0)
          for(binCircleList::iterator relative = binchecklist.begin();
              relative != binchecklist.end(); relative++)
           if(exists(track_val, *relative))
            for(pointerMap::iterator point = beginPoint(track_val, *relative);
                point != endPoint(track_val, *relative); ++point)
	    {
	      pointerMap::value_type &point_val = *point;
              if(point_val.second.first == 0)
              {
                double dphi = moddTracks[track_val.first.first].pPhi()
                            - moddPoints[point_val.first.second].pPhi();
                double deta = moddTracks[track_val.first.first].pEta()
                            - moddPoints[point_val.first.second].pEta();
                if(dphi*dphi + deta*deta > realradiussqr) continue;
                bin_val.second.insert(pointerMap::value_type(track_val.first, 
                    pointerPair( track_val.first.first, 
                                 point_val.first.second ) ) );
                getPointerMap(track_val, *relative).insert(
                  pointerMap::value_type( track_val.first, 
                    pointerPair( track_val.first.first, 
                                 point_val.first.second ) ) );
              }
            }
      }
  };

  pointerMap::iterator findbegin(trackMap::value_type &track)
  {  
    long b = bin(track.second); 
    if(empty(b)) { stringstream ss; ss << "Bin " << b << " is empty!"; throw ss.str(); }
    _binmap::value_type &bin_val = *(_binmap::find(b));
    pointerMap::iterator beg = bin_val.second.lower_bound(
      pointerPair(track.first,0) );
    return beg;  
  };  

  pointerMap::iterator findend(trackMap::value_type &track)
  {  
    long b = bin(track.second); 
    if(empty(b)) { stringstream ss; ss << "Bin " << b << " is empty!"; throw ss.str(); }
    _binmap::value_type &bin_val = *(_binmap::find(b));
    pointerMap::iterator ed = bin_val.second.upper_bound(
      pointerPair(track.first,0) );
    return ed;  
  };  

  pointerMap::iterator findbegin(pointMap::value_type &point)
  {  
    long b = bin(point.second); 
    if(empty(b)) { stringstream ss; ss << "Bin " << b << " is empty!"; throw ss.str(); }
    _binmap::value_type &bin_val = *(_binmap::find(b));
    pointerMap::iterator beg = bin_val.second.lower_bound(
      pointerPair(0, point.first) );
    return beg;  
  };

  pointerMap::iterator findend(pointMap::value_type &point)
  {  
    long b = bin(point.second); 
    if(empty(b)) { stringstream ss; ss << "Bin " << b << " is empty!"; throw ss.str(); }
    _binmap::value_type &bin_val = *(_binmap::find(b));
    pointerMap::iterator ed = bin_val.second.upper_bound(
      pointerPair(0, point.first) );
    return ed;  
  };

  /*
  pMapIteratorPair find(trackMap::value_type track)
  {  
    long b = bin(track.second); 
    if(empty(b)) { stringstream ss; ss << "Bin " << b << " is empty!"; throw ss.str(); }
    _binmap::value_type &bin_val = *(_binmap::find(b));
    pointerMap::iterator beg = bin_val.second.lower_bound(
      pointerPair(track.first,0) );
    pointerMap::iterator ed = bin_val.second.upper_bound(
      pointerPair(track.first,0) );
    return pMapIteratorPair(beg, ed);  
  };  

  pMapIteratorPair find(pointMap::value_type point)
  {  
    long b = bin(point.second); 
    if(empty(b)) { stringstream ss; ss << "Bin " << b << " is empty!"; throw ss.str(); }
    _binmap::value_type &bin_val = *(_binmap::find(b));
    pointerMap::iterator beg = bin_val.second.lower_bound(
      pointerPair(0, point.first) );
    pointerMap::iterator ed = bin_val.second.upper_bound(
      pointerPair(0, point.first) );
    return pMapIteratorPair(beg, ed);  
  };
  */

  inline StProjectedTrack &projectedTrack(pointerMap::value_type &it)
  {  return moddTracks[it.second.first]; };
  inline StCorrectedEmcPoint &correctedEmcPoint(pointerMap::value_type &it)
  {  return moddPoints[it.second.second]; };
};

class StEmcTpcFourPMaker : public StFourPMaker {
public: 
    const double radiussqr;
    double seconds;
    
public:
    StEmcTpcFourPMaker(const char* name, StMuDstMaker *pevent, 
      long pBins, long thBins, double pRad, double thRad, double rsqr);
    virtual Int_t Make();

    StEmcTpcBinMap binmap;   

    ClassDef(StEmcTpcFourPMaker,1)
};
#endif

