
/// \File StiKalmanTrackFinder.cxx
/// \Author Claude A Pruneau, 2001-2003
/// \Copyright(c) 2001, STAR  Experiment at BNL, All rights reserved.
/// \Note
/// Permission to use, copy, modify and distribute this software and its
/// documentation strictly for non-commercial purposes is hereby granted
/// without fee, provided that the above copyright notice appears in all
/// copies and that both the copyright notice and this permission notice
/// appear in the supporting documentation. The authors make no claims
/// about the suitability of this software for any purpose. It is
/// provided "as is" without express or implied warranty.
#include <cassert>
#include "Stiostream.h"
#include <stdexcept>
#include <math.h>
#include "TMath.h"
#include "TError.h"
#include "TStopwatch.h"
#include "StEnumerations.h"
#include "Sti/Base/Parameter.h"
#include "Sti/Base/EditableParameter.h"
#include "Sti/Base/EditableFilter.h"
#include "StiToolkit.h"
#include "StiHit.h"
#include "StiHitContainer.h"
#include "StiDetector.h"
#include "StiDetectorContainer.h"
#include "StiTrackContainer.h"
#include "StiTrack.h"
#include "StiTrackFinder.h"
//#include "StiTrackSeedFinder.h"
#include "StiTrack.h"
#include "StiKalmanTrackFinder.h"
#include "StiTrackContainer.h"
#include "StiKalmanTrack.h"
#include "StiKalmanTrackNode.h"
#include "StiDefaultTrackFilter.h"
#include "StiTrackFinderFilter.h"
#include "StiUtilities/StiDebug.h"
#include "StDetectorDbMaker/StiKalmanTrackFinderParameters.h"
#include "StMessMgr.h"
#include "StTpcHit.h"
#define TIME_StiKalmanTrackFinder
#ifdef TIME_StiKalmanTrackFinder
#include "Sti/StiTimer.h"
#endif

#include "StiKalmanTrackFitter.h" // just for err check
#include "StiTrackFinderFilter.h" // just for err check
#include "StiHitTest.h"


enum {kSeedTimg,kTrakTimg,kPrimTimg};
enum {kMaxTrackPerm = 10000,kMaxEventPerm=10000000};

static const double kRMinTpc =55;
int StiKalmanTrackFinder::_debug = 0;
ostream& operator<<(ostream&, const StiTrack&);
int gLevelOfFind = 0;
//______________________________________________________________________________
void StiKalmanTrackFinder::initialize()
{
  cout << "StiKalmanTrackFinder::initialize() -I- Started"<<endl;
  _toolkit = StiToolkit::instance();
  _trackNodeFactory  = _toolkit->getTrackNodeFactory();
  _detectorContainer = _toolkit->getDetectorContainer();
  _detectorContainer->clear();
  _hitContainer      = _toolkit->getHitContainer();
  _trackContainer    = _toolkit->getTrackContainer();
  /*
  StiDefaultTrackFilter * trackFilter = new StiDefaultTrackFilter("FinderTrackFilter","Reconstructed Track Filter");
  trackFilter->add( new EditableParameter("nPtsUsed","Use nPts", 1., 1., 0., 1., 1.,
                                          Parameter::Boolean, StiTrack::kPointCount) );
  trackFilter->add( new EditableParameter("nPtsMin", "Minimum nPts", 10., 10., 0., 100.,1.,
                                          Parameter::Integer,StiTrack::kPointCount) );
  trackFilter->add( new EditableParameter("nPtsMax", "Maximum nPts", 60., 60., 0., 100.,1.,
                                          Parameter::Integer,StiTrack::kPointCount) );
  trackFilter->add(new EditableParameter("lengthUsed","Use Length", 1., 1., 0.,1.,1.,Parameter::Boolean, StiTrack::kTrackLength));
  trackFilter->add(new EditableParameter("lengthMin", "Min Length", 0., 0., -300.,   300.,2,Parameter::Double, StiTrack::kTrackLength));
  trackFilter->add(new EditableParameter("lengthMax", "Max Length", 300.,  300., -300.,   300.,2,Parameter::Double, StiTrack::kTrackLength));
  _trackFilter = trackFilter;
  */
  _trackFilter = new StiTrackFinderFilter();
  //_toolkit->setFinderTrackFilter(_trackFilter);
  cout << "StiKalmanTrackFinder::initialize() -I- Done"<<endl;
}

StiKalmanTrackFinder::StiKalmanTrackFinder(StiToolkit*toolkit)
:
_toolkit(toolkit),
_trackFilter(0),
_trackNodeFactory(0),
_detectorContainer(0),
_hitContainer(0),
_trackContainer(0)
{
  cout << "StiKalmanTrackFinder::StiKalmanTrackFinder() - Started"<<endl;
memset(mTimg,0,sizeof(mTimg));
  assert(_toolkit);
  cout << "StiKalmanTrackFinder::StiKalmanTrackFinder() - Done"<<endl;
}
//______________________________________________________________________________
/*!
Reset the state of the finder  to "event not tracked"
 <p>
 The track factory, the track container are reset. This
 method is distinct from the "clear" method which reset
 the state to "event not loaded".
 */
//______________________________________________________________________________
void StiKalmanTrackFinder::reset()
{
  //cout << "StiKalmanTrackFinder::reset() -I- Starting" <<endl;
  _detectorContainer->reset();
  _trackContainer->clear();
  _trackNodeFactory->reset();
  _hitContainer->reset();
  for (int j=0;j<(int)_seedFinders.size();j++) { _seedFinders[j]->reset();}
  //cout << "StiKalmanTrackFinder::reset() -I- Done" <<endl;
}

//______________________________________________________________________________
/*!
Reset the state of the finder  to "no event loaded"
 <p>
 A reset or clear command is used to all components this tracker
 depends on. This include the hitContainer, the detector container,
 the hit, track, track node, mc track factories, the track containers,
 and the seed finder.
 */
//______________________________________________________________________________
void StiKalmanTrackFinder::clear()
{
  //cout << "StiKalmanTrackFinder::clear() -I- Starting" <<endl;
  _hitContainer->clear();
  _detectorContainer->clear();
  _trackContainer->clear();
  for (int j=0;j<(int)_seedFinders.size();j++) { _seedFinders[j]->clear();}
  //cout << "StiKalmanTrackFinder::clear() -I- Done" <<endl;
}

//______________________________________________________________________________
/*! Find all tracks associated with the current event.
<p>
Algorithm: In a <b>while loop</b>, obtain track seeds from
current track seed finder and proceed to extend it through the
detector.
<p>Found tracks are added to the track container if no track
filter is set or if they satisfy the track filter requirements.
*/
//______________________________________________________________________________
void StiKalmanTrackFinder::findTracks()
{
  mEventPerm = kMaxEventPerm;
  assert(_trackContainer );
  extendSeeds (0.);
}
//________________________________________________________________________________
Int_t StiKalmanTrackFinder::Fit(StiKalmanTrack *track, Double_t rMin) {
  int errType = kNoErrors; // no err by default

  Int_t nTSeed=0,nTAdd=0,nTFail=0,nTFilt=0,status = kNoErrors;
  Int_t nTpcHits=0,nSvtHits=0,nSsdHits=0,nIstHits=0,nPxlHits=0;

  do { //technical do
    track->setFlag(-1);
    int opt = StiKalmanTrack::kAppRR|StiKalmanTrack::kAppUPD;
    status = track->approx(opt); // should be filled by track->initialize()
StiDebug::Count("Xi2Helx2",track->getXi2());
    if (status) 	{nTSeed++; errType = abs(status)*100 + kApproxFail; break;}

    status = track->fit(kOutsideIn);
    if (status) 	{nTSeed++; errType = abs(status)*100 + kFitFail; break;}
    status = extendTrack(track,rMin); // 0 = OK 
    if (!status) status = _trackFilter->filter(track);
    if (status) {nTFilt++; errType = abs(status)*100 + kCheckFail;}
    if (errType!=kNoErrors) {track->reduce(); return errType;}

    //cout << "  ++++++++++++++++++++++++++++++ Adding Track"<<endl;
    //		Add DCA node
    StiHit dcaHit; dcaHit.makeDca();
    StiTrackNode *extenDca = track->extendToVertex(&dcaHit);
    if (extenDca) {
      track->add(extenDca,kOutsideIn);
    }
    //		End DCA node
    track->reduce();
    nTAdd++;
    track->test();
    track->setFlag(1);
    _trackContainer->push_back(track);
    track->setId(_trackContainer->size());
    track->reserveHits();
    nTpcHits+=track->getFitPointCount(kTpcId);
    nSvtHits+=track->getFitPointCount(kSvtId);
    nSsdHits+=track->getFitPointCount(kSsdId);
    nIstHits+=track->getFitPointCount(kIstId);
    nPxlHits+=track->getFitPointCount(kPxlId);
    //cout << "  ++++++++++++++++++++++++++++++ Added Track"<<endl;
    LOG_DEBUG << Form("StiKalmanTrackFinder::Fit:nbSeed=%d nTFail=%d nTFilt=%d nTAdd=%d", 
		      nTSeed,nTFail,nTFilt,nTAdd) << endm;
    LOG_DEBUG << Form("StiKalmanTrackFinder::Fit:nTpcHits=%d nSvtHits=%d  nSsdHits=%d nPxlHits=%d nIstHits=%d",
		      nTpcHits,nSvtHits,nSsdHits,nPxlHits,nIstHits)
	      << endm;
  } while(0);
  return errType;
}
//______________________________________________________________________________
void StiKalmanTrackFinder::extendSeeds(double rMin)
{
  static int nCall=0;nCall++;
  StiKalmanTrack *track;
  int nTTot=0,nTOK=0;
  for (int isf = 0; isf<(int)_seedFinders.size();isf++) {
    _seedFinders[isf]->startEvent();
    int nTtot=0,nTok=0;
    while (true ){
// 		obtain track seed from seed finder
    
      if (mTimg[kSeedTimg]) mTimg[kSeedTimg]->Start(0);

      track = (StiKalmanTrack*)_seedFinders[isf]->findTrack(rMin);

      if (mTimg[kSeedTimg]) mTimg[kSeedTimg]->Stop();
      if (!track) break; // no more seeds
      track->reserveHits(0); 	//Set timesUsed hits to zero

      nTTot++;nTtot++;
      if (mTimg[kTrakTimg]) mTimg[kTrakTimg]->Start(0);
      Int_t errType = Fit(track,rMin);
      _seedFinders[isf]->FeedBack(errType == kNoErrors);
      if (errType) {
        BFactory::Free(track);
      }else        {
        nTOK++;
        int nHits = track->getFitPointCount(kTpcId);
        if (nHits>=15) nTok++;
	assert(track->getChi2()<1000);
      }
      if (mTimg[kTrakTimg]) mTimg[kTrakTimg]->Stop();
    } 
    Info("extendSeeds:","Pass_%d NSeeds=%d NTraks=%d",isf,nTtot,nTok);
  }
  Info("extendSeeds","nTTot = %d nTOK = %d\n",nTTot,nTOK);

}
//______________________________________________________________________________
void StiKalmanTrackFinder::extendTracks(double rMin)
{
assert(0);
}
//______________________________________________________________________________
int StiKalmanTrackFinder::extendTrack(StiKalmanTrack *track,double rMin)
{
  static int nCall=0; nCall++;
  int trackExtended   =0;  
  int trackExtendedOut=0;
  int status = 0;
    // invoke tracker to find or extend this track
    //cout <<"StiKalmanTrack::find(int) -I- Outside-in"<<endl;
  {
    if (debug()) cout << "StiKalmanTrack::find seed " << *((StiTrack *) track);
    trackExtended = find(track,kOutsideIn,rMin);
    if (trackExtended){/* in CA case not extended is ok*/}
    status = track->refit();
    if(status) return kNotRefitedIn;

  }
    // decide if an outward pass is needed.
  const StiKalmanTrackNode * outerMostNode = track->getOuterMostNode(kGoodHit);
  if (!outerMostNode)
  {
    track->setFlag(-1);
    return kNotExtended;
  }
  if (outerMostNode->getX()<185. )
  {
    trackExtendedOut= find(track,kInsideOut);
    if (!trackExtendedOut) return kExtended;
    status = track->refit();
    if(status) return kNotRefitedOut;
  }
  return kExtended;
}
//______________________________________________________________________________
void StiKalmanTrackFinder::extendTracksToVertices(const std::vector<StiHit*> &vertices)
{
  static const double RMAX2d=StiKalmanTrackFinderParameters::instance()->maxDca2dZeroXY();
  static const double DMAX3d=StiKalmanTrackFinderParameters::instance()->maxDca3dVertex();

  StiKalmanTrackNode *extended=0;
  int goodCount= 0, plus=0, minus=0;
  int nTracks = _trackContainer->size();
  int nVertex =         vertices.size();  
  if (!nVertex || !nTracks) return;

  for (int iTrack=0;iTrack<nTracks;iTrack++)		{
    StiKalmanTrack * track = (StiKalmanTrack*)(*_trackContainer)[iTrack];  

    StiKalmanTrackNode *bestNode=0;  
    int bestVertex=0;
    StThreeVectorD nearBeam;
    track->getNearBeam(&nearBeam);
    const StiKalmanTrackNode *dcaNode = track->getLastNode();
    if (!dcaNode->isDca()) 		continue;

    if (nearBeam.perp2()>RMAX2d*RMAX2d) 		continue;
    for (int iVertex=0;iVertex<nVertex;iVertex++) {
      StiHit *vertex = vertices[iVertex];
      if (fabs(track->getDca(vertex)) > DMAX3d)    	continue;
      if (mTimg[kPrimTimg]) mTimg[kPrimTimg]->Start(0);

      extended = (StiKalmanTrackNode*)track->extendToVertex(vertex);
      if (mTimg[kPrimTimg]) mTimg[kPrimTimg]->Stop();

      if (!extended) 					continue;
      if (!bestNode) {bestNode=extended;bestVertex=iVertex+1;continue;}
      if (bestNode->getChi2()+log(bestNode->getDeterm())
         <extended->getChi2()+log(extended->getDeterm()))continue;
      BFactory::Free(bestNode);
      bestNode = extended; bestVertex=iVertex+1;
    }//End vertex loop
    
    if(!bestNode) 			continue;
    track->add(bestNode,kOutsideIn);
    track->setPrimary(bestVertex);
    int         ifail = 0;
static int REFIT=2005;
    bestNode->setUntouched();
if (REFIT) {
    ifail = track->refit();
    ifail |= (track->getInnerMostHitNode(kGoodHit)!=bestNode);
}
    track->reduce();
// something is wrong. It is not a primary
    if (ifail) { track->removeLastNode(); track->setPrimary(0); continue;}
    goodCount++;
    if (track->getCharge()>0) plus++; else minus++;

  }//End track loop 
  _nPrimTracks = goodCount;
  if (debug()) {
    cout << "SKTF::extendTracksToVertices(...) -I- rawCount:"<<nTracks<<endl
	 << "                                 extendedCount:"<<goodCount<<endl
	 << "                                          plus:"<<plus<<endl
	 << "                                         minus:"<<minus<<endl;
  }
}

/// Find extension (track) to the given track seed in the given direction
/// Return Ok      if operation was successful
//______________________________________________________________________________
class StiKalmanTrackFinder::QAFind {
public: 
  	QAFind()		{reset();                  }
void 	reset()			{mRMin=0; mSum=0; mHits =0; mNits=0; mQA=0;mWits=0;mSits=0;}
double  rmin() const            {return mRMin;}
double  sum()  const            {return mSum ;}	//summ of chi2
   int  hits() const 		{return mHits;} //total number of hits
   int  nits() const  		{return mNits;} //total number of no hits
   int  sits() const  		{return mSits;} //total number of silicon hits
   int  wits() const  		{return mWits;} //total weight of precision hits
   int  qa()   const  		{return mQA  ;} // quality flag for current level
  void  setRMin(double rm) 	{mRMin = rm  ;}
  void  addXi2(double Xi2)      {mSum += Xi2 ;}
  void  addHit(const StiHit *hit){mHits++; mQA=1;
                                 if (hit->x() < kRMinTpc) { mSits++;
                                 mWits+=StiKalmanTrackFinderParameters::instance()->hitWeight((int)hit->x());}
                                }
  
  void  setHits(int nHits){mHits=nHits; mQA=1;}
  void  setNits(int nNits){mNits=nNits;}
  void  addNit()		{mNits++; mQA=-1;}
  void  setQA(int qa) 		{mQA = qa;}
   int  compQA(const QAFind &qaTry) const;
private:
  double mRMin;  //minimal radius allowed for search
  double mSum; 	//summ of chi2
  int    mHits; //total number of hits
  int    mNits; //total number of no hits
  int    mSits; //total number of silicon hits
  int    mWits; //total weight of precision hits
  int    mQA;	// quality flag for current level
		//   qa =  1 == new hit accepted
		//   qa =  0 == no hits was expected. dead material or edge
		//   qa = -1 == hit expected but not found
		//   qa = -2 == close to beam, stop processing of it
		//   qa = -3 == fake track, stop processing of it
		//   qa = -4 == track can not be continued, stop processing of it

};
//______________________________________________________________________________
int StiKalmanTrackFinder::QAFind::compQA(const QAFind &qaTry) const
{
// return > 0 qaTry is better
// return < 0 qaTry is worse
// return== 0 qaTry is the same

static const StiKalmanTrackFinderParameters *tfp = StiKalmanTrackFinderParameters::instance();
//	One SVT hit is worse than zero
   if (!mWits        &&  qaTry.wits() &&  qaTry.wits() < tfp->sumWeight()) return -999;
   if (!qaTry.wits() &&  mWits        &&  mWits        < tfp->sumWeight()) return  999;

   int ians = qaTry.wits()-mWits;       if (ians)	return ians;
   ians =  qaTry.sits()-mSits;		if (ians)	return ians;
   ians =  qaTry.hits()-mHits;		if (ians)	return ians;
   ians =  mNits- qaTry.nits();	        if (ians)	return ians;
   if (mSum  <= qaTry.sum() ) 				return -1;
   							return  1;
}

//______________________________________________________________________________
bool StiKalmanTrackFinder::find(StiTrack * t, int direction,double rmin) 
{
static int nCall=0; nCall++;
  gLevelOfFind = 0;
  int nnBef,nnAft;
  double lnBef,lnAft;
  
  if(direction) rmin=0; //no limitation to outside
  StiKalmanTrack *track = dynamic_cast<StiKalmanTrack *> (t);
  nnBef = track->getNNodes(kGoodHit);
  lnBef = track->getTrackLength();

  StiKalmanTrackNode *leadNode = track->getInnOutMostNode(direction,kGoodHit);
  if (!leadNode) return 0;
  leadNode->cutTail(direction);
  track->setFirstLastNode(leadNode);
  QAFind qa; qa.setRMin(rmin);
  mTrackPerm = kMaxTrackPerm;
  mUseComb = useComb();
  qa.setHits(track->getPointCount());
  qa.setNits(track->getGapCount()   );
  if (direction && qa.hits()<5) 	return 0;
  find(track,direction,leadNode,qa);
assert(qa.hits()==track->getPointCount());

  track->setCombUsed(mUseComb);
  track->setFirstLastNode(leadNode);
  nnAft = track->getNNodes(kGoodHit);
  lnAft = track->getTrackLength();
  return (nnAft>nnBef || lnAft>(lnBef+0.5));
}
//______________________________________________________________________________
void StiKalmanTrackFinder::find(StiKalmanTrack * track, int direction
                              ,StiKalmanTrackNode *leadNode,QAFind &qa) 
{
static int nCall=0; nCall++;

static const double degToRad = 3.1415927/180.;
static const double radToDeg = 180./3.1415927;
static const double ref1  = 50.*degToRad;
//static  const double ref2  = 2.*3.1415927-ref1;
static  const double ref1a  = 110.*degToRad;
assert(direction || leadNode==track->getLastNode());



  //  const double ref2a  = 2.*3.1415927-ref1a;
  gLevelOfFind++;
  --mEventPerm;
  assert(mEventPerm>=0 && "FATAL::TOO MANY permutations");
  if (--mTrackPerm==0) { mUseComb = 0; }

  StiDetector *tDet=0;
  int status;
  StiKalmanTrackNode testNode;
  int position;
  StiHit * stiHit;
  double  leadRadius;

  const StiDetector *leadDet = leadNode->getDetector();
  leadRadius = leadDet->getPlacement()->getLayerRadius();
  assert(leadRadius>0 && leadRadius<1000);
  if (leadRadius < qa.rmin()) {gLevelOfFind--;qa.setQA(-4);return;}
  
  double xg = leadNode->x_g();
  double yg = leadNode->y_g();
  double projAngle = atan2(yg,xg);
  if(debug() > 2)cout << "Projection Angle:"<<projAngle*180/3.1415<<endl;
    
  vector<StiDetectorNode*>::const_iterator layer;
  vector<StiDetectorNode*>::const_reverse_iterator rlayer;

  if ((!direction)) {
    if (debug() > 2) cout <<endl<< "out-in"<<endl;
    rlayer=_detectorContainer->rbeginRadial(leadDet); rlayer++;
  } else {
    if (debug() > 2) cout <<endl<< "in-out"<<endl;
    layer=_detectorContainer->beginRadial(leadDet);    layer++;
  }

  if (debug() > 2) cout <<endl<< "lead node:" << *leadNode<<endl<<"lead det:"<<*leadDet<<endl;

  
  while (((!direction)? rlayer!=_detectorContainer->rendRadial() : layer!=_detectorContainer->endRadial()))
  {do{//technical do
    vector<StiDetectorNode*>::const_iterator sector;
    vector<StiDetector*> detectors;
    if (debug() > 2) cout << endl<<"lead node:" << *leadNode<<endl<<" lead det:"<<*leadDet;

      //find all relevant detectors to visit.
    sector = (!direction)? _detectorContainer->beginPhi(rlayer):_detectorContainer->beginPhi(layer);
    for ( ; (!direction)? sector!=_detectorContainer->endPhi(rlayer):sector!=_detectorContainer->endPhi(layer); ++sector)
    {
       StiDetector * detector = (*sector)->getData();
       if (detector == leadDet) continue;
       double angle  = detector->getPlacement()->getLayerAngle();
       double radius = detector->getPlacement()->getLayerRadius();
       if (radius < qa.rmin()) {gLevelOfFind--; qa.setQA(-4);return;}
       double diff = radius-leadRadius;if (!direction) diff = -diff;
       if (diff<-1e-6 && debug()>3) {
          LOG_DEBUG << Form("TrackFinder: Wrong order: (%s).(%g) and (%s).(%g)"
	  ,leadDet->getName().c_str(),leadRadius 
	  ,detector->getName().c_str(),radius) << endm;
       }
       
       
       Int_t shapeCode = detector->getShape()->getShapeCode();
       Double_t OpenAngle = ref1;
       if (shapeCode >= kCylindrical) {
	 OpenAngle = ((StiCylindricalShape *) detector->getShape())->getOpeningAngle();
       } else {
	 if (radius <= 50 )  OpenAngle = ref1a;
       }
       diff = projAngle-angle;
       if (diff >  M_PI) diff -= 2*M_PI;
       if (diff < -M_PI) diff += 2*M_PI;
       if (fabs(diff) > OpenAngle)	continue;
       detectors.push_back(detector);
    }
//		list of detectors candidates created
    int nDets = detectors.size(); 
    if (!nDets) continue;
//		and sorted by Phi angle
    if (nDets>1) sort(detectors.begin(),detectors.end(),CloserAngle(projAngle) );

//		There is additional loop. 1st loop for active only, second for non active
    int foundInDetLoop = 0;
    for (int nowActive=1; nowActive>=0; nowActive--) { //Additional activeNonActive loop

    for (vector<StiDetector*>::const_iterator d=detectors.begin();d!=detectors.end();++d)
    {
      tDet = *d;
      if ((tDet->isActive() != nowActive)) continue;
      if (debug() > 2) {
	cout << endl<< "target det:"<< *tDet;
	cout << endl<< "lead angle:" << projAngle*radToDeg 
	     <<" this angle:" << radToDeg*(*d)->getPlacement()->getNormalRefAngle()<<endl;
      }
      //begin tracking here...
      testNode.reduce();testNode.reset();
      testNode.setChi2(1e55);
      position = testNode.propagate(leadNode,tDet,direction);
      if (position) { 
	continue; // missed, will try the next available volume on this layer
      }
      testNode.setDetector(tDet);
      foundInDetLoop = 2016;
      int active = tDet->isActive(testNode.getY(),testNode.getZ());

      double maxChi2 = tDet->getTrackingParameters()->getMaxChi2ForSelection();

      StiHitContino hitCont;

      if (active) {

	if (debug() > 2)cout<<" search hits";
	// active detector may have a hit
	vector<StiHit*> & candidateHits = _hitContainer->getHits(testNode);//,true);
	vector<StiHit*>::iterator hitIter;
	for (hitIter=candidateHits.begin();hitIter!=candidateHits.end();++hitIter)
	{
	  stiHit = *hitIter;
          if (stiHit->detector() && stiHit->detector()!=tDet) continue;
          status = testNode.nudge(stiHit);
          testNode.setReady();
          if (status)		continue;
	  chi2 = testNode.evaluateChi2(stiHit);
	  if (chi2>maxChi2) 	continue;
	  hitCont.add(stiHit,chi2,testNode.getDeterm());
	  if (debug() > 2) cout << " hit selected"<<endl;
	}// for (hitIter)
      }//if(active)

      int nHits = hitCont.getNHits();

      testNode.setHitCand(nHits);
      if (direction) {
        nHits=1;
      } else {
        int flg = (testNode.getX()< kRMinTpc)? mUseComb &3:mUseComb>>2;
        if ((flg&2) || !nHits) 	nHits++;
        if ((flg&1)==0) 	nHits=1;
        
      }

      QAFind qaBest(qa),qaTry;
      for (int jHit=0;jHit<nHits; jHit++)
      {//Loop over Hits
        stiHit = hitCont.getHit(jHit);
	StiKalmanTrackNode * node = _trackNodeFactory->getInstance();
	*node = testNode;
        status = 0;
        do {//fake do
          if (!stiHit) break;
          node->setIHitCand(jHit);
          node->setHit(stiHit);
          status = node->updateNode();
          if (status)  break;
          node->setChi2(hitCont.getChi2(jHit));
          if (!direction && node->getX()< kRMinTpc) node->saveInfo(); //Save info for pulls 
        }while(0);
        if (status)  {_trackNodeFactory->free(node); continue;}

        qaTry = qa;
	track->add(node,direction,leadNode);
        nodeQA(node,position,active,qaTry);
        find(track,direction,node,qaTry);
        if (jHit==0) { qaBest=qaTry; continue;}
        int igor = qaBest.compQA(qaTry);
        if (igor<0)  { leadNode->remove(0);}
        else         { leadNode->remove(1);qaBest=qaTry;}
      }
      qa = qaBest; gLevelOfFind--; qa.setQA(-4); return;
    }//End Detectors
    if (foundInDetLoop) break;		//activeNonActive
    } //End of activeNonActive loop;
  }while(0);
  if(!direction){++rlayer;}else{++layer;}}
//end layers
  gLevelOfFind--;qa.setQA(-4);
  return;
}
//______________________________________________________________________________
void StiKalmanTrackFinder::nodeQA(StiKalmanTrackNode *node, int position
                                 ,int active,QAFind &qa)
{
  int maxNullCount           = StiKalmanTrackFinderParameters::instance()->maxNullCount()+3;
  int maxContiguousNullCount = StiKalmanTrackFinderParameters::instance()->maxContiguousNullCount()+3;
//		Check and count node
  StiHit *hit = node->getHit();
  if (hit) {
    if (debug() > 2)cout << " got Hit! "<<endl ;
//  const StiDetector *detector = hit->detector();
    qa.addXi2(node->getChi2() + log(node->getDeterm()));
    qa.addHit(hit);
    node->incHitCount();
    node->incContigHitCount();

    if (node->getContigHitCount()>StiKalmanTrackFinderParameters::instance()->minContiguousHitCountForNullReset())
       node->setContigNullCount();

  } else if (position>0 || !active) {// detectors edge - don't really expect a hit here
    qa.setQA(0);

  } else {// there should have been a hit but we found none
      if (debug() > 2) cout << " no hit but expected one"<<endl;
      node->incNullCount(); 
      node->incContigNullCount();
      node->setContigHitCount();
      qa.addNit();
      if (node->getNullCount()>maxNullCount) 			qa.setQA(-3);
      if (node->getContigNullCount()>maxContiguousNullCount)	qa.setQA(-3);
  }

}

//______________________________________________________________________________
StiTrack * StiKalmanTrackFinder::findTrack(double rMin)
{
assert(0); return 0;
}

//______________________________________________________________________________
void StiKalmanTrackFinder::setTiming()
{
  for (int it=0;it<(int)(sizeof(mTimg)/sizeof(mTimg[0]));it++){
    mTimg[it]= new TStopwatch(); mTimg[it]->Stop();    } 
}
//______________________________________________________________________________
void StiKalmanTrackFinder::finish() const
{
static const char *timg[] = {"SeedFnd","TrakFnd","PrimFnd",0};
  if (mTimg[0]) {
    for (int i=0;timg[i];i++) {
      Info("TrackFinder::Timing","%s(%d) \tCpuTime = %6.2f seconds,\tPerTrak = %g seconds"
      ,timg[i],mTimg[i]->Counter(),mTimg[i]->CpuTime()
      ,mTimg[i]->CpuTime()/mTimg[i]->Counter());    
  } }
}
//______________________________________________________________________________
int StiKalmanTrackFinder::getNTracks() const 
{ return _trackContainer->size();}

//______________________________________________________________________________
CloserAngle::CloserAngle(double refAngle)
  : _refAngle(refAngle)
{ }

//______________________________________________________________________________
bool CloserAngle::operator()(const StiDetector*lhs, const StiDetector* rhs)
{
  double lhsa = lhs->getPlacement()->getNormalRefAngle();
  double rhsa = rhs->getPlacement()->getNormalRefAngle();
  double lhsda = fabs(lhsa-_refAngle); if (lhsda>3.1415) lhsda-=3.1415;
  double rhsda = fabs(rhsa-_refAngle); if (rhsda>3.1415) rhsda-=3.1415;
  return lhsda<rhsda;
}

