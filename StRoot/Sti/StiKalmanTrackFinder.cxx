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
#include "Stiostream.h"
#include <stdexcept>
#include <math.h>
using namespace std;
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
#include "StiVertexFinder.h"
#include "StiDefaultTrackFilter.h"
#include "StiTrackFinderFilter.h"

#define TIME_StiKalmanTrackFinder
#ifdef TIME_StiKalmanTrackFinder
#include "Sti/StiTimer.h"
#endif

int StiKalmanTrackFinder::_debug = 0;
ostream& operator<<(ostream&, const StiTrack&);

//______________________________________________________________________________
void StiKalmanTrackFinder::initialize()
{
  cout << "StiKalmanTrackFinder::initialize() -I- Started"<<endl;
  lastMove = 0;
  _toolkit = StiToolkit::instance();
  _trackNodeFactory  = _toolkit->getTrackNodeFactory();
  _trackFactory      = _toolkit->getTrackFactory();
  _mcTrackFactory    = _toolkit->getMcTrackFactory();
  _hitFactory        = _toolkit->getHitFactory();
  _detectorContainer = _toolkit->getDetectorContainer();
  _detectorContainer->reset();
  _trackSeedFinder   = _toolkit->getTrackSeedFinder();
  _hitContainer      = _toolkit->getHitContainer();
  _trackContainer    = _toolkit->getTrackContainer();
  _mcTrackContainer  = _toolkit->getMcTrackContainer();
  _vertexFinder      = _toolkit->getVertexFinder();
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
: _toolkit(toolkit),
_trackFilter(0),
_trackSeedFinder(0),
_trackNodeFactory(0),
_trackFactory(0),
_mcTrackFactory(0),
_hitFactory(0),
_detectorContainer(0),
_hitContainer(0),
_trackContainer(0),
_mcTrackContainer(0),
_vertexFinder(0),
_eventFiller(0),
_event(0),
_mcEvent(0)
{
  cout << "StiKalmanTrackFinder::StiKalmanTrackFinder() - Started"<<endl;
StiKalmanTrack::setParameters(&_pars);
StiKalmanTrackNode::setParameters(&_pars);
  _pars.setName("KalmanTrackFinderParameters");
  if (!_toolkit)
    throw runtime_error("StiKalmanTrackFinder::StiKalmanTrackFinder(...) - FATAL - toolkit==0");
  cout << "StiKalmanTrackFinder::StiKalmanTrackFinder() - Done"<<endl;
}

//______________________________________________________________________________
StiKalmanTrackFinder::~StiKalmanTrackFinder()
{ }

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
  _trackFactory->reset();
  _trackNodeFactory->reset();
  _hitContainer->reset();
  _trackSeedFinder->reset();
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
  _mcTrackContainer->clear();
  _hitContainer->clear();
  _hitFactory->reset();
  _mcTrackFactory->reset();
  reset();
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
  if (!_trackContainer)   throw runtime_error("StiKalmanTrackFinder::findTracks() -F- _trackContainer==0");
  if (!_trackSeedFinder)  throw runtime_error("StiKalmanTrackFinder::findTracks() -F- _trackSeedFinder==0");
  StiTrack * track;
  _trackSeedFinder->reset();
  _trackContainer->clear();
  if (_trackFilter) _trackFilter->reset();
  try
    {
      while (true)
        {
        try
        {
          // obtain track seed from seed finder
static int nCall=0;nCall++;
          track = _trackSeedFinder->findTrack();
          if (!track) break; // no more seeds
          track->find();
	  //cout << " find completed" << endl;
	  if (track->getFlag()<1) continue;
	  //cout << " apply filter" << endl;
          if (!_trackFilter || _trackFilter->filter(track))
            {
	      //cout << "  ++++++++++++++++++++++++++++++ Adding Track"<<endl;
            _trackContainer->push_back(track);
            static_cast<StiKalmanTrack*>(track)->reserveHits();
	    //cout << "  ++++++++++++++++++++++++++++++ Added Track"<<endl;
	    if (track->getChi2()<0) track->setFlag(2);


	    //StiKalmanTrack * kt = static_cast<StiKalmanTrack*>(track);
	    //StiKalmanTrackNode * n = kt->extrapolateToBeam();
	    //if (n) cout << "node at beam:"<<*n<<endl;
	    //n = kt->extrapolateToRadius(220.);
	    //if (n) cout << "node at 220 cm:"<<*n<<endl;

            }

	  //else
	  //cout << " track not saved +++++++++++++++++++++++++++++++++++++!!!!!!" << endl;
        }
        catch (runtime_error & rte)
        {
          cout<< "StiKalmanTrackFinder::findTracks() - Run Time Error :" << rte.what() << endl;
        }
        }
    }
  catch (runtime_error & rte)
    {
    cout << "StiKalmanTrackFinder::findTracks() - Run Time Error (2) :" << rte.what() << endl;
    }
  //if (_trackFilter)
  //cout << "  Tracks Analyzed:"<< _trackFilter->getAnalyzedCount() << endl
  //			 << "         Accepted:"<< _trackFilter->getAcceptedCount() << endl;
  //else
  //  cout << "SKTF::findTracks() -I- Done"<<endl;
}

/* Fit all track produced by the track seed finder.
<p>
This method is useful when the seed finder returns full tracks produced
by a 3rd party track finder e.g. the tpt package.
<p>Fitted tracks are added to the track container if no track
filter is set or if they satisfy the track filter requirements.
void StiKalmanTrackFinder::fitTracks()
{
  StiTrack * track = 0;
  try
    {
      track = _trackSeedFinder->findTrack();
      if (track)
        {
        track->fit(kOutsideIn); //track->setFlag(0);
        if (!_trackFilter || _trackFilter->filter(track))  _trackContainer->push_back(track);
        }
    }
  catch (runtime_error & rte)
    {
    cout << "StiKalmanTrackFinder::fitTracks() - Run Time Error :" << rte.what() << endl;
    }
}
*/

//______________________________________________________________________________
/*
 Extend all known tracks to primary vertex
 <p>
 Attempt an extension of all known tracks to the given primary vertex. If the extension is successfull,
 the vertex is added to the track as a node. Node that in this implementation, it is assumed the
 track has been pruned and thus consists of a single node sequence (as opposed to a tree).
 <p>
 <ol>
 <li>Loop on all tracks currently stored in track container.</li>
 <li>It is assumed that the track does not already have a main vertex associated with it.</li>
 <li>Attempt extension to the given vertex by a call to "extendToMainVertex".
 <li>If extension is successfull, the given vertex is added as node to the track.
 </ol>
 <p>
 <h3>Note</h3>
 Any exception thrown by "getInnerMostNode()" or "extendTrackToVertex()" are
 caught here and reported with "cout".
 */
//______________________________________________________________________________
void StiKalmanTrackFinder::extendTracksToVertex(StiHit* vertex)
{
  //cout << "SKTF::extendTracksToVertex() - vertex position " << vertex->x_g() << ", " << vertex->y_g() << ", " << vertex->z_g() << endl;

  int rawCount = 0;
  int goodCount= 0;
  int plus=0;
  int minus=0;
  for (vector<StiTrack*>::const_iterator it=_trackContainer->begin();
       it!=_trackContainer->end();
       ++it)
    {
      try
	{
	  rawCount++;
	  StiKalmanTrack * track = dynamic_cast<StiKalmanTrack*>(*it);
	  if (!track) continue;
	  bool extended = false;
	  StiKalmanTrackNode * inner = track->getInnerMostNode();
	  double r = inner->getRefPosition();
	  if (r>4.1 && r<50) find(track,kOutsideIn);
	  extended = track->extendToVertex(vertex);
	  // simple diagnostics
	  if (extended) goodCount++;
	  if (track->getCharge()>0)
	    plus++;
	  else
	    minus++;
	}
      catch (runtime_error & rte)
	{
	  cout << "SKTF::extendTracksToVertex()"
	       << "-W- Run Time Error while extending a track to main vertex."<<endl
	       << "Error Message:" << endl
	       << rte.what() << endl;
	}
    }
  cout << "SKTF::extendTracksToVertex(StiHit* vertex) -I- rawCount:"<<rawCount<<endl
       << "                                          extendedCount:"<<goodCount<<endl
       << "                                                   plus:"<<plus<<endl
       << "                                                  minus:"<<minus<<endl;
}

/// Find extension (track) to the given track seed in the given direction
/// Return Ok      if operation was successful
//______________________________________________________________________________
class StiKalmanTrackFinder::QAFind {
public: 
  double sum; 	//summ of chi2
  int    hits;  //total number of hits
  int    nits;  //total number of no hits
  int    qa;	// quality flag for current level
		//   qa =  1 == new hit accepted
		//   qa =  0 == no hits was expected. dead material or edge
		//   qa = -1 == hit expected but not found
		//   qa = -2 == close to beam, stop processing of it
		//   qa = -3 == fake track, stop processing of it

  	QAFind()		{reset();                  }
void 	reset()			{sum=0; hits =0; nits=0; qa=0;}
void operator+=(const QAFind &q){sum+=q.sum; hits+=q.hits; nits+=q.nits;}
};

//______________________________________________________________________________
bool StiKalmanTrackFinder::find(StiTrack * t, int direction) // throws runtime_error, logic_error
{
static int nCall=0; nCall++;
StiKalmanTrackNode::Break(nCall);
  
  track = dynamic_cast<StiKalmanTrack *> (t);
  int dirIn = (direction==kOutsideIn);
  StiKalmanTrackNode *leadNode = track->getInnOutMostNode(!dirIn,2);
  if (!leadNode) throw runtime_error("SKTF::find() -E- track leadNode ==0");
  leadNode->cutTail(direction);
  assert(leadNode->isValid());
  QAFind qa;
  find(track,direction,leadNode,qa);
  track->setFirstLastNode(leadNode);
  return qa.hits>0;
}

//______________________________________________________________________________
void StiKalmanTrackFinder::find(StiKalmanTrack * track, int direction
                              ,StiKalmanTrackNode *leadNode,QAFind &qa) 
{
static int nCall=0; nCall++;
StiKalmanTrackNode::Break(nCall);

  const double degToRad = 3.1415927/180.;
  const double radToDeg = 180./3.1415927;
  const double ref1  = 50.*degToRad;
  //  const double ref2  = 2.*3.1415927-ref1;
  const double ref1a  = 110.*degToRad;
  //  const double ref2a  = 2.*3.1415927-ref1a;

  int status;
  int dirIn = (direction==kOutsideIn);
  StiKalmanTrackNode testNode;
  int position;
  StiHit * stiHit;
  double  leadAngle,leadRadius;

  assert(leadNode->isValid());
  const StiDetector *leadDet    = leadNode->getDetector();
  if (!leadDet)  throw runtime_error("SKTF::find() -E- leadDet==0");
  leadRadius = leadDet->getPlacement()->getNormalRadius();
  leadAngle  = leadDet->getPlacement()->getNormalRefAngle();


////  if (dirIn && !nRefit && leadRadius <100 && track->getNNodes(3)>10) {
////     nRefit++; track->refit(); if (!leadNode->isValid()) return 0;
////  }
  
  
  double xg = leadNode->x_g();
  double yg = leadNode->y_g();
  double projAngle = atan2(yg,xg);
  if(debug() > 2)cout << "Projection Angle:"<<projAngle*180/3.1415<<endl;
    
  vector<StiDetectorNode*>::const_iterator layer;
  vector<StiDetectorNode*>::const_reverse_iterator rlayer;

  if (dirIn) {
    if (debug() > 2) cout <<endl<< "out-in"<<endl;
    rlayer=_detectorContainer->rbeginRadial(leadDet); rlayer++;
  } else {
    if (debug() > 2) cout <<endl<< "in-out"<<endl;
    layer=_detectorContainer->beginRadial(leadDet);    layer++;
  }

  if (debug() > 2) cout <<endl<< "lead node:" << *leadNode<<endl<<"lead det:"<<*leadDet<<endl;

  while (((dirIn)? rlayer!=_detectorContainer->rendRadial() : layer!=_detectorContainer->endRadial()))
  {do{//technical do
    vector<StiDetectorNode*>::const_iterator sector;
    vector<StiDetector*> detectors;
    if (debug() > 2) cout << endl<<"lead node:" << *leadNode<<endl<<" lead det:"<<*leadDet;


      //find all relevant detectors to visit.
    sector = (dirIn)? _detectorContainer->beginPhi(rlayer):_detectorContainer->beginPhi(layer);
    while ( (dirIn)? sector!=_detectorContainer->endPhi(rlayer):sector!=_detectorContainer->endPhi(layer) )
    {
       StiDetector * detector = (*sector)->getData();
       double angle  = detector->getPlacement()->getNormalRefAngle();
       double radius = detector->getPlacement()->getNormalRadius();
       double diff = radius-leadRadius;if (dirIn) diff = -diff;
       if (diff<-1e-6 && debug()>3) {
          printf("TrackFinder: Wrong order: (%s).(%g) and (%s).(%g)\n"
	  ,leadDet->getName().c_str(),leadRadius 
	  ,detector->getName().c_str(),radius);
       }
       
       
       Int_t shapeCode = detector->getShape()->getShapeCode();
       Double_t OpenAngle = ref1;
       if (shapeCode >= kCylindrical) {
	 OpenAngle = ((StiCylindricalShape *) detector->getShape())->getOpeningAngle();
       } else {
	 if (radius <= 50 && radius>4.2)  OpenAngle = ref1a;
       }
       diff = projAngle-angle;
       if (diff >  M_PI) diff -= 2*M_PI;
       if (diff < -M_PI) diff += 2*M_PI;
       if (fabs(diff) <= OpenAngle) { detectors.push_back(detector);}
       ++sector;
    }
    int nDets = detectors.size(); 
    if (debug() > 2 && nDets==0) cout << "no detector of interest on this layer"<<endl;
    if (!nDets) continue;
    if (nDets>1) sort(detectors.begin(),detectors.end(),CloserAngle(projAngle) );
    for (vector<StiDetector*>::const_iterator d=detectors.begin();d!=detectors.end();++d){
      tDet = *d;
      if (debug() > 2) {
	cout << endl<< "target det:"<< *tDet;
	cout << endl<< "lead angle:" << projAngle*radToDeg 
	     <<" this angle:" << radToDeg*(*d)->getPlacement()->getNormalRefAngle()<<endl;
      }
      //begin tracking here...
      testNode.reset();
      testNode.setChi2(1e55);
      position = testNode.propagate(leadNode,tDet,direction);

      // CP Nov 2 Try doubling the chi2 in the SVT
      //if (testNode.getX()<40.) maxChi2 = 2* maxChi2;
      if(debug() > 2)  cout << "propagate returned:"<<position<<endl<< "testNode:"<<testNode;
      if (position<0 || position>kEdgeZplus) { 
	// not reaching this detector layer - stop track
	if (debug() > 2) cout << "TRACK DOES NOT REACH CURRENT volume"<<endl;
	if (debug() > 1) cout << StiKalmanTrackNode::Comment() << endl;
	continue; // will try the next available volume on this layer
      }

      if (testNode.isEnded()) { qa.qa= -4; return;}
      if (debug() > 2) cout << "position " << position << "<=kEdgeZplus";
      assert(testNode.isValid());
      testNode.setDetector(tDet);
      int active = tDet->isActive(testNode.getY(),testNode.getZ());

      if (debug() > 2) cout << " vol active:" << active<<endl;
      double maxChi2 = tDet->getTrackingParameters()->getMaxChi2ForSelection();


      StiHitContino hitCont;
      if (active) {

	if (debug() > 2)cout<<" search hits";
	// active detector may have a hit
	vector<StiHit*> & candidateHits = _hitContainer->getHits(testNode);//,true);
	vector<StiHit*>::iterator hitIter;
	if (debug() > 2) cout << " candidates:"<< candidateHits.size();
	for (hitIter=candidateHits.begin();hitIter!=candidateHits.end();++hitIter)
	{
	  stiHit = *hitIter;
          if (stiHit->detector() && stiHit->detector()!=tDet) continue;
          status = testNode.nudge(stiHit);
          testNode.setReady();
          if (status)		continue;
	  chi2 = testNode.evaluateChi2(stiHit);
	  if (debug() > 2)   cout<< " got chi2:"<< chi2 << " for hit:"<<*stiHit<<endl;
	  if (chi2>maxChi2) 	continue;
	  hitCont.add(stiHit,chi2);
	  if (debug() > 2) cout << " hit selected"<<endl;
	}// for (hitIter)
      }//if(active)

      int nHits = hitCont.getNHits();
      if (nHits==0) nHits=1;
      if (direction) nHits=1;
//      if (direction || testNode.getX()>22.5) nHits=1;
//      nHits=1;
      QAFind qaBest,qaTry;
      for (int jHit=0;jHit<nHits; jHit++)
      {//Loop over Hits
        stiHit = hitCont.getHit(jHit);
	StiKalmanTrackNode * node = _trackNodeFactory->getInstance();
	node->reset(); *node = testNode;
        status = 0;
        do {//fake do
          if (!stiHit) break;
          node->setHit(stiHit);
          status = node->updateNode();
          if (status)  break;
          node->setChi2(hitCont.getChi2(jHit));
        }while(0);
        if (status)  continue;

        nodeQA(node,position,active,qaTry);
	leadNode->add(node,direction);

        if (qaTry.qa>-2) find(track,direction,node,qaTry);

        if (jHit==0) { qaBest=qaTry; continue;}
        int igor = compQA(qaBest,qaTry);
        if (igor<0)  { leadNode->remove(0);}
        else         { leadNode->remove(1);qaBest=qaTry;}
      }
      qa += qaBest;
      return;
    }//End Detectors
  }while(0);if(dirIn){++rlayer;}else{++layer;}}
//end layers
  return;
}


//______________________________________________________________________________
void StiKalmanTrackFinder::nodeQA(StiKalmanTrackNode *node, int position
                                 ,int active,QAFind &qa)
{

  qa.reset();

  int maxNullCount           = _pars.maxNullCount+3;
  int maxContiguousNullCount = _pars.maxContiguousNullCount+3;
//		Check and count node
  if (node->getHit()) {
    if (debug() > 2)cout << " got Hit! "<<endl ;
    qa.sum = node->getChi2();
    qa.hits=1; qa.qa=1;
    node->getHitCount()++;
    node->getContigHitCount()++;
    if (node->getContigHitCount()>_pars.minContiguousHitCountForNullReset)
       node->getContigNullCount() = 0;

  } else if (position>0 || !active) {// detectors edge - don't really expect a hit here
    qa.qa=0;

  } else {// there should have been a hit but we found none
      if (debug() > 2) cout << " no hit but expected one"<<endl;
      node->getNullCount()++; 
      node->getContigNullCount()++;
      node->getContigHitCount() = 0;
      qa.nits=1; qa.qa=-1;
      if (node->getNullCount()>maxNullCount) 			qa.qa= -3;
      if (node->getContigNullCount()>maxContiguousNullCount)	qa.qa= -3;
  }//node->getHit()

  double xg = node->x_g();
  double yg = node->y_g();
  if (sqrt(xg*xg + yg*yg) < 4.2) qa.qa= -2;

}
//______________________________________________________________________________
int StiKalmanTrackFinder::compQA(QAFind &qaBest,QAFind &qaTry)
{

   if ( qaTry.qa   ==         -3)  	return -1;
   if (qaBest.qa   ==         -3)  	return  1;
   if (qaBest.hits >  qaTry.hits) 	return -1;
   if (qaBest.hits <  qaTry.hits) 	return  1;
   if (qaBest.nits <  qaTry.nits) 	return -1;
   if (qaBest.nits >  qaTry.nits) 	return  1;
   if (qaBest.sum  <= qaTry.sum ) 	return -1;
   					return  1;
}

//______________________________________________________________________________
StiTrack * StiKalmanTrackFinder::findTrack()
{
  StiTrack * track = 0;
  try
    {
      if (!_trackSeedFinder) throw runtime_error("StiKalmanTrackFinder::findTrack() -E- No Track seed finder instance available");
      track = _trackSeedFinder->findTrack();
      if (track)
        {
        track->find();
        if (!_trackFilter || _trackFilter->filter(track) ) _trackContainer->push_back(track);
        }
    }
  catch (runtime_error & rte)
    {
    cout << "StiKalmanTrackFinder::findTrack() - Run Time Error :\n" << rte.what() << endl;
    }
  return track;
}

/*
 void StiKalmanTrackFinder::fitNextTrack()
 {
   StiTrack * track = 0;
   try
     {
       track = _trackSeedFinder->findTrack();
       if (track)
         {
         track->fit(kOutsideIn);
         if (!_trackFilter || _trackFilter->filter(track) ) _trackContainer->push_back(track);
         }
     }
   catch (runtime_error & rte)
     {
     cout << "StiKalmanTrackFinder::fitNextTrack() - Run Time Error :" << rte.what() << endl;
     }
 }
 */

//______________________________________________________________________________
void StiKalmanTrackFinder::setParameters(const StiKalmanTrackFinderParameters & par)
{
  _pars = par;
}

//______________________________________________________________________________
EditableParameters & StiKalmanTrackFinder::getParameters()
{
  return _pars;
}

//______________________________________________________________________________
/// Get the vertex finder used by this track finder
StiVertexFinder * StiKalmanTrackFinder::getVertexFinder()
{
  return _vertexFinder;
}

//______________________________________________________________________________
/// Set the vertex finder used by this tracker
void StiKalmanTrackFinder::setVertexFinder(StiVertexFinder *vertexFinder)
{
  _vertexFinder = vertexFinder;
}


//______________________________________________________________________________
void StiKalmanTrackFinder::loadDS(TDataSet&ds)
{
  cout << "StiKalmanTrackFinder::loadDS(TDataSet*ds) -I- Starting" << endl;
  _pars.loadDS(ds);
  cout << "StiKalmanTrackFinder::loadDS(TDataSet*ds) -I- Done" << endl;
}

//______________________________________________________________________________
void StiKalmanTrackFinder::loadFS(ifstream & iFile)
{
  cout << "StiKalmanTrackFinder::loadFS(ifstream&) -I- Starting" << endl;
  _pars.loadFS(iFile);
  cout << "StiKalmanTrackFinder::loadFS(ifstream&) -I- Done" << endl;
}


//______________________________________________________________________________
void StiKalmanTrackFinder::setDefaults()
{
  cout << "StiKalmanTrackFinder::setDefaults() -I- Starting" << endl;
  _pars.setDefaults();
  cout << "StiKalmanTrackFinder::setDefaults() -I- Done" << endl;
}

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


/*if (inner->getX() < 4.5) 
  else
  {
  //xxxxxxxxxxxxxxxxx
  cout << " SPECIAL extend to vertex attempted" << endl;
  _detectorContainer->setToDetector(inner->getDetector());
  StiDetector * currentDet = **_detectorContainer;
  _detectorContainer->moveIn();
  StiDetector * tDet = **_detectorContainer;   
  if (!tDet || currentDet==tDet)
  {
  cout << "no more detectors to go to..."<<endl;
  }
  cout << *tDet<<endl;
  track->extendToVertex(vertex,tDet);
  cout << " SPECIAL extend to vertex - end" << endl;
  }*/
