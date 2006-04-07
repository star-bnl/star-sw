/*
 * $Id: StiTrackingPlots.cxx,v 2.27 2006/04/07 18:01:57 perev Exp $
 *
 *
 * $Log: StiTrackingPlots.cxx,v $
 * Revision 2.27  2006/04/07 18:01:57  perev
 * Back to the latest Sti
 *
 * Revision 2.25  2006/02/14 18:18:45  perev
 * getGlobalDca ==> getDca
 *
 * Revision 2.24  2005/02/07 18:33:43  fisyak
 * Add VMC dead material
 *
 * Revision 2.23  2005/01/21 03:15:05  pruneau
 * commented out offending cout
 *
 * Revision 2.22  2005/01/17 03:59:27  pruneau
 * change track container to vector
 *
 * Revision 2.21  2005/01/17 01:31:26  perev
 * New parameter model
 *
 * Revision 2.20  2004/12/23 15:07:18  pruneau
 * added point of closest approach to vertex (pca) histos
 *
 * Revision 2.19  2004/12/11 22:21:01  pruneau
 * new histos
 *
 * Revision 2.18  2004/12/01 14:02:08  pruneau
 * svt
 *
 * Revision 2.17  2004/11/22 19:21:45  pruneau
 * revamped the plotting package
 *
 * Revision 2.16  2004/11/10 21:45:11  pruneau
 * added diagnostic plots
 *
 * Revision 2.15  2004/11/08 15:32:57  pruneau
 * 3 sets of modifications
 * (1) Changed the StiPlacement class to hold keys to both the radial and angle placement. Propagated the use
 * of those keys in StiSvt StiTpc StiSsd and all relevant Sti classes.
 * (2) Changed the StiKalmanTrackFinder::find(StiTrack*) function's algorithm for the navigation of the
 * detector volumes. The new code uses an iterator to visit all relevant volumes. The code is now more robust and compact
 * as well as much easier to read and maintain.
 * (3) Changed the chi2 calculation in StiKalmanTrack::getChi2 and propagated the effects of this change
 * in both StiTrackingPlots and StiStEventFiller classes.
 *
 * Revision 2.14  2004/01/03 19:52:20  andrewar
 * Added check for valid kalman track cast.
 *
 * Revision 2.13  2003/11/24 18:37:20  andrewar
 * Reduced number of bins in 3D hists. There were memory problems.
 *
 * Revision 2.12  2003/11/14 22:31:57  andrewar
 * Added isPrimary() cut so only Primary tracks are histogrammed
 * (not a mix of Primary and global).
 *
 * Revision 2.11  2003/09/02 17:59:42  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 2.10  2003/07/30 19:19:23  pruneau
 * sigh
 *
 * Revision 2.8  2003/05/01 20:46:52  pruneau
 * changed error parametrization
 *
 * Revision 2.7  2003/04/29 18:48:34  pruneau
 * *** empty log message ***
 *
 * Revision 2.6  2003/04/04 14:44:22  pruneau
 * Fix to the hit error calculator and the getCharge methods.
 *
 * Revision 2.5  2003/03/31 17:19:02  pruneau
 * various
 *
 * Revision 2.4  2003/03/14 19:02:21  pruneau
 * various minor updates
 *
 * Revision 2.3  2003/03/13 21:21:29  pruneau
 * getPhase() fixed. MUST inclde -helicity()*pi/2
 *
 * Revision 2.2  2003/03/13 18:59:15  pruneau
 * various updates
 *
 * Revision 2.1  2003/03/12 16:36:03  andrewar
 * Trackng plots package added. Sti tracks are histogrammed at the Sti level
 * alowing comparison to MuDst and StEvent tracks.
 *
 */

#include "Stiostream.h"
#include <string>

#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TFile.h"

#include "Sti/StiDefaultTrackFilter.h"
#include "Sti/StiTrackContainer.h"
#include "Sti/StiTrack.h"
#include "Sti/StiTrackNode.h"
#include "Sti/StiKalmanTrack.h"
#include "Sti/StiKalmanTrackNode.h"
#include "Sti/StiTrackingPlots.h"

StiTrackingPlots::StiTrackingPlots()
  : HistogramGroup()
{ 
  initialize();
}
 
StiTrackingPlots::StiTrackingPlots(const string & name, const string & description)
  : HistogramGroup(name,description)
{ 
  initialize();
}
 

void StiTrackingPlots::initialize()
{
  int i,k;
  char label[64];

  cout <<"StiTrackingPlots::StiTrackingPlots() -I- Started"<<endl;

  /*
  radLengthZ   =book("radLengthZ","Absorption Length (%) vs. Z",400,-200,200,  100,0,100);
  radLengthPhi =book("radLengthPhi","Absorption Length (%) vs. Phi",128,0,360, 256,0,1);
  radLengthEta =book("radLengthEta","Absorption Length (%) vs. Eta",128,-2,2, 256,0,1);
  */

  // index = 0 : all good tracks
  // index = 1 : all good primary tracks
  //         2 : accepted global 
  //         3 : accepted primaries
  for (i=0;i<NPLOTS;++i)
    {
      sprintf(label,"t%d",i);
      _track[i] = book(label,label,600,0.,600.);
      for (k=i+1;k<NPLOTS;++k)
	{
	  sprintf(label,"t%dVs%d",k,i);
	  _track2D[i][k]   = book(label,label,100,0.,600.,100,0.,600.);
	}

      sprintf(label,"eta_%d",i);        _eta[i]        = book(label,label,200, -2.,2.);
      sprintf(label,"phi_%d",i);        _phi[i]        = book(label,label, 100,-3.1415927,3.1415927);
      sprintf(label,"dca_%d",i);        _dca[i]        = book(label,label,  100,0., 10.);
      sprintf(label,"gdca_%d",i);       _gdca[i]       = book(label,label,  100,0., 10.);
      sprintf(label,"pt_%d",i);         _pt[i]         = book(label,label,  250,0., 5.);
      sprintf(label,"nptsVsPt_%d",i);   _nptsVsPt[i]   = book(label,label,  40, 0., 4., 50, 0., 50.);
      sprintf(label,"nptsVsEta_%d",i);  _nptsVsEta[i]  = book(label,label,  40, -2., 2., 50, 0., 50.);
      sprintf(label,"ntpsVsPhi_%d",i);  _nptsVsPhi[i]  = book(label,label,  90, -3.1415927,3.1415927, 50, 0., 50.);
      sprintf(label,"nFitVsPt_%d",i);   _nFitVsPt[i]   = book(label,label,  40, 0., 4., 50, 0., 50.);
      sprintf(label,"nFitVsEta_%d",i);  _nFitVsEta[i]  = book(label,label,  40, -2., 2., 50, 0., 50.);
      sprintf(label,"nFitVsPhi_%d",i);  _nFitVsPhi[i]  = book(label,label,  90, -3.1415927,3.1415927, 50, 0., 50.);
      sprintf(label,"nFitVsN_%d",i);    _nFitVsN[i]    = book(label,label,  50,  0.,50.,50,0.,50);
      //sprintf(label,"nFitVsNSvt_%d",i); _nFitVsNSvt    = book(label,label,   5,0.,5.,5,0.,5);
      sprintf(label,"chi2_%d",i);       _chi2[i]       = book(label,label,120,-2.,10.);
      sprintf(label,"chi2VsNpts_%d",i); _chi2VsNpts[i] = book(label,label,50,0.,50.,120,-2.,10.);
      sprintf(label,"chi2VsDca_%d",i);  _chi2VsDca[i]  = book(label,label,50,0.,10.,120,-2.,10.);
      sprintf(label,"lastHitVsNode_%d",i); _xLastHitVsXLastNode[i] = book(label,label,200,0.,200.,200,0.,200.);
      sprintf(label,"svtNhitVsNode_%d",i);_svtNhitVsNode[i] = book(label,label,6, 0., 6., 6, 0., 6.);
      sprintf(label,"svtNfitVsNode_%d",i);_svtNfitVsNode[i] = book(label,label,6, 0., 6., 6, 0., 6.);
      sprintf(label,"pcaxy_%d",i);_pcaxy[i] = book(label,label, 80, -4., 4., 80, -4., 4.);
      sprintf(label,"pcazt_%d",i);_pcazt[i] = book(label,label, 80, -20., 20., 80, -4., 4.);
      
      for (int layer=0;layer<51;++layer)
	{
	  sprintf(label,"chi2_%d_L%d",i,layer);	      _chi2Inc[i][layer] = book(label,label,400,0.,100.);
	  sprintf(label,"dcaVschi2_%d_L%d",i,layer);  _chi2IncVsDca[i][layer] = book(label,label,400,0.,100.,30,0.,1.5);

	  double bound = 3.;
	  if (layer<6) bound = 1.5;
	  sprintf(label,"dx_%d_L%d",i,layer);         _dx[i][layer] = book(label,label,200,-bound,bound);
	  sprintf(label,"dy_%d_L%d",i,layer);         _dy[i][layer] = book(label,label,200,-bound,bound);
	  sprintf(label,"dz_%d_L%d",i,layer);         _dz[i][layer] = book(label,label,200,-bound,bound);
	  sprintf(label,"yPull_%d_L%d",i,layer);      _yPull[i][layer] = book(label,label,100,-5.,5.);
	  sprintf(label,"zPull_%d_L%d",i,layer);      _zPull[i][layer] = book(label,label,100,-5.,5.);
	  double yMax;
	  double zMax;
	  int    nY;
	  int    nZ;
	  int nSectors;
	  switch (layer)
	    {
	    case 0:
	    case 1: nSectors = 4; nY=8;  yMax=4.;     nZ=6; zMax=6.; break;
	    case 2:
	    case 3: nSectors = 6; nY=8;  yMax=4.;     nZ=6; zMax=12.; break;
	    case 4:
	    case 5: nSectors = 8; nY=8;  yMax=4.;     nZ=6; zMax=21.; break;
	    default: nSectors = 12; nY=8;  yMax=40.;  nZ=10; zMax=100.; break;
	    }
	  for (int  sector=0;sector<nSectors;++sector)
	    {
	      //sprintf(label,"dx_%d_L%d_S%d",i,layer,sector);        _dx[i][layer][sector]=book(label,label,100,-1.,1.);
	      //sprintf(label,"dy_%d_L%d_S%d",i,layer,sector);        _dy[i][layer][sector]=book(label,label,100,-1.,1.);
	      sprintf(label,"dyVsTanCA_%d_L%d_S%d",i,layer,sector);   _dyVsTanCA[i][layer][sector]=book(label,label,10,-1.,1.,100,-1.,1.);
	      sprintf(label,"dyVsY_%d_L%d_S%d",i,layer,sector);       _dyVsY[i][layer][sector]=book(label,label, nY, -yMax, yMax, 100, -2., 2.);
	      sprintf(label,"dyVsZ_%d_L%d_S%d",i,layer,sector);       _dyVsZ[i][layer][sector]=book(label,label, nZ, -zMax, zMax, 100, -2., 2.);
	      //sprintf(label,"dz_%d_L%d_S%d",i,layer,sector);	  _dz[i][layer][sector]=book(label,label,100,-1.,1.);
	      sprintf(label,"dzVsTanL_%d_L%d_S%d",i,layer,sector);    _dzVsTanL[i][layer][sector]=book(label,label,10,-1.,1.,100,-1.,1.);	  
	      sprintf(label,"dzVsY_%d_L%d_S%d",i,layer,sector);       _dzVsY[i][layer][sector]=book(label,label, nY, -yMax, yMax, 100, -2., 2.);
	      sprintf(label,"dzVsZ_%d_L%d_S%d",i,layer,sector);       _dzVsZ[i][layer][sector]=book(label,label, nZ, -zMax, zMax, 100, -2., 2.);
	    }//sector
	}//layer
    }//plot type
      /*
	//3d...
	mGDcavNptsvEtaA = book("mGDcavNptsvEtaA",
	"Global DCA vs. Npts vs. Eta, All charges",
	100,0.,20.,6,0.,645.,30,-2.,2.);
	//mPDcavNptsvEtaA = book("mPDcavNptsvEtaA",
	//			"Primary DCA vs. Npts vs. Eta, All charges",
	//			100,0.,20.,645,0.,645.,30,-2.0,2.);
	mGDcavNptsvPtA = book("mGDcavNptsvPtA",
	"Global DCA vs. Npts vs. Pt, All charges",
	100,0.,20.,6,0.,645.,10,0.,5.);
	//mPDcavNptsvPtA = book("mPDcavNptsvPtA",
	//			"Global DCA vs. Npts vs. Pt, All charges",
	//			100,0.,20.,645,0.,645.,50,0.,5.);
	
	mNptsvPtvEtaA = book("mNptsvPtvEtaA",
	"Npts vs. Pt. vs. Eta, All charges",
	6,0.,645.,30,-2.,2., 50,0.,5.);
	mGDcavEtavPtA = book("mGDcavEtavPtA",
	"Global DCA vs. Eta vs. Pt",
	100,0.,20.,10,-2.,2.,10,0.,5.);
	//mPDcavEtavPtA= book("mPDcavEtavPtA",
	//		      "Primary DCA vs. Eta vs. Pt"
	//		      100,0,20.,20,-2.,2.,50,0.,5.);
	
	mGDcavNptsvEtaP = book("mGDcavNptsvEtaP",
	"Global DCA vs. Npts vs. Eta, P All charges",
	20,0.,20.,6,0.,645.,15,-2.,2.);
	//mPDcavNptsvEtaP = book("mPDcavNptsvEtaP",
	//			"Primary DCA vs. Npts vs. Eta, All charges",
	//			100,0.,20.,645,0.,645.,30,-2.0,2.);
	mGDcavNptsvPtP = book("mGDcavNptsvPtP",
	"Global DCA vs. Npts vs. Pt, All charges",
	20,0.,20.,6,0.,645.,10,0.,5.);
	//mPDcavNptsvPtP = book("mPDcavNptsvPtP",
	//			"Global DCA vs. Npts vs. Pt, All charges",
	//			100,0.,20.,645,0.,645.,50,0.,5.);
	
	mNptsvPtvEtaP = book("mNptsvPtvEtaP",
	"Npts vs. Pt. vs. Eta, All charges",
	645,0.,645.,4,-2.,2., 10,0.,5.);
	mGDcavEtavPtP = book("mGDcavEtavPtP",
	"Global DCA vs. Eta vs. Pt",
	10,0,20.,4,-2.,2.,10,0.,5.);
	//mPDcavEtavPtP= book("mPDcavEtavPtP",
	//		      "Primary DCA vs. Eta vs. Pt"
	//		      100,0,20.,20,-2.,2.,50,0.,5.);
	
	mGDcavNptsvEtaM = book("mGDcavNptsvEtaM",
	"Global DCA vs. Npts vs. Eta, P All charges",
	10,0.,20.,6,0.,645.,4,-2.,2.);
	//mPDcavNptsvEtaM = book("mPDcavNptsvEtaM",
	//			"Primary DCA vs. Npts vs. Eta, All charges",
	//			100,0.,20.,645,0.,645.,30,-2.0,2.);
	mGDcavNptsvPtM = book("mGDcavNptsvPtM",
	"Global DCA vs. Npts vs. Pt, All charges",
	10,0.,20.,6,0.,645.,10,0.,5.);
	//mPDcavNptsvPtM = book("mPDcavNptsvPtM",
	//			"Global DCA vs. Npts vs. Pt, All charges",
	//			100,0.,20.,645,0.,645.,50,0.,5.);
	
	mNptsvPtvEtaM = book("mNptsvPtvEtaM",
	"Npts vs. Pt. vs. Eta, All charges",
	6,0.,645.,4,-2.,2., 10,0.,5.);
	mGDcavEtavPtM = book("mGDcavEtavPtM",
	"Global DCA vs. Eta vs. Pt",
	100,0,20.,20,-2.,2.,50,0.,5.);
	//mPDcavEtavPtM= book("mPDcavEtavPtM",
	//			      "Primary DCA vs. Eta vs. Pt"
	//			      100,0,20.,20,-2.,2.,50,0.,5.);
      */
  
  //cout <<"StiTrackingPlots::StiTrackingPlots() -I- Done"<<endl;
}
  
StiTrackingPlots::~StiTrackingPlots()
{
  cout << "StiTrackingPlots::~StiTrackingPlots() -I- Done"<<endl;
}




void StiTrackingPlots::fill(StiTrackContainer *mTrackStore, StiHit * vertex)
{
  //cout <<"StiTrackingPlots::fill() -I- Filling histos" <<endl;
  const StiTrack* track;
  const StiKalmanTrack * kTrack;

  int trackCount[6];
  for (int k=0;k<4;++k) trackCount[k] = 0;
  //loop over tracks
  for (vector<StiTrack*>::const_iterator trackIt = mTrackStore->begin(); 
       trackIt!=mTrackStore->end();
       ++trackIt)
    {
      //cout << " t " << endl;
      track = *trackIt;
      kTrack = dynamic_cast<const StiKalmanTrack *>(track);
      if( !track || !kTrack ) continue;
      
      bool svtVertex = false;
      if (vertex)  svtVertex = fabs(vertex->z())<10.;
      bool good    = (1==kTrack->getFlag());
      bool primary = kTrack->isPrimary();

      int nPts = kTrack->getPointCount();
      if(nPts<5) continue;

      int nFitPts    = kTrack->getFitPointCount();
#if 0
      int nFitPtsSvt = kTrack->getFitPointCount(2);
      int tpcPoints  = (kTrack->getNodes(1)).size();
#endif
      int svtPoints  = (kTrack->getNodes(2)).size();
      //int mPts=100*svtPoints+tpcPoints;
      

      double phi    = track->getPhi();
      double eta    = track->getPseudoRapidity();
      double pt     = track->getPt();
      double gdca   = track->getDca();
      double dca    = track->getDca();
#if 0
      double charge = track->getCharge();
#endif
      double chi2   = kTrack->getChi2();

      bool globalAccepted = false;
      bool primaryAccepted= false;
      if (pt>0.15 && fabs(eta)<0.5 && nFitPts>15 && gdca<3.) 
	{
	  globalAccepted = true;
	  if (primary) primaryAccepted = true;
	}
      //cout << " loop" << endl;
      for (int i=0;i<NPLOTS-1; ++i)
	{
	  //cout << " t:" << i << endl;
	  if (i==0 && !good)     continue;
	  if (i==1 && !primary)  continue;
	  if (i==2 && !globalAccepted)  continue;
	  if (i==3 && !primaryAccepted) continue;

	  if (i==4 && ! (primary&&svtVertex) ) continue;
	  if (i==5 && ! (svtPoints==1) ) continue;
	  if (i==6 && ! (svtPoints==2) ) continue;
	  if (i==7 && ! (svtPoints>=3) ) continue;
	  
	  trackCount[i]++;
	  _eta[i]->Fill(eta);
	  _phi[i]->Fill(phi);
	  _pt[i]->Fill(pt);
	  _gdca[i]->Fill(gdca);
	  _dca[i]->Fill(dca); 
	  _nptsVsPt[i]->Fill(pt,nPts);
	  _nptsVsEta[i]->Fill(eta,nPts);
	  _nptsVsPhi[i]->Fill(phi,nPts);
	  _nFitVsPt[i]->Fill(pt,nFitPts);
	  _nFitVsEta[i]->Fill(eta,nFitPts);
	  _nFitVsPhi[i]->Fill(phi,nFitPts);
	  _nFitVsN[i]->Fill(nPts,nFitPts);
	  _chi2VsNpts[i]->Fill(nPts,chi2);
	  _chi2VsDca[i]->Fill(gdca,chi2);
	  _chi2[i]->Fill(chi2);

	  StiKalmanTrackNode * pcaNode = kTrack->getInnerMostNode();
	  double pcax = pcaNode->x_g();
	  double pcay = pcaNode->y_g();
	  double pcaz = pcaNode->z_g();

	  double x1 = kTrack->getInnerMostNode()->getX();
	  double x2 = kTrack->getInnerMostHitNode()->getX();
	  _xLastHitVsXLastNode[i]->Fill(x1,x2);

	  _pcaxy[i]->Fill(pcax,pcay);
	  _pcazt[i]->Fill(pcaz,sqrt(pcax*pcax+pcay*pcay));

	  //cout << " F" << endl;
	  /* 3D temporarily disabled...
	     mGDcavNptsvEtaA->Fill(dca,mPts,eta);
	     mGDcavNptsvPtA->Fill(dca,mPts, pt);
	     mNptsvPtvEtaA->Fill(mPts,pt,eta);0
	     mGDcavEtavPtA->Fill(dca,eta,pt);
	     
	     if (svtPoints>0 && svtPoints<4)
	     {
	     int iii = svtPoints-1;
	     _chi2VsDcaSvt[iii]->Fill(kTrack->_vChi2, kTrack->_vDca);
	     _etaSvt[iii]->Fill(eta);
	     _phiSvt[iii]->Fill(phi);
	     _ptSvt[iii]->Fill(pt);
	     _globalDcaSvt[iii]->Fill(dca);
	     _nptsVsPtSvt[iii]->Fill(pt,nPts);
	     _nptsVsEtaSvt[iii]->Fill(eta,nPts);
	     _nptsVsPhiSvt[iii]->Fill(phi,nPts);
	     _chi2Svt[iii]->Fill(chi2);
	     _chi2VsNptsSvt[iii]->Fill(nPts,chi2);
	     }
	  */
	    
	  // loop through nodes
	  int svtNnodes=0;
	  int svtNhits=0;
	  int svtNfits=0;

	  StiKalmanTrackNode* leaf = kTrack->getLastNode();
	  StiKTNForwardIterator it(leaf);
	  StiKTNForwardIterator end = it.end();
	  while (it!=end) 
	    {
	      //cout << "+";
	      StiKalmanTrackNode& node = *it;
	      const StiHit * theHit = node.getHit();
	      const StiDetector * theDetector = node.getDetector();
	      if (theDetector && theDetector->isActive())
		{
		  //cout<<"Active"<< *theDetector;
		  
		  int id   = theDetector->getGroupId();
		  int key1 = theDetector->getKey(1);
		  int key2 = theDetector->getKey(2);
		  if (key1<0 || key2<0) {++it;continue;}
		  int key = -1;
		  if (id==1 && key1>=0) 
		    key=6+key1;
		  else if (id==2 && key1>=0) 
		    {
		      key=key1;
		      svtNnodes++;
		    }
		  //cout << " id:"<<id<<" key1:"<<key1<<" key2:"<<key2<<" key:"<<key<<endl;
		  if (theHit && key>=0 && key<51)
		    {
		      //cout << "=";
		      double dx = theHit->x() - node.getX();
		      double dy = theHit->y() - node.getY();
		      double dz = theHit->z() - node.getZ();
		      double theDca = sqrt(dy*dy+dz*dz);
		      double theChi2 = node.evaluateChi2(theHit);
		      double tanCA = node.getSin()/node.getCos();
		      double tanL  = node.getTanL();
		      if (key>50 || key<0) continue;
		      //cout << "OK";
		      if (id==2) 
			{
			  svtNhits++;
			  if (theChi2<10) svtNfits++;
			}
		      if (_chi2Inc[i][key] && _dyVsTanCA[i][key][key2]) 
			{
			  _chi2Inc[i][key]->Fill(theChi2);
			  _chi2IncVsDca[i][key]->Fill(theChi2,theDca);
			  if (node.getCyy()>0) _yPull[i][key]->Fill( dy/sqrt(node.getEyy()) );
			  if (node.getCzz()>0) _zPull[i][key]->Fill( dz/sqrt(node.getEzz()) );
			  if (theChi2<30) //svt only
			    {
			      _dx[i][key]->Fill(dx);
			      _dy[i][key]->Fill(dy);
			      _dyVsTanCA[i][key][key2]->Fill(tanCA,dy);
			      _dyVsY[i][key][key2]->Fill(node.getY(),dy);
			      _dyVsZ[i][key][key2]->Fill(node.getZ(),dy);
			      _dz[i][key]->Fill(dz);
			      _dzVsTanL[i][key][key2]->Fill(tanL,dz);
			      _dzVsY[i][key][key2]->Fill(node.getY(),dz);
			      _dzVsZ[i][key][key2]->Fill(node.getZ(),dz);
			    }
			}
		      //cout << "D"<<endl;
		    } //theHit
		} //theDetector
	      ++it;
	    } 	  // end of loop through nodes
	  _svtNhitVsNode[i]->Fill(svtNnodes,svtNhits);
	  _svtNfitVsNode[i]->Fill(svtNnodes,svtNfits);
	} // end of loop plot types
    }//end loop over tracks
  trackCount[0] = trackCount[0]/10;
  trackCount[1] = trackCount[1]/10;
  // some global histograms
  for (int j1=0;j1<NPLOTS-1;++j1)
    {
      _track[j1]->Fill(trackCount[j1]);
      for (int j2=j1+1;j2<NPLOTS-1;++j2)
	{
	  _track2D[j1][j2]->Fill(trackCount[j1],trackCount[j2]);
	}
    }
  //xxxxx
  if (trackCount[3]>0.9*trackCount[2]) return;


  //cout << " anomalous event" << endl;
  for (vector<StiTrack*>::const_iterator trackIt = mTrackStore->begin(); 
       trackIt!=mTrackStore->end();
       ++trackIt)
    {
      const StiTrack* track = *trackIt;
      kTrack = dynamic_cast<const StiKalmanTrack *>(track);
      if( !track || !kTrack ) continue;
#if 0
      bool good    = (1==kTrack->getFlag());
#endif
      bool primary = kTrack->isPrimary();

      int nPts = kTrack->getPointCount();
      if(nPts<5) continue;

      int nFitPts    = kTrack->getFitPointCount();
#if 0
      int nFitPtsSvt = kTrack->getFitPointCount(2);
      int tpcPoints  = (kTrack->getNodes(1)).size();
      int svtPoints  = (kTrack->getNodes(2)).size();
#endif
      //int mPts=100*svtPoints+tpcPoints;
      
      double phi    = track->getPhi();
      double eta    = track->getPseudoRapidity();
      double pt     = track->getPt();
      double gdca   = track->getDca();
      double dca    = track->getDca();
#if 0
      double charge = track->getCharge();
#endif
      double chi2   = kTrack->getChi2();

      bool globalAccepted = false;
      bool primaryAccepted= false;
      if (pt>0.15 && fabs(eta)<0.5 && nFitPts>15 && gdca<3.) 
	{
	  globalAccepted = true;
	  if (primary) primaryAccepted = true;
	}
      int i = NPLOTS-1;
	  _eta[i]->Fill(eta);
	  _phi[i]->Fill(phi);
	  _pt[i]->Fill(pt);
	  _gdca[i]->Fill(gdca);
	  _dca[i]->Fill(dca); 
	  _nptsVsPt[i]->Fill(pt,nPts);
	  _nptsVsEta[i]->Fill(eta,nPts);
	  _nptsVsPhi[i]->Fill(phi,nPts);
	  _nFitVsPt[i]->Fill(pt,nFitPts);
	  _nFitVsEta[i]->Fill(eta,nFitPts);
	  _nFitVsPhi[i]->Fill(phi,nFitPts);
	  _nFitVsN[i]->Fill(nPts,nFitPts);
	  _chi2VsNpts[i]->Fill(nPts,chi2);
	  _chi2VsDca[i]->Fill(gdca,chi2);
	  _chi2[i]->Fill(chi2);
	  double x1 = kTrack->getInnerMostNode()->getX();
	  double x2 = kTrack->getInnerMostHitNode()->getX();
	  _xLastHitVsXLastNode[i]->Fill(x1,x2);

	  // loop through nodes
	  StiKalmanTrackNode* leaf = kTrack->getLastNode();
	  StiKTNForwardIterator it(leaf);
	  StiKTNForwardIterator end = it.end();
	  while (it!=end) 
	    {
	      StiKalmanTrackNode& node = *it;
	      const StiHit * theHit = node.getHit();
	      const StiDetector * theDetector = node.getDetector();
	      if (theHit && theDetector)
		{
		  double dx = theHit->x() - node.getX();
		  double dy = theHit->y() - node.getY();
		  double dz = theHit->z() - node.getZ();
		  double theDca = sqrt(dy*dy+dz*dz);
		  double theChi2 = node.evaluateChi2(theHit);
		  double tanCA = node.getSin()/node.getCos();
		  double tanL  = node.getTanL();
		  int id   = theDetector->getGroupId();
		  int key1 = theDetector->getKey(1);
		  int key2 = theDetector->getKey(2);
		  int key;
		  if (id==1) 
		    key=5+key1;
		  else if (id==2) 
		    key=key1;
		  else 
		    continue;
		  if (key>50 || key<0 || id<1 || id>2) {++it;continue;}
		  _chi2Inc[i][key]->Fill(theChi2);
		  _chi2IncVsDca[i][key]->Fill(theChi2,theDca);
		  if (node.getCyy()>0) _yPull[i][key]->Fill( dy/sqrt(node.getEyy()) );
		  if (node.getCzz()>0) _zPull[i][key]->Fill( dz/sqrt(node.getEzz()) );
		  if (theChi2<30) //svt only
		    {
		      _dx[i][key]->Fill(dx);
		      _dy[i][key]->Fill(dy);
		      _dyVsTanCA[i][key][key2]->Fill(tanCA,dy);
		      _dz[i][key]->Fill(dz);
		      _dzVsTanL[i][key][key2]->Fill(tanL,dz);
		    }
		}
	      ++it;
	    }
	  // end of loop hrough nodes
    }//end loop over tracks
      
  //cout <<"StiTrackingPlots::fill() -I- Done Filling histos" <<endl;
}

