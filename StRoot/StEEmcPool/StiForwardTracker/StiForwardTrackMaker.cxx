// -- Author : Victor Perevoztchikov
// 
// $Id: StiForwardTrackMaker.cxx,v 1.10 2007/07/12 19:27:21 fisyak Exp $
#include "TFile.h"
#include "StMessMgr.h"

#include "StiForwardTrackMaker.h"
#include "StEvent.h"
#include "StPrimaryVertex.h"

#include "Sti/StiToolkit.h"
#include "Sti/StiKalmanTrack.h"
#include "Sti/StiKalmanTrackNode.h"
#include "StiMaker/StiMaker.h"
#include "Sti/StiTrackContainer.h"

#include "Sti/StiHitContainer.h"
#include "Sti/StiLocalTrackSeedFinder.h"
#include "Sti/StiHit.h"
#include "Sti/StiSortedHitIterator.h"
#include "Sti/StiDetectorContainer.h"
#include "Sti/StiKalmanTrackFinder.h"

#include "StDetectorId.h"
#include "StHit.h"
#include "TFile.h"
#define xG(t)   (t->x_g())
#define yG(t)   (t->y_g())
#define zG(t)   (t->z_g())
#define rxyG(t) sqrt(xG(t)*xG(t) + yG(t)*yG(t))
#define rG(t)	sqrt(xG(t)*xG(t) + yG(t)*yG(t) + zG(t)*zG(t))
#define xL(t)   (t->getX())
#define yL(t)   (t->getY())
#define zL(t)   (t->getZ())
#define ezL(t)  sqrt(t->getCzz())
#define eyL(t)  sqrt(t->getCyy())


ClassImp(StiForwardTrackMaker)

//_____________________________________________________________________________
StiForwardTrackMaker::StiForwardTrackMaker(const char *name):StMaker(name){
	mToolkit=0;
	mTotEve=0;
	mTotMatchedSeeds=0;
	mTotMatchedTracks=0;
	HList=0;
	memset(hA,0,sizeof(hA));
	mMaxTrkDcaRxy = 3.0;  // cm
	mMaxZdca = 4; // cm
	
	mMinEta = 0.9;
	mAllHits = 0;
	mForwardHits = 0;
	mTrackSeeds = 0;
	mSeedGenerator = 0;
	mTrackFinder = 0;
}


//_____________________________________________________________________________
StiForwardTrackMaker::~StiForwardTrackMaker(){
	//
}


//_____________________________________________________________________________
/// Init - is a first method the top level StChain calls to initialize all its makers 
Int_t StiForwardTrackMaker::Init(){
	
	//get a pointer to StiMaker:
	StiMaker* sti = (StiMaker*)StMaker::GetChain()->GetMaker("Sti");
	if(sti==0) {
		gMessMgr->Warning() <<GetName()<<"no STi Maker, it is fatal"<<endm;
		return kStErr;
	}
	
	//get pointer to Sti toolkit
	mToolkit = sti->getToolkit();
	assert(mToolkit); //internal error of Sti
	
	//get pointer to full hit container
	mAllHits = mToolkit->getHitContainer();
	assert(mAllHits);
	
	//instantiate the forward hit container
	mForwardHits = new StiHitContainer("mForwardHits","forward hits",mToolkit->getHitFactory());
	
	//instantiate the track seeds container
	mTrackSeeds = new StiTrackContainer("mTrackSeeds","forward track seeds");
	
	//instantiate the seed generator
	mSeedGenerator = new StiLocalTrackSeedFinder("mSeedGenerator","builds track seeds",mToolkit->getTrackFactory(), mForwardHits,mToolkit->getDetectorContainer());
	
	//initialize the Kalman track finder
	mTrackFinder = new StiKalmanTrackFinder(mToolkit);
	//mTrackFinder->initialize();
	
	HList=new TObjArray(0);
	initHisto();
	HList->ls();
	
	gMessMgr->Info() << GetName()
		<<"::Cuts"
		<<"\n MaxTrkDcaRxy/cm = "<< mMaxTrkDcaRxy
		<<"\n mMaxZdca = "<< mMaxZdca
		<<"\n mMinEta = "<< mMinEta
		<<endm;
	
	return StMaker::Init();
}


//_____________________________________________________________________________
/// Make - this method is called in loop for each event
Int_t StiForwardTrackMaker::Make(){
	return  MakeAfterSti();
}

//_____________________________________________________________________________
/// Inside Sti
Int_t StiForwardTrackMaker::MakeInSti()
{
	if(mToolkit==0) {
		gMessMgr->Warning()<<GetName()<<"no Sti tool kit "<<GetName()<<" is  OFF"<<endm;
		return kStErr;
	}
	else if(mAllHits==0) {
		gMessMgr->Warning() <<"no Sti hits present: "<<GetName()<<" is  OFF"<<endm;
		return kStErr;
	}
	
	
	int nV=vertL.size();
	StEvent *event = (StEvent *) GetInputDS("StEvent");
	
	gMessMgr->Info() << "\n JJJ1  "<<GetName()<<":: MakeInSti(), eveID="<<event->id()<<" nVert="<<nV<<endm;
		
	//print the position of each of the vertices, fill histo with zVertex
	int iv;
	for(iv=0;iv<nV;iv++) {
		VertexV V=vertL[iv];
		printf("iv=%d   Vz=%.2f +/-%.2f \n",iv,V.z,V.ez  );
		hA[1]->Fill(V.z);
	}
	
	getForwardHits(mAllHits, mForwardHits, mToolkit->getDetectorContainer(), mMinEta);
	cout<<"We started with " << mAllHits->size() << " hits and ended with " << mForwardHits->size() << " in the forward region" << endl;
	
	buildTrackSeeds(mForwardHits, mTrackSeeds, mSeedGenerator);
	
	//use Kalman to extend tracks, build covariance matrix, and add more hits
	extendTracks();	

	
	//make some histograms
	StiSortedHitIterator it = StiSortedHitIterator(mForwardHits, mToolkit->getDetectorContainer()->begin(), mToolkit->getDetectorContainer()->end());
	StiHit* hit = 0;
	float phi;
	while(it!=StiSortedHitIterator())
	{
		hit = &(*it);
		
		//eta
		hA[2]->Fill( -log( rxyG(hit)/(zG(hit)+rG(hit)) ) );	//eta of hit

		//phi (is this calculation correct?)
		phi = atan(yG(hit)/xG(hit));
		if(xG(hit)<0 && yG(hit)<0)	phi = phi - 3.14;
		else if(xG(hit)<0)			phi = phi + 3.14;
		hA[3]->Fill(phi);				
		
		it++;
	}

		
	//get the Sti track container...
	StiTrackContainer* tracks = mToolkit->getTrackContainer();
	if(tracks==0) {
		gMessMgr->Warning() <<"no STi tracks , skip eve"<<endm;
		return kStErr ;
	}
	
	cout<<"There are "<<tracks->getTrackCount(0)<<" original tracks and "<<mTrackSeeds->getTrackCount(0)<<" forward track seeds"<<endl;
	
	matchVertex(tracks,vertL,mMaxZdca,nV,hA[9],mTotMatchedTracks);
	
	matchVertex(mTrackSeeds,vertL,mMaxZdca,nV,hA[8],mTotMatchedSeeds);
			
	return kStOK;
}

//_____________________________________________________________________________
/// Inside Sti
Int_t StiForwardTrackMaker::MakeAfterSti(){
	
	StEvent *event = (StEvent *) GetInputDS("StEvent");
	assert(event);
	eveID=event->id();
	mTotEve++;
	int nV=event->numberOfPrimaryVertices();
	
	gMessMgr->Info() << "\n JJJ2  "<<GetName()<<"MakeAfterSti(), START nEve="<<mTotEve<<"  eveID="<<eveID<< " nPrimVertex="<<nV<< endm;
	
	hA[0]->Fill(nV);
	
	if(nV<=0) {
		gMessMgr->Info() << GetName()<<" event dropped, no vertex found"<<endm;
		return kStOK;
	}
	
	
	return kStOK;
}

//_____________________________________________________________________________
// called before each event
void 
StiForwardTrackMaker::Clear(const char* opt){
	gMessMgr->Info() <<GetName()<<"::Clear()"<<  endm;
	eveID=0;
	vertL.clear();
	mForwardHits->clear();
	mTrackSeeds->clear();
	mSeedGenerator->reset();
}   

//=======================================================
//=======================================================
void
StiForwardTrackMaker::initHisto() {
	assert(HList);
	hA[0]=new TH1F("nV","No. of vertices per eve",20,-0.5,19.5);
	hA[1]=new TH1F("zV","reconstructed vertices (any); Z (cm)",200,-200,200);
	
	hA[2] = new TH1F("etaNotUsed","eta of unused forward hits",20,0.8,2.1);
	hA[3] = new TH1F("phiNotUsed","phi of unused forward hits",20,-3.14,3.14);
	hA[4] = new TH1F("etaUsed","eta of hits used by previous tracker",20,0.8,2.1);
	hA[5] = new TH1F("phiUsed","phi of hits used by previous tracker",20,-3.14,3.14);
	hA[6] = new TH1F("seedHits","number of seed hits used for each track",20,1,20);
	hA[7] = new TH1F("trackHits1","total # of hits/track after inward Kalman",20,1,20);
	hA[10] = new TH1F("trackHits2","total # of hits/track after outward Kalman",20,1,20);
	hA[8] = new TH1F("deltaZkalman","zDCA-zVertex for tracks after Kalman",200,-200,200);
	hA[9] = new TH1F("deltaZtracks","zDCA-zVertex for old tracks",200,-200,200);
	
	int i;  
	for(i=0;i<mxHA; i++) if(hA[i]) HList->Add(hA[i]);
}

//-------------------------------------------------
//-------------------------------------------------
void
StiForwardTrackMaker::saveHisto(TString fname){
	TString outName=fname+".hist.root";
	TFile f( outName,"recreate");
	assert(f.IsOpen());
	printf("%d histos are written  to '%s' ...\n",HList->GetEntries(),outName.Data());
	HList->ls();
	HList->Write();
	f.Close();
}

//-------------------------------------------------
//-------------------------------------------------
Int_t
StiForwardTrackMaker::Finish(){
	saveHisto(GetName());
	cout<<"Total matched track seeds: "<<mTotMatchedSeeds<<" and total matched tracks: "<<mTotMatchedTracks<<endl;
	return kStOK;
}

//-------------------------------------------------
//-------------------------------------------------

void StiForwardTrackMaker::getForwardHits(StiHitContainer* allHits, StiHitContainer* forwardHits, StiDetectorContainer* detector, double minEta)
{
	StiSortedHitIterator it = StiSortedHitIterator(allHits, detector->begin(), detector->end());
	StiHit* hit = 0;
	
	while(it!=StiSortedHitIterator())
	{
		hit = &(*it);		
		StDetectorId id = (static_cast <const StHit*>(hit->stHit()))->detector();	//restricting to TPC hits; doesn't seem to do anything
		if( rxyG(hit)/(zG(hit)+rG(hit)) < exp(-minEta)  &&  id==kTpcId)
		{
			if(hit->timesUsed()==0)forwardHits->add(hit);
			else	//this is a previously used hit, just put it in a histogram
			{
				hA[4]->Fill( -log( rxyG(hit)/(zG(hit)+rG(hit)) ) );	//eta of hit
				
				//phi (is this calculation correct?)
				float phi = atan(yG(hit)/xG(hit));
				if(xG(hit)<0 && yG(hit)<0)	phi = phi - 3.14;
				else if(xG(hit)<0)			phi = phi + 3.14;
				hA[5]->Fill(phi);				
			}
		}
		it++;
	}
	
	forwardHits->sortHits();
}


void StiForwardTrackMaker::buildTrackSeeds(const StiHitContainer* forwardHits, StiTrackContainer* trackSeeds, StiLocalTrackSeedFinder* seedGenerator)
{
	StiTrack* track = seedGenerator->findTrack();
		
	while(track != 0)
	{
		hA[6]->Fill(track->getPointCount());
		trackSeeds->add(track);
		track = seedGenerator->findTrack();
	}
}

void StiForwardTrackMaker::extendTracks()
{
	mTrackFinder->initialize();
	//mTrackFinder->clear();
	
	float zDCA, ezDCA, RxyDCA;
	//int goodTracks = 0;
		
	for(StiTrackContainer::const_iterator it=mTrackSeeds->begin(); it!=mTrackSeeds->end(); it++)
	{
		while(mTrackFinder->find(*it,0,2)){}; //extend inward
		hA[7]->Fill((*it)->getPointCount());
		while(mTrackFinder->find(*it,1,0)){}; //extend outward
		hA[10]->Fill((*it)->getPointCount());
		
		if(examineTrackDca(static_cast<StiKalmanTrack*>(*it), zDCA, ezDCA, RxyDCA)){
	//		goodTracks++;
			cout << "This track passed the cut "<<**it<<endl;
		}		
	}
	
	//cout<<goodTracks<<" of the forward track seeds passed the examineTrackDca() cut"<<endl;
}
		
	


void StiForwardTrackMaker::matchVertex(StiTrackContainer* tracks, vector <VertexV> &vertL, double &mMaxZdca, int &nV, TH1* h, int &totalMatched)
{
	int nAll=0, nAny=0, nTry=0, nAcc=0;
	for (StiTrackContainer::const_iterator it=(*tracks).begin();  it!=(*tracks).end(); ++it) {
		const StiKalmanTrack* track = static_cast<StiKalmanTrack*>(*it);
		nAll++;
		if(track->getFlag()!=true) nAny++; // don't drop bad tracks for now, all track seeds currently flagged as bad
		//cout<<"\n#a kalTrack: nTr="<<nAny<<" flag="<<track->getFlag()<<"  nFitP="<<track->getFitPointCount()<<" is Prim="<<track->isPrimary()<<" pT="<<track->getPt()<<endl;
		// cout<<"#b kalTrack: pT="<<track->getPt()<<endl;
		//cout<<"#b kalTrack:"<<*track<<endl;
		
	float zDca, ezDca, rxyDca;
		if(!examineTrackDca(track, zDca,  ezDca, rxyDca)) continue;
		nTry++;
		//cout<<"#c nTry="<<nTry<<" zDca="<<zDca<<endl;
		
		//.... match track to a vertex
		for(int iv=0;iv<nV;iv++) {
			VertexV V=vertL[iv];
			h->Fill(zDca-V.z);
			if( fabs(zDca-V.z) > mMaxZdca )continue;
			nAcc++;
		//	cout<<" tr Matched , dZ="<<zDca-V.z<<" nAcc="<<nAcc<<endl;
			break; 	 
		}
	}
	
	gMessMgr->Info() << "\n"<<GetName()<<" We have "<<nAll<<" tracks, found "<<nAny<<" flagged tracks, try ZDca for "<<nTry<<", match to vertex:"<<nAcc<<"\n"<<endm;
	totalMatched = nAcc + totalMatched;
}	

bool
StiForwardTrackMaker::examineTrackDca(const StiKalmanTrack*track,
									 float &zDca, float &ezDca, float &rxyDca){
	
	// cout <<"#e  track->getPseudoRapidity()="<<track->getPseudoRapidity()<<" track->getFitPointCount()="<<track->getFitPointCount()<<endl;
	
	// .......... test DCA to beam .............
	StiKalmanTrack track1=*track; // clone track
	StiKalmanTrackNode* bmNode=track1.extrapolateToBeam();
	if(bmNode==0)  {
	//	cout<<"#a @beam  DCA NULL"<<endl; 
		return false ;
	}
	
	float rxy=rxyG(bmNode);
	//cout<<"#e @beam global DCA x:"<< bmNode->x_g()<<" y:"<< bmNode->y_g()<<" z:"<< bmNode->z_g()<<" Rxy="<< rxy <<endl;
	if(rxy>mMaxTrkDcaRxy) return false;
	
	//cout<<"#e inBeam |P|="<<bmNode->getP()<<" pT="<<bmNode->getPt()<<" local x="<<xL(bmNode)<<" y="<<yL(bmNode)<<" +/- "<<eyL(bmNode)<<" z="<<zL(bmNode)<<" +/- "<<ezL(bmNode)<<endl;
	
	zDca=zG(bmNode);
	ezDca=ezL(bmNode);
	rxyDca=rxy;
	
	return true;
}

//-------------------------------------------------
//-------------------------------------------------
void
StiForwardTrackMaker::addVertex(float z, float ez){
	VertexV V;
	V.z=z;
	V.ez=ez;
	vertL.push_back(V);
}

// $Log: StiForwardTrackMaker.cxx,v $
// Revision 1.10  2007/07/12 19:27:21  fisyak
// Add includes for TMath for ROOT 5.16
//
// Revision 1.9  2006/10/17 18:47:49  fisyak
// Remove reference to dead classes
//
// Revision 1.8  2006/08/06 23:29:44  perev
// ROOT5 corrs
//
// Revision 1.7  2005/09/26 14:03:54  kocolosk
// added Kalman propagation for forward track seeds
//
// Revision 1.6  2005/09/21 15:37:06  kocolosk
// modified matchVertex() so that |deltaZ| could be plotted separately for old, new tracks
//
// Revision 1.5  2005/09/14 14:15:06  kocolosk
// restrict to TPC hits, added histos, moved code to matchVertex() function
//
// Revision 1.4  2005/09/13 14:24:15  kocolosk
// implemented getForwardHits() and buildTrackSeeds() functions
//
// Revision 1.3  2005/09/12 21:08:21  balewski
// split Make to InSti and AfterSti
//
// Revision 1.2  2005/09/09 15:55:00  balewski
// prototype with hardcoded hacks
//
// Revision 1.1  2005/09/08 21:42:03  balewski
// star
//








