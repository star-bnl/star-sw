/****************************************************************
 * $Id: StRichPadMonitor.cxx,v 2.5 2000/11/01 16:59:18 lasiuk Exp $
 * Description:
 *  A Pad Monitor for the STAR-RICH.
 *  Runs only in ROOT
 *
 *****************************************************************
 *
 * $Log: StRichPadMonitor.cxx,v $
 * Revision 2.5  2000/11/01 16:59:18  lasiuk
 * MAJOR.  add ringInfo().  Clear() of TObjArray containers is simplified
 * (Valery suggestion)  Utilities added to draw lines and markers.
 * hilite() members based on StEvent hit flags now implemented.  This
 * makes use of the DrawableHits contained in the DrawableRings.
 *
 * Revision 2.4  2000/09/29 17:36:58  gans
 * Modified addHit(), StThreeVector<double> -> StThreeVectorF,other minor stuff
 *
 * Revision 2.1  2000/08/09 23:26:58  gans
 * Use TBoxes instead of TLines for Pad Monitor
 *
 * Revision 2.0  2000/08/09 16:28:04  gans
 * Created New Maker for all drawable objects.
 *
 * Revision 1.7  2000/06/16 02:00:51  lasiuk
 * add MC drawing diagnositics
 * add vector for storage
 * cosmetics for display
 *
 * Revision 1.5  2000/05/17 22:20:40  lasiuk
 * charge from the pixel
 *
 * Revision 1.4  2000/04/05 16:02:09  lasiuk
 * GEANT info now drawable
 *
 * Revision 1.3  2000/03/13 21:50:35  lasiuk
 * coordinates
 *
 * Revision 1.2  2000/02/29 18:19:38  lasiuk
 * Split needed classes into individual files
 *
 * Revision 1.1  2000/02/12 21:57:56  lasiuk
 * Initial revision
 ****************************************************************/
#ifdef __ROOT__

#include "StGlobals.hh"

#include "TLine.h"
#include "TFile.h"
#include "TNtuple.h"
#include "TText.h"
#include "TPaveText.h"
#include "StTrackDetectorInfo.h"

#include "StRrsMaker/StRichGeometryDb.h"
#include "StRrsMaker/StRichCoordinateTransform.h"
#include "StRrsMaker/StRichSinglePixel.h"
#include "StRrsMaker/StRichSingleMCPixel.h"

#include "StRichPadMonitor.h"
#include "StRichDrawableTPad.h"
#include "StRichDrawableMCTPad.h"
#include "StRichDrawableTG2T.h"
#include "StRichDrawableTHit.h"
#include "StRichDrawableTRings.h"
#include "StRichDrawableTTrack.h"
#include "StRichDrawableTMip.h"
// #include "StRichTControl.h"

#include "StRichPIDMaker/StRichTrack.h"
#include "StRichPIDMaker/StRichRingHit.h"

#include "StRichPadMonitorText.h"
#include "StTrackGeometry.h"
#include "StTrackFitTraits.h"

#include "StRchMaker/StRichSimpleHit.h"

#include "StEvent/StRichPidTraits.h"
#include "StEvent/StRichPid.h" 

StRichPadMonitor* StRichPadMonitor::mInstance = 0;

StRichPadMonitor* StRichPadMonitor::getInstance(StRichGeometryDb* geo)
{
    if(!mInstance)
	mInstance = new StRichPadMonitor(geo);
    
    return mInstance;
}

StRichPadMonitor::StRichPadMonitor(StRichGeometryDb* geoDb)
    : mGeometryDb(geoDb),  mLegendE(0),  mLegendPi(0), mLegendK(0), mLegendP(0),
      mFileEventNum(0),    mFileName(0), mZVertex(0),  mNumTracks(0)
{
    pionminus   = StPionMinus::instance();
    kaonminus   = StKaonMinus::instance();
    antiproton  = StAntiProton::instance();
    
    pionplus   = StPionPlus::instance();
    kaonplus   = StKaonPlus::instance();
    proton     = StProton::instance();  

    mListOfParticles.push_back(pionminus);
    mListOfParticles.push_back(pionplus);
    mListOfParticles.push_back(kaonminus);
    mListOfParticles.push_back(kaonplus);
    mListOfParticles.push_back(proton);
    mListOfParticles.push_back(antiproton);
    
    mTransform = StRichCoordinateTransform::getTransform(geoDb);

    // xtop, ytop, w h
    mRichCanvas = new TCanvas("richCanvas", "RICH Event Display",0,0,1200,900);
    //mRichCanvas->Range(-70,-50,85,50);
    mRichCanvas->Range(-70,-50,105,50);

    // ORiginal 1000,700 -- -70,-50,70,50
    //
    // make the text window
    mTextWindow = new StRichPadMonitorText();
    StRichDrawableTPad::setPadMonitorText(mTextWindow);
    StRichDrawableTG2T::setPadMonitor(this);
    //StRichTControl::setPadMonitor(this);
    //
    // make the boundaries of the detector
    mRowPitch  = mGeometryDb->rowPitch();  //.84;
    mPadPitch  = mGeometryDb->padPitch();  //.80;
    mPadLength = mGeometryDb->padLength(); //.79;
    mPadWidth  = mGeometryDb->padWidth();  //.75;

//     mHall = (St_Node*)GetDataSet("HALL");
//     if(!mHall) {
// 	cout << "HALL NOT GOTTEN" << endl;
// 	//return -9;  // kStErr
//     }
//     mHall->FindByName("RICH")->Draw();

	
    //
    // Define the extreme values of Quadrant 2
    // the values should be positive definite
    //      _________________________________
    //     | (zco,xco) --> (xco,yco)         |
    //     |
    //     |
    //     ..
    //                 (zci,xci)-->(xci,yci)
    //     |_________________________________|
    double yco = mGeometryDb->quadrantY0(2)+.5*mRowPitch;  //41.82;
    double xco = -mGeometryDb->quadrantX0(2)+.5*mPadPitch; //65.9;
    double yci = -mGeometryDb->quadrantY0(4)-.5*mRowPitch; //1.5;
    double xci = mGeometryDb->quadrantX0(4)-.5*mPadPitch;  //1.5;

    TLine aLine;
    aLine.SetLineWidth(2);

    TBox aBox;
    aBox.SetLineWidth(2);
    aBox.SetLineColor(1);
    aBox.SetFillStyle(0);

    // Quadrant 1 Outline
    aBox.DrawBox(xci,yci,xco,yco);

    // Quadrant 2 Outline
    aBox.DrawBox(-xci,yci,-xco,yco);

    // Quadrant 3 Outline
    aBox.DrawBox(-xci,-yci,-xco,-yco);

    // Quandrant 4 Outline
    aBox.DrawBox(xci,-yci,xco,-yco);
    
    //aLine.DrawLine(xco, -yci, xco, -yco);

    // color scale
    this->drawColorBox();
    this->drawLegend();
    // make control panel
    
    //
    // Make controls
    // mControls;
//     StRichTControl* g2tControl = new StRichTControl(1.5,43,5.5,48,"g2t");
//     g2tControl->SetTextColor(2);
//     g2tControl->Draw();
}

void StRichPadMonitor::drawColorBox()
{
    double lowerX = 75.;
    double upperX = 77.;
    double lowerY = -35;
    double upperY;

    //cout << "Draw Colors! " << endl;
    for(int ii=0; ii<1024; ii++) {
	upperY = lowerY + .07; 
	mColorBoxes.Add(new TBox(lowerX,lowerY,upperX,upperY));
	((TBox*)mColorBoxes.Last())->SetFillColor(GetColorAttribute(static_cast<double>(ii)));
	((TBox*)mColorBoxes.Last())->Draw();
	if((ii == 10)  || (ii == 50)  || (ii == 100) ||
	   (ii == 200) || (ii == 500) || (ii == 1000)) {
	    mTextLabels.Add( new TPaveText((lowerX-7),(lowerY-2),(lowerX-2),(lowerY+1)) );
	    char text[5];
	    sprintf(text,"%d",ii);
	    dynamic_cast<TPaveText*>(mTextLabels.Last())->AddText(text);
	    dynamic_cast<TPaveText*>(mTextLabels.Last())->Draw();
	}
	lowerY = upperY;
    }
}

StRichPadMonitor::~StRichPadMonitor()
{
    cout << "in ~StRichPadMonitor" << endl;
    this->clearAll();
    // must delete
    // mColorBoxes
    // mTextLabels
    delete mRichCanvas;

    delete mLegendE;     // legend pointers
    delete mLegendPi;
    delete mLegendK;
    delete mLegendP;

    delete mFileName;
    delete mFileEventNum;

    delete mZVertex;
    delete mNumTracks;
    cout << "out ~StRichPadMonitor" << endl;
}

void StRichPadMonitor::clearAll() {
  this->clearPads();
  this->clearG2T();
  this->clearHits();
  this->clearTracks();
  this->clearRingInfo();
  this->clearMisc();
}

void StRichPadMonitor::clearPads()
{
//     //cout << "StRichPadMonitor::clearPads()" << endl;

//     for(int ii=0; ii<mAllFilledPads.GetEntries(); ii++) {
// 	(mAllFilledPads[ii])->Delete();	    
//     }
//     mAllFilledPads.Clear();
//     mAllFilledPads.Expand(0);

    mAllFilledPads.Delete();
}

void StRichPadMonitor::clearG2T()
{
    //cout << "StRichPadMonitor::clearGeant()" << endl;
    //PR(mG2TSegments.GetEntries());

//     for(int ii=0; ii< mG2TSegments.GetEntries(); ii++) {
// 	(mG2TSegments[ii])->Delete();	    
//     }
//     mG2TSegments.Clear();
//     mG2TSegments.Expand(0);
    mG2TSegments.Delete();
}

void StRichPadMonitor::clearHits() {
//      cout << "StRichPadMonitor::clearHits()" << endl;
//     PR(mHits.GetEntries());

//     for(int ii=0; ii< mHits.GetEntries(); ii++) {
// 	(mHits[ii])->Delete();	    
//     }
//     mHits.Clear();
//     mHits.Expand(0);
    mHits.Delete();
}

void StRichPadMonitor::clearTracks() {
    //cout << "StRichPadMonitor::clearTracks()" << endl;
    for(unsigned int j=0; j<mVectorTracks.size(); j++) {
	delete mVectorTracks[j];
    }
  
    mVectorTracks.clear();
    mVectorTracks.resize(0);

    this->clearRingInfo();
}

void StRichPadMonitor::clearRingInfo() {
     cout << "StRichPadMonitor::clearRingInfo()" << endl;
//     for(int ii=0; ii< mRingInfo.GetEntries(); ii++) {
// 	(mRingInfo[ii])->Delete();	    
//     }
//     mRingInfo.Clear();
//     mRingInfo.Expand(0);
     
     mRingInfo.Delete();
}

void StRichPadMonitor::clearMisc()
{
//     cout << "StRichPadMonitor::clearMisc()" << endl;

//     for(int ii=0; ii<mMisc.GetEntries(); ii++) {
// 	(mMisc[ii])->Delete();	    
//     }
//     mMisc.Clear();
//     mMisc.Expand(0);

    mMisc.Delete();
}

void StRichPadMonitor::drawPads()
{
    //cout << "StRichPadMonitor::drawPads()" << endl;
    for(int ii=0; ii<mAllFilledPads.GetEntries(); ii++) {
	(mAllFilledPads[ii])->Draw();
    }
}

void
StRichPadMonitor::calculatePadPosition(const StRichSinglePixel* pad,
				       double* xl, double* yl, double* xu, double* yu)
{
    // use coordinate tform here
    StRichRawCoordinate raw(pad->pad(), pad->row());
    StRichLocalCoordinate local;
   (*mTransform)(raw,local);
   //PR(raw);
   //PR(local);
    double yo = local.position().y();
    double xo = local.position().x();
    *yu = yo + mPadLength/2;
    *yl = *yu - mPadLength;
    *xu = xo + mPadWidth/2;
    *xl = *xu - mPadWidth;
}

void StRichPadMonitor::drawPad(const StRichSingleMCPixel& mcPad)
{
    // Make it a drawable pad
    // Coordinate Transform
    double xl,xu,yl,yu;
    this->calculatePadPosition(&mcPad,&xl,&yl,&xu,&yu);
    StRichDrawableMCTPad* dtp = new StRichDrawableMCTPad(xl,yl,xu,yu,&mcPad);
    dtp->SetFillColor(GetColorAttribute(mcPad.charge())); // scale by ADC color
    dtp->SetLineColor(2);  // black
    dtp->Draw();
    mAllFilledPads.Add(dtp);
}

void StRichPadMonitor::drawPad(const StRichSinglePixel& pad)
{
    // Make it a drawable pad
    // Coordinate Transform
    double xl,xu,yl,yu;
    calculatePadPosition(&pad,&xl,&yl,&xu,&yu);
    StRichDrawableTPad* dtp = new StRichDrawableTPad(xl,yl,xu,yu,&pad);
    dtp->SetFillColor(GetColorAttribute(pad.charge())); // scale by ADC color
    dtp->SetLineColor(2);  // black
    dtp->Draw();
    mAllFilledPads.Add(dtp);
}

void StRichPadMonitor::drawG2T(const StRichG2TInfo& g2t)
{
    // Letter
    //
    StRichDrawableTG2T* ttx = new StRichDrawableTG2T(g2t);
    ttx->SetTextSize(.03);
    ttx->SetTextColor(1);
    ttx->Draw();
    mG2TSegments.Add(ttx);
}

void StRichPadMonitor::drawGeantGroup(int trackp, int color)
{
    //
    // Loop over all mG2TSegments
    //cout << "#mG2tSegments= " << mG2TSegments.GetEntries() << endl;
    for(int ii=0; ii<mG2TSegments.GetEntries(); ii++) {
 	if(dynamic_cast<StRichDrawableTG2T*>(mG2TSegments.At(ii))->mTrackp == trackp) {
	    int currentColor =
		dynamic_cast<StRichDrawableTG2T*>(mG2TSegments.At(ii))->GetTextColor();
	    switch(currentColor) {
	    case 1:
		currentColor = 2;
		break;
	    case 2:
		currentColor = 1;
		break;
	    default:
		currentColor = 1;
		break;
	    }		
 	    dynamic_cast<StRichDrawableTG2T*>(mG2TSegments.At(ii))->SetTextColor(currentColor);
	    dynamic_cast<StRichDrawableTG2T*>(mG2TSegments.At(ii))->Paint();
 	}
    }
     this->update();
}

void StRichPadMonitor::drawHit(StRichSimpleHit* hit)
{
#ifndef SUN
    // Letter
    //
    //cout << "StRichPadMonitor::drawHit() " << *hit << endl;
    StRichDrawableTHit* thit = new StRichDrawableTHit(*hit);
    thit->SetMarkerSize(1.5);
    thit->SetMarkerColor(1);
    thit->Draw();
    mHits.Add(thit);
#endif
}

void StRichPadMonitor::doResiduals(double zVertex,long numPrim[],int runId,int eventId){
    
    float tempArray[16] = {-999};
    float primaryArray[26] = {0};
    
    TFile *residFile = new TFile("resid.root","NEW");
    TNtuple *residNtuple    = 0;
    TNtuple * primaryNtuple = 0;
    unsigned int appending  = 0;     
     
    if( !residFile->IsOpen() ) {
	
	delete residFile;
	residFile = new TFile("resid.root","UPDATE");
	residNtuple = (TNtuple*)residFile->Get("residNtuple");
	primaryNtuple = (TNtuple*)residFile->Get("primaryNtuple");
	appending = 1;
    }
    else{
	residNtuple =
	    new TNtuple("residNtuple",
			"Residual Info",
			"run:evt:hitX:hitY:hitCharge:mipX:mipY:mipPx:mipPy:mipPz:zVert:assocToClosestMip:particleTypes:eta:nTpcHits:nTpcFitHits");
	primaryNtuple = new TNtuple("primaryNtuple","Track Info","run:evt:numPrim1:numPrim2:numPrim3:numPrim4:numPrim5:numPrim6:numPrim7:numPrim8:numPrim9:numPrim10:zVert:nTracks1:nTracks2:nTracks3:nTracks4:nTracks5:nTracks6:nTracks7:nTracks8:nTracks9:nTracks10:nTracks11:numHitsInRich:numPadsInRich");
    }
    
    primaryArray[0] = runId;
    primaryArray[1] = eventId;
    primaryArray[2] = numPrim[0]; // eta < -1
    primaryArray[3] = numPrim[1]; // -1 <= eta < -.75
    primaryArray[4] = numPrim[2]; // ...
    primaryArray[5] = numPrim[3];
    primaryArray[6] = numPrim[4];
    primaryArray[7] = numPrim[5];
    primaryArray[8] = numPrim[6];
    primaryArray[9] = numPrim[7];  // ...
    primaryArray[10] = numPrim[8]; // .75 <= eta < 1
    primaryArray[11] = numPrim[9]; // eta >= 1
    primaryArray[12] = zVertex;

    // Fill Number Of Tracks Hitting Rich Binned in Momentum
    for(unsigned int zz=0; zz<mVectorTracks.size(); zz++) {

	StRichTrack * currentRichTrack = mVectorTracks[zz]->getTrack();
	double momentum = currentRichTrack->getMomentum().mag();
	    
	if(momentum > 5) // Put In final Momentum Bin
	    momentum = 5;

	primaryArray[static_cast<unsigned int>(floor(13+2*momentum))]++;

    }
    
    
    primaryArray[24] = mHits.GetEntries();
    primaryArray[25] = mAllFilledPads.GetEntries();
    
    primaryNtuple->Fill(primaryArray);
    cout << "Fill primaryArray";
    float curHitX = 0;
    float curHitY = 0;
    float curMipX = 0;
    float curMipY = 0;
    
    for(int ii=0; ii< static_cast<int>(primaryArray[24]); ii++) { // Loop over All Hits
	
	StRichDrawableTHit * currentHit = static_cast<StRichDrawableTHit*>(mHits[ii]);
	StRichDrawableTMip * closestMIP = 0;
	StRichDrawableTMip * currentMIP = 0;
	
	double closestResid = 9999;
	double currentResid = 9999;
	
	for(unsigned int jj=0;jj<mVectorTracks.size();jj++){
	    
	    currentMIP = mVectorTracks[jj]->getProjectedMIP();
	    curHitX = currentHit->GetX();
	    curHitY = currentHit->GetY();
	    curMipX = currentMIP->GetX();
	    curMipY = currentMIP->GetY();
	    
	    currentResid = sqrt( (curHitX-curMipX)*(curHitX-curMipX)+
				 (curHitY-curMipY)*(curHitY-curMipY));
		
	    if(currentResid < closestResid){
		closestResid = currentResid;
		closestMIP = currentMIP;
	    }
	    
	}

	tempArray[0] = runId;
	tempArray[1] = eventId;
	tempArray[2] = curHitX;
	tempArray[3] = curHitY;
	tempArray[4] = currentHit->getCharge();
		
	if(closestMIP != 0){// ie, MIP found
	    
	    tempArray[5] = closestMIP->GetX();
	    tempArray[6] = closestMIP->GetY();
	    
	    StRichDrawableTTrack * curTTrack = closestMIP->getDrawableTTrack();
	    StRichTrack * curRichTrack = curTTrack->getTrack();
	    
	    StThreeVectorF tempMom = curRichTrack->getMomentumAtPadPlane();
	    tempArray[7] = tempMom.x();
	    tempArray[8] = tempMom.y();
	    tempArray[9] = tempMom.z();
	    tempArray[10] = zVertex;
	    tempArray[14] = curRichTrack->getStTrack()->detectorInfo()->numberOfPoints(kTpcId);
	    tempArray[15] = curRichTrack->getStTrack()->fitTraits().numberOfFitPoints(kTpcId);
	    
	    tempArray[11] = 0;
	    unsigned int array12 = 0;
	    
	    for(int ringCounter = 0 ; ringCounter < curTTrack->numberOfRings();ringCounter++){
		
		StRichDrawableTRings* currentRing = curTTrack->getRing(ringCounter);
		if(currentRing){
		    for(int hitCounter = 0;hitCounter < currentRing->numberOfHits();hitCounter++){
			StRichDrawableTHit * loopHit = currentRing->getHit(hitCounter);
			if((abs(loopHit->GetX() - curHitX) < .0001) &&
			   (abs(loopHit->GetY() - curHitY) < .0001)){
			    tempArray[11] = 1;
			    StParticleDefinition * currPart = currentRing->getParticle();
			    if(currPart == pionplus || currPart == pionminus)
				array12 |= 1;
			    else if(currPart == kaonplus || currPart == kaonminus)
				array12 |= 2;
			    else if(currPart == proton || currPart == antiproton)
				array12 |= 4;
			}
		    }
		}
	    }
	    
	    tempArray[12] = static_cast<float>(array12);
	    tempArray[13] = static_cast<float>(curRichTrack->getStTrack()->geometry()->momentum().pseudoRapidity());
	}
	residNtuple->Fill(tempArray);
    }
    
    if(appending) {
	residNtuple->Write("residNtuple",TObject::kOverwrite);
	primaryNtuple->Write("primaryNtuple",TObject::kOverwrite); }
    else{
	residNtuple->Write();
	primaryNtuple->Write();}
    
    residFile->Close();
    delete residFile;    
}

void StRichPadMonitor::addPad(StRichSinglePixel* pad)
{
    double xl,xu,yl,yu;
    this->calculatePadPosition(pad,&xl,&yl,&xu,&yu);
    
    StRichDrawableTPad* dtp = new StRichDrawableTPad(xl,yl,xu,yu, pad);

    dtp->SetLineWidth(1);
    dtp->SetLineColor(2);  // black
    dtp->SetFillColor(GetColorAttribute(pad->charge())); // scale by ADC color
    mAllFilledPads.Add(dtp);    
}

void StRichPadMonitor::update()
{
    mRichCanvas->Update();
}

Color_t StRichPadMonitor::GetColorAttribute(double amp)
{
    //return Color_t(50+(log(static_cast<int>((amp/200.)*50))) );

    double tmpAmp;
    // linear color scale
     tmpAmp = (amp>256) ? 256 : amp;
     Color_t ret = (50+(static_cast<int>((tmpAmp/256.)*50)) );
     if(ret <= 50)
 	ret +=1;
     return ret;

    // log color scale
//     if(amp<1)
// 	tmpAmp = 1;
//     else
// 	tmpAmp = amp;
//     //tmpAmp = max(1,amp);  // sigh. if only STL was used
//     Color_t ret = (50+(static_cast<int>(log(tmpAmp)*7.21)) );
//     if(ret <= 50)
// 	ret +=1;
//     return ret;
}

void StRichPadMonitor::addTrack(StRichTrack* track) {

    mVectorTracks.push_back(new StRichDrawableTTrack(track));

    //
    // Draw the projection onto the pad plane from the MIP (and p value)
    // in the StRichDrawableTTrack
    //
    //cout << "mVectorTracks.back()->getProjectedMIP()" << 
    mVectorTracks.back()->getProjectedMIP()->draw();
}

void StRichPadMonitor::drawRingInfo() {

    cout << "StRichPadMonitor::drawRingInfo() " << endl;
    // loop over all tracks

    int currentGoodTrack      = 0;
    
    float lowerY = 30;
    float upperY = 50;
    float ystep  = 22;

    float lowerX = 80.;//85.;
    float upperX = 95;//100.;
    float xstep  = 20.;

    float iy1,iy2,ix1,ix2;
    
//     cout << "mVectorTracks.size() " << mVectorTracks.size() << endl;
    for(size_t itrack=0; itrack<mVectorTracks.size(); itrack++) {
	StRichTrack* tt = mVectorTracks[itrack]->getTrack();

	if(tt->getMomentum().mag() < .3) continue;

	if(currentGoodTrack<4) {
	    ix1 = lowerX;
	    ix2 = upperX;
	    iy1 = lowerY-(ystep*currentGoodTrack);
	    iy2 = upperY-(ystep*currentGoodTrack);
	}
	else {
	    ix1 = lowerX+xstep;
	    ix2 = upperX+xstep;
	    iy1 = lowerY-(ystep*(currentGoodTrack-4));
	    iy2 = upperY-(ystep*(currentGoodTrack-4));
	}
    
// 	TPaveText* particledata = new TPaveText(85, initialBottomCorner-(stepSize*currentGoodTrack),
// 						100,initialTopCorner-(stepSize*currentGoodTrack));
 	TPaveText* particledata = new TPaveText(ix1, iy1, ix2, iy2);

	char title[50];

	//theta = acos(-pz/sqrt(px**2+py**2+pz**2));ls

	sprintf(title,"%.3f  %.3f", tt->getMomentum().mag(), tt->getTheta()*180./M_PI);
	particledata->AddText(title);
	int total[3]    = {0,0,0}; // total number of photons associated
	int inarea[3]   = {0,0,0}; // total number of photons in area
	int sig1[3]     = {0,0,0}; // total number of photons within 1 sigma
	int sig2[3]     = {0,0,0}; // total number of photons within 2 sigma
	float area[3]   = {0.,0.,0.}; // total area
	float tarea[3]  = {0.,0.,0.}; // truncated area

	//
	// loop over the hypothesis
	StRichPidTraits* theTraits = tt->getPidTrait();
	if(!theTraits) {cout << "drawringinfo error:::thetraits" << endl;}

	const StSPtrVecRichPid& thePids = theTraits->getAllPids();
// 	cout << "thePids.size() " << thePids.size() << endl;

	if(thePids.size() == 0) continue;
	
	for(size_t ii=0; ii<thePids.size(); ii++) {
	    StRichPid* pid = thePids[ii];

	    if(!pid) {cout << "not okay " << ii << endl; continue;}

	    area[ii] =  pid->getTotalArea();
	    tarea[ii] = pid->getTruncatedArea();
	    
	    const StPtrVecRichHit& theHits = pid->getAssociatedRichHits();

// 	    cout << "theHits.size() " << theHits.size() << endl;
	    for(size_t jj=0; jj<theHits.size(); jj++) {
		//cout << "pid->getParticleNumber() " << pid->getParticleNumber() << endl;
		switch(pid->getParticleNumber()) {
		case -211:      // pi- mListOfParticles[
		    if(theHits[jj])
			total[0]++;
		    if(theHits[jj]->isSet(eInAreaPi))
			inarea[0]++;
		    if(theHits[jj]->isSet(e1SigmaPi))
			sig1[0]++;
		    if(theHits[jj]->isSet(e2SigmaPi))
			sig2[0]++;
		    break;
		case -321:      // K- mListOfParticles[
		    if(theHits[jj])
			total[1]++;
		    if(theHits[jj]->isSet(eInAreaK))
			inarea[1]++;
		    if(theHits[jj]->isSet(e1SigmaK))
			sig1[1]++;
		    if(theHits[jj]->isSet(e2SigmaK))
			sig2[1]++;
		    break;
		case -2212:      // p- mListOfParticles[
		    if(theHits[jj])
			total[2]++;
		    if(theHits[jj]->isSet(eInAreap))
			inarea[2]++;
		    if(theHits[jj]->isSet(e1Sigmap))
			sig1[2]++;
		    if(theHits[jj]->isSet(e2Sigmap))
			sig2[2]++;
		    break;
		default:
		    cout << "StRichPadMonitor::drawRingInfo()";
		    cout << "\tERROR unknown particle " << pid->getParticleNumber() << endl;
		    break;
		}
	    }
	}
    
	char totalc[30];
	sprintf(totalc,"all %d %d %d",total[0],total[1],total[2]);
	particledata->AddText(totalc);
	
	char pareac[30];
	sprintf(pareac,"pa %d %d %d",inarea[0],inarea[1],inarea[2]);
	particledata->AddText(pareac);
	
	char sig1c[30];
	sprintf(sig1c,"1s %d %d %d",sig1[0],sig1[1],sig1[2]);
	particledata->AddText(sig1c);
	
	char sig2c[30];
	sprintf(sig2c,"2s %d %d %d",sig2[0],sig2[1],sig2[2]);
	particledata->AddText(sig2c);
	
	char areac[30];
	sprintf(areac,"A  %.0f %.0f %.0f",area[0],area[1],area[2]);
	particledata->AddText(areac);
	
	char tareac[30];
	sprintf(tareac,"TA %.0f %.0f %.0f",tarea[0],tarea[1],tarea[2]);
	particledata->AddText(tareac);
	
	particledata->SetTextSize(.02);
	particledata->Draw();
	
	mRingInfo.Add(particledata);
	currentGoodTrack++;
    }
}

void StRichPadMonitor::drawMarker(StThreeVectorF& from, int type, float size, int color)
{
    // TMarker(x1,y1,type=26)  (triangle)
    // store in mLine;
    TMarker* marker = new TMarker(from.x(),from.y(),type);
    marker->SetMarkerSize(size);
    marker->SetMarkerColor(color);
    marker->Draw();

    mMisc.Add(marker);
}

void StRichPadMonitor::drawLine(StThreeVectorF& from, StThreeVectorF& to)
{
    // TLine(x1,y1,x2,y2)
    // store in mLine;
    TLine* line = new TLine(from.x(),from.y(),to.x(),to.y());
    line->Draw();

    mMisc.Add(line);
}

StRichDrawableTTrack* StRichPadMonitor::getTrack(StRichTrack* track) {
  
    for(unsigned int i = 0; i < mVectorTracks.size() ; i++) {
	if(mVectorTracks[i]->getTrack() == track)
	    return mVectorTracks[i];
    }

    return 0;
}

void StRichPadMonitor::drawRings() {
    
    cout << "StRichPadMonitor::drawRings()" << endl;
    //
    // Loop over the tracks
    //
    for(unsigned int j=0; j<mVectorTracks.size(); j++) {
	for(int i=0; i<mVectorTracks[j]->numberOfRings(); i++) {
// 	    cout << "mVectorTracks[j]->getTrack()->getMomentum().mag() "
// 		 << mVectorTracks[j]->getTrack()->getMomentum().mag() << endl;
	    if( mVectorTracks[j]->getTrack()->getMomentum().mag() < 0.3 ) continue;

	    //
	    // Draw the rings and load all the DrawableTHits
	    // associated with a DrawableTRing
	    //
	    // an StRichDrawableTRing
	    //
	    mVectorTracks[j]->getRing(i)->draw();
	    
	} // the loop over the rings

	//
	// for the track in question
	//

	for (size_t particleIndex=0;
	     particleIndex<mListOfParticles.size();
	     particleIndex++) {
	    StParticleDefinition* particle = mListOfParticles[particleIndex];
	    
	    if (!particle) continue;
	    StRichDrawableTRings* currentTRing = mVectorTracks[j]->getRing(particle);
		
	    if (!currentTRing) continue;
	    
	    vector<StRichRingHit*> hits = mVectorTracks[j]->getTrack()->getRingHits(particle);
	    //cout << "part index: " << particleIndex << " hits " << hits.size() << endl;
	    //
	    // Loop over all the hits and add them to the DrawableTRing
	    //
	    for (size_t hitIndex=0; hitIndex<hits.size(); hitIndex++) {
		
		if( hits[hitIndex] && hits[hitIndex]->getHit()) {
		    currentTRing->addHit(hits[hitIndex]->getHit());
		}
	    } // loop over all the hits
	    
	}     // particle hypothesis
    }         // for all tracks
}

void StRichPadMonitor::hiLiteHits() {

    cout << "StRichPadMonitor::hiLiteHits()" << endl;

    //
    // loop over all tracks
    //
    for(size_t ii=0; ii<mVectorTracks.size(); ii++) {
	
	for (size_t particleIndex=0;
	     particleIndex<mListOfParticles.size();
	     particleIndex++) {
	    StParticleDefinition* particle = mListOfParticles[particleIndex];
	    
	    if (!particle) continue;
	    StRichDrawableTRings* currentTRing = mVectorTracks[ii]->getRing(particle);
		
	    if (!currentTRing) continue;
	    currentTRing->hilite();

	}  // particle types
    }      // tracks
    
}

void StRichPadMonitor::hiLiteHits(const StRichHitFlag& flag) {

    cout << "StRichPadMonitor::hiLiteHits(flag)" << endl;

    //
    // loop over all tracks
    //
    for(size_t ii=0; ii<mVectorTracks.size(); ii++) {
	
	for (size_t particleIndex=0;
	     particleIndex<mListOfParticles.size();
	     particleIndex++) {
	    StParticleDefinition* particle = mListOfParticles[particleIndex];
	    
	    if (!particle) continue;
	    StRichDrawableTRings* currentTRing = mVectorTracks[ii]->getRing(particle);
		
	    if (!currentTRing) continue;
	    currentTRing->hilite(flag);

	} // loop over particle type
    }     // loop over tracks
}

void StRichPadMonitor::drawZVertex(double zVert,int numTracksPrim,int numTracksSec) {
    
    if(mZVertex)   delete mZVertex;
    if(mNumTracks) delete mNumTracks;

    char tempChar[200];

    if(zVert < -67 || zVert > 67){
	sprintf(tempChar,"%d Vertex Not Above RICH: at %f",numTracksPrim,zVert);
	mNumTracks = new TText(2,0,tempChar);
	mZVertex = new TMarker(0,0,29);
    }
    else {
	mZVertex = new TMarker(zVert,0,29);
	sprintf(tempChar,"%d",numTracksPrim);
	mNumTracks = new TText(zVert+2,0,tempChar);
    }
    

    mNumTracks->SetTextColor(51);
    mNumTracks->SetTextSize(.023);
    mNumTracks->SetTextAlign(12);
    mNumTracks->Draw();
    
    mZVertex->SetMarkerColor(51);
    mZVertex->SetMarkerSize(2.9);
    mZVertex->Draw();
}

void StRichPadMonitor::drawLegend() {

    /* mLegendE = new TText(-60.0,-50.0,"e");
       mLegendE->SetTextColor(2);
       mLegendE->SetTextFont(1);
       mLegendE->Draw(); */
    
    mLegendPi = new TText(-60.0,-47.0,"p"); // Red p in greek
    mLegendPi->SetTextColor(2);
    mLegendPi->SetTextFont(122);
    mLegendPi->Draw();

    mLegendK = new TText(-55.0,-47.0,"K"); // black K in greek
    mLegendK->SetTextColor(1);
    mLegendK->SetTextFont(122);
    mLegendK->Draw();

    mLegendP = new TText(-50.0,-47.0,"p"); // green P
    mLegendP->SetTextColor(3);
    mLegendP->SetTextFont(1);
    mLegendP->Draw();
    
}

void StRichPadMonitor::drawEventInfo(Long_t runId,Long_t eventId) {

    // delete the old info
    if(mFileEventNum)	delete mFileEventNum;
    
    if (sprintf(mFileTextEventNum,"Run: %u \t Event: %u",
		static_cast<int>(runId),static_cast<int>(eventId))) {

	mFileEventNum = new TText(65.2,-46.5,mFileTextEventNum);
	mFileEventNum->SetTextColor(1);
	mFileEventNum->SetTextFont(1);
	mFileEventNum->SetTextSize(.05);
	mFileEventNum->SetTextAlign(31);
	mFileEventNum->Draw();
    }
    
}

void StRichPadMonitor::drawFileName(char * fileName) {

    if(mFileName) delete mFileName;
    
    mFileName = new TText(65.2,-48.7,fileName);
    mFileName->SetTextColor(1);
    mFileName->SetTextFont(1);
    mFileName->SetTextSize(.019495); // Found this works
    mFileName->SetTextAlign(31);
    mFileName->Draw();
}


void StRichPadMonitor::printCanvas(char* directory, char * filename, int eventNum){ 
    
    char tempChar[300];
    char* newFile = new char[100];
    int i = 0;
    while(filename[i] != '\0')
	i++;
    while( (i != 0) && (filename[i] != '/'))
	i--;
    if(filename[i] == '/') 
	i++;

    int j=0;
    int ii=i;
    while(j != 99) {
	if (filename[ii]=='r') break;
	newFile[j] = filename[ii++];
	j++;
    }
    newFile[j]='\0';

    sprintf(tempChar,"%s_%s%d.ps",directory,newFile,eventNum);
    mRichCanvas->Print(tempChar);
    delete newFile;
}

#endif
