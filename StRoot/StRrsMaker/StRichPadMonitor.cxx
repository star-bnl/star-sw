/****************************************************************
 * $Id: StRichPadMonitor.cxx,v 1.7 2000/06/16 02:00:51 lasiuk Exp $
 * Description:
 *  First aTtempt at a simple Pad Monitor.
 *  Runs only in ROOT
 *
 *****************************************************************
 *
 * $Log: StRichPadMonitor.cxx,v $
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
#include "TPaveText.h"

#include "StRichPadMonitor.h"

#include "StRichGeometryDb.h"
#include "StRichCoordinateTransform.h"
#include "StRichSinglePixel.h"
#include "StRichSingleMCPixel.h"
#include "StRichDrawableTPad.h"
#include "StRichDrawableMCTPad.h"
#include "StRichDrawableTG2T.h"
// #include "StRichTControl.h"

#include "StRchMaker/StRichSimpleHit.h"
#include "StRchMaker/StRichDrawableTHit.h"

#include "StRichPIDMaker/StRichTDrawableRings.h"
#include "StRichPIDMaker/StRichTDrawableTrack.h"
#include "StRichPIDMaker/StRichTrack.h"
#include "StRichPIDMaker/StRichTDrawableTrack.h"

#include "StRichPadMonitorText.h"
#include "TText.h"

StRichPadMonitor* StRichPadMonitor::mInstance = 0;

StRichPadMonitor* StRichPadMonitor::getInstance(StRichGeometryDb* geo)
{
    if(!mInstance)
	mInstance = new StRichPadMonitor(geo);
    
    return mInstance;
}

StRichPadMonitor::StRichPadMonitor(StRichGeometryDb* geoDb)
    : mGeometryDb(geoDb)
{

    mLegendE = 0; // ---> initialize legend pointer to null
    mLegendPi = 0;
    mLegendK = 0;
    mLegendP = 0;

    mFileName = 0;
    mFileEventNum = 0;
    
    mTransform = StRichCoordinateTransform::getTransform(geoDb);
    // xtop, ytop, w h
    mRichCanvas = new TCanvas("richCanvas", "RICH Event Display",0,0,1200,900);
    mRichCanvas->Range(-70,-50,85,50);

    // ORiginal 1000,700 -- -70,-50,70,50
    //
    // make the text window
    mTextWindow = new StRichPadMonitorText();
    StRichDrawableTPad::setPadMonitorText(mTextWindow);
    StRichDrawableTG2T::setPadMonitor(this);
//     StRichTControl::setPadMonitor(this);
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

//     cout << "yco (41.82) " << yco << endl;
//     cout << "yci (1.5)   " << yci << endl;
//     cout << "xco (65.5)  " << xco << endl;
//     cout << "xci (1.5)   " << xci << endl;

    TLine aLine;
    aLine.SetLineWidth(2);
    
    // Quadrant 1 outline
    aLine.DrawLine(xci, yco, xco, yco);
    aLine.DrawLine(xci, yci, xco, yci);
    aLine.DrawLine(xci, yco, xci, yci);
    aLine.DrawLine(xco, yco, xco, yci);
    
    // Quadrant 2 outline
    aLine.DrawLine(-xco, yco, -xci, yco);
    aLine.DrawLine(-xco, yci, -xci, yci);
    aLine.DrawLine(-xco, yco, -xco, yci);
    aLine.DrawLine(-xci, yco, -xci, yci);
    
    // Quadrant 3 outline
    aLine.DrawLine(-xco, -yci, -xci, -yci);
    aLine.DrawLine(-xco, -yco, -xci, -yco);
    aLine.DrawLine(-xco, -yci, -xco, -yco);
    aLine.DrawLine(-xci, -yci, -xci, -yco);
    
    // Quadrant 4 outline
    aLine.DrawLine(xci, -yci, xco, -yci);
    aLine.DrawLine(xci, -yco, xco, -yco);
    aLine.DrawLine(xci, -yci, xci, -yco);
    aLine.DrawLine(xco, -yci, xco, -yco);

    // color scale
    drawColorBox();
    drawLegend();
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
	if((ii == 10) || (ii == 50) || (ii == 100) || (ii == 200) || (ii == 500) || (ii == 1000)) {
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
}

void StRichPadMonitor::clearAll()
{
    this->clearPads();
    this->clearG2T();
    this->clearHits();
    cerr << "\n Hits Cleared\n";
    this->clearTracks();
}
void StRichPadMonitor::clearPads()
{
    //cout << "StRichPadMonitor::clearPads()" << endl;

    for(int ii=0; ii<mAllFilledPads.GetEntries(); ii++) {
	(mAllFilledPads[ii])->Delete();	    
    }
    mAllFilledPads.Clear();
    mAllFilledPads.Expand(0);
}

void StRichPadMonitor::clearG2T()
{
    //cout << "StRichPadMonitor::clearGeant()" << endl;
    //PR(mG2TSegments.GetEntries());

    for(int ii=0; ii< mG2TSegments.GetEntries(); ii++) {
	(mG2TSegments[ii])->Delete();	    
    }
    mG2TSegments.Clear();
    mG2TSegments.Expand(0);
}

void StRichPadMonitor::clearHits()
{
    //cout << "StRichPadMonitor::clearHits()" << endl;
    //PR(mHits.GetEntries());

    for(int ii=0; ii< mHits.GetEntries(); ii++) {
	(mHits[ii])->Delete();	    
    }
    mHits.Clear();
    mHits.Expand(0);
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
    calculatePadPosition(&mcPad,&xl,&yl,&xu,&yu);
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
    thit->SetMarkerSize(2.5);
    thit->SetMarkerColor(1);
    thit->Draw();
    mHits.Add(thit);
#endif
}


void StRichPadMonitor::addPad(StRichSinglePixel* pad)
{
    double xl,xu,yl,yu;
    calculatePadPosition(pad,&xl,&yl,&xu,&yu);
	
    StRichDrawableTPad* dtp = new StRichDrawableTPad(xl,yl,xu,yu, pad);

    dtp->SetLineWidth(1);
    dtp->SetLineColor(2);  // black
    //dtp->SetFillStyle(0);
    dtp->SetFillColor(GetColorAttribute(pad->charge())); // scale by ADC color
//     mAllFilledPads.push_back(dtp);    
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

void StRichPadMonitor::addTrack(StRichTrack* track)
{

    mVectorTracks.push_back(new StRichTDrawableTrack(track)); 
    
}

StRichTDrawableTrack* StRichPadMonitor::getTrack(StRichTrack* track)
{

    for(unsigned int i = 0; i < mVectorTracks.size() ; i++)
	{
	    if(mVectorTracks[i]->getTrack() == track)
		return mVectorTracks[i];
	}
    return 0;
}

void StRichPadMonitor::drawRings()
{
    for(unsigned int j = 0; j < mVectorTracks.size();j++) {
	for(int i = 0;i < mVectorTracks[j]->numberOfRings();i++) {
	    mVectorTracks[j]->getRing(i)->draw();
	}
	mVectorTracks[j]->getProjectedMIP()->Draw();
    }
}

void StRichPadMonitor::clearTracks()
{
  cout << "in clearTracks()\n";
    for(unsigned int j=0; j < mVectorTracks.size();j++) {
	delete mVectorTracks[j];
    }
	
    mVectorTracks.clear();
    mVectorTracks.resize(0);
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

    mLegendK = new TText(-55.0,-47.0,"K"); // black K
    mLegendK->SetTextColor(1);
    mLegendK->SetTextFont(1);
    mLegendK->Draw();

    mLegendP = new TText(-50.0,-47.0,"p"); // green P
    mLegendP->SetTextColor(3);
    mLegendP->SetTextFont(1);
    mLegendP->Draw();
    
}

void StRichPadMonitor::drawEventNum(Int_t eventNum) {
    
    if(mFileEventNum)            // delete the old info
	delete mFileEventNum;
    
    if (sprintf(mFileTextEventNum,"Event: %i",eventNum))
	{
	    mFileEventNum = new TText(65.2,-46.5,mFileTextEventNum);
	    mFileEventNum->SetTextColor(1);
	    mFileEventNum->SetTextFont(1);
	    mFileEventNum->SetTextSize(.05);
	    mFileEventNum->SetTextAlign(31);
	    mFileEventNum->Draw();
	}
}

void StRichPadMonitor::drawFileName(char * fileName) {

    if(mFileName)            
	delete mFileName;
    
    mFileName = new TText(65.2,-48.7,fileName);
    mFileName->SetTextColor(1);
    mFileName->SetTextFont(1);
    mFileName->SetTextSize(.019495); // Found this works
    mFileName->SetTextAlign(31);
    mFileName->Draw();
}

#endif
