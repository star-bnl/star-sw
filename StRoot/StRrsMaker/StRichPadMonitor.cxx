/****************************************************************
 * $Id: StRichPadMonitor.cxx,v 1.3 2000/03/13 21:50:35 lasiuk Exp $
 * Description:
 *  First aTtempt at a simple Pad Monitor.
 *  Runs only in ROOT
 *
 *****************************************************************
 *
 * $Log: StRichPadMonitor.cxx,v $
 * Revision 1.3  2000/03/13 21:50:35  lasiuk
 * coordinates
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
#include "StRichDrawableTPad.h"
#include "StRichPadMonitorText.h"
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
    mTransform = StRichCoordinateTransform::getTransform(geoDb);
    // xtop, ytop, w h
    mRichCanvas = new TCanvas("richCanvas", "RICH Event Display",0,0,1200,900);
    mRichCanvas->Range(-70,-50,85,50);

    // ORiginal 1000,700 -- -70,-50,70,50
    //
    // make the text window
    mTextWindow = new StRichPadMonitorText();
    StRichDrawableTPad::setPadMonitorText(mTextWindow);
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

    cout << "yco (41.82) " << yco << endl;
    cout << "yci (1.5)   " << yci << endl;
    cout << "xco (65.5)  " << xco << endl;
    cout << "xci (1.5)   " << xci << endl;

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
}

void StRichPadMonitor::drawColorBox()
{
    double lowerX = 75.;
    double upperX = 77.;
    double lowerY = -35;
    double upperY;
    cout << "Draw Colors! " << endl;
    for(int ii=0; ii<1024; ii++) {
	upperY = lowerY + .07; 
	mColorBoxes.Add(new TBox(lowerX,lowerY,upperX,upperY));
	((TBox*)mColorBoxes.Last())->SetFillColor(GetColorAttribute(ii));
	((TBox*)mColorBoxes.Last())->Draw();
	if((ii == 10) || (ii == 50) || (ii == 100) || (ii == 200) || (ii == 500) || (ii == 1000)) {
	    mTextLabels.Add( new TPaveText((lowerX-7),(lowerY-2),(lowerX-2),(lowerY+1)) );
	    char text[5];
	    sprintf(text,"%d",ii);
	    //((TPaveText*)mTextLabels.Last())->SetLabel(text);
	    ((TPaveText*)mTextLabels.Last())->AddText(text);
	    ((TPaveText*)mTextLabels.Last())->Draw();
	}
	lowerY = upperY;
    }
}

StRichPadMonitor::~StRichPadMonitor()
{
    clearPads();
    delete mRichCanvas;
}

void StRichPadMonitor::clearPads()
{
    cout << "StRichPadMonitor::clearPads()" << endl;
  // Delete the allocated space
  //for(int ii=0; ii<mAllFilledPads.size(); ii++) {
  PR(mAllFilledPads.GetEntries());

  for(int ii=0; ii<mAllFilledPads.GetEntries(); ii++) {
    (mAllFilledPads[ii])->Delete();	    
  }
  mAllFilledPads.Clear();
  PR(mAllFilledPads.GetEntries());
  mAllFilledPads.Expand(0);
  PR(mAllFilledPads.GetEntries());
}

void StRichPadMonitor::drawPads()
{
//     cout << "StRichPadMonitor::drawPads()" << endl;
//     for(int ii=0; ii<mAllFilledPads.size(); ii++) {
    for(int ii=0; ii<mAllFilledPads.GetEntries(); ii++) {
	(mAllFilledPads[ii])->Draw();
    }
}

void
StRichPadMonitor::calculatePadPosition(StRichSinglePixel* pad, double* xl, double* yl, double* xu, double* yu)
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

void StRichPadMonitor::drawPad(StRichSinglePixel pad)
{
    // Make it a drawable pad
    // Coordinate Transform
    double xl,xu,yl,yu;
    calculatePadPosition(&pad,&xl,&yl,&xu,&yu);
    StRichDrawableTPad* dtp = new StRichDrawableTPad(xl,yl,xu,yu,&pad);
    dtp->SetFillColor(GetColorAttribute(pad.amplitude())); // scale by ADC color
    dtp->SetLineColor(2);  // black
    dtp->Draw();
//     mAllFilledPads.push_back(dtp);
//     PR(mAllFilledPads.size());
    mAllFilledPads.Add(dtp);
//     PR(mAllFilledPads.GetEntries());
}


void StRichPadMonitor::addPad(StRichSinglePixel* pad)
{
    double xl,xu,yl,yu;
    calculatePadPosition(pad,&xl,&yl,&xu,&yu);
	
    StRichDrawableTPad* dtp = new StRichDrawableTPad(xl,yl,xu,yu, pad);

    dtp->SetLineWidth(1);
    dtp->SetLineColor(2);  // black
    //dtp->SetFillStyle(0);
    dtp->SetFillColor(GetColorAttribute(pad->amplitude())); // scale by ADC color
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

void StRichPadMonitor::addInnerRingPoint(double x, double y)
{
    mXPoints.push_back(x);
    mYPoints.push_back(y);
}
void StRichPadMonitor::addOuterRingPoint(double x, double y)
{
    mXOPoints.push_back(x);
    mYOPoints.push_back(y);
}

void StRichPadMonitor::drawRing()
{
 //    PR(mXPoints.size());
//     PR(mYPoints.size());
    if(!mXPoints.size()) {
	cout << "StRichPadMonitor::drawRing()\n";
	cout << "\tERROR\n";
	cout << "\tNo Points to Plot." << endl;
    }
    else {
	    for(int ii=1; ii<mXPoints.size(); ii++) {
// 		cout << "pts: "
// 		     << (mXPoints[ii-1]) << " "
// 		     << (mYPoints[ii-1]) << " "
// 		     << (mXPoints[ii])   << " "
// 		     << (mYPoints[ii]) << endl;
		TLine* aLine = new TLine(mXPoints[ii-1], mYPoints[ii-1],
					 mXPoints[ii],   mYPoints[ii]);
		mRingPoints.Add(aLine);
		aLine->Draw();

		TLine* bLine = new TLine(mXOPoints[ii-1], mYOPoints[ii-1],
					 mXOPoints[ii],   mYOPoints[ii]);
		mORingPoints.Add(bLine);
		//cout << "Try draw" << endl;
		bLine->Draw();
	    }
    }
}
#endif
