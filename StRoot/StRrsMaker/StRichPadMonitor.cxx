/****************************************************************
 * $Id: StRichPadMonitor.cxx,v 1.2 2000/02/29 18:19:38 lasiuk Exp $
 * Description:
 *  First aTtempt at a simple Pad Monitor.
 *  Runs only in ROOT
 *
 *****************************************************************
 *
 * $Log: StRichPadMonitor.cxx,v $
 * Revision 1.2  2000/02/29 18:19:38  lasiuk
 * Split needed classes into individual files
 *
 * Revision 1.2  2000/02/29 18:19:38  lasiuk
 * Split needed classes into individual files
 *
 * Revision 1.1  2000/02/12 21:57:56  lasiuk
 * Initial revision
 ****************************************************************/
#ifdef __ROOT__


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

    mTransform = new StRichCoordinateTransform(geoDb);
    : mGeometryDb(geoDb)
    mRichCanvas = new TCanvas("richCanvas", "RICH Event Display",0,0,1000,700);
    mRichCanvas->Range(-70,-50,70,50);
    // xtop, ytop, w h
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
    //     | (zco,xco)                       |
    // the values should be positive definite
    //      _________________________________
    //     | (zco,xco) --> (xco,yco)         |
    //                             (zci,xci)
    //     |
    double xco = mGeometryDb->quadrantX0(2)+.5*mRowPitch;  //41.82;
    double zco = -mGeometryDb->quadrantZ0(2)+.5*mPadPitch; //65.9;
    double xci = -mGeometryDb->quadrantX0(4)-.5*mRowPitch; //1.5;
    double zci = mGeometryDb->quadrantZ0(4)-.5*mPadPitch;  //1.5;

    cout << "xco (41.82) " << xco << endl;
    cout << "yco (41.82) " << yco << endl;
    cout << "zco (65.5)  " << zco << endl;
    cout << "zci (1.5)   " << zci << endl;
    cout << "yci (1.5)   " << yci << endl;
    cout << "xco (65.5)  " << xco << endl;
    cout << "xci (1.5)   " << xci << endl;

    TLine aLine;
    aLine.DrawLine(zci, xco, zco, xco);
    aLine.DrawLine(zci, xci, zco, xci);
    aLine.DrawLine(zci, xco, zci, xci);
    aLine.DrawLine(zco, xco, zco, xci);
    aLine.DrawLine(xci, yci, xco, yci);
    aLine.DrawLine(xci, yco, xci, yci);
    aLine.DrawLine(-zco, xco, -zci, xco);
    aLine.DrawLine(-zco, xci, -zci, xci);
    aLine.DrawLine(-zco, xco, -zco, xci);
    aLine.DrawLine(-zci, xco, -zci, xci);
    aLine.DrawLine(-xco, yci, -xci, yci);
    aLine.DrawLine(-xco, yco, -xco, yci);
    aLine.DrawLine(-zco, -xci, -zci, -xci);
    aLine.DrawLine(-zco, -xco, -zci, -xco);
    aLine.DrawLine(-zco, -xci, -zco, -xco);
    aLine.DrawLine(-zci, -xci, -zci, -xco);
    aLine.DrawLine(-xco, -yco, -xci, -yco);
    aLine.DrawLine(-xco, -yci, -xco, -yco);
    aLine.DrawLine(zci, -xci, zco, -xci);
    aLine.DrawLine(zci, -xco, zco, -xco);
    aLine.DrawLine(zci, -xci, zci, -xco);
    aLine.DrawLine(zco, -xci, zco, -xco);
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
StRichPadMonitor::calculatePadPosition(StRichSinglePixel* pad, double* zl, double* xl, double* zu, double* xu)

void
StRichPadMonitor::calculatePadPosition(StRichSinglePixel* pad, double* xl, double* yl, double* xu, double* yu)
{
    // use coordinate tform here
    StRichRawCoordinate raw(pad->pad(), pad->row());
    StRichLocalCoordinate local;
   //PR(raw);
    double zo = local.position().z();
    *xu = xo + mPadLength/2;
    *xl = *xu - mPadLength;
    *zu = zo + mPadWidth/2;
    *zl = *zu - mPadWidth;
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
    mRichCanvas->Update();
}
    return Color_t(50+(log(static_cast<int>(amp))) );
		bLine->Draw();
	    }
    }
}
#endif
