/******************************************************
 * $Id: StRichPadMonitor.cxx,v 1.1 2000/02/12 21:57:56 lasiuk Exp $
 * Description:
 *  First aTtempt at a simple Pad Monitor.
 *  Runs only in ROOT
 *
 * Auxiliary classes are kept here for now as well
 *
 * $Log: StRichPadMonitor.cxx,v $
 * Revision 1.1  2000/02/12 21:57:56  lasiuk
 * Initial revision
 *
 ******************************************************/
 * Revision 1.1  2000/02/12 21:57:56  lasiuk
 * Initial revision
#include "StRichPadMonitor.h"
#include "TLine.h"
 ****************************************************************/
StRichSinglePixel::StRichSinglePixel() {/*nopt*/}
StRichSinglePixel::StRichSinglePixel(int p, int r, int adc)
    : mPad(p), mRow(r), mAdc(adc) {}
#ifdef __ROOT__
StRichSinglePixel::~StRichSinglePixel() {/*nopt*/}

StRichDrawableTPad::StRichDrawableTPad() {/*nopt*/}
StRichDrawableTPad::StRichDrawableTPad(double xl, double yl, double xu, double yu, int adc)
    : TBox(xl,yl,xu,yu), mAdc(adc) {/* nopt */}

StRichDrawableTPad::~StRichDrawableTPad() {/*nopt*/}

void StRichDrawableTPad::draw()
{
    this->Draw();
}

void StRichDrawableTPad::setPosition(int p, int r)
{
    mRow = r;
    mPad = p;
}
#include "TPaveText.h"
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
    mRichCanvas = new TCanvas("richCanvas", "RICH Event Readouts Main Canvas",0,0,1000,700);
    mRichCanvas->Range(-70,-50,70,50);
    // make the text window
    mTextWindow = new StRichPadMonitorText();
    StRichDrawableTPad::setPadMonitorText(mTextWindow);
    //
    // make the boundaries of the detector
    mRowPitch  = mGeometryDb->rowPitch();  //.84;
    mPadPitch  = mGeometryDb->padPitch();  //.80;
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
    double xci = -mGeometryDb->quadrantX0(4)+.5*mRowPitch; //1.5;
    double zci = mGeometryDb->quadrantZ0(4)-.5*mPadPitch;  //1.5;

    cout << "xco (41.82) " << xco << endl;
    cout << "xci (1.5)   " << xco << endl;
    cout << "zco (65.9)  " << xco << endl;
    cout << "zci (1.5)   " << xco << endl;
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
    // Delete the allocated space
    for(int ii=0; ii<mAllFilledPads.GetEntries(); ii++) {
	(mAllFilledPads.At(ii))->Delete();	    
    }
  PR(mAllFilledPads.GetEntries());
  mAllFilledPads.Expand(0);
  PR(mAllFilledPads.GetEntries());
}
{
	(mAllFilledPads.At(ii))->Draw();
//     for(int ii=0; ii<mAllFilledPads.size(); ii++) {
    for(int ii=0; ii<mAllFilledPads.GetEntries(); ii++) {
	(mAllFilledPads[ii])->Draw();
    }
StRichPadMonitor::calculatePadPosition(StRichSinglePixel* pad, double* zl, double* xl, double* zu, double* xu)

void
    StRichRawCoordinate raw(pad->mPad, pad->mRow);
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
    StRichDrawableTPad* dtp = new StRichDrawableTPad(xl,yl,xu,yu,pad.mAdc);
    dtp->SetFillColor(9); // scale by ADC color
    calculatePadPosition(&pad,&xl,&yl,&xu,&yu);
    dtp->Draw();    
//     PR(mAllFilledPads.size());
    mAllFilledPads.Add(dtp);
}

    // Make it a drawable pad
    // Coordinate Transform

void StRichPadMonitor::addPad(StRichSinglePixel* pad)
{
    StRichDrawableTPad* dtp = new StRichDrawableTPad(xl,yl,xu,yu,pad->mAdc);
    calculatePadPosition(pad,&xl,&yl,&xu,&yu);
	
    dtp->SetLineColor(1);  // black

    dtp->SetFillColor(pad->mAdc); // scale by ADC color
    //dtp->SetFillStyle(0);
		bLine->Draw();
	    }
    }
}
#endif
