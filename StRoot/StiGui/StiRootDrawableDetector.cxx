//StiRootDrawableDetector.cxx
//M.L. Miller (Yale Software)
//04/01

#include <iostream>

//Root
#include "TRotMatrix.h"
#include "TShape.h"
#include "TNode.h"
#include "TVolume.h"
#include "TBRIK.h"
#include "TPCON.h"

//Sti
#include "StiDisplayManager.h"
#include "StiRootDrawableDetector.h"

void gStiEulerMatrixForRoot(double phi, double* xx); //rotation about z-axis by angle phi

StiRootDrawableDetector::StiRootDrawableDetector()
{
}

StiRootDrawableDetector::~StiRootDrawableDetector()
{
}

void StiRootDrawableDetector::draw()
{
    return;
}

void StiRootDrawableDetector::update()
{
    return;
}

const char* StiRootDrawableDetector::name() const
{
    return StiDetector::getName();
}

void StiRootDrawableDetector::makeShape()
{
    //cout <<"StiRootDrawableDetector::makeShape()"<<endl;

    //Make Shape
    
    //Make sure that our shape get's hung on the main node
    StiDisplayManager::instance()->cd();

    char* shapename = new char[200];
    sprintf(shapename,"Shape_%s",getName());
    // make rectangular or cylindrical shapes based on shape code
    if(getShapeCode() == kPlanar){
      mshape = new TBRIK(shapename,"BRIK","void", getThickness()/2., getHalfWidth(), getHalfDepth());
    }else{ // kCylindrical
      // R00T expects this angle in degrees, of all things
      double dHalfDeltaPhi = 180.*getHalfWidth()/getCenterRadius()/M_PI;
      double dMinRadius = getCenterRadius() - getThickness()/2;
      double dMaxRadius = getCenterRadius() + getThickness()/2;
      TPCON *pCon = new TPCON(shapename,"PCON","void", 
                              getCenterRefAngle() - dHalfDeltaPhi,
                              2.*dHalfDeltaPhi, 2);
      pCon->DefineSection(0, -getHalfDepth(), dMinRadius, dMaxRadius);
      pCon->DefineSection(1, getHalfDepth(), dMinRadius, dMaxRadius);

      mshape = pCon;
      pCon = 0;
    }
    mshape->SetLineColor(1);
    

    //Hang shape on a drawable node in local coordinates of shape
    //cout <<"Make Local Volume"<<endl;
    char* localnodename = new char[200];
    sprintf(localnodename, "local_node_%f_%f",getCenterRadius(), getCenterRefAngle());
    mselfnode = new TVolume(localnodename,"", mshape);

    //Now hand shape on node that is rotated w.r.t. global coordinates
    //cout <<"Rotate shape w.r.t. local center"<<endl;
    char* localmatrixname = new char[200];
    sprintf(localmatrixname, "local_matrix_%f_%f",getCenterRadius(), getCenterRefAngle());
    double xlocal[9];    
    gStiEulerMatrixForRoot(getOrientationAngle(), xlocal); //Make our euler-rotatin matrix
    mselfrotation = new TRotMatrix(localmatrixname, "void", xlocal);

    //cout <<"Make Global Node"<<endl;
    char* nodename = new char[200];
    sprintf(nodename, "node_%f_%f",getCenterRadius(), getCenterRefAngle());
    mnode = new TVolume();
    mnode->SetName(nodename);
    mnode->SetTitle(nodename);
    mnode->Add(mselfnode, 0., 0., 0., mselfrotation);

    //cout <<"\tRotate node w.r.t global"<<endl;
    char* matrixname = new char[200];
    sprintf(matrixname, "matrix_%f_%f",getCenterRadius(), getCenterRefAngle());
    double x[9];    
    gStiEulerMatrixForRoot(getCenterRefAngle(), x); //Make our euler-rotatin matrix
    mrotation = new TRotMatrix(matrixname, "void", x);

    //Set position of center of shape w.r.t. global coordinates
    //cout <<"Set Position of center of shape w.r.t. global"<<endl;
    if(getShapeCode() == kPlanar){
      double xcenter = getCenterRadius()*cos(getCenterRefAngle());
      double ycenter = getCenterRadius()*sin(getCenterRefAngle());
      mposition.setX(xcenter);
      mposition.setY(ycenter);
      mposition.setZ(getZCenter());
    }else{ // for kCylindrical, we assume center @ origin
      mposition.setX(0);
      mposition.setY(0);
      mposition.setZ(0);
    }

    //cout <<"Done Making Shape"<<endl;
    return;
}

void StiRootDrawableDetector::build(const char* buildfile)
{
    //cout <<"StiRootDrawableDetector::build()"<<endl;
    StiDetector::build(buildfile);
    makeShape();
    StiDisplayManager::instance()->addDrawable(this);
    //cout <<(*this)<<endl;
    return;
}
