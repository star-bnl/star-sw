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
    mshape = new TBRIK(shapename,"BRIK","void", getThickness()/2., getHalfWidth(), getHalfDepth());
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
    double xcenter = getCenterRadius()*cos(getCenterRefAngle());
    double ycenter = getCenterRadius()*sin(getCenterRefAngle());
    mposition.setX(xcenter);
    mposition.setY(ycenter);
    mposition.setZ(getZCenter());

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
