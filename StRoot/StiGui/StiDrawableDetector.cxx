//StiDrawableDetector.cxx
//M.L. Miller (Yale Software)
//04/01

#include <iostream>

//Root
#include "TRotMatrix.h"
#include "TShape.h"
#include "TNode.h"
#include "TBRIK.h"

//Sti
#include "StiDisplayManager.h"
#include "StiDrawableDetector.h"

void gStiEulerMatrixForRoot(double phi, double* xx); //rotation about z-axis by angle phi

StiDrawableDetector::StiDrawableDetector()
{
}

StiDrawableDetector::~StiDrawableDetector()
{
}

void StiDrawableDetector::draw()
{
    return;
}

void StiDrawableDetector::update()
{
    return;
}

const char* StiDrawableDetector::name() const
{
    return StiDetector::getName();
}

void StiDrawableDetector::makeShape()
{
    //cout <<"StiDrawableDetector::makeShape()"<<endl;

    //Make Shape
    
    //Make sure that our shape get's hung on the main node
    StiDisplayManager::instance()->cd();

    char* shapename = new char[200];
    sprintf(shapename,"Shape_%s",getName());
    mshape = new TBRIK(shapename,"BRIK","void", getThickness()/2., getHalfWidth(), getHalfDepth());
    mshape->SetLineColor(1);
    
    //Hang shape on a drawable node
    char* nodename = new char[200];
    sprintf(nodename, "node_%f_%f",getCenterRadius(), getCenterRefAngle());
    double xcenter = getCenterRadius()*cos(getCenterRefAngle());
    double ycenter = getCenterRadius()*sin(getCenterRefAngle());
    mnode = new TNode(nodename,"", mshape, xcenter, ycenter, getZCenter());
    
    //Account for rotation of object w.r.t. origin
    //cout <<"\tRotate node"<<endl;
    char* matrixname = new char[200];
    sprintf(matrixname, "matrix_%f_%f",getCenterRadius(), getCenterRefAngle());
    double x[9];    
    gStiEulerMatrixForRoot(getCenterRefAngle(), x); //Make our euler-rotatin matrix
    mrotation = new TRotMatrix(matrixname, "void", x);
    mnode->SetMatrix(mrotation);
    //cout <<"\tFinished making shape"<<endl;
    
    return;
}

void StiDrawableDetector::build(const char* buildfile)
{
    //cout <<"StiDrawableDetector::build()"<<endl;
    StiDetector::build(buildfile);
    makeShape();
    StiDisplayManager::instance()->addDrawable(this);
    //cout <<(*this)<<endl;
    return;
}
