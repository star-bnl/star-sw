//StiDrawableDetector.cxx
//M.L. Miller (Yale Software)
//04/01

#include <iostream>

//Root
#include "TRotMatrix.h"
#include "TShape.h"
#include "TNode.h"
#include "TBRIK.h"

//StiMaker
#include "StiMaker/StiDisplayManager.h"

//Sti
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

    //cout <<"\tMake shape"<<endl;
    char* shapename = new char[200];
    sprintf(shapename,"Shape_%s",getName());
    //cout <<"\tshapename: "<<shapename<<endl;
    Double_t dWidth = yMax - yMin;
    Double_t dYshift = (yMax + yMin)/2.; // how much yCenter differs from 0.
    mshape = new TBRIK(shapename,"BRIK","void", thickness/2., dWidth/2., 
                       halfDepth);
    mshape->SetLineColor(1);
    
    //Hang shape on a drawable node
    //cout <<"Make node"<<endl;
    char* nodename = new char[200];
    sprintf(nodename, "node_%f_%f",position, refAngle);
    double xcenter = position*cos(refAngle) - dYshift*sin(refAngle); 
    double ycenter = position*sin(refAngle) + dYshift*cos(refAngle);
    mnode = new TNode("nodename","", mshape, xcenter, ycenter, zCenter);
    
    //Account for rotation of object w.r.t. origin
    //cout <<"\tRotate node"<<endl;
    char* matrixname = new char[200];
    sprintf(matrixname, "matrix_%f_%f",position, refAngle);
    double x[9];    
    gStiEulerMatrixForRoot(refAngle, x); //Make our euler-rotatin matrix
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
