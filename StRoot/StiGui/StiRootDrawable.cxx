//StiRootDrawable.cxx
//M.L. Miller (Yale Software)
//06/01

#include <iostream>
#include <math.h>

//Root
#include "TRotMatrix.h"
#include "TShape.h"
#include "TNode.h"

//Sti
#include "StiRootDrawable.h"

StiRootDrawable::StiRootDrawable() : mrotation(0), mshape(0), mnode(0)
{
}

StiRootDrawable::~StiRootDrawable()
{
    //ROOT thinks it owns these objects, so it does the cleanup for us
    //delete mrotation;
    //mrotation=0;
    //delete mshape;
    //mshape=0;
    //delete mnode;
    //mnode=0;
}

void StiRootDrawable::setVisibility(bool val)
{
    (val) ? mshape->SetVisibility(1) : mshape->SetVisibility(0);
    return;
}

void StiRootDrawable::setColor(int val)
{
    mshape->SetLineColor(val);
    mnode->SetLineColor(val);
    return;
}

void gStiEulerMatrixForRoot(double phi, double* xx) //rotation about z-axis by angle phi
{
    //Expect phi in radians
    xx[0]=cos(phi);
    xx[1]=sin(phi);
    xx[2]=0.;
    xx[3]=-1.*sin(phi);
    xx[4]=cos(phi);
    xx[5]=0.;
    xx[6]=0.;
    xx[7]=0.;
    xx[8]=1.;
    return;
}
