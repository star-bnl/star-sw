///\file StiRootDrawable.cxx
///\author M.L. Miller (Yale Software)
///\date 06/2001

#include <iostream>
#include <math.h>
#include "TRotMatrix.h"
#include "TShape.h"
#include "TNode.h"
#include "TVolume.h"
#include "StiGui/StiRootDrawable.h"

StiRootDrawable::StiRootDrawable() 
  : mrotation(0), 
    mshape(0), 
    mnode(0), 
    mselfnode(0),
    mposition(0.,0.,0.)
{
}

///Destructor
///ROOT assumes ownership of TRotMatrix, TShape, TNode, TVolume
///so local instance are not to be destructed.
StiRootDrawable::~StiRootDrawable()
{}

void StiRootDrawable::setVisible(bool val)
{
  (val) ? mnode->SetVisibility(TVolume::kBothVisible) : mnode->SetVisibility(TVolume::kNoneVisible);
}

void StiRootDrawable::setColor(int val)
{
  //cout<<"StiRootDrawable::setColor(int val) - INFO - Started"<<endl;
  mshape->SetLineColor(val);
  mnode->SetLineColor(val);
  mselfnode->SetLineColor(val);
  //cout<<"StiRootDrawable::setColor(int val) - INFO - Done"<<endl;
}

///rotation about z-axis by angle phi
///Expect phi in radians
void gStiEulerMatrixForRoot(double phi, double* xx) 
{
    xx[0]=cos(phi);
    xx[1]=sin(phi);
    xx[2]=0.;
    xx[3]=-1.*sin(phi);
    xx[4]=cos(phi);
    xx[5]=0.;
    xx[6]=0.;
    xx[7]=0.;
    xx[8]=1.;
}
