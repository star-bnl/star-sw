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
#include "Sti/StiPlacement.h"
#include "Sti/StiShape.h"
#include "Sti/StiPlanarShape.h"
#include "Sti/StiCylindricalShape.h"
#include "Sti/StiConicalShape.h"
#include "StiRootDisplayManager.h"
#include "StiRootDrawableDetector.h"

//rotation about z-axis by angle phi
void gStiEulerMatrixForRoot(double phi, double* xx);

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

void StiRootDrawableDetector::build()
{
    //cout <<"StiRootDrawableDetector::build()"<<endl;
    StiDetector::build();
    makeShape();
    StiDrawable::setName( StiDetector::getName().c_str() );
    StiRootDisplayManager::instance()->addDrawable(this);
    //cout <<(*this)<<endl;
    return;
}

void StiRootDrawableDetector::makeShape()
{
    //cout <<"StiRootDrawableDetector::makeShape()"<<endl;

    //Make Shape
    
    //Make sure that our shape get's hung on the main node
    StiRootDisplayManager::instance()->cd();

    char* shapename = new char[200];
    sprintf(shapename,"Shape_%s",StiDetector::getName().c_str());
    // make rectangular or cylindrical shapes based on shape code
    StiPlacement *pPlacement = getPlacement();
    StiShapeCode code = getShape()->getShapeCode();
    if(code == kPlanar){
      StiPlanarShape *pPlane = 
          dynamic_cast<StiPlanarShape *>( getShape() );
      mshape = new TBRIK(shapename,"BRIK","void", 
                         pPlane->getThickness()/2.,
                         pPlane->getHalfWidth(), 
                         pPlane->getHalfDepth());
    }else if(code == kCylindrical){
      StiCylindricalShape *pCyl = 
          dynamic_cast<StiCylindricalShape *>( getShape() );
      // R00T expects these angles in degrees, of all things
      float fHalfOpening = pCyl->getOpeningAngle()/2.;
      float fStarting = pPlacement->getCenterRefAngle() - fHalfOpening;
      if(fStarting < 0) fStarting += 2*M_PI;
      float fMinRadius = pCyl->getOuterRadius() - pCyl->getThickness();
      float fMaxRadius = pCyl->getOuterRadius();

      mshape = new TPCON(shapename,"PCON","void", 
                         180./M_PI*fStarting, 360./M_PI*fHalfOpening, 2);
      ((TPCON *)mshape)->DefineSection(
          0, pPlacement->getZcenter() - pCyl->getHalfDepth(), 
          fMinRadius, fMaxRadius);
      ((TPCON *)mshape)->DefineSection(
          1, pPlacement->getZcenter() + pCyl->getHalfDepth(), 
          fMinRadius, fMaxRadius);
    }else if(code == kConical){
      StiConicalShape *pCone = 
          dynamic_cast<StiConicalShape *>( getShape() );
      // R00T expects these angles in degrees, of all things
      float fHalfOpening = pCone->getOpeningAngle()/2.;
      float fStarting = pPlacement->getCenterRefAngle() - fHalfOpening;
      if(fStarting < 0) fStarting += 2*M_PI;
      float fMinRadiusEast = pCone->getOuterRadiusEast() - 
          pCone->getThickness();
      float fMaxRadiusEast = pCone->getOuterRadiusEast();
      float fMinRadiusWest = pCone->getOuterRadiusWest() - 
          pCone->getThickness();
      float fMaxRadiusWest = pCone->getOuterRadiusWest();

      mshape = new TPCON(shapename,"PCON","void", 
                         180./M_PI*fStarting, 360./M_PI*fHalfOpening, 2);
      ((TPCON *)mshape)->DefineSection(
          0, pPlacement->getZcenter() - pCone->getHalfDepth(), 
          fMinRadiusEast, fMaxRadiusEast);
      ((TPCON *)mshape)->DefineSection(
          1, pPlacement->getZcenter() + pCone->getHalfDepth(), 
          fMinRadiusWest, fMaxRadiusWest);
    }
    mshape->SetLineColor(1);
    

    //Hang shape on a drawable node in local coordinates of shape
    //cout <<"Make Local Volume"<<endl;
    char* localnodename = new char[200];
    sprintf(localnodename, "local_node_%f_%f",
            getPlacement()->getCenterRadius(), 
            getPlacement()->getCenterRefAngle());
    mselfnode = new TVolume(localnodename,"", mshape);

    //Now hand shape on node that is rotated w.r.t. global coordinates
    //cout <<"Rotate shape w.r.t. local center"<<endl;
    char* localmatrixname = new char[200];
    sprintf(localmatrixname, "local_matrix_%f_%f",
            getPlacement()->getCenterRadius(), 
            getPlacement()->getCenterRefAngle());
    double xlocal[9];    
    //Make our euler-rotatin matrix
    gStiEulerMatrixForRoot(getPlacement()->getCenterOrientation(), xlocal); 
    mselfrotation = new TRotMatrix(localmatrixname, "void", xlocal);

    //cout <<"Make Global Node"<<endl;
    char* nodename = new char[200];
    sprintf(nodename, "node_%f_%f", getPlacement()->getCenterRadius(), 
            getPlacement()->getCenterRefAngle());
    mnode = new TVolume();
    mnode->SetName(nodename);
    mnode->SetTitle(nodename);
    mnode->Add(mselfnode, 0., 0., 0., mselfrotation);

    //cout <<"\tRotate node w.r.t global"<<endl;
    char* matrixname = new char[200];
    sprintf(matrixname, "matrix_%f_%f", getPlacement()->getCenterRadius(), 
            getPlacement()->getCenterRefAngle());
    double x[9];    
    //Make our euler-rotatin matrix
    if(getShape()->getShapeCode() == kPlanar){ // normal rotation
      gStiEulerMatrixForRoot(getPlacement()->getCenterRefAngle(), x); 
      mrotation = new TRotMatrix(matrixname, "void", x);
    }else{ // no rotation
      gStiEulerMatrixForRoot(0., x); 
      mrotation = new TRotMatrix(matrixname, "void", x);
    }

    //Set position of center of shape w.r.t. global coordinates
    //cout <<"Set Position of center of shape w.r.t. global"<<endl;
    if(getShape()->getShapeCode() == kPlanar){
      double xcenter = getPlacement()->getCenterRadius()*
          cos(getPlacement()->getCenterRefAngle());
      double ycenter = getPlacement()->getCenterRadius()*
          sin(getPlacement()->getCenterRefAngle());
      mposition.setX(xcenter);
      mposition.setY(ycenter);
      mposition.setZ(getPlacement()->getZcenter());
    }else{ // for kCylindrical, we assume center @ origin
      mposition.setX(0.);
      mposition.setY(0.);
      mposition.setZ(0.);
    }

    //cout <<"Done Making Shape"<<endl;
    return;
}

