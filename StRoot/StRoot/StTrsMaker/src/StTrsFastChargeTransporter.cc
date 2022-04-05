/**********************************************************************
 *
 * $Id: StTrsFastChargeTransporter.cc,v 1.19 2009/11/03 14:34:19 fisyak Exp $
 *
 * Author: brian June 1, 1998
 *
 **********************************************************************
 *
 * Description:  Real class to project charge segment on pad-plane
 *
 **********************************************************************
 *
 * $Log: StTrsFastChargeTransporter.cc,v $
 * Revision 1.19  2009/11/03 14:34:19  fisyak
 * Remove default in zFromTB
 *
 * Revision 1.18  2008/06/20 15:01:17  fisyak
 * move from StTrsData to StTpcRawData
 *
 * Revision 1.17  2003/09/07 03:49:06  perev
 * gcc 3.2 + WarnOff
 *
 * Revision 1.16  2003/09/02 17:59:19  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.15  2000/07/30 02:39:51  long
 * comment out diffusion calculations,they will be done in other place.
 *
 * Revision 1.14  2000/02/24 16:26:58  long
 * changes for field on cases ( calculation of diffusion as a function of field)
 *
 * Revision 1.14  2000/02/08  long
 * add sigmaL and sigmaT for all possible value of the field
 * Revision 1.13  2000/02/10 01:21:50  calderon
 * Switch to use StTpcDb.
 * Coordinates checked for consistency.
 * Fixed problems with StTrsIstream & StTrsOstream.
 *
 * Revision 1.12  1999/10/04 18:06:02  long
 *  output of z is not drift length any more due to the new definition of the coordinate system
 *
 * Revision 1.11  1999/07/13 17:46:44  lasiuk
 * diffusion
 *
 * Revision 1.10  1999/07/09 03:47:56  lasiuk
 * scale the centroid shift of charge distribution by the sqrt of the
 * number of electrons.  Original code was for single electrons ONLY!
 *
 * Revision 1.9  1999/04/07 15:05:08  lasiuk
 * typo
 *
 * Revision 1.8  1999/04/07 00:49:36  lasiuk
 * use the z offset for drift length
 *
 * Revision 1.7  1999/02/14 20:46:06  lasiuk
 * debug info
 *
 * Revision 1.6  1999/02/12 01:26:37  lasiuk
 * Limit debug output
 *
 * Revision 1.5  1999/02/10 18:02:54  lasiuk
 * debug output
 *
 * Revision 1.4  1999/01/28 02:47:03  lasiuk
 * drift length ambiguity
 *
 * Revision 1.3  1999/01/18 21:02:14  lasiuk
 * frisch grid for +/-z
 *
 * Revision 1.2  1999/01/18 10:14:26  lasiuk
 * pressure in mbar
 *
 * Revision 1.1  1998/11/10 17:12:24  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.3  1998/11/02 22:47:03  lasiuk
 * Initialization in base class
 * add attachment
 * add transparency
 *
 * Revision 1.2  1998/10/22 00:24:23  lasiuk
 * Oct 22
 *
 * Revision 1.1  1998/06/04 23:31:58  lasiuk
 * Initial Revision
 *
 **********************************************************************/
#include "StTrsFastChargeTransporter.hh"
#include <unistd.h>

StTrsChargeTransporter* StTrsFastChargeTransporter::mInstance = 0; // static data member

StTrsChargeTransporter* StTrsFastChargeTransporter::instance(StTpcGeometry* geo, StTpcSlowControl* sc, StTrsDeDx* gas, StMagneticField* mag)
{
    if(!mInstance) { // create an instance:
	mInstance = new StTrsFastChargeTransporter(geo, sc, gas, mag);
    }
    //else  do nothing

    return mInstance;
}

StTrsFastChargeTransporter::StTrsFastChargeTransporter(StTpcGeometry* geodb, StTpcSlowControl* scdb, StTrsDeDx* gasdb, StMagneticField* magdb)
    : StTrsChargeTransporter(geodb, scdb, gasdb, magdb)
{int i= fieldFactorTable(); if(i){/* nopt*/ }}

StTrsFastChargeTransporter::~StTrsFastChargeTransporter() {/* nopt */}

int StTrsFastChargeTransporter::fieldFactorTable()
{
  mB2[0]=2.143;//in KG^2
  mK[0]=0.51848343;
  mb[0]=1.0;
  mB2[1]=6.4286;//in KG^2
  mK[1]=0.34075;
  mb[1]=1.38088; 
  mB2[2]=12.14286;//in KG^2
  mK[2]=0.325;
  mb[2]=1.4821;
  mB2[3]=25.0;//in KG^2
  mK[3]=0.22222;
  mb[3]=2.7302;
  return 1;

}
void StTrsFastChargeTransporter::transportToWire(StTrsMiniChargeSegment& seg,double& SigmaL,double& SigmaT)
{ 
    double  b             =mMagDb->at(StThreeVector<double>(0,0,0)).z()/kilogauss;
            b             =b*b;//in KG^2 
    double  driftLength   =seg.position().z();//in cm
    double  drift         =driftLength;
   
    
    if( b <mB2[1])
        
         SigmaT= b<mB2[0]? mSigmaTransverse *::sqrt(drift/(mK[0]*b+mb[0])):
                         mSigmaTransverse *::sqrt(drift/(mK[1]*b+mb[1])) ;
    else
         SigmaT= b<mB2[2]? mSigmaTransverse *::sqrt(drift/(mK[2]*b+mb[2])):
                         mSigmaTransverse *::sqrt(drift/(mK[3]*b+mb[3])) ;
     SigmaL=   mSigmaLongitudinal*::sqrt(drift);
    // Projection onto pad plane
    //PR(mGeomDb->frischGrid());
    //double frischGrid = (seg.position().z() > 0) ?
    //mGeomDb->frischGrid() : -mGeomDb->frischGrid();
//     PR(frischGrid);
    // must keep time/z position of cluster! -->
    // replace z component with transit (drift) distance!
//     double driftLength = (frischGrid>0) ?
// 	frischGrid - seg.position().z() :
// 	seg.position().z() - frischGrid ;
    // With the present coordinate system, local z is now drift length.
    // MCBS Feb 2000
     // double driftLength = seg.position().z(); 
    
     

    
//     PR(driftLength);
    if (driftLength<0) {// PROBLEMS
	cout << "ERROR: StTrsFastChargeTransporter::transportToWire()" << endl;
	cout << "ERROR: Drift distance < 0" << endl;
	//continue; // Do something!!!
    }
    //
    // Diffusion to move the centroid of the charge cluster
    // while in transport.  Must be scaled by the number
    // of electrons in the charge cluster
    //
    // double ne = ::sqrt(seg.charge());
    if (mTransverseDiffusion) {
      //	seg.position().setX(mGaussDistribution.shoot(seg.position().x(),
      // 					     SigmaT/ne));
      //	seg.position().setY(mGaussDistribution.shoot(seg.position().y(),
      // 					     SigmaT/ne));
	
    } // else do not alter the position!
   
   if (mLongitudinalDiffusion) {
       
     //	seg.position().setZ(mGaussDistribution.shoot(seg.position().z(),
     //						     SigmaL/ne));
       
    }
    else {
	// must alter the position to reflect the drift length.  This information is
	// needed in order to properly treat the charge distribution onto the wire plane!
	
    }
    
    if (seg.position().y() > mGeomDb->firstOuterSectorAnodeWire())
    	seg.position().setZ(seg.position().z()+ mGeomDb->outerSectorzOffSet());
    else
       	seg.position().setZ(seg.position().z()+ mGeomDb->innerSectorzOffSet());
    //
    // Alter charge with:
    //     - absorption (O2)
    //     - wire grid transmision

    // O2 abosorption (stat)
    if(mChargeAttachment) {
	seg.setCharge(seg.charge()*chargeAttachment(driftLength));
    }
    
//     PR(seg.charge());

    // Grid Transparency (stat)
    if(mGatingGridTransparency) {
	//PR(wireGridTransmission());
	seg.setCharge(seg.charge()*wireGridTransmission());
    }

//     PR(seg.charge());
}

double StTrsFastChargeTransporter::wireGridTransmission()
{
    if(mDoTransparencyCalc)
	mTransparency = transparencyCalculation();
    return mTransparency;
}

double StTrsFastChargeTransporter::chargeAttachment(double driftL) const
{
//     PR(mAttachment);
//     PR(mO2Concentration);
//     PR(.760*bar);
//     PR(driftL);
//     PR(mSCDb->driftVelocity());
//     double suppression = 1.-mAttachment*(mO2Concentration/1.e+6)*sqr(.760*bar)*driftL/mSCDb->driftVelocity();
//     PR(suppression);

    return 1.-mAttachment*(mO2Concentration/1.e+6)*sqr(.760*bar)*driftL/mDriftVelocity; 
}
