/***************************************************************************
 *
 * $Id: StPidAmpConst.hh,v 1.1 2000/07/22 22:27:14 aihong Exp $
 *
 * Author: Aihong Tang & Richard Witt (FORTRAN Version),Kent State U.
 *         Send questions to aihong@cnr.physics.kent.edu
 ***************************************************************************
 *
 * Description:part of StPidAmpMaker package
 *             Definition of some global thing
 ***************************************************************************
 *
 * $Log: StPidAmpConst.hh,v $
 * Revision 1.1  2000/07/22 22:27:14  aihong
 * move files from StPidAmpMaker to StEventUtilities
 *
 * Revision 1.2  2000/07/12 15:38:32  aihong
 * update for real data
 *
 * Revision 1.1.1.1  2000/03/09 17:48:34  aihong
 * Installation of package
 *
 **************************************************************************/


#ifndef StPidAmpConst_hh
#define StPidAmpConst_hh

#include "SystemOfUnits.h"

#ifndef ST_NO_NAMESPACES
 using namespace units;
#endif

#define NParticleTypes    9 //number of particle type consided
#define SliceWidth        0.005
#define NPaths            11 //number of paths. SOULD BE AN ODD NUMBER!
#define BandsBegin        0.0  //begin rig
#define BandsEnd          5.0  //end rig
#define CalibFactor       2.4505e-07 //was 1.69e-07 //was 1.207e-07
#define Saturation        5.0e-4
#define NBandParam        7 //number of band parameters
#define NFitBandParam     3 //bandParam that get by fitting.
#define NAmpParam         4 //number of parameters for describing the amp. 
#define NResoParam        2 //number of parameters for linear function
#define NMaxHits          45 //max number of hits in tpc. 
#define PathHeight        1.0e-7 
#define VetexCut          2*centimeter //vetex cut for seperating e+/-
#define NBinNHits         45 //number of bins for nhits. for TH3D dependHisto
#define NBinPt            100 //..................pt......................
#define NBinX             10 //....................X....................
#define PtUpLimit         4.0 // just for dependHisto
#define XUpLimit          10 // just for dependHisto
#define NWindows4BG       4 //number of windows for mBGNet.
#define CheckMultiBGNet   0 //flag for checking how good de/dx ~ betaGamma from
                            //different particle type overlay on the same curve

#endif
