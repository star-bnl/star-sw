/***************************************************************************
 *
 * $Id: Linear.hh,v 1.1 2000/07/22 22:27:14 aihong Exp $
 *
 * Author: Aihong Tang & Richard Witt (FORTRAN Version),Kent State U.
 *         Send questions to aihong@cnr.physics.kent.edu
 ***************************************************************************
 *
 * Description:part of StPidAmpMaker package
 *             Linear function for describing resolution
 ***************************************************************************
 *
 * $Log: Linear.hh,v $
 * Revision 1.1  2000/07/22 22:27:14  aihong
 * move files from StPidAmpMaker to StEventUtilities
 *
 * Revision 1.1.1.1  2000/03/09 17:48:34  aihong
 * Installation of package
 *
 **************************************************************************/


#ifndef StPidAmpLinear_hh
#define StPidAmpLinear_hh

//-------------------------------------------------------------------
double Linear(double *dedxmean,double *par) {

 

          double lr = par[0]+par[1]*dedxmean[0];

          return lr;

       };

#endif
