/***************************************************************************
 *
 * $Id: BetheBlochFunction.hh,v 1.5 2007/07/12 19:35:07 fisyak Exp $
 *
 * Author: Aihong Tang & Richard Witt (FORTRAN Version),Kent State U.
 *         Send questions to aihong@cnr.physics.kent.edu
 ***************************************************************************
 *
 * Description:part of StPidAmpMaker package
 *             function describing Bethe-Block curve
 ***************************************************************************
 *
 * $Log: BetheBlochFunction.hh,v $
 * Revision 1.5  2007/07/12 19:35:07  fisyak
 * Change includes for ROOT 5.16
 *
 * Revision 1.4  2005/11/01 18:38:37  aihong
 * switch to functions from TMath
 *
 * Revision 1.3  2003/10/25 00:35:33  perev
 * Defence against non physical region added
 *
 * Revision 1.2  2003/09/02 17:58:09  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.1  2000/07/22 22:27:14  aihong
 * move files from StPidAmpMaker to StEventUtilities
 *
 * Revision 1.1  2000/07/12 15:38:32  aihong
 * update for real data
 *
 * Revision 1.2  2000/05/01 16:59:48  aihong
 * clean up
 *
 * Revision 1.1  2000/04/14 15:52:07  aihong
 * change name from BetheBlock to BetheBloch
 *
 * Revision 1.1.1.1  2000/03/09 17:48:34  aihong
 * Installation of package
 *
 **************************************************************************/
//This is identical to BetheBloch.hh
//change the name to BetheBlochFunction to avoid name corruption from StarClassLibary

#ifndef BetheBlochFunction_hh
#define BetheBlochFunction_hh
#include "TMath.h"
double BetheBlochFunction(double *rig,double *par) {


         double charge=double(fabs(par[3]));
         double m=double(par[4]);//mass
         double calib=par[5];//calibFactor
         double satura=par[6];//saturation

          double prefactor=par[0];
          double postfactor=par[1];
          double mFactor=par[2];
          double myValue;
          double b2, gb2;
          double myDedx;



          myValue =5059.0;


          gb2=(rig[0]*charge/m)*(rig[0]*charge/m);
        
          if (gb2 > 0.0) b2=1.0/(1.0 + 1.0/gb2);
          else return 0.0;
          if (myValue*gb2<=1.) return 0; //VP non physical region
	  myDedx=calib*charge*charge*(1.0/TMath::Power(b2,prefactor))*(TMath::Power(TMath::Log(myValue*gb2),0.7) - postfactor*b2 )- mFactor;
          if (myDedx > satura) myDedx=satura;   

          return myDedx;
      };
 
#endif

  //can not declare BetheBloch as a member of StPidAmpNet.
  //coz TF1(..) ask a pointer for a function, not for a pointer for a member func.
  //a type of pointer to a function ::* is diff. from a type of pointer to a member function.(*)


