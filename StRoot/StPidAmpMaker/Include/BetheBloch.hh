/***************************************************************************
 *
 * $Id: BetheBloch.hh,v 1.2 2000/05/01 16:59:48 aihong Exp $
 *
 * Author: Aihong Tang & Richard Witt (FORTRAN Version),Kent State U.
 *         Send questions to aihong@cnr.physics.kent.edu
 ***************************************************************************
 *
 * Description:part of StPidAmpMaker package
 *             function describing Bethe-Block curve
 ***************************************************************************
 *
 * $Log: BetheBloch.hh,v $
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


#ifndef BetheBloch_hh
#define BetheBloch_hh

double BetheBloch(double *rig,double *par) {


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

	  myDedx=calib*charge*charge*(1.0/pow(b2,prefactor))*(pow(log(myValue*gb2),0.86) - postfactor*pow(b2,1) )- mFactor;
          if (myDedx > satura) myDedx=satura;   

          return myDedx;
      };
 
#endif

  //can not declare BetheBloch as a member of StPidAmpNet.
  //coz TF1(..) ask a pointer for a function, not for a pointer for a member func.
  //a type of pointer to a function ::* is diff. from a type of pointer to a member function.(*)


