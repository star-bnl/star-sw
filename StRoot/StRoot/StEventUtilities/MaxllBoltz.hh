/***************************************************************************
 *
 * $Id: MaxllBoltz.hh,v 1.3 2003/09/02 17:58:09 perev Exp $
 *
 * Author: Aihong Tang & Richard Witt (FORTRAN Version),Kent State U.
 *         Send questions to aihong@cnr.physics.kent.edu
 ***************************************************************************
 *
 * Description:part of StPidAmpMaker package
 *             MaxllBoltz function
 ***************************************************************************
 *
 * $Log: MaxllBoltz.hh,v $
 * Revision 1.3  2003/09/02 17:58:09  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.2  2000/12/18 23:23:22  aihong
 * change formula
 *
 * Revision 1.1  2000/07/22 22:27:14  aihong
 * move files from StPidAmpMaker to StEventUtilities
 *
 * Revision 1.1.1.1  2000/03/09 17:48:34  aihong
 * Installation of package
 *
 **************************************************************************/

#ifndef MaxllBoltz_hh
#define MaxllBoltz_hh



double MaxllBoltz(double *rig,double *par) {
       double  intercept = par[3];// intrcept
       double  wd =exp(1.0/par[2]);

        if (rig[0]<intercept ) {return 0.0;} else {

       double aa; double bb;

           if(par[0]!=0.0 && wd!=0.0)  aa = 2.7182818*par[0]*wd;
    if (wd>0.0 && (par[1]-intercept)>0.0 && ::log(par[1]-intercept)!=0.0) bb =
 -1.0*(::log(wd))/::log(par[1]-intercept);  
      
  double mb=aa*(::pow((fabs(rig[0]) - fabs(intercept)),bb) * exp(-wd*::pow((fabs
(rig[0]) - fabs(intercept)),bb)));

          return mb;

          }
};

#endif


//par[0] peak height
//par[1] peak position
//par[2] width


