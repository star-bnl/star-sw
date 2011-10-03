/***************************************************************************
 *
 * $Id: MaxllBoltz.hh,v 1.1.1.1 2000/03/09 17:48:34 aihong Exp $
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
 * Revision 1.1.1.1  2000/03/09 17:48:34  aihong
 * Installation of package
 *
 **************************************************************************/


double MaxllBoltz(double *rig,double *par) {
       double  intercept = par[3];// intrcept

        if (rig[0]<intercept ) {return 0.0;} else {

       double aa; double bb;

           if(par[0]!=0.0 && par[2]!=0.0)  aa = 2.7182818*par[0]*par[2];
    if (par[2]>0.0 && (par[1]-intercept)>0.0 && log(par[1]-intercept)!=0.0) bb =
 -1.0*(log(par[2]))/log(par[1]-intercept);  
      
  double mb=aa*(pow((fabs(rig[0]) - fabs(intercept)),bb) * exp(-par[2]*pow((fabs
(rig[0]) - fabs(intercept)),bb)));

          return mb;

          }
      }

//par[0] peak height
//par[1] peak position
//par[2] width controll.  // the bigger this number, the narrower.


