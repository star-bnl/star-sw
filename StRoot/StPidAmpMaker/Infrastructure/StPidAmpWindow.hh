/***************************************************************************
 *
 * $Id: StPidAmpWindow.hh,v 1.1.1.1 2000/03/09 17:48:34 aihong Exp $
 *
 * Author: Aihong Tang & Richard Witt (FORTRAN Version),Kent State U.
 *         Send questions to aihong@cnr.physics.kent.edu
 ***************************************************************************
 *
 * Description:part of StPidAmpMaker package
 *             StPidAmpWindow define "making sense" region for fitting
 *             
 ***************************************************************************
 *
 * $Log: StPidAmpWindow.hh,v $
 * Revision 1.1.1.1  2000/03/09 17:48:34  aihong
 * Installation of package
 *
 **************************************************************************/


#ifndef StPidAmpWindow_hh
#define StPidAmpWindow_hh

#include "StPidAmpMaker/Infrastructure/StPidAmpCutVector.hh"




class StPidAmpWindow {

public:

      StPidAmpWindow();
      StPidAmpWindow(double s1, double e1); 
      StPidAmpWindow(double s1, double e1, double s2, double e2);
      StPidAmpWindow(double s1, double e1, double s2, double e2, double s3, double e3);
      StPidAmpWindow(double s1, double e1, double s2, double e2, double s3, double e3, double s4, double e4);
      ~StPidAmpWindow();

      void addWindow(double s1, double e1); 

      void removeLastWindow();
      void removeWindow(int i);

      double firstWindowBegin();
      double firstWindowEnd();

      double secondWindowBegin();
      double secondWindowEnd();

      double thirdWindowBegin();
      double thirdWindowEnd();

      double forthWindowBegin();
      double forthWindowEnd();

      double windowBegin(int i);
      double windowEnd(int i);

      int    NWindows();//number of windows

     
      bool isInWindow(double x); //see whether x is in  windows
      bool isInFirstWindow(double x);
      bool isInSecondWindow(double x);
      bool isInThirdWindow(double x);
      bool isInForthWindow(double x);
      int  getWindowIdex(double x); 

      double totalLength();//get the sum of window lengths
      double length(int i);//length of the ith window     

      StPidAmpCutVector cutVector();


private:



      StPidAmpCutVector mWindows;

};


#endif
