/***************************************************************************
 *
 * $Id: StPidAmpWindow.hh,v 1.3 2000/05/01 16:59:26 aihong Exp $
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
 * Revision 1.3  2000/05/01 16:59:26  aihong
 * clean up
 *
 * Revision 1.2  2000/04/09 16:18:23  aihong
 * add screen stuff
 *
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
      virtual ~StPidAmpWindow();

      void addWindow(double s1, double e1); 
      void addScreen(double s1, double e1);

      void removeLastWindow();
      void removeLastScreen();
      void removeWindow(int i);
      void removeScreen(int i);
      void removeAllScreens();

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
      int    NScreens(); //number of screens
     
      bool isInWindow(double x); //see whether x is in  windows
      bool isInScreen(double x);
      bool isInFirstWindow(double x);
      bool isInSecondWindow(double x);
      bool isInThirdWindow(double x);
      bool isInForthWindow(double x);
      int  getWindowIdex(double x); 

      double totalLength();//get the sum of window lengths
      double length(int i);//length of the ith window     

      StPidAmpCutVector cutVector();
      StPidAmpCutVector screenVector();

private:



      StPidAmpCutVector mWindows;
      StPidAmpCutVector mScreens;


};


#endif

//mScreens can screen out the active regeon defined by mWindows.
//such screen out regeon can span over multiple windows.
//the purpose of introducing screen is for band fitting 
//coz we need to screen out muon regeon when fitting BetheBlock,
//but we do not want to screen out muons when fitting amp. at the moment.
//see StPidAmpNet::fillSlices().
