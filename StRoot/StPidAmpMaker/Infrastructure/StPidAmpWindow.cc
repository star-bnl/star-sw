/***************************************************************************
 *
 * $Id: StPidAmpWindow.cc,v 1.5 2000/05/01 16:59:26 aihong Exp $
 *
 * Author: Aihong Tang & Richard Witt (FORTRAN Version),Kent State U.
 *         Send questions to aihong@cnr.physics.kent.edu
 ***************************************************************************
 *
 * Description:part of StPidAmpMaker package
 *             StPidAmpWindow define "making sense" region for fitting
 ***************************************************************************
 *
 * $Log: StPidAmpWindow.cc,v $
 * Revision 1.5  2000/05/01 16:59:26  aihong
 * clean up
 *
 * Revision 1.4  2000/04/11 15:36:20  aihong
 * add the implementation of NScreens()
 *
 * Revision 1.3  2000/04/09 16:18:23  aihong
 * add screen stuff
 *
 * Revision 1.2  2000/03/30 20:23:44  aihong
 * Modified getWindowIdex() & isIn*Window()
 *
 * Revision 1.1.1.1  2000/03/09 17:48:35  aihong
 * Installation of package
 *
 **************************************************************************/


#include "math.h"

#include "StPidAmpMaker/Infrastructure/StPidAmpWindow.hh"
#include "StPidAmpMaker/Infrastructure/StPidAmpCut.hh"

//---------------------------
StPidAmpWindow::StPidAmpWindow(){
  mWindows.clear();
  mScreens.clear();
}

//---------------------------
StPidAmpWindow::~StPidAmpWindow(){
  /* no-op */
}

//---------------------------
StPidAmpWindow::StPidAmpWindow(double s1, double e1){

        //all numbers for window should be positive.

      s1=fabs(s1);     e1=fabs(e1);
      StPidAmpCut cut("R", s1,e1);
      mScreens.clear();
      mWindows.clear();
      mWindows.push_back(cut);
}

//---------------------------
StPidAmpWindow::StPidAmpWindow(double s1, double e1, double s2, double e2){
       s1=fabs(s1); e1=fabs(e1); s2=fabs(s2); e2=fabs(e2);

      StPidAmpCut cut1("R", s1,e1);
      StPidAmpCut cut2("R", s2,e2);
       mScreens.clear();
       mWindows.clear();
       mWindows.push_back(cut1);
       mWindows.push_back(cut2);
}
     

//---------------------------
StPidAmpWindow::StPidAmpWindow(double s1, double e1, double s2, double e2, double s3, double e3){
  s1=fabs(s1); e1=fabs(e1); s2=fabs(s2); e2=fabs(e2); s3=fabs(s3); e3=fabs(e3);

       StPidAmpCut cut1("R", s1,e1);
       StPidAmpCut cut2("R", s2,e2);
       StPidAmpCut cut3("R", s3,e3);

       mScreens.clear();

       mWindows.clear();
       mWindows.push_back(cut1);
       mWindows.push_back(cut2);
       mWindows.push_back(cut3);
}
        
//---------------------------
StPidAmpWindow::StPidAmpWindow(double s1, double e1, double s2, double e2, double s3, double e3, double s4, double e4){
  s1=fabs(s1); e1=fabs(e1); s2=fabs(s2); e2=fabs(e2); s3=fabs(s3); e3=fabs(e3);
  s4=fabs(s4); e4=fabs(e4);

       StPidAmpCut cut1("R", s1,e1);
       StPidAmpCut cut2("R", s2,e2);
       StPidAmpCut cut3("R", s3,e3);
       StPidAmpCut cut4("R", s4,e4);
     
       mScreens.clear();

       mWindows.clear();
       mWindows.push_back(cut1);
       mWindows.push_back(cut2);
       mWindows.push_back(cut3);
       mWindows.push_back(cut4);
}



//---------------------------
void StPidAmpWindow::addWindow(double s1, double e1){
      s1=fabs(s1);     e1=fabs(e1);
      StPidAmpCut cut("R", s1,e1);
      mWindows.push_back(cut);
}

//---------------------------
void StPidAmpWindow::addScreen(double s1, double e1){
      s1=fabs(s1);     e1=fabs(e1);
      StPidAmpCut cut("R", s1,e1);
      mScreens.push_back(cut);
}


//---------------------------
void StPidAmpWindow::removeLastWindow(){

     if (mWindows.size()>0) mWindows.pop_back();
     else return;
}
//---------------------------
void StPidAmpWindow::removeLastScreen(){

     if (mScreens.size()>0) mScreens.pop_back();
     else return;
}

//---------------------------
void StPidAmpWindow::removeWindow(int i){

     int j=0;
     if (i>0 && i<int(mWindows.size())){ 
     StPidAmpCutIter iter;
     
     for (iter=mWindows.begin(); iter!=mWindows.end(); iter++){
       if (j==i) mWindows.erase(iter);
         j++;
     }
     }
}
//---------------------------
void StPidAmpWindow::removeScreen(int i){

     int j=0;
     if (i>0 && i<int(mScreens.size())){ 
     StPidAmpCutIter iter;
     
     for (iter=mScreens.begin(); iter!=mScreens.end(); iter++){
       if (j==i) mScreens.erase(iter);
         j++;
     }
     }
}
//---------------------------
void StPidAmpWindow::removeAllScreens(){
  mScreens.clear();
}


//---------------------------
bool StPidAmpWindow::isInWindow(double x){
      
      bool b=false;

      StPidAmpCutIter iter;
      StPidAmpCut theCut;
      for (iter=mWindows.begin(); iter!=mWindows.end(); iter++){
      theCut=*iter;
      b= b || (theCut.isInCut(x));
      }
   
      b = b && (!isInScreen(x));

      return b;
}

//---------------------------
bool StPidAmpWindow::isInScreen(double x){
      
      bool b=false;

      StPidAmpCutIter iter;
      StPidAmpCut theCut;
      for (iter=mScreens.begin(); iter!=mScreens.end(); iter++){
      theCut=*iter;
      b= b || (theCut.isInCut(x));
      }

      return b;
}
//---------------------------
bool StPidAmpWindow::isInFirstWindow(double x){

      bool b=false;      

  if (mWindows.size()>0){

      StPidAmpCutIter iter;
      StPidAmpCut theCut;
      iter=mWindows.begin();
      theCut=*iter;
      

      b=theCut.isInCut(x);
 
      //  } else if (mWindows.size()<=0) {return false;}
  } else  b=false;

      b = b && (!isInScreen(x));
  
      return b;


}
//---------------------------
bool StPidAmpWindow::isInSecondWindow(double x){

      bool b=false;      
      
  if (mWindows.size()>1){

      b=mWindows[1].isInCut(x);
 
  } else b=false;

      b = b && (!isInScreen(x));
      return b;

}

//---------------------------
bool StPidAmpWindow::isInThirdWindow(double x){

  bool b=false;
      
  if (mWindows.size()>2){

      b=mWindows[2].isInCut(x);
 
  } else b=false;

      b = b && (!isInScreen(x));
      return b;

}
//---------------------------
bool StPidAmpWindow::isInForthWindow(double x){

  bool b=false;      

  if (mWindows.size()>3){

      b=mWindows[3].isInCut(x);
 
  } else b=false;

      b = b && (!isInScreen(x));
      return b;

}

//---------------------------
int StPidAmpWindow::getWindowIdex(double x){
    
  if ( isInFirstWindow(x))       return 1; //window index begin with 1
  else if ( isInSecondWindow(x)) return 2;
  else if ( isInThirdWindow(x))  return 3;
  else if ( isInForthWindow(x))  return 4;

  else                           return -1;

}

//---------------------------
double StPidAmpWindow::totalLength(){

       double length=0.0;
   
      StPidAmpCutIter iter;
      StPidAmpCut theCut;
      for (iter=mWindows.begin(); iter!=mWindows.end(); iter++){
      theCut=*iter;
      length= length + theCut.length();
      }

      return length;
}


//---------------------------
double StPidAmpWindow::length(int i){
    
      double length=0.0;

      if (i<=int(mWindows.size())) length = mWindows[i-1].length();

      return length;
}




//---------------------------
double StPidAmpWindow::firstWindowBegin(){

      if (mWindows.size()>0) return mWindows[0].lowEdge();
      else return 0.0;
}
//---------------------------
double StPidAmpWindow::firstWindowEnd(){

      if (mWindows.size()>0) return mWindows[0].highEdge();
      else return 0.0;
}
//---------------------------
double StPidAmpWindow::secondWindowBegin(){

      if (mWindows.size()>1) return mWindows[1].lowEdge();
      else return 0.0;
}
//---------------------------
double StPidAmpWindow::secondWindowEnd(){

      if (mWindows.size()>1) return mWindows[1].highEdge();
      else return 0.0;
}
//---------------------------
double StPidAmpWindow::thirdWindowBegin(){

      if (mWindows.size()>2) return mWindows[2].lowEdge();
      else return 0.0;
}
//---------------------------
double StPidAmpWindow::thirdWindowEnd(){

      if (mWindows.size()>2) return mWindows[2].highEdge();
      else return 0.0;
}
//---------------------------
double StPidAmpWindow::forthWindowBegin(){

      if (mWindows.size()>3) return mWindows[3].lowEdge();
      else return 0.0;
}
//---------------------------
double StPidAmpWindow::forthWindowEnd(){

      if (mWindows.size()>3) return mWindows[3].highEdge();
      else return 0.0;
}
//---------------------------
double StPidAmpWindow::windowBegin(int i){

      if (int(mWindows.size())>(i-1)) return mWindows[i-1].lowEdge();
      else return 0.0;
}
//---------------------------
double StPidAmpWindow::windowEnd(int i){

      if (int(mWindows.size())>(i-1)) return mWindows[i-1].highEdge();
      else return 0.0;
}

//--------------------------
int StPidAmpWindow::NWindows(){
     return mWindows.size();
}
//--------------------------
int StPidAmpWindow::NScreens(){
      return mScreens.size();
}


//--------------------------
StPidAmpCutVector StPidAmpWindow::cutVector(){

     return mWindows;
}
 //--------------------------
StPidAmpCutVector StPidAmpWindow::screenVector(){

     return mScreens;
}

 
