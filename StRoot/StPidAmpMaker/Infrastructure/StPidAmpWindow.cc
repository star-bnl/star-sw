/***************************************************************************
 *
 * $Id: StPidAmpWindow.cc,v 1.1.1.1 2000/03/09 17:48:35 aihong Exp $
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
      mWindows.clear();
      mWindows.push_back(cut);
}

//---------------------------
StPidAmpWindow::StPidAmpWindow(double s1, double e1, double s2, double e2){
       s1=fabs(s1); e1=fabs(e1); s2=fabs(s2); e2=fabs(e2);

      StPidAmpCut cut1("R", s1,e1);
      StPidAmpCut cut2("R", s2,e2);
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
void StPidAmpWindow::removeLastWindow(){

     if (mWindows.size()>0) mWindows.pop_back();
     else return;
}

//---------------------------
void StPidAmpWindow::removeWindow(int i){

     int j=0;
     if (i>0 && i<mWindows.size()){ 
     StPidAmpCutIter iter;
     StPidAmpCutIter tmpIter;
     
     for (iter=mWindows.begin(); iter!=mWindows.end(); iter++){
       if (j==i) mWindows.erase(iter);
         j++;
     }
     }
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

      return b;
}
//---------------------------
bool StPidAmpWindow::isInFirstWindow(double x){
      
  if (mWindows.size()>0){


      StPidAmpCutIter iter;
      StPidAmpCut theCut;
      iter=mWindows.begin();
      theCut=*iter;
      

      return theCut.isInCut(x);
 
  } else if (mWindows.size()<=0) {return false;}



}
//---------------------------
bool StPidAmpWindow::isInSecondWindow(double x){
      
  if (mWindows.size()>1){

      return mWindows[1].isInCut(x);
 
  } else if (mWindows.size()<=1) {return false;}



}

//---------------------------
bool StPidAmpWindow::isInThirdWindow(double x){
      
  if (mWindows.size()>2){

      return mWindows[2].isInCut(x);
 
  } else if (mWindows.size()<=2) {return false;}



}
//---------------------------
bool StPidAmpWindow::isInForthWindow(double x){
      
  if (mWindows.size()>3){

      return mWindows[3].isInCut(x);
 
  } else if (mWindows.size()<=3) {return false;}



}

//---------------------------
int StPidAmpWindow::getWindowIdex(double x){
    
  if ( isInFirstWindow(x))       return 1; //window index begin with 1
  else if ( isInSecondWindow(x)) return 2;
  else if ( isInThirdWindow(x))  return 3;
  else if ( isInForthWindow(x))  return 4;

  else if ( !(isInWindow(x)))    return -1;

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

      if (i<=mWindows.size()) length = mWindows[i-1].length();

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

      if (mWindows.size()>(i-1)) return mWindows[i-1].lowEdge();
      else return 0.0;
}
//---------------------------
double StPidAmpWindow::windowEnd(int i){

      if (mWindows.size()>(i-1)) return mWindows[i-1].highEdge();
      else return 0.0;
}

//--------------------------
int StPidAmpWindow::NWindows(){
     return mWindows.size();
}

 //--------------------------
StPidAmpCutVector StPidAmpWindow::cutVector(){

     return mWindows;
}

 
