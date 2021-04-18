#ifndef SQLUTILS_H
#define SQLUTILS_H
#include "TStyle.h"
#include "TText.h"
#include "TSystem.h"
#include "TDatime.h"

unsigned long atoul(const char * field){

    char * pEnd;
    return  strtoul(field,&pEnd,0);

}


double atod(const char * field){
  char * pEnd;
  return strtod(field,&pEnd);
}

void runToDayMonth(int runday,int&month,int &day){
  // VALID FOR non leap years
  int daysInMonth[12] = {31,28,31,30,31,30,31,31,30,31,30,31};
  
  for(int i = 0; i<12 ;i++){
    
    if(runday <= daysInMonth[i]){
      month = i+1;
      day=runday;
      return;
    }
    else
      runday -= daysInMonth[i];
  }
  
  day = -999;
  month = -999;
  return;
}

int dayMonthToRunday(int month,int day){
  
  unsigned int daysInMonth[12] = {31,28,31,30,31,30,31,31,30,31,30,31};
  int runday = 0;
  
  for(int i = 1;i < month;i++)
    runday +=daysInMonth[i-1];
  
  runday += day;
  return runday;
  
}
int GMTToEastern(unsigned long & time,int runday){
  // Coverts to Eastern Time from GMT
  // returns 1 if Standard Time, 0 if Daylight Savings time
  // Using 2001 dates DST from April 1 (91) to october 28 (301)  
  // TimeOffset is 4hours for DST, 5 for EST
  
  if(runday >=91 && runday < 301){
    time -= 14400 ; // 4 hours in seconds
    return 0;
  }
  else{
    time-= 18000; // 5 hours in seconds
    return 1;
  }
  
}
int EasternToGMT(unsigned long & time,int runday){
  // Coverts to Eastern Time from GMT
  // returns 1 if Standard Time, 0 if Daylight Savings time
  // Using 2001 dates DST from April 1 (91) to october 28 (301)  
  // TimeOffset is 4hours for DST, 5 for EST
  
  if(runday >=91 && runday < 301){
    time += 14400 ; // 4 hours in seconds
    return 0;
  }
  else{
    time += 18000; // 5 hours in seconds
    return 1;
  }
  
}

int EasternToGMT(unsigned long & time,int month,int day){
  
  int runday = dayMonthToRunday(month,day);
  return EasternToGMT(time,runday);
  
}

int GMTToEastern(unsigned long & time,int month,int day){
  
  int runday = dayMonthToRunday(month,day);
  return GMTToEastern(time,runday);
  
}

void setName() {
  TText *t = new TText();
  t->SetNDC(1);
  t->SetTextFont(62);
  t->SetTextColor(1);
  t->SetTextSize(0.025);
  t->SetTextAlign(12);
  t->SetTextAngle(0);
  
  Int_t da=0;
  int month,day,year,temp;
  TDatime *dat = new TDatime(); 
  da = dat->GetDate();
  year = da/10000;
  month = da/100 - year*100;
  temp=da/100;
  day = da - temp*100; 
  char *date = new char[30];
  sprintf(date,"Jon Gans `\323# %s",dat->AsSQLString());
  t->DrawText(0.01,.01,date);
  
}

#endif
