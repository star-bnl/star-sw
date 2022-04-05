#include <iostream>
#include <fstream>

// detector numbers
#define BEMCMODULES 120
#define MAXDETBARREL 4
#define BTOW 1
#define BPRS 2
#define BSMDE 3
#define BSMDP 4

#define STATUS_OK 1
#define CAP1 124
#define CAP2 125

using namespace std;
TString changetime(TString time, int shift);

void bemcCalibTxtUpload(const char* gainfilename="./electron.gains.2012.final")
{
  const int ntowers=4800;
  
  gROOT->Macro("LoadLogger.C");
  gROOT->Macro("loadMuDst.C");
  gSystem->Load("StDaqLib");
  
  StBemcTablesWriter *bemctables = new StBemcTablesWriter();
  
  TString time = "20111220.000001";
  cout<<time.Data()<<endl;
  time = changetime(time,0); //get right format
  cout<<time.Data()<<endl;
  bemctables->loadTables(time.Data());
  ifstream infile(gainfilename);
  while(1){
    int id, status;
    float gain, rms;
    infile >> id >> gain >> rms >> status;
    if(!infile.good())break;
    
    bemctables->setCalib(BTOW,id,1,gain);
    bemctables->setCalibStatus(BTOW,id,(unsigned short)status);
  }
  return; //remove to write to DB
  //bemctables->writeToDb("bemcCalib",time.Data()); // Uncomment to write to DB
  infile.close(); 
}

TString changetime(TString timea, int shift)
{
  TString y,m,d,h,min,s,newtime;
  int year,month,day,hour,minute,second;
  y+=timea(0,4); cout<<"year "<<y<<endl;
  m+=timea(4,2); cout<<"month "<<m<<endl;
  d+=timea(6,2); cout<<"day "<<d<<endl;
  h+=timea(9,2); cout<<"hour "<<h<<endl;
  min+=timea(11,2); cout<<"minute "<<min<<endl;
  s+=timea(13,2); cout<<"second "<<s<<endl;

  TTimeStamp tstamp;
  tstamp.Set(y.Atoi(),m.Atoi(),d.Atoi(),h.Atoi(),min.Atoi(),s.Atoi(),0,1,shift);
  newtime += tstamp.AsString("yyyy-mm-dd HH:mm:ss");
  return newtime;
}
