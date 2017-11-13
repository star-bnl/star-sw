#include <iostream>
#include <sstream>
#include <string>
#include <time>
#include <cstdlib>
#include <math>


using namespace std;

static int startday=80;//Starting day of using physics pedestals
static int endday=365;//Last day of run( default end of year )

static int NQ = 4;
static int NL = 3;

static const int NRUN=10000;//some large number of runs for declaring arrays
static int nrun=0;//Acutal number of runs
static int ngoodrunP=0;//good run peaks point counter for the TGraph gP.  Also number of good standby runs
static int ngoodrunS=0;//good run sigma point counter for the TGraph gS.  Also number of good physics runs
static int NGOODS[NRUN];//Array that holds the run number index for the values array that corresponds to a good Standby runs
static int NGOODP[NRUN];//Array that holds the run number index for the values array that corresponds to a good Physics runs

static const int NID=252;//Maximum IDs FPS
static const int NVAL=8; //0=peak,1=sigma,2=rms,3=p95,4=p98,5=mip error,6=sigma error,7=sigma/peak
static int run[NRUN];//holds run number from sorted run list for that particular run
static int flag[NRUN];//flag for a given run (seems to only equal 1 since pedestal)
static Long64_t frun[NRUN];//run number from the file name for a given run
static Long64_t idx[NRUN];//Array that will hold sorted run numbers from 'frun'
static double values[NID][NVAL][NRUN];//Holds all the values for a given id, index 0=peak,1=sigma,2=rms,3=p95,4=p98, and run

//Function to get correct ID for FPS
int getID(int q, int l, int s)
{
  return (q-1)*3*21 + (l-1)*21 + (s-1);
}

//Function that replaces one character with another                                                                         
//Mostly used to replace ',' and '_' with spaces so stringstream can correctly assign values                                
string ReplaceChar( const string &orig, const char char1, const char char2 )
{
  string copy = orig;
  for( unsigned int i = 0; i < copy.length(); i++ )
    {
      if( copy.at(i) == char1 )
        {
          copy.at(i) = char2;
        }
    }
  return copy;
}//ReplaceChar( const string &orig, const char char1, const char char2 )

//Function to read the data from a particular runnumber.  Gets called in readall
void read(int irun, int runnum){
  //cout << "in read" << endl;
  char file[100];
  int yearday=runnum/1000;
  sprintf(file,"www/fps/%d/%d.mip.txt",yearday,runnum);
  ifstream iFile(file);
  if(!iFile.is_open()) { printf("Failed to open %s\n",file); return; }
  printf("irun=%d Reading %s\n",irun,file);
  run[irun]=runnum;
  flag[irun]=1;
  
  int nzero=0, nrms1=0;
  for(int id=0; id<NID; id++){
    int i,q,l,s;
    float peak,sigma,rms,p95,p98,miperr,sigerr;
    iFile >> i >> q >> l >> s >> peak >> sigma >> rms >> p95 >> p98 >> miperr >> sigerr;
    //printf("%d %d %d %d %d %f %f %f %f %f\n",irun,i,q,l,s,peak,sigma,rms,p95,p98);
    values[i][0][irun]=peak;  
    values[i][1][irun]=sigma; 
    values[i][2][irun]=rms;    
    values[i][3][irun]=p95;   
    values[i][4][irun]=p98;   
    values[i][5][irun]=miperr;
    values[i][6][irun]=sigerr;
    values[i][7][irun]=sigma/peak;
    if(peak<20.0) nzero++;
    if(rms<0.7) nrms1++;
  }
  iFile.close();
  printf("nzero=%4d  nrms1=%4d\n",nzero,nrms1);
  if(nzero<20){
    for(int id=0; id<NID; id++){
      for(int v=0; v<NVAL; v++){
	if(nrms1>100){
	  //cout << "Setpoint yes" << endl;
	  //gS[id][v]->SetPoint(ngoodrunS,double(ngoodrunS),values[id][v][irun]);
	  NGOODS[ngoodrunS] = irun;
	  //cout << "finished setting point" << endl;
	}else{
	  //cout << "Setpoint no" << endl;
	  //gP[id][v]->SetPoint(ngoodrunP,double(ngoodrunP),values[id][v][irun]);  
	  NGOODP[ngoodrunP] = irun;
	}
      }
    }    
    if(nrms1>100){
      ngoodrunS++;
    }else{
      ngoodrunP++;
    }
  }
}

//Function to read all the data from a particular yearday
void readall(int day){
  //cout << "in readall" << endl;
  Long64_t i=0;
  char tmp[100];
  Long64_t RunNum;
  int year=day/1000;
  int yearday=day%1000;
  //printf("%d %d %d\n",day,year,yearday);
  if(yearday!=0) {startday=yearday; endday=yearday;}
  for(int d=startday; d<=endday; d++){
    int yday=year*1000+d;
    //cout << "Yday: " << yday << endl;
    char dirname[100]; sprintf(dirname,"../www/fps/%5d/",yday);
    //printf("Directory name: %s\n",dirname);
    TSystemDirectory dir(dirname,dirname);
    TList *files = dir.GetListOfFiles();
    //cout << "in d for loop" << endl;
    if(files){
      TSystemFile *file;
      TString fname;
      TIter next(files);
      //cout << "in if(files)" << endl;
      while ((file=(TSystemFile*)next())) {
	fname = file->GetName();
	if (!file->IsDirectory() && fname.EndsWith(".mip.txt")) {
	  string filename = fname.Data();
	  string s_yearday = filename.substr(0,8);
	  stringstream s_yday(s_yearday);
	  s_yday >> RunNum;
	  s_yday.clear();
	  frun[i]=RunNum;
	  //cout << filename << " " << frun[i] << endl;
          i++;
	  //cout << "in while loop" << endl;
	}
      }
    }  
  }
  nrun=i;
  TMath::Sort(i,frun,idx,0);
  //cout << "sorted run numbers" << endl;
  cout << "Number of runs: " << nrun << endl;
  for(int j=0; j<nrun; j++)
    { 
      //cout << "Reading each file" << endl;
      //cout << "J:"<<j << "|IDX:"<<idx[j] << "|frun:"<<frun[idx[j]] << endl;
      read(j,(int)frun[idx[j]]);
    }
}

void make_ped_text_files()
{
  
  for( int k = 0; k < ngoodrunP; k++ )
    {
      stringstream FileName;
      FileName << "fps_good_physics_ped/" << frun[idx[NGOODP[k]]] << ".txt";
      ofstream out_file(FileName.str().c_str());

      for( int id=0; id < NID; id++ )
	{
	  out_file << id << " " <<  values[id][0][NGOODP[k]] << " " << values[id][1][NGOODP[k]] << endl;
	}
      out_file.close();
    }
  
  return;
}

//The quad and layer argements are so that I can run it only once or for all 0 for both means all
void fps_make_ped_files( int day=18000 )
{

  memset(values,0,sizeof(values));


  cout << "Reading all the files" << endl;
  readall(day);
  cout << "Finshed Reading" << endl;
  make_ped_text_files();
  
  return;

}

