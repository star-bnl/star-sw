#include "readTextFile.h"
   
/*
const char* textfile="links/P00hk.nofield.refitOS.undoPTCME.slice/dip5.typec.slice.txt";
const char* outfile="links/P00hk.nofield.refitOS.undoPTCME.slice/dip5.typec.slice.new.root";
*/

const char* textfile="REAL/finish_central_cut1.hist.txt";
const char* outfile="REAL/finish_central_cut1_new.hist.root";

int debug=0;
int main(int argc,char** argv)
{

  char* argvZ[] = {"-b"}; // batch mode, no gui
  Int_t argcZ = 1;
  TApplication r00t("r00t", &argcZ, argvZ);

  extern char* optarg;
  const char* options = "i:o:";
  Int_t chr, i=0,o=0;
  char textFile[300],outFile[300];
  while ((chr = getopt(argc, argv, options)) >= 0){
    switch(chr){
    case 'i': strcpy(textFile,optarg); i=1; break;
    case 'o': strcpy(outFile,optarg); o=1; break;
    }
  }
  if(!(i*o)){
    cout << "-i infile -o outfile" << endl;
    exit(-1);
  }
  
  cout << "reading textFile=" << textFile << endl;
  cout << "writing to " << outFile << endl;

  TFile outRoot(outFile,"RECREATE");
  if(!outRoot.IsOpen()) { 
	cout << "Cannot open " << outFile << endl; return -1;
  }
  ifstream iss(textFile);
  if(!iss) {
	cout << "Cannot open " << textFile << endl;
	return -1;
  }
  int count(0),limit(1000);
  TString buffer; char name[100],title[100];
  const int bufSz=100;
  char* buf=new char[bufSz];
  bool isarray=false,isnew=false;
  //double array[300];
  int nBin[3]={0},dim(0),iBin=0;
  float min[3]={0},max[3]={0};
  TH1* h=0;
  while(!iss.eof()){
   
    iss.getline(buf,bufSz-1);

    //    if(debug)cout << "the line is**" << buf << "**" << endl;

    if(strstr(buf,"name:")){
      buffer=buf;
      parse(buffer,"name:");
      strcpy(name,buffer.Data());
      isnew=true;
      if(h){ 
	iBin=0; 
	for(int i=0; i<3;i++){
	  nBin[i]=0; min[i]=0; max[i]=0;
	}
	int stat = h->Write(); 
	if(!stat) {
	  cout << "could not write to " << outFile << endl;
	}
	delete h; 
      }
      continue;
    }
    if(strstr(buf,"title:")){
      buffer=buf;
      parse(buffer,"title:");
      strcpy(title,buffer.Data());
      continue;
    }
    if(strstr(buf,"dim:")){
      buffer=buf;
      parse(buffer,"dim:");
      dim=atoi(buffer);
      continue;
    }
    if(strstr(buf,"ary:")){
      buffer=buf;
      parse(buffer,"ary:");
      isarray=atoi(buffer);
      continue;
    }
    if(strstr(buf,"bins:")){
      buffer=buf;
      parse(buffer,"bins:");
      nBin[iBin++]=atoi(buffer.Data());
      continue;
    }
    
    if(strstr(buf,"min:")){
      buffer=buf;
      parse(buffer,"min:");
      min[iBin-1]=atof(buffer.Data());
      continue;
    }
    if(strstr(buf,"max:")){
      buffer=buf;
      parse(buffer,"max:");
      max[iBin-1]=atof(buffer.Data());
      continue;
    }
    if(strstr(buf,"bin")){
      if(isnew){
	isnew=false;
	if(debug)
	  cout << "name=" <<name << " title=" << title 
	       << " dim=" << dim << endl;
	if(!isarray){
	  switch(dim){
	  case 1: h=new TH1D(name,title,nBin[0],min[0],max[0]); break;
	  case 2: h=new TH2D(name,title,
			     nBin[0],min[0],max[0],
			     nBin[1],min[1],max[1]); break;
	  case 3: h=new TH3D(name,title,
			     nBin[0],min[0],max[0],
			     nBin[1],min[1],max[1],
			     nBin[2],min[2],max[2]); break;
	  default: cout << "Wrong dim=" << dim << endl; exit(1);
	  }
	  for(int i=0; i<dim; i++){
	    cout << "nbin=" << nBin[i] << ",min="<<min[i] << ",max=" << max[i] << endl;
	  }
	}
	else{
	  cout << "Cannot handle array bins" << endl; exit(1);
	}
      }
      //TString temp,temp2;

      buffer=buf;
      buffer.ReplaceAll("bin=","");
      strcpy(buf,buffer.Data());
      
      char* a = strtok(buf," ");
      int bin=atoi(a);
      a=strtok(NULL," ");
      double value=atof(a);
      a=strtok(NULL," ");
      double error=atof(a);


      if(debug)
	cout << "bin=" << bin << " value=" << value 
	     << " error=" << error<<endl;

      
      if(!h){cout << "h not created?" << endl; return -1;}
      h->SetBinContent(bin,value);
      h->SetBinError(bin,error);

    }

    //    if(debug && ++count>limit) break;
    
  } // while
  // write the last histogram
  h->Write();

  cout << "done" << endl;
  outRoot.Close();

  return 0;
}

void parse(TString& a,char* b){
  a.ReplaceAll(b,""); a.ReplaceAll(" ","");
}
 


TString split(TString& a){
  TString temp=a.Data();
  temp.Replace(temp.First(' '),temp.Length(),"");
  a.Replace(0,a.First(' ')+1,"");
  return temp;
}

void removeLead(TString& a){
  while(a.First(' ')==0){
    a.Remove(0,1);
  }
}

void removeTrail(TString& a){
  a.Remove(a.First(' '));
}
