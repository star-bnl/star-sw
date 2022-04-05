#include <fstream>

const char* textfile="links/P00hk.nofield.refitOS.undoPTCME.slice/dip5.typec.slice.txt";
const char* outfile="links/P00hk.nofield.refitOS.undoPTCME.slice/dip5.typec.slice.new.root";

int debug=0;
void readTextFile(const char* textFile=textfile,
		  const char* outFile=outfile)
{
  cout << "reading textFile=" << textFile << endl;
  cout << "writing " << outFile << endl;

  TFile outRoot(outFile,"RECREATE");
  if(!outRoot.IsOpen()) return;

  ifstream iss(textFile);
  if(!iss) return;

  int count(0),limit(1000);
  TString buffer; char name[100],title[100],buf[500];
  bool isarray=false,new=false;
  double array[300];
  int nBin(0),min(0),max(0);
  TH1D* h=0;
  while(!iss.eof()){
   
    iss.getline(buf,500);    
    buffer=buf;

    if(debug)cout << "the line is**" << buf << "**" << endl;

    if(buffer.Contains("name:")){
      parse(buffer,"name:");
      strcpy(name,buffer.Data());
      new=true;
      if(h){ 
	int stat = h->Write(); 
	if(!stat) {
	  cout << "could not write to " << outFile << endl;
	}
	delete h; 
      }
      continue;
    }
    if(buffer.Contains("title:")){
      parse(buffer,"title:");
      strcpy(title,buffer.Data());
      continue;
    }
    if(buffer.Contains("bins:")){
      parse(buffer,"bins:");
      nBin=atoi(buffer.Data());
      continue;
    }
    if(buffer.Contains("isarray")){
      isarray=true; 
      iss.getline(buf,500);
      buffer=buf;
      if(debug)cout << "**" << buffer << "**" << endl;
      int k=0;
      count=0;
      while(1){
	TString temp=buffer;
	if(!temp.Length()) break;
	temp.Replace(temp.First(' '),temp.Length(),"");
	array[count++] = atof(temp.Data());
	buffer.Replace(0,buffer.First(' ')+1,"");
      }
      if(debug){
	for(int i=0; i<nBin+1; i++){ cout << array[i]<< " "; }
	cout << endl;
      }

      continue;
    }
    if(buffer.Contains("notarray")){
      isarray=false;
      iss.getline(buf,500); buffer = buf;
      parse(buffer,"min:");
      min=atof(buffer.Data());
      iss.getline(buf,500); buffer = buf;
      parse(buffer,"max:");
      max=atof(buffer.Data());
      continue;
    }
    if(buffer.Contains("bin=")){
      
      if(new){
	new=false;
	if(debug)
	  cout << "name=" <<name << " title=" << title 
	       << " nbin=" << nBin << endl;
	if(isarray){	  
	  h=new TH1D(name,title,nBin,array);
	}
	else{
	  h=new TH1D(name,title,nBin,min,max);
	}
      }
      buffer.ReplaceAll("bin=","");
      TString temp = split(buffer);
      int bin=atoi(temp.Data());

      buffer.ReplaceAll("value: ","");
      temp=split(buffer);
      double value=atof(temp.Data());

      buffer.ReplaceAll("error: ","");
      double error=atof(buffer.Data());
      
      if(debug)
	cout << "bin=" << bin << " value=" << value 
	     << " error=" << error<<endl;

      if(!h){cout << "h not created?" << endl; return;}
      h->SetBinContent(bin,value);
      h->SetBinError(bin,error);
    }

    if(debug && ++count>limit) break;
       
  }
  outRoot->Close();

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
