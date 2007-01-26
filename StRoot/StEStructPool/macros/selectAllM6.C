void selectAllM6(const char* infile, char* outputdir = "./"){
  // extracts files for cutbin mode 6 (z-vertex binning)
  // reads infile, writes final00.root to final09.root in outputdir


  cout << "  selectAllM6, input: " << infile << "\tout dir: " << outputdir << endl;
  gSystem->Load("StEStructPoolSupport.so");
  StEStructHAdd adder;
  TFile * tf=new TFile(infile);

  if(!tf){
    cout<<"error opening file "<<endl;
    return ;
  };

  int narray[1];
  int num=1;

  TString fname;
  for(int i=0; i<10; i++) {
    narray[0] = i;
    fname=outputdir; fname+="final0"; fname+=i; fname+=".root";
    cout << "writing " << fname << endl;
    adder.addCuts(fname,tf,narray,num);
  }


};


