void selectAllM2(const char* infile,  const char* outfile=""){
  // extracts files for cutbin mode 2 (soft/hard SS/AS)
  // creates directories soft, hard, SS, AS, other
  //  MUST BE RUN FROM DATA DIR

  cout << "  selectAllM2, input: " << infile << endl;

  gSystem->Load("StEStructPoolSupport.so");
  StEStructHAdd adder;
  TFile * tf=new TFile(infile);

  if(tf->IsZombie()){
    cout<<"error opening file "<<endl;
    return ;
  };

  //cout << "Creating directories soft hard SS AS other" << endl;
  gSystem->Exec("mkdir -p all soft hard SS AS other");

  /* 
  TString fname = infile;
  cout << fname << endl;
  TObjArray* toa = fname.Tokenize("/");
  tos = (TObjString*) toa->Last();
  fname = tos->String();
  cout << fname << endl;
  */

  TString fname = infile;       
  if(outfile[0]) fname = outfile;

  cout << "Writing to " << fname << endl;

  // OLD  0 = soft SS;  1 = hard SS;  2 = soft AS;  3 = hard AS              

  //   0 = soft SS;  1 = hard SS;  2 = other SS;  3 = soft AS;  4 = hard AS;  5 = other AS;              

  int num;
  TString ts;

  num = 2;
  int ns[2] = {0,3};
  ts = "soft/"; ts+=fname;
  cout << "  writing " << ts << endl;
  adder.addCuts(ts,tf,ns,num);

  num = 2;
  int nh[2] = {1,4};
  ts = "hard/"; ts+=fname;
  cout << "  writing " << ts << endl;
  adder.addCuts(ts,tf,nh,num);

  num = 2;
  int nh[2] = {2,5};
  ts = "other/"; ts+=fname;
  cout << "  writing " << ts << endl;
  adder.addCuts(ts,tf,nh,num);

  num = 3;
  int nss[3] = {0,1,2};
  ts = "SS/"; ts+=fname;
  cout << "  writing " << ts << endl;
  adder.addCuts(ts,tf,nss,num);

  num = 3;
  int nas[3] = {3,4,5};
  ts = "AS/"; ts+=fname;
  cout << "  writing " << ts << endl;
  adder.addCuts(ts,tf,nas,num);

  num = 6;
  int nall[6] = {0,1,2,3,4,5};
  ts = "all/"; ts+=fname;
  cout << "  writing " << ts << endl;
  adder.addCuts(ts,tf,nall,num);



}


