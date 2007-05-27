void selectAllM0(const char* infile, const char* outfile){
  cout << "Input: " << infile << "\t\tOutput: " << outfile << endl;
  
  //void addCuts(const char* outfile, const char* infile, int* nlist, int num, int all = 0)

  //do quick sanity check
  tf1 = new TFile(infile);
  if(tf1->IsZombie()) {
    cout << "ERROR reading " << infile << endl;
    return;
  }


  StEStructHAdd adder;

  int nlist[1] = {-1};

  adder.addCuts(outfile, infile, nlist, 1, 0);

};


