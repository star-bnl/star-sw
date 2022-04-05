TTree *getMCTree(char *fileListFile) {
  ifstream fin(fileListFile);
  char s[256];
  TChain *chain = new TChain("h999");
  while(fin >> s ) {
    chain->Add(s);
  }
  return (TTree *)chain;
}
