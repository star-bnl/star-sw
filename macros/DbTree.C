TTree *ntuple = 0;
void DbTree(const Char_t *server = "mysql://robinson.star.bnl.gov:3306",const Char_t *dbName="Calibrations_rich", const Char_t *tableN = "trigDetSums") {
  TSQLServer *dbserver = TSQLServer::Connect(server,"","");
  ntuple  = new TTreeSQL(dbserver, dbName, tableN); 
}
