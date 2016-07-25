TSQLServer *db = 0;
TTreeSQL *sqltree = 0;
void treeRunLog(const Char_t *dbnode = "mysql://dbx.star.bnl.gov:3316",//"mysql://robinson.star.bnl.gov:3306",
	     const Char_t *tabname = "MagFactor",
	     const Char_t *dbname = "RunLog") {
  TSQLServer *db = TSQLServer::Connect(dbnode,"nobody", "");
  //  db->Query("select * from MagFactor where beginTime like '2005-03-02%' order by beginTime");
  //  TString select("select dataID,entryTime,nodeID,elementID,unix_timestamp(beginTime),flavor,numRows,schemaID,deactive,ScaleFactor ");
  sqltree = new TTreeSQL(db,dbname,tabname);
}
