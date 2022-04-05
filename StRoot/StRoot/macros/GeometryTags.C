void GeometryTags() {
  if (gClassTable->GetID("StMaker") < 0) {
    gSystem->Load("St_base");
    gSystem->Load("StChain");
  }
  DbAlias_t *db = StMaker::GetDbAliases();
  DbAlias_t *dbTag = db;
  for (Int_t i = 0; dbTag->tag; i++, dbTag++) {
    cout << "Geometry Tags:\t" 
	 << dbTag->tag 
	 << "\t" << dbTag->date 
	 << "\t" << dbTag->time 
	 << "\t" << dbTag->geometry 
	 << "\t" << dbTag->comment << endl;
  }
}
