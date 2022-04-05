void DbList(){

  // example to print the list of tables retrievable from 
  // the database(s).
 
  gSystem->Load("StDbLib");

  StDbManager* mgr=StDbManager::Instance();
  StDbConfigNode* node = mgr->initConfig("StarDb","reconV0"); 


  mgr->setVerbose(true);
  node->printTree(0);
  //  delete node;
}







