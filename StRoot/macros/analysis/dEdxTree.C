void dEdxTree(Int_t nevents=10000,
	      const char *MainFile="/star/rcf/data03/reco/P00hi/2000/08/st_physics_1243022_raw_0016.dst.root",
	    //const char *MainFile="/star/data03/reco/P00hi/2000/09/st_physics_1246036_raw_0001.dst.root",
	      const char* rootFile = "dEdx_1246036.root")
{
  gROOT->LoadMacro("bfc.C");
  bfc(0,"dEdxTree",MainFile,0,rootFile);
  chain->SetDEBUG(0);
  cout <<"Starting Event Loop"<<endl;
  int istat=0,iev=1;
 EventLoop: if (iev<=nevents && !istat) {
   chain->Clear();
   cout << "---------------------- Processing Event : " << iev << endl;
   istat = chain->Make(iev); // This should call the Make() method in ALL makers
   if (istat) {
     cout << "Last Event Processed. Status = " << istat << endl;
   }
   iev++; goto EventLoop;
 } // Event Loop
}  


