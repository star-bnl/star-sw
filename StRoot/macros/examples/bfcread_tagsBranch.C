// $Id: bfcread_tagsBranch.C,v 1.7 2000/04/13 18:33:02 kathy Exp $
// $Log $

//======================================================================
// owner:  Kathy Turner 
//   taken from code provided by Sasha Vanyashin
//
// what it does:  reads .tags.root file produced from bfc & prints out info
//                - a file name is given as input to the macro
//                - this is a flat file (a TTree file)
//                - for first event, prints out values of all tables & tags
//                - for rest of events, counts # tags for each table
//
// branches -> tables
// leaves   -> entries (tags) in tables
//
//  Inputs to macro:
//     MainFile - input *.tags.root file
//     printEvent - event # to print out details for
//     fname - output file name to write QAInfo
//
//=======================================================================

void bfcread_tagsBranch(
 const char *MainFile="/afs/rhic/star/data/samples/gstar.tags.root",
  Int_t printEvent=1,
  const char *fname="qa_tags.out") 
{

  // start timer
  TStopwatch timer;
  timer.Start();

  cout << " Input .tags.root File Name = " << MainFile << endl;
  cout << " event # for which to  print details  = " << printEvent << endl;
  cout << " Output file containing QAInfo printouts = " << fname << endl;

  ofstream fout(fname);
  fout << " Running: bfcread_tagsBranch.C " << endl;
  fout << " Input .tags.root File Name = " << MainFile << endl;
  fout << " event # for which to  print details  = " << printEvent << endl;
  fout << " Output file containing QAInfo printouts = " << fname << endl;

// gather all files from the same Run into one chain for loading to tagDB
// can .Add more on here and then we will loop over them all 

  TChain chain("Tag");
  chain.Add(MainFile);

  cout << "   Total # events  = " << chain->GetEntries() << endl;
  fout << "   Total # events  = " << chain->GetEntries() << endl;

  TObjArray *files = chain.GetListOfFiles();
  TObjArray *branches = chain.GetListOfBranches();
  TObjArray *leaves = chain.GetListOfLeaves();

  Int_t nleaves = leaves->GetEntriesFast();
  Int_t nbranches = branches->GetEntriesFast();
  cout << "    tot num tables,tags = " << nbranches << "   " << nleaves << endl;
  fout << "    tot num tables,tags = " << nbranches << "   " << nleaves << endl;

  TString file;

//Loop over entries (events written to the tags.root file)
//test that all events can be read in & print out values for printEvent

  Float_t countEvents=0;
  Float_t countTables=0;
  Float_t countTagsTot[4]={0,0,0,0};

  Float_t AsumScaCPM=0;
  Float_t AsumScaCPS=0;
  Float_t AsumScaAM=0;
  Float_t AsumStrange=0;
  Float_t AsumFlowqx=0;
  Float_t AsumFlowqy=0;
  Float_t AsumFlown=0;
  Float_t AsumFlowm=0;
  Float_t AsumEvtHddr=0;
  Float_t AcntScaCPM=0;
  Float_t AcntScaCPS=0;
  Float_t AcntScaAM=0;
  Float_t AcntStrange=0;
  Float_t AcntFlowqx=0;
  Float_t AcntFlowqy=0;
  Float_t AcntFlown=0;
  Float_t AcntFlowm=0;
  Float_t AcntEvtHddr=0;

  for (Int_t k=0;k<chain->GetEntries();k++) {

    chain->GetEntry(k);

    countEvents++;

    //print values only for the first event in each fileof the chain
    //    if (k == *(chain.GetTreeOffset()+chain.GetTreeNumber()))

	file = (files->UncheckedAt(chain.GetTreeNumber()))->GetTitle();
        if (!k) {
            cout <<"    now reading file: " << file.Data() << endl;
            fout <<"    now reading file: " << file.Data() << endl;
        }

        cout <<" ----- Event # " << countEvents << endl;
        fout <<" ----- Event # " << countEvents << endl;
    
// must renew leaves for each file
	leaves = chain.GetListOfLeaves();

        Int_t countTags[4]={0,0,0,0};

// Now loop over leaves (values or tags in tables)
	for (Int_t l=0;l<nleaves;l++) {
	  leaf = (TLeaf*)leaves->UncheckedAt(l);
          branch = leaf->GetBranch();

          countTags[branches->IndexOf(branch)]++;
          countTagsTot[branches->IndexOf(branch)]++;
         
 
//sum & print out branch,tag name and value - for printEvent only
	  if (countEvents == printEvent) 
          { 
            cout << 
	      " QAInfo: table#,name: " << branches->IndexOf(branch) <<
	      ", " << branch->GetName() << 
              " -- has  tag: " << leaf->GetName() <<
              " = " << leaf->GetValue() << endl; 

            fout << 
	      " QAInfo: table#,name: " << branches->IndexOf(branch) <<
	      ", " << branch->GetName() << 
              " -- has  tag: " << leaf->GetName() <<
              " = " << leaf->GetValue() << endl; 
           
	  }

// Sums for ALL events:
// now sum up values in groups of tags so we can do a 
// rough check - this can be removed later when/if we
// have histograms
	     if ( (strcmp(branch->GetName(),"ScaTag")==0) &&
                  (strncmp(leaf->GetName(),"chargedParticles_Means",20)==0) )
	      {
		AcntScaCPM++;
	        AsumScaCPM += leaf->GetValue();
              }
	     if ( (strcmp(branch->GetName(),"ScaTag")==0) &&
                  (strncmp(leaf->GetName(),"chargedParticles_Sigmas",20)==0) )
     	      {
		AcntScaCPS++;
	        AsumScaCPS += leaf->GetValue();
              }
	     if ( (strcmp(branch->GetName(),"ScaTag")==0) &&
                  (strncmp(leaf->GetName(),"scaAnalysisMatrix",15)==0) )
	      {
		AcntScaAM++;
	        AsumScaAM += leaf->GetValue();
              }
	     if ( (strcmp(branch->GetName(),"StrangeTag")==0) )
	      {
		AcntStrange++;
	        AsumStrange += leaf->GetValue();
              }
	     if ( (strcmp(branch->GetName(),"FlowTag")==0) &&
                  (strncmp(leaf->GetName(),"qx",2)==0) )
	      {
		AcntFlowqx++;
	        AsumFlowqx += leaf->GetValue();
              }
	     if ( (strcmp(branch->GetName(),"FlowTag")==0) &&
                  (strncmp(leaf->GetName(),"qy",2)==0) )
	      {
		AcntFlowqy++;
	        AsumFlowqy += leaf->GetValue();
              }
	     if ( (strcmp(branch->GetName(),"FlowTag")==0) &&
                  (strncmp(leaf->GetName(),"n",1)==0) )
	      {
		AcntFlown++;
	        AsumFlown += leaf->GetValue();
              }
	     if ( (strcmp(branch->GetName(),"FlowTag")==0) &&
                  (strncmp(leaf->GetName(),"m",1)==0) )
	      {
		AcntFlowm++;
	        AsumFlowm += leaf->GetValue();
              }
	     if ( (strcmp(branch->GetName(),"EvtHddr")==0) )
	      {
		AcntEvtHddr++;
	        AsumEvtHddr += leaf->GetValue();
              }


	}



// print out for all events
        for (Int_t m=0; m<4; m++){
          cout << "  table "<< m << " has " << countTags[m] << " tags" << endl;
          fout << "  table "<< m << " has " << countTags[m] << " tags" << endl;
          countTables++;
        }
	
  }


// print out at end of processing all events:

  countTables /= countEvents;

  cout << endl << endl << 
    " QAInfo:  Read total # events = " << countEvents << endl;
  cout << " QAInfo:  #tables/event = " << countTables << endl << endl;
  fout << endl << endl << 
    " QAInfo:  Read total # events = " << countEvents << endl;
  fout << " QAInfo:  #tables/event = " << countTables << endl << endl;

  for (Int_t j=0; j<4; j++){
    countTagsTot[j] /= countEvents;
     cout << " QAInfo: table "<< j << " had " 
          << countTagsTot[j] << " tags per event" <<endl;
     fout << " QAInfo: table "<< j << " had " 
          << countTagsTot[j] << " tags per event" <<endl;
  }
  

        AsumEvtHddr /= AcntEvtHddr;
        AsumFlowqx  /= AcntFlowqx;
        AsumFlowqy  /= AcntFlowqy;
        AsumFlown   /= AcntFlown;
        AsumFlowm   /= AcntFlowm;
        AsumStrange /= AcntStrange;
        AsumScaCPM  /= AcntScaCPM;
        AsumScaCPS  /= AcntScaCPS;
        AsumScaAM   /= AcntScaAM;

	cout << endl <<  " QAInfo: ALL evt,  avg Flow qx      = " 
            << AsumFlowqx << endl;
        cout << " QAInfo: ALL evt,  avg Flow qy      = " 
            << AsumFlowqy << endl;
        cout << " QAInfo: ALL evt,  avg Flow n       = " 
            << AsumFlown  << endl; 
        cout << " QAInfo: ALL evt,  avg Flow m       = " 
            << AsumFlowm  << endl; 
        cout << " QAInfo: ALL evt,  avg Strange      = "
            << AsumStrange << endl; 
        cout << " QAInfo: ALL evt,  avg Sca CP mean  = " 
            << AsumScaCPM  << endl; 
        cout << " QAInfo: ALL evt,  avg Sca CP sig   = " 
            << AsumScaCPS  << endl; 
        cout << " QAInfo: ALL evt,  avg Sca An. Mtrx = " 
            << AsumScaAM   << endl; 
        cout << " QAInfo: ALL evt,  avg Evt Hddr     = " 
            << AsumEvtHddr << endl;

	fout << endl <<  " QAInfo: ALL evt,  avg Flow qx      = " 
            << AsumFlowqx << endl;
        fout << " QAInfo: ALL evt,  avg Flow qy      = " 
            << AsumFlowqy << endl;
        fout << " QAInfo: ALL evt,  avg Flow n       = " 
            << AsumFlown  << endl; 
        fout << " QAInfo: ALL evt,  avg Flow m       = " 
            << AsumFlowm  << endl; 
        fout << " QAInfo: ALL evt,  avg Strange      = "
            << AsumStrange << endl; 
        fout << " QAInfo: ALL evt,  avg Sca CP mean  = " 
            << AsumScaCPM  << endl; 
        fout << " QAInfo: ALL evt,  avg Sca CP sig   = " 
            << AsumScaCPS  << endl; 
        fout << " QAInfo: ALL evt,  avg Sca An. Mtrx = " 
            << AsumScaAM   << endl; 
        fout << " QAInfo: ALL evt,  avg Evt Hddr     = " 
            << AsumEvtHddr << endl;
       

  // stop timer and print results
  timer.Stop();
  cout<< endl << endl <<"RealTime="<<timer.RealTime()<<
       " seconds, CpuTime="<<timer.CpuTime()<<" seconds"<<endl;
}











