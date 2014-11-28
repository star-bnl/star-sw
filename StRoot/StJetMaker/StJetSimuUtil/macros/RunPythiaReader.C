
// NOTE - chain needs to be declared global so for StHbtEventReader
//=========================================================================================
class  StChain;
StChain *chain;
int total=0;

void RunPythiaReader(int nevents=10,
		     const char* jetInFile = "processed/pds1214_23_5000evts.jet.root"
		     )
{
    cout <<"read jet file:\t"<<jetInFile<<endl;
    
    string basename = firstHalf(jetInFile,"/processed/",".jet.root");
    cout <<"basename:\t"<<basename<<endl;
    TString ofn = TString("./assoc/") + TString(basename) + TString(".assoc.root");
    const char* outfile = ofn.Data();
    cout <<"write assoc file:\t"<<outfile<<endl;
    
    if (gClassTable->GetID("TTable") < 0) {
	gSystem->Load("libStar");
	gSystem->Load("libPhysics");
    }
    gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
    loadSharedLibraries();
    gSystem->Load("StMagF");
    gSystem->Load("StTpcDb");
    gSystem->Load("StDbUtilities");
    gSystem->Load("StMcEvent");
    gSystem->Load("StMcEventMaker");
    gSystem->Load("StDaqLib");
    gSystem->Load("StEmcRawMaker");
    gSystem->Load("StEmcADCtoEMaker");
    gSystem->Load("StEmcUtil");
    gSystem->Load("StDbLib");
    gSystem->Load("StDbBroker");  
    gSystem->Load("St_db_Maker");
    gSystem->Load("StEEmcUtil");// needed by EEMC-Db
    gSystem->Load("StEEmcDbMaker");
    gSystem->Load("StJetFinder");
    gSystem->Load("StJetMaker");

    double pi = atan(1.0)*4.0;
    cout << " loading done " << endl;
   
    chain= new StChain("StChain"); 
    chain->SetDebug(1);

    //Instantiate the JetReader
    StJetReader* jetReader = new StJetReader("JetReader",0);

    StPythiaAssociator* jetAssoc = new StPythiaAssociator("JetAssoc", outfile, jetReader);

    chain->PrintInfo();
    chain->Init();

    jetReader->InitFile(jetInFile);

    int ntotal = jetReader->tree()->GetEntries();
    
    chain->PrintInfo();
    
    for (Int_t iev=0;iev<nevents && iev<ntotal; iev++) {
	cout << "****************************************** " << endl;
	cout << "Working on eventNumber " << iev << endl;
	cout << "*************************1***************** " << endl;
	chain->Clear();
	int iret = chain->Make(iev); 
	total++;
	if (iret) {
	    cout << "Bad return code!" << endl;
	    break;
	}
    } 

    chain->Finish(); 
    cout << "****************************************** " << endl;
    cout << "total number of events  " << total << endl;
    cout << "****************************************** " << endl;      
}

string firstHalf(string infile, string begin, string end, int offset=0)
{
    cout <<"look for:\t"<<begin<<"\tin:\t"<<infile<<endl;
    unsigned int where1 = infile.find(begin);
    cout <<"look for:\t"<<end<<"\tin:\t"<<infile<<endl;
    unsigned int where2 = infile.find(end);
    if (where2==infile.npos) {
	return 0;
    }
    
    int start=where1+begin.size()+offset;
    int stop=where2;
    if (stop<=start) {
	cout <<"error, mismatch.abort()"<<endl; abort();
    }
    //cout <<"numbers of events is between indices: ["<<start<<","<<stop<<")"<<endl;
    string number;
    for (int i=start; i<stop; ++i) {
	//cout <<"\ti:\t"<<i<<"\t"<<infile[i]<<endl;
	number += infile[i];
    }
    return number;
}

