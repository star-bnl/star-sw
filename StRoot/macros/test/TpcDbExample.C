{
gSystem->Load("St_base"); 
gSystem->Load("StUtilities");
gSystem->Load("StChain");  
StChain* mchain = new StChain("StDbTest");
gSystem->Load("StDbLib.so"); 
gSystem->Load("StDbMaker.so");  
gSystem->Load("StTpcDb.so");     // load StTpcDb library
StDbMaker* mk = new StDbMaker("TrsES99","StarDb");  
//StDbMaker* mk = new StDbMaker("dball0","StarDb");
mk->SetTime(1);
mchain->Init();
mchain->Make();
cout << "number of rows = " << gStTpcDb->PadPlaneGeometry()->numberOfRows() << endl;
cout << "anode wire radius = " << gStTpcDb->WirePlaneGeometry()->anodeWireRadius() << endl;
cout << "inner field cage radius " << gStTpcDb->Dimensions()->ifcRadius() << endl;
}

