/***************************************************************************
 *
 * $Id: rch.C,v 1.1 1999/02/12 23:58:47 lyons Exp $
 *
 * Author: Dan Lyons
 ***************************************************************************
 *
 * Description: RICH offline software:
 *              StRchMaker.cxx - ROOT/STAR Maker for offline chain.
 *              Start at
 *  http://rsgi01.rhic.bnl.gov/STAR/html/comp_l/root/index2.html
 *              for more info, or at
 *  http://rsgi01.rhic.bnl.gov/star/starlib/doc/www/star.html
 *              if the other one disappears for some reason
 *
 * 
 ***************************************************************************
 *
 * $Log: rch.C,v $
 * Revision 1.1  1999/02/12 23:58:47  lyons
 * UNTESTED VERSION, but should hopefully work.
 * Should execute the StRchMaker after creating a sufficient chain.
 *
 **************************************************************************/


// Emulate BFC... from bfc.C:

TBrowser *b = 0;
class StChain;
StChain  *chain=0;

// Only load necessary parts

void Load(){
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("xdf2root");
    gSystem->Load("St_Tables");
 
 
    gSystem->Load("St_params_Maker");
    gSystem->Load("geometry");
    gSystem->Load("g2r");
    gSystem->Load("St_g2r");

    gSystem->Load("St_geant_Maker");
    gSystem->Load("St_TLA_Maker");
    gSystem->Load("St_xdfin_Maker");
    gSystem->Load(StRchMaker");
}

class St_geant_Maker;
St_geant_Maker *geant=0;

// *** Old copy of function from trs.C... decided to use bfc.C as prototype
// *** instead.

// void rch(const Int_t Nevents=1)
// {
//     // Init chain, run geant and rch maker
//     // modified from trs.C:
//   if (gClassTable->GetID("StChain") < 0) Load();
//   chain = new StChain("rch");
//   geant = new St_geant_Maker("geant","event/geant/Event");
//   geant->SetNwGEANT(20 000 000);
//   geant->SetIwtype(1);
//   geant->Do("gfile p /star/u2b/lasiuk/onemuon.fz");
//   StRchMaker *Rch = new StRchMaker("rch","event/raw_data/rch");
//   //  chain->PrintInfo();
//   // Init the mai chain and all its makers
//   int iInit = chain->Init();
//   if (iInit) chain->Fatal(iInit,"on init");
//   gBenchmark->Start("rch");
//   Int_t i=0;
//   for (Int_t i =1; i <= Nevents; i++){
//     if (chain->Make(i)) break;
//     St_DataSet *dst = chain->DataSet("dst");
//     if (i != Nevents) chain->Clear();
//     printf ("===========================================\n");
//     printf ("=========================================== Done with Event no. %d\n",i);
//     printf ("===========================================\n");
//   }
//   if (Nevents > 1) {
//     chain->Finish();
//     gBenchmark->Print("rch");
//   }
//   else b = new TBrowser;
// }

void rch(const Int_t Nevents=1)
{
    // ***modified from bfc.C:
    if (gClassTable->GetID("StChain") < 0) Load();
    // Create the main chain object
    if (chain) delete chain;
    chain = new StChain("rch"); // ***string change from bfc.C, "bfc"->"rch"
	cout << "2" << endl;
    // ***this stuff looks about right... I don't know.  from bfc.C:
    //  Create the makers to be called by the current chain
    St_params_Maker  *params = new St_params_Maker("params","params");
    // St_TLA_Maker       *geom = new
    //         St_TLA_Maker("geom","run/geant/Run");
    St_geant_Maker    *geant = new St_geant_Maker("geant","event/geant/Event");
    geant->SetNwGEANT(20 000 000);
    geant->SetIwtype(1);
    geant->Do("gfile p /disk1/star/test/psc0049_08_40evts.fzd");
	cout << "3.25" << endl;
   // geant->LoadGeometry("detp geometry field_only");
    // *** skip other makers, I think we're good.
    // *** Do load that RICH maker, though.
    // *** Sorry to follow the nonconformist underscore ban.  I just work here.
	cout << "3.5" << endl;
    StRchMaker *rch=new StRchMaker("rch","event/raw_data/rch");
    // *** follow tpc name/title convention, skipped "_raw" modifier.
    // *** back to bfc.C:
	cout << "3.75" << endl;
    chain->PrintInfo();
	cout << "4" << endl;     
    // Init the chain and all its makers
    chain->SetDebug(1);
	cout << "4.35" << endl;
    int iInit = chain->Init();
    if (iInit) chain->Fatal(iInit,"on init");
	cout << "4.5" << endl;
    // *** How does the chain know who its makers are?
    // *** Must be Magic!
    // *** Who am I to ask that of the chain, I know not my maker.
    // *** skip xdf_out/root_out portion of bfc.C, only interested in debug
    // *** info at this point. Loop over events from bfc.C:

    gBenchmark->Start("rch"); // *** string change bfc->rch
	cout << "5" << endl;
    Int_t i=0;
    for (Int_t i =1; i <= Nevents; i++){
	if (chain->Make(i)) break;
	// *** Skip DST crap, looks like all that stuff is commented out
	// *** everywhere.

	if (i != Nevents) chain->Clear();
	printf ("===========================================\n");
	printf ("====Done with Event no. %d\n",i);
	printf ("===========================================\n");
	// *** Hey, cin with the new and cout with the old...
	// ***       We don't be usin' that function no more!
    }
    if (Nevents > 1) {
 	      chain->Finish();
	      delete xdf_in;
	      // *** deleted xdf_out crap again.
	      gBenchmark->Print("rch");  // string change from bfc.C
    }

	  // else {if (!b)   b = new TBrowser;}
	  // *** What's this?  looks graphical, skip for now. 
    	
}

