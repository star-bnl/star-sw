//////////////////////////////////////////////////////////////////////
// $Id: StMiniEmbed.C,v 1.10 2007/09/09 17:30:17 fisyak Exp $
// owner: Manuel Calderon de la Barca Sanchez
//
// what it does: reads .geant.root file from emedding data, produces minimc.root file 
//               runs a chain with the makers: 
//                 StMcEventMaker,StAssociationMaker,
//                 StMiniMcEventMaker
// Note: it is currently written to write into a specific directory structure in PDSF
//       so if one needs to run elsewhere, and the output directory doesn't have the same
//       lower level directory structure, no output files will be done.
// $Log: StMiniEmbed.C,v $
// Revision 1.10  2007/09/09 17:30:17  fisyak
// use bfc.C for loading shared libraries
//
// Revision 1.9  2006/09/29 01:32:12  calderon
// use event branch instead of dst branch.
//
// Revision 1.8  2006/07/24 19:25:26  calderon
// Load EEmcUtil, needed by StMcEvent.
//
// Revision 1.7  2004/03/30 03:16:14  calderon
// Modifications for running in bfc.
//  - Changed to use StiIOInterface (IOMaker in normal mode, TreeMaker in bfc)
//  - Cleaned up Init(), InitRun() to handle the changing file names.
//  - Initialize lots of variables and pointers in constructor.
//  - Delete some pointers in Finish (deleting the TTree causes a seg fault, though.)
//  - Note that currently the StHits in the ITTF chain don't have a usedInFit() flag,
//    so there will be many messages complaining about this.
//  - Removed the mDebug data member, every Maker already has one, so change
//    to use that throughout the package.
//
// Revision 1.6  2003/07/09 01:07:23  calderon
// Addition of FTPC reference multiplicity
// Addition of other multiplicity values for StMiniMcEvent
// Changes to reflect the use of the setters and getters, no longer
// access the data members directly.
//
// Revision 1.5  2002/06/28 22:15:12  calderon
// Changes to deal with seg. faults in the file name handling:
// Conventions:
// StMiniMcMaker looks for the input file from the IO maker to figure out
// if the file has changed.  This is done using TString::Contains() in Make().
// Usually we will run one file at a time, but in order not to break Bum's scheme of being
// able to process several files in one go, this is left as is.  However, for
// embedding, the file name is not enough, in Eric's new scheme there are repeated
// file names.  This is resolved by adding a prefix to the output file name.  However,
// this prefix should not be overwritten, so the current code only replaces the
// string inside the output file name pertaining to the input file name, and leaves
// the prefix of the output file intact.  This was done for embedding looking for
// st_physics, and here is where the problem arose: hijing files begin with a different
// prefix.  To solve this problem, the input file name prefix is now an input parameter
// in the macro.
//
// StMiniEmbed.C and StMiniHijing.C now conform to this convention.  StMiniEmbed.C
// did not change its prototype, because all embedding files have st_phyics as prefix.
// StMiniHijing.C changed its prototype, now it takes as an input argument the prefix,
// but in order not to break Jenn's scripts if she was already using this macro,
// this parameter was added at the end and defaults to "rcf", which is appropriate
// for hijing files reconstructed in rcf.
//
// Revision 1.4  2002/06/11 19:09:34  calderon
// Bug fix: the filename that was set in the macro was being overwritten
// in InitRun, so the emb80x string which was added to the filename was lost.
// This was fixed by not replacing the filename in InitRun and only replacing
// the current filename starting from st_physics.
//
// Revision 1.3  2002/06/07 02:21:48  calderon
// Protection against empty vector in findFirstLastHit
//
//////////////////////////////////////////////////////////////////////

// const char* ffile="/auto/pdsfdv09/starprod/embedding/P01hj/HighpT_piminus_101/1243006_0003.26283/st_physics_1243006_raw_0003.dst.root";
//const char* ffile="/beta/starprod/embedding/P02gd/Rev/Piminus_801_minbias/2254002_0021.21333/st_physics_2254002_raw_0021.geant.root";
const char* ffile="/star/data16/reco/pp200/pythia6_205/0_2gev/cdf_a/y2004y/gheisha_on/p05ih/rcf1273_99_4000evts.geant.root";
void StMiniEmbed(Int_t nevents=2,
		 const char* MainFile=ffile,
		 const char* outDir = "./",
		 int commonHits=3)
{

  cout << "Using  : " << MainFile << endl;
  //
  // the string manipulations below are for use in PDSF, from
  // the /beta/starprod/embedding/ input directory
  // to the /auto/pdsfdv41/starprod/QA/McMiniDst/  output directory
  //
  TString outDirName = outDir;
  TString filename   = MainFile;
  TString embedrun   = MainFile;
  if (filename.Contains("Rev"))
      outDirName.Append("RevFullField/");
  else
      outDirName.Append("FullField/");

  if (filename.Contains("Piminus"))
      outDirName.Append("PiMinus/");
  if (filename.Contains("Piplus"))
      outDirName.Append("PiPlus/");
  if (filename.Contains("KMinus"))
      outDirName.Append("KMinus/");
  if (filename.Contains("KPlus"))
      outDirName.Append("KPlus/");
  if (filename.Contains("Pbar"))
      outDirName.Append("Pbar/");
  if (filename.Contains("Proton"))
      outDirName.Append("Proton/");
  int embedRunIndex = embedrun.Index("_",0);
  embedrun.Remove(0,embedRunIndex+1);
  embedRunIndex = embedrun.Index("_",0);
  embedrun.Remove(embedRunIndex);
  int fileBeginIndex = filename.Index("st_physics",0);
  filename.Remove(0,fileBeginIndex);
  filename.Prepend(embedrun);
  filename.Prepend("emb");
  cout << "outdir : " << outDirName << endl;
  cout << "Output : " << filename << endl;

  gROOT->LoadMacro("bfc.C");
  TString Chain("in,StEvent,gen_T,sim_T,readall,nodefault,minimcmk");
  bfc(-1,Chain.Data(),MainFile,0,filename);
  // Define the cuts for the Associations
  
  StMcParameterDB* parameterDB = StMcParameterDB::instance();  
  // TPC
  parameterDB->setXCutTpc(.5); // 5 mm
  parameterDB->setYCutTpc(.5); // 5 mm
  parameterDB->setZCutTpc(.5); // 5 mm
  parameterDB->setReqCommonHitsTpc(commonHits); // Require 3 hits in common for tracks to be associated
  // FTPC
  parameterDB->setRCutFtpc(.3); // 3 mm
  parameterDB->setPhiCutFtpc(5*(3.1415927/180.0)); // 5 degrees
  parameterDB->setReqCommonHitsFtpc(3); // Require 3 hits in common for tracks to be associated
  // SVT
  parameterDB->setXCutSvt(.08); // 800 um
  parameterDB->setYCutSvt(.08); // 800 um
  parameterDB->setZCutSvt(.08); // 800 um
  parameterDB->setReqCommonHitsSvt(1); // Require 1 hits in common for tracks to be associated
  
  
  // now execute the chain member functions
  
  //chain->PrintInfo();
  Int_t initStat = chain->Init(); // This should call the Init() method in ALL makers
  if (initStat) chain->Fatal(initStat, "during Init()");
  
  int istat=0,iev=1;
 EventLoop: if (iev<=nevents && istat!=2) {
   chain->Clear();
   cout << "---------------------- Processing Event : " << iev << " ----------------------" << endl;
   istat = chain->Make(iev); // This should call the Make() method in ALL makers
   if (istat == 2) { cout << "Last  Event Processed. Status = " << istat << endl; }
   if (istat == 3) { cout << "Error Event Processed. Status = " << istat << endl; }
   iev++; goto EventLoop;
 } // Event Loop
 
//  chain->Finish();
  
}

