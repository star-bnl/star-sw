void runBfc( int n, const char* filename = "../Simulation.HFjets/out4/rcf22000_15165031_1_1evts.fzd" )
{

  TString chainOpts;//("fzin gen_T geomT sim_T TpcRS -ittf -tpc_daq nodefault y2014x usexgeom misalign sti   DbV20160418 P2014a pxlHit istHit btof mtd mtdCalib BEmcChkStat CorrX OSpaceZ2 OGridLeak3D ODistoSmear -hitfilt  -evout  vfminuit -vfmce tpxclu pxlslowsim istslowsim nosvtit nossdit " ); // picoWrite PicoVtxDefault            DbV20150316 P2014a pxlHit istHit btof mtd mtdCalib BEmcChkStat CorrX OSpaceZ2 OGridLeak3D -hitfilt

  chainOpts += " fzin DbV20160418 P2014a pxlHit istHit btof vpd mtd mtdCalib BEmcChkStat CorrX OSpaceZ2 OGridLeak3D ODistoSmear -hitfilt tpxclu pxlslowsim istslowsim nosvtit nossdit btofit ";
  // chainOpts += " -vfminuit -vfmce vfppvnoctb beamline ";
  chainOpts += " vfmce ";
  // chainOpts += " ,sdt20140216,tpcdb ";
  chainOpts += " sdt20140424,tpcdb ";
  chainOpts += " mtdsim bana IAna TPCRS bbcsim btofdat btofmixer btofsim btofmatch MakeEvent vpdsim vpdcalib ";
  chainOpts += " simu dEdxY2 ";                    // Runs TPC fast simulation
  chainOpts += " -emcDY2 ";
  chainOpts += " sti ittf ";                      // Runs track finding and reconstruction using the "sti" tracker
  chainOpts += " gen_T,geomT,sim_T,AgML, emcY2 eess "; // Remove this later.
  chainOpts += " GeantOut,MiniMcMk,-in,useInTracker, EEss, cmudst";
  //  chainOpts += ",picowrite,picovtxvdefault";


  gSystem->Load("StarRoot.so");
  gROOT->LoadMacro("bfc.C");

  bfc(n,chainOpts,filename);

}
