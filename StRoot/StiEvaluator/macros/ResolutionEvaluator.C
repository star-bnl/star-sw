///#include "StiEvaluator/Evaluator.h"
///#include "StiEvaluator/ResolutionPlots.h"

void ResolutionEvaluator(const char * outputDirectory, bool writeHistos=true, bool writeHtml=false)
{
  // load all required libraries
  char * list[]={"St_base","StChain","StUtilities", "St_Tables", "StarClassLibrary",
		 "libsim_Tables","libglobal_Tables","geometry","St_g2t","St_geant_Maker","libGui",
		 "StIOMaker","StTreeMaker", "St_db_Maker","StDbLib","StDbBroker",
		 "StSvtDbMaker","StDbUtilities", "StTpcDb","StEvent","StEventMaker",
		 "StMcEvent","StMcEventMaker","StMiniMcEvent","StAssociationMaker",
		 "Sti", "StiEvaluator",
		 "last"};
  
  int i=0;  
  cout <<"Run.C::loadLibrairies() - INFO - Started"<<endl;
  while (list[i]!="last")
    {
      cout << "          Loading module:"<<list[i];
      gSystem->Load(list[i++]);
      cout << "\t\t- Done." << endl;
    }  

  // Instantiate/setup evaluator
  Evaluator evaluator;
  evaluator.add(new ResolutionPlots("LoPiPlusPr","Lo Primary piPlus resolution", 10.,2000.,-50.,50.,8,10,15, 3.,-1.,1.,0,0));
  evaluator.add(new ResolutionPlots("HiPiPlusPr","Hi Primary piPlus resolution",4000.,10000.,-50.,50.,9,10,15, 3.,-1.,1.,0,0));
  // run evaluator
  /*
    evaluator.run("rcf0183_01_300evts.minimc.root");
    evaluator.run("rcf0183_02_300evts.minimc.root");
    evaluator.run("rcf0183_03_300evts.minimc.root");
    evaluator.run("rcf0183_04_300evts.minimc.root");
    evaluator.run("rcf0183_05_300evts.minimc.root");
  */

  
  evaluator.run("/home/pruneau/testArea/archive/2003-08-04-1/rcf0183_02_300evts.minimc.root");
  evaluator.run("/home/pruneau/testArea/archive/2003-08-04-1/rcf0183_03_300evts.minimc.root");
  evaluator.run("/home/pruneau/testArea/archive/2003-08-04-1/rcf0183_05_300evts.minimc.root");

  // save files
  evaluator.save(outputDirectory);

  // save documentation 
  if (writeHtml) evaluator.saveHtml(outputDirectory);
}
