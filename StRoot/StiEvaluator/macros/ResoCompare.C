///#include "StiEvaluator/Evaluator.h"
///#include "StiEvaluator/ResolutionPlots.h"

void ResoCompare(const char * outputDirectory, bool writeHistos=true, bool writeHtml=false)
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
      cout << "\tLoading module:"<<list[i];
      gSystem->Load(list[i++]);
      cout << "\t\t- Done." << endl;
    }  

  // Instantiate/setup evaluator
  
  ResolutionPlots * rp1;
  ResolutionPlots * rp2;
  ResolutionPlots * rp3;
  ResolutionPlots * rp4;
  ResolutionPlots * rp5;
  ResolutionPlots * rp6;
  rp1=new ResolutionPlots("IttfPrPiPlus","Primary pi+ resolution", 
			  10.,20000.,-50.,50.,8,10,10, 3.,-1.,1.,0,0) ;
  rp2=new ResolutionPlots("IttfPrKPlus","Primary K+ resolution", 
			  10.,20000.,-50.,50.,11,10,10, 3.,-1.,1.,0,0);
  rp3=new ResolutionPlots("IttfPrProton","Primary proton resolution", 
			  10.,20000.,-50.,50.,14, 10,10, 3.,-1.,1.,0,0);
  Evaluator ittfEvaluator;
  ittfEvaluator.add(rp1);
  ittfEvaluator.add(rp2);
  ittfEvaluator.add(rp3);

  // run evaluator
  /*ittfEvaluator.run("rcf0183_01_300evts.minimc.root");
  ittfEvaluator.run("rcf0183_02_300evts.minimc.root");
  ittfEvaluator.run("rcf0183_03_300evts.minimc.root");
  ittfEvaluator.run("rcf0183_04_300evts.minimc.root");
  ittfEvaluator.run("rcf0183_05_300evts.minimc.root");
  */

  //ittfEvaluator.run("/home/pruneau/testArea/archive/2003-08-04-1/rcf0183_02_300evts.minimc.root");
  //ittfEvaluator.run("/home/pruneau/testArea/archive/2003-08-04-1/rcf0183_03_300evts.minimc.root");
  //ittfEvaluator.run("/home/pruneau/testArea/archive/2003-08-04-1/rcf0183_05_300evts.minimc.root");
  ittfEvaluator.run("../cvsAug7_tune1/rcf0183_01_300evts.minimc.root");
  ittfEvaluator.run("../cvsAug7_tune1/rcf0183_02_300evts.minimc.root");
  ittfEvaluator.run("../cvsAug7_tune1/rcf0183_03_300evts.minimc.root");
  ittfEvaluator.run("../cvsAug7_tune1/rcf0183_04_300evts.minimc.root");
  ittfEvaluator.run("../cvsAug7_tune1/rcf0183_05_300evts.minimc.root");

  // save files
  ittfEvaluator.save(outputDirectory);

  rp4=new ResolutionPlots("TptPrPiPlus","Primary pi+ resolution", 
			  10.,20000.,-50.,50.,8,10,10, 3.,-1.,1.,0,1) ;
  rp5=new ResolutionPlots("TptPrKPlus","Primary K+ resolution", 
			  10.,20000.,-50.,50.,11,10,10, 3.,-1.,1.,0,1);
  rp6=new ResolutionPlots("TptPrProton","Primary proton resolution", 
			  10.,20000.,-50.,50.,14,10,10, 3.,-1.,1.,0,1);
  Evaluator tptEvaluator;
  tptEvaluator.add(rp4);
  tptEvaluator.add(rp5);
  tptEvaluator.add(rp6);
  //tptEvaluator.run("/data/r25b/andrew/tpt/aug2-03/3/rcf0183_05_300evts.minimc.tpt.root");
  tptEvaluator.run("/data/r26b/andrew/cvs-aug5/tpt/rcf0183_01_300evts.minimc.root");
  tptEvaluator.run("/data/r26b/andrew/cvs-aug5/tpt/rcf0183_02_300evts.minimc.root");
  tptEvaluator.run("/data/r26b/andrew/cvs-aug5/tpt/rcf0183_03_300evts.minimc.root");
  tptEvaluator.run("/data/r26b/andrew/cvs-aug5/tpt/rcf0183_04_300evts.minimc.root");
  tptEvaluator.run("/data/r26b/andrew/cvs-aug5/tpt/rcf0183_05_300evts.minimc.root");
  
  // save files
  tptEvaluator.save(outputDirectory);
  
  // save documentation 
  if (writeHtml) 
    {
      tptEvaluator.saveHtml(outputDirectory,"ResolutionPion","Pion Resolution Performance",rp1,rp4);
      tptEvaluator.saveHtml(outputDirectory,"ResolutionKaon","Kaon Resolution Performance",rp2,rp5);
      tptEvaluator.saveHtml(outputDirectory,"ResolutionProton","Proton Resolution Performance",rp3,rp6);
    }

}
