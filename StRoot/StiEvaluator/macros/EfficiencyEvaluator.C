
void EfficiencyEvaluator(bool writeHistos=true, bool writeHtml=false)
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

  //ittf
  EfficiencyPlots * ittfLoPipNgt10 = new EfficiencyPlots("IttfEffPrLoPiPlusN10","ittf Low Occ- PiPlus Efficiency N>10",
							 0., 2000., -100., 100., 8, 10., 10., 
							 3., -1., 1., 0);
  EfficiencyPlots * ittfLoPipNgt20 = new EfficiencyPlots("IttfEffPrLoPiPlusN20","ittf Low Occ- PiPlus Efficiency N>20",
							 0., 2000., -100., 100., 8, 20., 20., 
							 1., -1., 1., 0);

  EfficiencyPlots * ittfMePipNgt10 = new EfficiencyPlots("IttfEffPrMePiPlusN10","ittf Med Occ- PiPlus Efficiency N>10",
							 2000., 4000., -100., 100., 8, 10., 10., 
							 3., -1., 1., 0);
  EfficiencyPlots * ittfMePipNgt20 = new EfficiencyPlots("IttfEffPrMePiPlusN20","ittf Med Occ- PiPlus Efficiency N>20",
							 2000., 4000., -100., 100., 8, 20., 20., 
							 1., -1., 1., 0);
  EfficiencyPlots * ittfHiPipNgt10 = new EfficiencyPlots("IttfEffPrHiPiPlusN10","ittf Hi  Occ- PiPlus Efficiency N>10",
							 4000.,94000., -100., 100., 8, 10., 10., 
							 3., -1., 1., 0);
  EfficiencyPlots * ittfHiPipNgt20 = new EfficiencyPlots("IttfEffPrHiPiPlusN20","ittf Hi  Occ- PiPlus Efficiency N>20",
							 4000.,94000., -100., 100., 8, 20., 20., 
							 1., -1., 1., 0);

  //tpt
  EfficiencyPlots * tptLoPipNgt10 = new EfficiencyPlots("TptEffPrLoPiPlusN10","tpt Low Occ- PiPlus Efficiency N>10",
							 0., 2000., -100., 100., 8, 10., 10., 
							 3., -1., 1., 0);
  EfficiencyPlots * tptLoPipNgt20 = new EfficiencyPlots("TptEffPrLoPiPlusN20","tpt Low Occ- PiPlus Efficiency N>20",
							 0., 2000., -100., 100., 8, 20., 20., 
							 1., -1., 1., 0);

  EfficiencyPlots * tptMePipNgt10 = new EfficiencyPlots("TptEffPrMePiPlusN10","tpt Med Occ- PiPlus Efficiency N>10",
							 2000., 4000., -100., 100., 8, 10., 10., 
							 3., -1., 1., 0);
  EfficiencyPlots * tptMePipNgt20 = new EfficiencyPlots("TptEffPrMePiPlusN20","tpt Med Occ- PiPlus Efficiency N>20",
							 2000., 4000., -100., 100., 8, 20., 20., 
							 1., -1., 1., 0);
  EfficiencyPlots * tptHiPipNgt10 = new EfficiencyPlots("TptEffPrHiPiPlusN10","tpt Hi  Occ- PiPlus Efficiency N>10",
							 4000.,94000., -100., 100., 8, 10., 10., 
							 3., -1., 1., 0);
  EfficiencyPlots * tptHiPipNgt20 = new EfficiencyPlots("TptEffPrHiPiPlusN20","tpt Hi  Occ- PiPlus Efficiency N>20",
							 4000.,94000., -100., 100., 8, 20., 20., 
							 1., -1., 1., 0);


  Evaluator ittfEvaluator;
  ittfEvaluator.add(ittfLoPipNgt10);
  ittfEvaluator.add(ittfLoPipNgt20);
  ittfEvaluator.add(ittfMePipNgt10);
  ittfEvaluator.add(ittfMePipNgt20);
  ittfEvaluator.add(ittfHiPipNgt10);
  ittfEvaluator.add(ittfHiPipNgt20);
  ittfEvaluator.run("../cvsAug7_tune1/rcf0183_01_300evts.minimc.root");
  ittfEvaluator.run("../cvsAug7_tune1/rcf0183_02_300evts.minimc.root");
  ittfEvaluator.run("../cvsAug7_tune1/rcf0183_03_300evts.minimc.root");
  ittfEvaluator.run("../cvsAug7_tune1/rcf0183_04_300evts.minimc.root");
  ittfEvaluator.run("../cvsAug7_tune1/rcf0183_05_300evts.minimc.root");
  //ittfEvaluator.run("/data/r26b/andrew/claude183a/rcf0183_01_300evts.minimc.root");
  //ittfEvaluator.run("/data/r26b/andrew/claude183a/rcf0183_02_300evts.minimc.root");
  //ittfEvaluator.run("/data/r26b/andrew/claude183a/rcf0183_03_300evts.minimc.root");
  //ittfEvaluator.run("/data/r26b/andrew/claude183a/rcf0183_05_300evts.minimc.root");
  //ittfEvaluator.run("/home/pruneau/testArea/cvsAug7_tune1/rcf0183_05_300evts.minimc.root");
  ittfEvaluator.save("../archive/2003-08-07-1/");
  
  Evaluator tptEvaluator;
  tptEvaluator.add(tptLoPipNgt10);
  tptEvaluator.add(tptLoPipNgt20);
  tptEvaluator.add(tptMePipNgt10);
  tptEvaluator.add(tptMePipNgt20);
  tptEvaluator.add(tptHiPipNgt10);
  tptEvaluator.add(tptHiPipNgt20);


  //tEvaluator.run("/data/r25b/andrew/tpt/aug2-03/3/rcf0183_05_300evts.minimc.tpt.root");
  tptEvaluator.run("/data/r26b/andrew/cvs-aug5/tpt/rcf0183_01_300evts.minimc.root");
  tptEvaluator.run("/data/r26b/andrew/cvs-aug5/tpt/rcf0183_02_300evts.minimc.root");
  tptEvaluator.run("/data/r26b/andrew/cvs-aug5/tpt/rcf0183_03_300evts.minimc.root");
  tptEvaluator.run("/data/r26b/andrew/cvs-aug5/tpt/rcf0183_04_300evts.minimc.root");
  tptEvaluator.run("/data/r26b/andrew/cvs-aug5/tpt/rcf0183_05_300evts.minimc.root");
  tptEvaluator.save("../archive/2003-08-07-1/");

  // save documentation 
  if (writeHtml) 
    {
      tptEvaluator.saveHtml("../archive/2003-08-07-1/",
			    "PionEfficiencyLowNgt10",
			    "Pion Efficiency Low Occ N>10",ittfLoPipNgt10,tptLoPipNgt10);
      tptEvaluator.saveHtml("../archive/2003-08-07-1/",
			    "PionEfficiencyLowNgt20",
			    "Pion Efficiency Low Occ N>20",ittfLoPipNgt20,tptLoPipNgt20);

      tptEvaluator.saveHtml("../archive/2003-08-07-1/",
			    "PionEfficiencyHighNgt10",
			    "Pion Efficiency High Occ N>10",ittfHiPipNgt10,tptHiPipNgt10);
      tptEvaluator.saveHtml("../archive/2003-08-07-1/",
			    "PionEfficiencyHighNgt20",
			    "Pion Efficiency High Occ N>20",ittfHiPipNgt20,tptHiPipNgt20);
    }
}
