///#include "StiEvaluator/Evaluator.h"
///#include "StiEvaluator/ResolutionPlots.h"

void ResolutionEvaluator(const char * inFile,
			 const char * outputDirectory,
			 bool writeHistos=true, bool writeHtml=false)
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
  Evaluator evaluator;
  ResolutionPlots * rp1;
  ResolutionPlots * rp2;
  ResolutionPlots * rp3;
  ResolutionPlots * rp4;
  ResolutionPlots * rp5;
  ResolutionPlots * rp6;
  rp1=new ResolutionPlots("LoPrPiPlus","Lo Occ Primary pi+ resolution", 
			  10.,2000.,-50.,50.,8,10,10, 3.,-1.,1.,0,0) ;
  rp2=new ResolutionPlots("HiPrPiPlus","Hi Occ Primary pi+ resolution",
			  4000.,10000.,-50.,50., 8, 10,10, 3.,-1.,1.,0,0);

  rp3=new ResolutionPlots("LoPrKPlus","Lo Occ Primary K+ resolution", 
			  10.,2000.,-50.,50.,11,10,10, 3.,-1.,1.,0,0);

  rp4=new ResolutionPlots("HiPrKPlus","Hi Occ Primary K+ resolution",
			  4000.,10000.,-50.,50., 11,10,10, 3.,-1.,1.,0,0);

  rp5=new ResolutionPlots("LoPrProton","Lo Occ Primary proton resolution", 
			  10.,2000.,-50.,50.,14, 10,10, 3.,-1.,1.,0,0);
  
  rp6=new ResolutionPlots("HiPrProton","Hi Occ Primary proton resolution",
			  4000.,10000.,-50.,50., 14,10,10, 3.,-1.,1.,0,0);

  evaluator.add(rp1);
  evaluator.add(rp2);
  /*evaluator.add(rp3);
  evaluator.add(rp4);
  evaluator.add(rp5);
  evaluator.add(rp6);*/


  // run evaluator
  evaluator.run(inFile);

  
  // save files
  evaluator.save(outputDirectory);
  
  // save documentation 
  if (writeHtml) 
    {
      evaluator.saveHtml(outputDirectory,"ResolutionPion","Pion Resolution Performance",rp2,rp1);
      //evaluator.saveHtml(outputDirectory,"ResolutionKaon","Kaon Resolution Performance",rp4,rp3);
      //evaluator.saveHtml(outputDirectory,"ResolutionProton","Proton Resolution Performance",rp6,rp5);
    }

}
