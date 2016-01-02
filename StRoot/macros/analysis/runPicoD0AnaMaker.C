/* **************************************************
 *  A macro to run StPicoD0AnaMaker
 *
 *  Authors:  **Mustafa Mustafa (mmustafa@lbl.gov)
 *
 *  **Code Maintainer
 *
 * **************************************************
 */


void runPicoD0AnaMaker(TString d0list, TString outFileName, TString badRunListFileName = "picoList_bad_MB.list")
{
   //Check STAR Library. Please set SL_version to the original star library used in the production from http://www.star.bnl.gov/devcgi/dbProdOptionRetrv.pl
#if 0
   string SL_version = "DEV2/SL15";
   string env_SL = getenv("STAR");
   if (env_SL.find(SL_version) == string::npos)
   {
      cout << "Environment Star Library does not match the requested library in runPicoD0EventMaker.C. Exiting..." << endl;
      exit(1);
   }
#endif
   gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
   loadSharedLibraries();

   gSystem->Load("StBTofUtil");
   gSystem->Load("StPicoDstMaker");
   gSystem->Load("StRefMultCorr");
   gSystem->Load("StPicoPrescales");
   gSystem->Load("StPicoCutsBase");
   gSystem->Load("StPicoD0EventMaker");
   gSystem->Load("StPicoD0AnaMaker");
   gSystem->Load("StPicoHFMaker");

   chain = new StChain();

   // create list of picoDst files
   TString command = "sed 's/hft\\\/d0tree/picodsts/g' " + d0list + " >correspondingPico.list";
   gSystem->Exec(command.Data());
   command = "sed -i 's/picoD0/picoDst/g' correspondingPico.list";
   gSystem->Exec(command.Data());
   StPicoDstMaker* picoDstMaker = new StPicoDstMaker(0, "correspondingPico.list", "picoDstMaker");
   StPicoD0AnaMaker*  picoD0AnaMaker = new StPicoD0AnaMaker("picoD0AnaMaker", d0list, outFileName.Data(), picoDstMaker);

   StHFCuts* d0Cuts = new StHFCuts("d0Cuts");
   picoD0AnaMaker->setHFCuts(d0Cuts);

   // -------------- USER variables -------------------------

   // -- File name of bad run list
   d0Cuts->setBadRunListFileName(badRunListFileName);

   // add your cuts here.

   // tracking
   d0Cuts->setCutNHitsFitMax(20);

   // pions
   d0Cuts->setCutTPCNSigmaPion(3.0);

   // kaons
   d0Cuts->setCutTPCNSigmaKaon(2.0);

   // kaonPion pair cuts
   float dcaDaughtersMax = 0.008;  // maximum
   float decayLengthMin  = 0.0030; // minimum
   float decayLengthMax  = 999999; //std::numeric_limits<float>::max();
   float cosThetaMin     = 0.90;   // minimum
   float minMass         = 1.6;
   float maxMass         = 2.1;
   d0Cuts->setCutSecondaryPair(dcaDaughtersMax, decayLengthMin, decayLengthMax, cosThetaMin, minMass, maxMass);

   chain->Init();
   int nEntries = picoD0AnaMaker->getEntries();
   for (int iEvent = 0; iEvent < nEntries; ++iEvent)
   {
      chain->Clear();
      int iret = chain->Make();
      if (iret)
      {
         cout << "Bad return code!" << iret << endl;
         break;
      }
   }

   chain->Finish();
   delete chain;

   // delete list of picos
   command = "rm -f correspondingPico.list";
   gSystem->Exec(command.Data());

}
