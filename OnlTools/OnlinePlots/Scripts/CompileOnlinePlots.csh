#!/bin/tcsh
# Check out the new OnlinePlots  to offline build tree
# 
         
echo --- Compiliant PPlots libraries
#stardev
cons
rm EvpPlotServer.C
echo --- Creating the ROOT macro
cat >EvpPlotServer.C<<ROOTMACRO
{
  gROOT->Macro("Load.C");
  gSystem->Load("RTS");
  gSystem->Load("StDaqLib");
  gSystem->Load("OnlinePlots");
  gSystem->Load("StDbLib");
  gSystem->Load("StEEmcDbMaker");
  gSystem->Load("StEmcUtil");
  gSystem->Load("StStrangeMuDstMaker");
  gSystem->Load("StMuDSTMaker");
  gSystem->Load("StEEmcPoolmuEztPanitkin");
  gSystem->Load("StBEMCPlots");
  gSystem->Load("StEvent");
  const char *homedir = gSystem->Getenv("ONLINEPLOTSDIR");
  if (!homedir) printf(" Attention: The environment variable \"ONLINEPLOTSDIR\" has not been set yet !!!\n");
  char* args[] = {
                "-path"
                ,"/a/9057008/"
                , "-map"
                , "test.map"
                , "-nocheck"
                , "-nogui"
                , "-start" 
                };
//  evpServerMain::main(sizeof(args)/sizeof(char*), args);
//  evpServerMain::main(int argc, const char* argv[]);
//  evpMainPresenter::main();
}
ROOTMACRO
more  EvpPlotServer.C
echo Do not forget to set the ONLINEPLOTSDIR environment variable properly
echo For example:
echo setenv ONLINEPLOTSDIR .
echo -----------------------
setenv ONLINEPLOTSDIR .
echo Use: rootn.exe EvpPlotServer.C
echo -------------------------------
#echo --- Launching ROOT session . . . 
#rootn.exe EvpPlotServer.C
