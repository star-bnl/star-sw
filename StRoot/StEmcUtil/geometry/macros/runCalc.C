/*!\file runCalc.C
\author Alexandre Suaide

This macro brings up the GUI for the StEmcCalc
*/
void runCalc()
{
// Load needed shared libs
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("libglobal_Tables");
    gSystem->Load("libsim_Tables");
    gSystem->Load("libgen_Tables");
    gSystem->Load("StUtilities");
    gSystem->Load("StIOMaker");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StEvent");
    gSystem->Load("St_Tables");
    gSystem->Load("StDaqLib");
    gSystem->Load("StEmcUtil");
   
    StEmcCalc *c = new StEmcCalc(gClient->GetRoot(),556,328,gROOT);    
}
