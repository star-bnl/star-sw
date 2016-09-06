/* 
   root.exe AdcTpcTanL.C
 */
void AdcTpcTanL(const Char_t *files="*.root", const Char_t *Out = "") {
  gROOT->LoadMacro("TpcT.C+");
  TpcTAdcTanL(files,Out);
}
