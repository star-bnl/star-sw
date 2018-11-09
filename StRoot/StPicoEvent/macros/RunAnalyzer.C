/**
 * \brief Helper script for runing picoDst analysis calling PicoDstAnalyzer.C
 *
 *  This macros takes inFileName argument with a picoDst.root file
 *  or with a list of files (name.lis or name.list). It sets _VANILLA_ROOT_
 *  (necessary for standalone and can be skipped on RACF), loads pre compiled
 *  libStPicoDst.so (from StPicoEvent), compiles and executes a text
 *  PicoDstAnalyzer.C macro with passing inFileName to it, and
 *  cleans up the directory from the compilation products at the end.
 *
 *  Some details:
 *    inFileName - is a name of name.picoDst.root file or a name
 *                 of a name.lis(t) files that contains a list of
 *                 name1.picoDst.root files.
 *    NOTE: inFileName should contain either /absolutePath/inFileName
 *          or /relative2currentDir/inFileName
 *  It is assumed that PicoDstAnalyzer.C is placed in the same
 *  directory where the RunAnalyzer.C is stored.
 *
 * \author Grigory Nigmatkulov
 * \date July 5, 2018
 */

// ROOT headers
#include "TROOT.h"
#include "TSystem.h"
#include "TString.h"

//_________________
void RunAnalyzer(const Char_t *inFileName = "../files/st_physics_12126101_raw_3040006.picoDst.root") {
  // Next line is not needed if you are not running in a standalone mode
  gROOT->ProcessLine("#define _VANILLA_ROOT_");
  gSystem->Load("../libStPicoDst.so");
  TString str;
  str = ".x PicoDstAnalyzer.C+(\"";
  str += inFileName;
  str += "\")";
  gROOT->ProcessLine( str.Data() );
  // Next line should be commented if you run in a batch mode
  gROOT->ProcessLine(".!rm -f PicoDstAnalyzer_C* ");
}
