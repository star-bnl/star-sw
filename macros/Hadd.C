/*
  foreach s (`ls -1 *.root | awk -F_ '{print $1"_"$2"_"$3}' | sort -u`)
  root.exe -q -b 'Hadd.C+("'$s'.Cl.root","'$s'*raw*.Cl.root")'
  end

  foreach s (`ls -1 *.root | awk -F_ '{print $1"_"$2"_"$3"_"$4}' | sort -u`)
  root.exe -q -b 'Hadd.C+("'$s'.ClnoW.root","'$s'*raw*.ClNoW.root")'
  end

  foreach s (`ls -1 *Cl.root | awk -F\. '{print $1"."$2}' | sort -u`)
  root.exe -q -b 'Hadd.C+("'$s'.Fit8.root","'$s'*.s*.Fit8.root")'
  end
  foreach s (`ls -1 *ClNoW.root | awk -F\_ '{print $1"_"$2"_"$3"_"$4}' | awk -F\. '{print $1}' | sort -u `)
  root.exe -q -b 'Hadd.C+("'$s'.ClnoW.root","'$s'*raw*.root")'
  end
foreach s ( `ls -1d *.root | awk -F_ '{print $4}' | sort -u` )
root.exe -q -b lMuDst.C 'Hadd.C+("'$s'.ClnoW.root","~/tfg/reco/2014/50M/SL15StiCAKFV/108*'$s'*raw*.root")'

  set f = 15108011_raw_1000001 ; set r = `echo ${f} | awk -F_ "{print $1} set m = st_physics_${f}.MuDst.root ; root.exe -q -b lMuDst.C 'Hadd.C+("'${m}'","~/tfg/reco/2014/50M/SL15StiCAKFV/108/15108011/'${r}'/st_*'${f}'_*.MuDst.root")' | tee tee hadd_${f}.log
end

*/
   
#include "RConfig.h"
#include <string>
#include "TFile.h"
#include "THashList.h"
#include "TKey.h"
#include "TObjString.h"
#include "Riostream.h"
#include "TClass.h"
#include "TSystem.h"
#include <stdlib.h>
#include "TDirIter.h"
#include "TFileMerger.h"
   
Int_t Hadd(const Char_t *targetname="Out.root", const Char_t *files = "*noW.root", Bool_t force=kTRUE ) {
  TDirIter Dir(files);
  //  Bool_t force = kFALSE;
  Bool_t skip_errors = kFALSE;
  Bool_t reoptimize = kFALSE;
  Bool_t noTrees = kFALSE;
  Int_t maxopenedfiles = 0;
  Int_t verbosity = 99;
  Int_t newcomp = 1;
  gSystem->Load("libSt_base");
  gSystem->Load("libStChain");
  gSystem->Load("libTreePlayer");
  
  if (verbosity > 1) {
    std::cout << "hadd Target file: " << targetname << std::endl;
  }
  
  TFileMerger merger(kFALSE,kFALSE);
  merger.SetMsgPrefix("hadd");
  merger.SetPrintLevel(verbosity - 1);
  if (maxopenedfiles > 0) {
    merger.SetMaxOpenedFiles(maxopenedfiles);
  }
  if (!merger.OutputFile(targetname,force,newcomp) ) {
    std::cerr << "hadd error opening target file (does " << targetname << " exist?)." << std::endl;
    std::cerr << "Pass \"-f\" argument to force re-creation of output file." << std::endl;
    exit(1);
  }
  
  Char_t *file = 0;
  while ((file = (Char_t *) Dir.NextFile())) {
    if( ! merger.AddFile(file) ) {
      if ( skip_errors ) {
	std::cerr << "hadd skipping file with error: " << file << std::endl;
      } else {
	std::cerr << "hadd exiting due to error in " << file << std::endl;
	return 1;
      }
    }
  }
  if (reoptimize) {
    merger.SetFastMethod(kFALSE);
  } else {
    if (merger.HasCompressionChange()) {
      // Don't warn if the user any request re-optimization.
      std::cout <<"hadd Sources and Target have different compression levels"<<std::endl;
      std::cout <<"hadd merging will be slower"<<std::endl;
    }
  }
  merger.SetNotrees(noTrees);
  Bool_t status = merger.Merge();
  
  if (status) {
    if (verbosity == 1) {
      std::cout << "hadd merged " << merger.GetMergeList()->GetEntries() << " input files in " << targetname << ".\n";
    }
    return 0;
  } else {
    if (verbosity == 1) {
      std::cout << "hadd failure during the merge of " << merger.GetMergeList()->GetEntries() << " input files in " << targetname << ".\n";
    }
    return 1;
  }
}
