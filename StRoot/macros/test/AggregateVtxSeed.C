//////////////////////////////////////////////////////////////////////////
//                                                                      //
// AggregateVtxSeed.C macro                                             //
// Author: G. Van Buren, BNL                                            //
// Description: uses StVertexSeedMaker to aggregate results of          //
//              vertex seed-finding from multiple runs within fills     //
// Usage: Reads histograms from directory given as argument. A blank    //
//        argument will use the directory in which the macro is run.    //
//        A directory name should end in a forward slash "/" for all    //
//        files named vertexseedhist.*.root in that directory to be     //
//        used (otherwise assumes you're actually specifying the        //
//        filenames). Output histograms go to the directory in which    //
//        the macro is run, and output database tables go to subdir:    //
//        StarDb/Calibrations/rhic/                                     //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

Int_t AggregateVtxSeed(char* dir=0);

Int_t AggregateVtxSeed(char* dir) {
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");
  gSystem->Load("StUtilities");
  gSystem->Load("StPass0CalibMaker");

  StVertexSeedMaker vtxSeedMk;
  Int_t nfiles = vtxSeedMk.Aggregate(dir);
  return nfiles;
}

// $Id: AggregateVtxSeed.C,v 1.1 2002/03/20 00:51:42 genevb Exp $
// $Log: AggregateVtxSeed.C,v $
// Revision 1.1  2002/03/20 00:51:42  genevb
// Introduction
//
