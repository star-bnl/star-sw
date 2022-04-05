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

Int_t AggregateVtxSeed(char* dir=0, const char* cuts="", const Int_t offset=0);

Int_t AggregateVtxSeed(char* dir, const char* cuts, const Int_t offset) {
  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();

  gSystem->Load("StDbBroker.so");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StDetectorDbMaker");
  gSystem->Load("StTpcDb");
  gSystem->Load("StDbUtilities");
  gSystem->Load("StdEdxY2Maker");
  gSystem->Load("StPicoEvent");
  gSystem->Load("StPicoDstMaker");
  gSystem->Load("StPass0CalibMaker");

  StVertexSeedMaker vtxSeedMk;
  //vtxSeedMk.UseFillDateTimeFromFile();
  //vtxSeedMk.SetNoClobber(kFALSE);
  Int_t nfiles = vtxSeedMk.Aggregate(dir,cuts,offset);
  return nfiles;
}

// $Id: AggregateVtxSeed.C,v 1.5 2017/08/22 17:32:13 genevb Exp $
// $Log: AggregateVtxSeed.C,v $
// Revision 1.5  2017/08/22 17:32:13  genevb
// Add Pico dependendence for StPass0CalibMaker lib
//
// Revision 1.4  2013/08/14 21:41:15  genevb
// Introduce time offsets, noclobber toggle
//
// Revision 1.3  2013/03/15 01:52:28  genevb
// library dependence
//
// Revision 1.2  2008/04/29 23:30:57  genevb
// Added cuts capability to Aggregate
//
// Revision 1.1  2006/05/09 21:48:45  genevb
// move macro to calib directory
//
// Revision 1.3  2005/07/01 23:57:40  genevb
// Allow use of StEvent/MuDst in finding vertex seed
//
// Revision 1.2  2003/02/11 21:43:15  genevb
// added needed library
//
// Revision 1.1  2002/03/20 00:51:42  genevb
// Introduction
//
