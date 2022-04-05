//*-- Author :    Valery Fine(fine@bnl.gov)   25/09/99  
//
// Test "micky" for RMath class
// $Id: micky.C,v 1.1 1999/09/26 23:43:51 fine Exp $
// $Log: micky.C,v $
// Revision 1.1  1999/09/26 23:43:51  fine
// micky test for some CERMNLIB matrix packages
//
//
{
  // This test should produce an output as follows:
  //  root.exe [0] .x micky.C
  //  MICKY executing.
  //    MICKY    2.71  950712  9.30
  //  ---------------------------------------------------------------
  //  Routing TMXM     testing MXMPY-1-2-3.
  //  Tests pass 1    11    21    31    10    110    210    310    12    0    
  //  ---------------------------------------------------------------
  //  Routing TMXM     testing MXMAD-1-2-3.
  //  Tests pass 1    11    21    31    10    110    210    310    12    0    
  //  ---------------------------------------------------------------
  //  Routing TMXM     testing MXMUB-1-2-3.
  //  Tests pass 1    11    21    31    10    110    210    310    12    0    
  //  ---------------------------------------------------------------
  //  Routing TMXM     testing MXMLRT - MXMLTR.
  //  Tests pass 0    1    10    11    10    
  //  ---------------------------------------------------------------
  //  Routing TTRCHO   testing TRCHLU-TRCHUL.
  //  Tests pass 11    12    21    22    10    
  //  ---------------------------------------------------------------
  //  Routing TTRCHO   testing TRSMUL-TRSMLU.
  //  Tests pass 11    12    22    21    10    
  //  ---------------------------------------------------------------
  //  Routing TTRINV   testing TRINV -TRSINV.
  //  Tests pass 11    12    21    22    10    
  //  ---------------------------------------------------------------
  //  Routing TTRLA    testing TRLA-TRLTA-TRAL-TRALT.
  //  Tests pass 111    112    121    122    131    132    141    142    12    
  //  ---------------------------------------------------------------
  //  Routing TTRLA    testing TRAAT-TRATA.
  //  Tests pass 211    221    121    
  //  ---------------------------------------------------------------
  //  Routing TTRLA    testing TRASAT-TRATSA-TRQSQ.
  //  Tests pass 311    321    331    122    
  //  ---------------------------------------------------------------
  //  Routing TTRLA    testing TRSA-TRAS-TRSAT-TRATS.
  //  Tests pass 411    421    431    441    131    
  //  ---------------------------------------------------------------
  //  Routing TTRLA    testing TRPCK-TRUPCK.
  // 

  gSystem->Load("St_base");
  gSystem->Load("St_baseTest");
  StMicky micky;
  // Test F110
  micky.Tmxm();
  // Test F112
  micky.ttrcho();
  micky.ttrinv();
  micky.ttrla();
}
