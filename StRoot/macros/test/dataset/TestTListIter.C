// $Id: TestTListIter.C,v 1.1 2004/10/06 16:45:37 fisyak Exp $
// Author: Valery Fine(fine@bnl.gov)   01/03/2001
//
//////////////////////////////////////////////////////////////////////////
// This macros tests the various methods of TFileIter class.
//////////////////////////////////////////////////////////////////////////
//
// Copyright(c) 2001 [BNL] Brookhaven National Laboratory, Valeri Fine (fine@bnl.gov).
// All right reserved"
//
// Permission to use, copy, modify and distribute this software and its 
// documentation for any purpose is hereby granted without fee,
// provided that the above copyright notice appear in all copies and
// that both that copyright notice and this permission notice appear
// in supporting documentation.  The author makes no
// representations about the suitability of this software for any
// purpose.  It is provided "as is" without express or implied warranty.
//////////////////////////////////////////////////////////////////////////

void TestTListIter(){
// This macros tests the various methods of TFileIter class.
  gSystem->Load("libRootKernel");
  TList testList;
  TString member = "member";
  int i = 0;
  for (i = 0; i < 10; i++) {
   // Fill list
     TString nextM = member;
     nextM += i;
     testList.Add(new TNamed(nextM,"Title"));
  }
  testList.ls();
  TLIST::iterator iter(&testList);
  i = 0;
  while (*iter != 0 ) {
     printf(" Name =  %s\n", (*iter)->GetName() );
     iter++; i++;
  }
  if ( i == 10) printf(" Ok ! \n");
  else printf(" Failed ! \n");
}
