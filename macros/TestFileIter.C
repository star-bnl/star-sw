// $Id: TestFileIter.C,v 1.1.1.1 2004/03/30 23:07:16 fisyak Exp $
// Author: Valery Fine(fine@bnl.gov)   01/03/2001
//
//////////////////////////////////////////////////////////////////////////
// This macros tests the various methods of TFileIter class.
//////////////////////////////////////////////////////////////////////////
//
// Copyright(c) 2001 [BNL] Brookhaven National Laboratory, Valeri Fine (fine@bnl.gov). All right reserved",
//
// Permission to use, copy, modify and distribute this software and its 
// documentation for any purpose is hereby granted without fee,
// provided that the above copyright notice appear in all copies and
// that both that copyright notice and this permission notice appear
// in supporting documentation.  The author makes no
// representations about the suitability of this software for any
// purpose.  It is provided "as is" without express or implied warranty.
//////////////////////////////////////////////////////////////////////////

void TestFileIter(){
// This macros tests the various methods of TFileIter class.
  gSystem->Load("libStar");

  //First create simple ROOT file
  TDataSet *ds = new TDataSet("event");
  TObject *nextObject = 0; 
  TRandom run;
  TRandom event;
  {
    TFileIter *outSet = new TFileIter("test.root","RECRETE");
    UInt_t totalEvent = 4;
    UInt_t runNumber  = 20010301;
    Int_t i=0;
    Int_t j=0;
    for (;j < 3;j++) {
      for (i = 1;i<totalEvent;i++) {
        outSet->NextEventPut(ds,UInt_t(i),UInt_t(runNumber+j+10*run.Rndm()-5));
      }
    }
    delete outSet;
  }
  printf(" ----------------------> TFile has been created <--------------------\n");
  TFile *f = new TFile("test.root");
  TFileIter readObj(f);
  // the number of the object available directly from "MyDataSet.root"
  Int_t size = readObj.TotalKeys();
  printf(" The total number of the objects: %d\n",size);

  //-----------------------------------------------------------------------
  // Loop over all objects, read them in to memory one by one

  printf(" -- > Loop over all objects, read them in to memory one by one < -- \n");
  for( readObj = 0; int(readObj) < size; readObj.SkipObjects() ){ 
      nextObject = *readObj; 
      printf(" %d bytes of the object \"%s\" of class \"%s\" written with TKey \"%s\"  has been read from file\n"
               ,readObj.GetObjlen()
               ,nextObject->GetName()
               ,nextObject->IsA()->GetName()
               ,(const char *)readObj
            );
      delete nextObject;
 }
//-----------------------------------------------------------------------
//  Now loop over all objects in inverse order
 printf(" -- > Now loop over all objects in inverse order < -- \n");
 for( readObj = size-1; (int)readObj >= 0; readObj.SkipObjects(-1))
 { 
      nextObject = *readObj; 
      if (nextObject) {
         printf(" Object \"%s\" of class \"%s\" written with TKey \"%s\"  has been read from file\n"
                ,nextObject->GetName()
                , nextObject->IsA()->GetName()
                ,(const char *)readObj
               );
        delete nextObject;
     } else {
       printf("Error reading file by index\n");
     }
 }
//-----------------------------------------------------------------------
// Loop over the objects starting from the object with the key name "event.02.01"
  printf(" -- > Loop over the objects starting from the object with the key name \"event.02.01\" < -- \n");
  for( readObj = "event.02.01"; (const char *)readObj != 0; readObj.SkipObjects()){ 
      nextObject = *readObj; 
      printf(" Object \"%s\" of class \"%s\" written with Tkey \"%s\"  has been read from file\n"
              , nextObject->GetName()
              , nextObject->IsA()->GetName()
              , (const char *)readObj
            );
      delete nextObject;
  }
// Loop over the objects starting from the object with the key name "event.02.01"
  printf(" -- > Loop over the objects starting from the 86-th object < -- \n");
  for( readObj = (const char *)(readObj = 86); (const char *)readObj != 0; readObj.SkipObjects()){ 
      nextObject = *readObj; 
      printf(" Object \"%s\" of class \"%s\" written with Tkey \"%s\"  has been read from file\n"
              , nextObject->GetName()
              , nextObject->IsA()->GetName()
              , (const char *)readObj
            );
      delete nextObject;
  }

}
