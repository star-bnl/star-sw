/*

  QAmergeRefHists.C
  Author:  G. Van Buren, BNL (Dec. 2010)
 
  Purpose: Merge two QA Reference histogram files oldFile & modFile into
           newFile. It does so by taking only those histograms listed in
           listFile from modFile, otherwise taking from oldFile.

*/

void QAmergeRefHists(char* listFile, char* oldFile, char* modFile, char* newFile) {

  gSystem->Load("St_base");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StUtilities");
  
  TFile oldFi(oldFile,"READ");
  TFile modFi(modFile,"READ");
  TFile newFi(newFile,"RECREATE");

  TList* oldKeys = oldFi.GetListOfKeys();
  TList* modKeys = modFi.GetListOfKeys();
  TList  newKeys;

  Char_t buffer[512];
  Int_t nkeys,i;

  nkeys = oldKeys->GetSize();
  for (i = 0; i < nkeys; i++) {
    newKeys.Add(oldKeys->At(i));
  }

  ifstream listFi(listFile);
  while (!listFi.eof()) {
    listFi >> buffer;
    TKey* modKey = (TKey*) (modKeys->FindObject(buffer));
    if (modKey) {
      TObject* oldKey = newKeys.FindObject(buffer);
      if (oldKey) {
        newKeys.AddAfter(oldKey,modKey);
        newKeys.Remove(oldKey);
      } else {
        newKeys.Add(modKey);
      }
    }
  }
  listFi.close();

  nkeys = newKeys.GetSize();
  for (i = 0; i < nkeys; i++) {
    TKey* key = (TKey*) (newKeys.At(i));
    TObject* obj = key->ReadObj();
    newFi.cd();
    obj->Write();
  }
  
  oldFi.Close();
  modFi.Close();
  //newFi.Close(); // causes a seg fault?

}

////////////////////////////////////////////////////////////////////////
// $Id: QAmergeRefHists.C,v 1.1 2010/12/23 01:10:09 genevb Exp $
// $Log: QAmergeRefHists.C,v $
// Revision 1.1  2010/12/23 01:10:09  genevb
// Introduce macro
//
//
