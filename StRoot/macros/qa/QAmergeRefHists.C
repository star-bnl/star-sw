/*

  QAmergeRefHists.C
  Author:  G. Van Buren, BNL (Dec. 2010)
 
  Purpose: Merge two QA Reference histogram files oldFile & modFile into
           newFile. It does so by taking only those histograms listed in
           listFile from modFile, otherwise taking from oldFile.

*/

void QAmergeRefHists(char* listFile, char* oldFile, char* modFile, char* newFile) {

  // STAR libs needed for StMultiH*F histograms
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
  TString histName,modName;
  TMap modNames;
  Int_t nkeys,i;

  nkeys = oldKeys->GetSize();
  // Start with the old keys
  for (i = 0; i < nkeys; i++) newKeys.Add(oldKeys->At(i));

  ifstream listFi(listFile);
  while (listFi.good()) {
    listFi >> buffer;
    histName = buffer;
    Bool_t modifyName = histName.BeginsWith("__");
    if (modifyName) {
      // Will modify the histogram name
      //  to whatever is between __*__x__
      Ssiz_t endNewName = histName.Index("__x__",5,2);
      if (endNewName > 0 && endNewName < histName.Length()-5) {
        modName = histName(2,endNewName-2);
        histName.Remove(0,endNewName+5);
      } else modifyName = kFALSE;
    }
    TKey* modKey = (TKey*) (modKeys->FindObject(histName.Data()));
    if (!modKey) {
      histName.ReplaceAll('_',' ');
      if (modifyName) modName.ReplaceAll('_',' ');
      modKey = (TKey*) (modKeys->FindObject(histName.Data()));
    }
    if (modKey) {
      // Don't add the same key twice
      if (newKeys.FindObject(modKey)) continue;

      // Look to see if we're replacing an old one
      TObject* oldKey = newKeys.FindObject(modifyName ?
                          modName.Data() : histName.Data());
      if (oldKey) {
        newKeys.AddAfter(oldKey,modKey);
        newKeys.Remove(oldKey);
      } else {
        newKeys.Add(modKey);
      }
      if (modifyName) modNames.Add(modKey, new TNamed(modName,modName));
    }
  }
  listFi.close();

  // Read and write the actual histogram objects
  nkeys = newKeys.GetSize();
  for (i = 0; i < nkeys; i++) {
    TKey* key = (TKey*) (newKeys.At(i));
    TObject* obj = key->ReadObj();
    TObject* modValue = modNames.GetValue(key);
    // Modify name if necessary
    if (modValue) ((TNamed*) obj)->SetName(modValue->GetName());
    newFi.cd();
    obj->Write();
  }
  
  oldFi.Close();
  modFi.Close();
  //newFi.Close(); // causes a seg fault?

}

////////////////////////////////////////////////////////////////////////
// $Id: QAmergeRefHists.C,v 1.4 2012/05/01 20:44:16 genevb Exp $
// $Log: QAmergeRefHists.C,v $
// Revision 1.4  2012/05/01 20:44:16  genevb
// Account for when underscore represent space
//
// Revision 1.3  2011/03/15 23:02:12  genevb
// Use __ delimeters, more careful about modifier spec
//
// Revision 1.2  2011/02/25 23:00:47  genevb
// Allow histogram name modification
//
// Revision 1.1  2010/12/23 01:10:09  genevb
// Introduce macro
//
//
