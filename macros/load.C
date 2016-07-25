// $Id: load.C,v 1.3 2013/11/12 20:00:34 fisyak Exp $
//=======================================================================
// owner: Yuri Fisyak
// what it does: 
//=======================================================================

//void load(Char_t *loadList = "St_base,StChain,StUtilities,StarClassLibrary,StTriggerDataMaker,StEvent,StEventUtilities,StBFChain,St_Tables,StMcEvent,StDbUtilities,StTpcMcAnalysisMaker"){
void load(Char_t *loadList = "libTable,St_base,StChain,StUtilities,St_Tables,StDbLib,StDbBroker,St_db_Maker,StEvent"){
//   if (gClassTable->GetID("TTable") < 0) {
//     gSystem->Load("libGeom");
//     gSystem->Load("libTable");
//   }
  TString opt(loadList);
  TString separator = "[^ ;,]+";
  TObjArray *array = opt.Tokenize(separator);
  TIter next(array);
  while ((objs = (TObjString *) next())) {gSystem->Load(objs->GetString());}
  delete array;
}
