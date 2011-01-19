// $Id: Load.C,v 1.20 2011/01/19 20:06:48 fisyak Exp $
//=======================================================================
// owner: Yuri Fisyak
// what it does: 
//=======================================================================

void Load(Char_t *loadList="St_base,St_Tables,StChain,StTpcDb,StUtilities,StDbLib,StDbBroker,St_db_Maker,StDetectorDbMaker,StTriggerDataMaker,StEvent,StEventUtilities,StBFChain"){
  TString opt(loadList);
  TString separator = "[^ ;,]+";
  TObjArray *array = opt.Tokenize(separator);
  TIter next(array);
  while ((objs = (TObjString *) next())) {gSystem->Load(objs->GetString());}
  delete array;
}
