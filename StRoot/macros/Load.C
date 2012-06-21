// $Id: Load.C,v 1.22 2012/06/21 19:00:06 fisyak Exp $
//=======================================================================
// owner: Yuri Fisyak
// what it does: 
//=======================================================================

void Load(Char_t *loadList="St_base,St_Tables,StChain,StDetectorDbMaker,StEvent,StTpcDb,StUtilities,StDbLib,StDbBroker,St_db_Maker,StTriggerDataMaker,StEventUtilities,StBFChain"){
  TString opt(loadList);
  TString separator = "[^ ;,]+";
  TObjArray *array = opt.Tokenize(separator);
  TIter next(array);
  while ((objs = (TObjString *) next())) {gSystem->Load(objs->GetString());}
  delete array;
}
