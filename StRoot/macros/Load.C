// $Id: Load.C,v 1.21 2011/02/01 23:52:53 genevb Exp $
//=======================================================================
// owner: Yuri Fisyak
// what it does: 
//=======================================================================

void Load(Char_t *loadList="St_base,St_Tables,StChain,StDetectorDbMaker,StTpcDb,StUtilities,StDbLib,StDbBroker,St_db_Maker,StTriggerDataMaker,StEvent,StEventUtilities,StBFChain"){
  TString opt(loadList);
  TString separator = "[^ ;,]+";
  TObjArray *array = opt.Tokenize(separator);
  TIter next(array);
  while ((objs = (TObjString *) next())) {gSystem->Load(objs->GetString());}
  delete array;
}
