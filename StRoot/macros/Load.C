// $Id: Load.C,v 1.18 2006/08/11 19:44:52 fisyak Exp $
//=======================================================================
// owner: Yuri Fisyak
// what it does: 
//=======================================================================

void Load(Char_t *loadList="St_base,StChain,StUtilities,StTriggerDataMaker,StEvent,StEventUtilities,StBFChain"){
  TString opt(loadList);
  TString separator = "[^ ;,]+";
  TObjArray *array = opt.Tokenize(separator);
  TIter next(array);
  while ((objs = (TObjString *) next())) {gSystem->Load(objs->GetString());}
  delete array;
}
