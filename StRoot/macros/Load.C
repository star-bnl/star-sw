// $Id: Load.C,v 1.19 2006/10/17 19:38:00 fisyak Exp $
//=======================================================================
// owner: Yuri Fisyak
// what it does: 
//=======================================================================

void Load(Char_t *loadList="St_base,St_Tables,StChain,StUtilities,StTriggerDataMaker,StEvent,StEventUtilities,StBFChain"){
  TString opt(loadList);
  TString separator = "[^ ;,]+";
  TObjArray *array = opt.Tokenize(separator);
  TIter next(array);
  while ((objs = (TObjString *) next())) {gSystem->Load(objs->GetString());}
  delete array;
}
