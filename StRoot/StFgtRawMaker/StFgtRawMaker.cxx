//
//
//
//
//
mEvent = (StEvent*)GetInputDS("StEvent");
mEvent->fgtRawCollection();

StRtsTable* rts_tbl = this->GetNextDaqElement("fgt/adc");
if(rts_tbl)
  mFgtRawData=(fgt_adc_t*)*rts_tbl.begin();
