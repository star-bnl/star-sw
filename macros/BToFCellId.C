Int_t BTofCellId(Int_t tray, Int_t module, Int_t cell) {
  //  Int_t cellId = (aHit->tray() - 1) * 192 + (aHit->module() - 1) * 6 + (aHit->cell() - 1);
  Int_t cellId = (tray - 1) * 192 + (module - 1) * 6 + (cell - 1);
  return cellId;
}
