UInt_t bits(UInt_t hw, UInt_t bit, UInt_t nbits) {
  return (hw>>bit) & ~(~0UL<<nbits);
}
