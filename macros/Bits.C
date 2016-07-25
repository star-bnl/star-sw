UInt_t Bits(UInt_t m, UInt_t bit, UInt_t nbits) {
  return (m >> bit) & ~(~0UL << nbits);
}
UInt_t NoSvtHits(UInt_t m) {
  return Bits(m,0,3);
}
UInt_t NoSsdHits(UInt_t m) {
  return Bits(m,3,3);
}
