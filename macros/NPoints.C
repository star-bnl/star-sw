UInt_t NPoints(Int_t NPackPoints, Int_t detector=0) {
  // Unpack NPackPoints = NoTpc + 100*(NoSvt + 10*NoSsd)
  Int_t NoTpc = NPackPoints%100;
  Int_t NoSvt = (NPackPoints/100)%10;
  Int_t NoSsd = NPackPoints/1000;
  if (detector == 1) return NoTpc;
  if (detector == 2) return NoSvt;
  if (detector == 3) return NoSsd;
  return NoSvt+NoSsd;
}
