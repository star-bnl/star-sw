Int_t LdStaf(Char_t *libs)
{
  Char_t buffer[300]="GEN/lib/";
  strcat(buffer,libs);
  return    gSystem.Load(buffer);
}
