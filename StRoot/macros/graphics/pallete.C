void palette()
{  
  const float  saturation = 1;
  const float  lightness = 0.5;
  const float  MaxHue = 280;
  const float  MinHue = 0;
  const int    MaxColors = 50;   
  int          palette[MaxColors];
  int          index;
  float        hue, r, g, b, rv, gv, bv;
  TColor       *color;
  unsigned int failures = 0;
  
  for (int i=0 ; i<MaxColors ; i++) {
      index = palette[i] = MaxColors+1+i;     
      color = new TColor(index, 0, 0, 0);
      hue = MaxHue-(i+1)*((MaxHue-MinHue)/MaxColors);
      color->HLStoRGB(hue, lightness, saturation, r, g, b);
      color->SetRGB(r, g, b);
      gGXW->GetRGB(index, rv, gv, bv);
      if (r != rv || g != gv || b != bv) {
          failures++;
          palette[i] =  i ? palette[i-1] : 1;
      }
  }
  if (failures)
      printf("palette(): couldn't allocate %d of %d colors\n", failures, MaxColors);
  gStyle->SetPalette(MaxColors, palette);
}
