// $Id: StarPalette.C,v 1.3 2000/01/10 15:21:08 kathy Exp $
// $Log: StarPalette.C,v $
// Revision 1.3  2000/01/10 15:21:08  kathy
// updated the *Palette macros for changes in ROOT (see email to software-l from Valery on 7jan00)
//
// Revision 1.2  1999/06/25 19:17:26  kathy
// fix the Palette macros so that you can run StarPalette directly from TestStarPalette - thanks, Thoams & Gene!
//
// Revision 1.1  1999/06/24 20:42:07  kathy
//  Jon Gans' macro to setup star color palette
//
//
//======================================================================
// owner: Jon Gans/Kathy Turner (took over 10jan00)
// what it does: 
//      Sets up STAR-specific color palette.
//      One should execute this at beginning of the root session
//      Then all the colors in order will be in order of spectrum &
//      darkest to lightest.
//
//    10jan00(KT) --> 
//       This color palette is now implemented automatically
//       by ROOT, so one doesn't need to use this!   However, I leave
//       it here as an example!!
//
//       To show how this macro actually works, run TestStarPalette.C!
//
//=======================================================================

void StarPalette()
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
      gVirtualX->GetRGB(index, rv, gv, bv);
      if (r != rv || g != gv || b != bv) {
          failures++;
          palette[i] =  i ? palette[i-1] : 1;
      }
  }
  if (failures)
      printf("StarPalette(): couldn't allocate %d of %d colors\n", failures, MaxColors);
  gStyle->SetPalette(MaxColors, palette);
}




