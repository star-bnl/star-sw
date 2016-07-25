struct Point_t {
  Float_t x;
  Float_t y;
};
Point_t Point;
void pixels(Char_t *FileName="pixels") {
  f = new TFile("PointT.root","RECREATE");
  PointP = new TNtuple("PointP","x and y from","x:y");
  FILE *fp = fopen(FileName,"r");
  char line[121];
  Int_t i = 0, j = 0;
  while (fgets(&line[0],120,fp)) {
    Point.x = i;
    sscanf(&line[0] ,"Pixel count = %i",&j);
    Point.y = j;
    PointP->Fill(&Point.x);
    i++;
    if (i%1000 == 1) cout << "i:j:\t" << i << "\t:\t" << j << "\t" << line;
  }
  fclose(fp);
  f->Write();
}
