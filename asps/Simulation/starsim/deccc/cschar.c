char *getPntB(int myDif);

void cschar_(int *j,int *nh)
{
  char *c = getPntB(*j);
  *c = *nh;
}


int mkchar_(int *j)
{
  char *c = getPntB(*j);
  return *c;
}
