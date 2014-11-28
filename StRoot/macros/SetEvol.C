void SetEvol(int flag = 1)
{
  const char *onoff[] = {"off","on"};
  gROOT->GetListOfSpecials()->Add(new TNamed("IOEvol",onoff[flag]));
}  
