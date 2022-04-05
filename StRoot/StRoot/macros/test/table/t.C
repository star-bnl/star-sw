class StTab; StTab   *tab=0;
class Tab_st;

void t()
{
  gSystem->Load("tab.so");
  tab = new StTab("Tab",4);
  gROOT->GetListOfBrowsables()->Add(tab);


  printf(" Fill table \n");
  for(int i=0; i<4; i++) {
    Tab_st rec;
    rec.pt  = float(i);
    rec.gid = -i;
    printf(" %i rec.pt %5.2f rec.gid %i \n",  i, rec.pt,  rec.gid);
    tab->AddAt(&rec);
  }
  tab->Print(0,5);
  printf(" Get Info from table \n");
  for(int i=0; i<4; i++) {
    printf(" %i rec->pt %5.2f rec->gid %i \n",  i, tab->GetTable(i)->pt,  tab->GetTable(i)->gid );
  }

  new TBrowser;
}
