void MakerList(StMaker *chain = 0) {
  if (! chain) return;
   TList *tl = chain->GetMakeList();
   if (!tl) return;
   TIter nextMaker(tl);
   StMaker *maker;
   cout << chain->GetName() << endl;
   while ((maker = dynamic_cast<StMaker*>(nextMaker()))) {
     if (!maker->IsActive()) continue;
     cout << "\t" << maker->GetName() << endl;
   }  
}
