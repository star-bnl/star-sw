St_DataSet *GetTableByType(const Char_t *type="dst_point") 
{
  // retur the name of the dataset object by its type
  St_DataSetIter next(chain,0)
  St_DataSet *t = 0;
  while (t = next()) { if (!strcmp(t->GetTitle(),type)) { cout << t->GetName() << endl; break; } }
  return t;
}

