
void main()
{
    StiCompositeTreeNode<MyFoo> root;
    
    //... code to add daughters to root ...
    
    StiCompositeLeafIterator<MyFoo> leafIt(root);
    copy(leafIt.const_begin(), leafIt.const_end(),
	 ostream_iterator(cout, " ")); //Stream nodes to screen
    //Now Stream first leaf:
    cout <<"First leaf: "<<*(leafIt.const_begin());

    return 1;
}
