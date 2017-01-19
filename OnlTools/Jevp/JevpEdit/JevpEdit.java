
import org.w3c.dom.*;
import java.io.*;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import java.util.*;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.tree.*;
import javax.swing.table.*;
import javax.swing.event.*;
import java.awt.dnd.*;
import java.awt.datatransfer.*;

class JNode {
    String name;
    JNode prev;
    JNode next;
    JNode parent;
    JNode child;
    boolean leaf;
    Hashtable<String,String> properties;
    
    public String toString() {
	return name;
    }

    JNode()
    {
	properties = new Hashtable<String,String>();

	name = null;
	prev = null;
	next = null;
	parent = null;
	child = null;
	leaf = false;
    }

    void addProperty(String property, String value)
    {
	properties.put(property, value);
    }

    void deleteProperties() {
      
	properties.clear();

	properties = new Hashtable<String,String>();
    }

    String getProperty(String property)
    {
	String value = properties.get(property);
	return value;
    }

    String getPropertyKey(int i) {
	int x = 0;
	for(Enumeration e = properties.keys() ; e.hasMoreElements(); x++) {
	    String ret = (String)e.nextElement();
	    if(x == i) return ret;
	}
	return null;
    }
    
    void setName(String name) {
	this.name = name;
    }

    void dump() {
	dump(this, "");
    }

    static void dump(JNode n, String pre)
    {
	String name = n.name;
	System.out.printf("%s/%s\n",pre,name);
       
	for(int i=0;;i++) {
	    String key = n.getPropertyKey(i);
	    if(key == null) break;
	    System.out.printf("\t\t\t%s --> %s\n", key, n.getProperty(key));
	}

	if(n.child != null) {
	    if(name.equals("")) {  
		dump(n.child, pre + name);
	    }
	    else 
		dump(n.child, pre + "/" + name);
	}

	if(n.next != null) {
	    dump(n.next, pre);
	}
    }


    // Everything below is for converting the XML tree to a JNode tree.
    static void dumpallXml(Node n, String pre)
    {
	String name = n.getNodeName();
	
	String val = n.getNodeValue();
	if(val == null) val = "";
	val = val.trim();

	if(!(name.equals("#text") && val.equals(""))) {
	    System.out.printf("%s Node %s : %s\n",pre, name, val);
	}


	Node c = n.getFirstChild();

	if(c != null) {
	    dumpallXml(c, pre + "   ");
	}

	Node s = n.getNextSibling();
	if(s != null) {
	    dumpallXml(s, pre);
	}
    }

    static String findText(Node xmlNode) {
	Node curr = xmlNode.getFirstChild();
	while(curr != null) {
	    if(curr.getNodeName().equals("#text")) {
		String val = curr.getNodeValue();
		if(val == null) val = "";
		val = val.trim();
		return val;
	    }

	    curr = curr.getNextSibling();
	}
	return "";
    }

    static void addProperties(JNode me, Node xmlNode) {
	Node curr = xmlNode.getFirstChild();
	while(curr != null) {
	    String n = curr.getNodeName();
	    
	    if(!(n.equals("#text") ||    // these possibilities are not properties, but all else are...
		 n.equals("tab") ||
		 n.equals("display_def") ||
		 n.equals("doc") ||
		 n.equals("histogram"))) {
		
		Node tt = curr.getFirstChild();
		String val = tt.getNodeValue();
		if(val == null) val = "";
		val = val.trim();
		
		me.addProperty(n, val);
	    }
	    
	    curr = curr.getNextSibling();
	    
	}
    }

  
	
    static void addChildren(JNode me, Node xmlNode) {
	    
	JNode cnode = null;
	
	Node curr = xmlNode.getFirstChild();
	while(curr != null) {
	    String n = curr.getNodeName();

	    if(n.equals("tab") ||
	       n.equals("display_def") ||
	       n.equals("doc") ||
	       n.equals("histogram")) {
		   
		JNode nnode = createNode(curr);
		nnode.parent = me;
		nnode.prev = cnode;
		if(me.child == null) me.child = nnode;
		if(cnode != null) cnode.next = nnode;
		cnode = nnode;

		if(n.equals("histogram")) cnode.leaf = true;
	    }	    
	    curr = curr.getNextSibling();
	}
    }

 
    
    static JNode createNode(Node xmlNode)
    {
	String mytype = xmlNode.getNodeName();

	if(!(mytype.equals("doc") ||
	     mytype.equals("display_def") ||
	     mytype.equals("tab") ||
	     mytype.equals("histogram"))) {
	    System.out.printf("This is an error.  Illegal type: %s\n",mytype);
	    return null;
	}

	//System.out.printf("mytype = %s\n",mytype);

	JNode me = new JNode();
	
	String myname = findText(xmlNode);
	me.setName(myname);
	addProperties(me, xmlNode);
	addChildren(me, xmlNode);

	return me;
    }

    static JNode createPallete(Node xmlNode)
    {
	JNode root = new JNode();
	root.setName("pallete");
	
	Node curr = xmlNode.getFirstChild();
	if(curr == null) return root;

	while(!curr.getNodeName().equals("pallete")  ) {
	    curr = curr.getNextSibling();
	    if(curr == null) return root;
	}


	Node ctab = curr.getFirstChild();
	JNode pnode = null;
	while(ctab != null) {
	    String n = ctab.getNodeName();
	    
	    if(n.equals("tab")) {
		String tname = findText(ctab);
		JNode tnode = new JNode();
		tnode.setName(tname);
		tnode.parent = root;
		if(pnode == null) 
		    root.child = tnode; 
		else
		    pnode.next = tnode;
		
		tnode.prev = pnode;
		pnode = tnode;
		tnode.leaf = false;
		

		JNode cHistNode = null;
		
		// Add pallete histograms
		curr = ctab.getFirstChild();
		while(curr != null) {
		    n = curr.getNodeName();
		    
		    if(n.equals("histogram")) {
			String hname = findText(curr);
			
			JNode nnode = new JNode();
			nnode.setName(hname);
			//		addProperties(nnode, curr);
			
			// link up...
			nnode.parent = tnode;
			if(cHistNode == null) {
			    cHistNode = nnode;
			    tnode.child = nnode;
			}
			else {
			    cHistNode.next = nnode;
			    nnode.prev = cHistNode;
			}
			nnode.leaf = true;
			cHistNode = nnode;
			
			
		    }
		    
		    curr = curr.getNextSibling();
		}
	    }

	    ctab = ctab.getNextSibling();
	}
	    
	return root;
    }

    void spaces(PrintStream out, int n) throws Exception
    {
	for(int i=0;i<n;i++) out.printf(" ");
    }


    void printProperties(PrintStream out, int depth) throws Exception {
	for(int i=0;;i++) {
	    String key = getPropertyKey(i);
	    if(key == null) break;
	    
	    spaces(out, depth*3+2);
	    out.printf("<%s>%s</%s>\n",key,getProperty(key),key);
	}
    }

    void writeXML(PrintStream out, int depth, boolean nocanvas) throws Exception {
	
	if(depth == 0) {   // this tab is a display def
	    out.printf("<display_def>%s\n", name);
	}
	else if(leaf) {    // its a histo!
	    spaces(out, depth*3);
	    out.printf("<histogram>%s\n", name);
	    printProperties(out, depth);
	    spaces(out, depth*3);
	    out.printf("</histogram>\n");

	    if(next != null) {
		next.writeXML(out, depth, nocanvas);
	    }

	    return;    
	}
	else {   // its a true tab
	    spaces(out, depth*3);
	    out.printf("<tab>%s\n",name);

	    // System.out.printf("tab %s\n",name);
	}

	if((child != null) && (child.leaf) && !nocanvas) {   // got a batch of histos, write out the canvas...
	    //spaces(out, depth*3+3);
	    //out.printf("<canvas>\n");
	    printProperties(out, depth+1);
	    
	    child.writeXML(out, depth+2, nocanvas);

	    //xxxxspaces(out, depth*3+3);
	    //out.printf("</canvas>\n");
	}
	else {
	    printProperties(out, depth);
	    if(child != null) {
		child.writeXML(out, depth+1, nocanvas);
	    }
	}
	
	if(depth == 0) {
	    out.printf("</display_def>\n");
	}
	else {
	    spaces(out, depth*3);
	    out.printf("</tab>\n");
	}

	if(next != null) {
	    next.writeXML(out, depth, nocanvas);
	}
    }
}


class PropertiesTableModel extends DefaultTableModel 
{
    JNode node;

    Vector<String> keys;
    Vector<String> values;

    PropertiesTableModel(JNode node) {
	this.node = node;

	//	addTableModelListener(this);

	keys = new Vector<String>();
	values = new Vector<String>();

	for(int i=0;i<10;i++) {
	    String v;
	    String k = node.getPropertyKey(i);
	    if(k == null) {
		k = "";
		v = "";
	    }
	    else {
		v = node.getProperty(k);
		if(v == null) v = "";
	    }

	    keys.add(k);
	    values.add(v);
	}
    }

    public int getColumnCount() { 
	return 2;
    }

    public int getRowCount() {
	if(keys == null) return 0;
	return keys.size();
    }

    public Object getValueAt(int row, int col) {
	if(col == 0) return keys.elementAt(row);
	else return values.elementAt(row);
    }

    public void setValueAt(Object o, int row, int col) {
	if(col == 0) keys.set(row,(String)o);
	else values.set(row,(String)o);
    }

    public boolean isCellEditable(int row, int col)
    {
	return true;
    }
}



class PropertyFrame extends JFrame
{
    JNode node;
    PropertiesTableModel model;
    JTable table;
    JTextField namefield;
    JLabel namelabel;

    public PropertyFrame(JNode node, int x, int y)
    {
	this.node = node;	

// 	addWindowListener(new WindowAdapter() {
//                 public void windowClosing(WindowEvent we) { updateNodeProperties(); }
//             });

	model = new PropertiesTableModel(node);
	

	JPanel namep = new JPanel();

	boolean en;

	if(node.leaf) {
	    en = false;
	}
	else {
	    en = true;
	}

	namefield = new JTextField(node.name);
	namefield.setPreferredSize(new Dimension(250,24));
	namelabel = new JLabel("Properties for: ");
	namep.add(namelabel, BorderLayout.WEST);
	namep.add(namefield, BorderLayout.CENTER);
	namefield.setEnabled(en);

	JScrollPane sp = new JScrollPane();
	sp.setPreferredSize(new Dimension(300,500));
	table = new JTable(model);
	sp.getViewport().add(table);
	getContentPane().add(namep, BorderLayout.NORTH);
	getContentPane().add(sp, BorderLayout.CENTER);

// 	JPanel p = new JPanel(new GridLayout(1,0));
// 	JButton button;
// 	button = new JButton("Add");
// 	button.addActionListener(this);
// 	p.add(button);
// 	button = new JButton("Delete");
// 	button.addActionListener(this);
// 	p.add(button);
// 	button = new JButton("Ok");
// 	button.addActionListener(this);
// 	p.add(button);
// 	getContentPane().add(p, BorderLayout.SOUTH);

	setLocation(x,y);
	pack();
	setVisible(true);
    }    
    
//     public void actionPerformed(ActionEvent e)
//     {
// 	// System.out.printf("%s\n",e.getActionCommand());

// // 	if(e.getActionCommand().equals("Add")) {
// // 	    model.keys.add("");
// // 	    model.values.add("");
// // 	    model.fireTableDataChanged();
// // 	}

// // 	if(e.getActionCommand().equals("Delete")) {
// // 	    int i = table.getSelectedRow();
// // 	    // System.out.printf("i=%d\n",i);
// // 	    if(i >= 0) {
// // 		model.values.remove(i);
// // 		model.keys.remove(i);
// // 	    }
// // 	    model.fireTableDataChanged();
// // 	}
	   
// // 	if(e.getActionCommand().equals("Ok")) {
// // 	    updateNodeProperties();
// // 	}
//     }
    
    void useNewNode(JNode node) {
	this.node = node;
	if(node == null) {
	    namefield.setText("none");
	    namefield.setEnabled(false);
	    return;
	}

	String labeltext;
	if(node.leaf) {
	    namefield.setText(node.name);
	    namefield.setEnabled(false);
	    // namelabel.setText("Properties for HISTOGRAM:  ");
	}
	else {
	    namefield.setText(node.name);
	    namefield.setEnabled(true);
	    //namelabel.setText("Properties for TAB:  ");
	}
	namefield.setPreferredSize(new Dimension(250,24));

	model = new PropertiesTableModel(node);
	table.setModel(model);
    }

    void saveNode() {
	// System.out.printf("Hello there...\n");
	if(node == null) return;

	if(namefield != null) {
	    node.setName(namefield.getText());
	}
	node.deleteProperties();
	for(int i=0;i<model.keys.size();i++) {
	    if(model.keys.elementAt(i).equals("")) continue;
	    node.addProperty(model.keys.elementAt(i), model.values.elementAt(i));
	}
	
	//dispose();
    }			     
}

public class JevpEdit extends JFrame implements ActionListener, TreeSelectionListener
{  
    String basedir;
    JNode treedata;
    JTree tree;
    DefaultMutableTreeNode root;
    DefaultTreeModel treeModel;

    JNode palleteData;
    JTree palleteTree;
    DefaultMutableTreeNode palleteRoot;
    DefaultTreeModel palleteModel;

    PropertyFrame myPropertyFrame;

    JPopupMenu popup;
    
    JevpEdit() {
	super("Jevp Histogram Presentation Editor");

	setSize(900,800);
	setLocation(100,100);
	//addWindowListener(new BasicWindowMonitor());    
	addWindowListener(new WindowAdapter() {
                public void windowClosing(WindowEvent we) { System.exit(0); }
            });

    }
	
    void insertChildren(JNode node, DefaultMutableTreeNode dispnode) {
	JNode curr = node.child;

	int i=0;
	while(curr != null) {
	    DefaultMutableTreeNode ndisp = new DefaultMutableTreeNode(curr);
	    treeModel.insertNodeInto(ndisp, dispnode, i);
	    insertChildren(curr, ndisp);
	    
	    i++;
	    curr = curr.next;
	}
    }

    public void init() {
	popup = new JPopupMenu();
	JMenuItem m = new JMenuItem("new sibling");
	m.addActionListener(this);
	popup.add(m);
	m = new JMenuItem("new child");
	popup.add(m);
	m.addActionListener(this);
	m = new JMenuItem("delete");
	m.addActionListener(this);
	popup.add(m);
	m = new JMenuItem("group in subdir");
	m.addActionListener(this);
	popup.add(m);
	m = new JMenuItem("check children");
	m.addActionListener(this);
	popup.add(m);

	root = new DefaultMutableTreeNode(treedata);
	treeModel = new DefaultTreeModel(root);
	tree = new JTree(treeModel);
	tree.setCellRenderer(new JCellRenderer());
	tree.addTreeSelectionListener(this);
	tree.setToggleClickCount(2);
	insertChildren(treedata, root);
	// expand
	DefaultMutableTreeNode x = (DefaultMutableTreeNode)root.getLastLeaf().getParent();
	if(x == null) System.out.printf("Shit/\n");
	else tree.expandPath(new TreePath(x.getPath()));

	addMouseListener(new MouseAdapter() {
		public void mouseClicked(MouseEvent me) {
		    something(me);
		}

		public void mousePressed(MouseEvent me) {
		    something(me);
		}

		public void mouseReleased(MouseEvent me) {
		    something(me);
		}
	    });

	tree.clearSelection();
	tree.addSelectionRow(1);
	
	palleteRoot = new DefaultMutableTreeNode(palleteData);
	palleteModel = new DefaultTreeModel(palleteRoot);
	palleteTree = new JTree(palleteModel);
	palleteTree.setCellRenderer(new JCellRenderer());
	palleteTree.setToggleClickCount(2);
	insertChildren(palleteData, palleteRoot);
	// expand
	DefaultMutableTreeNode xx = (DefaultMutableTreeNode)palleteRoot.getLastLeaf().getParent();
	if(xx == null) System.out.printf("Shit/\n");
	else palleteTree.expandPath(new TreePath(xx.getPath()));
	
	setLayout(new GridLayout(0,2));

	JScrollPane sp_pallete = new JScrollPane();
	sp_pallete.getViewport().add(palleteTree);

	JPanel bl = new JPanel(new BorderLayout());
	bl.add(new JLabel("Histogram List"), BorderLayout.NORTH);
	bl.add(sp_pallete, BorderLayout.CENTER);
	getContentPane().add(bl);

	JScrollPane sp_tree = new JScrollPane();
	sp_tree.getViewport().add(tree);

	bl = new JPanel(new BorderLayout());
	//	bl.getContentPane().setLayout(new BorderLayout());
	bl.add(new JLabel("Screen Descriptions"), BorderLayout.NORTH);
	bl.add(sp_tree, BorderLayout.CENTER);	
	
	getContentPane().add(bl);

	tree.setRootVisible(false);
	palleteTree.setRootVisible(false);

	palleteTree.addMouseListener(new MouseAdapter() {
		public void mouseClicked(MouseEvent me) {
		    doMouseClicked(me, palleteTree);
		}

		public void mousePressed(MouseEvent me) {
		    doMousePressed(me, palleteTree);
		}

		public void mouseReleased(MouseEvent me) {
		    doMouseReleased(me, palleteTree);
		}
	    });

	tree.addMouseListener(new MouseAdapter() {
		public void mouseClicked(MouseEvent me) {
		    doMouseClicked(me, tree);
		}
		
		public void mousePressed(MouseEvent me) {
		    doMousePressed(me, tree);
		}
		
		public void mouseReleased(MouseEvent me) {
		    doMouseReleased(me, tree);
		}
	    });

	JMenuBar mb = new JMenuBar();
	JMenu menu = new JMenu("File");
	mb.add(menu);
	JMenuItem mi = new JMenuItem("SaveAs...");
	mi.addActionListener(this);
	menu.add(mi);	
	mi = new JMenuItem("Help...");
	mi.addActionListener(this);
	menu.add(mi);
	setJMenuBar(mb);

	
	myPropertyFrame = new PropertyFrame(treedata.child, 1100,100);
    }

    public void valueChanged(TreeSelectionEvent e)
    {
	System.out.printf("Value\n");
	TreePath tp = e.getNewLeadSelectionPath();
	//System.out.printf("tp =" + tp);

	if(tp == null) {
	    myPropertyFrame.useNewNode(null);
	    return;
	}

	DefaultMutableTreeNode o = (DefaultMutableTreeNode)tp.getLastPathComponent();
	JNode n = (JNode)o.getUserObject();

	if(myPropertyFrame != null) {
	    myPropertyFrame.saveNode();
	    myPropertyFrame.useNewNode(n);
	}
    }

    DefaultMutableTreeNode createNewChildTab(JTree t, DefaultMutableTreeNode o)
    {
	JNode n = (JNode)o.getUserObject();

	// Handle node shuffling...
	JNode nnode = new JNode();
	nnode.setName("EmptyTab");
	nnode.parent = n;
	nnode.prev = null;
	nnode.next = n.child;
	if(n.child != null) n.child.prev = nnode;
	n.child = nnode;

	DefaultMutableTreeNode ntnode = new DefaultMutableTreeNode(nnode);
	o.insert(ntnode, 0);
		
	DefaultTreeModel m = (DefaultTreeModel)t.getModel();
	m.nodeStructureChanged(o);
	
	return ntnode;
    }


    JNode findChildOrSiblingByName(JNode node, String name) {
	if(node.name.equals(name)) {
	    return node;
	}

	JNode ret = null;
	if(node.next != null) {
	    ret = findChildOrSiblingByName(node.next, name);
	    if(ret != null) return ret;
	}

	if(node.child != null) {
	    ret = findChildOrSiblingByName(node.child, name);
	    if(ret != null) return ret;
	}

	return ret;
    }

    Boolean inPallete(String name)
    {
	JNode ret = findChildOrSiblingByName(palleteData, name);
	if(ret != null) return true;
	return false;
    }


    String checkChildren(JNode n, Boolean checkSiblings)
    {

	System.out.printf("Checking node %s\n",n.name);

	String me = null;
	String children = null;
	String siblings = null;
	
	if(n.child != null) {
	    children = checkChildren(n.child,true);
	}

	if(checkSiblings) {
	    if(n.next != null) {
		siblings = checkChildren(n.next,true);
	    }
	}

	if(n.leaf) {
	    if(!inPallete(n.name)) {
		me = n.name;
	    }
	}

	String answer = null;

	if(me != null) {
	    answer = me;
	}
	if(siblings != null) {
	    if(answer == null) {
		answer = siblings;
	    }
	    else {
		answer = answer + "," + siblings;
	    }
	}
	if(children != null) {
	    if(answer == null) {
		answer = children; 
	    }
	    else {
		answer = answer + "," + children;
	    }
	}

	System.out.printf("returning:  me=%s answer=%s\n",n.name,answer);

	return answer;
    }

    DefaultMutableTreeNode createNewSiblingTab(JTree t, DefaultMutableTreeNode o)
    {
	JNode n = (JNode)o.getUserObject();

	// Handle node shuffling...
	JNode nnode = new JNode();
	nnode.setName("EmptyTab");
	nnode.parent = n.parent;
	nnode.prev = n;
	nnode.next = n.next;
	if(n.next != null) n.next.prev = nnode;
	n.next = nnode;

	DefaultMutableTreeNode ntnode = new DefaultMutableTreeNode(nnode);
	DefaultMutableTreeNode parent = (DefaultMutableTreeNode)o.getParent();
	int idx = parent.getIndex(o);
	parent.insert(ntnode, idx+1);
		
	DefaultTreeModel m = (DefaultTreeModel)t.getModel();
	m.nodeStructureChanged(parent);	   

	return ntnode;
    }

    JTree menuTree;
    TreePath menuPath;
    
    public void actionPerformed(ActionEvent e)
    {
	System.out.printf("got %s for element %s\n", e.getActionCommand(), menuPath);

	if(e.getActionCommand().equals("Help...")) {
	    JOptionPane.showMessageDialog(null, "To start:   java JevpEdit <file>\n\nDrag and drop to move item:\n     +ctrl to copy\n      +shift to copy as a sibling"); 
	}
	if(e.getActionCommand().equals("SaveAs...")) {

	    //myPropertyFrame.saveNode();

	    JFileChooser d = new JFileChooser(basedir);
	    int x = d.showSaveDialog(this);
	    if(x == d.APPROVE_OPTION) {
		String fn = d.getName(d.getSelectedFile());
		// System.out.printf("File = %s\n",fn);

		try {
 		    if(myPropertyFrame != null)
 			myPropertyFrame.saveNode();

		    PrintStream out = new PrintStream(new FileOutputStream(d.getSelectedFile()));
		    
		    out.printf("<doc>\n");
		    treedata.child.writeXML(out, 0, false);

		    //out.printf("<pallete>\n");
		    //palleteData.child.writeXML(out, 1, true);
		    //out.printf("</pallete>\n");

		    out.printf("</doc>\n");
		    out.close();
		} catch(Exception ex) {
		    ex.printStackTrace();
		}
	    }
	    
	    return;
	}

	DefaultMutableTreeNode o = (DefaultMutableTreeNode)menuPath.getLastPathComponent();
	JNode n = (JNode)o.getUserObject();	

	if(e.getActionCommand().equals("new child")) {
	    createNewChildTab(menuTree, o);
       	}
	if(e.getActionCommand().equals("new sibling")) {
	    createNewSiblingTab(menuTree, o);
	}

	if(e.getActionCommand().equals("group in subdir")) {
	    TreePath []paths = menuTree.getSelectionPaths();
	    if(paths == null) {
		System.out.printf("Paths = null?  must have selected nodes...\n");
		return;
	    }

	    DefaultMutableTreeNode newtab = createNewSiblingTab(menuTree, o);
	    

	    for(int i=0;i<paths.length;i++) {
		DefaultMutableTreeNode node = (DefaultMutableTreeNode)paths[i].getLastPathComponent();
		
		JNode nn = (JNode)node.getUserObject();
		System.out.printf("selected node %s\n",nn.name);

		TreePath destpath = new TreePath(newtab.getPath());
		deepCopyNodeToChild(nn, destpath);

		deleteNode(node);
	    }
	}

	if(e.getActionCommand().equals("check children")) {
	    
	    JNode nn = (JNode)o.getUserObject();
	    String ans = checkChildren(nn, false);
	    System.out.printf("The following are not in the pallete: %s\n",ans);
	}


	if(e.getActionCommand().equals("delete")) {  // delete all children as well...
	    System.out.printf("got delete command: %s\n",n.name);

	    TreePath []paths = menuTree.getSelectionPaths();

	    
	    if(paths == null) {   // deal with the clicked upon row
		TreePath tp = new TreePath(o.getPath());
		deleteNode(tp);
	    }
	    else {                // otherwise use the selected rows...
		for(int i=0;i<paths.length;i++) {
		    deleteNode(paths[i]);
		}
	    }

	    // 	    // Deal with JNode structure...
	    // 	    if(n.next != null) {
	    // 		n.next.prev = n.prev;
	    // 	    }
	    // 	    if(n.prev != null) {
	    // 		n.prev.next = n.next;
	    // 	    }
	    // 	    if(n.prev == null) {
	    // 		n.parent.child = n.next;
	    // 	    }

	    // 	    // Deal with tree node structure...
	    // 	    DefaultMutableTreeNode parent = (DefaultMutableTreeNode)o.getParent();
	    // 	    int idx = parent.getIndex(o);
	    // 	    parent.remove(idx);
	    
	    // 	    DefaultTreeModel m = (DefaultTreeModel)menuTree.getModel();
	    // 	    m.nodeStructureChanged(parent);
	}
    }


    void doMouseClicked(MouseEvent me, JTree t) {
	System.out.printf("Do mouse clicked\n");

	TreePath tp = t.getPathForLocation(me.getX(), me.getY());
	if(tp == null) return;

	DefaultMutableTreeNode o = (DefaultMutableTreeNode)tp.getLastPathComponent();
	JNode n = (JNode)o.getUserObject();

	if(me.getButton() == me.BUTTON3) {
	    if(t != tree) return;  // only do the configuratble tree!


	    // System.out.printf("Popup trigger...\n");
	    menuTree = t;
	    menuPath = tp;
	    popup.show(me.getComponent(), me.getX(), me.getY());

	}
// 	else {
// 	    if((me.getClickCount() >= 2) && (t != palleteTree)) {
// 		new PropertyFrame(n, me.getX(), me.getY());
// 	    }
	    
// 	}
    }						
    
    public void something(MouseEvent me)
    {
	System.out.printf("Me = %s\n",me);
    }

    JNode dragNode;
    TreePath dragPath;
    JTree dragTree;

    void doMousePressed(MouseEvent me, JTree t)
    {
	System.out.printf("Do mouse pressed (%d, %d)\n",me.getX(), me.getY());


 	TreePath tp = t.getPathForLocation(me.getX(), me.getY());
	if(tp == null) return;
	
	DefaultMutableTreeNode o = (DefaultMutableTreeNode)tp.getLastPathComponent();
	JNode n = (JNode)o.getUserObject();

	dragNode = n;
	dragPath = tp;
	dragTree = t;
    }
    
    void doMouseReleased(MouseEvent me, JTree t)
    {
	
	//	System.out.printf("Mouse released...(%d, %d) %s %s\n",me.getX(),me.getY(),tree.getLocation(),palleteTree.getLocation());

	Point myPoint = me.getPoint();
	JTree him = (t==palleteTree) ? tree : palleteTree;
	Point hisPoint = SwingUtilities.convertPoint(t, myPoint, him);
	// System.out.printf("Release... My point %s his point %s\n",myPoint, hisPoint);


 	TreePath tp = t.getPathForLocation((int)myPoint.getX(), (int)myPoint.getY());
	if(tp == null) {
	    tp = him.getPathForLocation((int)hisPoint.getX(), (int)hisPoint.getY());
	    if(tp == null) {
		// System.out.printf("Still bad\n");
		return;
	    }
	    t = him;
	}
	
	if(t == palleteTree) {  // Don't allow moves into pallete tree...
	    // System.out.printf("Trying to move to pallete tree... nope!\n");
	    return;
	}

	DefaultMutableTreeNode o = (DefaultMutableTreeNode)tp.getLastPathComponent();
	JNode n = (JNode)o.getUserObject();

	// System.out.printf("Mouse released on node %s\n",n);

	if((dragNode != null) && (dragNode != n)) {
	    // System.out.printf("Dragged %s to %s\n",dragNode, n);

	    if(legalToCopyNode(dragNode, tp)) {

		// System.out.printf("Legal to copy node\n");
		DefaultMutableTreeNode nn = (DefaultMutableTreeNode)tp.getLastPathComponent();
		JNode nnn = (JNode)nn.getUserObject();

		if(nnn.leaf || ((me.getModifiersEx() & me.SHIFT_DOWN_MASK) == me.SHIFT_DOWN_MASK)) {
		    deepCopyNodeToNextSibling(dragNode, tp);
		}
		else {
		    deepCopyNodeToChild(dragNode, tp);
		}
		
		// if ctrl down copy...
		if((dragTree == tree) && ((me.getModifiersEx() & me.CTRL_DOWN_MASK) == 0)) {
		    // System.out.printf("Deleting path %s\n",dragPath);
		    deleteNode(dragPath);
		}
	    }
	    else {
		// System.out.printf("Not legal...\n");
	    }
	}
	
	dragNode = null;
	dragPath = null;
	dragTree = null;
    }
 
    JNode copyNodeTree(JNode node)  // initial one never has siblings for a copy...
    {
	return copyNodeTree(node, null);
    }

    JNode copyNodeTree(JNode node, JNode newtree_parent)
    {
	JNode x = new JNode();
	x.setName(node.name);
	x.leaf = node.leaf;
	x.parent = newtree_parent;

	if(node.child != null) {
	    JNode c = copyNodeTree(node.child, x);
	    x.child = c;
	}

	if(newtree_parent == null) return x;   // node may have siblings, but don't want them in the copy if par == null

	if(node.next != null) {
	    JNode n = copyNodeTree(node.next, newtree_parent);
	    n.prev = x;
	    x.next = n;
	}
	
	return x;
    }

    public void deepCopyNodeToChild(JNode node, TreePath path)
    {
	DefaultMutableTreeNode o = (DefaultMutableTreeNode)path.getLastPathComponent();
	JNode dnode = (JNode)o.getUserObject();

	JNode copy = copyNodeTree(node);

	if(dnode.leaf) {
	    System.out.printf("Copying to child of leaf... not a good idea!\n");
	}
	
	JNode parent = dnode;
	copy.prev = null;
	copy.parent = parent;
	copy.next = parent.child;
	if(parent.child != null) parent.child.prev = copy;
	parent.child = copy;
	
	// now tree structure...
	DefaultMutableTreeNode x = new DefaultMutableTreeNode(copy);
	treeModel.insertNodeInto(x, o, 0);
	insertChildren(copy, x);
    }

    public void deepCopyNodeToNextSibling(JNode node, TreePath path)
    {
	DefaultMutableTreeNode o = (DefaultMutableTreeNode)path.getLastPathComponent();
	JNode dnode = (JNode)o.getUserObject();

	JNode copy = copyNodeTree(node);

	copy.parent = dnode.parent;
	copy.prev = dnode;
	copy.next = dnode.next;
	if(dnode.next != null) dnode.next.prev = copy;
	dnode.next = copy;
	    
	// Now do tree structure...
	DefaultMutableTreeNode p = (DefaultMutableTreeNode)o.getParent();
	int idx = p.getIndex(o);
	DefaultMutableTreeNode x = new DefaultMutableTreeNode(copy);
	treeModel.insertNodeInto(x, p, idx+1);
	insertChildren(copy, x);
    }

    /*
      public void deepCopyNode(JNode node, TreePath path)
      {
      DefaultMutableTreeNode o = (DefaultMutableTreeNode)path.getLastPathComponent();
      JNode dnode = (JNode)o.getUserObject();


      // System.out.printf("copy...\n");

      JNode copy = copyNodeTree(node);
      //	copy.dump();

      if(node.leaf && !dnode.leaf) {  // insert leaf as child to tab
      // System.out.printf("Inserting as child\n");

      JNode parent = dnode;
	    
      // do JNode first...
      copy.prev = null;
      copy.parent = parent;
      copy.next = parent.child;
      if(parent.child != null) parent.child.prev = copy;
      parent.child = copy;
	    
      // now tree structure...
      // System.out.printf("Here...\n");
      DefaultMutableTreeNode x = new DefaultMutableTreeNode(copy);
      // System.out.printf("There\n");
      treeModel.insertNodeInto(x, o, 0);
      // System.out.printf("Anywhere\n");
	    
      //insertChildren(copy, x);
      }    
      else {  // otherwise, insert as sibling...
      //System.out.printf("Inserting as sibling\n");

      copy.parent = dnode.parent;
      copy.prev = dnode;
      copy.next = dnode.next;
      if(dnode.next != null) dnode.next.prev = copy;
      dnode.next = copy;
	    
      // Now do tree structure...
      DefaultMutableTreeNode p = (DefaultMutableTreeNode)o.getParent();
      int idx = p.getIndex(o);
      DefaultMutableTreeNode x = new DefaultMutableTreeNode(copy);
      treeModel.insertNodeInto(x, p, idx+1);

      //System.out.printf("Inserting children...\n");
      insertChildren(copy, x);
      //System.out.printf("Inserted children...\n");
      }
      }
    */

    public boolean legalToCopyNode(JNode node, TreePath path)
    {	
	// System.out.printf("Check if legal %s %s\n",node,path);

	DefaultMutableTreeNode o = (DefaultMutableTreeNode)path.getLastPathComponent();

	// System.out.printf("Got component\n");

	JNode dnode = (JNode)o.getUserObject();

	// System.out.printf("here\n");

	if(node.leaf && !dnode.leaf) { 
	    JNode child = dnode.child;
	    if(child != null) {
		if(!child.leaf) {
		    // System.out.printf("Can't insert a leaf to a tab containing tabs...\n");
		    return false;   // found a tab child.
		}
	    }
	}
	else if(!node.leaf && dnode.leaf) {   
	    // System.out.printf("Can't insert a tab into a leaf\n");
	    return false;
	}

	return true;
    }

    // 	if(node.leaf && dnode.leaf) {   // add node to same tab as dnode
    // 	    System.out.printf("Legal to insert a leaf into a tab with leaves...\n");
    // 	    return true;
    // 	}    
    // 	else if(node.leaf && !dnode.leaf) { 
    // 	    JNode child = dnode.child;
    // 	    while(child != null) {
    // 		if(!child.leaf) {
    // 		    System.out.printf("Can't insert a leaf to a tab containing tabs...\n");
    // 		    return false;   // found a tab child.
    // 		}
    // 	    }
    // 	    System.out.printf("Legal to insert a leaf into tab containing leaves...\n");
    // 	    return true;
    // 	}
    // 	else if(!node.leaf && dnode.leaf) { 
    // 	    System.out.printf("Can't insert a tab into the leaf level...\n");
    // 	    return false; 
    // 	}
    // 	else if(!node.leaf && !dnode.leaf) {   // can add a tab to a tab only if not leaves below...   
    // 	    JNode child = dnode.child;
    // 	    while(child != null) {
    // 		if(child.leaf) {
    // 		    System.out.printf("Can't insert a tab into to a tab containing leaves...\n");
    // 		    return false;   // found a tab child.
    // 		}
    // 	    }
    // 	    System.out.printf("Legal to insert a tab into tab containing tabs...\n");
    // 	    return true;
    // 	}
    // 	return false;
  
    public void deleteNode(TreePath path) 
    {
	DefaultMutableTreeNode o = (DefaultMutableTreeNode)path.getLastPathComponent();
	deleteNode(o);
    }

    public void deleteNode(DefaultMutableTreeNode o)
    {
	JNode node = (JNode)o.getUserObject();

	// Handle JNode manipulations...
	if(node.parent.child == node) node.parent.child = node.next;
	if(node.prev != null) node.prev.next = node.next;
	if(node.next != null) node.next.prev = node.prev;

	// Handle tree manipulations...
	DefaultMutableTreeNode parent = (DefaultMutableTreeNode)o.getParent();
	parent.remove(o);
	
	DefaultTreeModel m = (DefaultTreeModel)tree.getModel();
	m.nodeStructureChanged(parent);
    }

    public static void main(String[] args)
    {
	JevpEdit me = new JevpEdit();

	if(args.length == 0) {
	    System.out.printf("need a filename!");
	    return;
	}
	
	try {

	    // Load the xml document...
	    String s;
	    String pallete_name;
	    if(args[0].equals("-live")) {
		s = "/RTScache/conf/jevp/HistoDefs.txt";
		pallete_name = "/RTScache/conf/jevp/Pallete.txt";
	    }
	    else if(args[0].equals("-l4")) {
		s = "/RTScache/conf/l4jevp/HistoDefs.txt";
		pallete_name = "/RTScache/conf/l4jevp/Pallete.txt";
	    }
	    else {
		s = args[0];
		pallete_name = "Pallete.txt";
	    }
	    
	    
	    
	    File file = new File(s);

	    me.basedir = file.getAbsolutePath();

	    DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
	    DocumentBuilder db = dbf.newDocumentBuilder();
	    Document doc = db.parse(file);
	    Node xmlroot = doc.getDocumentElement();
	    // parse it into a JNode...
	    me.treedata = JNode.createNode(xmlroot);
	    //   me.treedata.dump();

	    DocumentBuilder pdb = dbf.newDocumentBuilder();
	    Document pdoc = pdb.parse(new File(pallete_name));
	    Node pxmlroot = pdoc.getDocumentElement();
	    me.palleteData = JNode.createPallete(pxmlroot);

	    me.init();
	    me.setVisible(true);

	} catch (Exception ex) {
	    ex.printStackTrace();
	}
    }    
}

class JCellRenderer extends DefaultTreeCellRenderer
{

    Icon folder = new ImageIcon("folder2.gif");
    Icon histo = new ImageIcon("histo2.gif");
    
    JCellRenderer()
    {
	setLeafIcon(histo);
	setOpenIcon(folder);
	setClosedIcon(folder);
    }

    public Component getTreeCellRendererComponent(JTree tree,
						  Object value, 
						  boolean select,
						  boolean expanded,
						  boolean leaf,
						  int row,
						  boolean hasFocus)
    {
	DefaultMutableTreeNode mtn = (DefaultMutableTreeNode)value;
	JNode o = (JNode)mtn.getUserObject();
	leaf = o.leaf;

	return super.getTreeCellRendererComponent(tree,
						  value,
						  select,
						  expanded,
						  leaf,
						  row,
						  hasFocus);
	
    }

}

