#include "jmlQtGui.h"
#include "RunDialog.h"

char *RunDialog::getResult() {
  return (char *)runLineEdit->text().ascii();
}

RunDialog::RunDialog(QWidget *parent)
  : QDialog(parent)
{
  okButton = createButton(tr("&Ok"), SLOT(accept()));
  cancelButton = createButton(tr("&Cancel"), SLOT(reject()));

  runLineEdit = createLineEdit("/net/evp/a");
  runLabel = new QLabel(tr("Run:"));
  createRunTable();

  QHBoxLayout *buttonsLayout = new QHBoxLayout;
  buttonsLayout->addStretch();
  buttonsLayout->addWidget(cancelButton);
  buttonsLayout->addWidget(okButton);

  QGridLayout *mainLayout = new QGridLayout;
  mainLayout->addWidget(runLabel, 0, 0);
  mainLayout->addWidget(runLineEdit, 0, 1);
  mainLayout->addWidget(runTable, 1, 0, 2, 10);
  mainLayout->addLayout(buttonsLayout, 11, 0, 2, 3);
  setLayout(mainLayout);

  setWindowTitle(tr("Choose Run"));
  resize(700, 300);
}

void RunDialog::fill()
{
  runTable->setRowCount(0);

  QString path = runLineEdit->text();
  QDir directory = QDir(path);

  QStringList files;
  //  if (fileName.isEmpty())
  //fileName = "*";
  files = directory.entryList(QStringList("*"),
			      QDir::Dirs | QDir::Files | QDir::NoSymLinks);

  showFiles(directory, files);
}


void RunDialog::showFiles(const QDir &directory, const QStringList &files)
{
  for (int i = 0; i < files.size(); ++i) {
    QFile file(directory.absoluteFilePath(files[i]));

    QTableWidgetItem *fileNameItem = new QTableWidgetItem(files[i]);
    fileNameItem->setFlags(Qt::ItemIsEnabled);
    int row = runTable->rowCount();
    runTable->insertRow(row);
    runTable->setItem(row, 0, fileNameItem);
       
  }
}

QPushButton *RunDialog::createButton(const QString &text, const char *member)
{
  QPushButton *button = new QPushButton(text);
  connect(button, SIGNAL(clicked()), this, member);
  return button;
}

QLineEdit *RunDialog::createLineEdit(const QString &text)
{
  QLineEdit *lineEdit = new QLineEdit;
  //  textEdit->setEditable(true);
  lineEdit->setText(text);
  lineEdit->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Preferred);
  return lineEdit;
}

void RunDialog::createRunTable()
{
  runTable = new QTableWidget(0, 1);
  QStringList labels;
  labels << tr("Run");
  runTable->setHorizontalHeaderLabels(labels);
  runTable->horizontalHeader()->setResizeMode(0, QHeaderView::Stretch);
  runTable->verticalHeader()->hide();
  runTable->setShowGrid(true);

  connect(runTable, SIGNAL(currentItemChanged(QTableWidgetItem*,QTableWidgetItem*)), this, SLOT(selectrow(QTableWidgetItem*,QTableWidgetItem*)));

  fill();
}

// void RunDialog::selectrow(int row, int col)
// {
//   printf("row=%d\n",row);
//   printf("Selected...\n");
//   QColor wcol("white");
//   QColor ocol("orange");
//   QBrush wbrush(wcol);
//   QBrush obrush(ocol);

//   curr->setBackground(obrush);
//   prev->setBackground(wbrush);
// }

void RunDialog::selectrow(QTableWidgetItem *curr, QTableWidgetItem *prev)
{
  printf("Selected...\n");
  QColor wcol("white");
  QColor ocol("yellow");
  QBrush wbrush(wcol);
  QBrush obrush(ocol);

  if(curr) {
    curr->setBackground(obrush);
    QString x = curr->text();
    runLineEdit->setText(x);
  }

  if(prev) 
    prev->setBackground(wbrush);

  
}
