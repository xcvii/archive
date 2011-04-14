#ifndef SCENE_H
#define SCENE_H

#include <QDeclarativeListProperty>
#include <QObject>
#include <QPixmap>
#include <QSharedPointer>

struct Tree;

class Picker : public QObject
{
    Q_OBJECT

    QPixmap _walkStrip;
    QPixmap _idleStrip;
    QPixmap _specStrip;

public:
    void setWalkStrip ();

    Q_PROPERTY (QPixmap walkStrip READ walkStrip NOTIFY walkStripChanged)
    QPixmap walkStrip () const { return _walkStrip; }

    Q_PROPERTY (QPixmap idleStrip READ idleStrip NOTIFY idleStripChanged)
    QPixmap idleStrip () const { return _idleStrip; }

    Q_PROPERTY (QPixmap specStrip READ specStrip NOTIFY specStripChanged)
    QPixmap specStrip () const { return _specStrip; }
};

class Scene : public QObject
{
    Q_OBJECT

    QList <Tree *> _trees;
    QList <Picker *> _pickers;

public:
    explicit Scene (QObject *parent = 0);

    Q_PROPERTY (QDeclarativeListProperty <Tree> trees READ trees CONSTANT)
    QDeclarativeListProperty <Tree> trees ()
    { return QDeclarativeListProperty <Tree> (this, _trees); }

    Q_PROPERTY (QDeclarativeListProperty <Picker> pickers READ pickers CONSTANT)
    QDeclarativeListProperty <Picker> pickers ()
    { return QDeclarativeListProperty <Picker> (this, _pickers); }

signals:

public slots:

};

#endif // SCENE_H
