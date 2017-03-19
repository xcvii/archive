#ifndef ORCHARD_H
#define ORCHARD_H

#include "messageserver.h"
#include "picker.h"

#include <QObject>
#include <QDeclarativeView>
#include <QDeclarativeImageProvider>
#include <QDeclarativeListProperty>
#include <QMap>
#include <QTimer>
#include <QAbstractListModel>
#include <QtAlgorithms>

class TreeState;

class Orchard : public QObject
{
    Q_OBJECT

    class PickerImageProvider : public QDeclarativeImageProvider
    {
        QMap <QString, QPixmap> _pixmaps;

    public:
        PickerImageProvider ();

        QPixmap requestPixmap (const QString &id, QSize *size,
                               const QSize &requestedSize);

        void addSprites (QString const &id, QPixmap const &pixmap);
    };

    class PickerModel : public QAbstractListModel
    {
        QList <Picker *> _pickers;

    public:
        enum PickerRoles { NameRole = Qt::UserRole + 1, StateRole };

        PickerModel ()
        {
            QHash <int, QByteArray> roles;
            roles[NameRole] = "nameProp";
            roles[StateRole] = "stateProp";
            setRoleNames (roles);
        }

        int rowCount (const QModelIndex &parent = QModelIndex ()) const
        { Q_UNUSED (parent); return _pickers.size (); }

        QVariant data (const QModelIndex &index, int role) const
        {
            if (!index.isValid ()) return QVariant ();
            if (role == NameRole) return _pickers[index.row ()]->name ();
            else if (role == StateRole) return _pickers[index.row ()]->pickerState ();
            else return QVariant ();
        }

        void add (Picker *picker)
        {
            beginInsertRows (QModelIndex (), rowCount (), rowCount ());
            _pickers << picker;
            endInsertRows ();
        }

        void remove (Picker *picker)
        {
            int i = qFind (_pickers, picker) - _pickers.begin ();
            beginRemoveRows (QModelIndex (), i, i);
            _pickers.removeAt (i);
            endRemoveRows ();
        }

        void changeState (Picker *picker, QString state)
        {
            picker->setPickerState (state);
            int i = qFind (_pickers, picker) - _pickers.begin ();
            dataChanged (index (i), index (i));
        }
    } _pickerModel;

    MessageServer const *_messageServer;
    PickerImageProvider _spriteProvider;
    QMap <int, Picker *> _pickers;
    QList <TreeState *> _treeStates;

public:
    explicit Orchard (MessageServer const *server, QObject *parent = 0);
    QDeclarativeImageProvider *getSpriteProvider ();

    QDeclarativeListProperty <Picker> pickers ();

    Q_PROPERTY (QDeclarativeListProperty <Picker> pickers READ pickers
                NOTIFY pickersChanged);

    QDeclarativeListProperty <TreeState> treeStates ();
    Q_PROPERTY (QDeclarativeListProperty <TreeState> treeStates READ treeStates
                NOTIFY treeStatesChanged);

    QAbstractListModel *pickerModel () { return &_pickerModel; }

signals:
    void pickersChanged ();
    void treeStatesChanged ();
    void appleHitsPicker (QString);

private slots:
    void handleAppleHitsPicker (QString);

public slots:
    void getMessage (int id);
    void pickerDC (int id);

};

class TreeState : public QObject
{
    Q_OBJECT

    QString _treeState;
    QTimer _timer;
public:
    TreeState (QString const &state);
    QString treeState () { return _treeState; }
    void setTreeState (QString const &state)
    { _treeState = state; emit treeStateChanged (); }

    Q_PROPERTY (QString treeState READ treeState WRITE setTreeState
                NOTIFY treeStateChanged)

signals:
    void treeStateChanged ();

public slots:
    void toggleState ();
};

#endif // ORCHARD_H
