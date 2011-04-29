#include "orchard.h"

#include "messageserver.h"

#include <QDeclarativeEngine>
#include <QDeclarativeContext>
#include <QDeclarativeComponent>

Orchard::Orchard(QObject *parent)
    : QObject (parent)
{
    qmlRegisterType <Picker> ();
    qmlRegisterType <TreeState> ();

    for (int i = 0; i < 5; ++i)
    {
        _treeStates.push_back (new TreeState ("invisible"));
    }
}

QDeclarativeImageProvider *
Orchard::getSpriteProvider ()
{
    return &_spriteProvider;
}

namespace
{
    void pickersAppend (QDeclarativeListProperty <Picker> *pickers, Picker *picker)
    {
        Q_UNUSED (pickers)
        Q_UNUSED (picker)
        return;
    }

    int pickersCount (QDeclarativeListProperty <Picker> *pickers)
    {
        return static_cast <QMap <int, Picker *> *> (pickers->data)->count ();
    }

    Picker *pickersAt (QDeclarativeListProperty <Picker> *pickers, int index)
    {
        return static_cast <QMap <int, Picker *> *> (pickers->data)
                ->values ().at (index);
    }
}

QDeclarativeListProperty <Picker>
Orchard::pickers ()
{
    return QDeclarativeListProperty <Picker> (this, &_pickers, &pickersAppend,
                                              &pickersCount, &pickersAt);
}

namespace
{
    void treeStatesAppend (
            QDeclarativeListProperty <TreeState> *treeStates,
            TreeState *treeState)
    {
        Q_UNUSED (treeStates)
        Q_UNUSED (treeState)
        return;
    }

    int treeStatesCount (
            QDeclarativeListProperty <TreeState> *treeStates)
    {
        return static_cast <QList <TreeState *> *> (treeStates->data)->count ();
    }

    TreeState *treeStatesAt (
            QDeclarativeListProperty <TreeState> *treeStates, int index)
    {
        return static_cast <QList <TreeState *> *> (
                treeStates->data)->at (index);
    }
}

QDeclarativeListProperty <TreeState>
Orchard::treeStates ()
{
    return QDeclarativeListProperty <TreeState> (this, &_treeStates,
                &treeStatesAppend, &treeStatesCount, &treeStatesAt);
}

void
Orchard::getMessage (int id)
{
    QString title, body;
    MessageServer *server = dynamic_cast <MessageServer *> (sender ());

    if (!server || !server->getMessage (id, title, body))
    {
        return;
    }

    if (0 == title.compare ("xyzzy"))
    {
        server->sendMessage (id, "nothing happens", "");
    }
    else if (0 == title.compare ("name") && 0 == _pickers.count (id))
    {
        _pickers[id] = new Picker (body, this);
        _pickerModel.add (_pickers[id]);
        //emit pickersChanged ();
        server->sendMessage (id, "response", QString (body) + " added");
        qDebug () << body << "added";
    }
    else if (0 == title.compare ("sprites") && _pickers.count (id))
    {
        QPixmap pixmap;
        pixmap.loadFromData (QByteArray::fromBase64 (body.toAscii ()));
        _spriteProvider.addSprites (_pickers[id]->name (), pixmap);
        server->sendMessage (id, "response",
            QString ("sprites added for " + _pickers[id]->name ()));
    }
    else if (0 == title.compare ("state") && _pickers.count (id))
    {
        _pickers[id]->setPickerState (body);
        _pickerModel.changeState (_pickers[id], body);
        server->sendMessage (id, "response",
            _pickers[id]->name () + " state changed to " + body);
        qDebug () << (_pickers[id]->name ()) << "state changed to" << body;
    }
}

void
Orchard::pickerDC (int id)
{
    qDebug () << _pickers[id]->name () << "disconnected";
    _pickers.remove (id);
    emit pickersChanged ();
}

Orchard::PickerImageProvider::PickerImageProvider ()
    : QDeclarativeImageProvider (Pixmap)
{ }

QPixmap
Orchard::PickerImageProvider::requestPixmap (const QString &id, QSize *size,
                                             const QSize &requestedSize)
{
    Q_UNUSED (size)
    Q_UNUSED (requestedSize)

    if (0 < _pixmaps.count (id))
    {
        return _pixmaps[id];
    }
    else
    {
        return QPixmap ();
    }
}

void
Orchard::PickerImageProvider::addSprites (QString const &id,
                                         QPixmap const &pixmap)
{
    _pixmaps[id] = pixmap;
}

TreeState::TreeState (QString const &state)
    : _treeState (state)
{
    connect (&_timer, SIGNAL (timeout ()), SLOT (toggleState ()));
    _timer.setInterval (5000 + rand () % 15000);
    _timer.start ();
}

void
TreeState::toggleState ()
{
    if (_treeState == "invisible")
    {
        _treeState = "ripe";
        _timer.setInterval (5000 + rand () % 15000);
    }
    else
    {
        _treeState = "invisible";
        _timer.setInterval (5000 + rand () % 5000);
    }

    emit treeStateChanged ();
}
